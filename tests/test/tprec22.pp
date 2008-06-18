// http://lists.freepascal.org/lists/fpc-devel/2008-June/013919.html

// I was interested to see if bit packing works when a record member spans
// byte boundaries, and in general it appears to work. However on my system
// I discovered a bug that this program illustrates.
//
// This program demonstrates a bug using a bitpacked record where a member
// crosses a byte boundary.
// The record structure is (on little endian systems -- Jonas):
//  Member: | bit15_9 | bit8_1 | bit0 |
//   Bits:  | 15 .. 9 | 8 .. 1 | 0    |
//   Value: | 0..127  | 0..255 | 0..1 |
//
// The structure  is mapped to a word via a variant record for convenience.
//
// The limited amount of testing done indicates that the record member bit8_1
// only causes a problem with a value of $FF, but the interesting thing is
// that the result varies depending on other (unrelated) program structure.
// For example the expected word result with bit 0 = 1, bits 1..9 = $FF and
// the rest 0, should be $01FF but I have seen the correct value as well as
// results of $0001, $0003, $0121, $012. Adding code before the tests seems
// to change the result, possibly/ indicating that some variable or register
// used in the bitpacking routine is not being cleared/initialized.
//
// Different compiler modes, optimisations, range checking were tried, but
// the results were the same.
//
// Note that using a variant record to show the value is only a convenience
// here and the error can be seen without a variant record by examining
// the struct directly, or by overlaying the word using the absolute keyword.
//
// Tested on Intel Core 2 Duo running Windows XP Pro SP2, Compiler version
// 2.2.0 [2007/09/09] and 2.3.1  [2008/02/03]



uses SysUtils;


type
    bit = 0..1;
    t7bit = 0..127;

    // A record to test behaviour over byte boundaries.
    BitStruct = bitpacked record
        bit0 : bit;
        bit8_1 : byte;   // This set to $FF causes problems...
        bit15_9 : t7bit;
    end;

    // Map the record to a word for convenience - but overlaying
    // a word using absolute instead a variant record produces
    // the same result.

    MappedStruct = packed record
        case boolean of
            false : (AsWord : word);
            true  : (AsBits : BitStruct);
    end;


procedure TestBits;
var
    TestLocal : MappedStruct;
begin
    TestLocal.AsBits.bit0 := 1;
    TestLocal.AsBits.bit8_1 := $FF;
    TestLocal.AsBits.bit15_9 := $0;
    if (TestLocal.AsBits.bit0<>1) or
       (TestLocal.AsBits.bit8_1<>$ff) or
       (TestLocal.AsBits.bit15_9<>0) then
      halt(1);
//    writeln('  Expected : $01FF, Got : $',IntToHex(TestLocal.AsWord,4),' (I get $0121 V2.2.0, $0109 V2.3.1)');
end;


var
    TestGlobal : MappedStruct;
begin
//Do test in main routine - on my system results in $0001.
// Also interesting  - using 'with TestGlobal, AsBits do begin ...' instead of
// fully qualified names returns different values in some cases.

    Writeln('Testing in main: | $00 | $FF | 1 |');
    TestGlobal.AsBits.bit0 := 1;
    TestGlobal.AsBits.bit8_1 := $FF;
    TestGlobal.AsBits.bit15_9 := $0;
    if (TestGlobal.AsBits.bit0<>1) or
       (TestGlobal.AsBits.bit8_1<>$ff) or
       (TestGlobal.AsBits.bit15_9<>0) then
      halt(2);
//    writeln('  Expected : $01FF, Got : $',IntToHex(TestGlobal.AsWord,4), ' (I get $0001 V2.2.0, $01F9 V2.3.1)');

// Test it in a procedure - results in $0121 on V2.2.0
    writeln;
    Writeln('Testing in procedure: | $01 | $FF | 1 |');
    TestBits;

//  Test this in main
    Writeln;
    Writeln('Back in main: | $3F | $FF | 1 |');
    TestGlobal.AsBits.bit0 := 1;
    TestGlobal.AsBits.bit8_1 := $FF;
    TestGlobal.AsBits.bit15_9 := $3F;
    if (TestGlobal.AsBits.bit0<>1) or
       (TestGlobal.AsBits.bit8_1<>$ff) or
       (TestGlobal.AsBits.bit15_9<>$3f) then
      halt(3);
//    writeln('  Expected : $7FFF, Got : $',IntToHex(TestGlobal.AsWord,4),' ($7E01 V2.2.0, $7FF9 V2.3.1)');

// and again in main.
    Writeln;
    Writeln('In main, | $7F | $FF | 1 |');
    TestGlobal.AsBits.bit0 := 1;
    TestGlobal.AsBits.bit8_1 := $FF;
    TestGlobal.AsBits.bit15_9 := $7F;
    if (TestGlobal.AsBits.bit0<>1) or
       (TestGlobal.AsBits.bit8_1<>$ff) or
       (TestGlobal.AsBits.bit15_9<>$7f) then
      halt(4);
//    writeln('  Expected : $FFFF, Got : $',IntToHex(TestGlobal.AsWord,4), ' ($FE01 V.2.2.0, $FFF9 V2.3.1)');


// Now set bits 8..1 to $FE
   Writeln;
   Writeln('Above tests, but with bits 8..1 set to  $FE - all work on my system');

    Writeln(' | $00 | $FE | 1 |');
    TestGlobal.AsBits.bit0 := 1;
    TestGlobal.AsBits.bit8_1 := $FE;
    TestGlobal.AsBits.bit15_9 := $0;
    if (TestGlobal.AsBits.bit0<>1) or
       (TestGlobal.AsBits.bit8_1<>$fe) or
       (TestGlobal.AsBits.bit15_9<>0) then
      halt(5);
//    writeln('  Expected : $01FD, Got : $',IntToHex(TestGlobal.AsWord,4));

    Writeln;
    Writeln(' | $3F | $FE | 1 |');
    TestGlobal.AsBits.bit0 := 1;
    TestGlobal.AsBits.bit8_1 := $FE;
    TestGlobal.AsBits.bit15_9 := $3F;
    if (TestGlobal.AsBits.bit0<>1) or
       (TestGlobal.AsBits.bit8_1<>$fe) or
       (TestGlobal.AsBits.bit15_9<>$3f) then
      halt(6);
//    writeln('  Expected : $7FFD, Got : $',IntToHex(TestGlobal.AsWord,4));

// and again in main.
    Writeln;
    Writeln(' | $7F | $FE | 1 |');
    TestGlobal.AsBits.bit0 := 1;
    TestGlobal.AsBits.bit8_1 := $FE;
    TestGlobal.AsBits.bit15_9 := $7F;
    if (TestGlobal.AsBits.bit0<>1) or
       (TestGlobal.AsBits.bit8_1<>$fe) or
       (TestGlobal.AsBits.bit15_9<>$7f) then
      halt(7);
//    writeln('  Expected : $FFFD, Got : $',IntToHex(TestGlobal.AsWord,4));

end.

