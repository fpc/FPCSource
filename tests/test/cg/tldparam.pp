{ This tests the passing of parameters of routines }
{ and how they are accessed.                       }
{ Tests secondload() and secondcallparan()         }

{ TO DO :                                         }
{   Add testing for complex parameters            }
{   such as string, arrays and sets               }


{ ***************************************************************** }
{                         SIMPLE TYPES                              }
{ ***************************************************************** }

  procedure testvaluebyte(b: byte);
   begin
     WriteLn(b);
   end;


  procedure testvalueword(w: word);
   begin
     WriteLn(w);
   end;


  procedure testvaluelong(l : longint);
   begin
     WriteLn(l);
   end;


  procedure testvarbyte(var b: byte);
    begin
      WriteLn(b);
    end;


  procedure testvarword(var w: word);
    begin
      writeln(w);
    end;


  procedure testvarlong(var l : longint);
    begin
      writeln(l);
    end;


  procedure testvaluemixedbyte(b: byte; w: word; l: longint);
    begin
      Writeln(b);
      writeln(w);
      writeln(l);
    end;


  procedure testvaluemixedlong(l : longint; w: word; b: byte);
    begin
      Writeln(l);
      writeln(w);
      writeln(b);
    end;


  procedure testvaluemixedbytebyte(b1: byte; b2: byte; b3: byte);
    begin
      Writeln(b1);
      writeln(b2);
      writeln(b3);
    end;

{$ifdef fpc}
  procedure testvalueint64(i : int64);
    begin
      WriteLn(i);
    end;

  procedure testvarint64(var i : int64);
   begin
     WriteLn(i);
   end;

  procedure testvaluemixedint64(b1: byte; i: int64; b2: byte);
   begin
     WriteLn(b1);
     WriteLn(i);
     WriteLn(b2);
   end;
{$endif}

   procedure testvaluereal(r: real);
    begin
      WriteLn(r);
    end;

   procedure testvaluesingle(s: single);
    begin
      WriteLn(s);
    end;

   procedure testvaluedouble(d: double);
     begin
       WriteLn(d);
     end;


   procedure testvaluemixedreal(b1: byte; r: real; b2: byte);
    begin
      WriteLn(b1);
      WriteLn(r);
      WriteLn(b2);
    end;

  procedure testvarreal(var r: real);
   begin
     WriteLn(r);
   end;

   { only check assembler code }
   { cannot be called directly }
   { because will crash system }
   procedure testint; interrupt;
    begin
    end;



{ ***************************************************************** }
{                        COMPLEX TYPES                              }
{ ***************************************************************** }



{ ***************************************************************** }
{                        RETURN TYPES                               }
{ ***************************************************************** }


   function testretbyte: byte;
     begin
       Write('(byte) : Value should be 127...');
       testretbyte:= 127;
     end;

   function testretword: word;
     begin
       Write('(word) : Value should be 43690...');
       testretword := 43690;
     end;

   function testretlong : longint;
     begin
       Write('(long) : Value should be -1...');
       testretlong := -1;
     end;

   function testretstring: string;
     begin
       Write('(string) : Value should be ''HELLO WORLD''...');
       testretstring := 'HELLO WORLD';
     end;

    function testretreal : real;
      begin
        Write('(real) : Value should be 12.12...');
        testretreal := 12.12;
      end;

    function testretsingle : single;
      begin
        Write('(single) : Value should be 13.13...');
        testretsingle := 13.13;
      end;

    function testretdouble : double;
      begin
        Write('(double) : Value should be 14.14...');
        testretdouble := 14.14;
      end;

    function testretpchar: pchar;
    begin
       Write('(pchar) : Value should be  ...');
       testretpchar := nil;
    end;

{$ifdef fpc}
    function testretint64 : int64;
      begin
        Write('(int64) : Value should be -127...');
        testretint64 := -127;
      end;

    function testretansi: ansistring;
      begin
         Write('(ansi) : Value should be ''HELLO WORLD''...');
         testretansi := 'HELLO WORLD';
      end;


{$ifdef fpc}

{$inline on}

   function testretbyteinline: byte; inline;
     begin
       Write('(byte) : Value should be 126...');
       testretbyteinline:= 126;
     end;

   function testretwordinline: word; inline;
     begin
       Write('(word) : Value should be 43689...');
       testretwordinline := 43689;
     end;

    function testretint64inline : int64;inline;
      begin
        Write('(int64) : Value should be -128...');
        testretint64inline := -128;
      end;

    function testretrealinline : real; inline;
      begin
        Write('(real) : Value should be 110.110...');
        testretrealinline := 110.110;
      end;

    function testretdoubleinline : double; inline;
      begin
        Write('(double) : Value should be 130.130...');
        testretdoubleinline := 130.130;
      end;


{$ifdef VER1_0}
   function testretbyteregs: byte; saveregisters;
     begin
       Write('(byte) : Value should be 125...');
       testretbyteregs:= 125;
     end;

   function testretwordregs: word; saveregisters;
     begin
       Write('(word) : Value should be 43688...');
       testretwordregs := 43688;
     end;

    function testretint64regs : int64;saveregisters;
      begin
        Write('(int64) : Value should be -130...');
        testretint64regs := -130;
      end;

    function testretrealregs : real; saveregisters;
      begin
        Write('(real) : Value should be -55.55...');
        testretrealregs := -55.55;
      end;

    function testretdoubleregs : double; saveregisters;
      begin
        Write('(double) : Value should be -77.14...');
        testretdoubleregs := -77.14;
      end;
{$endif VER1_0}

   function testretbytecdecl: byte; cdecl;
     begin
       Write('(byte) : Value should be 125...');
       testretbytecdecl:= 125;
     end;

   function testretwordcdecl: word; cdecl;
     begin
       Write('(word) : Value should be 43688...');
       testretwordcdecl := 43688;
     end;

    function testretint64cdecl : int64; cdecl;
      begin
        Write('(int64) : Value should be -130...');
        testretint64cdecl := -130;
      end;

    function testretrealcdecl : real; cdecl;
      begin
        Write('(real) : Value should be -55.55...');
        testretrealcdecl := -55.55;
      end;

    function testretdoublecdecl : double; cdecl;
      begin
        Write('(double) : Value should be -77.14...');
        testretdoublecdecl := -77.14;
      end;

{$endif}


{$endif}

var
 b: byte;
 w: word;
 l: longint;
 r: real;
{$ifdef fpc}
  i: int64;
{$endif}
begin
  WriteLn('------------------------------------------------------');
  WriteLN('           TESTING NON-COMPLEX PARAMETERS             ');
  WriteLn('------------------------------------------------------');
{  testint;}
  { check value parameters }
  Write('(byte value param) : Value should be 85...');
  testvaluebyte($55);
  Write('(word value param) : Value should be 43690...');
  testvalueword($AAAA);
  Write('(long value param) : Value should be -1...');
  testvaluelong(-1);
  { check variable parameters }
  b:=$55;
  w:=$AAAA;
  l:=-1;
  Write('(byte var param) : Value should be 85...');
  testvarbyte(b);
  Write('(word var param) : Value should be 43690...');
  testvarword(w);
  Write('(long var param) : Value should be -1...');
  testvarlong(l);
{$ifdef fpc}
  Write('(int64 value param) : Value should be 43690...');
  testvalueint64($AAAA);
  Write('(int64 var param) : Value should be appx. 187 00000000000...');
  i:= $AAAA;
  i:= i shl 32;
  testvarint64(i);
{$endif}
  writeln('(mixed value params) : Values should 85,43690,-1...');
  testvaluemixedbyte($55,$AAAA,-1);
  writeln('(mixed value params) : Values should be -1, 43690, 85...');
  testvaluemixedlong(-1,$AAAA,$55);
  writeln('(mixed value params): Values should be 0, 127, 254...');
  testvaluemixedbytebyte(0,127,254);
{$ifdef fpc}
  writeln('(mixed value params) : Value should be 0, -1, 254...');
  testvaluemixedint64(0,-1,254);
{$endif}
  write('(real value param) : Value should be 1.1...');
  testvaluereal(1.1);
  write('(single value param) : Value should be 2.2...');
  testvaluesingle(2.2);
  write('(double value param) : Value should be 3.3...');
  testvaluedouble(3.3);
  write('(real var param) : Value should be 7.7...');
  r:=7.7;
  testvarreal(r);
  writeln('(mixed value params) : Values should be 0, 10.7, 254...');
  testvaluemixedreal(0,10.7,254);

  WriteLn('------------------------------------------------------');
  WriteLN('              TESTING FUNCTION RESULTS                ');
  WriteLn('------------------------------------------------------');
  WriteLn('----------------------- NORMAL -----------------------');
  WriteLn(testretbyte);
  WriteLn(testretword);
  WriteLn(testretlong);
  WriteLn(testretstring);
  WriteLn(testretreal);
  WriteLn(testretsingle);
  WriteLn(testretdouble);
  WriteLn(testretpchar);
{$ifdef fpc}
  WriteLn(testretint64);
  WriteLn(testretansi);
  WriteLn('----------------------- INLINE -----------------------');
  WriteLn(testretbyteinline);
  WriteLn(testretwordinline);
  WriteLn(testretint64inline);
  WriteLn(testretrealinline);
  WriteLn(testretdoubleinline);
{$ifdef VER1_0}
  WriteLn('---------------------- SAVEREGS ----------------------');
  WriteLn(testretbyteregs);
  WriteLn(testretwordregs);
  WriteLn(testretint64regs);
  WriteLn(testretrealregs);
  WriteLn(testretdoubleregs);
{$endif VER1_0}
  WriteLn('------------------------ CDECL -----------------------');
  WriteLn(testretbytecdecl);
  WriteLn(testretwordcdecl);
  WriteLn(testretint64cdecl);
  WriteLn(testretrealcdecl);
  WriteLn(testretdoublecdecl);
{$endif}
end.
