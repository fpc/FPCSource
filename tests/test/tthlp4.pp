{ this tests that the correct helper is used for constants }

program tthlp4;

{$mode objfpc}
{$apptype console}

uses
  uthlp;

procedure TestResult(aActual, aExpected, aError: LongInt);
begin
  if aActual <> aExpected then begin
    Writeln('Expected: ', aExpected, ' got: ', aActual, ' error: ', aError);
    Halt(aError);
  end;
end;

var
  ml: MyLongInt;
begin
  Writeln('Ordinal constants');
  TestResult(2.Test, -1, 1);
  TestResult(-2.Test, -1, 2);
  TestResult(200.Test, 1, 3);
  TestResult(-200.Test, -2, 4);
  TestResult(40000.Test, 2, 5);
  TestResult(-20000.Test, -2, 6);
  TestResult(-40000.Test, -4, 7);
  TestResult(70000.Test, -4, 8);
  TestResult(3000000000.Test, 4, 9);
  TestResult($1ffffffff.Test, -8, 10);
  TestResult($1fffffffffffffff.Test, -8, 11);
  Writeln('Float constants');
  TestResult(1.25.Test, 4, 12);
{$if sizeof(Extended) = sizeof(Double)}
  TestResult(1.25e10.Test, 8, 14);
{$else}
  TestResult(1.25e10.Test, 10, 14);
{$endif}
  Writeln('Boolean constants');
  TestResult(True.Test, 1, 15);
  TestResult(False.Test, 1, 16);
  Writeln('String constants');
  TestResult('ShortString'.Test, 1, 17);
  TestResult('UnicodeString'#1234.Test, 4, 18);
  Writeln('Misc constants');
  TestResult(Nil.Test, 1, 19);
  TestResult(teOne.Test, 1, 20);
  TestResult('a'.Test, - 1, 21);
  TestResult(#1234.Test, - 2, 22);
{$push}
{$T-}
  // => Pointer
  TestResult((@ml).Test, 1, 23);
{$T+}
  // => Pointer as well
  TestResult((@ml).Test, 1, 24);
{$pop}
  Writeln('OK');
end.
