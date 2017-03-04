{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

var
  exitCode: integer = 0;

procedure RomanToIntTest(const testRoman: string;
                         const expectation: integer);
  var
    test: integer;
  begin
    test := RomanToInt(testRoman);
    if test <> expectation then
    begin
      writeln('Testing strUtils/RomanToInt: Test with ', testRoman, ' failed.');
      writeln('Returned number: ', test);
      writeln('Expected number: ', expectation);
      exitCode := 1;
    end;
  end; 

var
  i: integer;
  testRoman: string;
  testInteger: integer;

begin
  for i := 1 to 2000 do
  begin
    testInteger := i;
    testRoman := intToRoman(testInteger);
    RomanToIntTest(testRoman, testInteger);
  end;

  randomize;
  for i := 1 to 1000 do
  begin
    testInteger := random(1000000);
    testRoman := intToRoman(testInteger);
    RomanToIntTest(testRoman, testInteger);
  end;
 
  halt(exitCode);
end.
