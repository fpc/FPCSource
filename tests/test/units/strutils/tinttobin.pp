{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

var
  exitCode: integer = 0;

procedure IntToBinTest(const testinteger: integer;
                       const digits: integer;
                       const expectation: string;
                       const testnr: integer);
  var
    teststring: string;
  begin
    teststring := IntToBin(testinteger, digits);
    if teststring <> expectation then
    begin
      writeln('Testing strUtils/IntToBin: Test ', testnr, ' failed with number ', testinteger);
      writeln('Returned String: ', teststring);
      writeln('Expected String: ', expectation);
      exitCode := 1;
    end;
  end;
  
const
  codes: array[0..1] of char = ('0','1');

var
  i, j, value: integer;
  testinteger: integer;
  teststring: string;
  digits: integer;

begin
  digits := 32;
  setlength(teststring, digits);

  for testinteger := 0 to $7FFF do
  begin
    value := testinteger;
    for j :=  digits downto 1 do
    begin
      teststring[j] := codes[value mod 2];
      value := value div 2;
    end;
    IntToBinTest(testinteger, digits, teststring, 1 + testinteger);
  end;

  for testinteger := -$8000 to -$1 do
  begin
    value := -testinteger - 1; { prepare for 2's complement -1 }
    teststring[1] := '1';      { sign bit }
    teststring[digits] := codes[1 - (value mod 2)]; { inversion of 0 and 1}
    value := value div 2;
    for j :=  digits - 1 downto 2 do
    begin
      teststring[j] := codes[-(value mod 2) + 1];
      value := value div 2;
    end;
    IntToBinTest(testinteger, digits, teststring, $10000 + testinteger);
  end;

{$IF DECLARED(longint)}
  randomize;
  for i := 1 to 1000 do
  begin
    testinteger := $7FFF + random($80000000 - $7FFF);
    value := testinteger;
    for j :=  digits downto 1 do
    begin
      teststring[j] := codes[value mod 2];
      value := value div 2;
    end;
    IntToBinTest(testinteger, digits, teststring, $10000 + i);
  end;

  for i := 1 to 1000 do
  begin
    testinteger := -$8000 - random($80000000 - $8000);
    value := -testinteger - 1; { prepare for 2's complement -1 }
    teststring[1] := '1';      { sign bit }
    teststring[digits] := codes[1 - (value mod 2)]; { inversion of 0 and 1}
    value := value div 2;
    for j :=  digits - 1 downto 2 do
    begin
      teststring[j] := codes[-(value mod 2) + 1];
      value := value div 2;
    end;
    IntToBinTest(testinteger, digits, teststring, $10000 + 1000 + i);
  end;
{$IFEND}

  halt(exitCode);
end.
