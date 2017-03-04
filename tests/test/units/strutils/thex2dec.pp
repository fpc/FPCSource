{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

var
  exitCode: integer = 0;

procedure Hex2DecTest(const testhex: string;
                      const testdec: integer;
                      const testnr: integer);
  var
    tempdec: integer;
  begin
    tempdec := Hex2Dec(testhex);
    if tempdec <> testdec then
    begin
      writeln('Testing strUtils/Hex2Dec: Test ', testnr, ' with string ', testhex, ' failed.');
      writeln('Returned number: ', tempdec);
      writeln('Expected number: ', testdec);
      exitCode := 1;
    end;
  end;

const
{$IF DECLARED(longint)}
  maxLen = 8;  { The maximum number of hex digits for longint (32 bit) }
{$ELSE}
  maxLen = 4;  { The maximum number of hex digits for smallint (16 bit) }
{$IFEND}
  codes: array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

var
  i, j, length, digit: integer;
  testdec: integer;
  testhex: string;

begin
  for i := 0 to 15 do
  begin
    testhex := codes[i];
    testdec := i;
    Hex2DecTest(testhex, testdec, 1 + i);
    Hex2DecTest('$' + testhex, testdec, 1 + i);
  end;

  randomize;
  for i := 1 to 1000 do
  begin
    length := 2 + random(maxLen - 1);
    setlength(testhex, length);
    if length = maxLen then
      digit := random(8)  { The high byte can only go up to 7, because of ths sign bit }
    else
      digit := random(16);
    testhex[1] := codes[digit];
    testdec := digit;
    for j := 2 to length do
    begin
      digit := random(16);
      testhex[j] := codes[digit];
      testdec := testdec * 16 + digit;
    end;

    Hex2DecTest(testhex, testdec, 16 + i);
    Hex2DecTest('$' + testhex, testdec, 16 + i);
  end;

  halt(exitCode);
end.
