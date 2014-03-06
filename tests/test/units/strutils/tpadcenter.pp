{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

var
  exitCode: integer = 0;

procedure padCenterTest(const s: ansistring;
                        n: integer;
                        const expectation: ansistring;
                        const testnr: integer);

  begin
    if padCenter(s, n) <> expectation then
    begin
      writeln('Testing strUtils/PadCenter: Test ', testnr, 
              ' with N = ', n, ' failed.');
      exitCode := 1;
    end;
  end; 

var
  i, j: integer;
  testString: ansistring;

begin
  for i := 1 to 1024 do
  begin
    testString := 'abcd';
    for j := 1 to i - 4 do
      if odd(j) then
        testString := testString + ' '
      else
        testString := ' ' + testString;
    padCenterTest('abcd', i, testString, i);
  end;
  
  halt(exitCode);
end.
