{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

type
  Tbase = 2..36;

var
  exitCode: integer = 0;

procedure Dec2NumbTest(const number: integer;
                       const strlen: byte;
                       const base:   Tbase;
                       const expect: string;
                       const testnr: integer);
  var
    actual: string;
  begin
    actual := Dec2Numb(number, strlen, base);
    if actual <> expect then
    begin
      writeln('Testing strUtils/Dec2Numb: Test ', testnr, ' failed.');
      writeln('Number: ', number, ', base: ', base);
      writeln('Returned String: ', actual);
      writeln('Expected String: ', expect);
      exitCode := 1;
    end;
  end;

const
  codes: array[0..35] of char = ('0','1','2','3','4','5','6','7','8','9',
                                 'A','B','C','D','E','F','G','H','I','J',
                                 'K','L','M','N','O','P','Q','R','S','T',
                                 'U','V','W','X','Y','Z'
                                );

var
  number: integer;
  strlen: byte;
  base: Tbase;
  teststring: string;
  i, j, k, pos: integer;

begin
  i := 1;
  strlen := 10;
  for number := 0 to 1000 do
    for base := low(Tbase) to high(Tbase) do
    begin
      inc(i);
      teststring := '0000000000';
      pos := strlen;
      j := number;
      while j >= base do
      begin
        teststring[pos] := codes[j mod base];
        dec(pos);
        j := j div base;
      end;
      teststring[pos] := codes[j mod base];
      Dec2NumbTest(number, strlen, base, teststring, i);
    end;

  randomize;
  strlen := 20;
  for k := 0 to 1000 do
  begin
    number := random(512*1024);
    for base := low(Tbase) to high(Tbase) do
    begin
      inc(i);
      teststring := '00000000000000000000';
      pos := strlen;
      j := number;
      while j >= base do
      begin
        teststring[pos] := codes[j mod base];
        dec(pos);
        j := j div base;
      end;
      teststring[pos] := codes[j mod base];
      Dec2NumbTest(number, strlen, base, teststring, i);
    end;
  end;

  halt(exitCode);
end.
