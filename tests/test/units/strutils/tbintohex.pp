{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  Strings,
  StrUtils;

var
  exitCode: integer = 0;

Function Memblock(Size : Integer) : PChar;

begin
  Result:=getmem(Size);
  fillchar(Result^,Size,0);
end;

procedure BinToHexTest(const binValue: Pchar;
                       const binBufSize: integer;
                       const expectation: Pchar;
                       const testnr: integer);
  var
    hexValue: Pchar;
  begin
    hexValue := memblock(2*binBufSize + 1);
    BinToHex(binValue, hexValue, binBufSize);
    if strcomp(hexValue, expectation) <> 0 then
    begin
      writeln('Testing strUtils/BinToHex: Test ', testnr, ' failed.');
      writeln('Returned String: ', hexValue);
      writeln('Expected String: ', expectation);
      exitCode := 1;
    end;
    FreeMem(hexValue);
  end; 

const
  maxLen = 512;
  codes: array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  
var
  i, j, binBufSize, value: integer;
  testbin: Pchar;
  testhex: Pchar;

begin
  binBufSize := 1;
  testbin := memblock(binBufSize + 1);
  testhex := memblock(2*binBufSize + 1);
  for i := 0 to 255 do
  begin
    testbin[0] := char(i);
    testhex[0] := codes[i div 16];
    testhex[1] := codes[i mod 16];
     BinToHexTest(testbin, binBufSize, testhex, 1 + i);
  end;
  FreeMem(TestBin);
  FreeMem(TestHex);
  randomize;
  for i := 1 to 1000 do
  begin
    binBufSize := 1 + random(maxLen);
    testbin := memblock(binBufSize + 1);
    testhex := memblock(2*binBufSize + 1);
    for j := 0 to binBufSize - 1 do
    begin
      value := random(256);
      testbin[j]       := char(value);
      testhex[2*j]     := codes[value div 16];
      testhex[2*j + 1] := codes[value mod 16];
    end;
    BinToHexTest(testbin, binBufSize, testhex, 255 + i);
    FreeMem(TestBin);
    FreeMem(TestHex);
  end;
 
  halt(exitCode);
end.
