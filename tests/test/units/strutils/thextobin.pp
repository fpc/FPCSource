{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  SysUtils,
  StrUtils;

var
  exitCode: integer = 0;

Function Memblock(Size : Integer) : PChar;

begin
  Result:=getmem(Size);
  fillchar(Result^,Size,0);
end;


procedure HexToBinTest(const hexValue: Pchar;
                       const binBufSize: integer;
                       const expectation: Pchar;
                       const testnr: integer);
  var
    binValue: Pchar;
    l : Integer;
  begin
    L:=(binBufSize - 1 )  div 2 + 1;
    binValue := memblock(L);
    HexToBin(hexValue, binValue,L-1);
    if strcomp(binValue, expectation) <> 0 then
    begin
      writeln('Testing strUtils/HexToBin: Test ', testnr, ' failed.');
      writeln(binBufSize);
      writeln(strlen(hexValue));
      writeln(strlen(expectation));
      writeln(strlen(binValue));
      write('Hex Input: ', hexValue);
      writeln('Returned list: ', byte(binValue[0]));
      writeln('Expected list: ', byte(expectation[0]));
      exitCode := 1;
    end;
    FreeMem(binvalue);
  end; 

const
  maxLen = 512;
  codes: array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  
var
  i, j, binBufSize, value: integer;
  testbin: Pchar;
  testhex: Pchar;

begin
  binBufSize := 3;
  testhex := memblock(binBufSize);
  testbin := memblock(2);
  for i := 0 to 17 do
  begin
    testbin[0] := char(i);
    testhex[0] := codes[i div 16];
    testhex[1] := codes[i mod 16];
    HexToBinTest(testhex, binbufsize, testbin, 1 + i);
  end;
  FreeMem(testbin);
  FreeMem(testhex);
  randomize;
  for i := 1 to 1000 do
  begin
    binBufSize := 1 + random(maxLen);
    binBufSize := binBufSize * 2;
    testbin := memblock(binBufSize + 1);
    testhex := memblock(binBufSize * 2 + 1);
    for j := 0 to binBufSize - 1 do
    begin
      value := random(256);
      testbin[j]       := char(value);
      testhex[2*j]     := codes[value div 16];
      testhex[2*j + 1] := codes[value mod 16];
    end;
    HexToBinTest(testhex, binBufSize * 2 + 1, testbin, 255 + i);
    FreeMem(testbin);
    FreeMem(testhex);
  end;

  halt(exitCode);
end.
