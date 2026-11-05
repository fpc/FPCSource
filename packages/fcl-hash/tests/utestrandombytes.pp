{ Tests the system random byte source of fphashutils. }
unit utestrandombytes;

{$mode ObjFPC}{$H+}

interface

uses
  FPCUnit, TestRegistry, Classes, SysUtils, fphashutils;

type

  { TTestRandomBytes }

  TTestRandomBytes = class(TTestCase)
  private
    procedure CheckOSAvailable;
  Published
    Procedure TestDefaultIsOS;
    Procedure TestFillsBuffer;
    Procedure TestTwoCallsDiffer;
    Procedure TestIndependentOfRandSeed;
    Procedure TestZeroCount;
    Procedure TestLargeBuffer;
    Procedure TestCryptoNoZeroBytes;
    Procedure TestCryptoDiffers;
  end;

implementation

const
  cSize = 64;


// Number of bytes in aBuf that are zero
function CountZero(const aBuf: TBytes): Integer;

var
  I: Integer;

begin
  Result:=0;
  for I:=0 to Length(aBuf)-1 do
    if aBuf[I]=0 then
      Inc(Result);
end;


// Return aCount bytes from OSGetRandomBytes
function OSBytes(aCount: Integer): TBytes;

begin
  Result:=[];
  SetLength(Result, aCount);
  if not OSGetRandomBytes(PByte(Result), aCount) then
    Result:=[];
end;


procedure TTestRandomBytes.CheckOSAvailable;

begin
  if not OSRandomBytesAvailable then
    Ignore('no system random source on this target');
end;


procedure TTestRandomBytes.TestDefaultIsOS;

begin
  CheckOSAvailable;
  AssertTrue('GetRandomBytes defaults to OSGetRandomBytes', Pointer(GetRandomBytes)=Pointer(@OSGetRandomBytes));
end;


procedure TTestRandomBytes.TestFillsBuffer;

var
  lBuf: TBytes;

begin
  CheckOSAvailable;
  lBuf:=OSBytes(cSize);
  AssertEquals('OSGetRandomBytes succeeds', cSize, Length(lBuf));
  AssertTrue('the buffer is not left all zero', CountZero(lBuf)<cSize);
end;


procedure TTestRandomBytes.TestTwoCallsDiffer;

var
  lFirst, lSecond: TBytes;

begin
  CheckOSAvailable;
  lFirst:=OSBytes(cSize);
  lSecond:=OSBytes(cSize);
  AssertEquals('first call succeeds', cSize, Length(lFirst));
  AssertEquals('second call succeeds', cSize, Length(lSecond));
  AssertFalse('two calls give different bytes', CompareMem(@lFirst[0], @lSecond[0], cSize));
end;


procedure TTestRandomBytes.TestIndependentOfRandSeed;

var
  lFirst, lSecond: TBytes;

begin
  CheckOSAvailable;
  RandSeed:=42;
  lFirst:=[];
  SetLength(lFirst, cSize);
  AssertTrue('GetRandomBytes succeeds', GetRandomBytes(PByte(lFirst), cSize));
  RandSeed:=42;
  lSecond:=[];
  SetLength(lSecond, cSize);
  AssertTrue('GetRandomBytes succeeds again', GetRandomBytes(PByte(lSecond), cSize));
  AssertFalse('the bytes do not come from the RTL Random', CompareMem(@lFirst[0], @lSecond[0], cSize));
end;


procedure TTestRandomBytes.TestZeroCount;

var
  lByte: Byte;

begin
  CheckOSAvailable;
  lByte:=7;
  AssertTrue('a count of 0 succeeds', OSGetRandomBytes(@lByte, 0));
  AssertEquals('a count of 0 writes nothing', 7, lByte);
end;


procedure TTestRandomBytes.TestLargeBuffer;

const
  cLarge = 1024*1024;

var
  lBuf: TBytes;

begin
  CheckOSAvailable;
  lBuf:=OSBytes(cLarge);
  AssertEquals('1 MiB is filled in one call', cLarge, Length(lBuf));
  AssertTrue('the last 64 bytes are filled', CountZero(Copy(lBuf, cLarge-cSize, cSize))<cSize);
end;


procedure TTestRandomBytes.TestCryptoNoZeroBytes;

var
  lBuf: TBytes;

begin
  lBuf:=[];
  SetLength(lBuf, 4096);
  AssertTrue('CryptoGetRandomBytes succeeds', CryptoGetRandomBytes(PByte(lBuf), Length(lBuf), False));
  AssertEquals('no zero bytes when they are not allowed', 0, CountZero(lBuf));
end;


procedure TTestRandomBytes.TestCryptoDiffers;

var
  lFirst, lSecond: TBytes;

begin
  CheckOSAvailable;
  lFirst:=[];
  SetLength(lFirst, cSize);
  lSecond:=[];
  SetLength(lSecond, cSize);
  RandSeed:=42;
  AssertTrue('CryptoGetRandomBytes succeeds', CryptoGetRandomBytes(PByte(lFirst), cSize));
  RandSeed:=42;
  AssertTrue('CryptoGetRandomBytes succeeds again', CryptoGetRandomBytes(PByte(lSecond), cSize));
  AssertFalse('CryptoGetRandomBytes does not repeat after the same RandSeed', CompareMem(@lFirst[0], @lSecond[0], cSize));
end;


initialization
  RegisterTest(TTestRandomBytes);
end.
