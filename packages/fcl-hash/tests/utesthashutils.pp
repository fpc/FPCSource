unit utesthashutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fphashutils, fpsha256, fpsha512, fptlsbigint;

type

  { TTestHashUtils }

  TTestHashUtils = class(TTestCase)
  published
    procedure TestHexStrToBytesAnsiString;
    procedure TestHexStrToBytesUnicodeString;
    procedure TestHexStrToString;
    procedure TestBytesToVarPointer;
    procedure TestHexStrToBytesInvalid;
    procedure TestFallbackGeneratorSeed;
    procedure TestConstantTimeEqual;
    procedure TestDigestIsEqual;
    procedure TestBytesToVarSize;
    procedure TestStreamSeveralReads;
    procedure TestBigIntImportEmpty;
  end;

implementation

procedure TTestHashUtils.TestHexStrToBytesAnsiString;
var
  lBytes: TBytes;
begin
  HexStrToBytes(AnsiString('0A1bFF00'),lBytes);
  AssertEquals('Length',4,Length(lBytes));
  AssertEquals('Hex','0A1BFF00',UpperCase(BytesToHexStr(lBytes)));
end;


procedure TTestHashUtils.TestHexStrToBytesUnicodeString;
var
  lBytes: TBytes;
begin
  HexStrToBytes(UnicodeString('0A1bFF00'),lBytes);
  AssertEquals('Length',4,Length(lBytes));
  AssertEquals('Hex','0A1BFF00',UpperCase(BytesToHexStr(lBytes)));
  HexStrToBytes(UnicodeString(''),lBytes);
  AssertEquals('Empty string',0,Length(lBytes));
end;


procedure TTestHashUtils.TestHexStrToString;
begin
  AssertEquals('HexStrToString','AB'#0#255,HexStrToString('4142' + '00FF'));
end;


procedure TTestHashUtils.TestBytesToVarPointer;
var
  lBuffer: array[0..33] of Byte;
  lData: TBytes;
  I: Integer;
begin
  FillChar(lBuffer,SizeOf(lBuffer),$EE);
  lData:=[];
  SetLength(lData,32);
  for I:=0 to 31 do
    lData[I]:=I+1;
  BytesToVar(lData,Pointer(@lBuffer[1]));
  AssertEquals('Byte before the destination unchanged',$EE,lBuffer[0]);
  for I:=0 to 31 do
    AssertEquals('Byte '+IntToStr(I)+' copied',I+1,lBuffer[I+1]);
  AssertEquals('Byte after the destination unchanged',$EE,lBuffer[33]);
  BytesToVar(nil,Pointer(@lBuffer[1]));
  AssertEquals('Empty array copies nothing',1,lBuffer[1]);
end;


procedure TTestHashUtils.TestHexStrToBytesInvalid;

  // True when HexStrToBytes raises EHashUtil for aHex
  function Raises(const aHex: AnsiString): Boolean;
  var
    lBytes: TBytes;
  begin
    Result:=False;
    try
      HexStrToBytes(aHex,lBytes);
    except
      on EHashUtil do
        Result:=True;
    end;
  end;

begin
  AssertTrue('ZZ',Raises('ZZ'));
  AssertTrue('::',Raises('::'));
  AssertTrue('gg',Raises('gg'));
  AssertTrue('~~',Raises('~~'));
  AssertTrue('Two spaces',Raises('  '));
  AssertTrue('Odd length',Raises('abc'));
  AssertTrue('Invalid character after valid ones',Raises('00112G'));
  AssertFalse('Valid hex',Raises('00aAfF99'));
  AssertFalse('Empty string',Raises(''));
end;


var
  // State of SeedOnlyRandomBytes
  SeedCalls: Integer;
  SeedSize: Integer;
  SeedLastByte: Byte;

// Random source that fails the first request and fills later ones with 1, 2, 3, ... and SeedLastByte at the end
function SeedOnlyRandomBytes(aBytes: PByte; aCount: Integer): Boolean;
var
  I: Integer;
begin
  Inc(SeedCalls);
  Result:=SeedCalls>1;
  if not Result then
    Exit;
  SeedSize:=aCount;
  for I:=0 to aCount-1 do
    aBytes[I]:=I+1;
  aBytes[aCount-1]:=SeedLastByte;
end;


procedure TTestHashUtils.TestFallbackGeneratorSeed;

  // 64 bytes from CryptoGetRandomBytes when the fallback generator gets a seed ending in aLastByte
  function FallbackBytes(aLastByte: Byte): TBytes;
  begin
    Result:=[];
    SetLength(Result,64);
    SeedCalls:=0;
    SeedSize:=0;
    SeedLastByte:=aLastByte;
    AssertTrue('Fallback generator used',CryptoGetRandomBytes(PByte(Result),Length(Result)));
  end;

var
  lSaved: TGetRandomBytes;
  lFirst, lSecond: TBytes;
begin
  lSaved:=GetRandomBytes;
  try
    GetRandomBytes:=@SeedOnlyRandomBytes;
    lFirst:=FallbackBytes(1);
    AssertEquals('Seed size',12,SeedSize);
    lSecond:=FallbackBytes(2);
    AssertFalse('The last seed byte changes the output',CompareMem(@lFirst[0],@lSecond[0],Length(lFirst)));
  finally
    GetRandomBytes:=lSaved;
  end;
end;


procedure TTestHashUtils.TestConstantTimeEqual;
var
  A, B: array[0..15] of Byte;
begin
  FillChar(A,SizeOf(A),7);
  FillChar(B,SizeOf(B),7);
  AssertTrue('Equal',ConstantTimeEqual(@A,@B,SizeOf(A)));
  B[0]:=8;
  AssertFalse('First byte differs',ConstantTimeEqual(@A,@B,SizeOf(A)));
  B[0]:=7;
  B[15]:=6;
  AssertFalse('Last byte differs',ConstantTimeEqual(@A,@B,SizeOf(A)));
  AssertTrue('Only the first 15 bytes compared',ConstantTimeEqual(@A,@B,15));
  AssertTrue('Zero bytes',ConstantTimeEqual(@A,@B,0));
end;


procedure TTestHashUtils.TestDigestIsEqual;
var
  lSHA256: TSHA256;
  lDigest256: TSHA256Digest;
  lSHA512: TSHA512;
  lDigest512: TSHA512Digest;
begin
  lSHA256.Init;
  lSHA256.Update(TBytes([1,2,3]));
  lSHA256.Final;
  lDigest256:=lSHA256.Digest;
  AssertTrue('SHA-256 same digest',lSHA256.IsEqual(lDigest256));
  lDigest256[High(lDigest256)]:=lDigest256[High(lDigest256)] xor 1;
  AssertFalse('SHA-256 last byte differs',lSHA256.IsEqual(lDigest256));
  lSHA512.Init;
  lSHA512.Update(TBytes([1,2,3]));
  lSHA512.Final;
  lDigest512:=lSHA512.Digest;
  AssertTrue('SHA-512 same digest',lSHA512.IsEqual(lDigest512));
  lDigest512[0]:=lDigest512[0] xor 1;
  AssertFalse('SHA-512 first byte differs',lSHA512.IsEqual(lDigest512));
end;


procedure TTestHashUtils.TestBytesToVarSize;
var
  lBuffer: array[0..9] of Byte;
  I: Integer;
begin
  FillChar(lBuffer,SizeOf(lBuffer),$EE);
  BytesToVar(TBytes([1,2,3]),lBuffer,8);
  AssertEquals('Byte 0',1,lBuffer[0]);
  AssertEquals('Byte 2',3,lBuffer[2]);
  for I:=3 to 7 do
    AssertEquals('Byte '+IntToStr(I)+' set to zero',0,lBuffer[I]);
  AssertEquals('Byte after aSize unchanged',$EE,lBuffer[8]);
  FillChar(lBuffer,SizeOf(lBuffer),$EE);
  BytesToVar(TBytes([1,2,3,4,5]),lBuffer,3);
  AssertEquals('Only aSize bytes copied',3,lBuffer[2]);
  AssertEquals('Byte after aSize unchanged with longer input',$EE,lBuffer[3]);
  FillChar(lBuffer,SizeOf(lBuffer),$EE);
  BytesToVar(nil,lBuffer,4);
  AssertEquals('Empty input sets aSize bytes to zero',0,lBuffer[3]);
  AssertEquals('Byte after aSize unchanged with empty input',$EE,lBuffer[4]);
  BytesToVar(TBytes([1]),lBuffer,0);
  AssertEquals('Size 0 changes nothing',0,lBuffer[0]);
end;


procedure TTestHashUtils.TestStreamSeveralReads;
var
  lData: TBytes;
  lStream: TBytesStream;
  I: Integer;
  lHexa: AnsiString;
begin
  lData:=[];
  SetLength(lData,200000);
  for I:=0 to Length(lData)-1 do
    lData[I]:=(I*7+I shr 8) and $FF;
  lStream:=TBytesStream.Create(lData);
  try
    TSHA256.DigestHexa(lData,lHexa);
    lStream.Position:=0;
    AssertEquals('SHA-256',lHexa,TSHA256.StreamHexa(lStream));
    TSHA224.DigestHexa(lData,lHexa);
    lStream.Position:=0;
    AssertEquals('SHA-224',lHexa,TSHA224.StreamHexa(lStream));
    TSHA512.DigestHexa(lData,lHexa);
    lStream.Position:=0;
    AssertEquals('SHA-512',lHexa,TSHA512.StreamHexa(lStream));
    TSHA384.DigestHexa(lData,lHexa);
    lStream.Position:=0;
    AssertEquals('SHA-384',lHexa,TSHA384.StreamHexa(lStream));
  finally
    lStream.Free;
  end;
end;


procedure TTestHashUtils.TestBigIntImportEmpty;
var
  lContext: TBigIntContext;
  lBI: PBigInt;
  lOut: array[0..3] of Byte;
begin
  BIInitialize(lContext);
  try
    lBI:=BIImport(lContext,TBytes(nil));
    AssertEquals('Empty TBytes: one component',1,lBI^.Size);
    AssertEquals('Empty TBytes: value 0',0,BIBitCount(lBI));
    FillChar(lOut,SizeOf(lOut),$EE);
    BIExport(lContext,lBI,@lOut,SizeOf(lOut));
    AssertEquals('Empty TBytes exported as zero',0,lOut[0] or lOut[1] or lOut[2] or lOut[3]);
    lBI:=BIImport(lContext,AnsiString(''));
    AssertEquals('Empty AnsiString: one component',1,lBI^.Size);
    AssertEquals('Empty AnsiString: value 0',0,BIBitCount(lBI));
    BIRelease(lContext,lBI);
  finally
    BITerminate(lContext);
  end;
end;


initialization
  RegisterTest(TTestHashUtils);
end.
