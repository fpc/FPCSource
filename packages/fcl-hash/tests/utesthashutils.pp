unit utesthashutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fphashutils;

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


initialization
  RegisterTest(TTestHashUtils);
end.
