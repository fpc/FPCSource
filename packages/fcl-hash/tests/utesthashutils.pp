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


initialization
  RegisterTest(TTestHashUtils);
end.
