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


initialization
  RegisterTest(TTestHashUtils);
end.
