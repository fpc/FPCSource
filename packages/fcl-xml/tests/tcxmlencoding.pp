{
    This file is part of the Free Component Library

    Tests for the built-in US-ASCII decoding of the XML reader (issue #29571).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcxmlencoding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM, XMLRead;

type

  { TTestXMLEncoding }

  TTestXMLEncoding = class(TTestCase)
  private
    FData: RawByteString;
    FDoc: TXMLDocument;
    procedure ParseData;
    procedure Parse(const aData: RawByteString);
    function Declaration(const aEncoding: String): RawByteString;
  protected
    procedure TearDown; override;
  published
    procedure TestUSASCII;
    procedure TestUSASCIIAliases;
    procedure TestUSASCIICaseInsensitive;
    procedure TestUSASCIIPrintableRange;
    procedure TestUSASCIIHighByte;
    procedure TestUSASCIIHighByteAfterFirstBuffer;
    procedure TestUSASCIIHighByteInAttribute;
    procedure TestLatin1Unchanged;
    procedure TestUnknownEncoding;
  end;

implementation

const
  USASCIIAliases: array[0..9] of String = (
    'US-ASCII', 'ASCII', 'ANSI_X3.4-1968', 'ANSI_X3.4-1986', 'iso-ir-6',
    'ISO646-US', 'us', 'IBM367', 'cp367', 'csASCII');


procedure TTestXMLEncoding.ParseData;

var
  lStream: TMemoryStream;

begin
  FreeAndNil(FDoc);
  lStream:=TMemoryStream.Create;
  try
    if FData<>'' then
      lStream.WriteBuffer(FData[1],Length(FData));
    lStream.Position:=0;
    ReadXMLFile(FDoc,lStream);
  finally
    lStream.Free;
  end;
end;


procedure TTestXMLEncoding.Parse(const aData: RawByteString);

begin
  FData:=aData;
  ParseData;
end;


function TTestXMLEncoding.Declaration(const aEncoding: String): RawByteString;

begin
  Result:='<?xml version="1.0" encoding="'+aEncoding+'"?>';
end;


procedure TTestXMLEncoding.TearDown;

begin
  FreeAndNil(FDoc);
  inherited TearDown;
end;


procedure TTestXMLEncoding.TestUSASCII;

begin
  Parse(Declaration('us-ascii')+'<a>x</a>');
  AssertEquals('Root element','a',String(FDoc.DocumentElement.NodeName));
  AssertEquals('Text content','x',String(FDoc.DocumentElement.TextContent));
end;


procedure TTestXMLEncoding.TestUSASCIIAliases;

var
  lAlias: String;

begin
  for lAlias in USASCIIAliases do
    begin
    Parse(Declaration(lAlias)+'<a>'+lAlias+'</a>');
    AssertEquals('Text content with encoding '+lAlias,lAlias,String(FDoc.DocumentElement.TextContent));
    end;
end;


procedure TTestXMLEncoding.TestUSASCIICaseInsensitive;

begin
  Parse(Declaration('US-ascii')+'<a>y</a>');
  AssertEquals('Mixed case encoding name','y',String(FDoc.DocumentElement.TextContent));
  Parse(Declaration('ansi_x3.4-1968')+'<a>z</a>');
  AssertEquals('Lower case alias','z',String(FDoc.DocumentElement.TextContent));
end;


procedure TTestXMLEncoding.TestUSASCIIPrintableRange;

var
  lText: RawByteString;
  c: AnsiChar;

begin
  lText:='x';
  for c:=#32 to #126 do
    if not (c in ['<','&']) then
      lText:=lText+c;
  Parse(Declaration('us-ascii')+'<a>'+lText+'</a>');
  AssertEquals('All printable ASCII characters',String(lText),String(FDoc.DocumentElement.TextContent));
end;


procedure TTestXMLEncoding.TestUSASCIIHighByte;

begin
  FData:=Declaration('us-ascii')+'<a>caf'#$E9'</a>';
  AssertException('Byte $E9 in a US-ASCII document',EXMLReadError,@ParseData);
end;


procedure TTestXMLEncoding.TestUSASCIIHighByteAfterFirstBuffer;

begin
  FData:=Declaration('us-ascii')+'<a>'+StringOfChar('x',20000)+'?</a>';
  UniqueString(FData);
  FData[Length(FData)-4]:=AnsiChar($80);
  AssertException('Byte $80 after more than one read buffer',EXMLReadError,@ParseData);
end;


procedure TTestXMLEncoding.TestUSASCIIHighByteInAttribute;

begin
  FData:=Declaration('ASCII')+'<a b="'#$FF'"/>';
  AssertException('Byte $FF in an attribute value',EXMLReadError,@ParseData);
end;


procedure TTestXMLEncoding.TestLatin1Unchanged;

begin
  Parse(Declaration('iso-8859-1')+'<a>caf'#$E9'</a>');
  AssertEquals('Latin-1 byte $E9',String(UTF8Encode(UnicodeString('caf'#$00E9))),
    String(UTF8Encode(FDoc.DocumentElement.TextContent)));
end;


procedure TTestXMLEncoding.TestUnknownEncoding;

begin
  FData:=Declaration('x-no-such-encoding')+'<a>x</a>';
  AssertException('Unknown encoding',EXMLReadError,@ParseData);
end;


initialization
  RegisterTest(TTestXMLEncoding);
end.
