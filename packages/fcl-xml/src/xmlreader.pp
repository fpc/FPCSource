{
    This file is part of the Free Component Library

    TXMLReader - base class for streamed XML reading.
    Copyright (c) 2011 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit XmlReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlutils;

type
  TXMLReadState = (
    rsInitial,
    rsInteractive,
    rsError,
    rsEndOfFile,
    rsClosed
  );

  { TODO: move EXmlReadError here from xmlread unit,
    it must have location information available }
  EXmlError = class(Exception) end;


  TXMLReader = class(TObject)
  protected
    FReadState: TXMLReadState;
    FReadStringBuf: TWideCharBuf;
  protected
    function GetEOF: Boolean; virtual;
    function GetNameTable: THashTable; virtual; abstract;
    function GetDepth: Integer; virtual; abstract;
    function GetNodeType: TXMLNodeType; virtual; abstract;
    function GetValue: XMLString; virtual; abstract;
    function GetName: XMLString; virtual; abstract;
    function GetLocalName: XMLString; virtual; abstract;
    function GetPrefix: XMLString; virtual; abstract;
    function GetNamespaceUri: XMLString; virtual; abstract;
    function GetBaseUri: XMLString; virtual; abstract;
    function GetHasValue: Boolean; virtual; abstract;
    function GetAttributeCount: Integer; virtual; abstract;
    function GetIsDefault: Boolean; virtual; abstract;
  public
    destructor Destroy; override;
    function Read: Boolean; virtual; abstract;
    procedure Close; virtual; abstract;
    function MoveToFirstAttribute: Boolean; virtual; abstract;
    function MoveToNextAttribute: Boolean; virtual; abstract;
    function MoveToElement: Boolean; virtual; abstract;
    function ReadAttributeValue: Boolean; virtual; abstract;
    function MoveToContent: TXMLNodeType; virtual;
    procedure ResolveEntity; virtual; abstract;
    function ReadElementString: XMLString; overload;
    function ReadElementString(const aName: XMLString): XMLString; overload;
    function ReadElementString(const aLocalName, aNamespace: XMLString): XMLString; overload;
    procedure ReadEndElement; virtual;
    procedure ReadStartElement; overload;
    procedure ReadStartElement(const aName: XMLString); overload;
    procedure ReadStartElement(const aLocalName, aNamespace: XMLString); overload;
    function ReadString: XMLString; virtual;
    procedure Skip; virtual;
    function LookupNamespace(const APrefix: XMLString): XMLString; virtual; abstract;

    function GetAttribute(i: Integer): XMLString; virtual; abstract;
    function GetAttribute(const Name: XMLString): XMLString; virtual; abstract;
    function GetAttribute(const localName, nsUri: XMLString): XMLString; virtual; abstract;

    property NameTable: THashTable read GetNameTable;
    property nodeType: TXMLNodeType read GetNodeType;
    property ReadState: TXMLReadState read FReadState;
    property Depth: Integer read GetDepth;
    property EOF: Boolean read GetEOF;
    property Name: XMLString read GetName;
    property LocalName: XMLString read GetLocalName;
    property Prefix: XMLString read GetPrefix;
    property namespaceUri: XMLString read GetNamespaceUri;
    property Value: XMLString read GetValue;
    property HasValue: Boolean read GetHasValue;
    property AttributeCount: Integer read GetAttributeCount;
    property BaseUri: XMLString read GetBaseUri;
    property IsDefault: Boolean read GetIsDefault;
  end;

implementation

const
  ContentNodeTypes = [ntText, ntCDATA, ntElement, ntEndElement,
    ntEntityReference, ntEndEntity];

{ TXMLReader }

destructor TXMLReader.Destroy;
begin
  if Assigned(FReadStringBuf.Buffer) then
    FreeMem(FReadStringBuf.Buffer);
  inherited Destroy;
end;

function TXMLReader.GetEOF: Boolean;
begin
  result := (FReadState=rsEndOfFile);
end;

function TXMLReader.MoveToContent: TXMLNodeType;
begin
  if ReadState > rsInteractive then
  begin
    result := ntNone;
    exit;
  end;
  if nodeType = ntAttribute then
    MoveToElement;
  repeat
    result := nodeType;
    if result in ContentNodeTypes then
      exit;
  until not Read;
  result := ntNone;
end;

function TXMLReader.ReadElementString: XMLString;
begin
  ReadStartElement;
  result := ReadString;
  if NodeType <> ntEndElement then
    raise EXmlError.Create('Expecting end of element');
  Read;
end;

function TXMLReader.ReadElementString(const aName: XMLString): XMLString;
begin
  ReadStartElement(aName);
  result := ReadString;
  if NodeType <> ntEndElement then
    raise EXmlError.Create('Expecting end of element');
  Read;
end;

function TXMLReader.ReadElementString(const aLocalName, aNamespace: XMLString): XMLString;
begin
  ReadStartElement(aLocalName, aNamespace);
  result := ReadString;
  if NodeType <> ntEndElement then
    raise EXmlError.Create('Expecting end of element');
  Read;
end;

procedure TXMLReader.ReadEndElement;
begin
  if MoveToContent <> ntEndElement then
    raise EXmlError.Create('Expecting end of element');
  Read;
end;

procedure TXMLReader.ReadStartElement;
begin
  if MoveToContent <> ntElement then
    raise EXmlError.Create('Invalid node type');
  Read;
end;

procedure TXMLReader.ReadStartElement(const aName: XMLString);
begin
  if MoveToContent <> ntElement then
    raise EXmlError.Create('Invalid node type') ;
  if Name <> aName then
    raise EXmlError.CreateFmt('Element ''%s'' was not found',[aName]);
  Read;
end;

procedure TXMLReader.ReadStartElement(const aLocalName, aNamespace: XMLString);
begin
  if MoveToContent <> ntElement then
    raise EXmlError.Create('Invalid node type');
  if (localName <> aLocalName) or (NamespaceURI <> aNamespace) then
    raise EXmlError.CreateFmt('Element ''%s'' with namespace ''%s'' was not found',
      [aLocalName, aNamespace]);
  Read;
end;


function TXMLReader.ReadString: XMLString;
begin
  result := '';
  MoveToElement;
  if FReadStringBuf.Buffer = nil then
    BufAllocate(FReadStringBuf, 512);
  FReadStringBuf.Length := 0;

  if NodeType = ntElement then
    repeat
      Read;
      if NodeType in [ntText, ntCDATA, ntWhitespace, ntSignificantWhitespace] then
        BufAppendString(FReadStringBuf, Value)
      else
        Break;
    until False
  else
    while NodeType in [ntText,ntCDATA,ntWhitespace,ntSignificantWhitespace] do
    begin
      BufAppendString(FReadStringBuf, Value);
      Read;
    end;

  SetString(result, FReadStringBuf.Buffer, FReadStringBuf.Length);
  FReadStringBuf.Length := 0;
end;

procedure TXMLReader.Skip;
var
  i: Integer;
begin
  if ReadState <> rsInteractive then
    exit;
  MoveToElement;
  if (NodeType <> ntElement) then
  begin
    Read;
    exit;
  end;
  i := Depth;
  while Read and (i < Depth) do {loop};
  if NodeType = ntEndElement then
    Read;
end;

end.

