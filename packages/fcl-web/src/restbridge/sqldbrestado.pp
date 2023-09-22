{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST bridge : ADO-styled XML input/output

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestado;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.DateUtils, Data.Db, FpJson.Data, Xml.Dom, Xml.Read, 
  Xml.Writer, FpWeb.RestBridge.Schema, FpWeb.RestBridge.IO, FpWeb.RestBridge.Bridge;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, DateUtils, db,fpjson, dom, XMLRead, XMLWrite,sqldbrestschema,sqldbrestio, sqldbrestbridge;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TADOInputStreamer }

  TADOInputStreamer = Class(TRestInputStreamer)
  private
    FDataName: UTF8String;
    FRowName: UTF8String;
    FXML: TXMLDocument;
    FPacket : TDOMElement;
    FData : TDOMElement; // Equals FPacket
    FRow : TDOMElement;
  Protected
    function GetNodeText(N: TDOmNode): UnicodeString;
  Public
    Destructor Destroy; override;
    Class Function GetContentType: String; override;
    Function SelectObject(aIndex : Integer) : Boolean; override;
    function GetContentField(aName: UTF8string): TJSONData; override;
    procedure InitStreaming; override;
    Property XML : TXMLDocument Read FXML;
    Property Packet : TDOMElement Read FPacket;
    Property Data : TDOMElement Read FData;
    Property Row : TDOMElement Read FRow;
    Property DataName : UTF8String Read FDataName Write FDataName;
    Property RowName : UTF8String Read FRowName Write FRowName;
  end;

  { TADOOutputStreamer }

  TADOOutputStreamer = Class(TRestOutputStreamer)
  Private
    FDataName: UTF8String;
    FRowName: UTF8String;
    FXML: TXMLDocument;
    FData : TDOMElement; // Equals FRoot
    FRow: TDOMElement;
    FRoot: TDomElement;
    function CreateXSD: TDomElement;
  Public
    procedure EndData; override;
    procedure EndRow; override;
    procedure FinalizeOutput; override;
    procedure StartData; override;
    procedure StartRow; override;
    // Return Nil for null field.
    function FieldToXML(aPair: TRestFieldPair): TDOMElement; virtual;
    procedure WriteField(aPair: TRestFieldPair); override;
    procedure WriteMetadata(aFieldList: TRestFieldPairArray); override;
    Procedure CreateErrorContent(aCode : Integer; Const aMessage: String); override;
    Property XML : TXMLDocument Read FXML;
    Property Data : TDOMelement Read FData;
    Property Row : TDOMelement Read FRow;
  Public
    Destructor Destroy; override;
    Class Function GetContentType: String; override;
    Class function FileExtension : string; override;
    function RequireMetadata : Boolean; override;
   
    procedure InitStreaming; override;
    Property DataName : UTF8String Read FDataName Write FDataName;
    Property RowName : UTF8String Read FRowName Write FRowName;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}

{ TADOInputStreamer }

destructor TADOInputStreamer.Destroy;
begin
  FreeAndNil(FXML);
  inherited Destroy;
end;

class function TADOInputStreamer.GetContentType: String;
begin
  Result:='text/xml';
end;

function TADOInputStreamer.SelectObject(aIndex: Integer): Boolean;

Var
  N : TDomNode;
  NN : UnicodeString;
begin
  Result:=False;
  NN:=UTF8Decode(RowName);
  N:=FData.FindNode(NN);
  While (aIndex>0) and (N<>Nil) and (N.NodeName<>NN) and (N.NodeType<>ELEMENT_NODE) do
    begin
    N:=N.NextSibling;
    Dec(aIndex);
    end;
  Result:=(aIndex=0) and (N<>Nil);
  If Result then
    FRow:=N as TDomElement
  else
    FRow:=Nil;
end;

function TADOInputStreamer.GetNodeText(N: TDOmNode): UnicodeString;

Var
  V : TDomNode;

begin
  Result:='';
  V:=N.FirstChild;
  While (V<>Nil) and (V.NodeType<>TEXT_NODE) do
    V:=V.NextSibling;
  If Assigned(V) then
    Result:=V.NodeValue;
end;

function TADOInputStreamer.GetContentField(aName: UTF8string): TJSONData;

Var
  NN : UnicodeString;
  N : TDomNode;
begin
  NN:=UTF8Decode(aName);
  N:=FRow.FindNode(NN);
  if Assigned(N) and (N.NodeType=ELEMENT_NODE) then
    Result:=TJSONString.Create(UTF8Encode(GetNodeText(N)));
end;

procedure TADOInputStreamer.InitStreaming;

Var
  Msg : String;
  NN : UnicodeString;

begin
  if DataName='' then
    DataName:='Data';
  if RowName='' then
    RowName:='Row';
  FreeAndNil(FXML);
  if Stream.Size<=0 then
    exit;
  try
    ReadXMLFile(FXML,Stream);
  except
    On E : Exception do
      begin
      Msg:=E.Message;
      FXML:=Nil;
      end;
  end;
  if (FXML=Nil)  then
    Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsInvalidContent),SErrInvalidXMLInput,[Msg]);
  FPacket:=FXML.DocumentElement;
  NN:=UTF8Decode(DataName);
  if FPacket.NodeName<>NN then
    Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsInvalidContent),SErrInvalidXMLInput,[SErrMissingDocumentRoot]);
  FData:=FPacket;
end;

{ TADOOutputStreamer }


procedure TADOOutputStreamer.EndData;
begin
  FData:=Nil;
end;

procedure TADOOutputStreamer.EndRow;
begin
  FRow:=Nil;
end;

procedure TADOOutputStreamer.FinalizeOutput;

begin
{$IFNDEF VER3_0}
  if Not (ooHumanReadable in OutputOptions) then
    begin
    With TDOMWriter.Create(Stream,FXML) do
      try
        LineBreak:='';
        IndentSize:=0;
        WriteNode(FXML);
      finally
        Free;
      end;
    end
  else
{$ENDIF}
  WriteXML(FXML,Stream);
  FreeAndNil(FXML);
end;

procedure TADOOutputStreamer.StartData;
begin
  // Rows are straight under the Data packet
  FData:=FRoot;
end;

procedure TADOOutputStreamer.StartRow;
begin
  if (FRow<>Nil) then
    Raise ESQLDBRest.Create(Statuses.GetStatusCode(rsError),SErrDoubleRowStart);
  FRow:=FXML.CreateElement(UTF8Decode(RowName));
  FData.AppendChild(FRow);
end;

function TADOOutputStreamer.FieldToXML(aPair: TRestFieldPair): TDOMElement;

Var
  F : TField;
  S : UTF8String;

begin
  Result:=Nil;
  F:=aPair.DBField;;
  If (aPair.RestField.FieldType=rftUnknown) then
    raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsError),SErrUnsupportedRestFieldType, [aPair.RestField.PublicName]);
  If (F.IsNull) then
    Exit;
  S:=FieldToString(aPair.RestField.FieldType,F);
  Result:=FXML.CreateElement(UTF8Decode(aPair.RestField.PublicName));
  Result.AppendChild(FXML.CreateTextNode(UTF8Decode(S)));
end;

procedure TADOOutputStreamer.WriteField(aPair: TRestFieldPair);

Var
  D : TDOMElement;
  N : UTF8String;

begin
  N:=aPair.RestField.PublicName;
  if FRow=Nil then
    Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsError),SErrFieldWithoutRow,[N]);
  D:=FieldToXML(aPair);
  if (D=Nil) and (not HasOption(ooSparse)) then
    D:=FXML.CreateElement(UTF8Decode(aPair.RestField.PublicName));
  if D<>Nil then
    FRow.AppendChild(D);
end;

function TADOOutputStreamer.CreateXSD: TDomElement;

// Create XSD and append to root. Return element to which field list must be appended.

Var
  SN,N,E,TLN : TDomElement;

begin
  SN:=FXML.CreateElement('xs:schema');
  SN['id']:=Utf8Decode(DataName);
  SN['xmlns']:='';
  SN['xmlns:xs']:='http://www.w3.org/2001/XMLSchema';
  SN['xmlns:msdata']:= 'urn:schemas-microsoft-com:xml-msdata';
  FRoot.AppendChild(SN);
  // Add table list with 1 table.
  // Element
  N:=FXML.CreateElement('xs:element');
  SN.AppendChild(N);
  N['name']:=UTF8Decode(DataName);
  N['msdata:IsDataSet']:='true';
  N['msdata:UseCurrentLocale']:='true';
  // element is a complex type
  TLN:=FXML.CreateElement('xs:complexType');
  N.AppendChild(TLN);
  // Complex type is a choice (0..Unbounded] of records
  N:=FXML.CreateElement('xs:choice');
  TLN.AppendChild(N);
  N['minOccurs']:='0';
  N['maxOccurs']:='unbounded';
  // Each record is an element
  E:=FXML.CreateElement('xs:element');
  N.AppendChild(E);
  E['name']:=Utf8Decode(RowName);
  // Record is a complex type of fields
  N:=FXML.CreateElement('xs:complexType');
  E.AppendChild(N);
  // Fields are a sequence. To this sequence, the fields may be appended.
  Result:=FXML.CreateElement('xs:sequence');
  N.AppendChild(Result);
end;

Const
  XMLPropTypeNames : Array [TRestFieldType] of string = (
   'unknown',          { rtfUnknown }
   'xs:int',          { rftInteger }
   'xs:int',          { rftLargeInt}
   'xs:double',       { rftFloat }
   'xs:dateTime',     { rftDate }
   'xs:dateTime',     { rftTime }
   'xs:dateTime',     { rftDateTime }
   'xs:string',       { rftString }
   'xs:boolean',      { rftBoolean }
   'xs:base64Binary'  { rftBlob }
  );

procedure TADOOutputStreamer.WriteMetadata(aFieldList: TRestFieldPairArray);

Var
  FMetadata : TDOMElement;
  F : TDomElement;
  P : TREstFieldPair;
  I : integer;
  S : Utf8String;
  K : TRestFieldType;

begin
  FMetadata:=CreateXSD;
  For I:=0 to Length(aFieldList)-1 do
    begin
    P:=aFieldList[i];
    K:=P.RestField.FieldType;
    S:=XMLPropTypeNames[K];
    F:=FXML.CreateElement('xs:element');
    F['name']:=Utf8Decode(P.Restfield.PublicName);
    F['type']:=Utf8decode(S);
    F['minOccurs']:='0';
    FMetaData.AppendChild(F);
    end;
end;

class function TADOOutputStreamer.GetContentType: String;
begin
  Result:='text/xml';
end;

Class function TADOOutputStreamer.FileExtension : string; 

begin
  Result:='.xml';
end;


function TADOOutputStreamer.RequireMetadata: Boolean;
begin
  Result:=True;
end;

procedure TADOOutputStreamer.CreateErrorContent(aCode: Integer; const aMessage: String);

Var
  ErrorObj : TDomElement;

begin
  ErrorObj:=FXML.CreateElement(UTF8Decode(GetString(rpErrorRoot)));
  ErrorObj['code']:=UTF8Decode(IntToStr(aCode));
  ErrorObj['message']:=UTF8Decode(aMessage);
  FRoot.AppendChild(ErrorObj);
end;

destructor TADOOutputStreamer.Destroy;
begin
  FreeAndNil(FXML);
  inherited Destroy;
end;

procedure TADOOutputStreamer.InitStreaming;

begin
  FXML:=TXMLDocument.Create;
  FXML.XMLStandalone:=True;
  if DataName='' then
    DataName:='Data';
  FRoot:=FXML.CreateElement('Data');
  FXML.AppendChild(FRoot);
  if RowName='' then
    RowName:='Row';
end;

Initialization
  TADOInputStreamer.RegisterStreamer('ado');
  TADOOutputStreamer.RegisterStreamer('ado');
end.

