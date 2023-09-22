{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST CDS input/output

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestcds;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.DateUtils, Data.Db, FpJson.Data, Xml.Dom, Xml.Read, 
  Xml.Writer,FpWeb.RestBridge.Schema,FpWeb.RestBridge.IO, FpWeb.RestBridge.Bridge;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, DateUtils, db,fpjson, dom, XMLRead, XMLWrite,sqldbrestschema,sqldbrestio, sqldbrestbridge;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TCDSInputStreamer }

  TCDSInputStreamer = Class(TRestInputStreamer)
  private
    FXML: TXMLDocument;
    FPacket : TDOMElement;
    FROWData : TDOMElement;
    FRow : TDOMElement;
  Public
    Destructor Destroy; override;
    Class Function GetContentType: String; override;
    Class Function ForBufDataset: Boolean; virtual;
    Function SelectObject(aIndex : Integer) : Boolean; override;
    function GetContentField(aName: UTF8string): TJSONData; override;
    procedure InitStreaming; override;
    Property XML : TXMLDocument Read FXML;
    Property Packet : TDOMElement Read FPacket;
    Property RowData : TDOMElement Read FRowData;
    Property Row : TDOMElement Read FRow;
  end;

  { TCDSOutputStreamer }

  TCDSOutputStreamer = Class(TRestOutputStreamer)
  Private
    FXML: TXMLDocument;
    FDataPacket : TDOMElement;
    FMetaData : TDOMElement;
    FRow : TDOMElement;
    FRowData: TDOMElement;
  Protected
    Class Function ForBufDataset: Boolean; virtual;
    Procedure SetOutputOptions(AValue: TRestOutputOptions); override;
  Public
    procedure EndData; override;
    procedure EndRow; override;
    procedure FinalizeOutput; override;
    procedure StartData; override;
    procedure StartRow; override;
    // Return Nil for null field.
    procedure WriteField(aPair: TRestFieldPair); override;
    procedure WriteMetadata(aFieldList: TRestFieldPairArray); override;
    Procedure CreateErrorContent(aCode : Integer; Const aMessage: String); override;
    Property XML : TXMLDocument Read FXML;
    Property RowData : TDOMelement Read FRowData;
    Property Row : TDOMelement Read FRow;
    Property Metadata : TDOMelement Read FMetadata;
  Public
    Destructor Destroy; override;
    Class Function GetContentType: String; override;
    Class Function FileExtension : String; override;
    procedure InitStreaming; override;
  end;

  { TBufDatasetOutputStreamer }

  TBufDatasetOutputStreamer = Class(TCDSOutputStreamer)
  Protected
    Class Function ForBufDataset: Boolean; override;
  end;

  { TBufDatasetInputStreamer }

  TBufDatasetInputStreamer = Class(TCDSInputStreamer)
  Public
    Class Function ForBufDataset: Boolean; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}



Const
  DateTimeFmt = 'yyyymmddThh:nn:sszzz';


Const
  XMLPropTypeNames : Array [TRestFieldType] of UnicodeString = (
    'Unknown' {rftUnknown},
    'i4' {rftInteger},
    'i8' {rftLargeInt},
    'r8' {rftFloat},
    'dateTime' {rftDate},
    'dateTime' {rftTime},
    'dateTime' {rftDateTime},
    'string' {rftString},
    'boolean' {rftBoolean},
    'bin.hex:Binary' {rftBlob}
  );

{ TBufDatasetInputStreamer }

class function TBufDatasetInputStreamer.ForBufDataset: Boolean;
begin
  Result:=True;
end;

{ TBufDatasetOutputStreamer }

class function TBufDatasetOutputStreamer.ForBufDataset: Boolean;
begin
  Result:=True;
end;

{ TCDSInputStreamer }

destructor TCDSInputStreamer.Destroy;
begin
  FreeAndNil(FXML);
  inherited Destroy;
end;

class function TCDSInputStreamer.GetContentType: String;
begin
  Result:='text/xml';
end;

class function TCDSInputStreamer.ForBufDataset: Boolean;
begin
  Result:=False;
end;

function TCDSInputStreamer.SelectObject(aIndex: Integer): Boolean;

Var
  N : TDomNode;
  NN : UnicodeString;
begin
  Result:=False;
  NN:='ROW';
  N:=FRowData.FindNode(NN);
  if Not (Assigned(N) and (N is TDOMelement)) then
    raise ESQLDBRest.CreateFmt(400, SErrInvalidCDSMissingElement,[NN]);
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


function TCDSInputStreamer.GetContentField(aName: UTF8string): TJSONData;

Var
  NN : UnicodeString;

begin
  NN:=UTF8Decode(aName);
  if Assigned(FRow) and FRow.hasAttribute(NN) then
    Result:=TJSONString.Create(FRow.AttribStrings[NN])
  else
    Result:=Nil;
end;

procedure TCDSInputStreamer.InitStreaming;

Var
  Msg : String;
  N : TDomNode;

begin
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
    raise ESQLDBRest.CreateFmt(400, SErrInvalidXMLInput, [Msg]);
  FPacket:=FXML.DocumentElement;
  if (FPacket=Nil)  then
    raise ESQLDBRest.CreateFmt(400, SErrInvalidXMLInput, [SErrMissingDocumentRoot]);
  if (FPacket.NodeName<>'DATAPACKET') then
    Raise ESQLDBRest.CreateFmt(400,SErrInvalidCDSMissingElement,['DATAPACKET']);
  N:=FPacket.FindNode('ROWDATA');
  if Not (Assigned(N) and (N is TDOMelement)) then
    Raise ESQLDBRest.CreateFmt(400,SErrInvalidCDSMissingElement,[ROWDATA]);
  FRowData:=(N as TDOMelement);
end;

{ TCDSOutputStreamer }

class function TCDSOutputStreamer.ForBufDataset: Boolean;
begin
  Result:=False;
end;

procedure TCDSOutputStreamer.SetOutputOptions(AValue: TRestOutputOptions);
begin
  Include(AValue,ooMetadata); // We always need metadata
  inherited SetOutputOptions(AValue);
end;

procedure TCDSOutputStreamer.EndData;
begin
  FRowData:=Nil;
end;

procedure TCDSOutputStreamer.EndRow;
begin
  FRow:=Nil;
end;

procedure TCDSOutputStreamer.FinalizeOutput;

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

procedure TCDSOutputStreamer.StartData;

begin
  // Do nothing
end;

procedure TCDSOutputStreamer.StartRow;
begin
  if (FRow<>Nil) then
    Raise ESQLDBRest.Create(500,SErrDoubleRowStart);
  FRow:=FXML.CreateElement('ROW');
  FRowData.AppendChild(FRow);
end;

procedure TCDSOutputStreamer.WriteField(aPair: TRestFieldPair);

Var
  N : UTF8String;
  S : UTF8String;
  F : TField;

begin
  N:=aPair.RestField.PublicName;
  if FRow=Nil then
    Raise ESQLDBRest.CreateFmt(500,SErrFieldWithoutRow,[N]);
  F:=aPair.DBField;
  If (aPair.RestField.FieldType=rftUnknown) then
    raise ESQLDBRest.CreateFmt(500,SErrUnsupportedRestFieldType, [N]);
  If (F.IsNull) then
    Exit;
  if (aPair.RestField.FieldType in [rftDate,rftTime,rftDateTime]) then
    S:=FormatDateTime(DateTimeFmt,F.AsDateTime)
  else
    S:=FieldToString(aPair.RestField.FieldType,F);
  FRow[UTF8Decode(N)]:=UTF8Decode(S);
end;


procedure TCDSOutputStreamer.WriteMetadata(aFieldList: TRestFieldPairArray);

Var
  FL,F : TDOMElement;
  P : TREstFieldPair;
  S,ST : UnicodeString;
  ml : Integer;

begin
  FL:=FXML.CreateElement('FIELDS');
  FMetaData.AppendChild(FL);
  For P in aFieldList do
    begin
    S:=XMLPropTypeNames[P.RestField.FieldType];
    if (S<>'') then
      begin
      ST:='';
      if P.RestField.PublicName='ID' then
        ST:='autoinc';
      F:=FXML.CreateElement('FIELD');
      F['attrname']:=Utf8Decode(P.RestField.PublicName);
      F['fieldtype']:=S;
      if P.RestField.FieldType=rftString then
         begin
         ML:=P.RestField.MaxLen;
         if ML=0 then
           ML:=255;
         if ForBufDataset then
           F['width']:=Utf8Decode(IntToStr(P.RestField.MaxLen))
         else
           F['WIDTH']:=Utf8Decode(IntToStr(P.RestField.MaxLen));

         end;
      if (ST<>'') then
        F['subtype']:=ST;
      FL.AppendChild(F);
      end;
    end;
end;

class function TCDSOutputStreamer.GetContentType: String;
begin
  Result:='text/xml';
end;

class function TCDSOutputStreamer.FileExtension: String;
begin
  Result:='xml';
end;

procedure TCDSOutputStreamer.CreateErrorContent(aCode: Integer; const aMessage: String);

Var
  ErrorObj : TDomElement;

begin
  ErrorObj:=FXML.CreateElement(UTF8Decode(GetString(rpErrorRoot)));
  ErrorObj['code']:=UTF8Decode(IntToStr(aCode));
  ErrorObj['message']:=UTF8Decode(aMessage);
  FDataPacket.AppendChild(ErrorObj);
end;

destructor TCDSOutputStreamer.Destroy;
begin
  FreeAndNil(FXML);
  inherited Destroy;
end;

procedure TCDSOutputStreamer.InitStreaming;
begin
  FXML:=TXMLDocument.Create;
  FDataPacket:=FXML.CreateElement('DATAPACKET');
  FXML.AppendChild(FDataPacket);
  FDataPacket['Version']:='2.0';
  FMetaData:=FXML.CreateElement('METADATA');
  FDataPacket.AppendChild(FMetaData);
  FRowData:=FXML.CreateElement('ROWDATA');
  FDataPacket.AppendChild(FRowData);
end;

Initialization
  TCDSInputStreamer.RegisterStreamer('cds');
  TBufDatasetInputStreamer.RegisterStreamer('buf');
  TCDSOutputStreamer.RegisterStreamer('cds');
  TBufDatasetOutputStreamer.RegisterStreamer('buf');
end.

