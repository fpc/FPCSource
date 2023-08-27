{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST bridge : XML input/output

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestxml;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.DateUtils, Data.Db, FpJson.Data, Xml.Dom, Xml.Read, Xml.Writer, FpWeb.RestBridge.Schema,FpWeb.RestBridge.IO, FpWeb.RestBridge.Bridge;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, DateUtils, db,fpjson, dom, XMLRead, XMLWrite,sqldbrestschema,sqldbrestio, sqldbrestbridge;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TXMLInputStreamer }

  TXMLInputStreamer = Class(TRestInputStreamer)
  private
    FXML: TXMLDocument;
    FPacket : TDOMElement;
    FData : TDOMElement;
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
  end;

  { TXMLOutputStreamer }

  TXMLOutputStreamer = Class(TRestOutputStreamer)
  Private
    FXML: TXMLDocument;
    FData : TDOMElement;
    FRow: TDOMElement;
    FRoot: TDomElement;
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
    Class Function FileExtension: String; override;
    procedure InitStreaming; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}

{ TXMLInputStreamer }

destructor TXMLInputStreamer.Destroy;
begin
  FreeAndNil(FXML);
  inherited Destroy;
end;

class function TXMLInputStreamer.GetContentType: String;
begin
  Result:='text/xml';
end;

Class Function TXMLOutputStreamer.FileExtension: String;
begin
  Result:='.xml';  
end;


function TXMLInputStreamer.SelectObject(aIndex: Integer): Boolean;

Var
  N : TDomNode;
  NN : UnicodeString;
begin
  Result:=False;
  NN:=UTF8Decode(GetString(rpRowName));
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

Function TXMLInputStreamer.GetNodeText(N : TDOmNode) : UnicodeString;

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

function TXMLInputStreamer.GetContentField(aName: UTF8string): TJSONData;

Var
  NN : UnicodeString;
  N : TDomNode;
begin
  NN:=UTF8Decode(aName);
  N:=FRow.FindNode(NN);
  if Assigned(N) and (N.NodeType=ELEMENT_NODE) then
    Result:=TJSONString.Create(UTF8Encode(GetNodeText(N)));
end;

procedure TXMLInputStreamer.InitStreaming;

Var
  Msg : String;
  N : TDomNode;
  NN : UnicodeString;

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
    Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsInvalidContent),SErrInvalidXMLInput,[Msg]);
  FPacket:=FXML.DocumentElement;
  NN:=UTF8Decode(GetString(rpXMLDocumentRoot));
  if (NN<>'') then
    begin
    if FPacket.NodeName<>NN then
      Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsInvalidContent),SErrInvalidXMLInput,[SErrMissingDocumentRoot]);
    NN:=UTF8Decode(GetString(rpDataRoot));
    N:=FPacket.FindNode(NN);
    end
  else
    begin
    // if Documentroot is empty, data packet is the root element
    NN:=UTF8Decode(GetString(rpDataRoot));
    if (Packet.NodeName=NN) then
      N:=FPacket
    else
      N:=Nil
    end;
  if Not (Assigned(N) and (N is TDOMelement)) then
    Raise ESQLDBRest.CreateFmt(Statuses.GetStatusCode(rsInvalidContent),SErrInvalidXMLInputMissingElement,[NN]);
  FData:=(N as TDOMelement);
end;

{ TXMLOutputStreamer }


procedure TXMLOutputStreamer.EndData;
begin
  FData:=Nil;
end;

procedure TXMLOutputStreamer.EndRow;
begin
  FRow:=Nil;
end;

procedure TXMLOutputStreamer.FinalizeOutput;

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

procedure TXMLOutputStreamer.StartData;
begin
  FData:=FXML.CreateElement(UTF8Decode(GetString(rpDataRoot)));
  FRoot.AppendChild(FData);
end;

procedure TXMLOutputStreamer.StartRow;
begin
  if (FRow<>Nil) then
    Raise ESQLDBRest.Create(Statuses.GetStatusCode(rsError),SErrDoubleRowStart);
  FRow:=FXML.CreateElement(UTF8Decode(GetString(rpRowName)));
  FData.AppendChild(FRow);
end;

Function TXMLOutputStreamer.FieldToXML(aPair: TRestFieldPair) : TDomElement;

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

procedure TXMLOutputStreamer.WriteField(aPair: TRestFieldPair);

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

procedure TXMLOutputStreamer.WriteMetadata(aFieldList: TRestFieldPairArray);

Var
  M : TDOMElement;
  F : TDomElement;
  P : TREstFieldPair;
begin
  F:=FXML.CreateElement(UTF8Decode(GetString(rpMetaDataFields)));
  M:=FXML.CreateElement(UTF8Decode(GetString(rpMetaDataRoot)));
  M.AppendChild(F);
  FRoot.AppendChild(M);
  M:=F;
  For P in aFieldList do
    begin
    F:=FXML.CreateElement(UTF8Decode(GetString(rpMetaDataField)));
    M.AppendChild(F);
    F[UTF8Decode(GetString(rpFieldNameProp))]:=UTF8Decode(P.RestField.PublicName);
    F[UTF8Decode(GetString(rpFieldTypeProp))]:=UTF8Decode(typenames[P.RestField.FieldType]);
    Case P.RestField.FieldType of
      rftDate : F[UTF8Decode(GetString(rpFieldDateFormatProp))]:=UTF8Decode(GetString(rpDateFormat));
      rftTime : F[UTF8Decode(GetString(rpFieldDateFormatProp))]:=UTF8Decode(GetString(rpTimeFormat));
      rftDateTime : F[UTF8Decode(GetString(rpFieldDateFormatProp))]:=UTF8Decode(GetString(rpDateTimeFormat));
      rftString : F[UTF8Decode(GetString(rpFieldMaxLenProp))]:=UTF8Decode(IntToStr(P.DBField.Size));
    else
       ;
    end;
    end;
end;

class function TXMLOutputStreamer.GetContentType: String;
begin
  Result:='text/xml';
end;

procedure TXMLOutputStreamer.CreateErrorContent(aCode: Integer; const aMessage: String);

Var
  ErrorObj : TDomElement;

begin
  ErrorObj:=FXML.CreateElement(UTF8Decode(GetString(rpErrorRoot)));
  ErrorObj['code']:=UTF8Decode(IntToStr(aCode));
  ErrorObj['message']:=UTF8Decode(aMessage);
  FRoot.AppendChild(ErrorObj);
end;

destructor TXMLOutputStreamer.Destroy;
begin
  FreeAndNil(FXML);
  inherited Destroy;
end;

procedure TXMLOutputStreamer.InitStreaming;
begin
  FXML:=TXMLDocument.Create;
  FRoot:=FXML.CreateElement('datapacket');
  FXML.AppendChild(FRoot);
end;

Initialization
  TXMLInputStreamer.RegisterStreamer('xml');
  TXMLOutputStreamer.RegisterStreamer('xml');
end.

