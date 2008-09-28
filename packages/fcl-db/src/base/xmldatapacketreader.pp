{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2008 by Joost van der Sluis, member of the
    Free Pascal development team

    TXMLDatapacketReader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit XMLDatapacketReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Bufdataset, dom, db;

type
  { TXMLDatapacketReader }

  TXMLDatapacketReader = class(TDataPacketReader)
    XMLDocument    : TXMLDocument;
    DataPacketNode : TDOMElement;
    MetaDataNode   : TDOMNode;
    FieldsNode     : TDOMNode;
    FChangeLogNode,
    FParamsNode,
    FRowDataNode,
    FRecordNode       : TDOMNode;
  public
    destructor destroy; override;
    procedure LoadFieldDefs(AFieldDefs : TFieldDefs); override;
    procedure StoreFieldDefs(AFieldDefs : TFieldDefs); override;
    procedure GetRecordUpdState(var AIsUpdate, AAddRecordBuffer,
                     AIsFirstEntry: boolean); override;
    procedure EndStoreRecord(const AChangeLog : TChangeLogEntryArr); override;
    function GetCurrentRecord : boolean; override;
    procedure GotoNextRecord; override;
    procedure GotoElement(const AnElement : pointer); override;
    procedure InitLoadRecords(var AChangeLog : TChangeLogEntryArr); override;
    function GetCurrentElement: pointer; override;
    procedure RestoreRecord(ADataset : TBufDataset); override;
    procedure StoreRecord(ADataset : TBufDataset; RowState : TRowState); override;
    class function RecognizeStream(AStream : TStream) : boolean; override;
  end;

implementation

uses xmlwrite, xmlread;

const
  XMLFieldtypenames : Array [TFieldType] of String[15] =
    (
      'Unknown',
      'string',
      'i2',
      'i4',
      'i4',
      'boolean',
      'r8',
      'r8',
      'fixed',
      'date',
      'time',
      'datetime',
      'bin.hex',
      'bin.hex',
      'i4',
      'bin.hex',
      'bin.hex',
      'bin.hex',
      'bin.hex',
      'bin.hex',
      'bin.hex',
      'bin.hex',
      '',
      'string',
      'string',
      'i8',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      '',
      ''
    );

{ TXMLDatapacketReader }

destructor TXMLDatapacketReader.destroy;
begin
  FieldsNode.Free;
  MetaDataNode.Free;
  DataPacketNode.Free;
  XMLDocument.Free;
  inherited destroy;
end;

procedure TXMLDatapacketReader.LoadFieldDefs(AFieldDefs : TFieldDefs);

  function GetNodeAttribute(const aNode : TDOMNode; AttName : String) : string;
  var AnAttr : TDomNode;
  begin
    AnAttr := ANode.Attributes.GetNamedItem(AttName);
    if assigned(AnAttr) then result := AnAttr.NodeValue
    else result := '';
  end;

var i           : integer;
    AFieldDef   : TFieldDef;
    iFieldType  : TFieldType;
    FTString    : string;
    AFieldNode  : TDOMNode;

begin
  ReadXMLFile(XMLDocument,Stream);
  DataPacketNode := XMLDocument.FindNode('DATAPACKET') as TDOMElement;
  if not assigned(DataPacketNode) then DatabaseError('Onbekend formaat');

  MetaDataNode := DataPacketNode.FindNode('METADATA');
  if not assigned(MetaDataNode) then DatabaseError('Onbekend formaat');

  FieldsNode := MetaDataNode.FindNode('FIELDS');
  if not assigned(FieldsNode) then DatabaseError('Onbekend formaat');

  with FieldsNode.ChildNodes do for i := 0 to Count - 1 do
    begin
    AFieldNode := item[i];
    if AFieldNode.CompareName('FIELD')=0 then
      begin
      AFieldDef := TFieldDef.create(AFieldDefs);
      AFieldDef.DisplayName:=GetNodeAttribute(AFieldNode,'fieldname');
      AFieldDef.Name:=GetNodeAttribute(AFieldNode,'attrname');
      AFieldDef.Size:=StrToIntDef(GetNodeAttribute(AFieldNode,'width'),0);
      FTString:=GetNodeAttribute(AFieldNode,'fieldtype');

      AFieldDef.DataType:=ftUnknown;
      for iFieldType:=low(TFieldType) to high(TFieldType) do
       if SameText(XMLFieldtypenames[iFieldType],FTString) then
        begin
        AFieldDef.DataType:=iFieldType;
        break;
        end;
      end;
    end;

  FChangeLogNode := MetaDataNode.FindNode('PARAMS');
  if assigned(FChangeLogNode) then
    FChangeLogNode := FChangeLogNode.Attributes.GetNamedItem('CHANGE_LOG');

  FRowDataNode := DataPacketNode.FindNode('ROWDATA');
  FRecordNode := nil;

end;

procedure TXMLDatapacketReader.StoreFieldDefs(AFieldDefs: TFieldDefs);

var i           : integer;
    AFieldNode  : TDOMElement;

begin
  XMLDocument := TXMLDocument.Create;
  DataPacketNode := XMLDocument.CreateElement('DATAPACKET');
  DataPacketNode.SetAttribute('Version','2.0');

  MetaDataNode := XMLDocument.CreateElement('METADATA');
  FieldsNode := XMLDocument.CreateElement('FIELDS');

  for i := 0 to AFieldDefs.Count -1 do with AFieldDefs[i] do
    begin
    AFieldNode := XMLDocument.CreateElement('FIELD');
    if Name <> '' then AFieldNode.SetAttribute('fieldname',Name);
    AFieldNode.SetAttribute('attrname',DisplayName);
    if size <> 0 then AFieldNode.SetAttribute('width',IntToStr(Size));
    AFieldNode.SetAttribute('fieldtype',XMLFieldtypenames[DataType]);
    case DataType of
      ftAutoInc : begin
                  AFieldNode.SetAttribute('readonly','true');
                  AFieldNode.SetAttribute('subtype','Autoinc');
                  end;
      ftCurrency: AFieldNode.SetAttribute('subtype','Money');
      ftVarBytes,
        ftBlob  : AFieldNode.SetAttribute('subtype','Binary');
      ftMemo    : AFieldNode.SetAttribute('subtype','Text');
      ftTypedBinary,
        ftGraphic: AFieldNode.SetAttribute('subtype','Graphics');
      ftFmtMemo : AFieldNode.SetAttribute('subtype','Formatted');
      ftParadoxOle,
        ftDBaseOle : AFieldNode.SetAttribute('subtype','Ole');
    end; {case}
    if faReadonly in Attributes then AFieldNode.SetAttribute('readonly','true');

    FieldsNode.AppendChild(AFieldNode);
    end;

  MetaDataNode.AppendChild(FieldsNode);
  FParamsNode := XMLDocument.CreateElement('PARAMS');
  MetaDataNode.AppendChild(FParamsNode);
  DataPacketNode.AppendChild(MetaDataNode);
  FRowDataNode := XMLDocument.CreateElement('ROWDATA');
end;

procedure TXMLDatapacketReader.GetRecordUpdState(var AIsUpdate,
  AAddRecordBuffer, AIsFirstEntry: boolean);
var ARowStateNode  : TDOmNode;
    ARowState      : integer;

begin
  ARowStateNode := FRecordNode.Attributes.GetNamedItem('RowState');
  if ARowStateNode = nil then // This item is not edited
    begin
    AIsUpdate:=False;
    AAddRecordBuffer:=True;
    end
  else
    begin
    AIsUpdate:=True;
    ARowState:=StrToIntDef(ARowStateNode.NodeValue,0);
    AAddRecordBuffer:=((ARowState and 5) = 4)      // This item contains an inserted record which is not edited afterwards
                      or ((ARowState and 9) = 8); // This item contains the last edited record
    AIsFirstEntry:=((ARowState and 2) = 2)         // This item is deleted
                 or ((ARowState and 8) = 8)       // This item is a change
    end;
end;

procedure TXMLDatapacketReader.EndStoreRecord(const AChangeLog : TChangeLogEntryArr);
var ChangeLogStr : String;
    i            : integer;
begin
  ChangeLogStr:='';
  for i := 0 to length(AChangeLog) -1 do with AChangeLog[i] do
    begin
    ChangeLogStr:=ChangeLogStr+' '+inttostr(NewEntry)+' '+inttostr(OrigEntry)+' ';
    if UpdateKind=ukModify then ChangeLogStr := ChangeLogStr+'8';
    if UpdateKind=ukInsert then ChangeLogStr := ChangeLogStr+'4';
    if UpdateKind=ukDelete then ChangeLogStr := ChangeLogStr+'2';
    end;

  if ChangeLogStr<>'' then
    (FParamsNode as TDomElement).SetAttribute('CHANGE_LOG',Trim(ChangeLogStr));

  DataPacketNode.AppendChild(FRowDataNode);
  XMLDocument.AppendChild(DataPacketNode);

  WriteXML(XMLDocument,Stream);
end;

function TXMLDatapacketReader.GetCurrentRecord: boolean;
begin
  Result := assigned(FRecordNode);
end;

procedure TXMLDatapacketReader.InitLoadRecords(
  var AChangeLog: TChangeLogEntryArr);

var ChangeLogStr : String;
    i,cp         : integer;
    ps           : string;

begin
  FRecordNode := FRowDataNode.FirstChild;
  if assigned(FChangeLogNode) then
    ChangeLogStr:=FChangeLogNode.NodeValue
  else
    ChangeLogStr:='';
  ps := '';
  cp := 0;
  if ChangeLogStr<>'' then for i := 1 to length(ChangeLogStr)+1 do
    begin
    if not (ChangeLogStr[i] in [' ',#0]) then
      ps := ps + ChangeLogStr[i]
    else
      begin
      case (cp mod 3) of
        0 : begin
            SetLength(AChangeLog,length(AChangeLog)+1);
            AChangeLog[cp div 3].OrigEntry:=StrToIntDef(ps,0);
            end;
        1 : AChangeLog[cp div 3].NewEntry:=StrToIntDef(ps,0);
        2 : begin
            if ps = '2' then
              AChangeLog[cp div 3].UpdateKind:=ukDelete
            else if ps = '4' then
              AChangeLog[cp div 3].UpdateKind:=ukInsert
            else if ps = '8' then
              AChangeLog[cp div 3].UpdateKind:=ukModify;
            end;
      end; {case}
      ps := '';
      inc(cp);
      end;
    end;
end;

function TXMLDatapacketReader.GetCurrentElement: pointer;
begin
  Result:=FRecordNode;
end;

procedure TXMLDatapacketReader.RestoreRecord(ADataset : TBufDataset);
var FieldNr    : integer;
    AFieldNode : TDomNode;
begin
  with ADataset do for FieldNr:=0 to FieldCount-1 do
    begin
    AFieldNode := FRecordNode.Attributes.GetNamedItem(Fields[FieldNr].FieldName);
    if assigned(AFieldNode) then
      begin
      Fields[FieldNr].AsString := AFieldNode.NodeValue;  // set it to the sparebuf
      end
    end;
end;

procedure TXMLDatapacketReader.StoreRecord(ADataset: TBufDataset;
  RowState: TRowState);
var FieldNr : Integer;
    RowStateInt : Integer;
    ARecordNode : TDOMElement;
begin
  ARecordNode := XMLDocument.CreateElement('ROW');
  for FieldNr := 0 to ADataset.Fields.Count-1 do
    begin
    ARecordNode.SetAttribute(ADataset.fields[FieldNr].FieldName,ADataset.fields[FieldNr].AsString);
    end;
  RowStateInt:=0;
  if rsvOriginal in RowState then RowStateInt := RowStateInt+1;
  if rsvInserted in RowState then RowStateInt := RowStateInt+4;
  if rsvUpdated in RowState then RowStateInt := RowStateInt+8;
  RowStateInt:=integer(RowState);
  if RowStateInt<>0 then
    ARecordNode.SetAttribute('RowState',inttostr(RowStateInt));
  FRowDataNode.AppendChild(ARecordNode);
end;

class function TXMLDatapacketReader.RecognizeStream(AStream: TStream): boolean;
const XmlStart = '<?xml';
var s        : string;
    len      : integer;
begin
  Len := length(XmlStart);
  setlength(s,len);
  if (AStream.Read (s[1],len) = len)
  and (s=XmlStart) then
    Result := True
  else
    Result := False;
end;

procedure TXMLDatapacketReader.GotoNextRecord;
begin
  FRecordNode := FRecordNode.NextSibling;
  while assigned(FRecordNode) and (FRecordNode.CompareName('ROW')<>0) do
    FRecordNode := FRecordNode.NextSibling;
end;

procedure TXMLDatapacketReader.GotoElement(const AnElement: pointer);
begin
  FRecordNode:=TDomNode(AnElement);
end;

initialization
  RegisterDatapacketReader(TXMLDatapacketReader,dfXML);
end.
