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

{$IFNDEF FPC_DOTTEDUNITS}
unit XMLDatapacketReader;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Data.BufDataset, Xml.Dom, Data.Db;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Bufdataset, dom, db;
{$ENDIF FPC_DOTTEDUNITS}

type
  TChangeLogEntry = record
       UpdateKind : TUpdateKind;
       OrigEntry  : integer;
       NewEntry   : integer;
  end;
  TChangeLogEntryArr = array of TChangeLogEntry;

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
    FRecordNode    : TDOMNode;
    FChangeLog     : TChangeLogEntryArr;
    FEntryNr       : integer;
    FLastChange    : integer;
  private
    // Adds a FIELD element for each definition of aDefs to aFieldsNode
    procedure WriteFieldDefs(aDefs: TFieldDefs; aFieldsNode: TDOMNode);
    // Adds a definition to aDefs for each FIELD element of aFieldsNode
    procedure ReadFieldDefs(aFieldsNode: TDOMNode; aDefs: TFieldDefs);
    // Stores the field values of the current record of aDataSet in aRecordNode
    procedure WriteFields(aDataSet: TCustomBufDataset; aRecordNode: TDOMElement);
    // Sets the field values of the current record of aDataSet from aRecordNode
    procedure ReadFields(aDataSet: TCustomBufDataset; aRecordNode: TDOMNode);
    // Adds the value of a scalar field as attribute aAttrName of aNode, nothing when it is null
    procedure WriteScalar(AField: TField; aDef: TFieldDef; aNode: TDOMElement; const aAttrName: string);
    // Sets a scalar field from attribute aAttrName of aNode; null when aNode or the attribute is missing
    procedure ReadScalar(AField: TField; aDef: TFieldDef; aNode: TDOMNode; const aAttrName: string);
    // Adds an element for the ADT or array value of definition aDef (field aFullName) to aParent
    procedure WriteObjectValue(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aParent: TDOMElement);
    // Sets the ADT or array value of definition aDef (field aFullName) from the element in aParent
    procedure ReadObjectValue(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aParent: TDOMNode);
    // Adds the members of ADT definition aDef (field aFullName) to aNode
    procedure WriteElementMembers(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aNode: TDOMElement);
    // Sets the members of ADT definition aDef (field aFullName) from aNode
    procedure ReadElementMembers(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aNode: TDOMNode);
  public
    destructor destroy; override;
    procedure StoreFieldDefs(AnAutoIncValue : integer); override;
    procedure StoreRecord(ARowState : TRowState; AUpdOrder : integer = 0); override;
    procedure FinalizeStoreRecords; override;
    procedure LoadFieldDefs(var AnAutoIncValue : integer); override;
    procedure InitLoadRecords; override;
    function GetCurrentRecord : boolean; override;
    function GetRecordRowState(out AUpdOrder : Integer) : TRowState; override;
    procedure RestoreRecord; override;
    procedure GotoNextRecord; override;
    class function RecognizeStream(AStream : TStream) : boolean; override;
    class function SupportsNestedData : boolean; override;
    class function SupportsObjectFields : boolean; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Xml.Writer, Xml.Read, System.Hash.Base64;
{$ELSE FPC_DOTTEDUNITS}
uses xmlwrite, xmlread, base64;
{$ENDIF FPC_DOTTEDUNITS}

const
  XMLFieldTypeNames : Array [TFieldType] of String[16] =
    (
      'Unknown',
      'string',
      'i2',
      'i4',
      'i4',
      'boolean',
      'r8',
      'r8:Money',
      'fixed',
      'date',
      'time',
      'datetime',
      'bin.hex',
      'bin.hex',
      'i4:Autoinc',
      'bin.hex:Binary',
      'bin.hex:Text',
      'bin.hex:Graphics',
      'bin.hex:Formatted',
      'bin.hex:Ole',
      'bin.hex:Ole',
      'bin.hex:Graphics',
      '',
      'string',             // ftFixedChar
      'string.uni',         // ftWideString
      'i8',                 // ftLargeint
      'adt',                // ftADT
      'array',              // ftArray
      '',
      'nested',             // ftDataSet
      '',
      '',
      'variant',            // ftVariant
      '',
      '',
      'string:Guid',        // ftGuid
      '',
      'fixedFMT',           // ftFmtBCD
      'string.uni',         // ftFixedWideChar
      'bin.hex:WideText',   // ftWideMemo
      'SQLdateTime',        // ftOraTimeStamp
      '',                   // ftOraInterval
      'ui4',                // ftLongWord
      'i1',                 // ftShortint
      'ui1',                // ftByte
      '',                   // ftExtended
      'r4'                  // ftSingle
    );

  XMLDateTimeFormats : Array [ftDate..ftDateTime] of String = (
    'yyyymmdd', 'hh:nn:ss.zzz', 'yyyymmdd"T"hh:nn:ss.zzz');

resourcestring
  sUnknownXMLDatasetFormat = 'Unknown XML Dataset format';

// Converts a date, time or date-time value written with XMLDateTimeFormats
function TryXMLToDateTime(aType: TFieldType; const aValue: string; out aResult: TDateTime): Boolean;

var
  lDate, lTime: string;
  lYear, lMonth, lDay, lHour, lMin, lSec, lMSec: Integer;
  lPart: TDateTime;

begin
  Result := False;
  aResult := 0;
  lDate := '';
  lTime := '';
  case aType of
    ftDate: lDate := aValue;
    ftTime: lTime := aValue;
    ftDateTime:
      begin
      if (Length(aValue) <> 21) or (aValue[9] <> 'T') then
        Exit;
      lDate := Copy(aValue, 1, 8);
      lTime := Copy(aValue, 10, 12);
      end;
  end;
  if lDate <> '' then
    begin
    if (Length(lDate) <> 8) or not TryStrToInt(Copy(lDate, 1, 4), lYear)
       or not TryStrToInt(Copy(lDate, 5, 2), lMonth) or not TryStrToInt(Copy(lDate, 7, 2), lDay)
       or not TryEncodeDate(lYear, lMonth, lDay, lPart) then
      Exit;
    aResult := lPart;
    end;
  if lTime <> '' then
    begin
    if (Length(lTime) <> 12) or (lTime[3] <> ':') or (lTime[6] <> ':') or (lTime[9] <> '.')
       or not TryStrToInt(Copy(lTime, 1, 2), lHour) or not TryStrToInt(Copy(lTime, 4, 2), lMin)
       or not TryStrToInt(Copy(lTime, 7, 2), lSec) or not TryStrToInt(Copy(lTime, 10, 3), lMSec)
       or not TryEncodeTime(lHour, lMin, lSec, lMSec, lPart) then
      Exit;
    if aResult < 0 then
      aResult := aResult - lPart
    else
      aResult := aResult + lPart;
    end;
  Result := True;
end;

{ TXMLDatapacketReader }

destructor TXMLDatapacketReader.destroy;
begin
  FieldsNode.Free;
  MetaDataNode.Free;
  DataPacketNode.Free;
  XMLDocument.Free;
  inherited destroy;
end;

// The attribute name for the value of a scalar array element of definition aDef
function ElementAttrName(aDef: TFieldDef): string;

begin
  Result := aDef.Name;
  if Result = '' then
    Result := 'value';
end;


// The value of attribute AttName of aNode, empty when it is missing
function GetNodeAttribute(const aNode : TDOMNode; const AttName : String) : string;

var AnAttr : TDomNode;

begin
  AnAttr := ANode.Attributes.GetNamedItem(AttName);
  if assigned(AnAttr) then result := AnAttr.NodeValue
  else result := '';
end;


procedure TXMLDatapacketReader.LoadFieldDefs(var AnAutoIncValue: integer);

var i           : integer;
    AnAutoIncNode: TDomNode;

begin
  ReadXMLFile(XMLDocument,Stream);
  DataPacketNode := XMLDocument.FindNode('DATAPACKET') as TDOMElement;
  if not assigned(DataPacketNode) then DatabaseError(sUnknownXMLDatasetFormat);

  MetaDataNode := DataPacketNode.FindNode('METADATA');
  if not assigned(MetaDataNode) then DatabaseError(sUnknownXMLDatasetFormat);

  FieldsNode := MetaDataNode.FindNode('FIELDS');
  if not assigned(FieldsNode) then DatabaseError(sUnknownXMLDatasetFormat);

  ReadFieldDefs(FieldsNode,DataSet.FieldDefs);
  i:=0;
  with FieldsNode.ChildNodes do
    for i := 0 to Count - 1 do
      if (item[i].CompareName('FIELD')=0) and Assigned(item[i].Attributes.GetNamedItem('autoincvalue')) then
        SetNestedAutoInc(DataSet.FieldDefs.IndexOf(GetNodeAttribute(item[i],'attrname')),
                         StrToIntDef(GetNodeAttribute(item[i],'autoincvalue'),1));

  FParamsNode := MetaDataNode.FindNode('PARAMS');
  if assigned(FParamsNode) then
    begin
    FChangeLogNode := FParamsNode.Attributes.GetNamedItem('CHANGE_LOG');
    AnAutoIncNode := FParamsNode.Attributes.GetNamedItem('AUTOINCVALUE');
    if assigned(AnAutoIncNode) then
      AnAutoIncValue := StrToIntDef(AnAutoIncNode.NodeValue,-1);
    end;

  FRowDataNode := DataPacketNode.FindNode('ROWDATA');
  FRecordNode := nil;
end;

procedure TXMLDatapacketReader.ReadFieldDefs(aFieldsNode: TDOMNode; aDefs: TFieldDefs);

var i,s         : integer;
    AFieldDef   : TFieldDef;
    iFieldType  : TFieldType;
    FTString    : string;
    SubFTString : string;
    AFieldNode  : TDOMNode;
    AChildNode  : TDOMNode;

begin
  with aFieldsNode.ChildNodes do for i := 0 to Count - 1 do
    begin
    AFieldNode := item[i];
    if AFieldNode.CompareName('FIELD')=0 then
      begin
      AFieldDef := aDefs.AddFieldDef;
      AFieldDef.DisplayName:=GetNodeAttribute(AFieldNode,'fieldname');
      AFieldDef.Name:=GetNodeAttribute(AFieldNode,'attrname');
      // Difference in casing between CDS and bufdataset...
      S:=StrToIntDef(GetNodeAttribute(AFieldNode,'width'),-1);
      if (S=-1) then
        S:=StrToIntDef(GetNodeAttribute(AFieldNode,'WIDTH'),0);
      AFieldDef.Size:=s;
      FTString:=GetNodeAttribute(AFieldNode,'fieldtype');
      SubFTString:=GetNodeAttribute(AFieldNode,'subtype');
      if SubFTString<>'' then
        FTString:=FTString+':'+SubFTString;

      AFieldDef.DataType:=ftUnknown;
      if FTString<>'' then
       for iFieldType:=low(TFieldType) to high(TFieldType) do
        if SameText(XMLFieldtypenames[iFieldType],FTString) then
        begin
        AFieldDef.DataType:=iFieldType;
        break;
        end;
      S:=StrToIntDef(GetNodeAttribute(AFieldNode,'codepage'),CP_ACP);
      if S<>CP_ACP then
        SetFieldDefCodePage(AFieldDef,S);
      if AFieldDef.DataType in [ftADT, ftArray, ftDataSet] then
        begin
        AChildNode:=AFieldNode.FindNode('FIELDS');
        if Assigned(AChildNode) then
          ReadFieldDefs(AChildNode,AFieldDef.ChildDefs);
        end;
      end;
    end;
end;


procedure TXMLDatapacketReader.WriteFieldDefs(aDefs: TFieldDefs; aFieldsNode: TDOMNode);

var i,p         : integer;
    AFieldNode  : TDOMElement;
    AChildNode  : TDOMElement;
    AStringFT   : string;

begin
  for i := 0 to aDefs.Count - 1 do with aDefs[i] do
    begin
    AFieldNode := XMLDocument.CreateElement('FIELD');
    if Name <> '' then AFieldNode.SetAttribute('fieldname',Name);
    AFieldNode.SetAttribute('attrname',DisplayName);
    if size <> 0 then AFieldNode.SetAttribute('width',IntToStr(Size));
    AStringFT:=XMLFieldtypenames[DataType];
    p := pos(':',AStringFT);
    if p > 1 then
      begin
      AFieldNode.SetAttribute('fieldtype',copy(AStringFT,1,p-1));
      AFieldNode.SetAttribute('subtype',copy(AStringFT,p+1,25));
      end
    else
      AFieldNode.SetAttribute('fieldtype',AStringFT);
    if faReadonly in Attributes then AFieldNode.SetAttribute('readonly','true');
    if (DataType in [ftString, ftFixedChar, ftMemo]) and (Codepage<>CP_ACP) then
      AFieldNode.SetAttribute('codepage',IntToStr(Codepage));
    if (DataType in [ftADT, ftArray, ftDataSet]) and HasChildDefs then
      begin
      if (DataType=ftDataSet) and (aDefs=DataSet.FieldDefs) then
        AFieldNode.SetAttribute('autoincvalue',IntToStr(GetNestedAutoInc(i)));
      AChildNode := XMLDocument.CreateElement('FIELDS');
      WriteFieldDefs(ChildDefs,AChildNode);
      AFieldNode.AppendChild(AChildNode);
      end;

    aFieldsNode.AppendChild(AFieldNode);
    end;
end;


procedure TXMLDatapacketReader.StoreFieldDefs(AnAutoIncValue: integer);

begin
  XMLDocument := TXMLDocument.Create;
  DataPacketNode := XMLDocument.CreateElement('DATAPACKET');
  DataPacketNode.SetAttribute('Version','2.0');

  MetaDataNode := XMLDocument.CreateElement('METADATA');
  FieldsNode := XMLDocument.CreateElement('FIELDS');

  WriteFieldDefs(DataSet.FieldDefs,FieldsNode);

  MetaDataNode.AppendChild(FieldsNode);
  FParamsNode := XMLDocument.CreateElement('PARAMS');
  if AnAutoIncValue>-1 then
    (FParamsNode as TDomElement).SetAttribute('AUTOINCVALUE',IntToStr(AnAutoIncValue));

  MetaDataNode.AppendChild(FParamsNode);
  DataPacketNode.AppendChild(MetaDataNode);
  FRowDataNode := XMLDocument.CreateElement('ROWDATA');
  setlength(FChangeLog,0);
  FEntryNr:=0;
  FLastChange:=-1;
end;

procedure TXMLDatapacketReader.FinalizeStoreRecords;
var ChangeLogStr : String;
    i            : integer;
begin
  ChangeLogStr:='';
  for i := 0 to length(FChangeLog)-1 do with FChangeLog[i] do
    begin
    ChangeLogStr:=ChangeLogStr+' '+inttostr(NewEntry)+' '+inttostr(OrigEntry)+' ';
    if UpdateKind=ukModify then ChangeLogStr := ChangeLogStr+'8';
    if UpdateKind=ukInsert then ChangeLogStr := ChangeLogStr+'4';
    if UpdateKind=ukDelete then ChangeLogStr := ChangeLogStr+'2';
    end;
  setlength(FChangeLog,0);

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

function TXMLDatapacketReader.GetRecordRowState(out AUpdOrder: Integer
  ): TRowState;
var ARowStateNode  : TDOmNode;
    ARowState      : integer;
    i              : integer;
begin
  ARowStateNode := FRecordNode.Attributes.GetNamedItem('RowState');
  if ARowStateNode = nil then // This item is not edited
    Result := []
  else
    begin
    Result := ByteToRowState(StrToIntDef(ARowStateNode.NodeValue,0));
    if Result = [rsvOriginal] then
      begin
      for i := 0 to length(FChangeLog)-1 do
        if FChangeLog[i].NewEntry=FEntryNr then break;
      assert(FChangeLog[i].NewEntry=FEntryNr);
      end
    else
      begin
      for i := 0 to length(FChangeLog)-1 do
        if FChangeLog[i].OrigEntry=FEntryNr then break;
      assert(FChangeLog[i].OrigEntry=FEntryNr);
      end;
    AUpdOrder:=i;
    end;
end;

procedure TXMLDatapacketReader.InitLoadRecords;

var ChangeLogStr : String;
    I, StartI, cp: integer;
    ps           : string;

begin
  FRecordNode := FRowDataNode.FirstChild;
  FEntryNr := 1;
  setlength(FChangeLog,0);
  if assigned(FChangeLogNode) then
    ChangeLogStr:=FChangeLogNode.NodeValue
  else
    ChangeLogStr:='';
  ps := '';
  cp := 0;
  StartI := 1;
  if ChangeLogStr<>'' then
  for I := 1 to Length(ChangeLogStr)+1 do
    begin
    if (I>Length(ChangeLogStr)) or (ChangeLogStr[I] in [' ',#0]) then
      begin
      ps := Copy(ChangeLogStr, StartI, I-StartI);
      case (cp mod 3) of
        0 : begin
            SetLength(FChangeLog,length(FChangeLog)+1);
            FChangeLog[cp div 3].OrigEntry:=StrToIntDef(ps,0);
            end;
        1 : FChangeLog[cp div 3].NewEntry:=StrToIntDef(ps,0);
        2 : begin
            if ps = '2' then
              FChangeLog[cp div 3].UpdateKind:=ukDelete
            else if ps = '4' then
              FChangeLog[cp div 3].UpdateKind:=ukInsert
            else if ps = '8' then
              FChangeLog[cp div 3].UpdateKind:=ukModify;
            end;
      end; {case}
      inc(cp);
      StartI := I+1;
      end;
    end;
end;

procedure TXMLDatapacketReader.RestoreRecord;
begin
  ReadFields(DataSet,FRecordNode);
end;

procedure TXMLDatapacketReader.ReadFields(aDataSet: TCustomBufDataset; aRecordNode: TDOMNode);
var FieldNr      : integer;
    AFieldNode   : TDomNode;
    ARowNode     : TDomNode;
    AField: TField;
    ARows: TCustomBufDataset;
begin
  with aDataSet do for FieldNr:=0 to FieldDefs.Count-1 do
    begin
    AField := Fields.FieldByNumber(FieldDefs[FieldNr].FieldNo);
    if FieldDefs[FieldNr].DataType=ftDataSet then
      begin
      AFieldNode := aRecordNode.FindNode(FieldDefs[FieldNr].Name);
      if not Assigned(AFieldNode) then
        begin
        AField.SetData(nil);
        Continue;
        end;
      ARows := NestedRowsDataset(FieldDefs[FieldNr].ChildDefs);
      try
        SetRowsAutoInc(ARows,StrToIntDef(GetNodeAttribute(AFieldNode,'AUTOINCVALUE'),1));
        ARowNode := AFieldNode.FirstChild;
        while Assigned(ARowNode) do
          begin
          if ARowNode.NodeType=ELEMENT_NODE then
            begin
            ARows.Append;
            ReadFields(ARows,ARowNode);
            ARows.Post;
            end;
          ARowNode := ARowNode.NextSibling;
          end;
        StoreNestedRows(AField,ARows);
      finally
        ARows.Free;
      end;
      Continue;
      end;
    if FieldDefs[FieldNr].DataType in [ftADT, ftArray] then
      ReadObjectValue(aDataSet,FieldDefs[FieldNr],FieldDefs[FieldNr].Name,aRecordNode)
    else
      ReadScalar(AField,FieldDefs[FieldNr],aRecordNode,FieldDefs[FieldNr].Name);
    end;
end;

procedure TXMLDatapacketReader.ReadScalar(AField: TField; aDef: TFieldDef; aNode: TDOMNode; const aAttrName: string);
var AFieldNode   : TDomNode;
    s: string;
    ws: widestring;
    lDateTime: TDateTime;
begin
  if AField=nil then
    Exit;
  if Assigned(aNode) then
    AFieldNode := aNode.Attributes.GetNamedItem(aAttrName)
  else
    AFieldNode := nil;
  if assigned(AFieldNode) then
    begin
    s := AFieldNode.NodeValue;
    if (aDef.DataType in [ftBlob, ftBytes, ftVarBytes]) and (s <> '') then
      s := DecodeStringBase64(s);
    case aDef.DataType of
      ftBlob, ftMemo:
        RestoreBlobField(AField, @s[1], length(s));
      ftWideMemo:
        begin
        ws := s;
        RestoreBlobField(AField, @ws[1], length(ws)*sizeof(WideChar));
        end;
      ftString, ftFixedChar:
        AField.AsUTF8String := UTF8Encode(AFieldNode.NodeValue);
      ftDate, ftTime, ftDateTime:
        if TryXMLToDateTime(aDef.DataType, s, lDateTime) then
          AField.AsDateTime := lDateTime
        else
          AField.AsString := s;
      else;
        AField.AsString := s;  // set it to the filterbuffer
    end;
    end
  else
    AField.SetData(nil);
end;

procedure TXMLDatapacketReader.ReadObjectValue(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aParent: TDOMNode);
var lNode, lItem: TDOMNode;
    lElement: TFieldDef;
    lName: string;
    i, lIndex: Integer;
begin
  if Assigned(aParent) then
    lNode := aParent.FindNode(aDef.Name)
  else
    lNode := nil;
  if aDef.DataType=ftADT then
    begin
    ReadElementMembers(aDataSet,aDef,aFullName,lNode);
    Exit;
    end;
  lElement := aDef.ChildDefs[0];
  for i := 0 to aDef.Size-1 do
    begin
    lName := Format('%s[%d]',[aFullName,i]);
    lItem := nil;
    if Assigned(lNode) then
      begin
      lItem := lNode.FirstChild;
      while Assigned(lItem) do
        begin
        if (lItem.NodeType=ELEMENT_NODE) and (lItem.CompareName('ELEMENT')=0)
           and TryStrToInt(GetNodeAttribute(lItem,'index'),lIndex) and (lIndex=i) then
          Break;
        lItem := lItem.NextSibling;
        end;
      end;
    if lElement.DataType=ftADT then
      ReadElementMembers(aDataSet,lElement,lName,lItem)
    else
      ReadScalar(aDataSet.FieldList.Find(lName),lElement,lItem,ElementAttrName(lElement));
    end;
end;

procedure TXMLDatapacketReader.ReadElementMembers(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aNode: TDOMNode);
var i: Integer;
begin
  for i := 0 to aDef.ChildDefs.Count-1 do
    if aDef.ChildDefs[i].DataType in [ftADT, ftArray] then
      ReadObjectValue(aDataSet,aDef.ChildDefs[i],aFullName+'.'+aDef.ChildDefs[i].Name,aNode)
    else
      ReadScalar(aDataSet.FieldList.Find(aFullName+'.'+aDef.ChildDefs[i].Name),aDef.ChildDefs[i],aNode,aDef.ChildDefs[i].Name);
end;

procedure TXMLDatapacketReader.WriteFields(aDataSet: TCustomBufDataset; aRecordNode: TDOMElement);
var FieldNr : Integer;
    AFieldDef: TFieldDef;
    AField: TField;
    ARows: TCustomBufDataset;
    ARowsNode, ARowNode: TDOMElement;
begin
  with aDataSet do for FieldNr := 0 to FieldDefs.Count-1 do
    begin
    AFieldDef := FieldDefs[FieldNr];
    AField := Fields.FieldByNumber(AFieldDef.FieldNo);
    if AFieldDef.DataType=ftDataSet then
      begin
      ARows := NestedRowsDataset(AFieldDef.ChildDefs);
      try
        if LoadNestedRows(AField,ARows) then
          begin
          ARowsNode := XMLDocument.CreateElement(AFieldDef.Name);
          ARowsNode.SetAttribute('AUTOINCVALUE',IntToStr(GetRowsAutoInc(ARows)));
          while not ARows.EOF do
            begin
            ARowNode := XMLDocument.CreateElement('ROW'+AFieldDef.Name);
            WriteFields(ARows,ARowNode);
            ARowsNode.AppendChild(ARowNode);
            ARows.Next;
            end;
          aRecordNode.AppendChild(ARowsNode);
          end;
      finally
        ARows.Free;
      end;
      end
    else if AFieldDef.DataType in [ftADT, ftArray] then
      WriteObjectValue(aDataSet,AFieldDef,AFieldDef.Name,aRecordNode)
    else
      WriteScalar(AField,AFieldDef,aRecordNode,AFieldDef.Name);
    end;
end;

procedure TXMLDatapacketReader.WriteScalar(AField: TField; aDef: TFieldDef; aNode: TDOMElement; const aAttrName: string);
begin
  if (AField=nil) or AField.IsNull then
    Exit;
  if aDef.DataType in [ftBlob, ftBytes, ftVarBytes] then
    aNode.SetAttribute(aAttrName, EncodeStringBase64(AField.AsString))
  else if aDef.DataType in [ftString, ftFixedChar] then
    aNode.SetAttribute(aAttrName, UTF8Decode(AField.AsUTF8String))
  else if aDef.DataType in [ftDate, ftTime, ftDateTime] then
    aNode.SetAttribute(aAttrName, FormatDateTime(XMLDateTimeFormats[aDef.DataType], AField.AsDateTime))
  else
    aNode.SetAttribute(aAttrName, AField.AsString);
end;

procedure TXMLDatapacketReader.WriteObjectValue(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aParent: TDOMElement);
var lNode, lItem: TDOMElement;
    lElement: TFieldDef;
    i: Integer;
begin
  lNode := XMLDocument.CreateElement(aDef.Name);
  if aDef.DataType=ftADT then
    WriteElementMembers(aDataSet,aDef,aFullName,lNode)
  else
    begin
    lElement := aDef.ChildDefs[0];
    for i := 0 to aDef.Size-1 do
      begin
      lItem := XMLDocument.CreateElement('ELEMENT');
      if lElement.DataType=ftADT then
        WriteElementMembers(aDataSet,lElement,Format('%s[%d]',[aFullName,i]),lItem)
      else
        WriteScalar(aDataSet.FieldList.Find(Format('%s[%d]',[aFullName,i])),lElement,lItem,ElementAttrName(lElement));
      if lItem.HasAttributes or lItem.HasChildNodes then
        begin
        lItem.SetAttribute('index',IntToStr(i));
        lNode.AppendChild(lItem);
        end
      else
        lItem.Free;
      end;
    end;
  if lNode.HasAttributes or lNode.HasChildNodes then
    aParent.AppendChild(lNode)
  else
    lNode.Free;
end;

procedure TXMLDatapacketReader.WriteElementMembers(aDataSet: TCustomBufDataset; aDef: TFieldDef; const aFullName: string; aNode: TDOMElement);
var i: Integer;
begin
  for i := 0 to aDef.ChildDefs.Count-1 do
    if aDef.ChildDefs[i].DataType in [ftADT, ftArray] then
      WriteObjectValue(aDataSet,aDef.ChildDefs[i],aFullName+'.'+aDef.ChildDefs[i].Name,aNode)
    else
      WriteScalar(aDataSet.FieldList.Find(aFullName+'.'+aDef.ChildDefs[i].Name),aDef.ChildDefs[i],aNode,aDef.ChildDefs[i].Name);
end;

procedure TXMLDatapacketReader.StoreRecord(ARowState : TRowState; AUpdOrder : integer = 0);
var ARecordNode : TDOMElement;
begin
  inc(FEntryNr);
  ARecordNode := XMLDocument.CreateElement('ROW');
  WriteFields(DataSet,ARecordNode);
  if ARowState<>[] then
    begin
    ARecordNode.SetAttribute('RowState',inttostr(RowStateToByte(ARowState)));
    if AUpdOrder>=length(FChangeLog) then
      setlength(FChangeLog,AUpdOrder+1);
    if (rsvOriginal in ARowState) or (rsvDeleted in ARowState) then
      FChangeLog[AUpdOrder].OrigEntry:=FEntryNr;
    if (rsvDeleted in ARowState) or (rsvUpdated in ARowState) or (rsvInserted in ARowState) then
      FChangeLog[AUpdOrder].NewEntry:=FEntryNr;
    if ARowState=[rsvUpdated] then
      FChangeLog[AUpdOrder].UpdateKind := ukModify;
    if ARowState=[rsvInserted] then
      FChangeLog[AUpdOrder].UpdateKind := ukInsert;
    if ARowState=[rsvDeleted] then
      FChangeLog[AUpdOrder].UpdateKind := ukDelete;
    end;
  FRowDataNode.AppendChild(ARecordNode);
end;

class function TXMLDatapacketReader.RecognizeStream(AStream: TStream): boolean;
const XmlStart = '<?xml';
var s        : ansistring;
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

class function TXMLDatapacketReader.SupportsNestedData: boolean;
begin
  Result := True;
end;

class function TXMLDatapacketReader.SupportsObjectFields: boolean;
begin
  Result := True;
end;

procedure TXMLDatapacketReader.GotoNextRecord;
begin
  FRecordNode := FRecordNode.NextSibling;
  inc(FEntryNr);
  while assigned(FRecordNode) and (FRecordNode.CompareName('ROW')<>0) do
    FRecordNode := FRecordNode.NextSibling;
end;

initialization
  RegisterDatapacketReader(TXMLDatapacketReader,dfXML);
end.
