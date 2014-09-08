{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements binary serialization. The payload is architecture free.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_serialization;

interface

uses
  SysUtils, Classes, Contnrs,
  sdo, sdo_types,
  sdo_serialization_utils, sdo_serialization_xml;

const
  MAX_NESTED_LEVEL = 256;
    
type

  TObjectWriteOption = ( owoWriteChangeSummary, owoWriteReference, owoPostRefProperties );
  TObjectWriteOptions = set of TObjectWriteOption;
  TObjectReadOption = ( oroDontMake_BeginObjectRead, oroDontMake_EndScopeRead );
  TObjectReadOptions = set of TObjectReadOption;
  TReferenceObjectRecallItemType = ( rritPropertyValue, rritSettingItem );
  TPropListWriterProc = procedure (const AName : string; const AValueList : ISDODataObjectList) of object;
  TPropWriterProc = procedure (const AProp : ISDOProperty; const AObject : ISDODataObject) of object;
  TPropListReaderProc = procedure (const AName : string; const AValueList : ISDODataObjectList) of object;
  TPropReaderProc = procedure (const AProp : ISDOProperty; const AObject : ISDODataObject) of object;
  TPropHandlerInfo = record
    PropType : ISDOType;
    WriterProc     : TPropWriterProc;
    WriterListProc : TPropListWriterProc;
    ReaderProc     : TPropReaderProc;
    ReaderListProc : TPropListReaderProc;
  end;
  TSDOSerializer = class(TInterfacedObject, IInterface, ISDOSerializer)
  private
    FDataFactory : ISDODataFactory;
    FStreamer : ISDOSerializerStream;
    FNamespace : string;
    FWriteStarted : Boolean;
    FWriteEnded : Boolean;
    FProcs : array[TSDOTypeKind] of TPropHandlerInfo;
  private
    FSerializationStyleStack : array[0..(MAX_NESTED_LEVEL-1)] of TSerializationStyle;
    FSerializationStyleStackIndex : PtrInt;
    FRootName : string;
    FReferenceWriteRecallList : TObjectList;
    FReferenceReadRecallList : TObjectList;
    FChangeSummaryList : ISDODataObject;
    FChangeSummary : ISDOChangeSummary;
    FOptions : TSerializerOptions;
  private
    procedure Init();
    procedure PushSerializationStyle(const AValue : TSerializationStyle);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PopSerializationStyle();{$IFDEF USE_INLINE}inline;{$ENDIF}
    function getType(const AKind : TSDOTypeKind) : ISDOType;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function getSerializationStyle(const AProperty : ISDOProperty) : TSerializationStyle;{$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure WriteMetadata();
    procedure BeginWrite();
    procedure EndWrite();

    procedure ReadMetadata();

    procedure WriteBoolListProp(const AName : string; const AValueList : ISDODataObjectList);
    procedure WriteByteListProp(const AName : string; const AValueList : ISDODataObjectList);
{$IFDEF HAS_SDO_BYTES}
    procedure WriteBytesListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure WriteCharListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure WriteCurrencyListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_CURRENCY}
    procedure WriteDateListProp(const AName : string; const AValueList : ISDODataObjectList);
{$IFDEF HAS_SDO_DOUBLE}
    procedure WriteDoubleListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure WriteFloatListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_FLOAT}
    procedure WriteIntegerListProp(const AName : string; const AValueList : ISDODataObjectList);
{$IFDEF HAS_SDO_LONG}
    procedure WriteLongListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure WriteShortListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_SHORT}
    procedure WriteStringListProp(const AName : string; const AValueList : ISDODataObjectList);

    procedure WriteBoolProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
    procedure WriteByteProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$IFDEF HAS_SDO_BYTES}
    procedure WriteBytesProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure WriteCharProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure WriteCurrencyProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_CURRENCY}
    procedure WriteDateProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$IFDEF HAS_SDO_DOUBLE}
    procedure WriteDoubleProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure WriteFloatProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_FLOAT}
    procedure WriteIntegerProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$IFDEF HAS_SDO_LONG}
    procedure WriteLongProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure WriteShortProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_SHORT}
    procedure WriteStringProp(const AProp : ISDOProperty; const AObject : ISDODataObject);

    procedure ReadBoolProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
    procedure ReadByteProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$IFDEF HAS_SDO_BYTES}
    procedure ReadBytesProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure ReadCharProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure ReadCurrencyProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_CURRENCY}
    procedure ReadDateProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$IFDEF HAS_SDO_DOUBLE}
    procedure ReadDoubleProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure ReadFloatProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_FLOAT}
    procedure ReadIntegerProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$IFDEF HAS_SDO_LONG}
    procedure ReadLongProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure ReadShortProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
{$ENDIF HAS_SDO_SHORT}
    procedure ReadStringProp(const AProp : ISDOProperty; const AObject : ISDODataObject);

    procedure ReadBoolListProp(const AName : string; const AValueList : ISDODataObjectList);
    procedure ReadByteListProp(const AName : string; const AValueList : ISDODataObjectList);
{$IFDEF HAS_SDO_BYTES}
    procedure ReadBytesListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure ReadCharListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure ReadCurrencyListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_CURRENCY}
    procedure ReadDateListProp(const AName : string; const AValueList : ISDODataObjectList);
{$IFDEF HAS_SDO_DOUBLE}
    procedure ReadDoubleListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure ReadFloatListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_FLOAT}
    procedure ReadIntegerListProp(const AName : string; const AValueList : ISDODataObjectList);
{$IFDEF HAS_SDO_LONG}
    procedure ReadLongListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure ReadShortListProp(const AName : string; const AValueList : ISDODataObjectList);
{$ENDIF HAS_SDO_SHORT}
    procedure ReadStringListProp(const AName : string; const AValueList : ISDODataObjectList);

    function InternalReadObject(
      const AName : string;
      const AType : ISDOType;
      const AOwner,
            ADataObject : ISDODataObject;
      const AOptions : TObjectReadOptions
    ) : ISDODataObject;
    procedure InternalWriteObject(
      const AName : string;
      const AObject : ISDODataObject;
      const AOptions : TObjectWriteOptions;
      const AReference : TSDOString
    );
    procedure AddPropertyRefRecallWrite(
      const ARecallType : TReferenceObjectRecallItemType;
      const ADataObject : ISDODataObject;
      const AProperty : ISDOProperty;
      const AValue : ISDODataObject
    );
    function AddToChangeSummaryList(const ADataObject : ISDODataObject) : TSDOString;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure WritePendingRefProps();
    procedure ReadPendingRefProps();
    procedure WriteChangeSummaryMultiValueProps(
      const ACS : ISDOChangeSummary;
      const ADataObject : ISDODataObject
    );
    function getObjectPathInChangeSummary(const AObj : ISDODataObject) : TSDOString;
  protected
    function  GetDataFactory() : ISDODataFactory;
    function  GetStreamer() : ISDOSerializerStream;
    procedure WriteObject(
      const AName : string;
      const AObject : ISDODataObject;
      const AOptions : TObjectWriteOptions;
      const AReference : string
    );
    function ReadObject(
      const AName : string;
      const AType : ISDOType;
      const AOwner : ISDODataObject
    ) : ISDODataObject;
    procedure WriteChangeSummary(
      const AValue : ISDOChangeSummary;
      const ARootObjectName : string
    );
    procedure ReadChangeSummary(
      const AValue : ISDOChangeSummary;
      const ARootObjectName : string;
      const ARootObject : ISDODataObject
    );

    // ISDOSerializer implementation
    procedure save(
      const AName : string;
            AObject : ISDODataObject;
      const ADestStream : TStream
    );overload;
    procedure save(
            AObject : ISDODataObject;
      const ADestStream : TStream
    );overload;
    procedure save(
      const AName : string;
            AObject : ISDODataObject;
      const AFileName : string
    );overload;
    procedure save(
            AObject : ISDODataObject;
      const AFileName : string
    );overload;

    procedure load(
      const AStream : TStream;
            ADestList : ISDODataObjectList
    );overload;
    procedure load(
      const AFileName : string;
            ADestList : ISDODataObjectList
    );overload;
    function load(const AStream : TStream) : ISDODataObject;overload;
    function load(const AFileName : string) : ISDODataObject;overload;

    // these are the implementation's extensions
    function getOptions() : TSerializerOptions;
    procedure setOptions(const AValue : TSerializerOptions);
  public
    constructor Create(
      ADataFactory : ISDODataFactory;
      AStreamer    : ISDOSerializerStream
    );
    destructor Destroy();override;
  end;


implementation

uses
  TypInfo, sdo_consts, sdo_xsd_helper,
  StrUtils, sdo_imp_utils, sdo_xpath_helper, sdo_changesummary,
  sdo_utils, sdo_dataobject;

const
  //TManyValuePropAction = ( mvpaAppend, mvpaInsert, mvpaChange, mvpaDelete );
  PROP_ACTION_STRING : array[TManyValuePropAction] of TSDOString = (
                         s_append, s_insert, s_change, s_delete
                       );

function FindAction(const AString : string; out ARes : TManyValuePropAction) : Boolean;
var
  k : TManyValuePropAction;
begin
  Result := False;
  for k := Low(TManyValuePropAction) to High(TManyValuePropAction) do begin
    if AnsiSameText(AString,PROP_ACTION_STRING[k]) then begin
      ARes := k;
      Result := True;
      Break;
    end;
  end;
end;

function getObjectPath(
  const ADataObject : ISDODataObject;
  const ARootObjectName : string
) : TSDOString ;
var
  locStrBuffer : TSDOString;
begin
  locStrBuffer := Trim(getXpath(ADataObject));
  if IsStrEmpty(locStrBuffer) then
    locStrBuffer := Format('#/%s',[ARootObjectName])
  else
    locStrBuffer := Format('#/%s/%s',[ARootObjectName,locStrBuffer]);
  Result := locStrBuffer;
end;

type
  TReferenceObjectWriteRecall = class
  private
    FValue: ISDODataObject;
    FDataObject: ISDODataObject;
    FProp: ISDOProperty;
    FStreamPos: TStreamBookmark;
    FRecallType: TReferenceObjectRecallItemType;
  public
    destructor Destroy();override;
    property StreamPos : TStreamBookmark read FStreamPos;
    property RecallType : TReferenceObjectRecallItemType read FRecallType;
    property DataObject : ISDODataObject read FDataObject;
    property Prop : ISDOProperty read FProp;
    property Value : ISDODataObject read FValue;
  end;

{ TReferenceObjectWriteRecall }

destructor TReferenceObjectWriteRecall.Destroy();
begin
  FreeAndNil(FStreamPos);
  inherited;
end;

{ TSDOSerializer }

procedure TSDOSerializer.BeginWrite();
begin
  if not FWriteStarted then begin
    FWriteStarted := True;

    FStreamer.SetSerializationStyle(ssAttibuteSerialization);
    FStreamer.SetNameStyle(nsQualified);
    try
      FStreamer.BeginObject(s_datagraph,FDataFactory.getType(sdo_namespace,s_datagraph));
    finally
      FStreamer.SetNameStyle(nsUnqualified);
    end;
    if not (soExcludeSchema in FOptions) then
      WriteMetadata();
  end;
end;

constructor TSDOSerializer.Create(
  ADataFactory : ISDODataFactory;
  AStreamer    : ISDOSerializerStream
);
begin
  if ( ADataFactory = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataFactory');
  if ( AStreamer = nil ) then
    raise ESDOIllegalArgumentException.Create('AStreamer');
  FDataFactory := ADataFactory;
  FStreamer := AStreamer;
  FReferenceWriteRecallList := TObjectList.Create(True);
  FReferenceReadRecallList := TObjectList.Create(True);
  
  FProcs[BooleanType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BooleanType]);
  FProcs[BooleanType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteBoolProp;
  FProcs[BooleanType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteBoolListProp;
  FProcs[BooleanType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadBoolProp;
  FProcs[BooleanType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadBoolListProp;

  FProcs[ByteType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ByteType]);
  FProcs[ByteType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteByteProp;
  FProcs[ByteType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteByteListProp;
  FProcs[ByteType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadByteProp;
  FProcs[ByteType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadByteListProp;

{$IFDEF HAS_SDO_BYTES}
  FProcs[BytesType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[BytesType]);
  FProcs[BytesType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteBytesProp;
  FProcs[BytesType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteBytesListProp;
  FProcs[BytesType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadBytesProp;
  FProcs[BytesType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadBytesListProp;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
  FProcs[CharacterType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[CharacterType]);
  FProcs[CharacterType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteCharProp;
  FProcs[CharacterType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteCharListProp;
  FProcs[CharacterType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadCharProp;
  FProcs[CharacterType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadCharListProp;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
  FProcs[CurrencyType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[CurrencyType]);
  FProcs[CurrencyType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteCurrencyProp;
  FProcs[CurrencyType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteCurrencyListProp;
  FProcs[CurrencyType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadCurrencyProp;
  FProcs[CurrencyType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadCurrencyListProp;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
  FProcs[DoubleType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[DoubleType]);
  FProcs[DoubleType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteDoubleProp;
  FProcs[DoubleType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteDoubleListProp;
  FProcs[DoubleType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadDoubleProp;
  FProcs[DoubleType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadDoubleListProp;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
  FProcs[FloatType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[FloatType]);
  FProcs[FloatType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteFloatProp;
  FProcs[FloatType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteFloatListProp;
  FProcs[FloatType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadFloatProp;
  FProcs[FloatType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadFloatListProp;
{$ENDIF HAS_SDO_FLOAT}

  FProcs[DateTimeType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[DateTimeType]);
  FProcs[DateTimeType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteDateProp;
  FProcs[DateTimeType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteDateListProp;
  FProcs[DateTimeType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadDateProp;
  FProcs[DateTimeType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadDateListProp;

  FProcs[IntegerType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]);
  FProcs[IntegerType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteIntegerProp;
  FProcs[IntegerType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteIntegerListProp;
  FProcs[IntegerType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadIntegerProp;
  FProcs[IntegerType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadIntegerListProp;

{$IFDEF HAS_SDO_LONG}
  FProcs[LongType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[LongType]);
  FProcs[LongType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteLongProp;
  FProcs[LongType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteLongListProp;
  FProcs[LongType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadLongProp;
  FProcs[LongType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadLongListProp;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
  FProcs[ShortType].PropType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ShortType]);
  FProcs[ShortType].WriterProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteShortProp;
  FProcs[ShortType].WriterListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteShortListProp;
  FProcs[ShortType].ReaderProc     := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadShortProp;
  FProcs[ShortType].ReaderListProc := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadShortListProp;
{$ENDIF HAS_SDO_SHORT}

  FProcs[StringType].PropType  := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]);
  FProcs[StringType].WriterProc      := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteStringProp;
  FProcs[StringType].WriterListProc  := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}WriteStringListProp;
  FProcs[StringType].ReaderProc      := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadStringProp;
  FProcs[StringType].ReaderListProc  := {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}ReadStringListProp;
end;

destructor TSDOSerializer.Destroy();
begin
  try
    EndWrite();
  finally
    FreeAndNil(FReferenceWriteRecallList);
    FreeAndNil(FReferenceReadRecallList);
  end;
  inherited;
end;

procedure TSDOSerializer.EndWrite();
begin
  if FWriteStarted then begin
    if not FWriteEnded then begin
      FWriteEnded := True;
      FStreamer.EndScope();
    end;
  end;
end;

function TSDOSerializer.GetDataFactory() : ISDODataFactory;
begin
  Result := FDataFactory;
end;

function TSDOSerializer.GetStreamer() : ISDOSerializerStream;
begin
  Result := FStreamer;
end;

function TSDOSerializer.getType(const AKind: TSDOTypeKind): ISDOType;
begin
  Result := FProcs[AKind].PropType;
end;

procedure TSDOSerializer.Init();
  procedure ClearChangeSummaryList();
  var
    pl : ISDOPropertyList;
    p : ISDOProperty;
    ls : ISDODataObjectList;
    k, kx : PtrInt;
  begin
    pl := FChangeSummaryList.getInstanceProperties();
    for k := 0 to Pred(pl.getCount()) do begin
      p := pl.getItem(k);
      if p.getType().isDataObjectType() then begin
        if not p.isMany() then begin
          FChangeSummaryList.setDataObject(p,nil);
        end else begin
          ls := FChangeSummaryList.getList(p);
          kx := ls.size();
          if ( kx > 0 ) then begin
            for kx := 1 to kx do
              ls.delete(0);
          end;
        end;
      end;
    end;
  end;

begin
  if ( FChangeSummaryList <> nil ) then begin
    ClearChangeSummaryList();
    FChangeSummaryList := nil;
  end;
  FWriteStarted := False;
  FWriteEnded := False;
  FSerializationStyleStackIndex := -1;
  FNamespace := '';
  if Assigned(FStreamer) then
    FStreamer.Clear();
  FReferenceWriteRecallList.Clear();
  FReferenceReadRecallList.Clear();
end;

procedure TSDOSerializer.load(const AStream: TStream; ADestList: ISDODataObjectList);
var
  i, c : PtrInt;
  scopeList : TStringList;
  s : string;
  obj : ISDODataObject;
begin
  Init();
  try
    FStreamer.LoadFromStream(AStream);
    scopeList := TStringList.Create();
    try
      if ( FStreamer.GetScopeItemNames(ssNodeSerialization,scopeList) > 0 ) then begin
        if ( scopeList.IndexOf(s_xsd) >= 0 ) then
          ReadMetadata();
        c := scopeList.Count;
        if ( c > 0 ) then begin
          FStreamer.SetSerializationStyle(ssAttibuteSerialization);
          for i := 0 to Pred(c) do begin
            s := scopeList[i];
            if ( AnsiIndexStr(s,[s_xsd,s_changesummary]) < 0 ) then begin
              FStreamer.SetNameStyle(nsQualified);
              obj := ReadObject(s, nil, nil);
              ADestList.append(obj);
              scopeList.Objects[i] := TObject(obj);
            end;
          end;
          ReadPendingRefProps();
          if ( scopeList.IndexOf(s_changesummary) > -1 ) then begin
            c := ADestList.size();
            obj := nil;
            for i := 0 to Pred(c) do begin
              if ( ADestList.getDataObject(i).getChangeSummary() <> nil ) then begin
                obj := ADestList.getDataObject(i);
                Break;
              end;
            end;
            if ( obj = nil ) then
              raise ESDOSerializationException.Create('Invalid data stream : the stream contains a changes summary and no object to hold it.');
            i := scopeList.IndexOfObject(TObject(obj));
            ReadChangeSummary(obj.getChangeSummary(),scopeList[i],obj);
          end;
        end;
      end;
    finally
      scopeList.Free();
    end;
  finally
    Init();
  end;
end;

procedure TSDOSerializer.load(const AFileName : string; ADestList : ISDODataObjectList);
var
  flStream : TStream;
begin
  if not FileExists(AFileName) then
    raise ESDOSerializationException.CreateFmt(SMSG_FileNotFound,[AFileName]);
  flStream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    load(flStream,ADestList);
  finally
    flStream.Free();
  end;
end;

function TSDOSerializer.load(const AStream: TStream) : ISDODataObject;
var
  ls : ISDODataObjectList;
begin
  ls := TSDODataObjectList.Create(FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])) as ISDODataObjectList;
  load(AStream,ls);
  if ( ls.size() > 0 ) then
    Result := ls.getDataObject(0)
  else
    raise ESDOSerializationException.Create('No object found in this stream.');
end;

type
  TMultiValueRecallData = packed record
    Action : TManyValuePropAction;
    ActionIndex : PtrInt;
    Index : PtrInt;
  end;

  TReferenceObjectReadRecall = class
  private
    FDataObject: ISDODataObject;
    FRoot: ISDODataObject;
    FReferenceProperty: ISDOProperty;
    FAccessPath: string;
    FRecallType: TReferenceObjectRecallItemType;
    FIsSet: Boolean;
    FMultiValueData: TMultiValueRecallData;
  public
    constructor Create(
      const ARecallType : TReferenceObjectRecallItemType;
      const ARoot : ISDODataObject;
      const ADataObject : ISDODataObject;
      const AReferenceProperty : ISDOProperty;
      const AAccessPath : string;
      const AIsSet : Boolean
    );
    property IsSet : Boolean read FIsSet;
    property RecallType : TReferenceObjectRecallItemType read FRecallType;
    property Root : ISDODataObject read FRoot;
    property DataObject : ISDODataObject read FDataObject;
    property ReferenceProperty : ISDOProperty read FReferenceProperty;
    property AccessPath : string read FAccessPath;
    property MultiValueData : TMultiValueRecallData read FMultiValueData;
  end;

procedure TSDOSerializer.AddPropertyRefRecallWrite(
  const ARecallType : TReferenceObjectRecallItemType;
  const ADataObject: ISDODataObject;
  const AProperty: ISDOProperty;
  const AValue: ISDODataObject
);
var
  locRecall : TReferenceObjectWriteRecall;
begin
  locRecall := TReferenceObjectWriteRecall.Create();
  try
    locRecall.FRecallType := ARecallType;
    locRecall.FDataObject := ADataObject;
    locRecall.FProp := AProperty;
    locRecall.FStreamPos := FStreamer.GetBookMark();
    locRecall.FValue := AValue
  except
    FreeAndNil(locRecall);
    raise;
  end;
  FReferenceWriteRecallList.Add(locRecall);
end;

procedure TSDOSerializer.ReadPendingRefProps();

  function FindDataObject(
    const AItem : TReferenceObjectReadRecall;
    const APath : string;
    out AIndex : PtrInt;
    const AContainingRoot : ISDODataObject = nil
  ) : ISDODataObject;
  var
    locExpr : TXPathExpression;
    locProc : TXPathProcessor;
    locCtx : TXPathExecContext;
  begin
    locProc := nil;
    locExpr := TXPathExpression.Create();
    try
      locExpr.SetRoot(ParseXPath(APath));
      locProc := TXPathProcessor.Create();
      if ( AContainingRoot = nil ) then
        locProc.Context.SetObject(AItem.Root,nil)
      else
        locProc.Context.SetObject(AContainingRoot,nil);
      locProc.Execute(locExpr);
      locCtx := locProc.Context;
      if ( locCtx.PropertyOwner = nil ) or ( locCtx.CurrentProperty = nil ) then
        raise ESDOInvalidPathException.Create(APath);
      if locCtx.CurrentProperty.isMany() then begin
        Result := locCtx.ListItem.getDataObject();
        AIndex := locCtx.ListItem.getCursor().GetPosition();
      end else begin
        Result := locCtx.PropertyOwner.getDataObject(locCtx.CurrentProperty);
        AIndex := 0;
      end;
    finally
      locProc.Free();
      locExpr.Free();
    end;
  end;

  procedure InternalReadPendingRefProps_Properties();
  var
    k : PtrInt;
    itm : TReferenceObjectReadRecall;
    tmpBuffer : string;
    tmpObject : ISDODataObject;
    i : PtrInt;
  begin
    if ( FReferenceReadRecallList.Count > 0 ) then begin
      for k := 0 to Pred(FReferenceReadRecallList.Count) do begin
        itm := TReferenceObjectReadRecall(FReferenceReadRecallList[k]);
        if ( itm.RecallType = rritPropertyValue ) then begin
          tmpBuffer := xpath_ExcludeRootElement(itm.AccessPath,FRootName);
          if ( Pos(s_changeSummary + '/',tmpBuffer) = 1 ) then
            tmpObject := FChangeSummaryList.getDataObject(Copy(tmpBuffer, ( Length(s_changeSummary) + 1 {/} ), MaxInt))
          else
            tmpObject := FindDataObject(itm,tmpBuffer,i);
          itm.DataObject.setDataObject(itm.ReferenceProperty,tmpObject)
        end;
      end;
    end;
  end;

  procedure InternalReadPendingRefProps_Settings();
  var
    k : PtrInt;
    itm : TReferenceObjectReadRecall;
    tmpBuffer : string;
    tmpObject : ISDODataObject;
    ls : ISDOChangedDataObjectList;
    lsX : ISDOChangedDataObjectListEx;
    i, q, loc_index : PtrInt;
  begin
    if ( FReferenceReadRecallList.Count > 0 ) then begin
      for k := 0 to Pred(FReferenceReadRecallList.Count) do begin
        itm := TReferenceObjectReadRecall(FReferenceReadRecallList[k]);
        if ( itm.RecallType = rritSettingItem ) then begin
          tmpBuffer := xpath_ExcludeRootElement(itm.AccessPath,FRootName);
          if ( Pos(s_changeSummary + '/',tmpBuffer) = 1 ) then begin
            tmpBuffer := Copy(tmpBuffer, ( Length(s_changeSummary) + 1 {/} + 1 ), MaxInt);
            tmpObject := FindDataObject(itm,tmpBuffer,loc_index,FChangeSummaryList);
            loc_index := 0;
          end else begin
            tmpObject := FindDataObject(itm,tmpBuffer,loc_index);
          end;
          ls := itm.Root.getChangeSummary.getChangedDataObjects();
          if ( ls = nil ) then
            raise ESDOSerializationException.Create('Unable to find ChangeSummary.');
          lsX := ls as ISDOChangedDataObjectListEx;
          if not lsX.find(itm.DataObject,i) then
            raise ESDOSerializationException.Create('Invalid ChangeSummary stream.');
          if ( tmpObject <> nil ) and itm.ReferenceProperty.isContainment() then begin
            (tmpObject as ISDODataObjectEx).setContainer(itm.DataObject, itm.ReferenceProperty);
            if lsX.find(tmpObject,q) then
              lsX.getInfo(q).CaptureOldContainment();
          end;
          if itm.ReferenceProperty.isMany() then
            lsX.getInfo(i).GetChanges(itm.ReferenceProperty).AddAt(itm.MultiValueData.Index,itm.MultiValueData.Action,itm.MultiValueData.ActionIndex,tmpObject)
          else
            lsX.getInfo(i).ChangeList.append(TValueSetting.Create(itm.IsSet, (tmpObject = nil), tmpObject, itm.ReferenceProperty,loc_index));
        end;
      end;
    end;
  end;

begin
  InternalReadPendingRefProps_Properties();
  InternalReadPendingRefProps_Settings();
  FReferenceReadRecallList.Clear();
end;

function TSDOSerializer.AddToChangeSummaryList(const ADataObject: ISDODataObject): TSDOString;
var
  objTypeName : TSDOString;
  pls : ISDODataObjectList;
begin
  objTypeName := ADataObject.getType().getName();
  if ( FChangeSummaryList.getInstanceProperties().find(objTypeName) = nil ) then
    FDataFactory.addProperty(FChangeSummaryList,objTypeName,ADataObject.getType(),[pfIsMany]);
  pls := FChangeSummaryList.getList(objTypeName);
  pls.append(ADataObject);
  Result := Format('#/%s/%s[%d]',[s_changeSummary,objTypeName,pls.getCursor().GetPosition()]);
end;

procedure TSDOSerializer.WriteChangeSummaryMultiValueProps(
  const ACS: ISDOChangeSummary;
  const ADataObject: ISDODataObject
);
var
  locIntType, locStringType : ISDOType;

  procedure WriteItem_non_ref_type(const AItem : TManyValuePropChanges);
  var
    q, k : PtrInt;
    ch : TManyValuePropRecordData;
    pt : ISDOType;
  begin
    q := AItem.Count;
    if ( q > 0 ) then begin
      pt := AItem.Prop.getType();
      FStreamer.BeginObject(AItem.Prop.getName(), pt);
        FStreamer.BeginArray(s_listChanges, pt, [0,(q-1)]);
          for k := 0 to Pred(q) do begin
            FStreamer.BeginObject(s_listChanges, pt);
              ch := AItem.GetItem(k);
              PushSerializationStyle(ssAttibuteSerialization);
                FStreamer.Put(s_index, locIntType, ch.Index);
                FStreamer.Put(s_kind, locStringType, PROP_ACTION_STRING[ch.Action]);
                if ( ch.Action in [mvpaChange, mvpaDelete] ) then
                  FStreamer.Put(s_dataValues, pt, ch.Value);
              PopSerializationStyle();
            FStreamer.EndScope();
          end;
        FStreamer.EndScope();
      FStreamer.EndScope();
    end;
  end;

  procedure WriteItem_object_type(const AItem : TManyValuePropChanges);
  var
    q, k : PtrInt;
    ch : TManyValuePropRecordData;
    pt : ISDOType;
  begin
    q := AItem.Count;
    if ( q > 0 ) then begin
      pt := AItem.Prop.getType();
      FStreamer.BeginObject(AItem.Prop.getName(), pt);
        FStreamer.BeginArray(s_listChanges, pt, [0,(q-1)]);
          for k := 0 to Pred(q) do begin
            FStreamer.BeginObject(s_listChanges, pt);
              ch := AItem.GetItem(k);
              PushSerializationStyle(ssAttibuteSerialization);
                FStreamer.Put(s_index, locIntType, ch.Index);
                FStreamer.Put(s_kind, locStringType, PROP_ACTION_STRING[ch.Action]);
              PopSerializationStyle();
              if ( ch.Action in [mvpaChange, mvpaDelete] ) then
                AddPropertyRefRecallWrite(rritSettingItem,ADataObject,AItem.Prop,ch.Value.ObjectValue^);
            FStreamer.EndScope();
          end;
        FStreamer.EndScope();
      FStreamer.EndScope();
    end;
  end;

  procedure WriteItem_simple_ref_type(const AItem : TManyValuePropChanges);
  var
    q, k : PtrInt;
    ch : TManyValuePropRecordData;
    pt : ISDOType;
  begin
    q := AItem.Count;
    if ( q > 0 ) then begin
      pt := AItem.Prop.getType();
      FStreamer.BeginObject(AItem.Prop.getName(), pt);
        FStreamer.BeginArray(s_listChanges, pt, [0,(q-1)]);
          for k := 0 to Pred(q) do begin
            FStreamer.BeginObject(s_listChanges, pt);
              ch := AItem.GetItem(k);
              PushSerializationStyle(ssAttibuteSerialization);
                FStreamer.Put(s_index, locIntType, ch.Index);
                FStreamer.Put(s_kind, locStringType, PROP_ACTION_STRING[ch.Action]);
              PopSerializationStyle();
              if ( ch.Action in [mvpaChange, mvpaDelete] ) then begin
                case pt.getTypeEnum() of
                  StringType   :  FStreamer.Put(s_dataValues, pt, ch.Value.StringValue^);
  {$IFDEF HAS_SDO_BYTES}
                  BytesType    :  FStreamer.Put(s_dataValues, pt, ch.Value.BytesValue^);
  {$ENDIF HAS_SDO_BYTES}
                end;
              end;
            FStreamer.EndScope();
          end;
        FStreamer.EndScope();
      FStreamer.EndScope();
    end;
  end;

var
  i, c : PtrInt;
  ls : TManyValuePropChangesList;
  itm : TManyValuePropChanges;
begin
  ls := (ACS as ISDOChangeSummaryEx).FindManyValueChanges(ADataObject);
  if ( ls <> nil ) and ( ls.Count > 0 ) then begin
    locIntType := getType(IntegerType);
    locStringType := getType(StringType);
    c := ls.Count;
    for i := 0 to ( c - 1 ) do begin
      itm := ls[i];
      case itm.Prop.getTypeEnum() of
        BooleanType, ByteType
        {$IFDEF HAS_SDO_CHAR}, CharacterType{$ENDIF}
        {$IFDEF HAS_SDO_CURRENCY}, CurrencyType{$ENDIF}
        ,DateTimeType
        {$IFDEF HAS_SDO_DOUBLE},DoubleType{$ENDIF}
        {$IFDEF HAS_SDO_FLOAT},FloatType{$ENDIF}
        ,IntegerType
        {$IFDEF HAS_SDO_LONG},LongType{$ENDIF}
        {$IFDEF HAS_SDO_SHORT},ShortType{$ENDIF} :
          begin
            WriteItem_non_ref_type(itm);
          end;
        ObjectType : WriteItem_object_type(itm);
{$IFDEF HAS_SDO_BYTES}
        BytesType,
{$ENDIF HAS_SDO_BYTES}
        StringType :
          begin
            WriteItem_simple_ref_type(itm);
          end;
        else
          Assert(False, 'NOT IMPLEMENTED YET!');
      end;
    end;
  end;
end;

function TSDOSerializer.getSerializationStyle(const AProperty: ISDOProperty): TSerializationStyle;
begin
  //Result := ssAttibuteSerialization;
  if AProperty.isAttribute() then
    Result := ssAttibuteSerialization
  else
    Result := ssNodeSerialization;
end;

procedure TSDOSerializer.WriteObject(
  const AName: string;
  const AObject: ISDODataObject;
  const AOptions: TObjectWriteOptions;
  const AReference: string
);
begin
  FRootName := AName;
  InternalWriteObject(AName,AObject,AOptions,AReference);
end;

function TSDOSerializer.getObjectPathInChangeSummary(const AObj: ISDODataObject): TSDOString;
var
  r : TSDOString;
  x, p : ISDODataObject;
  prp : ISDOProperty;
  ls : ISDODataObjectList;
  locPos : PtrInt;
begin
  r := '';
  x := AObj;
  if not FChangeSummary.isDeleted(x) then begin
    p := x.getContainer();
    while ( p <> nil ) do begin
      prp := x.getContainmentProperty();
      if prp.isMany() then
        r := Format('%s[%d]/%s',[prp.getName(),indexOf(x,p.getList(prp)),r])
      else
        r := prp.getName() + '/' + r;
      x := p;
      p := x.getContainer();
    end;
    prp := FChangeSummaryList.getInstanceProperties().find(x.getType().getName());
    if ( prp <> nil ) then begin
      ls := FChangeSummaryList.getList(prp);
      locPos := indexOf(x,ls) ;
      if ( locPos > -1 ) then begin
        if ( r = '' ) then
          r := Format('#/%s/%s[%d]',[s_changeSummary,x.getType().getName(),locPos])
        else
          r := Format('#/%s/%s[%d]/%s',[s_changeSummary,x.getType().getName(),locPos,r]);
      end;
    end;
  end else begin
    p := x.getContainer();
    while (p <> nil) and FChangeSummary.isDeleted(p) do begin
      prp := x.getContainmentProperty();
      if prp.isMany() then
        r := Format('%s[%d]/%s',[prp.getName(),indexOf(x,p.getList(prp)),r])
      else
        r := prp.getName() + '/' + r;
      x := p;
      p := x.getContainer();
    end;
    prp := FChangeSummaryList.getInstanceProperties().find(x.getType().getName());
    if ( prp <> nil ) then begin
      ls := FChangeSummaryList.getList(prp);
      locPos := indexOf(x,ls) ;
      if ( locPos > -1 ) then begin
        if ( r = '' ) then
          r := Format('#/%s/%s[%d]',[s_changeSummary,x.getType().getName(),locPos])
        else
          r := Format('#/%s/%s[%d]/%s',[s_changeSummary,x.getType().getName(),locPos,r]);
      end;
    end;
  end;
  Result := r;
end;

function TSDOSerializer.getOptions() : TSerializerOptions;
begin
  Result := FOptions;
end;

procedure TSDOSerializer.WriteByteListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOByte;
begin
  tmpVal := AValueList.getByte();
  FStreamer.Put(AName,FProcs[ByteType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteByteProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOByte;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getByte(AProp);
    FStreamer.Put(AProp.getName(),FProcs[ByteType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadByteProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOByte;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[ByteType].PropType,localName,tmpVal) then
    AObject.setByte(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadByteListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOByte;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[ByteType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;

procedure TSDOSerializer.WriteDateProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDODateTime;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getDate(AProp);
    FStreamer.Put(AProp.getName(),FProcs[DateTimeType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.WriteDateListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDODateTime;
begin
  tmpVal := AValueList.getDate();
  FStreamer.Put(AName,FProcs[DateTimeType].PropType,tmpVal);
end;

procedure TSDOSerializer.ReadDateProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDODateTime;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[DateTimeType].PropType,localName,tmpVal) then
    AObject.setDate(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadDateListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDODateTime;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[DateTimeType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDOSerializer.WriteBytesListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOBytes;
begin
  tmpVal := AValueList.getBytes();
  FStreamer.Put(AName,FProcs[BytesType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteBytesProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOBytes;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getBytes(AProp);
    FStreamer.Put(AProp.getName(),FProcs[BytesType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadBytesProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOBytes;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[BytesType].PropType,localName,tmpVal) then
    AObject.setBytes(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadBytesListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOBytes;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[BytesType].PropType,localName,tmpVal) then
    AValueList.appendBytes(tmpVal);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDOSerializer.WriteCharListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOChar;
begin
  tmpVal := AValueList.getCharacter();
  FStreamer.Put(AName,FProcs[CharacterType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteCharProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOChar;
begin
  if not AObject.isNull(AProp) then begin
    tmpVal := AObject.getCharacter(AProp);
    FStreamer.Put(AProp.getName(),FProcs[CharacterType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadCharProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOChar;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[CharacterType].PropType,localName,tmpVal) then
    AObject.setCharacter(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);            
end;

procedure TSDOSerializer.ReadCharListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOChar;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[CharacterType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDOSerializer.WriteCurrencyListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOCurrency;
begin
  tmpVal := AValueList.getCurrency();
  FStreamer.Put(AName,FProcs[CurrencyType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteCurrencyProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOCurrency;
begin                   
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getCurrency(AProp);
    FStreamer.Put(AProp.getName(),FProcs[CurrencyType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadCurrencyProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOCurrency;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[CurrencyType].PropType,localName,tmpVal) then
    AObject.setCurrency(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadCurrencyListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOCurrency;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[CurrencyType].PropType,localName,tmpVal) then
    AValueList.appendCurrency(tmpVal);
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
procedure TSDOSerializer.WriteDoubleListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDODouble;
begin
  tmpVal := AValueList.getDouble();
  FStreamer.Put(AName,FProcs[DoubleType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteDoubleProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDODouble;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getDouble(AProp);
    FStreamer.Put(AProp.getName(),FProcs[DoubleType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadDoubleProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDODouble;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[DoubleType].PropType,localName,tmpVal) then
    AObject.setDouble(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadDoubleListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDODouble;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[DoubleType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure TSDOSerializer.WriteFloatListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOFloat;
begin
  tmpVal := AValueList.getFloat();
  FStreamer.Put(AName,FProcs[FloatType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteFloatProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOFloat;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getFloat(AProp);
    FStreamer.Put(AProp.getName(),FProcs[FloatType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadFloatProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOFloat;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[FloatType].PropType,localName,tmpVal) then
    AObject.setFloat(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadFloatListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOFloat;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[FloatType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDOSerializer.WriteLongListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOLong;
begin
  tmpVal := AValueList.getLong();
  FStreamer.Put(AName,FProcs[LongType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteLongProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOLong;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getLong(AProp);
    FStreamer.Put(AProp.getName(),FProcs[LongType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadLongProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOLong;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[LongType].PropType,localName,tmpVal) then
    AObject.setLong(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadLongListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOLong;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[LongType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDOSerializer.WriteShortListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOShort;
begin
  tmpVal := AValueList.getShort();
  FStreamer.Put(AName,FProcs[ShortType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteShortProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOShort;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getShort(AProp);
    FStreamer.Put(AProp.getName(),FProcs[ShortType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.ReadShortProp(
  const AProp: ISDOProperty;
  const AObject: ISDODataObject
);
var
  tmpVal : TSDOShort;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[ShortType].PropType,localName,tmpVal) then
    AObject.setShort(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadShortListProp(
  const AName: string;
  const AValueList: ISDODataObjectList
);
var
  tmpVal : TSDOShort;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[ShortType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;
{$ENDIF HAS_SDO_SHORT}

{ TReferenceObjectReadRecall }

constructor TReferenceObjectReadRecall.Create(
  const ARecallType : TReferenceObjectRecallItemType;
  const ARoot,
        ADataObject: ISDODataObject;
  const AReferenceProperty: ISDOProperty;
  const AAccessPath: string;
  const AIsSet : Boolean
);
begin
  FRecallType := ARecallType;
  FRoot := ARoot;
  FDataObject := ADataObject;
  FReferenceProperty := AReferenceProperty;
  FAccessPath := AAccessPath;
  FIsSet := AIsSet;
end;

procedure TSDOSerializer.WritePendingRefProps();
var
  i, c : PtrInt;
  locItem : TReferenceObjectWriteRecall;
  locStrBuffer : TSDOString;
  oldPos : TStreamBookmark;
begin
  c := FReferenceWriteRecallList.Count;
  if ( c > 0 ) then begin
    oldPos := FStreamer.GetBookMark();
    try
      for i := 0 to Pred(c) do begin
        locItem := TReferenceObjectWriteRecall(FReferenceWriteRecallList[i]);
        if not FStreamer.GotoBookmark(locItem.StreamPos) then
          raise ESDOSerializationException.Create('FStreamer seems to be corrupted, writing ChangeSummary reference properties.');
        if ( locItem.RecallType = rritPropertyValue ) or ( not locItem.Prop.isMany() ) then
          locStrBuffer := locItem.Prop.getName()
        else
          locStrBuffer := s_dataValues;
        FStreamer.BeginObject(locStrBuffer,locItem.Prop.getType());
        try
          if ( locItem.Value = nil ) then begin
            FStreamer.NilCurrentScope()
          end else begin
            locStrBuffer := getObjectPathInChangeSummary(locItem.Value);
            if locItem.Prop.isContainment() then begin
              if IsStrEmpty(locStrBuffer) then begin
                FStreamer.NilCurrentScope();
              end else begin
                FStreamer.SetNameStyle(nsQualified);
                  FStreamer.Put(sdo_namespace,s_ref,getType(StringType),locStrBuffer);
                FStreamer.SetNameStyle(nsUnqualified);
              end;
            end else begin
              FStreamer.PutScopeInnerValue(getType(StringType),locStrBuffer);
            end;
          end;
        finally
          FStreamer.EndScope();
        end;
      end;
    finally
      try
        FStreamer.GotoBookmark(oldPos);
      finally
        FreeAndNil(oldPos);
      end;
    end;
  end;
end;

function TSDOSerializer.InternalReadObject(
  const AName : string;
  const AType : ISDOType;
  const AOwner,
        ADataObject : ISDODataObject;
  const AOptions : TObjectReadOptions
): ISDODataObject;

  procedure ReadArrayProp(const AProp : ISDOProperty; const AObject : ISDODataObject);
  var
    ls : ISDODataObjectList;
    k, itemCount : PtrInt;
    rdrProc : TPropListReaderProc;
    itemName : string;
    typ : ISDOType;
  begin
    ls := AObject.getList(AProp);
    itemName := AProp.getName();
    typ := AProp.getType();
    itemCount := FStreamer.BeginArrayRead(itemName,typ,itemName);
    if (itemCount > -1) then begin
      try
        if typ.isDataObjectType() then begin
          if AProp.isContainment() then begin
            for k := 0 to Pred(itemCount) do begin
              InternalReadObject(itemName,typ,AObject,nil,[]);
            end;
          end else begin
            for k := 0 to Pred(itemCount) do begin
              ls.append(InternalReadObject(itemName,typ,AObject,nil,[]));
            end;
          end;
        end else begin
          rdrProc := FProcs[typ.getTypeEnum()].ReaderListProc;
          if Assigned(rdrProc) then begin
            PushSerializationStyle(ssNodeSerialization);
              for k := 0 to Pred(itemCount) do begin
                rdrProc(itemName,ls);
              end;
            PopSerializationStyle();
          end;
        end;
      finally
        FStreamer.EndScopeRead();
      end;
    end;
  end;

var
  resObject : ISDODataObject;

  procedure PushReferenceRecall(const AProp : ISDOProperty);
  var
    tmpNameBuffer : string;
    tmpBuffer : TSDOString;
  begin
    tmpNameBuffer := AProp.getName();
    if (FStreamer.BeginObjectRead(tmpNameBuffer,AProp.getType()) > -1) then begin
      try
        if not FStreamer.IsCurrentScopeNil() then begin
          tmpBuffer := '';
          if FStreamer.GetScopeInnerValue(getType(StringType),tmpBuffer) then begin
            FReferenceReadRecallList.Add(TReferenceObjectReadRecall.Create(rritPropertyValue,getRoot(resObject),resObject,AProp,tmpBuffer,True));
          end else begin
            resObject.unset(AProp);
          end;
        end;
      finally
        FStreamer.EndScopeRead();
      end;
    end;
  end;

  procedure ReadClosedObject();
  var
    pl : ISDOPropertyList;
    p : ISDOProperty;
    pt : ISDOType;
    propCount : PtrInt;
    k : PtrInt;
    prpProc : TPropReaderProc;
    locSerializationStyle : TSerializationStyle;
  begin
    pl := resObject.getType().getProperties();
    propCount := pl.getCount();
    if ( propCount > 0 ) then begin
      for k := 0 to Pred(propCount) do begin
        p := pl.getItem(k);
        if p.isMany() then begin
          ReadArrayProp(p,resObject);
        end else begin
          pt := p.getType();
          if pt.isDataObjectType() then begin
            if p.isContainment() then begin
              InternalReadObject(p.getName(),pt,resObject,nil,[]);
            end else begin
              //do not forget that the referencee may not be read at this moment!
              PushReferenceRecall(p);
            end;
          end else begin
            prpProc := FProcs[pt.getTypeEnum()].ReaderProc;
            if Assigned(prpProc) then begin
              if p.isAttribute() then
                locSerializationStyle := ssAttibuteSerialization
              else
                locSerializationStyle := ssNodeSerialization;
              PushSerializationStyle(locSerializationStyle);
                prpProc(p,resObject);
              PopSerializationStyle();
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ReadOpenedObject();
  var
    pl : ISDOPropertyList;
    p : ISDOProperty;
    pt : ISDOType;
    propCount : PtrInt;
    k : PtrInt;
    prpProc : TPropReaderProc;
    ls : TStringList;
    ss : string; 
    strBuffer : TSDOString;
    locSerializationStyle : TSerializationStyle;
  begin
    pl := resObject.getInstanceProperties();
    propCount := pl.getCount();
    if ( propCount > 0 ) then begin
      for k := 0 to Pred(propCount) do begin
        p := pl.getItem(k);
        if p.isMany() then begin
          ReadArrayProp(p,resObject);
        end else begin
          pt := p.getType();
          if pt.isDataObjectType() then begin
            if p.isContainment() then begin
              InternalReadObject(p.getName(),pt,resObject,nil,[]);
            end else begin
              //do not forget that the referencee may not be read at this moment!
              PushReferenceRecall(p);
            end;
          end else begin
            prpProc := FProcs[pt.getTypeEnum()].ReaderProc;
            if Assigned(prpProc) then begin
              if p.isAttribute() then
                locSerializationStyle := ssAttibuteSerialization
              else
                locSerializationStyle := ssNodeSerialization;
              PushSerializationStyle(locSerializationStyle);
                prpProc(p,resObject);
              PopSerializationStyle();
            end;
          end;
        end;
      end;
    end;
    ls := TStringList.Create();
    try
      ls.Duplicates := dupAccept;
      if ( FStreamer.GetScopeItemNames(ssAttibuteSerialization,ls) > 0 ) then begin
        for k := 0 to Pred(ls.Count) do begin
          ss := ls[k];
          if ( pl.find(ss) = nil ) then begin
            strBuffer := '';
            if FStreamer.Get(FProcs[StringType].PropType,ss,strBuffer) then
              resObject.setString(ss,strBuffer);
          end;
        end;
      end;
    finally
      ls.Free();
    end;
  end;

var
  space, localName : string;
  localType : ISDOType;
  locProp : ISDOProperty;
begin
  localType := AType;
  if ( localType = nil ) then
    raise ESDOTypeNotFoundException.Create(AName);
  localName := AName;
  space := localType.getURI();
  if (oroDontMake_BeginObjectRead in AOptions) or
     (FStreamer.BeginObjectRead(localName,localType) > -1)
  then begin
    try
      if not FStreamer.IsCurrentScopeNil() then begin
        if ( ADataObject = nil ) then begin
          if ( AOwner <> nil ) then begin
            //resObject := AOwner.createDataObject(AName);
            locProp := AOwner.getProperty(AName);
            resObject := AOwner.createDataObject(locProp);
            if locProp.isMany() and locProp.isContainment() then
              AOwner.getList(locProp).append(resObject);
          end else begin
            resObject := FDataFactory.createNew(localType);
          end;
        end else begin
          resObject := ADataObject
        end;
        FStreamer.SetNameStyle(nsUnqualified);
        if localType.isOpenType() then
          ReadOpenedObject()
        else
          ReadClosedObject();
      end;
      Result := resObject;
    finally
      if not (oroDontMake_EndScopeRead in AOptions) then
        FStreamer.EndScopeRead();
    end;
  end;
end;

function TSDOSerializer.load(const AFileName: string) : ISDODataObject;
var
  ls : ISDODataObjectList;
begin
  ls := TSDODataObjectList.Create(FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType])) as ISDODataObjectList;
  load(AFileName,ls);
  if ( ls.size() > 0 ) then
    Result := ls.getDataObject(0)
  else
    raise ESDOSerializationException.CreateFmt('No object found in this file : "%s".',[AFileName]);
end;

procedure TSDOSerializer.PopSerializationStyle();
begin
  Assert( ( FSerializationStyleStackIndex >= 0 ) );
  FStreamer.SetSerializationStyle(FSerializationStyleStack[FSerializationStyleStackIndex]);
  Dec(FSerializationStyleStackIndex);
end;

procedure TSDOSerializer.PushSerializationStyle(const AValue: TSerializationStyle);
begin
  Inc(FSerializationStyleStackIndex);
  Assert( ( FSerializationStyleStackIndex >= 0 ) and ( FSerializationStyleStackIndex < MAX_NESTED_LEVEL ) );
  FSerializationStyleStack[FSerializationStyleStackIndex] := FStreamer.GetSerializationStyle();
  FStreamer.SetSerializationStyle(AValue);
end;

procedure TSDOSerializer.ReadBoolListProp(const AName: string; const AValueList: ISDODataObjectList);
var
  tmpVal : TSDOBoolean;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[BooleanType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;

procedure TSDOSerializer.ReadBoolProp(const AProp: ISDOProperty; const AObject: ISDODataObject);
var
  tmpVal : TSDOBoolean;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[BooleanType].PropType,localName,tmpVal) then
    AObject.setBoolean(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadChangeSummary(
  const AValue: ISDOChangeSummary;
  const ARootObjectName : string;
  const ARootObject : ISDODataObject
);
var
  stringTypeObj : ISDOType;
  changedObjList : ISDOChangedDataObjectListEx;
  rootNameSpace, rootlocaName : string;
  recorder : TChangeRecorder;

  function FindObject(const APath : string) : ISDODataObject;
  var
    locPath : string;
  begin
    Result := nil;
    locPath := Trim(APath);
    if ( Length(locPath) > 0 ) then begin
      locPath := xpath_ExcludeRootElement(locPath,rootlocaName);
      if ( Length(locPath) = 0 ) then
        Result := ARootObject
      else
        Result := ARootObject.getDataObject(locPath);
    end;
  end;

  procedure ReadPropMultiValueChange(
    const AObject : ISDODataObject;
    const AProp : ISDOProperty;
    const AChangeInfo : TDataObjectChangeInfo
  );
  var
    locIntType, locStringType : ISDOType;
    itemType : ISDOType;
    nameBuffer : string; 
    stringBuffer : TSDOString;
    buffer : TValueBuffer;
    pbuffer : Pointer;
    oldNameStyle : TNameStyle;
    ls : TManyValuePropChanges;
    locIndex : TSDOInteger;
    locKind : TManyValuePropAction;
    locCount, locArrayReadIDX : PtrInt;
    locIsObjectProp : Boolean;
    locRecall : TReferenceObjectReadRecall;
  begin
    nameBuffer := AProp.getName();
    itemType := AProp.getType();
    if ( FStreamer.BeginObjectRead(nameBuffer, itemType) >= 0 ) then begin
      nameBuffer := s_listChanges;
      locCount := FStreamer.BeginArrayRead(nameBuffer, itemType, s_listChanges);
      if ( locCount >= 0 ) then begin
        nameBuffer := s_listChanges;
        ls := AChangeInfo.GetChanges(AProp);
        locIntType := getType(IntegerType);
        locStringType := getType(StringType);
        InitBufferResources(itemType.getTypeEnum(), buffer);
        try
          PushSerializationStyle(ssAttibuteSerialization);
            locArrayReadIDX := 0;
            locIsObjectProp := AProp.getType().isDataObjectType();
            if not locIsObjectProp then begin
              case AProp.getTypeEnum() of
                BooleanType,
                ByteType,
{$IFDEF HAS_SDO_CHAR}
                CharacterType,
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
                CurrencyType,
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
                DoubleType,
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
                FloatType,
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
                LongType,
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
                ShortType,
{$ENDIF HAS_SDO_SHORT}
                DateTimeType,
                IntegerType :
                  begin
                    pbuffer := @buffer;
                  end;
{$IFDEF HAS_SDO_BYTES}
                BytesType  : pbuffer := buffer.BytesValue;
{$ENDIF HAS_SDO_BYTES}
                StringType : pbuffer := buffer.StringValue;
                else 
                  Assert(False, 'NON IMPLEMENTED YET!');
              end;
            end;
            oldNameStyle := FStreamer.GetNameStyle();
            FStreamer.SetNameStyle(nsUnqualified);
              while ( locArrayReadIDX < locCount ) and ( FStreamer.BeginObjectRead(nameBuffer, itemType) > 0 ) do begin
                nameBuffer := s_index;
                if not FStreamer.Get(locIntType, nameBuffer, locIndex) then
                  raise ESDOSerializationException.CreateFmt('Invalid "%s" stream : unable to find "%s" attribute.',[s_listChanges,s_index]);
                nameBuffer := s_kind;
                if not FStreamer.Get(locStringType, nameBuffer, stringBuffer) then
                  raise ESDOSerializationException.CreateFmt('Invalid "%s" stream : unable to find "%s" attribute.',[s_listChanges,s_kind]);
                if not FindAction(stringBuffer,locKind) then
                  raise ESDOSerializationException.CreateFmt('Invalid "%s" stream : invalid "%s" value ( "%s" ).',[s_listChanges,s_kind,stringBuffer]);
                case locKind of
                  mvpaAppend, mvpaInsert : ls.Add(locKind,locIndex);
                  mvpaChange, mvpaDelete :
                    begin
                      nameBuffer := s_dataValues;
                      if locIsObjectProp then begin
                        nameBuffer := s_dataValues;
                        if ( FStreamer.BeginObjectRead(nameBuffer,stringTypeObj) < 0 ) then begin
                          raise ESDOSerializationException.CreateFmt('Invalid "%s" stream : "%s" not found.',[s_listChanges,s_dataValues]);
                        end;
                          if FStreamer.IsCurrentScopeNil() then begin
                            buffer.ObjectValue^ := nil;
                            ls.Add(locKind,locIndex,buffer);
                          end else begin
                            nameBuffer := s_ref;
                            PushSerializationStyle(ssAttibuteSerialization);
                              oldNameStyle := FStreamer.GetNameStyle();
                              FStreamer.SetNameStyle(nsQualified);
                                if not FStreamer.Get(sdo_namespace,stringTypeObj,nameBuffer,stringBuffer) then
                                  raise ESDOSerializationException.CreateFmt('Invalid "%s" stream : "%s" not found.',[s_listChanges,s_dataValues]);
                              FStreamer.SetNameStyle(oldNameStyle);
                            PopSerializationStyle();
                            stringBuffer := xpath_ExcludeRootElement(stringBuffer,ARootObjectName);
                            locRecall := TReferenceObjectReadRecall.Create(rritSettingItem,ARootObject,AObject,AProp,stringBuffer,True);
                            FReferenceReadRecallList.Add(locRecall);
                            locRecall.FMultiValueData.Action := locKind;
                            locRecall.FMultiValueData.ActionIndex := locIndex;
                            locRecall.FMultiValueData.Index := locArrayReadIDX;
                          end;
                        FStreamer.EndScopeRead();
                      end else begin
                        PushSerializationStyle(ssAttibuteSerialization);//PushSerializationStyle(getSerializationStyle(AProp));
                          if not FStreamer.Get(itemType,nameBuffer,pbuffer^) then
                            raise ESDOSerializationException.CreateFmt('Invalid "%s" stream : "%s" not found.',[s_listChanges,s_dataValues]);
                        PopSerializationStyle();
                        ls.Add(locKind,locIndex,buffer);
                      end;
                    end;
                  else
                    raise ESDONotImplementedException.CreateFmt('Reading this kind of data change : Type = TManyValuePropAction, Ord = %d',[Ord(locKind)]);
                end;
                FStreamer.EndScopeRead();
                Inc(locArrayReadIDX);
              end;
            FStreamer.SetNameStyle(oldNameStyle);
          PopSerializationStyle();
        finally
          FreeBufferResources(itemType.getTypeEnum(), buffer);
        end;
        FStreamer.EndScopeRead();
      end;
      FStreamer.EndScopeRead();
    end;
  end;

  procedure ReadPropSingleValueChange(
    const AObject : ISDODataObject;
    const AProp : ISDOProperty;
    const AChangeInfo : TDataObjectChangeInfo
  );
  const
    NIL_OBJ_BUFFER : ISDODataObject = nil;
  var
    nameBuffer : string;
    stringBuffer : TSDOString;
    bytesBuffer : TSDOBytes;
    buffer : TValueBuffer;
    oldNameStyle : TNameStyle;
  begin
    nameBuffer := AProp.getName();
    case AProp.getTypeEnum() of
      ChangeSummaryType : { nothing };
      BooleanType, ByteType,
      {$IFDEF HAS_SDO_CHAR}CharacterType,{$ENDIF}
      {$IFDEF HAS_SDO_CURRENCY}CurrencyType,{$ENDIF}
      DateTimeType,
      {$IFDEF HAS_SDO_DOUBLE}DoubleType,{$ENDIF}
      {$IFDEF HAS_SDO_FLOAT}FloatType,{$ENDIF}
      {$IFDEF HAS_SDO_LONG}LongType,{$ENDIF}
      {$IFDEF HAS_SDO_SHORT}ShortType,{$ENDIF}
      IntegerType :
        begin
          PushSerializationStyle(getSerializationStyle(AProp));
            if FStreamer.Get(AProp.getType(),nameBuffer,buffer) then
              AChangeInfo.ChangeList.append(TValueSetting.Create(True,False,buffer,AProp,0));
          PopSerializationStyle();
        end;
      StringType :
        begin
          PushSerializationStyle(getSerializationStyle(AProp));
            if FStreamer.Get(AProp.getType(),nameBuffer,stringBuffer) then
              AChangeInfo.ChangeList.append(TValueSetting.Create(True,False,stringBuffer,AProp,0));
          PopSerializationStyle();
        end;
{$IFDEF HAS_SDO_BYTES}
      BytesType :
        begin
          PushSerializationStyle(getSerializationStyle(AProp));
            if FStreamer.Get(AProp.getType(),nameBuffer,bytesBuffer) then
              AChangeInfo.ChangeList.append(TValueSetting.Create(True,False,bytesBuffer,AProp,0));
          PopSerializationStyle();
        end;
{$ENDIF HAS_SDO_BYTES}
      ObjectType :
        begin
          if AProp.isContainment() then begin
            if ( FStreamer.BeginObjectRead(nameBuffer,AProp.getType()) > -1 ) then begin
              oldNameStyle := FStreamer.GetNameStyle();
              FStreamer.SetNameStyle(nsQualified);
                nameBuffer := s_ref;
                if FStreamer.Get(sdo_namespace,stringTypeObj,nameBuffer,stringBuffer) then begin
                  if not IsStrEmpty(stringBuffer) then
                    FReferenceReadRecallList.Add(TReferenceObjectReadRecall.Create(rritSettingItem, ARootObject,AObject,AProp,stringBuffer,True));
                end;
              FStreamer.SetNameStyle(oldNameStyle);
              FStreamer.EndScopeRead();
            end;
          end else if AProp.isReference() then begin
            if ( FStreamer.BeginObjectRead(nameBuffer,AProp.getType()) > -1 ) then begin
              if FStreamer.IsCurrentScopeNil() then begin
                AChangeInfo.ChangeList.append(TValueSetting.Create(True,True,NIL_OBJ_BUFFER,AProp,0));
              end else begin
                if FStreamer.GetScopeInnerValue(stringTypeObj,stringBuffer) then begin
                  if not IsStrEmpty(stringBuffer) then
                    FReferenceReadRecallList.Add(TReferenceObjectReadRecall.Create(rritSettingItem, ARootObject,AObject,AProp,stringBuffer,True));
                end;
              end;
              FStreamer.EndScopeRead();
            end;
          end;
        end;
      else
        begin
          raise Exception.Create('NOT-IMPLEMENTED');
        end;
    end;
  end;

  procedure ReadPropChange(
    const AObject : ISDODataObject;
    const AProp : ISDOProperty;
    const AChangeInfo : TDataObjectChangeInfo;
    const AUnsetList : TStrings
  );
  const
    NIL_OBJ_BUFFER : ISDODataObject = nil;
  var
    buffer : TValueBuffer;
  begin
    if ( AUnsetList.IndexOf(AProp.getName()) > -1 ) then begin
      FillChar(buffer,SizeOf(buffer),#0);
      AChangeInfo.ChangeList.append(TValueSetting.Create(False,False,buffer,AProp,0));
    end else begin
      if AProp.isMany() then begin
        ReadPropMultiValueChange(AObject,AProp,AChangeInfo);
      end else begin
        ReadPropSingleValueChange(AObject,AProp,AChangeInfo);
      end;
    end;
  end;

  procedure ReadDeletedObject(const AName : string; const AType : ISDOType);{$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    locO : ISDODataObject;
  begin
    locO := InternalReadObject(AName,AType,nil,nil,[oroDontMake_BeginObjectRead,oroDontMake_EndScopeRead]);
    recorder.recordDeletion(locO);
    AddToChangeSummaryList(locO);
  end;

  procedure ReadCSObject(const AName : string);
  var
    nameBuffer, uriBuffer, tmpNameBuffer : string;
    objectRef, strBuffer : TSDOString;
    objType : ISDOType;
    obj : ISDODataObject;
    pls : ISDOPropertyList;
    q, k : PtrInt;
    objChangeInfo : TDataObjectChangeInfo;
    oldNameStyle : TNameStyle;
    locDeleted : Boolean;
    unsetList : TStringList;
  begin
    k := Pos('#',AName);
    if ( k = 0 ) then begin
      uriBuffer := ARootObject.getType().getURI();
      nameBuffer := AName;
    end else begin
      uriBuffer := Copy(AName,1,( k - 1 ));
      nameBuffer := Copy(AName,( k + 1 ),Length(AName));
    end;
    objType := FDataFactory.getType(uriBuffer,nameBuffer);
    pls := nil;
    FStreamer.BeginObjectRead(nameBuffer,objType);
    tmpNameBuffer := s_ref;
    oldNameStyle := FStreamer.GetNameStyle();
    FStreamer.SetNameStyle(nsQualified);
    locDeleted := not FStreamer.Get(sdo_namespace,stringTypeObj,tmpNameBuffer,objectRef);
    FStreamer.SetNameStyle(oldNameStyle);
    if locDeleted then begin
      ReadDeletedObject(nameBuffer,objType);
    end else begin
      obj := FindObject(objectRef);
      if ( obj = nil ) then
        raise ESDOSerializationException.CreateFmt('Object not found : "%s".',[objectRef]);
      AddToChangeSummaryList(obj);
      pls := objType.getProperties();
      q := pls.getCount();
      if ( q > 0 ) then begin
        oldNameStyle := FStreamer.GetNameStyle();
        FStreamer.SetNameStyle(nsQualified);
          nameBuffer := s_unset;
          strBuffer := '';
          FStreamer.Get(sdo_namespace,stringTypeObj,nameBuffer,strBuffer);
        FStreamer.SetNameStyle(oldNameStyle);
        unsetList := TStringList.Create();
        try
          unsetList.Duplicates := dupIgnore;
          unsetList.Sorted := True;
          unsetList.Delimiter := ' ';
          unsetList.DelimitedText := strBuffer;
          if changedObjList.find(obj,k) then
            objChangeInfo := changedObjList.getInfo(k)
          else
            objChangeInfo := recorder.recordUpdate(obj);
          for k := 0 to Pred(q) do begin
            ReadPropChange(obj,pls.getItem(k),objChangeInfo,unsetList);
          end;
          objChangeInfo.ExtractPendingOldValues();
        finally
          unsetList.Free();
        end;
      end;
    end;
    FStreamer.EndScopeRead();
  end;

  procedure ReadObjects();
  var
    ls, lsReaded : TStringList;
    c, i, k, locArrayLength : PtrInt;
    itm, nameBuffer, uriBuffer : string;
    objType : ISDOType;
  begin
    lsReaded := nil;
    ls := TStringList.Create();
    try
      if ( FStreamer.GetScopeItemNames(ssNodeSerialization,ls) > 0 ) then begin
        lsReaded := TStringList.Create();
        lsReaded.Duplicates := dupIgnore;
        lsReaded.Sorted := True;
        lsReaded.Add(s_create);
        lsReaded.Add(s_delete);
        changedObjList := AValue.getChangedDataObjects() as ISDOChangedDataObjectListEx;
        c := ls.Count;
        for i := 0 to Pred(c) do begin
          itm := ls[i];
          if ( lsReaded.IndexOf(itm) = -1 ) then begin
            lsReaded.Add(itm);
            nameBuffer := itm;
            k := Pos('#',itm);
            if ( k = 0 ) then begin
              uriBuffer := ARootObject.getType().getURI();
              nameBuffer := itm;
            end else begin
              uriBuffer := Copy(itm,1,( k - 1 ));
              nameBuffer := Copy(itm,( k + 1 ),Length(itm));
            end;
            objType := FDataFactory.getType(uriBuffer,nameBuffer);
            locArrayLength := FStreamer.BeginArrayRead(nameBuffer,objType,itm);
            if (locArrayLength > -1) then begin
              for k := 0 to ( locArrayLength - 1 ) do
                ReadCSObject(itm);
              FStreamer.EndScopeRead();
            end;
          end;
        end;
      end;
    finally
      lsReaded.Free();
      ls.Free();
    end;
  end;

  procedure ReadCreatedObjects();
  var
    tmpStr : TSDOString; 
    tmpBuffer : string;
    tmpObj : ISDODataObject;
  begin
    PushSerializationStyle(ssAttibuteSerialization);
    try
      tmpBuffer := s_create;
      if FStreamer.Get(getType(StringType),tmpBuffer,tmpStr) then begin
        repeat
          tmpBuffer := GetNextToken(tmpStr,' ');
          if ( Length(tmpBuffer) = 0 ) then
            Break;
          tmpBuffer := xpath_ExcludeRootElement(tmpBuffer,FRootName);
          tmpObj := ARootObject.getDataObject(tmpBuffer);
          if ( tmpObj <> nil ) then
            recorder.recordCreation(tmpObj);
        until False;
      end;
    finally
      PopSerializationStyle();
    end;
  end;

var
  tmpType : ISDOType;
  locName : string;
  i : PtrInt;
begin
  FStreamer.Initialize();
  FReferenceReadRecallList.Clear();
  FChangeSummaryList := FDataFactory.createNew(sdo_namespace,s_changeSummaryListObject);
  i := Pos('#',ARootObjectName);
  if ( i > 0 ) then begin
    rootNameSpace := Copy(ARootObjectName,1,( i - 1 ));
    rootlocaName := Copy(ARootObjectName,( i + 1 ), MaxInt);
  end else begin
    rootNameSpace := ARootObject.getType().getURI();
    rootlocaName := ARootObjectName;
  end;
  stringTypeObj := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]);
  tmpType := FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType]);
  locName := s_changeSummary;
  recorder := (ARootObject.getChangeSummary() as ISDOChangeSummaryEx).getRecorder();
  recorder.Deserializing := True;
  try
    if (FStreamer.BeginObjectRead(locName,tmpType) > -1) then begin
      try
        ReadCreatedObjects();
        ReadObjects();
        ReadPendingRefProps();
      finally
        FStreamer.EndScopeRead();
      end;
    end;
  finally
    recorder.Deserializing := False;
  end;
end;

procedure TSDOSerializer.ReadIntegerListProp(const AName: string; const AValueList: ISDODataObjectList);
var
  tmpVal : TSDOInteger;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[IntegerType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;

procedure TSDOSerializer.ReadIntegerProp(const AProp: ISDOProperty; const AObject: ISDODataObject);
var
  tmpVal : TSDOInteger;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[IntegerType].PropType,localName,tmpVal) then
    AObject.setInteger(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.ReadMetadata();
var
  s : string; 
  locXdsBuffer : TSDOString;
  locXsd : IXSDHelper;
begin
  s := s_xsd;
  if (FStreamer.BeginObjectRead(s,FDataFactory.getType(sdo_namespace,s_xsd)) > -1) then begin
    try
      locXdsBuffer := '';
      if AnsiSameText(s_xml,FStreamer.GetFormatName()) then begin
        locXdsBuffer := FStreamer.ReadBuffer(s_schema)
      end else begin
        s := s_xsd;
        FStreamer.Get(FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),s,locXdsBuffer);
      end;
      if not IsStrEmpty(locXdsBuffer) then begin
        locXsd := TXSDHelper.Create(FDataFactory);
        locXsd.LoadFromString(locXdsBuffer);
      end;
    finally
      FStreamer.EndScopeRead();
    end;
  end;
end;

function TSDOSerializer.ReadObject(
  const AName: string;
  const AType : ISDOType;
  const AOwner : ISDODataObject
) : ISDODataObject;

  function findType(const AUri, ALocalName : string) : ISDOType;
  var
    k, cc : PtrInt;
    tl : ISDOTypeList;
    typ : ISDOType;
  begin
    if not IsStrEmpty(AUri) then begin
      Result := FDataFactory.getTypes().find(AUri,ALocalName);
    end else begin
      Result := nil;
      tl := FDataFactory.getTypes();
      cc := tl.getCount();
      if ( cc > 0 ) then begin
        for k := 0 to Pred(cc) do begin
          typ := tl.getItem(k);
          if AnsiSameStr(ALocalName,typ.getName()) then begin
            Result := typ;
            Break;
          end;
        end;
        if not Assigned(Result) then begin
          for k := 0 to Pred(cc) do begin
            typ := tl.getItem(k);
            if AnsiSameText(ALocalName,typ.getName()) then begin
              Result := typ;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure DisableChangeSummary(const AObj : ISDODataObject);
  var
    locCS : ISDOChangeSummary;
  begin
    locCS := AObj.getChangeSummary();
    if ( locCS <> nil ) and locCS.isLogging() then
      locCS.endLogging();
  end;

var
  space, localName : string;
  i : PtrInt;
  localType : ISDOType;
  resObject : ISDODataObject;
begin
  if Assigned(AType) then begin
    localType := AType;
    localName := AName;
    space := localType.getURI();
  end else begin
    i := Pos('#',AName);
    if ( i > 0 ) then begin
      space := Copy(AName,1,Pred(i));
      localName := Copy(AName,Succ(i),Length(AName));
    end else begin
      space := '';
      localName := AName;
    end;
    localType := findType(space,localName);
    if ( localType = nil ) then
      raise ESDOTypeNotFoundException.Create(AName);
  end;
  FRootName := localName;
  if ( AOwner = nil ) then begin
    resObject := FDataFactory.createNew(localType);
  end else begin
    resObject := AOwner.createDataObject(AName);
  end;
  DisableChangeSummary(resObject);
  Result := InternalReadObject(localName,localType,AOwner,resObject,[]);
end;

procedure TSDOSerializer.ReadStringListProp(const AName: string; const AValueList: ISDODataObjectList);
var
  tmpVal : TSDOString;
  localName : string;
begin
  localName := AName;
  if FStreamer.Get(FProcs[StringType].PropType,localName,tmpVal) then
    AValueList.append(tmpVal);
end;

procedure TSDOSerializer.ReadStringProp(const AProp: ISDOProperty; const AObject: ISDODataObject);
var
  tmpVal : TSDOString;
  localName : string;
begin
  localName := AProp.getName();
  if FStreamer.Get(FProcs[StringType].PropType,localName,tmpVal) then
    AObject.setString(localName,tmpVal)
  else if AProp.isNullable() then
    AObject.setNull(AProp);
end;

procedure TSDOSerializer.save(
  const AName : string;
        AObject : ISDODataObject;
  const ADestStream : TStream
);
begin
  if ( AObject = nil ) then
    raise ESDOIllegalArgumentException.Create('AObject');
  Init();
  try
    FNamespace := AObject.getType().getURI();
    BeginWrite();
    try
      FStreamer.SetNameStyle(nsQualified);
      WriteObject(AName, AObject,[owoWriteChangeSummary],'');
    finally
      EndWrite();
    end;
    FStreamer.SaveToStream(ADestStream);
  finally
    Init();
  end;
end;

procedure TSDOSerializer.save(AObject: ISDODataObject; const ADestStream: TStream);
begin
  if ( AObject = nil ) then
    raise ESDOIllegalArgumentException.Create('AObject');
  save(AObject.getType().getName(),AObject,ADestStream);
end;

procedure TSDOSerializer.save(
  const AName: string;
        AObject: ISDODataObject;
  const AFileName: string
);
var
  flStream : TStream;
begin
  flStream := TFileStream.Create(AFileName,fmCreate);
  try
    save(AName,AObject,flStream);
  finally
    flStream.Free();
  end;
end;

procedure TSDOSerializer.save(AObject: ISDODataObject;  const AFileName: string);
begin
  if ( AObject = nil ) then
    raise ESDOIllegalArgumentException.Create('AObject');
  save(AObject.getType().getName(),AObject,AFileName);
end;

procedure TSDOSerializer.setOptions(const AValue: TSerializerOptions);
begin
  if (AValue <> FOptions) then
    FOptions := AValue;
end;

procedure TSDOSerializer.WriteBoolListProp(const AName: string; const AValueList : ISDODataObjectList);
var
  tmpVal : TSDOBoolean;
begin
  tmpVal := AValueList.getBoolean();
  FStreamer.Put(AName,FProcs[BooleanType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteBoolProp(const AProp : ISDOProperty; const AObject: ISDODataObject);
var
  tmpVal : TSDOBoolean;
begin
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getBoolean(AProp);
    FStreamer.Put(AProp.getName(),FProcs[BooleanType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.WriteChangeSummary(
  const AValue : ISDOChangeSummary;
  const ARootObjectName : string
);

  procedure WriteChange(const ADataObject : ISDODataObject);
  var
    unsetList : TSDOString;

    procedure WriteSetting(const ASetting : TValueSetting);
    var
      p : ISDOProperty;
      v : TValueBuffer;
      vstr : TSDOString;
      vbytes : TSDOBytes;
    begin
      p := ASetting.getProperty();
      if ASetting.isSet() then begin
        if p.getType().isDataType() then begin
          PushSerializationStyle(getSerializationStyle(p));
          case p.getTypeEnum() of
            BooleanType :
              begin
                v.BooleanValue := ASetting.getBooleanValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
            ByteType :
              begin
                v.ByteValue := ASetting.getByteValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$IFDEF HAS_SDO_BYTES}
            BytesType  :
              begin
                vbytes := ASetting.getBytesValue();
                FStreamer.Put(p.getName(),p.getType(),vbytes);
              end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
            CharacterType :
              begin
                v.CharValue := ASetting.getCharacterValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
            CurrencyType :
              begin
                v.CurrencyValue := ASetting.getCurrencyValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$ENDIF HAS_SDO_CURRENCY}
            DateTimeType :
              begin
                v.DateValue := ASetting.getDateValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$IFDEF HAS_SDO_DOUBLE}
            DoubleType :
              begin
                v.DoubleValue := ASetting.getDoubleValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
            FloatType :
              begin
                v.FloatValue := ASetting.getFloatValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$ENDIF HAS_SDO_FLOAT}
            IntegerType :
              begin
                v.IntegerValue := ASetting.getIntegerValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$IFDEF HAS_SDO_LONG}
            LongType :
              begin
                v.LongValue := ASetting.getLongValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
            ShortType :
              begin
                v.ShortValue := ASetting.getShortValue();
                FStreamer.Put(p.getName(),p.getType(),v);
              end;
{$ENDIF HAS_SDO_SHORT}
            StringType  :
              begin
                vstr := ASetting.getStringValue();
                FStreamer.Put(p.getName(),p.getType(),vstr);
              end;
            else
              raise Exception.Create('NOT-IMPLEMENTED');
          end;
          PopSerializationStyle();
        end else if p.getType().isDataObjectType() then begin
          if p.isContainment() then begin
            if ( ASetting.getDataObjectValue() = nil ) then begin
              FStreamer.BeginObject(p.getName(),p.getType());
                FStreamer.NilCurrentScope();
              FStreamer.EndScope();
            end else begin
              AddPropertyRefRecallWrite(rritSettingItem,ADataObject,p,ASetting.getDataObjectValue());
            end;
          end else if p.isReference() then begin
            if ( ASetting.getDataObjectValue() = nil ) then begin
              FStreamer.BeginObject(p.getName(),p.getType());
                FStreamer.NilCurrentScope();
              FStreamer.EndScope();
            end else begin
              if AValue.isDeleted(ASetting.getDataObjectValue()) then begin
                AddPropertyRefRecallWrite(rritSettingItem,ADataObject,p,ASetting.getDataObjectValue());
              end else begin
                vstr := Format('%s/%s',[ARootObjectName, AValue.getOldXpath(ASetting.getDataObjectValue())]);
                if AValue.isDeleted(ASetting.getDataObjectValue()) then
                  vstr := Format('#/%s/%s',[s_changeSummary,vstr]);
                FStreamer.BeginObject(p.getName(),p.getType());
                  FStreamer.SetNameStyle(nsQualified);
                    FStreamer.Put(sdo_namespace,s_ref,getType(StringType),vstr);
                  FStreamer.SetNameStyle(nsUnqualified);
                FStreamer.EndScope();
              end;
            end;
          end;
        end;
      end else begin
        unsetList := Format('%s %s',[unsetList,p.getName()]);
      end;
    end;

  var
    locType : ISDOType;
    locStrBuffer : TSDOString;
    ls : ISDOSettingList;
    c, i : PtrInt;
    locSetting : TValueSetting;
  begin
    AddToChangeSummaryList(ADataObject);
    locType := ADataObject.getType();
    FStreamer.BeginObject(locType.getName(),locType);
    try
      PushSerializationStyle(ssAttibuteSerialization);
      try
        locStrBuffer := getObjectPath(ADataObject,ARootObjectName);
        FStreamer.SetNameStyle(nsQualified);
          FStreamer.Put(sdo_namespace, s_ref,getType(StringType),locStrBuffer);
        FStreamer.SetNameStyle(nsUnqualified);
        ls := AValue.getOldValues(ADataObject);
        c := ls.size();
        if ( c > 0 ) then begin
          for i := 0 to Pred(c) do begin
            locSetting := ls.getItem(i);
            if not locSetting.getProperty().isMany() then
              WriteSetting(locSetting);
          end;
        end;
        unsetList := Trim(unsetList);
        if ( Length(unsetList) > 0 ) then begin
          FStreamer.SetNameStyle(nsQualified);
            FStreamer.Put(sdo_namespace,s_unset,getType(StringType),unsetList);
          FStreamer.SetNameStyle(nsUnqualified);
        end;
        WriteChangeSummaryMultiValueProps(AValue,ADataObject);
      finally
        PopSerializationStyle();
      end;
    finally
      FStreamer.EndScope();
    end;
  end;

var
  oldNameStyle : TNameStyle;
  ls : ISDOChangedDataObjectListEx;
  lsCreate, lsDelete, strBuffer : TSDOString;
  c, i : PtrInt;
  tmpObj : ISDODataObject;
  locChangeInfo : TDataObjectChangeInfo;
begin
  if ( AValue <> nil ) then begin
    FChangeSummary := AValue;
    try
      FChangeSummaryList := FDataFactory.createNew(sdo_namespace,s_changeSummaryListObject);
      ls := AValue.getChangedDataObjects() as ISDOChangedDataObjectListEx;
      oldNameStyle := FStreamer.GetNameStyle();
      FStreamer.SetNameStyle(nsUnqualified);
      try
        FStreamer.BeginObject(s_changeSummary,FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ChangeSummaryType]));
        try
          c := ls.size();
          if ( c > 0 ) then begin
            lsCreate := '';
            lsDelete := '';
            FStreamer.SetNameStyle(nsUnqualified);
            for i := 0 to Pred(c) do begin
              //if ( ls.getInfo(i).Index = 0 ) then begin
                case ls.getType(i) of
                  ctCreate :
                    begin
                      locChangeInfo := ls.getInfo(i);
                      locChangeInfo.CaptureOldContainment();
                      tmpObj := locChangeInfo.DataObject;
                      lsCreate := Format('%s %s',[lsCreate,getObjectPath(tmpObj,ARootObjectName)]);
                      {if locChangeInfo.OldContainmentProperty.isMany() then
                        lsCreate := Format('%s[%d]',[lsCreate,locChangeInfo.Index]);}
                    end;
                  ctChange : WriteChange(ls.getDataObject(i));
                  ctDelete :
                    begin
                      tmpObj := ls.getDataObject(i);
                      strBuffer := Format('#/%s/%s/%s',[s_changeSummary, ARootObjectName, AValue.getOldXpath(tmpObj)]);
                      lsDelete := Format('%s %s',[lsDelete, AddToChangeSummaryList(tmpObj)]);
                      InternalWriteObject(
                        tmpObj.getType().getName(),tmpObj,
                        [{owoWriteReference : set the ref only objects represented both in the CS and and the final doc}],
                        strBuffer
                      );
                    end;
                end;
              //end;
            end;
            lsCreate := Trim(lsCreate);
            if ( Length(lsCreate) > 0 ) then
              FStreamer.Put(s_create,getType(StringType),lsCreate);
            lsCreate := '';
            lsDelete := Trim(lsDelete);
            if ( Length(lsDelete) > 0 ) then
              FStreamer.Put(s_delete,getType(StringType),lsDelete);
            WritePendingRefProps();
            FReferenceWriteRecallList.Clear();
          end;
        finally
          FStreamer.EndScope();
        end;
      finally
        FStreamer.SetNameStyle(oldNameStyle);
        //FChangeSummaryList := nil;
      end;
    finally
      FChangeSummary := nil;
    end;
  end;
end;

procedure TSDOSerializer.WriteIntegerListProp(const AName: string; const AValueList: ISDODataObjectList);
var
  tmpVal : TSDOInteger;
begin
  tmpVal := AValueList.getInteger();
  FStreamer.Put(AName,FProcs[IntegerType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteIntegerProp(const AProp: ISDOProperty; const AObject : ISDODataObject);
var
  tmpVal : TSDOInteger;
begin            
  if not AObject.isNull(AProp) then begin 
    tmpVal := AObject.getInteger(AProp);
    FStreamer.Put(AProp.getName(),FProcs[IntegerType].PropType,tmpVal);
  end;
end;

procedure TSDOSerializer.WriteMetadata();
var
  locXdsBuffer : TSDOString;
  locXsd : IXSDHelper;
begin
  locXsd := TXSDHelper.Create(FDataFactory);
  locXdsBuffer := locXsd.Generate(FDataFactory.getTypes(),FNamespace);
  FStreamer.BeginObject(s_xsd,FDataFactory.getType(sdo_namespace,s_xsd));
  try
    if AnsiSameText(s_xml,FStreamer.GetFormatName()) then
      FStreamer.WriteBuffer(locXdsBuffer)
    else
      FStreamer.Put(s_xsd,FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[StringType]),locXdsBuffer);
  finally
    FStreamer.EndScope();
  end;
end;

procedure TSDOSerializer.InternalWriteObject(
  const AName : string;
  const AObject: ISDODataObject;
  const AOptions : TObjectWriteOptions;
  const AReference : TSDOString
);

  procedure WriteArrayProp(const AProp : ISDOProperty);
  var
    ls : ISDODataObjectList;
    k : PtrInt;
    crsr : ISDOCursor;
    wrtProc : TPropListWriterProc;
    bmk : ISDOCursorBookmark;
  begin
    ls := AObject.getList(AProp);
    k := ls.size() - 1;
    crsr := ls.getCursor();
    FStreamer.BeginArray(AProp.getName(),AProp.getType(),[0,k]);
    try
      PushSerializationStyle(ssNodeSerialization);
      try
        bmk := crsr.GetBookmark();
        try
          if AProp.getType().isDataObjectType() then begin
            crsr.Reset();
            while crsr.MoveNext() do begin
              InternalWriteObject(AProp.getName(),ls.getDataObject(),[],'');
            end;
          end else begin
            wrtProc := FProcs[AProp.getType().getTypeEnum()].WriterListProc;
            if Assigned(wrtProc) then begin
              crsr.Reset();
              while crsr.MoveNext() do begin
                wrtProc(AProp.getName(),ls);
              end;
            end;
          end;
        finally
          crsr.GotoBookmark(bmk);
        end;
      finally
        PopSerializationStyle();
      end;
    finally
      FStreamer.EndScope();
    end;
  end;

  procedure WriteRefObjectProp(const ADataObject : ISDODataObject; const AProp : ISDOProperty);
  var
    locStrBuffer : TSDOString;
    locObjBuffer : ISDODataObject;
  begin
    locObjBuffer := AObject.getDataObject(AProp);
    if ( locObjBuffer <> nil ) and ( owoPostRefProperties in AOptions ) then begin
      AddPropertyRefRecallWrite(rritPropertyValue,ADataObject,AProp,locObjBuffer);
    end else begin
      FStreamer.BeginObject(AProp.getName(),AProp.getType());
      try
        if ( locObjBuffer = nil ) then begin
          FStreamer.NilCurrentScope()
        end else begin
          if ( FChangeSummary <> nil ) and FChangeSummary.isDeleted(locObjBuffer) then
            locStrBuffer := getObjectPathInChangeSummary(locObjBuffer)
          else
            locStrBuffer := getObjectPath(locObjBuffer,FRootName);
          FStreamer.PutScopeInnerValue(getType(StringType),locStrBuffer);
        end;
      finally
        FStreamer.EndScope();
      end;
    end;
  end;

  procedure WriteReference();{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    FStreamer.SetNameStyle(nsQualified);
      FStreamer.Put(sdo_namespace, s_ref,getType(StringType),AReference);
    FStreamer.SetNameStyle(nsUnqualified);
  end;

  procedure Local_WriteNilObject();{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    FStreamer.BeginObject(AName,FDataFactory.getType(sdo_namespace,SDOTypeDefaultTypeNames[ObjectType]));
    try
      FStreamer.NilCurrentScope();
      if ( owoWriteReference in AOptions ) then
        WriteReference();
    finally
      FStreamer.SetNameStyle(nsUnqualified);
      FStreamer.EndScope();
    end;
  end;

  procedure Local_WriteChangeSummary();{$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    locChangeSummary : ISDOChangeSummary;
  begin
    locChangeSummary := AObject.getChangeSummary();
    if ( locChangeSummary <> nil ) then
      WriteChangeSummary(locChangeSummary,AName);
  end;

var
  locType : ISDOType;
  locPropList : ISDOPropertyList;
  locProp : ISDOProperty;
  i, c : PtrInt;
  prpProc : TPropWriterProc;
  locSerializationStyle : TSerializationStyle;
begin
  if ( AObject = nil ) then begin
    Local_WriteNilObject();
  end else begin
    if ( owoWriteChangeSummary in AOptions ) then
      Local_WriteChangeSummary();
    FStreamer.BeginObject(AName,AObject.getType());
    try
      if ( owoWriteReference in AOptions ) then
        WriteReference();
      FStreamer.SetNameStyle(nsUnqualified);
      { TODO : The Serialization style should be a Property based option! }
//    PushSerializationStyle(ssAttibuteSerialization);
  //  try
        locPropList := AObject.getInstanceProperties();
        c := locPropList.getCount();
        for i := 0 to Pred(c) do begin
          locProp := locPropList.getItem(i);
          if locProp.isMany() then begin
            WriteArrayProp(locProp);
          end else begin
            if locProp.isAttribute() then
              locSerializationStyle := ssAttibuteSerialization
            else
              locSerializationStyle := ssNodeSerialization;
            PushSerializationStyle(locSerializationStyle);
              locType := locProp.getType();
              if locType.isDataObjectType() then begin
                if locProp.isContainment() then begin
                  InternalWriteObject(locProp.getName(),AObject.getDataObject(locProp),[],'');
                end else if locProp.isReference() then begin
                  WriteRefObjectProp(AObject,locProp);
                end;
              end else begin
                prpProc := FProcs[locType.getTypeEnum()].WriterProc;
                if Assigned(prpProc) then
                  prpProc(locProp,AObject);
              end;
            PopSerializationStyle();
          end;
        end;
//    finally
//      PopSerializationStyle();
//    end;
    finally
      FStreamer.EndScope();
    end;
  end;
end;

procedure TSDOSerializer.WriteStringListProp(const AName: string; const AValueList: ISDODataObjectList);
var
  tmpVal : TSDOString;
begin
  tmpVal := AValueList.getString();
  FStreamer.Put(AName,FProcs[StringType].PropType,tmpVal);
end;

procedure TSDOSerializer.WriteStringProp(const AProp: ISDOProperty; const AObject: ISDODataObject);
var
  tmpVal : TSDOString;
begin
  if not AObject.isNull(AProp) then begin
    tmpVal := AObject.getString(AProp);
    FStreamer.Put(AProp.getName(),FProcs[StringType].PropType,tmpVal);
  end;
end;



end.
