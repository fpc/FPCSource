{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements some SDO DAS utilities

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_das_utils;

interface
uses
  SysUtils, Classes, DB,
  sdo, sdo_das, data_acces_intf;

const
  BOOLEAN_TRUE_CHARS : set of AnsiChar = ['T','t', 'Y', 'y', 'O', 'o', '1'];

  DefaultSdoType = StringType;

{$IFNDEF HAS_SDO_FLOAT}
  FloatType = DefaultSdoType;
{$ENDIF HAS_SDO_FLOAT}
{$IFNDEF HAS_SDO_DOUBLE}
  DoubleType = DefaultSdoType;
{$ENDIF HAS_SDO_DOUBLE}
{$IFNDEF HAS_SDO_TIME}
  TimeType = DefaultSdoType;
{$ENDIF HAS_SDO_TIME}

  FieldTypeToSDOTypeMAP : array[TFieldType] of TSDOTypeKind = (
    StringType, StringType, ShortType, IntType, IntType,
    //ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    BooleanType, FloatType, CurrencyType, DoubleType, DateType, TimeType, DateTimeType,
    //ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    BytesType, BytesType, IntType, BytesType, BytesType, BytesType, BytesType,
    //ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    BytesType, BytesType, BytesType, BytesType, StringType, StringType,
    //ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    LongType, BytesType, BytesType, BytesType, ObjectType, BytesType, BytesType,
    //ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    BytesType, BytesType, BytesType, StringType, TimeType, DoubleType
    //ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd
{$IF Declared(ftFixedWideChar)}
    , StringType, StringType
    //ftFixedWideChar, ftWideMemo
{$IFEND }
{$IF Declared(ftOraTimeStamp)}
    , StringType, StringType
    //ftOraTimeStamp, ftOraInterval
{$IFEND }
{$IF Declared(ftLongWord)}
    , StringType, StringType, StringType, StringType, StringType, StringType, StringType,
    //ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream,
    StringType, StringType, StringType
    //ftTimeStampOffset, ftObject, ftSingle
{$IFEND }
  );

type

  TSDODASHandlerItem = class
  private
    FHandler: ISDODASObjectHandler;
    FTargetType: ISDOType;
  public
    property TargetType : ISDOType read FTargetType;
    property Handler : ISDODASObjectHandler read FHandler;
  end;

  procedure AssignValue(
    AObject : ISDODataObject;
    AProp   : ISDOProperty;
    AField  : TField
  );
  function GenerateInsertQuery(
    const ARowType   : ISDOType;
    const ATableName : string
  ) : string;
  function CopyToTable(
    const ARows      : ISDODataObjectList;
    const ATableName : string;
    const ADac       : TDataAccessInterface
  ) : Integer;overload;
  procedure CopyToTable(
    const AObject    : ISDODataObject;
    const ATableName : string;
    const ADac       : TDataAccessInterface
  );overload;



implementation
uses
  sdo_date_utils, sdo_imp_utils, sdo_linked_list;

procedure AssignValue(AObject : ISDODataObject; AProp : ISDOProperty; AField : TField);

  procedure AssignChar();
  var
    s : string;
  begin
    s := AField.AsString;
    if ( Length(s) = 0 ) then
      AObject.setCharacter(AProp,#0)
    else
      AObject.setCharacter(AProp,s[1]);
  end;

  procedure AssignBoolean();
  var
    s : string;
  begin
    s := AField.AsString;
    AObject.setBoolean(AProp,(Length(s) > 0) and (s[1] in BOOLEAN_TRUE_CHARS));
  end;

  procedure AssignBlobField();
  var
    locStream : TMemoryStream;
    locBlobField : TBlobField;
    locBytes : TSDOBytes;
    {$IFDEF DELPHI}
      locBytes2 : TBytes;
    {$ENDIF DELPHI}
  begin
    locBlobField := TBlobField(AField);
    if (locBlobField.BlobSize = 0) then begin
      AObject.setBytes(AProp,nil);
    end else begin
    {$IFDEF DELPHI}
      if (locBlobField.BlobType = ftWideMemo) then
        locBytes2 := TEncoding.UTF8.GetBytes(locBlobField.AsWideString)
      else
        locBytes2 := locBlobField.AsBytes;
      SetLength(locBytes,Length(locBytes2));
      if (Length(locBytes) > 0) then
        Move(locBytes2[0],locBytes[0],Length(locBytes));
      AObject.setBytes(AProp,locBytes);
    {$ENDIF DELPHI}
    {$IFDEF FPC}
      locStream := TMemoryStream.Create();
      try
        locBlobField.SaveToStream(locStream);
        locBytes := StreamToVarBytes(locStream);
      finally
        locStream.Free();
      end;
      AObject.setBytes(AProp,locBytes);
    {$ENDIF FPC}
    end;
  end;

begin
  if AField.IsNull then begin
    AObject.setNull(AProp);
  end else begin
    case AProp.getTypeEnum() of
      BooleanType   :
        begin
          if AField.InheritsFrom(TBooleanField) then
            AObject.setBoolean(AProp,AField.AsBoolean)
          else
            AssignBoolean();
        end;
      ByteType      : AObject.setByte(AProp,AField.AsInteger);
      BytesType     :
        begin
          if AField.InheritsFrom(TBlobField) then
            AssignBlobField()
          else
            AObject.setBytes(AProp, StringToVarBytes(AField.AsString));
        end;
      CharacterType : AssignChar();
      CurrencyType  : AObject.setCurrency(AProp,AField.AsCurrency);
      DateTimeType  : AObject.setDate(AProp,DateTimeToDateTimeRec(AField.AsDateTime));
      DoubleType    : AObject.setDouble(AProp,AField.AsFloat);
      FloatType     : AObject.setFloat(AProp,AField.AsFloat);
      IntegerType   : AObject.setInteger(AProp,AField.AsInteger);
      LongType      :
        begin
          if AField.InheritsFrom(TLargeintField) then
            AObject.setLong(AProp,TLargeintField(AField).AsLargeInt)
          else
            AObject.setLong(AProp,AField.AsInteger);
        end;
      ShortType     : AObject.setShort(AProp,AField.AsInteger);
      StringType  :
        begin
        {$IFDEF DEFAULT_UNICODE}
          AObject.setString(AProp,AField.AsString);
        {$ELSE DEFAULT_UNICODE}
          if (AField.DataType in [ftWideString,ftWideMemo,ftFixedWideChar]) then
            AObject.setString(AProp,AField.AsWideString)
          else
            AObject.setString(AProp,UTF8Decode(AField.AsString));
        {$ENDIF DEFAULT_UNICODE}
        end
      else
        raise ESDONotImplementedException.Create('TSDODAS.ExecuteQuery()>AssignValue()');
    end;
  end;
end;

function GenerateInsertQuery(
  const ARowType   : ISDOType;
  const ATableName : string
) : string;
var
  locRowType : ISDOType;
  pl : ISDOPropertyList;
  p : ISDOProperty;
  locInsertSQLCol, locInsertSQLVal: string;
  k, c : Integer;
begin
  locRowType := ARowType;
  pl := locRowType.getProperties();
  locInsertSQLCol := '';
  locInsertSQLVal := '';
  c := 0;
  for k := 0 to pl.getCount() - 1 do begin
    p := pl.getItem(k);
    if p.getType().isDataType() then begin
      locInsertSQLCol := Format('%s, %s',[locInsertSQLCol,p.getName()]);
      locInsertSQLVal := Format('%s, :P%d',[locInsertSQLVal,c]);
      Inc(c);
    end;
  end;
  Delete(locInsertSQLCol,1,1);
  Delete(locInsertSQLVal,1,1);
  Result :=
    Format(
      'INSERT INTO %s(%s) VALUES(%s)',
      [Trim(ATableName),locInsertSQLCol,locInsertSQLVal]
    );
end;

function CopyToTable(
  const ARows      : ISDODataObjectList;
  const ATableName : string;
  const ADac       : TDataAccessInterface
) : Integer;
var
  locRowType, locPropType : ISDOType;
  locInsertSQLQuery: string;
  k, kc, kp : Integer;
  locCursor : ILinkedListCursor;
  locRow : ISDODataObject;
  locParams : array of Variant;
  locRowPropList : ISDOPropertyList;
  locIsDataType : array of record IsDataType : Boolean; Index : Integer; end;
begin
  Result := ARows.size();
  if (Result = 0) then
    exit;
  locRowType := ARows.getDataObject(0).getType();
  locRowPropList := locRowType.getProperties();
  locInsertSQLQuery := GenerateInsertQuery(locRowType,ATableName);
  kc := locRowPropList.getCount();
  SetLength(locIsDataType,kc);
  kp := 0;
  for k := 0 to kc - 1 do begin
    locPropType := locRowPropList.getItem(k).getType();
    locIsDataType[k].IsDataType := locPropType.isDataType();
    if locIsDataType[k].IsDataType then begin
      locIsDataType[k].Index := kp;
      Inc(kp);
    end;
  end;
  SetLength(locParams,kp);
  locCursor := ARows.getCursor();
  locCursor.Reset();
  while locCursor.MoveNext() do begin
    locRow := ARows.getDataObject();
    for k := 0 to kc - 1 do begin
      if locIsDataType[k].IsDataType then
        locParams[locIsDataType[k].Index] := locRow.getVariant(k);
    end;
    ADac.ExecuteNonDataset(locInsertSQLQuery,locParams);
  end;
end;

procedure CopyToTable(
  const AObject    : ISDODataObject;
  const ATableName : string;
  const ADac       : TDataAccessInterface
);
var
  locRowType, locPropType : ISDOType;
  locInsertSQLQuery: string;
  k, kc, kp : Integer;
  locParams : array of Variant;
  locRowPropList : ISDOPropertyList;
  locIsDataType : array of record IsDataType : Boolean; Index : Integer; end;
begin
  if (AObject = nil) then
    exit;
  locRowType := AObject.getType();
  locRowPropList := locRowType.getProperties();
  locInsertSQLQuery := GenerateInsertQuery(locRowType,ATableName);
  kc := locRowPropList.getCount();
  SetLength(locIsDataType,kc);
  kp := 0;
  for k := 0 to kc - 1 do begin
    locPropType := locRowPropList.getItem(k).getType();
    locIsDataType[k].IsDataType := locPropType.isDataType();
    if locIsDataType[k].IsDataType then begin
      locIsDataType[k].Index := kp;
      Inc(kp);
    end;
  end;
  SetLength(locParams,kp);
  for k := 0 to kc - 1 do begin
    if locIsDataType[k].IsDataType then
      locParams[locIsDataType[k].Index] := AObject.getVariant(k);
  end;
  ADac.ExecuteNonDataset(locInsertSQLQuery,locParams);
end;


end.
