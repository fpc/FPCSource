{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements some SDO utilities

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_utils;

interface
uses
  sdo, sdo_types;

type
  TListValueExtractProc = procedure (const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
  function getExtractor(const AType : TSDOTypeKind) : TListValueExtractProc ; {$IFDEF USE_INLINE}inline;{$ENDIF}

  function getRoot(const ADataObject : ISDODataObject) : ISDODataObject;
  function xpath_ExcludeRootElement(const APath, ARootName : string) : string;

  procedure ClearList(const AList : ISDODataObjectList);
  function indexOf(
    const ADataObject :ISDODataObject;
    const AList : ISDODataObjectList;
    out APos : ISDOCursorBookmark
  ) : Boolean; overload;
  function indexOf(
    const ADataObject :ISDODataObject;
    const AList : ISDODataObjectList
  ) : PtrInt; overload;
  function indexOf(
    const ADataObject :ISDODataObject;
    const AList : ISDOChangedDataObjectList
  ) : PtrInt; overload;

  function InheritsFrom(const AChild, AParent : ISDOType) : Boolean;

implementation
uses
  SysUtils;

function InheritsFrom(const AChild, AParent : ISDOType) : Boolean;
var
  locType : ISDOType;
begin
  Result := False;
  if (AParent = nil) then
    Exit;

  locType := AChild;
  while (locType <> nil) do begin
    if locType.equals(AParent) then begin
      Result := True;
      Break;
    end;
    locType := locType.getBaseType();
  end;
end;

function getRoot(const ADataObject : ISDODataObject) : ISDODataObject;
var
  p, q : ISDODataObject;
begin
  if ( ADataObject <> nil ) then begin
    p := ADataObject;
    repeat
      q := p;
      p := p.getContainer();
    until ( p = nil );
  end else begin
    q := nil;
  end;
  Result := q;
end;

function xpath_ExcludeRootElement(const APath, ARootName : string) : string;
var
  i : PtrInt;
begin
  Result := TrimLeft(APath);
  if ( Length(Result) > 0 ) then begin
    if ( Result[1] = '#' ) then
      Delete(Result,1,1);
    if ( Length(Result) > 0 ) then begin
      if ( Result[1] = '/' ) then
        Delete(Result,1,1);
      i := Pos(ARootName,Result);
      if ( i = 1 ) then begin
        if ( Length(Result) > Length(ARootName) ) then begin
          if ( Result[Length(ARootName) + 1] = '/' ) then
            Delete(Result,1,Length(ARootName) + 1);
        end else begin
          Result := '';
        end;
      end;
    end;
  end;
end;

procedure ClearList(const AList : ISDODataObjectList);
var
  crs : ISDOCursor;
begin
  if Assigned(AList) then begin
    crs := AList.getCursor();
    crs.Reset();
    while crs.MoveFirst() do begin
      AList.delete();
    end;
  end;
end;

function indexOf(
  const ADataObject :ISDODataObject;
  const AList : ISDODataObjectList;
  out APos : ISDOCursorBookmark
) : Boolean;
var
  bm : ISDOCursorBookmark;
  crs : ISDOCursor;
begin
  APos := nil;
  Result := False;
  crs := AList.getCursor();
  bm := crs.GetBookmark();
  try
    crs.Reset();
    while crs.MoveNext() do begin
      if ( ADataObject = AList.getDataObject() ) then begin
        APos := crs.GetBookmark();
        Result := True;
        Break;
      end;
    end;
  finally
    crs.GotoBookmark(bm);
  end;
end;

function indexOf(
  const ADataObject :ISDODataObject;
  const AList : ISDODataObjectList
) : PtrInt;
var
  bm : ISDOCursorBookmark;
  crs : ISDOCursor;
begin
  Result := -1;
  crs := AList.getCursor();
  bm := crs.GetBookmark();
  try
    crs.Reset();
    while crs.MoveNext() do begin
      if ( ADataObject = AList.getDataObject() ) then begin
        Result := crs.GetPosition();
        Break;
      end;
    end;
  finally
    crs.GotoBookmark(bm);
  end;
end;

function indexOf(
  const ADataObject :ISDODataObject;
  const AList : ISDOChangedDataObjectList
) : PtrInt;
var
  i, c : PtrInt;
begin
  Result := -1;
  c := AList.size();
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      if ( ADataObject = AList.getDataObject(i) ) then begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

// START : List value extractors
procedure list_extractor_error(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  raise ESDONotImplementedException.Create('list_extractor_error');
end;

procedure list_extractor_bool(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.BooleanValue := AList.getBoolean();
end;

procedure list_extractor_byte(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.ByteValue := AList.getByte();
end;

{$IFDEF HAS_SDO_BYTES}
procedure list_extractor_bytes(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.BytesValue^ := AList.getBytes();
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure list_extractor_char(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.CharValue := AList.getCharacter();
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure list_extractor_currency(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.CurrencyValue := AList.getCurrency();
end;
{$ENDIF HAS_SDO_CURRENCY}

procedure list_extractor_date(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.DateValue := AList.getDate();
end;

{$IFDEF HAS_SDO_DOUBLE}
procedure list_extractor_double(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.DoubleValue := AList.getDouble();
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
procedure list_extractor_float(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.FloatValue := AList.getFloat();
end;
{$ENDIF HAS_SDO_FLOAT}

procedure list_extractor_integer(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.IntegerValue := AList.getInteger();
end;

{$IFDEF HAS_SDO_LONG}
procedure list_extractor_long(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.LongValue := AList.getLong();
end;
{$ENDIF HAS_SDO_LONG}

procedure list_extractor_object(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.ObjectValue^ := AList.getDataObject();
end;

{$IFDEF HAS_SDO_SHORT}
procedure list_extractor_short(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.ShortValue := AList.getShort();
end;
{$ENDIF HAS_SDO_SHORT}

procedure list_extractor_string(const AList : ISDODataObjectList; var AValueBuffer : TValueBuffer);
begin
  AValueBuffer.StringValue^ := AList.getString();
end;

var
  ListValueExtractProcs : array[TSDOTypeKind] of TListValueExtractProc = (
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_bool, // BooleanType,
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_byte, // ByteType,
{$IFDEF HAS_SDO_BYTES}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_bytes, // BytesType,
{$ENDIF HAS_SDO_BYTES}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_error, // ChangeSummaryType,
{$IFDEF HAS_SDO_CHAR}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_char, // CharacterType,
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_currency, // CurrencyType,
{$ENDIF HAS_SDO_CURRENCY}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_date, // DateTimeType,
{$IFDEF HAS_SDO_DOUBLE}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_double, // DoubleType,
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_float, // FloatType,
{$ENDIF HAS_SDO_FLOAT}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_integer , //IntegerType,
{$IFDEF HAS_SDO_LONG}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_long, // LongType,
{$ENDIF HAS_SDO_LONG}    
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_object, // ObjectType,
{$IFDEF HAS_SDO_SHORT}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_short, // ShortType,
{$ENDIF HAS_SDO_SHORT}
    {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}list_extractor_string // StringType,
  );

function getExtractor(const AType : TSDOTypeKind) : TListValueExtractProc ;
begin
  Result := ListValueExtractProcs[AType];
end;

// END : List value extractors
end.
