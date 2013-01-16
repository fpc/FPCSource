{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements the change summary handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
{ some notes :
    > DataObject Life cycle
        Dataobject life cycle starts by their inclusion as a containment property value of :
          - a DataObject with an attached ChangeSummary ( the root ),
          - a DataObject that ultime containment parent is a DataObject with an attached ChangeSummary
        Dataobject life cycle ends when their containment parent is set to null.

        It is important to note that Dataobject life cycle is a logical one, not
        a memory reference one. Once a DataObject leave a Dataobject containment tree,
        it will be accepted later _only_ as a new DataObject regardless of the fact that
        it was an ancient member.
}
unit sdo_changesummary;

interface
uses
  SysUtils, Classes, Contnrs,
  sdo, sdo_types;

type

  TDataObjectChangeInfo = class;
  TChangeRecorder = class;
  TManyValuePropChanges = class;
  TManyValuePropChangesList = class;

  ISDOChangedDataObjectListEx = interface(ISDOChangedDataObjectList)
    ['{FCF8810B-EBC0-47CF-B029-12E4C3826D83}']
    function append(const ADataObject : ISDODataObject; const AChange : TChangeType) : TDataObjectChangeInfo;
    function find(const ADataObject : ISDODataObject; out AIndex : PtrInt) : Boolean;
    function getInfo(const AIndex : PtrInt) : TDataObjectChangeInfo;
    procedure Delete(const AIndex : PtrInt);
    procedure Extract(const AItem : TDataObjectChangeInfo);
    procedure Clear();
  end;

  ISDOChangeSummaryEx = interface(ISDOChangeSummary)
    ['{715F0043-8F48-4AAA-AC87-A8BFA6A135D8}']
    function getRecorder() : TChangeRecorder;
    // Suspends the logging without clearing the log
    procedure suspend();
    // resumes the logging without clearing the log
    procedure resume();

    function FindManyValueChanges(const ADataObject : ISDODataObject) : TManyValuePropChangesList;
  end;

  TSDOSettingList = class(TInterfacedObject,IInterface,ISDOSettingList)
  private
    FList : TObjectList;
  private
    procedure CheckIndex(const AIndex : PtrInt);{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function size() : PtrInt;
    function getItem(const AIndex : PtrInt) : TValueSetting;
    procedure insert (const AIndex : PtrInt; const ASetting : TValueSetting);
    procedure append (const ASetting : TValueSetting);
    procedure remove (const AIndex : PtrInt);
  public
    constructor Create();
    destructor Destroy();override;
  end;

  TManyValuePropAction = ( mvpaAppend, mvpaInsert, mvpaChange, mvpaDelete );

  TManyValuePropRecordData = class
  private
    FIndex: PtrInt;
    FAction: TManyValuePropAction;
    FValue: TValueBuffer;
    FOwner: TManyValuePropChanges;
  public
    constructor Create(const AOwner: TManyValuePropChanges);
    destructor Destroy();override;
    property Owner : TManyValuePropChanges read FOwner;
    property Action : TManyValuePropAction read FAction;
    property Index : PtrInt read FIndex;
    property Value : TValueBuffer read FValue;
  end;

  TManyValuePropChanges = class
  private
    FList : TObjectList;
    FProp: ISDOProperty;
  private
    function InternalAdd(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const ALocation : PtrInt = -1
    ) : TManyValuePropRecordData;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(const AProp : ISDOProperty);
    destructor Destroy();override;
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOBoolean
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_CHAR}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOChar
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function AddCurrency(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOCurrency
    ) : TManyValuePropRecordData;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDODouble
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOFloat
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOLong
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOShort
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}
{$IFDEF HAS_SDO_BYTES}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOBytes
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}

    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOInteger
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : ISDODataObject
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(
      const ALocation : PtrInt;
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : ISDODataObject
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TValueBuffer
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}

    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOString
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDOByte
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Add(
      const AAction : TManyValuePropAction;
      const AIndex : PtrInt;
      const AValue : TSDODateTime
    ) : TManyValuePropRecordData;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Count() : PtrInt;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetItem(const AIndex : PtrInt) : TManyValuePropRecordData;{$IFDEF USE_INLINE}inline;{$ENDIF}
    property Prop : ISDOProperty read FProp;
  end;

  TManyValuePropChangesList = class
  private
    FList : TObjectList;
  private
    function GetItem(const AIndex: PtrInt): TManyValuePropChanges;
  protected
    procedure Add(const AItem : TManyValuePropChanges);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IndexOf(const AProperty : ISDOProperty) : PtrInt;
  public
    constructor Create();
    destructor Destroy();override;
    function Count() : PtrInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Find(const AProperty : ISDOProperty) : TManyValuePropChanges;{$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[const AIndex : PtrInt] : TManyValuePropChanges read GetItem;default;
  end;

  TDataObjectChangeInfo = class
  private
    FDataObject : Pointer;
    FChangeList : ISDOSettingList;
    FChangeType : TChangeType;
    FOldContainer : Pointer;
    FOldContainmentProperty: ISDOProperty;
    FManyValuePropChangesList : TManyValuePropChangesList;
    // Contains the list properties that do not have their old values in FChangeList
    FManyValuePropPending : IInterfaceList;
    FIndex: PtrInt;
  private
    function GetDataObject: ISDODataObject;
    function getOldContainer: ISDODataObject;
  public
    constructor Create(
      const ADataObject : ISDODataObject;
      const AChangeType : TChangeType
    );
    destructor Destroy();override;
    procedure CaptureOldContainment();
    //procedure SetChangeType(const AChangeType : TChangeType);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure ExtractPendingOldValues();
    function FindChanges(const AProp : ISDOProperty) : TManyValuePropChanges;
    function GetChanges(const AProp : ISDOProperty) : TManyValuePropChanges;{$IFDEF USE_INLINE}inline;{$ENDIF}
    property DataObject : ISDODataObject read GetDataObject;
    property ChangeType : TChangeType read FChangeType;
    property Index : PtrInt read FIndex;
    property ChangeList : ISDOSettingList read FChangeList;
    property OldContainer : ISDODataObject read getOldContainer;
    property OldContainmentProperty : ISDOProperty read FOldContainmentProperty;
    property ManyValuePropChangesList : TManyValuePropChangesList read FManyValuePropChangesList;
  end;

  TSDOChangedDataObjectList = class(
    TInterfacedObject,
    IInterface,
    ISDOChangedDataObjectList,
    ISDOChangedDataObjectListEx
  )
  private
    FList : TObjectList;
  private
    procedure CheckIndex(const AIndex : PtrInt);{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    // ISDOChangedDataObjectList
    function size() : PtrInt;
    function getType(const AIndex : PtrInt) : TChangeType;
    function getDataObject(const AIndex : PtrInt) : ISDODataObject;
    //ISDOChangedDataObjectListEx
    function append(const ADataObject : ISDODataObject; const AChange : TChangeType) : TDataObjectChangeInfo;
    function find(const ADataObject : ISDODataObject; out AIndex : PtrInt) : Boolean;
    function getInfo(const AIndex : PtrInt) : TDataObjectChangeInfo;
    procedure Delete(const AIndex : PtrInt);
    procedure Extract(const AItem : TDataObjectChangeInfo);
    procedure Clear();
  public
    constructor Create();
    destructor Destroy();override;
  end;

  TRecordingStyle = (
    rsDontOverwrite, rsOverwrite
    //rsRecordNestedObjects, rsRecordNestedObjectRef, rsRecordNestedObjectCopy
  );
  TRecordingStyleSet = set of TRecordingStyle;
  TChangeRecorder = class
  private
    FStore : ISDOChangedDataObjectListEx;
    FOwner : Pointer; // do not keep a reference
    FDeserializing: Boolean;
  private
    function GetStore() : ISDOChangedDataObjectList;
    function GetOwner() : ISDOChangeSummaryEx;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function recordChange(
      const ADataObject : ISDODataObject;
      const AChangeType : TChangeType
    ) : TDataObjectChangeInfo;overload;
  public
    constructor Create(
      const AStore : ISDOChangedDataObjectList;
      const AOwner : ISDOChangeSummary
    );
    property Store : ISDOChangedDataObjectList read GetStore;
    procedure recordChange(
      const ADataObject : ISDODataObject;
      const AProperty   : ISDOProperty;
      const AOptions : TRecordingStyleSet = [rsDontOverwrite]
    );overload;
    // this method suppose the property cursor is set to "AIndex" position
    procedure recordChange(
      const ADataObject : ISDODataObject;
      const AProperty   : ISDOProperty;
      const AIndex      : PtrInt;
      const AAction     : TManyValuePropAction
    );overload;

    procedure recordCreation(const ADataObject : ISDODataObject);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure recordCreation(const ADataObject : ISDODataObject; const AIndex : PtrInt);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure recordDeletion(const ADataObject : ISDODataObject);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure recordDeletion(const ADataObject : ISDODataObject; const AIndex : PtrInt);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function recordUpdate(const ADataObject : ISDODataObject) : TDataObjectChangeInfo;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure UndoChangeItem(
      const AItem : TDataObjectChangeInfo;
      const AProcessNestedObjProps : Boolean
    );

    property Deserializing : Boolean read FDeserializing write FDeserializing;
  end;

  TSDOChangeSummary = class(
    TInterfacedObject,
    IInterface,
    ISDOChangeSummary,
    ISDOChangeSummaryEx
  )
  private
    FLogging : Boolean;
    FChangesData : ISDOChangedDataObjectListEx;
    FRecorder : TChangeRecorder;
  private
    procedure Clear();
  protected
    function getChangedDataObjects() : ISDOChangedDataObjectList;
    function getOldValues(const ADataObject : ISDODataObject) : ISDOSettingList;
    function getOldXpath(const ADataObject : ISDODataObject) : string;
    procedure beginLogging();
    procedure endLogging();
    function isLogging() : Boolean;
    function isCreated(const ADataObject : ISDODataObject) : Boolean;
    function isDeleted(const ADataObject : ISDODataObject) : Boolean;
    function isModified(const ADataObject : ISDODataObject) : Boolean;
    function getOldValue(const ADataObject : ISDODataObject; const AProperty : ISDOProperty) : TValueSetting;
    function getOldContainer(const ADataObject : ISDODataObject) : ISDODataObject;
    function getOldContainmentProperty(const ADataObject : ISDODataObject) : ISDOProperty;
    procedure undoChanges() ;
    //SequencePtr getOldSequence(DataObjectPtr dataObject);
    // -- > ISDOChangeSummaryEx
    function getRecorder() : TChangeRecorder;
    // Suspends the logging without clearing the log
    procedure suspend();
    // resumes the logging without clearing the log
    procedure resume();

    function FindManyValueChanges(const ADataObject : ISDODataObject) : TManyValuePropChangesList;
  public
    constructor Create(const AChangesData : ISDOChangedDataObjectList);
    destructor Destroy();override;
  end;

  {TfindDichoExtractor = function(const AList : TObjectList; const AIndex : PtrInt) : PtrInt;
  function findDicho(
    const AList  : TObjectList;
    const AItem  : PtrInt;
    out   AIndex : PtrInt;
    const AExtractor : TfindDichoExtractor
  ) : Boolean; }

implementation

uses
  sdo_utils;

{function DataObjectExtractor(const AList : TObjectList; const AIndex : PtrInt) : PtrInt;
begin
  Result := PtrInt(TDataObjectChangeInfo(AList[AIndex]).DataObject);
end;

function findDicho(
  const AList  : TObjectList;
  const AItem  : PtrInt;
  out   AIndex : PtrInt;
  const AExtractor : TfindDichoExtractor
) : Boolean;
var
  i, j, k, betterPos, objValue, listObjvalue : PtrInt;
  ok : Boolean;
begin
  ok := False;
  if ( AList.Count = 0 ) then begin
    betterPos := 0;
  end else begin
    objValue := AItem;
    i := 1;
    j := AList.Count;
    ok := False;
    while True do begin
      k := ( i + j ) div 2;
      listObjvalue := AExtractor(AList,(k - 1));
      if ( listObjvalue = objValue ) then begin
        ok := True;
        Break;
      end else if ( listObjvalue < objValue ) then begin
        if ( i = j ) then begin
          k := k + 1;
          Break;
        end;
        if ( i = k ) then
          Inc(i)
        else
          i := k;
      end else begin
        if ( i = j ) then begin
          k := k;
          Break;
        end;
        if ( j = k ) then
          Dec(j)
        else
          j := k;
      end;
    end;
    Dec(k);
    betterPos := k;
  end;
  AIndex := betterPos;
  Result := ok;
end; }

procedure listUndo_error(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  raise ESDONotImplementedException.CreateFmt('TListUndoItemProcs not implemented for this type : "%s".',[AItm.Owner.Prop.getType().getName()]);
end;

//--------------- START : Boolean procs -----------------------------
procedure listUndo_append_bool(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_bool');
  AList.delete();
end;

procedure listUndo_insert_bool(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_bool');
  AList.delete();
end;

procedure listUndo_delete_bool(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end;
  AList.insert(AItm.Index,AItm.Value.BooleanValue);
end;

procedure listUndo_change_bool(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_bool');
  AList.setBoolean(AItm.Index,AItm.Value.BooleanValue);
end;
//--------------- END: Boolean procs -----------------------------

//--------------- START : Byte procs -----------------------------
procedure listUndo_append_byte(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_byte');
  AList.delete();
end;

procedure listUndo_insert_byte(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_byte');
  AList.delete();
end;

procedure listUndo_delete_byte(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_byte');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_byte');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_byte');
  end;
  AList.insert(AItm.Index,AItm.Value.ByteValue);
end;

procedure listUndo_change_byte(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_byte');
  AList.setByte(AItm.Index,AItm.Value.ByteValue);
end;
//--------------- END: Byte procs -----------------------------

{$IFDEF HAS_SDO_BYTES}
//--------------- START : Char procs -----------------------------
procedure listUndo_append_bytes(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_bytes');
  AList.delete();
end;

procedure listUndo_insert_bytes(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_bytes');
  AList.delete();
end;

procedure listUndo_delete_bytes(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_bytes');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_bytes');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_bytes');
  end;
  AList.insertBytes(AItm.Index,AItm.Value.BytesValue^);
end;

procedure listUndo_change_bytes(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_bytes');
  AList.setBytes(AItm.Index,AItm.Value.BytesValue^);
end;
//--------------- END: Bytes procs -----------------------------
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
//--------------- START : Char procs -----------------------------
procedure listUndo_append_char(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_char');
  AList.delete();
end;

procedure listUndo_insert_char(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_char');
  AList.delete();
end;

procedure listUndo_delete_char(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_char');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_char');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_char');
  end;
  AList.insert(AItm.Index,AItm.Value.CharValue);
end;

procedure listUndo_change_char(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_char');
  AList.setCharacter(AItm.Index,AItm.Value.CharValue);
end;
//--------------- END: Char procs -----------------------------
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
//--------------- START : Currency procs -----------------------------
procedure listUndo_append_currency(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_currency');
  AList.delete();
end;

procedure listUndo_insert_currency(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_currency');
  AList.delete();
end;

procedure listUndo_delete_currency(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_currency');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_currency');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_currency');
  end;
  AList.insertCurrency(AItm.Index,AItm.Value.CurrencyValue);
end;

procedure listUndo_change_currency(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_currency');
  AList.setCurrency(AItm.Index,AItm.Value.CurrencyValue);
end;
//--------------- END: Currency procs -----------------------------
{$ENDIF HAS_SDO_CURRENCY}

//--------------- START : Date procs -----------------------------
procedure listUndo_append_date(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_date');
  AList.delete();
end;

procedure listUndo_insert_date(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_date');
  AList.delete();
end;

procedure listUndo_delete_date(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_date');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_date');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_date');
  end;
  AList.insert(AItm.Index,AItm.Value.DateValue);
end;

procedure listUndo_change_date(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_date');
  AList.setDate(AItm.Index,AItm.Value.DateValue);
end;
//--------------- END: Date procs -----------------------------

{$IFDEF HAS_SDO_DOUBLE}
//--------------- START : Double procs -----------------------------
procedure listUndo_append_double(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_double');
  AList.delete();
end;

procedure listUndo_insert_double(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_double');
  AList.delete();
end;

procedure listUndo_delete_double(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_double');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_double');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_double');
  end;
  AList.insert(AItm.Index,AItm.Value.DoubleValue);
end;

procedure listUndo_change_double(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_double');
  AList.setDouble(AItm.Index,AItm.Value.DoubleValue);
end;
//--------------- END: Double procs -----------------------------
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
//--------------- START : Float procs -----------------------------
procedure listUndo_append_float(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_float');
  AList.delete();
end;

procedure listUndo_insert_float(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_float');
  AList.delete();
end;

procedure listUndo_delete_float(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_float');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_float');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_float');
  end;
  AList.insert(AItm.Index,AItm.Value.FloatValue);
end;

procedure listUndo_change_float(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_float');
  AList.setFloat(AItm.Index,AItm.Value.FloatValue);
end;
//--------------- END: Float procs -----------------------------
{$ENDIF HAS_SDO_FLOAT}

//--------------- START : Integer procs -----------------------------
procedure listUndo_append_integer(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_integer');
  AList.delete();
end;

procedure listUndo_insert_integer(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_integer');
  AList.delete();
end;

procedure listUndo_delete_integer(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_integer');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_integer');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_integer');
  end;
  AList.insert(AItm.Index,AItm.Value.IntegerValue);
end;

procedure listUndo_change_integer(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_integer');
  AList.setInteger(AItm.Index,AItm.Value.IntegerValue);
end;
//--------------- END: Integer procs -----------------------------

{$IFDEF HAS_SDO_LONG}
//--------------- START : Long procs -----------------------------
procedure listUndo_append_long(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_long');
  AList.delete();
end;

procedure listUndo_insert_long(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_long');
  AList.delete();
end;

procedure listUndo_delete_long(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_long');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_long');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_long');
  end;
  AList.insert(AItm.Index,AItm.Value.LongValue);
end;

procedure listUndo_change_long(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_long');
  AList.setLong(AItm.Index,AItm.Value.LongValue);
end;
//--------------- END: Long procs -----------------------------
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
//--------------- START : Short procs -----------------------------
procedure listUndo_append_short(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_short');
  AList.delete();
end;

procedure listUndo_insert_short(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_short');
  AList.delete();
end;

procedure listUndo_delete_short(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_short');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_short');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_short');
  end;
  AList.insert(AItm.Index,AItm.Value.ShortValue);
end;

procedure listUndo_change_short(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_short');
  AList.setShort(AItm.Index,AItm.Value.ShortValue);
end;
//--------------- END: Short procs -----------------------------
{$ENDIF HAS_SDO_SHORT}

//--------------- START : Object procs -----------------------------
procedure listUndo_append_object(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_object');
  AList.delete();
end;

procedure listUndo_insert_object(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_object');
  AList.delete();
end;

procedure listUndo_delete_object(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end;
  AList.insert(AItm.Index,AItm.Value.ObjectValue^);
end;

procedure listUndo_change_object(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_object');
  AList.setDataObject(AItm.Index,AItm.Value.ObjectValue^);
end;
//--------------- END : Object procs -----------------------------

//--------------- START : String procs -----------------------------
procedure listUndo_append_string(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveLast() then
    raise ESDOInvalidStateOperationException.Create('listUndo_append_string');
  AList.delete();
end;

procedure listUndo_insert_string(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_insert_string');
  AList.delete();
end;

procedure listUndo_delete_string(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
var
  crs : ISDOCursor;
begin
  crs := AList.getCursor();
  if ( AList.size() = 0 ) then begin
    if ( AItm.Index > 0 ) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end else if ( AList.size() = AItm.Index ) then begin
    if not crs.MoveLast() then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end else begin
    if not crs.MoveTo(AItm.Index) then
      raise ESDOInvalidStateOperationException.Create('listUndo_delete_object');
  end;
  AList.insert(AItm.Index,AItm.Value.StringValue^);
end;

procedure listUndo_change_string(const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
begin
  if not AList.getCursor().MoveTo(AItm.Index) then
    raise ESDOInvalidStateOperationException.Create('listUndo_change_string');
  AList.setString(AItm.Index,AItm.Value.StringValue^);
end;
//--------------- END : String procs -----------------------------


type
  TListUndoItemProc = procedure (const AList : ISDODataObjectList; const AItm : TManyValuePropRecordData);
  TListUndoItemProcs = record
    appendProc : TListUndoItemProc;
    insertProc : TListUndoItemProc;
    deleteProc : TListUndoItemProc;
    changeProc : TListUndoItemProc;
  end;

var
  LIST_UNDO_PROCS : array[TSDOTypeKind] of TListUndoItemProcs = (
    ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_bool;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_bool;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_bool;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_bool;
    ), //BooleanType,
    ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_byte;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_byte;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_byte;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_byte;
    ),//ByteType,
{$IFDEF HAS_SDO_BYTES}
   ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_bytes;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_bytes;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_bytes;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_bytes;
    ),//BytesType,
{$ENDIF HAS_SDO_BYTES}
    ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_error;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_error;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_error;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_error;
    ), //ChangeSummaryType,
{$IFDEF HAS_SDO_CHAR}
    ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_char;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_char;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_char;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_char;
    ), //CharacterType,
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_currency;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_currency;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_currency;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_currency;
    ), //CurrencyType,
{$ENDIF HAS_SDO_CURRENCY}
    ( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_date;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_date;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_date;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_date;
    ) //DateTimeType,
{$IFDEF HAS_SDO_DOUBLE}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_double;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_double;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_double;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_double;
    )//DoubleType,
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_float;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_float;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_float;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_float;
    )//FloatType,
{$ENDIF HAS_SDO_FLOAT}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_integer;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_integer;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_integer;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_integer;
    )  //IntegerType,
{$IFDEF HAS_SDO_LONG}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_long;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_long;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_long;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_long;
    ) //LongType,
{$ENDIF HAS_SDO_LONG}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_object;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_object;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_object;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_object;
    )//ObjectType,
{$IFDEF HAS_SDO_SHORT}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_short;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_short;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_short;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_short;
    )//ShortType,
{$ENDIF HAS_SDO_SHORT}
   ,( appendProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_append_string;
      insertProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_insert_string;
      deleteProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_delete_string;
      changeProc : {$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}listUndo_change_string;
    ) //StringType,

  );
procedure UndoChanges(
  const AList : ISDODataObjectList;
  const AChangesList : TManyValuePropChanges
);
var
  i, c : PtrInt;
  itm : TManyValuePropRecordData;
  procs : TListUndoItemProcs;
begin
  c := AChangesList.Count;
  if ( c > 0 ) then begin
    procs := LIST_UNDO_PROCS[AChangesList.GetItem(0).Owner.Prop.getTypeEnum()];
    for i := Pred(c) downto 0 do begin
      itm := AChangesList.GetItem(i);
      case itm.Action of
        mvpaAppend : procs.appendProc(AList,itm);
        mvpaInsert : procs.insertProc(AList,itm);
        mvpaChange : procs.changeProc(AList,itm);
        mvpaDelete : procs.deleteProc(AList,itm);
      end;
    end;
  end;
end;

{ TSDOSettingList }

procedure TSDOSettingList.append(const ASetting: TValueSetting);
begin
  if ( ASetting = nil ) then
    raise ESDOIllegalArgumentException.Create('ASetting');
  if ( FList.IndexOf(ASetting) = -1 ) then
    FList.Add(ASetting);
end;

procedure TSDOSettingList.CheckIndex(const AIndex: PtrInt);
begin
  if ( AIndex < 0 ) or ( AIndex >= FList.Count ) then
    raise ESDOIndexOutOfRangeException.Create(AIndex);
end;

constructor TSDOSettingList.Create();
begin
  inherited Create();
  FList := TObjectList.Create(True);
end;

destructor TSDOSettingList.Destroy();
begin
  FList.Free();
  inherited;
end;

function TSDOSettingList.getItem(const AIndex: PtrInt): TValueSetting;
begin
  CheckIndex(AIndex);
  Result := TValueSetting(FList[AIndex]);
end;

procedure TSDOSettingList.insert(const AIndex : PtrInt; const ASetting: TValueSetting);
begin
  if ( ASetting = nil ) then
    raise ESDOIllegalArgumentException.Create('ASetting');
  if ( FList.IndexOf(ASetting) = -1 ) then begin
    if ( AIndex <> 0 ) then
      CheckIndex(AIndex);
    FList.Insert(AIndex,ASetting);
  end;
end;

procedure TSDOSettingList.remove(const AIndex: PtrInt);
begin
  CheckIndex(AIndex);
  FList.Delete(AIndex);
end;

function TSDOSettingList.size() : PtrInt;
begin
  Result := FList.Count;
end;

{ TChangeRecorder }

constructor TChangeRecorder.Create(
  const AStore: ISDOChangedDataObjectList;
  const AOwner : ISDOChangeSummary
);
var
  oX : ISDOChangeSummaryEx;
begin
  if ( AStore = nil ) or ( not Supports(AStore,ISDOChangedDataObjectListEx,FStore)) then
    raise ESDOIllegalArgumentException.Create('AStore');
  if ( AOwner = nil ) or ( not Supports(AOwner,ISDOChangeSummaryEx,oX) ) then
    raise ESDOIllegalArgumentException.Create('AOwner');
  FOwner := Pointer(oX);
end;

procedure TChangeRecorder.recordChange(
  const ADataObject : ISDODataObject;
  const AProperty : ISDOProperty;
  const AOptions : TRecordingStyleSet
);
var
  changeInfo : TDataObjectChangeInfo;
  i, c, oldSettingPos : PtrInt;
  propValue : TValueBuffer;
  propValueStr : TSDOString;
  propValueObj : ISDODataObject;
  propValueBytes : TSDOBytes;
  stg, xstg : TValueSetting;
  locProp : ISDOProperty;
begin
  if ( ADataObject = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataObject');
  if ( AProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AProperty');

  locProp := AProperty as ISDOProperty;
  oldSettingPos := -1;
  if FStore.find(ADataObject,i) then begin
    changeInfo := FStore.getInfo(i);
    c := changeInfo.ChangeList.size();
    for i := 0 to Pred(c) do begin
      xstg := changeInfo.ChangeList.getItem(i);
      if ( xstg.getProperty() = locProp ) then begin
        oldSettingPos := i;
        Break;
      end;
    end;
  end else begin
    changeInfo := recordChange(ADataObject,ctChange);
  end;
  if ( changeInfo.ChangeType <> ctCreate ) and ( oldSettingPos = -1 ) or ( rsOverwrite in AOptions ) then begin
    case locProp.getTypeEnum() of
      BooleanType     : propValue.BooleanValue := ADataObject.getBoolean(locProp);
      ByteType        : propValue.ByteValue := ADataObject.getByte(locProp);
{$IFDEF HAS_SDO_BYTES}
      BytesType       : propValueBytes := ADataObject.getBytes(locProp);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType   : propValue.CharValue := ADataObject.getCharacter(locProp);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType    : propValue.CurrencyValue := ADataObject.getCurrency(locProp);
{$ENDIF HAS_SDO_CURRENCY}
      DateTimeType    : propValue.DateValue := ADataObject.getDate(locProp);
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType      : propValue.DoubleValue := ADataObject.getDouble(locProp);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType       : propValue.FloatValue := ADataObject.getFloat(locProp);
{$ENDIF HAS_SDO_FLOAT}
      IntegerType     : propValue.IntegerValue := ADataObject.getInteger(locProp);
{$IFDEF HAS_SDO_LONG}
      LongType        : propValue.LongValue := ADataObject.getLong(locProp);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType       : propValue.ShortValue := ADataObject.getShort(locProp);
{$ENDIF HAS_SDO_SHORT}
      ObjectType      : propValueObj := ADataObject.getDataObject(locProp);
      StringType      : propValueStr := ADataObject.getString(locProp);
    end;
    case locProp.getTypeEnum() of
      BooleanType, ByteType, DateTimeType, IntegerType
{$IFDEF HAS_SDO_CHAR}
      ,CharacterType
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      ,CurrencyType
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
      ,DoubleType
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      ,FloatType
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
      ,LongType
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ,ShortType 
{$ENDIF HAS_SDO_SHORT}
       :
        begin
          stg := TValueSetting.Create(
                   ADataObject.isSet(locProp),
                   ADataObject.isNull(locProp),
                   propValue,
                   locProp,
                   0
                 );
        end;
{$IFDEF HAS_SDO_BYTES}
      BytesType       :
        begin
         stg := TValueSetting.Create(
                  ADataObject.isSet(locProp),
                  ADataObject.isNull(locProp),
                  propValueBytes,
                  locProp,
                  0
                );
        end;
{$ENDIF HAS_SDO_BYTES}
      ObjectType :
        begin
          stg := TValueSetting.Create(
                   ADataObject.isSet(locProp),
                   ADataObject.isNull(locProp),
                   propValueObj,
                   locProp,
                   0
                 );
        end;
      StringType  :
        begin
         stg := TValueSetting.Create(
                  ADataObject.isSet(locProp),
                  ADataObject.isNull(locProp),
                  propValueStr,
                  locProp,
                  0
                );
        end;
      else
        raise ESDOException.CreateFmt('This data type is not supported yet by ChangeSummary API : "%s".',[SDOTypeDefaultTypeNames[locProp.getTypeEnum()]]);
    end;
    if ( oldSettingPos > -1 ) then
      changeInfo.ChangeList.remove(oldSettingPos);
    changeInfo.ChangeList.append(stg);
  end;
end;

procedure TChangeRecorder.recordChange(
  const ADataObject : ISDODataObject;
  const AProperty : ISDOProperty;
  const AIndex : PtrInt;
  const AAction : TManyValuePropAction
);
var
  changeInfo : TDataObjectChangeInfo;
  locProp : ISDOProperty;
  changesList : TManyValuePropChanges;
  i : PtrInt;
  ls : ISDODataObjectList;
begin
  if ( ADataObject = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataObject');
  if ( AProperty = nil ) then
    raise ESDOIllegalArgumentException.Create('AProperty');
  locProp := AProperty as ISDOProperty;
  if FStore.find(ADataObject,i) then begin
    changeInfo := FStore.getInfo(i);
  end else begin
    changeInfo := recordChange(ADataObject,ctChange);
  end;
  changesList := changeInfo.GetChanges(locProp);
  ls := ADataObject.getList(locProp);
  if ( AAction in [mvpaChange, mvpaDelete, mvpaAppend, mvpaInsert] ) then begin
    case locProp.getTypeEnum() of
      BooleanType     : changesList.Add(AAction,AIndex,ls.getBoolean());
      ByteType        : changesList.Add(AAction,AIndex,ls.getByte());
{$IFDEF HAS_SDO_BYTES}
      BytesType       : changesList.Add(AAction,AIndex,ls.getBytes());
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType   : changesList.Add(AAction,AIndex,ls.getCharacter());
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType   : changesList.AddCurrency(AAction,AIndex,ls.getCurrency());
{$ENDIF HAS_SDO_CURRENCY}
      DateTimeType    : changesList.Add(AAction,AIndex,ls.getDate());
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType      : changesList.Add(AAction,AIndex,ls.getDouble());
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType       : changesList.Add(AAction,AIndex,ls.getFloat());
{$ENDIF HAS_SDO_FLOAT}
      IntegerType     : changesList.Add(AAction,AIndex,ls.getInteger());
{$IFDEF HAS_SDO_LONG}
      LongType        : changesList.Add(AAction,AIndex,ls.getLong());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType       : changesList.Add(AAction,AIndex,ls.getShort());
{$ENDIF HAS_SDO_SHORT}
      ObjectType      : changesList.Add(AAction,AIndex,ls.getDataObject());
      StringType      : changesList.Add(AAction,AIndex,ls.getString());
      else
        raise ESDOException.CreateFmt('This data type is not supported yet by ChangeSummary API : "%s".',[SDOTypeDefaultTypeNames[locProp.getTypeEnum()]]);
    end;
  end else begin
    changesList.Add(AAction,AIndex);
  end;
  if ( locProp.getTypeEnum() = ObjectType ) then begin
    case AAction of
      mvpaAppend, mvpaInsert :
        recordChange(ls.getDataObject(),ctCreate).FIndex := ls.getCursor().GetPosition();
      mvpaDelete :
        begin
          changeInfo := recordChange(ls.getDataObject(),ctDelete);
          if ( changeInfo <> nil ) then
            changeInfo.FIndex := ls.getCursor().GetPosition();
        end;
    end;
  end;
end;

function TChangeRecorder.GetStore() : ISDOChangedDataObjectList;
begin
  Result := FStore;
end;

function TChangeRecorder.recordChange(
  const ADataObject: ISDODataObject;
  const AChangeType: TChangeType
) : TDataObjectChangeInfo;

  procedure UndoContainedObjectsChanges(const AObj : ISDODataObject);

    procedure ProcessItem(const AProperty : ISDOProperty);
    var
      d : PtrInt;
      po : ISDODataObject;
      poc : TDataObjectChangeInfo;
    begin
      po := AObj.getDataObject(AProperty);
      if ( po <> nil ) then begin
        if FStore.find(po,d) then begin
          poc := FStore.getInfo(d);
          UndoChangeItem(poc,True);
          FStore.Extract(poc);
          FreeAndNil(poc);
          po := AObj.getDataObject(AProperty);
          if FStore.find(po,d) then begin
            poc := FStore.getInfo(d);
            UndoChangeItem(poc,True);
            FStore.Extract(poc);
            FreeAndNil(poc);
          end;
        end;
        if ( po <> nil ) then
          UndoContainedObjectsChanges(po);
      end;
    end;

    procedure ProcessMultiItem(const AProperty : ISDOProperty);
    var
      d : PtrInt;
      po : ISDODataObject;
      poc : TDataObjectChangeInfo;
      ls : ISDODataObjectList;
      crs : ISDOCursor;
    begin
      ls := AObj.getList(AProperty);
      crs := ls.getCursor();
      crs.Reset();
      while crs.MoveNext() do begin
        po := ls.getDataObject();
        if ( po <> nil ) then begin
          if FStore.find(po,d) then begin
            poc := FStore.getInfo(d);
            UndoChangeItem(poc,True);
            FStore.Extract(poc);
            FreeAndNil(poc);
          end;
          if ( po <> nil ) then
            UndoContainedObjectsChanges(po);
        end;
      end;
    end;

  var
    k, q : PtrInt;
    pls : ISDOPropertyList;
    p : ISDOProperty;
    oldLoggingState : Boolean;
  begin
    if not Deserializing then begin
      pls := AObj.getInstanceProperties();
      q := pls.getCount();
      if ( q > 0 ) then begin
        oldLoggingState := GetOwner().isLogging();
        if oldLoggingState then
          GetOwner().suspend();
        try
          for k := 0 to Pred(q) do begin
            p := pls.getItem(k);
            if p.getType().isDataObjectType() and p.isContainment() then begin
              if p.isMany() then
                ProcessMultiItem(p)
              else
                ProcessItem(p);
            end;
          end;
        finally
          if oldLoggingState then
            GetOwner().resume();
        end;
      end;
    end;
  end;

var
  i : PtrInt;
  locInfoOLD, locInfo : TDataObjectChangeInfo;
  locDoCreate, locOldLogginState : Boolean;
begin
  if ( ADataObject = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataObject');
  locInfoOLD := nil;
  locDoCreate := True;
  if FStore.find(ADataObject,i) then begin
    locInfo := FStore.getInfo(i);
  end else begin
    i := -1;
    locInfo := nil;
  end;
  if ( i > -1 ) and ( locInfo.ChangeType <> AChangeType ) then begin
    case AChangeType of
      ctCreate :
        begin
          case locInfo.ChangeType of
            ctChange : Assert(False, 'This should not happen because a object the life''cycle is Create->(Modify)*->(Delete)*');
            ctDelete : Assert(False, 'This should not happen because on deletion it is a copy of the object that is keep in TDataObjectChangeInfo.');
          end;
        end;
      ctChange :
        begin
          case locInfo.ChangeType of
            ctCreate : ; //Normal in the life's cycle Create->(Modify)*->(Delete)*
            ctDelete : Assert(False, 'This should not happen because on deletion it is a copy of the object that is keep in TDataObjectChangeInfo.');
          end;
        end;
      ctDelete :
        begin
          case locInfo.ChangeType of
            //Normal in the life's cycle Create->(Modify)*->(Delete)*
            ctCreate :
              begin
                locDoCreate := False;
                FStore.extract(locInfo);
                FreeAndNil(locInfo);
              end;
            ctChange :
              begin
                locOldLogginState := GetOwner().isLogging();
                locInfoOLD := locInfo;
                FStore.extract(locInfoOLD);
                locInfo := nil;
                GetOwner().suspend();
                try
                  //UndoContainedObjectsChanges(ADataObject);
                  UndoChangeItem(locInfoOLD,True);
                finally
                  try
                    if ( locOldLogginState <> GetOwner().isLogging() ) then begin
                      if locOldLogginState then
                        GetOwner().resume()
                      else
                        GetOwner().suspend();
                    end;
                  finally
                    FreeAndNil(locInfoOLD);
                  end;
                end;
              end;
          end;
        end;
    end;
  end;
  if ( locInfo = nil ) and locDoCreate then begin
    locInfo := FStore.append(ADataObject,AChangeType);
  end;
  if ( AChangeType = ctDelete ) then
    UndoContainedObjectsChanges(ADataObject);
  Result := locInfo;
end;

procedure TChangeRecorder.recordDeletion(const ADataObject: ISDODataObject);
begin
  recordChange(ADataObject,ctDelete);
end;

procedure TChangeRecorder.recordCreation(const ADataObject: ISDODataObject);
begin
  recordChange(ADataObject,ctCreate);
end;

function TChangeRecorder.recordUpdate(const ADataObject: ISDODataObject): TDataObjectChangeInfo;
begin
  Result := recordChange(ADataObject,ctChange);
end;

procedure TChangeRecorder.UndoChangeItem(
  const AItem: TDataObjectChangeInfo;
  const AProcessNestedObjProps : Boolean
);

  procedure UndoManyPropsChanges();
  var
    changesList : TManyValuePropChangesList;
    elt : TManyValuePropChanges;
    q, k, x, y : PtrInt;
    tmpObj : ISDODataObject;
  begin
    changesList := AItem.ManyValuePropChangesList;
    if ( changesList <> nil ) then begin
      q := changesList.Count;
      if ( q > 0 ) then begin
        for k := 0 to ( q - 1 ) do begin
          elt := changesList[k];
          UndoChanges(AItem.DataObject.getList(elt.Prop),elt);
          if elt.Prop.getType().isDataObjectType() and elt.Prop.isContainment() then begin
            if ( elt.Count > 0 ) then begin
              for x := 0 to ( elt.Count - 1 ) do begin
                tmpObj := elt.GetItem(x).Value.ObjectValue^;
                if ( tmpObj <> nil ) then begin
                  if FStore.find(tmpObj,y) then begin
                    FStore.Delete(y);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure UndoModify();
  var
    stg : TValueSetting;

    procedure ProcessNestedProp(const AProp : ISDOProperty);
    var
      propVal : ISDODataObject;
      kk : PtrInt;
      storeX : ISDOChangedDataObjectListEx;
    begin
      propVal := AItem.DataObject.getDataObject(stg.getProperty());
      storeX := Store as ISDOChangedDataObjectListEx;
      if ( propVal <> nil ) and ( propVal <> stg.getDataObjectValue() ) then begin
        if storeX.find(propVal,kk) then
          UndoChangeItem(storeX.getInfo(kk),True);
      end;
      if storeX.find(stg.getDataObjectValue(),kk) then begin
        UndoChangeItem(storeX.getInfo(kk),True);
      end;
    end;

  var
    q, k : Ptrint;
  begin
    q := AItem.ChangeList.size();
    if ( q > 0 ) then begin
      for k := Pred(q) downto 0 do begin
        stg := AItem.ChangeList.getItem(k);
        if not stg.getProperty().isMany() then begin
          case stg.getProperty().getTypeEnum() of
            BooleanType   : AItem.DataObject.setBoolean(stg.getProperty(),stg.getBooleanValue());
            ByteType      : AItem.DataObject.setByte(stg.getProperty(),stg.getByteValue());
{$IFDEF HAS_SDO_BYTES}
            BytesType     : AItem.DataObject.setBytes(stg.getProperty(),stg.getBytesValue());
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
            CharacterType : AItem.DataObject.setCharacter(stg.getProperty(),stg.getCharacterValue());
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
            CurrencyType  : AItem.DataObject.setCurrency(stg.getProperty(),stg.getCurrencyValue());
{$ENDIF HAS_SDO_CURRENCY}
            DateTimeType  : AItem.DataObject.setDate(stg.getProperty(),stg.getDateValue());
{$IFDEF HAS_SDO_DOUBLE}
            DoubleType    : AItem.DataObject.setDouble(stg.getProperty(),stg.getDoubleValue());
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
            FloatType     : AItem.DataObject.setFloat(stg.getProperty(),stg.getFloatValue());
{$ENDIF HAS_SDO_FLOAT}
            IntegerType   : AItem.DataObject.setInteger(stg.getProperty(),stg.getIntegerValue());
{$IFDEF HAS_SDO_LONG}
            LongType      : AItem.DataObject.setLong(stg.getProperty(),stg.getLongValue());
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
            ShortType     : AItem.DataObject.setShort(stg.getProperty(),stg.getShortValue());
{$ENDIF HAS_SDO_SHORT}
            ObjectType    :
              begin
                if AProcessNestedObjProps then begin
                  if stg.getProperty().isContainment() then
                    ProcessNestedProp(stg.getProperty())
                  else
                    AItem.DataObject.setDataObject(stg.getProperty(),stg.getDataObjectValue());
                end else begin
                  // In the Containment case, the creation of the contained object will set the value.
                  if stg.getProperty().isReference() then
                    AItem.DataObject.setDataObject(stg.getProperty(),stg.getDataObjectValue());
                end;
              end;
            StringType  : AItem.DataObject.setString(stg.getProperty(),stg.getStringValue());
            else
              raise ESDONotImplementedException.CreateFmt('UndoModify() not implemented for this type : %s',[stg.getProperty().getType().getName()]);
          end;
          if stg.isNull() then
            AItem.DataObject.setNull(stg.getProperty());
          if not stg.isSet() then
            AItem.DataObject.unset(stg.getProperty());
        end;
      end;
    end;
    if ( AItem.ManyValuePropChangesList <> nil ) then
      UndoManyPropsChanges();
  end;

  procedure UndoDelete();{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Assert(AItem.OldContainer <> nil);
    Assert(AItem.OldContainmentProperty <> nil);
    UndoManyPropsChanges();
    if not AItem.OldContainmentProperty.isMany() then
      AItem.OldContainer.setDataObject(AItem.OldContainmentProperty,AItem.DataObject);
  end;

  procedure UndoCreate();{$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Assert(AItem.OldContainer <> nil);
    Assert(AItem.OldContainmentProperty <> nil);
    UndoManyPropsChanges();
    if not AItem.OldContainmentProperty.isMany() then
      AItem.OldContainer.setDataObject(AItem.OldContainmentProperty,nil);
  end;

begin
  case AItem.ChangeType of
    ctCreate : UndoCreate();
    ctChange : UndoModify();
    ctDelete : UndoDelete();
  end;
end;

function TChangeRecorder.GetOwner() : ISDOChangeSummaryEx;
begin
  Result := ISDOChangeSummaryEx(FOwner);
end;

procedure TChangeRecorder.recordCreation(
  const ADataObject: ISDODataObject;
  const AIndex: PtrInt
);
begin
  recordChange(ADataObject,ctCreate).FIndex := AIndex;
end;

procedure TChangeRecorder.recordDeletion(
  const ADataObject: ISDODataObject;
  const AIndex: PtrInt
);
var
  locInfo : TDataObjectChangeInfo;
begin
  locInfo := recordChange(ADataObject,ctDelete);
  if ( locInfo <> nil ) then
    locInfo.FIndex := AIndex;
end;

{ TDataObjectChangeInfo }

procedure TDataObjectChangeInfo.CaptureOldContainment();
var
  ls : ISDODataObjectList;
  crs : ISDOCursor;
  oldPos : ISDOCursorBookmark;
  obj : ISDODataObject;
begin
  if ( FChangeType in [ctDelete,ctCreate] ) then begin
    obj := DataObject;
    FOldContainer := Pointer(obj.getContainer());
    if ( FOldContainer <> nil ) then begin
      FOldContainmentProperty := obj.getContainmentProperty();
      if ( FChangeType = ctCreate ) and obj.getContainmentProperty().isMany() then begin
        ls := obj.getContainer().getList(obj.getContainmentProperty());
        crs := ls.getCursor();
        if crs.IsPosValid() and ( ls.getDataObject() = DataObject ) then begin
          FIndex := crs.GetPosition();
        end else begin
          oldPos := crs.GetBookmark();
          try
            obj := DataObject;
            crs.Reset();
            while crs.MoveNext() do begin
              if ( ls.getDataObject() = obj ) then
                Break;
            end;
            if not crs.IsPosValid() then
              raise ESDOInvalidStateOperationException.Create('CaptureOldContainment');
            FIndex := crs.GetPosition();
          finally
            crs.GotoBookmark(oldPos);
          end;
        end;
      end;
    end;
  end;
end;

constructor TDataObjectChangeInfo.Create(
  const ADataObject: ISDODataObject;
  const AChangeType: TChangeType
);
begin
  if ( ADataObject = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataObject');
  if ( AChangeType = ctDelete ) then
    ISDODataObject(FDataObject) := ADataObject
  else
    FDataObject := Pointer(ADataObject);
  FChangeType := AChangeType;
  FChangeList := TSDOSettingList.Create();
  CaptureOldContainment();
end;

destructor TDataObjectChangeInfo.Destroy();
begin
  FreeAndNil(FManyValuePropChangesList);
  if ( FChangeType = ctDelete ) then
    ISDODataObject(FDataObject) := nil
  else
    FDataObject := nil;
  inherited;
end;

procedure CreateSettingsFromList(
  const AProperty : ISDOProperty;
  const AList : ISDODataObjectList;
  const ASettingList : ISDOSettingList
);
var
  crs : ISDOCursor;
  extractProc : TListValueExtractProc;
  buffer : TValueBuffer;
  pbuffer : Pointer;
  i : PtrInt;
  typKind : TSDOTypeKind;
begin
  typKind := AProperty.getTypeEnum();
  InitBufferResources(typKind, buffer);
  try
    case typKind of
{$IFDEF HAS_SDO_BYTES}
      BytesType     : pbuffer := buffer.BytesValue;
{$ENDIF HAS_SDO_BYTES}
      ObjectType    : pbuffer := buffer.ObjectValue;
      StringType    : pbuffer := buffer.StringValue;
      else
        pbuffer := @buffer;
    end;
    crs := AList.getCursor();
    crs.Reset();
    i := 0;
    extractProc := getExtractor(typKind);
    while crs.MoveNext() do begin
      extractProc(AList,buffer);
      ASettingList.append(TValueSetting.Create(True, False, pbuffer^, AProperty, i));
      Inc(i);
    end;
  finally
    FreeBufferResources(AProperty.getTypeEnum(), buffer);
  end;
end;

procedure TDataObjectChangeInfo.ExtractPendingOldValues();
var
  i, c : PtrInt;
  p : ISDOProperty;
  locChangeList : TManyValuePropChanges;
  ls : ISDODataObjectList;
begin
  if ( FManyValuePropPending <> nil ) then begin
    c := FManyValuePropPending.Count;
    for i := 0 to Pred(c) do begin
      p := FManyValuePropPending.First() as ISDOProperty;
      FManyValuePropPending.Delete(0);
      locChangeList := FindChanges(p);
      if ( locChangeList <> nil ) then begin
        ls := p.getType().getOwner().CreateList(p.getType());
        CopySimpleList(DataObject.getList(p),ls,p.getTypeEnum());
        UndoChanges(ls,locChangeList);
        CreateSettingsFromList(p, ls, FChangeList);
      end;
    end;
  end;
end;

function TDataObjectChangeInfo.FindChanges(const AProp: ISDOProperty): TManyValuePropChanges;
begin
  if not AProp.isMany() then
    raise ESDOIllegalArgumentException.Create('AProp');
  if ( FManyValuePropChangesList = nil ) then
    FManyValuePropChangesList := TManyValuePropChangesList.Create();
  Result := ManyValuePropChangesList.Find(AProp);
end;

function TDataObjectChangeInfo.GetChanges(const AProp: ISDOProperty) : TManyValuePropChanges;
begin
  Result := FindChanges(AProp);
  if ( Result = nil ) then begin
    Result := TManyValuePropChanges.Create(AProp);
    ManyValuePropChangesList.Add(Result);
    if ( FManyValuePropPending = nil ) then
      FManyValuePropPending := TInterfaceList.Create();
    FManyValuePropPending.Add(AProp);
  end;
end;

function TDataObjectChangeInfo.GetDataObject: ISDODataObject;
begin
  Result := ISDODataObject(FDataObject);
end;

function TDataObjectChangeInfo.getOldContainer: ISDODataObject;
begin
  Result := ISDODataObject(FOldContainer);
end;

{procedure TDataObjectChangeInfo.SetChangeType(const AChangeType: TChangeType);
begin
  if ( AChangeType <> FChangeType ) then begin
    FChangeType := AChangeType;
  end;
end;}

{ TSDOChangedDataObjectList }

function TSDOChangedDataObjectList.append(
  const ADataObject: ISDODataObject;
  const AChange: TChangeType
): TDataObjectChangeInfo;
var
  i : PtrInt;
begin
  if find(ADataObject,i) then
    raise ESDODuplicatedItemException.Create('ADataObject');
  Result := TDataObjectChangeInfo.Create(ADataObject,AChange);
  //FList.Insert(i, Result);
  FList.Add(Result);
end;

procedure TSDOChangedDataObjectList.CheckIndex(const AIndex: PtrInt);
begin
  if ( AIndex < 0 ) or ( AIndex >= FList.Count ) then
    raise ESDOIndexOutOfRangeException.Create(AIndex);
end;

procedure TSDOChangedDataObjectList.Clear();
begin
  FList.Clear();
end;

constructor TSDOChangedDataObjectList.Create();
begin
  FList := TObjectList.Create(True);
end;

procedure TSDOChangedDataObjectList.Delete(const AIndex: PtrInt);
begin
  CheckIndex(AIndex);
  FList.Delete(AIndex);
end;

destructor TSDOChangedDataObjectList.Destroy();
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TSDOChangedDataObjectList.Extract(const AItem: TDataObjectChangeInfo);
begin
  FList.Extract(AItem);
end;

function TSDOChangedDataObjectList.find(const ADataObject: ISDODataObject; out AIndex: PtrInt) : Boolean;
var
  c, i : PtrInt;
begin
  Result := False;
  c := FList.Count;
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      if ( Pointer(ADataObject) = TDataObjectChangeInfo(FList[i]).FDataObject ) then begin
        AIndex := i;
        Result := True;
        Break;
      end;
    end;
  end;
  //The list is no longer sorted!
  //Result := findDicho(FList,PtrInt(ADataObject),AIndex,@DataObjectExtractor);
end;

function TSDOChangedDataObjectList.getDataObject(const AIndex: PtrInt): ISDODataObject;
begin
  Result := getInfo(AIndex).DataObject;
end;

function TSDOChangedDataObjectList.getInfo(const AIndex: PtrInt): TDataObjectChangeInfo;
begin
  CheckIndex(AIndex);
  Result := TDataObjectChangeInfo(FList[AIndex]);
end;

function TSDOChangedDataObjectList.getType(const AIndex: PtrInt): TChangeType;
begin
  Result := getInfo(AIndex).ChangeType;
end;

function TSDOChangedDataObjectList.size() : PtrInt;
begin
  Result := FList.Count;
end;

{ TSDOChangeSummary }

procedure TSDOChangeSummary.beginLogging();
begin
  if not FLogging then begin
    Clear();
    FLogging := True;
  end;
end;

procedure TSDOChangeSummary.Clear();
begin
  FChangesData.Clear();
end;

constructor TSDOChangeSummary.Create(const AChangesData: ISDOChangedDataObjectList);
begin
  if ( AChangesData = nil ) or ( not Supports(AChangesData,ISDOChangedDataObjectListEx,FChangesData)) then
    raise ESDOIllegalArgumentException.Create('AChangesData');
  FRecorder := TChangeRecorder.Create(AChangesData,Self as ISDOChangeSummary);
end;

destructor TSDOChangeSummary.Destroy();
begin
  Clear();
  FreeAndNil(FRecorder);
  inherited;
end;

procedure TSDOChangeSummary.endLogging();
begin
  FLogging := False;
end;

function TSDOChangeSummary.FindManyValueChanges(const ADataObject: ISDODataObject): TManyValuePropChangesList;
var
  i : PtrInt;
begin
  if FChangesData.find(ADataObject,i) then
    Result := FChangesData.getInfo(i).ManyValuePropChangesList
  else
    Result := nil;
end;

function TSDOChangeSummary.getChangedDataObjects() : ISDOChangedDataObjectList;
begin
  Result := FChangesData;
end;

function TSDOChangeSummary.getOldContainer(const ADataObject: ISDODataObject): ISDODataObject;
var
  i : PtrInt;
begin
  if FChangesData.find(ADataObject,i) then
    Result := FChangesData.getInfo(i).OldContainer
  else
    Result := nil;
end;

function TSDOChangeSummary.getOldContainmentProperty(const ADataObject: ISDODataObject): ISDOProperty;
var
  i : PtrInt;
begin
  if FChangesData.find(ADataObject,i) then
    Result := FChangesData.getInfo(i).OldContainmentProperty
  else
    Result := nil;
  if ( Result = nil ) then
    raise ESDOInvalidStateOperationException.Create('getOldContainmentProperty');
end;

function TSDOChangeSummary.getOldValue(
  const ADataObject: ISDODataObject;
  const AProperty: ISDOProperty
) : TValueSetting;
var
  locLs : ISDOSettingList;
  i, c : PtrInt;
  locSetting : TValueSetting;
begin
  Result := nil;
  locLs := getOldValues(ADataObject);
  c := locLs.size();
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      locSetting := locLs.getItem(i);
      if ( locSetting.getProperty() = AProperty ) then begin
        Result := locSetting;
        Break;
      end;
    end;
  end;
end;

function TSDOChangeSummary.getOldValues(const ADataObject: ISDODataObject): ISDOSettingList;
var
  i : PtrInt;
  ci : TDataObjectChangeInfo;
begin
  if FChangesData.find(ADataObject,i) then begin
    ci := FChangesData.getInfo(i);
    ci.ExtractPendingOldValues();
    Result := ci.ChangeList;
  end else begin
    Result := TSDOSettingList.Create() as ISDOSettingList;
  end;
end;

function TSDOChangeSummary.getOldXpath(const ADataObject: ISDODataObject): string;
var
  locBuffer : string;
  obj : ISDODataObject;
begin
  if Assigned(ADataObject) and isDeleted(ADataObject) then begin
    obj := ADataObject;
    locBuffer := '';
    while ( obj <> nil ) do begin
      if ( obj.getContainer() = nil ) then
        Break;
      locBuffer := obj.getContainmentProperty().getName() + '/' + locBuffer;
      obj := obj.getContainer();
    end;
    while ( obj <> nil ) and isDeleted(obj) do begin
      if ( getOldContainer(obj) <> nil ) then begin
        locBuffer := getOldContainmentProperty(obj).getName() + '/' + locBuffer;
        obj := getOldContainer(obj);
      end else begin
        //obj := nil;
        Break;
      end;
    end;
  end;
  if ( Length(locBuffer) > 0 ) and ( locBuffer[Length(locBuffer)] = '/' ) then
    Delete(locBuffer,Length(locBuffer),1);
  Result := locBuffer;
end;

function TSDOChangeSummary.getRecorder() : TChangeRecorder;
begin
  Result := FRecorder;
end;

function TSDOChangeSummary.isCreated(const ADataObject: ISDODataObject): Boolean;
var
  i : PtrInt;
begin
  Result := FChangesData.find(ADataObject,i) and ( FChangesData.getInfo(i).ChangeType = ctCreate );
end;

function TSDOChangeSummary.isDeleted(const ADataObject: ISDODataObject): Boolean;
var
  i : PtrInt;
begin
  Result :=
    ( ADataObject <> nil ) and
    ( ( FChangesData.find(ADataObject,i) and ( FChangesData.getInfo(i).ChangeType = ctDelete ) ) or
      ( ( ADataObject.getContainer() <> nil ) and isDeleted(ADataObject.getContainer()) )
    );
end;

function TSDOChangeSummary.isLogging() : Boolean;
begin
  Result := FLogging;
end;

function TSDOChangeSummary.isModified(const ADataObject: ISDODataObject): Boolean;
var
  i : PtrInt;
begin
  Result := FChangesData.find(ADataObject,i) and ( FChangesData.getInfo(i).ChangeType = ctChange );
end;

procedure TSDOChangeSummary.resume();
begin
  if not FLogging then
    FLogging := True;
end;

procedure TSDOChangeSummary.suspend();
begin
  if FLogging then
    FLogging := False;
end;

procedure TSDOChangeSummary.undoChanges();
var
  ls : ISDOChangedDataObjectList;
  lsX : ISDOChangedDataObjectListEx;
  i, c : PtrInt;
  locChangeInfo : TDataObjectChangeInfo;
begin
  endLogging();
  ls := FChangesData;
  if ( ls <> nil ) then begin
    lsX := ls as ISDOChangedDataObjectListEx;
    c := lsX.size();
    for i := Pred(c) downto 0 do begin
      locChangeInfo := lsX.getInfo(i);
      FRecorder.undoChangeItem(locChangeInfo,True);
      lsX.Delete(i);
    end;
    lsX.Clear();
  end;
end;

{ TManyValuePropChanges }

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOInteger
): TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.IntegerValue := AValue;
end;

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOBoolean
): TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.BooleanValue := AValue;
end;

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOString
): TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.StringValue^ := AValue;
end;

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
end;

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TValueBuffer
) : TManyValuePropRecordData;
begin
  case Prop.getTypeEnum() of
    BooleanType    : Result := Add(AAction,AIndex,AValue.BooleanValue);
    ByteType       : Result := Add(AAction,AIndex,AValue.ByteValue);
{$IFDEF HAS_SDO_BYTES}
    BytesType      : Result := Add(AAction,AIndex,AValue.BytesValue^);
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType  : Result := Add(AAction,AIndex,AValue.CharValue);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType   : Result := AddCurrency(AAction,AIndex,AValue.CurrencyValue);
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType   : Result := Add(AAction,AIndex,AValue.DateValue);
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType     : Result := Add(AAction,AIndex,AValue.DoubleValue);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType      : Result := Add(AAction,AIndex,AValue.FloatValue);
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    LongType       : Result := Add(AAction,AIndex,AValue.LongValue);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType      : Result := Add(AAction,AIndex,AValue.ShortValue);
{$ENDIF HAS_SDO_SHORT}
    IntegerType    : Result := Add(AAction,AIndex,AValue.IntegerValue);
    ObjectType     : Result := Add(AAction,AIndex,AValue.ObjectValue^);
    StringType     : Result := Add(AAction,AIndex,AValue.StringValue^);
    else
      raise ESDONotImplementedException.CreateFmt('TManyValuePropChanges.Add not implemented for this type : "%s".',[Prop.getType().getName()]);
  end;
end;

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: ISDODataObject
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.ObjectValue^ := AValue;
end;

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOByte
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.ByteValue := AValue;
end;

{$IFDEF HAS_SDO_CHAR}
function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOChar
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.CharValue := AValue;
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
function TManyValuePropChanges.AddCurrency(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOCurrency
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.CurrencyValue := AValue;
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDODouble
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.DoubleValue := AValue;
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOFloat
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.FloatValue := AValue;
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOLong
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.LongValue := AValue;
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOShort
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.ShortValue := AValue;
end;
{$ENDIF HAS_SDO_SHORT}

{$IFDEF HAS_SDO_BYTES}
function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDOBytes
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.BytesValue^ := AValue;  
end;
{$ENDIF HAS_SDO_BYTES}

function TManyValuePropChanges.Add(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: TSDODateTime
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex);
  Result.FValue.DateValue := AValue;
end;

function TManyValuePropChanges.AddAt(
  const ALocation: PtrInt;
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const AValue: ISDODataObject
) : TManyValuePropRecordData;
begin
  Result := InternalAdd(AAction,AIndex,ALocation);
  Result.FValue.ObjectValue^ := AValue;
end;

function TManyValuePropChanges.Count() : PtrInt;
begin
  Result := FList.Count;
end;

constructor TManyValuePropChanges.Create(const AProp: ISDOProperty);
begin
  if ( AProp = nil ) then
    raise ESDOIllegalArgumentException.Create('AProp');
  FProp := AProp;
  FList := TObjectList.Create(True);
end;

destructor TManyValuePropChanges.Destroy();
begin
  FreeAndNil(FList);
  inherited;
end;

function TManyValuePropChanges.GetItem(const AIndex: PtrInt): TManyValuePropRecordData;
begin
  if ( AIndex >= 0 ) and ( AIndex < FList.Count ) then
    Result := TManyValuePropRecordData(FList[AIndex])
  else
    raise ESDOIndexOutOfRangeException.Create(AIndex);
end;

function TManyValuePropChanges.InternalAdd(
  const AAction: TManyValuePropAction;
  const AIndex: PtrInt;
  const ALocation : PtrInt
): TManyValuePropRecordData;
begin
  Result := TManyValuePropRecordData.Create(Self);
  if ( ALocation = -1 ) or ( ALocation = FList.Count ) then
    FList.Add(Result)
  else
    FList.Insert(ALocation,Result);
  Result.FAction := AAction;
  Result.FIndex := AIndex;
end;

{ TManyValuePropRecordData }

constructor TManyValuePropRecordData.Create(const AOwner: TManyValuePropChanges);
begin
  FOwner := AOwner;
  InitBufferResources(FOwner.Prop.getTypeEnum(), FValue);
end;

destructor TManyValuePropRecordData.Destroy();
begin
  if ( FOwner <> nil ) then
    FreeBufferResources(FOwner.Prop.getTypeEnum(), FValue);
  inherited;
end;

{ TManyValuePropChangesList }

procedure TManyValuePropChangesList.Add(const AItem: TManyValuePropChanges);
begin
  if ( FList.IndexOf(AItem) = -1 ) then
    FList.Add(AItem);
end;

function TManyValuePropChangesList.Count() : PtrInt;
begin
  Result := FList.Count;
end;

constructor TManyValuePropChangesList.Create();
begin
  FList := TObjectList.Create(True);
end;

destructor TManyValuePropChangesList.Destroy();
begin
  FreeAndNil(FList);
  inherited;
end;

function TManyValuePropChangesList.Find(const AProperty: ISDOProperty): TManyValuePropChanges;
var
  i : PtrInt;
begin
  i := IndexOf(AProperty);
  if ( i = -1 ) then
    Result := nil
  else
    Result := TManyValuePropChanges(FList[i]);
end;

function TManyValuePropChangesList.GetItem(const AIndex: PtrInt): TManyValuePropChanges;
begin
  Result := TManyValuePropChanges(FList[AIndex]);
end;

function TManyValuePropChangesList.IndexOf(const AProperty: ISDOProperty): PtrInt;
var
  i, c : PtrInt;
begin
  Result := -1;
  c := FList.Count;
  if ( c > 0 ) then begin
    for i := 0 to ( c - 1 ) do begin
      if ( GetItem(i).Prop = AProperty ) then begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

end.
