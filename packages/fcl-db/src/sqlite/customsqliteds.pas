unit CustomSqliteDS;

{
  This is TCustomSqliteDataset, a TDataset descendant class for use with fpc compiler
  Copyright (C) 2004-2007  Luiz Américo Pereira Câmara
  Email: pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$Mode ObjFpc}
{$H+}
{.$Define DEBUG_SQLITEDS}
{.$Define DEBUGACTIVEBUFFER}

interface

uses
  Classes, SysUtils, db;

type
  PDataRecord = ^DataRecord;
  PPDataRecord = ^PDataRecord;
  DataRecord = record
    Row: PPChar;
    BookmarkFlag: TBookmarkFlag;
    Next: PDataRecord;
    Previous: PDataRecord;
  end;
  
  TDSStream = class(TStream)
  private
    FActiveItem: PDataRecord;
    FFieldRow: PChar;
    FFieldIndex: Integer;
    FRowSize: Integer;
    FPosition: LongInt;
  public
    constructor Create(const ActiveItem: PDataRecord; FieldIndex: Integer);
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
  end;

  //callback types
  TSqliteCdeclCallback = function(UserData: Pointer; Count: LongInt; Values: PPChar; Names: PPChar): LongInt; cdecl;
  TSqliteCallback = function(UserData: Pointer; Count: LongInt; Values: PPChar; Names: PPChar): LongInt of object;
  TCallbackInfo = record
    Proc: TSqliteCallback;
    Data: Pointer;
  end;
  PCallbackInfo = ^TCallbackInfo;
  
  TRecordState = (rsAdded, rsDeleted, rsUpdated);
  TRecordStateSet = set of TRecordState;
  TQueryUpdatesCallback = procedure(UserData: Pointer; Values: PPChar; ABookmark: TBookmark; RecordState: TRecordState) of object;

  TGetSqlStrFunction = function(APChar: PChar): String;

  TSqliteOption = (soWildcardKey);
  TSqliteOptions = set of TSqliteOption;

  { TCustomSqliteDataset }

  TCustomSqliteDataset = class(TDataSet)
  private
    {$ifdef DEBUGACTIVEBUFFER}
    FFCurrentItem: PDataRecord;
    {$else}
    FCurrentItem: PDataRecord;
    {$endif}
    FInternalActiveBuffer: PDataRecord;
    FInsertBookmark: PDataRecord;
    FOnCallback: TSqliteCallback;
    FMasterLink: TMasterDataLink;
    FIndexFieldNames: String;
    FIndexFieldList: TList;
    FOnGetHandle: TDataSetNotifyEvent;
    FOptions: TSqliteOptions;
    FSQLList: TStrings;
    procedure CopyCacheToItem(AItem: PDataRecord);
    function GetIndexFields(Value: Integer): TField;
    procedure SetMasterIndexValue;
    procedure SetOptions(const AValue: TSqliteOptions);
    procedure UpdateCalcFieldList;
    procedure UpdateIndexFields;
    function FindRecordItem(StartItem: PDataRecord; const KeyFields: string; const KeyValues: Variant; LocateOptions: TLocateOptions; DoResync: Boolean): PDataRecord;
    procedure UpdateMasterDetailProperties;
  protected
    FPrimaryKey: String;
    FPrimaryKeyNo: Integer;
    FFileName: String;
    FSQL: String;
    FTableName: String;
    FSqlFilterTemplate: String;
    FAutoIncFieldNo: Integer;
    FNextAutoInc: Integer;
    FUpdatedItems: TFPList;
    FAddedItems: TFPList;
    FDeletedItems: TFPList;
    FCalcFieldList: TFPList;
    FReturnCode: Integer;
    FSqliteHandle: Pointer;
    FRowBufferSize: Integer;
    FRowCount: Integer;
    FRecordCount: Integer;
    FBeginItem: PDataRecord;
    FEndItem: PDataRecord;
    FCacheItem: PDataRecord;
    FGetSqlStr: array of TGetSqlStrFunction;
    FSaveOnClose: Boolean;
    FSaveOnRefetch: Boolean;
    FAutoIncrementKey: Boolean;
    FDataAllocated: Boolean;
    function SqliteExec(Sql: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer; virtual; abstract;
    procedure InternalCloseHandle; virtual; abstract;
    function InternalGetHandle: Pointer; virtual; abstract;
    procedure GetSqliteHandle;
    function GetSqliteVersion: String; virtual; abstract;
    procedure BuildLinkedList; virtual; abstract;
    procedure FreeItem(AItem: PDataRecord);
    procedure DisposeLinkedList;
    procedure SetDetailFilter;
    procedure MasterChanged(Sender: TObject);
    procedure SetMasterFields(const Value: String);
    function GetMasterFields: String;
    procedure SetMasterSource(Value: TDataSource);
    function GetMasterSource: TDataSource;
    procedure SetFileName(const Value: String);
    function GetRowsAffected: Integer; virtual; abstract;
    procedure RetrieveFieldDefs; virtual; abstract;
    //TDataSet overrides
    function AllocRecordBuffer: PChar; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure DoBeforeClose; override;
    procedure DoAfterInsert; override;
    procedure DoBeforeInsert; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    function GetRecordSize: Word; override; 
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalCancel; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetExpectedAppends(AValue: Integer);
    procedure SetExpectedUpdates(AValue: Integer);
    procedure SetExpectedDeletes(AValue: Integer);
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    function Locate(const KeyFields: String; const KeyValues: Variant; LocateOptions: TLocateOptions) : Boolean; override;
    function LocateNext(const KeyFields: String; const KeyValues: Variant; LocateOptions: TLocateOptions) : Boolean;
    function Lookup(const KeyFields: String; const KeyValues: Variant; const ResultFields: String): Variant; override;
    // Additional procedures
    function ApplyUpdates: Boolean;
    function CreateTable: Boolean;
    function CreateTable(const ATableName: String): Boolean;
    procedure ExecCallback(const ASql: String; UserData: Pointer = nil);
    procedure ExecSQL;
    procedure ExecSQL(const ASql: String);
    procedure ExecSQLList;
    procedure ExecuteDirect(const ASql: String); virtual; abstract;
    procedure QueryUpdates(RecordStates: TRecordStateSet; Callback: TQueryUpdatesCallback; UserData: Pointer = nil);
    function QuickQuery(const ASql: String):String;overload;
    function QuickQuery(const ASql: String; const AStrList: TStrings): String; overload;
    function QuickQuery(const ASql: String; const AStrList: TStrings; FillObjects: Boolean):String; virtual; abstract; overload;
    procedure RefetchData;
    function ReturnString: String; virtual; abstract;
    function TableExists: Boolean;
    function TableExists(const ATableName: String): Boolean;
    function UpdatesPending: Boolean;
    {$ifdef DEBUGACTIVEBUFFER}
    procedure SetCurrentItem(Value: PDataRecord);
    property FCurrentItem: PDataRecord read FFCurrentItem write SetCurrentItem;
    {$endif}
    property ExpectedAppends: Integer write SetExpectedAppends;
    property ExpectedUpdates: Integer write SetExpectedUpdates;
    property ExpectedDeletes: Integer write SetExpectedDeletes;
    property IndexFields[Value: Integer]: TField read GetIndexFields;
    property RowsAffected: Integer read GetRowsAffected;
    property ReturnCode: Integer read FReturnCode;
    property SqliteHandle: Pointer read FSqliteHandle;
    property SqliteVersion: String read GetSqliteVersion;
    property SQLList:TStrings read FSQLList;
   published
    property AutoIncrementKey: Boolean read FAutoIncrementKey write FAutoIncrementKey;
    property IndexFieldNames: string read FIndexFieldNames write FIndexFieldNames;
    property FileName: String read FFileName write SetFileName;
    property OnCallback: TSqliteCallback read FOnCallback write FOnCallback;
    property OnGetHandle: TDataSetNotifyEvent read FOnGetHandle write FOnGetHandle;
    property Options: TSqliteOptions read FOptions write SetOptions;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property SaveOnClose: Boolean read FSaveOnClose write FSaveOnClose; 
    property SaveOnRefetch: Boolean read FSaveOnRefetch write FSaveOnRefetch;
    property SQL: String read FSQL write FSQL;
    property TableName: String read FTableName write FTableName;   
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterFields: String read GetMasterFields write SetMasterFields;
    
    property Active;
    property FieldDefs;   
    //Events
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;
  
  function Num2SQLStr(APChar: PChar): String;
  function Char2SQLStr(APChar: PChar): String;


implementation

uses
  strutils, variants, dbconst;

const
  //sqlite2.x.x and sqlite3.x.x define these constants equally
  SQLITE_OK = 0;
  SQLITE_ROW = 100;
  
  NullString = 'NULL';
  

function CallbackDispatcher(UserData: Pointer; Count: LongInt; Values: PPchar; Names: PPchar): LongInt; cdecl;
begin
  with PCallbackInfo(UserData)^ do
    Result:= Proc(Data, Count, Values, Names);
end;
  
function Num2SQLStr(APChar: PChar): String;
begin
  if APChar = nil then
  begin
    Result := NullString;
    Exit;
  end;
  Result := String(APChar);
end;

function Char2SQLStr(APChar: PChar): String;
begin
  if APChar = nil then
  begin
    Result := NullString;
    Exit;
  end;
  //todo: create custom routine to directly transform PChar -> SQL str
  Result := String(APChar);
  if Pos('''', Result) > 0 then
    Result := AnsiReplaceStr(Result, '''', '''''');
  Result := '''' + Result + '''';
end;

// TDSStream

constructor TDSStream.Create(const ActiveItem: PDataRecord; FieldIndex: Integer);
begin
  inherited Create;
  //FPosition := 0;
  FActiveItem := ActiveItem;
  FFieldIndex := FieldIndex;
  FFieldRow := ActiveItem^.Row[FieldIndex];
  if FFieldRow <> nil then
    FRowSize := StrLen(FFieldRow);
  //else
  //  FRowSize := 0;  
end;  

function TDSStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  Case Origin of
    soFromBeginning : FPosition := Offset;
    soFromEnd       : FPosition := FRowSize + Offset;
    soFromCurrent   : FPosition := FPosition + Offset;
  end;
  Result := FPosition;
end;

function TDSStream.Write(const Buffer; Count: LongInt): LongInt;
var
  NewRow: PChar;
begin
  Result := Count;
  if Count = 0 then
    Exit;
  //FRowSize is always 0 when FPosition = 0,
  //so there's no need to check FPosition
  NewRow := StrAlloc(FRowSize + Count + 1);
  (NewRow + Count + FRowSize)^ := #0;
  if FRowSize > 0 then
    Move(FFieldRow^, NewRow^, FRowSize);
  Move(Buffer, (NewRow + FRowSize)^, Count);
  FActiveItem^.Row[FFieldIndex] := NewRow;
  StrDispose(FFieldRow);
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TDSStream.Write##');
  WriteLn('  FPosition(Before): ', FPosition);
  WriteLn('  FRowSize(Before): ', FRowSize);
  WriteLn('  FPosition(After): ', FPosition+Count);
  WriteLn('  FRowSize(After): ', StrLen(NewRow));
  //WriteLn('  Stream Value: ',NewRow);
  {$endif}
  FFieldRow := NewRow;
  FRowSize := StrLen(NewRow);
  Inc(FPosition, Count);
end; 
 
function TDSStream.Read(var Buffer; Count: Longint): LongInt;
var
  BytesToMove: Integer;
begin
  if (FRowSize - FPosition) >= Count then
    BytesToMove := Count
  else
    BytesToMove := FRowSize - FPosition;
  Move((FFieldRow + FPosition)^, Buffer, BytesToMove);
  Inc(FPosition, BytesToMove);
  Result := BytesToMove;
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TDSStream.Read##');
  WriteLn('  Bytes requested: ', Count);
  WriteLn('  Bytes moved: ', BytesToMove);
  WriteLn('  Stream.Size: ', FRowSize);
  //WriteLn('  Stream Value: ', FFieldRow);
  {$endif}
end; 
 
// TCustomSqliteDataset override methods

function TCustomSqliteDataset.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(SizeOf(PPDataRecord));
  PDataRecord(Pointer(Result)^) := FBeginItem;
end;

constructor TCustomSqliteDataset.Create(AOwner: TComponent);
begin
  // setup special items
  New(FBeginItem);
  New(FCacheItem);
  New(FEndItem);
  
  FBeginItem^.Previous := nil;
  FEndItem^.Next := nil;
  
  FBeginItem^.BookmarkFlag := bfBOF;
  FEndItem^.BookmarkFlag := bfEOF;
  
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := @MasterChanged;
  FMasterLink.OnMasterDisable := @MasterChanged;
  FIndexFieldList := TList.Create;
  BookmarkSize := SizeOf(Pointer);
  FUpdatedItems := TFPList.Create;
  FAddedItems := TFPList.Create;
  FDeletedItems := TFPList.Create;
  FSQLList := TStringList.Create;
  inherited Create(AOwner);
end;

function TCustomSqliteDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  if Mode = bmWrite then
  begin
    if not (State in [dsEdit, dsInsert]) then
    begin
      DatabaseErrorFmt(SNotEditing,[Name],Self);
      Exit;
    end;
    StrDispose(FCacheItem^.Row[Field.FieldNo - 1]);
    FCacheItem^.Row[Field.FieldNo - 1] := nil;
  end;
  Result:= TDSStream.Create(PPDataRecord(ActiveBuffer)^, Field.FieldNo - 1);
end;

procedure TCustomSqliteDataset.DoBeforeClose;
begin
  if FSaveOnClose then
    ApplyUpdates;
  inherited DoBeforeClose;
end;

procedure TCustomSqliteDataset.DoAfterInsert;
begin
  //an append or an insert in an empty dataset
  if EOF then
    FInsertBookmark := FEndItem
  else
    FInsertBookmark := FInternalActiveBuffer;
  inherited DoAfterInsert;
end;

procedure TCustomSqliteDataset.DoBeforeInsert;
begin
  FInternalActiveBuffer := PPDataRecord(ActiveBuffer)^;
  inherited DoBeforeInsert;
end;

destructor TCustomSqliteDataset.Destroy;
begin
  inherited Destroy;
  if FSqliteHandle <> nil then
    InternalCloseHandle;
  FUpdatedItems.Destroy;
  FAddedItems.Destroy;
  FDeletedItems.Destroy;
  FMasterLink.Destroy;
  FIndexFieldList.Destroy;
  FSQLList.Destroy;
  FCalcFieldList.Free;
  // dispose special items
  Dispose(FBeginItem);
  Dispose(FCacheItem);
  Dispose(FEndItem);
end;

function TCustomSqliteDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
var
  TempItem: PDataRecord;
begin
  Result := False;
  TempItem := FBeginItem^.Next;
  while TempItem <> FEndItem do
  begin
    if TempItem = PPDataRecord(ABookmark)^ then
    begin
      Result := True;
      Exit;
    end;
    TempItem := TempItem^.Next;
  end;
end;

function TCustomSqliteDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark
  ): LongInt;
var
  TempItem: PDataRecord;
begin
  if PPDataRecord(Bookmark1)^ = PPDataRecord(Bookmark2)^ then
  begin
    Result := 0;
    Exit;
  end;
  //assume Bookmark1 < Bookmark2
  Result := -1;
  TempItem := PPDataRecord(Bookmark1)^^.Previous;
  while TempItem <> FBeginItem do
  begin
    if TempItem = PPDataRecord(Bookmark2)^ then
    begin
      //Bookmark1 is greater than Bookmark2
      Result := 1;
      Exit;
    end;
    TempItem := TempItem^.Previous;
  end;
end;

procedure TCustomSqliteDataset.CopyCacheToItem(AItem: PDataRecord);
var
  i: Integer;
begin
  for i := 0 to FRowCount - 1 do
  begin
    StrDispose(AItem^.Row[i]);
    AItem^.Row[i] := FCacheItem^.Row[i];
    FCacheItem^.Row[i] := nil;
  end;
  AItem^.BookmarkFlag := FCacheItem^.BookmarkFlag;
end;

function TCustomSqliteDataset.GetIndexFields(Value: Integer): TField;
begin
  Result := TField(FIndexFieldList[Value]);
end;

procedure TCustomSqliteDataset.SetMasterIndexValue;
var
  i: Integer;
begin
  for i := 0 to FIndexFieldList.Count - 1 do
    TField(FIndexFieldList[i]).AsString := TField(FMasterLink.Fields[i]).AsString;
end;

procedure TCustomSqliteDataset.SetOptions(const AValue: TSqliteOptions);
begin
  FOptions := AValue;
end;

procedure TCustomSqliteDataset.UpdateCalcFieldList;
var
  i: Integer;
  AField: TField;
begin
  if FCalcFieldList = nil then
    FCalcFieldList := TFPList.Create
  else
    FCalcFieldList.Clear;
  for i := 0 to Fields.Count - 1 do
  begin
    AField := Fields[i];
    if AField.FieldKind in [fkCalculated, fkLookup] then
      FCalcFieldList.Add(AField);
  end;
end;

procedure TCustomSqliteDataset.DisposeLinkedList;
var
  TempItem: PDataRecord;
  i: Integer;
begin
  //Todo: insert debug info
  //Todo: see if FDataAllocated is still necessary
  FDataAllocated := False;
  TempItem := FBeginItem^.Next;
  while TempItem^.Next <> nil do
  begin
    TempItem := TempItem^.Next;
    FreeItem(TempItem^.Previous);
  end; 

  //Dispose Deleted Items
  //Directly access list pointer since the index check is already done in the loop
  for i := 0 to FDeletedItems.Count - 1 do
    FreeItem(PDataRecord(FDeletedItems.List^[i]));

  //Dispose FBeginItem.Row
  FreeMem(FBeginItem^.Row, FRowBufferSize);
    
  //Dispose cache item row
  for i:= 0 to FRowCount - 1 do
    StrDispose(FCacheItem^.Row[i]);
  FreeMem(FCacheItem^.Row, FRowBufferSize);
end;

procedure TCustomSqliteDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;

procedure TCustomSqliteDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Pointer(Data^) := PPDataRecord(Buffer)^;
end;

function TCustomSqliteDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PPDataRecord(Buffer)^^.BookmarkFlag;
end;

function TCustomSqliteDataset.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
var
  ValError: Word;
  FieldRow: PChar;
begin
  FieldRow := PPDataRecord(ActiveBuffer)^^.Row[Field.FieldNo - 1];
  Result := FieldRow <> nil;  
  if Result and (Buffer <> nil) then //supports GetIsNull
  begin
    case Field.Datatype of
    ftString:
      begin
        Move(FieldRow^, PChar(Buffer)^, StrLen(FieldRow) + 1);
      end;
    ftInteger, ftAutoInc:
      begin
        Val(String(FieldRow), LongInt(Buffer^), ValError);
        Result := ValError = 0;  
      end;
    ftBoolean, ftWord:
      begin
        Val(String(FieldRow), Word(Buffer^), ValError);
        Result := ValError = 0;
      end;    
    ftFloat, ftDateTime, ftTime, ftDate, ftCurrency:
      begin
        Val(String(FieldRow), Double(Buffer^), ValError);
        Result := ValError = 0; 
      end;
    ftLargeInt:
      begin
        Val(String(FieldRow), Int64(Buffer^), ValError);
        Result := ValError = 0;
      end;        
    end;
  end;        
end;

function TCustomSqliteDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(Field, Buffer, False);
end;

function TCustomSqliteDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOk;
  case GetMode of
    gmPrior:
      if (FCurrentItem^.Previous = FBeginItem) or (FCurrentItem = FBeginItem) then
      begin
        Result := grBOF;
        FCurrentItem := FBeginItem;
      end
      else
        FCurrentItem:=FCurrentItem^.Previous;
    gmCurrent:
      if (FCurrentItem = FBeginItem) or (FCurrentItem = FEndItem) then
         Result := grError;
    gmNext:
      if (FCurrentItem = FEndItem) or (FCurrentItem^.Next = FEndItem) then
        Result := grEOF
      else
        FCurrentItem := FCurrentItem^.Next;
  end; //case
  if Result = grOk then
  begin
    PDataRecord(Pointer(Buffer)^) := FCurrentItem;
    FCurrentItem^.BookmarkFlag := bfCurrent;
  end
    else if (Result = grError) and DoCheck then
      DatabaseError('No records found', Self);
end;

function TCustomSqliteDataset.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

function TCustomSqliteDataset.GetRecNo: Integer;
var
  TempItem, TempActive: PDataRecord;
begin
  Result := -1;
  if (FRecordCount = 0) or (State = dsInsert) then
    Exit;  
  TempItem := FBeginItem;
  TempActive := PPDataRecord(ActiveBuffer)^;
  if TempActive = FCacheItem then // Record is being edited
    TempActive := FInternalActiveBuffer;
  //RecNo is 1 based
  Inc(Result);
  while TempActive <> TempItem do
  begin
    if TempItem^.Next <> nil then
    begin
      Inc(Result);
      TempItem := TempItem^.Next;
    end  
    else
    begin
      Result := -1;
      DatabaseError('GetRecNo - ActiveItem Not Found', Self);
      break;
    end;      
  end;  
end;

function TCustomSqliteDataset.GetRecordSize: Word;
begin
  Result := SizeOf(PPDataRecord); //??
end;

procedure TCustomSqliteDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
var
  NewItem: PDataRecord;
begin
  {$ifdef DEBUG_SQLITEDS}
  if PPDataRecord(ActiveBuffer)^ <> FCacheItem then
    DatabaseError('PPDataRecord(ActiveBuffer) <> FCacheItem - Problem', Self);
  {$endif}
  New(NewItem);
  GetMem(NewItem^.Row, FRowBufferSize);
  //if is a detail dataset then set the index value
  if FMasterLink.Active then
    SetMasterIndexValue;
  //necessary to nullify the Row before copy the cache
  FillChar(NewItem^.Row^, FRowBufferSize, #0);
  CopyCacheToItem(NewItem);

  //insert in the linked list
  FInsertBookmark^.Previous^.Next := NewItem;
  NewItem^.Next := FInsertBookmark;
  NewItem^.Previous := FInsertBookmark^.Previous;
  FInsertBookmark^.Previous := NewItem;
  
  //update the cursor
  FCurrentItem := NewItem;
  
  Inc(FRecordCount);
  if FAutoIncFieldNo <> - 1 then
    Inc(FNextAutoInc);
  FAddedItems.Add(NewItem);
end;

procedure TCustomSqliteDataset.InternalClose;
begin
  //BindFields(False);
  if DefaultFields then
    DestroyFields;
  if FDataAllocated then
    DisposeLinkedList;  
  FAddedItems.Clear;
  FUpdatedItems.Clear;
  FDeletedItems.Clear;
  FRecordCount := 0;
end;

procedure TCustomSqliteDataset.InternalCancel;
var
  i: Integer;
begin
  PPDataRecord(ActiveBuffer)^ := FInternalActiveBuffer;
  //free the cache
  for i:= 0 to FRowCount - 1 do
  begin
    StrDispose(FCacheItem^.Row[i]);
    FCacheItem^.Row[i] := nil;
  end;
end;

procedure TCustomSqliteDataset.InternalDelete;
var
  TempItem: PDataRecord;
  ValError, TempInteger: Integer;
begin
  Dec(FRecordCount);
  TempItem := PPDataRecord(ActiveBuffer)^;
  TempItem^.Next^.Previous := TempItem^.Previous;
  TempItem^.Previous^.Next := TempItem^.Next;
  if FCurrentItem = TempItem then
  begin
    if FCurrentItem^.Previous <> FBeginItem then
      FCurrentItem := FCurrentItem^.Previous
    else
      FCurrentItem := FCurrentItem^.Next;  
  end; 
  // Dec FNextAutoInc (only if deleted item is the last record)  
  if FAutoIncFieldNo <> -1 then
  begin
    Val(String(TempItem^.Row[FAutoIncFieldNo]), TempInteger, ValError);
    if (ValError = 0) and (TempInteger = (FNextAutoInc - 1)) then
      Dec(FNextAutoInc);
  end;    
  // Update item lists
  FUpdatedItems.Remove(TempItem);
  if FAddedItems.Remove(TempItem) = -1 then
    FDeletedItems.Add(TempItem)
  else
    FreeItem(TempItem);
end;

procedure TCustomSqliteDataset.InternalEdit;
var
  i: Integer;
begin
  FInternalActiveBuffer := PPDataRecord(ActiveBuffer)^;
  //copy active item to cache
  for i:= 0 to FRowCount - 1 do
    FCacheItem^.Row[i] := StrNew(FInternalActiveBuffer^.Row[i]);
  FCacheItem^.BookmarkFlag := FInternalActiveBuffer^.BookmarkFlag;
  //now active buffer is the cache item
  PPDataRecord(ActiveBuffer)^ := FCacheItem;
end;

procedure TCustomSqliteDataset.InternalFirst;
begin
  FCurrentItem := FBeginItem;
end;

procedure TCustomSqliteDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentItem := PDataRecord(ABookmark^);
end;

procedure TCustomSqliteDataset.InternalInitFieldDefs;
begin
  if FSQL = '' then
  begin
    if FTablename = '' then
      DatabaseError('Tablename not set', Self);
    FSQL := 'Select * from ' + FTableName + ';';
  end;

  if FSqliteHandle = nil then
    GetSqliteHandle;

  RetrieveFieldDefs;
end;

procedure TCustomSqliteDataset.InternalInitRecord(Buffer: PChar);
var
  TempStr: String;
begin
  if FAutoIncFieldNo <> - 1 then
  begin
    Str(FNextAutoInc, TempStr);
    FCacheItem^.Row[FAutoIncFieldNo] := StrAlloc(Length(TempStr) + 1);
    StrPCopy(FCacheItem^.Row[FAutoIncFieldNo], TempStr);
  end;  
  PPDataRecord(Buffer)^ := FCacheItem;
  FCacheItem^.BookmarkFlag := bfInserted;
end;

procedure TCustomSqliteDataset.InternalLast;
begin
  FCurrentItem := FEndItem;
end;

procedure TCustomSqliteDataset.InternalOpen;
begin
  InternalInitFieldDefs;

  if DefaultFields then 
    CreateFields;
  BindFields(True);

  if CalcFieldsSize > 0 then
    UpdateCalcFieldList;
  UpdateIndexFields;

  if FMasterLink.DataSource <> nil then
    UpdateMasterDetailProperties;

  // Get PrimaryKeyNo if available
  if Fields.FindField(FPrimaryKey) <> nil then
    FPrimaryKeyNo := Fields.FindField(FPrimaryKey).FieldNo - 1
  else
    FPrimaryKeyNo := FAutoIncFieldNo; // -1 if there's no AutoIncField

  BuildLinkedList;               
  FCurrentItem := FBeginItem;
end;

procedure TCustomSqliteDataset.InternalPost;
begin
  if State <> dsEdit then
    InternalAddRecord(nil, True)
  else
  begin
    CopyCacheToItem(FInternalActiveBuffer);
    PPDataRecord(ActiveBuffer)^ := FInternalActiveBuffer;
    if (FUpdatedItems.IndexOf(FInternalActiveBuffer) = -1) and
      (FAddedItems.IndexOf(FInternalActiveBuffer) = -1) then
      FUpdatedItems.Add(FInternalActiveBuffer);
  end;
end;

procedure TCustomSqliteDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentItem := PPDataRecord(Buffer)^;
end;

function TCustomSqliteDataset.IsCursorOpen: Boolean;
begin
   Result := FDataAllocated;
end;

type
  TLocateCompareFunction = function (Value: PChar; const Key: String): Boolean;
  
  TLocateFieldInfo = record
    Index: Integer;
    Key: String;
    CompFunction: TLocateCompareFunction;
  end;

function CompInsensitivePartial(Value: PChar; const Key: String): Boolean;
begin
  if Value <> nil then
    Result := StrLIComp(Value, PChar(Key), Length(Key)) = 0
  else
    Result := False;
end;

function CompSensitivePartial(Value: PChar; const Key: String): Boolean;
begin
  if Value <> nil then
    Result := StrLComp(Value, PChar(Key), Length(Key)) = 0
  else
    Result := False;
end;

function CompInsensitive(Value: PChar; const Key: String): Boolean;
begin
  if Value <> nil then
    Result := StrIComp(Value, PChar(Key)) = 0
  else
    Result := False;
end;

function CompSensitive(Value: PChar; const Key: String): Boolean;
begin
  if Value <> nil then
    Result := StrComp(Value, PChar(Key)) = 0
  else
    Result := False;
end;

function CompSensitiveWild(Value: PChar; const Key: String): Boolean;
begin
  if Value <> nil then
    Result := IsWild(String(Value), Key, False)
  else
    Result := False;
end;

function CompInsensitiveWild(Value: PChar; const Key: String): Boolean;
begin
  if Value <> nil then
    Result := IsWild(String(Value), Key, True)
  else
    Result := False;
end;


function TCustomSqliteDataset.FindRecordItem(StartItem: PDataRecord; const KeyFields: string; const KeyValues: Variant; LocateOptions: TLocateOptions; DoResync:Boolean): PDataRecord;
var
  LocateFields: array of TLocateFieldInfo;
  AFieldList: TList;
  i, AFieldCount: Integer;
  MatchRecord: Boolean;
  AValue: String;
  TempItem: PDataRecord;
  
begin
  Result := nil;
  AFieldList := TList.Create;
  try
    GetFieldList(AFieldList, KeyFields);
    AFieldCount := AFieldList.Count;
    if AFieldCount > 1 then
    begin
      if VarIsArray(KeyValues) then
      begin
        if Succ(VarArrayHighBound(KeyValues, 1)) <> AFieldCount then
          DatabaseError('Number of fields does not correspond to number of values', Self);
      end
      else
        DatabaseError('Wrong number of values specified: expected an array of variants got a variant', Self);
    end;
    
    //set the array of the fields info
    SetLength(LocateFields, AFieldCount);
    
    for i := 0 to AFieldCount - 1 do
      with TField(AFieldList[i]) do
      begin
        if not (DataType in [ftFloat, ftDateTime, ftTime, ftDate]) then
        begin
          //the loPartialKey and loCaseInsensitive is ignored in numeric fields
          if DataType in [ftString, ftMemo] then
          begin
            if loPartialKey in LocateOptions then
            begin
              if loCaseInsensitive in LocateOptions then
                LocateFields[i].CompFunction := @CompInsensitivePartial
              else
                LocateFields[i].CompFunction := @CompSensitivePartial;
            end
            else
            if soWildcardKey in FOptions then
            begin
              if loCaseInsensitive in LocateOptions then
                LocateFields[i].CompFunction := @CompInsensitiveWild
              else
                LocateFields[i].CompFunction := @CompSensitiveWild;
            end
            else
            begin
              if loCaseInsensitive in LocateOptions then
                LocateFields[i].CompFunction := @CompInsensitive
              else
                LocateFields[i].CompFunction := @CompSensitive;
            end;
          end
          else
            LocateFields[i].CompFunction := @CompSensitive;
            
          if VarIsArray(KeyValues) then
            LocateFields[i].Key := KeyValues[i]
          else
            LocateFields[i].Key := KeyValues;
        end
        else
        begin
          LocateFields[i].CompFunction := @CompSensitive;
          //get float types in appropriate format
          if VarIsArray(KeyValues) then
            Str(VarToDateTime(keyvalues[i]), AValue)
          else
            Str(VarToDateTime(keyvalues), AValue);
          LocateFields[i].Key := Trim(AValue);
        end;
        LocateFields[i].Index := FieldNo - 1;
      end;
  finally
    AFieldList.Destroy;
  end;
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TCustomSqliteDataset.FindRecordItem##');
  WriteLn('  KeyFields: ', KeyFields);
  for i := 0 to AFieldCount - 1 do
  begin
    WriteLn('LocateFields[', i, ']');
    WriteLn('  Key: ', LocateFields[i].Key);
    WriteLn('  Index: ', LocateFields[i].Index);
  end;
  {$endif}        
  //Search the list
  TempItem := StartItem;
  while TempItem <> FEndItem do
  begin
    MatchRecord := True;
    for i:= 0 to AFieldCount - 1 do
    begin
      with LocateFields[i] do
      if not CompFunction(TempItem^.Row[Index], Key) then
      begin
        MatchRecord := False;
        break; //for
      end;
    end;
    if MatchRecord then
    begin
      Result := TempItem;
      if DoResync and (TempItem <> PPDataRecord(ActiveBuffer)^) then
      begin
        DoBeforeScroll;
        FCurrentItem := TempItem;
        Resync([]);
        DoAfterScroll;
      end;
      break; //while
    end;
    TempItem := TempItem^.Next;
  end;      
end;

procedure TCustomSqliteDataset.UpdateMasterDetailProperties;
var
  i: Integer;
begin
  if FMasterLink.Active and (FIndexFieldList.Count <> FMasterLink.Fields.Count) then
    DatabaseError('MasterFields count doesn''t match IndexFields count', Self);
  if FieldDefs.Count > 0 then
  begin
    //build the sql template used to filter the dataset
    FSqlFilterTemplate := 'SELECT ';
    for i := 0 to FieldDefs.Count - 2 do
      FSqlFilterTemplate := FSqlFilterTemplate + FieldDefs[i].Name + ',';
    FSqlFilterTemplate := FSqlFilterTemplate + FieldDefs[FieldDefs.Count - 1].Name +
      ' FROM ' + FTableName;
  end;
  //set FSQL considering MasterSource active record
  SetDetailFilter;
end;

procedure TCustomSqliteDataset.GetSqliteHandle;
begin
  if FFileName = '' then
    DatabaseError('Filename not set', Self);
  FSqliteHandle := InternalGetHandle;
  if Assigned(FOnGetHandle) then
    FOnGetHandle(Self);
end;

procedure TCustomSqliteDataset.FreeItem(AItem: PDataRecord);
var
  i: Integer;
begin
  for i:= 0 to FRowCount - 1 do
    StrDispose(AItem^.Row[i]);
  FreeMem(AItem^.Row, FRowBufferSize);
  Dispose(AItem);
end;

function TCustomSqliteDataset.Locate(const KeyFields: String; const KeyValues: Variant; LocateOptions: TLocateOptions): Boolean;
begin
  CheckBrowseMode;
  Result := FindRecordItem(FBeginItem^.Next, KeyFields, KeyValues, LocateOptions, True) <> nil;
end;
  
function TCustomSqliteDataset.LocateNext(const KeyFields: String; const KeyValues: Variant; LocateOptions: TLocateOptions): Boolean;
begin
  CheckBrowseMode;
  Result := FindRecordItem(PPDataRecord(ActiveBuffer)^^.Next, KeyFields, KeyValues, LocateOptions, True) <> nil;
end;
  
function TCustomSqliteDataset.Lookup(const KeyFields: String; const KeyValues: Variant; const ResultFields: String): Variant;
var
  TempItem: PDataRecord;
begin
  CheckBrowseMode;
  TempItem := FindRecordItem(FBeginItem^.Next, KeyFields, KeyValues, [], False);
  if TempItem <> nil then
    Result := TempItem^.Row[FieldByName(ResultFields).FieldNo - 1]
  else
    Result := Null;
end;  

procedure TCustomSqliteDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  //The BookMarkData is the Buffer itself: no need to set nothing;
end;

procedure TCustomSqliteDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PPDataRecord(Buffer)^^.BookmarkFlag := Value;
end;

procedure TCustomSqliteDataset.SetExpectedAppends(AValue: Integer);
begin
  FAddedItems.Capacity := AValue;
end;  

procedure TCustomSqliteDataset.SetExpectedUpdates(AValue: Integer);
begin
  FUpdatedItems.Capacity := AValue;
end;  

procedure TCustomSqliteDataset.SetExpectedDeletes(AValue: Integer);
begin
  FDeletedItems.Capacity := AValue;
end;  

procedure TCustomSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
var
  TempStr: String;
  FloatStr: PChar;
  FloatLen: Integer;
begin
  if not (State in [dsEdit, dsInsert]) then
    DatabaseErrorFmt(SNotEditing,[Name],Self);

  StrDispose(FCacheItem^.Row[Pred(Field.FieldNo)]);
  if Buffer <> nil then
  begin
    case Field.Datatype of
    ftString:
      begin            
        FCacheItem^.Row[Pred(Field.FieldNo)] := StrNew(PChar(Buffer));
      end;
    ftInteger:
      begin          
        Str(LongInt(Buffer^), TempStr);
        FCacheItem^.Row[Pred(Field.FieldNo)] := StrAlloc(Length(TempStr) + 1);
        Move(PChar(TempStr)^, (FCacheItem^.Row[Pred(Field.FieldNo)])^, Length(TempStr) + 1);
      end;
    ftBoolean, ftWord:
      begin
        Str(Word(Buffer^), TempStr);
        FCacheItem^.Row[Pred(Field.FieldNo)] := StrAlloc(Length(TempStr) + 1);
        Move(PChar(TempStr)^, (FCacheItem^.Row[Pred(Field.FieldNo)])^, Length(TempStr) + 1);
      end;  
    ftFloat, ftDateTime, ftDate, ftTime, ftCurrency:
      begin
        Str(Double(Buffer^), TempStr);
        //Str returns an space as the first character for positive values
        //and the - sign for negative values. It's necessary to remove the extra
        //space while keeping the - sign
        if TempStr[1] = ' ' then
        begin
          FloatStr := PChar(TempStr) + 1;
          FloatLen := Length(TempStr);
        end
        else
        begin
          FloatStr := PChar(TempStr);
          FloatLen := Length(TempStr) + 1;
        end;
        FCacheItem^.Row[Pred(Field.FieldNo)] := StrAlloc(FloatLen);
        Move(FloatStr^, (FCacheItem^.Row[Pred(Field.FieldNo)])^, FloatLen);
      end;
    ftLargeInt:
      begin
        Str(Int64(Buffer^), TempStr);
        FCacheItem^.Row[Pred(Field.FieldNo)] := StrAlloc(Length(TempStr) + 1);
        Move(PChar(TempStr)^, (FCacheItem^.Row[Pred(Field.FieldNo)])^, Length(TempStr) + 1);
      end;        
    end;// case
  end//if
  else
    FCacheItem^.Row[Pred(Field.FieldNo)] := nil;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Ptrint(Field));  
end;

procedure TCustomSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
  SetFieldData(Field, Buffer, False);
end;

procedure TCustomSqliteDataset.SetRecNo(Value: Integer);
var
  Counter: Integer;
  TempItem: PDataRecord;
begin
  if (Value > FRecordCount) or (Value <= 0) then
    DatabaseError('Record Number Out Of Range',Self);
  CheckBrowseMode;
  TempItem := FBeginItem;
  for Counter := 1 to Value do
    TempItem := TempItem^.Next;
  if TempItem <> PPDataRecord(ActiveBuffer)^ then
  begin
    DoBeforeScroll;
    FCurrentItem := TempItem;
    Resync([]);
    DoAfterScroll;
  end;
end;

// Specific functions 

procedure TCustomSqliteDataset.SetDetailFilter;
  function FieldToSqlStr(AField: TField): String;
  begin
    if not AField.IsNull then
    begin
      case AField.DataType of
        //todo: handle " caracter properly
        ftString, ftMemo:
          Result := '"' + AField.AsString + '"';
        ftDateTime, ftDate, ftTime:
          Str(AField.AsDateTime, Result);
      else
        Result := AField.AsString;
      end; //case
    end
    else
      Result:=NullString;
  end; //function

var
  AFilter: String;
  i: Integer;
begin
  if (FMasterLink.Dataset.RecordCount = 0) or not FMasterLink.Active then //Retrieve all data
    FSQL := FSqlFilterTemplate
  else
  begin
    AFilter := ' where ';
    for i := 0 to FMasterLink.Fields.Count - 1 do
    begin
      AFilter := AFilter + IndexFields[i].FieldName + ' = ' + FieldToSqlStr(TField(FMasterLink.Fields[i]));
      if i <> FMasterLink.Fields.Count - 1 then
        AFilter := AFilter + ' and ';
    end;
    FSQL := FSqlFilterTemplate + AFilter;
  end;
end;

procedure TCustomSqliteDataset.MasterChanged(Sender: TObject);
begin
  SetDetailFilter;
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TCustomSqliteDataset.MasterChanged##');
  WriteLn('  SQL used to filter detail dataset:');
  WriteLn('  ', FSQL);
  {$endif}
  RefetchData;
end;

procedure TCustomSqliteDataset.SetMasterFields(const Value: String);
begin
  FMasterLink.FieldNames := Value;
  if Active and FMasterLink.Active then
  begin
    UpdateIndexFields;
    if (FIndexFieldList.Count <> FMasterLink.Fields.Count) then
      DatabaseError('MasterFields count doesn''t match IndexFields count', Self);
  end;
end;

function TCustomSqliteDataset.GetMasterFields: String;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TCustomSqliteDataset.UpdateIndexFields;
begin
  FIndexFieldList.Clear;
  if FIndexFieldNames <> '' then
  begin
    try
      GetFieldList(FIndexFieldList, FIndexFieldNames);
    except
      on E: Exception do
      begin
        FIndexFieldList.Clear;
        DatabaseError('Error retrieving index fields: ' + E.Message);
      end;
    end;
  end;
end;

function TCustomSqliteDataset.GetMasterSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

procedure TCustomSqliteDataset.SetFileName(const Value: String);
begin
  if Value <> FFileName then
  begin
    if Active then
      DatabaseError('It''s not allowed to change Filename in an open dataset', Self);
    if FSqliteHandle <> nil then
      InternalCloseHandle;
    FFileName := Value;
  end;
end;

procedure TCustomSqliteDataset.SetMasterSource(Value: TDataSource);
begin
  FMasterLink.DataSource := Value;
end;

procedure TCustomSqliteDataset.ExecSQL(const ASQL: String);
begin
  if FSqliteHandle = nil then
    GetSqliteHandle;
  ExecuteDirect(ASQL);
end;

procedure TCustomSqliteDataset.ExecSQLList;
begin
  if FSqliteHandle = nil then
    GetSqliteHandle;
  SqliteExec(PChar(FSQLList.Text), nil, nil);
end;

procedure TCustomSqliteDataset.ExecSQL;
begin
  ExecSQL(FSQL);
end;

function TCustomSqliteDataset.ApplyUpdates: Boolean;
var
  iFields, iItems, StatementsCounter, TempReturnCode: Integer;
  SQLTemp, WhereKeyNameEqual, SQLLine, TemplateStr: String;
  TempItem: PDataRecord;
begin
  CheckBrowseMode;
  if not UpdatesPending then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  if FPrimaryKeyNo <> -1 then
  begin
    FReturnCode := SQLITE_OK;
    StatementsCounter := 0;
    WhereKeyNameEqual := ' WHERE ' + Fields[FPrimaryKeyNo].FieldName + ' = ';
    {$ifdef DEBUG_SQLITEDS}
    WriteLn('##TCustomSqliteDataset.ApplyUpdates##');
    if FPrimaryKeyNo = FAutoIncFieldNo then
      WriteLn('  Using an AutoInc field as primary key');
    WriteLn('  PrimaryKey: ', WhereKeyNameEqual);
    WriteLn('  PrimaryKeyNo: ', FPrimaryKeyNo);
    {$endif}
    SQLTemp := 'BEGIN;';
    // Delete Records
    if FDeletedItems.Count > 0 then
      TemplateStr := 'DELETE FROM ' + FTableName + WhereKeyNameEqual;
    for iItems := 0 to FDeletedItems.Count - 1 do
    begin
      TempItem := PDataRecord(FDeletedItems.List^[iItems]);
      SQLTemp := SQLTemp + (TemplateStr +
        String(TempItem^.Row[FPrimaryKeyNo]) + ';');
      FreeItem(TempItem);
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SQLTemp := SQLTemp + 'COMMIT;';
        TempReturnCode := SqliteExec(PChar(SQLTemp), nil, nil);
        if TempReturnCode <> SQLITE_OK then
          FReturnCode := TempReturnCode;  
        StatementsCounter := 0;
        SQLTemp := 'BEGIN;';
      end;    
    end;
    // Update changed records
    if FUpdatedItems.Count > 0 then
      TemplateStr := 'UPDATE ' + FTableName + ' SET ';
    for iItems := 0 to FUpdatedItems.Count - 1 do
    begin
      SQLLine := TemplateStr;
      for iFields := 0 to Fields.Count - 2 do
      begin
        SQLLine := SQLLine + (Fields[iFields].FieldName + ' = ' +
          FGetSqlStr[iFields](PDataRecord(FUpdatedItems[iItems])^.Row[iFields]) + ',');
      end;
      iFields := Fields.Count - 1;
      SQLLine := SQLLine + (Fields[iFields].FieldName + ' = ' +
        FGetSqlStr[iFields](PDataRecord(FUpdatedItems[iItems])^.Row[iFields]) +
        WhereKeyNameEqual +
        String(PDataRecord(FUpdatedItems[iItems])^.Row[FPrimaryKeyNo]) + ';');
      SQLTemp := SQLTemp + SQLLine;
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SQLTemp := SQLTemp + 'COMMIT;';
        TempReturnCode := SqliteExec(PChar(SQLTemp), nil, nil);
        if TempReturnCode <> SQLITE_OK then
          FReturnCode := TempReturnCode;
        StatementsCounter := 0;
        SQLTemp := 'BEGIN;';
      end;  
    end;
    // Add new records
    // Build TemplateStr
    if FAddedItems.Count > 0 then
    begin
      TemplateStr := 'INSERT INTO ' + FTableName + ' (';
      for iFields := 0 to Fields.Count - 2 do
        TemplateStr := TemplateStr + Fields[iFields].FieldName + ',';
      TemplateStr := TemplateStr + Fields[Fields.Count - 1].FieldName + ') VALUES (';
    end;  
    for iItems := 0 to FAddedItems.Count - 1 do
    begin
      SQLLine := TemplateStr;
      for iFields := 0 to Fields.Count - 2 do
        SQLLine := SQLLine + (FGetSqlStr[iFields](PDataRecord(FAddedItems[iItems])^.Row[iFields]) + ',');
      iFields := Fields.Count - 1;
      SQLLine := SQLLine + (FGetSqlStr[iFields](PDataRecord(FAddedItems[iItems])^.Row[iFields]) + ');' );
      SQLTemp := SQLTemp + SQLLine;
      Inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SQLTemp := SQLTemp + 'COMMIT;';
        TempReturnCode := SqliteExec(PChar(SQLTemp), nil, nil);
        if TempReturnCode <> SQLITE_OK then
          FReturnCode := TempReturnCode;
        StatementsCounter := 0;
        SQLTemp := 'BEGIN;';
      end;  
    end;  
    SQLTemp := SQLTemp + 'COMMIT;';
    {$ifdef DEBUG_SQLITEDS}
    WriteLn('  SQL: ',SqlTemp);
    {$endif}  
    FAddedItems.Clear;
    FUpdatedItems.Clear;
    FDeletedItems.Clear;
    TempReturnCode := SqliteExec(PChar(SQLTemp), nil, nil);
    if TempReturnCode <> SQLITE_OK then
      FReturnCode := TempReturnCode;
    Result := FReturnCode = SQLITE_OK;
  end;  
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('  Result: ', Result);
  {$endif}   
end;    

function TCustomSqliteDataset.CreateTable: Boolean;
begin
  Result := CreateTable(FTableName);
end;

function TCustomSqliteDataset.CreateTable(const ATableName: String): Boolean;
var
  SQLTemp: String;
  i: Integer;
begin
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TCustomSqliteDataset.CreateTable##');
  if ATableName = '' then
    WriteLn('  TableName Not Set');
  if FieldDefs.Count = 0 then
    WriteLn('  FieldDefs Not Initialized');
  {$endif}
  if (ATableName <> '') and (FieldDefs.Count > 0) then
  begin
    SQLTemp := 'CREATE TABLE ' + ATableName + ' (';
    for i := 0 to FieldDefs.Count - 1 do
    begin
      //todo: add index to autoinc field
      SQLTemp := SQLTemp + FieldDefs[i].Name;
      case FieldDefs[i].DataType of
        ftInteger:
          SQLTemp := SQLTemp + ' INTEGER';
        ftString:
          SQLTemp := SQLTemp + ' VARCHAR';
        ftBoolean:
          SQLTemp := SQLTemp + ' BOOL_INT';
        ftFloat:
          SQLTemp := SQLTemp + ' FLOAT';
        ftWord:
          SQLTemp := SQLTemp + ' WORD';
        ftDateTime:
          SQLTemp := SQLTemp + ' DATETIME';
        ftDate:
          SQLTemp := SQLTemp + ' DATE';
        ftTime:
          SQLTemp := SQLTemp + ' TIME';
        ftLargeInt:
          SQLTemp := SQLTemp + ' LARGEINT';
        ftCurrency:
          SQLTemp := SQLTemp + ' CURRENCY';
        ftAutoInc:
          SQLTemp := SQLTemp + ' AUTOINC_INT';
        ftMemo:
          SQLTemp := SQLTemp + ' TEXT';
      else
        DatabaseError('Field type "' + FieldTypeNames[FieldDefs[i].DataType] +
          '" not supported', Self);
      end;
      if UpperCase(FieldDefs[i].Name) = UpperCase(FPrimaryKey) then
        SQLTemp := SQLTemp + ' PRIMARY KEY';
      if i <> FieldDefs.Count - 1 then
        SQLTemp := SQLTemp + ' , ';
    end;
    SQLTemp := SQLTemp + ');';
    {$ifdef DEBUG_SQLITEDS}
    WriteLn('  SQL: ',SqlTemp);
    {$endif}
    ExecSQL(SQLTemp);
    Result := FReturnCode = SQLITE_OK;
  end
  else
    Result := False;
end;

procedure TCustomSqliteDataset.ExecCallback(const ASQL: String; UserData: Pointer = nil);
var
  CallbackInfo: TCallbackInfo;
begin
  if not Assigned(FOnCallback) then
    DatabaseError('OnCallback property not set', Self);
  if FSqliteHandle = nil then
    GetSqliteHandle;
  CallbackInfo.Data := UserData;
  CallbackInfo.Proc := FOnCallback;
  SqliteExec(PChar(ASQL), @CallbackDispatcher, @CallbackInfo);
end;


procedure TCustomSqliteDataset.QueryUpdates(RecordStates: TRecordStateSet; Callback: TQueryUpdatesCallback;
  UserData: Pointer = nil);
var
  i: Integer;
  TempItem: PDataRecord;
begin
  if not Assigned(Callback) then
    DatabaseError('Callback parameter not set', Self);
  CheckBrowseMode;
  if rsDeleted in RecordStates then
    with FDeletedItems do
    for i := 0 to Count - 1 do
      Callback(UserData,PDataRecord(Items[i])^.Row, nil, rsDeleted);
  if rsUpdated in RecordStates then
    with FUpdatedItems do
    for i := 0 to Count - 1 do
    begin
      TempItem := PDataRecord(Items[i]);
      Callback(UserData, TempItem^.Row, TBookmark(@TempItem), rsUpdated);
    end;
  if rsAdded in RecordStates then
    with FAddedItems do
    for i := 0 to Count - 1 do
    begin
      TempItem := PDataRecord(Items[i]);
      Callback(UserData, TempItem^.Row, TBookmark(@TempItem), rsAdded);
    end;
end;


procedure TCustomSqliteDataset.RefetchData;
var
  i: Integer;
begin
  //Close
  if FSaveOnRefetch then
    ApplyUpdates;
  if FDataAllocated then
    DisposeLinkedList;  
  FAddedItems.Clear;
  FUpdatedItems.Clear;
  FDeletedItems.Clear;
  //Reopen
  BuildLinkedList;               
  FCurrentItem := FBeginItem;
  for i := 0 to BufferCount - 1 do
    PPDataRecord(Buffers[i])^ := FBeginItem;
  Resync([]);
  DoAfterScroll;
end;

function TCustomSqliteDataset.TableExists: Boolean;
begin
  Result:=TableExists(FTableName);
end;

function TCustomSqliteDataset.TableExists(const ATableName: String): Boolean;
begin
  ExecSql('SELECT name FROM SQLITE_MASTER WHERE type = ''table'' AND name LIKE ''' + ATableName + ''';');
  Result := FReturnCode = SQLITE_ROW;
end;

function TCustomSqliteDataset.UpdatesPending: Boolean;
begin
  Result := (FUpdatedItems.Count > 0) or
    (FAddedItems.Count > 0) or (FDeletedItems.Count > 0);
end;

function TCustomSqliteDataset.QuickQuery(const ASQL: String): String;
begin
  Result := QuickQuery(ASQL, nil, False);
end;

function TCustomSqliteDataset.QuickQuery(const ASQL: String; const AStrList: TStrings): String;
begin
  Result := QuickQuery(ASQL, AStrList, False)
end;  


{$ifdef DEBUGACTIVEBUFFER}
procedure TCustomSqliteDataset.SetCurrentItem(Value:PDataRecord);
var
 ANo:Integer;

  function GetItemPos:Integer;
  var
    TempItem:PDataRecord;
  begin
    Result:= -1;
    TempItem:=FBeginItem;
    if Value = FCacheItem then
       Result:=-2
    else
    while Value <> TempItem do
    begin
     if TempItem^.Next <> nil then
     begin
       inc(Result);
       TempItem:=TempItem^.Next;
     end
     else
     begin
      Result:=-1;
      break;
     end;
    end;
  end;

begin
  if Value = FBeginItem then
  begin
    writeln('FCurrentItem set to FBeginItem: ',IntToHex(Integer(Value),0));
    FFCurrentItem:=Value;
  end
  else
    if Value = FEndItem then
    begin
      writeln('FCurrentItem set to FEndItem: ',IntToHex(Integer(Value),0));
      FFCurrentItem:=Value;
    end
    else
      if Value = FCacheItem then
      begin
        writeln('FCurrentItem set to FCacheItem: ',IntToHex(Integer(Value),0));
        FFCurrentItem:=Value;
      end
      else
      begin
        writeln('FCurrentItem set from ',IntToHex(Integer(FFCurrentItem),0),' to ',IntToHex(Integer(Value),0));
        Ano:=GetItemPos;
        writeln('Item position is ',ANo);
        FFCurrentItem:=Value;
      end;
end;
{$endif}

end.
