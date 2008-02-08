unit customsqliteds;

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
  Classes, SysUtils, Db;

type
  PDataRecord = ^DataRecord;
  PPDataRecord = ^PDataRecord;
  DataRecord = record
    Row: PPchar;
    BookmarkFlag: TBookmarkFlag;
    Next: PDataRecord;
    Previous: PDataRecord;
  end;
  
  TDSStream = class(TStream)
  private
    FActiveItem:PDataRecord;
    FFieldRow:PChar;  
    FFieldIndex:Integer;
    FRowSize: Integer;
    FPosition: Longint;
  public
    constructor Create(const ActiveItem: PDataRecord; FieldIndex:Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  //callback types
  TSqliteCdeclCallback = function (UserData:Pointer; Count:LongInt; Values:PPChar; Names:PPChar):LongInt;cdecl;
  TSqliteCallback = function (UserData:Pointer; Count:LongInt; Values:PPChar; Names:PPChar):LongInt of Object;
  TCallbackInfo = record
    Proc: TSqliteCallback;
    Data: Pointer;
  end;
  PCallbackInfo = ^TCallbackInfo;
  
  TRecordState = (rsAdded,rsDeleted,rsUpdated);
  TRecordStateSet = set of TRecordState;
  TQueryUpdatesCallback = procedure (UserData: Pointer; Values: PPChar; ABookmark: TBookmark; RecordState: TRecordState) of Object;

  TGetSqlStrFunction = function (APChar: PChar): String;

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
    FExpectedAppends: Integer;
    FExpectedDeletes: Integer;
    FExpectedUpdates: Integer;
    FOnCallback: TSqliteCallback;
    FSaveOnClose: Boolean;
    FSaveOnRefetch: Boolean;
    FAutoIncrementKey: Boolean;
    FMasterLink: TMasterDataLink;
    FIndexFieldNames: String;
    FIndexFieldList: TList;
    FSqlList:TStrings;
    procedure CopyCacheToItem(AItem: PDataRecord);
    function GetIndexFields(Value: Integer): TField;
    procedure SetMasterIndexValue;
    procedure UpdateIndexFields;
    function FindRecordItem(StartItem: PDataRecord; const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; DoResync:Boolean):PDataRecord;
  protected
    FPrimaryKey: String;
    FPrimaryKeyNo: Integer;
    FFileName: String;
    FSql: String;
    FTableName: String;
    FSelectSqlStr: String;
    FAutoIncFieldNo: Integer;
    FNextAutoInc:Integer;
    FUpdatedItems: TFPList;
    FAddedItems: TFPList;
    FDeletedItems: TFPList;
    FReturnCode: Integer;
    FSqliteHandle: Pointer;
    FDataAllocated: Boolean;
    FRowBufferSize: Integer;
    FRowCount: Integer;
    FRecordCount: Integer;
    FBeginItem: PDataRecord;
    FEndItem: PDataRecord;
    FCacheItem: PDataRecord;
    FGetSqlStr: array of TGetSqlStrFunction;
    function SqliteExec(Sql:PChar; ACallback: TSqliteCdeclCallback; Data: Pointer):Integer;virtual; abstract;
    procedure InternalCloseHandle;virtual;abstract;
    function InternalGetHandle: Pointer; virtual; abstract;
    procedure GetSqliteHandle;
    function GetSqliteVersion: String; virtual; abstract;
    procedure BuildLinkedList; virtual; abstract;
    procedure FreeItem(AItem: PDataRecord);
    procedure DisposeLinkedList;
    procedure SetDetailFilter;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetMasterFields(const Value:String);
    function GetMasterFields:String;
    procedure SetMasterSource(Value: TDataSource);
    function GetMasterSource:TDataSource;
    procedure SetFileName(const Value: String);
    function GetRowsAffected:Integer; virtual;abstract;
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
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetExpectedAppends(AValue:Integer);
    procedure SetExpectedUpdates(AValue:Integer);
    procedure SetExpectedDeletes(AValue:Integer);
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
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : Boolean; override;
    function LocateNext(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;override;
    // Additional procedures
    function ApplyUpdates: Boolean;
    function CreateTable: Boolean;
    function CreateTable(const ATableName: String): Boolean;
    procedure ExecCallback(const ASql: String; UserData: Pointer = nil);
    procedure ExecSQL;
    procedure ExecSQL(const ASql:String);
    procedure ExecSQLList;
    procedure ExecuteDirect(const ASql: String);virtual;abstract;
    procedure QueryUpdates(RecordStates: TRecordStateSet; Callback: TQueryUpdatesCallback; UserData: Pointer=nil);
    function QuickQuery(const ASql:String):String;overload;
    function QuickQuery(const ASql:String;const AStrList: TStrings):String;overload;
    function QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;virtual;abstract;overload;
    procedure RefetchData;
    function ReturnString: String; virtual;abstract;
    function TableExists: Boolean;
    function TableExists(const ATableName:String):Boolean;
    function UpdatesPending: Boolean;
    {$ifdef DEBUGACTIVEBUFFER}
    procedure SetCurrentItem(Value:PDataRecord);
    property FCurrentItem: PDataRecord read FFCurrentItem write SetCurrentItem;
    {$endif}
    property ExpectedAppends: Integer read FExpectedAppends write SetExpectedAppends;
    property ExpectedUpdates: Integer read FExpectedUpdates write SetExpectedUpdates;
    property ExpectedDeletes: Integer read FExpectedDeletes write SetExpectedDeletes;
    property IndexFields[Value: Integer]: TField read GetIndexFields;
    property RowsAffected: Integer read GetRowsAffected;
    property ReturnCode: Integer read FReturnCode;
    property SqliteHandle: Pointer read FSqliteHandle;
    property SqliteVersion: String read GetSqliteVersion;
    property SQLList:TStrings read FSqlList;
   published
    property AutoIncrementKey: Boolean read FAutoIncrementKey write FAutoIncrementKey;
    property IndexFieldNames: string read FIndexFieldNames write FIndexFieldNames;
    property FileName: String read FFileName write SetFileName;
    property OnCallback: TSqliteCallback read FOnCallback write FOnCallback;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property SaveOnClose: Boolean read FSaveOnClose write FSaveOnClose; 
    property SaveOnRefetch: Boolean read FSaveOnRefetch write FSaveOnRefetch;
    property SQL: String read FSql write FSql;
    property TableName: String read FTableName write FTableName;   
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    
    property Active;
       
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
  
  function Num2SqlStr(APChar: PChar): String;
  function Char2SqlStr(APChar: PChar): String;


implementation

uses
  strutils, variants, dbconst;

const
  //sqlite2.x.x and sqlite3.x.x define these constants equally
  SQLITE_OK = 0;
  SQLITE_ROW = 100;
  
  NullString = 'NULL';
  

function CallbackDispatcher(UserData:Pointer; Count:LongInt; Values:PPchar; Names:PPchar):LongInt;cdecl;
begin
  with PCallbackInfo(UserData)^ do
    Result:= Proc(Data,Count,Values,Names);
end;
  
function Num2SqlStr(APChar: PChar): String;
begin
  if APChar = nil then
  begin
    Result:=NullString;
    Exit;
  end;
  Result:=StrPas(APChar);
end;

function Char2SqlStr(APChar: PChar): String;
begin
  if APChar = nil then
  begin
    Result:=NullString;
    Exit;
  end;
  //todo: create custom routine to directly transform PChar -> SQL str
  Result:=StrPas(APChar);
  if Pos('''',Result) > 0 then
    Result:=AnsiReplaceStr(Result,'''','''''');
  Result:=''''+Result+'''';
end;

// TDSStream

constructor TDSStream.Create(const ActiveItem: PDataRecord; FieldIndex:Integer);
begin
  inherited Create;
  FPosition:=0;
  FActiveItem:=ActiveItem;
  FFieldIndex:=FieldIndex;
  FFieldRow:=ActiveItem^.Row[FieldIndex];
  if FFieldRow <> nil then
    FRowSize:=StrLen(FFieldRow)
  else
    FRowSize:=0;  
end;  

function TDSStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Case Origin of
    soFromBeginning : FPosition:=Offset;
    soFromEnd       : FPosition:=FRowSize+Offset;
    soFromCurrent   : FPosition:=FPosition+Offset;
  end;
  Result:=FPosition;
end;

function TDSStream.Write(const Buffer; Count: Longint): Longint; 
var
  NewRow:PChar;
begin
  Result:=Count;
  if Count = 0 then
    Exit;
  //FRowSize is always 0 when FPosition = 0,
  //so there's no need to check FPosition
  NewRow:=StrAlloc(FRowSize+Count+1);
  (NewRow+Count+FRowSize)^:=#0;
  if FRowSize > 0 then
    Move(FFieldRow^,NewRow^,FRowSize);
  Move(Buffer,(NewRow+FRowSize)^,Count);
  FActiveItem^.Row[FFieldIndex]:=NewRow;    
  StrDispose(FFieldRow);
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TDSStream.Write##');
  WriteLn('  FPosition(Before): ',FPosition);
  WriteLn('  FRowSize(Before): ',FRowSize);
  WriteLn('  FPosition(After): ',FPosition+Count);
  WriteLn('  FRowSize(After): ',StrLen(NewRow));
  //WriteLn('  Stream Value: ',NewRow);
  {$endif}
  FFieldRow:=NewRow;
  FRowSize:=StrLen(NewRow);
  Inc(FPosition,Count);
end; 
 
function TDSStream.Read(var Buffer; Count: Longint): Longint; 
var
  BytesToMove:Integer;
begin
  if (FRowSize - FPosition) >= Count then
    BytesToMove:=Count
  else
    BytesToMove:=FRowSize - FPosition;   
  Move((FFieldRow+FPosition)^,Buffer,BytesToMove);
  Inc(FPosition,BytesToMove);
  Result:=BytesToMove;  
  {$ifdef DEBUG_SQLITEDS}
  WriteLn('##TDSStream.Read##');
  WriteLn('  Bytes requested: ',Count);
  WriteLn('  Bytes moved: ',BytesToMove);
  WriteLn('  Stream.Size: ',FRowSize);
  //WriteLn('  Stream Value: ',FFieldRow);
  {$endif}
end; 
 
// TCustomSqliteDataset override methods

function TCustomSqliteDataset.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(SizeOf(PPDataRecord));
  PDataRecord(Pointer(Result)^):=FBeginItem;
end;

constructor TCustomSqliteDataset.Create(AOwner: TComponent);
begin
  // setup special items
  New(FBeginItem);
  New(FCacheItem);
  New(FEndItem);
  
  FBeginItem^.Previous := nil;
  FEndItem^.Next := nil;
  
  FBeginItem^.BookMarkFlag := bfBOF;
  FEndItem^.BookMarkFlag := bfEOF;
  
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := @MasterChanged;
  FMasterLink.OnMasterDisable := @MasterDisabled;
  FIndexFieldList := TList.Create;
  BookmarkSize := SizeOf(Pointer);
  FUpdatedItems := TFPList.Create;
  FAddedItems := TFPList.Create;
  FDeletedItems := TFPList.Create;
  FSqlList := TStringList.Create;
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
    FCacheItem^.Row[Field.FieldNo - 1]:=nil;
  end;
  Result:= TDSStream.Create(PPDataRecord(ActiveBuffer)^,Field.FieldNo - 1);
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
    FInsertBookmark:=FEndItem
  else
    FInsertBookmark:=FInternalActiveBuffer;
  inherited DoAfterInsert;
end;

procedure TCustomSqliteDataset.DoBeforeInsert;
begin
  FInternalActiveBuffer:=PPDataRecord(ActiveBuffer)^;
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
  FSqlList.Destroy;
  // dispose special items
  Dispose(FBeginItem);
  Dispose(FCacheItem);
  Dispose(FEndItem);
end;

function TCustomSqliteDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
var
  TempItem: PDataRecord;
begin
  Result:=False;
  TempItem:=FBeginItem^.Next;
  while TempItem <> FEndItem do
  begin
    if TempItem = PPDataRecord(ABookmark)^ then
    begin
      Result:=True;
      Exit;
    end;
    TempItem:=TempItem^.Next;
  end;
end;

function TCustomSqliteDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark
  ): Longint;
var
  TempItem: PDataRecord;
begin
  if PPDataRecord(Bookmark1)^ = PPDataRecord(Bookmark2)^  then
  begin
    Result:=0;
    Exit;
  end;
  //assume  Bookmark1 < Bookmark2
  Result:=-1;
  TempItem:=PPDataRecord(Bookmark1)^^.Previous;
  while TempItem <> FBeginItem do
  begin
    if TempItem = PPDataRecord(Bookmark2)^ then
    begin
      //Bookmark1 is greater than Bookmark2
      Result:=1;
      Exit;
    end;
    TempItem:=TempItem^.Previous;
  end;
end;

procedure TCustomSqliteDataset.CopyCacheToItem(AItem: PDataRecord);
var
  i:Integer;
begin
  for i := 0 to FRowCount - 1 do
  begin
    StrDispose(AItem^.Row[i]);
    AItem^.Row[i]:=FCacheItem^.Row[i];
    FCacheItem^.Row[i]:=nil;
  end;
  AItem^.BookmarkFlag:=FCacheItem^.BookmarkFlag;
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

procedure TCustomSqliteDataset.DisposeLinkedList;
var
  TempItem:PDataRecord;
  i:Integer;
begin
  //Todo: insert debug info
  //Todo: see if FDataAllocated is still necessary
  FDataAllocated:=False;
  TempItem:=FBeginItem^.Next;
  while TempItem^.Next <> nil do
  begin
    TempItem:=TempItem^.Next;
    FreeItem(TempItem^.Previous);
  end; 

  //Dispose Deleted Items
  //Directly access list pointer since the index check is already done in the loop
  for i:= 0 to FDeletedItems.Count - 1 do
    FreeItem(PDataRecord(FDeletedItems.List^[i]));

  //Dispose FBeginItem.Row
  FreeMem(FBeginItem^.Row,FRowBufferSize);
    
  //Dispose cache item row
  for i:= 0 to FRowCount - 1 do
    StrDispose(FCacheItem^.Row[i]);
  FreeMem(FCacheItem^.Row,FRowBufferSize);
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
  ValError:Word;
  FieldRow:PChar;
begin
  FieldRow:=PPDataRecord(ActiveBuffer)^^.Row[Field.FieldNo - 1];
  Result := FieldRow <> nil;  
  if Result and (Buffer <> nil) then //supports GetIsNull
  begin
    case Field.Datatype of
    ftString:
      begin
        Move(FieldRow^,PChar(Buffer)^,StrLen(FieldRow)+1);
      end;
    ftInteger,ftAutoInc:
      begin
        Val(StrPas(FieldRow),LongInt(Buffer^),ValError);
        Result:= ValError = 0;  
      end;
    ftBoolean,ftWord:
      begin
        Val(StrPas(FieldRow),Word(Buffer^),ValError);
        Result:= ValError = 0;
      end;    
    ftFloat,ftDateTime,ftTime,ftDate,ftCurrency:
      begin
        Val(StrPas(FieldRow),Double(Buffer^),ValError);
        Result:= ValError = 0; 
      end;
    ftLargeInt:
      begin
        Val(StrPas(FieldRow),Int64(Buffer^),ValError);
        Result:= ValError = 0;
      end;        
    end;
  end;        
end;

function TCustomSqliteDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result:=GetFieldData(Field, Buffer, False);
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
        FCurrentItem:=FCurrentItem^.Next;
  end; //case
  if Result = grOk then
  begin
    PDataRecord(Pointer(Buffer)^):=FCurrentItem;
    FCurrentItem^.BookmarkFlag := bfCurrent;
  end
    else if (Result = grError) and DoCheck then
      DatabaseError('No records found',Self);
end;

function TCustomSqliteDataset.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

function TCustomSqliteDataset.GetRecNo: Integer;
var
  TempItem,TempActive:PDataRecord;
begin
  Result:= -1;
  if (FRecordCount = 0) or (State = dsInsert) then
    Exit;  
  TempItem:=FBeginItem;
  TempActive:=PPDataRecord(ActiveBuffer)^;
  if TempActive = FCacheItem then // Record is being edited
  begin
    TempActive:=FInternalActiveBuffer;
  end;
  //RecNo is 1 based
  inc(Result);
  while TempActive <> TempItem do
  begin
    if TempItem^.Next <> nil then
    begin
      inc(Result);
      TempItem:=TempItem^.Next;
    end  
    else
    begin
      Result:=-1;
      DatabaseError('GetRecNo - ActiveItem Not Found',Self);
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
    DatabaseError('PPDataRecord(ActiveBuffer) <> FCacheItem - Problem',Self);
  {$endif}
  New(NewItem);
  GetMem(NewItem^.Row,FRowBufferSize);
  //if is a detail dataset then set the index value
  if FMasterLink.Active then
    SetMasterIndexValue;
  //necessary to nullify the Row before copy the cache
  FillChar(NewItem^.Row^,FRowBufferSize,#0);
  CopyCacheToItem(NewItem);

  //insert in the linked list
  FInsertBookmark^.Previous^.Next:=NewItem;
  NewItem^.Next:=FInsertBookmark;
  NewItem^.Previous:=FInsertBookmark^.Previous;
  FInsertBookmark^.Previous:=NewItem;
  
  //update the cursor
  FCurrentItem:=NewItem;
  
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
  FRecordCount:=0;
end;

procedure TCustomSqliteDataset.InternalCancel;
var
  i: Integer;
begin
  PPDataRecord(ActiveBuffer)^:=FInternalActiveBuffer;
  //free the cache
  for i:= 0 to FRowCount - 1 do
  begin
    StrDispose(FCacheItem^.Row[i]);
    FCacheItem^.Row[i]:=nil;
  end;
end;

procedure TCustomSqliteDataset.InternalDelete;
var
  TempItem:PDataRecord;
  ValError,TempInteger:Integer;
begin
  Dec(FRecordCount);
  TempItem:=PPDataRecord(ActiveBuffer)^;
  TempItem^.Next^.Previous:=TempItem^.Previous;
  TempItem^.Previous^.Next:=TempItem^.Next;
  if FCurrentItem = TempItem then
  begin
    if FCurrentItem^.Previous <> FBeginItem then
      FCurrentItem:= FCurrentItem^.Previous
    else
      FCurrentItem:= FCurrentItem^.Next;  
  end; 
  // Dec FNextAutoInc (only if deleted item is the last record)  
  if FAutoIncFieldNo <> -1 then
  begin
    Val(StrPas(TempItem^.Row[FAutoIncFieldNo]),TempInteger,ValError);
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
  FInternalActiveBuffer:=PPDataRecord(ActiveBuffer)^;
  //copy active item to cache
  for i:= 0 to FRowCount - 1 do
    FCacheItem^.Row[i]:=StrNew(FInternalActiveBuffer^.Row[i]);
  FCacheItem^.BookmarkFlag:=FInternalActiveBuffer^.BookmarkFlag;
  //now active buffer is the cache item
  PPDataRecord(ActiveBuffer)^:=FCacheItem;
end;

procedure TCustomSqliteDataset.InternalFirst;
begin
  FCurrentItem := FBeginItem;
end;

procedure TCustomSqliteDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentItem := PDataRecord(ABookmark^);
end;

procedure TCustomSqliteDataset.InternalInitRecord(Buffer: PChar);
var
  TempStr:String;  
begin
  if FAutoIncFieldNo <> - 1 then
  begin
    Str(FNextAutoInc,TempStr);
    FCacheItem^.Row[FAutoIncFieldNo]:=StrAlloc(Length(TempStr)+1);
    StrPCopy(FCacheItem^.Row[FAutoIncFieldNo],TempStr);
  end;  
  PPDataRecord(Buffer)^:=FCacheItem;    
  FCacheItem^.BookmarkFlag:=bfInserted;
end;

procedure TCustomSqliteDataset.InternalLast;
begin
  FCurrentItem := FEndItem;
end;

procedure TCustomSqliteDataset.InternalOpen;
var
  i:Integer;
begin
  if FMasterLink.DataSource <> nil then
  begin
    //todo: retrieve only necessary fields
    FSql := 'Select * from '+FTableName+';'; // forced to obtain all fields
  end;

  if FSql = '' then
  begin
    if FTablename = '' then
      DatabaseError('Tablename not set',Self);
    FSql := 'Select * from '+FTableName+';';
  end;

  if FSqliteHandle = nil then
    GetSqliteHandle;
    
  InternalInitFieldDefs;
  //todo: move this to InitFieldDefs
  FSelectSqlStr:='SELECT ';
  for i:= 0 to FieldDefs.Count - 2 do
    FSelectSqlStr:=FSelectSqlStr+FieldDefs[i].Name+',';
  FSelectSqlStr:=FSelectSqlStr+FieldDefs[FieldDefs.Count - 1].Name+
    ' FROM '+FTableName;

  if DefaultFields then 
    CreateFields;
  BindFields(True);

  UpdateIndexFields;
  if FMasterLink.Active then
  begin
    if FIndexFieldList.Count <> FMasterLink.Fields.Count then
      DatabaseError('MasterFields count doesn''t match IndexFields count',Self);
    //Set FSql considering MasterSource active record
    SetDetailFilter;
  end;

  // Get PrimaryKeyNo if available
  if Fields.FindField(FPrimaryKey) <> nil then
    FPrimaryKeyNo:=Fields.FindField(FPrimaryKey).FieldNo - 1  
  else
    FPrimaryKeyNo:=FAutoIncFieldNo; // -1 if there's no AutoIncField 

  BuildLinkedList;               
  FCurrentItem:=FBeginItem;
end;

procedure TCustomSqliteDataset.InternalPost;
begin
  if State <> dsEdit then
    InternalAddRecord(nil, True)
  else
  begin
    CopyCacheToItem(FInternalActiveBuffer);
    PPDataRecord(ActiveBuffer)^:=FInternalActiveBuffer;
    if (FUpdatedItems.IndexOf(FInternalActiveBuffer) = -1) and
      (FAddedItems.IndexOf(FInternalActiveBuffer) = -1) then
      FUpdatedItems.Add(FInternalActiveBuffer);
  end;
end;

procedure TCustomSqliteDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentItem:=PPDataRecord(Buffer)^;
end;

function TCustomSqliteDataset.IsCursorOpen: Boolean;
begin
   Result := FDataAllocated;
end;

type
  TLocateCompareFunction = function (Value, Key: PChar): Boolean;
  
  TLocateFieldInfo = record
    Index: Integer;
    Key: String;
    CompFunction: TLocateCompareFunction;
  end;

function CompInsensitivePartial(Value, Key: PChar): Boolean;
begin
  if Value <> nil then
    Result := StrLIComp(Value, Key, StrLen(Key)) = 0
  else
    Result := False;
end;

function CompSensitivePartial(Value, Key: PChar): Boolean;
begin
  if Value <> nil then
    Result := StrLComp(Value, Key, StrLen(Key)) = 0
  else
    Result := False;
end;

function CompInsensitive(Value, Key: PChar): Boolean;
begin
  if Value <> nil then
    Result := StrIComp(Value, Key) = 0
  else
    Result := False;
end;

function CompSensitive(Value, Key: PChar): Boolean;
begin
  if Value <> nil then
    Result := StrComp(Value, Key) = 0
  else
    Result := False;
end;

function TCustomSqliteDataset.FindRecordItem(StartItem: PDataRecord; const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; DoResync:Boolean):PDataRecord;
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
        if Succ(VarArrayHighBound(KeyValues,1)) <> AFieldCount then
          DatabaseError('Number of fields does not correspond to number of values',Self);
      end
      else
        DatabaseError('Wrong number of values specified: expected an array of variants got a variant',Self);
    end;
    
    //set the array of the fields info
    SetLength(LocateFields, AFieldCount);
    
    for i := 0 to AFieldCount - 1 do
      with TField(AFieldList[i]) do
      begin
        if not (DataType in [ftFloat,ftDateTime,ftTime,ftDate]) then
        begin
          //the loPartialKey and loCaseInsensitive is ignored in numeric fields
          if DataType in [ftString, ftMemo] then
          begin
            if loPartialKey in Options then
            begin
              if loCaseInsensitive in Options then
                LocateFields[i].CompFunction := @CompInsensitivePartial
              else
                LocateFields[i].CompFunction := @CompSensitivePartial;
            end
            else
            begin
              if loCaseInsensitive in Options then
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
  writeln('##TCustomSqliteDataset.FindRecordItem##');
  writeln('  KeyFields: ', KeyFields);
  for i := 0 to AFieldCount - 1 do
  begin
    writeln('LocateFields[',i,']');
    writeln('  Key: ', LocateFields[i].Key);
    writeln('  Index: ', LocateFields[i].Index);
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
      if not CompFunction(TempItem^.Row[Index], PChar(Key)) then
      begin
        MatchRecord := False;
        Break;//for
      end;
    end;
    if MatchRecord then
    begin
      Result := TempItem;
      if DoResync then
      begin
        FCurrentItem := TempItem;
        Resync([]);
      end;
      Break;//while
    end;
    TempItem := TempItem^.Next;
  end;      
end;

procedure TCustomSqliteDataset.GetSqliteHandle;
begin
  if FFileName = '' then
    DatabaseError('Filename not set',Self);
  FSqliteHandle := InternalGetHandle;
end;

procedure TCustomSqliteDataset.FreeItem(AItem: PDataRecord);
var
  i: Integer;
begin
  for i:= 0 to FRowCount - 1 do
    StrDispose(AItem^.Row[i]);
  FreeMem(AItem^.Row,FRowBufferSize);
  Dispose(AItem);
end;

function TCustomSqliteDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : Boolean;
begin
  CheckBrowseMode;
  Result := FindRecordItem(FBeginItem^.Next, KeyFields, KeyValues, Options, True) <> nil;
end;
  
function TCustomSqliteDataset.LocateNext(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : Boolean;
begin
  CheckBrowseMode;
  Result := FindRecordItem(PPDataRecord(ActiveBuffer)^^.Next, KeyFields, KeyValues, Options, True) <> nil;
end;
  
function TCustomSqliteDataset.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
var
  TempItem: PDataRecord;
begin
  CheckBrowseMode;
  TempItem := FindRecordItem(FBeginItem^.Next, KeyFields, KeyValues, [], False);
  if TempItem <> nil then
    Result := TempItem^.Row[FieldByName(ResultFields).FieldNo - 1]
  else
    Result := False;
end;  

procedure TCustomSqliteDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  //The BookMarkData is the Buffer itself: no need to set nothing;
end;

procedure TCustomSqliteDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PPDataRecord(Buffer)^^.BookmarkFlag := Value;
end;

procedure TCustomSqliteDataset.SetExpectedAppends(AValue:Integer);
begin
  FAddedItems.Capacity:=AValue;
end;  

procedure TCustomSqliteDataset.SetExpectedUpdates(AValue:Integer);
begin
  FUpdatedItems.Capacity:=AValue;
end;  

procedure TCustomSqliteDataset.SetExpectedDeletes(AValue:Integer);
begin
  FDeletedItems.Capacity:=AValue;
end;  

procedure TCustomSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
var
  TempStr:String;
begin
  if not (State in [dsEdit, dsInsert]) then
    DatabaseErrorFmt(SNotEditing,[Name],Self);

  StrDispose(FCacheItem^.Row[Pred(Field.FieldNo)]);
  if Buffer <> nil then
  begin
    case Field.Datatype of
    ftString:
      begin            
        FCacheItem^.Row[Pred(Field.FieldNo)]:=StrNew(PChar(Buffer));
      end;
    ftInteger:
      begin          
        Str(LongInt(Buffer^),TempStr);  
        FCacheItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
        Move(PChar(TempStr)^,(FCacheItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr)+1);
      end;
    ftBoolean,ftWord:
      begin
        Str(Word(Buffer^),TempStr);  
        FCacheItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
        Move(PChar(TempStr)^,(FCacheItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr)+1);
      end;  
    ftFloat,ftDateTime,ftDate,ftTime,ftCurrency:
      begin
        Str(Double(Buffer^),TempStr);  
        FCacheItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr));
        //Skips the first space that str returns
        //todo: make a custom Str?
        Move((PChar(TempStr)+1)^,(FCacheItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr));
      end;
    ftLargeInt:
      begin
        Str(Int64(Buffer^),TempStr);  
        FCacheItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
        Move(PChar(TempStr)^,(FCacheItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr)+1);
      end;        
    end;// case
  end//if
  else
    FCacheItem^.Row[Pred(Field.FieldNo)]:=nil;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Ptrint(Field));  
end;

procedure TCustomSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
  SetFieldData(Field, Buffer, False);
end;

procedure TCustomSqliteDataset.SetRecNo(Value: Integer);
var
  Counter:Integer;
  TempItem:PDataRecord;
begin
  if (Value > FRecordCount) or (Value <= 0) then
    DatabaseError('Record Number Out Of Range',Self);
  CheckBrowseMode;
  TempItem:=FBeginItem;
  for Counter := 1 to Value do
    TempItem:=TempItem^.Next;
  FCurrentItem:=TempItem;
  Resync([]);
end;

// Specific functions 

procedure TCustomSqliteDataset.SetDetailFilter;
  function FieldToSqlStr(AField:TField):String;
  begin
    if not AField.IsNull then
    begin
      case AField.DataType of
        ftString,ftMemo: Result:='"'+AField.AsString+'"';//todo: handle " caracter properly
        ftDateTime,ftDate,ftTime:Str(AField.AsDateTime,Result);
      else
        Result:=AField.AsString;
      end;//case
    end
    else
      Result:=NullString;
  end;//function

var
  AFilter:String;
  i:Integer;
begin
  if FMasterLink.Dataset.RecordCount = 0 then //Retrieve all data
    FSql:='Select * from '+FTableName
  else
  begin
    AFilter:=' where ';
    for i:= 0 to FMasterLink.Fields.Count - 1 do
    begin
      AFilter:=AFilter + IndexFields[i].FieldName +' = '+ FieldToSqlStr(TField(FMasterLink.Fields[i]));
      if i <> FMasterLink.Fields.Count - 1 then
        AFilter:= AFilter + ' and ';
    end;
    FSql:='Select * from '+FTableName+AFilter;
  end;
end;

procedure TCustomSqliteDataset.MasterChanged(Sender: TObject);
begin
  SetDetailFilter;
  {$ifdef DEBUG_SQLITEDS}
  writeln('##TCustomSqliteDataset.MasterChanged##');
  writeln('  SQL used to filter detail dataset:');
  writeln('  ',FSql);
  {$endif}
  RefetchData;
end;

procedure TCustomSqliteDataset.MasterDisabled(Sender: TObject);
begin
  FSql:='Select * from '+FTableName+';'; 
  RefetchData;
end;

procedure TCustomSqliteDataset.SetMasterFields(const Value: String);
begin
  FMasterLink.FieldNames:=Value;
  if Active and FMasterLink.Active then
  begin
    UpdateIndexFields;
    if (FIndexFieldList.Count <> FMasterLink.Fields.Count) then
      DatabaseError('MasterFields count doesn''t match IndexFields count',Self);
  end;
end;

function TCustomSqliteDataset.GetMasterFields: String;
begin
  Result:=FMasterLink.FieldNames;
end;

procedure TCustomSqliteDataset.UpdateIndexFields;
begin
  FIndexFieldList.Clear;
  if FIndexFieldNames <> '' then
  begin
    try
      GetFieldList(FIndexFieldList, FIndexFieldNames);
    except
      FIndexFieldList.Clear;
      raise;
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
      DatabaseError('It''s not allowed to change Filename in an open dataset',Self);
    if FSqliteHandle <> nil then
      InternalCloseHandle;
    FFileName:=Value;
  end;
end;

procedure TCustomSqliteDataset.SetMasterSource(Value: TDataSource);
begin
  FMasterLink.DataSource := Value;
end;

procedure TCustomSqliteDataset.ExecSQL(const ASql:String);
begin
  if FSqliteHandle = nil then
    GetSqliteHandle;
  ExecuteDirect(ASql);
end;

procedure TCustomSqliteDataset.ExecSQLList;
begin
  if FSqliteHandle = nil then
    GetSqliteHandle;
  SqliteExec(PChar(FSqlList.Text),nil,nil);
end;

procedure TCustomSqliteDataset.ExecSQL;
begin
  ExecSQL(FSql);
end;

function TCustomSqliteDataset.ApplyUpdates:Boolean;
var
  iFields, iItems, StatementsCounter, TempReturnCode:Integer;
  SqlTemp,WhereKeyNameEqual,ASqlLine,TemplateStr:String;
  TempItem: PDataRecord;
begin
  CheckBrowseMode;
  if not UpdatesPending then
  begin
    Result:=True;
    Exit;
  end;
  Result:=False;
  if FPrimaryKeyNo <> -1 then
  begin
    FReturnCode := SQLITE_OK;
    StatementsCounter:=0;
    WhereKeyNameEqual:=' WHERE '+Fields[FPrimaryKeyNo].FieldName+' = ';
    {$ifdef DEBUG_SQLITEDS}
    WriteLn('##TCustomSqliteDataset.ApplyUpdates##');
    if FPrimaryKeyNo = FAutoIncFieldNo then
      WriteLn('  Using an AutoInc field as primary key');
    WriteLn('  PrimaryKey: ',WhereKeyNameEqual);
    WriteLn('  PrimaryKeyNo: ',FPrimaryKeyNo);
    {$endif}
    SqlTemp:='BEGIN;';
    // Delete Records
    if FDeletedItems.Count > 0 then
      TemplateStr:='DELETE FROM '+FTableName+WhereKeyNameEqual;
    for iItems:= 0 to FDeletedItems.Count - 1 do
    begin
      TempItem:=PDataRecord(FDeletedItems.List^[iItems]);
      SqlTemp:=SqlTemp+(TemplateStr+
        StrPas(TempItem^.Row[FPrimaryKeyNo])+';');
      FreeItem(TempItem);
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'COMMIT;';
        TempReturnCode := SqliteExec(PChar(SqlTemp),nil,nil);
        if TempReturnCode <> SQLITE_OK then
          FReturnCode := TempReturnCode;  
        StatementsCounter:=0;
        SqlTemp:='BEGIN;';
      end;    
    end;
    // Update changed records
    if FUpdatedItems.Count > 0 then
      TemplateStr:='UPDATE '+FTableName+' SET ';
    for iItems:= 0 to FUpdatedItems.Count - 1 do
    begin
      ASqlLine:=TemplateStr;
      for iFields:= 0 to Fields.Count - 2 do
      begin
        ASqlLine:=ASqlLine + (Fields[iFields].FieldName +' = '+
          FGetSqlStr[iFields](PDataRecord(FUpdatedItems[iItems])^.Row[iFields])+',');
      end;
      iFields:=Fields.Count - 1;
      ASqlLine:=ASqlLine + (Fields[iFields].FieldName +' = '+
        FGetSqlStr[iFields](PDataRecord(FUpdatedItems[iItems])^.Row[iFields])+
        WhereKeyNameEqual+StrPas(PDataRecord(FUpdatedItems[iItems])^.Row[FPrimaryKeyNo])+';');
      SqlTemp:=SqlTemp + ASqlLine;
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'COMMIT;';
        TempReturnCode := SqliteExec(PChar(SqlTemp),nil,nil);
        if TempReturnCode <> SQLITE_OK then
          FReturnCode := TempReturnCode;
        StatementsCounter:=0;
        SqlTemp:='BEGIN;';
      end;  
    end;
    // Add new records
    // Build TemplateStr
    if FAddedItems.Count > 0 then
    begin
      TemplateStr:='INSERT INTO '+FTableName+ ' (';
      for iFields:= 0 to Fields.Count - 2 do
        TemplateStr:=TemplateStr + Fields[iFields].FieldName+',';
      TemplateStr:= TemplateStr+Fields[Fields.Count - 1].FieldName+') VALUES (';
    end;  
    for iItems:= 0 to FAddedItems.Count - 1 do
    begin
      ASqlLine:=TemplateStr;
      for iFields:= 0 to Fields.Count - 2 do
      begin
        ASqlLine:=ASqlLine + (FGetSqlStr[iFields](PDataRecord(FAddedItems[iItems])^.Row[iFields])+',');
      end;
      //todo: see if i can assume iFields = Fields.Count-2 safely
      iFields:=Fields.Count - 1;
      ASqlLine:=ASqlLine + (FGetSqlStr[iFields](PDataRecord(FAddedItems[iItems])^.Row[iFields])+');');
      SqlTemp:=SqlTemp + ASqlLine;    
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'COMMIT;';
        TempReturnCode := SqliteExec(PChar(SqlTemp),nil,nil);
        if TempReturnCode <> SQLITE_OK then
          FReturnCode := TempReturnCode;
        StatementsCounter:=0;
        SqlTemp:='BEGIN;';
      end;  
    end;  
    SqlTemp:=SqlTemp+'COMMIT;';
    {$ifdef DEBUG_SQLITEDS}
    writeln('  SQL: ',SqlTemp);
    {$endif}  
   FAddedItems.Clear;
   FUpdatedItems.Clear;
   FDeletedItems.Clear;   
   TempReturnCode := SqliteExec(PChar(SqlTemp),nil,nil);
   if TempReturnCode <> SQLITE_OK then
     FReturnCode := TempReturnCode;
   Result:= FReturnCode = SQLITE_OK;
  end;  
  {$ifdef DEBUG_SQLITEDS}
    writeln('  Result: ',Result);
  {$endif}   
end;    

function TCustomSqliteDataset.CreateTable: Boolean;
begin
  Result:=CreateTable(FTableName);
end;

function TCustomSqliteDataset.CreateTable(const ATableName: String): Boolean;
var
  SqlTemp:String;
  i:Integer;
begin
  {$ifdef DEBUG_SQLITEDS}
  writeln('##TCustomSqliteDataset.CreateTable##');
  if ATableName = '' then
    WriteLn('  TableName Not Set');
  if FieldDefs.Count = 0 then
    WriteLn('  FieldDefs Not Initialized');
  {$endif}
  if (ATableName <> '') and (FieldDefs.Count > 0) then
  begin
    SqlTemp:='CREATE TABLE '+ATableName+' (';
    for i := 0 to FieldDefs.Count-1 do
    begin
      //todo: add index to autoinc field
      SqlTemp:=SqlTemp + FieldDefs[i].Name;
      case FieldDefs[i].DataType of
        ftInteger:
          SqlTemp:=SqlTemp + ' INTEGER';
        ftString:
          SqlTemp:=SqlTemp + ' VARCHAR';
        ftBoolean:
          SqlTemp:=SqlTemp + ' BOOL_INT';
        ftFloat:
          SqlTemp:=SqlTemp + ' FLOAT';
        ftWord:
          SqlTemp:=SqlTemp + ' WORD';
        ftDateTime:
          SqlTemp:=SqlTemp + ' DATETIME';
        ftDate:
          SqlTemp:=SqlTemp + ' DATE';
        ftTime:
          SqlTemp:=SqlTemp + ' TIME';
        ftLargeInt:
          SqlTemp:=SqlTemp + ' LARGEINT';
        ftCurrency:
          SqlTemp:=SqlTemp + ' CURRENCY';
        ftAutoInc:
          SqlTemp:=SqlTemp + ' AUTOINC_INT';
        ftMemo:
          SqlTemp:=SqlTemp + ' TEXT';
      else
        DatabaseError('Field type "'+FieldTypeNames[FieldDefs[i].DataType]+'" not supported',Self);
      end;
      if UpperCase(FieldDefs[i].Name) = UpperCase(FPrimaryKey) then
        SqlTemp:=SqlTemp + ' PRIMARY KEY';
      if i <> FieldDefs.Count - 1 then
        SqlTemp:=SqlTemp+ ' , ';
    end;
    SqlTemp:=SqlTemp+');';
    {$ifdef DEBUG_SQLITEDS}
    writeln('  SQL: ',SqlTemp);
    {$endif}
    ExecSQL(SqlTemp);
    Result:= FReturnCode = SQLITE_OK;
  end
  else
    Result:=False;
end;

procedure TCustomSqliteDataset.ExecCallback(const ASql: String; UserData: Pointer = nil);
var
  CallbackInfo: TCallbackInfo;
begin
  if not Assigned(FOnCallback) then
    DatabaseError('OnCallback property not set',Self);
  if FSqliteHandle = nil then
    GetSqliteHandle;
  CallbackInfo.Data:=UserData;
  CallbackInfo.Proc:=FOnCallback;
  SqliteExec(PChar(ASql),@CallbackDispatcher,@CallbackInfo);
end;


procedure TCustomSqliteDataset.QueryUpdates(RecordStates: TRecordStateSet; Callback: TQueryUpdatesCallback;
  UserData: Pointer = nil);
var
  i: Integer;
  TempItem: PDataRecord;
begin
  if not Assigned(Callback) then
    DatabaseError('Callback parameter not set',Self);
  CheckBrowseMode;
  if rsDeleted in RecordStates then
    with FDeletedItems do
    for i:= 0 to Count - 1 do
      Callback(UserData,PDataRecord(Items[i])^.Row,nil,rsDeleted);
  if rsUpdated in RecordStates then
    with FUpdatedItems do
    for i:= 0 to Count - 1 do
    begin
      TempItem:=PDataRecord(Items[i]);
      Callback(UserData,TempItem^.Row,TBookmark(@TempItem),rsUpdated);
    end;
  if rsAdded in RecordStates then
    with FAddedItems do
    for i:= 0 to Count - 1 do
    begin
      TempItem:=PDataRecord(Items[i]);
      Callback(UserData,TempItem^.Row,TBookmark(@TempItem),rsAdded);
    end;
end;


procedure TCustomSqliteDataset.RefetchData;
var
  i:Integer;
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
  FCurrentItem:=FBeginItem;
  for i := 0 to BufferCount - 1 do
    PPDataRecord(Buffers[i])^:=FBeginItem;
  Resync([]);
  DoAfterScroll;
end;

function TCustomSqliteDataset.TableExists: Boolean;
begin
  Result:=TableExists(FTableName);
end;

function TCustomSqliteDataset.TableExists(const ATableName: String): Boolean;
begin
  ExecSql('SELECT name FROM SQLITE_MASTER WHERE type = ''table'' AND name LIKE '''+ ATableName+ ''';');
  Result := FReturnCode = SQLITE_ROW;
end;

function TCustomSqliteDataset.UpdatesPending: Boolean;
begin
  Result:= (FUpdatedItems.Count > 0) or
    (FAddedItems.Count > 0) or (FDeletedItems.Count > 0);
end;

function TCustomSqliteDataset.QuickQuery(const ASql:String):String;
begin
  Result:=QuickQuery(ASql,nil,False); 
end;

function TCustomSqliteDataset.QuickQuery(const ASql:String;const AStrList: TStrings):String;
begin
  Result:=QuickQuery(ASql,AStrList,False)
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
