unit customsqliteds;

{
    This is TCustomSqliteDataset, a TDataset descendant class for use with fpc compiler
    Copyright (C) 2004  Luiz Américo Pereira Câmara
    Email: pascalive@bol.com.br

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

{$Mode ObjFpc}
{$H+}
{ $Define USE_SQLITEDS_INTERNALS}
{ $Define DEBUG}
{ $Define DEBUGACTIVEBUFFER}

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
  
  TSqliteCallback = function (UserData:Pointer; Columns:longint; Values:PPchar; ColumnNames:PPchar):longint;cdecl;
  
  { TCustomSqliteDataset }

  TCustomSqliteDataset = class(TDataSet)
  private
    {$ifdef DEBUGACTIVEBUFFER}
    FFCurrentItem: PDataRecord;
    {$else}
    FCurrentItem: PDataRecord;
    {$endif}
    FBufferSize: Integer;
    FExpectedAppends: Integer;
    FExpectedDeletes: Integer;
    FExpectedUpdates: Integer;
    //FPersistentHandle: Boolean;
    FSaveOnClose: Boolean;
    FSaveOnRefetch: Boolean;
    FAutoIncrementKey: Boolean;
    FMasterLink: TMasterDataLink;
    FIndexFieldNames: String;
    FIndexFieldList: TList;
    FSqlList:TStrings;
    function GetIndexFields(Value: Integer): TField;
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
    FOrphanItems: TFPList;
    FSqliteReturnId: Integer;
    FSqliteHandle: Pointer;
    FDataAllocated: Boolean;
    FRowBufferSize: Integer;
    FRowCount: Integer;
    FRecordCount: Integer;
    FBeginItem: PDataRecord;
    FEndItem: PDataRecord;
    FCacheItem: PDataRecord;
    function SqliteExec(AHandle: Pointer; Sql:PChar):Integer;virtual; abstract;
    procedure InternalCloseHandle;virtual;abstract;
    function InternalGetHandle: Pointer; virtual; abstract;
    procedure GetSqliteHandle;
    function GetSqliteVersion: String; virtual; abstract;
    procedure BuildLinkedList; virtual; abstract;
    procedure DisposeLinkedList;
    procedure SetDetailFilter;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetMasterFields(Value:String);
    function GetMasterFields:String;
    procedure SetMasterSource(Value: TDataSource);
    function GetMasterSource:TDataSource;
    procedure SetFileName(Value: String);
    function GetRowsAffected:Integer; virtual;abstract;
    //TDataSet overrides
    function AllocRecordBuffer: PChar; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    function GetRecordSize: Word; override; 
    procedure InternalAddRecord(Buffer: Pointer; DoAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
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
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : boolean; override;   
    function LocateNext(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;{$ifndef ver2_0_0}override;{$endif}
    // Additional procedures
    function ApplyUpdates: Boolean;
    function CreateTable: Boolean;
    function CreateTable(const ATableName: String): Boolean;
    procedure ExecSQL;
    procedure ExecSQL(const ASql:String);
    procedure ExecSQLList;
    procedure ExecuteDirect(const ASql: String);virtual;abstract;
    function QuickQuery(const ASql:String):String;overload;
    function QuickQuery(const ASql:String;const AStrList: TStrings):String;overload;
    function QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;virtual;abstract;overload;
    procedure RefetchData;
    function SqliteReturnString: String; virtual;abstract;
    function TableExists: Boolean;overload;
    function TableExists(const ATableName:String):Boolean;virtual;abstract;overload;
    function UpdatesPending: Boolean;
    {$ifdef DEBUGACTIVEBUFFER}
    procedure SetCurrentItem(Value:PDataRecord);
    property FCurrentItem: PDataRecord read FFCurrentItem write SetCurrentItem;
    {$endif}
    {$ifdef USE_SQLITEDS_INTERNALS}
    property BeginItem: PDataRecord read FBeginItem;
    property EndItem: PDataRecord read FEndItem;
    property UpdatedItems: TFPList read FUpdatedItems;
    property AddedItems: TFPList read FAddedItems;
    property DeletedItems: TFPList read FDeletedItems;
    {$endif}
    property ExpectedAppends: Integer read FExpectedAppends write SetExpectedAppends;
    property ExpectedUpdates: Integer read FExpectedUpdates write SetExpectedUpdates;
    property ExpectedDeletes: Integer read FExpectedDeletes write SetExpectedDeletes;
    property IndexFields[Value: Integer]: TField read GetIndexFields;
    property RowsAffected: Integer read GetRowsAffected;
    //property PersistentHandle: boolean read FPersistentHandle write FPersistentHandle;
    property SqliteReturnId: Integer read FSqliteReturnId;
    property SqliteHandle: Pointer read FSqliteHandle;
    property SqliteVersion: String read GetSqliteVersion;
    property SQLList:TStrings read FSqlList;
   published
    property AutoIncrementKey: Boolean read FAutoIncrementKey write FAutoIncrementKey;
    property IndexFieldNames: string read FIndexFieldNames write FIndexFieldNames;
    property FileName: String read FFileName write SetFileName;
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
    property OnDeleteError;
    property OnEditError;
  end;
  
implementation

uses
  strutils, variants;

const
  SQLITE_OK = 0;//sqlite2.x.x and sqlite3.x.x defines this equal

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
  {$ifdef DEBUG}
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
  {$ifdef DEBUG}
  WriteLn('##TDSStream.Read##');
  WriteLn('  Bytes requested: ',Count);
  WriteLn('  Bytes moved: ',BytesToMove);
  WriteLn('  Stream.Size: ',FRowSize);
  //WriteLn('  Stream Value: ',FFieldRow);
  {$endif}
end; 
 
// TCustomSqliteDataset override methods

function TCustomSqliteDataset.AllocRecordBuffer: PChar;
var
  APointer:Pointer;
begin
  APointer := AllocMem(FBufferSize);
  PDataRecord(APointer^):=FBeginItem;
  Result:=APointer;
end;

constructor TCustomSqliteDataset.Create(AOwner: TComponent);
begin
  // setup special items
  New(FBeginItem);
  New(FCacheItem);
  New(FEndItem);
  
  FBeginItem^.Previous:=nil;
  FEndItem^.Next:=nil;
  
  FBeginItem^.BookMarkFlag:=bfBOF;
  FCacheItem^.BookMarkFlag:=bfEOF;
  FEndItem^.BookMarkFlag:=bfEOF;
  
  FMasterLink:=TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange:=@MasterChanged;
  FMasterLink.OnMasterDisable:=@MasterDisabled;
  FIndexFieldList:=TList.Create;
  BookmarkSize := SizeOf(Pointer);
  FBufferSize := SizeOf(PPDataRecord);
  FUpdatedItems:= TFPList.Create;
  FAddedItems:= TFPList.Create;
  FOrphanItems:= TFPList.Create;
  FDeletedItems:= TFPList.Create;
  FSqlList:=TStringList.Create;
  inherited Create(AOwner);
end;

function TCustomSqliteDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  ActiveItem:PDataRecord;
begin
  if Mode = bmWrite then
  begin
    ActiveItem:=PPDataRecord(ActiveBuffer)^;
    if (ActiveItem <> FCacheItem) and (FUpdatedItems.IndexOf(ActiveItem) = -1) and (FAddedItems.IndexOf(ActiveItem) = -1) then
      FUpdatedItems.Add(ActiveItem);
    StrDispose(ActiveItem^.Row[Field.FieldNo - 1]);
    ActiveItem^.Row[Field.FieldNo - 1]:=nil;
  end;
  Result:= TDSStream.Create(PPDataRecord(ActiveBuffer)^,Field.FieldNo - 1);
end;  

destructor TCustomSqliteDataset.Destroy;
begin
  inherited Destroy;
  if FSqliteHandle <> nil then
    InternalCloseHandle;
  FUpdatedItems.Destroy;
  FAddedItems.Destroy;
  FDeletedItems.Destroy;
  FOrphanItems.Destroy;
  FMasterLink.Destroy;
  FIndexFieldList.Destroy;
  FSqlList.Destroy;
  // dispose special items
  Dispose(FBeginItem);
  Dispose(FCacheItem);
  Dispose(FEndItem);
end;

function TCustomSqliteDataset.GetIndexFields(Value: Integer): TField;
begin
  if (Value < 0) or (Value > FIndexFieldList.Count - 1) then
    DatabaseError('Error acessing IndexFields: Index out of bonds',Self);
  Result:= TField(FIndexFieldList[Value]);
end;

procedure TCustomSqliteDataset.DisposeLinkedList;
var
  TempItem:PDataRecord;
  Counter,I:Integer;
begin
  //Todo: insert debug info
  //Todo: see if FDataAllocated is still necessary
  FDataAllocated:=False;
  TempItem:=FBeginItem^.Next;
  //Todo: see if is necessary to check if TempItem is nil (aparently is not)
  if TempItem <> nil then
    while TempItem^.Next <> nil do
    begin
      //Todo: Add procedure to Dispose and Free a Row ?
      for Counter:= 0 to FRowCount - 1 do
        StrDispose(TempItem^.Row[Counter]);  
      FreeMem(TempItem^.Row,FRowBufferSize);
      TempItem:=TempItem^.Next;
      Dispose(TempItem^.Previous);
    end; 
  
  //Dispose FBeginItem.Row
  FreeMem(FBeginItem^.Row,FRowBufferSize);
    
  //Dispose cache item
  for Counter:= 0 to FRowCount - 1 do
    StrDispose(FCacheItem^.Row[Counter]);
  FreeMem(FCacheItem^.Row,FRowBufferSize);

  //Dispose OrphanItems
  for Counter:= 0 to FOrphanItems.Count - 1 do
  begin
    TempItem:=PDataRecord(FOrphanItems[Counter]);
    for I:= 0 to FRowCount - 1 do
      StrDispose(TempItem^.Row[I]);  
    FreeMem(TempItem^.Row,FRowBufferSize);
    Dispose(TempItem);  
  end;     
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
  if FRecordCount = 0 then
    Exit;  
  TempItem:=FBeginItem;
  TempActive:=PPDataRecord(ActiveBuffer)^;
  if TempActive = FCacheItem then // Record not posted yet
    Result:=FRecordCount 
  else
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
        DatabaseError('Sqliteds.GetRecNo - ActiveItem Not Found',Self);
        break;    
      end;      
    end;  
end;

function TCustomSqliteDataset.GetRecordSize: Word;
begin
  Result := FBufferSize; //??
end;

procedure TCustomSqliteDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
var
  NewItem: PDataRecord;
  Counter:Integer;
begin
  {$ifdef DEBUG}
  if PPDataRecord(Buffer)^ <> FCacheItem then
    DatabaseError('PPDataRecord(Buffer) <> FCacheItem - Problem',Self);
  {$endif}
  New(NewItem);
  GetMem(NewItem^.Row,FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do 
    NewItem^.Row[Counter]:=StrNew(FCacheItem^.Row[Counter]);   
  FEndItem^.Previous^.Next:=NewItem;
  NewItem^.Previous:=FEndItem^.Previous;
  NewItem^.Next:=FEndItem;
  FEndItem^.Previous:=NewItem;
  Inc(FRecordCount);
  if FAutoIncFieldNo <> - 1 then
    Inc(FNextAutoInc);
  FAddedItems.Add(NewItem);
end;

procedure TCustomSqliteDataset.InternalClose;
begin
  if FSaveOnClose then
    ApplyUpdates;
  //BindFields(False);
  if DefaultFields then
    DestroyFields;
  if FDataAllocated then
    DisposeLinkedList;  
  {
  if (FSqliteHandle <> nil) and not FPersistentHandle then
  begin
    InternalCloseHandle;
    FSqliteHandle := nil;
  end;
  }
  FAddedItems.Clear;
  FUpdatedItems.Clear;
  FDeletedItems.Clear;
  FOrphanItems.Clear;
  FRecordCount:=0;
end;

procedure TCustomSqliteDataset.InternalDelete;
var
  TempItem:PDataRecord;
  ValError,TempInteger:Integer;
begin
  If FRecordCount = 0 then
    Exit;
  Dec(FRecordCount);
  TempItem:=PPDataRecord(ActiveBuffer)^;
  // Remove from changed list
  FUpdatedItems.Remove(TempItem);
  if FAddedItems.Remove(TempItem) = -1 then
    FDeletedItems.Add(TempItem);
  //Todo: see if FOrphanItems is necessary:
  //  in ApplyUpdates a check could be done
  //  to avoid "delete" the AddedItems
  FOrphanItems.Add(TempItem);
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
  Counter:Integer;
  TempStr:String;  
begin
  for Counter:= 0 to FRowCount - 1 do
  begin
    StrDispose(FCacheItem^.Row[Counter]);
    FCacheItem^.Row[Counter]:=nil;
  end;
  if FAutoIncFieldNo <> - 1 then
  begin
    Str(FNextAutoInc,TempStr);
    FCacheItem^.Row[FAutoIncFieldNo]:=StrAlloc(Length(TempStr)+1);
    StrPCopy(FCacheItem^.Row[FAutoIncFieldNo],TempStr);
  end;  
  PPDataRecord(Buffer)^:=FCacheItem;    
end;

procedure TCustomSqliteDataset.InternalLast;
begin
  FCurrentItem := FEndItem;
end;

procedure TCustomSqliteDataset.InternalOpen;
var
  i:Integer;
begin
  if MasterSource <> nil then
  begin
    //todo: retrieve only necessary fields
    FSql := 'Select * from '+FTableName+';'; // forced to obtain all fields
    FMasterLink.FieldNames:=FMasterLink.FieldNames; //workaround to fill MasterLinks.Fields
    //if FMasterLink.Fields.Count = 0 MasterChanged will not be called anyway so ignore it
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
  //writeln(FSelectSqlStr);

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
  if (State<>dsEdit) then 
    InternalAddRecord(ActiveBuffer,True);
end;

procedure TCustomSqliteDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentItem:=PPDataRecord(Buffer)^;
end;

function TCustomSqliteDataset.IsCursorOpen: Boolean;
begin
   Result := FDataAllocated;
end;

function TCustomSqliteDataset.FindRecordItem(StartItem: PDataRecord; const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; DoResync:Boolean):PDataRecord;
var
  AValue:String;
  AField:TField;
  AFieldIndex:Integer;
  TempItem:PDataRecord;
begin
  Result:=nil;
  // Now, it allows to search only one field and ignores options 
  AField:=Fields.FieldByName(KeyFields); //FieldByName raises an exception if field not found
  AFieldIndex:=AField.FieldNo - 1;  
  //get float types in appropriate format
  if not (AField.DataType in [ftFloat,ftDateTime,ftTime,ftDate]) then
    AValue:=keyvalues
  else
  begin
    Str(VarToDateTime(keyvalues),AValue);
    AValue:=Trim(AValue);
  end;  
  {$ifdef DEBUG}
  writeln('##TCustomSqliteDataset.FindRecordItem##');
  writeln('  KeyFields: ',keyfields);
  writeln('  KeyValues: ',keyvalues);
  writeln('  AValue: ',AValue);
  {$endif}        
  //Search the list
  TempItem:=StartItem;
  while TempItem <> FEndItem do
  begin
    if TempItem^.Row[AFieldIndex] <> nil then
    begin
      if StrComp(TempItem^.Row[AFieldIndex],PChar(AValue)) = 0 then
      begin
        Result:=TempItem;
        if DoResync then
        begin
          FCurrentItem:=TempItem;
          Resync([]);
        end;  
        Break;
      end;
    end;    
    TempItem:=TempItem^.Next;
  end;      
end;

procedure TCustomSqliteDataset.GetSqliteHandle;
begin
  if FFileName = '' then
    DatabaseError ('Filename not set',Self);
  //todo:Handle opening non db files
  FSqliteHandle:=InternalGetHandle;
end;

function TCustomSqliteDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : boolean;
begin
  Result:=FindRecordItem(FBeginItem^.Next,KeyFields,KeyValues,Options,True) <> nil;  
end;
  
function TCustomSqliteDataset.LocateNext(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions) : boolean;
begin
  Result:=FindRecordItem(PPDataRecord(ActiveBuffer)^^.Next,KeyFields,KeyValues,Options,True) <> nil;
end;
  
function TCustomSqliteDataset.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
var
  TempItem:PDataRecord;
begin
  TempItem:=FindRecordItem(FBeginItem^.Next,KeyFields,KeyValues,[],False);
  if TempItem <> nil then
    Result:=TempItem^.Row[FieldByName(ResultFields).FieldNo - 1]
  else
    Result:=False;      
end;  

procedure TCustomSqliteDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  //The BookMarkData is the Buffer itself;
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
  FOrphanItems.Capacity:=AValue;  
end;  

procedure TCustomSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
var
  TempStr:String;
  ActiveItem:PDataRecord;
begin
  ActiveItem:=PPDataRecord(ActiveBuffer)^;
  if (ActiveItem <> FCacheItem) and (FUpdatedItems.IndexOf(ActiveItem) = -1) and (FAddedItems.IndexOf(ActiveItem) = -1) then
    FUpdatedItems.Add(ActiveItem);
    
  StrDispose(ActiveItem^.Row[Pred(Field.FieldNo)]);
  if Buffer <> nil then
  begin
    case Field.Datatype of
    ftString:
      begin            
        ActiveItem^.Row[Pred(Field.FieldNo)]:=StrNew(PChar(Buffer));
      end;
    ftInteger:
      begin          
        Str(LongInt(Buffer^),TempStr);  
        ActiveItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
        Move(PChar(TempStr)^,(ActiveItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr)+1);
      end;
    ftBoolean,ftWord:
      begin
        Str(Word(Buffer^),TempStr);  
        ActiveItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
        Move(PChar(TempStr)^,(ActiveItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr)+1);
      end;  
    ftFloat,ftDateTime,ftDate,ftTime,ftCurrency:
      begin
        Str(Double(Buffer^),TempStr);  
        ActiveItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr));
        //Skips the first space that str returns
        //todo: make a custom Str?
        Move((PChar(TempStr)+1)^,(ActiveItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr));
      end;
    ftLargeInt:
      begin
        Str(Int64(Buffer^),TempStr);  
        ActiveItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
        Move(PChar(TempStr)^,(ActiveItem^.Row[Pred(Field.FieldNo)])^,Length(TempStr)+1);
      end;        
    end;// case
  end//if
  else
    ActiveItem^.Row[Pred(Field.FieldNo)]:=nil;        
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
  if (Value >= FRecordCount) or (Value < 0) then
    DatabaseError('Record Number Out Of Range',Self);
  TempItem:=FBeginItem;
  for Counter := 0 to Value do
    TempItem:=TempItem^.Next;
  FCurrentItem:=TempItem;
  Resync([]);
end;

// Specific functions 

procedure TCustomSqliteDataset.SetDetailFilter;
  function FieldToSqlStr(AField:TField):String;
  begin
    case AField.DataType of
      ftString,ftMemo: Result:='"'+AField.AsString+'"';//todo: handle " caracter properly
      ftDateTime,ftDate,ftTime:Str(AField.AsDateTime,Result);
    else
      Result:=AField.AsString;
    end;//case
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
  {$ifdef DEBUG}
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

procedure TCustomSqliteDataset.SetMasterFields(Value: String);
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

procedure TCustomSqliteDataset.SetFileName(Value: String);
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
  SqliteExec(FSqliteHandle,PChar(FSqlList.Text));
end;

procedure TCustomSqliteDataset.ExecSQL;
begin
  ExecSQL(FSql);
end;

function GetSqlStr(IsString: boolean; APChar: PChar): String;
begin
  if APChar = nil then
  begin
    Result:='NULL';
    Exit;
  end;  
  Result:=StrPas(APChar);
  if IsString then
  begin
    if Pos('''',Result) > 0 then
      Result:=AnsiReplaceStr(Result,'''','''''');
    Result:=''''+Result+'''';    
  end;  
end;  

function TCustomSqliteDataset.ApplyUpdates:Boolean;
var
  CounterFields,CounterItems,StatementsCounter:Integer;
  SqlTemp,WhereKeyNameEqual,ASqlLine,TemplateStr:String;
begin
  if not UpdatesPending then
  begin
    Result:=True;
    Exit;
  end;
  Result:=False;
  if FPrimaryKeyNo <> -1 then
  begin
    StatementsCounter:=0;
    WhereKeyNameEqual:=' WHERE '+Fields[FPrimaryKeyNo].FieldName+' = ';
    {$ifdef DEBUG}
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
    for CounterItems:= 0 to FDeletedItems.Count - 1 do  
    begin
      SqlTemp:=SqlTemp+(TemplateStr+
        StrPas(PDataRecord(FDeletedItems[CounterItems])^.Row[FPrimaryKeyNo])+';');    
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'COMMIT;';
        FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
        StatementsCounter:=0;
        SqlTemp:='BEGIN;';
      end;    
    end;
    // Update changed records
    if FUpdatedItems.Count > 0 then
      TemplateStr:='UPDATE '+FTableName+' SET ';
    for CounterItems:= 0 to FUpdatedItems.Count - 1 do  
    begin
      ASqlLine:=TemplateStr;
      for CounterFields:= 0 to Fields.Count - 2 do
      begin
        ASqlLine:=ASqlLine + (Fields[CounterFields].FieldName +' = '+
          GetSqlStr((Fields[CounterFields].DataType in [ftString,ftMemo]),
            PDataRecord(FUpdatedItems[CounterItems])^.Row[CounterFields])+',');        
      end;
        ASqlLine:=ASqlLine + (Fields[Fields.Count - 1].FieldName +' = '+
          GetSqlStr((Fields[Fields.Count - 1].DataType in [ftString,ftMemo]),PDataRecord(FUpdatedItems[CounterItems])^.Row[Fields.Count - 1])+
          WhereKeyNameEqual+StrPas(PDataRecord(FUpdatedItems[CounterItems])^.Row[FPrimaryKeyNo])+';');
      SqlTemp:=SqlTemp + ASqlLine;
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'COMMIT;';
        FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
        StatementsCounter:=0;
        SqlTemp:='BEGIN;';
      end;  
    end;
    // Add new records
    // Build TemplateStr
    if FAddedItems.Count > 0 then
    begin
      TemplateStr:='INSERT INTO '+FTableName+ ' (';
      for CounterFields:= 0 to Fields.Count - 1 do
      begin
        TemplateStr:=TemplateStr + Fields[CounterFields].FieldName; 
        if CounterFields <> Fields.Count - 1 then
          TemplateStr:=TemplateStr+',';
      end; 
      TemplateStr:=TemplateStr+') VALUES (';
    end;  
    for CounterItems:= 0 to FAddedItems.Count - 1 do  
    begin
      ASqlLine:=TemplateStr;
      for CounterFields:= 0 to Fields.Count - 2 do
      begin
        ASqlLine:=ASqlLine + (GetSqlStr((Fields[CounterFields].DataType in [ftString,ftMemo]),
          PDataRecord(FAddedItems[CounterItems])^.Row[CounterFields])+',');        
      end;
      ASqlLine:=ASqlLine + (GetSqlStr((Fields[Fields.Count -1].DataType in [ftString,ftMemo]),
        PDataRecord(FAddedItems[CounterItems])^.Row[Fields.Count - 1])+');');
      SqlTemp:=SqlTemp + ASqlLine;    
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'COMMIT;';
        FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
        StatementsCounter:=0;
        SqlTemp:='BEGIN;';
      end;  
    end;  
    SqlTemp:=SqlTemp+'COMMIT;';
    {$ifdef DEBUG}
    writeln('  SQL: ',SqlTemp);
    {$endif}  
   FAddedItems.Clear;
   FUpdatedItems.Clear;
   FDeletedItems.Clear;   
   FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
   Result:= FSqliteReturnId = SQLITE_OK;
  end;  
  {$ifdef DEBUG}
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
  {$ifdef DEBUG}
  writeln('##TCustomSqliteDataset.CreateTable##');
  if ATableName = '' then
    WriteLn('  TableName Not Set');
  if FieldDefs.Count = 0 then
    WriteLn('  FieldDefs Not Initialized');
  {$endif}
  if (ATableName <> '') and (FieldDefs.Count > 0) then
  begin
    if FSqliteHandle = nil then
      GetSqliteHandle;
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
    {$ifdef DEBUG}
    writeln('  SQL: ',SqlTemp);
    {$endif}
    FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
    Result:= FSqliteReturnId = SQLITE_OK;
  end
  else
    Result:=False;
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
  FOrphanItems.Clear;
  //Reopen
  BuildLinkedList;               
  FCurrentItem:=FBeginItem;
  for i := 0 to BufferCount - 1 do
    PPDataRecord(Buffers[i])^:=FBeginItem;
  Resync([]);
end;

function TCustomSqliteDataset.TableExists: Boolean;
begin
  Result:=TableExists(FTableName);
end;

function TCustomSqliteDataset.UpdatesPending: Boolean;
begin
  //Sometimes FBeginItem is inserted in FUpdatedItems
  FUpdatedItems.Remove(FBeginItem);
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
