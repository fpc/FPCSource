unit sqliteds;

{
    This is SqliteDS/TSqliteDataset, a TDataset descendant class for use with fpc compiler
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

uses Classes, SysUtils, Db
  {$ifdef DEBUG}
  ,Crt
  {$endif}
  ;

type
  PDataRecord = ^DataRecord;
  PPDataRecord = ^PDataRecord;
  DataRecord = record
    Row: PPchar;
    BookmarkData: Pointer;
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
  
  { TSqliteDataset }

  TSqliteDataset = class(TDataSet)
  private
    FFileName: String;
    FSql: String;
    FTableName: String;
    FPrimaryKey: String;
    FPrimaryKeyNo: Integer;
    FAutoIncFieldNo: Integer;
    FNextAutoInc:Integer;
    {$ifdef DEBUGACTIVEBUFFER}
    FFCurrentItem: PDataRecord;
    {$else}
    FCurrentItem: PDataRecord;
    {$endif}
    FBeginItem: PDataRecord;
    FEndItem: PDataRecord;
    FCacheItem: PDataRecord;
    FBufferSize: Integer;
    FRowBufferSize: Integer;
    FRowCount: Integer;
    FRecordCount: Integer;
    FExpectedAppends: Integer;
    FExpectedDeletes: Integer;
    FExpectedUpdates: Integer;
    FSqliteReturnId: Integer;
    FDataAllocated: Boolean;
    FSaveOnClose: Boolean;
    FSaveOnRefetch: Boolean;
    FComplexSql: Boolean;
    FSqliteHandle: Pointer;
    FUpdatedItems: TList;
    FAddedItems: TList;
    FDeletedItems: TList;
    FOrphanItems: TList;
  protected
    procedure DisposeLinkedList;
    procedure BuildLinkedList; virtual;
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
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
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
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    // Additional procedures
    function ApplyUpdates: Boolean; virtual;
    function CreateTable: Boolean; virtual;
    function ExecSQL:Integer;
    function ExecSQL(ASql:String):Integer;
    function TableExists: Boolean;
    procedure RefetchData;
    function SqliteReturnString: String;
    function UpdatesPending: Boolean;
    {$ifdef DEBUGACTIVEBUFFER}
    procedure SetCurrentItem(Value:PDataRecord);
    property FCurrentItem: PDataRecord read FFCurrentItem write SetCurrentItem;
    {$endif}
    {$ifdef USE_SQLITEDS_INTERNALS}
    property BeginItem: PDataRecord read FBeginItem;
    property EndItem: PDataRecord read FEndItem;
    property UpdatedItems: TList read FUpdatedItems;
    property AddedItems: TList read FAddedItems;
    property DeletedItems: TList read FDeletedItems;
    {$endif}
    property ComplexSql: Boolean read FComplexSql write FComplexSql;
    property ExpectedAppends: Integer read FExpectedAppends write SetExpectedAppends;
    property ExpectedUpdates: Integer read FExpectedUpdates write SetExpectedUpdates;
    property ExpectedDeletes: Integer read FExpectedDeletes write SetExpectedDeletes;
    property SqliteReturnId: Integer read FSqliteReturnId;
   published 
    property FileName: String read FFileName write FFileName;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property SaveOnClose: Boolean read FSaveOnClose write FSaveOnClose; 
    property SaveOnRefetch: Boolean read FSaveOnRefetch write FSaveOnRefetch;
    property SQL: String read FSql write FSql;
    property TableName: String read FTableName write FTableName;   
    //property Active;
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
    property OnDeleteError;
    property OnEditError;
  end;
  
  procedure Register;
  
implementation

uses SQLite,strutils;

function GetAutoIncValue(NextValue: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPChar): integer; cdecl;
var
  CodeError, TempInt: Integer;
begin
  TempInt:=-1;
  if ColumnValues[0] <> nil then
  begin
    Val(StrPas(ColumnValues[0]),TempInt,CodeError);  
    if CodeError <> 0 then
      DatabaseError('SqliteDs - Error trying to get last autoinc value');
  end;  
  Integer(NextValue^):=Succ(TempInt);
  Result:=1;
end;  

function GetFieldDefs(TheDataset: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPChar): integer; cdecl;
var
  FieldSize:Word;
  Counter:Integer;
  AType:TFieldType;
  ColumnStr:String;
begin
 // Sqlite is typeless (allows any type in any field) 
 // regardless of what is in Create Table, but returns 
 // exactly what is in Create Table statement
 // here is a trick to get the datatype.
 // If the field contains another type, there will be problems  
 For Counter:= 0 to Columns - 1 do
 begin
   ColumnStr:= UpCase(StrPas(ColumnNames[Counter + Columns]));
   If (ColumnStr = 'INTEGER') then  
   begin  
     AType:= ftInteger;
     FieldSize:=SizeOf(Integer); 
   end else if (ColumnStr = 'BOOLEAN') then
   begin
     AType:= ftBoolean;
     FieldSize:=SizeOf(Boolean);
   end else if (ColumnStr = 'FLOAT') then
   begin
     AType:= ftFloat;
     FieldSize:=SizeOf(Double);
   end else if (ColumnStr = 'WORD') then
   begin
     AType:= ftWord;
     FieldSize:=SizeOf(Word);
   end else if (ColumnStr = 'DATETIME') then
   begin
     AType:= ftDateTime;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'DATE') then
   begin
     AType:= ftDate;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'TIME') then
   begin
     AType:= ftTime;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'MEMO') then
   begin
     AType:= ftMemo;
     FieldSize:=10;//??  
   end else if (ColumnStr = 'AUTOINC') then
   begin
     AType:= ftAutoInc;
     FieldSize:=SizeOf(Integer);
     if TSqliteDataset(TheDataset).FAutoIncFieldNo = -1 then
       TSqliteDataset(TheDataset).FAutoIncFieldNo:= Counter;
   end else 
   begin
     AType:= ftString;
     FieldSize:=10; //??
   end;   
   TDataset(TheDataset).FieldDefs.Add(StrPas(ColumnNames[Counter]), AType, FieldSize, False);    
 end;
 result:=-1;
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
  //Todo: see how TDbMemo read/write to field and choose best if order
  if FPosition = 0 then
  begin
    NewRow:=StrAlloc(Count+1);
    (NewRow+Count)^:=#0;
    Move(Buffer,NewRow^,Count);    
  end
  else
  begin
    NewRow:=StrAlloc(FRowSize+Count+1);
    (NewRow+Count+FRowSize)^:=#0;
    Move(FFieldRow^,NewRow^,FRowSize);
    Move(Buffer,(NewRow+FRowSize)^,Count);
  end;
  FActiveItem^.Row[FFieldIndex]:=NewRow;    
  StrDispose(FFieldRow);
  FFieldRow:=NewRow;
  FRowSize:=StrLen(NewRow);
  Inc(FPosition,Count);
  {$ifdef DEBUG}
  WriteLn('Writing a BlobStream');
  WriteLn('Stream.Size: ',StrLen(NewRow));
  WriteLn('Stream Value: ',NewRow);
  WriteLn('FPosition:',FPosition);
  {$endif}
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
  WriteLn('Reading a BlobStream');
  WriteLn('Bytes requested: ',Count);
  WriteLn('Bytes Moved: ',BytesToMove);
  WriteLn('Stream.Size: ',FRowSize);
  WriteLn('Stream Value: ',FFieldRow);
  {$endif}
end; 
 
// TSqliteDataset override methods

function TSqliteDataset.AllocRecordBuffer: PChar;
var
  APointer:Pointer;
begin
  APointer := AllocMem(FBufferSize);
  PDataRecord(APointer^):=FBeginItem;
  Result:=APointer;
end;

procedure TSqliteDataset.BuildLinkedList;
var
  TempItem:PDataRecord;
  vm:Pointer;
  ColumnNames,ColumnValues:PPChar;
  Counter:Integer;
begin
  //Get AutoInc Field initial value
  if FAutoIncFieldNo <> -1 then
    sqlite_exec(FSqliteHandle,PChar('Select Max('+Fields[FAutoIncFieldNo].FieldName+') from ' + FTableName),
      @GetAutoIncValue,@FNextAutoInc,nil);  
  
  FSqliteReturnId:=sqlite_compile(FSqliteHandle,Pchar(FSql),nil,@vm,nil);  
  if FSqliteReturnId <> SQLITE_OK then
  case FSqliteReturnId of
  SQLITE_ERROR:
    DatabaseError('Invalid SQL',Self);
  else
    DatabaseError('Error returned by sqlite while retrieving data: '+SqliteReturnString,Self);
  end;  
  
  FDataAllocated:=True;
  New(FBeginItem);
  FBeginItem^.Next:=nil;
  FBeginItem^.Previous:=nil;
  FBeginItem^.BookMarkFlag:=bfBOF;
  TempItem:=FBeginItem;
  FRecordCount:=0; 
  FSqliteReturnId:=sqlite_step(vm,@FRowCount,@ColumnValues,@ColumnNames);
  while FSqliteReturnId = SQLITE_ROW do
  begin
    Inc(FRecordCount);
    New(TempItem^.Next);
    TempItem^.Next^.Previous:=TempItem;
    TempItem:=TempItem^.Next;
    GetMem(TempItem^.Row,FRowBufferSize);
    For Counter := 0 to FRowCount - 1 do
      TempItem^.Row[Counter]:=StrNew(ColumnValues[Counter]);     
    FSqliteReturnId:=sqlite_step(vm,@FRowCount,@ColumnValues,@ColumnNames);  
  end;
  sqlite_finalize(vm, nil); 
  // Init EndItem        
  if FRecordCount <> 0 then 
  begin
    New(TempItem^.Next);
    TempItem^.Next^.Previous:=TempItem;
    FEndItem:=TempItem^.Next;    
  end  
  else
  begin
    New(FEndItem);
    FEndItem^.Previous:=FBeginItem;    
    FBeginItem^.Next:=FEndItem;
  end;   
  FEndItem^.Next:=nil;
  // Alloc item used in append/insert 
  New(FCacheItem);
  GetMem(FCacheItem^.Row,FRowBufferSize);
  For Counter := 0 to FRowCount - 1 do
    FCacheItem^.Row[Counter]:=nil;
  // Fill FBeginItem.Row with nil -> necessary for avoid exceptions in empty datasets       
  GetMem(FBeginItem^.Row,FRowBufferSize);
  For Counter := 0 to FRowCount - 1 do
    FBeginItem^.Row[Counter]:=nil;
end;

constructor TSqliteDataset.Create(AOwner: TComponent);
begin
  BookmarkSize := SizeOf(Pointer);
  FBufferSize := SizeOf(PPDataRecord);
  FUpdatedItems:= TList.Create;
  FUpdatedItems.Capacity:=20;
  FAddedItems:= TList.Create;
  FAddedItems.Capacity:=20;
  FOrphanItems:= TList.Create;
  FOrphanItems.Capacity:=20;
  FDeletedItems:= TList.Create;
  FDeletedItems.Capacity:=20;
  inherited Create(AOwner);
end;

function TSqliteDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:= TDSStream.Create(PPDataRecord(ActiveBuffer)^,Field.FieldNo - 1);
end;  

destructor TSqliteDataset.Destroy;
begin
  inherited Destroy;
  FUpdatedItems.Destroy;
  FAddedItems.Destroy;
  FDeletedItems.Destroy;
  FOrphanItems.Destroy;
end;

procedure TSqliteDataset.DisposeLinkedList;
var
  TempItem:PDataRecord;
  Counter,I:Integer;
begin
  //Todo: insert debug info
  FDataAllocated:=False;
  TempItem:=FBeginItem^.Next;
  if TempItem <> nil then
    while TempItem^.Next <> nil do
    begin
      for Counter:= 0 to FRowCount - 1 do
        StrDispose(TempItem^.Row[Counter]);  
      FreeMem(TempItem^.Row,FRowBufferSize);
      TempItem:=TempItem^.Next;
      Dispose(TempItem^.Previous);
    end; 
  
  //Dispose FBeginItem
  FreeMem(FBeginItem^.Row,FRowBufferSize);
  Dispose(FBeginItem);
    
  //Dispose cache item
  for Counter:= 0 to FRowCount - 1 do
    StrDispose(FCacheItem^.Row[Counter]);
  FreeMem(FCacheItem^.Row,FRowBufferSize);
  Dispose(FCacheItem);
  
  // Free last item (FEndItem)
  Dispose(TempItem);  
  
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

procedure TSqliteDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;

procedure TSqliteDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Pointer(Data^) := PPDataRecord(Buffer)^^.BookmarkData;
end;

function TSqliteDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PPDataRecord(Buffer)^^.BookmarkFlag;
end;

function TSqliteDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
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
    ftInteger,ftBoolean,ftWord,ftAutoInc:
      begin
        Val(StrPas(FieldRow),LongInt(Buffer^),ValError);
        Result:= ValError = 0;  
      end;
    ftFloat,ftDateTime,ftTime,ftDate:
      begin
        Val(StrPas(FieldRow),Double(Buffer^),ValError);
        Result:= ValError = 0; 
      end;    
    end;
  end;        
end;

function TSqliteDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
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
    with FCurrentItem^ do
    begin
      BookmarkData := FCurrentItem;
      BookmarkFlag := bfCurrent;
    end;
  end
    else if (Result = grError) and DoCheck then
      DatabaseError('SqliteDs - No records',Self);
end;

function TSqliteDataset.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

function TSqliteDataset.GetRecNo: Integer;
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

function TSqliteDataset.GetRecordSize: Word;
begin
  Result := FBufferSize; //??
end;

procedure TSqliteDataset.InternalAddRecord(Buffer: Pointer; DoAppend: Boolean);
var
  NewItem: PDataRecord;
  Counter:Integer;
begin
  //Todo: implement insert ??
  if PPDataRecord(Buffer)^ <> FCacheItem then
    DatabaseError('PPDataRecord(Buffer) <> FCacheItem - Problem',Self);
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

procedure TSqliteDataset.InternalClose;
begin
  if FSaveOnClose then
    ApplyUpdates;  
  //BindFields(False);  
  if DefaultFields then
    DestroyFields;
  if FDataAllocated then
    DisposeLinkedList;  
  if FSqliteHandle <> nil then
  begin
    sqlite_close(FSqliteHandle);
    FSqliteHandle := nil;
  end;
  FAddedItems.Clear;
  FUpdatedItems.Clear;
  FDeletedItems.Clear;
  FOrphanItems.Clear;
  FRecordCount:=0;
end;

procedure TSqliteDataset.InternalDelete;
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

procedure TSqliteDataset.InternalFirst;
begin
  FCurrentItem := FBeginItem;
end;

procedure TSqliteDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentItem := PDataRecord(ABookmark^);
end;

procedure TSqliteDataset.InternalHandleException;
begin
  //??
end;

procedure TSqliteDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  sqlite_exec(FSqliteHandle,PChar('PRAGMA empty_result_callbacks = ON;PRAGMA show_datatypes = ON;'),nil,nil,nil);
  FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(FSql),@GetFieldDefs,Self,nil);
  {
  if FSqliteReturnId <> SQLITE_ABORT then
     DatabaseError(SqliteReturnString,Self);
  }
  FRowBufferSize:=(SizeOf(PPChar)*FieldDefs.Count);
end;


procedure TSqliteDataset.InternalInitRecord(Buffer: PChar);
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

procedure TSqliteDataset.InternalLast;
begin
  FCurrentItem := FEndItem;
end;

procedure TSqliteDataset.InternalOpen;
begin
  FAutoIncFieldNo:=-1;
  if not FileExists(FFileName) then
    DatabaseError('TSqliteDataset - File '+FFileName+' not found');
  if (FTablename = '') and not (FComplexSql) then
    DatabaseError('TSqliteDataset - Tablename not set');  
  FSqliteHandle:=sqlite_open(PChar(FFileName),0,nil);
  if FSql = '' then
    FSql := 'Select * from '+FTableName+';';
  InternalInitFieldDefs;
  if DefaultFields then 
    CreateFields;
  BindFields(True);
  // Get PrimaryKeyNo if available
  if Fields.FindField(FPrimaryKey) <> nil then
    FPrimaryKeyNo:=Fields.FindField(FPrimaryKey).FieldNo - 1  
  else
    FPrimaryKeyNo:=FAutoIncFieldNo; // -1 if there's no AutoIncField 
       
  BuildLinkedList;               
  FCurrentItem:=FBeginItem;  
end;

procedure TSqliteDataset.InternalPost;
begin
  if (State<>dsEdit) then 
    InternalAddRecord(ActiveBuffer,True);
end;

procedure TSqliteDataset.InternalSetToRecord(Buffer: PChar);
begin
  //Todo: see why only under linux InternalSetToRecord is called with FCacheItem as parameter
  if PPDataRecord(Buffer)^ <> FCacheItem then
    FCurrentItem:=PPDataRecord(Buffer)^;
end;

function TSqliteDataset.IsCursorOpen: Boolean;
begin
   Result := FDataAllocated;
end;

procedure TSqliteDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PPDataRecord(Buffer)^^.BookmarkData := Pointer(Data^);
end;

procedure TSqliteDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PPDataRecord(Buffer)^^.BookmarkFlag := Value;
end;

procedure TSqliteDataset.SetExpectedAppends(AValue:Integer);
begin
  if Assigned(FAddedItems) then
    FAddedItems.Capacity:=AValue;
end;  

procedure TSqliteDataset.SetExpectedUpdates(AValue:Integer);
begin
  if Assigned(FUpdatedItems) then
    FUpdatedItems.Capacity:=AValue;
end;  

procedure TSqliteDataset.SetExpectedDeletes(AValue:Integer);
begin
  if Assigned(FDeletedItems) then
    FDeletedItems.Capacity:=AValue;
end;  

procedure TSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  TempStr:String;
  ActiveItem:PDataRecord;
begin
  ActiveItem:=PPDataRecord(ActiveBuffer)^;
  if (ActiveItem <> FCacheItem) and (FUpdatedItems.IndexOf(ActiveItem) = -1) and (FAddedItems.IndexOf(ActiveItem) = -1) then
    FUpdatedItems.Add(ActiveItem);
  if Buffer = nil then
    ActiveItem^.Row[Pred(Field.FieldNo)]:=nil
  else
    begin
      StrDispose(ActiveItem^.Row[Pred(Field.FieldNo)]);
      case Field.Datatype of
      ftString:
        begin            
          ActiveItem^.Row[Pred(Field.FieldNo)]:=StrNew(PChar(Buffer));
        end;
      ftInteger,ftBoolean,ftWord:
        begin          
          Str(LongInt(Buffer^),TempStr);  
          ActiveItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
          StrPCopy(ActiveItem^.Row[Pred(Field.FieldNo)],TempStr);
        end;
      ftFloat,ftDateTime,ftDate,ftTime:
        begin
          Str(Double(Buffer^),TempStr);  
          ActiveItem^.Row[Pred(Field.FieldNo)]:=StrAlloc(Length(TempStr)+1);
          StrPCopy(ActiveItem^.Row[Pred(Field.FieldNo)],TempStr);
        end;    
      end;// case
    end;//if        
end;

procedure TSqliteDataset.SetRecNo(Value: Integer);
var
  Counter:Integer;
  TempItem:PDataRecord;
begin
  if (Value >= FRecordCount) or (Value < 0) then
    DatabaseError('SqliteDs - Record Number Out Of Range');
  TempItem:=FBeginItem;
  for Counter := 0 to Value do
    TempItem:=TempItem^.Next;
  PPDataRecord(ActiveBuffer)^:=TempItem;   
end;

// Specific functions 

function TSqliteDataset.ExecSQL(ASql:String):Integer;
var
  AHandle: Pointer;
begin
  Result:=0;
  //Todo check if Filename exists
  if FSqliteHandle <> nil then
    AHandle:=FSqliteHandle
  else 
    if FFileName <> '' then  
      AHandle := sqlite_open(PChar(FFilename),0,nil)
    else
      DatabaseError ('ExecSql - FileName not set');    
  FSqliteReturnId:= sqlite_exec(AHandle,PChar(ASql),nil,nil,nil);
  Result:=sqlite_changes(AHandle);     
  if AHandle <> FSqliteHandle then
    sqlite_close(AHandle);
end;    

function TSqliteDataset.ExecSQL:Integer;
begin
  Result:=ExecSQL(FSql);  
end;

function TSqliteDataset.ApplyUpdates:Boolean;
var
  CounterFields,CounterItems,StatementsCounter:Integer;
  SqlTemp,KeyName,ASqlLine,TemplateStr:String;
begin
  Result:=False;
  if (FPrimaryKeyNo <> -1) and not FComplexSql then
  begin
    StatementsCounter:=0;
    KeyName:=Fields[FPrimaryKeyNo].FieldName;
    {$ifdef DEBUG}
    WriteLn('ApplyUpdates called');
    if FPrimaryKeyNo = FAutoIncFieldNo then
      WriteLn('Using an AutoInc field as primary key');
    WriteLn('PrimaryKey: ',KeyName);
    WriteLn('PrimaryKeyNo: ',FPrimaryKeyNo);
    {$endif}
    SqlTemp:='BEGIN TRANSACTION;';
    // Update changed records
    if FUpdatedItems.Count > 0 then
      TemplateStr:='UPDATE '+FTableName+' SET ';
    for CounterItems:= 0 to FUpdatedItems.Count - 1 do  
    begin
      ASqlLine:=TemplateStr;
      for CounterFields:= 0 to Fields.Count - 1 do
      begin
        if PDataRecord(FUpdatedItems[CounterItems])^.Row[CounterFields] <> nil then
        begin
          ASqlLine:=ASqlLine + Fields[CounterFields].FieldName +' = ';
          if not (Fields[CounterFields].DataType in [ftString,ftMemo]) then
            ASqlLine:=ASqlLine+StrPas(PDataRecord(FUpdatedItems[CounterItems])^.Row[CounterFields])+ ','
          else
            ASqlLine:=ASqlLine+''''+
              AnsiReplaceStr(StrPas(PDataRecord(FUpdatedItems[CounterItems])^.Row[CounterFields]),'''','''''')+''',';  
        end
        else
          ASqlLine:=ASqlLine + Fields[CounterFields].FieldName +' = NULL,';  
      end;
      //Todo: see if system.delete trunks AnsiString
      system.delete(ASqlLine,Length(ASqlLine),1);
      SqlTemp:=SqlTemp + ASqlLine+' WHERE '+KeyName+' = '+StrPas(PDataRecord(FUpdatedItems[CounterItems])^.Row[FPrimaryKeyNo])+';';
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'END TRANSACTION;';
        FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(SqlTemp),nil,nil,nil);
        StatementsCounter:=0;
        SqlTemp:='BEGIN TRANSACTION;';
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
      for CounterFields:= 0 to Fields.Count - 1 do
      begin
        if PDataRecord(FAddedItems[CounterItems])^.Row[CounterFields] <> nil then
        begin
          if not (Fields[CounterFields].DataType in [ftString,ftMemo]) then
            ASqlLine:=ASqlLine+StrPas(PDataRecord(FAddedItems[CounterItems])^.Row[CounterFields])
          else
            ASqlLine:=ASqlLine+''''+
              AnsiReplaceStr(StrPas(PDataRecord(FAddedItems[CounterItems])^.Row[CounterFields]),'''','''''')+'''';  
        end
        else
          ASqlLine:=ASqlLine + 'NULL';
        //Todo: see if delete ASqline is faster
        if CounterFields <> Fields.Count - 1 then
          ASqlLine:=ASqlLine+',';
      end;
      SqlTemp:=SqlTemp+ASqlLine+');';    
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'END TRANSACTION;';
        FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(SqlTemp),nil,nil,nil);
        StatementsCounter:=0;
        SqlTemp:='BEGIN TRANSACTION;';
      end;  
    end;  
    // Delete Items
    if FDeletedItems.Count > 0 then
      TemplateStr:='DELETE FROM '+FTableName+ ' WHERE '+KeyName+' = ';
    for CounterItems:= 0 to FDeletedItems.Count - 1 do  
    begin
      SqlTemp:=SqlTemp+TemplateStr+
        StrPas(PDataRecord(FDeletedItems[CounterItems])^.Row[FPrimaryKeyNo])+';';    
      inc(StatementsCounter);
      //ApplyUpdates each 400 statements
      if StatementsCounter = 400 then
      begin
        SqlTemp:=SqlTemp+'END TRANSACTION;';
        FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(SqlTemp),nil,nil,nil);
        StatementsCounter:=0;
        SqlTemp:='BEGIN TRANSACTION;';
      end;    
    end;
    SqlTemp:=SqlTemp+'END TRANSACTION;';
    {$ifdef DEBUG}
    writeln('ApplyUpdates Sql: ',SqlTemp);
    {$endif}  
   FAddedItems.Clear;
   FUpdatedItems.Clear;
   FDeletedItems.Clear;   
   FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(SqlTemp),nil,nil,nil);
   Result:= FSqliteReturnId = SQLITE_OK;
  end;  
  {$ifdef DEBUG}
    writeln('ApplyUpdates Result: ',Result);
  {$endif}   
end;    

function TSqliteDataset.CreateTable: Boolean;
var
  SqlTemp:String;
  Counter:Integer;
begin
  {$ifdef DEBUG}
  if FTableName = '' then
    WriteLn('CreateTable : TableName Not Set');
  if FieldDefs.Count = 0 then 
    WriteLn('CreateTable : FieldDefs Not Initialized');
  {$endif}
  if (FTableName <> '') and (FieldDefs.Count > 0) then
  begin
    FSqliteHandle:= sqlite_open(PChar(FFileName),0,nil);
    SqlTemp:='CREATE TABLE '+FTableName+' (';
    for Counter := 0 to FieldDefs.Count-1 do
    begin
      SqlTemp:=SqlTemp + FieldDefs[Counter].Name;
      case FieldDefs[Counter].DataType of
        ftInteger:
          SqlTemp:=SqlTemp + ' INTEGER';
        ftString:
          SqlTemp:=SqlTemp + ' VARCHAR';
        ftBoolean:
          SqlTemp:=SqlTemp + ' BOOLEAN';
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
        ftAutoInc:
          SqlTemp:=SqlTemp + ' AUTOINC'; 
        ftMemo:
          SqlTemp:=SqlTemp + ' MEMO';   
      else
        SqlTemp:=SqlTemp + ' VARCHAR';    
      end;
      if Counter <>  FieldDefs.Count - 1 then
        SqlTemp:=SqlTemp+ ' , ';   
    end;
    SqlTemp:=SqlTemp+');';
    {$ifdef DEBUG}
    writeln('CreateTable Sql: ',SqlTemp);
    {$endif}  
    FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(SqlTemp),nil,nil,nil);   
    Result:= FSqliteReturnId = SQLITE_OK;
    sqlite_close(FSqliteHandle);
  end
  else
    Result:=False;  
end; 

function TSqliteDataset.TableExists: Boolean;
var
  AHandle,vm:Pointer;
  ColumnNames,ColumnValues:PPChar;
  AInt:Integer;
begin
  Result:=False;
  if not (FTableName = '') and FileExists(FFileName) then
  begin
    if FSqliteHandle = nil then
    begin
      {$ifdef DEBUG}
      writeln('TableExists - FSqliteHandle=nil : Opening a file');
      {$endif}
      AHandle:=sqlite_open(PChar(FFileName),0,nil);
    end  
    else
    begin
      {$ifdef DEBUG}
      writeln('TableExists - FSqliteHandle<>nil : Using FSqliteHandle');
      {$endif}
      AHandle:=FSqliteHandle;
    end;
    FSqliteReturnId:=sqlite_compile(AHandle,
      Pchar('SELECT name FROM SQLITE_MASTER WHERE type = ''table'' AND name LIKE '''+ FTableName+ ''';'),
      nil,@vm,nil);
    {$ifdef DEBUG}
    WriteLn('TableExists.sqlite_compile - SqliteReturnString:',SqliteReturnString);  
    {$endif}
    FSqliteReturnId:=sqlite_step(vm,@AInt,@ColumnValues,@ColumnNames);
    {$ifdef DEBUG}
    WriteLn('TableExists.sqlite_step - SqliteReturnString:',SqliteReturnString);  
    {$endif}
    Result:=FSqliteReturnId = SQLITE_ROW;
    sqlite_finalize(vm, nil); 
    if (FSqliteHandle = nil) then         
      sqlite_close(AHandle);
  end;
  {$ifdef DEBUG}
  WriteLn('TableExists ('+FTableName+') Result:',Result);
  {$endif}  
end;  

procedure TSqliteDataset.RefetchData;
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
  FRecordCount:=0;  
  //Reopen
  BuildLinkedList;               
  FCurrentItem:=FBeginItem;
  Resync([]);
end;  

function TSqliteDataset.SqliteReturnString: String;
begin
 case FSqliteReturnId of
      SQLITE_OK           : Result := 'SQLITE_OK          '; 
      SQLITE_ERROR        : Result := 'SQLITE_ERROR       ';
      SQLITE_INTERNAL     : Result := 'SQLITE_INTERNAL    '; 
      SQLITE_PERM         : Result := 'SQLITE_PERM        '; 
      SQLITE_ABORT        : Result := 'SQLITE_ABORT       '; 
      SQLITE_BUSY         : Result := 'SQLITE_BUSY        '; 
      SQLITE_LOCKED       : Result := 'SQLITE_LOCKED      '; 
      SQLITE_NOMEM        : Result := 'SQLITE_NOMEM       '; 
      SQLITE_READONLY     : Result := 'SQLITE_READONLY    '; 
      SQLITE_INTERRUPT    : Result := 'SQLITE_INTERRUPT   '; 
      SQLITE_IOERR        : Result := 'SQLITE_IOERR       '; 
      SQLITE_CORRUPT      : Result := 'SQLITE_CORRUPT     '; 
      SQLITE_NOTFOUND     : Result := 'SQLITE_NOTFOUND    '; 
      SQLITE_FULL         : Result := 'SQLITE_FULL        '; 
      SQLITE_CANTOPEN     : Result := 'SQLITE_CANTOPEN    '; 
      SQLITE_PROTOCOL     : Result := 'SQLITE_PROTOCOL    '; 
      SQLITE_EMPTY        : Result := 'SQLITE_EMPTY       '; 
      SQLITE_SCHEMA       : Result := 'SQLITE_SCHEMA      '; 
      SQLITE_TOOBIG       : Result := 'SQLITE_TOOBIG      '; 
      SQLITE_CONSTRAINT   : Result := 'SQLITE_CONSTRAINT  '; 
      SQLITE_MISMATCH     : Result := 'SQLITE_MISMATCH    '; 
      SQLITE_MISUSE       : Result := 'SQLITE_MISUSE      '; 
      SQLITE_NOLFS        : Result := 'SQLITE_NOLFS       '; 
      SQLITE_AUTH         : Result := 'SQLITE_AUTH        '; 
      SQLITE_FORMAT       : Result := 'SQLITE_FORMAT      '; 
     // SQLITE_RANGE        : Result := 'SQLITE_RANGE       '; 
      SQLITE_ROW          : Result := 'SQLITE_ROW         '; 
      SQLITE_DONE         : Result := 'SQLITE_DONE        '; 
  else
    Result:='Unknow Return Value';    
 end;
end;

function TSqliteDataset.UpdatesPending: Boolean;
begin
  Result:= (FDeletedItems.Count > 0) or
    (FAddedItems.Count > 0) or (FUpdatedItems.Count > 0);
end;
 
procedure Register;
begin
  RegisterComponents('Data Access', [TSqliteDataset]);
end;

{$ifdef DEBUGACTIVEBUFFER}
procedure TSqliteDataset.SetCurrentItem(Value:PDataRecord);
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
