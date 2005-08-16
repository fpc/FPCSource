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
    FPrimaryKey: String;
    FPrimaryKeyNo: Integer;

    {$ifdef DEBUGACTIVEBUFFER}
    FFCurrentItem: PDataRecord;
    {$else}
    FCurrentItem: PDataRecord;
    {$endif}
    FBufferSize: Integer;
    FExpectedAppends: Integer;
    FExpectedDeletes: Integer;
    FExpectedUpdates: Integer;
    FSaveOnClose: Boolean;
    FSaveOnRefetch: Boolean;
    FComplexSql: Boolean;
    FUpdatedItems: TFPList;
    FAddedItems: TFPList;
    FDeletedItems: TFPList;
    FOrphanItems: TFPList;
    FMasterLink: TMasterDataLink;
    FIndexFieldNames: String;
    FIndexFieldList: TList;
    function GetIndexFields(Value: Integer): TField;
    procedure UpdateIndexFields;
  protected
    FFileName: String;
    FSql: String;
    FTableName: String;
    FAutoIncFieldNo: Integer;
    FNextAutoInc:Integer;
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
    procedure SqliteClose(AHandle: Pointer);virtual;abstract;
    function GetSqliteHandle: Pointer; virtual; abstract;
    function GetSqliteVersion: String; virtual; abstract;
    procedure BuildLinkedList; virtual; abstract;
    function SqliteReturnString: String; virtual; abstract;
    function TableExists: Boolean;virtual;abstract;
    procedure DisposeLinkedList;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetMasterFields(Value:String);
    function GetMasterFields:String;
    procedure SetMasterSource(Value: TDataSource);
    function GetMasterSource:TDataSource;
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
    function ExecSQL(const ASql:String):Integer;
    function QuickQuery(const ASql:String):String;overload;
    function QuickQuery(const ASql:String;const AStrList: TStrings):String;overload;
    function QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;virtual;abstract;overload;
    procedure RefetchData;
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
    property ComplexSql: Boolean read FComplexSql write FComplexSql;
    property ExpectedAppends: Integer read FExpectedAppends write SetExpectedAppends;
    property ExpectedUpdates: Integer read FExpectedUpdates write SetExpectedUpdates;
    property ExpectedDeletes: Integer read FExpectedDeletes write SetExpectedDeletes;
    property IndexFields[Value: Integer]: TField read GetIndexFields;
    property SqliteReturnId: Integer read FSqliteReturnId;
    property SqliteHandle: Pointer read FSqliteHandle;
    property SqliteVersion: String read GetSqliteVersion;
   published
    property IndexFieldNames: string read FIndexFieldNames write FIndexFieldNames;
    property FileName: String read FFileName write FFileName;
    property PrimaryKey: String read FPrimaryKey write FPrimaryKey;
    property SaveOnClose: Boolean read FSaveOnClose write FSaveOnClose; 
    property SaveOnRefetch: Boolean read FSaveOnRefetch write FSaveOnRefetch;
    property SQL: String read FSql write FSql;
    property TableName: String read FTableName write FTableName;   
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    
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
    property OnDeleteError;
    property OnEditError;
  end;
  
implementation

uses
  strutils;

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
  FUpdatedItems.Capacity:=20;
  FAddedItems:= TFPList.Create;
  FAddedItems.Capacity:=20;
  FOrphanItems:= TFPList.Create;
  FOrphanItems.Capacity:=20;
  FDeletedItems:= TFPList.Create;
  FDeletedItems.Capacity:=20;
  inherited Create(AOwner);
end;

function TCustomSqliteDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:= TDSStream.Create(PPDataRecord(ActiveBuffer)^,Field.FieldNo - 1);
end;  

destructor TCustomSqliteDataset.Destroy;
begin
  inherited Destroy;
  FUpdatedItems.Destroy;
  FAddedItems.Destroy;
  FDeletedItems.Destroy;
  FOrphanItems.Destroy;
  FMasterLink.Destroy;
  FIndexFieldList.Destroy;
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

function TCustomSqliteDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
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
      DatabaseError('SqliteDs - No records',Self);
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
  if FSqliteHandle <> nil then
  begin
    SqliteClose(FSqliteHandle);
    FSqliteHandle := nil;
  end;
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

procedure TCustomSqliteDataset.InternalHandleException;
begin
  //??
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
begin
  FAutoIncFieldNo:=-1;
  if not FileExists(FFileName) then
    DatabaseError('TCustomSqliteDataset - File "'+FFileName+'" not found',Self);
  if (FTablename = '') and not (FComplexSql) then
    DatabaseError('TCustomSqliteDataset - Tablename not set',Self);

  if MasterSource <> nil then
  begin
    FSql := 'Select * from '+FTableName+';'; // forced to obtain all fields
    FMasterLink.FieldNames:=MasterFields; //this should fill MasterLinks.Fields
    //todo: ignore if Fields.Count = 0 (OnMasterChanged will not be called) or
    // raise a error?
    //if (FMasterLink.Fields.Count = 0) and (MasterSource.DataSet.Active) then
    //   DatabaseError('Master Fields are not defined correctly');
  end;
  
  FSqliteHandle:=GetSqliteHandle;
  if FSql = '' then
    FSql := 'Select * from '+FTableName+';';
  InternalInitFieldDefs;

  if DefaultFields then 
    CreateFields;

  BindFields(True);

  UpdateIndexFields;
  if FMasterLink.Active and (FIndexFieldList.Count <> FMasterLink.Fields.Count) then
    DatabaseError('MasterFields count doesnt match IndexFields count',Self);

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

procedure TCustomSqliteDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  //The BookMarkData is the Buffer itsef;
end;

procedure TCustomSqliteDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PPDataRecord(Buffer)^^.BookmarkFlag := Value;
end;

procedure TCustomSqliteDataset.SetExpectedAppends(AValue:Integer);
begin
  if Assigned(FAddedItems) then
    FAddedItems.Capacity:=AValue;
end;  

procedure TCustomSqliteDataset.SetExpectedUpdates(AValue:Integer);
begin
  if Assigned(FUpdatedItems) then
    FUpdatedItems.Capacity:=AValue;
end;  

procedure TCustomSqliteDataset.SetExpectedDeletes(AValue:Integer);
begin
  if Assigned(FDeletedItems) then
    FDeletedItems.Capacity:=AValue;
  if Assigned(FOrphanItems) then
    FOrphanItems.Capacity:=AValue;  
end;  

procedure TCustomSqliteDataset.SetFieldData(Field: TField; Buffer: Pointer);
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
  end//if
  else
    ActiveItem^.Row[Pred(Field.FieldNo)]:=nil;        
end;

procedure TCustomSqliteDataset.SetRecNo(Value: Integer);
var
  Counter:Integer;
  TempItem:PDataRecord;
begin
  if (Value >= FRecordCount) or (Value < 0) then
    DatabaseError('SqliteDs - Record Number Out Of Range',Self);
  TempItem:=FBeginItem;
  for Counter := 0 to Value do
    TempItem:=TempItem^.Next;
  PPDataRecord(ActiveBuffer)^:=TempItem;   
end;

// Specific functions 

procedure TCustomSqliteDataset.MasterChanged(Sender: TObject);
  function GetSqlStr(AField:TField):String;
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
  AFilter:=' where ';
  for i:= 0 to FMasterLink.Fields.Count - 1 do
  begin
    AFilter:=AFilter + IndexFields[i].FieldName +' = '+ GetSqlStr(TField(FMasterLink.Fields[i]));
    if i <> FMasterLink.Fields.Count - 1 then
      AFilter:= AFilter + ' and ';
  end;
  FSql:='Select * from '+FTableName+AFilter;
  {$ifdef DEBUG}
  writeln('Sql used to filter detail dataset:');
  writeln(FSql);
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
  if Active then
    DatabaseError('It''s not allowed to set MasterFields property in a open dataset',Self);
  FMasterLink.FieldNames:=Value;
end;

function TCustomSqliteDataset.GetMasterFields: String;
begin
  Result:=FMasterLink.FieldNames;
end;


procedure TCustomSqliteDataset.UpdateIndexFields;
begin
  if FIndexFieldNames <> '' then
  begin
    FIndexFieldList.Clear;
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

procedure TCustomSqliteDataset.SetMasterSource(Value: TDataSource);
begin
  FMasterLink.DataSource := Value;
end;


function TCustomSqliteDataset.ExecSQL(const ASql:String):Integer;
var
  AHandle: Pointer;
begin
  Result:=0;
  //Todo check if Filename exists
  if FSqliteHandle <> nil then
    AHandle:=FSqliteHandle
  else 
    if FFileName <> '' then  
      AHandle := GetSqliteHandle
    else
      DatabaseError ('ExecSql - FileName not set',Self);    
  FSqliteReturnId:= SqliteExec(AHandle,PChar(ASql));
  //todo: add a way to get the num of changes
  //Result:=sqlite_changes(AHandle);
  if AHandle <> FSqliteHandle then
    SqliteClose(AHandle);
end;    

function TCustomSqliteDataset.ExecSQL:Integer;
begin
  Result:=ExecSQL(FSql);  
end;

function TCustomSqliteDataset.ApplyUpdates:Boolean;
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
    // In some situations (LCL apps) FBeginItems is inserted in FUpdatedItems
    FUpdatedItems.Remove(FBeginItem);
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
        FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
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
        FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
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
        FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
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
   FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
   Result:= FSqliteReturnId = SQLITE_OK;
  end;  
  {$ifdef DEBUG}
    writeln('ApplyUpdates Result: ',Result);
  {$endif}   
end;    

function TCustomSqliteDataset.CreateTable: Boolean;
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
    FSqliteHandle:= GetSqliteHandle;
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
    FSqliteReturnId:=SqliteExec(FSqliteHandle,PChar(SqlTemp));
    Result:= FSqliteReturnId = SQLITE_OK;
    SqliteClose(FSqliteHandle);
    FSqliteHandle:=nil;
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


function TCustomSqliteDataset.UpdatesPending: Boolean;
begin
  Result:= (FDeletedItems.Count > 0) or
    (FAddedItems.Count > 0) or (FUpdatedItems.Count > 0);
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
