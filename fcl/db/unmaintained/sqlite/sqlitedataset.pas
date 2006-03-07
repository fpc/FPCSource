{$mode objfpc}
{$h+}
unit SQLiteDataset;
{
Improved class sqLite,copyright(c) 2002-2003 Marcin Krzetowski
metal4@box43.gnet.pl
http://www.a-i.prv.pl
simple class interface for SQLite. Hacked in by Ben Hochstrasser (bhoc@surfeu.ch)
Thanks to Roger Reghin (RReghin@scelectric.ca) for his idea to ValueList.

}

interface

uses
  Classes,db,sysutils,Contnrs;

type
  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Index: Integer;
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;

type
        pBinBookMark = ^tBinBookMark;
        tBinBookmark = record
        RecPtr : Int64;
end;


type
  TSQLiteExecCallback = function(Sender: TObject; Columns: Integer; ColumnValues: Pointer; ColumnNames: Pointer): integer of object; cdecl;
  TSQLiteBusyCallback = function(Sender: TObject; ObjectName: PChar; BusyCount: integer): integer of object; cdecl;
  TOnData = Procedure(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: String) of object;
  TOnBusy = Procedure(Sender: TObject; ObjectName: String; BusyCount: integer; var Cancel: Boolean) of object;
  TOnQueryComplete = Procedure(Sender: TObject) of object;


Type
        tSqliteField = class(tObject)
protected
        FOwner : tObject;
        data : string;
        fFieldKind: tFieldKind;
        fFieldType: tFieldType;
{        tIntegerType : Integer;
        tLongIntegerType : int64;
        tDateTimeType : tDateTime;}
//        procedure SetName(const Value: string);
        procedure SetFieldKind(const Value: tFieldKind);
        procedure SetFieldType(const Value: tFieldType);
public

        constructor create(aOwner : tObject);
        destructor destroy; override;
        procedure SetData(pt : pChar; NativeFormat : boolean);
        function GetData(Buffer: Pointer; NativeFormat : Boolean) : boolean;
        function GetData(Buffer: Pointer{=True}) : boolean;
//        property FieldName : string read fName write SetName;
        property FieldKind : tFieldKind read fFieldKind write SetFieldKind;
        property FieldType : tFieldType read fFieldType write SetFieldType;


end;

tSqliteRows = class (tObject)
private
        function getItem(index: integer): tSqliteField;
        procedure SetItem(index: integer; const Value: tSqliteField);
        function checkIndex(index : integer) : boolean;


public
        BookmarkFlag : tBookmarkFlag;
        Bookmark : LongInt;
        DataPointer : Pointer;
        constructor Create(fieldCount : integer);
        destructor destroy; override;
        procedure Push(item : tSqliteField);
        function Pop : tSqliteField;
        property Items[index : integer] : tSqliteField read getItem write SetItem;
        procedure Clear;
        procedure ClearCalcFields;
        function add(pt : Pchar; ptName : pCHar) : boolean;


protected
        fbuffercount : integer;
        fBuffer : ^tSqliteField;
        internalCount : integer;
        procedure clearBuffer;
end;


  TSQLite = class(TDataSet)
  private
    maxLengthInit : boolean;
    maxiL : pinteger;
    maxilcount : integer;
    fDoExceptions : boolean;
    fDoSQL : boolean;
    fIsCancel: boolean;
    fSQLite: Pointer;
    fMsg: String;
    fIsOpen: Boolean;
    fBusy: Boolean;
    fError: Integer;
    fVersion: String;
    fEncoding: String;
    fTable: tStrings;
    fLstName: TStringList;
    fLstVal: TStringList;
//    fbuffer : tObjectList;
    fOnData: TOnData;
    fOnBusy: TOnBusy;
    fOnQueryComplete: TOnQueryComplete;
    fBusyTimeout: integer;
    fPMsg: PChar;
    fChangeCount: integer;
    fSQL: tStringlist;
    fonwer : tComponent;
    fDataBaseName : string;
    fDataBase: string;
    fTableName: string;
    factive : boolean;
    procedure SetBusyTimeout(Timeout: integer);
    procedure SetDataBase(DBFileName: String);
    procedure setTableName(const Value: string);
    function getIsCancel: boolean;
    procedure clearBuffer;
  protected
    fCalcFieldsOfs,fRecordSize : integer;
    fBookMarkOfs,fRecordBufferSize : integer;
    fCurrentRecord : int64;
    fRecordCount : int64;
    fCursorOpen : boolean;
    fFieldOffset : tList;
  //  procedure internalInsert; override;
    function getActive: boolean;
   // procedure setActive(Value: boolean); override;
    function getRecNo : integer; override;
    function getBookmarkFlag(Buffer : pChar) : tBookMarkFlag; override;
    procedure InitBufferPointers;
    procedure GetBookmarkData(Buffer : pChar; Data : Pointer); override;
    procedure SetBookMarkData(Buffer : pChar; Data : Pointer); override;
    procedure InternalGotoBookmark(ABookMark : Pointer) ; override;
    function FieldDefsStored : boolean;
    procedure ClearCalcFields(Buffer : pChar); override;
    procedure OpenCursor(InfoQuery : Boolean); override;
    function getRecordCount : integer; override;
    procedure SetRecNo (value : integer); override;
    function getRecord(Buffer : pChar; GetMode : tGetMode; DoCheck : Boolean): tGetResult; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalAddRecord(Buffer : Pointer; DoAppend : boolean); override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalInitRecord(Buffer : pChar); override;
    procedure InternalLast;override;
    procedure InternalPost;override;
    procedure InternalSetToRecord (Buffer : pChar); override;
    function isCursorOpen : Boolean; override;
    procedure SetBookmarkFlag(Buffer : pChar; value : tBookmarkFlag); override;
    procedure SetFieldData(Field : tField; Buffer : Pointer); override;
    function allocRecordBuffer : pChar; override;
    procedure FreeRecordBuffer(var Buffer : pChar); override;
    function getRecordSize : Word; override;
    function getCanModify : boolean; override;
  public
      fbuffer : tObjectList; //po zakonczeniu debuggowania usunac
    constructor create(Aowner : tComponent); override;
    destructor Destroy; override;
    function getFieldData(Field : tField; Buffer : Pointer) : boolean; override;
    function Query(ASql: String{table= nil}) : Boolean;
    Function Query(ASQL: String; Table: TStrings): boolean;
    function ExecSQL : boolean;
    function ErrorMessage(ErrNo: Integer): string;
    function IsComplete(ASql: String): boolean;
    function LastInsertRow: integer;
    procedure Cancel; override;
    function DatabaseDetails(Table: TStrings): boolean;
    function CreateTable : boolean;
    procedure countMaxiLength(pt: pChar;index : int64);
    procedure InitMaxLength(length : integer);
  published
    property LastErrorMessage: string read fMsg;
    property LastError: Integer read fError;
    property Version: String read fVersion;
    property Encoding: String read fEncoding;
    property OnData: TOnData read fOnData write fOnData;
    property OnBusy: TOnBusy read fOnBusy write fOnBusy;
    property OnQueryComplete: TOnQueryComplete read fOnQueryComplete write fOnQueryComplete;
    property BusyTimeout: Integer read fBusyTimeout write SetBusyTimeout;
    property ChangeCount: Integer read fChangeCount;
    property SQL : tStringlist  read fSQL write fSQL;
//    property Fields : tstringlist read fFields;
    property DataBase : string read fDataBase write SetDataBase;
    property TableName : string read fTableName write setTableName;
    property Active : boolean read getActive write setActive;
    property isCancel : boolean read getIsCancel;
    property DoExceptions : boolean read fDoExceptions write fDoExceptions  stored true default true;
  end;
  function Pas2SQLStr(const PasString: string): string;
  function SQL2PasStr(const SQLString: string): string;
  function QuoteStr(const s: string; QuoteChar: Char): string;
  function UnQuoteStr(const s: string; QuoteChar: Char): string;
  function QuoteStr(const s: string{; QuoteChar: Char = #39}): string;
  function UnQuoteStr(const s: string{; QuoteChar: Char = #39}): string;
  procedure ValueList(const ColumnNames, ColumnValues: String; NameValuePairs: TStrings);


  procedure Register;

implementation

{$ifndef dynload}
uses sqlite;
{$else}
uses dynlibs;

function GetProcAddress(S : String) : Pointer;

begin

end;
{$endif}
const
  SQLITE_OK         =  0;   // Successful result
  SQLITE_ERROR      =  1;   // SQL error or missing database
  SQLITE_INTERNAL   =  2;   // An internal logic error in SQLite
  SQLITE_PERM       =  3;   // Access permission denied
  SQLITE_ABORT      =  4;   // Callback routine requested an abort
  SQLITE_BUSY       =  5;   // The database file is locked
  SQLITE_LOCKED     =  6;   // A table in the database is locked
  SQLITE_NOMEM      =  7;   // A malloc() failed
  SQLITE_READONLY   =  8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT  =  9;   // Operation terminated by sqlite_interrupt()
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // (Internal Only) Table or record not found
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // Database lock protocol error
  SQLITE_EMPTY      = 16;   // (Internal Only) Database table is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // Too much data for one row of a table
  SQLITE_CONSTRAINT = 19;   // Abort due to contraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITEDLL: PChar  = 'sqlite.dll';
  DblQuote: Char    = '"';
  SngQuote: Char    = #39;
  Crlf: String      = #13#10;
  Tab: Char         = #9;
  _DO_EXCEPTIONS = 1; //Handle or not exceptions in dataset

{$ifdef dynload}
var
  SQLite_Open: function(dbname: PChar; mode: Integer; var ErrMsg: PChar): Pointer; cdecl;
  SQLite_Close: procedure(db: Pointer); cdecl;
  SQLite_Exec: function(db: Pointer; SQLStatement: PChar; CallbackPtr: Pointer; Sender: TObject; var ErrMsg: PChar): integer; cdecl;
  SQLite_Version: function(): PChar; cdecl;
  SQLite_Encoding: function(): PChar; cdecl;
  SQLite_ErrorString: function(ErrNo: Integer): PChar; cdecl;
  SQLite_GetTable: function(db: Pointer; SQLStatement: PChar; var ResultPtr: Pointer; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PChar): integer; cdecl;
  SQLite_FreeTable: procedure(Table: PChar); cdecl;
  SQLite_FreeMem: procedure(P: PChar); cdecl;
  SQLite_Complete: function(P: PChar): boolean; cdecl;
  SQLite_LastInsertRow: function(db: Pointer): integer; cdecl;
  SQLite_Cancel: procedure(db: Pointer); cdecl;
  SQLite_BusyHandler: procedure(db: Pointer; CallbackPtr: Pointer; Sender: TObject); cdecl;
  SQLite_BusyTimeout: procedure(db: Pointer; TimeOut: integer); cdecl;
  SQLite_Changes: function(db: Pointer): integer; cdecl;
  LibsLoaded: Boolean;
  DLLHandle: THandle;
{$endif}

Var
  MsgNoError: String;

function QuoteStr(const s: string): string;
begin
  Result := QuoteStr(S,#39);
end;

function QuoteStr(const s: string; QuoteChar: Char): string;
begin
  Result := Concat(QuoteChar, s, QuoteChar);
end;

function UnQuoteStr(const s: string): string;
begin
  Result := UnQuoteStr(s,#39);
end;

function UnQuoteStr(const s: string; QuoteChar: Char): string;
begin
  Result := s;
  if length(Result) > 1 then
  begin
    if Result[1] = QuoteChar then
      Delete(Result, 1, 1);
    if Result[Length(Result)] = QuoteChar then
      Delete(Result, Length(Result), 1);
  end;
end;

function Pas2SQLStr(const PasString: string): string;
var
  n: integer;
begin
  Result := SQL2PasStr(PasString);
  n := Length(Result);
  while n > 0 do
  begin
    if Result[n] = SngQuote then
      Insert(SngQuote, Result, n);
    dec(n);
  end;
  Result := QuoteStr(Result);
end;

function SQL2PasStr(const SQLString: string): string;
const
  DblSngQuote: String = #39#39;
var
  p: integer;
begin
  Result := SQLString;
  p := pos(DblSngQuote, Result);
  while p > 0 do
  begin
    Delete(Result, p, 1);
    p := pos(DblSngQuote, Result);
  end;
  Result := UnQuoteStr(Result);
end;

procedure ValueList(const ColumnNames, ColumnValues: String; NameValuePairs: TStrings);
var
  n: integer;
  lstName, lstValue: TStringList;
begin
  if NameValuePairs <> nil then
  begin
    lstName := TStringList.Create;
    lstValue := TStringList.Create;
    lstName.CommaText := ColumnNames;
    lstValue.CommaText := ColumnValues;
    NameValuePairs.Clear;
    if lstName.Count = LstValue.Count then
      if lstName.Count > 0 then
        for n := 0 to lstName.Count - 1 do
          NameValuePairs.Append(Concat(lstName.Strings[n], '=', lstValue.Strings[n]));
    lstValue.Free;
    lstName.Free;
  end;
end;

{$ifdef dynload}
function LoadLibs: Boolean;
begin
  Result := False;
  DLLHandle := LoadLibrary(SQLITEDLL);
  if DLLHandle <> 0 then
  begin
    @SQLite_Open := GetProcAddress(DLLHandle, 'sqlite_open');
    if not Assigned(@SQLite_Open) then exit;
    @SQLite_Close := GetProcAddress(DLLHandle, 'sqlite_close');
    if not Assigned(@SQLite_Close) then exit;
    @SQLite_Exec := GetProcAddress(DLLHandle, 'sqlite_exec');
    if not Assigned(@SQLite_Exec) then exit;
    @SQLite_Version := GetProcAddress(DLLHandle, 'sqlite_libversion');
    if not Assigned(@SQLite_Version) then exit;
    @SQLite_Encoding := GetProcAddress(DLLHandle, 'sqlite_libencoding');
    if not Assigned(@SQLite_Encoding) then exit;
    @SQLite_ErrorString := GetProcAddress(DLLHandle, 'sqlite_error_string');
    if not Assigned(@SQLite_ErrorString) then exit;
    @SQLite_GetTable := GetProcAddress(DLLHandle, 'sqlite_get_table');
    if not Assigned(@SQLite_GetTable) then exit;
    @SQLite_FreeTable := GetProcAddress(DLLHandle, 'sqlite_free_table');
    if not Assigned(@SQLite_FreeTable) then exit;
    @SQLite_FreeMem := GetProcAddress(DLLHandle, 'sqlite_freemem');
    if not Assigned(@SQLite_FreeMem) then exit;
    @SQLite_Complete := GetProcAddress(DLLHandle, 'sqlite_complete');
    if not Assigned(@SQLite_Complete) then exit;
    @SQLite_LastInsertRow := GetProcAddress(DLLHandle, 'sqlite_last_insert_rowid');
    if not Assigned(@SQLite_LastInsertRow) then exit;
    @SQLite_Cancel := GetProcAddress(DLLHandle, 'sqlite_interrupt');
    if not Assigned(@SQLite_Cancel) then exit;
    @SQLite_BusyTimeout := GetProcAddress(DLLHandle, 'sqlite_busy_timeout');
    if not Assigned(@SQLite_BusyTimeout) then exit;
    @SQLite_BusyHandler := GetProcAddress(DLLHandle, 'sqlite_busy_handler');
    if not Assigned(@SQLite_BusyHandler) then exit;
    @SQLite_Changes := GetProcAddress(DLLHandle, 'sqlite_changes');
    if not Assigned(@SQLite_Changes) then exit;
    Result := True;
  end;
end;
{$endif}

function SystemErrorMsg(ErrNo: Integer): String;
var
  buf: PChar;
  size: Integer;
  MsgLen: Integer;
begin
{  msglen:=0;
  size := 256;
  GetMem(buf, size);

  If ErrNo = - 1 then
    ErrNo := GetLastError;
  MsgLen := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, 0, buf, size, nil);

  if MsgLen = 0 then
    Result := 'ERROR'
  else
    Result := buf;
}
  Result := ('SystemErrorMsg Not Implemented');
end;

function SystemErrorMsg: String;

begin
  SystemErrorMsg(-1);
end;

function BusyCallback(Sender: Pointer; ObjectName: PChar; BusyCount: integer): integer; cdecl;
var
  sObjName: String;
  bCancel: Boolean;
begin
  Result := -1;
  with TSQLite(Sender) do
  begin
    if Assigned(fOnBusy) then
    begin
      bCancel := False;
      sObjName := ObjectName;
      fOnBusy(Tsqlite(Sender), sObjName, BusyCount, bCancel);
      if bCancel then
        Result := 0;
    end;
  end;
end;

function ExecCallback(Sender: TObject; Columns: Integer; ColumnValues: Pointer; ColumnNames: Pointer): integer; cdecl;
var
  PVal, PName: ^PChar;
  n: integer;
  sVal, sName: String;
begin
  Result := 0;
  with Sender as TSQLite do
  begin
    if (Assigned(fOnData) or Assigned(fTable)) then
    begin
      fLstName.Clear;
      fLstVal.Clear;
      if Columns > 0 then
      begin
        PName := ColumnNames;
        PVal := ColumnValues;
        for n := 0 to Columns - 1 do
        begin
          fLstName.Append(PName^);
          fLstVal.Append(PVal^);
          if Assigned(fTable) then
          begin
                fTable.Append(PVal^);
          end;
          inc(PName);
          inc(PVal);
        end;
      end;
      sVal := fLstVal.CommaText;
      sName := fLstName.CommaText;
      if Assigned(fOnData) then
        fOnData(Sender, Columns, sName, sVal);

    end;
//    InternalOpen;
  end;
end;

function ExecCallback2(Sender: TObject; Columns: Integer; ColumnValues: Pointer; ColumnNames: Pointer): integer; cdecl;
var
  PVal, PName: ^PChar;
  n: integer;
  sVal, sName: String;
  t : tSqliteRows;
  p : pointer;
  temp : LongInt;

begin
  Result := 0;
  with Sender as TSQLite do begin
    if (Assigned(fOnData) or assigned(fBuffer)) then begin
    fLstName.Clear;
//      fLstVal.Clear;
      if Columns > 0 then begin
        PName := ColumnNames;
        PVal := ColumnValues;
        fBuffer.Add(tSqliteRows.Create(Columns));
        temp:=fBuffer.count-1;
        initMaxLength(columns);
        for n := 0 to Columns - 1 do begin
                fLstName.Append(PName^);
                if Assigned(fBuffer) then begin
                        p:=fBuffer.Items[temp];
                        t:=tSqliteRows(p);
                        if t=nil then continue;
                        t.Add(PVAL^,PNAME^);

                end;
                countMaxiLength(PVAL^,n);
          inc(PName);
          inc(PVal);
        end;
        // at last we add the bookmark info
        t.Bookmark:=temp;
      end;
      if Assigned(fOnData) then begin
              sVal := fLstVal.CommaText;
              sName := fLstName.CommaText;
              fOnData(Sender, Columns, sName, sVal);
      end;

    end;
//    InternalOpen;
  end;
end;

procedure  TSQLite.SetDataBase(DBFileName: String);
var
  afPMsg: PChar;
begin

  fError := SQLITE_ERROR;
  fIsOpen := False;
  fOnData := nil;
  fOnBusy := nil;
  fOnQueryComplete := nil;
  fChangeCount := 0;
{$ifdef dynload}
  if LibsLoaded then
    begin
{$endif}
    fSQLite := SQLite_Open(PChar(DBFileName), 1, @afPMsg);
    SQLite_FreeMem(afPMsg);
    if fSQLite <> nil then
    begin
      {$ifndef fpc}
      fVersion := strpas(SQLite_Version);
      fEncoding := strpas(SQLite_Encoding);
      {$endif}
      fIsOpen := True;
      fError := SQLITE_OK;
    end;
{$ifdef dynload}
  end;
{$endif}
  fMsg := ErrorMessage(fError);
end;

destructor TSQLite.Destroy;
begin
try
if assigned(fSQl) then begin
        fsql.free;
        fsql:=nil;
end;
  if fIsOpen then
    SQLite_Close(fSQLite);
  fIsOpen := False;
if assigned(fLstName) then begin
  fLstName.Free;
  fLstName:=nil;
end;
if assigned(fLstVal) then begin
  fLstVal.Free;
  fLstVal:=nil;
end;
  fSQLite := nil;
  fOnData := nil;
  fOnBusy := nil;
  fOnQueryComplete := nil;
  fLstName := nil;
  fLstVal := nil;
if assigned(fBuffer) then begin
        clearBuffer;
        fBuffer.Free;
        fBuffer:=nil;
end;
except
end;
  inherited Destroy;
end;

function TSQLite.Query(ASql: String): boolean;

begin
  Result:=Query(ASql,Nil);
end;

function TSQLite.Query(ASql: String; Table: TStrings): boolean;
//var
//  fPMsg: PChar;
begin
  maxLengthInit:=false;
  fError := SQLITE_ERROR;
  if fIsOpen then
  begin
    fPMsg := nil;
    fBusy := True;
    fTable := Table;
    if fTable <> nil then
      fTable.Clear;
    fError := SQLite_Exec(fSQLite, PChar(ASql), @ExecCallback, Self, @fPMsg);
    SQLite_FreeMem(fPMsg);
    fChangeCount := SQLite_Changes(fSQLite);
    fTable := nil;
    fBusy := False;
    if Assigned(fOnQueryComplete) then
      fOnQueryComplete(Self);
  end;
  fMsg := ErrorMessage(fError);
  Result := not (fError <> SQLITE_OK);//function should return true, if execution of query ends ok..
  if result and not active then
        factive:=true;
  fDoSql:=true;
end;


procedure TSQLite.SetBusyTimeout(Timeout: Integer);
begin
  fBusyTimeout := Timeout;
  if fIsOpen then
  begin
    SQLite_Busy_Timeout(fSQLite, fBusyTimeout);
    if fBusyTimeout > 0 then
      SQLite_Busy_Handler(fSQLite, @BusyCallback, Self)
    else
      SQLite_Busy_Handler(fSQLite, nil, nil);
  end;
end;

function TSQLite.LastInsertRow: integer;
begin
  if fIsOpen then
    Result := SQLite_Last_Insert_Rowid(fSQLite)
  else
    Result := -1;
end;

function TSQLite.ErrorMessage(ErrNo: Integer): string;
begin
{$ifdef dynload}
  if LibsLoaded then
  begin
{$endif}
    if ErrNo = 0 then
      Result := MsgNoError
    else
      Result := SQLite_Error_String(ErrNo);
{$ifdef dynload}
  end else
    Raise exception.Create('Library "sqlite.dll" not found.');
{$endif}
end;

function TSQLite.IsComplete(ASql: String): boolean;
begin
  Result := SQLite_Complete(PChar(ASql))=0;
end;

function TSQLite.DatabaseDetails(Table: TStrings): boolean;
begin
  Result := Query('SELECT * FROM SQLITE_MASTER;', Table);
end;

function TSQLite.ExecSQL: boolean;
var i : integer;

begin
  result:=false;
  maxLengthInit:=false;
  fError := SQLITE_ERROR;
  if fIsOpen then
  begin
    fPMsg := nil;
    fBusy := True;

    if fTable <> nil then
      fTable.Clear;
    for i:=0 to fsql.Count-1 do begin
    fError := SQLite_Exec(fSQLite, PChar(fSql[i]), @ExecCallback2, Self, @fPMsg);
    SQLite_FreeMem(fPMsg);
    end;
    fChangeCount := SQLite_Changes(fSQLite);
    fTable := nil;
    fBusy := False;
    if Assigned(fOnQueryComplete) then
      fOnQueryComplete(Self);
  end;
  fMsg := ErrorMessage(fError);
  Result :=not (fError <> SQLITE_OK);
  if result and not active then
        factive:=true;
  fDoSQl:=true;
end;

constructor TSQLite.Create(Aowner: tComponent);
begin
inherited create(Aowner);
fLstName := TStringList.Create;
fLstVal := TStringList.Create;
fDoSql:=false;
fsql:=tStringList.Create;
fOnwer:=owner;
fBuffer:=tObjectList.Create(true);
if length(fDataBase)>1 then
        setDataBase(fDataBase);
end;

procedure TSQLite.setTableName(const Value: string);
begin
if (not active) and (length(value)>0) then begin
  fTableName := Value;
  sql.Clear;
  sql.add('select rowid,* from '+tableName+';');
end;
end;

function TSQLite.getActive: boolean;
begin
result:=fActive;
end;

{
procedure TSQLite.setActive(Value: boolean);
begin
  if value then
    begin
    //switch for  active=true;
    if active then
      active:=false;
    end
  else
    begin
    fDoSQL:=value;
    end;
  inherited setActive(value);
end;
}

function TSQLite.getRecNo: integer;
begin
result:=self.fCurrentRecord;
end;

procedure TSQLite.Cancel;
begin
  inherited;
  fIsCancel := False;
  if fBusy and fIsOpen then
  begin
    do_SQLite_interrupt(fSQLite);
    fBusy := false;
    fIsCancel := True;
  end;

end;

function TSQLite.getIsCancel: boolean;
begin

end;

function TSQLite.getBookmarkFlag(Buffer: pChar): tBookMarkFlag;
begin
result:= pRecInfo(Buffer)^.BookmarkFlag;
end;

procedure TSQLite.InitBufferPointers;
begin
fCalcFieldsOfs :=fRecordSize;
//fRecInfoOfs :=fCalcFieldsOfs + CalcFieldsSize;
//fBookMarkOfs := fRecInfoOfs+SizeOf(tRecInfo);
fRecordBufferSize :=fBookmarkOfs + BookmarkSize;
end;

procedure TSQLite.GetBookmarkData(Buffer: pChar; Data: Pointer);
begin
Move(Buffer[fBookMarkOfs],Data^,SizeOf(tBinBookMark));
//implementacja jest watpliwa
end;

procedure TSQLite.SetBookMarkData(Buffer: pChar; Data: Pointer);
begin
Move(Data^,Buffer[fBookMarkOfs],SizeOf(tbinBookMark));

end;

procedure TSQLite.InternalGotoBookmark(ABookMark: Pointer);
begin
with pBinBookMark(ABookMark)^ do begin
    fCurrentRecord :=RecPtr;
end;
end;

function TSQLite.FieldDefsStored: boolean;
begin

end;

procedure TSQLite.ClearCalcFields(Buffer: pChar);
var p : pointer;
t : tSQliteRows;
begin
inherited;
p:=buffer;
if p<>nil then begin
try
        t:=tSQliteRows(p);
        t.clearCalcFields;
except
end;
end;
end;

function TSQLite.getRecordCount: integer;
begin
result :=fRecordCount;
end;

procedure TSQLite.OpenCursor(InfoQuery: Boolean);
begin
  inherited;

end;

procedure TSQLite.SetRecNo(value: integer);
begin
  inherited;

end;

function TSQLite.CreateTable: boolean;
begin

end;

function TSQLite.getRecord(Buffer: pChar; GetMode: tGetMode;
  DoCheck: Boolean): tGetResult;
begin
if fRecordCount<1 then
        result:=grEof
else begin
        result:=grOk;
        Case GetMode of
                gmNext :
                        if fCurrentRecord>= (fRecordCount-1) then
                                result:=grEof
                        else
                                Inc(fCurrentRecord);
                gmPrior :
                        if (fCurrentRecord <=0) then
                                result:=grBof
                        else
                                Dec(fCurrentRecord);
                gmCurrent :
                        if (fCurrentRecord >= fRecordCount) or (fCurrentRecord <0) then
                                result:=grError;
        end;
end;
if result=grOk then begin
        self.fRecordBufferSize:=sizeOf(fBuffer[fCurrentRecord]);
        self.fRecordSize:=self.fRecordBufferSize;
       // Buffer:=fBuffer.List[fcurrentRecord];
        //read data from psyh buffer sqlite..;)
        GetCalcFields(Buffer);
 {       with fBuffer.Items[fCurrentRecord] as tSqliteRows do begin
                BookmarkFlag := bfCurrent;
        end;}
    with PRecInfo(Buffer)^ do
    begin
      Index := fCurrentRecord;
      BookmarkFlag := bfCurrent;
      Bookmark := Integer (fCurrentRecord);
    end;



end;
if result=grError then begin
        if DoCheck and DoExceptions then
                raise edataBaseError.Create('Invalid Record');
end;
end;

procedure TSQLite.InternalInitFieldDefs;
var i : integer;
begin
FieldDefs.Clear;
for i:=0 to fLstname.Count-1 do begin
        FieldDefs.Add(fLstName[i],ftString,MaxiL[i],false);
end;
end;

procedure TSQLite.InternalOpen;
begin
if fBUffer<>nil then begin
        clearBuffer;

end;
if (length(tableName)>0) and (fSQL.Count<1) then begin
        fsql.add('select rowid,* from '+fTableName);
end;
if not fDoSQL then
        fActive:=execSQL;
InternalInitFieldDefs;
{
if ((fLstName.count-1)>0) and (fBuffer<>nil) then
        fRecordCount:=(fBuffer.Count-1) div (fLstName.Count-1)
else
        fRecordCount:=0;
}
if  (fBuffer<>nil) then
        fRecordCount:=(fBuffer.Count-1)
else
        fRecordCount:=0;
if DefaultFields then
        CreateFields;
BindFields(true);
FisOpen:=true;
  FRecordSize := sizeof (TRecInfo);
  FCurrentRecord := -1;
  BookmarkSize := sizeOf (Integer);
end;

procedure TSQLite.InternalClose;
begin
clearBuffer;
end;

function TSQLite.allocRecordBuffer: pChar;
var p : pointer;
begin
//now is time to calculate currentRecordSize...
  GetMem(Result,GetRecordSize);
  FillChar(Result^,GetRecordSize,0);
end;

procedure TSQLite.FreeRecordBuffer(var Buffer: pChar);
begin
//FreeMem(Buffer,sizeOf(Buffer));
FreeMem(Buffer,GetRecordSize);
end;

function TSQLite.getRecordSize: Word;
begin

  Result:=sizeof(TRecInfo);

end;

procedure TSQLite.InternalAddRecord(Buffer: Pointer; DoAppend: boolean);
begin

end;

procedure TSQLite.InternalDelete;
begin

end;

procedure TSQLite.InternalFirst;
begin
  self.fCurrentRecord:=0;
end;

procedure TSQLite.InternalInitRecord(Buffer: pChar);
begin

end;

procedure TSQLite.InternalLast;
begin
  fCurrentRecord:=fRecordCount;
end;

procedure TSQLite.InternalPost;
begin
end;

procedure TSQLite.InternalSetToRecord(Buffer: pChar);

begin

end;

function TSQLite.isCursorOpen: Boolean;
begin

end;


procedure TSQLite.SetFieldData(Field: tField; Buffer: Pointer);
// var aa : string;
begin
// Does NOthing ??
// aa:=Field.NewValue;
//  inherited;

end;

procedure TSQLite.SetBookmarkFlag(Buffer: pChar; value: tBookmarkFlag);
begin
//  inherited;

end;

function TSQLite.getFieldData(Field: tField; Buffer: Pointer): boolean;
var i,k : integer;
p : tSqliteField;
r : tSqliteRows;
pt : pointer;
begin

result:=false;
k:=fieldDefs.Count-1;
self.fLstName.Count;
r:=fBuffer[PRecInfo(ActiveBuffer)^.Index] as tSqliteRows;
if r=nil then exit;
for i:=0 to k do begin
                if lowercase(fLstName[i])=lowercase(field.FieldName) then begin
                        p:=r.items[i];
                        if p = nil then break;
                        p.GetData(Buffer,true);
                        result:=true;
                        break;
                end;
end;
end;

{ tSqliteRows }

procedure tSqliteRows.Push(item: tSqliteField);
begin
if internalcount<fBuffercount then begin
        fBuffer[internalCount]:=item;
        inc(internalCount);
end;
end;

constructor tSqliteRows.Create(fieldCount: integer);
begin

inherited create;

if fieldCount<=0 then
        fieldCount:=1;
 fbuffercount:=fieldcount+1;
getmem(fBuffer,fbuffercount*sizeof(pointer));
end;

destructor tSqliteRows.destroy;


begin

  clearBuffer;
  inherited;
end;

function tSqliteRows.Pop: tSqliteField;
begin
result:=nil;
if (internalCount>0) and (internalCount<fBuffercount) then begin
        result:=fBuffer[internalCount];
        Dec(internalCount);
end;
end;

function tSqliteRows.getItem(index: integer): tSqliteField;
begin
result:=nil;
if checkIndex(index) then
        result:=fBuffer[Index];
end;

procedure tSqliteRows.SetItem(index: integer; const Value: tSqliteField);
begin
if checkIndex(index) then
        fBuffer[index]:=Value;
end;

function tSqliteRows.checkIndex(index : integer): boolean;
begin
result:=false;
if (index>=0) and (index<internalCount) then
        result:=true;
end;

procedure tSqliteRows.clearBuffer;
var i : integer;
begin
if internalcount>0 then begin
for i:=0 to internalCount -1  do begin
        if fBuffer[i]<>nil then begin
                fBuffer[i].Free;
                fBuffer[i]:=nil;
        try
        except
                continue;
        end;
        end;
end;
fbuffercount:=0;
FreeMem(fBuffer);
end;

end;

procedure tSqliteRows.Clear;
begin
clearBuffer;
internalCount:=0;
end;

procedure tSqliteRows.ClearCalcFields;
begin

end;

function tSqliteRows.Add(pt: pChar;ptName : pChar):boolean;
var tmp : int64;
begin
Push(tSqliteField.Create(nil));
tmp:=internalCount-1;
items[tmp].FieldKind:=fkData;
items[tmp].SetFieldType(ftString);
items[tmp].SetData(pt,true);
end;


procedure tSqlite.countMaxiLength(pt: pChar; index : int64);
begin
if length(pt)>maxil[index] then
        maxiL[index]:=length(pt);
end;

{ tSqliteField }

constructor tSqliteField.create(aOwner: tObject);
begin

inherited create;
fOwner:=aOwner;
end;

destructor tSqliteField.destroy;
begin

  inherited;
end;

function tSqliteField.GetData(Buffer: Pointer) : boolean;

begin
  Result:=GetData(Buffer,True);
end ;

function tSqliteField.GetData(Buffer: Pointer;
  NativeFormat: Boolean): boolean;
  var
 l,tIntegerType : integer;
 tDateTimeType : tDateTime;
begin
try
result:=false;

if not nativeFormat then begin
        Move(data,Buffer^,sizeOf(data));
        result:=true;
end else begin
        case self.fieldType of
        ftInteger : begin
                        tIntegerType:=StrToInt(data);
                        Move(tIntegerType,Buffer^,sizeOf(data));
                end;
        ftDateTime  : begin
                        tDateTimeType:=StrToDate(data);
                        Move(tDateTimeType,Buffer^,sizeOf(data));
                end;
        ftString : begin
                  //      L:=length(data);
                  //      Move(data,Buffer^,l);
                      StrCopy (Buffer, pchar(data));
                end;
        else
                        Move(data,Buffer^,sizeOf(data));
        end;
        result:=true;
end;
except
        Buffer:=nil;
end;
end;

procedure tSqliteField.SetData(pt: pChar; NativeFormat: boolean);
begin
data:=pt;
end;

procedure tSqliteField.SetFieldKind(const Value: tFieldKind);
begin
  fFieldKind := Value;
end;

procedure tSqliteField.SetFieldType(const Value: tFieldType);
begin
  fFieldType := Value;
end;
{
procedure tSqliteField.SetName(const Value: string);
begin
  fName := Value;
end;
 }
function TSQLite.getCanModify: boolean;
begin
result:=false;
exit;//temporary
if length(fTableName)>0 then
        result:=true;
end;

procedure TSQLite.InitMaxLength(length: integer);
begin
if not maxLengthInit and (length>0) then begin
        maxLengthInit:=true;
        maxilcount:=length;
        getmem(maxiL,maxilcount*sizeof(integer));
end;
end;

procedure TSQLite.clearBuffer;

begin
if assigned(fBuffer) then begin
        if fBuffer.count>0 then begin
                fBuffer.pack;
                fBuffer.clear;
        end;
end;
if assigned(fLstVal) then begin
        fLstVal.Clear;
end;
if assigned(fLstName) then begin
        fLstName.Clear;
end;
end;

{
procedure TSQLite.internalInsert;
begin
  inherited;
 if not getCanModify then exit;
end;
}

procedure Register;
begin
  RegisterComponents('MK', [tSqlite]);
end;

initialization
{$ifdef dynload}
  LibsLoaded := LoadLibs;
{$endif}
{$ifdef fpc}
  MsgNoError := SystemErrorMsg(0);
{$else}
  MsgNoError := 'The operation completed successfully';
{$endif}

finalization
{$ifdef dynload}
  if DLLHandle <> 0 then
    FreeLibrary(DLLHandle);
{$endif}



end.
