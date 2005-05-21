{
    Copyright (c) 2000 by Pavel Stingl

    Interbase database & dataset

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Interbase;

{$mode objfpc}
{$H+}
{$M+}   // ### remove this!!!

interface

uses SysUtils, Classes, IBase60, DB;

type

  PInteger = ^integer;
  PSmallInt= ^smallint;

  TIBDatabase = class;
  TIBTransaction = class;
  TIBQuery = class;
  TIBStoredProc = class;

  EInterBaseError = class(Exception);

{ TIBDatabase }

  TIBDatabase = class (TDatabase)
  private
    FIBDatabaseHandle    : pointer;
    FPassword            : string;
    FStatus              : array [0..19] of ISC_STATUS;
    FTransaction         : TIBTransaction;
    FUserName            : string;
    FCharSet             : string;
    FDialect             : integer;
    FRole                : String;

    procedure SetDBDialect;
    procedure SetTransaction(Value : TIBTransaction);
  protected
    procedure Notification(AComponent: TComponent; 
      Operation: TOperation); override;
    function GetHandle : pointer; virtual;
      { This procedure makes connection to Interbase server internally.
        Is visible only by descendants, in application programming
        will be invisible. Connection you must establish by setting
        @link(Connected) property to true, or by call of Open method.
      }
    procedure DoInternalConnect; override;
      { This procedure disconnects object from IB server internally.
        Is visible only by descendants, in application programming
        will be invisible. Disconnection you must make by setting
        @link(Connected) property to false, or by call of Close method.
      }
    procedure DoInternalDisconnect; override;
  public
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    destructor Destroy; override;
    property Handle: Pointer read GetHandle;
  published
    { On connect, TIBDatabase object retrieve SQL dialect of database file,
      and sets this property to responding value }
    property Dialect  : integer read FDialect write FDialect;
    { Before firing Open method you must set @link(Password),@link(DatabaseName),
      @link(UserName) properties in order of successfull connect to database }
    property Password : string read FPassword write FPassword;
    { This property holds default transaction for database. You must assign it by hand
      now, default assignment becomes handy, in next release, with transaction
      handling and evidence }
    property Transaction : TIBTransaction read FTransaction write SetTransaction;
    { Before firing Open method you must set @link(Password),@link(DatabaseName),
      @link(UserName) properties in order of successfull connect to database }
    property UserName : string read FUserName write FUserName;
    { The character set used in SQL statements }
    property CharSet : string read FCharSet write FCharSet;

    { Identifies, if connection to Interbase server is established, or not.
      Instead of calling Open, Close methods you can connect or disconnect
      by setting this property to true or false.
    }
    property Connected;
    { This property holds database connect string. On local server it will be
      absolute path to the db file, if you wanna connect over network, this
      path looks like this: <server_name>:<path_on_server>, where server_name
      is absolute IP address, or name of server in DNS or hosts file, path_on_server
      is absolute path to the file again }
    Property Role :  String read FRole write FRole;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

{ TIBTransaction }

  {
    Interbase has two modes for commit and rollback transactions,
    the difference is simple. If you execute Commit or Rollback,
    current transaction ends, and you must create new one.
    If you, on other side, need only commit or rollback data
    without transaction closing, execute with CommitRetaining or
    RollbackRetaining. Transaction handle, environment etc. will be
    as same as before action. Possible values are : caNone, caCommit, caCommitRetaining, caRollback,
    caRollbackRetaining
  }

  TCommitRollbackAction = (caNone, caCommit, caCommitRetaining, caRollback,
    caRollbackRetaining);
  TAccessMode = (amReadWrite, amReadOnly);
  TIsolationLevel = (ilConcurrent, ilConsistent, ilReadCommittedRecV,
    ilReadCommitted);
  TLockResolution = (lrWait, lrNoWait);
  TTableReservation = (trNone, trSharedLockRead, trSharedLockWrite,
    trProtectedLockRead, trProtectedLockWrite);

  TIBTransaction = class (TComponent)
  private
    FTransactionHandle   : pointer;               // Transaction handle
    FAction              : TCommitRollbackAction;
    FActive              : boolean;
    FTPB                 : string;                // Transaction parameter buffer
    FDatabase            : TIBDatabase;
    FAccessMode          : TAccessMode;
    FIsolationLevel      : TIsolationLevel;
    FLockResolution      : TLockResolution;
    FTableReservation    : TTableReservation;
    FStatus              : array [0..19] of ISC_STATUS;

    procedure SetActive(Value : boolean);
    procedure SetTPB;
  protected
    function GetHandle : pointer; virtual;
  public
    { Commits all actions, which was made in transaction, and closes transaction}
    procedure Commit; virtual;
    { Commits all actions, closes transaction, and creates new one }
    procedure CommitRetaining; virtual;
    { Rollbacks all actions made in transaction, and closes transaction }
    procedure Rollback; virtual;
    { Rollbacks all actions made in transaction, closes trans. and creates new one }
    procedure RollbackRetaining; virtual;
    { Creates new transaction. If transaction is active, closes it and make new one.
      Action taken while closing responds to @link(Action) property settings }
    procedure StartTransaction;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Handle: Pointer read GetHandle;
  published
    { Default action while closing transaction by setting
     @link(Active) property. For details see @link(TCommitRollbackAction)}
    property Action : TCommitRollbackAction read FAction write FAction;
    { Is set to true while transaction is active, false if not.
      If you set it manually to true, object executes
      @link(StartTransaction) method, if transaction is
      active, and you set Active to false, object executes
      one of @link(Commit), @link(CommitRetaining), @link(Rollback),
      @link(RollbackRetaining) methods, depending on @link(Action) property
      setting.
    }
    property Active : boolean read FActive write SetActive;
    { Transaction must be assigned to some database session, for which purpose
      you must use this property}
    property Database : TIBDatabase read FDatabase write FDatabase;

    { These four properties will be used in next StartTransaction calls }
    property AccessMode: TAccessMode
      read FAccessMode write FAccessMode default amReadWrite;
    property IsolationLevel: TIsolationLevel
      read FIsolationLevel write FIsolationLevel default ilConcurrent;
    property LockResolution: TLockResolution
      read FLockResolution write FLockResolution default lrWait;
    property TableReservation: TTableReservation
      read FTableReservation write FTableReservation default trNone;
  end;

{ TIBQuery }

  PIBBookmark = ^TIBBookmark;
  TIBBookmark = record
    BookmarkData : integer;
    BookmarkFlag : TBookmarkFlag;
  end;

  TStatementType = (stNone, stSelect, stInsert, stUpdate, stDelete,
    stDDL, stGetSegment, stPutSegment, stExecProcedure,
    stStartTrans, stCommit, stRollback, stSelectForUpd);

  TIBQuery = class (TDBDataset)
  private
    FOpen                : Boolean;
    FTransaction         : TIBTransaction;
    FDatabase            : TIBDatabase;
    FStatus              : array [0..19] of ISC_STATUS;
    FFieldFlag           : array [0..1023] of IBase60.Short;
    FBufferSize          : integer;
    FSQLDA               : PXSQLDA;
    FSQLDAAllocated      : integer;
    FStatement           : pointer;
    FRecordCount         : integer;
    FRecordSize          : word;
    FCurrentRecord       : integer;
    FSQL                 : TStrings;
    FIsEOF               : boolean;
    FStatementType       : TStatementType;
    FLoadingFieldDefs    : boolean;

    procedure SetDatabase(Value : TIBDatabase);
    procedure SetTransaction(Value : TIBTransaction);
    procedure AllocSQLDA(Count : integer);
    procedure AllocStatement;
    procedure FreeStatement;
    procedure PrepareStatement;
    procedure DescribeStatement;
    procedure SetUpSQLVars;
    procedure AllocFldBuffers;
    procedure FreeFldBuffers;
    procedure Fetch;
    function LoadBufferFromSQLDA(Buffer : PChar): TGetResult;
    procedure GetStatementType;
    procedure SetFieldSizes;
    procedure TranslateFldType(SQLType, SQLLen : integer; var LensSet : boolean;
      var TrType : TFieldType; var TrLen : word);

    procedure ExecuteImmediate;
    procedure ExecuteParams;
    procedure Execute;

    // conversion methods
    procedure GetDateTime(CurrBuff, Buffer : pointer; AType : integer);
    procedure GetFloat(CurrBuff, Buffer : pointer; Field : TField);

  protected

    // abstract & virual methods of TDataset
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: integer; override;
    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;
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
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  public
    { This method is used for executing sql statements, which
      doesn't return any rows. (insert,delete,update, and DDL commands) }
    procedure ExecSQL; virtual;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Query must have transaction assigned. If transaction is not assigned, and database
      is, object looks, if database have default transaction, and assigns it }
    property Transaction : TIBTransaction read FTransaction write SetTransaction;
    { Use this property to determine, which database session can query use }
    property Database    : TIBDatabase read FDatabase write SetDatabase;
    { This property holds SQL command, which you want to execute }
    property SQL         : TStrings read FSQL write FSQL;
    // Publish TDataset properties.
    property Active;
    property AutoCalcFields;
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
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

{ TIBStoredProc - not implemented - yet :-/}

  TIBStoredProc = class (TDataset)
  private
  protected
  public
  published
  end;

implementation

type

  TTm = packed record
    tm_sec : longint;
    tm_min : longint;
    tm_hour : longint;
    tm_mday : longint;
    tm_mon : longint;
    tm_year : longint;
    tm_wday : longint;
    tm_yday : longint;
    tm_isdst : longint;
    __tm_gmtoff : longint;
    __tm_zone : Pchar;
  end;

procedure CheckError(ProcName : string; Status : array of ISC_STATUS);
var
  buf : array [0..1024] of char;
  p   : pointer;
  Msg : string;
begin
  if ((Status[0] = 1) and (Status[1] <> 0)) then
  begin
    p := @Status;
    while isc_interprete(Buf, @p) > 0 do
      Msg := Msg + #10' -' + StrPas(Buf);
    raise EInterBaseError.Create(ProcName + ': ' + Msg);
  end;
end;

{ TIBDatabase }

procedure TIBDatabase.SetDBDialect;
var
  x : integer;
  Len : integer;
  Buffer : string;
  ResBuf : array [0..39] of byte;
begin
  Buffer := Chr(isc_info_db_sql_dialect) + Chr(isc_info_end);
  if isc_database_info(@FStatus, @FIBDatabaseHandle, Length(Buffer),
    @Buffer[1], SizeOf(ResBuf), @ResBuf) <> 0 then
      CheckError('TIBDatabse.SetDBDialect', FStatus);
  x := 0;
  while x < 40 do
    case ResBuf[x] of
      isc_info_db_sql_dialect :
        begin
          Inc(x);
          Len := isc_vax_integer(@ResBuf[x], 2);
          Inc(x, 2);
          FDialect := isc_vax_integer(@ResBuf[x], Len);
          Inc(x, Len);
        end;
      isc_info_end : Break;
    end;
end;

procedure TIBDatabase.SetTransaction(Value : TIBTransaction);
begin
  if Value <> FTransaction then
  begin
    if FTransaction <> nil then
    begin
      if FTransaction.Active then
        raise EInterBaseError.Create(
          'Cannot assign transaction while old transaction active!');
      FTransaction.RemoveFreeNotification(Self);
    end;

    FTransaction := Value;

    if FTransaction <> nil then
    begin
      FTransaction.Database := Self;
      FTransaction.FreeNotification(Self);
    end;
  end;
end;

function TIBDatabase.GetHandle: pointer;
begin
  Result := FIBDatabaseHandle;
end;

procedure TIBDatabase.DoInternalConnect;
var
  DPB : string;
begin
  if Connected then
    Close;
  DPB := chr(isc_dpb_version1);
  if (FUserName <> '') then
  begin
    DPB := DPB + chr(isc_dpb_user_name) + chr(Length(FUserName)) + FUserName;
    if (FPassword <> '') then
      DPB := DPB + chr(isc_dpb_password) + chr(Length(FPassword)) + FPassword;
  end;
  if (FRole <> '') then
     DPB := DPB + chr(isc_dpb_sql_role_name) + chr(Length(FRole)) + FRole;
  if Length(CharSet) > 0 then
    DPB := DPB + Chr(isc_dpb_lc_ctype) + Chr(Length(CharSet)) + CharSet;

  if (DatabaseName = '') then
    raise EInterBaseError.Create('TIBDatabase.Open: Database connect string (DatabaseName) not filled in!');
  FIBDatabaseHandle := nil;
  if isc_attach_database(@FStatus, Length(DatabaseName), @DatabaseName[1], @FIBDatabaseHandle,
         Length(DPB), @DPB[1]) <> 0 then
    CheckError('TIBDatabase.Open', FStatus);
  SetDBDialect;
end;

procedure TIBDatabase.DoInternalDisconnect;
begin
  if not Connected then
  begin
    FIBDatabaseHandle := nil;
    Exit;
  end;
  isc_detach_database(@FStatus[0], @FIBDatabaseHandle);
  CheckError('TIBDatabase.Close', FStatus);
end;

procedure TIBDatabase.StartTransaction;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TIBDatabase.StartTransaction: Transaction not set');
  FTransaction.Active := True;
end;

procedure TIBDatabase.EndTransaction;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TIBDatabase.EndTransaction: Transaction not set');
  FTransaction.Active := False;
end;

destructor TIBDatabase.Destroy;
begin
  if FTransaction <> nil then
  begin
    FTransaction.Active := False;
    FTransaction.Database := nil;
  end;
  inherited Destroy;
end;

procedure TIBDatabase.Notification(AComponent: TComponent; 
  Operation: TOperation); 
begin
  inherited;
  if (AComponent = FTransaction) and (Operation = opRemove) then
    FTransaction := nil;
end;

{ TIBTransaction }

procedure TIBTransaction.SetActive(Value : boolean);
begin
  if FActive and (not Value) then
    Rollback
  else if (not FActive) and Value then
    StartTransaction;
end;

procedure TIBTransaction.SetTPB;
begin
  FTPB := chr(isc_tpb_version3);

  case FAccessMode of
    amReadWrite : FTPB := FTPB + chr(isc_tpb_write);
    amReadOnly  : FTPB := FTPB + chr(isc_tpb_read);
  end;

  case FIsolationLevel of
    ilConsistent        : FTPB := FTPB + chr(isc_tpb_consistency);
    ilConcurrent        : FTPB := FTPB + chr(isc_tpb_concurrency);
    ilReadCommittedRecV : FTPB := FTPB + chr(isc_tpb_read_committed) +
      chr(isc_tpb_rec_version);
    ilReadCommitted     : FTPB := FTPB + chr(isc_tpb_read_committed) +
      chr(isc_tpb_no_rec_version);
  end;

  case FLockResolution of
    lrWait   : FTPB := FTPB + chr(isc_tpb_wait);
    lrNoWait : FTPB := FTPB + chr(isc_tpb_nowait);
  end;

  case FTableReservation of
    trSharedLockRead     : FTPB := FTPB + chr(isc_tpb_shared) +
      chr(isc_tpb_lock_read);
    trSharedLockWrite    : FTPB := FTPB + chr(isc_tpb_shared) +
      chr(isc_tpb_lock_write);
    trProtectedLockRead  : FTPB := FTPB + chr(isc_tpb_protected) +
      chr(isc_tpb_lock_read);
    trProtectedLockWrite : FTPB := FTPB + chr(isc_tpb_protected) +
      chr(isc_tpb_lock_write);
  end;
end;

function TIBTransaction.GetHandle: pointer;
begin
  Result := FTransactionHandle;
end;

procedure TIBTransaction.Commit;
begin
  if not FActive then Exit;
  if isc_commit_transaction(@FStatus, @FTransactionHandle) <> 0 then
    CheckError('TIBTransaction.Commit', FStatus)
  else FActive := False;
end;

procedure TIBTransaction.CommitRetaining;
begin
  if not FActive then Exit;
  if isc_commit_retaining(@FStatus, @FTransactionHandle) <> 0 then
    CheckError('TIBTransaction.CommitRetaining', FStatus);
end;

procedure TIBTransaction.Rollback;
begin
  if not FActive then Exit;
  if isc_rollback_transaction(@FStatus, @FTransactionHandle) <> 0 then
    CheckError('TIBTransaction.Rollback', FStatus)
  else FActive := False;
end;

procedure TIBTransaction.RollbackRetaining;
begin
  if not FActive then Exit;
  if isc_rollback_retaining(@FStatus, @FTransactionHandle) <> 0 then
    CheckError('TIBTransaction.RollbackRetaining', FStatus);
end;

procedure TIBTransaction.StartTransaction;
var
  DBHandle : pointer;
begin
  if Active then Active := False;

  if FDatabase = nil then
    raise EInterBaseError.Create('TIBTransaction.StartTransaction: Database not assigned!');

  if not Database.Connected then
    Database.Open;

  DBHandle := Database.GetHandle;
  SetTPB;
  FTransactionHandle := nil;

  if isc_start_transaction(@FStatus, @FTransactionHandle, 1,
     [@DBHandle, Length(FTPB), @FTPB[1]]) <> 0 then
    CheckError('TIBTransaction.StartTransaction',FStatus)
  else FActive := True;
end;

constructor TIBTransaction.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIsolationLevel := ilConcurrent;
end;

destructor TIBTransaction.Destroy;
begin
  // This will also do a Rollback, if the transaction is currently active
  Active := False;

  if Database <> nil then
    Database.Transaction := nil;
  inherited Destroy;
end;

{ TIBQuery }

type
  { For now, we could simply say here that TFieldDataPrefix = boolean.
    But making TFieldDataPrefix as record will be allow to very easy add
    similar things like "IsNull" in the future.
    Any information that has constant length, and should be
    specified separately for every field of every row can be added as
    another TFieldDataPrefix field. }
  TFieldDataPrefix = record
    IsNull: boolean;
  end;
  PFieldDataPrefix = ^TFieldDataPrefix;

procedure TIBQuery.SetTransaction(Value : TIBTransaction);
begin
  CheckInactive;
  if (FTransaction <> Value) then
    FTransaction := Value;
end;

procedure TIBQuery.SetDatabase(Value : TIBDatabase);
begin
  CheckInactive;
  if (FDatabase <> Value) then
  begin
    FDatabase := Value;
    if (FTransaction = nil) and (Assigned(FDatabase.Transaction)) then
      SetTransaction(FDatabase.Transaction);
  end;
end;

procedure TIBQuery.AllocSQLDA(Count : integer);
begin
  if FSQLDAAllocated > 0 then
    FreeMem(FSQLDA);
  GetMem(FSQLDA, XSQLDA_Length(Count));
  { Zero out the memory block to avoid problems with exceptions within the
    constructor of this class. }
  FillChar(FSQLDA^, XSQLDA_Length(Count), 0);
  FSQLDAAllocated := Count;
  FSQLDA^.Version := sqlda_version1;
  FSQLDA^.SQLN := Count;
end;

procedure TIBQuery.AllocStatement;
var
  dh : pointer;
begin
  if not FDatabase.Connected then
    FDatabase.Open;
  dh := FDatabase.GetHandle;

  if isc_dsql_allocate_statement(@FStatus, @dh, @FStatement) <> 0 then
    CheckError('TIBQuery.AllocStatement', FStatus);
end;

procedure TIBQuery.FreeStatement;
begin
  if isc_dsql_free_statement(@FStatus, @FStatement, DSQL_Drop) <> 0 then
    CheckError('TIBQuery.FreeStatement', FStatus);
  FStatement := nil;
end;

procedure TIBQuery.PrepareStatement;
var
  Buf : string;
  x   : integer;
  tr  : pointer;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TIBQuery.Execute: Transaction not set');
  if not FTransaction.Active then
    FTransaction.StartTransaction;

  tr := FTransaction.GetHandle;

  for x := 0 to FSQL.Count - 1 do
    Buf := Buf + FSQL[x] + ' ';

  if isc_dsql_prepare(@FStatus, @tr, @FStatement, 0, @Buf[1], Database.Dialect, nil) <> 0 then
    CheckError('TIBQuery.PrepareStatement', FStatus);
end;

procedure TIBQuery.DescribeStatement;
begin
  if isc_dsql_describe(@FStatus, @FStatement, 1, FSQLDA) <> 0 then
    CheckError('TIBQuery.DescribeStatement', FStatus);
  if FSQLDA^.SQLD > FSQLDA^.SQLN then
  begin
    AllocSQLDA(FSQLDA^.SQLD);
    if isc_dsql_describe(@FStatus, @FStatement, 1, FSQLDA) <> 0 then
      CheckError('TIBQuery.DescribeStatement', FStatus);
  end;
end;

procedure TIBQuery.SetUpSQLVars;
var
  x : integer;
begin
  for x := 0 to FSQLDA^.SQLN - 1 do
  begin
    case FSQLDA^.SQLVar[x].SQLType of
      sql_varying + 1:
        FSQLDA^.SQLVar[x].SQLType := sql_varying;
      sql_text + 1   :
        FSQLDA^.SQLVar[x].SQLType := sql_text;
      sql_short, sql_short + 1, sql_long + 1:
        FSQLDA^.SQLVar[x].SQLType := sql_long;
      sql_float + 1  :
        FSQLDA^.SQLVar[x].SQLType := sql_float;
      sql_double + 1 :
        FSQLDA^.SQLVar[x].SQLType := sql_double;
      sql_blob + 1   :
        FSQLDA^.SQLVar[x].SQLType := sql_blob;
      sql_type_time + 1   :
        FSQLDA^.SQLVar[x].SQLType := sql_type_time;
      sql_timestamp + 1:
        FSQLDA^.SQLVar[x].SQLType := sql_timestamp;
    end;
  end;
end;

procedure TIBQuery.AllocFldBuffers;
var
  x  : shortint;
begin
  {$R-}
  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    FSQLDA^.SQLVar[x].SQLData := AllocMem(FSQLDA^.SQLVar[x].SQLLen);
    FSQLDA^.SQLVar[x].SQLInd  := @FFieldFlag[x];
  end;
  {$R+}
end;

procedure TIBQuery.FreeFldBuffers;
var
  x  : integer;
begin
  {$R-}
  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    if FSQLDA^.SQLVar[x].SQLData <> nil then
    begin
      FreeMem(FSQLDA^.SQLVar[x].SQLData);
      FSQLDA^.SQLVar[x].SQLData := nil;
    end;
  end;
  {$R+}
end;

procedure TIBQuery.Fetch;
var
  retcode : integer;
begin
  if not (FStatementType in [stSelect]) then
    Exit;

  retcode := isc_dsql_fetch(@FStatus, @FStatement, 1, FSQLDA);
  if (retcode <> 0) and (retcode <> 100) then
    CheckError('TIBQuery.Fetch', FStatus);

  FIsEOF := (retcode = 100);
end;

function TIBQuery.LoadBufferFromSQLDA(Buffer : PChar): TGetResult;
var
  x          : integer;
  VarcharLen : word;
begin

  Fetch;
  if FIsEOF then
  begin
    Result := grEOF;
    Exit;
  end;

  {$R-}
  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    with FSQLDA^.SQLVar[x] do
    begin
      PFieldDataPrefix(Buffer)^.IsNull :=
        { If 1st bit of SQLType is not set then field *cannot* be null,
          and we shouldn't check SQLInd }
        ((SQLType and 1) <> 0) and (SQLInd^ = -1);
      Inc(Buffer, SizeOf(TFieldDataPrefix));

      if ((SQLType and not 1) = SQL_VARYING) then
      begin
        Move(SQLData^, VarcharLen, 2);
        Move((SQLData + 2)^, Buffer^, VarcharLen);
        PChar(Buffer + VarcharLen)^ := #0;
      end
      else Move(SQLData^, Buffer^, SQLLen);
      Inc(Buffer, SQLLen);
    end;
  end;
  {$R+}
  Result := grOK;

end;

procedure TIBQuery.GetStatementType;
var
  x : integer;
  ResBuf : array [0..7] of char;
begin
  FStatementType := stNone;
  x := isc_info_sql_stmt_type;
  if isc_dsql_sql_info(@FStatus, @FStatement, SizeOf(X),
    @x, SizeOf(ResBuf), @ResBuf) <> 0 then
    CheckError('TIBQuery.GetStatementType', FStatus);
  if Ord(ResBuf[0]) = isc_info_sql_stmt_type then
  begin
    x := isc_vax_integer(@ResBuf[1], 2);
    FStatementType := TStatementType(isc_vax_integer(@ResBuf[3], x));
  end;
end;

procedure TIBQuery.SetFieldSizes;
var
  x : integer;
begin
  FRecordSize := 0;
  {$R-}
  for x := 0 to FSQLDA^.SQLD - 1 do
    Inc(FRecordSize, FSQLDA^.SQLVar[x].SQLLen);
  {$R+}
  Inc(FRecordSize, SizeOf(TFieldDataPrefix) * FSQLDA^.SQLD);

  FBufferSize := FRecordSize + SizeOf(TIBBookmark);
end;

procedure TIBQuery.TranslateFldType(SQLType, SQLLen : integer; var LensSet : boolean;
  var TrType : TFieldType; var TrLen : word);
begin
  LensSet := False;

  case (SQLType and not 1) of
    SQL_VARYING :
      begin
        LensSet := True;
        TrType := ftString;
        TrLen := SQLLen;
      end;
    SQL_TEXT :
      begin
        LensSet := True;
        TrType := ftString;
        TrLen := SQLLen;
      end;
    SQL_TYPE_DATE :
        TrType := ftDateTime;
    SQL_TYPE_TIME :
        TrType := ftDateTime;
    SQL_TIMESTAMP :
        TrType := ftDateTime;
    SQL_ARRAY :
      begin
      end;
    SQL_BLOB :
      begin
      end;
    SQL_SHORT :
      begin
        LensSet := True;
        TrLen := SQLLen;
        TrType := ftInteger;
      end;
    SQL_LONG :
      begin
        LensSet := True;
        TrLen := SQLLen;
        TrType := ftInteger;
      end;
    SQL_INT64 :
        {TrType := ftInt64};
    SQL_DOUBLE :
      begin
        LensSet := True;
        TrLen := SQLLen;
        TrType := ftFloat;
      end;
    SQL_FLOAT :
      begin
        LensSet := True;
        TrLen := SQLLen;
        TrType := ftFloat;
      end;
  end;
end;

procedure TIBQuery.ExecuteImmediate;
begin
end;

procedure TIBQuery.ExecuteParams;
begin
  //!! to be implemented
end;

procedure TIBQuery.Execute;
var
  tr : pointer;
begin
  if FTransaction = nil then
    raise EDatabaseError.Create('TIBQuery.Execute: Transaction not set');
  if not FTransaction.Active then
    FTransaction.StartTransaction;
  tr := FTransaction.GetHandle;
  if isc_dsql_execute(@FStatus, @tr, @FStatement, 1, nil) <> 0 then
    CheckError('TIBQuery.Execute', FStatus);
end;

procedure TIBQuery.GetDateTime(CurrBuff, Buffer : pointer; AType : integer);
var
  CTime : TTm;          // C struct time
  STime : TSystemTime;  // System time
  PTime : TDateTime;    // Pascal time
begin
  case (AType and not 1) of
    SQL_TYPE_DATE :
      isc_decode_sql_date(PISC_DATE(CurrBuff), @CTime);
    SQL_TYPE_TIME :
      isc_decode_sql_time(PISC_TIME(CurrBuff), @CTime);
    SQL_TIMESTAMP :
      isc_decode_timestamp(PISC_TIMESTAMP(CurrBuff), @CTime);
  end;
  STime.Year        := CTime.tm_year + 1900;
  STime.Month       := CTime.tm_mon + 1;
  STime.Day         := CTime.tm_mday;
  STime.Hour        := CTime.tm_hour;
  STime.Minute      := CTime.tm_min;
  STime.Second      := CTime.tm_sec;
  STime.Millisecond := 0;

  PTime := SystemTimeToDateTime(STime);
  Move(PTime, Buffer^, SizeOf(PTime));
end;

procedure TIBQuery.GetFloat(CurrBuff, Buffer : pointer; Field : TField);
var
  Ext : extended;
  Dbl : double;
  Sin : single;
begin
  case Field.Size of
    4 :
      begin
        Move(CurrBuff^, Sin, 4);
        //Ext := Sin;
        Dbl := Sin;
      end;
    8 :
      begin
        Move(CurrBuff^, Dbl, 8);
        //Ext := Dbl;
      end;
    10:
      begin
        Move(CurrBuff^, Ext, 10);
        Dbl := Ext;
      end;
  end;
  //Move(Ext, Buffer^, 10);
  Move(Dbl, Buffer^, 8);
end;

function TIBQuery.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(FBufferSize);
end;

procedure TIBQuery.FreeRecordBuffer(var Buffer: PChar);
begin
  if Assigned(@Buffer) then
    FreeMem(Buffer);
end;

procedure TIBQuery.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PIBBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TIBQuery.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PIBBookmark(Buffer + FRecordSize)^.BookmarkFlag;
end;

function TIBQuery.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  x : longint;
  b : longint;
  CurrBuff : PChar;
begin
  Result := False;
  CurrBuff := ActiveBuffer;

  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    {$R-}
    if (Field.FieldName = FSQLDA^.SQLVar[x].AliasName) then
    begin
      Result := not PFieldDataPrefix(CurrBuff)^.IsNull;

      if Result and (Buffer <> nil) then
      begin
        Inc(CurrBuff, SizeOf(TFieldDataPrefix));
        case Field.DataType of
          ftInteger :
            begin
              b := 0;
              Move(b, Buffer^, 4);
              Move(CurrBuff^, Buffer^, Field.Size);
            end;
          ftDate, ftTime, ftDateTime:
            GetDateTime(CurrBuff, Buffer, FSQLDA^.SQLVar[x].SQLType);
          ftString  :
            begin
              Move(CurrBuff^, Buffer^, Field.Size);
              PChar(Buffer + Field.Size)^ := #0;
            end;
          ftFloat   :
            GetFloat(CurrBuff, Buffer, Field);
        end;
      end;

      Break;
    end
    else Inc(CurrBuff, FSQLDA^.SQLVar[x].SQLLen + SizeOf(TFieldDataPrefix));
    {$R+}
  end;
end;

function TIBQuery.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if FStatementType <> stSelect then
  begin
    Result := grEOF;
    Exit;
  end;
  if FIsEOF then
    Result := grEOF
  else begin
    Result := grOK;
    case GetMode of
      gmPrior :
        if FCurrentRecord <= 0 then
        begin
          Result := grBOF;
          FCurrentRecord := -1;
        end
        else Dec(FCurrentRecord);
      gmCurrent :
        if (FCurrentRecord < 0) or (FCurrentRecord >= RecordCount) then
          Result := grError;
      gmNext :
        if FCurrentRecord >= (RecordCount - 1) then
        begin
          Result := LoadBufferFromSQLDA(Buffer);
          if Result = grOK then
          begin
            Inc(FCurrentRecord);
            Inc(FRecordCount);
          end;
        end
        else Inc(FCurrentRecord);
    end;
  end;

  if Result = grOK then
  begin
    with PIBBookmark(Buffer + FRecordSize)^ do
    begin
      BookmarkData := FCurrentRecord;
      BookmarkFlag := bfCurrent;
    end;
  end
  else if (Result = grError) then
    DatabaseError('No record');
end;

function TIBQuery.GetRecordCount: integer;
begin
  Result := FRecordCount;
end;

function TIBQuery.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

procedure TIBQuery.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
  // not implemented - sql dataset
end;

procedure TIBQuery.InternalClose;
begin
  FreeFldBuffers;
  FreeStatement;
  if DefaultFields then
    DestroyFields;
  FIsEOF := False;
  FCurrentRecord := -1;
  FBufferSize := 0;
  FRecordSize := 0;
  FRecordCount:= 0;
  FOpen:=False;
end;

procedure TIBQuery.InternalDelete;
begin
  // not implemented - sql dataset
end;

procedure TIBQuery.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TIBQuery.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecord := PInteger(ABookmark)^;
end;

procedure TIBQuery.InternalHandleException;
begin
end;

procedure TIBQuery.InternalInitFieldDefs;
var
  x         : integer;
  lenset    : boolean;
  TransLen  : word;
  TransType : TFieldType;
begin
  if FLoadingFieldDefs then
    Exit;

  FLoadingFieldDefs := True;

  try
    FieldDefs.Clear;
    {$R-}
    for x := 0 to FSQLDA^.SQLD - 1 do
    begin
      TranslateFldType(FSQLDA^.SQLVar[x].SQLType, FSQLDA^.SQLVar[x].SQLLen, lenset,
        TransType, TransLen);
      TFieldDef.Create(FieldDefs, FSQLDA^.SQLVar[x].AliasName, TransType,
        TransLen, False, (x + 1));
    end;
    {$R+}
  finally
    FLoadingFieldDefs := False;
  end;
end;

procedure TIBQuery.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FBufferSize, #0);
end;

procedure TIBQuery.InternalLast;
begin
  FCurrentRecord := RecordCount;
end;

procedure TIBQuery.InternalOpen;
begin
  try
    AllocStatement;
    PrepareStatement;
    GetStatementType;
    if FStatementType in [stSelect] then
    begin
      DescribeStatement;
      AllocFldBuffers;
      Execute;
      FOpen:=True;
      InternalInitFieldDefs;
      if DefaultFields then
        CreateFields;
      SetFieldSizes;
      BindFields(True);
    end
    else Execute;
  except
    on E:Exception do
      raise;
  end;
end;

procedure TIBQuery.InternalPost;
begin
  // not implemented - sql dataset
end;

procedure TIBQuery.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentRecord := PIBBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TIBQuery.IsCursorOpen: Boolean;
begin
  Result := FOpen;
end;

procedure TIBQuery.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PIBBookmark(Buffer + FRecordSize)^.BookmarkFlag := Value;
end;

procedure TIBQuery.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PIBBookmark(Buffer + FRecordSize)^.BookmarkData := PInteger(Data)^;
end;

procedure TIBQuery.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

// public part

procedure TIBQuery.ExecSQL;
begin
  AllocStatement;
  try
    PrepareStatement;
    GetStatementType;
    Execute;
  finally
    FreeStatement;
  end;
end;

constructor TIBQuery.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FCurrentRecord := -1;
  AllocSQLDA(10);
end;

destructor TIBQuery.Destroy;
begin
  if Active then Close;
  FSQL.Free;
  inherited Destroy;
  FreeMem(FSQLDA);
end;

{ TIBStoredProc }

end.
