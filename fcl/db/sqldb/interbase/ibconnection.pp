unit IBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBase60, sqldb, db;
  
type
  TIBCursor = record
                Status    : array [0..19] of ISC_STATUS;
                Statement : pointer;
                SQLDA     : PXSQLDA;
              end;
  PIBCursor = ^TIBCursor;

  TAccessMode = (amReadWrite, amReadOnly);
  TIsolationLevel = (ilConcurrent, ilConsistent, ilReadCommittedRecV,
    ilReadCommitted);
  TLockResolution = (lrWait, lrNoWait);
  TTableReservation = (trNone, trSharedLockRead, trSharedLockWrite,
    trProtectedLockRead, trProtectedLockWrite);

  TIBTrans  = record
                TransactionHandle   : pointer;
                TPB                 : string;                // Transaction parameter buffer
                Status              : array [0..19] of ISC_STATUS;
                AccessMode          : TAccessMode;
                IsolationLevel      : TIsolationLevel;
                LockResolution      : TLockResolution;
                TableReservation    : TTableReservation;
              end;
  PIBTrans = ^TIBTrans;

  TIBConnection = class (TSQLConnection)
  private
    FSQLDAAllocated      : integer;
    FSQLDatabaseHandle   : pointer;
    FStatus              : array [0..19] of ISC_STATUS;
    FFieldFlag           : array [0..1023] of shortint;
    FDialect             : integer;
    procedure SetDBDialect;
    procedure AllocSQLDA(Cursor : pointer;Count : integer);
    procedure TranslateFldType(SQLType, SQLLen : integer; var LensSet : boolean;
      var TrType : TFieldType; var TrLen : word);
    procedure SetTPB(trans : pointer);
    // conversion methods
    procedure GetDateTime(CurrBuff, Buffer : pointer; AType : integer);
    procedure GetFloat(CurrBuff, Buffer : pointer; Field : TField);
  protected
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

  public
    function GetCursor : pointer; override;
    procedure FreeCursor(cursor : pointer); override;
    function GetTrans : pointer; override;
    procedure FreeTrans(trans : pointer); override;
    procedure AllocStatement(cursor : Pointer); override;
    procedure FreeStatement(cursor : pointer); override;
    procedure PrepareStatement(cursor: pointer;ATransaction : TSQLTransaction;buf : string); override;
    procedure DescribeStatement(cursor : pointer); override;
    procedure AllocFldBuffers(cursor : pointer); override;
    procedure FreeFldBuffers(cursor : pointer); override;
    procedure Execute(cursor: pointer;atransaction:tSQLtransaction); override;
    procedure AddFieldDefs(cursor: pointer; FieldDefs : TfieldDefs); override;
    function GetFieldSizes(cursor : pointer) : integer; override;
    function Fetch(cursor : pointer) : boolean; override;
    procedure LoadFieldsFromBuffer(cursor : pointer;buffer: pchar); override;
    function GetFieldData(cursor : pointer; Field: TField; Buffer: Pointer;currbuff:pchar): Boolean; override;
    function GetStatementType(cursor : pointer) : tStatementType; override;
    function GetTransactionHandle(trans : pointer): pointer; override;
    function Commit(trans : pointer) : boolean; override;
    function RollBack(trans : pointer) : boolean; override;
    function StartTransaction(trans : pointer) : boolean; override;
    procedure CommitRetaining(trans : pointer); override;
    procedure RollBackRetaining(trans : pointer); override;

  published
    property Dialect  : integer read FDialect write FDialect;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
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
    raise ESQLdbError.Create(ProcName + ': ' + Msg);
  end;
end;

procedure TIBConnection.SetTPB(trans : pointer);
begin
  with PIBTrans(trans)^ do
    begin
    TPB := chr(isc_tpb_version3);

    case PIBTrans(trans)^.AccessMode of
      amReadWrite : TPB := TPB + chr(isc_tpb_write);
      amReadOnly  : TPB := TPB + chr(isc_tpb_read);
    end;

    case PIBTrans(trans)^.IsolationLevel of
      ilConsistent        : TPB := TPB + chr(isc_tpb_consistency);
      ilConcurrent        : TPB := TPB + chr(isc_tpb_concurrency);
      ilReadCommittedRecV : TPB := TPB + chr(isc_tpb_read_committed) +
        chr(isc_tpb_rec_version);
      ilReadCommitted     : TPB := TPB + chr(isc_tpb_read_committed) +
        chr(isc_tpb_no_rec_version);
    end;

    case PIBTrans(trans)^.LockResolution of
      lrWait   : TPB := TPB + chr(isc_tpb_wait);
      lrNoWait : TPB := TPB + chr(isc_tpb_nowait);
    end;

    case PIBTrans(trans)^.TableReservation of
      trSharedLockRead     : TPB := TPB + chr(isc_tpb_shared) +
        chr(isc_tpb_lock_read);
      trSharedLockWrite    : TPB := TPB + chr(isc_tpb_shared) +
        chr(isc_tpb_lock_write);
      trProtectedLockRead  : TPB := TPB + chr(isc_tpb_protected) +
        chr(isc_tpb_lock_read);
      trProtectedLockWrite : TPB := TPB + chr(isc_tpb_protected) +
        chr(isc_tpb_lock_write);
    end;
    end;
end;

function TIBConnection.GetTransactionHandle(trans : pointer): pointer;
begin
  Result := PIBTrans(trans)^.TransactionHandle;
end;

function TIBConnection.Commit(trans : pointer) : boolean;
begin
  result := false;
  if isc_commit_transaction(@PIBTrans(trans)^.Status, @PIBTrans(trans)^.TransactionHandle) <> 0 then
    CheckError('TSQLTransaction.Commit', PIBTrans(trans)^.Status)
  else result := true;
end;

function TIBConnection.RollBack(trans : pointer) : boolean;
begin
  result := false;
  if isc_rollback_transaction(@PIBTrans(trans)^.Status, @PIBTrans(trans)^.TransactionHandle) <> 0 then
    CheckError('TIBConnection.Rollback', PIBTrans(trans)^.Status)
  else result := true;
end;

function TIBConnection.StartTransaction(trans : pointer) : boolean;
var
  DBHandle : pointer;
begin
  result := false;

  DBHandle := GetHandle;
  SetTPB(trans);
  pibtrans(trans)^.TransactionHandle := nil;

  if isc_start_transaction(@pibtrans(trans)^.Status, @pibtrans(trans)^.TransactionHandle, 1,
     [@DBHandle, Length(pibtrans(trans)^.TPB), @pibtrans(trans)^.TPB[1]]) <> 0 then
    CheckError('TIBConnection.StartTransaction',pibtrans(trans)^.Status)
  else Result := True;
end;


procedure TIBConnection.CommitRetaining(trans : pointer);
begin
  if isc_commit_retaining(@PIBTrans(trans)^.Status, @PIBTrans(trans)^.TransactionHandle) <> 0 then
    CheckError('TIBConnection.CommitRetaining', PIBTrans(trans)^.Status);
end;

procedure TIBConnection.RollBackRetaining(trans : pointer);
begin
  if isc_rollback_retaining(@PIBTrans(trans)^.Status, @PIBTrans(trans)^.TransactionHandle) <> 0 then
    CheckError('TIBConnection.RollBackRetaining', PIBTrans(trans)^.Status);
end;

function TIBConnection.GetTrans : pointer;

begin
  Result := AllocMem(sizeof(TIBTrans));
  PIBTrans(result)^.IsolationLevel := ilReadCommitted;
end;

procedure TIBConnection.FreeTrans(trans : pointer);

begin
  if assigned(PIBTrans(trans)) then
    freemem(PIBTrans(trans));
end;


procedure TIBConnection.DoInternalConnect;
var
  DPB : string;
begin
  inherited dointernalconnect;

  DPB := chr(isc_dpb_version1);
  if (UserName <> '') then
  begin
    DPB := DPB + chr(isc_dpb_user_name) + chr(Length(UserName)) + UserName;
    if (Password <> '') then
      DPB := DPB + chr(isc_dpb_password) + chr(Length(Password)) + Password;
  end;
  if (Role <> '') then
     DPB := DPB + chr(isc_dpb_sql_role_name) + chr(Length(Role)) + Role;
  if Length(CharSet) > 0 then
    DPB := DPB + Chr(isc_dpb_lc_ctype) + Chr(Length(CharSet)) + CharSet;

  if (DatabaseName = '') then
    raise ESQLdbError.Create('TIBConnection.DoInternalConnect: Database connect string (DatabaseName) not filled in!');
  FSQLDatabaseHandle := nil;
  if isc_attach_database(@FStatus, Length(DatabaseName), @DatabaseName[1], @FSQLDatabaseHandle,
         Length(DPB), @DPB[1]) <> 0 then
    CheckError('TIBConnection.DoInternalConnect', FStatus);
  SetDBDialect;
end;

procedure TIBConnection.DoInternalDisconnect;
begin
  if not Connected then
  begin
    FSQLDatabaseHandle := nil;
    Exit;
  end;

  isc_detach_database(@FStatus[0], @FSQLDatabaseHandle);
  CheckError('TIBConnection.Close', FStatus);
end;


procedure TIBConnection.SetDBDialect;
var
  x : integer;
  Len : integer;
  Buffer : string;
  ResBuf : array [0..39] of byte;
begin
  Buffer := Chr(isc_info_db_sql_dialect) + Chr(isc_info_end);
  if isc_database_info(@FStatus, @FSQLDatabaseHandle, Length(Buffer),
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


procedure TIBConnection.AllocSQLDA(Cursor : pointer;Count : integer);
begin
  if FSQLDAAllocated > 0 then
    FreeMem(PIBCursor(cursor)^.SQLDA);
  GetMem(PIBCursor(cursor)^.SQLDA, XSQLDA_Length(Count));
  { Zero out the memory block to avoid problems with exceptions within the
    constructor of this class. }
  FillChar(PIBCursor(cursor)^.SQLDA^, XSQLDA_Length(Count), 0);
  FSQLDAAllocated := Count;
  PIBCursor(cursor)^.SQLDA^.Version := sqlda_version1;
  PIBCursor(cursor)^.SQLDA^.SQLN := Count;
end;

procedure TIBConnection.TranslateFldType(SQLType, SQLLen : integer; var LensSet : boolean;
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

function TIBConnection.GetCursor : pointer;

begin
  Result := AllocMem(sizeof(TIBCursor));
  AllocSQLDA(result,10);
end;

procedure TIBConnection.FreeCursor(cursor : pointer);

begin
  if assigned(PIBCursor(cursor)) then
    freemem(PIBCursor(cursor));
end;

procedure TIBConnection.FreeStatement(cursor : pointer);
begin
  if isc_dsql_free_statement(@PIBCursor(cursor)^.Status, @PIBCursor(cursor)^.Statement, DSQL_Drop) <> 0 then
    CheckError('TIBConnection.FreeStatement', PIBCursor(cursor)^.Status);
  PIBCursor(cursor)^.Statement := nil;
end;

procedure TIBConnection.AllocStatement(cursor : pointer);
var
  dh    : pointer;
begin
  dh := GetHandle;

  if isc_dsql_allocate_statement(@PIBCursor(cursor)^.Status, @dh, @PIBCursor(cursor)^.Statement) <> 0 then
    CheckError('TIBConnection.AllocStatement', PIBCursor(cursor)^.Status);
end;


procedure TIBConnection.PrepareStatement(cursor: pointer;ATransaction : TSQLTransaction;buf : string);

var tr : pointer;

begin
  tr := aTransaction.Handle;

  if isc_dsql_prepare(@PIBCursor(cursor)^.Status, @tr, @PIBCursor(cursor)^.Statement, 0, @Buf[1], Dialect, nil) <> 0 then
    CheckError('TIBConnection.PrepareStatement', PIBCursor(cursor)^.Status);
end;

procedure TIBConnection.DescribeStatement(cursor : pointer);

begin
  with PIBCursor(cursor)^ do
    begin
    if isc_dsql_describe(@Status, @Statement, 1, SQLDA) <> 0 then
      CheckError('TSQLQuery.DescribeStatement', Status);
    if SQLDA^.SQLD > SQLDA^.SQLN then
      begin
      AllocSQLDA(PIBCursor(cursor),SQLDA^.SQLD);
      if isc_dsql_describe(@Status, @Statement, 1, SQLDA) <> 0 then
        CheckError('TSQLQuery.DescribeStatement', Status);
      end;
    end;
end;

procedure TIBConnection.FreeFldBuffers(cursor : pointer);
var
  x  : shortint;
begin
  {$R-}
  for x := 0 to PIBCursor(cursor)^.SQLDA^.SQLD - 1 do
  begin
    if PIBCursor(cursor)^.SQLDA^.SQLVar[x].SQLData <> nil then
    begin
      FreeMem(PIBCursor(cursor)^.SQLDA^.SQLVar[x].SQLData);
      PIBCursor(cursor)^.SQLDA^.SQLVar[x].SQLData := nil;
    end;
  end;
  {$R+}
end;


procedure TIBConnection.AllocFldBuffers(cursor : pointer);
var
  x  : shortint;
begin
  {$R-}
  for x := 0 to PIBCursor(cursor)^.SQLDA^.SQLD - 1 do
  begin
    PIBCursor(cursor)^.SQLDA^.SQLVar[x].SQLData := AllocMem(PIBCursor(cursor)^.SQLDA^.SQLVar[x].SQLLen);
    PIBCursor(cursor)^.SQLDA^.SQLVar[x].SQLInd  := @FFieldFlag[x];
  end;
  {$R+}
end;

procedure TIBConnection.Execute(cursor: pointer;atransaction:tSQLtransaction);
var tr : pointer;
begin
  tr := aTransaction.Handle;

  if isc_dsql_execute(@PIBCursor(cursor)^.Status, @tr, @PIBCursor(cursor)^.Statement, 1, nil) <> 0 then
    CheckError('TSQLQuery.Execute', PIBCursor(cursor)^.Status);
end;

procedure TIBConnection.AddFieldDefs(cursor: pointer; FieldDefs : TfieldDefs);
var
  x         : integer;
  lenset    : boolean;
  TransLen  : word;
  TransType : TFieldType;

begin
  {$R-}
  with PIBCursor(cursor)^ do
    begin
    for x := 0 to SQLDA^.SQLD - 1 do
      begin
      TranslateFldType(SQLDA^.SQLVar[x].SQLType, SQLDA^.SQLVar[x].SQLLen, lenset,
        TransType, TransLen);
      TFieldDef.Create(FieldDefs, SQLDA^.SQLVar[x].SQLName, TransType,
        TransLen, False, (x + 1));
      end;
    end;
  {$R+}
end;

function TIBConnection.GetFieldSizes(cursor : pointer) : integer;
var
  x,recsize : integer;
begin
  recsize := 0;
  {$R-}
  with PIBCursor(cursor)^ do
    for x := 0 to SQLDA^.SQLD - 1 do
      Inc(recsize, SQLDA^.SQLVar[x].SQLLen);
  {$R+}
  result := recsize;
end;

function TIBConnection.GetHandle: pointer;
begin
  Result := FSQLDatabaseHandle;
end;

function TIBConnection.Fetch(cursor : pointer) : boolean;
var
  retcode : integer;
begin
  retcode := isc_dsql_fetch(@PIBCursor(cursor)^.Status, @PIBCursor(cursor)^.Statement, 1, PIBCursor(cursor)^.SQLDA);
  if (retcode <> 0) and (retcode <> 100) then
    CheckError('TSQLQuery.Fetch', PIBCursor(cursor)^.Status);

  Result := (retcode = 100);
end;

procedure TIBConnection.LoadFieldsFromBuffer(cursor : pointer;buffer : pchar);
var
  x          : integer;
  VarcharLen : word;
begin
  {$R-}
  with PIBCursor(cursor)^ do for x := 0 to SQLDA^.SQLD - 1 do
    begin
    with SQLDA^.SQLVar[x] do
      begin
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
end;

function TIBConnection.GetFieldData(Cursor : pointer;Field: TField; Buffer: Pointer;currbuff : pchar): Boolean;
var
  x : longint;
  b : longint;
begin
  Result := False;

  with PIBCursor(cursor)^ do
    for x := 0 to SQLDA^.SQLD - 1 do
  begin
    {$R-}
    if (Field.FieldName = SQLDA^.SQLVar[x].SQLName) then
    begin
      case Field.DataType of
        ftInteger :
          begin
            b := 0;
            Move(b, Buffer^, 4);
            Move(CurrBuff^, Buffer^, Field.Size);
          end;
        ftDate, ftTime, ftDateTime:
          GetDateTime(CurrBuff, Buffer, SQLDA^.SQLVar[x].SQLType);
        ftString  :
          begin
            Move(CurrBuff^, Buffer^, Field.Size);
            PChar(Buffer + Field.Size)^ := #0;
          end;
        ftFloat   :
          GetFloat(CurrBuff, Buffer, Field);
      end;

      Result := True;

      Break;
    end
    else Inc(CurrBuff, SQLDA^.SQLVar[x].SQLLen);
    {$R+}
  end;
end;

procedure TIBConnection.GetDateTime(CurrBuff, Buffer : pointer; AType : integer);
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

procedure TIBConnection.GetFloat(CurrBuff, Buffer : pointer; Field : TField);
var
  Ext : extended;
  Dbl : double;
  Sin : single;
begin
  case Field.Size of
    4 :
      begin
        Move(CurrBuff^, Sin, 4);
        Ext := Sin;
      end;
    8 :
      begin
        Move(CurrBuff^, Dbl, 8);
        Ext := Dbl;
      end;
    10: Move(CurrBuff^, Ext, 10);
  end;
  Move(Ext, Buffer^, 10);
end;

function TIBConnection.GetStatementType(cursor : pointer) : TStatementType;
var
  x : integer;
  ResBuf : array [0..7] of char;
begin
  Result := stNone;
  x := isc_info_sql_stmt_type;
  if isc_dsql_sql_info(@PIBCursor(cursor)^.Status, @PIBCursor(cursor)^.Statement, SizeOf(X),
    @x, SizeOf(ResBuf), @ResBuf) <> 0 then
    CheckError('TIBConnection.GetStatementType', PIBCursor(cursor)^.Status);
  if Ord(ResBuf[0]) = isc_info_sql_stmt_type then
  begin
    x := isc_vax_integer(@ResBuf[1], 2);
    Result := TStatementType(isc_vax_integer(@ResBuf[3], x));
  end;
end;

end.

