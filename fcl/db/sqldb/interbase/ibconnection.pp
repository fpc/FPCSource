unit IBConnection;

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils, sqldb, db, math, dbconst,
{$IfDef LinkDynamically}
  ibase60dyn;
{$Else}
  ibase60;
{$EndIf}

type
  TAccessMode = (amReadWrite, amReadOnly);
  TIsolationLevel = (ilConcurrent, ilConsistent, ilReadCommittedRecV,
    ilReadCommitted);
  TLockResolution = (lrWait, lrNoWait);
  TTableReservation = (trNone, trSharedLockRead, trSharedLockWrite,
    trProtectedLockRead, trProtectedLockWrite);

  TIBCursor = Class(TSQLHandle)
    protected
    Status               : array [0..19] of ISC_STATUS;
    Statement            : pointer;
    FFieldFlag           : array of shortint;
    SQLDA                : PXSQLDA;
  end;

  TIBTrans = Class(TSQLHandle)
    protected
    TransactionHandle   : pointer;
    TPB                 : string;                // Transaction parameter buffer
    Status              : array [0..19] of ISC_STATUS;
    AccessMode          : TAccessMode;
    IsolationLevel      : TIsolationLevel;
    LockResolution      : TLockResolution;
    TableReservation    : TTableReservation;
  end;

  TIBConnection = class (TSQLConnection)
  private
    FSQLDatabaseHandle   : pointer;
    FStatus              : array [0..19] of ISC_STATUS;
    FDialect             : integer;

    procedure SetDBDialect;
    procedure AllocSQLDA(Cursor : TIBCursor;Count : integer);
    procedure TranslateFldType(SQLType, SQLLen, SQLScale : integer; var LensSet : boolean;
      var TrType : TFieldType; var TrLen : word);
    procedure SetTPB(trans : TIBtrans);
    // conversion methods
    procedure GetDateTime(CurrBuff, Buffer : pointer; AType : integer);
    procedure GetFloat(CurrBuff, Buffer : pointer; Field : TFieldDef);
    procedure CheckError(ProcName : string; Status : array of ISC_STATUS);
  protected
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    Function AllocateCursorHandle : TSQLHandle; override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure FreeStatement(cursor : TSQLHandle); override;
    procedure PrepareStatement(cursor: TSQLHandle;ATransaction : TSQLTransaction;buf : string); override;
    procedure FreeFldBuffers(cursor : TSQLHandle); override;
    procedure Execute(cursor: TSQLHandle;atransaction:tSQLtransaction); override;
    procedure AddFieldDefs(cursor: TSQLHandle;FieldDefs : TfieldDefs); override;
    function Fetch(cursor : TSQLHandle) : boolean; override;
    function LoadField(cursor : TSQLHandle;FieldDef : TfieldDef;buffer : pointer) : boolean; override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartdbTransaction(trans : TSQLHandle) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    procedure UpdateIndexDefs(var IndexDefs : TIndexDefs;TableName : string); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
  published
    property Dialect  : integer read FDialect write FDialect;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

implementation

resourcestring
  SErrNoDatabaseName = 'Database connect string (DatabaseName) not filled in!';

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

procedure TIBConnection.CheckError(ProcName : string; Status : array of ISC_STATUS);
var
  buf : array [0..1024] of char;
  p   : pointer;
  Msg : string;
begin
  if ((Status[0] = 1) and (Status[1] <> 0)) then
  begin
    p := @Status;
    msg := '';
    while isc_interprete(Buf, @p) > 0 do
      Msg := Msg + #10' -' + StrPas(Buf);
    DatabaseError(ProcName + ': ' + Msg,self);
  end;
end;

procedure TIBConnection.SetTPB(trans : TIBtrans);
begin
  with trans do
    begin
    TPB := chr(isc_tpb_version3);

    case AccessMode of
      amReadWrite : TPB := TPB + chr(isc_tpb_write);
      amReadOnly  : TPB := TPB + chr(isc_tpb_read);
    end;

    case IsolationLevel of
      ilConsistent        : TPB := TPB + chr(isc_tpb_consistency);
      ilConcurrent        : TPB := TPB + chr(isc_tpb_concurrency);
      ilReadCommittedRecV : TPB := TPB + chr(isc_tpb_read_committed) +
        chr(isc_tpb_rec_version);
      ilReadCommitted     : TPB := TPB + chr(isc_tpb_read_committed) +
        chr(isc_tpb_no_rec_version);
    end;

    case LockResolution of
      lrWait   : TPB := TPB + chr(isc_tpb_wait);
      lrNoWait : TPB := TPB + chr(isc_tpb_nowait);
    end;

    case TableReservation of
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

function TIBConnection.GetTransactionHandle(trans : TSQLHandle): pointer;
begin
  Result := (trans as TIBtrans).TransactionHandle;
end;

function TIBConnection.Commit(trans : TSQLHandle) : boolean;
begin
  result := false;
  with (trans as TIBTrans) do
    if isc_commit_transaction(@Status, @TransactionHandle) <> 0 then
      CheckError('Commit', Status)
    else result := true;
end;

function TIBConnection.RollBack(trans : TSQLHandle) : boolean;
begin
  result := false;
  if isc_rollback_transaction(@TIBTrans(trans).Status, @TIBTrans(trans).TransactionHandle) <> 0 then
    CheckError('Rollback', TIBTrans(trans).Status)
  else result := true;
end;

function TIBConnection.StartDBTransaction(trans : TSQLHandle) : boolean;
var
  DBHandle : pointer;
  tr       : TIBTrans;
begin
  result := false;

  DBHandle := GetHandle;
  tr := trans as TIBtrans;
  SetTPB(tr);
  with tr do
    begin
    TransactionHandle := nil;

    if isc_start_transaction(@Status, @TransactionHandle, 1,
       [@DBHandle, Length(TPB), @TPB[1]]) <> 0 then
      CheckError('StartTransaction',Status)
    else Result := True;
    end;
end;


procedure TIBConnection.CommitRetaining(trans : TSQLHandle);
begin
  with trans as TIBtrans do
    if isc_commit_retaining(@Status, @TransactionHandle) <> 0 then
      CheckError('CommitRetaining', Status);
end;

procedure TIBConnection.RollBackRetaining(trans : TSQLHandle);
begin
  with trans as TIBtrans do
    if isc_rollback_retaining(@Status, @TransactionHandle) <> 0 then
      CheckError('RollBackRetaining', Status);
end;


procedure TIBConnection.DoInternalConnect;
var
  DPB : string;
begin
{$IfDef LinkDynamically}
  InitialiseIBase60;
{$EndIf}
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
    DatabaseError(SErrNoDatabaseName,self);

  FSQLDatabaseHandle := nil;
  if isc_attach_database(@FStatus, Length(DatabaseName), @DatabaseName[1], @FSQLDatabaseHandle,
         Length(DPB), @DPB[1]) <> 0 then
    CheckError('DoInternalConnect', FStatus);
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
  CheckError('Close', FStatus);
{$IfDef LinkDynamically}
  ReleaseIBase60;
{$EndIf}

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
      CheckError('SetDBDialect', FStatus);
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


procedure TIBConnection.AllocSQLDA(Cursor : TIBcursor;Count : integer);

begin
  with cursor as TIBCursor do
    begin
    reAllocMem(SQLDA, XSQLDA_Length(Count));
    { Zero out the memory block to avoid problems with exceptions within the
      constructor of this class. }
    FillChar(SQLDA^, XSQLDA_Length(Count), 0);
    SQLDA^.Version := sqlda_version1;
    SQLDA^.SQLN := Count;
    end;
end;

procedure TIBConnection.TranslateFldType(SQLType, SQLLen, SQLScale : integer; var LensSet : boolean;
  var TrType : TFieldType; var TrLen : word);
begin
  LensSet := False;

  if (SQLScale >= -4) and (SQLScale <= -1) then //in [-4..-1] then
    begin
    LensSet := True;
    TrLen := SQLScale;
    TrType := ftBCD
    end
  else case (SQLType and not 1) of
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
      TrType := ftDate{Time};
    SQL_TYPE_TIME :
        TrType := ftDateTime;
    SQL_TIMESTAMP :
        TrType := ftDateTime;
    SQL_ARRAY :
      begin
        TrType := ftArray;
        LensSet := true;
        TrLen := SQLLen;
      end;
    SQL_BLOB :
      begin
          TrType := ftBlob;
          LensSet := True;
          TrLen := SQLLen;
      end;
    SQL_SHORT :
        TrType := ftInteger;
    SQL_LONG :
      begin
        LensSet := True;
        TrLen := 0;
        TrType := ftInteger;
      end;
    SQL_INT64 :
        TrType := ftLargeInt;
    SQL_DOUBLE :
      begin
        LensSet := True;
        TrLen := 0;
        TrType := ftFloat;
      end;
    SQL_FLOAT :
      begin
        LensSet := True;
        TrLen := 0;
        TrType := ftFloat;
      end
    else
      begin
        LensSet := True;
        TrLen := 0;
        TrType := ftUnknown;
      end;
  end;
end;

Function TIBConnection.AllocateCursorHandle : TSQLHandle;

var curs : TIBCursor;

begin
  curs := TIBCursor.create;
  curs.sqlda := nil;
  curs.statement := nil;
  AllocSQLDA(curs,1);
  result := curs;
end;

Function TIBConnection.AllocateTransactionHandle : TSQLHandle;

begin
  result := TIBTrans.create;
end;

procedure TIBConnection.FreeStatement(cursor : TSQLHandle);
begin
  with cursor as TIBcursor do
    begin
    if isc_dsql_free_statement(@Status, @Statement, DSQL_Drop) <> 0 then
      CheckError('FreeStatement', Status);
    Statement := nil;
    end;
  reAllocMem((cursor as tibcursor).SQLDA,0);
end;

procedure TIBConnection.PrepareStatement(cursor: TSQLHandle;ATransaction : TSQLTransaction;buf : string);

var dh    : pointer;
    tr    : pointer;
    x     : shortint;

begin
  with cursor as TIBcursor do
    begin
    dh := GetHandle;
    if isc_dsql_allocate_statement(@Status, @dh, @Statement) <> 0 then
      CheckError('PrepareStatement', Status);
    tr := aTransaction.Handle;
    if isc_dsql_prepare(@Status, @tr, @Statement, 0, @Buf[1], Dialect, nil) <> 0 then
      CheckError('PrepareStatement', Status);
    if StatementType = stselect then
      begin
      if isc_dsql_describe(@Status, @Statement, 1, SQLDA) <> 0 then
        CheckError('PrepareSelect', Status);
      if SQLDA^.SQLD > SQLDA^.SQLN then
        begin
        AllocSQLDA((cursor as TIBCursor),SQLDA^.SQLD);
        if isc_dsql_describe(@Status, @Statement, 1, SQLDA) <> 0 then
          CheckError('PrepareSelect', Status);
        end;
      {$R-}
      SetLength(FFieldFlag,SQLDA^.SQLD);
      for x := 0 to SQLDA^.SQLD - 1 do with SQLDA^.SQLVar[x] do
        begin
        if ((SQLType and not 1) = SQL_VARYING) then
          SQLData := AllocMem(SQLDA^.SQLVar[x].SQLLen+2)
        else
          SQLData := AllocMem(SQLDA^.SQLVar[x].SQLLen);
        SQLInd  := @FFieldFlag[x];
        end;
      {$R+}
      end;
    end;
end;

procedure TIBConnection.FreeFldBuffers(cursor : TSQLHandle);
var
  x  : shortint;
begin
  {$R-}
  with cursor as TIBCursor do
    for x := 0 to SQLDA^.SQLD - 1 do
      reAllocMem(SQLDA^.SQLVar[x].SQLData,0);
  {$R+}
end;

procedure TIBConnection.Execute(cursor: TSQLHandle;atransaction:tSQLtransaction);
var tr : pointer;
begin
  tr := aTransaction.Handle;

  with cursor as TIBCursor do
    if isc_dsql_execute(@Status, @tr, @Statement, 1, nil) <> 0 then
      CheckError('Execute', Status);
end;

procedure TIBConnection.AddFieldDefs(cursor: TSQLHandle;FieldDefs : TfieldDefs);
var
  x         : integer;
  lenset    : boolean;
  TransLen  : word;
  TransType : TFieldType;
  FD        : TFieldDef;

begin
  {$R-}
  with cursor as TIBCursor do
    begin
    for x := 0 to SQLDA^.SQLD - 1 do
      begin
      TranslateFldType(SQLDA^.SQLVar[x].SQLType, SQLDA^.SQLVar[x].SQLLen, SQLDA^.SQLVar[x].SQLScale,
        lenset, TransType, TransLen);
      FD := TFieldDef.Create(FieldDefs, SQLDA^.SQLVar[x].SQLName, TransType,
         TransLen, False, (x + 1));
      if TransType = ftBCD then FD.precision := SQLDA^.SQLVar[x].SQLLen;
      FD.DisplayName := SQLDA^.SQLVar[x].AliasName;
      end;
    end;
  {$R+}
end;

function TIBConnection.GetHandle: pointer;
begin
  Result := FSQLDatabaseHandle;
end;

function TIBConnection.Fetch(cursor : TSQLHandle) : boolean;
var
  retcode : integer;
begin
  with cursor as TIBCursor do
    begin
    retcode := isc_dsql_fetch(@Status, @Statement, 1, SQLDA);
    if (retcode <> 0) and (retcode <> 100) then
      CheckError('Fetch', Status);
    end;
  Result := (retcode <> 100);
end;

function TIBConnection.LoadField(cursor : TSQLHandle;FieldDef : TfieldDef;buffer : pointer) : boolean;

var
  x          : integer;
  VarcharLen : word;
  CurrBuff     : pchar;
  b            : longint;
  li           : largeint;
  c            : currency;

begin
  with cursor as TIBCursor do
    begin
{$R-}
    for x := 0 to SQLDA^.SQLD - 1 do
      if SQLDA^.SQLVar[x].AliasName = FieldDef.Name then break;

    if SQLDA^.SQLVar[x].AliasName <> FieldDef.Name then
      DatabaseErrorFmt(SFieldNotFound,[FieldDef.Name],self);

    if SQLDA^.SQLVar[x].SQLInd^ = -1 then
      result := false
    else
      begin

      with SQLDA^.SQLVar[x] do
        if ((SQLType and not 1) = SQL_VARYING) then
          begin
          Move(SQLData^, VarcharLen, 2);
          CurrBuff := SQLData + 2;
          end
        else
          begin
          CurrBuff := SQLData;
          VarCharLen := SQLDA^.SQLVar[x].SQLLen;
          end;

      Result := true;
      case FieldDef.DataType of
        ftBCD :
          begin
            c := 0;
            Move(CurrBuff^, c, SQLDA^.SQLVar[x].SQLLen);
            c := c*intpower(10,4+SQLDA^.SQLVar[x].SQLScale);
            Move(c, buffer^ , sizeof(c));
          end;
        ftInteger :
          begin
            b := 0;
            Move(b, Buffer^, sizeof(longint));
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
          end;
        ftLargeint :
          begin
            li := 0;
            Move(li, Buffer^, sizeof(largeint));
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
          end;
        ftDate, ftTime, ftDateTime:
          GetDateTime(CurrBuff, Buffer, SQLDA^.SQLVar[x].SQLType);
        ftString  :
          begin
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
            PChar(Buffer + VarCharLen)^ := #0;
          end;
        ftFloat   :
          GetFloat(CurrBuff, Buffer, FieldDef)
      else result := false;
      end;
      end;
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

function TIBConnection.GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string;

var s : string;

begin
  case SchemaType of
    stTables     : s := 'select '+
                          'rdb$relation_id          as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'rdb$relation_name        as table_name, '+
                          '0                        as table_type '+
                        'from '+
                          'rdb$relations '+
                        'where '+
                          '(rdb$system_flag = 0 or rdb$system_flag is null)'; // and rdb$view_blr is null
    stSysTables  : s := 'select '+
                          'rdb$relation_id          as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'rdb$relation_name        as table_name, '+
                          '0                        as table_type '+
                        'from '+
                          'rdb$relations '+
                        'where '+
                          '(rdb$system_flag > 0)'; // and rdb$view_blr is null
    stProcedures : s := 'select '+
                           'rdb$procedure_id        as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'rdb$procedure_name       as proc_name, '+
                          '0                        as proc_type, '+
                          'rdb$procedure_inputs     as in_params, '+
                          'rdb$procedure_outputs    as out_params '+
                        'from '+
                          'rdb$procedures '+
                        'WHERE '+
                          '(rdb$system_flag = 0 or rdb$system_flag is null)';
    stColumns    : s := 'select '+
                           'rdb$procedure_id        as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'rdb$relation_name        as table_name, '+
                          'rdb$field_name           as column name, '+
                          'rdb$field_position       as column_position, '+
                          '0                        as column_type, '+
                          '0                        as column_datatype, '+
                          '''''                     as column_typename, '+
                          '0                        as column_subtype, '+
                          '0                        as column_precision, '+
                          '0                        as column_scale, '+
                          '0                        as column_length, '+
                          '0                        as column_nullable '+
                        'from '+
                          'rdb$relation_fields '+
                        'WHERE '+
                          '(rdb$system_flag = 0 or rdb$system_flag is null)';
  else
    DatabaseError(SMetadataUnavailable)
  end; {case}
  result := s;
end;


procedure TIBConnection.UpdateIndexDefs(var IndexDefs : TIndexDefs;TableName : string);

var qry : TSQLQuery;

begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  qry := tsqlquery.Create(nil);
  qry.transaction := Transaction;
  qry.database := Self;
  with qry do
    begin
    ReadOnly := True;
    sql.clear;
    sql.add('select '+
              'ind.rdb$index_name, '+
              'ind.rdb$relation_name, '+
              'ind.rdb$unique_flag, '+
              'ind_seg.rdb$field_name, '+
              'rel_con.rdb$constraint_type '+
            'from '+
              'rdb$index_segments ind_seg, '+
              'rdb$indices ind '+
             'left outer join '+
              'rdb$relation_constraints rel_con '+
             'on '+
              'rel_con.rdb$index_name = ind.rdb$index_name '+
            'where '+
              '(ind_seg.rdb$index_name = ind.rdb$index_name) and '+
              '(ind.rdb$relation_name=''' +  UpperCase(TableName) +''') '+
            'order by '+
              'ind.rdb$index_name;');
    open;
    end;
    
  while not qry.eof do with IndexDefs.AddIndexDef do
    begin
    Name := trim(qry.fields[0].asstring);
    Fields := trim(qry.Fields[3].asstring);
    If qry.fields[4].asstring = 'PRIMARY KEY' then options := options + [ixPrimary];
    If qry.fields[2].asinteger = 1 then options := options + [ixUnique];
    qry.next;
    while (name = qry.fields[0].asstring) and (not qry.eof) do
      begin
      Fields := Fields + ';' + trim(qry.Fields[3].asstring);
      qry.next;
      end;
    end;
  qry.close;
  qry.free;
end;

procedure TIBConnection.GetFloat(CurrBuff, Buffer : pointer; Field : TFieldDef);
var
  Ext : extended;
  Dbl : double;
  Sin : single;
begin
  case Field.Size of
    4 :
      begin
        Move(CurrBuff^, Sin, 4);
        Dbl := Sin;
      end;
    8 :
      begin
        Move(CurrBuff^, Dbl, 8);
      end;
    10:
      begin
        Move(CurrBuff^, Ext, 10);
        Dbl := double(Ext);
      end;
  end;
  Move(Dbl, Buffer^, 8);
end;

end.

