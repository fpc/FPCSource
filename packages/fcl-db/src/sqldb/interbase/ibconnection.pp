unit IBConnection;

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils, sqldb, db, math, dbconst, bufdataset,
{$IfDef LinkDynamically}
  ibase60dyn;
{$Else}
  ibase60;
{$EndIf}

const
  DEFDIALECT = 3;

type

  EIBDatabaseError = class(EDatabaseError)
    public
      GDSErrorCode : Longint;
  end;

  { TIBCursor }

  TIBCursor = Class(TSQLCursor)
    protected
    Status               : array [0..19] of ISC_STATUS;
    Statement            : pointer;
    SQLDA                : PXSQLDA;
    in_SQLDA             : PXSQLDA;
    ParamBinding         : array of integer;
    FieldBinding         : array of integer;
  end;

  TIBTrans = Class(TSQLHandle)
    protected
    TransactionHandle   : pointer;
    TPB                 : string;                // Transaction parameter buffer
    Status              : array [0..19] of ISC_STATUS;
  end;

  { TIBConnection }

  TIBConnection = class (TSQLConnection)
  private
    FSQLDatabaseHandle   : pointer;
    FStatus              : array [0..19] of ISC_STATUS;
    FDialect             : integer;
    FDBDialect           : integer;
    FBLobSegmentSize     : word;

    procedure ConnectFB;
    function GetDialect: integer;
    procedure AllocSQLDA(var aSQLDA : PXSQLDA;Count : integer);
    procedure TranslateFldType(SQLType, SQLLen, SQLScale : integer;
      var TrType : TFieldType; var TrLen : word);
    // conversion methods
    procedure GetDateTime(CurrBuff, Buffer : pointer; AType : integer);
    procedure SetDateTime(CurrBuff: pointer; PTime : TDateTime; AType : integer);
    procedure GetFloat(CurrBuff, Buffer : pointer; Size : Byte);
    procedure SetFloat(CurrBuff: pointer; Dbl: Double; Size: integer);
    procedure CheckError(ProcName : string; Status : PISC_STATUS);
    function getMaxBlobSize(blobHandle : TIsc_Blob_Handle) : longInt;
    procedure SetParameters(cursor : TSQLCursor;AParams : TParams);
    procedure FreeSQLDABuffer(var aSQLDA : PXSQLDA);
    function  IsDialectStored: boolean;
  protected
    procedure DoConnect; override;
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    function GetHandle : pointer; override;

    Function AllocateCursorHandle : TSQLCursor; override;
    Procedure DeAllocateCursorHandle(var cursor : TSQLCursor); override;
    Function AllocateTransactionHandle : TSQLHandle; override;

    procedure PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams); override;
    procedure UnPrepareStatement(cursor : TSQLCursor); override;
    procedure FreeFldBuffers(cursor : TSQLCursor); override;
    procedure Execute(cursor: TSQLCursor;atransaction:tSQLtransaction; AParams : TParams); override;
    procedure AddFieldDefs(cursor: TSQLCursor;FieldDefs : TfieldDefs); override;
    function Fetch(cursor : TSQLCursor) : boolean; override;
    function LoadField(cursor : TSQLCursor;FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean; override;
    function GetTransactionHandle(trans : TSQLHandle): pointer; override;
    function Commit(trans : TSQLHandle) : boolean; override;
    function RollBack(trans : TSQLHandle) : boolean; override;
    function StartdbTransaction(trans : TSQLHandle; AParams : string) : boolean; override;
    procedure CommitRetaining(trans : TSQLHandle); override;
    procedure RollBackRetaining(trans : TSQLHandle); override;
    procedure UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TSQLTransaction); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure CreateDB; override;
    procedure DropDB; override;
    property BlobSegmentSize : word read FBlobSegmentSize write FBlobSegmentSize;
    function GetDBDialect: integer;
  published
    property DatabaseName;
    property Dialect : integer read GetDialect write FDialect stored IsDialectStored default DEFDIALECT;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;
  
  { TIBConnectionDef }
  
  TIBConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;
                  
implementation

uses strutils;

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

procedure TIBConnection.CheckError(ProcName : string; Status : PISC_STATUS);
var
  buf : array [0..1023] of char;
  Msg : string;
  E   : EIBDatabaseError;
  Err : longint;
  
begin
  if ((Status[0] = 1) and (Status[1] <> 0)) then
  begin
    Err := Status[1];
    msg := '';
    while isc_interprete(Buf, @Status) > 0 do
      Msg := Msg + LineEnding +' -' + StrPas(Buf);
    E := EIBDatabaseError.CreateFmt('%s : %s : %s',[self.Name,ProcName,Msg]);
    E.GDSErrorCode := Err;
    Raise E;
  end;
end;


constructor TIBConnection.Create(AOwner : TComponent);

begin
  inherited;
  FConnOptions := FConnOptions + [sqSupportParams] + [sqEscapeRepeat] + [sqQuoteFieldnames];
  FBLobSegmentSize := 80;
  FDialect := -1;
  FDBDialect := -1;
end;


function TIBConnection.GetTransactionHandle(trans : TSQLHandle): pointer;
begin
  Result := (trans as TIBtrans).TransactionHandle;
end;

function TIBConnection.Commit(trans : TSQLHandle) : boolean;
begin
  result := false;
  with (trans as TIBTrans) do
    if isc_commit_transaction(@Status[0], @TransactionHandle) <> 0 then
      CheckError('Commit', Status)
    else result := true;
end;

function TIBConnection.RollBack(trans : TSQLHandle) : boolean;
begin
  result := false;
  if isc_rollback_transaction(@TIBTrans(trans).Status[0], @TIBTrans(trans).TransactionHandle) <> 0 then
    CheckError('Rollback', TIBTrans(trans).Status)
  else result := true;
end;

function TIBConnection.StartDBTransaction(trans : TSQLHandle;AParams : String) : boolean;
var
  DBHandle : pointer;
  tr       : TIBTrans;
  i        : integer;
  s        : string;
begin
  result := false;

  DBHandle := GetHandle;
  tr := trans as TIBtrans;
  with tr do
    begin
    TPB := chr(isc_tpb_version3);

    i := 1;
    s := ExtractSubStr(AParams,i,stdWordDelims);
    while s <> '' do
      begin
      if s='isc_tpb_write' then TPB := TPB + chr(isc_tpb_write)
      else if s='isc_tpb_read' then TPB := TPB + chr(isc_tpb_read)
      else if s='isc_tpb_consistency' then TPB := TPB + chr(isc_tpb_consistency)
      else if s='isc_tpb_concurrency' then TPB := TPB + chr(isc_tpb_concurrency)
      else if s='isc_tpb_read_committed' then TPB := TPB + chr(isc_tpb_read_committed)
      else if s='isc_tpb_rec_version' then TPB := TPB + chr(isc_tpb_rec_version)
      else if s='isc_tpb_no_rec_version' then TPB := TPB + chr(isc_tpb_no_rec_version)
      else if s='isc_tpb_wait' then TPB := TPB + chr(isc_tpb_wait)
      else if s='isc_tpb_nowait' then TPB := TPB + chr(isc_tpb_nowait)
      else if s='isc_tpb_shared' then TPB := TPB + chr(isc_tpb_shared)
      else if s='isc_tpb_protected' then TPB := TPB + chr(isc_tpb_protected)
      else if s='isc_tpb_exclusive' then TPB := TPB + chr(isc_tpb_exclusive)
      else if s='isc_tpb_lock_read' then TPB := TPB + chr(isc_tpb_lock_read)
      else if s='isc_tpb_lock_write' then TPB := TPB + chr(isc_tpb_lock_write)
      else if s='isc_tpb_verb_time' then TPB := TPB + chr(isc_tpb_verb_time)
      else if s='isc_tpb_commit_time' then TPB := TPB + chr(isc_tpb_commit_time)
      else if s='isc_tpb_ignore_limbo' then TPB := TPB + chr(isc_tpb_ignore_limbo)
      else if s='isc_tpb_autocommit' then TPB := TPB + chr(isc_tpb_autocommit)
      else if s='isc_tpb_restart_requests' then TPB := TPB + chr(isc_tpb_restart_requests)
      else if s='isc_tpb_no_auto_undo' then TPB := TPB + chr(isc_tpb_no_auto_undo);
      s := ExtractSubStr(AParams,i,stdWordDelims);

      end;

    TransactionHandle := nil;

    if isc_start_transaction(@Status[0], @TransactionHandle, 1,
       [@DBHandle, Length(TPB), @TPB[1]]) <> 0 then
      CheckError('StartTransaction',Status)
    else Result := True;
    end;
end;


procedure TIBConnection.CommitRetaining(trans : TSQLHandle);
begin
  with trans as TIBtrans do
    if isc_commit_retaining(@Status[0], @TransactionHandle) <> 0 then
      CheckError('CommitRetaining', Status);
end;

procedure TIBConnection.RollBackRetaining(trans : TSQLHandle);
begin
  with trans as TIBtrans do
    if isc_rollback_retaining(@Status[0], @TransactionHandle) <> 0 then
      CheckError('RollBackRetaining', Status);
end;
procedure TIBConnection.DropDB;

begin
  CheckDisConnected;

{$IfDef LinkDynamically}
  InitialiseIBase60;
{$EndIf}

  ConnectFB;

  if isc_drop_database(@FStatus[0], @FSQLDatabaseHandle) <> 0 then
    CheckError('DropDB', FStatus);

{$IfDef LinkDynamically}
  ReleaseIBase60;
{$EndIf}
end;

procedure TIBConnection.CreateDB;

var ASQLDatabaseHandle,
    ASQLTransactionHandle : pointer;
    CreateSQL : String;
    pagesize : String;
begin
  CheckDisConnected;
{$IfDef LinkDynamically}
  InitialiseIBase60;
{$EndIf}
  ASQLDatabaseHandle := nil;
  ASQLTransactionHandle := nil;

  CreateSQL := 'CREATE DATABASE ';
  if HostName <> '' then CreateSQL := CreateSQL + ''''+ HostName+':'+DatabaseName + ''''
    else CreateSQL := CreateSQL + '''' + DatabaseName + '''';
  if UserName <> '' then
    CreateSQL := CreateSQL + ' USER ''' + Username + '''';
  if Password <> '' then
    CreateSQL := CreateSQL + ' PASSWORD ''' + Password + '''';
  pagesize := params.Values['PAGE_SIZE'];
  if pagesize <> '' then
    CreateSQL := CreateSQL + ' PAGE_SIZE '+pagesize;

  if isc_dsql_execute_immediate(@FStatus[0],@ASQLDatabaseHandle,@ASQLTransactionHandle,length(CreateSQL),@CreateSQL[1],Dialect,nil) <> 0 then
    CheckError('CreateDB', FStatus);

  if isc_detach_database(@FStatus[0], @ASQLDatabaseHandle) <> 0 then
    CheckError('CreateDB', FStatus);
{$IfDef LinkDynamically}
  ReleaseIBase60;
{$EndIf}
end;

procedure TIBConnection.DoInternalConnect;

begin
{$IfDef LinkDynamically}
  InitialiseIBase60;
{$EndIf}
  inherited dointernalconnect;

  ConnectFB;
end;

procedure TIBConnection.DoInternalDisconnect;
begin
  FDialect := -1;
  FDBDialect := -1;
  if not Connected then
  begin
    FSQLDatabaseHandle := nil;
    Exit;
  end;

  if isc_detach_database(@FStatus[0], @FSQLDatabaseHandle) <> 0 then
    CheckError('Close', FStatus);
{$IfDef LinkDynamically}
  ReleaseIBase60;
{$EndIf}
end;


function TIBConnection.GetDBDialect: integer;
var
  x : integer;
  Len : integer;
  Buffer : array [0..1] of byte;
  ResBuf : array [0..39] of byte;
begin
  result := -1;
  if Connected then
    begin
    Buffer[0] := isc_info_db_sql_dialect;
    Buffer[1] := isc_info_end;
    if isc_database_info(@FStatus[0], @FSQLDatabaseHandle, Length(Buffer),
      pchar(@Buffer[0]), SizeOf(ResBuf), pchar(@ResBuf[0])) <> 0 then
        CheckError('SetDBDialect', FStatus);
    x := 0;
    while x < 40 do
      case ResBuf[x] of
        isc_info_db_sql_dialect :
          begin
          Inc(x);
          Len := isc_vax_integer(pchar(@ResBuf[x]), 2);
          Inc(x, 2);
          Result := isc_vax_integer(pchar(@ResBuf[x]), Len);
          Inc(x, Len);
          end;
        isc_info_end : Break;
      else
        inc(x);
      end;
    end;
end;

procedure TIBConnection.ConnectFB;
var
  ADatabaseName: String;
  DPB: string;
begin
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

  FSQLDatabaseHandle := nil;
  if HostName <> '' then ADatabaseName := HostName+':'+DatabaseName
    else ADatabaseName := DatabaseName;
  if isc_attach_database(@FStatus[0], Length(ADatabaseName), @ADatabaseName[1],
    @FSQLDatabaseHandle,
         Length(DPB), @DPB[1]) <> 0 then
    CheckError('DoInternalConnect', FStatus);
end;

function TIBConnection.GetDialect: integer;
begin
  if FDialect = -1 then
  begin
    if FDBDialect = -1 then
      Result := DEFDIALECT
    else
      Result := FDBDialect;
  end else
    Result := FDialect;
end;

procedure TIBConnection.AllocSQLDA(var aSQLDA : PXSQLDA;Count : integer);

begin
  FreeSQLDABuffer(aSQLDA);

  if count > -1 then
    begin
    reAllocMem(aSQLDA, XSQLDA_Length(Count));
    { Zero out the memory block to avoid problems with exceptions within the
      constructor of this class. }
    FillChar(aSQLDA^, XSQLDA_Length(Count), 0);

    aSQLDA^.Version := sqlda_version1;
    aSQLDA^.SQLN := Count;
    end
  else
    reAllocMem(aSQLDA,0);
end;

procedure TIBConnection.TranslateFldType(SQLType, SQLLen, SQLScale : integer;
           var TrType : TFieldType; var TrLen : word);
begin
  trlen := 0;
  if SQLScale < 0 then
    begin
    if (SQLScale >= -4) and (SQLScale <= -1) then //in [-4..-1] then
      begin
      TrLen := abs(SQLScale);
      TrType := ftBCD
      end
    else
      TrType := ftFMTBcd;
    end
  else case (SQLType and not 1) of
    SQL_VARYING,SQL_TEXT :
      begin
        TrType := ftString;
        if SQLLen > dsMaxStringSize then
          TrLen := dsMaxStringSize
        else
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
        TrLen := SQLLen;
      end;
    SQL_BLOB :
      begin
        TrType := ftBlob;
        TrLen := SQLLen;
      end;
    SQL_SHORT :
        TrType := ftSmallint;
    SQL_LONG :
        TrType := ftInteger;
    SQL_INT64 :
        TrType := ftLargeInt;
    SQL_DOUBLE :
        TrType := ftFloat;
    SQL_FLOAT :
        TrType := ftFloat;
    else
        TrType := ftUnknown;
  end;
end;

Function TIBConnection.AllocateCursorHandle : TSQLCursor;

var curs : TIBCursor;

begin
  curs := TIBCursor.create;
  curs.sqlda := nil;
  curs.statement := nil;
  curs.FPrepared := False;
  AllocSQLDA(curs.SQLDA,0);
  result := curs;
end;

procedure TIBConnection.DeAllocateCursorHandle(var cursor : TSQLCursor);

begin
  if assigned(cursor) then with cursor as TIBCursor do
    begin
    AllocSQLDA(SQLDA,-1);
    AllocSQLDA(in_SQLDA,-1);
    end;
  FreeAndNil(cursor);
end;

Function TIBConnection.AllocateTransactionHandle : TSQLHandle;

begin
  result := TIBTrans.create;
end;

procedure TIBConnection.PrepareStatement(cursor: TSQLCursor;ATransaction : TSQLTransaction;buf : string; AParams : TParams);

var dh    : pointer;
    tr    : pointer;
    x     : shortint;

begin
  with cursor as TIBcursor do
    begin
    dh := GetHandle;
    if isc_dsql_allocate_statement(@Status[0], @dh, @Statement) <> 0 then
      CheckError('PrepareStatement', Status);
    tr := aTransaction.Handle;
    
    if assigned(AParams) and (AParams.count > 0) then
      buf := AParams.ParseSQL(buf,false,sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions,psInterbase,paramBinding);

    if isc_dsql_prepare(@Status[0], @tr, @Statement, 0, @Buf[1], Dialect, nil) <> 0 then
      CheckError('PrepareStatement', Status);
    FPrepared := True;
    if assigned(AParams) and (AParams.count > 0) then
      begin
      AllocSQLDA(in_SQLDA,Length(ParamBinding));
      if isc_dsql_describe_bind(@Status[0], @Statement, 1, in_SQLDA) <> 0 then
        CheckError('PrepareStatement', Status);
      if in_SQLDA^.SQLD > in_SQLDA^.SQLN then
        DatabaseError(SParameterCountIncorrect,self);
      {$R-}
      for x := 0 to in_SQLDA^.SQLD - 1 do with in_SQLDA^.SQLVar[x] do
        begin
        if ((SQLType and not 1) = SQL_VARYING) then
          SQLData := AllocMem(in_SQLDA^.SQLVar[x].SQLLen+2)
        else
          SQLData := AllocMem(in_SQLDA^.SQLVar[x].SQLLen);
        if (sqltype and 1) = 1 then New(SQLInd);
        end;
      {$R+}
      end
    else
      AllocSQLDA(in_SQLDA,0);
    if FStatementType = stselect then
      begin
      FPrepared := False;
      if isc_dsql_describe(@Status[0], @Statement, 1, SQLDA) <> 0 then
        CheckError('PrepareSelect', Status);
      if SQLDA^.SQLD > SQLDA^.SQLN then
        begin
        AllocSQLDA(SQLDA,SQLDA^.SQLD);
        if isc_dsql_describe(@Status[0], @Statement, 1, SQLDA) <> 0 then
          CheckError('PrepareSelect', Status);
        end;
      {$R-}
      for x := 0 to SQLDA^.SQLD - 1 do with SQLDA^.SQLVar[x] do
        begin
        if ((SQLType and not 1) = SQL_VARYING) then
          SQLData := AllocMem(SQLDA^.SQLVar[x].SQLLen+2)
        else
          SQLData := AllocMem(SQLDA^.SQLVar[x].SQLLen);
        if (SQLType and 1) = 1 then New(SQLInd);
        end;
      {$R+}
      end;
    end;
end;

procedure TIBConnection.UnPrepareStatement(cursor : TSQLCursor);

begin
  with cursor as TIBcursor do
    begin
    if isc_dsql_free_statement(@Status[0], @Statement, DSQL_Drop) <> 0 then
      CheckError('FreeStatement', Status);
    Statement := nil;
    FPrepared := False;
    end;
end;

procedure TIBConnection.FreeSQLDABuffer(var aSQLDA : PXSQLDA);

var x : shortint;

begin
{$R-}
  if assigned(aSQLDA) then
    for x := 0 to aSQLDA^.SQLN - 1 do
      begin
      reAllocMem(aSQLDA^.SQLVar[x].SQLData,0);
      if assigned(aSQLDA^.SQLVar[x].sqlind) then
        begin
        Dispose(aSQLDA^.SQLVar[x].sqlind);
        aSQLDA^.SQLVar[x].sqlind := nil;
        end
        
      end;
{$R+}
end;

function TIBConnection.IsDialectStored: boolean;
begin
  result := (FDialect<>-1);
end;

procedure TIBConnection.DoConnect;
begin
  inherited DoConnect;
  FDbDialect := GetDBDialect;
end;

procedure TIBConnection.FreeFldBuffers(cursor : TSQLCursor);

begin
  with cursor as TIBCursor do
    begin
    FreeSQLDABuffer(SQLDA);
    FreeSQLDABuffer(in_SQLDA);
    SetLength(FieldBinding,0);
    end;
end;

procedure TIBConnection.Execute(cursor: TSQLCursor;atransaction:tSQLtransaction; AParams : TParams);
var tr : pointer;
begin
  tr := aTransaction.Handle;
  if Assigned(APArams) and (AParams.count > 0) then SetParameters(cursor, AParams);
  with cursor as TIBCursor do
    if isc_dsql_execute2(@Status[0], @tr, @Statement, 1, in_SQLDA, nil) <> 0 then
      CheckError('Execute', Status);
end;


procedure TIBConnection.AddFieldDefs(cursor: TSQLCursor;FieldDefs : TfieldDefs);
var
  x         : integer;
  TransLen  : word;
  TransType : TFieldType;
  FD        : TFieldDef;

begin
  {$R-}
  with cursor as TIBCursor do
    begin
    setlength(FieldBinding,SQLDA^.SQLD);
    for x := 0 to SQLDA^.SQLD - 1 do
      begin
      TranslateFldType(SQLDA^.SQLVar[x].SQLType, SQLDA^.SQLVar[x].SQLLen, SQLDA^.SQLVar[x].SQLScale,
        TransType, TransLen);
      FD := TFieldDef.Create(FieldDefs, SQLDA^.SQLVar[x].AliasName, TransType,
         TransLen, False, (x + 1));
      if TransType = ftBCD then FD.precision := SQLDA^.SQLVar[x].SQLLen;
//      FD.DisplayName := SQLDA^.SQLVar[x].AliasName;
      FieldBinding[FD.FieldNo-1] := x;
      end;
    end;
  {$R+}
end;

function TIBConnection.GetHandle: pointer;
begin
  Result := FSQLDatabaseHandle;
end;

function TIBConnection.Fetch(cursor : TSQLCursor) : boolean;
var
  retcode : integer;
begin
  with cursor as TIBCursor do
    begin
    retcode := isc_dsql_fetch(@Status[0], @Statement, 1, SQLDA);
    if (retcode <> 0) and (retcode <> 100) then
      CheckError('Fetch', Status);
    end;
  Result := (retcode <> 100);
end;

procedure TIBConnection.SetParameters(cursor : TSQLCursor;AParams : TParams);

var ParNr,SQLVarNr : integer;
    s               : string;
    i               : integer;
    li              : LargeInt;
    currbuff        : pchar;
    w               : word;

    TransactionHandle : pointer;
    blobId            : ISC_QUAD;
    blobHandle        : Isc_blob_Handle;
    BlobSize,
    BlobBytesWritten  : longint;
    
  procedure SetBlobParam;
  
  begin
{$R-}
    with cursor as TIBCursor do
      begin
      TransactionHandle := transaction.Handle;
      blobhandle := nil;
      if isc_create_blob(@FStatus[0], @FSQLDatabaseHandle, @TransactionHandle, @blobHandle, @blobId) <> 0 then
       CheckError('TIBConnection.CreateBlobStream', FStatus);

      s := AParams[ParNr].AsString;
      BlobSize := length(s);

      BlobBytesWritten := 0;
      i := 0;

      while BlobBytesWritten < (BlobSize-BlobSegmentSize) do
        begin
        isc_put_segment(@FStatus[0], @blobHandle, BlobSegmentSize, @s[(i*BlobSegmentSize)+1]);
        inc(BlobBytesWritten,BlobSegmentSize);
        inc(i);
        end;
      if BlobBytesWritten <> BlobSize then
        isc_put_segment(@FStatus[0], @blobHandle, BlobSize-BlobBytesWritten, @s[(i*BlobSegmentSize)+1]);

      if isc_close_blob(@FStatus[0], @blobHandle) <> 0 then
        CheckError('TIBConnection.CreateBlobStream isc_close_blob', FStatus);
      Move(blobId, in_sqlda^.SQLvar[SQLVarNr].SQLData^, in_SQLDA^.SQLVar[SQLVarNr].SQLLen);
      end;
{$R+}
  end;

begin
{$R-}
  with cursor as TIBCursor do for SQLVarNr := 0 to High(ParamBinding){AParams.count-1} do
    begin
    ParNr := ParamBinding[SQLVarNr];
    if AParams[ParNr].IsNull then
      begin
      If Assigned(in_sqlda^.SQLvar[SQLVarNr].SQLInd) then
        in_sqlda^.SQLvar[SQLVarNr].SQLInd^ := -1;
      end
    else
      begin
      if assigned(in_sqlda^.SQLvar[SQLVarNr].SQLInd) then in_sqlda^.SQLvar[SQLVarNr].SQLInd^ := 0;

      case (in_sqlda^.SQLvar[SQLVarNr].sqltype and not 1) of
        SQL_LONG :
          begin
          i := AParams[ParNr].AsInteger;
          Move(i, in_sqlda^.SQLvar[SQLVarNr].SQLData^, in_SQLDA^.SQLVar[SQLVarNr].SQLLen);
          end;
        SQL_SHORT :
          begin
          i := AParams[ParNr].AsSmallInt;
          Move(i, in_sqlda^.SQLvar[SQLVarNr].SQLData^, in_SQLDA^.SQLVar[SQLVarNr].SQLLen);
          end;
        SQL_BLOB :
          SetBlobParam;
        SQL_VARYING, SQL_TEXT :
          begin
          s := AParams[ParNr].AsString;
          w := length(s); // a word is enough, since the max-length of a string in interbase is 32k
          if ((in_sqlda^.SQLvar[SQLVarNr].SQLType and not 1) = SQL_VARYING) then
            begin
            in_sqlda^.SQLvar[SQLVarNr].SQLLen := w;
            ReAllocMem(in_sqlda^.SQLvar[SQLVarNr].SQLData,in_SQLDA^.SQLVar[SQLVarNr].SQLLen+2);
            CurrBuff := in_sqlda^.SQLvar[SQLVarNr].SQLData;
            move(w,CurrBuff^,sizeof(w));
            inc(CurrBuff,2);
            end
          else
            CurrBuff := in_sqlda^.SQLvar[SQLVarNr].SQLData;
          Move(s[1], CurrBuff^, w);
          end;
        SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP :
          SetDateTime(in_sqlda^.SQLvar[SQLVarNr].SQLData, AParams[ParNr].AsDateTime, in_SQLDA^.SQLVar[SQLVarNr].SQLType);
        SQL_INT64:
          begin
          li := AParams[ParNr].AsLargeInt;
          Move(li, in_sqlda^.SQLvar[SQLVarNr].SQLData^, in_SQLDA^.SQLVar[SQLVarNr].SQLLen);
          end;
        SQL_DOUBLE, SQL_FLOAT:
          SetFloat(in_sqlda^.SQLvar[SQLVarNr].SQLData, AParams[ParNr].AsFloat, in_SQLDA^.SQLVar[SQLVarNr].SQLLen);
      else
        DatabaseErrorFmt(SUnsupportedParameter,[Fieldtypenames[AParams[ParNr].DataType]],self);
      end {case}
      end;
    end;
{$R+}
end;

function TIBConnection.LoadField(cursor : TSQLCursor;FieldDef : TfieldDef;buffer : pointer; out CreateBlob : boolean) : boolean;

var
  x          : integer;
  VarcharLen : word;
  CurrBuff     : pchar;
  c            : currency;

begin
  CreateBlob := False;
  with cursor as TIBCursor do
    begin
{$R-}
    x := FieldBinding[FieldDef.FieldNo-1];

    // Joost, 5 jan 2006: I disabled the following, since it's usefull for
    // debugging, but it also slows things down. In principle things can only go
    // wrong when FieldDefs is changed while the dataset is opened. A user just
    // shoudn't do that. ;) (The same is done in PQConnection)

    // if SQLDA^.SQLVar[x].AliasName <> FieldDef.Name then
    // DatabaseErrorFmt(SFieldNotFound,[FieldDef.Name],self);
    if assigned(SQLDA^.SQLVar[x].SQLInd) and (SQLDA^.SQLVar[x].SQLInd^ = -1) then
      result := false
    else
      begin

      with SQLDA^.SQLVar[x] do
        if ((SQLType and not 1) = SQL_VARYING) then
          begin
          Move(SQLData^, VarcharLen, 2);
          if VarcharLen > dsMaxStringSize then
            VarcharLen:=dsMaxStringSize;
          CurrBuff := SQLData + 2;
          end
        else
          begin
          CurrBuff := SQLData;
          VarCharLen := FieldDef.Size;
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
            FillByte(buffer^,sizeof(Longint),0);
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
          end;
        ftLargeint :
          begin
            FillByte(buffer^,sizeof(LargeInt),0);
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
          end;
        ftSmallint :
          begin
            FillByte(buffer^,sizeof(Smallint),0);
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
          end;
        ftDate, ftTime, ftDateTime:
          GetDateTime(CurrBuff, Buffer, SQLDA^.SQLVar[x].SQLType);
        ftString  :
          begin
            Move(CurrBuff^, Buffer^, VarCharLen);
            PChar(Buffer + VarCharLen)^ := #0;
          end;
        ftFloat   :
          GetFloat(CurrBuff, Buffer, SQLDA^.SQLVar[x].SQLLen);
        ftBlob : begin  // load the BlobIb in field's buffer
            FillByte(buffer^,sizeof(TBufBlobField),0);
            Move(CurrBuff^, Buffer^, SQLDA^.SQLVar[x].SQLLen);
         end

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

procedure TIBConnection.SetDateTime(CurrBuff: pointer; PTime : TDateTime; AType : integer);
var
  CTime : TTm;          // C struct time
  STime : TSystemTime;  // System time
begin
  DateTimeToSystemTime(PTime,STime);
  
  CTime.tm_year := STime.Year - 1900;
  CTime.tm_mon  := STime.Month -1;
  CTime.tm_mday := STime.Day;
  CTime.tm_hour := STime.Hour;
  CTime.tm_min  := STime.Minute;
  CTime.tm_sec  := STime.Second;

  case (AType and not 1) of
    SQL_TYPE_DATE :
      isc_encode_sql_date(@CTime, PISC_DATE(CurrBuff));
    SQL_TYPE_TIME :
      isc_encode_sql_time(@CTime, PISC_TIME(CurrBuff));
    SQL_TIMESTAMP :
      isc_encode_timestamp(@CTime, PISC_TIMESTAMP(CurrBuff));
  end;
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
                          '(rdb$system_flag = 0 or rdb$system_flag is null) ' + // and rdb$view_blr is null
                        'order by rdb$relation_name';

    stSysTables  : s := 'select '+
                          'rdb$relation_id          as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'rdb$relation_name        as table_name, '+
                          '0                        as table_type '+
                        'from '+
                          'rdb$relations '+
                        'where '+
                          '(rdb$system_flag > 0) ' + // and rdb$view_blr is null
                        'order by rdb$relation_name';

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
                           'rdb$field_id            as recno, '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          '''''                     as schema_name, '+
                          'rdb$relation_name        as table_name, '+
                          'rdb$field_name           as column_name, '+
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
                          '(rdb$system_flag = 0 or rdb$system_flag is null) and (rdb$relation_name = ''' + Uppercase(SchemaObjectName) + ''') ' +
                        'order by rdb$field_name';
  else
    DatabaseError(SMetadataUnavailable)
  end; {case}
  result := s;
end;


procedure TIBConnection.UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string);

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
              'rel_con.rdb$constraint_type, '+
              'ind.rdb$index_type '+
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
    If qry.fields[5].asInteger = 1 then options:=options+[ixDescending];
    qry.next;
    while (name = trim(qry.fields[0].asstring)) and (not qry.eof) do
      begin
      Fields := Fields + ';' + trim(qry.Fields[3].asstring);
      qry.next;
      end;
    end;
  qry.close;
  qry.free;
end;

procedure TIBConnection.SetFloat(CurrBuff: pointer; Dbl: Double; Size: integer);

var
  Ext : extended;
  Sin : single;
begin
  case Size of
    4 :
      begin
        Sin := Dbl;
        Move(Sin, CurrBuff^, 4);
      end;
    8 :
      begin
        Move(Dbl, CurrBuff^, 8);
      end;
    10:
      begin
        Ext := Dbl;
        Move(Ext, CurrBuff^, 10);
      end;
  end;
end;

procedure TIBConnection.GetFloat(CurrBuff, Buffer : pointer; Size : byte);
var
  Ext : extended;
  Dbl : double;
  Sin : single;
begin
  case Size of
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


function TIBConnection.getMaxBlobSize(blobHandle : TIsc_Blob_Handle) : longInt;
var
  iscInfoBlobMaxSegment : byte = isc_info_blob_max_segment;
  blobInfo : array[0..50] of byte;

begin
  if isc_blob_info(@Fstatus[0], @blobHandle, sizeof(iscInfoBlobMaxSegment), pchar(@iscInfoBlobMaxSegment), sizeof(blobInfo) - 2, pchar(@blobInfo[0])) <> 0 then
    CheckError('isc_blob_info', FStatus);
  if blobInfo[0]  = isc_info_blob_max_segment then
    begin
      result :=  isc_vax_integer(pchar(@blobInfo[3]), isc_vax_integer(pchar(@blobInfo[1]), 2));
    end
  else
     CheckError('isc_blob_info', FStatus);
end;

procedure TIBConnection.LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TSQLTransaction);
const
  isc_segstr_eof = 335544367; // It's not defined in ibase60 but in ibase40. Would it be better to define in ibase60?

var
  blobHandle : Isc_blob_Handle;
  blobSegment : pointer;
  blobSegLen : word;
  maxBlobSize : longInt;
  TransactionHandle : pointer;
  blobId : PISC_QUAD;
  ptr : Pointer;
begin
  blobId := PISC_QUAD(@(ABlobBuf^.ConnBlobBuffer));

  TransactionHandle := Atransaction.Handle;
  blobHandle := nil;

  if isc_open_blob(@FStatus[0], @FSQLDatabaseHandle, @TransactionHandle, @blobHandle, blobId) <> 0 then
    CheckError('TIBConnection.CreateBlobStream', FStatus);

  maxBlobSize := getMaxBlobSize(blobHandle);

  blobSegment := AllocMem(maxBlobSize);

  with ABlobBuf^.BlobBuffer^ do
    begin
    Size := 0;
    while (isc_get_segment(@FStatus[0], @blobHandle, @blobSegLen, maxBlobSize, blobSegment) = 0) do
      begin
      ReAllocMem(Buffer,Size+blobSegLen);
      ptr := Buffer+Size;
      move(blobSegment^,ptr^,blobSegLen);
      inc(Size,blobSegLen);
      end;
    end;
  freemem(blobSegment);

  if FStatus[1] = isc_segstr_eof then
    begin
      if isc_close_blob(@FStatus[0], @blobHandle) <> 0 then
        CheckError('TIBConnection.CreateBlobStream isc_close_blob', FStatus);
    end
  else
    CheckError('TIBConnection.CreateBlobStream isc_get_segment', FStatus);
end;

function TIBConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;

var info_request       : string;
    resbuf             : array[0..63] of byte;
    i                  : integer;
    BlockSize,
    subBlockSize       : integer;
    SelectedRows,
    InsertedRows       : integer;
    
begin
  SelectedRows:=-1;
  InsertedRows:=-1;

  if assigned(cursor) then with cursor as TIBCursor do
   if assigned(statement) then
    begin
    info_request := chr(isc_info_sql_records);
    if isc_dsql_sql_info(@Status[0],@Statement,Length(info_request), @info_request[1],sizeof(resbuf),@resbuf) <> 0 then
      CheckError('RowsAffected', Status);

    i := 0;
    while not (byte(resbuf[i]) in [isc_info_end,isc_info_truncated]) do
      begin
      BlockSize:=isc_vax_integer(@resbuf[i+1],2);
      if resbuf[i]=isc_info_sql_records then
        begin
        inc(i,3);
        BlockSize:=BlockSize+i;
        while (resbuf[i] <> isc_info_end) and (i < BlockSize) do
          begin
          subBlockSize:=isc_vax_integer(@resbuf[i+1],2);
          if resbuf[i] = isc_info_req_select_count then
            SelectedRows := isc_vax_integer(@resbuf[i+3],subBlockSize)
          else if resbuf[i] = isc_info_req_insert_count then
            InsertedRows := isc_vax_integer(@resbuf[i+3],subBlockSize);
          inc(i,subBlockSize+3);
          end;
        end
      else
        inc(i,BlockSize+3);
      end;
    end;
  if SelectedRows>0 then result:=SelectedRows
  else Result:=InsertedRows;
end;

{ TIBConnectionDef }

class function TIBConnectionDef.TypeName: String;
begin
  Result:='Firebird';
end;
  
class function TIBConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TIBConnection;
end;
    
class function TIBConnectionDef.Description: String;
begin
  Result:='Connect to Firebird/Interbase directly via the client library';
end;

initialization
  RegisterConnection(TIBConnectionDef);

finalization
  UnRegisterConnection(TIBConnectionDef);
end.
