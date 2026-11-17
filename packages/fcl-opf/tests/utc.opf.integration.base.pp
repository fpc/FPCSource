unit utc.opf.integration.base;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.IniFiles, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3,
  Data.PQ.Connection,
  Data.IB.Connection,
  Data.MySQL80.Connection,
  Data.SqlDb.Oracle,
  FpcUnit.Test,
{$ELSE}
  Classes, SysUtils, IniFiles, db, sqldb,
  sqlite3conn,
  pqconnection,
  ibconnection,
  mysql80conn,
  oracleconnection,
  fpcunit,
{$ENDIF}
  Opf.Consts, Opf.Driver, Opf.Sql, Opf.DDL;

Type

  { TDatabaseParams }

  TDatabaseParams = record
    Host: String;
    Port: String;
    Database: String;
    User: String;
    Password: String;
  end;

  { TDatabaseSetupHelper }

  TDatabaseSetupHelper = class
  public
    class function GetSectionName: String; virtual; abstract;
    class function GetEnvPrefix: String; virtual; abstract;
    class function GetDialect: TSQLDialect; virtual; abstract;
    function CreateConnection(const aParams: TDatabaseParams): TSQLConnection; virtual; abstract;
    procedure ExecDDL(aConn: TSQLConnection; const aSQL: String); virtual;
    procedure DeleteTables(aConn: TSQLConnection; const aTables: array of String); virtual;
    procedure Cleanup; virtual;
  end;

  TDatabaseSetupHelperClass = class of TDatabaseSetupHelper;

  { TSQLiteSetupHelper }

  TSQLiteSetupHelper = class(TDatabaseSetupHelper)
  private
    FDBFile: String;
  public
    class function GetSectionName: String; override;
    class function GetEnvPrefix: String; override;
    class function GetDialect: TSQLDialect; override;
    function CreateConnection(const aParams: TDatabaseParams): TSQLConnection; override;
    procedure Cleanup; override;
  end;

  { TPgSQLSetupHelper }

  TPgSQLSetupHelper = class(TDatabaseSetupHelper)
  public
    class function GetSectionName: String; override;
    class function GetEnvPrefix: String; override;
    class function GetDialect: TSQLDialect; override;
    function CreateConnection(const aParams: TDatabaseParams): TSQLConnection; override;
  end;

  { TFirebirdSetupHelper }

  TFirebirdSetupHelper = class(TDatabaseSetupHelper)
  public
    class function GetSectionName: String; override;
    class function GetEnvPrefix: String; override;
    class function GetDialect: TSQLDialect; override;
    function CreateConnection(const aParams: TDatabaseParams): TSQLConnection; override;
    procedure ExecDDL(aConn: TSQLConnection; const aSQL: String); override;
    procedure DeleteTables(aConn: TSQLConnection; const aTables: array of String); override;
  end;

  { TMySQLSetupHelper }

  TMySQLSetupHelper = class(TDatabaseSetupHelper)
  public
    class function GetSectionName: String; override;
    class function GetEnvPrefix: String; override;
    class function GetDialect: TSQLDialect; override;
    function CreateConnection(const aParams: TDatabaseParams): TSQLConnection; override;
  end;

  { TOracleSetupHelper }

  TOracleSetupHelper = class(TDatabaseSetupHelper)
  public
    class function GetSectionName: String; override;
    class function GetEnvPrefix: String; override;
    class function GetDialect: TSQLDialect; override;
    function CreateConnection(const aParams: TDatabaseParams): TSQLConnection; override;
    procedure DeleteTables(aConn: TSQLConnection; const aTables: array of String); override;
  end;

  { TTestIntegration }

  TTestIntegration = class(TTestCase)
  protected
    FConn: TSQLConnection;
    FHelper: TDatabaseSetupHelper;
    class function ReadDatabaseParams(const aSection, aEnvVarPrefix: String): TDatabaseParams;
    class procedure DoDeleteTables(aConn: TSQLConnection; const aTables: array of String);
    procedure SetUpDatabase(aHelperClass: TDatabaseSetupHelperClass);
    procedure TearDownDatabase;
    procedure DeleteTables(const aTables: array of String);
    procedure ExecDDL(const aSQL: String);
  end;

  { TTestIntegrationSetupSQLite — legacy, kept during transition }

  TTestIntegrationSetupSQLite = class(TTestIntegration)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TTestIntegrationSetupPgSQL — legacy, kept during transition }

  TTestIntegrationSetupPgSQL = class(TTestIntegration)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TTestIntegrationSetupFirebird — legacy, kept during transition }

  TTestIntegrationSetupFirebird = class(TTestIntegration)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ExecFirebirdDDL(const aSQL: String);
  end;

  { TTestIntegrationSetupMySQL — legacy, kept during transition }

  TTestIntegrationSetupMySQL = class(TTestIntegration)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

function TableExists(aConn: TSQLConnection; const aTable: String): Boolean;
procedure SetupLogging;

implementation

{ TableExists }

function TableExists(aConn: TSQLConnection; const aTable: String): Boolean;
var
  Tables: TStringList;
begin
  Result:=False;
  if (not Assigned(aConn)) or (not aConn.Connected) then
    Exit;
  if aConn.Transaction.Active then
    aConn.Transaction.Rollback;
  aConn.Transaction.StartTransaction;
  Tables:=TStringList.Create;
  try
    aConn.GetTableNames(Tables);
    Result:=Tables.IndexOf(aTable) >= 0;
  finally
    Tables.Free;
  end;
  aConn.Transaction.Commit;
end;

{ TDatabaseSetupHelper }

procedure TDatabaseSetupHelper.ExecDDL(aConn: TSQLConnection; const aSQL: String);
var
  Q: TSQLQuery;
begin
  if (not Assigned(aConn)) or (not aConn.Connected) then
    Exit;
  if aConn.Transaction.Active then
    aConn.Transaction.Rollback;
  aConn.Transaction.StartTransaction;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConn;
    Q.Transaction:=aConn.Transaction;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
  aConn.Transaction.Commit;
end;

procedure TDatabaseSetupHelper.DeleteTables(aConn: TSQLConnection; const aTables: array of String);
var
  S: String;
begin
  for S in aTables do
    ExecDDL(aConn, 'DROP TABLE IF EXISTS ' + S);
end;

procedure TDatabaseSetupHelper.Cleanup;
begin
  // no-op by default
end;

{ TSQLiteSetupHelper }

class function TSQLiteSetupHelper.GetSectionName: String;
begin
  Result:='SQLite';
end;

class function TSQLiteSetupHelper.GetEnvPrefix: String;
begin
  Result:='OPF_SQLITE';
end;

class function TSQLiteSetupHelper.GetDialect: TSQLDialect;
begin
  Result:=dSQLite;
end;

function TSQLiteSetupHelper.CreateConnection(const aParams: TDatabaseParams): TSQLConnection;
var
  Conn: TSQLite3Connection;
  Dir: String;
  T: TSQLTransaction;
begin
  Dir:=aParams.Database;
  if Dir='' then
    Dir:=GetTempDir;
  FDBFile:=IncludeTrailingPathDelimiter(Dir) + 'opf_test_' + FormatDateTime('hhnnsszzz', Now) + '.db';
  if FileExists(FDBFile) then
    DeleteFile(FDBFile);
  Conn:=TSQLite3Connection.Create(Nil);
  Conn.DatabaseName:=FDBFile;
  T:=TSQLTransaction.Create(Conn);
  T.Database:=Conn;
  Conn.Transaction:=T;
  Conn.Open;
  Result:=Conn;
end;

procedure TSQLiteSetupHelper.Cleanup;
begin
  if (FDBFile<>'') and FileExists(FDBFile) then
    DeleteFile(FDBFile);
end;

{ TPgSQLSetupHelper }

class function TPgSQLSetupHelper.GetSectionName: String;
begin
  Result:='PostgreSQL';
end;

class function TPgSQLSetupHelper.GetEnvPrefix: String;
begin
  Result:='OPF_PG';
end;

class function TPgSQLSetupHelper.GetDialect: TSQLDialect;
begin
  Result:=dPostGres;
end;

function TPgSQLSetupHelper.CreateConnection(const aParams: TDatabaseParams): TSQLConnection;
var
  Conn: TPQConnection;
  T: TSQLTransaction;
begin
  if aParams.Database='' then
    Exit(Nil);
  Conn:=TPQConnection.Create(Nil);
  try
    if aParams.Host<>'' then
      Conn.HostName:=aParams.Host;
    if aParams.Port<>'' then
      Conn.Params.Values['port']:=aParams.Port;
    Conn.DatabaseName:=aParams.Database;
    if aParams.User<>'' then
      Conn.UserName:=aParams.User;
    Conn.Password:=aParams.Password;
    T:=TSQLTransaction.Create(Conn);
    T.Database:=Conn;
    Conn.Transaction:=T;
    Conn.Open;
    Result:=Conn;
  except
    Conn.Free;
    Result:=Nil;
  end;
end;

{ TFirebirdSetupHelper }

class function TFirebirdSetupHelper.GetSectionName: String;
begin
  Result:='Firebird';
end;

class function TFirebirdSetupHelper.GetEnvPrefix: String;
begin
  Result:='OPF_FB';
end;

class function TFirebirdSetupHelper.GetDialect: TSQLDialect;
begin
  Result:=dFirebird3;
end;

function TFirebirdSetupHelper.CreateConnection(const aParams: TDatabaseParams): TSQLConnection;
var
  Conn: TIBConnection;
  T: TSQLTransaction;
begin
  if aParams.Database='' then
    Exit(Nil);
  Conn:=TIBConnection.Create(Nil);
  try
    if aParams.Host<>'' then
      Conn.HostName:=aParams.Host;
    Conn.DatabaseName:=aParams.Database;
    if aParams.User<>'' then
      Conn.UserName:=aParams.User;
    if aParams.Password<>'' then
      Conn.Password:=aParams.Password;
    T:=TSQLTransaction.Create(Conn);
    T.Database:=Conn;
    Conn.Transaction:=T;
    Conn.Open;
    Result:=Conn;
  except
    Conn.Free;
    Result:=Nil;
  end;
end;

procedure TFirebirdSetupHelper.ExecDDL(aConn: TSQLConnection; const aSQL: String);
var
  Q: TSQLQuery;
begin
  if (not Assigned(aConn)) or (not aConn.Connected) then
    Exit;
  if aConn.Transaction.Active then
    aConn.Transaction.Rollback;
  try
    aConn.Transaction.StartTransaction;
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=aConn;
      Q.Transaction:=aConn.Transaction;
      Q.SQL.Text:=aSQL;
      Q.ExecSQL;
    finally
      Q.Free;
    end;
    aConn.Transaction.Commit;
  except
    if aConn.Transaction.Active then
      aConn.Transaction.Rollback;
  end;
end;

procedure TFirebirdSetupHelper.DeleteTables(aConn: TSQLConnection; const aTables: array of String);
var
  S: String;
begin
  for S in aTables do
    ExecDDL(aConn, 'DROP TABLE ' + S);
end;

{ TMySQLSetupHelper }

class function TMySQLSetupHelper.GetSectionName: String;
begin
  Result:='MySQL';
end;

class function TMySQLSetupHelper.GetEnvPrefix: String;
begin
  Result:='OPF_MYSQL';
end;

class function TMySQLSetupHelper.GetDialect: TSQLDialect;
begin
  Result:=dMySQL;
end;

function TMySQLSetupHelper.CreateConnection(const aParams: TDatabaseParams): TSQLConnection;
var
  Conn: TMySQL80Connection;
  T: TSQLTransaction;
begin
  if aParams.Database='' then
    Exit(Nil);
  Conn:=TMySQL80Connection.Create(Nil);
  try
    if aParams.Host<>'' then
      Conn.HostName:=aParams.Host;
    if aParams.Port<>'' then
      Conn.Params.Values['port']:=aParams.Port;
    Conn.DatabaseName:=aParams.Database;
    if aParams.User<>'' then
      Conn.UserName:=aParams.User;
    Conn.Password:=aParams.Password;
    T:=TSQLTransaction.Create(Conn);
    T.Database:=Conn;
    Conn.Transaction:=T;
    Conn.Open;
    Result:=Conn;
  except
    Conn.Free;
    Result:=Nil;
  end;
end;

{ TOracleSetupHelper }

class function TOracleSetupHelper.GetSectionName: String;
begin
  Result:='Oracle';
end;

class function TOracleSetupHelper.GetEnvPrefix: String;
begin
  Result:='OPF_ORA';
end;

class function TOracleSetupHelper.GetDialect: TSQLDialect;
begin
  Result:=dOracle;
end;

function TOracleSetupHelper.CreateConnection(const aParams: TDatabaseParams): TSQLConnection;
var
  Conn: TOracleConnection;
  T: TSQLTransaction;
begin
  if aParams.Database='' then
    Exit(Nil);
  Conn:=TOracleConnection.Create(Nil);
  try
    Conn.HostName:=aParams.Host;
    if (aParams.Host<>'') and (aParams.Port<>'') then
      Conn.HostName:=aParams.Host+':'+aParams.Port;
    Conn.DatabaseName:=aParams.Database;
    if aParams.User<>'' then
      Conn.UserName:=aParams.User;
    Conn.Password:=aParams.Password;
    Conn.CharSet:='utf8';
    T:=TSQLTransaction.Create(Conn);
    T.Database:=Conn;
    Conn.Transaction:=T;
    Conn.Open;
    Result:=Conn;
  except
    Conn.Free;
    Result:=Nil;
  end;
end;

procedure TOracleSetupHelper.DeleteTables(aConn: TSQLConnection; const aTables: array of String);
var
  S: String;
begin
  for S in aTables do
    try
      ExecDDL(aConn, 'DROP TABLE ' + S + ' CASCADE CONSTRAINTS PURGE');
    except
      // ORA-00942: the table does not exist
      if aConn.Transaction.Active then
        aConn.Transaction.Rollback;
    end;
end;

{ TTestIntegration }

class function TTestIntegration.ReadDatabaseParams(const aSection, aEnvVarPrefix: String): TDatabaseParams;
var
  Ini: TIniFile;
  IniPath: String;

  function ReadParam(const aKey, aEnvSuffix: String): String;
  begin
    Result:='';
    if Assigned(Ini) then
      Result:=Ini.ReadString(aSection, aKey, '');
    if Result='' then
      Result:=GetEnvironmentVariable(aEnvVarPrefix + '_' + aEnvSuffix);
  end;

begin
  Result:=Default(TDatabaseParams);
  IniPath:=ExtractFilePath(ParamStr(0)) + 'databases.ini';
  Ini:=nil;
  try
    if FileExists(IniPath) then
      Ini:=TIniFile.Create(IniPath);
    Result.Host    :=ReadParam('Host', 'HOST');
    Result.Port    :=ReadParam('Port', 'PORT');
    Result.Database:=ReadParam('Database', 'DB');
    Result.User    :=ReadParam('User', 'USER');
    Result.Password:=ReadParam('Password', 'PASS');
  finally
    Ini.Free;
  end;
end;

class procedure TTestIntegration.DoDeleteTables(aConn: TSQLConnection; const aTables: array of String);
var
  Q: TSQLQuery;
  S: String;
begin
  if (not Assigned(aConn)) or (not aConn.Connected) then
    Exit;
  if aConn.Transaction.Active then
    aConn.Transaction.Rollback;
  aConn.Transaction.StartTransaction;
  for S in aTables do
  begin
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=aConn;
      Q.Transaction:=aConn.Transaction;
      Q.SQL.Text:='DROP TABLE IF EXISTS ' + S;
      Q.ExecSQL;
    finally
      Q.Free;
    end;
  end;
  aConn.Transaction.Commit;
end;

procedure TTestIntegration.SetUpDatabase(aHelperClass: TDatabaseSetupHelperClass);
var
  Params: TDatabaseParams;
begin
  FHelper:=aHelperClass.Create;
  Params:=ReadDatabaseParams(aHelperClass.GetSectionName, aHelperClass.GetEnvPrefix);
  FConn:=FHelper.CreateConnection(Params);
  if FConn = nil then
  begin
    // TearDown does not run when SetUp raises: free the helper here
    FreeAndNil(FHelper);
    Ignore(Format(SErrIntegrationBackendUnavailable, [aHelperClass.GetSectionName]));
  end;
  OPSQLBuilder.SQLDialect:=aHelperClass.GetDialect;
  OPDDLBuilder.SQLDialect:=aHelperClass.GetDialect;
end;

procedure TTestIntegration.TearDownDatabase;
begin
  if Assigned(FConn) then
  begin
    if FConn.Connected then
    begin
      if Assigned(FConn.Transaction) and FConn.Transaction.Active then
        FConn.Transaction.Rollback;
      FConn.Close;
    end;
    FreeAndNil(FConn);
  end;
  if Assigned(FHelper) then
  begin
    FHelper.Cleanup;
    FreeAndNil(FHelper);
  end;
end;

procedure TTestIntegration.DeleteTables(const aTables: array of String);
begin
  if Assigned(FHelper) then
    FHelper.DeleteTables(FConn, aTables)
  else
    DoDeleteTables(FConn, aTables);
end;

procedure TTestIntegration.ExecDDL(const aSQL: String);
begin
  if Assigned(FHelper) then
    FHelper.ExecDDL(FConn, aSQL);
end;

{ TTestIntegrationSetupSQLite — legacy wrapper }

procedure TTestIntegrationSetupSQLite.SetUp;
begin
  inherited SetUp;
  SetUpDatabase(TSQLiteSetupHelper);
end;

procedure TTestIntegrationSetupSQLite.TearDown;
begin
  TearDownDatabase;
  inherited TearDown;
end;

{ TTestIntegrationSetupPgSQL — legacy wrapper }

procedure TTestIntegrationSetupPgSQL.SetUp;
begin
  inherited SetUp;
  SetUpDatabase(TPgSQLSetupHelper);
end;

procedure TTestIntegrationSetupPgSQL.TearDown;
begin
  TearDownDatabase;
  inherited TearDown;
end;

{ TTestIntegrationSetupFirebird — legacy wrapper }

procedure TTestIntegrationSetupFirebird.SetUp;
begin
  inherited SetUp;
  SetUpDatabase(TFirebirdSetupHelper);
end;

procedure TTestIntegrationSetupFirebird.TearDown;
begin
  TearDownDatabase;
  inherited TearDown;
end;

procedure TTestIntegrationSetupFirebird.ExecFirebirdDDL(const aSQL: String);
begin
  ExecDDL(aSQL);
end;

{ TTestIntegrationSetupMySQL — legacy wrapper }

procedure TTestIntegrationSetupMySQL.SetUp;
begin
  inherited SetUp;
  SetUpDatabase(TMySQLSetupHelper);
end;

procedure TTestIntegrationSetupMySQL.TearDown;
begin
  TearDownDatabase;
  inherited TearDown;
end;

{ TDBLogger — helper class for GlobalDBLogHook (requires method pointer) }

type
  TDBLogger = class
  private
    FFile: TextFile;
    FActive: Boolean;
  public
    constructor Create(const aFileName: String);
    destructor Destroy; override;
    procedure LogEvent(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
  end;

var
  DBLogger: TDBLogger = nil;

constructor TDBLogger.Create(const aFileName: String);
begin
  inherited Create;
  AssignFile(FFile, aFileName);
  Rewrite(FFile);
  FActive:=True;
end;

destructor TDBLogger.Destroy;
begin
  if FActive then
    CloseFile(FFile);
  inherited Destroy;
end;

procedure TDBLogger.LogEvent(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
var
  CN: String;
begin
  if not FActive then Exit;
  if Assigned(Sender) then
    begin
    CN:=Sender.Name;
    if CN='' then
      CN:=Sender.ClassName;
    end
  else
    CN:='(nil)';
  WriteLn(FFile, FormatDateTime('hh:nn:ss.zzz', Now), ' [', CN, '] (',EventType,'): ',Msg);
  Flush(FFile);
end;

{ SetupLogging }

procedure SetupLogging;
var
  Ini: TIniFile;
  IniPath, LogFile: String;
begin
  IniPath:=ExtractFilePath(ParamStr(0)) + 'databases.ini';
  if not FileExists(IniPath) then
    Exit;
  Ini:=TIniFile.Create(IniPath);
  try
    if not Ini.ReadBool('Logging', 'Enabled', False) then
      Exit;
    LogFile:=Ini.ReadString('Logging', 'FileName', '');
    if LogFile='' then
      LogFile:=ExtractFilePath(ParamStr(0)) + 'sqldb.log';
    DBLogger:=TDBLogger.Create(LogFile);
    GlobalDBLogHook:=@DBLogger.LogEvent;
  finally
    Ini.Free;
  end;
end;

finalization
  FreeAndNil(DBLogger);

end.
