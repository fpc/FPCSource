unit tcsetup;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, testdecorator, tsdb, sqldb, pqconnection;

type
  { TDBHelper }

  TDBHelper = class
    class var SQL : TTestSQL;
    class var Conn : TPQConnection;
    class function CreateQuery(const aSQL : String) : TSQLQuery;
    class procedure setup;
    class procedure TearDown;
    class procedure ClearTable(const aTable : string);
    class procedure ClearAllTables;
    class function IDQuery(const aSQL : String) : Int64;
    class procedure ExecAndCommit(Qry: TSQLQuery);
    class procedure ExecSQL(const aSQL: String);
    class function CountRecords(const aTable : String; const aFilter : String = '') : Int64;
    class procedure MaybeRollback;
  end;
  { TDBDecorator }

  TDBDecorator = class(TTestSetup)
  Public
    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  end;

implementation

uses inifiles;


const
  SDatabase   = 'Database';
  KeyName     = 'Name';
  KeyHost     = 'Host';
  KeyUser     = 'UserName';
  KeyPassword = 'Password';
  KeyPort     = 'Port';

{ TDBDecorator }

class function TDBHelper.CreateQuery(const aSQL: String): TSQLQuery;
begin
  Result:=TSQLQuery.Create(Conn);
  Result.DataBase:=Conn;
  Result.Transaction:=Conn.Transaction;
  Result.SQL.Text:=aSQL;
end;

class procedure TDBHelper.setup;

var
  Ini : TCustomIniFile;
  lFileName,lName,lHost,lUser,lPassword: String;
  lPort : Integer;

begin
  lFileName:=ExtractFilePath(Paramstr(0))+'testdigest.ini';
  if not FileExists(lFileName) then
    TAssert.Fail('No config filename %s',[lFileName]);
  Ini:=TMemIniFile.Create(lFileName);
  try
    lName:=Ini.ReadString(SDatabase,KeyName,'');
    if lName='testsuite' then
      TAssert.Fail('As a precaution, test database cannot be called testsuite');
    lHost:=Ini.ReadString(SDatabase,KeyHost,'');
    lUser:=Ini.ReadString(SDatabase,KeyUser,'');
    lPassword:=Ini.ReadString(SDatabase,KeyPassword,'');
    lPort:=Ini.ReadInteger(SDatabase,KeyPort,0);
  finally
    Ini.Free;
  end;
  SQL:=TTestSQL.create(lName,lHost,lUser,lPassword,lPort);
  if not SQL.ConnectToDatabase then
    TAssert.Fail('Could not connect to database');
  Conn:=TPQConnection.Create(Nil);
  Conn.DatabaseName:=lName;
  Conn.HostName:=lHost;
  Conn.UserName:=lUser;
  Conn.Password:=lPassword;
  if lPort<>0 then
    Conn.Params.values['port']:=IntToStr(lPort);
  conn.Transaction:=TSQLTransaction.Create(Conn);
  conn.Connected:=True;
  (*
  l:=TStringList.Create;
  try
    conn.GetTableNames(l);
    writeln('Tables:');
    Writeln('-------');
    Writeln(l.text);
    Writeln('-------');


  finally
    l.Free
  end;
  *)
end;

class procedure TDBHelper.TearDown;
begin
  FreeAndNil(SQL);
  FreeAndNil(Conn);
end;

class procedure TDBHelper.ExecAndCommit(Qry : TSQLQuery);

begin
  if not Qry.SQLTransaction.Active then
    Qry.SQLTransaction.StartTransaction;
  try
    Qry.ExecSQL;
    if Qry.SQLTransaction.Active then
      Qry.SQLTransaction.Commit;
  except
    if Qry.SQLTransaction.Active then
      Qry.SQLTransaction.RollBack;
    Raise;
  end;
end;

class procedure TDBHelper.ExecSQL(const aSQL : String);

var
  Qry : TSQLQuery;
begin
  // Truncate would be faster, but we have foreign keys
  Qry:=CreateQuery(aSQL);
  try
    ExecAndCommit(Qry);
  finally
    Qry.Free;
  end;
end;

class function TDBHelper.CountRecords(const aTable: String; const aFilter: String): Int64;
var
  lSQL : String;
begin
  lSQL:='select count(*) as thecount from '+aTable;
  if aFilter<>'' then
    lSQL:=lSQL+' where '+aFilter;
  Result:=IDQuery(lSQL);
end;

class procedure TDBHelper.MaybeRollback;
begin
  if Assigned(Conn) and Assigned(Conn.Transaction) and Conn.Transaction.Active then
    Conn.Transaction.RollBack;
end;

class procedure TDBHelper.ClearTable(const aTable: string);

begin
  // Truncate would be faster, but we have foreign keys
  ExecSQL('delete from '+aTable);
end;

class procedure TDBHelper.ClearAllTables;
begin
  ClearTable('TESTRUNHISTORY');
  ClearTable('TESTPREVIOUSRESULTS');
  ClearTable('TESTLASTRESULTS');
  ClearTable('TESTRESULTS');
  ClearTable('TESTRUN');
  ClearTable('CHECKALLRTLLOG');
  ClearTable('CHECKALLRTL');
  ClearTable('TESTPLATFORM');
  ClearTable('TESTOS');
  ClearTable('TESTCPU');
  ClearTable('TESTCATEGORY');
  ClearTable('TESTVERSION');
  ClearTable('TESTS');
end;

class function TDBHelper.IDQuery(const aSQL: String): Int64;

var
  Qry : TSQLQuery;

begin
  Qry:=CreateQuery(aSQL);
  try
    if not Qry.SQLTransaction.Active then
      Qry.SQLTransaction.StartTransaction;
    try
      Qry.Open;
      Result:=Qry.Fields[0].AsLargeInt;
      if Qry.SQLTransaction.Active then
        Qry.SQLTransaction.Commit;
    except
      if Qry.SQLTransaction.Active then
        Qry.SQLTransaction.RollBack;
      Raise;
    end;
  finally
    Qry.Free;
  end;
end;


procedure TDBDecorator.OneTimeSetup;

begin
  TDBHelper.Setup;
end;

procedure TDBDecorator.OneTimeTearDown;
begin
  TDBHelper.TearDown;
end;


end.

