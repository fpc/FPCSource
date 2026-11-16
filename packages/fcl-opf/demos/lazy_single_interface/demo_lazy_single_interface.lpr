{ demo_lazy_single_interface - Lazy loading of N:1 single object via IOPLazyInit
  ===============================================================================
  This demo shows how to use TOPLazy<T> with the IOPLazyInit interface
  (FPC 3.2.2 compatible) to defer loading of a single related parent object
  (N:1 reverse reference) until the property is first accessed.

  Domain: TEmployee has a FK DepartmentID pointing to TDepartment.
  The lazy field wraps a TDepartment instance and loads it on demand.

  Compile:  fpc demo_lazy_single_interface.lpr -Fu../../src
  Run:      ./demo_lazy_single_interface
}
program demo_lazy_single_interface;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Relations,
  Opf.Proxy,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_lazy_single_interface.db';

type
  TOPLazyDepartment = specialize TOPLazy<TObject>;

  { TDepartment:
    The parent entity }
  TDepartment = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TEmployee: 
    Child with lazy N:1 reference to department via IOPLazyInit }
  TEmployee = class(TObject, IOPPersistenceState, IOPLazyInit)
  private
    FID: Integer;
    FDepartmentID: Integer;
    FEmpName: String;
    FState: TObjectPersistenceState;
    FDepartment: TDepartment;
    FLazyDepartment: TOPLazyDepartment;
    FManager: TObject;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetDepartment: TDepartment;
    procedure DoLoadDepartment(aTarget: TObject);
    procedure InitLazyLoaders(aManager: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property DepartmentID: Integer read FDepartmentID write FDepartmentID;
    property EmpName: String read FEmpName write FEmpName;
  end;

{ TDepartment implementation }

function TDepartment.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TDepartment.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ TEmployee implementation }

function TEmployee.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TEmployee.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TEmployee.Create;
begin
  inherited Create;
  FDepartment := TDepartment.Create;
  FLazyDepartment.Init(FDepartment);
end;

destructor TEmployee.Destroy;
begin
  FDepartment.Free;
  inherited Destroy;
end;

function TEmployee.GetDepartment: TDepartment;
begin
  Result := TDepartment(FLazyDepartment.Value);
end;

procedure TEmployee.DoLoadDepartment(aTarget: TObject);
begin
  { For N:1: set FK as PK on target, then Load }
  TDepartment(aTarget).ID := Self.DepartmentID;
  if Self.DepartmentID <> 0 then
    TOPManager(FManager).Load(aTarget);
end;

procedure TEmployee.InitLazyLoaders(aManager: TObject);
begin
  FManager := aManager;
  FLazyDepartment.SetLoader(@DoLoadDepartment);
end;

{ Database helpers }

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := Trans;
    Q.SQL.Text := aSQL;
    if not Trans.Active then
      Trans.StartTransaction;
    Q.ExecSQL;
    Trans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  Trans := TSQLTransaction.Create(Conn);
  Conn.Transaction := Trans;
  Conn.Open;

  ExecSQL('CREATE TABLE Department (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQL('CREATE TABLE Employee (ID INTEGER PRIMARY KEY, DepartmentID INTEGER, EmpName TEXT)');

  OPRegistry.RegisterPersister(TDepartment, TRTTIPersister);
  OPRegistry.RegisterPersister(TEmployee, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TDepartment, TEmployee, 'DepartmentID');

  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  Trans.Free;
  Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ Main }

var
  Dept: TDepartment;
  Emp: TEmployee;
begin
  SetupDatabase;
  try
    WriteLn('--- INSERT ---');
    Dept := TDepartment.Create;
    try
      Dept.ID := 1;
      Dept.Name := 'Engineering';
      Dept.FState := opsNew;
      OPManager.Save(Dept);
      WriteLn('Saved department: "', Dept.Name, '"');
    finally
      Dept.Free;
    end;

    Emp := TEmployee.Create;
    try
      Emp.ID := 10;
      Emp.DepartmentID := 1;
      Emp.EmpName := 'Alice';
      Emp.FState := opsNew;
      OPManager.Save(Emp);
      WriteLn('Saved employee: "', Emp.EmpName, '" DeptID=', Emp.DepartmentID);
    finally
      Emp.Free;
    end;
    WriteLn;

    { Load employee - department is NOT loaded yet }
    WriteLn('--- LAZY LOAD N:1 ---');
    Emp := TEmployee.Create;
    try
      Emp.ID := 10;
      Emp.FState := opsKey;
      OPManager.Load(Emp);
      WriteLn('Loaded employee: "', Emp.EmpName, '" DeptID=', Emp.DepartmentID);
      WriteLn('Department loaded yet? ', Emp.FLazyDepartment.IsLoaded);

      WriteLn('Accessing Department...');
      Dept := Emp.GetDepartment;
      WriteLn('Department: ID=', Dept.ID, ' Name="', Dept.Name, '"');
      WriteLn('Department loaded now? ', Emp.FLazyDepartment.IsLoaded);
    finally
      Emp.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
