{ demo_custom_sql - User-provided SQL with ORM result mapping
  ==============================================================
  This demo shows how to use custom SQL queries while still
  making use of the ORM's result-set-to-object mapping.

  Use cases for custom SQL:
  - Complex JOINs across multiple tables
  - Aggregate queries (GROUP BY, COUNT, SUM)
  - Performance-critical queries that need hand-tuned SQL
  - Queries that don't map to a single table

  The ORM maps result set columns to object properties by name,
  so your custom SQL must use column aliases matching property names.

  Compile:  fpc demo_custom_sql.lpr -Fu../../src
  Run:      ./demo_custom_sql
}
program demo_custom_sql;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_custom_sql.db';

type
  { TEmployee - model for the Employee table }
  TEmployee = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FDepartment: String;
    FSalary: Currency;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Department: String read FDepartment write FDepartment;
    property Salary: Currency read FSalary write FSalary;
  end;

function TEmployee.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TEmployee.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.ExecSQL;
    SetupTrans.Commit;
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
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  ExecSQL('CREATE TABLE Employee ' +
    '(ID INTEGER PRIMARY KEY, Name TEXT, Department TEXT, Salary REAL)');

  OPRegistry.RegisterPersister(TEmployee, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  if Assigned(SetupTrans) then
  begin
    SetupTrans.Free;
    SetupTrans := nil;
  end;
  if Assigned(Conn) then
  begin
    if Conn.Connected then
      Conn.Close;
    Conn.Free;
  end;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

procedure InsertEmployee(aID: Integer; const aName, aDept: String; aSalary: Currency);
var
  Emp: TEmployee;
begin
  Emp := TEmployee.Create;
  try
    Emp.ID := aID;
    Emp.Name := aName;
    Emp.Department := aDept;
    Emp.Salary := aSalary;
    Emp.FState := opsNew;
    OPManager.Save(Emp);
  finally
    Emp.Free;
  end;
end;

var
  Emp: TEmployee;
  EmpList: TObjectList;
  I: Integer;
begin
  SetupDatabase;
  try
    { Seed data }
    InsertEmployee(1, 'Alice',   'Engineering', 95000);
    InsertEmployee(2, 'Bob',     'Engineering', 85000);
    InsertEmployee(3, 'Carol',   'Sales',       72000);
    InsertEmployee(4, 'Dave',    'Sales',       68000);
    InsertEmployee(5, 'Eve',     'Engineering', 110000);
    WriteLn('Inserted 5 employees.');
    WriteLn;

    { Load a single object with parameters:
      Load one employee using a custom WHERE clause.
      The 'array of const' parameter format uses alternating name/value pairs:
      ['ParamName', ParamValue, 'ParamName2', ParamValue2, ...] }
    WriteLn('--- LoadSQL: single object ---');
    Emp := TEmployee.Create;
    try
      OPManager.LoadSQL(Emp,
        'SELECT ID, Name, Department, Salary FROM Employee WHERE ID = :id',
        ['id', 3]);
      WriteLn('Loaded employee #3: Name="', Emp.Name,
              '" Dept="', Emp.Department,
              '" Salary=', CurrToStr(Emp.Salary));
    finally
      Emp.Free;
    end;
    WriteLn;

    { list with parameter:
      Load all employees in a specific department.
      Result set is mapped to a list of TEmployee objects. }
    WriteLn('--- LoadSQL: filtered list ---');
    EmpList := TObjectList.Create(True);
    try
      OPManager.LoadSQL(EmpList, TEmployee,
        'SELECT ID, Name, Department, Salary FROM Employee WHERE Department = :dept',
        ['dept', 'Engineering']);
      WriteLn('Engineering employees (', EmpList.Count, '):');
      for I := 0 to EmpList.Count - 1 do
      begin
        Emp := TEmployee(EmpList[I]);
        WriteLn('  [', I, '] Name="', Emp.Name, '" Salary=', CurrToStr(Emp.Salary));
      end;
    finally
      EmpList.Free;
    end;
    WriteLn;

    { Ordered query:
      Custom SQL with ORDER BY - the ORM maps results in the order returned. }
    WriteLn('--- LoadSQL: ordered by salary ---');
    EmpList := TObjectList.Create(True);
    try
      OPManager.LoadSQL(EmpList, TEmployee,
        'SELECT ID, Name, Department, Salary FROM Employee ORDER BY Salary DESC');
      WriteLn('All employees by salary (descending):');
      for I := 0 to EmpList.Count - 1 do
      begin
        Emp := TEmployee(EmpList[I]);
        WriteLn('  [', I, '] Name="', Emp.Name,
                '" Dept="', Emp.Department,
                '" Salary=', CurrToStr(Emp.Salary));
      end;
    finally
      EmpList.Free;
    end;
    WriteLn;

    { With multiple parameters:
      Multiple parameters can be passed as alternating name/value pairs. }
    WriteLn('--- LoadSQL: multiple parameters ---');
    EmpList := TObjectList.Create(True);
    try
      OPManager.LoadSQL(EmpList, TEmployee,
        'SELECT ID, Name, Department, Salary FROM Employee ' +
        'WHERE Department = :dept AND Salary >= :minsal',
        ['dept', 'Engineering', 'minsal', 90000]);
      WriteLn('Engineering employees earning >= 90000 (', EmpList.Count, '):');
      for I := 0 to EmpList.Count - 1 do
      begin
        Emp := TEmployee(EmpList[I]);
        WriteLn('  [', I, '] Name="', Emp.Name, '" Salary=', CurrToStr(Emp.Salary));
      end;
    finally
      EmpList.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
