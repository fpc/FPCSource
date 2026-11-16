{ demo_criteria - Parameterized criteria API with pagination
  =============================================================
  This demo shows the type-safe, parameterized criteria builder
  for constructing WHERE clauses without raw SQL strings.

  Features demonstrated:
  - Basic comparisons (Equal, Greater, Like, Contains)
  - Nested sub-expressions with AddAnd/AddOr
  - IN lists and BETWEEN ranges
  - NOT negation
  - ORDER BY with ASC/DESC
  - LIMIT/OFFSET pagination
  - IncludeDeleted for soft-delete bypass
  - Property-to-field-name resolution (users write property names,
    SQL uses database column names)

  All values are parameterized - no SQL injection risk.

  Compile:  lazbuild demo_criteria.lpi
  Run:      ./demo_criteria
}
program demo_criteria;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Criteria,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_criteria.db';

type

  { TEmployee - model with several filterable fields }

  TEmployee = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FFirstName: String;
    FLastName: String;
    FDepartment: String;
    FAge: Integer;
    FSalary: Double;
    FActive: Boolean;
    FDeleted: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property FirstName: String read FFirstName write FFirstName;
    property LastName: String read FLastName write FLastName;
    property Department: String read FDepartment write FDepartment;
    property Age: Integer read FAge write FAge;
    property Salary: Double read FSalary write FSalary;
    property Active: Boolean read FActive write FActive;
    property Deleted: Boolean read FDeleted write FDeleted;
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
  Def: TPersisterDef;
  Field: TOPFieldMap;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  OPSQLBuilder.SQLDialect := dSQLite;

  Def := OPRegistry.RegisterPersister(TEmployee, TRTTIPersister);
  Def.TableName := 'Employees';
  Field := Def.FieldMap.FindByPropertyName('Deleted');
  Field.Options := Field.Options + [foSoftDelete];

  ExecSQL('CREATE TABLE Employees (ID INTEGER PRIMARY KEY, ' +
          'FirstName TEXT, LastName TEXT, Department TEXT, ' +
          'Age INTEGER, Salary REAL, Active BOOLEAN, Deleted BOOLEAN)');

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure InsertEmployee(aID: Integer; const aFirst, aLast, aDept: String;
  aAge: Integer; aSalary: Double; aActive: Boolean);
var
  E: TEmployee;
begin
  E := TEmployee.Create;
  try
    E.ID := aID;
    E.FirstName := aFirst;
    E.LastName := aLast;
    E.Department := aDept;
    E.Age := aAge;
    E.Salary := aSalary;
    E.Active := aActive;
    E.FState := opsNew;
    OPManager.Save(E);
  finally
    E.Free;
  end;
end;

procedure SeedData;
begin
  InsertEmployee(1,  'Alice',   'Smith',    'Engineering', 32, 95000,  True);
  InsertEmployee(2,  'Bob',     'Jones',    'Engineering', 28, 82000,  True);
  InsertEmployee(3,  'Charlie', 'Brown',    'Marketing',   45, 75000,  True);
  InsertEmployee(4,  'Diana',   'Prince',   'Marketing',   38, 88000,  False);
  InsertEmployee(5,  'Eve',     'Davis',    'Sales',       25, 65000,  True);
  InsertEmployee(6,  'Frank',   'Miller',   'Sales',       52, 92000,  True);
  InsertEmployee(7,  'Grace',   'Lee',      'Engineering', 29, 87000,  True);
  InsertEmployee(8,  'Hank',    'Wilson',   'HR',          41, 71000,  False);
  InsertEmployee(9,  'Ivy',     'Taylor',   'HR',          33, 68000,  True);
  InsertEmployee(10, 'Jack',    'Anderson', 'Engineering', 36, 99000,  True);
  WriteLn('Seeded 10 employees.');
  WriteLn;
end;

procedure PrintList(aList: TObjectList; const aTitle: String);
var
  I: Integer;
  E: TEmployee;
begin
  WriteLn(aTitle, ' (', aList.Count, ' results):');
  for I := 0 to aList.Count - 1 do
  begin
    E := TEmployee(aList[I]);
    WriteLn(Format('  [%d] %s %s | %s | Age=%d | Salary=%.0f | Active=%s',
      [E.ID, E.FirstName, E.LastName, E.Department, E.Age, E.Salary,
       BoolToStr(E.Active, 'Yes', 'No')]));
  end;
  WriteLn;
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

var
  List: TObjectList;
  C: TOPCriteria;
  Emp: TEmployee;
begin
  SetupDatabase;
  try
    SeedData;

    {  Part 1: Simple comparison }
    WriteLn('=== Part 1: Simple comparison ===');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Department', opEqual, 'Engineering');
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Department = Engineering');
    finally
      List.Free;
      C.Free;
    end;

    {  Part 2: Multiple conditions with AND }
    WriteLn('=== Part 2: AND conditions ===');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Active', opEqual, True)
       .AddAnd('Salary', opGreaterOrEqual, Double(80000));
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Active=True AND Salary >= 80000');
    finally
      List.Free;
      C.Free;
    end;

    { Part 3: Nested sub-expression }
    WriteLn('=== Part 3: Nested sub-expression ===');
    WriteLn('Active AND (Department=Engineering OR Department=Sales)');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Active', opEqual, True)
       .AddAnd(TOPCriteria.Create
         .Where('Department', opEqual, 'Engineering')
         .AddOr('Department', opEqual, 'Sales'));
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Active AND (Eng OR Sales)');
    finally
      List.Free;
      C.Free;
    end;

    { Part 4: Contains (LIKE with wildcards) }
    WriteLn('=== Part 4: Contains ===');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('LastName', opContains, 'on');
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'LastName contains "on"');
    finally
      List.Free;
      C.Free;
    end;

    { Part 5: IN list }
    WriteLn('=== Part 5: IN list ===');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.WhereIn('Department', ['HR', 'Marketing']);
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Department IN (HR, Marketing)');
    finally
      List.Free;
      C.Free;
    end;

    { Part 6: BETWEEN range }
    WriteLn('=== Part 6: BETWEEN range ===');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.WhereBetween('Age', [30, 40]);
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Age BETWEEN 30 AND 40');
    finally
      List.Free;
      C.Free;
    end;

    { Part 7: NOT }
    WriteLn('=== Part 7: NOT ===');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Department', opEqual, 'Engineering').Not_;
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'NOT (Department = Engineering)');
    finally
      List.Free;
      C.Free;
    end;

    { Part 8: ORDER BY + LIMIT + OFFSET (pagination) }
    WriteLn('=== Part 8: Pagination ===');
    WriteLn('Top 3 by salary (descending):');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Active', opEqual, True)
       .OrderBy('Salary', True)
       .Limit(3);
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Top 3 active by salary');
    finally
      List.Free;
      C.Free;
    end;

    WriteLn('Page 2 (skip 3, take 3):');
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Active', opEqual, True)
       .OrderBy('Salary', True)
       .Limit(3).Offset(3);
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Page 2 active by salary');
    finally
      List.Free;
      C.Free;
    end;

    { Part 9: Soft delete + IncludeDeleted }
    WriteLn('=== Part 9: Soft delete + IncludeDeleted ===');

    // Soft-delete employee #4
    Emp := TEmployee.Create;
    try
      Emp.ID := 4;
      OPManager.Load(Emp);
      WriteLn('Soft-deleting: ', Emp.FirstName, ' ', Emp.LastName);
      OPManager.Delete(Emp);
    finally
      Emp.Free;
    end;

    WriteLn;
    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Department', opEqual, 'Marketing');
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Marketing (soft-deleted filtered)');
    finally
      List.Free;
      C.Free;
    end;

    C := TOPCriteria.Create;
    List := TObjectList.Create(True);
    try
      C.Where('Department', opEqual, 'Marketing')
       .IncludeDeleted;
      OPManager.Load(List, TEmployee, C);
      PrintList(List, 'Marketing (IncludeDeleted)');
    finally
      List.Free;
      C.Free;
    end;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
