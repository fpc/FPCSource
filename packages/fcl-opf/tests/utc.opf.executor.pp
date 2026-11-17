unit utc.opf.executor;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, db, sqldb,
  sqlite3conn,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Registry, Opf.Fields, Opf.Sql, Opf.Executor, Opf.ListReg,
{$IFDEF FPC_DOTTEDUNITS}
  System.Contnrs;
{$ELSE}
  contnrs;
{$ENDIF}

Type

  { TTestObject - simple object for testing }

  TTestObject = class
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
  public
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TTestPersister }

  TTestPersister = class(TOPPersister)
  private
    FCallCount: Integer;
    FFieldNames: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    function ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean; override;
    function PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean; override;
    property CallCount: Integer read FCallCount;
    property FieldNames: TStringList read FFieldNames;
  end;

  { TTestOPExecutor }

  TTestOPExecutor = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FFieldMap: TOPFieldMapList;
    FPersister: TTestPersister;
    FObj: TTestObject;
    FExecutor: TOPExecutor;
    procedure CreateTestTable;
    function CountRows: Integer;
    procedure DoSetExecutorInstanceClass;
    procedure DoExecNilConnection;
    procedure DoExecNilPersister;
    procedure DoExecEmptyFieldMap;
    procedure DoExecNilObject;
    procedure DoExecEmptySQL;
    procedure DoExecSQLFailure;
    procedure DoQueryMissingColumn;
    procedure DoQuerySQLNilList;
    procedure DoQuerySQLNilManipulator;
    procedure InsertRow(aID: Integer; const aName: String; aAge: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteSQLBindsParameters;
    procedure TestExecuteSQLUpdate;
    procedure TestExecuteSQLDelete;
    procedure TestExecuteSQLNilConnection;
    procedure TestExecuteSQLNilPersister;
    procedure TestExecuteSQLEmptyFieldMap;
    procedure TestExecuteSQLNilObject;
    procedure TestExecuteSQLEmptySQL;
    procedure TestExecuteSQLExecFailure;
    procedure TestQuerySQLSingleObject;
    procedure TestQuerySQLUnmappedColumns;
    procedure TestQuerySQLMissingColumn;
    procedure TestQuerySQLListMultipleRows;
    procedure TestQuerySQLListEmptyResult;
    procedure TestQuerySQLOpensConnection;
    procedure TestQuerySQLNilList;
    procedure TestQuerySQLNilManipulator;
    procedure TestExecuteSQLOpensConnection;
    procedure TestClearInstanceResetsClass;
    procedure TestSetInstanceClassGuard;
  end;

  TCustomExecutor = class(TOPExecutor);

  { TTestExecutorErrorDiagnostics }

  TTestExecutorErrorDiagnostics = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FFieldMap: TOPFieldMapList;
    FPersister: TTestPersister;
    FObj: TTestObject;
    FExecutor: TOPExecutor;
    procedure CreateTestTable;
    procedure InsertRow(aID: Integer; const aName: String; aAge: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExecuteSQLNilConnectionMessage;
    procedure TestExecuteSQLNilObjectMessage;
    procedure TestExecuteSQLEmptySQLMessage;
    procedure TestQuerySQLMissingColumnMessage;
  end;

implementation

{ TTestPersister }

constructor TTestPersister.Create;
begin
  inherited Create;
  FCallCount:=0;
  FFieldNames:=TStringList.Create;
end;

destructor TTestPersister.Destroy;
begin
  FreeAndNil(FFieldNames);
  inherited Destroy;
end;

procedure TTestPersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
begin
  // do nothing - tests populate manually
end;

function TTestPersister.ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean;
var
  Obj: TTestObject;
begin
  Result:=True;
  Obj:=Instance as TTestObject;
  Inc(FCallCount);
  FFieldNames.Add(aFieldMap.FieldName);
  if SameText(aFieldMap.PropertyName, 'ID') then
    Obj.ID:=aField.AsInteger
  else if SameText(aFieldMap.PropertyName, 'Name') then
    Obj.Name:=aField.AsString
  else if SameText(aFieldMap.PropertyName, 'Age') then
    Obj.Age:=aField.AsInteger
  else
    Result:=False;
end;

function TTestPersister.PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
var
  Obj: TTestObject;
begin
  Result:=True;
  Obj:=Instance as TTestObject;
  Inc(FCallCount);
  FFieldNames.Add(aFieldMap.FieldName);
  if SameText(aFieldMap.PropertyName, 'ID') then
    aParam.AsInteger:=Obj.ID
  else if SameText(aFieldMap.PropertyName, 'Name') then
    aParam.AsString:=Obj.Name
  else if SameText(aFieldMap.PropertyName, 'Age') then
    aParam.AsInteger:=Obj.Age
  else
    Result:=False;
end;

{ TTestOPExecutor }

procedure TTestOPExecutor.CreateTestTable;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='CREATE TABLE TestPerson (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestOPExecutor.CountRows: Integer;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='SELECT COUNT(*) FROM TestPerson';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
    if FTrans.Active then
      FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestOPExecutor.SetUp;
begin
  inherited SetUp;
  FConn:=TSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  CreateTestTable;
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  FPersister:=TTestPersister.Create;
  FObj:=TTestObject.Create;
  FObj.ID:=1;
  FObj.Name:='Alice';
  FObj.Age:=30;
  FExecutor:=TOPExecutor.Create(Nil);
end;

procedure TTestOPExecutor.TearDown;
begin
  FreeAndNil(FExecutor);
  FreeAndNil(FObj);
  FreeAndNil(FPersister);
  FreeAndNil(FFieldMap);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestOPExecutor.TestExecuteSQLBindsParameters;
var
  SQL: UTF8String;
  Q: TSQLQuery;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInInsert]);
  FFieldMap.AddField('Age', 'Age', [foInInsert]);
  SQL:=OPSQLBuilder.GetInsertSQL('TestPerson', FFieldMap.AsArray);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, SQL, FConn);
  // Verify row was inserted
  AssertEquals('Row count after insert', 1, CountRows);
  // Verify persister was called for all 3 fields with correct names
  AssertEquals('PersistProperty call count', 3, FPersister.CallCount);
  AssertEquals('Field 0 bound', 'ID', FPersister.FieldNames[0]);
  AssertEquals('Field 1 bound', 'Name', FPersister.FieldNames[1]);
  AssertEquals('Field 2 bound', 'Age', FPersister.FieldNames[2]);
  // Verify values by reading back
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='SELECT ID, Name, Age FROM TestPerson WHERE ID=1';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.Open;
    AssertFalse('Query should return a row', Q.EOF);
    AssertEquals('ID value', 1, Q.FieldByName('ID').AsInteger);
    AssertEquals('Name value', 'Alice', Q.FieldByName('Name').AsString);
    AssertEquals('Age value', 30, Q.FieldByName('Age').AsInteger);
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TTestOPExecutor.TestExecuteSQLUpdate;
var
  InsertSQL, UpdateSQL: UTF8String;
  Q: TSQLQuery;
begin
  // First insert a row
  FFieldMap.AddField('ID', 'ID', [foInKey, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInInsert, foInUpdate]);
  FFieldMap.AddField('Age', 'Age', [foInInsert, foInUpdate]);
  InsertSQL:=OPSQLBuilder.GetInsertSQL('TestPerson', FFieldMap.AsArray);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, InsertSQL, FConn);
  // Now update
  FObj.Name:='Bob';
  FObj.Age:=25;
  UpdateSQL:=OPSQLBuilder.GetUpdateSQL('TestPerson', FFieldMap.AsArray);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, UpdateSQL, FConn);
  // Verify values changed
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='SELECT Name, Age FROM TestPerson WHERE ID=1';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.Open;
    AssertFalse('Query should return a row', Q.EOF);
    AssertEquals('Updated name', 'Bob', Q.FieldByName('Name').AsString);
    AssertEquals('Updated age', 25, Q.FieldByName('Age').AsInteger);
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TTestOPExecutor.TestExecuteSQLDelete;
var
  InsertSQL, DeleteSQL: UTF8String;
begin
  // Insert first
  FFieldMap.AddField('ID', 'ID', [foInKey, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInInsert]);
  FFieldMap.AddField('Age', 'Age', [foInInsert]);
  InsertSQL:=OPSQLBuilder.GetInsertSQL('TestPerson', FFieldMap.AsArray);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, InsertSQL, FConn);
  AssertEquals('Row count after insert', 1, CountRows);
  // Now delete
  DeleteSQL:=OPSQLBuilder.GetDeleteSQL('TestPerson', FFieldMap.AsArray);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, DeleteSQL, FConn);
  AssertEquals('Row count after delete', 0, CountRows);
end;

procedure TTestOPExecutor.DoExecNilConnection;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, 'DELETE FROM TestPerson WHERE ID=:ID', Nil);
end;

procedure TTestOPExecutor.TestExecuteSQLNilConnection;
begin
  AssertException('Nil connection raises EOPFExecutor', EOPFExecutor, @DoExecNilConnection);
end;

procedure TTestOPExecutor.DoExecNilPersister;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  FExecutor.ExecuteSQL(FObj, Nil, FFieldMap, 'DELETE FROM TestPerson WHERE ID=:ID', FConn);
end;

procedure TTestOPExecutor.TestExecuteSQLNilPersister;
begin
  AssertException('Nil persister raises EOPFExecutor', EOPFExecutor, @DoExecNilPersister);
end;

procedure TTestOPExecutor.DoExecEmptyFieldMap;
var
  EmptyMap: TOPFieldMapList;
begin
  EmptyMap:=TOPFieldMapList.Create(TOPFieldMap);
  try
    FExecutor.ExecuteSQL(FObj, FPersister, EmptyMap, 'DELETE FROM TestPerson WHERE ID=:ID', FConn);
  finally
    EmptyMap.Free;
  end;
end;

procedure TTestOPExecutor.TestExecuteSQLEmptyFieldMap;
begin
  AssertException('Empty field map raises EOPFExecutor', EOPFExecutor, @DoExecEmptyFieldMap);
end;

procedure TTestOPExecutor.DoExecNilObject;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  FExecutor.ExecuteSQL(Nil, FPersister, FFieldMap, 'DELETE FROM TestPerson WHERE ID=:ID', FConn);
end;

procedure TTestOPExecutor.TestExecuteSQLNilObject;
begin
  AssertException('Nil object raises EOPFExecutor', EOPFExecutor, @DoExecNilObject);
end;

procedure TTestOPExecutor.DoExecEmptySQL;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, '', FConn);
end;

procedure TTestOPExecutor.TestExecuteSQLEmptySQL;
begin
  AssertException('Empty SQL raises EOPFExecutor', EOPFExecutor, @DoExecEmptySQL);
end;

procedure TTestOPExecutor.DoExecSQLFailure;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInInsert]);
  FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap,
    'INSERT INTO NonExistentTable (ID, Name) VALUES (:ID, :Name)', FConn);
end;

procedure TTestOPExecutor.TestExecuteSQLExecFailure;
begin
  AssertException('SQL failure raises EOPFExecutor', EOPFExecutor, @DoExecSQLFailure);
end;

procedure TTestOPExecutor.InsertRow(aID: Integer; const aName: String; aAge: Integer);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='INSERT INTO TestPerson (ID, Name, Age) VALUES (:ID, :Name, :Age)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.Params.ParamByName('ID').AsInteger:=aID;
    Q.Params.ParamByName('Name').AsString:=aName;
    Q.Params.ParamByName('Age').AsInteger:=aAge;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestOPExecutor.TestQuerySQLSingleObject;
begin
  InsertRow(1, 'Alice', 30);
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  FFieldMap.AddField('Name', 'Name', [foInSelect]);
  FFieldMap.AddField('Age', 'Age', [foInSelect]);
  FObj.ID:=0;
  FObj.Name:='';
  FObj.Age:=0;
  FExecutor.QuerySQL(FObj, FPersister, FFieldMap,
    'SELECT ID, Name, Age FROM TestPerson WHERE ID=1', FConn);
  AssertEquals('ID populated', 1, FObj.ID);
  AssertEquals('Name populated', 'Alice', FObj.Name);
  AssertEquals('Age populated', 30, FObj.Age);
end;

procedure TTestOPExecutor.TestQuerySQLUnmappedColumns;
begin
  InsertRow(1, 'Alice', 30);
  // Field map only maps ID and Name — Age is an unmapped result column
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  FFieldMap.AddField('Name', 'Name', [foInSelect]);
  FObj.ID:=0;
  FObj.Name:='';
  FObj.Age:=0;
  FExecutor.QuerySQL(FObj, FPersister, FFieldMap,
    'SELECT ID, Name, Age FROM TestPerson WHERE ID=1', FConn);
  AssertEquals('ID populated', 1, FObj.ID);
  AssertEquals('Name populated', 'Alice', FObj.Name);
  // Age was not mapped so should remain at reset value
  AssertEquals('Age unchanged', 0, FObj.Age);
end;

procedure TTestOPExecutor.DoQueryMissingColumn;
begin
  InsertRow(1, 'Alice', 30);
  // Field map expects 'Email' which doesn't exist in the table
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  FFieldMap.AddField('Email', 'Email', [foInSelect]);
  FExecutor.QuerySQL(FObj, FPersister, FFieldMap,
    'SELECT ID FROM TestPerson WHERE ID=1', FConn);
end;

procedure TTestOPExecutor.TestQuerySQLMissingColumn;
begin
  AssertException('Missing column raises EOPFExecutor', EOPFExecutor, @DoQueryMissingColumn);
end;

procedure TTestOPExecutor.TestQuerySQLListMultipleRows;
var
  List: TFPObjectList;
  Manip: TOPListManipulator;
  Obj: TTestObject;
begin
  InsertRow(1, 'Alice', 30);
  InsertRow(2, 'Bob', 25);
  InsertRow(3, 'Carol', 35);
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  FFieldMap.AddField('Name', 'Name', [foInSelect]);
  FFieldMap.AddField('Age', 'Age', [foInSelect]);
  Manip:=OPListRegistry.FindManipulator(TFPObjectList);
  List:=TFPObjectList.Create(True);
  try
    FExecutor.QuerySQLList(List, Manip, TTestObject, FPersister, FFieldMap,
      'SELECT ID, Name, Age FROM TestPerson ORDER BY ID', FConn);
    AssertEquals('List count', 3, List.Count);
    Obj:=List[0] as TTestObject;
    AssertEquals('Row 1 ID', 1, Obj.ID);
    AssertEquals('Row 1 Name', 'Alice', Obj.Name);
    AssertEquals('Row 1 Age', 30, Obj.Age);
    Obj:=List[1] as TTestObject;
    AssertEquals('Row 2 ID', 2, Obj.ID);
    AssertEquals('Row 2 Name', 'Bob', Obj.Name);
    Obj:=List[2] as TTestObject;
    AssertEquals('Row 3 ID', 3, Obj.ID);
    AssertEquals('Row 3 Name', 'Carol', Obj.Name);
    AssertEquals('Row 3 Age', 35, Obj.Age);
  finally
    List.Free;
  end;
end;

procedure TTestOPExecutor.TestQuerySQLListEmptyResult;
var
  List: TFPObjectList;
  Manip: TOPListManipulator;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  FFieldMap.AddField('Name', 'Name', [foInSelect]);
  Manip:=OPListRegistry.FindManipulator(TFPObjectList);
  List:=TFPObjectList.Create(True);
  try
    FExecutor.QuerySQLList(List, Manip, TTestObject, FPersister, FFieldMap,
      'SELECT ID, Name FROM TestPerson WHERE 1=0', FConn);
    AssertEquals('List count', 0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestOPExecutor.TestQuerySQLOpensConnection;
var
  Conn2: TSQLite3Connection;
  Trans2: TSQLTransaction;
  Q: TSQLQuery;
  DBFile: String;
begin
  // Use a file-based database so data persists across close/reopen
  DBFile:=GetTempDir + 'test_opf_conn.db';
  if FileExists(DBFile) then
    DeleteFile(DBFile);
  Conn2:=TSQLite3Connection.Create(Nil);
  Trans2:=TSQLTransaction.Create(Nil);
  try
    Conn2.DatabaseName:=DBFile;
    Trans2.Database:=Conn2;
    Conn2.Transaction:=Trans2;
    Conn2.Open;
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=Conn2;
      Q.Transaction:=Trans2;
      Q.SQL.Text:='CREATE TABLE TestPerson (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)';
      if not Trans2.Active then
        Trans2.StartTransaction;
      Q.ExecSQL;
      Trans2.Commit;
    finally
      Q.Free;
    end;
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=Conn2;
      Q.Transaction:=Trans2;
      Q.SQL.Text:='INSERT INTO TestPerson (ID, Name, Age) VALUES (1, ''Test'', 20)';
      if not Trans2.Active then
        Trans2.StartTransaction;
      Q.ExecSQL;
      Trans2.Commit;
    finally
      Q.Free;
    end;
    Conn2.Close;
    AssertFalse('Connection should be closed', Conn2.Connected);
    // Now QuerySQL should open it automatically
    FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
    FFieldMap.AddField('Name', 'Name', [foInSelect]);
    FObj.ID:=0;
    FObj.Name:='';
    FExecutor.QuerySQL(FObj, FPersister, FFieldMap,
      'SELECT ID, Name FROM TestPerson WHERE ID=1', Conn2);
    AssertTrue('Connection should now be open', Conn2.Connected);
    AssertEquals('ID populated', 1, FObj.ID);
    AssertEquals('Name populated', 'Test', FObj.Name);
  finally
    if Conn2.Connected then
      Conn2.Close;
    Trans2.Free;
    Conn2.Free;
    if FileExists(DBFile) then
      DeleteFile(DBFile);
  end;
end;

procedure TTestOPExecutor.DoQuerySQLNilList;
var
  Manip: TOPListManipulator;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  Manip:=OPListRegistry.FindManipulator(TFPObjectList);
  FExecutor.QuerySQLList(Nil, Manip, TTestObject, FPersister, FFieldMap,
    'SELECT ID FROM TestPerson', FConn);
end;

procedure TTestOPExecutor.TestQuerySQLNilList;
begin
  AssertException('Nil list raises EOPFExecutor', EOPFExecutor, @DoQuerySQLNilList);
end;

procedure TTestOPExecutor.DoQuerySQLNilManipulator;
var
  List: TFPObjectList;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  List:=TFPObjectList.Create(True);
  try
    FExecutor.QuerySQLList(List, Nil, TTestObject, FPersister, FFieldMap,
      'SELECT ID FROM TestPerson', FConn);
  finally
    List.Free;
  end;
end;

procedure TTestOPExecutor.TestQuerySQLNilManipulator;
begin
  AssertException('Nil manipulator raises EOPFExecutor', EOPFExecutor, @DoQuerySQLNilManipulator);
end;

procedure TTestOPExecutor.TestExecuteSQLOpensConnection;
var
  Conn2: TSQLite3Connection;
  Trans2: TSQLTransaction;
  Q: TSQLQuery;
  DBFile: String;
  RowCount: Integer;
begin
  DBFile:=GetTempDir + 'test_opf_exec_conn.db';
  if FileExists(DBFile) then
    DeleteFile(DBFile);
  Conn2:=TSQLite3Connection.Create(Nil);
  Trans2:=TSQLTransaction.Create(Nil);
  try
    Conn2.DatabaseName:=DBFile;
    Trans2.Database:=Conn2;
    Conn2.Transaction:=Trans2;
    Conn2.Open;
    // Create table
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=Conn2;
      Q.Transaction:=Trans2;
      Q.SQL.Text:='CREATE TABLE TestPerson (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)';
      if not Trans2.Active then
        Trans2.StartTransaction;
      Q.ExecSQL;
      Trans2.Commit;
    finally
      Q.Free;
    end;
    // Close connection
    Conn2.Close;
    AssertFalse('Connection should be closed', Conn2.Connected);
    // ExecuteSQL should auto-open the connection
    FFieldMap.AddField('ID', 'ID', [foInKey, foInInsert]);
    FFieldMap.AddField('Name', 'Name', [foInInsert]);
    FFieldMap.AddField('Age', 'Age', [foInInsert]);
    FObj.ID:=1;
    FObj.Name:='Test';
    FObj.Age:=20;
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap,
      'INSERT INTO TestPerson (ID, Name, Age) VALUES (:ID, :Name, :Age)', Conn2);
    AssertTrue('Connection should now be open', Conn2.Connected);
    // Verify the row was actually inserted
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=Conn2;
      Q.Transaction:=Trans2;
      Q.SQL.Text:='SELECT COUNT(*) FROM TestPerson';
      if not Trans2.Active then
        Trans2.StartTransaction;
      Q.Open;
      RowCount:=Q.Fields[0].AsInteger;
      Q.Close;
      if Trans2.Active then
        Trans2.Commit;
    finally
      Q.Free;
    end;
    AssertEquals('Row should be inserted', 1, RowCount);
  finally
    if Conn2.Connected then
      Conn2.Close;
    Trans2.Free;
    Conn2.Free;
    if FileExists(DBFile) then
      DeleteFile(DBFile);
  end;
end;

procedure TTestOPExecutor.TestClearInstanceResetsClass;
var
  Inst: TOPExecutor;
begin
  TOPExecutor.ClearInstance;
  TOPExecutor.SetInstanceClass(TCustomExecutor);
  Inst:=TOPExecutor.Instance;
  AssertNotNull('Instance should exist', Inst);
  AssertTrue('Instance should be TCustomExecutor', Inst.ClassType = TCustomExecutor);
  // Clear and get a new instance — should use default class, not TCustomExecutor
  TOPExecutor.ClearInstance;
  Inst:=TOPExecutor.Instance;
  AssertNotNull('Instance after clear should exist', Inst);
  AssertTrue('Instance should be TOPExecutor (default)', Inst.ClassType = TOPExecutor);
  TOPExecutor.ClearInstance;
end;

procedure TTestOPExecutor.DoSetExecutorInstanceClass;
begin
  TOPExecutor.SetInstanceClass(TCustomExecutor);
end;

procedure TTestOPExecutor.TestSetInstanceClassGuard;
var
  Inst: TOPExecutor;
begin
  TOPExecutor.ClearInstance;
  TOPExecutor.SetInstanceClass(TCustomExecutor);
  try
    Inst:=TOPExecutor.Instance;
    AssertNotNull('Have instance', Inst);
    AssertEquals('Correct class', TCustomExecutor, Inst.ClassType);
    AssertException('Cannot set class if instance is active', EOPFExecutor, @Self.DoSetExecutorInstanceClass);
  finally
    TOPExecutor.ClearInstance;
    TOPExecutor.SetInstanceClass(Nil);
  end;
end;

{ TTestExecutorErrorDiagnostics }

procedure TTestExecutorErrorDiagnostics.CreateTestTable;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='CREATE TABLE TestPerson (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestExecutorErrorDiagnostics.InsertRow(aID: Integer; const aName: String; aAge: Integer);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='INSERT INTO TestPerson (ID, Name, Age) VALUES (:ID, :Name, :Age)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.Params.ParamByName('ID').AsInteger:=aID;
    Q.Params.ParamByName('Name').AsString:=aName;
    Q.Params.ParamByName('Age').AsInteger:=aAge;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestExecutorErrorDiagnostics.SetUp;
begin
  inherited SetUp;
  FConn:=TSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  CreateTestTable;
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  FPersister:=TTestPersister.Create;
  FObj:=TTestObject.Create;
  FObj.ID:=1;
  FObj.Name:='Alice';
  FObj.Age:=30;
  FExecutor:=TOPExecutor.Create(Nil);
end;

procedure TTestExecutorErrorDiagnostics.TearDown;
begin
  FreeAndNil(FExecutor);
  FreeAndNil(FObj);
  FreeAndNil(FPersister);
  FreeAndNil(FFieldMap);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestExecutorErrorDiagnostics.TestExecuteSQLNilConnectionMessage;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  try
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, 'DELETE FROM TestPerson WHERE ID=:ID', Nil);
    Fail('Expected EOPFExecutor');
  except
    on E: EOPFExecutor do
    begin
      AssertTrue('Message contains class name', Pos('TOPExecutor', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('ExecuteSQL', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('Connection is nil', E.Message) > 0);
    end;
  end;
end;

procedure TTestExecutorErrorDiagnostics.TestExecuteSQLNilObjectMessage;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  try
    FExecutor.ExecuteSQL(Nil, FPersister, FFieldMap, 'DELETE FROM TestPerson WHERE ID=:ID', FConn);
    Fail('Expected EOPFExecutor');
  except
    on E: EOPFExecutor do
    begin
      AssertTrue('Message contains class name', Pos('TOPExecutor', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('ExecuteSQL', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('Object instance is nil', E.Message) > 0);
    end;
  end;
end;

procedure TTestExecutorErrorDiagnostics.TestExecuteSQLEmptySQLMessage;
begin
  FFieldMap.AddField('ID', 'ID', [foInKey]);
  try
    FExecutor.ExecuteSQL(FObj, FPersister, FFieldMap, '', FConn);
    Fail('Expected EOPFExecutor');
  except
    on E: EOPFExecutor do
    begin
      AssertTrue('Message contains class name', Pos('TOPExecutor', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('ExecuteSQL', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('SQL statement is empty', E.Message) > 0);
    end;
  end;
end;

procedure TTestExecutorErrorDiagnostics.TestQuerySQLMissingColumnMessage;
begin
  InsertRow(1, 'Alice', 30);
  FFieldMap.AddField('ID', 'ID', [foInKey, foInSelect]);
  FFieldMap.AddField('Email', 'Email', [foInSelect]);
  try
    FExecutor.QuerySQL(FObj, FPersister, FFieldMap,
      'SELECT ID FROM TestPerson WHERE ID=1', FConn);
    Fail('Expected EOPFExecutor');
  except
    on E: EOPFExecutor do
    begin
      AssertTrue('Message contains class name', Pos('TOPExecutor', E.Message) > 0);
      AssertTrue('Message contains operation', Pos('QuerySQL', E.Message) > 0);
      AssertTrue('Message contains column name', Pos('Email', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('Result set missing column', E.Message) > 0);
    end;
  end;
end;

initialization
  RegisterTest(TTestOPExecutor);
  RegisterTest(TTestExecutorErrorDiagnostics);

end.
