unit utc.opf.schema;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, typinfo,
  fpcunit, testregistry,
  Opf.Schema, Opf.Driver, Opf.Registry, Opf.Fields, Opf.Relations, Opf.M2M, Opf.DDL,
  utc.opf.integration.base;

type

  { TSQLiteSchemaIntrospectionTest }

  TSQLiteSchemaIntrospectionTest = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FDriver: TSQLDriverSQLite;
    procedure ExecSQL(const aSQL: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIntrospectEmptyDatabase;
    procedure TestIntrospectSingleTable;
    procedure TestIntrospectPrimaryKey;
    procedure TestIntrospectCompositePK;
    procedure TestIntrospectForeignKey;
    procedure TestIntrospectIndex;
    procedure TestIntrospectUniqueIndex;
    procedure TestIntrospectMultipleTables;
    procedure TestIntrospectIgnoresInternalTables;
    procedure TestFindTable;
  end;

  { TSchemaDiffTest }

  TSchemaDiffTest = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FRelReg: TOPRelationRegistry;
    FM2MReg: TOPManyToManyRegistry;
    FDDLBuilder: TOPDDLBuilder;
    function MakeSchema(const aTables: array of UTF8String): TOPSchemaInfo;
    function MakeSchemaWithColumns(const aTableName: UTF8String;
      const aColNames, aColTypes: array of UTF8String;
      const aColNullable: array of Boolean): TOPSchemaInfo;
    function RegisterSimpleClass(const aTableName: UTF8String;
      const aFieldNames: array of UTF8String;
      const aFieldKinds: array of TTypeKind;
      const aFieldRequired: array of Boolean): TPersisterDef;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDiffEmptyBoth;
    procedure TestDiffAddTable;
    procedure TestDiffDropTable;
    procedure TestDiffAddColumn;
    procedure TestDiffDropColumn;
    procedure TestDiffAlterColumnType;
    procedure TestDiffAlterColumnNullable;
    procedure TestDiffAddIndex;
    procedure TestDiffDropIndex;
    procedure TestDiffAddForeignKey;
    procedure TestDiffDropForeignKey;
    procedure TestDiffAddJunctionTable;
    procedure TestDiffNoChanges;
    procedure TestDiffCaseInsensitive;
  end;

  { TSchemaDiffIntegrationSQLiteTest }

  TSchemaDiffIntegrationSQLiteTest = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure ExecSQL(const aSQL: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDiffIntegrationSQLite;
  end;

implementation

{ TSQLiteSchemaIntrospectionTest }

procedure TSQLiteSchemaIntrospectionTest.ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.SetUp;
begin
  FConn:=TSQLite3Connection.Create(nil);
  FTrans:=TSQLTransaction.Create(nil);
  FConn.DatabaseName:=':memory:';
  FConn.Transaction:=FTrans;
  FConn.Open;
  FDriver:=TSQLDriverSQLite.Create(dSQLite);
end;

procedure TSQLiteSchemaIntrospectionTest.TearDown;
begin
  FreeAndNil(FDriver);
  if Assigned(FTrans) then
  begin
    if FTrans.Active then
      FTrans.Rollback;
    FreeAndNil(FTrans);
  end;
  if Assigned(FConn) then
  begin
    if FConn.Connected then
      FConn.Close;
    FreeAndNil(FConn);
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectEmptyDatabase;
var
  Schema: TOPSchemaInfo;
begin
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Empty DB should have 0 tables', 0, Length(Schema.Tables));
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectSingleTable;
var
  Schema: TOPSchemaInfo;
  Idx: Integer;
begin
  ExecSQL('CREATE TABLE Person (ID INTEGER PRIMARY KEY, Name VARCHAR(100) NOT NULL, Age INTEGER)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Should find 1 table', 1, Length(Schema.Tables));
    AssertEquals('Table name', 'Person', Schema.Tables[0].TableName);
    AssertEquals('Column count', 3, Length(Schema.Tables[0].Columns));
    // Check columns
    AssertEquals('Col 0 name', 'ID', Schema.Tables[0].Columns[0].ColumnName);
    AssertEquals('Col 0 type', 'INTEGER', Schema.Tables[0].Columns[0].DataType);
    AssertEquals('Col 1 name', 'Name', Schema.Tables[0].Columns[1].ColumnName);
    AssertEquals('Col 1 type', 'VARCHAR(100)', Schema.Tables[0].Columns[1].DataType);
    AssertFalse('Col 1 not nullable', Schema.Tables[0].Columns[1].IsNullable);
    AssertEquals('Col 2 name', 'Age', Schema.Tables[0].Columns[2].ColumnName);
    AssertTrue('Col 2 nullable', Schema.Tables[0].Columns[2].IsNullable);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectPrimaryKey;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE Item (ID INTEGER PRIMARY KEY, Label TEXT)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('PK column count', 1, Length(Schema.Tables[0].PrimaryKey));
    AssertEquals('PK column', 'ID', Schema.Tables[0].PrimaryKey[0]);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectCompositePK;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE JoinTable (AID INTEGER NOT NULL, BID INTEGER NOT NULL, PRIMARY KEY(AID, BID))');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Composite PK column count', 2, Length(Schema.Tables[0].PrimaryKey));
    AssertEquals('PK col 0', 'AID', Schema.Tables[0].PrimaryKey[0]);
    AssertEquals('PK col 1', 'BID', Schema.Tables[0].PrimaryKey[1]);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectForeignKey;
var
  Schema: TOPSchemaInfo;
  ChildIdx: Integer;
begin
  ExecSQL('CREATE TABLE Parent (ID INTEGER PRIMARY KEY)');
  ExecSQL('CREATE TABLE Child (ID INTEGER PRIMARY KEY, ParentID INTEGER REFERENCES Parent(ID) ON DELETE CASCADE)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    ChildIdx:=Schema.FindTable('Child');
    AssertTrue('Child table found', ChildIdx>=0);
    AssertEquals('FK count', 1, Length(Schema.Tables[ChildIdx].ForeignKeys));
    AssertEquals('FK column', 'ParentID', Schema.Tables[ChildIdx].ForeignKeys[0].ColumnName);
    AssertEquals('FK ref table', 'Parent', Schema.Tables[ChildIdx].ForeignKeys[0].ReferencedTable);
    AssertEquals('FK ref col', 'ID', Schema.Tables[ChildIdx].ForeignKeys[0].ReferencedColumn);
    AssertEquals('FK on delete', 'CASCADE', Schema.Tables[ChildIdx].ForeignKeys[0].OnDelete);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectIndex;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE Product (ID INTEGER PRIMARY KEY, Code TEXT, Category TEXT)');
  ExecSQL('CREATE INDEX idx_product_code ON Product(Code)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Index count', 1, Length(Schema.Tables[0].Indexes));
    AssertEquals('Index name', 'idx_product_code', Schema.Tables[0].Indexes[0].IndexName);
    AssertFalse('Index not unique', Schema.Tables[0].Indexes[0].IsUnique);
    AssertEquals('Index col count', 1, Length(Schema.Tables[0].Indexes[0].Columns));
    AssertEquals('Index col', 'Code', Schema.Tables[0].Indexes[0].Columns[0]);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectUniqueIndex;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE Account (ID INTEGER PRIMARY KEY, Email TEXT UNIQUE)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Index count', 1, Length(Schema.Tables[0].Indexes));
    AssertTrue('Index is unique', Schema.Tables[0].Indexes[0].IsUnique);
    AssertEquals('Index col count', 1, Length(Schema.Tables[0].Indexes[0].Columns));
    AssertEquals('Index col', 'Email', Schema.Tables[0].Indexes[0].Columns[0]);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectMultipleTables;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE Alpha (ID INTEGER PRIMARY KEY)');
  ExecSQL('CREATE TABLE Beta (ID INTEGER PRIMARY KEY)');
  ExecSQL('CREATE TABLE Gamma (ID INTEGER PRIMARY KEY)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Table count', 3, Length(Schema.Tables));
    // Tables ordered by name
    AssertEquals('Table 0', 'Alpha', Schema.Tables[0].TableName);
    AssertEquals('Table 1', 'Beta', Schema.Tables[1].TableName);
    AssertEquals('Table 2', 'Gamma', Schema.Tables[2].TableName);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestIntrospectIgnoresInternalTables;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE UserData (ID INTEGER PRIMARY KEY)');
  ExecSQL('CREATE TABLE _opf_migrations (ID INTEGER PRIMARY KEY, Version INTEGER)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertEquals('Should only see user table', 1, Length(Schema.Tables));
    AssertEquals('Table name', 'UserData', Schema.Tables[0].TableName);
  finally
    Schema.Free;
  end;
end;

procedure TSQLiteSchemaIntrospectionTest.TestFindTable;
var
  Schema: TOPSchemaInfo;
begin
  ExecSQL('CREATE TABLE Foo (ID INTEGER PRIMARY KEY)');
  ExecSQL('CREATE TABLE Bar (ID INTEGER PRIMARY KEY)');
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    AssertTrue('FindTable Foo', Schema.FindTable('Foo')>=0);
    AssertTrue('FindTable Bar', Schema.FindTable('Bar')>=0);
    AssertEquals('FindTable case-insensitive', Schema.FindTable('foo'), Schema.FindTable('FOO'));
    AssertEquals('FindTable not found', -1, Schema.FindTable('Baz'));
    AssertTrue('TableExists Foo', Schema.TableExists('Foo'));
    AssertFalse('TableExists Baz', Schema.TableExists('Baz'));
  finally
    Schema.Free;
  end;
end;

{ TDialectSchemaIntrospectionTest }

type

  { TDialectSchemaIntrospectionTest }

  TDialectSchemaIntrospectionTest = class(TTestIntegration)
  private
    FDriver: TSQLDriver;
    FHelperClass: TDatabaseSetupHelperClass;
  protected
    procedure SetUpDialect(aHelperClass: TDatabaseSetupHelperClass);
    procedure TearDownDialect;
  published
    procedure TestIntrospectEmptyOrExisting;
  end;

  { TPgSQLSchemaIntrospectionTest }

  TPgSQLSchemaIntrospectionTest = class(TDialectSchemaIntrospectionTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TMySQLSchemaIntrospectionTest }

  TMySQLSchemaIntrospectionTest = class(TDialectSchemaIntrospectionTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TFirebirdSchemaIntrospectionTest }

  TFirebirdSchemaIntrospectionTest = class(TDialectSchemaIntrospectionTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

{ TDialectSchemaIntrospectionTest }

procedure TDialectSchemaIntrospectionTest.SetUpDialect(aHelperClass: TDatabaseSetupHelperClass);
begin
  FHelperClass:=aHelperClass;
  SetUpDatabase(aHelperClass);
  FDriver:=TSQLDriver.CreateDriver(aHelperClass.GetDialect);
end;

procedure TDialectSchemaIntrospectionTest.TearDownDialect;
begin
  FreeAndNil(FDriver);
  TearDownDatabase;
end;

procedure TDialectSchemaIntrospectionTest.TestIntrospectEmptyOrExisting;
var
  Schema: TOPSchemaInfo;
begin
  Schema:=FDriver.IntrospectSchema(FConn);
  try
    // Basic sanity: schema object created, tables array is valid
    AssertTrue('Schema should not be nil', Schema<>nil);
    AssertTrue('Tables count should be >= 0', Length(Schema.Tables)>=0);
    // If tables exist, verify basic structure
    if Length(Schema.Tables)>0 then
    begin
      AssertTrue('First table should have a name', Schema.Tables[0].TableName<>'');
      AssertTrue('First table should have columns', Length(Schema.Tables[0].Columns)>0);
    end;
  finally
    Schema.Free;
  end;
end;

{ TPgSQLSchemaIntrospectionTest }

procedure TPgSQLSchemaIntrospectionTest.SetUp;
begin
  SetUpDialect(TPgSQLSetupHelper);
end;

procedure TPgSQLSchemaIntrospectionTest.TearDown;
begin
  TearDownDialect;
end;

{ TMySQLSchemaIntrospectionTest }

procedure TMySQLSchemaIntrospectionTest.SetUp;
begin
  SetUpDialect(TMySQLSetupHelper);
end;

procedure TMySQLSchemaIntrospectionTest.TearDown;
begin
  TearDownDialect;
end;

{ TFirebirdSchemaIntrospectionTest }

procedure TFirebirdSchemaIntrospectionTest.SetUp;
begin
  SetUpDialect(TFirebirdSetupHelper);
end;

procedure TFirebirdSchemaIntrospectionTest.TearDown;
begin
  TearDownDialect;
end;

{ Standalone helper }

function FindDiff(const aDiffs: TOPSchemaDiffArray; aKind: TSchemaDiffKind;
  const aTable: UTF8String; const aColumn: UTF8String = ''): Integer;
var
  I: Integer;
begin
  for I:=0 to High(aDiffs) do
    if (aDiffs[I].Kind=aKind) and SameText(aDiffs[I].TableName, aTable) then
      if (aColumn='') or SameText(aDiffs[I].ColumnName, aColumn) then
        Exit(I);
  Result:=-1;
end;

{ TSchemaDiffTest - Helper methods }

function TSchemaDiffTest.MakeSchema(const aTables: array of UTF8String): TOPSchemaInfo;
var
  I: Integer;
begin
  Result:=TOPSchemaInfo.Create;
  SetLength(Result.Tables, Length(aTables));
  for I:=0 to High(aTables) do
    Result.Tables[I].TableName:=aTables[I];
end;

function TSchemaDiffTest.MakeSchemaWithColumns(const aTableName: UTF8String;
  const aColNames, aColTypes: array of UTF8String;
  const aColNullable: array of Boolean): TOPSchemaInfo;
var
  I: Integer;
begin
  Result:=TOPSchemaInfo.Create;
  SetLength(Result.Tables, 1);
  Result.Tables[0].TableName:=aTableName;
  SetLength(Result.Tables[0].Columns, Length(aColNames));
  for I:=0 to High(aColNames) do
  begin
    Result.Tables[0].Columns[I].ColumnName:=aColNames[I];
    Result.Tables[0].Columns[I].DataType:=aColTypes[I];
    Result.Tables[0].Columns[I].IsNullable:=aColNullable[I];
  end;
end;

function TSchemaDiffTest.RegisterSimpleClass(const aTableName: UTF8String;
  const aFieldNames: array of UTF8String;
  const aFieldKinds: array of TTypeKind;
  const aFieldRequired: array of Boolean): TPersisterDef;
var
  I: Integer;
  FM: TOPFieldMap;
begin
  Result:=FRegistry.RegisterPersister(TObject, nil);
  Result.TableName:=aTableName;
  Result.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  for I:=0 to High(aFieldNames) do
  begin
    FM:=TOPFieldMap(Result.FieldMap.Add);
    FM.FieldName:=aFieldNames[I];
    FM.PropertyName:=aFieldNames[I];
    FM.PropertyKind:=aFieldKinds[I];
    if aFieldRequired[I] then
      FM.Options:=[foRequired]
    else
      FM.Options:=[];
  end;
end;

procedure TSchemaDiffTest.SetUp;
begin
  FRegistry:=TOPRegistry.Create(nil);
  FRelReg:=TOPRelationRegistry.Create(nil);
  FRelReg.Registry:=FRegistry;
  FM2MReg:=TOPManyToManyRegistry.Create(nil);
  FM2MReg.Registry:=FRegistry;
  FDDLBuilder:=TOPDDLBuilder.Create(nil);
  FDDLBuilder.SQLDialect:=dSQLite;
  FDDLBuilder.Registry:=FRegistry;
end;

procedure TSchemaDiffTest.TearDown;
begin
  FreeAndNil(FDDLBuilder);
  FreeAndNil(FM2MReg);
  FreeAndNil(FRelReg);
  FreeAndNil(FRegistry);
end;

{ TSchemaDiffTest - Tests }

procedure TSchemaDiffTest.TestDiffEmptyBoth;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  Schema:=TOPSchemaInfo.Create;
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertEquals('No diffs for empty both', 0, Length(Diffs));
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAddTable;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  RegisterSimpleClass('Person', ['Id', 'Name'],
    [tkInteger, tkAString], [True, False]);
  Schema:=TOPSchemaInfo.Create; // empty actual
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkAddTable', FindDiff(Diffs, sdkAddTable, 'Person')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffDropTable;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  // No classes registered, but actual has a table
  Schema:=MakeSchema(['OldTable']);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkDropTable', FindDiff(Diffs, sdkDropTable, 'OldTable')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAddColumn;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  // Register class with 2 fields, actual table has only 1 column
  RegisterSimpleClass('Person', ['Id', 'Name'],
    [tkInteger, tkAString], [True, False]);
  Schema:=MakeSchemaWithColumns('Person', ['Id'], ['INTEGER'], [False]);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkAddColumn for Name', FindDiff(Diffs, sdkAddColumn, 'Person', 'Name')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffDropColumn;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  // Register class with 1 field, actual table has 2 columns
  RegisterSimpleClass('Person', ['Id'],
    [tkInteger], [True]);
  Schema:=MakeSchemaWithColumns('Person', ['Id', 'OldCol'], ['INTEGER', 'TEXT'], [False, True]);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkDropColumn for OldCol', FindDiff(Diffs, sdkDropColumn, 'Person', 'OldCol')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAlterColumnType;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
  DiffIdx: Integer;
begin
  // Register field as INTEGER, actual is TEXT
  RegisterSimpleClass('Person', ['Id'],
    [tkInteger], [False]);
  Schema:=MakeSchemaWithColumns('Person', ['Id'], ['TEXT'], [True]);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    DiffIdx:=FindDiff(Diffs, sdkAlterColumnType, 'Person', 'Id');
    AssertTrue('Should have sdkAlterColumnType', DiffIdx>=0);
    AssertTrue('Detail contains expected', Pos('expected=', Diffs[DiffIdx].Detail)>0);
    AssertTrue('Detail contains actual=TEXT', Pos('actual=TEXT', Diffs[DiffIdx].Detail)>0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAlterColumnNullable;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
  DiffIdx: Integer;
begin
  // Register field as required (NOT NULL), actual is nullable
  RegisterSimpleClass('Person', ['Id'],
    [tkInteger], [True]);
  Schema:=MakeSchemaWithColumns('Person', ['Id'], ['INTEGER'], [True]); // IsNullable=True
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    DiffIdx:=FindDiff(Diffs, sdkAlterColumnNullable, 'Person', 'Id');
    AssertTrue('Should have sdkAlterColumnNullable', DiffIdx>=0);
    AssertEquals('Detail should be NOT NULL', 'NOT NULL', Diffs[DiffIdx].Detail);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAddIndex;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
  PD: TPersisterDef;
  Idxs: TOPIndexDefArray;
begin
  PD:=RegisterSimpleClass('Person', ['Id', 'Name'],
    [tkInteger, tkAString], [True, False]);
  SetLength(Idxs, 1);
  Idxs[0].IndexName:='idx_person_name';
  Idxs[0].FieldNames:='Name';
  PD.Indexes:=Idxs;
  // Actual table has no indexes
  Schema:=MakeSchemaWithColumns('Person', ['Id', 'Name'], ['INTEGER', 'VARCHAR(255)'], [False, True]);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkAddIndex', FindDiff(Diffs, sdkAddIndex, 'Person')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffDropIndex;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  RegisterSimpleClass('Person', ['Id'],
    [tkInteger], [True]);
  // Actual table has an index that is not in desired
  Schema:=MakeSchemaWithColumns('Person', ['Id'], ['INTEGER'], [False]);
  SetLength(Schema.Tables[0].Indexes, 1);
  Schema.Tables[0].Indexes[0].IndexName:='idx_old_index';
  SetLength(Schema.Tables[0].Indexes[0].Columns, 1);
  Schema.Tables[0].Indexes[0].Columns[0]:='Id';
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkDropIndex', FindDiff(Diffs, sdkDropIndex, 'Person')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAddForeignKey;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
  ParentPD, ChildPD: TPersisterDef;
begin
  ParentPD:=RegisterSimpleClass('Parent', ['Id'],
    [tkInteger], [True]);
  ChildPD:=FRegistry.RegisterPersister(TComponent, nil);
  ChildPD.TableName:='Child';
  ChildPD.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  with TOPFieldMap(ChildPD.FieldMap.Add) do begin FieldName:='Id'; PropertyName:='Id'; PropertyKind:=tkInteger; Options:=[foRequired]; end;
  with TOPFieldMap(ChildPD.FieldMap.Add) do begin FieldName:='ParentId'; PropertyName:='ParentId'; PropertyKind:=tkInteger; end;
  FRelReg.RegisterRelationship(TObject, TComponent, 'ParentId', False, '');

  // Actual: both tables exist but no FK
  Schema:=TOPSchemaInfo.Create;
  SetLength(Schema.Tables, 2);
  Schema.Tables[0].TableName:='Parent';
  SetLength(Schema.Tables[0].Columns, 1);
  Schema.Tables[0].Columns[0].ColumnName:='Id';
  Schema.Tables[0].Columns[0].DataType:='INTEGER';
  Schema.Tables[1].TableName:='Child';
  SetLength(Schema.Tables[1].Columns, 2);
  Schema.Tables[1].Columns[0].ColumnName:='Id';
  Schema.Tables[1].Columns[0].DataType:='INTEGER';
  Schema.Tables[1].Columns[1].ColumnName:='ParentId';
  Schema.Tables[1].Columns[1].DataType:='INTEGER';
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkAddForeignKey', FindDiff(Diffs, sdkAddForeignKey, 'Child', 'ParentId')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffDropForeignKey;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  RegisterSimpleClass('Person', ['Id'],
    [tkInteger], [True]);
  // Actual has a FK that is not in desired relations
  Schema:=MakeSchemaWithColumns('Person', ['Id'], ['INTEGER'], [False]);
  SetLength(Schema.Tables[0].ForeignKeys, 1);
  Schema.Tables[0].ForeignKeys[0].ConstraintName:='fk_old';
  Schema.Tables[0].ForeignKeys[0].ColumnName:='OldRef';
  Schema.Tables[0].ForeignKeys[0].ReferencedTable:='Other';
  Schema.Tables[0].ForeignKeys[0].ReferencedColumn:='Id';
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkDropForeignKey', FindDiff(Diffs, sdkDropForeignKey, 'Person')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffAddJunctionTable;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
  PDB: TPersisterDef;
begin
  // Register classes for M2M
  RegisterSimpleClass('ClassA', ['Id'], [tkInteger], [True]);
  PDB:=FRegistry.RegisterPersister(TComponent, nil);
  PDB.TableName:='ClassB';
  PDB.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  with TOPFieldMap(PDB.FieldMap.Add) do
  begin
    FieldName:='Id'; PropertyName:='Id'; PropertyKind:=tkInteger; Options:=[foRequired];
  end;
  FM2MReg.RegisterManyToMany(TObject, TComponent, 'AB_Junction', 'ClassAId', 'ClassBId');

  // Actual: no junction table
  Schema:=TOPSchemaInfo.Create;
  SetLength(Schema.Tables, 2);
  Schema.Tables[0].TableName:='ClassA';
  SetLength(Schema.Tables[0].Columns, 1);
  Schema.Tables[0].Columns[0].ColumnName:='Id';
  Schema.Tables[0].Columns[0].DataType:='INTEGER';
  Schema.Tables[1].TableName:='ClassB';
  SetLength(Schema.Tables[1].Columns, 1);
  Schema.Tables[1].Columns[0].ColumnName:='Id';
  Schema.Tables[1].Columns[0].DataType:='INTEGER';
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertTrue('Should have sdkAddJunctionTable', FindDiff(Diffs, sdkAddJunctionTable, 'AB_Junction')>=0);
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffNoChanges;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  // Register class with 1 INT field (not required)
  RegisterSimpleClass('Person', ['Id'],
    [tkInteger], [False]);
  // Actual matches: INTEGER (normalized to match DDL builder's INT), nullable
  Schema:=MakeSchemaWithColumns('Person', ['Id'], ['INTEGER'], [True]);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    AssertEquals('No diffs when schema matches', 0, Length(Diffs));
  finally
    Schema.Free;
  end;
end;

procedure TSchemaDiffTest.TestDiffCaseInsensitive;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
begin
  // Register with lowercase, actual with uppercase — should match (no diff)
  RegisterSimpleClass('person', ['id'],
    [tkInteger], [False]);
  Schema:=MakeSchemaWithColumns('PERSON', ['ID'], ['INTEGER'], [True]);
  try
    Diffs:=TOPSchemaDiff.Compare(FRegistry, FRelReg, FM2MReg, Schema, FDDLBuilder);
    // Should not report add/drop table or column
    AssertEquals('FindDiff sdkAddTable', -1, FindDiff(Diffs, sdkAddTable, 'person'));
    AssertEquals('FindDiff sdkDropTable', -1, FindDiff(Diffs, sdkDropTable, 'PERSON'));
    AssertEquals('FindDiff sdkAddColumn', -1, FindDiff(Diffs, sdkAddColumn, 'person', 'id'));
    AssertEquals('FindDiff sdkDropColumn', -1, FindDiff(Diffs, sdkDropColumn, 'PERSON', 'ID'));
  finally
    Schema.Free;
  end;
end;

{ TSchemaDiffIntegrationSQLiteTest }

procedure TSchemaDiffIntegrationSQLiteTest.ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TSchemaDiffIntegrationSQLiteTest.SetUp;
begin
  FConn:=TSQLite3Connection.Create(nil);
  FTrans:=TSQLTransaction.Create(nil);
  FConn.DatabaseName:=':memory:';
  FConn.Transaction:=FTrans;
  FConn.Open;
end;

procedure TSchemaDiffIntegrationSQLiteTest.TearDown;
begin
  if Assigned(FTrans) then
  begin
    if FTrans.Active then
      FTrans.Rollback;
    FreeAndNil(FTrans);
  end;
  if Assigned(FConn) then
  begin
    if FConn.Connected then
      FConn.Close;
    FreeAndNil(FConn);
  end;
end;

procedure TSchemaDiffIntegrationSQLiteTest.TestDiffIntegrationSQLite;
var
  Schema: TOPSchemaInfo;
  Diffs: TOPSchemaDiffArray;
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
  DDL: TOPDDLBuilder;
  Driver: TSQLDriverSQLite;
  PD: TPersisterDef;
begin
  // Create actual DB schema: Person with Id and Name
  ExecSQL('CREATE TABLE Person (Id INTEGER PRIMARY KEY, Name VARCHAR(100) NOT NULL)');

  // Register desired model: Person with Id, Name, and Age (extra column)
  // Name is no longer required (nullability change)
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  RelReg.Registry:=Reg;
  M2MReg:=TOPManyToManyRegistry.Create(nil);
  M2MReg.Registry:=Reg;
  DDL:=TOPDDLBuilder.Create(nil);
  DDL.SQLDialect:=dSQLite;
  DDL.Registry:=Reg;
  Driver:=TSQLDriverSQLite.Create(dSQLite);
  try
    PD:=Reg.RegisterPersister(TObject, nil);
    PD.TableName:='Person';
    PD.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
    with TOPFieldMap(PD.FieldMap.Add) do begin FieldName:='Id'; PropertyName:='Id'; PropertyKind:=tkInteger; Options:=[foRequired]; end;
    with TOPFieldMap(PD.FieldMap.Add) do begin FieldName:='Name'; PropertyName:='Name'; PropertyKind:=tkAString; MaxSize:=100; end;
    with TOPFieldMap(PD.FieldMap.Add) do begin FieldName:='Age'; PropertyName:='Age'; PropertyKind:=tkInteger; end;

    // Introspect actual
    Schema:=Driver.IntrospectSchema(FConn);
    try
      // Compare
      Diffs:=TOPSchemaDiff.Compare(Reg, RelReg, M2MReg, Schema, DDL);

      // Verify: Age should be sdkAddColumn
      AssertTrue('Should detect sdkAddColumn for Age',
        FindDiff(Diffs, sdkAddColumn, 'Person', 'Age')>=0);

      // Verify: Name nullability changed (was NOT NULL, now nullable)
      AssertTrue('Should detect sdkAlterColumnNullable for Name',
        FindDiff(Diffs, sdkAlterColumnNullable, 'Person', 'Name')>=0);

      // Should NOT report add/drop table
      AssertEquals('No sdkAddTable', -1, FindDiff(Diffs, sdkAddTable, 'Person'));
      AssertEquals('No sdkDropTable', -1, FindDiff(Diffs, sdkDropTable, 'Person'));
    finally
      Schema.Free;
    end;
  finally
    Driver.Free;
    DDL.Free;
    M2MReg.Free;
    RelReg.Free;
    Reg.Free;
  end;
end;

initialization
  RegisterTest(TSQLiteSchemaIntrospectionTest);
  RegisterTest(TPgSQLSchemaIntrospectionTest);
  RegisterTest(TMySQLSchemaIntrospectionTest);
  RegisterTest(TFirebirdSchemaIntrospectionTest);
  RegisterTest(TSchemaDiffTest);
  RegisterTest(TSchemaDiffIntegrationSQLiteTest);

end.
