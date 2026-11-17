unit utc.opf.schemacontracts;

{ Tests for DDL, schema comparison and migration contracts:
  - generated constraint names shortened to the dialect's identifier length,
    user supplied names that are too long raise
  - DEFAULT literals for strings (MySQL backslash) and dates
  - creation order with relation cycles, CreateSchema statement order
  - new tables get their indexes and foreign keys, diff order
  - primary key index detection, narrowing type changes
  - migrations skip changes the database cannot make
  - atomic schema changes on SQLite (transactional DDL) }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Variants, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, Variants, DB, sqldb, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Driver, Opf.Sql, Opf.DDL, Opf.Schema,
  Opf.Registry, Opf.Relations, Opf.M2M, Opf.Migration, Opf.Persister.RTTI,
  utc.opf.failconn;

type

  { Fixtures }

  TSCParent = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TSCChild = class(TPersistent)
  private
    FID: Integer;
    FLabel: String;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Label_: String read FLabel write FLabel;
  end;

  TSCOther = class(TPersistent)
  private
    FID: Integer;
    FCode: String;
  published
    property ID: Integer read FID write FID;
    property Code: String read FCode write FCode;
  end;

  { TTestDDLContracts }

  TTestDDLContracts = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    function Builder(aDialect: TSQLDialect): TOPDDLBuilder;
    function MakeField(const aName: String; aKind: TTypeKind; aTypeInfo: PTypeInfo): TOPFieldMap;
    function FieldDef(aDialect: TSQLDialect; aField: TOPFieldMap): UTF8String;
    procedure RegisterParentChild;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGeneratedPKNameShortened;
    procedure TestGeneratedFKNameShortened;
    procedure TestShortNamesUnchanged;
    procedure TestLongUserIndexNameRaises;
    procedure TestLongUserConstraintNameRaises;
    procedure TestLongUserPKNameRaises;
    procedure TestDefaultStringQuoted;
    procedure TestDefaultStringMySQLBackslash;
    procedure TestDefaultDateTimeLiteral;
    procedure TestDefaultDateFromFloat;
    procedure TestDefaultTimeLiteral;
    procedure TestDefaultDateFloatStorage;
    procedure TestCreationOrderCycleRaises;
    procedure TestCreationOrderCycleAllowed;
    procedure TestCreateSchemaSQLOrder;
    procedure TestCreateSchemaSQLMutualRelations;
    procedure TestCreateSchemaSQLPKConstraintName;
  end;

  { TTestSchemaDiffContracts }

  TTestSchemaDiffContracts = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FBuilder: TOPDDLBuilder;
    FSchema: TOPSchemaInfo;
    function Compare: TOPSchemaDiffArray;
    function AddTable(const aName: String; const aColumns: array of String): Integer;
    procedure AddIndex(aTable: Integer; const aName: String; const aColumns: array of String);
    function Column(const aType: String; aLength: Integer = 0; aPrecision: Integer = 0; aScale: Integer = 0): TOPColumnInfo;
    function CountKind(const aDiffs: TOPSchemaDiffArray; aKind: TSchemaDiffKind; const aDetail: String = ''): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNewTableGetsIndexDiff;
    procedure TestNewTableGetsForeignKeyDiff;
    procedure TestDiffOrderTablesFirst;
    procedure TestSortDiffsStable;
    procedure TestPKIndexByColumnsKept;
    procedure TestPKIndexByNameKept;
    procedure TestIndexWithPKInNameDropped;
    procedure TestNarrowingVarchar;
    procedure TestWideningVarchar;
    procedure TestNarrowingToFixedFromText;
    procedure TestNarrowingDecimal;
    procedure TestWideningInteger;
    procedure TestNarrowingInteger;
    procedure TestNarrowingOtherFamily;
    procedure TestNarrowingFlagOnDiff;
  end;

  { TTestMigrationContracts }

  TTestMigrationContracts = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FMigrator: TOPSchemaMigrator;
    procedure Exec(const aSQL: String);
    function TableExists(const aName: String): Boolean;
    function CurrentVersion: Integer;
    procedure RegisterParent;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTypeChangeSkippedOnSQLite;
    procedure TestTypeChangeDryRunDoesNotRaise;
    procedure TestFailedMigrationLeavesNothing;
    procedure TestRerunAfterFailedMigration;
    procedure TestFailedCreateSchemaLeavesNothing;
    procedure TestCreateSchemaRecordsVersion;
  end;

implementation

{ TTestDDLContracts }

procedure TTestDDLContracts.SetUp;
begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
end;

procedure TTestDDLContracts.TearDown;
begin
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

function TTestDDLContracts.Builder(aDialect: TSQLDialect): TOPDDLBuilder;
begin
  Result:=TOPDDLBuilder.Create(Nil);
  Result.SQLDialect:=aDialect;
  Result.Registry:=FRegistry;
end;

function TTestDDLContracts.MakeField(const aName: String; aKind: TTypeKind; aTypeInfo: PTypeInfo): TOPFieldMap;
var
  L: TOPFieldMapList;
begin
  // Owned by a list that the registry frees: keep it simple, free in the test
  L:=TOPFieldMapList.Create(TOPFieldMap);
  Result:=L.Add as TOPFieldMap;
  Result.FieldName:=aName;
  Result.PropertyName:=aName;
  Result.PropertyKind:=aKind;
  Result.PropertyTypeInfo:=aTypeInfo;
end;

function TTestDDLContracts.FieldDef(aDialect: TSQLDialect; aField: TOPFieldMap): UTF8String;
var
  B: TOPDDLBuilder;
begin
  B:=Builder(aDialect);
  try
    Result:=B.GetPropertyDefSQL('T', aField);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.RegisterParentChild;
var
  D: TPersisterDef;
begin
  D:=FRegistry.RegisterPersister(TSCParent, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  D:=FRegistry.RegisterPersister(TSCChild, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  FRelations.RegisterRelationship(TSCParent, TSCChild, 'ParentID');
end;

procedure TTestDDLContracts.TestGeneratedPKNameShortened;
var
  B: TOPDDLBuilder;
  D: TPersisterDef;
  Long, SQL: UTF8String;
begin
  D:=FRegistry.RegisterPersister(TSCParent, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  Long:='Customer_Order_Line_Archive_2026';
  B:=Builder(dFirebird3);
  try
    SQL:=B.GetCreateSQL(Long, D.FieldMap.AsArray);
    AssertTrue('Full name not used: '+SQL, Pos(Long+'_PK', SQL)=0);
    AssertTrue('Shortened name used: '+SQL, Pos(OPFShortenIdentifier(Long+'_PK', 31), SQL)>0);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestGeneratedFKNameShortened;
var
  B: TOPDDLBuilder;
  SQL, Expected: UTF8String;
begin
  B:=Builder(dOracle);
  try
    SQL:=B.GetAddForeignKeySQL('Customer_Order_Line', 'ParentID', 'Customer_Order', 'ID', False);
    Expected:=OPFShortenIdentifier('fk_Customer_Order_Line_Customer_Order', 30);
    AssertEquals('Length', 30, Length(Expected));
    AssertTrue('Shortened name used: '+SQL, Pos(' '+Expected+' ', SQL)>0);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestShortNamesUnchanged;
var
  B: TOPDDLBuilder;
begin
  B:=Builder(dOracle);
  try
    AssertEquals('ALTER TABLE Line ADD CONSTRAINT fk_Line_Head FOREIGN KEY (HeadID) REFERENCES Head(ID)',
      B.GetAddForeignKeySQL('Line', 'HeadID', 'Head', 'ID', False));
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestLongUserIndexNameRaises;
var
  B: TOPDDLBuilder;
  Idx: TOPIndexDef;
  Raised: Boolean;
begin
  Idx.IndexName:='idx_customer_order_line_archive_by_date';
  Idx.FieldNames:='Name';
  B:=Builder(dFirebird3);
  try
    Raised:=False;
    try
      B.GetCreateIndexSQL('T', Idx);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Firebird: too long', Raised);
  finally
    B.Free;
  end;
  B:=Builder(dPostGres);
  try
    AssertTrue('PostgreSQL: fits', Pos(Idx.IndexName, B.GetCreateIndexSQL('T', Idx))>0);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestLongUserConstraintNameRaises;
var
  B: TOPDDLBuilder;
  Raised: Boolean;
begin
  B:=Builder(dOracle);
  try
    Raised:=False;
    try
      B.GetAddForeignKeySQL('Line', 'HeadID', 'Head', 'ID', False, 'fk_line_head_with_a_long_explicit_name');
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue(Raised);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestLongUserPKNameRaises;
var
  B: TOPDDLBuilder;
  D: TPersisterDef;
  Raised: Boolean;
begin
  D:=FRegistry.RegisterPersister(TSCParent, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  B:=Builder(dFirebird);
  try
    Raised:=False;
    try
      B.GetCreateSQL('T', D.FieldMap.AsArray, 'pk_a_primary_key_name_that_is_too_long');
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue(Raised);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestDefaultStringQuoted;
var
  F: TOPFieldMap;
begin
  F:=MakeField('S', tkAString, Nil);
  try
    F.MaxSize:=20;
    F.DefaultValue:='a\b''c';
    AssertEquals('VARCHAR(20) DEFAULT ''a\b''''c''', FieldDef(dPostGres, F));
  finally
    F.Collection.Free;
  end;
end;

procedure TTestDDLContracts.TestDefaultStringMySQLBackslash;
var
  F: TOPFieldMap;
begin
  F:=MakeField('S', tkAString, Nil);
  try
    F.MaxSize:=20;
    F.DefaultValue:='a\b''c';
    AssertEquals('VARCHAR(20) DEFAULT ''a\\b''''c''', FieldDef(dMySQL, F));
  finally
    F.Collection.Free;
  end;
end;

procedure TTestDDLContracts.TestDefaultDateTimeLiteral;
var
  F: TOPFieldMap;
  D: TDateTime;
begin
  F:=MakeField('At', tkFloat, TypeInfo(TDateTime));
  try
    D:=EncodeDate(2024, 2, 29)+EncodeTime(10, 30, 0, 0);
    F.DefaultValue:=VarFromDateTime(D);
    AssertTrue(FieldDef(dPostGres, F), Pos('DEFAULT ''2024-02-29 10:30:00.000''', FieldDef(dPostGres, F))>0);
  finally
    F.Collection.Free;
  end;
end;

procedure TTestDDLContracts.TestDefaultDateFromFloat;
var
  F: TOPFieldMap;
begin
  F:=MakeField('Day', tkFloat, TypeInfo(TDate));
  try
    // Defaults from attributes arrive as a float
    F.DefaultValue:=Double(EncodeDate(2024, 2, 29));
    AssertTrue(FieldDef(dFirebird3, F), Pos('DEFAULT ''2024-02-29''', FieldDef(dFirebird3, F))>0);
  finally
    F.Collection.Free;
  end;
end;

procedure TTestDDLContracts.TestDefaultTimeLiteral;
var
  F: TOPFieldMap;
begin
  F:=MakeField('At', tkFloat, TypeInfo(TTime));
  try
    F.DefaultValue:=Double(EncodeTime(8, 15, 0, 0));
    AssertTrue(FieldDef(dPostGres, F), Pos('DEFAULT ''08:15:00.000''', FieldDef(dPostGres, F))>0);
  finally
    F.Collection.Free;
  end;
end;

procedure TTestDDLContracts.TestDefaultDateFloatStorage;
var
  F: TOPFieldMap;
  Saved: TOPDateTimeStorage;
  S: UTF8String;
begin
  Saved:=OPFDateTimeStorage;
  F:=MakeField('Day', tkFloat, TypeInfo(TDate));
  try
    OPFDateTimeStorage:=dtsFloat;
    F.DefaultValue:=Double(45351);
    S:=FieldDef(dPostGres, F);
    AssertTrue('Float default: '+S, Pos('DEFAULT 45351', S)>0);
  finally
    OPFDateTimeStorage:=Saved;
    F.Collection.Free;
  end;
end;

procedure TTestDDLContracts.TestCreationOrderCycleRaises;
var
  B: TOPDDLBuilder;
  Raised: Boolean;
begin
  RegisterParentChild;
  FRelations.RegisterRelationship(TSCChild, TSCParent, 'ID');
  B:=Builder(dPostGres);
  try
    Raised:=False;
    try
      B.GetCreationOrder(FRegistry, FRelations);
    except
      on E: EOPFDDL do
        Raised:=True;
    end;
    AssertTrue(Raised);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestCreationOrderCycleAllowed;
var
  B: TOPDDLBuilder;
  Order: TClassArray;
begin
  RegisterParentChild;
  FRelations.RegisterRelationship(TSCChild, TSCParent, 'ID');
  B:=Builder(dPostGres);
  try
    Order:=B.GetCreationOrder(FRegistry, FRelations, True);
    AssertEquals('All classes', 2, Length(Order));
    AssertTrue('Registration order', Order[0]=TSCParent);
    AssertTrue('Registration order', Order[1]=TSCChild);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestCreateSchemaSQLOrder;
var
  B: TOPDDLBuilder;
  SQL: TUTF8StringArray;
  I, LastCreate, FirstAlter: Integer;
begin
  RegisterParentChild;
  B:=Builder(dPostGres);
  try
    SQL:=B.GetCreateSchemaSQL(FRegistry, FRelations, FM2M, [], [scoForeignKeys, scoIndexes]);
    LastCreate:=-1;
    FirstAlter:=-1;
    for I:=0 to High(SQL) do
      if Pos('CREATE TABLE', SQL[I])=1 then
        LastCreate:=I
      else if (Pos('ALTER TABLE', SQL[I])=1) and (FirstAlter<0) then
        FirstAlter:=I;
    AssertEquals('Statements', 3, Length(SQL));
    AssertTrue('Foreign key after the tables', FirstAlter>LastCreate);
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestCreateSchemaSQLMutualRelations;
var
  B: TOPDDLBuilder;
  SQL: TUTF8StringArray;
begin
  RegisterParentChild;
  FRelations.RegisterRelationship(TSCChild, TSCParent, 'ID');
  B:=Builder(dPostGres);
  try
    SQL:=B.GetCreateSchemaSQL(FRegistry, FRelations, FM2M, [], [scoForeignKeys]);
    AssertEquals('2 tables and 2 foreign keys', 4, Length(SQL));
  finally
    B.Free;
  end;
end;

procedure TTestDDLContracts.TestCreateSchemaSQLPKConstraintName;
var
  B: TOPDDLBuilder;
  SQL: TUTF8StringArray;
begin
  RegisterParentChild;
  FRegistry.FindPersister(TSCParent).PKConstraintName:='pk_parent_custom';
  B:=Builder(dPostGres);
  try
    SQL:=B.GetCreateSchemaSQL(FRegistry, FRelations, FM2M, [], []);
    AssertTrue(SQL[0], Pos('CONSTRAINT pk_parent_custom', SQL[0])>0);
  finally
    B.Free;
  end;
end;

{ TTestSchemaDiffContracts }

procedure TTestSchemaDiffContracts.SetUp;
var
  D: TPersisterDef;
  Idx: TOPIndexDef;
begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  D:=FRegistry.RegisterPersister(TSCParent, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  D.FieldMap.FindByPropertyName('Name').MaxSize:=50;
  D:=FRegistry.RegisterPersister(TSCChild, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  Idx.IndexName:='idx_child_label';
  Idx.FieldNames:='Label_';
  D.Indexes:=[Idx];
  FRelations.RegisterRelationship(TSCParent, TSCChild, 'ParentID');
  FBuilder:=TOPDDLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dPostGres;
  FBuilder.Registry:=FRegistry;
  FSchema:=TOPSchemaInfo.Create;
end;

procedure TTestSchemaDiffContracts.TearDown;
begin
  FreeAndNil(FSchema);
  FreeAndNil(FBuilder);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

function TTestSchemaDiffContracts.Compare: TOPSchemaDiffArray;
begin
  Result:=TOPSchemaDiff.Compare(FRegistry, FRelations, FM2M, FSchema, FBuilder);
end;

function TTestSchemaDiffContracts.Column(const aType: String; aLength, aPrecision, aScale: Integer): TOPColumnInfo;
begin
  Result:=Default(TOPColumnInfo);
  Result.DataType:=aType;
  Result.MaxLength:=aLength;
  Result.NumericPrecision:=aPrecision;
  Result.NumericScale:=aScale;
  Result.IsNullable:=True;
end;

function TTestSchemaDiffContracts.AddTable(const aName: String; const aColumns: array of String): Integer;
var
  I: Integer;
begin
  Result:=Length(FSchema.Tables);
  SetLength(FSchema.Tables, Result+1);
  FSchema.Tables[Result].TableName:=aName;
  SetLength(FSchema.Tables[Result].Columns, Length(aColumns));
  for I:=0 to High(aColumns) do
  begin
    FSchema.Tables[Result].Columns[I]:=Column('INTEGER');
    FSchema.Tables[Result].Columns[I].ColumnName:=aColumns[I];
  end;
  FSchema.Tables[Result].PrimaryKey:=['ID'];
end;

procedure TTestSchemaDiffContracts.AddIndex(aTable: Integer; const aName: String; const aColumns: array of String);
var
  N, I: Integer;
begin
  N:=Length(FSchema.Tables[aTable].Indexes);
  SetLength(FSchema.Tables[aTable].Indexes, N+1);
  FSchema.Tables[aTable].Indexes[N].IndexName:=aName;
  SetLength(FSchema.Tables[aTable].Indexes[N].Columns, Length(aColumns));
  for I:=0 to High(aColumns) do
    FSchema.Tables[aTable].Indexes[N].Columns[I]:=aColumns[I];
end;

function TTestSchemaDiffContracts.CountKind(const aDiffs: TOPSchemaDiffArray; aKind: TSchemaDiffKind; const aDetail: String): Integer;
var
  I: Integer;
begin
  Result:=0;
  for I:=0 to High(aDiffs) do
    if (aDiffs[I].Kind=aKind) and ((aDetail='') or SameText(aDiffs[I].Detail, aDetail)) then
      Inc(Result);
end;

procedure TTestSchemaDiffContracts.TestNewTableGetsIndexDiff;
var
  D: TOPSchemaDiffArray;
begin
  // Nothing exists yet
  D:=Compare;
  AssertEquals('Tables', 2, CountKind(D, sdkAddTable));
  AssertEquals('Index of the new table', 1, CountKind(D, sdkAddIndex, 'idx_child_label'));
end;

procedure TTestSchemaDiffContracts.TestNewTableGetsForeignKeyDiff;
var
  D: TOPSchemaDiffArray;
begin
  AddTable('SCParent', ['ID', 'Name']);
  D:=Compare;
  AssertEquals('Child table', 1, CountKind(D, sdkAddTable));
  AssertEquals('Foreign key of the new table', 1, CountKind(D, sdkAddForeignKey));
end;

procedure TTestSchemaDiffContracts.TestDiffOrderTablesFirst;
var
  D: TOPSchemaDiffArray;
  I, LastTable, FirstOther: Integer;
begin
  D:=Compare;
  LastTable:=-1;
  FirstOther:=Length(D);
  for I:=0 to High(D) do
    if D[I].Kind=sdkAddTable then
      LastTable:=I
    else if I<FirstOther then
      FirstOther:=I;
  AssertTrue('Tables before indexes and foreign keys', LastTable<FirstOther);
  AssertTrue('Index before foreign key',
    (CountKind(D, sdkAddForeignKey)=0) or (D[High(D)].Kind=sdkAddForeignKey));
end;

procedure TTestSchemaDiffContracts.TestSortDiffsStable;
var
  D: TOPSchemaDiffArray;

  procedure Add(aKind: TSchemaDiffKind; const aTable: String);
  begin
    SetLength(D, Length(D)+1);
    D[High(D)]:=Default(TOPSchemaDiffItem);
    D[High(D)].Kind:=aKind;
    D[High(D)].TableName:=aTable;
  end;

begin
  D:=Nil;
  Add(sdkDropTable, 'X');
  Add(sdkAddForeignKey, 'A');
  Add(sdkAddTable, 'B');
  Add(sdkAddColumn, 'C');
  Add(sdkAddTable, 'D');
  Add(sdkDropForeignKey, 'E');
  Add(sdkDropColumn, 'F');
  SortDiffs(D);
  AssertEquals('B', D[0].TableName);
  AssertEquals('D', D[1].TableName);
  // A changed relation keeps its constraint name: the old one goes first
  AssertEquals('Foreign key dropped before any is added', 'E', D[2].TableName);
  AssertEquals('C', D[3].TableName);
  AssertEquals('A', D[4].TableName);
  AssertEquals('F', D[5].TableName);
  AssertEquals('X', D[6].TableName);
end;

procedure TTestSchemaDiffContracts.TestPKIndexByColumnsKept;
var
  T: Integer;
begin
  T:=AddTable('SCParent', ['ID', 'Name']);
  AddIndex(T, 'some_generated_name', ['ID']);
  AddTable('SCChild', ['ID', 'ParentID', 'Label_']);
  AssertEquals('Primary key index not dropped', 0, CountKind(Compare, sdkDropIndex));
end;

procedure TTestSchemaDiffContracts.TestPKIndexByNameKept;
var
  T: Integer;
begin
  T:=AddTable('SCParent', ['ID', 'Name']);
  // Columns not reported by the introspection
  AddIndex(T, 'SCParent_PK', []);
  AddIndex(T, 'SCParent_pkey', []);
  AddTable('SCChild', ['ID', 'ParentID', 'Label_']);
  AssertEquals(0, CountKind(Compare, sdkDropIndex));
end;

procedure TTestSchemaDiffContracts.TestIndexWithPKInNameDropped;
var
  T: Integer;
begin
  T:=AddTable('SCParent', ['ID', 'Name']);
  AddIndex(T, 'IDX_PKG_NAME', ['Name']);
  AddTable('SCChild', ['ID', 'ParentID', 'Label_']);
  AssertEquals('Not a primary key index', 1, CountKind(Compare, sdkDropIndex, 'IDX_PKG_NAME'));
end;

procedure TTestSchemaDiffContracts.TestNarrowingVarchar;
begin
  AssertTrue(IsNarrowingTypeChange('VARCHAR(50)', Column('VARCHAR(80)')));
  AssertTrue('Length from MaxLength', IsNarrowingTypeChange('VARCHAR(50)', Column('character varying', 80)));
end;

procedure TTestSchemaDiffContracts.TestWideningVarchar;
begin
  AssertFalse(IsNarrowingTypeChange('VARCHAR(80)', Column('VARCHAR(50)')));
  AssertFalse('To text', IsNarrowingTypeChange('TEXT', Column('VARCHAR(50)')));
  AssertFalse('CHAR to VARCHAR', IsNarrowingTypeChange('VARCHAR(10)', Column('CHAR(10)')));
end;

procedure TTestSchemaDiffContracts.TestNarrowingToFixedFromText;
begin
  AssertTrue('Text to VARCHAR', IsNarrowingTypeChange('VARCHAR(255)', Column('TEXT')));
  AssertTrue('VARCHAR to CHAR', IsNarrowingTypeChange('CHAR(10)', Column('VARCHAR(10)')));
end;

procedure TTestSchemaDiffContracts.TestNarrowingDecimal;
begin
  AssertTrue('Scale', IsNarrowingTypeChange('DECIMAL(18,2)', Column('NUMERIC', 0, 18, 4)));
  AssertTrue('Integer digits', IsNarrowingTypeChange('DECIMAL(10,4)', Column('NUMERIC(18,4)')));
  AssertFalse('Wider', IsNarrowingTypeChange('DECIMAL(18,4)', Column('NUMERIC(12,4)')));
end;

procedure TTestSchemaDiffContracts.TestWideningInteger;
begin
  AssertFalse(IsNarrowingTypeChange('BIGINT', Column('INTEGER')));
  AssertFalse(IsNarrowingTypeChange('INT', Column('SMALLINT')));
end;

procedure TTestSchemaDiffContracts.TestNarrowingInteger;
begin
  AssertTrue(IsNarrowingTypeChange('INT', Column('BIGINT')));
end;

procedure TTestSchemaDiffContracts.TestNarrowingOtherFamily;
begin
  AssertTrue('Text to integer', IsNarrowingTypeChange('INT', Column('VARCHAR(10)')));
  AssertTrue('Timestamp to date', IsNarrowingTypeChange('DATE', Column('TIMESTAMP')));
end;

procedure TTestSchemaDiffContracts.TestNarrowingFlagOnDiff;
var
  D: TOPSchemaDiffArray;
  T, I: Integer;
  Found: Boolean;
begin
  T:=AddTable('SCParent', ['ID', 'Name']);
  FSchema.Tables[T].Columns[1]:=Column('VARCHAR(80)');
  FSchema.Tables[T].Columns[1].ColumnName:='Name';
  AddTable('SCChild', ['ID', 'ParentID', 'Label_']);
  D:=Compare;
  Found:=False;
  for I:=0 to High(D) do
    if (D[I].Kind=sdkAlterColumnType) and SameText(D[I].ColumnName, 'Name') then
    begin
      Found:=True;
      AssertTrue('80 to 50 is narrowing', D[I].Narrowing);
    end;
  AssertTrue('Type change found', Found);
end;

{ TTestMigrationContracts }

procedure TTestMigrationContracts.SetUp;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FRegistry:=TOPRegistry.Create(Nil);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  FMigrator:=TOPSchemaMigrator.Create(Nil);
  FMigrator.Registry:=FRegistry;
  FMigrator.RelationRegistry:=FRelations;
  FMigrator.M2MRegistry:=FM2M;
  FMigrator.SQLDialect:=dSQLite;
end;

procedure TTestMigrationContracts.TearDown;
begin
  FreeAndNil(FMigrator);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestMigrationContracts.Exec(const aSQL: String);
begin
  if FTrans.Active then
    FTrans.Commit;
  FConn.ExecuteDirect(aSQL);
  FTrans.Commit;
end;

function TTestMigrationContracts.TableExists(const aName: String): Boolean;
var
  L: TStringList;
begin
  L:=TStringList.Create;
  try
    FConn.GetTableNames(L);
    if FTrans.Active then
      FTrans.Commit;
    Result:=L.IndexOf(aName)>=0;
  finally
    L.Free;
  end;
end;

function TTestMigrationContracts.CurrentVersion: Integer;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:='SELECT MAX(Version) FROM '+cMigrationTable;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
  finally
    Q.Free;
  end;
  if FTrans.Active then
    FTrans.Commit;
end;

procedure TTestMigrationContracts.RegisterParent;
var
  D: TPersisterDef;
begin
  D:=FRegistry.RegisterPersister(TSCParent, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  D.FieldMap.FindByPropertyName('Name').MaxSize:=50;
end;

procedure TTestMigrationContracts.TestTypeChangeSkippedOnSQLite;
var
  R: TOPMigrationResult;
begin
  RegisterParent;
  FMigrator.CreateSchema(FConn);
  FRegistry.FindPersister(TSCParent).FieldMap.FindByPropertyName('Name').MaxSize:=80;
  R:=FMigrator.MigrateSchema(FConn, 2);
  AssertEquals('Nothing to execute', 0, R.StatementsExecuted);
  AssertEquals('Version still recorded', 2, CurrentVersion);
end;

procedure TTestMigrationContracts.TestTypeChangeDryRunDoesNotRaise;
var
  R: TOPMigrationResult;
begin
  RegisterParent;
  FMigrator.CreateSchema(FConn);
  FRegistry.FindPersister(TSCParent).FieldMap.FindByPropertyName('Name').MaxSize:=80;
  R:=FMigrator.MigrateSchema(FConn, 2, [moDryRun]);
  AssertEquals('Difference found', 1, R.DiffsFound);
  AssertEquals('But no SQL', 0, Length(R.SQL));
end;

{ Statement 3 of the migration fails: an index name that is already used }

procedure TTestMigrationContracts.TestFailedMigrationLeavesNothing;
var
  D: TPersisterDef;
  Idx: TOPIndexDef;
  Raised: Boolean;
begin
  RegisterParent;
  FMigrator.CreateSchema(FConn);
  Exec('CREATE TABLE Unrelated (ID INTEGER)');
  Exec('CREATE INDEX idx_taken ON Unrelated (ID)');
  D:=FRegistry.RegisterPersister(TSCChild, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  D:=FRegistry.RegisterPersister(TSCOther, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  Idx.IndexName:='idx_taken';
  Idx.FieldNames:='Code';
  D.Indexes:=[Idx];
  Raised:=False;
  try
    FMigrator.MigrateSchema(FConn, 2);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Migration failed', Raised);
  AssertFalse('First new table rolled back', TableExists('SCChild'));
  AssertFalse('Second new table rolled back', TableExists('SCOther'));
  AssertEquals('Version unchanged', 1, CurrentVersion);
end;

procedure TTestMigrationContracts.TestRerunAfterFailedMigration;
var
  D: TPersisterDef;
  Idx: TOPIndexDef;
begin
  RegisterParent;
  FMigrator.CreateSchema(FConn);
  Exec('CREATE TABLE Unrelated (ID INTEGER)');
  Exec('CREATE INDEX idx_taken ON Unrelated (ID)');
  D:=FRegistry.RegisterPersister(TSCOther, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  Idx.IndexName:='idx_taken';
  Idx.FieldNames:='Code';
  D.Indexes:=[Idx];
  try
    FMigrator.MigrateSchema(FConn, 2);
  except
    on E: EOPF do ;
  end;
  Exec('DROP INDEX idx_taken');
  FMigrator.MigrateSchema(FConn, 2);
  AssertTrue('Table created', TableExists('SCOther'));
  AssertEquals('Version recorded', 2, CurrentVersion);
end;

procedure TTestMigrationContracts.TestFailedCreateSchemaLeavesNothing;
var
  D: TPersisterDef;
  Raised: Boolean;
begin
  RegisterParent;
  D:=FRegistry.RegisterPersister(TSCOther, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  // The second CREATE TABLE fails
  Exec('CREATE TABLE SCOther (ID INTEGER)');
  Raised:=False;
  try
    FMigrator.CreateSchema(FConn);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Failed', Raised);
  AssertFalse('First table rolled back', TableExists('SCParent'));
  AssertFalse('No tracking table', TableExists(cMigrationTable));
end;

procedure TTestMigrationContracts.TestCreateSchemaRecordsVersion;
begin
  RegisterParent;
  FMigrator.CreateSchema(FConn);
  AssertTrue(TableExists('SCParent'));
  AssertEquals(1, CurrentVersion);
end;

initialization
  RegisterTests([TTestDDLContracts, TTestSchemaDiffContracts, TTestMigrationContracts]);
end.
