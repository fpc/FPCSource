unit utc.opf.ddl;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, fpcunit, testregistry,
{$ENDIF}
  Variants,
  Opf.Types, Opf.Driver, Opf.DDL, Opf.Sql, Opf.Fields, Opf.M2M, Opf.Registry,
  Opf.Persister.RTTI, Opf.Relations, Opf.Schema, sqlite3conn, sqldb;

Type
  TJunctionTestClassA = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  TJunctionTestClassB = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  TDepTestClassC = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  TDepTestClassD = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  TDepTestSelfRef = class(TObject)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
  end;

  TMyOPDDLBuilder = Class(TOPDDLBuilder);

  { TTestDialectDDLBuilder - subclass that overrides GetBooleanKeyword for OCP verification }

  TTestDialectDDLBuilder = Class(TOPDDLBuilder)
  Protected
    function GetBooleanKeyword: TSQLKeyWord; override;
  end;

  { TTestDDLBuilder }

  TTestDDLBuilder = Class(TTestCase)
  private
    FBuild: TOPDDLBuilder;
    FMap: TOPFieldMapList;
    FTable: UTF8String;
    procedure SetinstanceClass;
    procedure DoCreateFloatNegativeBound;
    procedure DoCreateEmptyFieldMap;
    procedure DoCreateEmptyTableName;
    procedure DoCreateInjectedTableName;
    procedure TestBool(aDialect: TSQLDialect);
    procedure TestChar(aKind: TTypeKind);
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Function Add(aKind: TTypeKind;aMin,aMax : Integer; const aName : string = '') : TOPFieldMap;
    Property Builder : TOPDDLBuilder Read FBuild;
    Property Map : TOPFieldMapList Read FMap;
    Property Table : UTF8String Read FTable;
  Published
    Procedure TestCreateFormatting;
    Procedure TestCreateSmallint;
    Procedure TestCreateInt;
    Procedure TestCreateBigint;
    Procedure TestCreateString;
    Procedure TestCreateChar;
    Procedure TestCreateUChar;
    Procedure TestCreateWChar;
    Procedure TestCreateFloat;
    procedure TestCreateBoolean;
    procedure TestCreateWithPrimaryKey;
    procedure TestCreateWithPrimaryKeyQuoted;
    procedure TestCreateWithPrimaryKeyNewlines;
    procedure TestCreateWithCompositePrimaryKey;
    procedure TestGlobalInstance;
    procedure TestGlobalInstanceClass;
    procedure TestDialectSubclassOverride;
    procedure TestCreateFloatNegativeBound;
    procedure TestMemoPostgreSQL;
    procedure TestCreateEmptyFieldMap;
    procedure TestCreateEmptyTableName;
    procedure TestCreateUnsafeTableName;
    procedure TestCreateSetAsInt;
    procedure TestCreateSetAsString;
    procedure TestCreateEnumAsSmallInt;
    procedure TestCreateEnumAsString;
    procedure TestCreateVariantAsMemo;
    procedure TestCreateNullableColumn;
    procedure TestCreateNullableWithRequired;
    procedure TestCreateIfNotExistsSQLite;
    procedure TestCreateIfNotExistsUnsupportedDialect;
    procedure TestCreateWithoutIfNotExists;
    // Keyword tests
    procedure TestKeywordAlter;
    procedure TestKeywordDrop;
    procedure TestKeywordForeignKey;
    procedure TestKeywordCascade;
    procedure TestKeywordUnique;
    // Constraint name tests
    procedure TestConstraintNameFK;
    procedure TestConstraintNameUnique;
    procedure TestConstraintNameFKCustomPrefix;
    // Foreign key SQL tests
    procedure TestForeignKeySQL;
    procedure TestForeignKeySQLCascade;
    procedure TestForeignKeySQLNoCascade;
    procedure TestForeignKeySQLCustomName;
    procedure TestForeignKeySQLQuoted;
    procedure TestForeignKeySQLIfNotExistsPostgres;
    procedure TestForeignKeySQLIfNotExistsPostgresQuoted;
    procedure TestForeignKeySQLIfNotExistsOtherDialect;
    procedure TestForeignKeySQLLowerCase;
    // Junction table DDL tests
    procedure TestJunctionTableSQL;
    procedure TestJunctionTableSQLCascade;
    procedure TestJunctionTableSQLNoCascade;
    procedure TestJunctionTableSQLStringPK;
    procedure TestJunctionTableSQLQuoted;
    procedure TestJunctionTableSQLLowerCase;
    // Creation order tests
    procedure TestCreationOrderIndependent;
    procedure TestCreationOrderParentChild;
    procedure TestCreationOrderDiamond;
    procedure TestCreationOrderCircular;
    procedure TestCreationOrderSelfRef;
    // Schema integration test
    procedure TestCreateSchemaIntegration;
    // UNIQUE and DEFAULT tests
    procedure TestPropertyDefSQLUnique;
    procedure TestPropertyDefSQLRequiredUnique;
    procedure TestPropertyDefSQLDefaultString;
    procedure TestPropertyDefSQLDefaultInteger;
    procedure TestPropertyDefSQLDefaultBoolean;
    procedure TestPropertyDefSQLDefaultFloat;
    procedure TestPropertyDefSQLUniqueWithDefault;
    // ALTER TABLE DDL tests
    procedure TestGetAddColumnSQL;
    procedure TestGetDropColumnSQL;
    procedure TestGetAlterColumnTypeSQL;
    procedure TestGetAlterColumnNullableSetNotNull;
    procedure TestGetAlterColumnNullableDropNotNull;
    procedure TestGetDropTableSQL;
    procedure TestGetDropIndexSQL;
    procedure TestGetDropForeignKeySQL;
    procedure TestGenerateMigrationSQL;
    // MySQL dialect tests
    procedure TestGetAlterColumnTypeSQLMySQL;
    procedure TestGetDropForeignKeySQLMySQL;
    // SQLite limitation tests
    procedure TestGetAlterColumnTypeSQLSQLite;
    procedure TestGetAlterColumnNullableSQLSQLite;
    // MySQL limitation test
    procedure TestGetAlterColumnNullableSQLMySQL;
  end;



implementation

{ TTestDialectDDLBuilder }

function TTestDialectDDLBuilder.GetBooleanKeyword: TSQLKeyWord;
begin
  Result:=kwBoolean;
end;

{ TTestDDLBuilder }

procedure TTestDDLBuilder.SetUp;
begin
  inherited SetUp;
  FBuild:=TOPDDLBuilder.Create(Nil);
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestDDLBuilder.TearDown;
begin
  FreeAndNil(FBuild);
  FreeAndNil(FMap);
  inherited TearDown;
end;

function TTestDDLBuilder.Add(aKind: TTypeKind; aMin, aMax: Integer; const aName: string): TOPFieldMap;

Var
  FN : String;

begin
  FN:=aName;
  if FN='' then
    FN:='F1';
  Result:=Map.AddField(FN,FN,[]);
  Result.PropertyKind:=aKind;
  Result.MinSize:=aMin;
  Result.MaxSize:=aMax;
end;

procedure TTestDDLBuilder.TestCreateFormatting;
Var
  S : UTF8string;

begin
  Add(tkInteger,0,maxSmallint);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 SMALLINT)',S);
  S:=Builder.GetCreateSQL('a',Map.AsArray,[coForceQuotes]);
  AssertEquals('Create quoted','CREATE TABLE "a" ("F1" SMALLINT)',S);
  S:=Builder.GetCreateSQL('a',Map.AsArray,[coForceNewLine]);
  AssertEquals('Create newlines','CREATE TABLE a ('+sLineBreak+'  F1 SMALLINT'+sLineBreak+'  )',S);
  Builder.OPtions:=[sboKeyWordsLowerCase];
  S:=Builder.GetCreateSQL('a',Map.AsArray,[]);
  AssertEquals('Create lowercase','create table a (F1 smallint)',S);
end;

procedure TTestDDLBuilder.TestCreateSmallint;

Var
  S : UTF8string;

begin
  Add(tkInteger,0,maxSmallint);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 SMALLINT)',S);
  Map.Clear;
  Add(tkInteger,0,maxSmallint).Options:=[foRequired];
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create req','CREATE TABLE a (F1 SMALLINT NOT NULL)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetCreateSQL('a',Map.AsArray,[]);
  AssertEquals('Create req lowercase','create table a (F1 smallint not null)',S);
end;

procedure TTestDDLBuilder.TestCreateInt;

Var
  S : UTF8string;

begin
  Add(tkInteger,0,maxSmallint+1);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create maxbound','CREATE TABLE a (F1 INT)',S);
  Map.Clear;
  Add(tkInteger,0,0);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create no maxbound','CREATE TABLE a (F1 INT)',S);
  Map.Clear;
  Add(tkInteger,0,0).OPtions:=[foRequired];
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create req no maxbound','CREATE TABLE a (F1 INT NOT NULL)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetCreateSQL('a',Map.AsArray,[]);
  AssertEquals('Create lowercase req no maxbound','create table a (F1 int not null)',S);
end;

procedure TTestDDLBuilder.TestCreateBigint;

Var
  S : UTF8string;

begin
  Add(tkInt64,0,maxSmallint+1);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 BIGINT)',S);
  Map.Clear;
  Add(tkInt64,0,0);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create no bound','CREATE TABLE a (F1 BIGINT)',S);
  Map.Clear;
  Add(tkInt64,0,0).Options:=[foRequired];
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create no bound','CREATE TABLE a (F1 BIGINT NOT NULL)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetCreateSQL('a',Map.AsArray,[]);
  AssertEquals('Create no bound lowercase ','create table a (F1 bigint not null)',S);
end;

procedure TTestDDLBuilder.TestCreateString;

Var
  S : UTF8string;
  D : TSQLDialect;

begin
  Add(tkString,0,32);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 VARCHAR(32))',S);
  Map.Clear;
  Add(tkString,32,32);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create char','CREATE TABLE a (F1 CHAR(32))',S);
  Map.Clear;
  Add(tkString,0,32).Options:=[foRequired];
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create req','CREATE TABLE a (F1 VARCHAR(32) NOT NULL)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetCreateSQL('a',Map.AsArray,[]);
  AssertEquals('Create req','create table a (F1 varchar(32) not null)',S);
  Builder.Options:=[];
  Map.Clear;
  Add(tkString,0,MaxStringLen+1).Options:=[];
  for d in TSQLDialect do
    begin
    Builder.SQLDialect:=D;
    S:=Builder.GetCreateSQL('a',Map.AsArray,[]);
    AssertEquals(D.ToString+' Create memo','CREATE TABLE a (F1 '+TMyOPDDLBuilder(Builder).MemoField+')',S);
    end;
end;

procedure TTestDDLBuilder.TestChar(aKind : TTypeKind);

Var
  S : UTF8string;

begin
  AssertTrue('Correct kind',aKind in [tkChar,tkUChar,tkWChar]);
  Add(aKind,0,0);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 CHAR(1))',S);
  Map.Clear;
  Add(aKind,0,10);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create varchar','CREATE TABLE a (F1 VARCHAR(10))',S);
end;

procedure TTestDDLBuilder.TestCreateChar;
begin
  TestChar(tkChar);
end;

procedure TTestDDLBuilder.TestCreateUChar;

begin
  TestChar(tkUChar);
end;

procedure TTestDDLBuilder.TestCreateWChar;
begin
  TestChar(tkWChar);
end;

procedure TTestDDLBuilder.TestCreateFloat;

Var
  S : UTF8string;

begin
  Add(tkFloat,0,0);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 FLOAT)',S);
  map.Clear;
  Add(tkFloat,4,12);
  Builder.SQLDialect:=dFirebird;
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals('Create','CREATE TABLE a (F1 DECIMAL(12,4))',S);
end;

procedure TTestDDLBuilder.TestBool(aDialect : TSQLDialect);

Var
  S, BoolName : UTF8string;

  // The column type of a boolean: the SMALLINT type of the dialect or the boolean keyword
  function BoolType: UTF8String;
  begin
    if TMyOPDDLBuilder(Builder).GetBooleanKeyword=kwSmallInt then
      Result:=TMyOPDDLBuilder(Builder).SmallIntTypeSQL
    else
      Result:=TMyOPDDLBuilder(Builder).KeyWord(TMyOPDDLBuilder(Builder).GetBooleanKeyword);
  end;

begin
  Map.Clear;
  Builder.Options:=[];
  Builder.SQLDialect:=aDialect;
  Add(tkBool,0,0);
  BoolName:=BoolType;
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals(aDialect.ToString+' Create','CREATE TABLE a (F1 '+BoolName+')',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  BoolName:=BoolType;
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertEquals(aDialect.ToString+' Create lower','create table a (F1 '+BoolName+')',S);
end;


procedure TTestDDLBuilder.TestCreateBoolean;

Var
  D : TSQLDialect;

begin
  For D in TSQLDialect do
    TestBool(D);
end;

procedure TTestDDLBuilder.TestCreateWithPrimaryKey;
var
  S: UTF8String;
begin
  Map.AddField('ID','ID',[foInKey]).PropertyKind:=tkInteger;
  Map[0].MinSize:=0;
  Map[0].MaxSize:=MaxInt;
  Map.AddField('Name','Name',[foInSelect, foInInsert]).PropertyKind:=tkAString;
  Map[1].MaxSize:=100;
  S:=Builder.GetCreateSQL('Contact',Map.AsArray);
  AssertEquals('PK constraint',
    'CREATE TABLE Contact (ID INT, Name VARCHAR(100), CONSTRAINT Contact_PK PRIMARY KEY(ID))',S);
end;

procedure TTestDDLBuilder.TestCreateWithPrimaryKeyQuoted;
var
  S: UTF8String;
begin
  Map.AddField('ID','ID',[foInKey]).PropertyKind:=tkInteger;
  Map[0].MinSize:=0;
  Map[0].MaxSize:=MaxInt;
  Map.AddField('Name','Name',[foInSelect]).PropertyKind:=tkAString;
  Map[1].MaxSize:=100;
  S:=Builder.GetCreateSQL('Contact',Map.AsArray,[coForceQuotes]);
  AssertEquals('PK constraint quoted',
    'CREATE TABLE "Contact" ("ID" INT, "Name" VARCHAR(100), CONSTRAINT Contact_PK PRIMARY KEY("ID"))',S);
end;

procedure TTestDDLBuilder.TestCreateWithPrimaryKeyNewlines;
var
  S: UTF8String;
begin
  Map.AddField('ID','ID',[foInKey]).PropertyKind:=tkInteger;
  Map[0].MinSize:=0;
  Map[0].MaxSize:=MaxInt;
  Map.AddField('Name','Name',[foInSelect]).PropertyKind:=tkAString;
  Map[1].MaxSize:=100;
  S:=Builder.GetCreateSQL('Contact',Map.AsArray,[coForceNewLine]);
  AssertEquals('PK constraint newlines',
    'CREATE TABLE Contact ('+sLineBreak+'  ID INT, '+sLineBreak+'  Name VARCHAR(100), '+sLineBreak+'  CONSTRAINT Contact_PK PRIMARY KEY(ID)'+sLineBreak+'  )',S);
end;

procedure TTestDDLBuilder.TestCreateWithCompositePrimaryKey;
var
  S: UTF8String;
begin
  Map.AddField('OrderID','OrderID',[foInKey]).PropertyKind:=tkInteger;
  Map[0].MinSize:=0;
  Map[0].MaxSize:=MaxInt;
  Map.AddField('LineNo','LineNo',[foInKey]).PropertyKind:=tkInteger;
  Map[1].MinSize:=0;
  Map[1].MaxSize:=MaxSmallint;
  Map.AddField('Product','Product',[foInSelect]).PropertyKind:=tkAString;
  Map[2].MaxSize:=50;
  S:=Builder.GetCreateSQL('OrderLine',Map.AsArray);
  AssertEquals('Composite PK',
    'CREATE TABLE OrderLine (OrderID INT, LineNo SMALLINT, Product VARCHAR(50), CONSTRAINT OrderLine_PK PRIMARY KEY(OrderID, LineNo))',S);
end;

procedure TTestDDLBuilder.TestGlobalInstance;
Var
  R : TOPDDLBuilder;
begin
  R:=TOPDDLBuilder.Instance;
  AssertNotNull('Have instance',R);
  TOPDDLBuilder.ClearInstance;
  AssertNotNull('Have instance',TOPDDLBuilder.Instance);
end;

procedure TTestDDLBuilder.SetinstanceClass;
begin
  TOPDDLBuilder.SetInstanceClass(TMyOPDDLBuilder);
end;

procedure TTestDDLBuilder.TestGlobalInstanceClass;

Var
  R : TOPDDLBuilder;

begin
  TOPDDLBuilder.ClearInstance;
  Self.SetInstanceClass;
  try
    R:=TOPDDLBuilder.Instance;
    AssertNotNull('Have instance',R);
    AssertEquals('Correct class',TMyOPDDLBuilder,R.ClassType);
    AssertException('Cannot set class if instance is active',EOPFDDL,@Self.SetinstanceClass);
  finally
    TOPDDLBuilder.ClearInstance;
    TOPDDLBuilder.SetInstanceClass(Nil);
  end;
end;

procedure TTestDDLBuilder.TestDialectSubclassOverride;

Var
  Sub: TTestDialectDDLBuilder;
  S: UTF8String;
  M: TOPFieldMapList;

begin
  Sub:=TTestDialectDDLBuilder.Create(Nil);
  M:=TOPFieldMapList.Create(TOPFieldMap);
  try
    Sub.SQLDialect:=dVanilla;
    M.AddField('F1','F1',[]).PropertyKind:=tkBool;
    S:=Sub.GetPropertyDefSQL('a',M[0]);
    AssertEquals('Subclass overrides boolean keyword','BOOLEAN',S);
  finally
    M.Free;
    Sub.Free;
  end;
end;

procedure TTestDDLBuilder.DoCreateFloatNegativeBound;
begin
  Builder.GetCreateSQL('a',Map.AsArray);
end;

procedure TTestDDLBuilder.TestCreateFloatNegativeBound;
begin
  Add(tkFloat,2,-5);
  AssertException('Negative MaxSize for float', EOPFDDL, @DoCreateFloatNegativeBound);
end;

procedure TTestDDLBuilder.TestMemoPostgreSQL;
var
  S: UTF8String;
begin
  Builder.SQLDialect:=dPostGres;
  Add(tkAString,0,65000);
  S:=Builder.GetCreateSQL('a',Map.AsArray);
  AssertTrue('PostgreSQL memo should use TEXT', Pos('TEXT', S) > 0);
end;

procedure TTestDDLBuilder.DoCreateEmptyFieldMap;
var
  EmptyMap: TOPFieldMapArray;
begin
  SetLength(EmptyMap,0);
  Builder.GetCreateSQL('a',EmptyMap);
end;

procedure TTestDDLBuilder.DoCreateEmptyTableName;
begin
  Add(tkInteger,0,0);
  Builder.GetCreateSQL('',Map.AsArray);
end;

procedure TTestDDLBuilder.TestCreateEmptyFieldMap;
begin
  AssertException('Empty field map', EOPFDDL, @DoCreateEmptyFieldMap);
end;

procedure TTestDDLBuilder.TestCreateEmptyTableName;
begin
  AssertException('Empty table name', EOPFDDL, @DoCreateEmptyTableName);
end;

procedure TTestDDLBuilder.DoCreateInjectedTableName;
begin
  Add(tkInteger,0,0);
  Builder.GetCreateSQL('t; DROP TABLE t',Map.AsArray);
end;

procedure TTestDDLBuilder.TestCreateUnsafeTableName;
begin
  AssertException('Unsafe table name', EOPFDDL, @DoCreateInjectedTableName);
end;

procedure TTestDDLBuilder.TestCreateSetAsInt;
var
  S: UTF8String;
begin
  Add(tkSet, 0, 0);
  S := Builder.GetCreateSQL('a', Map.AsArray);
  AssertEquals('Set as INT', 'CREATE TABLE a (F1 INT)', S);
end;

procedure TTestDDLBuilder.TestCreateSetAsString;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkSet, 0, 50);
  F.Options := [foAsString];
  S := Builder.GetCreateSQL('a', Map.AsArray);
  AssertEquals('Set as VARCHAR', 'CREATE TABLE a (F1 VARCHAR(50))', S);
end;

procedure TTestDDLBuilder.TestCreateEnumAsSmallInt;
var
  S: UTF8String;
begin
  Add(tkEnumeration, 0, 0);
  S := Builder.GetCreateSQL('a', Map.AsArray);
  AssertEquals('Enum as SMALLINT', 'CREATE TABLE a (F1 SMALLINT)', S);
end;

procedure TTestDDLBuilder.TestCreateEnumAsString;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkEnumeration, 0, 30);
  F.Options := [foAsString];
  S := Builder.GetCreateSQL('a', Map.AsArray);
  AssertEquals('Enum as VARCHAR', 'CREATE TABLE a (F1 VARCHAR(30))', S);
end;

procedure TTestDDLBuilder.TestCreateVariantAsMemo;
var
  S: UTF8String;
  D: TSQLDialect;
begin
  Add(tkVariant, 0, 0);
  for D in TSQLDialect do
  begin
    Builder.SQLDialect := D;
    S := Builder.GetCreateSQL('a', Map.AsArray);
    AssertEquals(D.ToString + ' Variant as memo',
      'CREATE TABLE a (F1 ' + TMyOPDDLBuilder(Builder).MemoField + ')', S);
    Map.Clear;
    Add(tkVariant, 0, 0);
  end;
end;

procedure TTestDDLBuilder.TestCreateNullableColumn;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkInteger, 0, 0);
  F.Options := [foNullable];
  S := Builder.GetCreateSQL('a', Map.AsArray);
  AssertEquals('Nullable column has no NOT NULL', 'CREATE TABLE a (F1 INT)', S);
end;

procedure TTestDDLBuilder.TestCreateNullableWithRequired;
var
  F: TOPFieldMap;
  Raised: Boolean;
begin
  F := Add(tkInteger, 0, 0);
  Raised := False;
  try
    F.Options := [foNullable, foRequired];
  except
    on E: EFieldMap do
      Raised := True;
  end;
  AssertTrue('Setting foNullable+foRequired raises EFieldMap', Raised);
end;

procedure TTestDDLBuilder.TestCreateIfNotExistsSQLite;
var
  S: UTF8String;
begin
  Builder.SQLDialect := dSQLite;
  Add(tkInteger, 0, 0, 'ID');
  Add(tkAString, 0, 255, 'Name');
  S := Builder.GetCreateSQL('Film', Map.AsArray, [coIfNotExists]);
  AssertTrue('Contains IF NOT EXISTS', Pos('IF NOT EXISTS', S) > 0);
  AssertTrue('Starts with CREATE TABLE IF NOT EXISTS',
    Pos('CREATE TABLE IF NOT EXISTS', S) = 1);
end;

procedure TTestDDLBuilder.TestCreateIfNotExistsUnsupportedDialect;
var
  Raised: Boolean;
begin
  Raised := False;
  Builder.SQLDialect := dSQLServer;
  Add(tkInteger, 0, 0, 'ID');
  try
    Builder.GetCreateSQL('Test', Map.AsArray, [coIfNotExists]);
  except
    on E: EOPFDDL do
      Raised := True;
  end;
  AssertTrue('dSQLServer with coIfNotExists raises EOPFDDL', Raised);
end;

procedure TTestDDLBuilder.TestCreateWithoutIfNotExists;
var
  S: UTF8String;
begin
  Builder.SQLDialect := dSQLite;
  Add(tkInteger, 0, 0, 'ID');
  S := Builder.GetCreateSQL('Film', Map.AsArray, []);
  AssertTrue('No IF NOT EXISTS without option', Pos('IF NOT EXISTS', S) = 0);
  AssertTrue('Starts with CREATE TABLE Film', Pos('CREATE TABLE Film', S) = 1);
end;

procedure TTestDDLBuilder.TestKeywordAlter;
begin
  AssertEquals('kwAlter', 'ALTER', kwAlter.ToString);
end;

procedure TTestDDLBuilder.TestKeywordDrop;
begin
  AssertEquals('kwDrop', 'DROP', kwDrop.ToString);
end;

procedure TTestDDLBuilder.TestKeywordForeignKey;
begin
  AssertEquals('kwForeign', 'FOREIGN', kwForeign.ToString);
  AssertEquals('kwKey', 'KEY', kwKey.ToString);
  AssertEquals('kwReferences', 'REFERENCES', kwReferences.ToString);
end;

procedure TTestDDLBuilder.TestKeywordCascade;
begin
  AssertEquals('kwCascade', 'CASCADE', kwCascade.ToString);
end;

procedure TTestDDLBuilder.TestKeywordUnique;
begin
  AssertEquals('kwUnique', 'UNIQUE', kwUnique.ToString);
end;

procedure TTestDDLBuilder.TestConstraintNameFK;
var
  S: UTF8String;
begin
  S := TMyOPDDLBuilder(Builder).GetConstraintName('OrderLine', 'Film', ctForeignKey);
  AssertEquals('FK constraint name', 'fk_OrderLine_Film', S);
end;

procedure TTestDDLBuilder.TestConstraintNameUnique;
var
  S: UTF8String;
begin
  S := TMyOPDDLBuilder(Builder).GetConstraintName('Person', 'Email', ctUnique);
  AssertEquals('Unique constraint name', 'uq_Person_Email', S);
end;

procedure TTestDDLBuilder.TestConstraintNameFKCustomPrefix;
var
  S: UTF8String;
begin
  Builder.FKPrefix := 'FK_';
  S := TMyOPDDLBuilder(Builder).GetConstraintName('OrderLine', 'Film', ctForeignKey);
  AssertEquals('Custom FK prefix', 'FK_OrderLine_Film', S);
end;

procedure TTestDDLBuilder.TestForeignKeySQL;
var
  S: UTF8String;
begin
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False);
  AssertEquals('Basic FK SQL',
    'ALTER TABLE OrderLine ADD CONSTRAINT fk_OrderLine_Film FOREIGN KEY (FilmID) REFERENCES Film(ID)', S);
end;

procedure TTestDDLBuilder.TestForeignKeySQLCascade;
var
  S: UTF8String;
begin
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', True);
  AssertEquals('FK SQL with CASCADE',
    'ALTER TABLE OrderLine ADD CONSTRAINT fk_OrderLine_Film FOREIGN KEY (FilmID) REFERENCES Film(ID) ON DELETE CASCADE', S);
end;

procedure TTestDDLBuilder.TestForeignKeySQLNoCascade;
var
  S: UTF8String;
begin
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False);
  AssertTrue('No ON DELETE without cascade', Pos('ON DELETE', S) = 0);
end;

procedure TTestDDLBuilder.TestForeignKeySQLCustomName;
var
  S: UTF8String;
begin
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False, 'my_fk');
  AssertTrue('Custom constraint name used', Pos('CONSTRAINT my_fk FOREIGN', S) > 0);
  AssertTrue('No auto-generated fk_ prefix', Pos('fk_OrderLine', S) = 0);
end;

procedure TTestDDLBuilder.TestForeignKeySQLIfNotExistsPostgres;
var
  S: UTF8String;
begin
  Builder.SQLDialect := dPostGres;
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False, '', [coIfNotExists]);
  AssertEquals('Catalog check around ADD CONSTRAINT',
    'DO $opf$BEGIN IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = ''fk_orderline_film'''+
    ' AND conrelid = to_regclass(''OrderLine'')) THEN '+
    'ALTER TABLE OrderLine ADD CONSTRAINT fk_OrderLine_Film FOREIGN KEY (FilmID) REFERENCES Film(ID)'+
    '; END IF; END$opf$', S);
  AssertTrue('No colon: sqldb would see a parameter', Pos(':', S) = 0);
end;

procedure TTestDDLBuilder.TestForeignKeySQLIfNotExistsPostgresQuoted;
var
  S: UTF8String;
begin
  Builder.SQLDialect := dPostGres;
  Builder.Options := [sboQuoteIdentifiers];
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False, '', [coIfNotExists]);
  AssertTrue('Quoted constraint name kept as written: '+S, Pos('conname = ''fk_OrderLine_Film''', S) > 0);
  AssertTrue('Quoted table passed to to_regclass: '+S, Pos('to_regclass(''"OrderLine"'')', S) > 0);
end;

procedure TTestDDLBuilder.TestForeignKeySQLIfNotExistsOtherDialect;
var
  S: UTF8String;
begin
  Builder.SQLDialect := dFirebird;
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False, '', [coIfNotExists]);
  AssertTrue('Plain ALTER TABLE: '+S, Pos('ALTER TABLE', S) = 1);
end;

procedure TTestDDLBuilder.TestForeignKeySQLQuoted;
var
  S: UTF8String;
begin
  Builder.Options := [sboQuoteIdentifiers];
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', False);
  AssertTrue('Child table quoted', Pos('"OrderLine"', S) > 0);
  AssertTrue('FK field quoted', Pos('"FilmID"', S) > 0);
  AssertTrue('Parent table quoted', Pos('"Film"', S) > 0);
  AssertTrue('PK field quoted', Pos('"ID"', S) > 0);
  AssertTrue('Constraint name quoted', Pos('"fk_OrderLine_Film"', S) > 0);
end;

procedure TTestDDLBuilder.TestForeignKeySQLLowerCase;
var
  S: UTF8String;
begin
  Builder.Options := [sboKeywordsLowerCase];
  S := Builder.GetAddForeignKeySQL('OrderLine', 'FilmID', 'Film', 'ID', True);
  AssertTrue('alter lowercase', Pos('alter', S) > 0);
  AssertTrue('table lowercase', Pos('table', S) > 0);
  AssertTrue('foreign lowercase', Pos('foreign', S) > 0);
  AssertTrue('key lowercase', Pos('key', S) > 0);
  AssertTrue('references lowercase', Pos('references', S) > 0);
  AssertTrue('cascade lowercase', Pos('cascade', S) > 0);
end;

procedure TTestDDLBuilder.TestJunctionTableSQL;
var
  Reg: TOPRegistry;
  DefA, DefB: TPersisterDef;
  M2MDef: TOPManyToManyDef;
  S: UTF8String;
begin
  Reg:=TOPRegistry.Create(nil);
  M2MDef:=TOPManyToManyDef.Create(nil);
  try
    DefA:=Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    DefA.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefB:=Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    DefB.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    M2MDef.ClassA:=TJunctionTestClassA;
    M2MDef.ClassB:=TJunctionTestClassB;
    M2MDef.JunctionTable:='ClassAClassB';
    M2MDef.ForeignKeyA:='ClassAID';
    M2MDef.ForeignKeyB:='ClassBID';
    M2MDef.DBCascade:=False;
    S:=Builder.GetCreateJunctionTableSQL(M2MDef, Reg);
    AssertEquals('Basic junction table SQL',
      'CREATE TABLE ClassAClassB (ClassAID INT NOT NULL, ClassBID INT NOT NULL, CONSTRAINT ClassAClassB_PK PRIMARY KEY(ClassAID, ClassBID))', S);
  finally
    M2MDef.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestJunctionTableSQLCascade;
var
  Reg: TOPRegistry;
  DefA, DefB: TPersisterDef;
  M2MDef: TOPManyToManyDef;
  S: UTF8String;
begin
  Reg:=TOPRegistry.Create(nil);
  M2MDef:=TOPManyToManyDef.Create(nil);
  try
    DefA:=Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    DefA.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefB:=Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    DefB.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    M2MDef.ClassA:=TJunctionTestClassA;
    M2MDef.ClassB:=TJunctionTestClassB;
    M2MDef.JunctionTable:='ClassAClassB';
    M2MDef.ForeignKeyA:='ClassAID';
    M2MDef.ForeignKeyB:='ClassBID';
    M2MDef.DBCascade:=True;
    S:=Builder.GetCreateJunctionTableSQL(M2MDef, Reg);
    AssertTrue('Has FK for ClassA', Pos('FOREIGN KEY (ClassAID) REFERENCES JunctionTestClassA(ID) ON DELETE CASCADE', S) > 0);
    AssertTrue('Has FK for ClassB', Pos('FOREIGN KEY (ClassBID) REFERENCES JunctionTestClassB(ID) ON DELETE CASCADE', S) > 0);
  finally
    M2MDef.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestJunctionTableSQLNoCascade;
var
  Reg: TOPRegistry;
  DefA, DefB: TPersisterDef;
  M2MDef: TOPManyToManyDef;
  S: UTF8String;
begin
  Reg:=TOPRegistry.Create(nil);
  M2MDef:=TOPManyToManyDef.Create(nil);
  try
    DefA:=Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    DefA.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefB:=Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    DefB.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    M2MDef.ClassA:=TJunctionTestClassA;
    M2MDef.ClassB:=TJunctionTestClassB;
    M2MDef.JunctionTable:='ClassAClassB';
    M2MDef.ForeignKeyA:='ClassAID';
    M2MDef.ForeignKeyB:='ClassBID';
    M2MDef.DBCascade:=False;
    S:=Builder.GetCreateJunctionTableSQL(M2MDef, Reg);
    AssertTrue('No FOREIGN KEY without cascade', Pos('FOREIGN KEY', S) = 0);
    AssertTrue('No REFERENCES without cascade', Pos('REFERENCES', S) = 0);
  finally
    M2MDef.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestJunctionTableSQLStringPK;
var
  Reg: TOPRegistry;
  DefA, DefB: TPersisterDef;
  M2MDef: TOPManyToManyDef;
  S: UTF8String;
begin
  Reg:=TOPRegistry.Create(nil);
  M2MDef:=TOPManyToManyDef.Create(nil);
  try
    DefA:=Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    DefA.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefA.FieldMap.FindByFieldName('ID').PropertyKind:=tkAString;
    DefA.FieldMap.FindByFieldName('ID').MaxSize:=36;
    DefB:=Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    DefB.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefB.FieldMap.FindByFieldName('ID').PropertyKind:=tkAString;
    DefB.FieldMap.FindByFieldName('ID').MaxSize:=36;
    M2MDef.ClassA:=TJunctionTestClassA;
    M2MDef.ClassB:=TJunctionTestClassB;
    M2MDef.JunctionTable:='ClassAClassB';
    M2MDef.ForeignKeyA:='ClassAID';
    M2MDef.ForeignKeyB:='ClassBID';
    M2MDef.DBCascade:=False;
    S:=Builder.GetCreateJunctionTableSQL(M2MDef, Reg);
    AssertTrue('ClassAID is VARCHAR(36)', Pos('ClassAID VARCHAR(36) NOT NULL', S) > 0);
    AssertTrue('ClassBID is VARCHAR(36)', Pos('ClassBID VARCHAR(36) NOT NULL', S) > 0);
  finally
    M2MDef.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestJunctionTableSQLQuoted;
var
  Reg: TOPRegistry;
  DefA, DefB: TPersisterDef;
  M2MDef: TOPManyToManyDef;
  S: UTF8String;
begin
  Reg:=TOPRegistry.Create(nil);
  M2MDef:=TOPManyToManyDef.Create(nil);
  try
    DefA:=Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    DefA.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefB:=Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    DefB.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    M2MDef.ClassA:=TJunctionTestClassA;
    M2MDef.ClassB:=TJunctionTestClassB;
    M2MDef.JunctionTable:='ClassAClassB';
    M2MDef.ForeignKeyA:='ClassAID';
    M2MDef.ForeignKeyB:='ClassBID';
    M2MDef.DBCascade:=False;
    Builder.Options:=[sboQuoteIdentifiers];
    S:=Builder.GetCreateJunctionTableSQL(M2MDef, Reg);
    AssertTrue('Table name quoted', Pos('"ClassAClassB"', S) > 0);
    AssertTrue('FKA quoted', Pos('"ClassAID"', S) > 0);
    AssertTrue('FKB quoted', Pos('"ClassBID"', S) > 0);
    AssertTrue('PK constraint quoted', Pos('"ClassAClassB_PK"', S) > 0);
  finally
    M2MDef.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestJunctionTableSQLLowerCase;
var
  Reg: TOPRegistry;
  DefA, DefB: TPersisterDef;
  M2MDef: TOPManyToManyDef;
  S: UTF8String;
begin
  Reg:=TOPRegistry.Create(nil);
  M2MDef:=TOPManyToManyDef.Create(nil);
  try
    DefA:=Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    DefA.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    DefB:=Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    DefB.FieldMap.FindByFieldName('ID').Options:=[foInKey];
    M2MDef.ClassA:=TJunctionTestClassA;
    M2MDef.ClassB:=TJunctionTestClassB;
    M2MDef.JunctionTable:='ClassAClassB';
    M2MDef.ForeignKeyA:='ClassAID';
    M2MDef.ForeignKeyB:='ClassBID';
    M2MDef.DBCascade:=True;
    Builder.Options:=[sboKeywordsLowerCase];
    S:=Builder.GetCreateJunctionTableSQL(M2MDef, Reg);
    AssertTrue('create lowercase', Pos('create', S) > 0);
    AssertTrue('table lowercase', Pos('table', S) > 0);
    AssertTrue('not lowercase', Pos('not', S) > 0);
    AssertTrue('null lowercase', Pos('null', S) > 0);
    AssertTrue('constraint lowercase', Pos('constraint', S) > 0);
    AssertTrue('primary key lowercase', Pos('primary key', S) > 0);
    AssertTrue('foreign lowercase', Pos('foreign', S) > 0);
    AssertTrue('cascade lowercase', Pos('cascade', S) > 0);
  finally
    M2MDef.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestCreationOrderIndependent;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  Order: TClassArray;
begin
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  try
    RelReg.Registry:=Reg;
    Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    Reg.RegisterPersister(TDepTestClassC, TRTTIPersister);
    Order:=Builder.GetCreationOrder(Reg, RelReg);
    AssertEquals('Count', 3, Length(Order));
    AssertEquals('First', TJunctionTestClassA, Order[0]);
    AssertEquals('Second', TJunctionTestClassB, Order[1]);
    AssertEquals('Third', TDepTestClassC, Order[2]);
  finally
    RelReg.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestCreationOrderParentChild;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  Order: TClassArray;
begin
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  try
    RelReg.Registry:=Reg;
    // Register child first, parent second — order should still be parent first
    Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    Reg.Persisters[0].FieldMap.FindByFieldName('ID').Options:=[foInKey];
    Reg.Persisters[1].FieldMap.FindByFieldName('ID').Options:=[foInKey];
    RelReg.RegisterRelationship(TJunctionTestClassA, TJunctionTestClassB, 'ID', False);
    Order:=Builder.GetCreationOrder(Reg, RelReg);
    AssertEquals('Count', 2, Length(Order));
    AssertEquals('Parent first', TJunctionTestClassA, Order[0]);
    AssertEquals('Child second', TJunctionTestClassB, Order[1]);
  finally
    RelReg.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestCreationOrderDiamond;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  Order: TClassArray;
  IdxA, IdxB, IdxC, IdxD: Integer;
begin
  // D has no deps, B→D, C→D, A→B, A→C
  // Valid orders: D,B,C,A or D,C,B,A
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  try
    RelReg.Registry:=Reg;
    Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);  // 0: A (depends on B and C)
    Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);  // 1: B (depends on D)
    Reg.RegisterPersister(TDepTestClassC, TRTTIPersister);       // 2: C (depends on D)
    Reg.RegisterPersister(TDepTestClassD, TRTTIPersister);       // 3: D (no deps)
    // A→B means B is parent, A is child
    RelReg.RegisterRelationship(TJunctionTestClassB, TJunctionTestClassA, 'ID', False);
    RelReg.RegisterRelationship(TDepTestClassC, TJunctionTestClassA, 'ID', False);
    RelReg.RegisterRelationship(TDepTestClassD, TJunctionTestClassB, 'ID', False);
    RelReg.RegisterRelationship(TDepTestClassD, TDepTestClassC, 'ID', False);
    Order:=Builder.GetCreationOrder(Reg, RelReg);
    AssertEquals('Count', 4, Length(Order));
    // Find positions
    IdxA:=-1; IdxB:=-1; IdxC:=-1; IdxD:=-1;
    if Order[0]=TJunctionTestClassA then IdxA:=0
    else if Order[0]=TJunctionTestClassB then IdxB:=0
    else if Order[0]=TDepTestClassC then IdxC:=0
    else if Order[0]=TDepTestClassD then IdxD:=0;
    if Order[1]=TJunctionTestClassA then IdxA:=1
    else if Order[1]=TJunctionTestClassB then IdxB:=1
    else if Order[1]=TDepTestClassC then IdxC:=1
    else if Order[1]=TDepTestClassD then IdxD:=1;
    if Order[2]=TJunctionTestClassA then IdxA:=2
    else if Order[2]=TJunctionTestClassB then IdxB:=2
    else if Order[2]=TDepTestClassC then IdxC:=2
    else if Order[2]=TDepTestClassD then IdxD:=2;
    if Order[3]=TJunctionTestClassA then IdxA:=3
    else if Order[3]=TJunctionTestClassB then IdxB:=3
    else if Order[3]=TDepTestClassC then IdxC:=3
    else if Order[3]=TDepTestClassD then IdxD:=3;
    AssertTrue('D before B', IdxD < IdxB);
    AssertTrue('D before C', IdxD < IdxC);
    AssertTrue('B before A', IdxB < IdxA);
    AssertTrue('C before A', IdxC < IdxA);
  finally
    RelReg.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestCreationOrderCircular;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  Raised: Boolean;
begin
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  try
    RelReg.Registry:=Reg;
    Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    // A→B and B→A creates a cycle
    RelReg.RegisterRelationship(TJunctionTestClassA, TJunctionTestClassB, 'ID', False);
    RelReg.RegisterRelationship(TJunctionTestClassB, TJunctionTestClassA, 'ID', False);
    Raised:=False;
    try
      Builder.GetCreationOrder(Reg, RelReg);
    except
      on E: EOPFDDL do
        Raised:=True;
    end;
    AssertTrue('Circular dependency raises EOPFDDL', Raised);
  finally
    RelReg.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestCreationOrderSelfRef;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  Order: TClassArray;
begin
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  try
    RelReg.Registry:=Reg;
    Reg.RegisterPersister(TDepTestSelfRef, TRTTIPersister);
    Reg.Persisters[0].FieldMap.FindByFieldName('ID').Options:=[foInKey];
    // Self-referencing FK: ParentID → same class
    RelReg.RegisterRelationship(TDepTestSelfRef, TDepTestSelfRef, 'ParentID', False);
    Order:=Builder.GetCreationOrder(Reg, RelReg);
    AssertEquals('Count', 1, Length(Order));
    AssertEquals('Self-ref class', TDepTestSelfRef, Order[0]);
  finally
    RelReg.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestCreateSchemaIntegration;
var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
  Q: TSQLQuery;
  Tables: TStringList;
begin
  Conn:=TSQLite3Connection.Create(nil);
  Trans:=TSQLTransaction.Create(nil);
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  M2MReg:=TOPManyToManyRegistry.Create(nil);
  Tables:=TStringList.Create;
  try
    Conn.DatabaseName:=':memory:';
    Trans.Database:=Conn;
    Conn.Transaction:=Trans;
    Conn.Open;

    RelReg.Registry:=Reg;
    M2MReg.Registry:=Reg;
    // Register two classes with PKs
    Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    Reg.RegisterPersister(TJunctionTestClassB, TRTTIPersister);
    Reg.Persisters[0].FieldMap.FindByFieldName('ID').Options:=[foInKey];
    Reg.Persisters[1].FieldMap.FindByFieldName('ID').Options:=[foInKey];
    // Register parent-child relation
    RelReg.RegisterRelationship(TJunctionTestClassA, TJunctionTestClassB, 'ID', False);
    // Register M2M
    M2MReg.RegisterManyToMany(TJunctionTestClassA, TJunctionTestClassB,
      'AB_Junction', 'AID', 'BID', False);

    // Execute CreateSchema — skip scoForeignKeys because SQLite does not support ALTER TABLE ADD CONSTRAINT
    Builder.CreateSchema(Conn, Reg, RelReg, M2MReg, [], [scoIndexes, scoJunctionTables]);

    // Verify tables exist by querying sqlite_master
    Q:=TSQLQuery.Create(nil);
    try
      Q.Database:=Conn;
      Q.Transaction:=Trans;
      Trans.StartTransaction;
      Q.SQL.Text:='SELECT name FROM sqlite_master WHERE type=''table'' ORDER BY name';
      Q.Open;
      while not Q.EOF do
      begin
        Tables.Add(Q.FieldByName('name').AsString);
        Q.Next;
      end;
      Q.Close;
      Trans.Commit;
    finally
      Q.Free;
    end;

    AssertTrue('JunctionTestClassA table exists', Tables.IndexOf('JunctionTestClassA') >= 0);
    AssertTrue('JunctionTestClassB table exists', Tables.IndexOf('JunctionTestClassB') >= 0);
    AssertTrue('AB_Junction table exists', Tables.IndexOf('AB_Junction') >= 0);
  finally
    Tables.Free;
    M2MReg.Free;
    RelReg.Free;
    Reg.Free;
    Trans.Free;
    Conn.Free;
  end;
end;

procedure TTestDDLBuilder.TestPropertyDefSQLUnique;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkAString, 0, 100);
  F.Options := [foUnique];
  S := Builder.GetPropertyDefSQL('a', F);
  AssertEquals('UNIQUE', 'VARCHAR(100) UNIQUE', S);
end;

procedure TTestDDLBuilder.TestPropertyDefSQLRequiredUnique;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkAString, 0, 100);
  F.Options := [foRequired, foUnique];
  S := Builder.GetPropertyDefSQL('a', F);
  AssertEquals('NOT NULL UNIQUE', 'VARCHAR(100) NOT NULL UNIQUE', S);
end;

procedure TTestDDLBuilder.TestPropertyDefSQLDefaultString;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkAString, 0, 100);
  F.DefaultValue := 'hello';
  S := Builder.GetPropertyDefSQL('a', F);
  AssertEquals('DEFAULT string', 'VARCHAR(100) DEFAULT ''hello''', S);
end;

procedure TTestDDLBuilder.TestPropertyDefSQLDefaultInteger;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkInteger, 0, 0);
  F.DefaultValue := 42;
  S := Builder.GetPropertyDefSQL('a', F);
  AssertEquals('DEFAULT integer', 'INT DEFAULT 42', S);
end;

procedure TTestDDLBuilder.TestPropertyDefSQLDefaultBoolean;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkBool, 0, 0);
  F.DefaultValue := True;
  S := Builder.GetPropertyDefSQL('a', F);
  AssertTrue('Contains DEFAULT 1', Pos('DEFAULT 1', S) > 0);
end;

procedure TTestDDLBuilder.TestPropertyDefSQLDefaultFloat;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkFloat, 0, 0);
  F.DefaultValue := 3.14;
  S := Builder.GetPropertyDefSQL('a', F);
  AssertEquals('DEFAULT float', 'FLOAT DEFAULT 3.14', S);
end;

procedure TTestDDLBuilder.TestPropertyDefSQLUniqueWithDefault;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkInteger, 0, 0);
  F.Options := [foRequired, foUnique];
  F.DefaultValue := 0;
  S := Builder.GetPropertyDefSQL('a', F);
  AssertEquals('NOT NULL UNIQUE DEFAULT', 'INT NOT NULL UNIQUE DEFAULT 0', S);
end;

procedure TTestDDLBuilder.TestGetAddColumnSQL;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkAString, 0, 100);
  F.Options := [foRequired];
  S := Builder.GetAddColumnSQL('Person', F);
  AssertEquals('ADD COLUMN SQL',
    'ALTER TABLE Person ADD COLUMN F1 VARCHAR(100) NOT NULL', S);
end;

procedure TTestDDLBuilder.TestGetDropColumnSQL;
var
  S: UTF8String;
begin
  S := Builder.GetDropColumnSQL('Person', 'OldCol');
  AssertEquals('DROP COLUMN SQL',
    'ALTER TABLE Person DROP COLUMN OldCol', S);
end;

procedure TTestDDLBuilder.TestGetAlterColumnTypeSQL;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  F := Add(tkAString, 0, 200);
  S := Builder.GetAlterColumnTypeSQL('Person', 'Name', F);
  AssertEquals('ALTER COLUMN TYPE SQL',
    'ALTER TABLE Person ALTER COLUMN Name TYPE VARCHAR(200)', S);
end;

procedure TTestDDLBuilder.TestGetAlterColumnNullableSetNotNull;
var
  S: UTF8String;
begin
  S := Builder.GetAlterColumnNullableSQL('Person', 'Email', False);
  AssertEquals('SET NOT NULL SQL',
    'ALTER TABLE Person ALTER COLUMN Email SET NOT NULL', S);
end;

procedure TTestDDLBuilder.TestGetAlterColumnNullableDropNotNull;
var
  S: UTF8String;
begin
  S := Builder.GetAlterColumnNullableSQL('Person', 'Email', True);
  AssertEquals('DROP NOT NULL SQL',
    'ALTER TABLE Person ALTER COLUMN Email DROP NOT NULL', S);
end;

procedure TTestDDLBuilder.TestGetDropTableSQL;
var
  S: UTF8String;
begin
  S := Builder.GetDropTableSQL('OldTable');
  AssertEquals('DROP TABLE SQL', 'DROP TABLE OldTable', S);
end;

procedure TTestDDLBuilder.TestGetDropIndexSQL;
var
  S: UTF8String;
begin
  S := Builder.GetDropIndexSQL('idx_person_name');
  AssertEquals('DROP INDEX SQL', 'DROP INDEX idx_person_name', S);
end;

procedure TTestDDLBuilder.TestGetDropForeignKeySQL;
var
  S: UTF8String;
begin
  S := Builder.GetDropForeignKeySQL('OrderLine', 'fk_OrderLine_Film');
  AssertEquals('DROP CONSTRAINT SQL',
    'ALTER TABLE OrderLine DROP CONSTRAINT fk_OrderLine_Film', S);
end;

procedure TTestDDLBuilder.TestGenerateMigrationSQL;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
  Diffs: TOPSchemaDiffArray;
  SQLs: TUTF8StringArray;
begin
  Reg:=TOPRegistry.Create(nil);
  RelReg:=TOPRelationRegistry.Create(nil);
  M2MReg:=TOPManyToManyRegistry.Create(nil);
  try
    RelReg.Registry:=Reg;
    M2MReg.Registry:=Reg;
    Reg.RegisterPersister(TJunctionTestClassA, TRTTIPersister);
    Reg.Persisters[0].FieldMap.FindByFieldName('ID').Options:=[foInKey];

    // Create diff array with add table + drop table
    SetLength(Diffs, 2);
    Diffs[0].Kind:=sdkAddTable;
    Diffs[0].TableName:='JunctionTestClassA';
    Diffs[1].Kind:=sdkDropTable;
    Diffs[1].TableName:='ObsoleteTable';

    SQLs:=Builder.GenerateMigrationSQL(Diffs, Reg, RelReg, M2MReg);
    AssertEquals('SQL count', 2, Length(SQLs));
    AssertTrue('CREATE TABLE', Pos('CREATE TABLE', SQLs[0]) > 0);
    AssertEquals('DROP TABLE', 'DROP TABLE ObsoleteTable', SQLs[1]);
  finally
    M2MReg.Free;
    RelReg.Free;
    Reg.Free;
  end;
end;

procedure TTestDDLBuilder.TestGetAlterColumnTypeSQLMySQL;
var
  F: TOPFieldMap;
  S: UTF8String;
begin
  Builder.SQLDialect := dMySQL;
  F := Add(tkAString, 0, 200);
  S := Builder.GetAlterColumnTypeSQL('Person', 'Name', F);
  AssertTrue('MODIFY COLUMN for MySQL', Pos('MODIFY COLUMN', S) > 0);
  AssertTrue('No ALTER COLUMN for MySQL', Pos('ALTER COLUMN', S) = 0);
end;

procedure TTestDDLBuilder.TestGetDropForeignKeySQLMySQL;
var
  S: UTF8String;
begin
  Builder.SQLDialect := dMySQL;
  S := Builder.GetDropForeignKeySQL('OrderLine', 'fk_OrderLine_Film');
  AssertTrue('DROP FOREIGN KEY for MySQL', Pos('DROP FOREIGN KEY', S) > 0);
  AssertTrue('No DROP CONSTRAINT for MySQL', Pos('DROP CONSTRAINT', S) = 0);
end;

procedure TTestDDLBuilder.TestGetAlterColumnTypeSQLSQLite;
var
  F: TOPFieldMap;
  Raised: Boolean;
begin
  Builder.SQLDialect := dSQLite;
  F := Add(tkAString, 0, 200);
  Raised := False;
  try
    Builder.GetAlterColumnTypeSQL('Person', 'Name', F);
  except
    on E: EOPFDDL do
      Raised := True;
  end;
  AssertTrue('SQLite ALTER COLUMN TYPE raises EOPFDDL', Raised);
end;

procedure TTestDDLBuilder.TestGetAlterColumnNullableSQLSQLite;
var
  Raised: Boolean;
begin
  Builder.SQLDialect := dSQLite;
  Raised := False;
  try
    Builder.GetAlterColumnNullableSQL('Person', 'Email', False);
  except
    on E: EOPFDDL do
      Raised := True;
  end;
  AssertTrue('SQLite ALTER COLUMN NULLABLE raises EOPFDDL', Raised);
end;

procedure TTestDDLBuilder.TestGetAlterColumnNullableSQLMySQL;
var
  Raised: Boolean;
begin
  Builder.SQLDialect := dMySQL;
  Raised := False;
  try
    Builder.GetAlterColumnNullableSQL('Person', 'Email', False);
  except
    on E: EOPFDDL do
      Raised := True;
  end;
  AssertTrue('MySQL ALTER COLUMN NULLABLE raises EOPFDDL', Raised);
end;

initialization
  RegisterTest(TTestDDLBuilder);
end.
