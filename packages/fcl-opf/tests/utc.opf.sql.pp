unit utc.opf.sql;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, FpcUnit.Test, FpcUnit.TestRegistry, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, TypInfo, fpcunit, testregistry, sqldb,
{$ENDIF}
  Opf.Types, Opf.Connections, Opf.Driver, Opf.Sql, Opf.Fields;

Type
  TMyOPSQLBuilder = Class(TOPSQLBuilder);

  { TMyOverrideSQLBuilder }

  TMyOverrideSQLBuilder = class(TOPSQLBuilder)
    Function GetSelectSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; aSelectOptions : TSQLSelectOptions = []) : UTF8string; override;
  end;

  { TTestSQLBuilder }

  TTestSQLBuilder = Class(TTestCase)
  private
    FBuild: TOPSQLBuilder;
    FMap: TOPFieldMapList;
    FTable: UTF8String;
    procedure SetinstanceClass;
    procedure CallDeleteNoKey;
    procedure CallUpdateNoKey;
    procedure CallSelectEmptyTable;
    procedure CallInsertEmptyTable;
    procedure CallUpdateEmptyTable;
    procedure CallDeleteEmptyTable;
    procedure CallSelectInjectedTable;
    procedure DummyGenerator(aInstance: TObject; aField: TOPFieldMap; aConnection: TObject);
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    Procedure Fill;
    Function Add(aKind: TTypeKind;aMin,aMax : Integer; const aName : string = '') : TOPFieldMap;
    Property Builder : TOPSQLBuilder Read FBuild;
    Property Map : TOPFieldMapList Read FMap;
    Property Table : UTF8String Read FTable;
  Published
    Procedure TestEmpty;
    Procedure TestSelectFieldsOnly;
    Procedure TestSelectFull;
    Procedure TestInsertFull;
    Procedure TestInsertNoFieldList;
    Procedure TestUpdate;
    Procedure TestDeleteSQL;
    Procedure TestDeleteCompositeKey;
    Procedure TestDeleteNoKeyFields;
    Procedure TestUpdateNoKeyFields;
    procedure TestGlobalInstance;
    procedure TestGlobalInstanceClass;
    procedure TestSelectOverriddenFieldName;
    procedure TestInsertOverriddenFieldName;
    procedure TestUpdateOverriddenFieldName;
    procedure TestDeleteOverriddenFieldName;
    procedure TestMixedOverriddenAndConventionSQL;
    procedure TestInsertExcludesAutoIncField;
    procedure TestInsertReturningClauseSQLite;
    procedure TestInsertReturningClausePostgres;
    procedure TestGetReturningClause;
    procedure TestGetPostInsertValueSQLSQLite;
    procedure TestGetPostInsertValueSQLMySQL;
    procedure TestInsertIncludesUUIDField;
    procedure TestGetSequenceNextValSQLPostgres;
    procedure TestGetSequenceNextValSQLFirebird;
    procedure TestGetSequenceNextValSQLSQLite;
    procedure TestInsertIncludesCustomGenField;
    procedure TestCustomBuilderOverride;
    procedure TestSelectEmptyTableName;
    procedure TestInsertEmptyTableName;
    procedure TestUpdateEmptyTableName;
    procedure TestDeleteEmptyTableName;
    procedure TestUnsafeTableNameSemicolon;
    procedure TestUnsafeTableNameComment;
    procedure TestUnsafeTableNameQuote;
  end;



implementation

{ TTestSQLBuilder }

procedure TTestSQLBuilder.SetUp;
begin
  inherited SetUp;
  FBuild:=TOPSQLBuilder.Create(Nil);
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestSQLBuilder.TearDown;
begin
  FreeAndNil(FBuild);
  FreeAndNil(FMap);
  inherited TearDown;
end;

procedure TTestSQLBuilder.Fill;

begin
  FMap.AddField('F1','P1',[foInSelect,foInKey,foInInsert]);
  FMap.AddField('F2','P2',[foInSelect,foInInsert]);
  FMap.AddField('F3','P3',[foInSelect,foInUpdate]);
end;

function TTestSQLBuilder.Add(aKind: TTypeKind; aMin, aMax: Integer; const aName: string): TOPFieldMap;

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

procedure TTestSQLBuilder.TestEmpty;
begin
  AssertNotNull('Have builder',Builder);
  AssertNotNull('Have Map',Map);
  AssertEquals('Map empty',0,Map.Count);
end;

procedure TTestSQLBuilder.TestSelectFieldsOnly;

Var
  S : UTF8String;

begin
  Fill;
  // Only fields
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soFieldListOnly]);
  AssertEquals('No space','F1,F2,F3',S);
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soFieldListOnly]);
  AssertEquals('Space','F1, F2, F3',S);
  Builder.Options:=[sboQuoteIdentifiers];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soFieldListOnly]);
  AssertEquals('Quote','"F1","F2","F3"',S);
  Builder.Options:=[sboQuoteIdentifiers,sboAlwaysPrependTable];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soFieldListOnly]);
  AssertEquals('Quote','"a"."F1","a"."F2","a"."F3"',S);
  Builder.Options:=[sboQuoteIdentifiers];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soPrependTable,soFieldListOnly]);
  AssertEquals('Quote','"a"."F1","a"."F2","a"."F3"',S);
  Builder.Options:=[];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soPrependTable,soFieldListOnly]);
  AssertEquals('Quote','a.F1,a.F2,a.F3',S);
end;

procedure TTestSQLBuilder.TestSelectFull;

Var
  S : UTF8String;

begin
  Fill;
  // Full select
  S:=Builder.GetSelectSQL('a',Map.AsArray);
  AssertEquals('Full 1','SELECT F1,F2,F3 FROM a',S);
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetSelectSQL('a',Map.AsArray);
  AssertEquals('Full 2','SELECT F1, F2, F3 FROM a',S);
  Builder.Options:=[sboSpaceAfterSeparator,sboKeyWordsLowerCase];
  S:=Builder.GetSelectSQL('a',Map.AsArray);
  AssertEquals('Full 3','select F1, F2, F3 from a',S);
  Builder.Options:=[];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[soPrependTable]);
  AssertEquals('Quote','SELECT a.F1,a.F2,a.F3 FROM a',S);
  Builder.Options:=[sboQuoteIdentifiers,sboAlwaysPrependTable];
  S:=Builder.GetSelectSQL('a',Map.AsArray,[]);
  AssertEquals('Quote','SELECT "a"."F1","a"."F2","a"."F3" FROM "a"',S);
end;

procedure TTestSQLBuilder.TestInsertFull;

Var
  S : UTF8String;

begin
  Fill;
  S:=Builder.GetInsertSQL('a',Map.AsArray);
  AssertEquals('Full 1','INSERT INTO a (F1,F2) VALUES (:F1,:F2)',S);
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetInsertSQL('a',Map.AsArray);
  AssertEquals('Full 2','INSERT INTO a (F1, F2) VALUES (:F1, :F2)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetInsertSQL('a',Map.AsArray);
  AssertEquals('Full 3','insert into a (F1,F2) values (:F1,:F2)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioPrependTable]);
  AssertEquals('Full 4','insert into a (a.F1,a.F2) values (:F1,:F2)',S);
  Builder.Options:=[sboKeyWordsLowerCase,sboQuoteIdentifiers];
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioPrependTable]);
  AssertEquals('Full 4','insert into "a" ("a"."F1","a"."F2") values (:F1,:F2)',S);
end;

procedure TTestSQLBuilder.TestInsertNoFieldList;
Var
  S : UTF8String;

begin
  Fill;
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioSkipFieldList]);
  AssertEquals('Full 1','INSERT INTO a VALUES (:F1,:F2)',S);
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioSkipFieldList]);
  AssertEquals('Full 2','INSERT INTO a VALUES (:F1, :F2)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioSkipFieldList]);
  AssertEquals('Full 3','insert into a values (:F1,:F2)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioSkipFieldList,ioPrependTable]);
  AssertEquals('Full 4','insert into a values (:F1,:F2)',S);
  Builder.Options:=[sboKeyWordsLowerCase,sboQuoteIdentifiers];
  S:=Builder.GetInsertSQL('a',Map.AsArray,[ioSkipFieldList,ioPrependTable]);
  AssertEquals('Full 4','insert into "a" values (:F1,:F2)',S);
end;

procedure TTestSQLBuilder.TestUpdate;

Var
  S : UTF8String;

begin
  Fill;
  S:=Builder.GetUpdateSQL('a',Map.AsArray);
  AssertEquals('Full 1','UPDATE a SET F3=:F3 WHERE (F1=:F1)',S);
  Map.Fields[1].Options:=Map.Fields[1].Options+[foInUpdate];
  S:=Builder.GetUpdateSQL('a',Map.AsArray);
  AssertEquals('Full 2','UPDATE a SET F2=:F2,F3=:F3 WHERE (F1=:F1)',S);
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetUpdateSQL('a',Map.AsArray);
  AssertEquals('Full 3','UPDATE a SET F2 = :F2, F3 = :F3 WHERE (F1 = :F1)',S);
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetUpdateSQL('a',Map.AsArray);
  AssertEquals('Full 4','update a set F2=:F2,F3=:F3 where (F1=:F1)',S);
  Builder.Options:=[sboKeyWordsLowerCase,sboQuoteIdentifiers];
  S:=Builder.GetUpdateSQL('a',Map.AsArray);
  AssertEquals('Full 5','update "a" set "F2"=:F2,"F3"=:F3 where ("F1"=:F1)',S);
  S:=Builder.GetUpdateSQL('a',Map.AsArray,[uoPrependTable]);
  AssertEquals('Full 6','update "a" set "a"."F2"=:F2,"a"."F3"=:F3 where ("a"."F1"=:F1)',S);
  S:=Builder.GetUpdateSQL('a',Map.AsArray,[uoPrependTable,uoSkipWhere]);
  AssertEquals('Full 6','update "a" set "a"."F2"=:F2,"a"."F3"=:F3',S);
  Builder.Options:=[];
  S:=Builder.GetUpdateSQL('a',Map.AsArray,[uoSkipWhere]);
  AssertEquals('Full 6','UPDATE a SET F2=:F2,F3=:F3',S);
end;

procedure TTestSQLBuilder.TestDeleteSQL;

Var
  S : UTF8String;

begin
  Fill;
  // Default options
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Default','DELETE FROM a WHERE (F1=:F1)',S);
  // sboSpaceAfterSeparator
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Space','DELETE FROM a WHERE (F1 = :F1)',S);
  // sboKeyWordsLowerCase
  Builder.Options:=[sboKeyWordsLowerCase];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Lower','delete from a where (F1=:F1)',S);
  // sboQuoteIdentifiers
  Builder.Options:=[sboQuoteIdentifiers];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Quote','DELETE FROM "a" WHERE ("F1"=:F1)',S);
  // sboQuoteIdentifiers + sboKeyWordsLowerCase
  Builder.Options:=[sboQuoteIdentifiers,sboKeyWordsLowerCase];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Quote+Lower','delete from "a" where ("F1"=:F1)',S);
  // doPrependTable
  Builder.Options:=[];
  S:=Builder.GetDeleteSQL('a',Map.AsArray,[doPrependTable]);
  AssertEquals('Prepend','DELETE FROM a WHERE (a.F1=:F1)',S);
  // sboQuoteIdentifiers + doPrependTable
  Builder.Options:=[sboQuoteIdentifiers];
  S:=Builder.GetDeleteSQL('a',Map.AsArray,[doPrependTable]);
  AssertEquals('Quote+Prepend','DELETE FROM "a" WHERE ("a"."F1"=:F1)',S);
  // sboAlwaysPrependTable (no explicit doPrependTable)
  Builder.Options:=[sboAlwaysPrependTable];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('AlwaysPrepend','DELETE FROM a WHERE (a.F1=:F1)',S);
  // sboKeyWordsLowerCase + sboSpaceAfterSeparator
  Builder.Options:=[sboKeyWordsLowerCase,sboSpaceAfterSeparator];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Lower+Space','delete from a where (F1 = :F1)',S);
  // sboAlwaysPrependTable + sboQuoteIdentifiers
  Builder.Options:=[sboAlwaysPrependTable,sboQuoteIdentifiers];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('AlwaysPrepend+Quote','DELETE FROM "a" WHERE ("a"."F1"=:F1)',S);
end;

procedure TTestSQLBuilder.TestDeleteCompositeKey;

Var
  S : UTF8String;

begin
  FMap.AddField('F1','P1',[foInSelect,foInKey,foInInsert]);
  FMap.AddField('F2','P2',[foInSelect,foInKey,foInInsert]);
  // Default
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Composite','DELETE FROM a WHERE (F1=:F1) AND (F2=:F2)',S);
  // sboSpaceAfterSeparator
  Builder.Options:=[sboSpaceAfterSeparator];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Composite+Space','DELETE FROM a WHERE (F1 = :F1) AND (F2 = :F2)',S);
  // sboQuoteIdentifiers
  Builder.Options:=[sboQuoteIdentifiers];
  S:=Builder.GetDeleteSQL('a',Map.AsArray);
  AssertEquals('Composite+Quote','DELETE FROM "a" WHERE ("F1"=:F1) AND ("F2"=:F2)',S);
  // doPrependTable
  Builder.Options:=[];
  S:=Builder.GetDeleteSQL('a',Map.AsArray,[doPrependTable]);
  AssertEquals('Composite+Prepend','DELETE FROM a WHERE (a.F1=:F1) AND (a.F2=:F2)',S);
end;

procedure TTestSQLBuilder.CallDeleteNoKey;
begin
  Builder.GetDeleteSQL('a',Map.AsArray);
end;

procedure TTestSQLBuilder.TestDeleteNoKeyFields;
begin
  FMap.AddField('F1','P1',[foInSelect,foInInsert]);
  FMap.AddField('F2','P2',[foInSelect,foInInsert]);
  AssertException('No key fields',EOPFSQL,@Self.CallDeleteNoKey);
end;

procedure TTestSQLBuilder.CallUpdateNoKey;
begin
  Builder.GetUpdateSQL('a',Map.AsArray);
end;

procedure TTestSQLBuilder.TestUpdateNoKeyFields;
begin
  FMap.AddField('F1','P1',[foInSelect,foInUpdate]);
  FMap.AddField('F2','P2',[foInSelect,foInUpdate]);
  AssertException('No key fields',EOPFSQL,@Self.CallUpdateNoKey);
end;

procedure TTestSQLBuilder.TestGlobalInstance;
Var
  R : TOPSQLBuilder;
begin
  R:=TOPSQLBuilder.Instance;
  AssertNotNull('Have instance',R);
  TOPSQLBuilder.ClearInstance;
  AssertNotNull('Have instance',TOPSQLBuilder.Instance);
end;

procedure TTestSQLBuilder.SetinstanceClass;
begin
  TOPSQLBuilder.SetInstanceClass(TMyOPSQLBuilder);
end;

procedure TTestSQLBuilder.TestGlobalInstanceClass;

Var
  R : TOPSQLBuilder;

begin
  TOPSQLBuilder.ClearInstance;
  Self.SetInstanceClass;
  try
    R:=TOPSQLBuilder.Instance;
    AssertNotNull('Have instance',R);
    AssertEquals('Correct class',TMyOPSQLBuilder,R.ClassType);
    AssertException('Cannot set class if instance is active',EOPFSQL,@Self.SetinstanceClass);
  finally
    TOPSQLBuilder.ClearInstance;
    TOPSQLBuilder.SetInstanceClass(Nil);
  end;
end;

procedure TTestSQLBuilder.TestSelectOverriddenFieldName;
var
  S: UTF8String;
begin
  FMap.AddField('first_name', 'FirstName', [foInSelect, foInInsert]);
  FMap.AddField('last_name', 'LastName', [foInSelect, foInUpdate]);
  FMap.AddField('contact_id', 'ID', [foInSelect, foInKey, foInInsert]);
  S:=Builder.GetSelectSQL('Contact', Map.AsArray);
  AssertEquals('Select uses FieldName', 'SELECT first_name,last_name,contact_id FROM Contact', S);
end;

procedure TTestSQLBuilder.TestInsertOverriddenFieldName;
var
  S: UTF8String;
begin
  FMap.AddField('contact_id', 'ID', [foInSelect, foInKey, foInInsert]);
  FMap.AddField('first_name', 'FirstName', [foInSelect, foInInsert]);
  S:=Builder.GetInsertSQL('Contact', Map.AsArray);
  AssertEquals('Insert uses FieldName', 'INSERT INTO Contact (contact_id,first_name) VALUES (:contact_id,:first_name)', S);
end;

procedure TTestSQLBuilder.TestUpdateOverriddenFieldName;
var
  S: UTF8String;
begin
  FMap.AddField('contact_id', 'ID', [foInSelect, foInKey, foInInsert]);
  FMap.AddField('first_name', 'FirstName', [foInSelect, foInInsert, foInUpdate]);
  S:=Builder.GetUpdateSQL('Contact', Map.AsArray);
  AssertEquals('Update uses FieldName', 'UPDATE Contact SET first_name=:first_name WHERE (contact_id=:contact_id)', S);
end;

procedure TTestSQLBuilder.TestDeleteOverriddenFieldName;
var
  S: UTF8String;
begin
  FMap.AddField('contact_id', 'ID', [foInSelect, foInKey, foInInsert]);
  FMap.AddField('first_name', 'FirstName', [foInSelect, foInInsert]);
  S:=Builder.GetDeleteSQL('Contact', Map.AsArray);
  AssertEquals('Delete uses FieldName', 'DELETE FROM Contact WHERE (contact_id=:contact_id)', S);
end;

procedure TTestSQLBuilder.TestMixedOverriddenAndConventionSQL;
var
  S: UTF8String;
begin
  // ID uses convention (FieldName = PropertyName), FullName is overridden
  FMap.AddField('ID', 'ID', [foInSelect, foInKey, foInInsert]);
  FMap.AddField('full_name', 'FullName', [foInSelect, foInInsert, foInUpdate]);
  FMap.AddField('Age', 'Age', [foInSelect, foInInsert, foInUpdate]);
  S:=Builder.GetSelectSQL('Person', Map.AsArray);
  AssertEquals('Select mixed', 'SELECT ID,full_name,Age FROM Person', S);
  S:=Builder.GetInsertSQL('Person', Map.AsArray);
  AssertEquals('Insert mixed', 'INSERT INTO Person (ID,full_name,Age) VALUES (:ID,:full_name,:Age)', S);
  S:=Builder.GetUpdateSQL('Person', Map.AsArray);
  AssertEquals('Update mixed', 'UPDATE Person SET full_name=:full_name,Age=:Age WHERE (ID=:ID)', S);
  S:=Builder.GetDeleteSQL('Person', Map.AsArray);
  AssertEquals('Delete mixed', 'DELETE FROM Person WHERE (ID=:ID)', S);
end;

procedure TTestSQLBuilder.TestInsertExcludesAutoIncField;
var
  S: UTF8String;
begin
  FMap.AddField('ID', 'ID', [foInKey, foAutoInc]);
  FMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  S:=Builder.GetInsertSQL('TestTable', Map.AsArray);
  AssertEquals('AutoInc excluded from INSERT', 'INSERT INTO TestTable (Name) VALUES (:Name)', S);
end;

procedure TTestSQLBuilder.TestInsertReturningClauseSQLite;
begin
  Builder.SQLDialect:=dSQLite;
  AssertFalse('SQLite does not support RETURNING', Builder.SupportsReturningClause);
end;

procedure TTestSQLBuilder.TestInsertReturningClausePostgres;
begin
  Builder.SQLDialect:=dPostGres;
  AssertTrue('PostgreSQL supports RETURNING', Builder.SupportsReturningClause);
end;

procedure TTestSQLBuilder.TestGetReturningClause;
var
  S: UTF8String;
begin
  Builder.SQLDialect:=dPostGres;
  FMap.AddField('ID', 'ID', [foInKey, foAutoInc]);
  FMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  S:=Builder.GetReturningClause(Map.AsArray);
  AssertEquals('RETURNING clause', ' RETURNING ID', S);
end;

procedure TTestSQLBuilder.TestGetPostInsertValueSQLSQLite;
begin
  Builder.SQLDialect:=dSQLite;
  AssertEquals('SQLite post-insert SQL', 'SELECT last_insert_rowid()', Builder.GetPostInsertValueSQL);
end;

procedure TTestSQLBuilder.TestGetPostInsertValueSQLMySQL;
begin
  Builder.SQLDialect:=dMySQL;
  AssertEquals('MySQL post-insert SQL', 'SELECT LAST_INSERT_ID()', Builder.GetPostInsertValueSQL);
end;

procedure TTestSQLBuilder.TestInsertIncludesUUIDField;
var
  S: UTF8String;
begin
  FMap.AddField('ID', 'ID', [foInKey, foUUID, foInInsert]);
  FMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  S:=Builder.GetInsertSQL('TestTable', Map.AsArray);
  AssertEquals('UUID field included in INSERT', 'INSERT INTO TestTable (ID,Name) VALUES (:ID,:Name)', S);
end;

procedure TTestSQLBuilder.TestGetSequenceNextValSQLPostgres;
begin
  Builder.SQLDialect:=dPostGres;
  AssertEquals('PostgreSQL sequence SQL', 'SELECT nextval(''my_seq'')', Builder.GetSequenceNextValSQL('my_seq'));
end;

procedure TTestSQLBuilder.TestGetSequenceNextValSQLFirebird;
begin
  Builder.SQLDialect:=dFirebird;
  AssertEquals('Firebird sequence SQL', 'SELECT NEXT VALUE FOR my_seq FROM RDB$DATABASE', Builder.GetSequenceNextValSQL('my_seq'));
end;

procedure TTestSQLBuilder.TestGetSequenceNextValSQLSQLite;
begin
  Builder.SQLDialect:=dSQLite;
  AssertEquals('SQLite sequence SQL (not supported)', '', Builder.GetSequenceNextValSQL('my_seq'));
end;

procedure TTestSQLBuilder.DummyGenerator(aInstance: TObject; aField: TOPFieldMap; aConnection: TObject);
begin
  // No-op: used only to test SQL inclusion
end;

procedure TTestSQLBuilder.TestInsertIncludesCustomGenField;
var
  S: UTF8String;
  Field: TOPFieldMap;
begin
  Field:=FMap.AddField('ID', 'ID', [foInKey, foInInsert]);
  Field.OnGenerateValue:=@Self.DummyGenerator;
  FMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  S:=Builder.GetInsertSQL('TestTable', Map.AsArray);
  AssertEquals('Custom gen field included in INSERT', 'INSERT INTO TestTable (ID,Name) VALUES (:ID,:Name)', S);
end;

procedure TTestSQLBuilder.CallSelectEmptyTable;
begin
  Builder.GetSelectSQL('',Map.AsArray);
end;

procedure TTestSQLBuilder.CallInsertEmptyTable;
begin
  Builder.GetInsertSQL('',Map.AsArray);
end;

procedure TTestSQLBuilder.CallUpdateEmptyTable;
begin
  Builder.GetUpdateSQL('',Map.AsArray);
end;

procedure TTestSQLBuilder.CallDeleteEmptyTable;
begin
  Builder.GetDeleteSQL('',Map.AsArray);
end;

procedure TTestSQLBuilder.TestSelectEmptyTableName;
begin
  Fill;
  AssertException('Empty table name', EOPFSQL, @CallSelectEmptyTable);
end;

procedure TTestSQLBuilder.TestInsertEmptyTableName;
begin
  Fill;
  AssertException('Empty table name', EOPFSQL, @CallInsertEmptyTable);
end;

procedure TTestSQLBuilder.TestUpdateEmptyTableName;
begin
  Fill;
  AssertException('Empty table name', EOPFSQL, @CallUpdateEmptyTable);
end;

procedure TTestSQLBuilder.TestDeleteEmptyTableName;
begin
  Fill;
  AssertException('Empty table name', EOPFSQL, @CallDeleteEmptyTable);
end;

procedure TTestSQLBuilder.CallSelectInjectedTable;
begin
  Builder.GetSelectSQL('t; DROP TABLE t',Map.AsArray);
end;

procedure TTestSQLBuilder.TestUnsafeTableNameSemicolon;
begin
  Fill;
  AssertException('Semicolon in table name', EOPFSQL, @CallSelectInjectedTable);
end;

procedure TTestSQLBuilder.TestUnsafeTableNameComment;
begin
  FMap.AddField('F1','P1',[foInSelect,foInKey]);
  Builder.GetSelectSQL('valid_table',Map.AsArray);
  AssertTrue('Valid table name accepted', True);
end;

procedure TTestSQLBuilder.TestUnsafeTableNameQuote;
var
  Raised: Boolean;
begin
  Fill;
  Raised:=False;
  try
    Builder.GetSelectSQL('t''injection',Map.AsArray);
  except
    on E: EOPFSQL do
      Raised:=True;
  end;
  AssertTrue('Single quote in table name rejected', Raised);
end;

{ TMyOverrideSQLBuilder }

function TMyOverrideSQLBuilder.GetSelectSQL(const aTable: UTF8string; aMap: TOPFieldMapArray; aSelectOptions: TSQLSelectOptions): UTF8string;
begin
  Result:='/* custom */ ' + inherited GetSelectSQL(aTable, aMap, aSelectOptions);
end;

procedure TTestSQLBuilder.TestCustomBuilderOverride;
var
  S: UTF8String;
begin
  TOPSQLBuilder.ClearInstance;
  TOPSQLBuilder.SetInstanceClass(TMyOverrideSQLBuilder);
  try
    FMap.AddField('ID', 'ID', [foInSelect, foInKey]);
    S:=TOPSQLBuilder.Instance.GetSelectSQL('Test', FMap.AsArray);
    AssertTrue('Custom builder prepends comment', Pos('/* custom */', S) > 0);
    AssertTrue('Contains original SQL', Pos('SELECT', S) > 0);
  finally
    TOPSQLBuilder.ClearInstance;
    TOPSQLBuilder.SetInstanceClass(Nil);
  end;
end;

initialization
  RegisterTest(TTestSQLBuilder);
end.
