unit utc.opf.driver;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry, sqldb,
{$ENDIF}
  Opf.Driver, Opf.Fields, Opf.Schema;

Type

  { TMyCustomDriver — test custom driver for registration }

  TMyCustomDriver = Class(TSQLDriverMySQL)
  public
    class function GetReservedWords: TUTF8StringArray; override;
  end;

  { TTestSQLDriver }

  TTestSQLDriver = Class(TTestCase)
  Protected
    Procedure TearDown; override;
  Published
    // Factory tests
    procedure TestCreateDriverVanilla;
    procedure TestCreateDriverSQLite;
    procedure TestCreateDriverMySQL;
    procedure TestCreateDriverPostgreSQL;
    procedure TestCreateDriverFirebird;
    procedure TestCreateDriverFirebird3;
    procedure TestCreateDriverSQLServer;
    procedure TestCreateDriverOracle;
    // Registration tests
    procedure TestRegisterCustomDriver;
    procedure TestRegisterDriverOverridesDefault;
    // SQL builder delegation tests
    procedure TestSupportsReturningClausePostgres;
    procedure TestSupportsReturningClauseFirebird;
    procedure TestSupportsReturningClauseSQLite;
    procedure TestGetPostInsertValueSQLSQLite;
    procedure TestGetPostInsertValueSQLMySQL;
    procedure TestGetPostInsertValueSQLSQLServer;
    procedure TestGetPostInsertValueSQLVanilla;
    procedure TestGetSequenceNextValSQLPostgres;
    procedure TestGetSequenceNextValSQLFirebird;
    procedure TestGetSequenceNextValSQLOracle;
    procedure TestGetSequenceNextValSQLSQLite;
    // DDL delegation tests
    procedure TestGetBooleanKeywordVanilla;
    procedure TestGetBooleanKeywordPostgres;
    procedure TestGetBooleanKeywordSQLServer;
    procedure TestGetBooleanKeywordFirebird3;
    procedure TestGetBlobKeywordPostgres;
    procedure TestGetBlobKeywordDefault;
    procedure TestGetMemoKeywordPostgres;
    procedure TestGetMemoKeywordSQLServer;
    procedure TestGetMemoKeywordOracle;
    procedure TestGetMemoKeywordFirebird;
    procedure TestGetMemoSuffixFirebird;
    procedure TestGetMemoSuffixDefault;
    procedure TestNeedsFloatOverDecimalVanilla;
    procedure TestNeedsFloatOverDecimalPostgres;
    procedure TestSupportsIfNotExistsVanilla;
    procedure TestSupportsIfNotExistsSQLite;
    procedure TestSupportsIfNotExistsFirebird;
    procedure TestSupportsIfNotExistsFirebird3;
    procedure TestSupportsIfNotExistsSQLServer;
    // Firebird 2.5 can be introspected (migrated) like Firebird 3
    procedure TestFirebirdIntrospectSchema;
    // Quoting tests
    procedure TestQuoteIdentifierDefault;
    procedure TestQuoteIdentifierMySQL;
    procedure TestQuoteIdentifierSQLServer;
    procedure TestQuoteIdentifierEscapes;
    // Reserved word strategy tests
    procedure TestTransformIdentifierNone;
    procedure TestTransformIdentifierAppendChar;
    procedure TestTransformIdentifierAppendCharNotReserved;
    procedure TestTransformIdentifierQuoteOnDemand;
    procedure TestTransformIdentifierQuoteOnDemandNotReserved;
    procedure TestTransformIdentifierAlwaysQuote;
    // Reserved word lists
    procedure TestSQLiteHasReservedWords;
    procedure TestMySQLHasReservedWords;
    procedure TestPostgreSQLHasReservedWords;
    procedure TestFirebirdHasReservedWords;
    procedure TestSQLServerHasReservedWords;
    procedure TestOracleHasReservedWords;
    procedure TestVanillaHasReservedWords;
    procedure TestIsReservedWordCaseInsensitive;
    // Default driver base
    procedure TestBaseDriverDefaults;
    // Dialect property
    procedure TestDialectProperty;
  end;

implementation

{ TMyCustomDriver }

class function TMyCustomDriver.GetReservedWords: TUTF8StringArray;
begin
  Result:=TUTF8StringArray.Create('CUSTOM','RESERVED');
end;

{ TTestSQLDriver }

procedure TTestSQLDriver.TearDown;
begin
  // Clear any registered custom drivers
  TSQLDriver.RegisterDriver(dMySQL, Nil);
  inherited TearDown;
end;

// Factory tests

procedure TTestSQLDriver.TestCreateDriverVanilla;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dVanilla);
  try
    AssertNotNull('Driver created', D);
    AssertTrue('Correct class', D is TSQLDriverVanilla);
    AssertEquals('Correct dialect', Ord(dVanilla), Ord(D.Dialect));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverSQLite;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertTrue('Correct class', D is TSQLDriverSQLite);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverMySQL;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertTrue('Correct class', D is TSQLDriverMySQL);
    AssertEquals('Backtick quoting', '`', D.QuoteChar);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverPostgreSQL;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertTrue('Correct class', D is TSQLDriverPostgreSQL);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverFirebird;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird);
  try
    AssertTrue('Correct class', D is TSQLDriverFirebird);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverFirebird3;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird3);
  try
    AssertTrue('Correct class', D is TSQLDriverFirebird3);
    AssertTrue('Inherits from Firebird', D is TSQLDriverFirebird);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverSQLServer;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertTrue('Correct class', D is TSQLDriverSQLServer);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestCreateDriverOracle;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dOracle);
  try
    AssertTrue('Correct class', D is TSQLDriverOracle);
  finally
    D.Free;
  end;
end;

// Registration tests

procedure TTestSQLDriver.TestRegisterCustomDriver;
var D: TSQLDriver;
begin
  TSQLDriver.RegisterDriver(dMySQL, TMyCustomDriver);
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertTrue('Custom driver returned', D is TMyCustomDriver);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestRegisterDriverOverridesDefault;
var D: TSQLDriver;
begin
  // First verify default
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertTrue('Default is TSQLDriverMySQL', D.ClassType = TSQLDriverMySQL);
  finally
    D.Free;
  end;
  // Register and verify override
  TSQLDriver.RegisterDriver(dMySQL, TMyCustomDriver);
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertTrue('Override is TMyCustomDriver', D is TMyCustomDriver);
  finally
    D.Free;
  end;
  // Unregister and verify default restored
  TSQLDriver.RegisterDriver(dMySQL, Nil);
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertTrue('Unregistered back to default', D.ClassType = TSQLDriverMySQL);
  finally
    D.Free;
  end;
end;

// SQL builder delegation tests

procedure TTestSQLDriver.TestSupportsReturningClausePostgres;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertTrue('PostgreSQL supports RETURNING', D.SupportsReturningClause);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsReturningClauseFirebird;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird);
  try
    AssertTrue('Firebird supports RETURNING', D.SupportsReturningClause);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsReturningClauseSQLite;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertFalse('SQLite does not support RETURNING', D.SupportsReturningClause);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetPostInsertValueSQLSQLite;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertEquals('SQLite post-insert', 'SELECT last_insert_rowid()', D.GetPostInsertValueSQL);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetPostInsertValueSQLMySQL;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertEquals('MySQL post-insert', 'SELECT LAST_INSERT_ID()', D.GetPostInsertValueSQL);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetPostInsertValueSQLSQLServer;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertEquals('SQL Server post-insert', 'SELECT SCOPE_IDENTITY()', D.GetPostInsertValueSQL);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetPostInsertValueSQLVanilla;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dVanilla);
  try
    AssertEquals('Vanilla post-insert empty', '', D.GetPostInsertValueSQL);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetSequenceNextValSQLPostgres;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('PostgreSQL sequence', 'SELECT nextval(''my_seq'')', D.GetSequenceNextValSQL('my_seq'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetSequenceNextValSQLFirebird;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird);
  try
    AssertEquals('Firebird sequence', 'SELECT NEXT VALUE FOR my_seq FROM RDB$DATABASE', D.GetSequenceNextValSQL('my_seq'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetSequenceNextValSQLOracle;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dOracle);
  try
    AssertEquals('Oracle sequence', 'SELECT my_seq.NEXTVAL FROM DUAL', D.GetSequenceNextValSQL('my_seq'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetSequenceNextValSQLSQLite;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertEquals('SQLite sequence empty', '', D.GetSequenceNextValSQL('my_seq'));
  finally
    D.Free;
  end;
end;

// DDL delegation tests

procedure TTestSQLDriver.TestGetBooleanKeywordVanilla;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dVanilla);
  try
    AssertEquals('Vanilla boolean', Ord(kwSmallInt), Ord(D.GetBooleanKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetBooleanKeywordPostgres;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('PgSQL boolean', Ord(kwBoolean), Ord(D.GetBooleanKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetBooleanKeywordSQLServer;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertEquals('SQL Server boolean', Ord(kwBit), Ord(D.GetBooleanKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetBooleanKeywordFirebird3;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird3);
  try
    AssertEquals('FB3 boolean', Ord(kwBoolean), Ord(D.GetBooleanKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetBlobKeywordPostgres;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('PgSQL blob', Ord(kwByteA), Ord(D.GetBlobKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetBlobKeywordDefault;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertEquals('SQLite blob', Ord(kwBlob), Ord(D.GetBlobKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetMemoKeywordPostgres;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('PgSQL memo', Ord(kwText), Ord(D.GetMemoKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetMemoKeywordSQLServer;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertEquals('SQL Server memo', Ord(kwMemo), Ord(D.GetMemoKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetMemoKeywordOracle;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dOracle);
  try
    AssertEquals('Oracle memo', Ord(kwClob), Ord(D.GetMemoKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetMemoKeywordFirebird;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird);
  try
    AssertEquals('Firebird memo', Ord(kwBlob), Ord(D.GetMemoKeyword));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetMemoSuffixFirebird;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird);
  try
    AssertEquals('Firebird memo suffix', ' SUB_TYPE 1', D.GetMemoSuffix);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestGetMemoSuffixDefault;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('Default memo suffix empty', '', D.GetMemoSuffix);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestNeedsFloatOverDecimalVanilla;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dVanilla);
  try
    AssertTrue('Vanilla needs float', D.NeedsFloatOverDecimal);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestNeedsFloatOverDecimalPostgres;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertFalse('PgSQL does not need float', D.NeedsFloatOverDecimal);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsIfNotExistsVanilla;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dVanilla);
  try
    AssertTrue('Vanilla supports IF NOT EXISTS', D.SupportsIfNotExists);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsIfNotExistsSQLite;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertTrue('SQLite supports IF NOT EXISTS', D.SupportsIfNotExists);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsIfNotExistsFirebird;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird);
  try
    AssertFalse('Firebird does not support IF NOT EXISTS', D.SupportsIfNotExists);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsIfNotExistsFirebird3;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dFirebird3);
  try
    // CREATE TABLE IF NOT EXISTS arrived in Firebird 6
    AssertFalse('Firebird3 does not support IF NOT EXISTS', D.SupportsIfNotExists);
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestFirebirdIntrospectSchema;
type
  TIntrospect = function(aConnection: TSQLConnection): TOPSchemaInfo of object;
var
  D25, D3, DV: TSQLDriver;
  M25, M3, MV: TIntrospect;
begin
  D25:=TSQLDriver.CreateDriver(dFirebird);
  D3:=TSQLDriver.CreateDriver(dFirebird3);
  DV:=TSQLDriver.CreateDriver(dVanilla);
  try
    M25:=@D25.IntrospectSchema;
    M3:=@D3.IntrospectSchema;
    MV:=@DV.IntrospectSchema;
    // The base class raises "not supported"
    AssertTrue('Firebird 2.5 has an introspection', TMethod(M25).Code<>TMethod(MV).Code);
    AssertTrue('The one of Firebird 3', TMethod(M25).Code=TMethod(M3).Code);
  finally
    DV.Free;
    D3.Free;
    D25.Free;
  end;
end;

procedure TTestSQLDriver.TestSupportsIfNotExistsSQLServer;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertFalse('SQL Server does not support IF NOT EXISTS', D.SupportsIfNotExists);
  finally
    D.Free;
  end;
end;

// Quoting tests

procedure TTestSQLDriver.TestQuoteIdentifierDefault;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('Default quoting', '"MyTable"', D.QuoteIdentifier('MyTable'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestQuoteIdentifierMySQL;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dMySQL);
  try
    AssertEquals('MySQL backtick quoting', '`MyTable`', D.QuoteIdentifier('MyTable'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestQuoteIdentifierSQLServer;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertEquals('SQL Server bracket quoting', '[MyTable]', D.QuoteIdentifier('MyTable'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestQuoteIdentifierEscapes;
var D: TSQLDriver;
begin
  // Test escaping embedded quote char
  D:=TSQLDriver.CreateDriver(dPostGres);
  try
    AssertEquals('Escape double quote', '"My""Table"', D.QuoteIdentifier('My"Table'));
  finally
    D.Free;
  end;
  // Test SQL Server bracket escaping
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    AssertEquals('Escape bracket', '[My]]Table]', D.QuoteIdentifier('My]Table'));
  finally
    D.Free;
  end;
end;

// Reserved word strategy tests

procedure TTestSQLDriver.TestTransformIdentifierNone;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    D.ReservedWordStrategy:=rwsNone;
    AssertEquals('rwsNone returns unchanged', 'SELECT', D.TransformIdentifier('SELECT'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestTransformIdentifierAppendChar;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    D.ReservedWordStrategy:=rwsAppendChar;
    AssertEquals('rwsAppendChar appends _', 'VALUE_', D.TransformIdentifier('VALUE'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestTransformIdentifierAppendCharNotReserved;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    D.ReservedWordStrategy:=rwsAppendChar;
    AssertEquals('Non-reserved unchanged', 'MyField', D.TransformIdentifier('MyField'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestTransformIdentifierQuoteOnDemand;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    D.ReservedWordStrategy:=rwsQuoteOnDemand;
    AssertEquals('rwsQuoteOnDemand quotes reserved', '"VALUE"', D.TransformIdentifier('VALUE'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestTransformIdentifierQuoteOnDemandNotReserved;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    D.ReservedWordStrategy:=rwsQuoteOnDemand;
    AssertEquals('Non-reserved unchanged', 'MyField', D.TransformIdentifier('MyField'));
  finally
    D.Free;
  end;
end;

procedure TTestSQLDriver.TestTransformIdentifierAlwaysQuote;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    D.ReservedWordStrategy:=rwsAlwaysQuote;
    AssertEquals('rwsAlwaysQuote quotes all', '"MyField"', D.TransformIdentifier('MyField'));
  finally
    D.Free;
  end;
end;

// Reserved word lists

procedure TTestSQLDriver.TestSQLiteHasReservedWords;
begin
  AssertTrue('SQLite has reserved words', Length(TSQLDriverSQLite.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestMySQLHasReservedWords;
begin
  AssertTrue('MySQL has reserved words', Length(TSQLDriverMySQL.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestPostgreSQLHasReservedWords;
begin
  AssertTrue('PostgreSQL has reserved words', Length(TSQLDriverPostgreSQL.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestFirebirdHasReservedWords;
begin
  AssertTrue('Firebird has reserved words', Length(TSQLDriverFirebird.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestSQLServerHasReservedWords;
begin
  AssertTrue('SQL Server has reserved words', Length(TSQLDriverSQLServer.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestOracleHasReservedWords;
begin
  AssertTrue('Oracle has reserved words', Length(TSQLDriverOracle.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestVanillaHasReservedWords;
begin
  AssertTrue('Vanilla has reserved words', Length(TSQLDriverVanilla.GetReservedWords) > 0);
end;

procedure TTestSQLDriver.TestIsReservedWordCaseInsensitive;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLite);
  try
    AssertTrue('Uppercase reserved', D.IsReservedWord('SELECT'));
    AssertTrue('Lowercase reserved', D.IsReservedWord('select'));
    AssertTrue('Mixed case reserved', D.IsReservedWord('Select'));
    AssertFalse('Not reserved', D.IsReservedWord('MyCustomField'));
  finally
    D.Free;
  end;
end;

// Base driver defaults

procedure TTestSQLDriver.TestBaseDriverDefaults;
var D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dVanilla);
  try
    AssertEquals('Default quote char', '"', D.QuoteChar);
    AssertEquals('Default strategy', Ord(rwsNone), Ord(D.ReservedWordStrategy));
  finally
    D.Free;
  end;
end;

// Dialect property

procedure TTestSQLDriver.TestDialectProperty;
var D: TSQLDriver;
  Dia: TSQLDialect;
begin
  for Dia in TSQLDialect do
  begin
    D:=TSQLDriver.CreateDriver(Dia);
    try
      AssertEquals('Dialect matches', Ord(Dia), Ord(D.Dialect));
    finally
      D.Free;
    end;
  end;
end;

initialization
  RegisterTest(TTestSQLDriver);
end.
