unit utc.opf.dialectcontracts;

{ Tests for dialect contracts:
  - LIKE: opContains escapes % _ and the escape character, SQL Server also [
  - LIKE on numbers, booleans and dates raises
  - SQL Server pagination gets an ORDER BY
  - auto-increment retrieval per dialect: RETURNING, query after insert,
    same batch (SQL Server), RETURNING INTO (Oracle), none (raises) }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, StrUtils, DB, sqldb, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Criteria, Opf.Driver, Opf.Sql, Opf.DDL, Opf.Registry,
  Opf.Manager, Opf.Persister.RTTI, utc.opf.failconn;

type

  TDXItem = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TDXNoKey = class(TPersistent)
  private
    FName: String;
    FCode: Integer;
  published
    property Name: String read FName write FName;
    property Code: Integer read FCode write FCode;
  end;

  TDXManager = class(TOPManager)
  public
    function CriteriaSQL(aDef: TPersisterDef; aCriteria: TOPCriteria): UTF8String;
  end;

  { TTestLikeEscaping }

  TTestLikeEscaping = class(TTestCase)
  private
    FCrit: TOPCriteria;
    function Pattern(const aText: UTF8String): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPercentEscaped;
    procedure TestUnderscoreEscaped;
    procedure TestEscapeCharEscaped;
    procedure TestPlainTextUnchanged;
    procedure TestEscapeClause;
    procedure TestGenerateTwiceSameValue;
    procedure TestLikeNotEscaped;
    procedure TestSQLServerBracket;
    procedure TestOtherDialectsKeepBracket;
    procedure TestContainsIntegerRaises;
    procedure TestLikeDoubleRaises;
    procedure TestAddAndContainsDateRaises;
    procedure TestRaiseKeepsParams;
    procedure TestCloneKeepsText;
  end;

  { TTestDialectQueries }

  TTestDialectQueries = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FBuilder: TOPSQLBuilder;
    FManager: TDXManager;
    FDef, FNoKeyDef: TPersisterDef;
    function SQLFor(aDialect: TSQLDialect; aCriteria: TOPCriteria; aDef: TPersisterDef = Nil): UTF8String;
    function InsertSQL(aDialect: TSQLDialect): UTF8String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSQLServerLimitGetsKeyOrder;
    procedure TestSQLServerOffsetGetsKeyOrder;
    procedure TestSQLServerNoKeyOrdersByNull;
    procedure TestSQLServerKeepsOwnOrder;
    procedure TestPostgresLimitNoOrder;
    procedure TestRetrievalPerDialect;
    procedure TestSQLServerInsertSameBatch;
    procedure TestOracleInsertReturningInto;
    procedure TestOracleParamNames;
    procedure TestPostgresInsertReturning;
    procedure TestSQLiteInsertUnchanged;
  end;

  { TTestAutoIncExecution }

  TTestAutoIncExecution = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FBuilder: TOPSQLBuilder;
    FManager: TOPManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestVanillaRaises;
    procedure TestSQLiteAssignsKey;
  end;

implementation

{ TDXItem }

function TDXItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TDXItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TDXManager }

function TDXManager.CriteriaSQL(aDef: TPersisterDef; aCriteria: TOPCriteria): UTF8String;
begin
  Result:=BuildCriteriaSelectSQL(aDef, aCriteria);
end;

{ TTestLikeEscaping }

procedure TTestLikeEscaping.SetUp;
begin
  inherited SetUp;
  FCrit:=TOPCriteria.Create;
end;

procedure TTestLikeEscaping.TearDown;
begin
  FreeAndNil(FCrit);
  inherited TearDown;
end;

function TTestLikeEscaping.Pattern(const aText: UTF8String): String;
begin
  FCrit.Where('Name', opContains, aText);
  FCrit.ToWhereSQL(Nil);
  Result:=FCrit.Params[0].AsString;
end;

procedure TTestLikeEscaping.TestPercentEscaped;
begin
  AssertEquals('%50!%%', Pattern('50%'));
end;

procedure TTestLikeEscaping.TestUnderscoreEscaped;
begin
  AssertEquals('%a!_b%', Pattern('a_b'));
end;

procedure TTestLikeEscaping.TestEscapeCharEscaped;
begin
  AssertEquals('%wow!!%', Pattern('wow!'));
end;

procedure TTestLikeEscaping.TestPlainTextUnchanged;
begin
  AssertEquals('%ali%', Pattern('ali'));
end;

procedure TTestLikeEscaping.TestEscapeClause;
begin
  FCrit.Where('Name', opContains, 'x');
  AssertEquals('Name LIKE :p1 ESCAPE ''!''', FCrit.ToWhereSQL(Nil));
end;

procedure TTestLikeEscaping.TestGenerateTwiceSameValue;
begin
  FCrit.Where('Name', opContains, '5%');
  FCrit.ToWhereSQL(Nil);
  FCrit.ToWhereSQL(Nil);
  AssertEquals('Not escaped twice', '%5!%%', FCrit.Params[0].AsString);
end;

procedure TTestLikeEscaping.TestLikeNotEscaped;
begin
  // opLike takes a pattern written by the caller
  FCrit.Where('Name', opLike, 'a%');
  AssertEquals('Name LIKE :p1', FCrit.ToWhereSQL(Nil));
  AssertEquals('a%', FCrit.Params[0].AsString);
end;

procedure TTestLikeEscaping.TestSQLServerBracket;
var
  D: TSQLDriver;
begin
  D:=TSQLDriver.CreateDriver(dSQLServer);
  try
    FCrit.Where('Name', opContains, '[x]');
    FCrit.ToWhereSQL(Nil, Nil, @D.EscapeLikeText);
    AssertEquals('%![x]%', FCrit.Params[0].AsString);
  finally
    D.Free;
  end;
end;

procedure TTestLikeEscaping.TestOtherDialectsKeepBracket;
var
  D: TSQLDriver;
begin
  // Firebird and Oracle reject the escape character before other characters
  D:=TSQLDriver.CreateDriver(dFirebird3);
  try
    FCrit.Where('Name', opContains, '[x]');
    FCrit.ToWhereSQL(Nil, Nil, @D.EscapeLikeText);
    AssertEquals('%[x]%', FCrit.Params[0].AsString);
  finally
    D.Free;
  end;
end;

procedure TTestLikeEscaping.TestContainsIntegerRaises;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FCrit.Where('Age', opContains, 5);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestLikeEscaping.TestLikeDoubleRaises;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FCrit.Where('Price', opLike, Double(1.5));
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestLikeEscaping.TestAddAndContainsDateRaises;
var
  Raised: Boolean;
begin
  FCrit.Where('Name', opEqual, 'a');
  Raised:=False;
  try
    FCrit.AddAnd('Born', opNotLike, TDateTime(Now));
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestLikeEscaping.TestRaiseKeepsParams;
begin
  FCrit.Where('Name', opEqual, 'a');
  try
    FCrit.AddOr('Age', opContains, 5);
  except
    on E: EOPF do ;
  end;
  AssertEquals('Failed parameter removed', 1, FCrit.Params.Count);
  FCrit.AddAnd('Code', opEqual, 2);
  AssertEquals('Numbering continues', 'p2', FCrit.Params[1].Name);
  AssertEquals('Name = :p1 AND Code = :p2', FCrit.ToWhereSQL(Nil));
end;

procedure TTestLikeEscaping.TestCloneKeepsText;
var
  C: TOPCriteria;
begin
  FCrit.Where('Name', opContains, '1_2');
  C:=FCrit.Clone;
  try
    C.ToWhereSQL(Nil);
    AssertEquals('%1!_2%', C.Params[0].AsString);
  finally
    C.Free;
  end;
end;

{ TTestDialectQueries }

procedure TTestDialectQueries.SetUp;
begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  FDef:=FRegistry.RegisterPersister(TDXItem, TRTTIPersister);
  FDef.FieldMap.FindByPropertyName('ID').Options:=FDef.FieldMap.FindByPropertyName('ID').Options+[foAutoInc]-[foInInsert];
  FNoKeyDef:=FRegistry.RegisterPersister(TDXNoKey, Nil);
  FNoKeyDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  FNoKeyDef.FieldMap.AddField('Name', 'Name').PropertyKind:=tkAString;
  FBuilder:=TOPSQLBuilder.Create(Nil);
  FBuilder.Registry:=FRegistry;
  FManager:=TDXManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.SQLBuilder:=FBuilder;
end;

procedure TTestDialectQueries.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FBuilder);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

function TTestDialectQueries.SQLFor(aDialect: TSQLDialect; aCriteria: TOPCriteria; aDef: TPersisterDef): UTF8String;
begin
  if aDef=Nil then
    aDef:=FDef;
  FBuilder.SQLDialect:=aDialect;
  Result:=FManager.CriteriaSQL(aDef, aCriteria);
end;

function TTestDialectQueries.InsertSQL(aDialect: TSQLDialect): UTF8String;
begin
  FBuilder.SQLDialect:=aDialect;
  Result:=FBuilder.GetInsertSQL('T', FDef.FieldMap.AsArray);
end;

procedure TTestDialectQueries.TestSQLServerLimitGetsKeyOrder;
var
  C: TOPCriteria;
  S: UTF8String;
begin
  C:=TOPCriteria.Create;
  try
    C.Limit(5);
    S:=SQLFor(dSQLServer, C);
    AssertTrue(S, Pos('ORDER BY ID OFFSET 0 ROWS FETCH NEXT 5 ROWS ONLY', S)>0);
  finally
    C.Free;
  end;
end;

procedure TTestDialectQueries.TestSQLServerOffsetGetsKeyOrder;
var
  C: TOPCriteria;
  S: UTF8String;
begin
  C:=TOPCriteria.Create;
  try
    C.Offset(10);
    S:=SQLFor(dSQLServer, C);
    AssertTrue(S, Pos('ORDER BY ID OFFSET 10 ROWS', S)>0);
  finally
    C.Free;
  end;
end;

procedure TTestDialectQueries.TestSQLServerNoKeyOrdersByNull;
var
  C: TOPCriteria;
  S: UTF8String;
begin
  C:=TOPCriteria.Create;
  try
    C.Limit(5);
    S:=SQLFor(dSQLServer, C, FNoKeyDef);
    AssertTrue(S, Pos('ORDER BY (SELECT NULL) OFFSET', S)>0);
  finally
    C.Free;
  end;
end;

procedure TTestDialectQueries.TestSQLServerKeepsOwnOrder;
var
  C: TOPCriteria;
  S: UTF8String;
begin
  C:=TOPCriteria.Create;
  try
    C.OrderBy('Name').Limit(5);
    S:=SQLFor(dSQLServer, C);
    AssertTrue(S, Pos('ORDER BY Name OFFSET', S)>0);
    AssertEquals('One ORDER BY', Pos('ORDER BY', S), RPos('ORDER BY', S));
  finally
    C.Free;
  end;
end;

procedure TTestDialectQueries.TestPostgresLimitNoOrder;
var
  C: TOPCriteria;
  S: UTF8String;
begin
  C:=TOPCriteria.Create;
  try
    C.Limit(5);
    S:=SQLFor(dPostGres, C);
    AssertEquals('No ORDER BY added', 0, Pos('ORDER BY', S));
  finally
    C.Free;
  end;
end;

procedure TTestDialectQueries.TestRetrievalPerDialect;

  procedure Check(aDialect: TSQLDialect; aExpected: TOPAutoIncRetrieval);
  var
    D: TSQLDriver;
  begin
    D:=TSQLDriver.CreateDriver(aDialect);
    try
      AssertEquals(aDialect.ToString, Ord(aExpected), Ord(D.AutoIncRetrieval));
    finally
      D.Free;
    end;
  end;

begin
  Check(dSQLite, airPostQuery);
  Check(dMySQL, airPostQuery);
  Check(dPostGres, airReturning);
  Check(dFirebird3, airReturning);
  Check(dSQLServer, airSameBatch);
  Check(dOracle, airReturningInto);
  Check(dVanilla, airNone);
end;

procedure TTestDialectQueries.TestSQLServerInsertSameBatch;
begin
  // NOCOUNT is a session setting: it must be off again before later statements
  // (RowsAffected drives the optimistic locking and delete checks)
  AssertEquals('SET NOCOUNT ON; INSERT INTO T (Name) VALUES (:Name); SET NOCOUNT OFF; SELECT SCOPE_IDENTITY()',
    InsertSQL(dSQLServer));
end;

procedure TTestDialectQueries.TestOracleInsertReturningInto;
begin
  AssertEquals('INSERT INTO T (Name) VALUES (:Name) RETURNING ID INTO :opf_ret',
    InsertSQL(dOracle));
end;

procedure TTestDialectQueries.TestOracleParamNames;
var
  F: TOPFieldMap;

  function ParamName(aDialect: TSQLDialect; const aFieldName: String): String;
  begin
    F.FieldName:=aFieldName;
    FBuilder.SQLDialect:=aDialect;
    Result:=FBuilder.FieldParamName(F);
  end;

begin
  F:=TOPFieldMap.Create(Nil);
  try
    AssertEquals('Reserved word kept on PostgreSQL', 'Level', ParamName(dPostGres, 'Level'));
    AssertEquals('Reserved word encoded on Oracle', OPFEncodedParamName('Level'), ParamName(dOracle, 'Level'));
    AssertEquals('Leading underscore encoded on Oracle', OPFEncodedParamName('_x'), ParamName(dOracle, '_x'));
    AssertEquals('NAME is a valid Oracle bind name', 'Name', ParamName(dOracle, 'Name'));
    AssertEquals('Plain name kept on Oracle', 'Title', ParamName(dOracle, 'Title'));
  finally
    F.Free;
  end;
end;

procedure TTestDialectQueries.TestPostgresInsertReturning;
begin
  AssertEquals('INSERT INTO T (Name) VALUES (:Name) RETURNING ID', InsertSQL(dPostGres));
end;

procedure TTestDialectQueries.TestSQLiteInsertUnchanged;
begin
  AssertEquals('INSERT INTO T (Name) VALUES (:Name)', InsertSQL(dSQLite));
end;

{ TTestAutoIncExecution }

procedure TTestAutoIncExecution.SetUp;
var
  C: TOPConnection;
  D: TPersisterDef;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FConn.ExecuteDirect('CREATE TABLE DXItem (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT)');
  FTrans.Commit;
  FRegistry:=TOPRegistry.Create(Nil);
  D:=FRegistry.RegisterPersister(TDXItem, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foAutoInc]-[foInInsert];
  FBuilder:=TOPSQLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dSQLite;
  FBuilder.Registry:=FRegistry;
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.SQLBuilder:=FBuilder;
  C:=FManager.Connections.Add as TOPConnection;
  C.SingleConnection:=FConn;
  C.IsDefault:=True;
end;

procedure TTestAutoIncExecution.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FBuilder);
  FreeAndNil(FRegistry);
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestAutoIncExecution.TestVanillaRaises;
var
  O: TDXItem;
  Raised: Boolean;
begin
  FBuilder.SQLDialect:=dVanilla;
  O:=TDXItem.Create;
  try
    O.Name:='x';
    O.State:=opsNew;
    Raised:=False;
    try
      FManager.Save(O);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('No silent key 0', Raised);
  finally
    O.Free;
  end;
end;

procedure TTestAutoIncExecution.TestSQLiteAssignsKey;
var
  O: TDXItem;
begin
  O:=TDXItem.Create;
  try
    O.Name:='x';
    O.State:=opsNew;
    FManager.Save(O);
    AssertEquals(1, O.ID);
  finally
    O.Free;
  end;
end;

initialization
  RegisterTests([TTestLikeEscaping, TTestDialectQueries, TTestAutoIncExecution]);
end.
