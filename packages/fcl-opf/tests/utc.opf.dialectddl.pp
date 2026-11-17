unit utc.opf.dialectddl;

{ Dialect specific DDL and type handling:
  - column types for dates, currency, floats, 64-bit integers, memo/blob
  - auto-increment primary keys per dialect
  - ALTER TABLE syntax per dialect
  - parameter types for date/time and currency
  - ExecDDL restores the connection's transaction when the commit fails
  - date/time and currency round trip through the manager (SQLite) }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.DateUtils, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, DateUtils, DB, sqldb,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Manager, Opf.Sql, Opf.DDL, Opf.Driver,
  Opf.Persister.RTTI, utc.opf.failconn;

type

  { TDDLTypes: one property per interesting type }

  TDDLTypes = class(TPersistent, IOPPersistenceState)
  private
    FAt: TTime;
    FBig: Int64;
    FBorn: TDateTime;
    FDay: TDate;
    FID: Integer;
    FPrice: Currency;
    FRatio: Double;
    FSmall: Single;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Big: Int64 read FBig write FBig;
    property Born: TDateTime read FBorn write FBorn;
    property Day: TDate read FDay write FDay;
    property At: TTime read FAt write FAt;
    property Price: Currency read FPrice write FPrice;
    property Ratio: Double read FRatio write FRatio;
    property Small: Single read FSmall write FSmall;
  end;

  { TDDLBigKey: 64-bit auto-increment key }

  TDDLBigKey = class(TPersistent)
  private
    FID: Int64;
    FName: String;
  published
    property ID: Int64 read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestDialectDDL }

  TTestDialectDDL = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FBuilder: TOPDDLBuilder;
    FDef: TPersisterDef;
    FBigDef: TPersisterDef;
    FSavedStorage: TOPDateTimeStorage;
    function ColumnType(aDialect: TSQLDialect; const aProp: UTF8String): UTF8String;
    function CreateSQL(aDialect: TSQLDialect; aDef: TPersisterDef): UTF8String;
    procedure AssertRaisesDDL(const aMsg: String; aDialect: TSQLDialect; aDef: TPersisterDef);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Float kinds
    procedure TestFloatKinds;
    // Types per dialect
    procedure TestDateTimeTypesDefault;
    procedure TestDateTimeTypesSQLServer;
    procedure TestDateTimeTypesMySQL;
    procedure TestDateTimeTypesOracle;
    procedure TestDateTimeAsFloatGlobal;
    procedure TestDateTimeAsFloatPerField;
    procedure TestCurrencyType;
    procedure TestDoubleTypes;
    procedure TestSingleType;
    procedure TestBigIntTypes;
    procedure TestIntegerTypesOracle;
    procedure TestExplicitPrecisionWins;
    procedure TestMemoBlobSQLServer;
    procedure TestUntypedFloatUnchanged;
    // Auto-increment
    procedure TestAutoIncPostgres;
    procedure TestAutoIncPostgres64;
    procedure TestAutoIncFirebird3;
    procedure TestAutoIncFirebird3_64;
    procedure TestAutoIncFirebird25Raises;
    procedure TestAutoIncMySQL;
    procedure TestAutoIncSQLServer;
    procedure TestAutoIncOracle;
    procedure TestAutoIncSQLite;
    procedure TestAutoIncCompositeKeyRaises;
    // ALTER
    procedure TestAddColumnPerDialect;
    procedure TestDropColumnPerDialect;
    procedure TestAlterTypePerDialect;
    procedure TestAlterTypeMySQLKeepsNotNull;
    procedure TestAlterNullablePerDialect;
    procedure TestAlterNullableNeedsFieldOnMySQLAndSQLServer;
    procedure TestAlterNullableFirebird25Raises;
    procedure TestDropIndexPerDialect;
    procedure TestDropIndexWithoutTableRaisesOnMySQL;
    // Parameter types
    procedure TestFloatParamTypes;
    procedure TestFloatParamTypesLegacyStorage;
  end;

  { TTestDDLExecRecovery }

  TTestDDLExecRecovery = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FBuilder: TOPDDLBuilder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommitFailureRestoresTransaction;
    procedure TestExecFailureRestoresTransaction;
    procedure TestConnectionUsableAfterFailure;
    procedure TestErrorIsEOPFDDL;
  end;

  { TTestTypeRoundTrip: native dates and currency through the manager on SQLite }

  TTestTypeRoundTrip = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FSQLBuilder: TOPSQLBuilder;
    FDDLBuilder: TOPDDLBuilder;
    FManager: TOPManager;
    function SaveAndLoad(aObj: TDDLTypes): TDDLTypes;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDateTimeWithMilliseconds;
    procedure TestDateOnly;
    procedure TestTimeOnly;
    procedure TestCurrencyExact;
    procedure TestNegativeCurrency;
    procedure TestInt64Large;
    procedure TestDoubleSmall;
    procedure TestCreatedTableUsesNativeTypes;
  end;

implementation

{ TDDLTypes }

function TDDLTypes.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TDDLTypes.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestDialectDDL }

function TTestDialectDDL.ColumnType(aDialect: TSQLDialect; const aProp: UTF8String): UTF8String;
begin
  FBuilder.SQLDialect:=aDialect;
  Result:=FBuilder.GetPropertyDefSQL(FDef.TableName, FDef.FieldMap.FindByPropertyName(aProp));
end;

function TTestDialectDDL.CreateSQL(aDialect: TSQLDialect; aDef: TPersisterDef): UTF8String;
begin
  FBuilder.SQLDialect:=aDialect;
  Result:=FBuilder.GetCreateSQL(aDef.TableName, aDef.FieldMap.AsArray);
end;

procedure TTestDialectDDL.AssertRaisesDDL(const aMsg: String; aDialect: TSQLDialect; aDef: TPersisterDef);
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    CreateSQL(aDialect, aDef);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(aMsg, Raised);
end;

procedure TTestDialectDDL.SetUp;
var
  F: TOPFieldMap;
begin
  inherited SetUp;
  FSavedStorage:=OPFDateTimeStorage;
  FRegistry:=TOPRegistry.Create(Nil);
  FDef:=FRegistry.RegisterPersister(TDDLTypes, TRTTIPersister);
  F:=FDef.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  FBigDef:=FRegistry.RegisterPersister(TDDLBigKey, TRTTIPersister);
  F:=FBigDef.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  FBuilder:=TOPDDLBuilder.Create(Nil);
  FBuilder.Registry:=FRegistry;
end;

procedure TTestDialectDDL.TearDown;
begin
  OPFDateTimeStorage:=FSavedStorage;
  FreeAndNil(FBuilder);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

procedure TTestDialectDDL.TestFloatKinds;
var
  M: TOPFieldMapList;
begin
  M:=FDef.FieldMap;
  AssertTrue('TDateTime', OPFFloatKind(M.FindByPropertyName('Born'))=fkDateTime);
  AssertTrue('TDate', OPFFloatKind(M.FindByPropertyName('Day'))=fkDate);
  AssertTrue('TTime', OPFFloatKind(M.FindByPropertyName('At'))=fkTime);
  AssertTrue('Currency', OPFFloatKind(M.FindByPropertyName('Price'))=fkCurrency);
  AssertTrue('Double', OPFFloatKind(M.FindByPropertyName('Ratio'))=fkDouble);
  AssertTrue('Single', OPFFloatKind(M.FindByPropertyName('Small'))=fkSingle);
  AssertTrue('Not a float', OPFFloatKind(M.FindByPropertyName('ID'))=fkUnknown);
end;

procedure TTestDialectDDL.TestDateTimeTypesDefault;
var
  D: TSQLDialect;
begin
  for D in [dPostGres, dFirebird3, dSQLite] do
  begin
    AssertEquals('TDateTime '+D.ToString, 'TIMESTAMP', ColumnType(D, 'Born'));
    AssertEquals('TDate '+D.ToString, 'DATE', ColumnType(D, 'Day'));
    AssertEquals('TTime '+D.ToString, 'TIME', ColumnType(D, 'At'));
  end;
end;

procedure TTestDialectDDL.TestDateTimeTypesSQLServer;
begin
  AssertEquals('TDateTime', 'DATETIME2', ColumnType(dSQLServer, 'Born'));
  AssertEquals('TDate', 'DATE', ColumnType(dSQLServer, 'Day'));
  AssertEquals('TTime', 'TIME', ColumnType(dSQLServer, 'At'));
end;

procedure TTestDialectDDL.TestDateTimeTypesMySQL;
begin
  AssertEquals('TDateTime', 'DATETIME(3)', ColumnType(dMySQL, 'Born'));
  AssertEquals('TDate', 'DATE', ColumnType(dMySQL, 'Day'));
  AssertEquals('TTime', 'TIME(3)', ColumnType(dMySQL, 'At'));
end;

procedure TTestDialectDDL.TestDateTimeTypesOracle;
begin
  AssertEquals('TDateTime', 'TIMESTAMP', ColumnType(dOracle, 'Born'));
  AssertEquals('TDate', 'DATE', ColumnType(dOracle, 'Day'));
  AssertEquals('TTime has no Oracle type', 'TIMESTAMP', ColumnType(dOracle, 'At'));
end;

procedure TTestDialectDDL.TestDateTimeAsFloatGlobal;
begin
  OPFDateTimeStorage:=dtsFloat;
  AssertEquals('Numeric storage', 'DOUBLE PRECISION', ColumnType(dPostGres, 'Born'));
  AssertEquals('Numeric storage date', 'DOUBLE PRECISION', ColumnType(dPostGres, 'Day'));
end;

procedure TTestDialectDDL.TestDateTimeAsFloatPerField;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Born');
  F.Options:=F.Options+[foDateTimeAsFloat];
  AssertEquals('Field kept numeric', 'DOUBLE PRECISION', ColumnType(dPostGres, 'Born'));
  AssertEquals('Other date field native', 'DATE', ColumnType(dPostGres, 'Day'));
end;

procedure TTestDialectDDL.TestCurrencyType;
begin
  AssertEquals('PostgreSQL', 'DECIMAL(18,4)', ColumnType(dPostGres, 'Price'));
  AssertEquals('Firebird', 'DECIMAL(18,4)', ColumnType(dFirebird3, 'Price'));
  AssertEquals('Oracle', 'NUMBER(18,4)', ColumnType(dOracle, 'Price'));
  AssertEquals('Vanilla prefers FLOAT', 'FLOAT', ColumnType(dVanilla, 'Price'));
end;

procedure TTestDialectDDL.TestDoubleTypes;
begin
  AssertEquals('PostgreSQL', 'DOUBLE PRECISION', ColumnType(dPostGres, 'Ratio'));
  AssertEquals('Firebird (FLOAT is single precision)', 'DOUBLE PRECISION', ColumnType(dFirebird3, 'Ratio'));
  AssertEquals('MySQL (FLOAT is single precision)', 'DOUBLE', ColumnType(dMySQL, 'Ratio'));
  AssertEquals('SQL Server', 'FLOAT(53)', ColumnType(dSQLServer, 'Ratio'));
  AssertEquals('Oracle', 'BINARY_DOUBLE', ColumnType(dOracle, 'Ratio'));
end;

procedure TTestDialectDDL.TestSingleType;
begin
  AssertEquals('PostgreSQL', 'REAL', ColumnType(dPostGres, 'Small'));
  AssertEquals('MySQL', 'FLOAT', ColumnType(dMySQL, 'Small'));
end;

procedure TTestDialectDDL.TestBigIntTypes;
begin
  AssertEquals('PostgreSQL', 'BIGINT', ColumnType(dPostGres, 'Big'));
  AssertEquals('Oracle has no BIGINT', 'NUMBER(19)', ColumnType(dOracle, 'Big'));
end;

procedure TTestDialectDDL.TestIntegerTypesOracle;
var
  F: TOPFieldMap;
begin
  F:=TOPFieldMap.Create(Nil);
  try
    F.FieldName:='N';
    F.PropertyKind:=tkInteger;
    FBuilder.SQLDialect:=dOracle;
    AssertEquals('Oracle 32-bit integer', 'NUMBER(10)', FBuilder.GetPropertyDefSQL('t', F));
    FBuilder.SQLDialect:=dPostGres;
    AssertEquals('PostgreSQL 32-bit integer', 'INT', FBuilder.GetPropertyDefSQL('t', F));
    F.MaxSize:=100;
    FBuilder.SQLDialect:=dOracle;
    AssertEquals('Oracle 16-bit integer', 'NUMBER(5)', FBuilder.GetPropertyDefSQL('t', F));
    F.MaxSize:=0;
    F.PropertyKind:=tkBool;
    AssertEquals('Oracle boolean', 'NUMBER(5)', FBuilder.GetPropertyDefSQL('t', F));
  finally
    F.Free;
  end;
end;

procedure TTestDialectDDL.TestExplicitPrecisionWins;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Ratio');
  F.MaxSize:=12;
  F.MinSize:=4;
  AssertEquals('Explicit precision', 'DECIMAL(12,4)', ColumnType(dPostGres, 'Ratio'));
end;

procedure TTestDialectDDL.TestMemoBlobSQLServer;
var
  F: TOPFieldMap;
begin
  FBuilder.SQLDialect:=dSQLServer;
  F:=TOPFieldMap.Create(Nil);
  try
    F.FieldName:='Notes';
    F.PropertyKind:=tkVariant; // stored as memo
    AssertEquals('Memo', 'NVARCHAR(MAX)', FBuilder.GetPropertyDefSQL('t', F));
  finally
    F.Free;
  end;
end;

procedure TTestDialectDDL.TestUntypedFloatUnchanged;
var
  F: TOPFieldMap;
begin
  FBuilder.SQLDialect:=dPostGres;
  F:=TOPFieldMap.Create(Nil);
  try
    F.FieldName:='X';
    F.PropertyKind:=tkFloat;
    AssertEquals('No type info: generic FLOAT', 'FLOAT', FBuilder.GetPropertyDefSQL('t', F));
  finally
    F.Free;
  end;
end;

procedure TTestDialectDDL.TestAutoIncPostgres;
begin
  AssertTrue(Pos('ID SERIAL PRIMARY KEY', CreateSQL(dPostGres, FDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncPostgres64;
begin
  AssertTrue(Pos('ID BIGSERIAL PRIMARY KEY', CreateSQL(dPostGres, FBigDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncFirebird3;
begin
  AssertTrue(CreateSQL(dFirebird3, FDef),
    Pos('ID INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY', CreateSQL(dFirebird3, FDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncFirebird3_64;
begin
  AssertTrue(Pos('ID BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY', CreateSQL(dFirebird3, FBigDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncFirebird25Raises;
begin
  AssertRaisesDDL('Firebird 2.5 has no identity columns', dFirebird, FDef);
end;

procedure TTestDialectDDL.TestAutoIncMySQL;
begin
  AssertTrue(Pos('ID INTEGER PRIMARY KEY AUTO_INCREMENT', CreateSQL(dMySQL, FDef))>0);
  AssertTrue(Pos('ID BIGINT PRIMARY KEY AUTO_INCREMENT', CreateSQL(dMySQL, FBigDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncSQLServer;
begin
  AssertTrue(Pos('ID INT IDENTITY(1,1) PRIMARY KEY', CreateSQL(dSQLServer, FDef))>0);
  AssertTrue(Pos('ID BIGINT IDENTITY(1,1) PRIMARY KEY', CreateSQL(dSQLServer, FBigDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncOracle;
begin
  AssertTrue(Pos('ID NUMBER(10) GENERATED BY DEFAULT ON NULL AS IDENTITY PRIMARY KEY', CreateSQL(dOracle, FDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncSQLite;
begin
  AssertTrue(Pos('ID INTEGER PRIMARY KEY', CreateSQL(dSQLite, FBigDef))>0);
end;

procedure TTestDialectDDL.TestAutoIncCompositeKeyRaises;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Big');
  F.Options:=F.Options+[foInKey];
  AssertRaisesDDL('Auto-increment in a composite key', dPostGres, FDef);
end;

procedure TTestDialectDDL.TestAddColumnPerDialect;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Big');
  FBuilder.SQLDialect:=dPostGres;
  AssertEquals('PostgreSQL', 'ALTER TABLE DDLTypes ADD COLUMN Big BIGINT', FBuilder.GetAddColumnSQL('DDLTypes', F));
  FBuilder.SQLDialect:=dFirebird3;
  AssertEquals('Firebird', 'ALTER TABLE DDLTypes ADD Big BIGINT', FBuilder.GetAddColumnSQL('DDLTypes', F));
  FBuilder.SQLDialect:=dSQLServer;
  AssertEquals('SQL Server', 'ALTER TABLE DDLTypes ADD Big BIGINT', FBuilder.GetAddColumnSQL('DDLTypes', F));
  FBuilder.SQLDialect:=dOracle;
  AssertEquals('Oracle', 'ALTER TABLE DDLTypes ADD (Big NUMBER(19))', FBuilder.GetAddColumnSQL('DDLTypes', F));
end;

procedure TTestDialectDDL.TestDropColumnPerDialect;
begin
  FBuilder.SQLDialect:=dFirebird3;
  AssertEquals('Firebird', 'ALTER TABLE T DROP Col', FBuilder.GetDropColumnSQL('T', 'Col'));
  FBuilder.SQLDialect:=dPostGres;
  AssertEquals('PostgreSQL', 'ALTER TABLE T DROP COLUMN Col', FBuilder.GetDropColumnSQL('T', 'Col'));
end;

procedure TTestDialectDDL.TestAlterTypePerDialect;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Big');
  FBuilder.SQLDialect:=dPostGres;
  AssertEquals('PostgreSQL', 'ALTER TABLE T ALTER COLUMN Big TYPE BIGINT', FBuilder.GetAlterColumnTypeSQL('T', 'Big', F));
  FBuilder.SQLDialect:=dFirebird3;
  AssertEquals('Firebird', 'ALTER TABLE T ALTER COLUMN Big TYPE BIGINT', FBuilder.GetAlterColumnTypeSQL('T', 'Big', F));
  FBuilder.SQLDialect:=dSQLServer;
  AssertEquals('SQL Server', 'ALTER TABLE T ALTER COLUMN Big BIGINT NULL', FBuilder.GetAlterColumnTypeSQL('T', 'Big', F));
  FBuilder.SQLDialect:=dOracle;
  AssertEquals('Oracle', 'ALTER TABLE T MODIFY (Big NUMBER(19))', FBuilder.GetAlterColumnTypeSQL('T', 'Big', F));
end;

procedure TTestDialectDDL.TestAlterTypeMySQLKeepsNotNull;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Big');
  F.Options:=F.Options+[foRequired];
  FBuilder.SQLDialect:=dMySQL;
  AssertEquals('MODIFY repeats NOT NULL', 'ALTER TABLE T MODIFY COLUMN Big BIGINT NOT NULL',
    FBuilder.GetAlterColumnTypeSQL('T', 'Big', F));
end;

procedure TTestDialectDDL.TestAlterNullablePerDialect;
var
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Big');
  FBuilder.SQLDialect:=dFirebird3;
  AssertEquals('Firebird 3', 'ALTER TABLE T ALTER COLUMN Big SET NOT NULL', FBuilder.GetAlterColumnNullableSQL('T', 'Big', False));
  FBuilder.SQLDialect:=dOracle;
  AssertEquals('Oracle', 'ALTER TABLE T MODIFY (Big NULL)', FBuilder.GetAlterColumnNullableSQL('T', 'Big', True));
  FBuilder.SQLDialect:=dSQLServer;
  AssertEquals('SQL Server', 'ALTER TABLE T ALTER COLUMN Big BIGINT NOT NULL', FBuilder.GetAlterColumnNullableSQL('T', 'Big', False, F));
  FBuilder.SQLDialect:=dMySQL;
  AssertEquals('MySQL', 'ALTER TABLE T MODIFY COLUMN Big BIGINT NULL', FBuilder.GetAlterColumnNullableSQL('T', 'Big', True, F));
end;

procedure TTestDialectDDL.TestAlterNullableNeedsFieldOnMySQLAndSQLServer;
var
  D: TSQLDialect;
  Raised: Boolean;
begin
  for D in [dMySQL, dSQLServer] do
  begin
    FBuilder.SQLDialect:=D;
    Raised:=False;
    try
      FBuilder.GetAlterColumnNullableSQL('T', 'Big', False);
    except
      on EOPFDDL do
        Raised:=True;
    end;
    AssertTrue('Raises without field: '+D.ToString, Raised);
  end;
end;

procedure TTestDialectDDL.TestAlterNullableFirebird25Raises;
var
  Raised: Boolean;
begin
  FBuilder.SQLDialect:=dFirebird;
  Raised:=False;
  try
    FBuilder.GetAlterColumnNullableSQL('T', 'Big', False);
  except
    on EOPFDDL do
      Raised:=True;
  end;
  AssertTrue('Firebird 2.5 cannot change NOT NULL', Raised);
end;

procedure TTestDialectDDL.TestDropIndexPerDialect;
begin
  FBuilder.SQLDialect:=dMySQL;
  AssertEquals('MySQL', 'DROP INDEX idx ON T', FBuilder.GetDropIndexSQL('idx', 'T'));
  FBuilder.SQLDialect:=dSQLServer;
  AssertEquals('SQL Server', 'DROP INDEX idx ON T', FBuilder.GetDropIndexSQL('idx', 'T'));
  FBuilder.SQLDialect:=dPostGres;
  AssertEquals('PostgreSQL ignores the table', 'DROP INDEX idx', FBuilder.GetDropIndexSQL('idx', 'T'));
end;

procedure TTestDialectDDL.TestDropIndexWithoutTableRaisesOnMySQL;
var
  Raised: Boolean;
begin
  FBuilder.SQLDialect:=dMySQL;
  Raised:=False;
  try
    FBuilder.GetDropIndexSQL('idx');
  except
    on EOPFDDL do
      Raised:=True;
  end;
  AssertTrue('Table required', Raised);
end;

procedure TTestDialectDDL.TestFloatParamTypes;
var
  P: TParam;
  M: TOPFieldMapList;
begin
  M:=FDef.FieldMap;
  P:=TParam.Create(Nil);
  try
    OPFSetFloatParam(P, M.FindByPropertyName('Born'), EncodeDateTime(2026,10,15,13,14,15,16));
    AssertEquals('TDateTime', Ord(ftDateTime), Ord(P.DataType));
    AssertEquals('TDateTime value', EncodeDateTime(2026,10,15,13,14,15,16), P.AsDateTime, 1e-9);
    OPFSetFloatParam(P, M.FindByPropertyName('Day'), EncodeDate(2026,10,15));
    AssertEquals('TDate', Ord(ftDate), Ord(P.DataType));
    OPFSetFloatParam(P, M.FindByPropertyName('At'), EncodeTime(13,14,15,0));
    AssertEquals('TTime', Ord(ftTime), Ord(P.DataType));
    OPFSetFloatParam(P, M.FindByPropertyName('Price'), 12.3456);
    AssertEquals('Currency as BCD', Ord(ftBCD), Ord(P.DataType));
    AssertEquals('Currency value', 12.3456, P.AsCurrency, 0);
    OPFSetFloatParam(P, M.FindByPropertyName('Ratio'), 1.5);
    AssertEquals('Double', Ord(ftFloat), Ord(P.DataType));
  finally
    P.Free;
  end;
end;

procedure TTestDialectDDL.TestFloatParamTypesLegacyStorage;
var
  P: TParam;
begin
  OPFDateTimeStorage:=dtsFloat;
  P:=TParam.Create(Nil);
  try
    OPFSetFloatParam(P, FDef.FieldMap.FindByPropertyName('Born'), 45000.5);
    AssertEquals('Legacy numeric date', Ord(ftFloat), Ord(P.DataType));
    AssertEquals('Value', 45000.5, P.AsFloat, 0);
  finally
    P.Free;
  end;
end;

{ TTestDDLExecRecovery }

procedure TTestDDLExecRecovery.SetUp;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FBuilder:=TOPDDLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dSQLite;
end;

procedure TTestDDLExecRecovery.TearDown;
begin
  FreeAndNil(FBuilder);
  FConn.FailCommit:=False;
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestDDLExecRecovery.TestCommitFailureRestoresTransaction;
begin
  FConn.FailCommit:=True;
  try
    FBuilder.ExecDDL(FConn, 'CREATE TABLE T1 (ID INTEGER)');
    Fail('Commit failure must raise');
  except
    on EOPFDDL do ;
  end;
  AssertSame('Connection transaction restored', FTrans, FConn.Transaction);
  AssertEquals('No transaction leaked', 1, FConn.TransactionCount);
end;

procedure TTestDDLExecRecovery.TestExecFailureRestoresTransaction;
begin
  try
    FBuilder.ExecDDL(FConn, 'CREATE TABLE (');
    Fail('Invalid DDL must raise');
  except
    on EOPFDDL do ;
  end;
  AssertSame('Connection transaction restored', FTrans, FConn.Transaction);
  AssertEquals('No transaction leaked', 1, FConn.TransactionCount);
end;

procedure TTestDDLExecRecovery.TestConnectionUsableAfterFailure;
begin
  FConn.FailCommit:=True;
  try
    FBuilder.ExecDDL(FConn, 'CREATE TABLE T1 (ID INTEGER)');
  except
    on EOPFDDL do ;
  end;
  FConn.FailCommit:=False;
  FBuilder.ExecDDL(FConn, 'CREATE TABLE T2 (ID INTEGER)');
  AssertSame('Transaction restored', FTrans, FConn.Transaction);
end;

procedure TTestDDLExecRecovery.TestErrorIsEOPFDDL;
var
  Msg: String;
begin
  Msg:='';
  FConn.FailCommit:=True;
  try
    FBuilder.ExecDDL(FConn, 'CREATE TABLE T1 (ID INTEGER)');
  except
    on E: EOPFDDL do
      Msg:=E.Message;
  end;
  AssertTrue('Original error in message: '+Msg, Pos('Simulated commit failure', Msg)>0);
end;

{ TTestTypeRoundTrip }

function TTestTypeRoundTrip.SaveAndLoad(aObj: TDDLTypes): TDDLTypes;
begin
  aObj.State:=opsNew;
  FManager.Save(aObj);
  Result:=TDDLTypes.Create;
  Result.ID:=aObj.ID;
  FManager.Load(Result);
end;

procedure TTestTypeRoundTrip.SetUp;
var
  C: TOPConnection;
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FRegistry:=TOPRegistry.Create(Nil);
  D:=FRegistry.RegisterPersister(TDDLTypes, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  FDDLBuilder:=TOPDDLBuilder.Create(Nil);
  FDDLBuilder.SQLDialect:=dSQLite;
  FDDLBuilder.Registry:=FRegistry;
  FDDLBuilder.CreateTable(FConn, D.TableName, D.FieldMap.AsArray);
  FSQLBuilder:=TOPSQLBuilder.Create(Nil);
  FSQLBuilder.SQLDialect:=dSQLite;
  FSQLBuilder.Registry:=FRegistry;
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.SQLBuilder:=FSQLBuilder;
  C:=FManager.Connections.Add as TOPConnection;
  C.SingleConnection:=FConn;
  C.IsDefault:=True;
end;

procedure TTestTypeRoundTrip.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FSQLBuilder);
  FreeAndNil(FDDLBuilder);
  FreeAndNil(FRegistry);
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestTypeRoundTrip.TestDateTimeWithMilliseconds;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.Born:=EncodeDateTime(2026,10,15,13,14,15,678);
    L:=SaveAndLoad(O);
    try
      AssertEquals('Milliseconds kept', DateTimeToStr(O.Born)+'.678',
        DateTimeToStr(L.Born)+'.'+IntToStr(MilliSecondOf(L.Born)));
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestDateOnly;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.Day:=EncodeDate(1999,12,31);
    L:=SaveAndLoad(O);
    try
      AssertEquals('Date', EncodeDate(1999,12,31), L.Day, 1e-9);
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestTimeOnly;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.At:=EncodeTime(23,59,59,0);
    L:=SaveAndLoad(O);
    try
      AssertEquals('Time', TimeToStr(EncodeTime(23,59,59,0)), TimeToStr(L.At));
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestCurrencyExact;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.Price:=12345678.1234;
    L:=SaveAndLoad(O);
    try
      AssertTrue('Currency exact: '+CurrToStr(L.Price), L.Price=O.Price);
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestNegativeCurrency;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.Price:=-0.0001;
    L:=SaveAndLoad(O);
    try
      AssertTrue('Smallest negative unit: '+CurrToStr(L.Price), L.Price=O.Price);
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestInt64Large;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.Big:=Int64(1) shl 40 + 1;
    L:=SaveAndLoad(O);
    try
      AssertEquals('Int64 beyond 2^31', O.Big, L.Big);
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestDoubleSmall;
var
  O, L: TDDLTypes;
begin
  O:=TDDLTypes.Create;
  try
    O.Ratio:=1e-300;
    L:=SaveAndLoad(O);
    try
      AssertEquals('Small double', 1e-300, L.Ratio, 1e-310);
    finally
      L.Free;
    end;
  finally
    O.Free;
  end;
end;

procedure TTestTypeRoundTrip.TestCreatedTableUsesNativeTypes;
var
  Q: TSQLQuery;
  S: String;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:='SELECT sql FROM sqlite_master WHERE name=''DDLTypes''';
    Q.Open;
    S:=Q.Fields[0].AsString;
    Q.Close;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  AssertTrue('TIMESTAMP: '+S, Pos('Born TIMESTAMP', S)>0);
  AssertTrue('DATE: '+S, Pos('Day DATE', S)>0);
  AssertTrue('DECIMAL: '+S, Pos('Price DECIMAL(18,4)', S)>0);
end;

initialization
  RegisterTests([TTestDialectDDL, TTestDDLExecRecovery, TTestTypeRoundTrip]);
end.
