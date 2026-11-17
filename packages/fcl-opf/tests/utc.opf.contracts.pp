unit utc.opf.contracts;

{ Tests for the contracts of criteria, SQL builder, driver and manager:
  - merging sub-criteria settings, Where after Where, paging validation
  - array of const parameter conversion (criteria and LoadSQL)
  - paging SQL per dialect, empty INSERT, empty UPDATE
  - parameter names for columns that are not plain identifiers
  - literals, identifier length limits, sequence names }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, System.Contnrs, Data.Db,
  FpcUnit.Test, FpcUnit.TestRegistry, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, Variants, contnrs, DB, fpcunit, testregistry, sqldb,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Criteria, Opf.Driver, Opf.Sql, Opf.DDL,
  Opf.Registry, Opf.Manager, Opf.Persister.RTTI, utc.opf.failconn;

type

  { TTestCriteriaContracts }

  TTestCriteriaContracts = class(TTestCase)
  private
    FCrit: TOPCriteria;
    FSub: TOPCriteria;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Sub-criteria settings
    procedure TestMergeAppendsOrderBy;
    procedure TestMergeSkipsDuplicateOrderBy;
    procedure TestMergeIncludeDeleted;
    procedure TestMergeTakesLimitOffset;
    procedure TestMergeKeepsOwnLimit;
    procedure TestMergeSameLimitAccepted;
    procedure TestMergeConflictingLimitRaises;
    procedure TestMergeConflictingOffsetRaises;
    procedure TestMergeConflictLeavesBothUnchanged;
    procedure TestMergeSettingsOnlySub;
    procedure TestMergeCopyKeepsSubSettings;
    // Where after Where
    procedure TestSecondWhereResetsParams;
    procedure TestSecondWhereResetsConditions;
    procedure TestSecondWhereKeepsSettings;
    procedure TestWhereRawAfterWhere;
    procedure TestWhereInAfterWhere;
    // Paging validation
    procedure TestLimitMinusOneAccepted;
    procedure TestLimitBelowMinusOneRaises;
    procedure TestOffsetBelowMinusOneRaises;
    // Array of const values
    procedure TestVarRecInteger;
    procedure TestVarRecInt64;
    procedure TestVarRecQWord;
    procedure TestVarRecQWordTooLargeRaises;
    procedure TestVarRecCurrency;
    procedure TestVarRecExtended;
    procedure TestVarRecBoolean;
    procedure TestVarRecUnicodeString;
    procedure TestVarRecWideChar;
    procedure TestVarRecPWideChar;
    procedure TestVarRecPChar;
    procedure TestVarRecVariantInteger;
    procedure TestVarRecVariantString;
    procedure TestVarRecVariantDate;
    procedure TestVarRecVariantNull;
    procedure TestVarRecUnsupportedRaises;
    procedure TestVarRecFailureKeepsParamCount;
    procedure TestBetweenCountRaisesEOPF;
  end;

  { TTestDriverContracts }

  TTestDriverContracts = class(TTestCase)
  private
    function LimitSQL(aDialect: TSQLDialect; aLimit, aOffset: Integer): UTF8String;
    function Drv(aDialect: TSQLDialect): TSQLDriver;
  published
    procedure TestLimitOffsetDefault;
    procedure TestOffsetOnlySQLite;
    procedure TestOffsetOnlyMySQL;
    procedure TestOffsetOnlyPostgreSQL;
    procedure TestLimitZeroEmitsNothing;
    procedure TestFirebirdRows;
    procedure TestFirebirdRowsNoOverflow;
    procedure TestFirebird3OffsetFetch;
    procedure TestEmptyInsertDefault;
    procedure TestEmptyInsertMySQL;
    procedure TestEmptyInsertOracle;
    procedure TestQuoteStringLiteral;
    procedure TestQuoteStringLiteralMySQLBackslash;
    procedure TestDateTimeLiterals;
    procedure TestDateTimeLiteralIgnoresLocale;
    procedure TestDateTimeLiteralOracle;
    procedure TestMaxIdentifierLength;
    procedure TestTransactionalDDLFlags;
    procedure TestSameBatchResetSQL;
    procedure TestAlterCapabilities;
    procedure TestSequenceNamesFormatted;
    procedure TestSequenceNameQualified;
    procedure TestSequenceNameUnsafeRaises;
    procedure TestShortenIdentifier;
    procedure TestShortenIdentifierKeepsUTF8;
  end;

  { TTestSQLBuilderContracts }

  TTestSQLBuilderContracts = class(TTestCase)
  private
    FBuilder: TOPSQLBuilder;
    FMap: TOPFieldMapList;
    function AddField(const aName: UTF8String; aOptions: TOPFieldOptions): TOPFieldMap;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAllAutoIncInsertSQLite;
    procedure TestAllAutoIncInsertPostgreSQLReturning;
    procedure TestAllAutoIncInsertMySQL;
    procedure TestEmptyUpdateRaises;
    procedure TestPlainParamNameUnchanged;
    procedure TestSpacedColumnParamName;
    procedure TestReservedPrefixParamName;
    procedure TestSimilarNamesDistinctParams;
    procedure TestSoftDeleteParamName;
  end;

  { TCtrItem }

  TCtrItem = class(TPersistent, IOPPersistenceState)
  private
    FCode: Integer;
    FID: Integer;
    FName: String;
    FOpfName: String;
    FSpaced: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Spaced: String read FSpaced write FSpaced;
    property OpfName: String read FOpfName write FOpfName;
    property Code: Integer read FCode write FCode;
  end;

  { TCtrKeyOnly }

  TCtrKeyOnly = class(TPersistent)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestManagerContracts }

  TTestManagerContracts = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FSQLBuilder: TOPSQLBuilder;
    FDDLBuilder: TOPDDLBuilder;
    FManager: TOPManager;
    FItemDef: TPersisterDef;
    function NewItem(const aName: String): TCtrItem;
    function CountRows(const aSQL: String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLimitZeroLoadsNothing;
    procedure TestLimitZeroRunsNoQuery;
    procedure TestOffsetOnlyLoads;
    procedure TestSpecialColumnNamesRoundTrip;
    procedure TestUpdateSpecialColumnNames;
    procedure TestAllAutoIncInsert;
    procedure TestReadOnlyChangeNeedsNoUpdate;
    procedure TestLoadSQLUnicodeParam;
    procedure TestLoadSQLCurrencyAndVariantParams;
  end;

implementation

{ 'Zoë' as UTF-8, built from bytes so no source codepage conversion applies }

function NonAsciiName: UTF8String;
const
  Bytes: array[0..3] of Byte = (Ord('Z'), Ord('o'), $C3, $AB);
var
  I: Integer;
begin
  SetLength(Result, Length(Bytes));
  for I:=0 to High(Bytes) do
    Result[I+1]:=AnsiChar(Bytes[I]);
end;

{ The bytes of a string in hex: compares without any code page conversion }

function Hex(const S: RawByteString): String;
var
  I: Integer;
begin
  Result:='';
  for I:=1 to Length(S) do
    Result:=Result+IntToHex(Ord(S[I]), 2);
end;

{ TTestCriteriaContracts }

procedure TTestCriteriaContracts.SetUp;
begin
  inherited SetUp;
  FCrit:=TOPCriteria.Create;
  FSub:=TOPCriteria.Create;
end;

procedure TTestCriteriaContracts.TearDown;
begin
  FreeAndNil(FSub);
  FreeAndNil(FCrit);
  inherited TearDown;
end;

procedure TTestCriteriaContracts.TestMergeAppendsOrderBy;
begin
  FCrit.Where('A', opEqual, 1).OrderBy('A');
  FSub.Where('B', opEqual, 2).OrderBy('B', True);
  FCrit.AddAndCopy(FSub);
  AssertEquals('ORDER BY A, B DESC', FCrit.ToOrderBySQL(Nil));
end;

procedure TTestCriteriaContracts.TestMergeSkipsDuplicateOrderBy;
begin
  FCrit.Where('A', opEqual, 1).OrderBy('A', True);
  FSub.Where('B', opEqual, 2).OrderBy('a').OrderBy('B');
  FCrit.AddAndCopy(FSub);
  AssertEquals('Own direction wins', 'ORDER BY A DESC, B', FCrit.ToOrderBySQL(Nil));
end;

procedure TTestCriteriaContracts.TestMergeIncludeDeleted;
begin
  FCrit.Where('A', opEqual, 1);
  FSub.Where('B', opEqual, 2).IncludeDeleted;
  AssertFalse('Before', FCrit.HasIncludeDeleted);
  FCrit.AddOrCopy(FSub);
  AssertTrue('After', FCrit.HasIncludeDeleted);
end;

procedure TTestCriteriaContracts.TestMergeTakesLimitOffset;
begin
  FCrit.Where('A', opEqual, 1);
  FSub.Where('B', opEqual, 2).Limit(5).Offset(10);
  FCrit.AddAndCopy(FSub);
  AssertEquals('Limit', 5, FCrit.GetLimitValue);
  AssertEquals('Offset', 10, FCrit.GetOffsetValue);
end;

procedure TTestCriteriaContracts.TestMergeKeepsOwnLimit;
begin
  FCrit.Where('A', opEqual, 1).Limit(3);
  FSub.Where('B', opEqual, 2).Offset(7);
  FCrit.AddAndCopy(FSub);
  AssertEquals('Own limit', 3, FCrit.GetLimitValue);
  AssertEquals('Sub offset', 7, FCrit.GetOffsetValue);
end;

procedure TTestCriteriaContracts.TestMergeSameLimitAccepted;
begin
  FCrit.Where('A', opEqual, 1).Limit(3);
  FSub.Where('B', opEqual, 2).Limit(3);
  FCrit.AddAndCopy(FSub);
  AssertEquals(3, FCrit.GetLimitValue);
end;

procedure TTestCriteriaContracts.TestMergeConflictingLimitRaises;
var
  Raised: Boolean;
begin
  FCrit.Where('A', opEqual, 1).Limit(3);
  FSub.Where('B', opEqual, 2).Limit(4);
  Raised:=False;
  try
    FCrit.AddAndCopy(FSub);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Conflicting limits raise', Raised);
end;

procedure TTestCriteriaContracts.TestMergeConflictingOffsetRaises;
var
  Raised: Boolean;
begin
  FCrit.Where('A', opEqual, 1).Offset(3);
  FSub.Where('B', opEqual, 2).Offset(4);
  Raised:=False;
  try
    FCrit.AddOrCopy(FSub);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Conflicting offsets raise', Raised);
end;

procedure TTestCriteriaContracts.TestMergeConflictLeavesBothUnchanged;
var
  Sub: TOPCriteria;
begin
  FCrit.Where('A', opEqual, 1).Limit(3);
  Sub:=TOPCriteria.Create;
  Sub.Where('B', opEqual, 2).Limit(4).OrderBy('B').IncludeDeleted;
  try
    // The sub is freed when the merge raises (heaptrc reports a leak otherwise)
    FCrit.AddAnd(Sub);
    Fail('Expected EOPF');
  except
    on E: EOPF do ;
  end;
  AssertEquals('Where unchanged', 'A = :p1', FCrit.ToWhereSQL(Nil));
  AssertEquals('Params unchanged', 1, FCrit.Params.Count);
  AssertEquals('No order', '', FCrit.ToOrderBySQL(Nil));
  AssertFalse('No IncludeDeleted', FCrit.HasIncludeDeleted);
end;

procedure TTestCriteriaContracts.TestMergeSettingsOnlySub;
begin
  FCrit.Where('A', opEqual, 1);
  FSub.OrderBy('A', True).Limit(2).IncludeDeleted;
  FCrit.AddAndCopy(FSub);
  AssertEquals('No condition added', 'A = :p1', FCrit.ToWhereSQL(Nil));
  AssertEquals('Order taken', 'ORDER BY A DESC', FCrit.ToOrderBySQL(Nil));
  AssertEquals('Limit taken', 2, FCrit.GetLimitValue);
  AssertTrue('IncludeDeleted taken', FCrit.HasIncludeDeleted);
end;

procedure TTestCriteriaContracts.TestMergeCopyKeepsSubSettings;
begin
  FCrit.Where('A', opEqual, 1);
  FSub.Where('B', opEqual, 2).OrderBy('B').Limit(2);
  FCrit.AddAndCopy(FSub);
  AssertEquals('Sub order', 'ORDER BY B', FSub.ToOrderBySQL(Nil));
  AssertEquals('Sub limit', 2, FSub.GetLimitValue);
end;

procedure TTestCriteriaContracts.TestSecondWhereResetsParams;
begin
  FCrit.Where('A', opEqual, 1).AddAnd('B', opEqual, 'x');
  FCrit.Where('C', opEqual, 3);
  AssertEquals('One param', 1, FCrit.Params.Count);
  AssertEquals('Renumbered', 'p1', FCrit.Params[0].Name);
  AssertEquals('Value', 3, FCrit.Params[0].AsInteger);
end;

procedure TTestCriteriaContracts.TestSecondWhereResetsConditions;
begin
  FCrit.Where('A', opEqual, 1).AddOr('B', opEqual, 2);
  FCrit.Where('C', opEqual, 3).AddAnd('D', opEqual, 4);
  AssertEquals('C = :p1 AND D = :p2', FCrit.ToWhereSQL(Nil));
end;

procedure TTestCriteriaContracts.TestSecondWhereKeepsSettings;
begin
  FCrit.Where('A', opEqual, 1).OrderBy('A').Limit(5).IncludeDeleted;
  FCrit.Where('B', opEqual, 2);
  AssertEquals('Order', 'ORDER BY A', FCrit.ToOrderBySQL(Nil));
  AssertEquals('Limit', 5, FCrit.GetLimitValue);
  AssertTrue('IncludeDeleted', FCrit.HasIncludeDeleted);
end;

procedure TTestCriteriaContracts.TestWhereRawAfterWhere;
begin
  FCrit.Where('A', opEqual, 1);
  FCrit.WhereRaw('X > ?', [5]);
  AssertEquals('(X > :p1)', FCrit.ToWhereSQL(Nil));
  AssertEquals(1, FCrit.Params.Count);
end;

procedure TTestCriteriaContracts.TestWhereInAfterWhere;
begin
  FCrit.Where('A', opEqual, 1);
  FCrit.WhereIn('B', [1, 2]);
  AssertEquals('B IN (:p1,:p2)', FCrit.ToWhereSQL(Nil));
  AssertEquals(2, FCrit.Params.Count);
end;

procedure TTestCriteriaContracts.TestLimitMinusOneAccepted;
begin
  FCrit.Limit(5).Limit(-1).Offset(-1);
  AssertEquals('Limit', -1, FCrit.GetLimitValue);
  AssertEquals('Offset', -1, FCrit.GetOffsetValue);
end;

procedure TTestCriteriaContracts.TestLimitBelowMinusOneRaises;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FCrit.Limit(-2);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
  AssertEquals('Unchanged', -1, FCrit.GetLimitValue);
end;

procedure TTestCriteriaContracts.TestOffsetBelowMinusOneRaises;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FCrit.Offset(-5);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestCriteriaContracts.TestVarRecInteger;
begin
  FCrit.WhereIn('A', [42]);
  AssertTrue('Type', FCrit.Params[0].DataType=ftInteger);
  AssertEquals(42, FCrit.Params[0].AsInteger);
end;

procedure TTestCriteriaContracts.TestVarRecInt64;
begin
  FCrit.WhereIn('A', [Int64(1) shl 40]);
  AssertTrue('Type', FCrit.Params[0].DataType=ftLargeint);
  AssertEquals(Int64(1) shl 40, FCrit.Params[0].AsLargeInt);
end;

procedure TTestCriteriaContracts.TestVarRecQWord;
var
  Q: QWord;
begin
  Q:=QWord(High(Int64));
  FCrit.WhereIn('A', [Q]);
  AssertTrue('Type', FCrit.Params[0].DataType=ftLargeint);
  AssertEquals(High(Int64), FCrit.Params[0].AsLargeInt);
end;

procedure TTestCriteriaContracts.TestVarRecQWordTooLargeRaises;
var
  Q: QWord;
  Raised: Boolean;
begin
  Q:=QWord(High(Int64))+1;
  Raised:=False;
  try
    FCrit.WhereIn('A', [Q]);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestCriteriaContracts.TestVarRecCurrency;
var
  C: Currency;
begin
  C:=123456789012.3456;
  FCrit.WhereIn('A', [C]);
  AssertTrue('Exact type', FCrit.Params[0].DataType=ftBCD);
  AssertEquals('Exact value', '123456789012.3456', CurrToStr(FCrit.Params[0].AsCurrency, DefaultFormatSettings));
end;

procedure TTestCriteriaContracts.TestVarRecExtended;
begin
  FCrit.WhereIn('A', [1.5]);
  AssertTrue('Type', FCrit.Params[0].DataType=ftFloat);
  AssertEquals(1.5, FCrit.Params[0].AsFloat, 0);
end;

procedure TTestCriteriaContracts.TestVarRecBoolean;
begin
  FCrit.WhereIn('A', [True]);
  AssertTrue('Type', FCrit.Params[0].DataType=ftBoolean);
  AssertTrue(FCrit.Params[0].AsBoolean);
end;

procedure TTestCriteriaContracts.TestVarRecUnicodeString;
var
  U: UnicodeString;
begin
  U:=UTF8Decode(NonAsciiName);
  FCrit.WhereIn('A', [U]);
  AssertTrue('Unicode text', FCrit.Params[0].AsUnicodeString=UTF8Decode(NonAsciiName));
end;

procedure TTestCriteriaContracts.TestVarRecWideChar;
var
  W: WideChar;
begin
  W:=WideChar($EB);
  FCrit.WhereIn('A', [W]);
  AssertEquals('Character', $EB, Ord(FCrit.Params[0].AsUnicodeString[1]));
end;

procedure TTestCriteriaContracts.TestVarRecPWideChar;
var
  U: UnicodeString;
begin
  U:=UTF8Decode(NonAsciiName);
  FCrit.WhereIn('A', [PWideChar(U)]);
  AssertTrue(FCrit.Params[0].AsUnicodeString=UTF8Decode(NonAsciiName));
end;

procedure TTestCriteriaContracts.TestVarRecPChar;
var
  S: AnsiString;
begin
  S:='abc';
  FCrit.WhereIn('A', [PChar(S)]);
  AssertEquals('abc', FCrit.Params[0].AsString);
end;

procedure TTestCriteriaContracts.TestVarRecVariantInteger;
var
  V: Variant;
begin
  V:=7;
  FCrit.WhereIn('A', [V]);
  AssertEquals(7, FCrit.Params[0].AsInteger);
end;

procedure TTestCriteriaContracts.TestVarRecVariantString;
var
  V: Variant;
begin
  V:=UTF8Decode(NonAsciiName);
  FCrit.WhereIn('A', [V]);
  AssertTrue(FCrit.Params[0].AsUnicodeString=UTF8Decode(NonAsciiName));
end;

procedure TTestCriteriaContracts.TestVarRecVariantDate;
var
  V: Variant;
  D: TDateTime;
begin
  D:=EncodeDate(2024, 2, 29)+EncodeTime(13, 14, 15, 0);
  V:=D;
  FCrit.WhereIn('A', [V]);
  AssertTrue('Type', FCrit.Params[0].DataType=ftDateTime);
  AssertEquals(D, FCrit.Params[0].AsDateTime, 1e-9);
end;

procedure TTestCriteriaContracts.TestVarRecVariantNull;
var
  V: Variant;
begin
  V:=Null;
  FCrit.WhereIn('A', [V]);
  AssertTrue(FCrit.Params[0].IsNull);
end;

procedure TTestCriteriaContracts.TestVarRecUnsupportedRaises;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FCrit.WhereIn('A', [TObject(Nil)]);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
end;

procedure TTestCriteriaContracts.TestVarRecFailureKeepsParamCount;
begin
  FCrit.Where('A', opEqual, 1);
  try
    FCrit.AddAndIn('B', [2, TObject(Nil)]);
  except
    on E: EOPF do ;
  end;
  // The successfully added value stays, the failed one is removed
  AssertEquals('Params', 2, FCrit.Params.Count);
  FCrit.AddAnd('C', opEqual, 3);
  AssertEquals('Next name continues', 'p3', FCrit.Params[2].Name);
end;

procedure TTestCriteriaContracts.TestBetweenCountRaisesEOPF;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FCrit.WhereBetween('A', [1]);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('WhereBetween', Raised);
  Raised:=False;
  try
    FCrit.AddAndBetween('A', [1, 2, 3]);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('AddAndBetween', Raised);
end;

{ TTestDriverContracts }

function TTestDriverContracts.Drv(aDialect: TSQLDialect): TSQLDriver;
begin
  Result:=TSQLDriver.CreateDriver(aDialect);
end;

function TTestDriverContracts.LimitSQL(aDialect: TSQLDialect; aLimit, aOffset: Integer): UTF8String;
var
  D: TSQLDriver;
begin
  D:=Drv(aDialect);
  try
    Result:=D.GetLimitOffsetSQL(aLimit, aOffset);
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestLimitOffsetDefault;
begin
  AssertEquals('Both', 'LIMIT 5 OFFSET 10', LimitSQL(dSQLite, 5, 10));
  AssertEquals('Limit', 'LIMIT 5', LimitSQL(dPostGres, 5, -1));
  AssertEquals('None', '', LimitSQL(dMySQL, -1, -1));
end;

procedure TTestDriverContracts.TestOffsetOnlySQLite;
begin
  AssertEquals('LIMIT -1 OFFSET 10', LimitSQL(dSQLite, -1, 10));
end;

procedure TTestDriverContracts.TestOffsetOnlyMySQL;
begin
  AssertEquals('LIMIT 18446744073709551615 OFFSET 10', LimitSQL(dMySQL, -1, 10));
end;

procedure TTestDriverContracts.TestOffsetOnlyPostgreSQL;
begin
  AssertEquals('OFFSET 10', LimitSQL(dPostGres, -1, 10));
end;

procedure TTestDriverContracts.TestLimitZeroEmitsNothing;
begin
  // The manager does not query at all for Limit(0)
  AssertEquals('', LimitSQL(dSQLite, 0, 0));
end;

procedure TTestDriverContracts.TestFirebirdRows;
begin
  AssertEquals('Both', 'ROWS 11 TO 15', LimitSQL(dFirebird, 5, 10));
  AssertEquals('Limit', 'ROWS 1 TO 5', LimitSQL(dFirebird, 5, -1));
end;

procedure TTestDriverContracts.TestFirebirdRowsNoOverflow;
begin
  AssertEquals('Offset+limit', 'ROWS 2147483648 TO 2147483657', LimitSQL(dFirebird, 10, High(Integer)));
  AssertEquals('Offset only', 'ROWS 2147483648 TO 9223372036854775807', LimitSQL(dFirebird, -1, High(Integer)));
end;

procedure TTestDriverContracts.TestFirebird3OffsetFetch;
begin
  AssertEquals('OFFSET 10 ROWS FETCH NEXT 5 ROWS ONLY', LimitSQL(dFirebird3, 5, 10));
end;

procedure TTestDriverContracts.TestEmptyInsertDefault;
var
  D: TSQLDriver;
begin
  D:=Drv(dPostGres);
  try
    AssertEquals('INSERT INTO t DEFAULT VALUES', D.GetEmptyInsertSQL('t', 'id'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestEmptyInsertMySQL;
var
  D: TSQLDriver;
begin
  D:=Drv(dMySQL);
  try
    AssertEquals('INSERT INTO t () VALUES ()', D.GetEmptyInsertSQL('t', 'id'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestEmptyInsertOracle;
var
  D: TSQLDriver;
begin
  D:=Drv(dOracle);
  try
    AssertEquals('INSERT INTO t (id) VALUES (DEFAULT)', D.GetEmptyInsertSQL('t', 'id'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestQuoteStringLiteral;
var
  D: TSQLDriver;
begin
  D:=Drv(dPostGres);
  try
    AssertEquals('''it''''s \n''', D.QuoteStringLiteral('it''s \n'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestQuoteStringLiteralMySQLBackslash;
var
  D: TSQLDriver;
begin
  D:=Drv(dMySQL);
  try
    AssertEquals('''it''''s \\n''', D.QuoteStringLiteral('it''s \n'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestDateTimeLiterals;
var
  D: TSQLDriver;
  V: TDateTime;
begin
  V:=EncodeDate(2024, 2, 29)+EncodeTime(13, 4, 5, 6);
  D:=Drv(dPostGres);
  try
    AssertEquals('DateTime', '''2024-02-29 13:04:05.006''', D.GetDateTimeLiteral(fkDateTime, V));
    AssertEquals('Date', '''2024-02-29''', D.GetDateTimeLiteral(fkDate, V));
    AssertEquals('Time', '''13:04:05.006''', D.GetDateTimeLiteral(fkTime, V));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestDateTimeLiteralIgnoresLocale;
var
  D: TSQLDriver;
  Saved: TFormatSettings;
begin
  Saved:=DefaultFormatSettings;
  D:=Drv(dSQLite);
  try
    DefaultFormatSettings.DateSeparator:='/';
    DefaultFormatSettings.TimeSeparator:='.';
    DefaultFormatSettings.DecimalSeparator:=',';
    AssertEquals('''2024-02-29 13:04:05.006''',
      D.GetDateTimeLiteral(fkDateTime, EncodeDate(2024, 2, 29)+EncodeTime(13, 4, 5, 6)));
  finally
    DefaultFormatSettings:=Saved;
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestDateTimeLiteralOracle;
var
  D: TSQLDriver;
begin
  D:=Drv(dOracle);
  try
    AssertEquals('Date', 'DATE ''2024-02-29''', D.GetDateTimeLiteral(fkDate, EncodeDate(2024, 2, 29)));
    AssertEquals('DateTime', 'TIMESTAMP ''2024-02-29 00:00:00.000''', D.GetDateTimeLiteral(fkDateTime, EncodeDate(2024, 2, 29)));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestMaxIdentifierLength;

  procedure Check(aDialect: TSQLDialect; aExpected: Integer);
  var
    D: TSQLDriver;
  begin
    D:=Drv(aDialect);
    try
      AssertEquals(aDialect.ToString, aExpected, D.MaxIdentifierLength);
    finally
      D.Free;
    end;
  end;

begin
  Check(dSQLite, 0);
  Check(dPostGres, 63);
  Check(dMySQL, 64);
  Check(dFirebird, 31);
  Check(dFirebird3, 31);
  Check(dSQLServer, 128);
  Check(dOracle, 30);
end;

procedure TTestDriverContracts.TestTransactionalDDLFlags;

  procedure Check(aDialect: TSQLDialect; aExpected: Boolean);
  var
    D: TSQLDriver;
  begin
    D:=Drv(aDialect);
    try
      AssertEquals(aDialect.ToString, aExpected, D.SupportsTransactionalDDL);
    finally
      D.Free;
    end;
  end;

begin
  Check(dSQLite, True);
  Check(dPostGres, True);
  Check(dMySQL, False);
  Check(dFirebird3, False);
  Check(dOracle, False);
  Check(dSQLServer, True);
end;

procedure TTestDriverContracts.TestSameBatchResetSQL;
var
  D: TSQLDriver;
begin
  D:=Drv(dSQLServer);
  try
    AssertEquals('SQL Server', 'SET NOCOUNT OFF', D.GetSameBatchResetSQL);
  finally
    D.Free;
  end;
  D:=Drv(dSQLite);
  try
    AssertEquals('SQLite', '', D.GetSameBatchResetSQL);
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestAlterCapabilities;
var
  D: TSQLDriver;
begin
  D:=Drv(dSQLite);
  try
    AssertFalse('SQLite type', D.CanAlterColumnType);
    AssertFalse('SQLite null', D.CanAlterNullability);
  finally
    D.Free;
  end;
  D:=Drv(dFirebird);
  try
    AssertTrue('FB2.5 type', D.CanAlterColumnType);
    AssertFalse('FB2.5 null', D.CanAlterNullability);
  finally
    D.Free;
  end;
  D:=Drv(dFirebird3);
  try
    AssertTrue('FB3 null', D.CanAlterNullability);
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestSequenceNamesFormatted;
var
  D: TSQLDriver;
begin
  D:=Drv(dPostGres);
  try
    AssertEquals('PG plain', 'SELECT nextval(''my_seq'')', D.GetSequenceNextValSQL('my_seq'));
    AssertEquals('PG irregular name', 'SELECT nextval(''"my seq"'')', D.GetSequenceNextValSQL('my seq'));
  finally
    D.Free;
  end;
  D:=Drv(dFirebird3);
  try
    AssertEquals('FB', 'SELECT NEXT VALUE FOR "my seq" FROM RDB$DATABASE', D.GetSequenceNextValSQL('my seq'));
  finally
    D.Free;
  end;
  D:=Drv(dOracle);
  try
    AssertEquals('Oracle', 'SELECT my_seq.NEXTVAL FROM DUAL', D.GetSequenceNextValSQL('my_seq'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestSequenceNameQualified;
var
  D: TSQLDriver;
begin
  D:=Drv(dPostGres);
  try
    AssertEquals('SELECT nextval(''app.my_seq'')', D.GetSequenceNextValSQL('app.my_seq'));
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestSequenceNameUnsafeRaises;
var
  D: TSQLDriver;
  Raised: Boolean;
begin
  D:=Drv(dPostGres);
  try
    Raised:=False;
    try
      D.GetSequenceNextValSQL('s''); DROP TABLE x; --');
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue(Raised);
  finally
    D.Free;
  end;
end;

procedure TTestDriverContracts.TestShortenIdentifier;
var
  Long, S1, S2: UTF8String;
begin
  AssertEquals('Fits', 'fk_a_b', OPFShortenIdentifier('fk_a_b', 31));
  AssertEquals('No limit', StringOfChar('x', 100), OPFShortenIdentifier(StringOfChar('x', 100), 0));
  Long:='fk_' + StringOfChar('a', 40) + '_parent';
  S1:=OPFShortenIdentifier(Long, 31);
  AssertEquals('Length', 31, Length(S1));
  AssertEquals('Deterministic', S1, OPFShortenIdentifier(Long, 31));
  S2:=OPFShortenIdentifier('fk_' + StringOfChar('a', 40) + '_parenx', 31);
  AssertTrue('Distinct names stay distinct', S1<>S2);
end;

procedure TTestDriverContracts.TestShortenIdentifierKeepsUTF8;
var
  Long, S: UTF8String;
  I: Integer;
begin
  // 'ë' (2 bytes) repeated: the cut must not split a sequence
  Long:='';
  for I:=1 to 20 do
    Long:=Long + #$C3#$AB;
  S:=OPFShortenIdentifier(Long, 30);
  AssertTrue('Fits', Length(S)<=30);
  // 30-9 = 21 bytes would cut the 11th character: 20 bytes are kept
  AssertEquals('Whole characters', 20+9, Length(S));
end;

{ TTestSQLBuilderContracts }

function TTestSQLBuilderContracts.AddField(const aName: UTF8String; aOptions: TOPFieldOptions): TOPFieldMap;
begin
  Result:=FMap.Add as TOPFieldMap;
  Result.FieldName:=aName;
  Result.PropertyName:=aName;
  Result.PropertyKind:=tkInteger;
  Result.Options:=aOptions;
end;

procedure TTestSQLBuilderContracts.SetUp;
begin
  inherited SetUp;
  FBuilder:=TOPSQLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dSQLite;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestSQLBuilderContracts.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FBuilder);
  inherited TearDown;
end;

procedure TTestSQLBuilderContracts.TestAllAutoIncInsertSQLite;
begin
  AddField('id', [foInKey, foAutoInc, foInInsert, foInUpdate]);
  AssertEquals('INSERT INTO t DEFAULT VALUES', FBuilder.GetInsertSQL('t', FMap.AsArray));
end;

procedure TTestSQLBuilderContracts.TestAllAutoIncInsertPostgreSQLReturning;
begin
  FBuilder.SQLDialect:=dPostGres;
  AddField('id', [foInKey, foAutoInc, foInInsert]);
  AssertEquals('INSERT INTO t DEFAULT VALUES RETURNING id', FBuilder.GetInsertSQL('t', FMap.AsArray));
end;

procedure TTestSQLBuilderContracts.TestAllAutoIncInsertMySQL;
begin
  FBuilder.SQLDialect:=dMySQL;
  AddField('id', [foInKey, foAutoInc, foInInsert]);
  AssertEquals('INSERT INTO t () VALUES ()', FBuilder.GetInsertSQL('t', FMap.AsArray));
end;

procedure TTestSQLBuilderContracts.TestEmptyUpdateRaises;
var
  Raised: Boolean;
begin
  AddField('id', [foInKey]);
  AddField('ro', [foInInsert]);
  Raised:=False;
  try
    FBuilder.GetUpdateSQL('t', FMap.AsArray);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('No SET list raises', Raised);
end;

procedure TTestSQLBuilderContracts.TestPlainParamNameUnchanged;
var
  F: TOPFieldMap;
begin
  F:=AddField('Name_1', [foInInsert]);
  AssertEquals('Name_1', FBuilder.FieldParamName(F));
  AssertEquals('INSERT INTO t (Name_1) VALUES (:Name_1)', FBuilder.GetInsertSQL('t', FMap.AsArray));
end;

procedure TTestSQLBuilderContracts.TestSpacedColumnParamName;
var
  F: TOPFieldMap;
  P: String;
begin
  F:=AddField('My Col', [foInInsert]);
  P:=FBuilder.FieldParamName(F);
  AssertEquals('Prefix', 'opf_c_My_Col_', Copy(P, 1, 13));
  AssertEquals('Hash length', 13+8, Length(P));
  AssertEquals('INSERT INTO t ("My Col") VALUES (:'+P+')', FBuilder.GetInsertSQL('t', FMap.AsArray));
end;

procedure TTestSQLBuilderContracts.TestReservedPrefixParamName;
var
  F, G: TOPFieldMap;
begin
  F:=AddField('opf_sd', [foInInsert]);
  G:=AddField('_opf_oldversion', [foInInsert]);
  AssertTrue('opf_ column renamed', FBuilder.FieldParamName(F)<>'opf_sd');
  AssertTrue('_opf_ column renamed', FBuilder.FieldParamName(G)<>'_opf_oldversion');
  AssertEquals('Same name in SQL and binding', FBuilder.FieldParamName(F), OPFFieldParamName('opf_sd'));
end;

procedure TTestSQLBuilderContracts.TestSimilarNamesDistinctParams;
var
  F, G: TOPFieldMap;
begin
  F:=AddField('a b', [foInInsert]);
  G:=AddField('a-b', [foInInsert]);
  AssertTrue(FBuilder.FieldParamName(F)<>FBuilder.FieldParamName(G));
end;

procedure TTestSQLBuilderContracts.TestSoftDeleteParamName;
var
  SD: TOPFieldMap;
begin
  AddField('id', [foInKey]);
  SD:=AddField('deleted', [foInUpdate]);
  AssertEquals('UPDATE t SET deleted = :opf_sd WHERE (id=:id)',
    FBuilder.GetSoftDeleteSQL('t', FMap.AsArray, SD));
end;

{ TCtrItem }

function TCtrItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TCtrItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestManagerContracts }

function TTestManagerContracts.NewItem(const aName: String): TCtrItem;
begin
  Result:=TCtrItem.Create;
  Result.Name:=aName;
  Result.Spaced:='s-' + aName;
  Result.OpfName:='o-' + aName;
  Result.Code:=7;
  Result.State:=opsNew;
end;

function TTestManagerContracts.CountRows(const aSQL: String): Integer;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=aSQL;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
  finally
    Q.Free;
  end;
  if FTrans.Active then
    FTrans.Commit;
end;

procedure TTestManagerContracts.SetUp;
var
  C: TOPConnection;
  F: TOPFieldMap;
  D: TPersisterDef;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FRegistry:=TOPRegistry.Create(Nil);
  FItemDef:=FRegistry.RegisterPersister(TCtrItem, TRTTIPersister);
  F:=FItemDef.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  FItemDef.FieldMap.FindByPropertyName('Spaced').FieldName:='My Col';
  FItemDef.FieldMap.FindByPropertyName('OpfName').FieldName:='opf_sd';
  // Code is written on insert only
  F:=FItemDef.FieldMap.FindByPropertyName('Code');
  F.Options:=F.Options-[foInUpdate];
  D:=FRegistry.RegisterPersister(TCtrKeyOnly, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  FDDLBuilder:=TOPDDLBuilder.Create(Nil);
  FDDLBuilder.SQLDialect:=dSQLite;
  FDDLBuilder.Registry:=FRegistry;
  FDDLBuilder.CreateTable(FConn, FItemDef.TableName, FItemDef.FieldMap.AsArray);
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

procedure TTestManagerContracts.TearDown;
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

procedure TTestManagerContracts.TestLimitZeroLoadsNothing;
var
  L: TFPObjectList;
  C: TOPCriteria;
  I: Integer;
  Item: TCtrItem;
begin
  for I:=1 to 3 do
  begin
    Item:=NewItem('n'+IntToStr(I));
    try
      FManager.Save(Item);
    finally
      Item.Free;
    end;
  end;
  L:=TFPObjectList.Create(True);
  C:=TOPCriteria.Create;
  try
    C.Limit(0);
    FManager.Load(L, TCtrItem, C);
    AssertEquals('No rows', 0, L.Count);
    C.Limit(2);
    FManager.Load(L, TCtrItem, C);
    AssertEquals('Limit 2', 2, L.Count);
  finally
    C.Free;
    L.Free;
  end;
end;

procedure TTestManagerContracts.TestLimitZeroRunsNoQuery;
var
  L: TFPObjectList;
  C: TOPCriteria;
begin
  L:=TFPObjectList.Create(True);
  C:=TOPCriteria.Create;
  try
    C.Limit(0);
    // Any statement would fail on this connection
    FConn.FailStart:=True;
    FManager.Load(L, TCtrItem, C);
    AssertEquals(0, L.Count);
  finally
    FConn.FailStart:=False;
    C.Free;
    L.Free;
  end;
end;

procedure TTestManagerContracts.TestOffsetOnlyLoads;
var
  L: TFPObjectList;
  C: TOPCriteria;
  I: Integer;
  Item: TCtrItem;
begin
  for I:=1 to 3 do
  begin
    Item:=NewItem('n'+IntToStr(I));
    try
      FManager.Save(Item);
    finally
      Item.Free;
    end;
  end;
  L:=TFPObjectList.Create(True);
  C:=TOPCriteria.Create;
  try
    // SQLite needs a LIMIT before OFFSET
    C.OrderBy('ID').Offset(1);
    FManager.Load(L, TCtrItem, C);
    AssertEquals('Rows after offset', 2, L.Count);
    AssertEquals('First', 'n2', TCtrItem(L[0]).Name);
  finally
    C.Free;
    L.Free;
  end;
end;

procedure TTestManagerContracts.TestSpecialColumnNamesRoundTrip;
var
  A, B: TCtrItem;
begin
  A:=NewItem('x');
  B:=TCtrItem.Create;
  try
    FManager.Save(A);
    AssertTrue('ID assigned', A.ID>0);
    B.ID:=A.ID;
    FManager.Load(B);
    AssertEquals('Spaced column', 's-x', B.Spaced);
    AssertEquals('opf_ column', 'o-x', B.OpfName);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerContracts.TestUpdateSpecialColumnNames;
var
  A, B: TCtrItem;
begin
  A:=NewItem('x');
  B:=TCtrItem.Create;
  try
    FManager.Save(A);
    A.Spaced:='changed';
    A.OpfName:='changed too';
    A.State:=opsUpdate;
    FManager.Save(A);
    B.ID:=A.ID;
    FManager.Load(B);
    AssertEquals('Spaced column', 'changed', B.Spaced);
    AssertEquals('opf_ column', 'changed too', B.OpfName);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerContracts.TestAllAutoIncInsert;
var
  K: TCtrKeyOnly;
begin
  K:=TCtrKeyOnly.Create;
  try
    FManager.Save(K, opsNew);
    AssertEquals('First ID', 1, K.ID);
    K.ID:=0;
    FManager.Save(K, opsNew);
    AssertEquals('Second ID', 2, K.ID);
  finally
    K.Free;
  end;
  AssertEquals('Rows', 2, CountRows('SELECT COUNT(*) FROM '+FRegistry.FindPersister(TCtrKeyOnly).TableName));
end;

procedure TTestManagerContracts.TestReadOnlyChangeNeedsNoUpdate;
var
  A, B: TCtrItem;
begin
  A:=NewItem('x');
  B:=TCtrItem.Create;
  try
    FManager.Save(A);
    // Only Code changes, and Code is not written by UPDATE
    A.Code:=99;
    A.State:=opsUpdate;
    FManager.Save(A);
    AssertTrue('Saved', A.State=opsClean);
    B.ID:=A.ID;
    FManager.Load(B);
    AssertEquals('Not written', 7, B.Code);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerContracts.TestLoadSQLUnicodeParam;
var
  A, B: TCtrItem;
  Q: TSQLQuery;
begin
  A:=NewItem('placeholder');
  B:=TCtrItem.Create;
  try
    FManager.Save(A);
    // Store the text as UnicodeString: an AnsiString property depends on the
    // system code page of the test program
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=FConn;
      Q.Transaction:=FTrans;
      Q.SQL.Text:='UPDATE '+FItemDef.TableName+' SET Name = :N';
      Q.Params[0].AsUnicodeString:=UTF8Decode(NonAsciiName);
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
    FManager.LoadSQL(B, 'SELECT * FROM '+FItemDef.TableName+' WHERE Name = :N', ['N', UTF8Decode(NonAsciiName)]);
    AssertEquals('Found by UnicodeString value', A.ID, B.ID);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerContracts.TestLoadSQLCurrencyAndVariantParams;
var
  A, B: TCtrItem;
  C: Currency;
  V: Variant;
begin
  A:=NewItem('cur');
  B:=TCtrItem.Create;
  try
    FManager.Save(A);
    C:=7;
    V:='cur';
    FManager.LoadSQL(B, 'SELECT * FROM '+FItemDef.TableName+' WHERE Code = :C AND Name = :N', ['C', C, 'N', V]);
    AssertEquals(A.ID, B.ID);
  finally
    B.Free;
    A.Free;
  end;
end;

initialization
  RegisterTests([TTestCriteriaContracts, TTestDriverContracts, TTestSQLBuilderContracts,
    TTestManagerContracts]);
end.
