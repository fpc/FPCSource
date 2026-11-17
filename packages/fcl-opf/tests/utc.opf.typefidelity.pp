unit utc.opf.typefidelity;

{ Tests for value fidelity of integer, string and character types:
  - Cardinal above High(Integer), QWord up to High(Int64), QWord overflow
  - UnicodeString / WideString with characters outside the system code page
  - Char / WideChar, including #0
  - DDL column types for these properties
  - change detection for these types
  - Level 2 TNullable<T> with Cardinal, Byte, enumeration and Char (FPC 3.3.1+) }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Variants, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, Variants, DB, sqldb, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.FieldValue, Opf.Driver, Opf.Sql, Opf.DDL,
  Opf.Registry, Opf.Manager, Opf.Persister.RTTI,
{$IFNDEF VER3_2}
  Opf.Persister.ExtRTTI, Opf.Nullable, {$IFDEF FPC_DOTTEDUNITS}System.Nullable{$ELSE}nullable{$ENDIF},
{$ENDIF}
  utc.opf.failconn;

type

  { TTFItem: one property per type }

  TTFItem = class(TPersistent, IOPPersistenceState)
  private
    FCard: Cardinal;
    FCh: Char;
    FID: Integer;
    FQ: QWord;
    FSmall: Word;
    FState: TObjectPersistenceState;
    FU: UnicodeString;
    FW: WideString;
    FWCh: WideChar;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Card: Cardinal read FCard write FCard;
    property Q: QWord read FQ write FQ;
    property Small: Word read FSmall write FSmall;
    property U: UnicodeString read FU write FU;
    property W: WideString read FW write FW;
    property Ch: Char read FCh write FCh;
    property WCh: WideChar read FWCh write FWCh;
  end;

  { TTFMoney: Currency and Extended, through a field, a getter and an index }

  TTFMoney = class(TPersistent)
  private
    FID: Integer;
    FPrice: Currency;
    FAmounts: array[0..1] of Currency;
    FRatio: Extended;
    function GetTotal: Currency;
    procedure SetTotal(const aValue: Currency);
    function GetAmount(aIndex: Integer): Currency;
    procedure SetAmount(aIndex: Integer; aValue: Currency);
  published
    property ID: Integer read FID write FID;
    property Price: Currency read FPrice write FPrice;
    property Total: Currency read GetTotal write SetTotal;
    property Amount1: Currency index 1 read GetAmount write SetAmount;
    property Ratio: Extended read FRatio write FRatio;
  end;

  { TTFCardKey: Cardinal auto-increment key }

  TTFCardKey = class(TPersistent)
  private
    FID: Cardinal;
    FName: String;
  published
    property ID: Cardinal read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestTypeFidelity }

  TTestTypeFidelity = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FSQLBuilder: TOPSQLBuilder;
    FDDLBuilder: TOPDDLBuilder;
    FManager: TOPManager;
    FDef: TPersisterDef;
    function SaveAndLoad(aObj: TTFItem): TTFItem;
    function ColumnSQL(aDialect: TSQLDialect; const aProp: String): UTF8String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCardinalAboveHighInteger;
    procedure TestCardinalMax;
    procedure TestWordRoundTrip;
    procedure TestQWordMaxInt64;
    procedure TestQWordTooLargeRaises;
    procedure TestUnicodeStringRoundTrip;
    procedure TestWideStringRoundTrip;
    procedure TestEmptyUnicodeString;
    procedure TestCharRoundTrip;
    procedure TestCharZero;
    procedure TestWideCharRoundTrip;
    procedure TestCardinalColumnIsBigInt;
    procedure TestQWordColumnIsBigInt;
    procedure TestWordColumnStaysInt;
    procedure TestOracleCardinalColumn;
    procedure TestCardinalAutoIncIs64Bit;
    procedure TestChangeDetectedUnicode;
    procedure TestChangeDetectedQWord;
    procedure TestChangeDetectedChar;
    procedure TestUnchangedUnicodeIsClean;
    // Unicode parameters must be ftString: sqldb does not quote ftWideString
    // when it substitutes parameters into the SQL text (MySQL, MSSQL)
    procedure TestUnicodeParamsBoundAsString;
    procedure TestUnicodeParamKeepsText;
    procedure TestUnicodeParamQuotedInSQLText;
    procedure TestUnicodeArgsBoundAsString;
    // R2-L10: an AnsiChar above #127 survives the UTF-8 round trip
    procedure TestAnsiCharAbove127RoundTrip;
    // R2-L15: change detection in the last digits of Currency and Extended
    procedure TestChangeDetectedCurrencyLastDigit;
    procedure TestChangeDetectedExtendedLastDigit;
    // R2-L16: Currency properties without a detour through Extended
    procedure TestCurrencyPropAccess;
  end;

{$IFNDEF VER3_2}

  TTFColor = (tfcRed, tfcGreen, tfcBlue);
  TTFColors = set of TTFColor;

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullableCardinal = specialize TNullable<Cardinal>;
  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullableByte = specialize TNullable<Byte>;
  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullableColor = specialize TNullable<TTFColor>;
  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullableChar = specialize TNullable<Char>;
  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullableUnicode = specialize TNullable<UnicodeString>;
  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullableColors = specialize TNullable<TTFColors>;

  { TTFNullItem: Level 2 nullable fields }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFNullItem = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FState: TObjectPersistenceState;
    FCard: TTFNullableCardinal;
    FTiny: TTFNullableByte;
    FColor: TTFNullableColor;
    FLetter: TTFNullableChar;
    FText: TTFNullableUnicode;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
    property Card: TTFNullableCardinal read FCard write FCard;
    property Tiny: TTFNullableByte read FTiny write FTiny;
    property Color: TTFNullableColor read FColor write FColor;
    property Letter: TTFNullableChar read FLetter write FLetter;
    property Text: TTFNullableUnicode read FText write FText;
  published
    property ID: Integer read FID write FID;
  end;

  { TTFBadNullItem: a set cannot be stored }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTFBadNullItem = class(TPersistent)
  private
    FID: Integer;
    FColors: TTFNullableColors;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestNullableFidelity }

  TTestNullableFidelity = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FSQLBuilder: TOPSQLBuilder;
    FDDLBuilder: TOPDDLBuilder;
    FManager: TOPManager;
    function SaveAndLoad(aObj: TTFNullItem): TTFNullItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestValuesRoundTrip;
    procedure TestNullsRoundTrip;
    procedure TestCardinalAboveHighInteger;
    procedure TestUnicodeText;
    procedure TestChangeDetected;
    procedure TestUnsupportedTypeRaisesAtRegistration;
  end;

{$ENDIF}

implementation

{ Text with characters outside Latin-1: 'Zoë', a CJK character and an emoji }

function MixedText: UnicodeString;
begin
  Result:='Zo'+WideChar($EB)+' '+WideChar($6F22)+' '+WideChar($D83D)+WideChar($DE00);
end;

type
  { Access to the protected TSQLConnection.GetAsSQLText }
  TTFSQLTextConnection = class(TSQLConnection);

{ Bind the first element of aArgs to aParam }

procedure AssignArg(aParam: TParam; const aArgs: array of const);
begin
  OPFAssignVarRec(aArgs[0], aParam);
end;

{ TTFItem }

{ TTFMoney }

function TTFMoney.GetTotal: Currency;
begin
  Result:=FAmounts[0];
end;

procedure TTFMoney.SetTotal(const aValue: Currency);
begin
  FAmounts[0]:=aValue;
end;

function TTFMoney.GetAmount(aIndex: Integer): Currency;
begin
  Result:=FAmounts[aIndex];
end;

procedure TTFMoney.SetAmount(aIndex: Integer; aValue: Currency);
begin
  FAmounts[aIndex]:=aValue;
end;

function TTFItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TTFItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestTypeFidelity }

function TTestTypeFidelity.SaveAndLoad(aObj: TTFItem): TTFItem;
begin
  aObj.State:=opsNew;
  FManager.Save(aObj);
  Result:=TTFItem.Create;
  Result.ID:=aObj.ID;
  FManager.Load(Result);
end;

function TTestTypeFidelity.ColumnSQL(aDialect: TSQLDialect; const aProp: String): UTF8String;
var
  B: TOPDDLBuilder;
begin
  B:=TOPDDLBuilder.Create(Nil);
  try
    B.SQLDialect:=aDialect;
    Result:=B.GetPropertyDefSQL('T', FDef.FieldMap.FindByPropertyName(aProp));
  finally
    B.Free;
  end;
end;

procedure TTestTypeFidelity.SetUp;
var
  C: TOPConnection;
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
  FDef:=FRegistry.RegisterPersister(TTFItem, TRTTIPersister);
  F:=FDef.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  FDDLBuilder:=TOPDDLBuilder.Create(Nil);
  FDDLBuilder.SQLDialect:=dSQLite;
  FDDLBuilder.Registry:=FRegistry;
  FDDLBuilder.CreateTable(FConn, FDef.TableName, FDef.FieldMap.AsArray);
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

procedure TTestTypeFidelity.TearDown;
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

procedure TTestTypeFidelity.TestCardinalAboveHighInteger;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Card:=4000000000;
    B:=SaveAndLoad(A);
    try
      AssertEquals(Int64(4000000000), Int64(B.Card));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestCardinalMax;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Card:=High(Cardinal);
    B:=SaveAndLoad(A);
    try
      AssertEquals(Int64(High(Cardinal)), Int64(B.Card));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestWordRoundTrip;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Small:=65535;
    B:=SaveAndLoad(A);
    try
      AssertEquals(65535, B.Small);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestQWordMaxInt64;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Q:=QWord(High(Int64));
    B:=SaveAndLoad(A);
    try
      AssertTrue('High(Int64)', B.Q=QWord(High(Int64)));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestQWordTooLargeRaises;
var
  A: TTFItem;
  Raised: Boolean;
begin
  A:=TTFItem.Create;
  try
    A.Q:=QWord(High(Int64))+1;
    A.State:=opsNew;
    Raised:=False;
    try
      FManager.Save(A);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Not silently stored as a negative number', Raised);
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestUnicodeStringRoundTrip;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.U:=MixedText;
    B:=SaveAndLoad(A);
    try
      AssertTrue('Same text', B.U=MixedText);
      AssertEquals('Length', Length(MixedText), Length(B.U));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestWideStringRoundTrip;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.W:=MixedText;
    B:=SaveAndLoad(A);
    try
      AssertTrue('Same text', B.W=MixedText);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestEmptyUnicodeString;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.U:='';
    B:=SaveAndLoad(A);
    try
      AssertTrue(B.U='');
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestCharRoundTrip;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Ch:='x';
    B:=SaveAndLoad(A);
    try
      AssertEquals('x', B.Ch);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestCharZero;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Ch:=#0;
    B:=SaveAndLoad(A);
    try
      AssertEquals(0, Ord(B.Ch));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestWideCharRoundTrip;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.WCh:=WideChar($6F22);
    B:=SaveAndLoad(A);
    try
      AssertEquals($6F22, Ord(B.WCh));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestCardinalColumnIsBigInt;
begin
  AssertEquals('BIGINT', ColumnSQL(dPostGres, 'Card'));
end;

procedure TTestTypeFidelity.TestQWordColumnIsBigInt;
begin
  AssertEquals('BIGINT', ColumnSQL(dFirebird3, 'Q'));
end;

procedure TTestTypeFidelity.TestWordColumnStaysInt;
begin
  AssertEquals('INT', ColumnSQL(dPostGres, 'Small'));
end;

procedure TTestTypeFidelity.TestOracleCardinalColumn;
begin
  AssertEquals('NUMBER(19)', ColumnSQL(dOracle, 'Card'));
end;

procedure TTestTypeFidelity.TestCardinalAutoIncIs64Bit;
var
  D: TPersisterDef;
  F: TOPFieldMap;
  B: TOPDDLBuilder;
  SQL: UTF8String;
begin
  D:=FRegistry.RegisterPersister(TTFCardKey, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  B:=TOPDDLBuilder.Create(Nil);
  try
    B.SQLDialect:=dPostGres;
    SQL:=B.GetCreateSQL(D.TableName, D.FieldMap.AsArray);
    AssertTrue('BIGSERIAL in '+SQL, Pos('BIGSERIAL', SQL)>0);
  finally
    B.Free;
  end;
end;

procedure TTestTypeFidelity.TestChangeDetectedUnicode;
var
  A: TTFItem;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('U');
  A:=TTFItem.Create;
  try
    // Two texts that are the same in a single byte code page ('?' for both)
    A.U:=WideChar($6F22);
    AssertTrue(OPFReadFieldValue(A, F, V1));
    A.U:=WideChar($6F23);
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestChangeDetectedQWord;
var
  A: TTFItem;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Q');
  A:=TTFItem.Create;
  try
    A.Q:=1;
    AssertTrue('Readable', OPFReadFieldValue(A, F, V1));
    A.Q:=2;
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestChangeDetectedChar;
var
  A: TTFItem;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('Ch');
  A:=TTFItem.Create;
  try
    A.Ch:='a';
    AssertTrue('Readable', OPFReadFieldValue(A, F, V1));
    A.Ch:='b';
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestAnsiCharAbove127RoundTrip;
var
  A, B: TTFItem;
begin
  A:=TTFItem.Create;
  try
    A.Ch:=#233;
    B:=SaveAndLoad(A);
    try
      AssertEquals('Same character', Ord(#233), Ord(B.Ch));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
  // Valid UTF-8 in the database
  if DefaultSystemCodePage=CP_UTF8 then
    AssertEquals('UTF-8 text', #$C3#$A9, OPFCharToText(tkChar, 233));
end;

procedure TTestTypeFidelity.TestChangeDetectedCurrencyLastDigit;
var
  R: TOPRegistry;
  D: TPersisterDef;
  M: TTFMoney;
  V1, V2: Variant;
begin
  R:=TOPRegistry.Create(Nil);
  M:=TTFMoney.Create;
  try
    D:=R.RegisterPersister(TTFMoney, TRTTIPersister);
    M.Price:=9223372036854.7750;
    AssertTrue(OPFReadFieldValue(M, D.FieldMap.FindByPropertyName('Price'), V1));
    M.Price:=9223372036854.7751;
    AssertTrue(OPFReadFieldValue(M, D.FieldMap.FindByPropertyName('Price'), V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    M.Free;
    R.Free;
  end;
end;

procedure TTestTypeFidelity.TestChangeDetectedExtendedLastDigit;
var
  R: TOPRegistry;
  D: TPersisterDef;
  M: TTFMoney;
  V1, V2: Variant;
  E: Extended;
begin
  // Only where Extended is larger than Double
  {$IFNDEF FPC_HAS_TYPE_EXTENDED}
  Exit;
  {$ENDIF}
  R:=TOPRegistry.Create(Nil);
  M:=TTFMoney.Create;
  try
    D:=R.RegisterPersister(TTFMoney, TRTTIPersister);
    E:=1;
    E:=E/3;
    M.Ratio:=E;
    AssertTrue(OPFReadFieldValue(M, D.FieldMap.FindByPropertyName('Ratio'), V1));
    // Differs in the bits that a Double does not have
    E:=E+E*1E-18;
    AssertTrue('Same as Double', Double(E)=Double(M.Ratio));
    M.Ratio:=E;
    AssertTrue(OPFReadFieldValue(M, D.FieldMap.FindByPropertyName('Ratio'), V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    M.Free;
    R.Free;
  end;
end;

procedure TTestTypeFidelity.TestCurrencyPropAccess;
const
  Big: Currency = 922337203685477.5807;
var
  M: TTFMoney;
begin
  M:=TTFMoney.Create;
  try
    OPFSetCurrencyProp(M, GetPropInfo(M, 'Price'), Big);
    AssertTrue('Field write', M.Price=Big);
    AssertTrue('Field read', OPFGetCurrencyProp(M, GetPropInfo(M, 'Price'))=Big);
    OPFSetCurrencyProp(M, GetPropInfo(M, 'Total'), -Big);
    AssertTrue('Method write', M.Total=-Big);
    AssertTrue('Method read', OPFGetCurrencyProp(M, GetPropInfo(M, 'Total'))=-Big);
    OPFSetCurrencyProp(M, GetPropInfo(M, 'Amount1'), Big);
    AssertTrue('Index write', M.Amount1=Big);
    AssertTrue('Index read', OPFGetCurrencyProp(M, GetPropInfo(M, 'Amount1'))=Big);
  finally
    M.Free;
  end;
end;

procedure TTestTypeFidelity.TestUnchangedUnicodeIsClean;
var
  A: TTFItem;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  F:=FDef.FieldMap.FindByPropertyName('U');
  A:=TTFItem.Create;
  try
    A.U:=MixedText;
    AssertTrue(OPFReadFieldValue(A, F, V1));
    A.U:=Copy(MixedText, 1, Length(MixedText));
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertTrue('No change', OPFSameVariant(V1, V2));
  finally
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestUnicodeParamsBoundAsString;
const
  Props: array[0..3] of String = ('U', 'W', 'Ch', 'WCh');
var
  A: TTFItem;
  Pers: TRTTIPersister;
  P: TParam;
  I: Integer;
begin
  A:=TTFItem.Create;
  Pers:=TRTTIPersister.Create;
  P:=TParam.Create(Nil);
  try
    A.U:=MixedText;
    A.W:=MixedText;
    A.Ch:='c';
    A.WCh:=WideChar($6F22);
    for I:=0 to High(Props) do
    begin
      P.Clear;
      AssertTrue(Props[I]+' persisted', Pers.PersistProperty(A, FDef.FieldMap.FindByPropertyName(Props[I]), P));
      AssertEquals(Props[I]+' DataType', Ord(ftString), Ord(P.DataType));
    end;
  finally
    P.Free;
    Pers.Free;
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestUnicodeParamKeepsText;
var
  A: TTFItem;
  Pers: TRTTIPersister;
  P: TParam;
begin
  A:=TTFItem.Create;
  Pers:=TRTTIPersister.Create;
  P:=TParam.Create(Nil);
  try
    A.U:=MixedText;
    Pers.PersistProperty(A, FDef.FieldMap.FindByPropertyName('U'), P);
    AssertTrue('UTF-8 text unchanged', P.AsUTF8String=UTF8Encode(MixedText));
    A.WCh:=WideChar($6F22);
    Pers.PersistProperty(A, FDef.FieldMap.FindByPropertyName('WCh'), P);
    AssertTrue('WideChar as UTF-8', P.AsUTF8String=UTF8Encode(UnicodeString(WideChar($6F22))));
  finally
    P.Free;
    Pers.Free;
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestUnicodeParamQuotedInSQLText;
const
  Attack = 'x'' OR ''1''=''1';
var
  A: TTFItem;
  Pers: TRTTIPersister;
  P: TParam;
begin
  A:=TTFItem.Create;
  Pers:=TRTTIPersister.Create;
  P:=TParam.Create(Nil);
  try
    A.U:=Attack;
    Pers.PersistProperty(A, FDef.FieldMap.FindByPropertyName('U'), P);
    // This is the text MySQL and MSSQL connections substitute for the parameter
    AssertEquals('Quoted with doubled quotes', QuotedStr(Attack),
                 TTFSQLTextConnection(TSQLConnection(FConn)).GetAsSQLText(P));
    P.Clear;
    AssignArg(P, [UnicodeString(Attack)]);
    AssertEquals('Argument quoted', QuotedStr(Attack), TTFSQLTextConnection(TSQLConnection(FConn)).GetAsSQLText(P));
  finally
    P.Free;
    Pers.Free;
    A.Free;
  end;
end;

procedure TTestTypeFidelity.TestUnicodeArgsBoundAsString;
var
  P: TParam;
  U: UnicodeString;
  W: WideString;
  WC: WideChar;
begin
  U:=MixedText;
  W:=MixedText;
  WC:=WideChar($6F22);
  P:=TParam.Create(Nil);
  try
    AssignArg(P, [U]);
    AssertEquals('UnicodeString DataType', Ord(ftString), Ord(P.DataType));
    AssertTrue('UnicodeString text', P.AsUTF8String=UTF8Encode(MixedText));
    P.Clear;
    AssignArg(P, [W]);
    AssertEquals('WideString DataType', Ord(ftString), Ord(P.DataType));
    AssertTrue('WideString text', P.AsUTF8String=UTF8Encode(MixedText));
    P.Clear;
    AssignArg(P, [WC]);
    AssertEquals('WideChar DataType', Ord(ftString), Ord(P.DataType));
    AssertTrue('WideChar text', P.AsUTF8String=UTF8Encode(UnicodeString(WC)));
    P.Clear;
    AssignArg(P, [PWideChar(U)]);
    AssertEquals('PWideChar DataType', Ord(ftString), Ord(P.DataType));
    AssertTrue('PWideChar text', P.AsUTF8String=UTF8Encode(MixedText));
    P.Clear;
    OPFAssignVariant(Variant(U), P);
    AssertEquals('Variant DataType', Ord(ftString), Ord(P.DataType));
    AssertTrue('Variant text', P.AsUTF8String=UTF8Encode(MixedText));
  finally
    P.Free;
  end;
end;

{$IFNDEF VER3_2}

{ TTFNullItem }

function TTFNullItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TTFNullItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestNullableFidelity }

function TTestNullableFidelity.SaveAndLoad(aObj: TTFNullItem): TTFNullItem;
begin
  aObj.State:=opsNew;
  FManager.Save(aObj);
  Result:=TTFNullItem.Create;
  Result.ID:=aObj.ID;
  FManager.Load(Result);
end;

procedure TTestNullableFidelity.SetUp;
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
  D:=FRegistry.RegisterPersister(TTFNullItem, TExtRTTIPersister);
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

procedure TTestNullableFidelity.TearDown;
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

procedure TTestNullableFidelity.TestValuesRoundTrip;
var
  A, B: TTFNullItem;
begin
  A:=TTFNullItem.Create;
  try
    A.Card:=12;
    A.Tiny:=255;
    A.Color:=tfcBlue;
    A.Letter:='q';
    A.Text:='abc';
    B:=SaveAndLoad(A);
    try
      AssertTrue('Card', B.Card.HasValue and (B.Card.Value=12));
      AssertTrue('Tiny', B.Tiny.HasValue and (B.Tiny.Value=255));
      AssertTrue('Color', B.Color.HasValue and (B.Color.Value=tfcBlue));
      AssertTrue('Letter', B.Letter.HasValue and (B.Letter.Value='q'));
      AssertTrue('Text', B.Text.HasValue and (B.Text.Value='abc'));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestNullableFidelity.TestNullsRoundTrip;
var
  A, B: TTFNullItem;
begin
  A:=TTFNullItem.Create;
  try
    B:=SaveAndLoad(A);
    try
      AssertFalse('Card', B.Card.HasValue);
      AssertFalse('Tiny', B.Tiny.HasValue);
      AssertFalse('Color', B.Color.HasValue);
      AssertFalse('Letter', B.Letter.HasValue);
      AssertFalse('Text', B.Text.HasValue);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestNullableFidelity.TestCardinalAboveHighInteger;
var
  A, B: TTFNullItem;
begin
  A:=TTFNullItem.Create;
  try
    A.Card:=High(Cardinal);
    B:=SaveAndLoad(A);
    try
      AssertEquals(Int64(High(Cardinal)), Int64(B.Card.Value));
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestNullableFidelity.TestUnicodeText;
var
  A, B: TTFNullItem;
begin
  A:=TTFNullItem.Create;
  try
    A.Text:=MixedText;
    B:=SaveAndLoad(A);
    try
      AssertTrue(B.Text.Value=MixedText);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

procedure TTestNullableFidelity.TestChangeDetected;
var
  A: TTFNullItem;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  F:=FRegistry.FindPersister(TTFNullItem).FieldMap.FindByPropertyName('Color');
  A:=TTFNullItem.Create;
  try
    A.Color:=tfcRed;
    AssertTrue('Readable', OPFReadFieldValue(A, F, V1));
    A.Color:=tfcGreen;
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    A.Free;
  end;
end;

procedure TTestNullableFidelity.TestUnsupportedTypeRaisesAtRegistration;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FRegistry.RegisterPersister(TTFBadNullItem, TExtRTTIPersister);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue(Raised);
  AssertNull('Not registered', FRegistry.FindPersister(TTFBadNullItem));
end;

{$ENDIF}

initialization
  RegisterTest(TTestTypeFidelity);
{$IFNDEF VER3_2}
  RegisterTest(TTestNullableFidelity);
{$ENDIF}
end.
