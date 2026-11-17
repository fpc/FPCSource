unit utc.opf.persister.rtti;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, db, sqldb,
  sqlite3conn,
  fpcunit, testregistry,
{$ENDIF}
  Variants,
  Opf.Types, Opf.Fields, Opf.Persister.RTTI;

type

  { Test fixture classes — must descend from TPersistent for published RTTI }

  TTestStatus = (tsActive, tsInactive, tsPending);

  { TTestContact }

  TTestContact = class(TPersistent)
  private
    FID: Integer;
    FFirstName: String;
    FLastName: String;
    FAge: Integer;
  published
    property ID: Integer read FID write FID;
    property FirstName: String read FFirstName write FFirstName;
    property LastName: String read FLastName write FLastName;
    property Age: Integer read FAge write FAge;
  end;

  { TTestProduct }

  TTestProduct = class(TPersistent)
  private
    FID: Int64;
    FPrice: Double;
    FActive: Boolean;
    FName: String;
    FStatus: TTestStatus;
  published
    property ID: Int64 read FID write FID;
    property Price: Double read FPrice write FPrice;
    property Active: Boolean read FActive write FActive;
    property Name: String read FName write FName;
    property Status: TTestStatus read FStatus write FStatus;
  end;

  { TTestOption / TTestOptions — set type used in extended type tests }

  TTestOption = (toA, toB);
  TTestOptions = set of TTestOption;

  { TTestUnsupportedDynArray — has a dynamic array published property (genuinely unsupported) }

  TIntArray = array of Integer;

  TTestUnsupportedDynArray = class(TPersistent)
  private
    FItems: TIntArray;
  published
    property Items: TIntArray read FItems write FItems;
  end;

  { TTestExtendedTypes — fixture with set, enum, and variant published properties }

  TTestExtendedTypes = class(TPersistent)
  private
    FID: Integer;
    FOptions: TTestOptions;
    FStatus: TTestStatus;
    FExtra: Variant;
  published
    property ID: Integer read FID write FID;
    property Options: TTestOptions read FOptions write FOptions;
    property Status: TTestStatus read FStatus write FStatus;
    property Extra: Variant read FExtra write FExtra;
  end;

  { TTestNullableContact — fixture with field-based stored specifier for nullable tests }

  TTestNullableContact = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
    FAgeStored: Boolean;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge stored FAgeStored;
  end;

  { TTestMethodStoredContact — fixture with method-based stored specifier }

  TTestMethodStoredContact = class(TPersistent)
  private
    FID: Integer;
    FScore: Integer;
    FScoreIsNull: Boolean;
    function IsScoreStored: Boolean;
  published
    property ID: Integer read FID write FID;
    property Score: Integer read FScore write FScore stored IsScoreStored;
  end;

  { TTestAddress — nested class for JSON blob tests }

  TTestAddress = class(TPersistent)
  private
    FStreet: String;
    FCity: String;
    FZipCode: String;
  published
    property Street: String read FStreet write FStreet;
    property City: String read FCity write FCity;
    property ZipCode: String read FZipCode write FZipCode;
  end;

  { TTestContactWithAddress — has a class-typed published property }

  TTestContactWithAddress = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FAddress: TTestAddress;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Address: TTestAddress read FAddress write FAddress;
  end;

  { TTestNote — has a TStrings published property }

  TTestNote = class(TPersistent)
  private
    FID: Integer;
    FLines: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Lines: TStrings read FLines write FLines;
  end;

  { TTestRTTIPersister }

  TTestRTTIPersister = class(TTestCase)
  private
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure PopulateContact;
    procedure PopulateProduct;
  published
    procedure PopulateFieldMapCreatesEntries;
    procedure FieldNameMatchesPropertyName;
    procedure TableNameStripsLeadingT;
    procedure PropertyTypeInfoStored;
    procedure StringPropertyMapped;
    procedure IntegerPropertyMapped;
    procedure Int64PropertyMapped;
    procedure FloatPropertyMapped;
    procedure BooleanPropertyMapped;
    procedure UnsupportedPropertyTypeRaises;
  end;

  { TTestRTTIClassProperty }

  TTestRTTIClassProperty = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    procedure CreateContactAddressTable;
    procedure CreateNoteTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
    procedure PopulateContactAddressMap;
    procedure PopulateNoteMap;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Field map tests }
    procedure PopulateFieldMapIncludesClassProperty;
    { PersistProperty tests }
    procedure PersistPropertyClassToJSON;
    procedure PersistPropertyNilClassSetsNull;
    procedure PersistPropertyTStringsToText;
    procedure PersistPropertyNilTStringsSetsNull;
    { ApplyProperty tests }
    procedure ApplyPropertyJSONToClass;
    procedure ApplyPropertyNullFieldLeavesProperty;
    procedure ApplyPropertyAutoCreatesNilProperty;
    procedure ApplyPropertyTextToTStrings;
  end;

  { TTestRTTIErrorDiagnostics }

  TTestRTTIErrorDiagnostics = class(TTestCase)
  private
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUnsupportedPropertyTypeMessage;
    procedure TestDuplicateFieldNameMessage;
    procedure TestDuplicatePropertyNameMessage;
  end;

  { TTestRTTIExtendedTypes }

  TTestRTTIExtendedTypes = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    procedure CreateExtendedTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PopulateFieldMapIncludesSet;
    procedure PopulateFieldMapIncludesVariant;
    procedure SetAsIntegerRoundTrip;
    procedure SetAsStringRoundTrip;
    procedure EnumAsStringRoundTrip;
    procedure EnumAsIntegerRoundTrip;
    procedure VariantRoundTrip;
    procedure SkipUnknownTypesTrue;
    procedure SkipUnknownTypesFalseRaises;
  end;

  { TTestRTTINullable }

  TTestRTTINullable = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    procedure CreateNullableTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PopulateFieldMapDetectsStoredField;
    procedure PopulateFieldMapDefaultStoredNotNullable;
    procedure PersistPropertyNullWhenStoredFalse;
    procedure PersistPropertyValueWhenStoredTrue;
    procedure ApplyPropertyNullSetsStoredFalseAndDefault;
    procedure ApplyPropertyValueSetsStoredTrue;
    procedure PersistPropertyMethodBasedStored;
    procedure ApplyPropertyNullMethodBasedSetsDefault;
    procedure PopulateFieldMapMethodBasedIsNullable;
  end;

  { TTestRTTIPersisterRoundTrip }

  TTestRTTIPersisterRoundTrip = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    procedure CreateContactTable;
    procedure CreateProductTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
    procedure PopulateContactMap;
    procedure PopulateProductMap;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { PersistProperty tests }
    procedure PersistPropertyString;
    procedure PersistPropertyInteger;
    procedure PersistPropertyInt64;
    procedure PersistPropertyFloat;
    procedure PersistPropertyBoolean;
    procedure PersistPropertyEnumeration;
    { ApplyProperty tests }
    procedure ApplyPropertyString;
    procedure ApplyPropertyInteger;
    procedure ApplyPropertyInt64;
    procedure ApplyPropertyFloat;
    procedure ApplyPropertyBoolean;
    procedure ApplyPropertyEnumeration;
    { Null handling }
    procedure ApplyPropertyNull;
    { Full round-trip }
    procedure FullRoundTrip;
  end;

implementation

{ TTestRTTIPersister }

procedure TTestRTTIPersister.SetUp;
begin
  inherited SetUp;
  FPersister := TRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestRTTIPersister.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  inherited TearDown;
end;

procedure TTestRTTIPersister.PopulateContact;
begin
  FPersister.PopulateFieldMap(TTestContact, FMap);
end;

procedure TTestRTTIPersister.PopulateProduct;
begin
  FPersister.PopulateFieldMap(TTestProduct, FMap);
end;

procedure TTestRTTIPersister.PopulateFieldMapCreatesEntries;
begin
  PopulateContact;
  AssertEquals('TTestContact should have 4 fields', 4, FMap.Count);
end;

procedure TTestRTTIPersister.FieldNameMatchesPropertyName;
var
  I: Integer;
begin
  PopulateContact;
  for I := 0 to FMap.Count - 1 do
    AssertEquals('FieldName = PropertyName for entry ' + IntToStr(I),
      FMap[I].PropertyName, FMap[I].FieldName);
end;

procedure TTestRTTIPersister.TableNameStripsLeadingT;
begin
  AssertEquals('Table name strips T', 'TestContact',
    TRTTIPersister.GetTableName(TTestContact));
end;

procedure TTestRTTIPersister.PropertyTypeInfoStored;
var
  F: TOPFieldMap;
begin
  PopulateContact;
  F := FMap.FindByPropertyName('ID');
  AssertNotNull('ID field found', F);
  AssertTrue('ID has TypeInfo', F.PropertyTypeInfo <> nil);
  AssertTrue('ID kind is tkInteger', F.PropertyKind = tkInteger);
end;

procedure TTestRTTIPersister.StringPropertyMapped;
var
  F: TOPFieldMap;
begin
  PopulateContact;
  F := FMap.FindByPropertyName('FirstName');
  AssertNotNull('FirstName field found', F);
  AssertTrue('FirstName is string kind',
    F.PropertyKind in [tkSString, tkAString, tkWString, tkUString]);
end;

procedure TTestRTTIPersister.IntegerPropertyMapped;
var
  F: TOPFieldMap;
begin
  PopulateContact;
  F := FMap.FindByPropertyName('Age');
  AssertNotNull('Age field found', F);
  AssertTrue('Age kind is tkInteger', F.PropertyKind = tkInteger);
end;

procedure TTestRTTIPersister.Int64PropertyMapped;
var
  F: TOPFieldMap;
begin
  PopulateProduct;
  F := FMap.FindByPropertyName('ID');
  AssertNotNull('ID field found', F);
  AssertTrue('ID kind is tkInt64', F.PropertyKind = tkInt64);
end;

procedure TTestRTTIPersister.FloatPropertyMapped;
var
  F: TOPFieldMap;
begin
  PopulateProduct;
  F := FMap.FindByPropertyName('Price');
  AssertNotNull('Price field found', F);
  AssertTrue('Price kind is tkFloat', F.PropertyKind = tkFloat);
end;

procedure TTestRTTIPersister.BooleanPropertyMapped;
var
  F: TOPFieldMap;
begin
  PopulateProduct;
  F := FMap.FindByPropertyName('Active');
  AssertNotNull('Active field found', F);
  AssertTrue('Active kind is tkBool', F.PropertyKind = tkBool);
end;

procedure TTestRTTIPersister.UnsupportedPropertyTypeRaises;
begin
  try
    FPersister.PopulateFieldMap(TTestUnsupportedDynArray, FMap);
    Fail('Expected EFieldMap for unsupported property type');
  except
    on E: EFieldMap do
      AssertTrue('Error message contains property name',
        Pos('Items', E.Message) > 0);
  end;
end;

{ TTestContactWithAddress }

constructor TTestContactWithAddress.Create;
begin
  inherited Create;
  FAddress := TTestAddress.Create;
end;

destructor TTestContactWithAddress.Destroy;
begin
  FAddress.Free;
  inherited Destroy;
end;

{ TTestNote }

constructor TTestNote.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TTestNote.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

{ TTestRTTIClassProperty }

procedure TTestRTTIClassProperty.SetUp;
begin
  inherited SetUp;
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
  FPersister := TRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestRTTIClassProperty.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestRTTIClassProperty.CreateContactAddressTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestContactWithAddress (ID INTEGER, Name TEXT, Address TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestRTTIClassProperty.CreateNoteTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestNote (ID INTEGER, Lines TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestRTTIClassProperty.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestRTTIClassProperty.PopulateContactAddressMap;
begin
  FPersister.PopulateFieldMap(TTestContactWithAddress, FMap);
end;

procedure TTestRTTIClassProperty.PopulateNoteMap;
begin
  FPersister.PopulateFieldMap(TTestNote, FMap);
end;

procedure TTestRTTIClassProperty.PopulateFieldMapIncludesClassProperty;
var
  F: TOPFieldMap;
begin
  PopulateContactAddressMap;
  AssertEquals('TTestContactWithAddress should have 3 fields', 3, FMap.Count);
  F := FMap.FindByPropertyName('Address');
  AssertNotNull('Address field found', F);
  AssertTrue('Address kind is tkClass', F.PropertyKind = tkClass);
end;

procedure TTestRTTIClassProperty.PersistPropertyClassToJSON;
var
  Obj: TTestContactWithAddress;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  JSON: String;
begin
  PopulateContactAddressMap;
  Obj := TTestContactWithAddress.Create;
  try
    Obj.Address.Street := 'Main St';
    Obj.Address.City := 'Springfield';
    FM := FMap.FindByPropertyName('Address');
    CreateContactAddressTable;
    Q := MakeQuery('INSERT INTO TestContactWithAddress (Address) VALUES (:Address)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Address'));
      JSON := Q.Params.ParamByName('Address').AsString;
      AssertTrue('JSON contains Street', Pos('Main St', JSON) > 0);
      AssertTrue('JSON contains City', Pos('Springfield', JSON) > 0);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.PersistPropertyNilClassSetsNull;
var
  Obj: TTestContactWithAddress;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateContactAddressMap;
  Obj := TTestContactWithAddress.Create;
  try
    Obj.Address.Free;
    Obj.Address := nil;
    FM := FMap.FindByPropertyName('Address');
    CreateContactAddressTable;
    Q := MakeQuery('INSERT INTO TestContactWithAddress (Address) VALUES (:Address)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Address'));
      AssertTrue('Param is null', Q.Params.ParamByName('Address').IsNull);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.PersistPropertyTStringsToText;
var
  Obj: TTestNote;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateNoteMap;
  Obj := TTestNote.Create;
  try
    Obj.Lines.Add('Line 1');
    Obj.Lines.Add('Line 2');
    Obj.Lines.Add('Line 3');
    FM := FMap.FindByPropertyName('Lines');
    CreateNoteTable;
    Q := MakeQuery('INSERT INTO TestNote (Lines) VALUES (:Lines)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Lines'));
      AssertEquals('Param value', Obj.Lines.Text, Q.Params.ParamByName('Lines').AsString);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.PersistPropertyNilTStringsSetsNull;
var
  Obj: TTestNote;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateNoteMap;
  Obj := TTestNote.Create;
  try
    Obj.Lines.Free;
    Obj.Lines := nil;
    FM := FMap.FindByPropertyName('Lines');
    CreateNoteTable;
    Q := MakeQuery('INSERT INTO TestNote (Lines) VALUES (:Lines)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Lines'));
      AssertTrue('Param is null', Q.Params.ParamByName('Lines').IsNull);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.ApplyPropertyJSONToClass;
var
  Obj: TTestContactWithAddress;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateContactAddressMap;
  CreateContactAddressTable;
  Q := MakeQuery('INSERT INTO TestContactWithAddress (ID, Address) VALUES (1, ''{"Street":"Oak Ave","City":"Portland","ZipCode":"97201"}'')');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestContactWithAddress.Create;
  try
    FM := FMap.FindByPropertyName('Address');
    Q := MakeQuery('SELECT Address FROM TestContactWithAddress WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Address'));
      AssertEquals('Street', 'Oak Ave', Obj.Address.Street);
      AssertEquals('City', 'Portland', Obj.Address.City);
      AssertEquals('ZipCode', '97201', Obj.Address.ZipCode);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.ApplyPropertyNullFieldLeavesProperty;
var
  Obj: TTestContactWithAddress;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateContactAddressMap;
  CreateContactAddressTable;
  Q := MakeQuery('INSERT INTO TestContactWithAddress (ID, Address) VALUES (1, NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestContactWithAddress.Create;
  try
    Obj.Address.Street := 'Original';
    FM := FMap.FindByPropertyName('Address');
    Q := MakeQuery('SELECT Address FROM TestContactWithAddress WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Address'));
      AssertEquals('Address unchanged', 'Original', Obj.Address.Street);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.ApplyPropertyAutoCreatesNilProperty;
var
  Obj: TTestContactWithAddress;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateContactAddressMap;
  CreateContactAddressTable;
  Q := MakeQuery('INSERT INTO TestContactWithAddress (ID, Address) VALUES (1, ''{"Street":"Elm St","City":"Salem","ZipCode":"97301"}'')');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestContactWithAddress.Create;
  try
    { Free the constructor-created Address, set to nil }
    Obj.Address.Free;
    Obj.Address := nil;
    FM := FMap.FindByPropertyName('Address');
    Q := MakeQuery('SELECT Address FROM TestContactWithAddress WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Address'));
      AssertNotNull('Address auto-created', Obj.Address);
      AssertEquals('Street', 'Elm St', Obj.Address.Street);
      AssertEquals('City', 'Salem', Obj.Address.City);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIClassProperty.ApplyPropertyTextToTStrings;
var
  Obj: TTestNote;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  ExpectedText: String;
begin
  PopulateNoteMap;
  CreateNoteTable;
  ExpectedText := 'Line A' + LineEnding + 'Line B' + LineEnding;
  Q := MakeQuery('INSERT INTO TestNote (ID, Lines) VALUES (1, :Lines)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.Params.ParamByName('Lines').AsString := ExpectedText;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestNote.Create;
  try
    FM := FMap.FindByPropertyName('Lines');
    Q := MakeQuery('SELECT Lines FROM TestNote WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Lines'));
      AssertEquals('Lines.Text matches', ExpectedText, Obj.Lines.Text);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ TTestRTTIPersisterRoundTrip }

procedure TTestRTTIPersisterRoundTrip.SetUp;
begin
  inherited SetUp;
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
  FPersister := TRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestRTTIPersisterRoundTrip.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestRTTIPersisterRoundTrip.CreateContactTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestContact (ID INTEGER, FirstName TEXT, LastName TEXT, Age INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.CreateProductTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestProduct (ID BIGINT, Price REAL, Active INTEGER, Name TEXT, Status INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestRTTIPersisterRoundTrip.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestRTTIPersisterRoundTrip.PopulateContactMap;
begin
  FPersister.PopulateFieldMap(TTestContact, FMap);
end;

procedure TTestRTTIPersisterRoundTrip.PopulateProductMap;
begin
  FPersister.PopulateFieldMap(TTestProduct, FMap);
end;

{ PersistProperty tests — set property on object, persist to param, verify param }

procedure TTestRTTIPersisterRoundTrip.PersistPropertyString;
var
  Obj: TTestContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateContactMap;
  Obj := TTestContact.Create;
  try
    Obj.FirstName := 'Alice';
    FM := FMap.FindByPropertyName('FirstName');
    CreateContactTable;
    Q := MakeQuery('INSERT INTO TestContact (FirstName) VALUES (:FirstName)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      R := FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('FirstName'));
      AssertTrue('PersistProperty returns True', R);
      AssertEquals('Param value', 'Alice', Q.Params.ParamByName('FirstName').AsString);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.PersistPropertyInteger;
var
  Obj: TTestContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateContactMap;
  Obj := TTestContact.Create;
  try
    Obj.Age := 42;
    FM := FMap.FindByPropertyName('Age');
    CreateContactTable;
    Q := MakeQuery('INSERT INTO TestContact (Age) VALUES (:Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      R := FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Age'));
      AssertTrue('PersistProperty returns True', R);
      AssertEquals('Param value', 42, Q.Params.ParamByName('Age').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.PersistPropertyInt64;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  Obj := TTestProduct.Create;
  try
    Obj.ID := 9876543210;
    FM := FMap.FindByPropertyName('ID');
    CreateProductTable;
    Q := MakeQuery('INSERT INTO TestProduct (ID) VALUES (:ID)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      R := FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('ID'));
      AssertTrue('PersistProperty returns True', R);
      AssertEquals('Param value', 9876543210, Q.Params.ParamByName('ID').AsLargeInt);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.PersistPropertyFloat;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  Obj := TTestProduct.Create;
  try
    Obj.Price := 19.99;
    FM := FMap.FindByPropertyName('Price');
    CreateProductTable;
    Q := MakeQuery('INSERT INTO TestProduct (Price) VALUES (:Price)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      R := FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Price'));
      AssertTrue('PersistProperty returns True', R);
      AssertEquals('Param value', 19.99, Q.Params.ParamByName('Price').AsFloat, 0.001);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.PersistPropertyBoolean;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  Obj := TTestProduct.Create;
  try
    Obj.Active := True;
    FM := FMap.FindByPropertyName('Active');
    CreateProductTable;
    Q := MakeQuery('INSERT INTO TestProduct (Active) VALUES (:Active)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      R := FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Active'));
      AssertTrue('PersistProperty returns True', R);
      AssertTrue('Param value', Q.Params.ParamByName('Active').AsBoolean);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ ApplyProperty tests — insert row, select back, apply field to object }

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyString;
var
  Obj: TTestContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateContactMap;
  CreateContactTable;
  Q := MakeQuery('INSERT INTO TestContact (FirstName) VALUES (''Bob'')');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestContact.Create;
  try
    FM := FMap.FindByPropertyName('FirstName');
    Q := MakeQuery('SELECT FirstName FROM TestContact');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      R := FPersister.ApplyProperty(Obj, FM, Q.FieldByName('FirstName'));
      AssertTrue('ApplyProperty returns True', R);
      AssertEquals('Property value', 'Bob', Obj.FirstName);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyInteger;
var
  Obj: TTestContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateContactMap;
  CreateContactTable;
  Q := MakeQuery('INSERT INTO TestContact (Age) VALUES (55)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestContact.Create;
  try
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('SELECT Age FROM TestContact');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      R := FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Age'));
      AssertTrue('ApplyProperty returns True', R);
      AssertEquals('Property value', 55, Obj.Age);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyInt64;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  CreateProductTable;
  Q := MakeQuery('INSERT INTO TestProduct (ID) VALUES (9876543210)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestProduct.Create;
  try
    FM := FMap.FindByPropertyName('ID');
    Q := MakeQuery('SELECT ID FROM TestProduct');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      R := FPersister.ApplyProperty(Obj, FM, Q.FieldByName('ID'));
      AssertTrue('ApplyProperty returns True', R);
      AssertEquals('Property value', 9876543210, Obj.ID);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyFloat;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  CreateProductTable;
  Q := MakeQuery('INSERT INTO TestProduct (Price) VALUES (29.95)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestProduct.Create;
  try
    FM := FMap.FindByPropertyName('Price');
    Q := MakeQuery('SELECT Price FROM TestProduct');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      R := FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Price'));
      AssertTrue('ApplyProperty returns True', R);
      AssertEquals('Property value', 29.95, Obj.Price, 0.001);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyBoolean;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  CreateProductTable;
  Q := MakeQuery('INSERT INTO TestProduct (Active) VALUES (1)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestProduct.Create;
  try
    Obj.Active := False;
    FM := FMap.FindByPropertyName('Active');
    Q := MakeQuery('SELECT Active FROM TestProduct');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      R := FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Active'));
      AssertTrue('ApplyProperty returns True', R);
      AssertTrue('Property value', Obj.Active);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.PersistPropertyEnumeration;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  Obj := TTestProduct.Create;
  try
    Obj.Status := tsPending;
    FM := FMap.FindByPropertyName('Status');
    CreateProductTable;
    Q := MakeQuery('INSERT INTO TestProduct (Status) VALUES (:Status)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      R := FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Status'));
      AssertTrue('PersistProperty returns True', R);
      AssertEquals('Param value', Ord(tsPending), Q.Params.ParamByName('Status').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyEnumeration;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  R: Boolean;
begin
  PopulateProductMap;
  CreateProductTable;
  Q := MakeQuery('INSERT INTO TestProduct (Status) VALUES (1)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestProduct.Create;
  try
    Obj.Status := tsActive;
    FM := FMap.FindByPropertyName('Status');
    Q := MakeQuery('SELECT Status FROM TestProduct');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      R := FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Status'));
      AssertTrue('ApplyProperty returns True', R);
      AssertEquals('Property value', Ord(tsInactive), Ord(Obj.Status));
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.ApplyPropertyNull;
var
  Obj: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateProductMap;
  CreateProductTable;
  { Insert row with all NULLs }
  Q := MakeQuery('INSERT INTO TestProduct (ID, Price, Active, Name) VALUES (NULL, NULL, NULL, NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestProduct.Create;
  try
    { Set non-default values first to verify they get reset }
    Obj.ID := 999;
    Obj.Price := 99.99;
    Obj.Active := True;
    Obj.Name := 'ShouldBeCleared';
    Q := MakeQuery('SELECT ID, Price, Active, Name FROM TestProduct');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      { Apply each null field }
      FM := FMap.FindByPropertyName('ID');
      AssertTrue('Null Int64 returns True', FPersister.ApplyProperty(Obj, FM, Q.FieldByName('ID')));
      FM := FMap.FindByPropertyName('Price');
      AssertTrue('Null Float returns True', FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Price')));
      FM := FMap.FindByPropertyName('Active');
      AssertTrue('Null Boolean returns True', FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Active')));
      FM := FMap.FindByPropertyName('Name');
      AssertTrue('Null String returns True', FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Name')));
      { Verify defaults }
      AssertEquals('Null Int64 defaults to 0', 0, Obj.ID);
      AssertEquals('Null Float defaults to 0.0', 0.0, Obj.Price, 0.001);
      AssertFalse('Null Boolean defaults to False', Obj.Active);
      AssertEquals('Null String defaults to empty', '', Obj.Name);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTIPersisterRoundTrip.FullRoundTrip;
var
  Src, Dest: TTestProduct;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  PopulateProductMap;
  CreateProductTable;
  { Persist: object → params → database }
  Src := TTestProduct.Create;
  try
    Src.ID := 42;
    Src.Price := 9.99;
    Src.Active := True;
    Src.Name := 'Widget';
    Q := MakeQuery('INSERT INTO TestProduct (ID, Price, Active, Name) VALUES (:ID, :Price, :Active, :Name)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FM := FMap.FindByPropertyName('ID');
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('ID'));
      FM := FMap.FindByPropertyName('Price');
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Price'));
      FM := FMap.FindByPropertyName('Active');
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Active'));
      FM := FMap.FindByPropertyName('Name');
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Name'));
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  { Apply: database → fields → object }
  Dest := TTestProduct.Create;
  try
    Q := MakeQuery('SELECT ID, Price, Active, Name FROM TestProduct WHERE ID=42');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      AssertFalse('Should have a row', Q.EOF);
      FM := FMap.FindByPropertyName('ID');
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('ID'));
      FM := FMap.FindByPropertyName('Price');
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Price'));
      FM := FMap.FindByPropertyName('Active');
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Active'));
      FM := FMap.FindByPropertyName('Name');
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Name'));
      Q.Close;
    finally
      Q.Free;
    end;
    AssertEquals('ID round-trip', 42, Dest.ID);
    AssertEquals('Price round-trip', 9.99, Dest.Price, 0.001);
    AssertTrue('Active round-trip', Dest.Active);
    AssertEquals('Name round-trip', 'Widget', Dest.Name);
  finally
    Dest.Free;
  end;
end;

{ TTestRTTIErrorDiagnostics }

procedure TTestRTTIErrorDiagnostics.SetUp;
begin
  inherited SetUp;
  FPersister:=TRTTIPersister.Create;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestRTTIErrorDiagnostics.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  inherited TearDown;
end;

procedure TTestRTTIErrorDiagnostics.TestUnsupportedPropertyTypeMessage;
begin
  try
    FPersister.PopulateFieldMap(TTestUnsupportedDynArray, FMap);
    Fail('Expected EFieldMap');
  except
    on E: EFieldMap do
    begin
      AssertTrue('Message contains property name', Pos('Items', E.Message) > 0);
      AssertTrue('Message contains reason', Pos('Unsupported object property type', E.Message) > 0);
    end;
  end;
end;

procedure TTestRTTIErrorDiagnostics.TestDuplicateFieldNameMessage;
begin
  FMap.AddField('Name', 'Name');
  try
    FMap.AddField('Name', 'OtherProp');
    Fail('Expected EFieldMap');
  except
    on E: EFieldMap do
    begin
      AssertTrue('Message contains reason', Pos('Duplicate field name', E.Message) > 0);
      AssertTrue('Message contains field name', Pos('Name', E.Message) > 0);
    end;
  end;
end;

procedure TTestRTTIErrorDiagnostics.TestDuplicatePropertyNameMessage;
begin
  FMap.AddField('Name1', 'Prop');
  try
    FMap.AddField('Name2', 'Prop');
    Fail('Expected EFieldMap');
  except
    on E: EFieldMap do
    begin
      AssertTrue('Message contains reason', Pos('Duplicate property name', E.Message) > 0);
      AssertTrue('Message contains property name', Pos('Prop', E.Message) > 0);
    end;
  end;
end;

{ TTestRTTIExtendedTypes }

procedure TTestRTTIExtendedTypes.SetUp;
begin
  inherited SetUp;
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
  FPersister := TRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestRTTIExtendedTypes.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestRTTIExtendedTypes.CreateExtendedTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestExtendedTypes (ID INTEGER, Options INTEGER, Status INTEGER, Extra TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestRTTIExtendedTypes.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestRTTIExtendedTypes.PopulateFieldMapIncludesSet;
var
  F: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  F := FMap.FindByPropertyName('Options');
  AssertNotNull('Options field found', F);
  AssertTrue('Options kind is tkSet', F.PropertyKind = tkSet);
end;

procedure TTestRTTIExtendedTypes.PopulateFieldMapIncludesVariant;
var
  F: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  F := FMap.FindByPropertyName('Extra');
  AssertNotNull('Extra field found', F);
  AssertTrue('Extra kind is tkVariant', F.PropertyKind = tkVariant);
end;

procedure TTestRTTIExtendedTypes.SetAsIntegerRoundTrip;
var
  Src, Dest: TTestExtendedTypes;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  CreateExtendedTable;
  { Persist }
  Src := TTestExtendedTypes.Create;
  try
    Src.Options := [toA, toB];
    FM := FMap.FindByPropertyName('Options');
    Q := MakeQuery('INSERT INTO TestExtendedTypes (Options) VALUES (:Options)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Options'));
      AssertTrue('Param value non-zero', Q.Params.ParamByName('Options').AsInteger <> 0);
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  { Apply }
  Dest := TTestExtendedTypes.Create;
  try
    Dest.Options := [];
    Q := MakeQuery('SELECT Options FROM TestExtendedTypes');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Options'));
      AssertTrue('toA in result', toA in Dest.Options);
      AssertTrue('toB in result', toB in Dest.Options);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TTestRTTIExtendedTypes.SetAsStringRoundTrip;
var
  Src, Dest: TTestExtendedTypes;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  S: String;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  FM := FMap.FindByPropertyName('Options');
  FM.Options := FM.Options + [foAsString];
  { Create table with TEXT column for string storage }
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestSetStr (Options TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  { Persist }
  Src := TTestExtendedTypes.Create;
  try
    Src.Options := [toA, toB];
    Q := MakeQuery('INSERT INTO TestSetStr (Options) VALUES (:Options)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Options'));
      S := Q.Params.ParamByName('Options').AsString;
      AssertTrue('Contains toA', Pos('toA', S) > 0);
      AssertTrue('Contains toB', Pos('toB', S) > 0);
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  { Apply }
  Dest := TTestExtendedTypes.Create;
  try
    Dest.Options := [];
    Q := MakeQuery('SELECT Options FROM TestSetStr');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Options'));
      AssertTrue('toA in result', toA in Dest.Options);
      AssertTrue('toB in result', toB in Dest.Options);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TTestRTTIExtendedTypes.EnumAsStringRoundTrip;
var
  Src, Dest: TTestExtendedTypes;
  Q: TSQLQuery;
  FM: TOPFieldMap;
  S: String;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  FM := FMap.FindByPropertyName('Status');
  FM.Options := FM.Options + [foAsString];
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestEnumStr (Status TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  { Persist }
  Src := TTestExtendedTypes.Create;
  try
    Src.Status := tsPending;
    Q := MakeQuery('INSERT INTO TestEnumStr (Status) VALUES (:Status)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Status'));
      S := Q.Params.ParamByName('Status').AsString;
      AssertEquals('Enum as string', 'tsPending', S);
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  { Apply }
  Dest := TTestExtendedTypes.Create;
  try
    Dest.Status := tsActive;
    Q := MakeQuery('SELECT Status FROM TestEnumStr');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Status'));
      AssertEquals('Enum round-trip', Ord(tsPending), Ord(Dest.Status));
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TTestRTTIExtendedTypes.EnumAsIntegerRoundTrip;
var
  Src, Dest: TTestExtendedTypes;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  CreateExtendedTable;
  FM := FMap.FindByPropertyName('Status');
  { No foAsString — integer mode (existing behavior) }
  Src := TTestExtendedTypes.Create;
  try
    Src.Status := tsInactive;
    Q := MakeQuery('INSERT INTO TestExtendedTypes (Status) VALUES (:Status)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Status'));
      AssertEquals('Param value', Ord(tsInactive), Q.Params.ParamByName('Status').AsInteger);
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  Dest := TTestExtendedTypes.Create;
  try
    Dest.Status := tsActive;
    Q := MakeQuery('SELECT Status FROM TestExtendedTypes');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Status'));
      AssertEquals('Enum int round-trip', Ord(tsInactive), Ord(Dest.Status));
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TTestRTTIExtendedTypes.VariantRoundTrip;
var
  Src, Dest: TTestExtendedTypes;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestExtendedTypes, FMap);
  CreateExtendedTable;
  FM := FMap.FindByPropertyName('Extra');
  { Persist string variant }
  Src := TTestExtendedTypes.Create;
  try
    Src.Extra := 'hello';
    Q := MakeQuery('INSERT INTO TestExtendedTypes (Extra) VALUES (:Extra)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Src, FM, Q.Params.ParamByName('Extra'));
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Src.Free;
  end;
  { Apply }
  Dest := TTestExtendedTypes.Create;
  try
    Q := MakeQuery('SELECT Extra FROM TestExtendedTypes');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Dest, FM, Q.FieldByName('Extra'));
      AssertEquals('Variant string round-trip', 'hello', String(Dest.Extra));
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Dest.Free;
  end;
end;

procedure TTestRTTIExtendedTypes.SkipUnknownTypesTrue;
begin
  FMap.SkipUnknownTypes := True;
  FPersister.PopulateFieldMap(TTestUnsupportedDynArray, FMap);
  AssertEquals('Unsupported property skipped', 0, FMap.Count);
end;

procedure TTestRTTIExtendedTypes.SkipUnknownTypesFalseRaises;
begin
  FMap.SkipUnknownTypes := False;
  try
    FPersister.PopulateFieldMap(TTestUnsupportedDynArray, FMap);
    Fail('Expected EFieldMap for unsupported property type');
  except
    on E: EFieldMap do
      AssertTrue('Error message contains property name',
        Pos('Items', E.Message) > 0);
  end;
end;

{ TTestMethodStoredContact }

function TTestMethodStoredContact.IsScoreStored: Boolean;
begin
  Result := not FScoreIsNull;
end;

{ TTestRTTINullable }

procedure TTestRTTINullable.SetUp;
begin
  inherited SetUp;
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
  FPersister := TRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestRTTINullable.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestRTTINullable.CreateNullableTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestNullable (ID INTEGER, Name TEXT, Age INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestRTTINullable.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestRTTINullable.PopulateFieldMapDetectsStoredField;
var
  F: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableContact, FMap);
  F := FMap.FindByPropertyName('Age');
  AssertNotNull('Age field found', F);
  AssertTrue('foNullable set for field-based stored', foNullable in F.Options);
  AssertNotNull('PropInfo stored', F.PropInfo);
end;

procedure TTestRTTINullable.PopulateFieldMapDefaultStoredNotNullable;
var
  F: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestContact, FMap);
  F := FMap.FindByPropertyName('Age');
  AssertNotNull('Age field found', F);
  AssertFalse('foNullable NOT set for default stored', foNullable in F.Options);
end;

procedure TTestRTTINullable.PersistPropertyNullWhenStoredFalse;
var
  Obj: TTestNullableContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableContact, FMap);
  CreateNullableTable;
  Obj := TTestNullableContact.Create;
  try
    Obj.Age := 42;
    Obj.FAgeStored := False;
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('INSERT INTO TestNullable (Age) VALUES (:Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Age'));
      AssertTrue('Param is null when stored=False', Q.Params.ParamByName('Age').IsNull);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTINullable.PersistPropertyValueWhenStoredTrue;
var
  Obj: TTestNullableContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableContact, FMap);
  CreateNullableTable;
  Obj := TTestNullableContact.Create;
  try
    Obj.Age := 25;
    Obj.FAgeStored := True;
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('INSERT INTO TestNullable (Age) VALUES (:Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Age'));
      AssertFalse('Param is NOT null when stored=True', Q.Params.ParamByName('Age').IsNull);
      AssertEquals('Param value', 25, Q.Params.ParamByName('Age').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTINullable.ApplyPropertyNullSetsStoredFalseAndDefault;
var
  Obj: TTestNullableContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableContact, FMap);
  CreateNullableTable;
  { Insert a row with NULL Age }
  Q := MakeQuery('INSERT INTO TestNullable (ID, Name, Age) VALUES (1, ''Test'', NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestNullableContact.Create;
  try
    Obj.Age := 99;
    Obj.FAgeStored := True;
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('SELECT Age FROM TestNullable WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Age'));
      AssertFalse('FAgeStored is False after null load', Obj.FAgeStored);
      AssertEquals('Age is default (0) after null load', 0, Obj.Age);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTINullable.ApplyPropertyValueSetsStoredTrue;
var
  Obj: TTestNullableContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableContact, FMap);
  CreateNullableTable;
  { Insert a row with non-null Age }
  Q := MakeQuery('INSERT INTO TestNullable (ID, Name, Age) VALUES (1, ''Test'', 30)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  Obj := TTestNullableContact.Create;
  try
    Obj.Age := 0;
    Obj.FAgeStored := False;
    FM := FMap.FindByPropertyName('Age');
    Q := MakeQuery('SELECT Age FROM TestNullable WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Age'));
      AssertTrue('FAgeStored is True after non-null load', Obj.FAgeStored);
      AssertEquals('Age value', 30, Obj.Age);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTINullable.PersistPropertyMethodBasedStored;
var
  Obj: TTestMethodStoredContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestMethodStoredContact, FMap);
  CreateNullableTable;
  { Test method returns False (null) }
  Obj := TTestMethodStoredContact.Create;
  try
    Obj.Score := 100;
    Obj.FScoreIsNull := True; { IsScoreStored returns False }
    FM := FMap.FindByPropertyName('Score');
    Q := MakeQuery('INSERT INTO TestNullable (Age) VALUES (:Score)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      // A stored method is the streaming "not default" test, not a null flag
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Score'));
      AssertFalse('Value written although the method returns False', Q.Params.ParamByName('Score').IsNull);
      AssertEquals('Value', 100, Q.Params.ParamByName('Score').AsInteger);
      // Explicitly nullable: the method decides
      FM.Options:=FM.Options+[foNullable];
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Score'));
      AssertTrue('Param is null with foNullable when method returns False', Q.Params.ParamByName('Score').IsNull);
      FM.Options:=FM.Options-[foNullable];
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
  { Test method returns True (non-null) }
  Obj := TTestMethodStoredContact.Create;
  try
    Obj.Score := 50;
    Obj.FScoreIsNull := False; { IsScoreStored returns True }
    FM := FMap.FindByPropertyName('Score');
    Q := MakeQuery('INSERT INTO TestNullable (Age) VALUES (:Score)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('Score'));
      AssertFalse('Param is NOT null when method returns True', Q.Params.ParamByName('Score').IsNull);
      AssertEquals('Param value', 50, Q.Params.ParamByName('Score').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTINullable.ApplyPropertyNullMethodBasedSetsDefault;
var
  Obj: TTestMethodStoredContact;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestMethodStoredContact, FMap);
  CreateNullableTable;
  { Insert a row with NULL Score }
  Q := MakeQuery('INSERT INTO TestNullable (ID, Age) VALUES (1, NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
  { Load NULL into method-based stored property — should set type default }
  Obj := TTestMethodStoredContact.Create;
  try
    Obj.Score := 99;
    Obj.FScoreIsNull := False; { IsScoreStored returns True initially }
    FM := FMap.FindByPropertyName('Score');
    Q := MakeQuery('SELECT Age FROM TestNullable WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('Age'));
      AssertEquals('Score is default (0) after null load', 0, Obj.Score);
      { Method-based stored is read-only — FScoreIsNull is NOT touched by framework }
      AssertFalse('FScoreIsNull unchanged (method is read-only)', Obj.FScoreIsNull);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestRTTINullable.PopulateFieldMapMethodBasedIsNullable;
var
  F: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestMethodStoredContact, FMap);
  F := FMap.FindByPropertyName('Score');
  AssertNotNull('Score field found', F);
  AssertFalse('No foNullable for a method-based stored (streaming style)', foNullable in F.Options);
end;

initialization
  RegisterTest(TTestRTTIPersister);
  RegisterTest(TTestRTTIClassProperty);
  RegisterTest(TTestRTTIErrorDiagnostics);
  RegisterTest(TTestRTTIPersisterRoundTrip);
  RegisterTest(TTestRTTIExtendedTypes);
  RegisterTest(TTestRTTINullable);
end.
