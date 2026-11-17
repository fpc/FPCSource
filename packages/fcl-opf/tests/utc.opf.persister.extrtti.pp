unit utc.opf.persister.extrtti;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

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
  Opf.Types, Opf.Fields,
  Opf.Persister.ExtRTTI, Opf.Nullable;

type

  { TTestTNullableObj — Level 2: class with TNullable record fields }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTestTNullableObj = class(TObject)
  private
    FID: Integer;
    FNullAge: TNullableInteger;
    FNullName: TNullableString;
    FNullScore: TNullableDouble;
    FNullActive: TNullableBoolean;
    FNullBigVal: TNullableInt64;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestMixedObj — Level 2 + published properties coexisting }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTestMixedObj = class(TObject)
  private
    FID: Integer;
    FName: String;
    FNullAge: TNullableInteger;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestTNullableIntObj — Level 2 TNullable integration fixture }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTestTNullableIntObj = class(TObject)
  private
    FID: Integer;
    FNullAge: TNullableInteger;
    FNullName: TNullableString;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestTNullableMixedIntObj — Level 2 TNullable + published property mixed fixture }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TTestTNullableMixedIntObj = class(TObject)
  private
    FID: Integer;
    FLabel: String;
    FNullScore: TNullableDouble;
  published
    property ID: Integer read FID write FID;
    property Title: String read FLabel write FLabel;
  end;

  { TTestTNullableLevel2 — unit tests for TNullable Level 2 extended RTTI }

  TTestTNullableLevel2 = class(TTestCase)
  private
    FPersister: TExtRTTIPersister;
    FMap: TOPFieldMapList;
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure CreateTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PopulateFieldMapDiscoversTNullableFields;
    procedure PopulateFieldMapSetsNullableOption;
    procedure PersistPropertyWithHasValueTrue;
    procedure PersistPropertyWithHasValueFalse;
    procedure ApplyPropertyNonNullSetsHasValueTrue;
    procedure ApplyPropertyNullSetsHasValueFalse;
    procedure MixedClassPublishedAndTNullableCoexist;
    procedure PreDeclaredTypesIntegerStringDoubleBooleanInt64;
  end;

  { TTestTNullableRoundTripSQLite — Level 2 TNullable integration test }

  TTestTNullableRoundTripSQLite = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FPersister: TExtRTTIPersister;
    FMap: TOPFieldMapList;
    procedure CreateTNullableTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTNullableRoundTrip;
  end;

implementation

{ TTestTNullableLevel2 }

procedure TTestTNullableLevel2.SetUp;
begin
  inherited SetUp;
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
  FPersister := TExtRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestTNullableLevel2.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestTNullableLevel2.CreateTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestTNull (ID INTEGER PRIMARY KEY, NullAge INTEGER, NullName TEXT, NullScore REAL, NullActive INTEGER, NullBigVal INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestTNullableLevel2.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestTNullableLevel2.PopulateFieldMapDiscoversTNullableFields;
var
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  FM := FMap.FindByPropertyName('NullAge');
  AssertNotNull('NullAge discovered', FM);
  AssertEquals('NullAge inner type is tkInteger', Ord(tkInteger), Ord(FM.PropertyKind));
  FM := FMap.FindByPropertyName('NullName');
  AssertNotNull('NullName discovered', FM);
  AssertTrue('NullName inner type is string', FM.PropertyKind in [tkAString, tkSString, tkUString, tkWString]);
  FM := FMap.FindByPropertyName('NullScore');
  AssertNotNull('NullScore discovered', FM);
  AssertEquals('NullScore inner type is tkFloat', Ord(tkFloat), Ord(FM.PropertyKind));
  FM := FMap.FindByPropertyName('NullActive');
  AssertNotNull('NullActive discovered', FM);
  AssertEquals('NullActive inner type is tkBool', Ord(tkBool), Ord(FM.PropertyKind));
  FM := FMap.FindByPropertyName('NullBigVal');
  AssertNotNull('NullBigVal discovered', FM);
  AssertEquals('NullBigVal inner type is tkInt64', Ord(tkInt64), Ord(FM.PropertyKind));
end;

procedure TTestTNullableLevel2.PopulateFieldMapSetsNullableOption;
var
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  FM := FMap.FindByPropertyName('NullAge');
  AssertTrue('foNullable set on NullAge', foNullable in FM.Options);
  AssertNull('PropInfo is nil for TNullable field', FM.PropInfo);
  AssertTrue('Marked as nullable record', FM.MemberKind = omkNullableRecord);
  AssertTrue('RTTI field kept', FM.RttiMember <> nil);
  AssertTrue('Data left to the user', FM.Data = nil);
end;

procedure TTestTNullableLevel2.PersistPropertyWithHasValueTrue;
var
  Obj: TTestTNullableObj;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  CreateTable;
  Obj := TTestTNullableObj.Create;
  try
    Obj.FNullAge := 42;
    FM := FMap.FindByPropertyName('NullAge');
    Q := MakeQuery('INSERT INTO TestTNull (ID, NullAge) VALUES (:ID, :NullAge)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('NullAge'));
      AssertFalse('NullAge param is not null', Q.Params.ParamByName('NullAge').IsNull);
      AssertEquals('NullAge param value', 42, Q.Params.ParamByName('NullAge').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestTNullableLevel2.PersistPropertyWithHasValueFalse;
var
  Obj: TTestTNullableObj;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  CreateTable;
  Obj := TTestTNullableObj.Create;
  try
    { FNullAge defaults to HasValue=False (TNullable.Initialize sets FHasValue=False) }
    FM := FMap.FindByPropertyName('NullAge');
    Q := MakeQuery('INSERT INTO TestTNull (ID, NullAge) VALUES (:ID, :NullAge)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM, Q.Params.ParamByName('NullAge'));
      AssertTrue('NullAge param is null', Q.Params.ParamByName('NullAge').IsNull);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestTNullableLevel2.ApplyPropertyNonNullSetsHasValueTrue;
var
  Obj: TTestTNullableObj;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  CreateTable;
  Q := MakeQuery('INSERT INTO TestTNull (ID, NullAge) VALUES (1, 55)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;

  Obj := TTestTNullableObj.Create;
  try
    FM := FMap.FindByPropertyName('NullAge');
    Q := MakeQuery('SELECT NullAge FROM TestTNull WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('NullAge'));
      AssertTrue('FNullAge.HasValue is True', Obj.FNullAge.HasValue);
      AssertEquals('FNullAge.Value is 55', 55, Obj.FNullAge.Value);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestTNullableLevel2.ApplyPropertyNullSetsHasValueFalse;
var
  Obj: TTestTNullableObj;
  Q: TSQLQuery;
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  CreateTable;
  Q := MakeQuery('INSERT INTO TestTNull (ID, NullAge) VALUES (1, NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;

  Obj := TTestTNullableObj.Create;
  try
    Obj.FNullAge := 99;  { Pre-set to verify it gets cleared }
    FM := FMap.FindByPropertyName('NullAge');
    Q := MakeQuery('SELECT NullAge FROM TestTNull WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM, Q.FieldByName('NullAge'));
      AssertFalse('FNullAge.HasValue is False after NULL load', Obj.FNullAge.HasValue);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestTNullableLevel2.MixedClassPublishedAndTNullableCoexist;
var
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestMixedObj, FMap);
  { Published properties should be discovered }
  FM := FMap.FindByPropertyName('ID');
  AssertNotNull('ID from published', FM);
  AssertNotNull('ID has PropInfo', FM.PropInfo);
  FM := FMap.FindByPropertyName('Name');
  AssertNotNull('Name from published', FM);
  AssertNotNull('Name has PropInfo', FM.PropInfo);
  { TNullable field should also be discovered }
  FM := FMap.FindByPropertyName('NullAge');
  AssertNotNull('NullAge from extended RTTI', FM);
  AssertNull('NullAge PropInfo is nil', FM.PropInfo);
  AssertTrue('NullAge has foNullable', foNullable in FM.Options);
  AssertEquals('NullAge inner type is tkInteger', Ord(tkInteger), Ord(FM.PropertyKind));
  { Total: 3 fields }
  AssertEquals('Total field count', 3, FMap.Count);
end;

procedure TTestTNullableLevel2.PreDeclaredTypesIntegerStringDoubleBooleanInt64;
var
  FM: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestTNullableObj, FMap);
  { Integer }
  FM := FMap.FindByPropertyName('NullAge');
  AssertNotNull('TNullableInteger discovered', FM);
  AssertEquals('Integer inner type', Ord(tkInteger), Ord(FM.PropertyKind));
  { String }
  FM := FMap.FindByPropertyName('NullName');
  AssertNotNull('TNullableString discovered', FM);
  AssertTrue('String inner type', FM.PropertyKind in [tkAString, tkSString, tkUString, tkWString]);
  { Double }
  FM := FMap.FindByPropertyName('NullScore');
  AssertNotNull('TNullableDouble discovered', FM);
  AssertEquals('Double inner type', Ord(tkFloat), Ord(FM.PropertyKind));
  { Boolean }
  FM := FMap.FindByPropertyName('NullActive');
  AssertNotNull('TNullableBoolean discovered', FM);
  AssertEquals('Boolean inner type', Ord(tkBool), Ord(FM.PropertyKind));
  { Int64 }
  FM := FMap.FindByPropertyName('NullBigVal');
  AssertNotNull('TNullableInt64 discovered', FM);
  AssertEquals('Int64 inner type', Ord(tkInt64), Ord(FM.PropertyKind));
end;

{ TTestTNullableRoundTripSQLite }

procedure TTestTNullableRoundTripSQLite.SetUp;
begin
  inherited SetUp;
  FConn := TSQLite3Connection.Create(nil);
  FConn.DatabaseName := ':memory:';
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Database := FConn;
  FConn.Transaction := FTrans;
  FConn.Open;
  FPersister := TExtRTTIPersister.Create;
  FMap := TOPFieldMapList.Create(TOPFieldMap);
end;

procedure TTestTNullableRoundTripSQLite.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestTNullableRoundTripSQLite.CreateTNullableTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestTNullable (ID INTEGER PRIMARY KEY, NullAge INTEGER, NullName TEXT, NullScore REAL, Title TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestTNullableRoundTripSQLite.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestTNullableRoundTripSQLite.TestTNullableRoundTrip;
var
  Obj: TTestTNullableIntObj;
  MixedObj: TTestTNullableMixedIntObj;
  Q: TSQLQuery;
  FM_ID, FM_NullAge, FM_NullName: TOPFieldMap;
  MixMap: TOPFieldMapList;
  FM_MixID, FM_Title, FM_NullScore: TOPFieldMap;
begin
  { --- Part 1: TNullable with value and TNullable as null --- }
  FPersister.PopulateFieldMap(TTestTNullableIntObj, FMap);
  CreateTNullableTable;

  FM_ID := FMap.FindByPropertyName('ID');
  FM_NullAge := FMap.FindByPropertyName('NullAge');
  FM_NullName := FMap.FindByPropertyName('NullName');
  AssertNotNull('FM_ID', FM_ID);
  AssertNotNull('FM_NullAge', FM_NullAge);
  AssertNotNull('FM_NullName', FM_NullName);

  { Save: NullAge=42 (has value), NullName=null (no value) }
  Obj := TTestTNullableIntObj.Create;
  try
    Obj.FID := 1;
    Obj.FNullAge := 42;
    { FNullName defaults to HasValue=False }
    Q := MakeQuery('INSERT INTO TestTNullable (ID, NullAge, NullName) VALUES (:ID, :NullAge, :NullName)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM_ID, Q.Params.ParamByName('ID'));
      FPersister.PersistProperty(Obj, FM_NullAge, Q.Params.ParamByName('NullAge'));
      FPersister.PersistProperty(Obj, FM_NullName, Q.Params.ParamByName('NullName'));
      AssertFalse('NullAge param not null', Q.Params.ParamByName('NullAge').IsNull);
      AssertEquals('NullAge param value', 42, Q.Params.ParamByName('NullAge').AsInteger);
      AssertTrue('NullName param is null', Q.Params.ParamByName('NullName').IsNull);
      Q.ExecSQL;
      FTrans.Commit;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;

  { Load back }
  Obj := TTestTNullableIntObj.Create;
  try
    Q := MakeQuery('SELECT ID, NullAge, NullName FROM TestTNullable WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM_ID, Q.FieldByName('ID'));
      FPersister.ApplyProperty(Obj, FM_NullAge, Q.FieldByName('NullAge'));
      FPersister.ApplyProperty(Obj, FM_NullName, Q.FieldByName('NullName'));
      AssertEquals('ID loaded', 1, Obj.FID);
      AssertTrue('NullAge HasValue', Obj.FNullAge.HasValue);
      AssertEquals('NullAge value', 42, Obj.FNullAge.Value);
      AssertFalse('NullName HasValue is False', Obj.FNullName.HasValue);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;

  { --- Part 2: Mixed class with published properties AND TNullable fields --- }
  MixMap := TOPFieldMapList.Create(TOPFieldMap);
  try
    FPersister.PopulateFieldMap(TTestTNullableMixedIntObj, MixMap);
    FM_MixID := MixMap.FindByPropertyName('ID');
    FM_Title := MixMap.FindByPropertyName('Title');
    FM_NullScore := MixMap.FindByPropertyName('NullScore');
    AssertNotNull('FM_MixID', FM_MixID);
    AssertNotNull('FM_Title', FM_Title);
    AssertNotNull('FM_NullScore', FM_NullScore);

    MixedObj := TTestTNullableMixedIntObj.Create;
    try
      MixedObj.FID := 2;
      MixedObj.FLabel := 'Hello';
      MixedObj.FNullScore := 9.5;
      Q := MakeQuery('INSERT INTO TestTNullable (ID, Title, NullScore) VALUES (:ID, :Title, :NullScore)');
      try
        if not FTrans.Active then
          FTrans.StartTransaction;
        FPersister.PersistProperty(MixedObj, FM_MixID, Q.Params.ParamByName('ID'));
        FPersister.PersistProperty(MixedObj, FM_Title, Q.Params.ParamByName('Title'));
        FPersister.PersistProperty(MixedObj, FM_NullScore, Q.Params.ParamByName('NullScore'));
        Q.ExecSQL;
        FTrans.Commit;
      finally
        Q.Free;
      end;
    finally
      MixedObj.Free;
    end;

    { Load back }
    MixedObj := TTestTNullableMixedIntObj.Create;
    try
      Q := MakeQuery('SELECT ID, Title, NullScore FROM TestTNullable WHERE ID=2');
      try
        if not FTrans.Active then
          FTrans.StartTransaction;
        Q.Open;
        FPersister.ApplyProperty(MixedObj, FM_MixID, Q.FieldByName('ID'));
        FPersister.ApplyProperty(MixedObj, FM_Title, Q.FieldByName('Title'));
        FPersister.ApplyProperty(MixedObj, FM_NullScore, Q.FieldByName('NullScore'));
        AssertEquals('Mixed ID', 2, MixedObj.FID);
        AssertEquals('Mixed Title', 'Hello', MixedObj.FLabel);
        AssertTrue('NullScore HasValue', MixedObj.FNullScore.HasValue);
        AssertEquals('NullScore value', 9.5, MixedObj.FNullScore.Value, 0.001);
        Q.Close;
      finally
        Q.Free;
      end;
    finally
      MixedObj.Free;
    end;
  finally
    MixMap.Free;
  end;
end;

initialization
  RegisterTest(TTestTNullableLevel2);
  RegisterTest(TTestTNullableRoundTripSQLite);

end.
