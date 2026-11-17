unit utc.opf.nullable;

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
  Opf.Types, Opf.Fields, Opf.Persister.RTTI, Opf.Nullable;

type

  { TTestManualNullable — manual IOPFNullable implementation (no base class) }

  TTestManualNullable = class(TPersistent, IOPFNullable)
  private
    FNulls: TStringList;
    FID: Integer;
    FAge: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    function IsNull(const aPropertyName: UTF8String): Boolean;
    procedure SetNull(const aPropertyName: UTF8String);
    procedure ClearNull(const aPropertyName: UTF8String);
  published
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write FAge;
    property Name: String read FName write FName;
  end;

  { TTestNullableObj — inherits from TOPFNullableObject }

  TTestNullableObj = class(TOPFNullableObject)
  private
    FID: Integer;
    FAge: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write FAge;
    property Name: String read FName write FName;
  end;

  { TTestNonNullableObj — class WITHOUT IOPFNullable (backward compat) }

  TTestNonNullableObj = class(TPersistent)
  private
    FID: Integer;
    FAge: Integer;
  published
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write FAge;
  end;

  { TTestNullableWithStored — has BOTH IOPFNullable AND stored specifier }

  TTestNullableWithStored = class(TPersistent, IOPFNullable)
  private
    FNulls: TStringList;
    FID: Integer;
    FScore: Integer;
    FScoreStored: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IsNull(const aPropertyName: UTF8String): Boolean;
    procedure SetNull(const aPropertyName: UTF8String);
    procedure ClearNull(const aPropertyName: UTF8String);
  published
    property ID: Integer read FID write FID;
    property Score: Integer read FScore write FScore stored FScoreStored;
  end;

  { TTestNullableBaseClass — tests for TOPFNullableObject }

  TTestNullableBaseClass = class(TTestCase)
  published
    procedure SetNullClearNullIsNullRoundTrip;
    procedure AllPropertiesDefaultNonNull;
  end;

  { TTestNullableInherited — tests with TOPFNullableObject-derived class }

  TTestNullableInherited = class(TTestCase)
  private
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure CreateTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PersistPropertyClearsParamWhenIsNullTrue;
    procedure PersistPropertyWritesValueWhenIsNullFalse;
    procedure ApplyPropertyCallsSetNullOnNullField;
    procedure ApplyPropertyCallsClearNullOnNonNullField;
  end;

  { TTestNullableBackwardCompat — class without IOPFNullable }

  TTestNullableBackwardCompat = class(TTestCase)
  private
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure CreateTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NonNullableClassNoNullHandling;
  end;

  { TTestNullablePrecedence — IOPFNullable takes precedence over stored }

  TTestNullablePrecedence = class(TTestCase)
  private
    FPersister: TRTTIPersister;
    FMap: TOPFieldMapList;
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure CreateTable;
    function MakeQuery(const aSQL: string): TSQLQuery;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IOPFNullableTakesPrecedenceOverStored;
  end;

implementation

{ TTestManualNullable }

constructor TTestManualNullable.Create;
begin
  inherited Create;
  FNulls := TStringList.Create;
  FNulls.Sorted := True;
  FNulls.Duplicates := dupIgnore;
end;

destructor TTestManualNullable.Destroy;
begin
  FNulls.Free;
  inherited Destroy;
end;

function TTestManualNullable.IsNull(const aPropertyName: UTF8String): Boolean;
begin
  Result := FNulls.IndexOf(aPropertyName) >= 0;
end;

procedure TTestManualNullable.SetNull(const aPropertyName: UTF8String);
begin
  FNulls.Add(aPropertyName);
end;

procedure TTestManualNullable.ClearNull(const aPropertyName: UTF8String);
var
  Idx: Integer;
begin
  Idx := FNulls.IndexOf(aPropertyName);
  if Idx >= 0 then
    FNulls.Delete(Idx);
end;

{ TTestNullableWithStored }

constructor TTestNullableWithStored.Create;
begin
  inherited Create;
  FNulls := TStringList.Create;
  FNulls.Sorted := True;
  FNulls.Duplicates := dupIgnore;
  FScoreStored := True;
end;

destructor TTestNullableWithStored.Destroy;
begin
  FNulls.Free;
  inherited Destroy;
end;

function TTestNullableWithStored.IsNull(const aPropertyName: UTF8String): Boolean;
begin
  Result := FNulls.IndexOf(aPropertyName) >= 0;
end;

procedure TTestNullableWithStored.SetNull(const aPropertyName: UTF8String);
begin
  FNulls.Add(aPropertyName);
end;

procedure TTestNullableWithStored.ClearNull(const aPropertyName: UTF8String);
var
  Idx: Integer;
begin
  Idx := FNulls.IndexOf(aPropertyName);
  if Idx >= 0 then
    FNulls.Delete(Idx);
end;

{ TTestNullableBaseClass }

procedure TTestNullableBaseClass.SetNullClearNullIsNullRoundTrip;
var
  Obj: TOPFNullableObject;
begin
  Obj := TOPFNullableObject.Create;
  try
    AssertFalse('Age starts non-null', Obj.IsNull('Age'));
    Obj.SetNull('Age');
    AssertTrue('Age is null after SetNull', Obj.IsNull('Age'));
    Obj.ClearNull('Age');
    AssertFalse('Age is non-null after ClearNull', Obj.IsNull('Age'));
  finally
    Obj.Free;
  end;
end;

procedure TTestNullableBaseClass.AllPropertiesDefaultNonNull;
var
  Obj: TOPFNullableObject;
begin
  Obj := TOPFNullableObject.Create;
  try
    AssertFalse('Age default non-null', Obj.IsNull('Age'));
    AssertFalse('Name default non-null', Obj.IsNull('Name'));
    AssertFalse('ID default non-null', Obj.IsNull('ID'));
  finally
    Obj.Free;
  end;
end;

{ TTestNullableInherited }

procedure TTestNullableInherited.SetUp;
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

procedure TTestNullableInherited.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestNullableInherited.CreateTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestNullObj (ID INTEGER PRIMARY KEY, Age INTEGER, Name TEXT)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestNullableInherited.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestNullableInherited.PersistPropertyClearsParamWhenIsNullTrue;
var
  Obj: TTestNullableObj;
  Q: TSQLQuery;
  FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableObj, FMap);
  CreateTable;
  Obj := TTestNullableObj.Create;
  try
    Obj.Age := 42;
    Obj.SetNull('Age');
    FM_Age := FMap.FindByPropertyName('Age');
    Q := MakeQuery('INSERT INTO TestNullObj (ID, Age) VALUES (:ID, :Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM_Age, Q.Params.ParamByName('Age'));
      AssertTrue('Age param is null', Q.Params.ParamByName('Age').IsNull);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestNullableInherited.PersistPropertyWritesValueWhenIsNullFalse;
var
  Obj: TTestNullableObj;
  Q: TSQLQuery;
  FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableObj, FMap);
  CreateTable;
  Obj := TTestNullableObj.Create;
  try
    Obj.Age := 42;
    { Age is NOT null — default state }
    FM_Age := FMap.FindByPropertyName('Age');
    Q := MakeQuery('INSERT INTO TestNullObj (ID, Age) VALUES (:ID, :Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM_Age, Q.Params.ParamByName('Age'));
      AssertFalse('Age param is not null', Q.Params.ParamByName('Age').IsNull);
      AssertEquals('Age param value', 42, Q.Params.ParamByName('Age').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestNullableInherited.ApplyPropertyCallsSetNullOnNullField;
var
  Obj: TTestNullableObj;
  Q: TSQLQuery;
  FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableObj, FMap);
  CreateTable;
  { Insert a row with NULL Age }
  Q := MakeQuery('INSERT INTO TestNullObj (ID, Name, Age) VALUES (1, ''Test'', NULL)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;

  Obj := TTestNullableObj.Create;
  try
    Obj.Age := 99;
    FM_Age := FMap.FindByPropertyName('Age');
    Q := MakeQuery('SELECT Age FROM TestNullObj WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM_Age, Q.FieldByName('Age'));
      AssertTrue('IsNull(Age) is True after loading NULL', Obj.IsNull('Age'));
      AssertEquals('Age is default (0)', 0, Obj.Age);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestNullableInherited.ApplyPropertyCallsClearNullOnNonNullField;
var
  Obj: TTestNullableObj;
  Q: TSQLQuery;
  FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableObj, FMap);
  CreateTable;
  { Insert a row with non-NULL Age }
  Q := MakeQuery('INSERT INTO TestNullObj (ID, Name, Age) VALUES (1, ''Test'', 30)');
  try
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;

  Obj := TTestNullableObj.Create;
  try
    Obj.SetNull('Age'); { Preload as null to verify ClearNull is called }
    FM_Age := FMap.FindByPropertyName('Age');
    Q := MakeQuery('SELECT Age FROM TestNullObj WHERE ID=1');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      Q.Open;
      FPersister.ApplyProperty(Obj, FM_Age, Q.FieldByName('Age'));
      AssertFalse('IsNull(Age) is False after loading value', Obj.IsNull('Age'));
      AssertEquals('Age is 30', 30, Obj.Age);
      Q.Close;
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ TTestNullableBackwardCompat }

procedure TTestNullableBackwardCompat.SetUp;
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

procedure TTestNullableBackwardCompat.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestNullableBackwardCompat.CreateTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestNonNull (ID INTEGER PRIMARY KEY, Age INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestNullableBackwardCompat.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestNullableBackwardCompat.NonNullableClassNoNullHandling;
var
  Obj: TTestNonNullableObj;
  Q: TSQLQuery;
  FM_Age: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNonNullableObj, FMap);
  CreateTable;
  { Verify that foNullable is NOT set for non-IOPFNullable class without stored specifiers }
  FM_Age := FMap.FindByPropertyName('Age');
  AssertFalse('foNullable not set for non-IOPFNullable', foNullable in FM_Age.Options);
  { Persist — should write value normally }
  Obj := TTestNonNullableObj.Create;
  try
    Obj.Age := 25;
    Q := MakeQuery('INSERT INTO TestNonNull (ID, Age) VALUES (:ID, :Age)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM_Age, Q.Params.ParamByName('Age'));
      AssertFalse('Age param not null', Q.Params.ParamByName('Age').IsNull);
      AssertEquals('Age param value', 25, Q.Params.ParamByName('Age').AsInteger);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ TTestNullablePrecedence }

procedure TTestNullablePrecedence.SetUp;
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

procedure TTestNullablePrecedence.TearDown;
begin
  FreeAndNil(FMap);
  FreeAndNil(FPersister);
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestNullablePrecedence.CreateTable;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'CREATE TABLE TestPrecedence (ID INTEGER PRIMARY KEY, Score INTEGER)';
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestNullablePrecedence.MakeQuery(const aSQL: string): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.Database := FConn;
  Result.Transaction := FTrans;
  Result.SQL.Text := aSQL;
end;

procedure TTestNullablePrecedence.IOPFNullableTakesPrecedenceOverStored;
var
  Obj: TTestNullableWithStored;
  Q: TSQLQuery;
  FM_Score: TOPFieldMap;
begin
  FPersister.PopulateFieldMap(TTestNullableWithStored, FMap);
  CreateTable;
  { IOPFNullable says null, stored specifier says stored=True.
    IOPFNullable should win. }
  Obj := TTestNullableWithStored.Create;
  try
    Obj.Score := 100;
    Obj.FScoreStored := True;  { stored=True — Level 0 would NOT persist as null }
    Obj.SetNull('Score');       { Level 1 says null — should take precedence }
    FM_Score := FMap.FindByPropertyName('Score');
    AssertTrue('foNullable set', foNullable in FM_Score.Options);
    Q := MakeQuery('INSERT INTO TestPrecedence (ID, Score) VALUES (:ID, :Score)');
    try
      if not FTrans.Active then
        FTrans.StartTransaction;
      FPersister.PersistProperty(Obj, FM_Score, Q.Params.ParamByName('Score'));
      AssertTrue('Score param is null (IOPFNullable wins over stored)', Q.Params.ParamByName('Score').IsNull);
    finally
      Q.Free;
    end;
  finally
    Obj.Free;
  end;
end;

{ Level 2 TTestTNullableLevel2 moved to utc.opf.persister.extrtti.pp }

initialization
  RegisterTest(TTestNullableBaseClass);
  RegisterTest(TTestNullableInherited);
  RegisterTest(TTestNullableBackwardCompat);
  RegisterTest(TTestNullablePrecedence);

end.
