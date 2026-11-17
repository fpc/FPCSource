unit utc.opf.nulldirty;

{ Dirty tracking of null state and of values the trackers used to mishandle:
  - Level 0 (stored specifier), Level 1 (IOPFNullable), Level 2 (TNullable<T>)
  - a change of only the null state must be detected
  - variant properties changing type must not raise
  - unreadable fields are always dirty
  - the persister and the trackers agree on what is null
  - end to end: setting a field to NULL is written to the database }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, Variants, DB, sqldb, sqlite3conn,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Manager, Opf.Sql, Opf.Driver,
  Opf.Persister.RTTI, Opf.Nullable, Opf.FieldValue, Opf.SnapshotHooks, Opf.Snapshot,
  Opf.Memento
{$IFNDEF VER3_2}
  , Opf.Persister.ExtRTTI, Opf.Memento.ExtRTTI
{$ENDIF}
  ;

type

  { Level 0: a field-based stored specifier marks the value as present }

  TND0Person = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FAge: Integer;
    FAgeSet: Boolean;
    FName: String;
    FNameSet: Boolean;
    FState: TObjectPersistenceState;
    procedure SetAge(aValue: Integer);
    procedure SetName(const aValue: String);
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    // Make the property null without touching the value
    procedure ClearAge;
    procedure ClearName;
  published
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write SetAge stored FAgeSet;
    property Name: String read FName write SetName stored FNameSet;
  end;

  { Level 1: IOPFNullable }

  {$M+}
  TND1Person = class(TOPFNullableObject, IOPPersistenceState)
  private
    FID: Integer;
    FAge: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write FAge;
    property Name: String read FName write FName;
  end;
  {$M-}

{$IFNDEF VER3_2}
  { Level 2: TNullable<T> record fields }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TND2Person = class(TPersistent)
  private
    FID: Integer;
    FNullAge: TNullableInteger;
    FNullName: TNullableString;
  public
    property NullAge: TNullableInteger read FNullAge write FNullAge;
    property NullName: TNullableString read FNullName write FNullName;
  published
    property ID: Integer read FID write FID;
  end;
{$ENDIF}

  { Variant property }

  TNDVariant = class(TPersistent)
  private
    FID: Integer;
    FV: Variant;
  published
    property ID: Integer read FID write FID;
    property V: Variant read FV write FV;
  end;

  { TTestNullDirty }

  TTestNullDirty = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FTracker: TOPSnapshotManager;
    function MapOf(aClass: TClass): TOPFieldMapList;
    function Changes(aObject: TObject; aMap: TOPFieldMapList): TUTF8StringArray;
    function HasName(const aNames: TUTF8StringArray; const aName: UTF8String): Boolean;
    procedure AssertParity(aObject: TObject; aMap: TOPFieldMapList; const aProp: UTF8String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Level 0
    procedure TestLevel0NullOnlyChangeIsDirty;
    procedure TestLevel0ValueToNullIsDirty;
    procedure TestLevel0NullToValueIsDirty;
    procedure TestLevel0NullToNullIsClean;
    procedure TestLevel0NullValueChangeWhileNullIsClean;
    procedure TestLevel0Unchanged;
    // Level 1
    procedure TestLevel1SetNullIsDirty;
    procedure TestLevel1ClearNullIsDirty;
    procedure TestLevel1NullToNullIsClean;
    procedure TestLevel1OtherPropertyUnaffected;
    // Parity persister / trackers
    procedure TestParityLevel0;
    procedure TestParityLevel1;
    procedure TestNonNullableNeverNull;
    // Variants
    procedure TestVariantTypeChangeDoesNotRaise;
    procedure TestVariantStringToNumberIsDirty;
    procedure TestVariantSameValueClean;
    procedure TestVariantEmptyToNullIsDirty;
    procedure TestVariantArrayChange;
    procedure TestVariantArrayUnchanged;
    // Unreadable
    procedure TestUnreadableFieldAlwaysDirty;
    procedure TestUnreadableFieldCaptureDoesNotRaise;
    // OPFSameVariant
    procedure TestSameVariantRules;
{$IFNDEF VER3_2}
    // Level 2
    procedure TestLevel2CaptureDoesNotRaise;
    procedure TestLevel2NullToValueIsDirty;
    procedure TestLevel2ValueToNullIsDirty;
    procedure TestLevel2ValueChangeIsDirty;
    procedure TestLevel2UnchangedIsClean;
    procedure TestLevel2IsFieldNull;
    // Memento
    procedure TestMementoLevel0NullOnlyChange;
    procedure TestMementoLevel1NullOnlyChange;
    procedure TestMementoLevel1RestoresNullFlag;
    procedure TestMementoLevel0RestoresStoredFlag;
    procedure TestMementoVariantTypeChange;
{$ENDIF}
  end;

  { TTestNullDirtySave: end to end against SQLite }

  TTestNullDirtySave = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FBuilder: TOPSQLBuilder;
    FManager: TOPManager;
    FTracker: TOPSnapshotManager;
    procedure Exec(const aSQL: String);
    function AgeIsNull(const aTable: String; aID: Integer): Boolean;
    function AgeValue(const aTable: String; aID: Integer): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLevel0SetNullWritesNull;
    procedure TestLevel0NullToValueWritesValue;
    procedure TestLevel1SetNullWritesNull;
    procedure TestLevel1ClearNullWritesValue;
    procedure TestUnchangedSkipsUpdate;
  end;

implementation

{ TND0Person }

procedure TND0Person.SetAge(aValue: Integer);
begin
  FAge:=aValue;
  FAgeSet:=True;
end;

procedure TND0Person.SetName(const aValue: String);
begin
  FName:=aValue;
  FNameSet:=True;
end;

function TND0Person.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TND0Person.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

procedure TND0Person.ClearAge;
begin
  FAgeSet:=False;
end;

procedure TND0Person.ClearName;
begin
  FNameSet:=False;
end;

{ TND1Person }

function TND1Person.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TND1Person.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestNullDirty }

function TTestNullDirty.MapOf(aClass: TClass): TOPFieldMapList;
begin
  Result:=FRegistry.FindPersister(aClass).FieldMap;
end;

function TTestNullDirty.Changes(aObject: TObject; aMap: TOPFieldMapList): TUTF8StringArray;
begin
  Result:=Nil;
  AssertTrue('Object is tracked', FTracker.GetChanges(aObject, aMap, Result));
end;

function TTestNullDirty.HasName(const aNames: TUTF8StringArray; const aName: UTF8String): Boolean;
var
  S: UTF8String;
begin
  Result:=False;
  for S in aNames do
    if SameText(S, aName) then
      Exit(True);
end;

procedure TTestNullDirty.AssertParity(aObject: TObject; aMap: TOPFieldMapList; const aProp: UTF8String);
var
  P: TRTTIPersister;
  Prm: TParam;
  F: TOPFieldMap;
  V: Variant;
begin
  F:=aMap.FindByPropertyName(aProp);
  P:=TRTTIPersister.Create;
  Prm:=TParam.Create(Nil);
  try
    P.PersistProperty(aObject, F, Prm);
    AssertEquals('Persister and OPFIsFieldNull agree on '+aProp, Prm.IsNull, OPFIsFieldNull(aObject, F));
    AssertTrue('Readable', OPFReadFieldValue(aObject, F, V));
    AssertEquals('Captured value null state agrees on '+aProp, Prm.IsNull, VarIsNull(V));
  finally
    Prm.Free;
    P.Free;
  end;
end;

procedure TTestNullDirty.SetUp;
begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  FRegistry.RegisterPersister(TND0Person, TRTTIPersister);
  FRegistry.RegisterPersister(TND1Person, TRTTIPersister);
  FTracker:=TOPSnapshotManager.Create(Nil);
  FTracker.Manager:=Nil;
end;

procedure TTestNullDirty.TearDown;
begin
  FreeAndNil(FTracker);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

procedure TTestNullDirty.TestLevel0NullOnlyChangeIsDirty;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    P.Age:=42;
    FTracker.Capture(P, MapOf(TND0Person));
    P.ClearAge; // FAge still 42
    AssertTrue('Null-only change detected', HasName(Changes(P, MapOf(TND0Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel0ValueToNullIsDirty;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    P.Age:=42;
    FTracker.Capture(P, MapOf(TND0Person));
    P.Age:=0;
    P.ClearAge;
    AssertTrue('Value to null', HasName(Changes(P, MapOf(TND0Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel0NullToValueIsDirty;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    FTracker.Capture(P, MapOf(TND0Person)); // Age null, value 0
    P.Age:=0; // Same raw value, but no longer null
    AssertTrue('Null to value', HasName(Changes(P, MapOf(TND0Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel0NullToNullIsClean;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    FTracker.Capture(P, MapOf(TND0Person));
    AssertFalse('Null to null', HasName(Changes(P, MapOf(TND0Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel0NullValueChangeWhileNullIsClean;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    FTracker.Capture(P, MapOf(TND0Person));
    // The backing value changes, but the property stays null: nothing to write
    P.FAge:=99;
    AssertFalse('Hidden value change while null', HasName(Changes(P, MapOf(TND0Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel0Unchanged;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    P.Age:=1;
    P.Name:='x';
    FTracker.Capture(P, MapOf(TND0Person));
    AssertEquals('No changes', 0, Length(Changes(P, MapOf(TND0Person))));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel1SetNullIsDirty;
var
  P: TND1Person;
begin
  P:=TND1Person.Create;
  try
    P.Age:=42;
    FTracker.Capture(P, MapOf(TND1Person));
    P.SetNull('Age');
    AssertTrue('SetNull detected', HasName(Changes(P, MapOf(TND1Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel1ClearNullIsDirty;
var
  P: TND1Person;
begin
  P:=TND1Person.Create;
  try
    P.SetNull('Age');
    FTracker.Capture(P, MapOf(TND1Person));
    P.ClearNull('Age');
    AssertTrue('ClearNull detected', HasName(Changes(P, MapOf(TND1Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel1NullToNullIsClean;
var
  P: TND1Person;
begin
  P:=TND1Person.Create;
  try
    P.SetNull('Age');
    FTracker.Capture(P, MapOf(TND1Person));
    P.Age:=7; // Ignored: still null
    AssertFalse('Still null', HasName(Changes(P, MapOf(TND1Person)), 'Age'));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel1OtherPropertyUnaffected;
var
  P: TND1Person;
  N: TUTF8StringArray;
begin
  P:=TND1Person.Create;
  try
    FTracker.Capture(P, MapOf(TND1Person));
    P.SetNull('Age');
    N:=Changes(P, MapOf(TND1Person));
    AssertEquals('Only Age', 1, Length(N));
    AssertEquals('Age', 'Age', N[0]);
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestParityLevel0;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    AssertParity(P, MapOf(TND0Person), 'Age');
    P.Age:=5;
    AssertParity(P, MapOf(TND0Person), 'Age');
    P.ClearAge;
    AssertParity(P, MapOf(TND0Person), 'Age');
    P.Name:='n';
    AssertParity(P, MapOf(TND0Person), 'Name');
    P.ClearName;
    AssertParity(P, MapOf(TND0Person), 'Name');
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestParityLevel1;
var
  P: TND1Person;
begin
  P:=TND1Person.Create;
  try
    AssertParity(P, MapOf(TND1Person), 'Age');
    P.SetNull('Age');
    AssertParity(P, MapOf(TND1Person), 'Age');
    P.ClearNull('Age');
    AssertParity(P, MapOf(TND1Person), 'Age');
    P.SetNull('Name');
    AssertParity(P, MapOf(TND1Person), 'Name');
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestNonNullableNeverNull;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    AssertFalse('ID has no dynamic stored specifier',
      foNullable in MapOf(TND0Person).FindByPropertyName('ID').Options);
    AssertFalse('Never null', OPFIsFieldNull(P, MapOf(TND0Person).FindByPropertyName('ID')));
  finally
    P.Free;
  end;
end;

function VariantMap: TOPFieldMapList;
var
  F: TOPFieldMap;
begin
  Result:=TOPFieldMapList.Create(TOPFieldMap);
  F:=Result.AddField('ID', 'ID');
  F.PropertyKind:=tkInteger;
  F.Options:=[foInKey];
  F:=Result.AddField('V', 'V');
  F.PropertyKind:=tkVariant;
  F.Options:=[foInUpdate];
end;

procedure TTestNullDirty.TestVariantTypeChangeDoesNotRaise;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    O.V:='abc';
    FTracker.Capture(O, M);
    O.V:=5;
    AssertTrue('Type change is dirty and does not raise', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestVariantStringToNumberIsDirty;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    O.V:='1';
    FTracker.Capture(O, M);
    O.V:=1;
    AssertTrue('''1'' -> 1 is a change of the stored value', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestVariantSameValueClean;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    O.V:='abc';
    FTracker.Capture(O, M);
    O.V:='ab'+'c';
    AssertFalse('Same value', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestVariantEmptyToNullIsDirty;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    FTracker.Capture(O, M);
    O.V:=Null;
    AssertTrue('Unassigned -> Null', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestVariantArrayChange;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    O.V:=VarArrayOf([1, 2, 3]);
    FTracker.Capture(O, M);
    O.V:=VarArrayOf([1, 2, 4]);
    AssertTrue('Array element change', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestVariantArrayUnchanged;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    O.V:=VarArrayOf([1, 'a']);
    FTracker.Capture(O, M);
    O.V:=VarArrayOf([1, 'a']);
    AssertFalse('Equal arrays', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

function GhostMap: TOPFieldMapList;
var
  F: TOPFieldMap;
begin
  Result:=VariantMap;
  F:=Result.AddField('ghost_col', 'Ghost');
  F.PropertyKind:=tkInteger;
  F.Options:=[foInUpdate];
end;

procedure TTestNullDirty.TestUnreadableFieldAlwaysDirty;
var
  O: TNDVariant;
  M: TOPFieldMapList;
begin
  O:=TNDVariant.Create;
  M:=GhostMap;
  try
    FTracker.Capture(O, M);
    AssertTrue('Unreadable field reported', HasName(Changes(O, M), 'Ghost'));
    AssertFalse('Readable unchanged field not reported', HasName(Changes(O, M), 'V'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestUnreadableFieldCaptureDoesNotRaise;
var
  O: TNDVariant;
  M: TOPFieldMapList;
  V: Variant;
begin
  O:=TNDVariant.Create;
  M:=GhostMap;
  try
    AssertFalse('Cannot read', OPFReadFieldValue(O, M.FindByPropertyName('Ghost'), V));
    AssertTrue('Unassigned', VarIsEmpty(V));
    OPFCapturePropertyValues(O, M);
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestSameVariantRules;
begin
  AssertTrue('Empty = Empty', OPFSameVariant(Unassigned, Unassigned));
  AssertTrue('Null = Null', OPFSameVariant(Null, Null));
  AssertFalse('Empty <> Null', OPFSameVariant(Unassigned, Null));
  AssertFalse('Null <> 0', OPFSameVariant(Null, 0));
  AssertFalse('Empty <> ''''', OPFSameVariant(Unassigned, ''));
  AssertTrue('1 = 1', OPFSameVariant(1, 1));
  AssertFalse('1 <> 2', OPFSameVariant(1, 2));
  AssertFalse('''1'' <> 1', OPFSameVariant('1', 1));
  AssertFalse('''abc'' <> 5 without raising', OPFSameVariant('abc', 5));
  AssertTrue('Case sensitive equal', OPFSameVariant('Abc', 'Abc'));
  AssertFalse('Case sensitive', OPFSameVariant('abc', 'ABC'));
  AssertFalse('Array <> scalar', OPFSameVariant(VarArrayOf([1]), 1));
  AssertFalse('Different bounds', OPFSameVariant(VarArrayOf([1]), VarArrayOf([1, 2])));
  AssertTrue('Nested arrays', OPFSameVariant(VarArrayOf([VarArrayOf([1])]), VarArrayOf([VarArrayOf([1])])));
end;

{$IFNDEF VER3_2}

function Level2Map: TOPFieldMapList;
var
  P: TExtRTTIPersister;
begin
  Result:=TOPFieldMapList.Create(TOPFieldMap);
  // As the registry does
  Result.DefaultFieldOptions:=[foInSelect, foInInsert, foInUpdate];
  P:=TExtRTTIPersister.Create;
  try
    P.PopulateFieldMap(TND2Person, Result);
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestLevel2CaptureDoesNotRaise;
var
  O: TND2Person;
  M: TOPFieldMapList;
begin
  O:=TND2Person.Create;
  M:=Level2Map;
  try
    AssertNotNull('Level 2 field mapped', M.FindByPropertyName('NullAge'));
    FTracker.Capture(O, M);
    AssertEquals('No changes', 0, Length(Changes(O, M)));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestLevel2NullToValueIsDirty;
var
  O: TND2Person;
  M: TOPFieldMapList;
begin
  O:=TND2Person.Create;
  M:=Level2Map;
  try
    FTracker.Capture(O, M);
    O.FNullAge.Value:=0;
    AssertTrue('Null to 0', HasName(Changes(O, M), 'NullAge'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestLevel2ValueToNullIsDirty;
var
  O: TND2Person;
  M: TOPFieldMapList;
begin
  O:=TND2Person.Create;
  M:=Level2Map;
  try
    O.FNullName.Value:='x';
    FTracker.Capture(O, M);
    O.FNullName.Clear;
    AssertTrue('Value to null', HasName(Changes(O, M), 'NullName'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestLevel2ValueChangeIsDirty;
var
  O: TND2Person;
  M: TOPFieldMapList;
begin
  O:=TND2Person.Create;
  M:=Level2Map;
  try
    O.FNullAge.Value:=1;
    FTracker.Capture(O, M);
    O.FNullAge.Value:=2;
    AssertTrue('Value change', HasName(Changes(O, M), 'NullAge'));
    AssertFalse('Other field unchanged', HasName(Changes(O, M), 'NullName'));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestLevel2UnchangedIsClean;
var
  O: TND2Person;
  M: TOPFieldMapList;
begin
  O:=TND2Person.Create;
  M:=Level2Map;
  try
    O.FNullAge.Value:=1;
    O.FNullName.Value:='a';
    FTracker.Capture(O, M);
    O.FNullAge.Value:=1;
    AssertEquals('No changes', 0, Length(Changes(O, M)));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestLevel2IsFieldNull;
var
  O: TND2Person;
  M: TOPFieldMapList;
begin
  O:=TND2Person.Create;
  M:=Level2Map;
  try
    AssertTrue('Null initially', OPFIsFieldNull(O, M.FindByPropertyName('NullAge')));
    O.FNullAge.Value:=3;
    AssertFalse('Not null with a value', OPFIsFieldNull(O, M.FindByPropertyName('NullAge')));
  finally
    M.Free;
    O.Free;
  end;
end;

procedure TTestNullDirty.TestMementoLevel0NullOnlyChange;
var
  P: TND0Person;
  Mem: TOPMemento;
  N: TUTF8StringArray;
begin
  P:=TND0Person.Create;
  try
    P.Age:=42;
    Mem:=OPDefaultMementoFactory(P, MapOf(TND0Person));
    try
      P.ClearAge;
      N:=Mem.GetChanges(P);
      AssertTrue('Memento detects null-only change', HasName(N, 'Age'));
    finally
      Mem.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestMementoLevel1NullOnlyChange;
var
  P: TND1Person;
  Mem: TOPMemento;
begin
  P:=TND1Person.Create;
  try
    P.Age:=42;
    Mem:=OPDefaultMementoFactory(P, MapOf(TND1Person));
    try
      P.SetNull('Age');
      AssertTrue('Memento detects SetNull', HasName(Mem.GetChanges(P), 'Age'));
      P.ClearNull('Age');
      AssertFalse('Back to the captured state', HasName(Mem.GetChanges(P), 'Age'));
    finally
      Mem.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestMementoLevel1RestoresNullFlag;
var
  P: TND1Person;
  Mem: TOPMemento;
begin
  P:=TND1Person.Create;
  try
    P.SetNull('Age');
    Mem:=OPDefaultMementoFactory(P, MapOf(TND1Person));
    try
      P.ClearNull('Age');
      P.Age:=5;
      Mem.RestoreTo(P);
      AssertTrue('Null flag restored', P.IsNull('Age'));
    finally
      Mem.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestMementoLevel0RestoresStoredFlag;
var
  P: TND0Person;
  Mem: TOPMemento;
begin
  P:=TND0Person.Create;
  try
    Mem:=OPDefaultMementoFactory(P, MapOf(TND0Person)); // Age null
    try
      P.Age:=5; // Setter marks it stored
      Mem.RestoreTo(P);
      AssertFalse('Stored flag restored after the setter ran', P.FAgeSet);
    finally
      Mem.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TTestNullDirty.TestMementoVariantTypeChange;
var
  O: TNDVariant;
  M: TOPFieldMapList;
  Mem: TOPMemento;
begin
  O:=TNDVariant.Create;
  M:=VariantMap;
  try
    O.V:='abc';
    Mem:=OPDefaultMementoFactory(O, M);
    try
      O.V:=5;
      AssertTrue('Type change detected without raising', HasName(Mem.GetChanges(O), 'V'));
    finally
      Mem.Free;
    end;
  finally
    M.Free;
    O.Free;
  end;
end;

{$ENDIF}

{ TTestNullDirtySave }

procedure TTestNullDirtySave.Exec(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestNullDirtySave.AgeIsNull(const aTable: String; aID: Integer): Boolean;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:=Format('SELECT Age FROM %s WHERE ID=%d', [aTable, aID]);
    Q.Open;
    Result:=Q.Fields[0].IsNull;
    Q.Close;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestNullDirtySave.AgeValue(const aTable: String; aID: Integer): Integer;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:=Format('SELECT Age FROM %s WHERE ID=%d', [aTable, aID]);
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestNullDirtySave.SetUp;
var
  C: TOPConnection;
begin
  inherited SetUp;
  FConn:=TSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  Exec('CREATE TABLE ND0Person (ID INTEGER PRIMARY KEY, Age INTEGER, Name TEXT)');
  Exec('CREATE TABLE ND1Person (ID INTEGER PRIMARY KEY, Age INTEGER, Name TEXT)');
  Exec('INSERT INTO ND0Person VALUES (1, 42, ''a'')');
  Exec('INSERT INTO ND0Person VALUES (2, NULL, ''b'')');
  Exec('INSERT INTO ND1Person VALUES (1, 42, ''a'')');
  Exec('INSERT INTO ND1Person VALUES (2, NULL, ''b'')');
  FRegistry:=TOPRegistry.Create(Nil);
  FRegistry.RegisterPersister(TND0Person, TRTTIPersister);
  FRegistry.RegisterPersister(TND1Person, TRTTIPersister);
  FBuilder:=TOPSQLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dSQLite;
  FBuilder.Registry:=FRegistry;
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.SQLBuilder:=FBuilder;
  C:=FManager.Connections.Add as TOPConnection;
  C.SingleConnection:=FConn;
  C.IsDefault:=True;
  FTracker:=TOPSnapshotManager.Create(Nil);
  FTracker.Manager:=Nil;
  FTracker.Manager:=FManager;
end;

procedure TTestNullDirtySave.TearDown;
begin
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FBuilder);
  FreeAndNil(FRegistry);
  if Assigned(FConn) then
  begin
    if FTrans.Active then
      FTrans.Rollback;
    FConn.Close;
  end;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestNullDirtySave.TestLevel0SetNullWritesNull;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    P.ID:=1;
    FManager.Load(P);
    AssertEquals('Loaded', 42, P.Age);
    P.ClearAge;
    FManager.Save(P, opsUpdate);
    AssertTrue('Age written as NULL', AgeIsNull('ND0Person', 1));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirtySave.TestLevel0NullToValueWritesValue;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    P.ID:=2;
    FManager.Load(P);
    AssertFalse('Loaded as null', P.FAgeSet);
    P.Age:=0; // Raw value unchanged, null state changed
    FManager.Save(P, opsUpdate);
    AssertFalse('No longer NULL', AgeIsNull('ND0Person', 2));
    AssertEquals('Value 0 written', 0, AgeValue('ND0Person', 2));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirtySave.TestLevel1SetNullWritesNull;
var
  P: TND1Person;
begin
  P:=TND1Person.Create;
  try
    P.ID:=1;
    FManager.Load(P);
    P.SetNull('Age');
    FManager.Save(P, opsUpdate);
    AssertTrue('Age written as NULL', AgeIsNull('ND1Person', 1));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirtySave.TestLevel1ClearNullWritesValue;
var
  P: TND1Person;
begin
  P:=TND1Person.Create;
  try
    P.ID:=2;
    FManager.Load(P);
    AssertTrue('Loaded as null', P.IsNull('Age'));
    P.ClearNull('Age');
    FManager.Save(P, opsUpdate);
    AssertFalse('No longer NULL', AgeIsNull('ND1Person', 2));
  finally
    P.Free;
  end;
end;

procedure TTestNullDirtySave.TestUnchangedSkipsUpdate;
var
  P: TND0Person;
begin
  P:=TND0Person.Create;
  try
    P.ID:=2;
    FManager.Load(P);
    // Change the row behind the manager's back: an UPDATE would overwrite it
    Exec('UPDATE ND0Person SET Age=7 WHERE ID=2');
    FManager.Save(P, opsUpdate);
    AssertEquals('Unchanged object not written', 7, AgeValue('ND0Person', 2));
  finally
    P.Free;
  end;
end;

initialization
  RegisterTests([TTestNullDirty, TTestNullDirtySave]);
end.
