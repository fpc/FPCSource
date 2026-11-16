unit utc.opf.attrcontracts;

{ Tests for TAttributePersister contracts (FPC 3.3.1+):
  - pkFields / pkBoth: class fields are read and written through extended RTTI
  - nullable support: Level 0 (stored field), Level 2 (TNullable<T>)
  - change detection for class fields
  - the factory entry: kept when a definition is freed, creates subclasses with
    their own or an ancestor constructor, never replaces one of the application }

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

interface

uses
  Classes, SysUtils, typinfo, Variants, db, sqldb, fpcunit, testregistry, nullable,
  Opf.Types, Opf.Fields, Opf.FieldValue, Opf.Factory, Opf.Registry, Opf.Attributes,
  Opf.Persister.ExtRTTI, Opf.Persister.RTTI, Opf.Nullable, Opf.Driver, Opf.Sql, Opf.DDL,
  Opf.Manager, Opf.Relations, utc.opf.failconn;

type
  TACColor = (accRed, accGreen, accBlue);
  TACColors = set of TACColor;

  {$RTTI EXPLICIT PROPERTIES([vcPublished]) FIELDS([vcPrivate, vcPublished])}

  [OPTableName('ac_fields')]
  [OPPersistKind(pkFields)]
  [OPVisibility([vcPrivate])]
  [OPPrimaryKey('ID')]
  TACFields = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FPrice: Double;
    FColors: TACColors;
    FCount: Cardinal;
    FLines: TStrings;
    [OPSkip]
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Colors: TACColors read FColors write FColors;
    property Count: Cardinal read FCount write FCount;
    property Lines: TStrings read FLines;
  end;

  [OPTableName('ac_both')]
  [OPPersistKind(pkBoth)]
  [OPVisibility([vcPrivate, vcPublished])]
  [OPPrimaryKey('ID')]
  TACBoth = class(TPersistent, IOPPersistenceState)
  private
    [OPSkip]
    FID: Integer;
    [OPSkip]
    FName: String;
    FCode: String;
    [OPSkip]
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property Code: String read FCode write FCode;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  [OPTableName('ac_nullable')]
  [OPPrimaryKey('ID')]
  TACNullable = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FAge: Integer;
    FAgeStored: Boolean;
    FScore: TNullableInteger;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    procedure SetAge(aValue: Integer);
  public
    property Score: TNullableInteger read FScore write FScore;
    property AgeStored: Boolean read FAgeStored write FAgeStored;
  published
    property ID: Integer read FID write FID;
    property Age: Integer read FAge write SetAge stored FAgeStored;
  end;

  TACBigSet = set of 0..63;

  // R2-H3: a set of more than 32 elements cannot be stored as a number
  [OPTableName('ac_bigset')]
  [OPPersistKind(pkFields)]
  [OPVisibility([vcPrivate])]
  [OPPrimaryKey('ID')]
  TACBigSetNumber = class(TPersistent)
  private
    FID: Integer;
    FFlags: TACBigSet;
  public
    property ID: Integer read FID write FID;
    property Flags: TACBigSet read FFlags write FFlags;
  end;

  [OPTableName('ac_bigtext')]
  [OPPersistKind(pkFields)]
  [OPVisibility([vcPrivate])]
  [OPPrimaryKey('ID')]
  TACBigSetText = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    [OPFieldOptions([foAsString])]
    FFlags: TACBigSet;
    [OPSkip]
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property ID: Integer read FID write FID;
    property Flags: TACBigSet read FFlags write FFlags;
  end;

  // R2-L11: an enumeration stored as text
  [OPTableName('ac_enumtext')]
  [OPPersistKind(pkFields)]
  [OPVisibility([vcPrivate])]
  [OPPrimaryKey('ID')]
  TACEnumText = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    [OPFieldOptions([foAsString])]
    FColor: TACColor;
    [OPSkip]
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property ID: Integer read FID write FID;
    property Color: TACColor read FColor write FColor;
  end;

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  [OPTableName('ac_ctor')]
  TACWithCtor = class(TPersistent)
  private
    FID: Integer;
    FMarker: String;
  public
    constructor Create;
  published
    property ID: Integer read FID write FID;
    property Marker: String read FMarker write FMarker;
  end;

  // R2-M10: not registered, no constructor of its own
  TACWithCtorSub = class(TACWithCtor)
  end;

  // Not registered, a constructor of its own
  TACWithCtorOwnSub = class(TACWithCtor)
  public
    constructor Create;
  end;

  // R2-M13: another constructor declared before Create
  [OPTableName('ac_ctor2')]
  TACTwoCtors = class(TPersistent)
  private
    FID: Integer;
    FMarker: String;
  public
    constructor CreateDefault;
    constructor Create;
  published
    property ID: Integer read FID write FID;
    property Marker: String read FMarker write FMarker;
  end;

  // R2-M11: the application registered a factory
  [OPTableName('ac_ctor3')]
  TACUserFactory = class(TPersistent)
  private
    FID: Integer;
    FMarker: String;
  public
    constructor Create;
  published
    property ID: Integer read FID write FID;
    property Marker: String read FMarker write FMarker;
  end;

  // R2-M12: registered in two registries
  [OPTableName('ac_ctor4')]
  TACTwoRegistries = class(TPersistent)
  private
    FID: Integer;
    FMarker: String;
  public
    constructor Create;
  published
    property ID: Integer read FID write FID;
    property Marker: String read FMarker write FMarker;
  end;

  [OPTableName('ac_rparent')]
  [OPPrimaryKey('ID')]
  TACRelParent = class(TPersistent)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  [OPTableName('ac_rchild')]
  [OPPrimaryKey('ID')]
  TACRelChild = class(TPersistent)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    [OPRelation('TACRelParent')]
    property ParentID: Integer read FParentID write FParentID;
  end;

  [OPTableName('ac_qchild')]
  [OPPrimaryKey('ID')]
  TACQualifiedChild = class(TPersistent)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    [OPRelation('utc.opf.attrcontracts.TACRelParent')]
    property ParentID: Integer read FParentID write FParentID;
  end;

  [OPTableName('ac_nchild')]
  [OPPrimaryKey('ID')]
  TACNamedChild = class(TPersistent)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    [OPRelation('TACRelParent', 'fk_ac_named')]
    property ParentID: Integer read FParentID write FParentID;
  end;

  [OPTableName('ac_cchild')]
  [OPPrimaryKey('ID')]
  TACNamedCascadeChild = class(TPersistent)
  private
    FID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    [OPRelation('TACRelParent', 'fk_ac_cascade', True)]
    property ParentID: Integer read FParentID write FParentID;
  end;

  { TTestAttrContracts }

  TTestAttrContracts = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FSQLBuilder: TOPSQLBuilder;
    FDDLBuilder: TOPDDLBuilder;
    FManager: TOPManager;
    function Reg(aClass: TClass): TPersisterDef;
    function ChildRelation(aChild: TClass): TOPRelationDef;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFieldsMapped;
    procedure TestFieldsRoundTrip;
    procedure TestFieldsUpdate;
    procedure TestFieldChangeDetected;
    procedure TestBothRoundTrip;
    procedure TestLevel0Nullable;
    procedure TestLevel2Nullable;
    procedure TestFreedDefinitionUnregistersFactory;
    procedure TestUnregisteredSubclassRunsAncestorConstructor;
    procedure TestUnregisteredSubclassRunsOwnConstructor;
    procedure TestCreatePreferredConstructor;
    procedure TestUserFactoryNotReplaced;
    procedure TestSecondRegistryKeepsFactory;
    procedure TestLargeSetAsNumberRejected;
    procedure TestLargeSetAsTextRoundTrip;
    procedure TestUnknownEnumTextRaises;
    procedure TestNullClearsObjectField;
    procedure TestFailedRelationRegistrationLeavesNothing;
    procedure TestPrivateRegistryHasOwnRelations;
    procedure TestQualifiedParentName;
    procedure TestFKNameIsConstraintName;
    procedure TestFKNameUsedInForeignKeySQL;
    procedure TestFKNameWithCascade;
    procedure TestNoFKNameGeneratesName;
  end;

implementation

{ TACFields }

constructor TACFields.Create;
begin
  inherited Create;
  FLines:=TStringList.Create;
end;

destructor TACFields.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TACFields.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TACFields.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TACBoth }

function TACBoth.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TACBoth.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TACNullable }

function TACNullable.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TACNullable.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

procedure TACNullable.SetAge(aValue: Integer);
begin
  FAge:=aValue;
  FAgeStored:=True;
end;

{ TACWithCtor }

constructor TACWithCtor.Create;
begin
  inherited Create;
  FMarker:='made';
end;

{ TACWithCtorOwnSub }

constructor TACWithCtorOwnSub.Create;
begin
  inherited Create;
  Marker:='sub';
end;

{ TACTwoCtors }

constructor TACTwoCtors.CreateDefault;
begin
  inherited Create;
  FMarker:='default';
end;

constructor TACTwoCtors.Create;
begin
  inherited Create;
  FMarker:='create';
end;

{ TACUserFactory }

constructor TACUserFactory.Create;
begin
  inherited Create;
  FMarker:='made';
end;

function ACUserFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
begin
  Result:=TACUserFactory.Create;
  TACUserFactory(Result).Marker:='user';
end;

{ TACTwoRegistries }

constructor TACTwoRegistries.Create;
begin
  inherited Create;
  FMarker:='made';
end;

{ TACBigSetText }

function TACBigSetText.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TACBigSetText.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TACEnumText }

function TACEnumText.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TACEnumText.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TTestAttrContracts }

function TTestAttrContracts.Reg(aClass: TClass): TPersisterDef;
begin
  Result:=FRegistry.RegisterPersister(aClass, TAttributePersister);
  FDDLBuilder.CreateTable(FConn, Result.TableName, Result.FieldMap.AsArray);
end;

procedure TTestAttrContracts.SetUp;
var
  C: TOPConnection;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FRegistry:=TOPRegistry.Create(Nil);
  FDDLBuilder:=TOPDDLBuilder.Create(Nil);
  FDDLBuilder.SQLDialect:=dSQLite;
  FDDLBuilder.Registry:=FRegistry;
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

procedure TTestAttrContracts.TearDown;
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

procedure TTestAttrContracts.TestFieldsMapped;
var
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  D:=Reg(TACFields);
  F:=D.FieldMap.FindByFieldName('Name');
  AssertNotNull('Name mapped', F);
  AssertTrue('Field member', F.MemberKind=omkField);
  AssertNotNull('RTTI field kept', F.RttiMember);
  AssertNull('State skipped', D.FieldMap.FindByFieldName('State'));
end;

procedure TTestAttrContracts.TestFieldsRoundTrip;
var
  A, B: TACFields;
begin
  Reg(TACFields);
  A:=TACFields.Create;
  B:=TACFields.Create;
  try
    A.ID:=1;
    A.Name:='fields';
    A.Price:=12.5;
    A.Colors:=[accRed, accBlue];
    A.Count:=4000000000;
    A.Lines.Text:='l1'+LineEnding+'l2';
    FManager.Save(A, opsNew);
    B.ID:=1;
    FManager.Load(B);
    AssertEquals('String', 'fields', B.Name);
    AssertEquals('Double', 12.5, B.Price, 0);
    AssertTrue('Set', B.Colors=[accRed, accBlue]);
    AssertEquals('Cardinal', Int64(4000000000), Int64(B.Count));
    AssertEquals('Object', 2, B.Lines.Count);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestFieldsUpdate;
var
  A, B: TACFields;
begin
  Reg(TACFields);
  A:=TACFields.Create;
  B:=TACFields.Create;
  try
    A.ID:=1;
    A.Name:='first';
    FManager.Save(A, opsNew);
    A.Name:='second';
    FManager.Save(A, opsUpdate);
    B.ID:=1;
    FManager.Load(B);
    AssertEquals('second', B.Name);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestFieldChangeDetected;
var
  D: TPersisterDef;
  A: TACFields;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  D:=Reg(TACFields);
  F:=D.FieldMap.FindByFieldName('Colors');
  A:=TACFields.Create;
  try
    A.Colors:=[accRed];
    AssertTrue('Readable', OPFReadFieldValue(A, F, V1));
    A.Colors:=[accGreen];
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
  finally
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestBothRoundTrip;
var
  A, B: TACBoth;
begin
  Reg(TACBoth);
  A:=TACBoth.Create;
  B:=TACBoth.Create;
  try
    A.ID:=3;
    A.Name:='prop';
    A.Code:='field';
    FManager.Save(A, opsNew);
    B.ID:=3;
    FManager.Load(B);
    AssertEquals('Property', 'prop', B.Name);
    AssertEquals('Field', 'field', B.Code);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestLevel0Nullable;
var
  D: TPersisterDef;
  A, B: TACNullable;
begin
  D:=Reg(TACNullable);
  AssertTrue('Stored field: nullable', foNullable in D.FieldMap.FindByPropertyName('Age').Options);
  A:=TACNullable.Create;
  B:=TACNullable.Create;
  try
    A.ID:=1;
    // Age never set: not stored, so NULL
    FManager.Save(A, opsNew);
    B.ID:=1;
    B.Age:=99;
    FManager.Load(B);
    AssertFalse('NULL loaded', B.AgeStored);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestLevel2Nullable;
var
  D: TPersisterDef;
  A, B: TACNullable;
begin
  D:=Reg(TACNullable);
  AssertNotNull('TNullable field mapped', D.FieldMap.FindByFieldName('Score'));
  A:=TACNullable.Create;
  B:=TACNullable.Create;
  try
    A.ID:=1;
    FManager.Save(A, opsNew);
    B.ID:=1;
    B.Score:=5;
    FManager.Load(B);
    AssertFalse('NULL loaded', B.Score.HasValue);
    A.Score:=7;
    FManager.Save(A, opsUpdate);
    FManager.Load(B);
    AssertTrue('Value loaded', B.Score.HasValue and (B.Score.Value=7));
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestFreedDefinitionUnregistersFactory;
var
  R: TOPRegistry;
  O: TObject;
begin
  R:=TOPRegistry.Create(Nil);
  try
    R.RegisterPersister(TACWithCtor, TExtRTTIPersister);
    AssertTrue('Factory registered', OPFactory.HasFactory(TACWithCtor));
  finally
    R.Free;
  end;
  // The factory does not depend on the definition: it stays, and creates
  // objects with the constructor of the class (it called into the freed
  // definition before)
  AssertTrue('Factory kept', OPFactory.HasFactory(TACWithCtor));
  O:=OPFactory.CreateInstance(TACWithCtor, Nil);
  try
    AssertEquals('Constructor ran', 'made', TACWithCtor(O).Marker);
  finally
    O.Free;
  end;
end;

procedure TTestAttrContracts.TestUnregisteredSubclassRunsAncestorConstructor;
var
  O: TObject;
begin
  FRegistry.RegisterPersister(TACWithCtor, TAttributePersister);
  O:=OPFactory.CreateInstance(TACWithCtorSub, Nil);
  try
    AssertEquals('Class', TACWithCtorSub, O.ClassType);
    AssertEquals('Ancestor constructor ran', 'made', TACWithCtorSub(O).Marker);
  finally
    O.Free;
  end;
end;

procedure TTestAttrContracts.TestUnregisteredSubclassRunsOwnConstructor;
var
  O: TObject;
begin
  FRegistry.RegisterPersister(TACWithCtor, TAttributePersister);
  O:=OPFactory.CreateInstance(TACWithCtorOwnSub, Nil);
  try
    AssertEquals('Class', TACWithCtorOwnSub, O.ClassType);
    AssertEquals('Own constructor ran', 'sub', TACWithCtorOwnSub(O).Marker);
  finally
    O.Free;
  end;
end;

procedure TTestAttrContracts.TestCreatePreferredConstructor;
var
  O: TObject;
begin
  FRegistry.RegisterPersister(TACTwoCtors, TAttributePersister);
  O:=OPFactory.CreateInstance(TACTwoCtors, Nil);
  try
    AssertEquals('Create, not CreateDefault', 'create', TACTwoCtors(O).Marker);
  finally
    O.Free;
  end;
end;

procedure TTestAttrContracts.TestUserFactoryNotReplaced;
var
  R: TOPRegistry;
  O: TObject;
begin
  OPFactory.RegisterFactory(TACUserFactory, @ACUserFactory);
  try
    R:=TOPRegistry.Create(Nil);
    try
      R.RegisterPersister(TACUserFactory, TAttributePersister);
    finally
      R.Free;
    end;
    // Neither registration nor freeing the definition touched it
    O:=OPFactory.CreateInstance(TACUserFactory, Nil);
    try
      AssertEquals('User factory', 'user', TACUserFactory(O).Marker);
    finally
      O.Free;
    end;
  finally
    OPFactory.UnregisterFactory(TACUserFactory);
  end;
end;

procedure TTestAttrContracts.TestSecondRegistryKeepsFactory;
var
  R1, R2: TOPRegistry;
  O: TObject;
begin
  R1:=TOPRegistry.Create(Nil);
  try
    R1.RegisterPersister(TACTwoRegistries, TAttributePersister);
    R2:=TOPRegistry.Create(Nil);
    try
      R2.RegisterPersister(TACTwoRegistries, TAttributePersister);
    finally
      R2.Free;
    end;
    // R1 still needs the factory
    O:=OPFactory.CreateInstance(TACTwoRegistries, Nil);
    try
      AssertEquals('Constructor ran', 'made', TACTwoRegistries(O).Marker);
    finally
      O.Free;
    end;
  finally
    R1.Free;
  end;
end;

procedure TTestAttrContracts.TestLargeSetAsNumberRejected;
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FRegistry.RegisterPersister(TACBigSetNumber, TAttributePersister);
  except
    on E: Exception do
    begin
      Raised:=True;
      AssertTrue('Clear message: '+E.Message, Pos('foAsString', E.Message)>0);
    end;
  end;
  AssertTrue('Rejected at registration', Raised);
  AssertNull('Not registered', FRegistry.FindPersister(TACBigSetNumber));
end;

procedure TTestAttrContracts.TestLargeSetAsTextRoundTrip;
var
  D: TPersisterDef;
  A, B: TACBigSetText;
  V1, V2: Variant;
  F: TOPFieldMap;
begin
  D:=Reg(TACBigSetText);
  A:=TACBigSetText.Create;
  B:=TACBigSetText.Create;
  try
    A.ID:=1;
    A.Flags:=[1, 35, 63];
    FManager.Save(A, opsNew);
    B.ID:=1;
    FManager.Load(B);
    AssertTrue('Loaded exactly', B.Flags=[1, 35, 63]);
    // An element above 31 is a change
    F:=D.FieldMap.FindByFieldName('Flags');
    AssertTrue(OPFReadFieldValue(A, F, V1));
    A.Flags:=A.Flags+[40];
    AssertTrue(OPFReadFieldValue(A, F, V2));
    AssertFalse('Change detected', OPFSameVariant(V1, V2));
    FManager.Save(A, opsUpdate);
    FManager.Load(B);
    AssertTrue('Element 40 saved', 40 in B.Flags);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestUnknownEnumTextRaises;
var
  A, B: TACEnumText;
  Raised: Boolean;
begin
  Reg(TACEnumText);
  A:=TACEnumText.Create;
  B:=TACEnumText.Create;
  try
    A.ID:=1;
    A.Color:=accGreen;
    FManager.Save(A, opsNew);
    FConn.ExecuteDirect('UPDATE ac_enumtext SET Color=''accPurple'' WHERE ID=1');
    FTrans.Commit;
    B.ID:=1;
    Raised:=False;
    try
      FManager.Load(B);
    except
      on E: Exception do
        Raised:=Pos('accPurple', E.Message)>0;
    end;
    AssertTrue('Unknown name raises', Raised);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestNullClearsObjectField;
var
  A, B: TACFields;
begin
  Reg(TACFields);
  A:=TACFields.Create;
  B:=TACFields.Create;
  try
    A.ID:=1;
    A.Lines.Text:='l1';
    FManager.Save(A, opsNew);
    FConn.ExecuteDirect('UPDATE ac_fields SET Lines=NULL WHERE ID=1');
    FTrans.Commit;
    B.ID:=1;
    B.Lines.Text:='stale';
    FManager.Load(B);
    AssertEquals('Emptied', 0, B.Lines.Count);
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestAttrContracts.TestFailedRelationRegistrationLeavesNothing;
var
  Raised: Boolean;
  Rels: TOPRelationRegistry;
begin
  // The parent is not registered yet: the relation fails
  Raised:=False;
  try
    FRegistry.RegisterPersister(TACRelChild, TAttributePersister);
  except
    on E: EOPF do
      Raised:=True;
  end;
  AssertTrue('Fails', Raised);
  AssertNull('Child not registered', FRegistry.FindPersister(TACRelChild));
  // After registering the parent, a retry works
  FRegistry.RegisterPersister(TACRelParent, TAttributePersister);
  FRegistry.RegisterPersister(TACRelChild, TAttributePersister);
  Rels:=TOPRelationRegistry(FRegistry.RelationRegistry);
  AssertEquals('One relation', 1, Length(Rels.FindChildRelations(TACRelParent)));
end;

procedure TTestAttrContracts.TestPrivateRegistryHasOwnRelations;
var
  Rels: TOPRelationRegistry;
begin
  FRegistry.RegisterPersister(TACRelParent, TAttributePersister);
  FRegistry.RegisterPersister(TACRelChild, TAttributePersister);
  Rels:=TOPRelationRegistry(FRegistry.RelationRegistry);
  AssertTrue('Not the global relation registry', Rels<>TOPRelationRegistry.Instance);
  AssertSame('Tied to this registry', FRegistry, Rels.Registry);
  AssertEquals('Not in the global one', 0, Length(TOPRelationRegistry.Instance.FindChildRelations(TACRelParent)));
end;

procedure TTestAttrContracts.TestQualifiedParentName;
var
  Rels: TOPRelationRegistry;
begin
  FRegistry.RegisterPersister(TACRelParent, TAttributePersister);
  FRegistry.RegisterPersister(TACQualifiedChild, TAttributePersister);
  Rels:=TOPRelationRegistry(FRegistry.RelationRegistry);
  AssertEquals('Parent found by unit and class name', 1, Length(Rels.FindChildRelations(TACRelParent)));
end;

function TTestAttrContracts.ChildRelation(aChild: TClass): TOPRelationDef;
begin
  FRegistry.RegisterPersister(TACRelParent, TAttributePersister);
  FRegistry.RegisterPersister(aChild, TAttributePersister);
  Result:=TOPRelationRegistry(FRegistry.RelationRegistry).FindParentRelation(aChild);
  AssertNotNull('Relation registered', Result);
end;

procedure TTestAttrContracts.TestFKNameIsConstraintName;
begin
  AssertEquals('Constraint name from the attribute', 'fk_ac_named',
    ChildRelation(TACNamedChild).ConstraintName);
end;

procedure TTestAttrContracts.TestFKNameUsedInForeignKeySQL;
var
  SQL: String;
begin
  SQL:=FDDLBuilder.GetForeignKeySQL(ChildRelation(TACNamedChild), FRegistry);
  AssertTrue('Named constraint: '+SQL, Pos('CONSTRAINT fk_ac_named FOREIGN KEY', SQL)>0);
  AssertTrue('No cascade: '+SQL, Pos('CASCADE', SQL)=0);
end;

procedure TTestAttrContracts.TestFKNameWithCascade;
var
  R: TOPRelationDef;
  SQL: String;
begin
  R:=ChildRelation(TACNamedCascadeChild);
  AssertEquals('Constraint name', 'fk_ac_cascade', R.ConstraintName);
  AssertTrue('Cascade', R.DBCascade);
  SQL:=FDDLBuilder.GetForeignKeySQL(R, FRegistry);
  AssertTrue('Named constraint: '+SQL, Pos('CONSTRAINT fk_ac_cascade FOREIGN KEY', SQL)>0);
  AssertTrue('Cascade: '+SQL, Pos('ON DELETE CASCADE', SQL)>0);
end;

procedure TTestAttrContracts.TestNoFKNameGeneratesName;
var
  R: TOPRelationDef;
  SQL: String;
begin
  R:=ChildRelation(TACRelChild);
  AssertEquals('No constraint name', '', R.ConstraintName);
  SQL:=FDDLBuilder.GetForeignKeySQL(R, FRegistry);
  AssertTrue('Generated name: '+SQL, Pos('CONSTRAINT '+FDDLBuilder.FKPrefix+'ac_rchild_ac_rparent ', SQL)>0);
end;

initialization
  RegisterTest(TTestAttrContracts);
end.
