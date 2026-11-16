unit utc.opf.factory;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Opf.Factory, Opf.Registry, Opf.Fields,
  Opf.Manager
  {$IFNDEF VER3_2}
  , Opf.Persister.ExtRTTI
  {$ENDIF}
  ;

type

  { TTestParent }

  TTestParent = class(TObject)
  private
    FCreatedBy: UTF8String;
  public
    property CreatedBy: UTF8String read FCreatedBy write FCreatedBy;
  end;

  { TTestChild — descends from TTestParent }

  TTestChild = class(TTestParent)
  end;

  { TTestUnrelated }

  TTestUnrelated = class(TObject)
  end;

  { TTestComponent — TComponent descendant for owner-passing test }

  TTestComponent = class(TComponent)
  end;

  {$IFNDEF VER3_2}
  { TTestWithCustomCreate — has a custom parameterless constructor.
    {$RTTI EXPLICIT} is required for GetMethods to discover constructors. }

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  TTestWithCustomCreate = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FInitialized: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    property Initialized: Boolean read FInitialized;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestNoCustomCreate — uses default TObject.Create, no override }

  TTestNoCustomCreate = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
  end;

  { TRttiFactoryIntegrationTest }

  TRttiFactoryIntegrationTest = class(TTestCase)
  private
    FRegistry: TOPRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAutoFactoryForCustomConstructor;
    procedure TestNoAutoFactoryForDefaultConstructor;
  end;
  {$ENDIF}

  { TTestFactoryHelper — provides method callbacks }

  TTestFactoryHelper = class(TObject)
  private
    FCallCount: Integer;
    FLastClass: TClass;
    FLastReason: TOPFactoryReason;
    FLastListObject: TObject;
  public
    constructor Create;
    function MethodFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
    property CallCount: Integer read FCallCount;
    property LastClass: TClass read FLastClass;
    property LastReason: TOPFactoryReason read FLastReason;
    property LastListObject: TObject read FLastListObject;
  end;

  { TOPClassFactoryTest }

  TOPClassFactoryTest = class(TTestCase)
  private
    FFactory: TOPClassFactory;
    FHelper: TTestFactoryHelper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateInstanceDefault;
    procedure TestRegisterMethodFactory;
    procedure TestRegisterFuncFactory;
    procedure TestHierarchyLookup;
    procedure TestHierarchyExactMatch;
    procedure TestNoHierarchyForUnrelated;
    procedure TestContextPassed;
    procedure TestUnregister;
    procedure TestReplaceFactory;
    procedure TestHasFactory;
    procedure TestComponentDefaultWithComponentOwner;
    procedure TestComponentDefaultWithNonComponentOwner;
    procedure TestComponentDefaultWithNilContext;
    // R2-M11: a default factory never replaces one that exists
    procedure TestDefaultFactoryKeepsExisting;
    // R2-M15: registering while other threads create objects
    procedure TestRegisterWhileCreatingInThreads;
  end;

implementation

{ Helper plain function factory }

function PlainFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
begin
  Result := aClass.Create;
  if Result is TTestParent then
    TTestParent(Result).CreatedBy := 'PlainFactory';
end;

{ TTestFactoryHelper }

constructor TTestFactoryHelper.Create;
begin
  inherited Create;
  FCallCount := 0;
  FLastClass := nil;
  FLastReason := frListItem;
  FLastListObject := nil;
end;

function TTestFactoryHelper.MethodFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
begin
  Inc(FCallCount);
  FLastClass := aClass;
  if Assigned(aContext) then
  begin
    FLastReason := aContext.Reason;
    FLastListObject := aContext.ListObject;
  end;
  Result := aClass.Create;
  if Result is TTestParent then
    TTestParent(Result).CreatedBy := 'MethodFactory';
end;

{ TOPClassFactoryTest }

procedure TOPClassFactoryTest.SetUp;
begin
  FFactory := TOPClassFactory.Create;
  FHelper := TTestFactoryHelper.Create;
end;

procedure TOPClassFactoryTest.TearDown;
begin
  FreeAndNil(FHelper);
  FreeAndNil(FFactory);
end;

procedure TOPClassFactoryTest.TestCreateInstanceDefault;
var
  Obj: TObject;
begin
  Obj := FFactory.CreateInstance(TTestParent, nil);
  try
    AssertTrue('Should create TTestParent', Obj is TTestParent);
    AssertEquals('CreatedBy should be empty (default constructor)', '', TTestParent(Obj).CreatedBy);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestRegisterMethodFactory;
var
  Obj: TObject;
begin
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  Obj := FFactory.CreateInstance(TTestParent, nil);
  try
    AssertTrue('Should create TTestParent', Obj is TTestParent);
    AssertEquals('CreatedBy', 'MethodFactory', TTestParent(Obj).CreatedBy);
    AssertEquals('CallCount', 1, FHelper.CallCount);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestRegisterFuncFactory;
var
  Obj: TObject;
begin
  FFactory.RegisterFactory(TTestParent, @PlainFactory);
  Obj := FFactory.CreateInstance(TTestParent, nil);
  try
    AssertTrue('Should create TTestParent', Obj is TTestParent);
    AssertEquals('CreatedBy', 'PlainFactory', TTestParent(Obj).CreatedBy);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestHierarchyLookup;
var
  Obj: TObject;
begin
  { Register factory for TTestParent but create TTestChild }
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  Obj := FFactory.CreateInstance(TTestChild, nil);
  try
    AssertTrue('Should create TTestChild', Obj is TTestChild);
    AssertEquals('CreatedBy', 'MethodFactory', TTestParent(Obj).CreatedBy);
    AssertEquals('LastClass should be TTestChild', 'TTestChild', FHelper.LastClass.ClassName);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestHierarchyExactMatch;
var
  Obj: TObject;
begin
  { Register for both parent and child — child gets its own factory }
  FFactory.RegisterFactory(TTestParent, @PlainFactory);
  FFactory.RegisterFactory(TTestChild, @FHelper.MethodFactory);
  Obj := FFactory.CreateInstance(TTestChild, nil);
  try
    AssertEquals('Exact match for TTestChild', 'MethodFactory', TTestParent(Obj).CreatedBy);
  finally
    Obj.Free;
  end;
  { Parent still uses its own factory }
  Obj := FFactory.CreateInstance(TTestParent, nil);
  try
    AssertEquals('Parent uses PlainFactory', 'PlainFactory', TTestParent(Obj).CreatedBy);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestNoHierarchyForUnrelated;
var
  Obj: TObject;
begin
  { Register factory for TTestParent — TTestUnrelated should NOT use it }
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  Obj := FFactory.CreateInstance(TTestUnrelated, nil);
  try
    AssertTrue('Should create TTestUnrelated', Obj is TTestUnrelated);
    AssertEquals('MethodFactory should not have been called', 0, FHelper.CallCount);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestContextPassed;
var
  Obj: TObject;
  Ctx: TOPFactoryContext;
  DummyList: TObject;
begin
  DummyList := TObject.Create;
  try
    FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
    Ctx := TOPFactoryContext.Create(frChild);
    try
      Ctx.ListObject := DummyList;
      Ctx.OwnerObject := nil;
      Ctx.PropertyName := 'Items';
      Obj := FFactory.CreateInstance(TTestParent, Ctx);
      try
        AssertEquals('Reason should be frChild', Ord(frChild), Ord(FHelper.LastReason));
        AssertSame('ListObject should be passed', DummyList, FHelper.LastListObject);
      finally
        Obj.Free;
      end;
    finally
      Ctx.Free;
    end;
  finally
    DummyList.Free;
  end;
end;

procedure TOPClassFactoryTest.TestUnregister;
var
  Obj: TObject;
begin
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  FFactory.UnregisterFactory(TTestParent);
  Obj := FFactory.CreateInstance(TTestParent, nil);
  try
    AssertEquals('Should fall back to default', '', TTestParent(Obj).CreatedBy);
    AssertEquals('MethodFactory should not be called', 0, FHelper.CallCount);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestReplaceFactory;
var
  Obj: TObject;
begin
  FFactory.RegisterFactory(TTestParent, @PlainFactory);
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  Obj := FFactory.CreateInstance(TTestParent, nil);
  try
    AssertEquals('Should use replaced factory', 'MethodFactory', TTestParent(Obj).CreatedBy);
  finally
    Obj.Free;
  end;
end;

procedure TOPClassFactoryTest.TestDefaultFactoryKeepsExisting;
var
  Obj: TObject;
begin
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  AssertFalse('Ancestor factory exists', FFactory.RegisterDefaultFactory(TTestChild, @PlainFactory));
  AssertFalse('Own factory exists', FFactory.RegisterDefaultFactory(TTestParent, @PlainFactory));
  Obj := FFactory.CreateInstance(TTestChild, nil);
  try
    AssertEquals('Existing factory used', 'MethodFactory', TTestParent(Obj).CreatedBy);
  finally
    Obj.Free;
  end;
  AssertTrue('Registered when none exists', FFactory.RegisterDefaultFactory(TTestUnrelated, @PlainFactory));
  AssertTrue(FFactory.HasFactory(TTestUnrelated));
end;

type
  TFactoryThread = class(TThread)
  public
    Factory: TOPClassFactory;
    Failed: Boolean;
    procedure Execute; override;
  end;

procedure TFactoryThread.Execute;
var
  I: Integer;
  Obj: TObject;
begin
  try
    for I := 1 to 20000 do
    begin
      Obj := Factory.CreateInstance(TTestChild, nil);
      if TTestParent(Obj).CreatedBy <> 'PlainFactory' then
        Failed := True;
      Obj.Free;
    end;
  except
    Failed := True;
  end;
end;

procedure TOPClassFactoryTest.TestRegisterWhileCreatingInThreads;
const
  Others: array[0..5] of TClass = (TTestUnrelated, TTestComponent, TList, TStringList,
    TCollection, TComponent);
var
  T: array[0..3] of TFactoryThread;
  I, J: Integer;
  Running: Boolean;
begin
  FFactory.RegisterFactory(TTestParent, @PlainFactory);
  for I := 0 to High(T) do
  begin
    T[I] := TFactoryThread.Create(True);
    T[I].Factory := FFactory;
  end;
  try
    for I := 0 to High(T) do
      T[I].Start;
    // The entry array grows and shrinks while the threads look up entries
    repeat
      for J := 0 to High(Others) do
        FFactory.RegisterFactory(Others[J], @PlainFactory);
      for J := High(Others) downto 0 do
        FFactory.UnregisterFactory(Others[J]);
      Running := False;
      for I := 0 to High(T) do
        Running := Running or not T[I].Finished;
    until not Running;
    for I := 0 to High(T) do
      AssertFalse('Thread '+IntToStr(I)+' always found the factory', T[I].Failed);
  finally
    for I := 0 to High(T) do
    begin
      T[I].WaitFor;
      T[I].Free;
    end;
  end;
end;

procedure TOPClassFactoryTest.TestHasFactory;
begin
  AssertFalse('No factory registered yet', FFactory.HasFactory(TTestParent));
  FFactory.RegisterFactory(TTestParent, @FHelper.MethodFactory);
  AssertTrue('Factory registered for TTestParent', FFactory.HasFactory(TTestParent));
  AssertTrue('TTestChild inherits factory', FFactory.HasFactory(TTestChild));
  AssertFalse('TTestUnrelated has no factory', FFactory.HasFactory(TTestUnrelated));
end;

procedure TOPClassFactoryTest.TestComponentDefaultWithComponentOwner;
var
  Owner: TComponent;
  Obj: TObject;
  Ctx: TOPFactoryContext;
begin
  Owner := TComponent.Create(nil);
  try
    Ctx := TOPFactoryContext.Create(frListItem);
    try
      Ctx.OwnerObject := Owner;
      Obj := FFactory.CreateInstance(TTestComponent, Ctx);
      try
        AssertTrue('Should create TTestComponent', Obj is TTestComponent);
        AssertSame('Owner should be set', Owner, TTestComponent(Obj).Owner);
      finally
        Obj.Free;
      end;
    finally
      Ctx.Free;
    end;
  finally
    Owner.Free;
  end;
end;

procedure TOPClassFactoryTest.TestComponentDefaultWithNonComponentOwner;
var
  NonComp: TObject;
  Obj: TObject;
  Ctx: TOPFactoryContext;
begin
  NonComp := TObject.Create;
  try
    Ctx := TOPFactoryContext.Create(frListItem);
    try
      Ctx.OwnerObject := NonComp;
      Obj := FFactory.CreateInstance(TTestComponent, Ctx);
      try
        AssertTrue('Should create TTestComponent', Obj is TTestComponent);
        AssertTrue('Owner should be nil', TTestComponent(Obj).Owner = nil);
      finally
        Obj.Free;
      end;
    finally
      Ctx.Free;
    end;
  finally
    NonComp.Free;
  end;
end;

procedure TOPClassFactoryTest.TestComponentDefaultWithNilContext;
var
  Obj: TObject;
begin
  Obj := FFactory.CreateInstance(TTestComponent, nil);
  try
    AssertTrue('Should create TTestComponent', Obj is TTestComponent);
    AssertTrue('Owner should be nil', TTestComponent(Obj).Owner = nil);
  finally
    Obj.Free;
  end;
end;

{$IFNDEF VER3_2}
{ TTestWithCustomCreate }

constructor TTestWithCustomCreate.Create;
begin
  inherited Create;
  FInitialized := True;
  FName := 'CustomCreated';
end;

function TTestWithCustomCreate.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTestWithCustomCreate.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ TTestNoCustomCreate }

function TTestNoCustomCreate.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTestNoCustomCreate.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ TRttiFactoryIntegrationTest }

procedure TRttiFactoryIntegrationTest.SetUp;
begin
  FRegistry := TOPRegistry.Create(nil);
end;

procedure TRttiFactoryIntegrationTest.TearDown;
begin
  FreeAndNil(FRegistry);
end;

procedure TRttiFactoryIntegrationTest.TestAutoFactoryForCustomConstructor;
var
  Obj: TObject;
begin
  FRegistry.RegisterPersister(TTestWithCustomCreate, TExtRTTIPersister);
  // The factory should have been auto-registered by AfterRegister
  AssertTrue('Factory should be registered', OPFactory.HasFactory(TTestWithCustomCreate));
  Obj := OPFactory.CreateInstance(TTestWithCustomCreate, nil);
  try
    AssertTrue('Should be TTestWithCustomCreate', Obj is TTestWithCustomCreate);
    AssertTrue('Custom constructor should have run', TTestWithCustomCreate(Obj).Initialized);
    AssertEquals('Name should be set by constructor', 'CustomCreated', TTestWithCustomCreate(Obj).Name);
  finally
    Obj.Free;
  end;
end;

procedure TRttiFactoryIntegrationTest.TestNoAutoFactoryForDefaultConstructor;
begin
  FRegistry.RegisterPersister(TTestNoCustomCreate, TExtRTTIPersister);
  // No factory should be registered — only has TObject.Create
  AssertFalse('No factory for default constructor', OPFactory.HasFactory(TTestNoCustomCreate));
end;
{$ENDIF}

initialization
  RegisterTest(TOPClassFactoryTest);
  {$IFNDEF VER3_2}
  RegisterTest(TRttiFactoryIntegrationTest);
  {$ENDIF}

end.
