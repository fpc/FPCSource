unit utc.opf.listreg;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry,
  System.Contnrs, System.Generics.Collections,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
  contnrs, fgl,
{$ENDIF}
  Opf.Types, Opf.ListReg;

type

  { TCustomTestManipulator - for TestCustomManipulator }

  TCustomTestManipulator = class(TOPListManipulator)
  private
    FItems: TFPList;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure AddItem(aList: TObject; aItem: TObject); override;
    procedure ClearList(aList: TObject); override;
    function ItemCount(aList: TObject): Integer; override;
    function GetItem(aList: TObject; aIndex: Integer): TObject; override;
  end;

  TCustomListClass = class(TObject)
  end;

  TTestObjectForGList = class(TObject)
  end;

  TTestGObjectList = specialize TFPGObjectList<TTestObjectForGList>;
  TTestGObjectListManipulator = specialize TOPGObjectListManipulator<TTestObjectForGList>;

  { TTestOPListRegistry }

  TTestOPListRegistry = class(TTestCase)
  private
    FReg: TOPListRegistry;
    procedure RegisterCustomDuplicate;
    procedure DoSetListRegInstanceClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property Reg: TOPListRegistry Read FReg;
  published
    procedure TestEmpty;
    procedure TestRegisterFPList;
    procedure TestFPListManipulator;
    procedure TestObjectListManipulator;
    procedure TestFPObjectListManipulator;
    procedure TestGObjectListManipulator;
    procedure TestCustomManipulator;
    procedure TestDuplicateRegistration;
    procedure TestFindUnregistered;
    procedure TestGlobalInstance;
    procedure TestGlobalInstanceClass;
    procedure TestSetInstanceClassGuard;
  end;

  TCustomListRegistry = class(TOPListRegistry);

implementation

{ TCustomTestManipulator }

constructor TCustomTestManipulator.Create;
begin
  inherited Create;
  FItems:=TFPList.Create;
end;

destructor TCustomTestManipulator.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TCustomTestManipulator.AddItem(aList: TObject; aItem: TObject);
begin
  FItems.Add(Pointer(aItem));
end;

procedure TCustomTestManipulator.ClearList(aList: TObject);
begin
  FItems.Clear;
end;

function TCustomTestManipulator.ItemCount(aList: TObject): Integer;
begin
  Result:=FItems.Count;
end;

function TCustomTestManipulator.GetItem(aList: TObject; aIndex: Integer): TObject;
begin
  Result:=TObject(FItems[aIndex]);
end;

{ TTestOPListRegistry }

procedure TTestOPListRegistry.RegisterCustomDuplicate;
begin
  Reg.RegisterListClass(TCustomListClass, TCustomTestManipulator.Create);
end;

procedure TTestOPListRegistry.SetUp;
begin
  inherited SetUp;
  FReg:=TOPListRegistry.Create(Nil);
  TOPListRegistry.ClearInstance;
end;

procedure TTestOPListRegistry.TearDown;
begin
  TOPListRegistry.ClearInstance;
  TOPListRegistry.SetInstanceClass(Nil);
  FreeAndNil(FReg);
  inherited TearDown;
end;

procedure TTestOPListRegistry.TestEmpty;
begin
  AssertNotNull('Have registry', Reg);
  AssertEquals('Empty', 0, Reg.ListCount);
end;

procedure TTestOPListRegistry.TestRegisterFPList;
Var
  R: TOPListRegistry;
  M: TOPListManipulator;
begin
  R:=TOPListRegistry.Instance;
  AssertTrue('Has registered classes', R.ListCount >= 3);
  M:=R.FindManipulator(TFPList);
  AssertNotNull('Found TFPList manipulator', M);
end;

procedure TTestOPListRegistry.TestFPListManipulator;
Var
  R: TOPListRegistry;
  L: TFPList;
  O1, O2: TObject;
  M: TOPListManipulator;
begin
  R:=TOPListRegistry.Instance;
  M:=R.FindManipulator(TFPList);
  AssertNotNull('Have manipulator', M);
  L:=TFPList.Create;
  try
    O1:=TObject.Create;
    O2:=TObject.Create;
    M.AddItem(L, O1);
    M.AddItem(L, O2);
    AssertEquals('Count after add', 2, M.ItemCount(L));
    AssertSame('Item 0', O1, M.GetItem(L, 0));
    AssertSame('Item 1', O2, M.GetItem(L, 1));
    M.ClearList(L);
    AssertEquals('Count after clear', 0, M.ItemCount(L));
  finally
    O1.Free;
    O2.Free;
    L.Free;
  end;
end;

procedure TTestOPListRegistry.TestObjectListManipulator;
Var
  R: TOPListRegistry;
  L: TObjectList;
  O1, O2: TObject;
  M: TOPListManipulator;
begin
  R:=TOPListRegistry.Instance;
  M:=R.FindManipulator(TObjectList);
  AssertNotNull('Have manipulator', M);
  L:=TObjectList.Create(False);
  try
    O1:=TObject.Create;
    O2:=TObject.Create;
    M.AddItem(L, O1);
    M.AddItem(L, O2);
    AssertEquals('Count after add', 2, M.ItemCount(L));
    AssertSame('Item 0', O1, M.GetItem(L, 0));
    AssertSame('Item 1', O2, M.GetItem(L, 1));
    M.ClearList(L);
    AssertEquals('Count after clear', 0, M.ItemCount(L));
  finally
    O1.Free;
    O2.Free;
    L.Free;
  end;
end;

procedure TTestOPListRegistry.TestFPObjectListManipulator;
Var
  R: TOPListRegistry;
  L: TFPObjectList;
  O1, O2: TObject;
  M: TOPListManipulator;
begin
  R:=TOPListRegistry.Instance;
  M:=R.FindManipulator(TFPObjectList);
  AssertNotNull('Have manipulator', M);
  L:=TFPObjectList.Create(False);
  try
    O1:=TObject.Create;
    O2:=TObject.Create;
    M.AddItem(L, O1);
    M.AddItem(L, O2);
    AssertEquals('Count after add', 2, M.ItemCount(L));
    AssertSame('Item 0', O1, M.GetItem(L, 0));
    AssertSame('Item 1', O2, M.GetItem(L, 1));
    M.ClearList(L);
    AssertEquals('Count after clear', 0, M.ItemCount(L));
  finally
    O1.Free;
    O2.Free;
    L.Free;
  end;
end;

procedure TTestOPListRegistry.TestGObjectListManipulator;
Var
  L: TTestGObjectList;
  O1, O2: TTestObjectForGList;
  M: TOPListManipulator;
begin
  Reg.RegisterListClass(TTestGObjectList, TTestGObjectListManipulator.Create);
  M:=Reg.FindManipulator(TTestGObjectList);
  AssertNotNull('Have manipulator', M);
  L:=TTestGObjectList.Create(False);
  try
    O1:=TTestObjectForGList.Create;
    O2:=TTestObjectForGList.Create;
    M.AddItem(L, O1);
    M.AddItem(L, O2);
    AssertEquals('Count after add', 2, M.ItemCount(L));
    AssertSame('Item 0', O1, M.GetItem(L, 0));
    AssertSame('Item 1', O2, M.GetItem(L, 1));
    M.ClearList(L);
    AssertEquals('Count after clear', 0, M.ItemCount(L));
  finally
    O1.Free;
    O2.Free;
    L.Free;
  end;
end;

procedure TTestOPListRegistry.TestCustomManipulator;
Var
  M: TCustomTestManipulator;
  Found: TOPListManipulator;
  O: TObject;
begin
  M:=TCustomTestManipulator.Create;
  Reg.RegisterListClass(TCustomListClass, M);
  Found:=Reg.FindManipulator(TCustomListClass);
  AssertSame('Custom manipulator', M, Found);
  O:=TObject.Create;
  try
    M.AddItem(nil, O);
    AssertEquals('Custom count', 1, M.ItemCount(nil));
    AssertSame('Custom item', O, M.GetItem(nil, 0));
    M.ClearList(nil);
    AssertEquals('Custom clear', 0, M.ItemCount(nil));
  finally
    O.Free;
  end;
end;

procedure TTestOPListRegistry.TestDuplicateRegistration;
begin
  Reg.RegisterListClass(TCustomListClass, TCustomTestManipulator.Create);
  AssertException('Duplicate raises', EOPFListReg, @RegisterCustomDuplicate);
end;

procedure TTestOPListRegistry.TestFindUnregistered;
begin
  AssertNull('Unregistered returns nil', Reg.FindManipulator(TTestOPListRegistry));
end;

procedure TTestOPListRegistry.TestGlobalInstance;
Var
  R: TOPListRegistry;
begin
  R:=TOPListRegistry.Instance;
  AssertNotNull('Have instance', R);
  AssertTrue('Has TFPList', R.FindManipulator(TFPList)<>nil);
  AssertTrue('Has TObjectList', R.FindManipulator(TObjectList)<>nil);
  AssertTrue('Has TFPObjectList', R.FindManipulator(TFPObjectList)<>nil);
  TOPListRegistry.ClearInstance;
  AssertNotNull('Have instance after clear', TOPListRegistry.Instance);
end;

procedure TTestOPListRegistry.TestGlobalInstanceClass;
begin
  TOPListRegistry.ClearInstance;
  TOPListRegistry.SetInstanceClass(TOPListRegistry);
  try
    AssertNotNull('Have instance', TOPListRegistry.Instance);
    AssertEquals('Correct class', TOPListRegistry, TOPListRegistry.Instance.ClassType);
  finally
    TOPListRegistry.ClearInstance;
    TOPListRegistry.SetInstanceClass(Nil);
  end;
end;

procedure TTestOPListRegistry.DoSetListRegInstanceClass;
begin
  TOPListRegistry.SetInstanceClass(TCustomListRegistry);
end;

procedure TTestOPListRegistry.TestSetInstanceClassGuard;
var
  Inst: TOPListRegistry;
begin
  TOPListRegistry.ClearInstance;
  TOPListRegistry.SetInstanceClass(TCustomListRegistry);
  try
    Inst:=TOPListRegistry.Instance;
    AssertNotNull('Have instance', Inst);
    AssertEquals('Correct class', TCustomListRegistry, Inst.ClassType);
    AssertException('Cannot set class if instance is active', EOPFListReg, @Self.DoSetListRegInstanceClass);
  finally
    TOPListRegistry.ClearInstance;
    TOPListRegistry.SetInstanceClass(Nil);
  end;
end;

initialization
  RegisterTests([TTestOPListRegistry]);
end.
