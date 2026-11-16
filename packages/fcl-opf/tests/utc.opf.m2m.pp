unit utc.opf.m2m;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  FpcUnit.TestRegistry, FpcUnit.TestDecorator,
{$ELSE}
  Classes, SysUtils,
  testregistry, testdecorator,
{$ENDIF}
  fpcunit,
  Opf.Types, Opf.Registry, Opf.M2M, Opf.Persister.RTTI;

Type

  { TTestM2MClassA }

  TTestM2MClassA = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestM2MClassB }

  TTestM2MClassB = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestM2MClassC }

  TTestM2MClassC = class(TObject)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  { TTestM2MUnregistered }

  TTestM2MUnregistered = class(TObject)
  end;

  { TCustomM2MRegistry }

  TCustomM2MRegistry = class(TOPManyToManyRegistry);

  { TTestManyToManyRegistry }

  TTestManyToManyRegistry = class(TTestCase)
  private
    procedure RegisterDuplicate;
    procedure RegisterDuplicateReverse;
    procedure RegisterUnregisteredA;
    procedure RegisterUnregisteredB;
    procedure RegisterSelfRelation;
    procedure RegisterEmptyJunction;
    procedure RegisterEmptyFKA;
    procedure RegisterEmptyFKB;
    procedure RegisterNilA;
    procedure RegisterNilB;
    procedure DoSetInstanceClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegisterManyToMany;
    procedure TestFindDefForward;
    procedure TestFindDefReverse;
    procedure TestFindDefNotFound;
    procedure TestFindDefsForClassAsA;
    procedure TestFindDefsForClassAsB;
    procedure TestFindDefsForClassMultiple;
    procedure TestFindDefsForClassEmpty;
    procedure TestIndexOfDefFound;
    procedure TestIndexOfDefBidirectional;
    procedure TestIndexOfDefNotFound;
    procedure TestAssignRoundTrip;
    procedure TestClearInstanceFreesAll;
    procedure TestDuplicateRaises;
    procedure TestDuplicateReverseRaises;
    procedure TestUnregisteredClassARaises;
    procedure TestUnregisteredClassBRaises;
    procedure TestEmptyJunctionTableRaises;
    procedure TestEmptyForeignKeyARaises;
    procedure TestEmptyForeignKeyBRaises;
    procedure TestSelfRelationRaises;
    procedure TestNilClassARaises;
    procedure TestNilClassBRaises;
    procedure TestDBCascadeDefaultFalse;
    procedure TestDBCascadeExplicitTrue;
    procedure TestListPropertyNameDefaults;
    procedure TestListPropertyNameExplicit;
    procedure TestSetInstanceClassAndGuard;
    procedure TestDefCount;
    procedure TestDefsAccessor;
  end;

implementation

{ TTestManyToManyRegistry }

procedure TTestManyToManyRegistry.SetUp;
begin
  inherited SetUp;
  OPRegistry.RegisterPersister(TTestM2MClassA, TRTTIPersister);
  OPRegistry.RegisterPersister(TTestM2MClassB, TRTTIPersister);
  OPRegistry.RegisterPersister(TTestM2MClassC, TRTTIPersister);
end;

procedure TTestManyToManyRegistry.TearDown;
begin
  TOPManyToManyRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestManyToManyRegistry.RegisterDuplicate;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterDuplicateReverse;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassB, TTestM2MClassA, 'ba_junction', 'fk_b', 'fk_a');
end;

procedure TTestManyToManyRegistry.RegisterUnregisteredA;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MUnregistered, TTestM2MClassB, 'junc', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterUnregisteredB;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MUnregistered, 'junc', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterSelfRelation;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassA, 'junc', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterEmptyJunction;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, '', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterEmptyFKA;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'junc', '', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterEmptyFKB;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'junc', 'fk_a', '');
end;

procedure TTestManyToManyRegistry.RegisterNilA;
begin
  OPM2MRegistry.RegisterManyToMany(nil, TTestM2MClassB, 'junc', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.RegisterNilB;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, nil, 'junc', 'fk_a', 'fk_b');
end;

procedure TTestManyToManyRegistry.DoSetInstanceClass;
begin
  TOPManyToManyRegistry.SetInstanceClass(TCustomM2MRegistry);
end;

{ Test methods }

procedure TTestManyToManyRegistry.TestRegisterManyToMany;
var
  Def: TOPManyToManyDef;
begin
  Def:=OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB,
    'ab_junction', 'fk_a', 'fk_b', True, 'ListA', 'ListB');
  AssertNotNull('Result not nil', Def);
  AssertEquals('ClassA', TTestM2MClassA, Def.ClassA);
  AssertEquals('ClassB', TTestM2MClassB, Def.ClassB);
  AssertEquals('JunctionTable', 'ab_junction', Def.JunctionTable);
  AssertEquals('ForeignKeyA', 'fk_a', Def.ForeignKeyA);
  AssertEquals('ForeignKeyB', 'fk_b', Def.ForeignKeyB);
  AssertEquals('DBCascade', True, Def.DBCascade);
  AssertEquals('ListPropertyNameA', 'ListA', Def.ListPropertyNameA);
  AssertEquals('ListPropertyNameB', 'ListB', Def.ListPropertyNameB);
end;

procedure TTestManyToManyRegistry.TestFindDefForward;
var
  Def: TOPManyToManyDef;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Def:=OPM2MRegistry.FindDef(TTestM2MClassA, TTestM2MClassB);
  AssertNotNull('FindDef forward not nil', Def);
  AssertEquals('ClassA', TTestM2MClassA, Def.ClassA);
  AssertEquals('ClassB', TTestM2MClassB, Def.ClassB);
end;

procedure TTestManyToManyRegistry.TestFindDefReverse;
var
  Def: TOPManyToManyDef;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Def:=OPM2MRegistry.FindDef(TTestM2MClassB, TTestM2MClassA);
  AssertNotNull('FindDef reverse not nil', Def);
  AssertEquals('ClassA preserved as registered', TTestM2MClassA, Def.ClassA);
  AssertEquals('ClassB preserved as registered', TTestM2MClassB, Def.ClassB);
end;

procedure TTestManyToManyRegistry.TestFindDefNotFound;
var
  Def: TOPManyToManyDef;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Def:=OPM2MRegistry.FindDef(TTestM2MClassA, TTestM2MClassC);
  AssertNull('Unrelated pair returns nil', Def);
end;

procedure TTestManyToManyRegistry.TestFindDefsForClassAsA;
var
  Defs: TOPManyToManyDefArray;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Defs:=OPM2MRegistry.FindDefsForClass(TTestM2MClassA);
  AssertEquals('Length', 1, Length(Defs));
  AssertEquals('ClassA', TTestM2MClassA, Defs[0].ClassA);
end;

procedure TTestManyToManyRegistry.TestFindDefsForClassAsB;
var
  Defs: TOPManyToManyDefArray;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Defs:=OPM2MRegistry.FindDefsForClass(TTestM2MClassB);
  AssertEquals('Length', 1, Length(Defs));
  AssertEquals('ClassB', TTestM2MClassB, Defs[0].ClassB);
end;

procedure TTestManyToManyRegistry.TestFindDefsForClassMultiple;
var
  Defs: TOPManyToManyDefArray;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassC, 'ac_junction', 'fk_a', 'fk_c');
  Defs:=OPM2MRegistry.FindDefsForClass(TTestM2MClassA);
  AssertEquals('Two defs for ClassA', 2, Length(Defs));
end;

procedure TTestManyToManyRegistry.TestFindDefsForClassEmpty;
var
  Defs: TOPManyToManyDefArray;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Defs:=OPM2MRegistry.FindDefsForClass(TTestM2MClassC);
  AssertEquals('Empty array', 0, Length(Defs));
end;

procedure TTestManyToManyRegistry.TestIndexOfDefFound;
var
  Idx: Integer;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Idx:=OPM2MRegistry.IndexOfDef(TTestM2MClassA, TTestM2MClassB);
  AssertTrue('Index >= 0', Idx >= 0);
  AssertEquals('Correct index', 0, Idx);
end;

procedure TTestManyToManyRegistry.TestIndexOfDefBidirectional;
var
  Idx1, Idx2: Integer;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Idx1:=OPM2MRegistry.IndexOfDef(TTestM2MClassA, TTestM2MClassB);
  Idx2:=OPM2MRegistry.IndexOfDef(TTestM2MClassB, TTestM2MClassA);
  AssertEquals('Same index both directions', Idx1, Idx2);
end;

procedure TTestManyToManyRegistry.TestIndexOfDefNotFound;
var
  Idx: Integer;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  Idx:=OPM2MRegistry.IndexOfDef(TTestM2MClassA, TTestM2MClassC);
  AssertEquals('Not found', -1, Idx);
end;

procedure TTestManyToManyRegistry.TestAssignRoundTrip;
var
  Src, Dst: TOPManyToManyDef;
begin
  Src:=TOPManyToManyDef.Create(nil);
  Dst:=TOPManyToManyDef.Create(nil);
  try
    Src.ClassA:=TTestM2MClassA;
    Src.ClassB:=TTestM2MClassB;
    Src.JunctionTable:='jt';
    Src.ForeignKeyA:='fk_a';
    Src.ForeignKeyB:='fk_b';
    Src.DBCascade:=True;
    Src.ListPropertyNameA:='ListA';
    Src.ListPropertyNameB:='ListB';
    Dst.Assign(Src);
    AssertEquals('ClassA', TTestM2MClassA, Dst.ClassA);
    AssertEquals('ClassB', TTestM2MClassB, Dst.ClassB);
    AssertEquals('JunctionTable', 'jt', Dst.JunctionTable);
    AssertEquals('ForeignKeyA', 'fk_a', Dst.ForeignKeyA);
    AssertEquals('ForeignKeyB', 'fk_b', Dst.ForeignKeyB);
    AssertEquals('DBCascade', True, Dst.DBCascade);
    AssertEquals('ListPropertyNameA', 'ListA', Dst.ListPropertyNameA);
    AssertEquals('ListPropertyNameB', 'ListB', Dst.ListPropertyNameB);
  finally
    Dst.Free;
    Src.Free;
  end;
end;

procedure TTestManyToManyRegistry.TestClearInstanceFreesAll;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertEquals('Before clear', 1, OPM2MRegistry.DefCount);
  TOPManyToManyRegistry.ClearInstance;
  AssertEquals('After clear', 0, OPM2MRegistry.DefCount);
end;

procedure TTestManyToManyRegistry.TestDuplicateRaises;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertException('Duplicate raises', EOPFManyToMany, @RegisterDuplicate);
end;

procedure TTestManyToManyRegistry.TestDuplicateReverseRaises;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertException('Duplicate reverse raises', EOPFManyToMany, @RegisterDuplicateReverse);
end;

procedure TTestManyToManyRegistry.TestUnregisteredClassARaises;
begin
  AssertException('Unregistered ClassA raises', EOPFManyToMany, @RegisterUnregisteredA);
end;

procedure TTestManyToManyRegistry.TestUnregisteredClassBRaises;
begin
  AssertException('Unregistered ClassB raises', EOPFManyToMany, @RegisterUnregisteredB);
end;

procedure TTestManyToManyRegistry.TestEmptyJunctionTableRaises;
begin
  AssertException('Empty junction raises', EOPFManyToMany, @RegisterEmptyJunction);
end;

procedure TTestManyToManyRegistry.TestEmptyForeignKeyARaises;
begin
  AssertException('Empty FKA raises', EOPFManyToMany, @RegisterEmptyFKA);
end;

procedure TTestManyToManyRegistry.TestEmptyForeignKeyBRaises;
begin
  AssertException('Empty FKB raises', EOPFManyToMany, @RegisterEmptyFKB);
end;

procedure TTestManyToManyRegistry.TestSelfRelationRaises;
begin
  AssertException('Self-relation raises', EOPFManyToMany, @RegisterSelfRelation);
end;

procedure TTestManyToManyRegistry.TestNilClassARaises;
begin
  AssertException('Nil ClassA raises', EOPFManyToMany, @RegisterNilA);
end;

procedure TTestManyToManyRegistry.TestNilClassBRaises;
begin
  AssertException('Nil ClassB raises', EOPFManyToMany, @RegisterNilB);
end;

procedure TTestManyToManyRegistry.TestDBCascadeDefaultFalse;
var
  Def: TOPManyToManyDef;
begin
  Def:=OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertEquals('DBCascade defaults to False', False, Def.DBCascade);
end;

procedure TTestManyToManyRegistry.TestDBCascadeExplicitTrue;
var
  Def: TOPManyToManyDef;
begin
  Def:=OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b', True);
  AssertEquals('DBCascade is True', True, Def.DBCascade);
end;

procedure TTestManyToManyRegistry.TestListPropertyNameDefaults;
var
  Def: TOPManyToManyDef;
begin
  Def:=OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertEquals('ListPropertyNameA default empty', '', Def.ListPropertyNameA);
  AssertEquals('ListPropertyNameB default empty', '', Def.ListPropertyNameB);
end;

procedure TTestManyToManyRegistry.TestListPropertyNameExplicit;
var
  Def: TOPManyToManyDef;
begin
  Def:=OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB,
    'ab_junction', 'fk_a', 'fk_b', False, 'MyListA', 'MyListB');
  AssertEquals('ListPropertyNameA', 'MyListA', Def.ListPropertyNameA);
  AssertEquals('ListPropertyNameB', 'MyListB', Def.ListPropertyNameB);
end;

procedure TTestManyToManyRegistry.TestSetInstanceClassAndGuard;
var
  Inst: TOPManyToManyRegistry;
begin
  TOPManyToManyRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  TOPManyToManyRegistry.SetInstanceClass(TCustomM2MRegistry);
  try
    Inst:=TOPManyToManyRegistry.Instance;
    AssertNotNull('Have instance', Inst);
    AssertEquals('Correct class', TCustomM2MRegistry, Inst.ClassType);
    AssertException('Cannot set class if instance is active', EOPFManyToMany, @Self.DoSetInstanceClass);
  finally
    TOPManyToManyRegistry.ClearInstance;
    TOPManyToManyRegistry.SetInstanceClass(Nil);
  end;
end;

procedure TTestManyToManyRegistry.TestDefCount;
begin
  AssertEquals('Initial count', 0, OPM2MRegistry.DefCount);
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertEquals('After first', 1, OPM2MRegistry.DefCount);
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassC, 'ac_junction', 'fk_a', 'fk_c');
  AssertEquals('After second', 2, OPM2MRegistry.DefCount);
end;

procedure TTestManyToManyRegistry.TestDefsAccessor;
var
  Def: TOPManyToManyDef;
begin
  OPM2MRegistry.RegisterManyToMany(TTestM2MClassA, TTestM2MClassB, 'ab_junction', 'fk_a', 'fk_b');
  AssertEquals('DefCount', 1, OPM2MRegistry.DefCount);
  Def:=OPM2MRegistry.Defs[0];
  AssertNotNull('Defs[0] not nil', Def);
  AssertEquals('ClassA', TTestM2MClassA, Def.ClassA);
  AssertEquals('ClassB', TTestM2MClassB, Def.ClassB);
  AssertEquals('JunctionTable', 'ab_junction', Def.JunctionTable);
end;

initialization
  RegisterTest(TTestManyToManyRegistry);

end.
