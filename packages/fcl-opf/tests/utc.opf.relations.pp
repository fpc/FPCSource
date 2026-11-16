unit utc.opf.relations;

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
  Opf.Types, Opf.Registry, Opf.Fields, Opf.Relations, Opf.Persister.RTTI;

Type

  { TTestParent }

  TTestParent = class(TObject)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TTestChild }

  TTestChild = class(TObject)
  private
    FID: Integer;
    FParentID: Integer;
    FValue: String;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Value: String read FValue write FValue;
  end;

  { TTestChild2 }

  TTestChild2 = class(TObject)
  private
    FID: Integer;
    FParentID: Integer;
    FDescription: String;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Description: String read FDescription write FDescription;
  end;

  { TTestUnregistered }

  TTestUnregistered = class(TObject)
  end;

  { TTestRelationRegistry }

  TCustomRelationRegistry = class(TOPRelationRegistry);

  TTestRelationRegistry = class(TTestCase)
  private
    procedure RegisterDuplicate;
    procedure RegisterUnregisteredParent;
    procedure RegisterUnregisteredChild;
    procedure RegisterBadFK;
    procedure DoSetRelationInstanceClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegisterRelationship;
    procedure TestFindChildRelations;
    procedure TestFindChildRelationsEmpty;
    procedure TestFindParentRelation;
    procedure TestFindParentRelationNone;
    procedure TestDuplicateRelationshipRaises;
    procedure TestParentNotRegisteredRaises;
    procedure TestChildNotRegisteredRaises;
    procedure TestForeignKeyNotFoundRaises;
    procedure TestMultipleChildren;
    procedure TestIndexOfRelation;
    procedure TestIndexOfRelationNotFound;
    procedure TestBidirectionalRelationshipsDistinct;
    procedure TestRelationCountEmpty;
    procedure TestRelationCountAfterClear;
    procedure TestCaseInsensitiveFKLookup;
    procedure TestRegisterRelationshipDBCascadeDefault;
    procedure TestRegisterRelationshipDBCascadeTrue;
    procedure TestAssignCopiesDBCascade;
    procedure TestRegisterRelationshipListPropertyDefault;
    procedure TestRegisterRelationshipListPropertyExplicit;
    procedure TestAssignCopiesListPropertyName;
    procedure TestAssignCopiesConstraintName;
    procedure TestSetInstanceClassAndGuard;
    procedure TestRelationsAccessor;
  end;

implementation

{ TTestRelationRegistry }

procedure TTestRelationRegistry.SetUp;
begin
  inherited SetUp;
  OPRegistry.RegisterPersister(TTestParent, TRTTIPersister);
  OPRegistry.RegisterPersister(TTestChild, TRTTIPersister);
  OPRegistry.RegisterPersister(TTestChild2, TRTTIPersister);
end;

procedure TTestRelationRegistry.TearDown;
begin
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  inherited TearDown;
end;

procedure TTestRelationRegistry.RegisterDuplicate;
begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
end;

procedure TTestRelationRegistry.RegisterUnregisteredParent;
begin
  OPRelationRegistry.RegisterRelationship(TTestUnregistered, TTestChild, 'ParentID');
end;

procedure TTestRelationRegistry.RegisterUnregisteredChild;
begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestUnregistered, 'ParentID');
end;

procedure TTestRelationRegistry.RegisterBadFK;
begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'NonExistentField');
end;

procedure TTestRelationRegistry.TestRegisterRelationship;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  AssertNotNull('Result not nil', Rel);
  AssertEquals('ParentClass', TTestParent, Rel.ParentClass);
  AssertEquals('ChildClass', TTestChild, Rel.ChildClass);
  AssertEquals('ForeignKeyFieldName', 'ParentID', Rel.ForeignKeyFieldName);
  AssertEquals('RelationCount', 1, OPRelationRegistry.RelationCount);
end;

procedure TTestRelationRegistry.TestFindChildRelations;

var
  Rels: TOPRelationDefArray;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  Rels:=OPRelationRegistry.FindChildRelations(TTestParent);
  AssertEquals('Length', 1, Length(Rels));
  AssertEquals('ChildClass', TTestChild, Rels[0].ChildClass);
end;

procedure TTestRelationRegistry.TestFindChildRelationsEmpty;

var
  Rels: TOPRelationDefArray;

begin
  Rels:=OPRelationRegistry.FindChildRelations(TTestParent);
  AssertEquals('Length', 0, Length(Rels));
end;

procedure TTestRelationRegistry.TestFindParentRelation;

var
  Rel: TOPRelationDef;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  Rel:=OPRelationRegistry.FindParentRelation(TTestChild);
  AssertNotNull('Result not nil', Rel);
  AssertEquals('ParentClass', TTestParent, Rel.ParentClass);
end;

procedure TTestRelationRegistry.TestFindParentRelationNone;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.FindParentRelation(TTestParent);
  AssertNull('No parent', Rel);
end;

procedure TTestRelationRegistry.TestDuplicateRelationshipRaises;
begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  AssertException('Duplicate raises', EOPFRelation, @RegisterDuplicate);
end;

procedure TTestRelationRegistry.TestParentNotRegisteredRaises;
begin
  AssertException('Unregistered parent raises', EOPFRelation, @RegisterUnregisteredParent);
end;

procedure TTestRelationRegistry.TestChildNotRegisteredRaises;
begin
  AssertException('Unregistered child raises', EOPFRelation, @RegisterUnregisteredChild);
end;

procedure TTestRelationRegistry.TestForeignKeyNotFoundRaises;
begin
  AssertException('Bad FK raises', EOPFRelation, @RegisterBadFK);
end;

procedure TTestRelationRegistry.TestMultipleChildren;

var
  Rels: TOPRelationDefArray;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild2, 'ParentID');
  Rels:=OPRelationRegistry.FindChildRelations(TTestParent);
  AssertEquals('Length', 2, Length(Rels));
end;

procedure TTestRelationRegistry.TestIndexOfRelation;

var
  Idx: Integer;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  Idx:=OPRelationRegistry.IndexOfRelation(TTestParent, TTestChild);
  AssertTrue('Index >= 0', Idx >= 0);
  AssertEquals('Correct index', 0, Idx);
end;

procedure TTestRelationRegistry.TestIndexOfRelationNotFound;

var
  Idx: Integer;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  Idx:=OPRelationRegistry.IndexOfRelation(TTestChild, TTestParent);
  AssertEquals('Not found', -1, Idx);
end;

procedure TTestRelationRegistry.TestBidirectionalRelationshipsDistinct;

var
  Rel1, Rel2: TOPRelationDef;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  Rel1:=OPRelationRegistry.FindChildRelations(TTestParent)[0];
  OPRelationRegistry.RegisterRelationship(TTestChild, TTestParent, 'ID');
  Rel2:=OPRelationRegistry.FindChildRelations(TTestChild)[0];
  AssertNotNull('Parent->Child exists', Rel1);
  AssertNotNull('Child->Parent exists', Rel2);
  AssertEquals('Different relations', TTestParent, Rel1.ParentClass);
  AssertEquals('Reverse relation', TTestChild, Rel2.ParentClass);
  AssertEquals('Two child relations for TTestChild', 1, Length(OPRelationRegistry.FindChildRelations(TTestChild)));
end;

procedure TTestRelationRegistry.TestRelationCountEmpty;

begin
  AssertEquals('Empty count', 0, OPRelationRegistry.RelationCount);
end;

procedure TTestRelationRegistry.TestRelationCountAfterClear;

begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  AssertEquals('Before clear', 1, OPRelationRegistry.RelationCount);
  TOPRelationRegistry.ClearInstance;
  AssertEquals('After clear', 0, OPRelationRegistry.RelationCount);
end;

procedure TTestRelationRegistry.TestCaseInsensitiveFKLookup;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'parentid');
  AssertNotNull('Case insensitive FK', Rel);
  AssertEquals('FK stored as provided', 'parentid', Rel.ForeignKeyFieldName);
end;

procedure TTestRelationRegistry.TestRegisterRelationshipDBCascadeDefault;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  AssertEquals('DBCascade defaults to False', False, Rel.DBCascade);
end;

procedure TTestRelationRegistry.TestRegisterRelationshipDBCascadeTrue;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID', True);
  AssertEquals('DBCascade is True', True, Rel.DBCascade);
end;

procedure TTestRelationRegistry.TestAssignCopiesDBCascade;

var
  Src, Dst: TOPRelationDef;

begin
  Src:=TOPRelationDef.Create(nil);
  Dst:=TOPRelationDef.Create(nil);
  try
    Src.DBCascade:=True;
    Dst.Assign(Src);
    AssertEquals('Assign copies DBCascade', True, Dst.DBCascade);
  finally
    Dst.Free;
    Src.Free;
  end;
end;

procedure TTestRelationRegistry.TestRegisterRelationshipListPropertyDefault;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  AssertEquals('ListPropertyName defaults to empty', '', Rel.ListPropertyName);
end;

procedure TTestRelationRegistry.TestRegisterRelationshipListPropertyExplicit;

var
  Rel: TOPRelationDef;

begin
  Rel:=OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID', False, 'AddressList');
  AssertEquals('ListPropertyName is AddressList', 'AddressList', Rel.ListPropertyName);
end;

procedure TTestRelationRegistry.TestAssignCopiesListPropertyName;

var
  Src, Dst: TOPRelationDef;

begin
  Src:=TOPRelationDef.Create(nil);
  Dst:=TOPRelationDef.Create(nil);
  try
    Src.ListPropertyName:='MyList';
    Dst.Assign(Src);
    AssertEquals('Assign copies ListPropertyName', 'MyList', Dst.ListPropertyName);
  finally
    Dst.Free;
    Src.Free;
  end;
end;

procedure TTestRelationRegistry.TestAssignCopiesConstraintName;
var
  Src, Dst: TOPRelationDef;
begin
  Src:=TOPRelationDef.Create(nil);
  Dst:=TOPRelationDef.Create(nil);
  try
    Src.ConstraintName:='my_custom_fk';
    Dst.Assign(Src);
    AssertEquals('Assign copies ConstraintName', 'my_custom_fk', Dst.ConstraintName);
  finally
    Dst.Free;
    Src.Free;
  end;
end;

procedure TTestRelationRegistry.DoSetRelationInstanceClass;
begin
  TOPRelationRegistry.SetInstanceClass(TCustomRelationRegistry);
end;

procedure TTestRelationRegistry.TestSetInstanceClassAndGuard;
var
  Inst: TOPRelationRegistry;
begin
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  TOPRelationRegistry.SetInstanceClass(TCustomRelationRegistry);
  try
    Inst:=TOPRelationRegistry.Instance;
    AssertNotNull('Have instance', Inst);
    AssertEquals('Correct class', TCustomRelationRegistry, Inst.ClassType);
    AssertException('Cannot set class if instance is active', EOPFRelation, @Self.DoSetRelationInstanceClass);
  finally
    TOPRelationRegistry.ClearInstance;
    TOPRelationRegistry.SetInstanceClass(Nil);
  end;
end;

procedure TTestRelationRegistry.TestRelationsAccessor;
var
  Rel: TOPRelationDef;
begin
  OPRelationRegistry.RegisterRelationship(TTestParent, TTestChild, 'ParentID');
  AssertEquals('RelationCount', 1, OPRelationRegistry.RelationCount);
  Rel := OPRelationRegistry.Relations[0];
  AssertNotNull('Relations[0] not nil', Rel);
  AssertTrue('ParentClass', Rel.ParentClass = TTestParent);
  AssertTrue('ChildClass', Rel.ChildClass = TTestChild);
  AssertEquals('ForeignKeyFieldName', 'ParentID', Rel.ForeignKeyFieldName);
end;

initialization
  RegisterTest(TTestRelationRegistry);

end.
