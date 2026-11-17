unit utc.opf.registry;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry, Data.Db, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry, db, sqldb,
{$ENDIF}
  Opf.Types, Opf.Connections, Opf.Fields, Opf.Registry, Opf.Persister.RTTI;

type
  TMyClass = class(TObject)
  end;

  TMyClass1 = class(TMyClass)
  end;
  TMyClass2 = class(TMyClass)
  end;

  { TMyPersister }

  TMyPersister = Class(TOPPersister)
    Procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    Function ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean; override;
    Function PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean; override;
  end;
  TMyPersister1 = Class(TMyPersister);
  TMyPersister2 = Class(TMyPersister);
  MyStrangeClass = Class(TObject);

  { TTestRegContact - test fixture with published properties for registration tests }

  TTestRegContact = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  // PK resolution test classes
  TTestContact = class(TObject)
  end;

  TTestOrder = class(TObject)
  end;

  TTestExplicitKey = class(TObject)
  end;

  TTestNoKey = class(TObject)
  end;

  ITestPrimaryKey = Interface ['{937F69B6-B4B0-4A95-AFA6-005BD80573FD}']
    Function GetPrimaryKey(AInstance : TObject) : TStringArray;
    Procedure SetPrimaryKey(AInstance : TObject; aValue : TStringArray);
    Function HasPrimaryKeyValue : Boolean;
  end;

  { TTestWithPKInterface }

  TTestWithPKInterface = class(TObject, ITestPrimaryKey)
    Function GetPrimaryKey(AInstance : TObject) : TStringArray;
    Procedure SetPrimaryKey(AInstance : TObject; aValue : TStringArray);
    Function HasPrimaryKeyValue : Boolean;
  end;

  { TPersisterDefTest }

  TPersisterDefTest = Class(TTestCase)
  private
    FDef: TPersisterDef;
  Protected
    Procedure Fill;
    Class Procedure FillProps(D : TPersisterDef; AIndex : Integer);
    Class Procedure TestProps(D : TPersisterDef; AIndex : Integer);
    procedure SetUp; override;
    procedure TearDown; override;
    Property Def : TPersisterDef Read FDef;
  published
    Procedure TestEmpty;
    Procedure TestFill;
    Procedure TestAssign;
    Procedure TestTableName;
  end;

  { TFPOPRegistryTest }

  TMyDef  = Class(TPersisterDef);

  { TMyRegistry }

  TMyRegistry = Class(TOPRegistry)
  Protected
    Function CreateDefs: TPersisterDefs; override;
  end;

  { TPKResolutionTest }

  TPKResolutionTest = class(TTestCase)
  private
    FDef: TPersisterDef;
    FFieldMap: TOPFieldMapList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property Def: TPersisterDef Read FDef;
  published
    procedure TestResolvePKConventionID;
    procedure TestResolvePKConventionClassNameID;
    procedure TestResolvePKExplicit;
    procedure TestResolvePKInterface;
    procedure TestResolvePKComposite;
    procedure TestResolvePKNone;
    procedure TestResolvePKCaseInsensitive;
    procedure TestResolvePKInterfacePreemptsConvention;
    procedure TestResolvePKNilFieldMapRaises;
  end;

  TOPRegistryTest = class(TTestCase)
  private
    procedure SetinstanceClass;
  protected
    FReg : TOPRegistry;
    Procedure Fill;
    Procedure Add;
    Procedure AddNil;
    procedure SetUp; override;
    procedure TearDown; override;
    Property Reg : TOPRegistry Read FReg;
  published
    procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestFill;
    Procedure TestIndexOfClass;
    Procedure TestFindClass;
    Procedure TestDuplicate;
    Procedure TestCreateDefs;
    Procedure TestGlobalInstance;
    Procedure TestGlobalInstanceClass;
    procedure TestCustomRegistryOverride;
  end;

  { TTestCustomEmailContact - fixture for custom persister tests }

  TTestCustomEmailContact = class(TPersistent)
  private
    FID: Integer;
    FEmail: String;
  published
    property ID: Integer read FID write FID;
    property Email: String read FEmail write FEmail;
  end;

  { TTestCustomPersister - custom persister with explicit field mappings }

  TTestCustomPersister = class(TRTTIPersister)
  public
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    class function GetTableName(aClass: TClass): UTF8String; override;
  end;

  { TCustomPersisterTest }

  TCustomPersisterTest = class(TTestCase)
  private
    FReg: TOPRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCustomPersisterFieldMap;
    procedure TestCustomPersisterFindPersister;
    procedure TestCustomPersisterTableName;
  end;

  { TPostRegOverrideTest }

  TPostRegOverrideTest = class(TTestCase)
  private
    FReg: TOPRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOverrideFieldNamePostRegistration;
    procedure TestUnmodifiedFieldsUseConvention;
  end;

  { TRTTIRegistrationTest }

  TRTTIRegistrationTest = class(TTestCase)
  private
    FReg: TOPRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegisterWithRTTIPersister;
    procedure TestRegisterAutoPopulatesFieldMap;
    procedure TestRegisterResolvesPrimaryKey;
    procedure TestFindPersisterAfterRegister;
    procedure TestRegisterNilPersisterClass;
  end;

implementation

{ TMyPersister }

procedure TMyPersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
begin
  // no-op stub
end;

function TMyPersister.ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result:=False;
end;

function TMyPersister.PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result:=False;
end;

{ TPersisterDefTest }

procedure TPersisterDefTest.Fill;

begin
  FillProps(FDef,0);
end;

class procedure TPersisterDefTest.FillProps(D: TPersisterDef; AIndex: Integer);

Var
  N : String;
begin
  N:=IntToStr(aIndex);
  Case aIndex mod 3 of
    0 : begin
        D.InstanceClass:=TMyClass;
        D.PersisterClass:=TMyPersister;
        end;
    1 : begin
        D.InstanceClass:=TMyClass1;
        D.PersisterClass:=TMyPersister1;
        end;
    2 : begin
        D.InstanceClass:=TMyClass2;
        D.PersisterClass:=TMyPersister2;
        end;
  end;
  D.CacheSQL:=(aIndex mod 2)=0;
  D.InsertSQL:='INSERT TABLE'+N;
  D.UpdateSQL:='UPDATE TABLE'+N;
  D.DeleteSQL:='DELETE TABLE'+N;
  D.InstanceSQL:='SELECT TABLE'+N;
  D.ListSQL:='SELECT ALL TABLE'+N;
  D.TableName:='T'+N;
end;

class procedure TPersisterDefTest.TestProps(D: TPersisterDef; AIndex: Integer);
Var
  N : String;
begin
  N:=IntToStr(aIndex);
  Case aIndex mod 3 of
    0 : begin
        AssertEquals('Instance class 0',TMyClass,D.InstanceClass);
        AssertEquals('Persister class 0',TMyPersister,D.PersisterClass);
        end;
    1 : begin
        AssertEquals('Instance class 1',TMyClass1,D.InstanceClass);
        AssertEquals('Persister class 1',TMyPersister1,D.PersisterClass);
        end;
    2 : begin
        AssertEquals('Instance class 2',TMyClass2,D.InstanceClass);
        AssertEquals('Persister class 2',TMyPersister2,D.PersisterClass);
        end;
  end;
  AssertEquals('TableName','T'+N,D.TableName);
  AssertEquals('CacheSQL',(aIndex mod 2)=0,D.CacheSQL);
  AssertEquals('InsertSQL', 'INSERT TABLE'+N,D.InsertSQL);
  AssertEquals('UpdateSQL', 'UPDATE TABLE'+N,D.UpdateSQL);
  AssertEquals('DeleteSQL', 'DELETE TABLE'+N,D.DeleteSQL);
  AssertEquals('InstanceSQL', 'SELECT TABLE'+N,D.InstanceSQL);
  AssertEquals('ListSQL', 'SELECT ALL TABLE'+N,D.ListSQL);
end;

procedure TPersisterDefTest.SetUp;
begin
  inherited SetUp;
  FDef:=TPersisterDef.Create(nil);
end;

procedure TPersisterDefTest.TearDown;
begin
  FreeAndNil(FDef);
  inherited TearDown;
end;

procedure TPersisterDefTest.TestEmpty;
begin
  AssertNotNull('Have def',Def);
end;

procedure TPersisterDefTest.TestFill;
begin
  Fill;
  TestProps(Def,0);
end;

procedure TPersisterDefTest.TestAssign;

var
  D : TPersisterDef;

begin
  Fill;
  D:=TPersisterDef.Create(nil);
  try
    D.Assign(Def);
    TestProps(D,0);
  finally
    D.Free;
  end;
end;

procedure TPersisterDefTest.TestTableName;
begin
  AssertEquals('Default, no T','MyStrangeClass',TOPPersister.GetTableName(MyStrangeClass));
  AssertEquals('Default, with T','MyClass1',TOPPersister.GetTableName(TMyClass1));
end;

{ TMyRegistry }

function TMyRegistry.CreateDefs: TPersisterDefs;
begin
  Result:=TPersisterDefs.Create(TMyDef);
end;

{ TOPRegistryTest }

procedure TOPRegistryTest.Fill;
begin
  Add;
  Reg.RegisterPersister(TMyClass1,TMyPersister1);
  Reg.RegisterPersister(TMyClass2,TMyPersister2);
end;

procedure TOPRegistryTest.Add;
begin
  Reg.RegisterPersister(TMyClass,TMyPersister);
end;

procedure TOPRegistryTest.AddNil;
begin
  Reg.RegisterPersister(Nil,TMyPersister);
end;

procedure TOPRegistryTest.SetUp;
begin
  inherited SetUp;
  FReg:=TOPRegistry.Create(Nil);
  TOPRegistry.ClearInstance;
end;

procedure TOPRegistryTest.TearDown;
begin
  TOPRegistry.ClearInstance;
  TOPRegistry.SetInstanceClass(Nil);
  FreeAndNil(FReg);
  inherited TearDown;
end;

procedure TOPRegistryTest.TestEmpty;
begin
  AssertNotNull('Have list',Reg);
  AssertEquals('Empty',0,Reg.PersisterCount);
end;

procedure TOPRegistryTest.TestAdd;

Var
  D : TPersisterDef;

begin
  D:=Reg.RegisterPersister(TMyClass,TMyPersister);
  AssertEquals('Count',1,Reg.PersisterCount);
  AssertSame('Result',D,Reg[0]);
  AssertEquals('1 instance',TMyClass,Reg[0].InstanceClass);
  AssertEquals('1 Persister',TMyPersister,Reg[0].PersisterClass);
end;

procedure TOPRegistryTest.TestFill;
begin
  Fill;
  AssertEquals('Count',3,Reg.PersisterCount);
  AssertEquals('1 instance',TMyClass,Reg[0].InstanceClass);
  AssertEquals('1 Persister',TMyPersister,Reg[0].PersisterClass);
  AssertEquals('2 instance',TMyClass1,Reg[1].InstanceClass);
  AssertEquals('2 Persister',TMyPersister1,Reg[1].PersisterClass);
  AssertEquals('1 instance',TMyClass2,Reg[2].InstanceClass);
  AssertEquals('1 Persister',TMyPersister2,Reg[2].PersisterClass);
end;

procedure TOPRegistryTest.TestIndexOfClass;
begin
  Fill;
  AssertEquals('Nonexist',-1,Reg.IndexOfPersister(TOPRegistryTest));
  AssertEquals('First ',0,Reg.IndexOfPersister(TMyClass));
  AssertEquals('Second ',1,Reg.IndexOfPersister(TMyClass1));
  AssertEquals('Third ',2,Reg.IndexOfPersister(TMyClass2));
end;

procedure TOPRegistryTest.TestFindClass;
begin
  Fill;
  AssertNull('Nonexist',Reg.FindPersister(TOPRegistryTest));
  AssertSame('First ',Reg[0],Reg.FindPersister(TMyClass));
  AssertSame('Second ',Reg[1],Reg.FindPersister(TMyClass1));
  AssertSame('Third ',Reg[2],Reg.FindPersister(TMyClass2));
end;

procedure TOPRegistryTest.TestDuplicate;
begin
  Add;
  AssertException('No duplicates',EOPFRegistry,@Add);
end;

procedure TOPRegistryTest.TestCreateDefs;

Var
  R : TOPRegistry;
  D : TPersisterDef;

begin
  R:=TMyRegistry.Create(Nil);
  try
    D:=R.RegisterPersister(TMyClass,TMypersister);
    AssertEquals('Correct class',TMyDef,D.ClassType);
  finally
    R.Free;
  end;
end;

procedure TOPRegistryTest.TestGlobalInstance;
Var
  R : TOPRegistry;
begin
  R:=TOPRegistry.Instance;
  AssertNotNull('Have instance',R);
  TOPRegistry.ClearInstance;
  AssertNotNull('Have instance',TOPRegistry.Instance);
end;

procedure TOPRegistryTest.SetinstanceClass;
begin
  TOPRegistry.SetInstanceClass(TMyRegistry);
end;

procedure TOPRegistryTest.TestGlobalInstanceClass;

Var
  R : TOPRegistry;

begin
  TOPRegistry.ClearInstance;
  Self.SetInstanceClass;
  try
    R:=TOPRegistry.Instance;
    AssertNotNull('Have instance',R);
    AssertEquals('Correct class',TMyRegistry,R.ClassType);
    AssertException('Cannot set class if instance is active',EOPFRegistry,@Self.SetinstanceClass);
  finally
    TOPRegistry.ClearInstance;
    TOPRegistry.SetInstanceClass(Nil);
  end;
end;

procedure TOPRegistryTest.TestCustomRegistryOverride;
var
  R: TOPRegistry;
  D: TPersisterDef;
begin
  TOPRegistry.ClearInstance;
  TOPRegistry.SetInstanceClass(TMyRegistry);
  try
    R:=TOPRegistry.Instance;
    AssertNotNull('Have instance', R);
    AssertEquals('Correct class', TMyRegistry, R.ClassType);
    D:=R.RegisterPersister(TMyClass, TMyPersister);
    AssertEquals('CreateDefs override used', TMyDef, D.ClassType);
  finally
    TOPRegistry.ClearInstance;
    TOPRegistry.SetInstanceClass(Nil);
  end;
end;

{ TTestWithPKInterface }

function TTestWithPKInterface.GetPrimaryKey(AInstance: TObject): TStringArray;
begin
  Result:=nil;
end;

procedure TTestWithPKInterface.SetPrimaryKey(AInstance: TObject; aValue: TStringArray);
begin
  // stub
end;

function TTestWithPKInterface.HasPrimaryKeyValue: Boolean;
begin
  Result:=False;
end;

{ TPKResolutionTest }

procedure TPKResolutionTest.SetUp;
begin
  inherited SetUp;
  FDef:=TPersisterDef.Create(nil);
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  FDef.FieldMap:=FFieldMap;
end;

procedure TPKResolutionTest.TearDown;
begin
  FreeAndNil(FDef);
  inherited TearDown;
end;

procedure TPKResolutionTest.TestResolvePKConventionID;
var
  F: TOPFieldMap;
begin
  Def.InstanceClass:=TTestContact;
  FFieldMap.AddField('ID', 'ID', [foInSelect, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  Def.ResolvePrimaryKey;
  F:=FFieldMap.FindByPropertyName('ID');
  AssertTrue('ID field has foInKey', foInKey in F.Options);
end;

procedure TPKResolutionTest.TestResolvePKConventionClassNameID;
var
  F: TOPFieldMap;
begin
  Def.InstanceClass:=TTestOrder;
  FFieldMap.AddField('OrderID', 'TestOrderID', [foInSelect, foInInsert]);
  FFieldMap.AddField('Amount', 'Amount', [foInSelect, foInInsert]);
  Def.ResolvePrimaryKey;
  F:=FFieldMap.FindByPropertyName('TestOrderID');
  AssertTrue('TestOrderID field has foInKey', foInKey in F.Options);
end;

procedure TPKResolutionTest.TestResolvePKExplicit;
var
  F: TOPFieldMap;
begin
  Def.InstanceClass:=TTestExplicitKey;
  FFieldMap.AddField('MyKey', 'MyKey', [foInKey, foInSelect]);
  FFieldMap.AddField('ID', 'ID', [foInSelect, foInInsert]);
  Def.ResolvePrimaryKey;
  F:=FFieldMap.FindByPropertyName('ID');
  AssertFalse('ID field should not have foInKey', foInKey in F.Options);
  F:=FFieldMap.FindByPropertyName('MyKey');
  AssertTrue('MyKey field keeps foInKey', foInKey in F.Options);
end;

procedure TPKResolutionTest.TestResolvePKInterface;
begin
  Def.InstanceClass:=TTestWithPKInterface;
  FFieldMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  Def.ResolvePrimaryKey;
  AssertTrue('HasIOPPrimaryKey is True', Def.HasIOPPrimaryKey);
  AssertFalse('No key fields marked', Def.HasKeyFields);
end;

procedure TPKResolutionTest.TestResolvePKComposite;
var
  F1, F2: TOPFieldMap;
begin
  Def.InstanceClass:=TTestContact;
  FFieldMap.AddField('Key1', 'Key1', [foInKey, foInSelect]);
  FFieldMap.AddField('Key2', 'Key2', [foInKey, foInSelect]);
  FFieldMap.AddField('Name', 'Name', [foInSelect]);
  Def.ResolvePrimaryKey;
  F1:=FFieldMap.FindByPropertyName('Key1');
  F2:=FFieldMap.FindByPropertyName('Key2');
  AssertTrue('Key1 has foInKey', foInKey in F1.Options);
  AssertTrue('Key2 has foInKey', foInKey in F2.Options);
end;

procedure TPKResolutionTest.TestResolvePKNone;
begin
  Def.InstanceClass:=TTestNoKey;
  FFieldMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  FFieldMap.AddField('Value', 'Value', [foInSelect, foInInsert]);
  try
    Def.ResolvePrimaryKey;
    Fail('Expected EOPFRegistry');
  except
    on E: EOPFRegistry do
      AssertTrue('Error message contains class name', Pos('TTestNoKey', E.Message) > 0);
  end;
end;

procedure TPKResolutionTest.TestResolvePKCaseInsensitive;
var
  F: TOPFieldMap;
begin
  Def.InstanceClass:=TTestContact;
  FFieldMap.AddField('id', 'id', [foInSelect, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  Def.ResolvePrimaryKey;
  F:=FFieldMap.FindByPropertyName('id');
  AssertTrue('lowercase id field has foInKey', foInKey in F.Options);
end;

procedure TPKResolutionTest.TestResolvePKInterfacePreemptsConvention;
var
  F: TOPFieldMap;
begin
  Def.InstanceClass:=TTestWithPKInterface;
  FFieldMap.AddField('ID', 'ID', [foInSelect, foInInsert]);
  FFieldMap.AddField('Name', 'Name', [foInSelect, foInInsert]);
  Def.ResolvePrimaryKey;
  AssertTrue('HasIOPPrimaryKey is True', Def.HasIOPPrimaryKey);
  F:=FFieldMap.FindByPropertyName('ID');
  AssertFalse('ID field should not have foInKey when interface present', foInKey in F.Options);
end;

procedure TPKResolutionTest.TestResolvePKNilFieldMapRaises;
var
  D: TPersisterDef;
begin
  D:=TPersisterDef.Create(nil);
  try
    D.InstanceClass:=TTestContact;
    try
      D.ResolvePrimaryKey;
      Fail('Expected EOPFRegistry for nil field map');
    except
      on E: EOPFRegistry do
        AssertTrue('Error raised for nil field map', True);
    end;
  finally
    D.Free;
  end;
end;

{ TTestCustomPersister }

procedure TTestCustomPersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
begin
  aMap.AddField('contact_id', 'ID', [foInKey, foInSelect, foInInsert]);
  aMap.AddField('email_address', 'Email', [foInSelect, foInInsert, foInUpdate]);
end;

class function TTestCustomPersister.GetTableName(aClass: TClass): UTF8String;
begin
  Result:='CustomEmailTable';
end;

{ TCustomPersisterTest }

procedure TCustomPersisterTest.SetUp;
begin
  inherited SetUp;
  FReg:=TOPRegistry.Create(Nil);
end;

procedure TCustomPersisterTest.TearDown;
begin
  FreeAndNil(FReg);
  inherited TearDown;
end;

procedure TCustomPersisterTest.TestCustomPersisterFieldMap;
var
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  D:=FReg.RegisterPersister(TTestCustomEmailContact, TTestCustomPersister);
  AssertNotNull('Def created', D);
  AssertEquals('FieldMap count', 2, D.FieldMap.Count);
  F:=D.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field found', F);
  AssertEquals('ID FieldName', 'contact_id', F.FieldName);
  AssertTrue('ID has foInKey', foInKey in F.Options);
  F:=D.FieldMap.FindByPropertyName('Email');
  AssertNotNull('Email field found', F);
  AssertEquals('Email FieldName', 'email_address', F.FieldName);
end;

procedure TCustomPersisterTest.TestCustomPersisterFindPersister;
var
  D, Found: TPersisterDef;
begin
  D:=FReg.RegisterPersister(TTestCustomEmailContact, TTestCustomPersister);
  Found:=FReg.FindPersister(TTestCustomEmailContact);
  AssertNotNull('FindPersister returns non-nil', Found);
  AssertSame('FindPersister returns same def', D, Found);
  AssertEquals('PersisterClass matches', TTestCustomPersister, Found.PersisterClass);
end;

procedure TCustomPersisterTest.TestCustomPersisterTableName;
var
  D: TPersisterDef;
begin
  D:=FReg.RegisterPersister(TTestCustomEmailContact, TTestCustomPersister);
  AssertEquals('TableName from custom persister', 'CustomEmailTable', D.TableName);
end;

{ TPostRegOverrideTest }

procedure TPostRegOverrideTest.SetUp;
begin
  inherited SetUp;
  FReg:=TOPRegistry.Create(Nil);
end;

procedure TPostRegOverrideTest.TearDown;
begin
  FreeAndNil(FReg);
  inherited TearDown;
end;

procedure TPostRegOverrideTest.TestOverrideFieldNamePostRegistration;
var
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  D:=FReg.RegisterPersister(TTestRegContact, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('Name');
  AssertNotNull('Name field exists', F);
  // Before override, FieldName equals PropertyName
  AssertEquals('Default FieldName', 'Name', F.FieldName);
  // Override
  F.FieldName:='full_name';
  AssertEquals('Overridden FieldName', 'full_name', F.FieldName);
  AssertEquals('PropertyName unchanged', 'Name', F.PropertyName);
end;

procedure TPostRegOverrideTest.TestUnmodifiedFieldsUseConvention;
var
  D: TPersisterDef;
  FID, FName: TOPFieldMap;
begin
  D:=FReg.RegisterPersister(TTestRegContact, TRTTIPersister);
  // Override only Name
  FName:=D.FieldMap.FindByPropertyName('Name');
  FName.FieldName:='full_name';
  // ID should still use convention
  FID:=D.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field exists', FID);
  AssertEquals('ID FieldName uses convention', 'ID', FID.FieldName);
  // Name is overridden
  AssertEquals('Name FieldName overridden', 'full_name', FName.FieldName);
end;

{ TRTTIRegistrationTest }

procedure TRTTIRegistrationTest.SetUp;
begin
  inherited SetUp;
  FReg:=TOPRegistry.Create(Nil);
end;

procedure TRTTIRegistrationTest.TearDown;
begin
  FreeAndNil(FReg);
  inherited TearDown;
end;

procedure TRTTIRegistrationTest.TestRegisterWithRTTIPersister;
var
  D: TPersisterDef;
begin
  D:=FReg.RegisterPersister(TTestRegContact, TRTTIPersister);
  AssertNotNull('Def created', D);
  AssertNotNull('FieldMap created', D.FieldMap);
  AssertEquals('FieldMap has 2 entries', 2, D.FieldMap.Count);
end;

procedure TRTTIRegistrationTest.TestRegisterAutoPopulatesFieldMap;
var
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  D:=FReg.RegisterPersister(TTestRegContact, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field found', F);
  AssertTrue('ID has TypeInfo', F.PropertyTypeInfo <> nil);
  F:=D.FieldMap.FindByPropertyName('Name');
  AssertNotNull('Name field found', F);
  AssertTrue('Name has TypeInfo', F.PropertyTypeInfo <> nil);
end;

procedure TRTTIRegistrationTest.TestRegisterResolvesPrimaryKey;
var
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  D:=FReg.RegisterPersister(TTestRegContact, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  AssertNotNull('ID field found', F);
  AssertTrue('ID field has foInKey', foInKey in F.Options);
end;

procedure TRTTIRegistrationTest.TestFindPersisterAfterRegister;
var
  D, Found: TPersisterDef;
begin
  D:=FReg.RegisterPersister(TTestRegContact, TRTTIPersister);
  Found:=FReg.FindPersister(TTestRegContact);
  AssertNotNull('FindPersister returns non-nil', Found);
  AssertSame('FindPersister returns same def', D, Found);
  AssertEquals('InstanceClass matches', TTestRegContact, Found.InstanceClass);
  AssertEquals('PersisterClass matches', TRTTIPersister, Found.PersisterClass);
end;

procedure TRTTIRegistrationTest.TestRegisterNilPersisterClass;
var
  D: TPersisterDef;
begin
  D:=FReg.RegisterPersister(TMyClass, Nil);
  AssertNotNull('Def created', D);
  AssertNotNull('An empty field map to fill', D.FieldMap);
  AssertEquals('Empty', 0, D.FieldMap.Count);
  AssertNull('PersisterClass is nil', D.PersisterClass);
end;

initialization
  RegisterTests([TPersisterDefTest,TOPRegistryTest,TPKResolutionTest,TRTTIRegistrationTest,TCustomPersisterTest,TPostRegOverrideTest]);
end.

