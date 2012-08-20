unit tcclasstype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pparser, pastree, testregistry, tctypeparser;

type

  { TTestClassType }

  TTestClassType = Class(TBaseTestTypeParser)
  Private
    FDecl : TStrings;
    FClass : TPasClassType;
    FMember1: TPasElement;
    FParent : String;
    FEnded,
    FStarted: Boolean;
    function GetF1: TPasVariable;
    function GetM(AIndex : Integer): TPasElement;
    function GetMM(AIndex : Integer): TPasProcedure;
    function GetMF1: TPasFunction;
    function GetP1: TPasProperty;
    function GetP2: TPasProperty;
    function GetT(AIndex : Integer) : TPasType;
  protected
    Procedure StartClass (AParent : String = 'TObject'; InterfaceList : String = '');
    Procedure StartVisibility(A : TPasMemberVisibility);
    Procedure EndClass(AEnd : String = 'end');
    Procedure AddMember(S : String);
    Procedure ParseClass;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DefaultMethod;
    Procedure AssertVisibility(V : TPasMemberVisibility = visDefault; Member : TPasElement = Nil);
    procedure AssertMemberType(AType : TClass; Member : TPaselement = Nil);
    procedure AssertMemberName(AName : string; Member : TPaselement = Nil);
    Procedure AssertProperty(P : TPasProperty; AVisibility : TPasMemberVisibility;AName,ARead,AWrite,AStored,AImplements : String; AArgCount : Integer; ADefault,ANodefault : Boolean);
    Property TheClass : TPasClassType Read FClass;
    Property Members[AIndex : Integer] : TPasElement Read GetM;
    Property Member1 : TPasElement Read FMember1;
    Property Field1 : TPasVariable Read GetF1;
    Property Method1 : TPasProcedure Index 0 Read GetMM;
    Property Method2 : TPasProcedure Index 1 Read GetMM;
    Property Method3 : TPasProcedure index 2 Read GetMM;
    Property FunctionMethod1 : TPasFunction Read GetMF1;
    Property Property1 : TPasProperty Read GetP1;
    Property Property2 : TPasProperty Read GetP2;
    Property Type1 : TPasType Index 0 Read GetT;
    Property Type2 : TPasType Index 1 Read GetT;
  published
    procedure TestEmpty;
    procedure TestEmptyDeprecated;
    procedure TestEmptyEnd;
    procedure TestEmptyEndNoParent;
    Procedure TestOneInterface;
    Procedure TestTwoInterfaces;
    Procedure TestOneField;
    Procedure TestOneVarField;
    Procedure TestOneClassField;
    Procedure TestOneFieldVisibility;
    Procedure TestOneFieldDeprecated;
    Procedure TestTwoFields;
    Procedure TestTwoFieldsB;
    Procedure TestTwoVarFieldsB;
    Procedure TestTwoFieldsVisibility;
    procedure TestHintFieldDeprecated;
    procedure TestHintFieldPlatform;
    procedure TestHintFieldExperimental;
    procedure TestHintFieldLibraryError;
    procedure TestHintFieldUninmplemented;
    Procedure TestMethodSimple;
    Procedure TestClassMethodSimple;
    Procedure TestFunctionMethodSimple;
    Procedure TestClassFunctionMethodSimple;
    Procedure TestMethodOneArg;
    Procedure TestMethodVirtual;
    Procedure TestMethodVirtualSemicolon;
    Procedure TestMethodVirtualAbstract;
    Procedure TestMethodOverride;
    procedure TestMethodDynamic;
    procedure TestMethodReintroduce;
    procedure TestMethodInline;
    Procedure TestMethodVisibility;
    Procedure TestMethodHint;
    Procedure TestMethodVirtualHint;
    Procedure Test2Methods;
    Procedure Test2MethodsDifferentVisibility;
    Procedure TestPropertyRedeclare;
    Procedure TestPropertyReadOnly;
    Procedure TestPropertyReadWrite;
    Procedure TestPropertyWriteOnly;
    Procedure TestPropertyDefault;
    Procedure TestPropertyNoDefault;
    Procedure TestPropertyIndex;
    Procedure TestPropertyStored;
    Procedure TestPropertyStoredFalse;
    Procedure TestPropertyFullyQualifiedType;
    Procedure TestPropertyArrayReadOnly;
    Procedure TestPropertyArrayReadWrite;
    Procedure TestPropertyArrayReadOnlyDefault;
    Procedure TestPropertyArrayReadWriteDefault;
    Procedure TestPropertyArrayMultiDimReadOnly;
    Procedure TestPropertyImplements;
    Procedure TestPropertyImplementsFullyQualifiedName;
    Procedure TestPropertyReadFromRecordField;
    procedure TestPropertyReadWriteFromRecordField;
    Procedure TestLocalSimpleType;
    Procedure TestLocalSimpleTypes;
  end;

implementation

{ TTestClassType }

function TTestClassType.GetM(AIndex : Integer): TPasElement;
begin
  AssertNotNull('Have class',TheClass);
  if (AIndex>=TheClass.Members.Count) then
    Fail('No member '+IntToStr(AIndex));
  AssertNotNull('Have member'+IntToStr(AIndex),TheClass.Members[AIndex]);
  If Not (TObject(TheClass.Members[AIndex]) is TPasElement) then
    Fail('Member '+IntTostr(AIndex)+' is not a Tpaselement');
  Result:=TPasElement(TheClass.Members[AIndex])
end;

function TTestClassType.GetMM(AIndex : integer): TPasProcedure;
begin
  AssertNotNull('Have member '+IntToStr(AIndex),Members[AIndex]);
  AssertEquals('Member is method '+IntToStr(AIndex),TPasProcedure,Members[Aindex].ClassType);
  Result:=TPasProcedure(Members[Aindex]);
end;

function TTestClassType.GetMF1: TPasFunction;
begin
  AssertNotNull('Have 1 member',Member1);
  AssertEquals('Member 1 is function method',TPasFunction,Member1.ClassType);
  Result:=TPasFunction(Member1);
end;

function TTestClassType.GetP1: TPasProperty;
begin
  AssertNotNull('Have 1 member',Member1);
  AssertEquals('Member 1 is property',TPasProperty,Member1.ClassType);
  Result:=TPasProperty(Member1);
end;

function TTestClassType.GetP2: TPasProperty;
begin
  AssertNotNull('Have 2 members',Members[1]);
  AssertEquals('Member 1 is property',TPasProperty,Members[1].ClassType);
  Result:=TPasProperty(Members[1]);
end;

function TTestClassType.GetT(Aindex :integer): TPasType;
begin
  AssertNotNull('Have member '+IntToStr(AIndex),Members[AIndex]);
  if not (Members[AIndex] is TPasType) then
    Fail('Member '+IntToStr(AIndex)+' is not a type');
  Result:=TPasType(Members[AIndex]);
end;

function TTestClassType.GetF1: TPasVariable;
begin
  AssertNotNull('Have 1 member',Member1);
  AssertEquals('Member 1 is field',TPasVariable,Member1.ClassType);
  Result:=TPasVariable(Member1);
end;

procedure TTestClassType.StartClass(AParent: String = 'TObject'; InterfaceList: String = '');

Var
  S : String;
begin
  FStarted:=True;
  S:='TMyClass = Class';
  if (AParent<>'') then
    begin
    S:=S+'('+AParent;
    if (InterfaceList<>'') then
      S:=S+','+InterfaceList;
    S:=S+')';
    end;
  FDecl.Add(S);
  FParent:=AParent;
end;

procedure TTestClassType.StartVisibility(A: TPasMemberVisibility);
begin
  if not FStarted then
    StartClass;
  FDecl.Add('  '+VisibilityNames[A]);
end;

procedure TTestClassType.EndClass(AEnd: String);
begin
  if FEnded then exit;
  if not FStarted then
    StartClass;
  FEnded:=True;
  if (AEnd<>'') then
    FDecl.Add('  '+AEnd);
end;

procedure TTestClassType.AddMember(S: String);
begin
  if Not FStarted then
    StartClass;
  FDecl.Add('    '+S+';');
end;

procedure TTestClassType.ParseClass;
begin
  EndClass;
  Add('Type');
  Add('  '+TrimRight(FDecl.Text)+';');
  ParseDeclarations;
  AssertEquals('One class type definition',1,Declarations.Classes.Count);
  AssertEquals('First declaration is type definition.',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  FClass:=TObject(Declarations.Classes[0]) as TPasClassType;
  if (FParent<>'') then
     begin
     AssertNotNull('Have parent class',TheClass.AncestorType);
     AssertEquals('Parent class',TPasUnresolvedTypeRef,TheClass.AncestorType.ClassType);
     AssertEquals('Parent class name','TObject',TPasUnresolvedTypeRef(TheClass.AncestorType).Name);
     end;
  if TheClass.Members.Count>0 then
    FMember1:=TObject(TheClass.Members[0]) as TPaselement;
end;

procedure TTestClassType.SetUp;
begin
  inherited SetUp;
  FDecl:=TstringList.Create;
  FClass:=Nil;
  FParent:='';
  FStarted:=False;
end;

procedure TTestClassType.TearDown;
begin
  FClass:=Nil;
  FreeAndNil(FDecl);
  inherited TearDown;
end;

procedure TTestClassType.AssertVisibility(V: TPasMemberVisibility;
  Member: TPasElement);
begin
  If Member=Nil then
    Member:=Member1;
  AssertNotNull('Cannot get visibility of null member',Member);
  AssertEquals('Visibility of '+Member.Name,V,Member.Visibility);
end;

procedure TTestClassType.AssertMemberType(AType: TClass; Member: TPaselement);
begin
  If Member=Nil then
    Member:=Member1;
  AssertEquals('Member '+Member.Name+' type',AType,Member.ClassType);
end;

procedure TTestClassType.AssertMemberName(AName: string; Member: TPaselement);
begin
  If Member=Nil then
    Member:=Member1;
  AssertEquals('Member name ',AName,Member.Name)
end;

procedure TTestClassType.AssertProperty(P: TPasProperty; AVisibility : TPasMemberVisibility;AName, ARead, AWrite,
  AStored,AImplements: String; AArgCount: Integer; ADefault, ANodefault: Boolean);
begin
  AssertEquals(P.Name+': Visibility',AVisibility,P.Visibility);
  Assertequals(P.Name+': No args',AArgCount,P.Args.Count);
  Assertequals(P.Name+': Read accessor',ARead,P.ReadAccessorName);
  Assertequals(P.Name+': Write accessor',AWrite,P.WriteAccessorName);
  Assertequals(P.Name+': implements name',AImplements,P.ImplementsName);
  Assertequals(P.Name+': stored accessor name',AStored,P.StoredAccessorName);
  Assertequals(P.Name+': default',ADefault,P.IsDefault);
  Assertequals(P.Name+': nodefault',ANodefault,P.IsNoDefault);
end;

procedure TTestClassType.TestEmpty;
begin
  EndClass('');
  ParseClass;
  AssertEquals('No members',0,TheClass.Members.Count);
end;

procedure TTestClassType.TestEmptyDeprecated;
begin
  EndClass('end deprecated');
  ParseClass;
  AssertEquals('No members',0,TheClass.Members.Count);
  HaveHint(hDeprecated,Theclass.Hints);
end;

procedure TTestClassType.TestEmptyEnd;
begin
  ParseClass;
  AssertEquals('No members',0,TheClass.Members.Count);
end;

procedure TTestClassType.TestEmptyEndNoParent;
begin
  StartClass('','');
  ParseClass;
  AssertEquals('No members',0,TheClass.Members.Count);
end;

procedure TTestClassType.TestOneInterface;
begin
  StartClass('TObject','ISomething');
  ParseClass;
  AssertEquals('Have 1 interface',1,TheClass.Interfaces.Count);
  AssertNotNull('Correct class',TheClass.Interfaces[0]);
  AssertEquals('Correct class',TPasUnresolvedTypeRef,TObject(TheClass.Interfaces[0]).ClassType);
  AssertEquals('Interface name','ISomething',TPasUnresolvedTypeRef(TheClass.Interfaces[0]).Name);
end;

procedure TTestClassType.TestTwoInterfaces;
begin
  StartClass('TObject','ISomething, ISomethingElse');
  ParseClass;
  AssertEquals('Have 2 interface',2,TheClass.Interfaces.Count);
  AssertNotNull('Correct class',TheClass.Interfaces[0]);
  AssertEquals('Correct class',TPasUnresolvedTypeRef,TObject(TheClass.Interfaces[0]).ClassType);
  AssertEquals('Interface name','ISomething',TPasUnresolvedTypeRef(TheClass.Interfaces[0]).Name);
  AssertNotNull('Correct class',TheClass.Interfaces[1]);
  AssertEquals('Correct class',TPasUnresolvedTypeRef,TObject(TheClass.Interfaces[1]).ClassType);
  AssertEquals('Interface name','ISomethingElse',TPasUnresolvedTypeRef(TheClass.Interfaces[1]).Name);
end;

procedure TTestClassType.TestOneField;
begin
  AddMember('a : integer');
  ParseClass;
  AssertNotNull('Have 1 field',Field1);
  AssertMemberName('a');
  AssertVisibility;
end;

procedure TTestClassType.TestOneVarField;
begin
  StartVisibility(visPublished);
  FDecl.Add('var');
  AddMember('a : integer');
  ParseClass;
  AssertNotNull('Have 1 field',Field1);
  AssertMemberName('a');
  AssertVisibility(visPublished);
end;

procedure TTestClassType.TestOneClassField;
begin
  StartVisibility(visPublished);
  FDecl.Add('class var');
  AddMember('a : integer');
  ParseClass;
  AssertNotNull('Have 1 field',Field1);
  AssertMemberName('a');
  AssertVisibility(visPublished);
  if not (vmClass in Field1.VarModifiers) then
     Fail('Field is not a class field');
end;

procedure TTestClassType.TestOneFieldVisibility;
begin
  StartVisibility(visPublished);
  AddMember('a : integer');
  ParseClass;
  AssertNotNull('Have 1 field',Field1);
  AssertMemberName('a');
  AssertVisibility(visPublished);
end;

procedure TTestClassType.TestOneFieldDeprecated;
begin
  AddMember('a : integer deprecated');
  ParseClass;
  AssertNotNull('Have 1 field',Field1);
  AssertMemberName('a');
  HaveHint(hDeprecated,Member1.Hints);
  AssertVisibility;
end;

procedure TTestClassType.TestTwoFields;
begin
  AddMember('a : integer');
  AddMember('b : integer');
  ParseClass;
  AssertEquals('2 members',2,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('a');
  AssertVisibility;
  AssertNotNull('Have field',Members[1]);
  AssertMemberName('b',Members[1]);
  AssertMemberType(TPasVariable,Members[1]);
  AssertVisibility(visDefault,Members[1]);
end;

procedure TTestClassType.TestTwoFieldsB;
begin
  AddMember('a,b : integer');
  ParseClass;
  AssertEquals('2 members',2,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('a');
  AssertVisibility;
  AssertNotNull('Have field',Members[1]);
  AssertMemberName('b',Members[1]);
  AssertMemberType(TPasVariable,Members[1]);
  AssertVisibility(visDefault,Members[1]);
end;

procedure TTestClassType.TestTwoVarFieldsB;
begin
  StartVisibility(visPublic);
  FDecl.Add('var');
  AddMember('a,b : integer');
  ParseClass;
  AssertEquals('2 members',2,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('a');
  AssertVisibility(vispublic);
  AssertNotNull('Have field',Members[1]);
  AssertMemberName('b',Members[1]);
  AssertMemberType(TPasVariable,Members[1]);
  AssertVisibility(visPublic,Members[1]);
end;

procedure TTestClassType.TestTwoFieldsVisibility;
begin
  StartVisibility(visPublic);
  AddMember('a,b : integer');
  ParseClass;
  AssertEquals('2 members',2,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('a');
  AssertVisibility(vispublic);
  AssertNotNull('Have field',Members[1]);
  AssertMemberName('b',Members[1]);
  AssertMemberType(TPasVariable,Members[1]);
  AssertVisibility(visPublic,Members[1]);
end;

procedure TTestClassType.TestHintFieldDeprecated;
begin
  AddMember('deprecated : integer');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('deprecated');
end;

procedure TTestClassType.TestHintFieldPlatform;
begin
  AddMember('platform : integer');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('platform');
end;

procedure TTestClassType.TestHintFieldLibraryError;
begin
  AddMember('library: integer');
  AssertException(EParserError,@ParseClass);
end;

procedure TTestClassType.TestHintFieldExperimental;
begin
  AddMember('experimental: integer');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('experimental');
end;

procedure TTestClassType.TestHintFieldUninmplemented;
begin
  AddMember('unimplemented: integer');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertNotNull('Have field',Field1);
  AssertMemberName('unimplemented');
end;

procedure TTestClassType.TestMethodSimple;
begin
  AddMember('Procedure DoSomething');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertNotNull('Have method',Method1);
  AssertMemberName('DoSomething');
  AssertEquals('No modifiers',[],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
  AssertNotNull('Method proc type',Method1.ProcType);
  AssertEquals('No arguments',0,Method1.ProcType.Args.Count)
end;

procedure TTestClassType.TestClassMethodSimple;
begin
  AddMember('Class Procedure DoSomething');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertEquals('1 class procedure',TPasClassProcedure,members[0].ClassType);
  AssertEquals('Default visibility',visDefault,Members[0].Visibility);
  AssertMemberName('DoSomething');
  AssertEquals('No modifiers',[],TPasClassProcedure(Members[0]).Modifiers);
  AssertEquals('Default calling convention',ccDefault, TPasClassProcedure(Members[0]).ProcType.CallingConvention);
  AssertNotNull('Method proc type',TPasClassProcedure(Members[0]).ProcType);
  AssertEquals('No arguments',0,TPasClassProcedure(Members[0]).ProcType.Args.Count)
end;

procedure TTestClassType.TestFunctionMethodSimple;
begin
  AddMember('Function DoSomething : integer');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertEquals('Default visibility',visDefault,FunctionMethod1.Visibility);
  AssertNotNull('Have method',Member1);
  AssertMemberName('DoSomething');
  AssertEquals('No modifiers',[],functionMethod1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, functionMethod1.ProcType.CallingConvention);
  AssertNotNull('Method proc type',functionMethod1.ProcType);
  AssertEquals('No arguments',0,functionMethod1.ProcType.Args.Count)
end;

procedure TTestClassType.TestClassFunctionMethodSimple;
begin
  AddMember('Class Function DoSomething : integer');
  ParseClass;
  AssertEquals('1 members',1,TheClass.members.Count);
  AssertEquals('1 class procedure',TPasClassFunction,members[0].ClassType);
  AssertEquals('Default visibility',visDefault,Members[0].Visibility);
  AssertMemberName('DoSomething');
  AssertEquals('No modifiers',[],TPasClassFunction(members[0]).Modifiers);
  AssertEquals('Default calling convention',ccDefault, TPasClassFunction(members[0]).ProcType.CallingConvention);
  AssertNotNull('Method proc type',TPasClassFunction(members[0]).ProcType);
  AssertEquals('No arguments',0,TPasClassFunction(members[0]).ProcType.Args.Count)
end;

procedure TTestClassType.DefaultMethod;

begin
  if TheClass.members.Count<1 then
    Fail('No members for method');
  AssertNotNull('Have method',Method1);
  AssertNotNull('Method proc type',Method1.ProcType);
    AssertMemberName('DoSomething');
  AssertEquals('1 argument',1,Method1.ProcType.Args.Count) ;
  AssertEquals('Argument name','A',TPasVariable(Method1.ProcType.Args[0]).Name);
end;

procedure TTestClassType.TestMethodOneArg;
begin
  AddMember('Procedure DoSomething(A : Integer)');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('No modifiers',[],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodVirtual;
begin
  AddMember('Procedure DoSomething(A : Integer) virtual');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Virtual modifiers',[pmVirtual],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodVirtualSemicolon;
begin
  AddMember('Procedure DoSomething(A : Integer); virtual');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Virtual modifiers',[pmVirtual],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodVirtualAbstract;
begin
  AddMember('Procedure DoSomething(A : Integer) virtual abstract');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Virtual, abstract modifiers',[pmVirtual,pmAbstract],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;


procedure TTestClassType.TestMethodOverride;
begin
  AddMember('Procedure DoSomething(A : Integer) override');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Override modifiers',[pmoverride],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodReintroduce;
begin
  AddMember('Procedure DoSomething(A : Integer) ReIntroduce');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Reintroduce modifiers',[pmReintroduce],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodDynamic;
begin
  AddMember('Procedure DoSomething(A : Integer) dynamic');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Dynamic modifiers',[pmDynamic],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodInline;
begin
  AddMember('Procedure DoSomething(A : Integer) inline');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Inline modifiers',[pmInline],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodVisibility;
begin
  StartVisibility(visPublic);
  AddMember('Procedure DoSomething(A : Integer)');
  ParseClass;
  DefaultMethod;
  AssertEquals('Public visibility',visPublic,Method1.Visibility);
  AssertEquals('No modifiers',[],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodHint;
begin
  AddMember('Procedure DoSomething(A : Integer) deprecated');
  ParseClass;
  DefaultMethod;
  HaveHint(hDeprecated,Member1.Hints);
  HaveHint(hDeprecated,Method1.ProcType.Hints);
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('No modifiers',[],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.TestMethodVirtualHint;
begin
  AddMember('Procedure DoSomething(A : Integer) virtual; deprecated');
  ParseClass;
  DefaultMethod;
  HaveHint(hDeprecated,Member1.Hints);
  HaveHint(hDeprecated,Method1.ProcType.Hints);
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('virtual modifiers',[pmVirtual],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
end;

procedure TTestClassType.Test2Methods;
begin
  AddMember('Procedure DoSomething(A : Integer) virtual');
  AddMember('Procedure DoSomethingElse');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Virtual modifiers',[pmVirtual],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
  AssertEquals('Default visibility',visDefault,Members[1].Visibility);
  AssertEquals('Default visibility',TPasProcedure,Members[1].ClassType);
  AssertEquals('Virtual modifiers',[],TPasProcedure(Members[1]).Modifiers);
  AssertEquals('Default calling convention',ccDefault, TPasProcedure(Members[1]).ProcType.CallingConvention);
end;

procedure TTestClassType.Test2MethodsDifferentVisibility;
begin
  AddMember('Procedure DoSomething(A : Integer) virtual');
  StartVisibility(visPublic);
  AddMember('Procedure DoSomethingElse');
  ParseClass;
  DefaultMethod;
  AssertEquals('Default visibility',visDefault,Method1.Visibility);
  AssertEquals('Virtual modifiers',[pmVirtual],Method1.Modifiers);
  AssertEquals('Default calling convention',ccDefault, Method1.ProcType.CallingConvention);
  AssertEquals('2 Public visibility',visPublic,Members[1].Visibility);
  AssertEquals('2 Default visibility',TPasProcedure,Members[1].ClassType);
  AssertEquals('2 No modifiers',[],TPasProcedure(Members[1]).Modifiers);
  AssertEquals('2 Default calling convention',ccDefault, TPasProcedure(Members[1]).ProcType.CallingConvention);

end;

procedure TTestClassType.TestPropertyRedeclare;
begin
  StartVisibility(visPublished);
  AddMember('Property Something');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','','','','',0,False,False);
  AssertNull('No type',Property1.VarType);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyReadOnly;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Read FSomething');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyReadWrite;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Read FSomething Write FSomething');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','FSomething','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyWriteOnly;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Write FSomething');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','','FSomething','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyDefault;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Read FSomething Write FSomething default 1');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','FSomething','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertExpression('Default expression',Property1.DefaultExpr,pekNumber,'1');
  Assertequals('Default value','1',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyNoDefault;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Read FSomething Write FSomething nodefault');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','FSomething','','',0,False,True);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyIndex;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Index 2 Read GetF Write SetF');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','GetF','SetF','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertExpression('Index expression',Property1.IndexExpr,pekNumber,'2');
  Assertequals('index','2',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyStored;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Read GetF Write SetF Stored CheckStored');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','GetF','SetF','CheckStored','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyStoredFalse;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : integer Read GetF Write SetF Stored False');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','GetF','SetF','False','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyFullyQualifiedType;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : unita.typeb Read FSomething');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','unita.typeb',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyArrayReadOnly;
Var
  A : TPasArgument;
begin
  StartVisibility(visPublished);
  AddMember('Property Somethings[AIndex : Integer] : integer Read GetF');
  ParseClass;
  AssertProperty(Property1,visPublished,'Somethings','GetF','','','',1,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertEquals('Argument class',TPasArgument,TObject(Property1.Args[0]).ClassType);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
  // Argument
  A:=TPasArgument(Property1.Args[0]);
  AssertEquals('Argument name','AIndex',A.Name);
  AssertNotNull('Argument class', A.ArgType);
  AssertEquals('Argument class type',TPasUnresolvedTypeRef,A.ArgType.ClassType);
  AssertEquals('Argument class type name','Integer',A.ArgType.Name);
end;

procedure TTestClassType.TestPropertyArrayReadWrite;
Var
  A : TPasArgument;
begin
  StartVisibility(visPublished);
  AddMember('Property Somethings[AIndex : Integer] : integer Read GetF Write SetF');
  ParseClass;
  AssertProperty(Property1,visPublished,'Somethings','GetF','SetF','','',1,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
  // Argument
  AssertEquals('Argument class',TPasArgument,TObject(Property1.Args[0]).ClassType);
  A:=TPasArgument(Property1.Args[0]);
  AssertEquals('Argument name','AIndex',A.Name);
  AssertNotNull('Argument class', A.ArgType);
  AssertEquals('Argument class type',TPasUnresolvedTypeRef,A.ArgType.ClassType);
  AssertEquals('Argument class type name','Integer',A.ArgType.Name);
end;

procedure TTestClassType.TestPropertyArrayReadOnlyDefault;

Var
  A : TPasArgument;
begin
  StartVisibility(visPublished);
  AddMember('Property Somethings[AIndex : Integer] : integer Read GetF; default');
  ParseClass;
  AssertProperty(Property1,visPublished,'Somethings','GetF','','','',1,True,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
  // Argument
  AssertEquals('Argument class',TPasArgument,TObject(Property1.Args[0]).ClassType);
  A:=TPasArgument(Property1.Args[0]);
  AssertEquals('Argument name','AIndex',A.Name);
  AssertNotNull('Argument class', A.ArgType);
  AssertEquals('Argument class type',TPasUnresolvedTypeRef,A.ArgType.ClassType);
  AssertEquals('Argument class type name','Integer',A.ArgType.Name);
end;

procedure TTestClassType.TestPropertyArrayReadWriteDefault;
Var
  A : TPasArgument;
begin
  StartVisibility(visPublished);
  AddMember('Property Somethings[AIndex : Integer] : integer Read GetF Write SetF; default');
  ParseClass;
  AssertProperty(Property1,visPublished,'Somethings','GetF','SetF','','',1,True,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
  // Argument
  AssertEquals('Argument class',TPasArgument,TObject(Property1.Args[0]).ClassType);
  A:=TPasArgument(Property1.Args[0]);
  AssertEquals('Argument name','AIndex',A.Name);
  AssertNotNull('Argument class', A.ArgType);
  AssertEquals('Argument class type',TPasUnresolvedTypeRef,A.ArgType.ClassType);
  AssertEquals('Argument class type name','Integer',A.ArgType.Name);
end;

procedure TTestClassType.TestPropertyArrayMultiDimReadOnly;
Var
  A : TPasArgument;
begin
  StartVisibility(visPublished);
  AddMember('Property Somethings[ACol : Integer,ARow : Integer] : integer Read GetF; default');
  ParseClass;
  AssertProperty(Property1,visPublished,'Somethings','GetF','','','',2,True,False);
  AssertEquals('Published property',vispublished,Property1.Visibility);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  AssertNull('No Index expression',Property1.IndexExpr);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Default expression',Property1.DefaultExpr);
  Assertequals('No Default value','',Property1.DefaultValue);
  // Argument 1
  AssertEquals('Argument 1 class',TPasArgument,TObject(Property1.Args[0]).ClassType);
  A:=TPasArgument(Property1.Args[0]);
  AssertEquals('Argument 1name','ACol',A.Name);
  AssertNotNull('Argument  1class', A.ArgType);
  AssertEquals('Argument 1 class type',TPasUnresolvedTypeRef,A.ArgType.ClassType);
  AssertEquals('Argument 1 class type name','Integer',A.ArgType.Name);
  // Argument 2
  AssertEquals('Argument 2 class',TPasArgument,TObject(Property1.Args[1]).ClassType);
  A:=TPasArgument(Property1.Args[1]);
  AssertEquals('Argument 2 name','ARow',A.Name);
  AssertNotNull('Argument 2 class', A.ArgType);
  AssertEquals('Argument 2 class type',TPasUnresolvedTypeRef,A.ArgType.ClassType);
  AssertEquals('Argument 2 class type name','Integer',A.ArgType.Name);
end;

procedure TTestClassType.TestPropertyImplements;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : AInterface Read FSomething Implements ISomeInterface');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','','','ISomeInterface',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','AInterface',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No default expression',Property1.DefaultExpr);
  Assertequals('Default value','',Property1.DefaultValue);

end;

procedure TTestClassType.TestPropertyImplementsFullyQualifiedName;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : AInterface Read FSomething Implements UnitB.ISomeInterface');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FSomething','','','UnitB.ISomeInterface',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','AInterface',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No default expression',Property1.DefaultExpr);
  Assertequals('Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyReadFromRecordField;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : Integer Read FPoint.X');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FPoint.X','','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No default expression',Property1.DefaultExpr);
  Assertequals('Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestPropertyReadWriteFromRecordField;
begin
  StartVisibility(visPublished);
  AddMember('Property Something : Integer Read FPoint.X Write FPoint.X');
  ParseClass;
  AssertProperty(Property1,visPublished,'Something','FPoint.X','FPoint.X','','',0,False,False);
  AssertNotNull('Have type',Property1.VarType);
  AssertEquals('Property type class type',TPasUnresolvedTypeRef,Property1.vartype.ClassType);
  AssertEquals('Property type name','Integer',Property1.vartype.name);
  Assertequals('No index','',Property1.IndexValue);
  AssertNull('No Index expression',Property1.IndexExpr);
  AssertNull('No default expression',Property1.DefaultExpr);
  Assertequals('Default value','',Property1.DefaultValue);
end;

procedure TTestClassType.TestLocalSimpleType;
begin
  StartVisibility(visPublic);
  FDecl.add('Type');
  AddMember('TDirection = (left,right)');
  AddMember('Procedure Something');
  ParseClass;
  AssertEquals('Local Enumeration type',TPasEnumType, Type1.ClassType);
  AssertEquals('Visibility is correct',VisPublic, Type1.Visibility);
  AssertEquals('Type name','TDirection', Type1.Name);
  AssertSame('Type parent is class',TheClass, Type1.Parent);
  AssertNotNull('Member 2 is procedure',Method2);
  AssertEquals('method name','Something', Method2.Name);
end;

procedure TTestClassType.TestLocalSimpleTypes;
begin
  StartVisibility(visPublic);
  FDecl.add('Type');
  AddMember('TDirection = (left,right)');
  AddMember('TVerticalDirection = (up,down)');
  AddMember('Procedure Something');
  ParseClass;
  AssertEquals('Local Enumeration type',TPasEnumType, Type1.ClassType);
  AssertEquals('Visibility is correct',VisPublic, Type1.Visibility);
  AssertEquals('Type name','TDirection', Type1.Name);
  AssertSame('Type parent is class',TheClass, Type1.Parent);
  AssertEquals('Local Enumeration type',TPasEnumType, Type2.ClassType);
  AssertEquals('Visibility is correct',VisPublic, Type2.Visibility);
  AssertEquals('Type name','TVerticalDirection', Type2.Name);
  AssertSame('Type parent is class',TheClass, Type2.Parent);
  AssertNotNull('Member 2 is procedure',Method3);
  AssertEquals('method name','Something', Method3.Name);
end;

initialization

  RegisterTest(TTestClassType);
end.

