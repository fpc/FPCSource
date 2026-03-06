unit TCGenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, testregistry, pscanner, tctypeparser;

Type

  { TTestGenerics - for resolver see unit tcresolvegenerics }

  TTestGenerics = Class(TBaseTestTypeParser)
  Published
    // generic types
    Procedure TestObjectGenerics;
    Procedure TestRecordGenerics;
    Procedure TestArrayGenerics;
    Procedure TestArrayGenericsDelphi;
    Procedure TestProcTypeGenerics;
    Procedure TestDeclarationDelphi;
    Procedure TestDeclarationFPC;
    Procedure TestDeclarationFPCNoSpaces;
    Procedure TestMethodImplementation;

    // generic constraints
    Procedure TestGenericConstraint;
    Procedure TestGenericInterfaceConstraint;
    Procedure TestDeclarationConstraint;

    // specialize type
    Procedure TestSpecializationDelphi;
    Procedure TestDeclarationDelphiSpecialize;
    Procedure TestInlineSpecializationInArgument;
    Procedure TestSpecializeNested;
    Procedure TestInlineSpecializeInStatement;
    Procedure TestInlineSpecializeInStatementDelphi;

    // generic functions
    Procedure TestGenericFunction_Program;
    Procedure TestGenericFunction_Unit;

    // generic method
    Procedure TestGenericMethod_Program;
    Procedure TestGenericMethod_OverloadDelphi;

    // const generic parameters
    Procedure TestConstGeneric_Basic;
    Procedure TestConstGeneric_MultiParam;
    Procedure TestConstGeneric_GroupConst;
    Procedure TestConstGeneric_Boolean;
    Procedure TestConstGeneric_Delphi;
    Procedure TestConstGeneric_GetDeclaration;
    Procedure TestConstGeneric_SpecializeExpr;
    Procedure TestConstGeneric_SpecializeString;
    Procedure TestConstGeneric_SpecializeBool;
    Procedure TestConstGeneric_SpecializeMixed;
    Procedure TestConstGeneric_SpecializeNeg;
    Procedure TestConstGeneric_SpecializeSet;

    // packed generic types (bug #38134)
    Procedure TestPackedGenericObject;
    Procedure TestPackedGenericObjectDelphi;
    Procedure TestPackedGenericRecord;
    Procedure TestPackedGenericRecordDelphi;
    procedure TestComplexSpecialization;
  end;

implementation

procedure TTestGenerics.TestObjectGenerics;
begin
  Add([
    'Type',
    'Generic TSomeClass<T> = Object',
    '  b : T;',
    'end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestRecordGenerics;
begin
  Add([
    'Type',
    '  Generic TSome<T> = Record',
    '    b : T;',
    '  end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestArrayGenerics;
begin
  Add([
    'Type',
    '  Generic TSome<T> = array of T;',
    '  Generic TStatic<R,T> = array[R] of T;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestArrayGenericsDelphi;
begin
  Add([
    '{$mode delphi}',
    'Type',
    '  TSome<T> = array of T;',
    '  TStatic<R,T> = array[R] of T;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestProcTypeGenerics;
begin
  Add([
    'Type',
    '  Generic TSome<T> = procedure(v: T);',
    '  Generic TFunc<R,T> = function(b: R): T;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestDeclarationDelphi;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches ;
  Source.Add('Type');
  Source.Add('  TSomeClass<T,T2> = Class(TObject)');
  Source.Add('    b : T;');
  Source.Add('    b2 : T2;');
  Source.Add('    FItems: ^TArray<T>;');
  Source.Add('  type');
  Source.Add('    TDictionaryEnumerator = TDictionary<T, TEmptyRecord>.TKeyEnumerator;');
  Source.Add('  end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);
end;

procedure TTestGenerics.TestDeclarationFPC;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches;
  Source.Add('Type');
  Source.Add('  TSomeClass<T;T2> = Class(TObject)');
  Source.Add('    b : T;');
  Source.Add('    b2 : T2;');
  Source.Add('  end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);
end;

procedure TTestGenerics.TestDeclarationFPCNoSpaces;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches;
  Source.Add('Type');
  Source.Add('  TSomeClass<T;T2>=Class(TObject)');
  Source.Add('    b : T;');
  Source.Add('    b2 : T2;');
  Source.Add('  end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);
end;

procedure TTestGenerics.TestMethodImplementation;
begin
  With source do
    begin
    Add('unit afile;');
    Add('{$MODE DELPHI}');
    Add('interface');
    Add('type');
    Add('  TTest<T> =  object');
    Add('    procedure foo(v:T);');
    Add('    procedure bar<Y>(v:T);');
    Add('  type');
    Add('    TSub = class');
    Add('      procedure DoIt<Y>(v:T);');
    Add('    end;');
    Add('  end;');
    Add('implementation');
    Add('procedure TTest<T>.foo;');
    Add('begin');
    Add('end;');
    Add('procedure TTest<T>.bar<Y>;');
    Add('begin');
    Add('end;');
    Add('procedure TTest<T>.TSub.DoIt<Y>;');
    Add('begin');
    Add('end;');
    end;
  ParseModule;
end;

procedure TTestGenerics.TestGenericConstraint;
begin
  Add([
    'Type',
    'Generic TSomeClass<T: TObject> = class',
    '  b : T;',
    'end;',
    'Generic TBird<T: class> = class',
    '  c : specialize TBird<T>;',
    'end;',
    'Generic TEagle<T: record> = class',
    'end;',
    'Generic TEagle<T: constructor> = class',
    'end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestGenericInterfaceConstraint;
begin
  Add([
    'Type',
    'TIntfA = interface end;',
    'TIntfB = interface end;',
    'TBird = class(TInterfacedObject,TIntfA,TIntfB) end;',
    'Generic TAnt<T: TIntfA, TIntfB> = class',
    '  b: T;',
    '  c: specialize TAnt<T>;',
    'end;',
    'Generic TFly<T: TIntfA, TIntfB; S> = class',
    '  b: S;',
    '  c: specialize TFly<T>;',
    'end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestDeclarationConstraint;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches ;
  Source.Add('Type');
  Source.Add('  TSomeClass<T: T2> = Class(TObject)');
  Source.Add('    b : T;');
  Source.Add('  end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('1 template types',1,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertEquals('Type constraint is recorded','T2',TPasGenericTemplateType(T.GenericTemplateTypes[0]).TypeConstraint);
end;

procedure TTestGenerics.TestSpecializationDelphi;
begin
  Add('{$mode delphi}');
  ParseType('TFPGList<integer>',TPasSpecializeType,'');
end;

procedure TTestGenerics.TestDeclarationDelphiSpecialize;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches ;
  Source.Add('Type');
  Source.Add('  TSomeClass<T,T2> = Class(TSomeGeneric<Integer,Integer>)');
  Source.Add('    b : T;');
  Source.Add('    b2 : T2;');
  Source.Add('  end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertEquals('Name is correct','TSomeClass',T.Name);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);
end;

procedure TTestGenerics.TestInlineSpecializationInArgument;
begin
  With source do
    begin
    Add('unit afile;');
    Add('{$MODE DELPHI}');
    Add('interface');
    Add('type');
    Add('  TFoo=class');
    Add('    procedure foo(var Node:TSomeGeneric<TBoundingBox>;const index:Integer);');
    Add('  end;');
    Add('implementation');
    end;
  ParseModule;
end;

procedure TTestGenerics.TestSpecializeNested;
begin
  Add([
    'Type',
    '  generic TSomeClass<A,B> = class(specialize TOther<A,specialize TAnother<B>>) end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestInlineSpecializeInStatement;
begin
  Add([
  '{$mode objfpc}',
  'begin',
  '  vec:=specialize TVector<double>.create;',
  '  t:=specialize a<b>;',
  //'  t:=specialize a<b.specialize c<d,e.f>>;',
  //'  t:=a.specialize b<c>;',
  '  t:=specialize a<b>.c;',
  '']);
  ParseModule;
end;

procedure TTestGenerics.TestInlineSpecializeInStatementDelphi;
begin
  Add([
  '{$mode delphi}',
  'begin',
  '  vec:=TVector<double>.create;',
  '  b:=a<b;',
  '  t:=a<b.c<d,e.f>>;',
  '  t:=a.b<c>;',
  '  t:=a<b>.c;',
  // forbidden:'  t:=a<b<c>.d>;',
  '']);
  ParseModule;
end;

procedure TTestGenerics.TestGenericFunction_Program;
begin
  Add([
  'generic function IfThen<T>(val:boolean;const iftrue:T; const iffalse:T) :T; inline; overload;',
  'begin',
  'end;',
  'begin',
  '  specialize IfThen<word>(true,2,3);',
  '']);
  ParseModule;
end;

procedure TTestGenerics.TestGenericFunction_Unit;
begin
  Add([
  'unit afile;',
  'interface',
  'generic function Get<T>(val: T) :T;',
  'implementation',
  'generic function Get<T>(val: T) :T;',
  'begin',
  'end;',
  'initialization',
  '  specialize GetIt<word>(2);',
  '']);
  ParseModule;
end;

procedure TTestGenerics.TestGenericMethod_Program;
begin
  Add([
  '{$mode objfpc}',
  'type',
  '  TObject = class',
  '    generic function Get<T>(val: T) :T;',
  '  type TBird = word;',
  '  generic procedure Fly<T>;',
  '  const C = 1;',
  '  generic procedure Run<T>;',
  '  end;',
  'generic function TObject.Get<T>(val: T) :T;',
  'begin',
  'end;',
  'begin',
  '  TObject.specialize GetIt<word>(2);',
  '']);
  ParseModule;
end;

procedure TTestGenerics.TestGenericMethod_OverloadDelphi;
begin
  Add([
  '{$mode delphi}',
  'type',
  '  TObject = class',
  '    procedure Fly<S>; overload;',
  '    procedure Fly<T>(val: T); overload;',
  '  end;',
  'procedure TObject.Fly<S>;',
  'begin',
  'end;',
  'procedure TObject.Fly<T>(val: word);',
  'begin',
  'end;',
  'var o : TObject;',
  'begin',
  '  o.Fly<word>;',
  '  o.Fly<word>();',
  '  o.Fly<longint>(3);',
  '  with o do begin',
  '    Fly<word>;',
  '    Fly<word>();',
  '    Fly<longint>(13);',
  '  end;',
  '']);
  ParseModule;
end;

procedure TTestGenerics.TestConstGeneric_Basic;
var
  GT: TPasGenericTemplateType;
begin
  Add([
    'Type',
    'Generic TTest<const U: integer> = record',
    '  end;',
    '']);
  ParseDeclarations;
  AssertEquals('One class',1,Declarations.Classes.Count);
  AssertNotNull('have generic templates',TPasRecordType(Declarations.Classes[0]).GenericTemplateTypes);
  AssertEquals('1 template type',1,TPasRecordType(Declarations.Classes[0]).GenericTemplateTypes.Count);
  GT:=TPasGenericTemplateType(TPasRecordType(Declarations.Classes[0]).GenericTemplateTypes[0]);
  AssertEquals('Name is U','U',GT.Name);
  AssertEquals('IsConst',True,GT.IsConst);
  AssertEquals('Has constraint',1,Length(GT.Constraints));
end;

procedure TTestGenerics.TestConstGeneric_MultiParam;
var
  GT0, GT1: TPasGenericTemplateType;
  R: TPasRecordType;
begin
  Add([
    'Type',
    'Generic TTest<T; const U: integer> = record',
    '  end;',
    '']);
  ParseDeclarations;
  R:=TPasRecordType(Declarations.Classes[0]);
  AssertEquals('2 template types',2,R.GenericTemplateTypes.Count);
  GT0:=TPasGenericTemplateType(R.GenericTemplateTypes[0]);
  GT1:=TPasGenericTemplateType(R.GenericTemplateTypes[1]);
  AssertEquals('T not const',False,GT0.IsConst);
  AssertEquals('U is const',True,GT1.IsConst);
end;

procedure TTestGenerics.TestConstGeneric_GroupConst;
var
  R: TPasRecordType;
  I: Integer;
begin
  Add([
    'Type',
    'Generic TTest<T1,T2; const U1,U2: integer> = record',
    '  end;',
    '']);
  ParseDeclarations;
  R:=TPasRecordType(Declarations.Classes[0]);
  AssertEquals('4 template types',4,R.GenericTemplateTypes.Count);
  for I:=0 to 1 do
    AssertEquals('T'+IntToStr(I+1)+' not const',False,
      TPasGenericTemplateType(R.GenericTemplateTypes[I]).IsConst);
  for I:=2 to 3 do
    AssertEquals('U'+IntToStr(I-1)+' is const',True,
      TPasGenericTemplateType(R.GenericTemplateTypes[I]).IsConst);
end;

procedure TTestGenerics.TestConstGeneric_Boolean;
var
  GT: TPasGenericTemplateType;
begin
  Add([
    'Type',
    'Generic TTest<const U: boolean> = record',
    '  end;',
    '']);
  ParseDeclarations;
  GT:=TPasGenericTemplateType(TPasRecordType(Declarations.Classes[0]).GenericTemplateTypes[0]);
  AssertEquals('IsConst',True,GT.IsConst);
end;

procedure TTestGenerics.TestConstGeneric_Delphi;
var
  T: TPasClassType;
  GT: TPasGenericTemplateType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches;
  Add([
    'Type',
    '  TTest<const N: LongInt> = class',
    '  end;',
    '']);
  ParseDeclarations;
  AssertEquals('One class',1,Declarations.Classes.Count);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('1 template type',1,T.GenericTemplateTypes.Count);
  GT:=TPasGenericTemplateType(T.GenericTemplateTypes[0]);
  AssertEquals('Name is N','N',GT.Name);
  AssertEquals('IsConst',True,GT.IsConst);
end;

procedure TTestGenerics.TestConstGeneric_GetDeclaration;
var
  GT: TPasGenericTemplateType;
  S: String;
begin
  Add([
    'Type',
    'Generic TTest<const U: integer> = record',
    '  end;',
    '']);
  ParseDeclarations;
  GT:=TPasGenericTemplateType(TPasRecordType(Declarations.Classes[0]).GenericTemplateTypes[0]);
  S:=GT.GetDeclaration(True);
  AssertTrue('GetDeclaration starts with const',Copy(S,1,6)='const ');
end;

procedure TTestGenerics.TestConstGeneric_SpecializeExpr;
var
  S: TPasSpecializeType;
begin
  Add([
    'Type',
    '  A = specialize TTest<100>;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  AssertTrue('Is specialize',TObject(Declarations.Types[0]) is TPasSpecializeType);
  S:=TPasSpecializeType(Declarations.Types[0]);
  AssertEquals('1 param',1,S.Params.Count);
  AssertTrue('Param is TPrimitiveExpr',TObject(S.Params[0]) is TPrimitiveExpr);
  AssertEquals('Value','100',TPrimitiveExpr(S.Params[0]).Value);
end;

procedure TTestGenerics.TestConstGeneric_SpecializeString;
var
  S: TPasSpecializeType;
begin
  Add([
    'Type',
    '  A = specialize TTest<''hello''>;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  S:=TPasSpecializeType(Declarations.Types[0]);
  AssertEquals('1 param',1,S.Params.Count);
  AssertTrue('Param is TPrimitiveExpr',TObject(S.Params[0]) is TPrimitiveExpr);
end;

procedure TTestGenerics.TestConstGeneric_SpecializeBool;
var
  S: TPasSpecializeType;
begin
  Add([
    'Type',
    '  A = specialize TTest<true>;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  S:=TPasSpecializeType(Declarations.Types[0]);
  AssertEquals('1 param',1,S.Params.Count);
  AssertTrue('Param is TPasExpr',TObject(S.Params[0]) is TPasExpr);
end;

procedure TTestGenerics.TestConstGeneric_SpecializeMixed;
var
  S: TPasSpecializeType;
begin
  Add([
    'Type',
    '  A = specialize TTest<integer, 42>;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  S:=TPasSpecializeType(Declarations.Types[0]);
  AssertEquals('2 params',2,S.Params.Count);
  AssertTrue('Param 0 is TPasType',TObject(S.Params[0]) is TPasType);
  AssertTrue('Param 1 is TPrimitiveExpr',TObject(S.Params[1]) is TPrimitiveExpr);
end;

procedure TTestGenerics.TestConstGeneric_SpecializeNeg;
var
  S: TPasSpecializeType;
begin
  Add([
    'Type',
    '  A = specialize TTest<-1>;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  S:=TPasSpecializeType(Declarations.Types[0]);
  AssertEquals('1 param',1,S.Params.Count);
  AssertTrue('Param is TUnaryExpr',TObject(S.Params[0]) is TUnaryExpr);
end;

procedure TTestGenerics.TestConstGeneric_SpecializeSet;
var
  S: TPasSpecializeType;
begin
  Add([
    'Type',
    '  A = specialize TTest<[1, 2]>;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  S:=TPasSpecializeType(Declarations.Types[0]);
  AssertEquals('1 param',1,S.Params.Count);
  AssertTrue('Param is TPasExpr',TObject(S.Params[0]) is TPasExpr);
end;

procedure TTestGenerics.TestPackedGenericObject;
var
  T: TPasClassType;
begin
  Add([
    'Type',
    'Generic TSomeClass<T> = packed object',
    '  b : T;',
    'end;',
    '']);
  ParseDeclarations;
  AssertEquals('One class',1,Declarations.Classes.Count);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertEquals('Object kind',Ord(okObject),Ord(T.ObjKind));
  AssertEquals('Is packed',Ord(pmPacked),Ord(T.PackMode));
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('1 template type',1,T.GenericTemplateTypes.Count);
end;

procedure TTestGenerics.TestPackedGenericObjectDelphi;
var
  T: TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches;
  Add([
    'Type',
    'TSomeClass<T>=packed object',
    '  b : T;',
    'end;',
    '']);
  ParseDeclarations;
  AssertEquals('One class',1,Declarations.Classes.Count);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertEquals('Object kind',Ord(okObject),Ord(T.ObjKind));
  AssertEquals('Is packed',Ord(pmPacked),Ord(T.PackMode));
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('1 template type',1,T.GenericTemplateTypes.Count);
end;

procedure TTestGenerics.TestPackedGenericRecord;
var
  R: TPasRecordType;
begin
  Add([
    'Type',
    'Generic TSomeRecord<T> = packed record',
    '  b : T;',
    'end;',
    '']);
  ParseDeclarations;
  AssertEquals('One class',1,Declarations.Classes.Count);
  R:=TPasRecordType(Declarations.Classes[0]);
  AssertEquals('Is packed',Ord(pmPacked),Ord(R.PackMode));
  AssertNotNull('have generic templates',R.GenericTemplateTypes);
  AssertEquals('1 template type',1,R.GenericTemplateTypes.Count);
end;

procedure TTestGenerics.TestPackedGenericRecordDelphi;
var
  R: TPasRecordType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches;
  Add([
    'Type',
    'TSomeRecord<T>=packed record',
    '  b : T;',
    'end;',
    '']);
  ParseDeclarations;
  AssertEquals('One type',1,Declarations.Types.Count);
  R:=TPasRecordType(Declarations.Types[0]);
  AssertEquals('Is packed',Ord(pmPacked),Ord(R.PackMode));
  AssertNotNull('have generic templates',R.GenericTemplateTypes);
  AssertEquals('1 template type',1,R.GenericTemplateTypes.Count);
end;

procedure TTestGenerics.TestComplexSpecialization;
begin
  Add([
  'type',
  '  generic TGenLazShiftListFixedSize<T; _TConfT: TLazListConfigFixSize> = object(',
  '    specialize TGenLazShiftList<',
  '      specialize TTypeToPointerGeneric<T>.PT, _TConfT.specialize _TC<T, _TConfT>',
  '    >',
  '  )',
  '  end;',
  '']);
  ParseDeclarations;
end;

initialization
  RegisterTest(TTestGenerics);
end.

