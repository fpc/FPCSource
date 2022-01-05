unit tctsparser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcparser, jsscanner, jsbase, jstree, jstoken;

Type

  { TTestTypeScriptParser }

  TTestTypeScriptParser = Class(TTestBaseJSParser)
  private
  Public
    Procedure StartTS(aSource : String);
    procedure CheckMembers(M : TJSSourceElements; aVars,aFunctions,aStatements,aClasses,aModules,aNamespaces,aTypes : Integer);
  Published
    Procedure TestDeclareModule;
    Procedure TestDeclareModuleIdentifier;
    Procedure TestDeclareNamespace;
    Procedure TestDeclareGlobal;
    Procedure TestDeclareUniqueConst;
    Procedure TestDeclareFunction;
    Procedure TestDeclareFunctionOptionalArg;
    Procedure TestDeclareFunctionSpreadArg;
    Procedure TestDeclareFunctionLiteralArg;
    Procedure TestDeclareTypeAlias;
    Procedure TestDeclareTypeAliasDotted;
    Procedure TestDeclareTypeTypeQuery;
    Procedure TestDeclareTypeUnion;
    Procedure TestDeclareTypeUnionConsts;
    Procedure TestDeclareTypeUnionAllowEmpty;
    Procedure TestDeclareUnionTuple;
    Procedure TestDeclareUnionIntersectionType;
    Procedure TestDeclareTypeTuple;
    Procedure TestDeclareTypeArray;
    Procedure TestDeclareTypeArrayOfUnionType;
    Procedure TestDeclareTypeArrayOfFunctionType;
    Procedure TestDeclareTypeArrayGeneric;
    Procedure TestDeclareTypeArrayGenericConst;
    Procedure TestDeclareTypeArrayKeyOf;
    Procedure TestDeclareTypeGeneric;
    Procedure TestDeclareTypeGenericGeneric;
    Procedure TestDeclareTypeGenericNested;
    procedure TestDeclareTypeGenericNestedTwice;
    Procedure TestDeclareTypeGenericAssigned;
    Procedure TestDeclareTypeGenericConstSubtype;
    Procedure TestDeclareTypeObject;
    Procedure TestDeclareTypeObjectLineBreak;
    Procedure TestDeclareTypeObjectPropRef;
    procedure TestDeclareTypeObjectIndexSignature;
    Procedure TestDeclareTypeArrowFunction;
    Procedure TestDeclareTypeArrowFunctionShort;
    Procedure TestDeclareTypeArrowFunctionTwoArgs;
    Procedure TestDeclareTypeArrowFunctionArgsCommaEnd;
    procedure TestDeclareTypeGenericConstraint;
    procedure TestDeclareTypeTypeImport;
    procedure TestDeclareTypeTypeOfImport;
    procedure TestDeclareTypeTypeOfDefault;
    procedure TestDeclareTypeReadonly;
    procedure TestDeclareArgTypeInferred;
    Procedure TestDeclareEnum;
    Procedure TestDeclareEnumKeywordElement;
    Procedure TestDeclareConstEnum;
    Procedure TestDeclareConstEnumAssigned;
    procedure TestDeclareInterfaceEmpty;
    procedure TestDeclareAmbientInterfaceEmpty;
    procedure TestDeclareInterfaceExtendsEmpty;
    procedure TestDeclareInterfaceExtendsTwoEmpty;
    procedure TestDeclareInterfaceProperty;
    procedure TestDeclareInterfaceOptionalProperty;
    procedure TestDeclareInterfaceUntypedProperty;
    procedure TestDeclareInterfaceOptionalUntypedProperty;
    procedure TestDeclareInterfaceMethod;
    procedure TestDeclareInterfaceMethodNoReturn;
    procedure TestDeclareInterfaceCallable;
    procedure TestDeclareInterfaceMethodThisReturn;
    procedure TestDeclateInterfaceGenericProperty;
    procedure TestDeclateInterfaceGeneric;
    procedure TestDeclareInterfaceExtendsGeneric;
    Procedure TestDeclareInterfaceHidden;
    procedure TestDeclareClassEmpty;
    procedure TestDeclareAbstractClassEmpty;
    procedure TestDeclareClassIndexSignature;
    procedure TestDeclareClassIndexSignatureIn;
    procedure TestDeclareClassExtendsGeneric;
    procedure TestDeclareClassGeneric;
    procedure TestDeclareClassStaticFunction;
    procedure TestDeclareClassStaticFunctionUntypedArg;
    procedure TestDeclareClassStaticReadonlyProperty;
    procedure TestDeclareClassReadonlyProperty;
    procedure TestDeclareClassGenericFunctionProperty;
    procedure TestDeclareClassImplements;
    procedure TestDeclareClassExtendsFunctionCall;
    Procedure TestDeclareClassStaticReadOnly;
    procedure TestDeclareInterfaceIndexSignature;
    procedure TestDeclareInterfaceFixedStringProperty;
    procedure TestDeclareInterfaceFixedBooleanProperty;
    procedure TestDeclareInterfaceFixedUnionProperty;
    procedure TestDeclareInterfaceDestructuredFunctionProperty;
    procedure TestDeclareInterfaceNoSemicolon;
    procedure TestDeclareConstructorSignature;
    procedure TestDeclareFunctionDestructuredParam;
    procedure TestDeclareFunctionIs;
    procedure TestDeclareFunctionThisParam;
    procedure TestExportNamespacedClass;
    procedure TestExportAbstractClass;
    procedure TestExportClassConstructor;
    procedure TestExportInterface;
    procedure TestExportInterfaceDefault;
    procedure TestExportInterfaceIndexSignature;
    procedure TestExportInterfaceIndexSignatureOptional;
    procedure TestExportImportStatement;
    procedure TestExportAssignStatement;
    procedure TestExportAsNamespace;
    procedure TestExportNamespaceClass;
    procedure TestExportDefaultFunction;
    procedure TestExportFunction;
    procedure TestExportTypeArrowFunctionArgsComma;
    procedure TestDeclareExportInterfaceDestructuredFunctionProperty;
    procedure TestExportEnum;
    procedure TestExportObjectUnion;
    Procedure TestExportNoSemicolon;
    Procedure TestExportAsKeyWord;
    procedure TestNamespaceInterfaceFUnction;
  end;

implementation

{ TTestTypeScriptParser }

procedure TTestTypeScriptParser.StartTS(aSource: String);
begin
  CreateParser(aSource,ecma2021,True);
end;

procedure TTestTypeScriptParser.CheckMembers(M : TJSSourceElements; aVars, aFunctions, aStatements, aClasses, aModules, aNamespaces,aTypes: Integer);
begin
  AssertEquals('functions count',aFunctions,M.Functions.Count);
  AssertEquals('vars count',aVars,M.Vars.Count);
  AssertEquals('Statements count',aStatements,M.Statements.Count);
  AssertEquals('classes count',aClasses,M.Classes.Count);
  AssertEquals('namespaces count',aNamespaces,M.NameSpaces.Count);
  AssertEquals('Modules count',aModules,M.Modules.Count);
  AssertEquals('Types count',aTypes,M.Modules.Count);
end;

procedure TTestTypeScriptParser.TestDeclareModule;

Var
  M : TJSModuleDeclaration;

begin
  StartTS('declare module "a" { }');
  M:=GetFirstModule;
  AssertEquals('Name','a',M.Name);
  CheckMembers(M.Members,0,0,0,0,0,0,0);
end;

procedure TTestTypeScriptParser.TestDeclareModuleIdentifier;

Var
  M : TJSModuleDeclaration;

begin
  StartTS('declare module a { }');
  M:=GetFirstModule;
  AssertEquals('Name','a',M.Name);
  CheckMembers(M.Members,0,0,0,0,0,0,0);
end;

procedure TTestTypeScriptParser.TestDeclareNamespace;
Var
  N : TJSNamespaceDeclaration;

begin
  StartTS('declare namespace a { }');
  N:=GetFirstNamespace;
  AssertEquals('Name','a',N.Name);
  CheckMembers(N.Members,0,0,0,0,0,0,0);
end;

procedure TTestTypeScriptParser.TestDeclareGlobal;
Var
  N : TJSNamespaceDeclaration;

begin
  StartTS('declare global { }');
  N:=GetFirstNamespace;
  AssertEquals('Name','',N.Name);
  AssertEquals('Global',True,N.IsGlobal);
  CheckMembers(N.Members,0,0,0,0,0,0,0);
end;

procedure TTestTypeScriptParser.TestDeclareUniqueConst;

Var
  V : TJSVarDeclaration;

begin
  StartTS('const A : unique symbol');
  V:=TJSVarDeclaration(CheckClass('Var',TJSVarDeclaration,GetFirstVar));
  AssertEquals('Unique ',True,V.IsUnique);
end;

procedure TTestTypeScriptParser.TestDeclareFunction;

Var
  F : TJSFunctionDeclarationStatement;

begin
  StartTS('declare function a (b : number) : string ;');
  F:=GetFirstFunction;
  AssertEquals('Name','a',F.AFunction.Name);
  AssertNull('No body',F.AFunction.Body);
  AssertNotNull('Have params',F.AFunction.TypedParams);
  AssertEquals('Param Count',1,F.AFunction.TypedParams.Count);
  AssertEquals('Param 1 name','b',F.AFunction.TypedParams.Names[0]);
  AssertEquals('Param 1 optional',False,F.AFunction.TypedParams.Params[0].IsOptional);
end;

procedure TTestTypeScriptParser.TestDeclareFunctionOptionalArg;
Var
  F : TJSFunctionDeclarationStatement;

begin
  StartTS('declare function a (b ? : number) : string ;');
  F:=GetFirstFunction;
  AssertEquals('Name','a',F.AFunction.Name);
  AssertNull('No body',F.AFunction.Body);
  AssertNotNull('Have params',F.AFunction.TypedParams);
  AssertEquals('Param Count',1,F.AFunction.TypedParams.Count);
  AssertEquals('Param 1 name','b',F.AFunction.TypedParams.Names[0]);
  AssertEquals('Param 1 optional',True,F.AFunction.TypedParams.Params[0].IsOptional);
end;

procedure TTestTypeScriptParser.TestDeclareFunctionSpreadArg;
Var
  F : TJSFunctionDeclarationStatement;

begin
  StartTS('declare function a (...b ? : number) : string ;');
  F:=GetFirstFunction;
  AssertEquals('Name','a',F.AFunction.Name);
  AssertNull('No body',F.AFunction.Body);
  AssertNotNull('Have params',F.AFunction.TypedParams);
  AssertEquals('Param Count',1,F.AFunction.TypedParams.Count);
  AssertEquals('Param 1 name','b',F.AFunction.TypedParams.Names[0]);
  AssertEquals('Param 1 optional',True,F.AFunction.TypedParams.Params[0].IsOptional);
  AssertEquals('Param 1 spread',True,F.AFunction.TypedParams.Params[0].IsSpread);
end;

procedure TTestTypeScriptParser.TestDeclareFunctionLiteralArg;
Var
  F : TJSFunctionDeclarationStatement;
  R : TJSFixedValueReference;

begin
  StartTS('declare function a (b : ''number'') : string ;');
  F:=GetFirstFunction;
  AssertEquals('Name','a',F.AFunction.Name);
  AssertNull('No body',F.AFunction.Body);
  AssertNotNull('Have params',F.AFunction.TypedParams);
  AssertEquals('Param Count',1,F.AFunction.TypedParams.Count);
  AssertEquals('Param 1 name','b',F.AFunction.TypedParams.Names[0]);
  AssertEquals('Param 1 optional',False,F.AFunction.TypedParams.Params[0].IsOptional);
  AssertEquals('Param 1 spread',False,F.AFunction.TypedParams.Params[0].IsSpread);
  AssertNotNull('Param 1 type',F.AFunction.TypedParams.Types[0]);
  R:=TJSFixedValueReference(CheckClass('Param 1 type',TJSFixedValueReference,F.AFunction.TypedParams.Types[0]));
  AssertNotNull('Have Fixedvalue',R.FixedValue);
  AssertNotNull('Have Fixedvalue.Value',R.FixedValue.Value);
  AssertEquals('Fixed value','number',R.FixedValue.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareTypeAlias;

Var
  T : TJSTypeDeclaration;

begin
  StartTS('type a = b;');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTypeReference,T.TypeDef.ClassType);
  AssertEquals('Correct name def class','b',TJSTypeReference(T.TypeDef).Name);
  AssertEquals('Correct IsTypeOf',False,TJSTypeReference(T.TypeDef).IsTypeOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeAliasDotted;
Var
  T : TJSTypeDeclaration;

begin
  StartTS('type a = b.c;');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTypeReference,T.TypeDef.ClassType);
  AssertEquals('Correct name def class','b.c',TJSTypeReference(T.TypeDef).Name);
  AssertEquals('Correct isTypeOf',False,TJSTypeReference(T.TypeDef).IsTypeOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeTypeQuery;
Var
  T : TJSTypeDeclaration;

begin
  StartTS('type a = typeof b;');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTypeReference,T.TypeDef.ClassType);
  AssertEquals('Correct name def class','b',TJSTypeReference(T.TypeDef).Name);
  AssertEquals('Correct name def class',True,TJSTypeReference(T.TypeDef).IsTypeOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeUnion;

Var
  T : TJSTypeDeclaration;
  U : TJSUnionTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = string | number');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSUnionTypeDef,T.TypeDef.ClassType);
  U:=TJSUnionTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2,U.TypeCount);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[0].ClassType);
  R:=U.Types[0] as TJSTypeReference;
  AssertEquals('Name type 0','string',R.Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[1].ClassType);
  R:=U.Types[1] as TJSTypeReference;
  AssertEquals('Name type 1','number',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeUnionConsts;

Var
  T : TJSTypeDeclaration;
  U : TJSUnionTypeDef;
  R : TJSFixedValueReference;

begin
  StartTS('type a = "string" | "number"');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSUnionTypeDef,T.TypeDef.ClassType);
  U:=TJSUnionTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2,U.TypeCount);
  AssertEquals('Correct type of element 0',TJSFixedValueReference, U.Types[0].ClassType);
  R:=U.Types[0] as TJSFixedValueReference;
  AssertEquals('Name type 0','string',R.FixedValue.Value.AsString);
  AssertEquals('Correct type of element 0',TJSFixedValueReference, U.Types[1].ClassType);
  R:=U.Types[1] as TJSFixedValueReference;
  AssertEquals('Name type 1','number',R.FixedValue.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareTypeUnionAllowEmpty;
Var
  T : TJSTypeDeclaration;
  U : TJSUnionTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = | string | number');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSUnionTypeDef,T.TypeDef.ClassType);
  U:=TJSUnionTypeDef(T.TypeDef);
  AssertEquals('Allow empty ',True,U.AllowEmpty);
  AssertEquals('Correct number of elements',2,U.TypeCount);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[0].ClassType);
  R:=U.Types[0] as TJSTypeReference;
  AssertEquals('Name type 0','string',R.Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[1].ClassType);
  R:=U.Types[1] as TJSTypeReference;
  AssertEquals('Name type 1','number',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareUnionTuple;
Var
  T : TJSTypeDeclaration;
begin
  StartTS('declare type Color = string | [number, number, number, number] | ColorObject;');
  T:=GetFirstType;
  AssertEquals('Name','Color',T.Name);
end;

procedure TTestTypeScriptParser.TestDeclareUnionIntersectionType;
Var
  T : TJSTypeDeclaration;
  U : TJSUnionTypeDef;

begin
  StartTs('declare type MyType = number | (string | number) ;');
  T:=GetFirstType;
  AssertEquals('Name','MyType',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSUnionTypeDef,T.TypeDef.ClassType);
  U:=TJSUnionTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2,U.TypeCount);
end;

procedure TTestTypeScriptParser.TestDeclareTypeTuple;

Var
  T : TJSTypeDeclaration;
  U : TJSTupleTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = [string , number]');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTupleTypeDef,T.TypeDef.ClassType);
  U:=TJSTupleTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2,U.TypeCount);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[0].ClassType);
  R:=U.Types[0] as TJSTypeReference;
  AssertEquals('Name type 0','string',R.Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[1].ClassType);
  R:=U.Types[1] as TJSTypeReference;
  AssertEquals('Name type 1','number',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArray;
Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = string[]');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSArrayTypeDef,T.TypeDef.ClassType);
  A:=TJSArrayTypeDef(T.TypeDef);
  AssertNotNull('have base type',A.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, A.BaseType.ClassType);
  R:=A.BaseType as TJSTypeReference;
  AssertEquals('Name base type','string',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrayOfUnionType;
Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  U : TJSUnionTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = (string | number)[]');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  A:=TJSArrayTypeDef(CheckClass(T.TypeDef,TJSArrayTypeDef,'Type def class'));
  U:=TJSUnionTypeDef(CheckClass(A.BaseType,TJSUnionTypeDef,'base type Union class '));
  AssertEquals('Union Count',2,U.TypeCount);
  R:=TJSTypeReference(CheckClass(U.Types[0],TJSTypeReference,'Union class element 0'));
  AssertEquals('Name base type','string',R.Name);
  R:=TJSTypeReference(CheckClass(U.Types[1],TJSTypeReference,'Union class element 1'));
  AssertEquals('Name base type','number',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrayOfFunctionType;
Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  F : TJSArrowFunctionTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = (() => string)[]');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  A:=TJSArrayTypeDef(CheckClass(T.TypeDef,TJSArrayTypeDef,'Type def class'));
  F:=TJSArrowFunctionTypeDef(CheckClass(A.BaseType,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',F.aFunction);
  R:=TJSTypeReference(CheckClass(F.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name','string',R.Name);
  AssertEquals('function argument count',0,F.aFunction.TypedParams.Count);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrayKeyOf;


Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = string[keyof B]');
   T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSArrayTypeDef,T.TypeDef.ClassType);
  A:=TJSArrayTypeDef(T.TypeDef);
  AssertNotNull('have base type',A.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, A.BaseType.ClassType);
  R:=A.BaseType as TJSTypeReference;
  AssertEquals('Name base type','string',R.Name);
  AssertEquals('Correct type of base type',TJSTypeReference, A.IndexType.ClassType);
  R:=A.IndexType as TJSTypeReference;
  AssertEquals('Name base type','B',R.Name);
  AssertEquals('Keyof type',True,R.IsKeyOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrayGeneric;

Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = Array<string>');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSArrayTypeDef,T.TypeDef.ClassType);
  A:=TJSArrayTypeDef(T.TypeDef);
  AssertNotNull('have base type',A.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, A.BaseType.ClassType);
  R:=A.BaseType as TJSTypeReference;
  AssertEquals('Name base type','string',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrayGenericConst;
Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  U : TJSUnionTypeDef;

begin
  StartTS('type a = Array<"params" | "query" | "headers" | "body">;');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSArrayTypeDef,T.TypeDef.ClassType);
  A:=TJSArrayTypeDef(T.TypeDef);
  AssertNotNull('have base type',A.BaseType);
  AssertEquals('Correct type of base type',TJSUnionTypeDef, A.BaseType.ClassType);
  U:=A.BaseType as TJSUnionTypeDef;
  AssertEquals('Element count',4,U.TypeCount);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGeneric;
Var
  T : TJSTypeDeclaration;
  G : TJSGenericTypeRef;
  R : TJSTypeReference;

begin
  StartTS('type a = Shell<string>');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSGenericTypeRef,T.TypeDef.ClassType);
  G:=TJSGenericTypeRef(T.TypeDef);
  AssertNotNull('have base type',G.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, G.BaseType.ClassType);
  R:=G.BaseType as TJSTypeReference;
  AssertEquals('Name base type','Shell',R.Name);
  AssertEquals('Correct number of parameters',1, G.ParamTypeCount);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, G.ParamTypes[0].Classtype);
  R:=G.ParamTypes[0] as TJSTypeReference;
  AssertEquals('Name base type','string',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericGeneric;
Var
  T : TJSTypeDeclaration;
  G : TJSTupleTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type Pair<T> = [T,T]');
  T:=GetFirstType;
  AssertEquals('Name','Pair',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct number of parameters',1, T.TypeParams.Count);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, T.TypeParams[0].Node.Classtype);
  R:=T.TypeParams[0].Node as TJSTypeReference;
  AssertEquals('Name base type','T',R.Name);
  AssertEquals('Correct type def class',TJSTupleTypeDef,T.TypeDef.ClassType);
  G:=TJSTupleTypeDef(T.TypeDef);
  AssertEquals('Correct number of parameters',2, G.TypeCount);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, G.Types[0].Classtype);
  R:=G.Types[0] as TJSTypeReference;
  AssertEquals('Name base type','T',R.Name);
  AssertEquals('Correct type of parameter 1',TJSTypeReference, G.Types[1].Classtype);
  R:=G.Types[1] as TJSTypeReference;
  AssertEquals('Name base type','T',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericNested;
Var
  T : TJSTypeDeclaration;
  G : TJSGenericTypeRef;
  R : TJSTypeReference;

begin
  StartTS('type a = A<Q,B<C>>;');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSGenericTypeRef,T.TypeDef.ClassType);
  G:=TJSGenericTypeRef(T.TypeDef);
  AssertNotNull('have base type',G.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, G.BaseType.ClassType);
  R:=G.BaseType as TJSTypeReference;
  AssertEquals('Name base type','A',R.Name);
  AssertEquals('Correct number of parameters',2, G.ParamTypeCount);
  // Arg 1
  AssertEquals('Correct type of parameter 0',TJSTypeReference, G.ParamTypes[0].Classtype);
  R:=G.ParamTypes[0] as TJSTypeReference;
  AssertEquals('Name base type','Q',R.Name);
  // Arg 2
  AssertEquals('Correct type of base type',TJSGenericTypeRef, G.ParamTypes[1].Classtype);
  G:=G.ParamTypes[1] as TJSGenericTypeRef;
  AssertNotNull('have base type',G.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, G.BaseType.ClassType);
  R:=G.BaseType as TJSTypeReference;
  AssertEquals('Name base type','B',R.Name);
  AssertEquals('Correct number of parameters',1, G.ParamTypeCount);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, G.ParamTypes[0].Classtype);
  R:=G.ParamTypes[0] as TJSTypeReference;
  AssertEquals('Name base type','C',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericNestedTwice;

Var
  T : TJSTypeDeclaration;

begin
  StartTS('type a = A<Q<B<C<D>>>>;');
  T:=GetFirstType;
  AssertEquals('Name','a',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSGenericTypeRef,T.TypeDef.ClassType);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericAssigned;
Var
  T : TJSTypeDeclaration;
  R : TJSTypeReference;
  P : TJSNamedParamTypeDef;
begin
  StartTS('type TA<TEvent = any, TResult = any> = B;');
  T:=TJSTypeDeclaration(CheckClass('First type',TJSTypeDeclaration,GetFirstType));
  R:=TJSTypeReference(CheckClass('Correct type',TJSTypeReference,T.TypeDef));
  AssertEquals('Name','B',R.Name);
  AssertEquals('Type param count',2,T.TypeParams.Count);
  // Param 1
  P:=TJSNamedParamTypeDef(CheckClass('Param 1,',TJSNamedParamTypeDef,T.TypeParams.JSElements[0]));
  R:=TJSTypeReference(CheckClass('Correct type',TJSTypeReference,P.ParamName));
  AssertEquals('Name','TEvent',R.Name);
  R:=TJSTypeReference(CheckClass('Correct type',TJSTypeReference,P.ParamType));
  AssertEquals('Name','any',R.Name);
  // Param 2
  P:=TJSNamedParamTypeDef(CheckClass('Param 2,',TJSNamedParamTypeDef,T.TypeParams.JSElements[1]));
  R:=TJSTypeReference(CheckClass('Correct type',TJSTypeReference,P.ParamName));
  AssertEquals('Name','TResult',R.Name);
  R:=TJSTypeReference(CheckClass('Correct type',TJSTypeReference,P.ParamType));
  AssertEquals('Name','any',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericConstSubtype;

Var
  T : TJSTypeDeclaration;
  G : TJSGenericTypeRef;
  R : TJSTypeReference;
begin
  StartTS('type StatusCodeMap = Record<string, "trace" | "debug" | "info" | "warn" | "error">');
  T:=TJSTypeDeclaration(CheckClass('First type',TJSTypeDeclaration,GetFirstType));
  AssertEquals('Name','StatusCodeMap',T.Name);
  G:=TJSGenericTypeRef(CheckClass('Correct type',TJSGenericTypeRef,T.TypeDef));
  R:=TJSTypeReference(CheckClass('Correct type',TJSTypeReference,G.BaseType));
  AssertEquals('Name','Record',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeObject;

Var
  T : TJSTypeDeclaration;
  O : TJSObjectTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type TreeItem = {left : TreeItem, right : TreeItem};');
  T:=GetFirstType;
  AssertEquals('Name','TreeItem',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type of typedef',TJSObjectTypeDef, T.TypeDef.ClassType);
  O:=TJSObjectTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2, O.ElementCount);
  AssertEquals('Correct name of element 0','left', O.Elements[0].Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, O.Elements[0].ElementType.ClassType);
  R:=O.Elements[0].ElementType as TJSTypeReference;
  AssertEquals('Name base type','TreeItem',R.Name);
  AssertEquals('Correct name of element 0','right', O.Elements[1].Name);
  AssertEquals('Correct type of element 1',TJSTypeReference, O.Elements[1].ElementType.ClassType);
  R:=O.Elements[1].ElementType as TJSTypeReference;
  AssertEquals('Name base type','TreeItem',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeObjectLineBreak;
Var
  T : TJSTypeDeclaration;
  O : TJSObjectTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type TreeItem = {'#10+
          'left : TreeItem'#10+
          'right : TreeItem'#10+
          '};');
  T:=GetFirstType;
  AssertEquals('Name','TreeItem',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type of typedef',TJSObjectTypeDef, T.TypeDef.ClassType);
  O:=TJSObjectTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2, O.ElementCount);
  AssertEquals('Correct name of element 0','left', O.Elements[0].Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, O.Elements[0].ElementType.ClassType);
  R:=O.Elements[0].ElementType as TJSTypeReference;
  AssertEquals('Name base type','TreeItem',R.Name);
  AssertEquals('Correct name of element 0','right', O.Elements[1].Name);
  AssertEquals('Correct type of element 1',TJSTypeReference, O.Elements[1].ElementType.ClassType);
  R:=O.Elements[1].ElementType as TJSTypeReference;
  AssertEquals('Name base type','TreeItem',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeObjectIndexSignature;
Var
  T : TJSTypeDeclaration;
  O : TJSObjectTypeDef;
//  R : TJSTypeReference;
  SigDecl: TJSIndexSignatureDeclaration;

begin
  StartTS('type TreeItem = { [x: string]: never, [x: number]: never };');
  T:=GetFirstType;
  AssertEquals('Name','TreeItem',T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type of typedef',TJSObjectTypeDef, T.TypeDef.ClassType);
  O:=TJSObjectTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2, O.ElementCount);
  SigDecl:=TJSIndexSignatureDeclaration(CheckClass('Index signature',TJSIndexSignatureDeclaration,O.Elements[0]));
  AssertEquals('No name','',SigDecl.Name);
  AssertEquals('Index type','string',SigDecl.IndexType);
end;




procedure TTestTypeScriptParser.TestDeclareTypeArrowFunction;

Var
  T : TJSTypeDeclaration;
  A : TJSArrowFunctionTypeDef;
  P : TJSTypedParam;
  R : TJSTypeReference;

begin
  StartTS('type CallBack = (data : t) => void;');
  T:=GetFirstType;
  AssertEquals('Name','CallBack',T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name','void',R.Name);
  AssertEquals('function argument count',1,A.aFunction.TypedParams.Count);
  P:=A.aFunction.TypedParams.Params[0];
  AssertEquals('param name','data',P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param type'));
  AssertEquals('param type name','t',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrowFunctionShort;
Var
  T : TJSTypeDeclaration;
  A : TJSArrowFunctionTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type CallBack = () => void;');
  T:=GetFirstType;
  AssertEquals('Name','CallBack',T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name','void',R.Name);
  AssertEquals('function argument count',0,A.aFunction.TypedParams.Count);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrowFunctionTwoArgs;
Var
  T : TJSTypeDeclaration;
  A : TJSArrowFunctionTypeDef;
  P : TJSTypedParam;
  R : TJSTypeReference;

begin
  StartTS('type CallBack = (data : t, context: t2) => void;');
  T:=GetFirstType;
  AssertEquals('Name','CallBack',T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name','void',R.Name);
  AssertEquals('function argument count',2,A.aFunction.TypedParams.Count);
  P:=A.aFunction.TypedParams.Params[0];
  AssertEquals('param name','data',P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param 0 type'));
  AssertEquals('param type name','t',R.Name);
  P:=A.aFunction.TypedParams.Params[1];
  AssertEquals('param name','context',P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param 1 type'));
  AssertEquals('param type name','t2',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrowFunctionArgsCommaEnd;
Var
  T : TJSTypeDeclaration;
  A : TJSArrowFunctionTypeDef;
  P : TJSTypedParam;
  R : TJSTypeReference;
begin
  StartTS('type CallBack = ( '#10+
          '  reject: FN,'#10+
          ') => void;');
  T:=GetFirstType;
  AssertEquals('Name','CallBack',T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name','void',R.Name);
  AssertEquals('function argument count',1,A.aFunction.TypedParams.Count);
  P:=A.aFunction.TypedParams.Params[0];
  AssertEquals('param name','reject',P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param 0 type'));
  AssertEquals('param type name','FN',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeObjectPropRef;

Var
  aDecl : TJSTypeDeclaration;
  ObjType : TJSObjectTypeDef;
  PropType : TJSPropertyDeclaration;
  U : TJSUnionTypeDef;
  A : TJSArrayTypeDef;
  F : TJSFixedValueReference;

begin
  // rimraf.Options &
  StartTs('type Options =  {'#10 +
          '      glob?: rimraf.Options["glob"] | true;'#10 +
          '     disableGlob?: never;'#10 +
          '  };');
  aDecl:=TJSTypeDeclaration(CheckClass('First type',TJSTypeDeclaration,GetFirstType));
  ObjType:=TJSObjectTypeDef(CheckClass('a type',TJSObjectTypeDef,aDecl.TypeDef));
  PropType:=TJSPropertyDeclaration(CheckClass('First prop',TJSPropertyDeclaration,ObjType.Elements[0]));
  U:=TJSUnionTypeDef(CheckClass('Prop type',TJSUnionTypeDef,PropType.ElementType));
  A:=TJSArrayTypeDef(CheckClass('First el',TJSArrayTypeDef,U.Types[0]));
  F:=TJSFixedValueReference(CheckClass('Array Index',TJSFixedValueReference,A.IndexType));
  AssertEquals('value','glob',F.FixedValue.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareEnum;

Var
  E : TJSEnumDeclaration;
  ET : TJSEnumTypeDef;

begin
  StartTS('enum Colors {red,blue,green};');
  E:=GetFirstEnum;
  AssertEquals('Name','Colors',E.Name);
  ET:=TJSEnumTypeDef(CheckClass(E.EnumDef,TJSEnumTypeDef,'Have enum definition'));
  AssertEquals('Member count',3,ET.NameCount);
  AssertEquals('Member 0','red',ET.Names[0]);
  AssertEquals('Member 1','blue',ET.Names[1]);
  AssertEquals('Member 2','green',ET.Names[2]);
end;

procedure TTestTypeScriptParser.TestDeclareEnumKeywordElement;
Var
  E : TJSEnumDeclaration;
  ET : TJSEnumTypeDef;

begin
  StartTS('enum Colors {delete,new,green};');
  E:=GetFirstEnum;
  AssertEquals('Name','Colors',E.Name);
  ET:=TJSEnumTypeDef(CheckClass(E.EnumDef,TJSEnumTypeDef,'Have enum definition'));
  AssertEquals('Member count',3,ET.NameCount);
  AssertEquals('Member 0','delete',ET.Names[0]);
  AssertEquals('Member 1','new',ET.Names[1]);
  AssertEquals('Member 2','green',ET.Names[2]);

end;

procedure TTestTypeScriptParser.TestDeclareConstEnum;
Var
  Stat : TJSEnumStatement;
  E : TJSEnumDeclaration;
  ET : TJSEnumTypeDef;

begin
  StartTS('const enum Colors {red,blue,green};');
  Stat:=TJSEnumStatement(CheckClass('First statement',TJSEnumStatement,GetFirstStatement));
  E:=TJSEnumDeclaration(CheckClass('Decl',TJSEnumDeclaration,Stat.EnumDecl));
  AssertEquals('Name','Colors',E.Name);
  ET:=TJSEnumTypeDef(CheckClass(E.EnumDef,TJSEnumTypeDef,'Have enum definition'));
  AssertEquals('Is const',True,ET.IsConst);
  AssertEquals('Member count',3,ET.NameCount);
  AssertEquals('Member 0','red',ET.Names[0]);
  AssertEquals('Member 1','blue',ET.Names[1]);
  AssertEquals('Member 2','green',ET.Names[2]);
end;

procedure TTestTypeScriptParser.TestDeclareConstEnumAssigned;
Var
  Stat : TJSEnumStatement;
  E : TJSEnumDeclaration;
  ET : TJSEnumTypeDef;
  Lit : TJSLiteral;

begin
  StartTS('const enum Colors {red = 1,blue = 2,green};');
  Stat:=TJSEnumStatement(CheckClass('First statement',TJSEnumStatement,GetFirstStatement));
  E:=TJSEnumDeclaration(CheckClass('Decl',TJSEnumDeclaration,Stat.EnumDecl));
  AssertEquals('Name','Colors',E.Name);
  ET:=TJSEnumTypeDef(CheckClass(E.EnumDef,TJSEnumTypeDef,'Have enum definition'));
  AssertEquals('Is const',True,ET.IsConst);
  AssertEquals('Member count',3,ET.NameCount);
  AssertEquals('Member 0','red',ET.Names[0]);
  lit:=TJSLiteral(CheckClass('Member 0 assigned',TJSLiteral,ET.Elements[0].Value));
  AssertEquals('Lit value',1,Lit.Value.AsNumber);
  AssertEquals('Member 1','blue',ET.Names[1]);
  lit:=TJSLiteral(CheckClass('Member 1 assigned',TJSLiteral,ET.Elements[1].Value));
  AssertEquals('Lit value',2,Lit.Value.AsNumber);
  AssertEquals('Member 2','green',ET.Names[2]);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceEmpty;

Var
  Decl : TJSInterfaceDeclaration;

begin
  StartTS('interface Empty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Empty',Decl.Name);
  AssertNull('No Extends',Decl.Extends);
//  AssertEquals('Extends count',0,Decl.Extends.Count);
  AssertEquals('Members count',0,Decl.Values.Count);
end;

procedure TTestTypeScriptParser.TestDeclareAmbientInterfaceEmpty;

Var
  Decl : TJSInterfaceDeclaration;

begin
  StartTS('declare interface Empty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertEquals('First type node is type declaration',True,GetInterfaces.Nodes[0].IsAmbient);
  AssertNotNull('Have declaration');
  AssertEquals('Name','Empty',Decl.Name);
  AssertNull('No Extends',Decl.Extends);
//  AssertEquals('Extends count',0,Decl.Extends.Count);
  AssertEquals('Members count',0,Decl.Values.Count);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceExtendsEmpty;
Var
  Decl : TJSInterfaceDeclaration;
  Lit : TJSLiteral;

begin
  StartTS('interface Empty extends ParentEmpty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Empty',Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
  AssertEquals('Extends count',1,Decl.Extends.Count);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[0].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name','ParentEmpty',Lit.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceExtendsTwoEmpty;
Var
  Decl : TJSInterfaceDeclaration;
  Lit : TJSLiteral;

begin
  StartTS('interface Empty extends ParentEmpty,MoreEmpty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Empty',Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
  AssertEquals('Extends count',2,Decl.Extends.Count);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[0].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name','ParentEmpty',Lit.Value.AsString);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[1].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name','MoreEmpty',Lit.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceProperty;

Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface Friend { name: string; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Friend',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name','name',Prop.Name);
  AssertEquals('Property optional',koDefault,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name','string',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceOptionalProperty;
Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface Friend { name ? : string; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Friend',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name','name',Prop.Name);
  AssertEquals('Property optional',koOptional,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name','string',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceUntypedProperty;

Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface Friend { name ; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Friend',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name','name',Prop.Name);
  AssertEquals('Property optional',koDefault,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name','any',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceOptionalUntypedProperty;

Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface Friend { name ? ; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Friend',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name','name',Prop.Name);
  AssertEquals('Property optional',koOptional,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name','any',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceMethod;

Var
  Decl : TJSInterfaceDeclaration;
  M : TJSMethodDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface JQuery { text (content: string) : string; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name','text',M.Name);
  AssertEquals('Method optional',koDefault,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name','string',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceMethodNoReturn;
Var
  Decl : TJSInterfaceDeclaration;
  M : TJSMethodDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface JQuery { text (content: string) ; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name','text',M.Name);
  AssertEquals('Method optional',koDefault,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name','any',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceCallable;
Var
  Decl : TJSInterfaceDeclaration;
  M : TJSMethodDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface JQuery { (query : string) : JQuery; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name','',M.Name);
  AssertEquals('Method optional',koDefault,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name','JQuery',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceMethodThisReturn;
Var
  Decl : TJSInterfaceDeclaration;
  M : TJSMethodDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface JQuery { text (content: string) : this; };');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name','text',M.Name);
  AssertEquals('Method optional',koDefault,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name','this',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclateInterfaceGenericProperty;

Var
  Decl:TJSInterfaceDeclaration;
  Prop: TJSPropertyDeclaration;
  R: TJSTypeReference;
  U : TJSUnionTypeDef;
  G : TJSGenericTypeRef;

begin
  STartTS('export interface CalendarBaseProps { '+
  '    dayComponent?: React.Component<DayComponentProps> | React.SFC<DayComponentProps>; '+
  '    disabledByDefault?: boolean;'+
  '};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertEquals('Count elements',2,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name','dayComponent',Prop.Name);
  AssertEquals('Property optional',koOptional,Prop.Optional);
  U:=TJSUnionTypeDef(CheckClass('Property type',TJSUnionTypeDef,Prop.ElementType));
  G:=TJSGenericTypeRef(CheckClass('Union 1',TJSGenericTypeRef,U.Types[0]));
  R:=TJSTypeReference(CheckClass('Generic base type',TJSTypeReference,G.BaseType));
  AssertEquals('Generic 1 base type Name ','React.Component',R.Name);
  G:=TJSGenericTypeRef(CheckClass('Union 2',TJSGenericTypeRef,U.Types[1]));
  R:=TJSTypeReference(CheckClass('Generic base type',TJSTypeReference,G.BaseType));
  AssertEquals('Generic 2 base type Name ','React.SFC',R.Name);
end;

procedure TTestTypeScriptParser.TestDeclateInterfaceGeneric;
Var
  Decl : TJSInterfaceDeclaration;
  R : TJSTypeReference;
  Lit : TJSLiteral;

begin
  StartTS('interface Empty <TItem> extends ParentEmpty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name','Empty',Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
  AssertEquals('Type param count',1,Decl.TypeParams.Count);
  R:=TJSTypeReference(CheckClass('Type param 1',TJSTypeReference,Decl.TypeParams.Nodes[0].Node));
  AssertEquals('Type param Name','TItem',R.Name);
  AssertEquals('Extends count',1,Decl.Extends.Count);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[0].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name','ParentEmpty',Lit.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceExtendsGeneric;
Var
  Decl : TJSInterfaceDeclaration;
  R : TJSTypeReference;

begin
  StartTS('interface Empty <TItem> extends ParentEmpty<Titem> {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration',Decl);
  AssertEquals('Name','Empty',Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
  AssertEquals('Type param count',1,Decl.TypeParams.Count);
  R:=TJSTypeReference(CheckClass('Type param 1',TJSTypeReference,Decl.TypeParams.Nodes[0].Node));
  AssertEquals('Type param Name','TItem',R.Name);
  AssertEquals('Extends count',1,Decl.Extends.Count);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceHidden;
Var
  Decl : TJSInterfaceDeclaration;
begin
  StartTS('interface JQuery extends JQuerySlickInitials {'#10+
          '   hidden: ''mozHidden'' | ''webkitHidden'' | ''hidden'';'#10+
          '}');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration',Decl);
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
end;

procedure TTestTypeScriptParser.TestDeclareClassEmpty;
Var
  Decl : TJSClassDeclaration;

begin
  StartTS('declare class JQuery {};');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertNull('Members',Decl.Members);
end;

procedure TTestTypeScriptParser.TestDeclareAbstractClassEmpty;
Var
  Decl : TJSClassDeclaration;

begin
  StartTS('declare abstract class JQuery {};');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertNull('Members',Decl.Members);
  AssertTrue('Abstract',Decl.IsAbstract);
end;

procedure TTestTypeScriptParser.TestDeclareClassIndexSignature;
Var
  Decl : TJSClassDeclaration;
  SigDecl : TJSIndexSignatureDeclaration;

begin
  StartTS('class TDictionary { [a : string] : string; } ;');
  Decl:=TJSClassDeclaration(CheckClass('Interface',TJSClassDeclaration,GetFirstClass));
  AssertEquals('Interface name','TDictionary',Decl.Name);
  AssertEquals('One var',1,Decl.Members.Vars.Count);
  SigDecl:=TJSIndexSignatureDeclaration(CheckClass('Index signature',TJSIndexSignatureDeclaration,Decl.Members.Vars[0].Node));
  AssertEquals('No name','',SigDecl.Name);
  AssertEquals('Index type','string',SigDecl.IndexType);
end;

procedure TTestTypeScriptParser.TestDeclareClassIndexSignatureIn;
Var
  Decl : TJSClassDeclaration;
  SigDecl : TJSIndexSignatureDeclaration;
  IndexType : TJSTypeReference;

begin
  StartTS('class TDictionary { [a in B] : string; } ;');
  Decl:=TJSClassDeclaration(CheckClass('Interface',TJSClassDeclaration,GetFirstClass));
  AssertEquals('Interface name','TDictionary',Decl.Name);
  AssertEquals('One var',1,Decl.Members.Vars.Count);
  SigDecl:=TJSIndexSignatureDeclaration(CheckClass('Index signature',TJSIndexSignatureDeclaration,Decl.Members.Vars[0].Node));
  AssertEquals('No name','',SigDecl.Name);
  IndexType:=TJSTypeReference(CheckClass('Index type',TJSTypeReference,SigDecl.InIndexType));
  AssertEquals('Type ref name','B',IndexType.Name);
end;

procedure TTestTypeScriptParser.TestDeclareClassExtendsGeneric;
Var
  Decl : TJSAmbientClassDeclaration;
  Gen : TJSGenericTypeRef;
  Ref : TJSTypeReference;

begin
  StartTS('declare class JQuery extends MyClass<any> {};');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  Gen:=TJSGenericTypeRef(CheckClass('Extends class',TJSGenericTypeRef,Decl.Extends));
  Ref:=TJSTypeReference(CheckClass('Base class',TJSTypeReference,Gen.BaseType));
  AssertEquals('Name','MyClass',Ref.Name);
  AssertNull('Members',Decl.Members);
end;

procedure TTestTypeScriptParser.TestDeclareClassGeneric;
Var
  Decl : TJSAmbientClassDeclaration;
  Ref : TJSTypeReference;

begin
  StartTS('declare class JQuery<any> extends MyClass {};');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  Ref:=TJSTypeReference(CheckClass('Base class',TJSTypeReference,Decl.extends));
  AssertEquals('Name','MyClass',Ref.Name);
  AssertNotNull('Have type params',Decl.TypeParams);
  AssertEquals('Type Param count ',1,Decl.TypeParams.Count);
  Ref:=TJSTypeReference(CheckClass('Base class',TJSTypeReference,Decl.TypeParams.Nodes[0].Node));
  AssertEquals('Name','any',Ref.Name);
  AssertNull('Members',Decl.Members);
end;

procedure TTestTypeScriptParser.TestDeclareClassStaticFunction;
Var
  Decl : TJSClassDeclaration;
begin
  StartTS('declare class JQuery { static extends() : string; };');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertEquals('Function count',1,Decl.Members.Functions.Count);
end;

procedure TTestTypeScriptParser.TestDeclareClassStaticFunctionUntypedArg;
Var
  Decl : TJSClassDeclaration;
begin
  StartTS('declare class JQuery { private soso(a) ; };');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertEquals('Function count',1,Decl.Members.Functions.Count);
end;

procedure TTestTypeScriptParser.TestDeclareClassStaticReadonlyProperty;
Var
  Decl : TJSClassDeclaration;
  Prop : TJSPropertyDeclaration;

begin
  StartTS('declare class JQuery { static readonly a : string; };');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertEquals('Var count',1,Decl.Members.vars.Count);
  Prop:=TJSPropertyDeclaration(CheckClass('Class',TJSPropertyDeclaration,Decl.Members.vars.JSElements[0]));
  AssertTrue('Prop readonly',Prop.IsReadOnly);
  AssertTrue('Prop Static',Prop.IsStatic);
end;

procedure TTestTypeScriptParser.TestDeclareClassReadonlyProperty;
Var
  Decl : TJSClassDeclaration;
  Prop : TJSPropertyDeclaration;
begin
  StartTS('declare class JQuery { readonly a : string; };');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertEquals('Var count',1,Decl.Members.vars.Count);
  Prop:=TJSPropertyDeclaration(CheckClass('Class',TJSPropertyDeclaration,Decl.Members.vars.JSElements[0]));
  AssertTrue('Prop readonly',Prop.IsReadOnly);
  AssertFalse('Prop Static',Prop.IsStatic);
end;

procedure TTestTypeScriptParser.TestDeclareClassGenericFunctionProperty;
begin
  StartTS('declare class changeRequestInterceptor {'#10+
          ' getRequest: <T>(request: T, entity: Entity, index: number) => T;'#10+
          '}');
  GetFirstClass(True);
end;

procedure TTestTypeScriptParser.TestDeclareClassImplements;
Var
  Decl : TJSClassDeclaration;
  Prop : TJSPropertyDeclaration;
begin
  StartTS('declare class JQuery implements IQuery { readonly a : string; };');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertEquals('Var count',1,Decl.Members.vars.Count);
  Prop:=TJSPropertyDeclaration(CheckClass('Class',TJSPropertyDeclaration,Decl.Members.vars.JSElements[0]));
  AssertTrue('Prop readonly',Prop.IsReadOnly);
  AssertFalse('Prop Static',Prop.IsStatic);
end;

procedure TTestTypeScriptParser.TestDeclareClassExtendsFunctionCall;

Var
  Decl: TJSAmbientClassDeclaration;
  E : TJSTypeFuncCall;

begin
   StartTS('declare class TextArea extends Component.extend(TextSupport) {}');
   Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
   AssertNotNull('Have declaration',Decl);
   AssertEquals('Name','TextArea',Decl.Name);
   E:=TJSTypeFuncCall(CheckClass('Extends',TJSTypeFuncCall,Decl.Extends));
   AssertEquals('Function name','Component.extend',E.Name);
end;

procedure TTestTypeScriptParser.TestDeclareClassStaticReadOnly;

Var
  Decl: TJSAmbientClassDeclaration;
  E : TJSClassConstDeclaration;

begin
   StartTS('declare class A { static readonly B = 1 }');
   Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
   AssertNotNull('Have declaration',Decl);
   AssertEquals('Name','A',Decl.Name);
   AssertEquals('member count',1,Decl.ClassDef.ElementCount);
   AssertEquals('Member name','B',Decl.ClassDef.Elements[0].Name);
   E:=TJSClassConstDeclaration(CheckClass('Member class',TJSClassConstDeclaration,Decl.ClassDef.Elements[0]));
   AssertNotNull(E);
end;



procedure TTestTypeScriptParser.TestDeclareInterfaceIndexSignature;
Var
  Decl : TJSInterfaceDeclaration;
  SigDecl : TJSIndexSignatureDeclaration;

begin
  StartTS('interface TDictionary { [a : string] : string; } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','TDictionary',Decl.Name);
  AssertEquals('One var',1,Decl.ElementCount);
  SigDecl:=TJSIndexSignatureDeclaration(CheckClass('Index signature',TJSIndexSignatureDeclaration,Decl.Elements[0]));
  AssertEquals('No name','',SigDecl.Name);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceFixedStringProperty;
Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
begin
  StartTS('export interface Color { R : "red" } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
  AssertEquals('One member',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Element 0',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('member name','R',Prop.Name);
  AssertEquals('member fixed value','red',Prop.FixedStringValue);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceFixedBooleanProperty;
Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
  Fix : TJSFixedValueReference;

begin
  StartTS('export interface Color { R : false } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
  AssertEquals('One member',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Element 0',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('member name','R',Prop.Name);
  Fix:=TJSFixedValueReference(CheckClass('member fixed value',TJSFixedValueReference,Prop.ElementType));
  AssertNotNull('Value literal',Fix.FixedValue);

end;

procedure TTestTypeScriptParser.TestDeclareInterfaceFixedUnionProperty;

Var
  Decl : TJSInterfaceDeclaration;
  Prop : TJSPropertyDeclaration;
  U : TJSUnionTypeDef;
  Fix : TJSFixedValueReference;

begin
  StartTS('export interface Color { R : false | ''string'' } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
  AssertEquals('One member',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Element 0',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('member name','R',Prop.Name);
  U:=TJSUnionTypeDef(CheckClass('member fixed value',TJSUnionTypeDef,Prop.ElementType));
  AssertEquals('Member count',2,U.TypeCount);
  Fix:=TJSFixedValueReference(CheckClass('member fixed value',TJSFixedValueReference,U.Types[0]));
  AssertNotNull('Value literal',Fix.FixedValue);
  Fix:=TJSFixedValueReference(CheckClass('member fixed value',TJSFixedValueReference,U.Types[1]));
  AssertNotNull('Value literal',Fix.FixedValue);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceDestructuredFunctionProperty;
begin
  StartTs('interface I18nComponentProps {'#10+
          ' children: ({ i18n, i18nHash }: { i18n: I18n, i18nHash?: string }) => ReactNode;'#10+
          '}');
  AssertNotNull(GetFirstStatement);
end;

procedure TTestTypeScriptParser.TestDeclareExportInterfaceDestructuredFunctionProperty;
begin
  StartTs('export interface I18nComponentProps {'#10+
          ' children: ({ i18n, i18nHash }: { i18n: I18n, i18nHash?: string }) => ReactNode;'#10+
          '}');
  AssertNotNull(GetFirstStatement);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceNoSemicolon;
begin
  StartTs(' interface Conversions {'#10+
          '        ansi: (ansi: number) => string'#10+
          '        rgb: (r: number, g: number, b: number) => string'#10+
          '}');
  AssertNotNull(GetFirstStatement);
end;

procedure TTestTypeScriptParser.TestDeclareConstructorSignature;
Var
  Decl : TJSInterfaceDeclaration;
  meth : TJSMethodDeclaration;
begin
  StartTS('export interface Color { new (r: number, g:number, b : number) : Color; } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
  AssertEquals('One member',1,Decl.ElementCount);
  Meth:=TJSMethodDeclaration(CheckClass('Element 0',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('member name','new',Meth.Name);
end;

procedure TTestTypeScriptParser.TestDeclareFunctionDestructuredParam;
begin
  StartTS('declare function A({b,c,d} : E): F;');
  GetStatements;
end;

procedure TTestTypeScriptParser.TestDeclareFunctionIs;
begin
  StartTS('declare function hasOwnProperty(target: A): target is B ;');
  GetStatements;
end;

procedure TTestTypeScriptParser.TestDeclareFunctionThisParam;
begin
  StartTS('declare function hasOwnProperty(this: A): B ;');
  GetStatements;
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericConstraint;

Var
  Decl : TJSTypeDeclaration;

begin
  StartTs('type ValueMap<T extends BaseType, Datum> = { [key: string]: number | string | boolean | null | ValueFn<T, Datum, number | string | boolean | null> };');
  Decl:=GetFirstType;
  AssertNotNull(Decl);
end;

procedure TTestTypeScriptParser.TestDeclareTypeTypeImport;

Var
  Decl : TJSImportTypeRef;

begin
  StartTS('  type AppChannel = import(''main'').AppChannel;');
  Decl:=TJSImportTypeRef(CheckClass('First',TJSImportTypeRef,GetFirstType.TypeDef));
  AssertEquals('Name','AppChannel',Decl.Name);
  AssertEquals('File','main',Decl.fileName);
end;

procedure TTestTypeScriptParser.TestDeclareTypeTypeOfImport;

Var
  Decl : TJSImportTypeRef;

begin
  StartTS('  type AppChannel = typeof import(''main'').AppChannel;');
  Decl:=TJSImportTypeRef(CheckClass('First',TJSImportTypeRef,GetFirstType.TypeDef));
  AssertEquals('Name','AppChannel',Decl.Name);
  AssertEquals('File','main',Decl.fileName);
  AssertEquals('File',True,Decl.IsTypeOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeTypeOfDefault;
Var
  Decl : TJSImportTypeRef;

begin
  StartTS('  type AppChannel = typeof import(''main'').default;');
  Decl:=TJSImportTypeRef(CheckClass('First',TJSImportTypeRef,GetFirstType.TypeDef));
  AssertEquals('Name','default',Decl.Name);
  AssertEquals('File','main',Decl.fileName);
  AssertEquals('File',True,Decl.IsTypeOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeReadonly;
Var
  Decl : TJSFunctionStatement;
  Def : TJSFuncDef;
  R : TJSTypeReference;

begin
  StartTS('declare function markdownTable(table: readonly string): string;');
  Decl:=GetFirstFunction;
  AssertNotNull('Have decl',Decl.AFunction);
  Def:=Decl.AFunction;
  AssertEquals('Params',1,Def.TypedParams.Count);
  R:=TJSTypeReference(CheckClass('Arg type',TJSTypeReference,Def.TypedParams.Types[0]));
  AssertTrue('Read only',R.IsReadonly);
end;

procedure TTestTypeScriptParser.TestDeclareArgTypeInferred;
Var
  Decl : TJSTypeDeclaration;
begin
  StartTS('type A = (args: infer B) => C;');
  Decl:=GetFirstType;
  AssertNotNull(Decl);
end;

procedure TTestTypeScriptParser.TestExportNamespacedClass;

Var
  NS : TJSNamespaceDeclaration;
  Exp : TJSExportStatement;

begin
  StartTS('export namespace Collection {'#10+
  '    type EventType = string;'#10+
  '    class Event extends events.Event {'#10+
  '        constructor(type: EventType, opt_element?: any);'#10+
  '        element: any;'#10+
  '    }'#10+
  '}');
  Exp:=TJSExportStatement(CheckClass('Export',TJSExportStatement,GetFirstStatement));
  NS:=TJSNamespaceDeclaration(CheckClass('Namespace', TJSNamespaceDeclaration, Exp.Declaration));
  AssertEquals('NS name','Collection',NS.Name);
end;


procedure TTestTypeScriptParser.TestExportClassConstructor;
Var
  Decl : TJSClassDeclaration;

begin
  StartTS('export class JQuery { constructor (); };');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertNotNull('Members',Decl.Members);
  AssertEquals('members count',1,Decl.Members.Functions.Count);
  AssertFalse('Abstract',Decl.IsAbstract);
end;

procedure TTestTypeScriptParser.TestExportAbstractClass;
Var
  Decl : TJSAmbientClassDeclaration;

begin
  StartTS('export abstract class JQuery {};');
  Decl:=TJSAmbientClassDeclaration(CheckClass('Class',TJSAmbientClassDeclaration,GetFirstClass(True)));
  AssertNotNull('Have declaration');
  AssertEquals('Name','JQuery',Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertNull('Members',Decl.Members);
  AssertTrue('Abstract',Decl.IsAbstract);
end;

procedure TTestTypeScriptParser.TestExportInterface;
Var
  Decl : TJSInterfaceDeclaration;
begin
  StartTS('export interface Color { R,G,B : string; } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
end;

procedure TTestTypeScriptParser.TestExportInterfaceDefault;
Var
  Decl : TJSInterfaceDeclaration;
begin
  StartTS('export default  interface Color { R,G,B : string; } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
end;

procedure TTestTypeScriptParser.TestExportInterfaceIndexSignature;
Var
  Decl : TJSInterfaceDeclaration;
  SigDecl : TJSIndexSignatureDeclaration;
begin
  StartTS('export interface Color { level : string; [key : string] : any; } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
  AssertEquals('One var',2,Decl.ElementCount);
  SigDecl:=TJSIndexSignatureDeclaration(CheckClass('Index signature',TJSIndexSignatureDeclaration,Decl.Elements[1]));
  AssertEquals('No name','',SigDecl.Name);
end;

procedure TTestTypeScriptParser.TestExportInterfaceIndexSignatureOptional;
Var
  Decl : TJSInterfaceDeclaration;
  SigDecl : TJSIndexSignatureDeclaration;
begin
  StartTS('export interface Color { [key : string] ? : any; } ;');
  Decl:=TJSInterfaceDeclaration(CheckClass('Interface',TJSInterfaceDeclaration,GetFirstInterface));
  AssertEquals('Interface name','Color',Decl.Name);
  AssertEquals('One var',1,Decl.ElementCount);
  SigDecl:=TJSIndexSignatureDeclaration(CheckClass('Index signature',TJSIndexSignatureDeclaration,Decl.Elements[0]));
  AssertEquals('No name','',SigDecl.Name);
end;

procedure TTestTypeScriptParser.TestExportImportStatement;

Var
  expDecl : TJSExportStatement;
  Decl : TJSImportStatement;
  Call : TJSCallExpression;

begin
  StartTS('export import LocaleConfig = require("locale");');
  ExpDecl:=TJSExportStatement(CheckClass('import',TJSExportStatement,GetFirstStatement));
  Decl:=TJSImportStatement(CheckClass('import',TJSImportStatement,expDecl.Declaration));
  Call:=TJSCallExpression(CheckClass('Have import assign', TJSCallExpression, Decl.Expression));
  AssertNotNull('Have call expression',Call.Expr);
  CheckClass('Call function name',TJSPrimaryExpressionIdent,Call.Expr);
  AssertEquals('call function','require',TJSPrimaryExpressionIdent(Call.Expr).Name);
end;

procedure TTestTypeScriptParser.TestExportAssignStatement;

var
  Decl: TJSExportStatement;

begin
  StartTS('export = LocaleConfig;');
  Decl:=TJSExportStatement(CheckClass('export',TJSExportStatement,GetFirstStatement));
  AssertNotNull('Have assignment',Decl.Declaration);

end;

procedure TTestTypeScriptParser.TestExportAsNamespace;
var
  Decl: TJSExportStatement;

begin
  StartTS('export as namespace B;');
  Decl:=TJSExportStatement(CheckClass('export',TJSExportStatement,GetFirstStatement));
  AssertEquals('Namespace export','B',Decl.NameSpaceExport);
end;

procedure TTestTypeScriptParser.TestExportNamespaceClass;

var
  Decl: TJSNamespaceDeclaration;

begin
  StartTS('declare namespace g { export class Grid { collection(name?: string): mongo.Collection; } };');
  Decl:=TJSNamespaceDeclaration(CheckClass('export',TJSNamespaceDeclaration,GetFirstNameSpace));
  AssertNotNull('Have namespace',Decl);
end;

procedure TTestTypeScriptParser.TestExportFunction;

Var
  Exp : TJSExportStatement;

begin
  StartTs('export function clone(stylesheet: CSSStyleSheet): CSSStyleSheet;'#10+
          'export function parse(token: string): CSSStyleSheet;');
  AssertEquals('Statement count',2,GetStatements.Count);
  Exp:=TJSExportStatement(CheckClass('export',TJSExportStatement,GetFirstStatement));
  AssertNotNull('Have statement',Exp);
end;

procedure TTestTypeScriptParser.TestExportDefaultFunction;

Var
  Exp : TJSExportStatement;

begin
  StartTs('export default function (stylesheet: CSSStyleSheet): CSSStyleSheet;');
  AssertEquals('Statement count',1,GetStatements.Count);
  Exp:=TJSExportStatement(CheckClass('export',TJSExportStatement,GetFirstStatement));
  AssertNotNull('Have statement',Exp);
end;

procedure TTestTypeScriptParser.TestExportTypeArrowFunctionArgsComma;
Var
  S : TJSExportStatement;

begin
  StartTS('export type ConnectionRequestCb = ('#10+
          '      info: ConnectionRequestInfo,'#10+
          '      accept: ConnectionRequestAcceptFn,'#10+
          '      reject: ConnectionRequestRejectFn,'#10+
          '  ) => void;');
  S:=TJSExportStatement(CheckClass('First statement',TJSExportStatement,GetFirstStatement));
  CheckClass('Decl',TJSTypeDeclaration,S.Declaration);
end;

procedure TTestTypeScriptParser.TestExportEnum;
Var
  S : TJSExportStatement;
  E : TJSEnumDeclaration;
  ET : TJSEnumTypeDef;

begin
  StartTS('export enum Colors {red,blue,green};');
  S:=TJSExportStatement(CheckClass('First statement',TJSExportStatement,GetFirstStatement));
  E:=TJSEnumDeclaration(CheckClass('Decl',TJSEnumDeclaration,S.Declaration));
  AssertEquals('Name','Colors',E.Name);
  ET:=TJSEnumTypeDef(CheckClass(E.EnumDef,TJSEnumTypeDef,'Have enum definition'));
  AssertEquals('Member count',3,ET.NameCount);
  AssertEquals('Member 0','red',ET.Names[0]);
  AssertEquals('Member 1','blue',ET.Names[1]);
  AssertEquals('Member 2','green',ET.Names[2]);
end;

procedure TTestTypeScriptParser.TestExportObjectUnion;

begin
  StartTs('export type VisitorIdentifier = { id: string } | { user_id: string };'#10+
          'export interface Visitor {'#10+
          '  type: "visitor"; '#10+
          '}');
  AssertNotNull(GetStatements);
end;

procedure TTestTypeScriptParser.TestExportNoSemicolon;
begin
  StartTs('export function intlReducer(state: IntlState | undefined, action: IntlAction): IntlState'#10+
          'export function updateIntl (opts: IntlState): IntlAction');
  AssertNotNull(GetStatements);
end;

procedure TTestTypeScriptParser.TestExportAsKeyWord;
begin
  StartTs('export { del as delete };');
  AssertNotNull(GetStatements);
end;

procedure TTestTypeScriptParser.TestNamespaceInterfaceFunction;

Var
  NS : TJSNameSpaceStatement;

begin
  StartTs('declare namespace Rox {'#10+
          '  interface RoxContainer {'#10+
          '    [key: string]: Flag | RoxNumber | RoxString;'#10+
          '  }'#10+
          '  function register(namespace: string, roxContainer: RoxContainer): void;'#10+
          '  function register(roxContainer: RoxContainer): void;'#10+
          '}');
  NS:=TJSNameSpaceStatement(CheckClass('NS',TJSNameSpaceStatement,GetFirstStatement));
  AssertNotNull(NS);
end;

initialization
  RegisterTest(TTestTypeScriptParser);
end.

