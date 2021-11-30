unit tctsparser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcparser, jsscanner, jsbase, jstree, jstoken, jsparser;

Type

  { TTestTypeScriptParser }

  TTestTypeScriptParser = Class(TTestBaseJSParser)
  private
  Public
    Procedure StartTS(aSource : String);
    procedure CheckMembers(M : TJSSourceElements; aVars,aFunctions,aStatements,aClasses,aModules,aNamespaces,aTypes : Integer);
  Published
    Procedure TestDeclareModule;
    Procedure TestDeclareNamespace;
    Procedure TestDeclareFunction;
    Procedure TestDeclareTypeAlias;
    Procedure TestDeclareTypeTypeQuery;
    Procedure TestDeclareTypeUnion;
    Procedure TestDeclareTypeTuple;
    Procedure TestDeclareTypeArray;
    Procedure TestDeclareTypeArrayOfUnionType;
    Procedure TestDeclareTypeArrayOfFunctionType;
    Procedure TestDeclareTypeArrayGeneric;
    Procedure TestDeclareTypeGeneric;
    Procedure TestDeclareTypeGenericGeneric;
    Procedure TestDeclareTypeObject;
    Procedure TestDeclareTypeArrowFunction;
    Procedure TestDeclareTypeArrowFunctionShort;
    Procedure TestDeclareTypeArrowFunctionTwoArgs;
    Procedure TestDeclareEnum;
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
    procedure TestDeclareClassEmpty;
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
  AssertEquals('Name',Unicodestring('a'),M.Name);
  CheckMembers(M.Members,0,0,0,0,0,0,0);
end;

procedure TTestTypeScriptParser.TestDeclareNamespace;
Var
  N : TJSNamespaceDeclaration;

begin
  StartTS('declare namespace a { }');
  N:=GetFirstNamespace;
  AssertEquals('Name',Unicodestring('a'),N.Name);
  CheckMembers(N.Members,0,0,0,0,0,0,0);
end;

procedure TTestTypeScriptParser.TestDeclareFunction;

Var
  F : TJSFunctionDeclarationStatement;

begin
  StartTS('declare function a (b : number) : string ;');
  F:=GetFirstFunction;
  AssertEquals('Name',Unicodestring('a'),F.AFunction.Name);
  AssertNull('No body',F.AFunction.Body);
end;

procedure TTestTypeScriptParser.TestDeclareTypeAlias;

Var
  T : TJSTypeDeclaration;

begin
  StartTS('type a = b;');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTypeReference,T.TypeDef.ClassType);
  AssertEquals('Correct name def class',Unicodestring('b'),TJSTypeReference(T.TypeDef).Name);
  AssertEquals('Correct name def class',False,TJSTypeReference(T.TypeDef).IsTypeOf);
end;

procedure TTestTypeScriptParser.TestDeclareTypeTypeQuery;
Var
  T : TJSTypeDeclaration;

begin
  StartTS('type a = typeof b;');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTypeReference,T.TypeDef.ClassType);
  AssertEquals('Correct name def class',Unicodestring('b'),TJSTypeReference(T.TypeDef).Name);
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
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSUnionTypeDef,T.TypeDef.ClassType);
  U:=TJSUnionTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2,U.TypeCount);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[0].ClassType);
  R:=U.Types[0] as TJSTypeReference;
  AssertEquals('Name type 0',Unicodestring('string'),R.Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[1].ClassType);
  R:=U.Types[1] as TJSTypeReference;
  AssertEquals('Name type 1',Unicodestring('number'),R.Name);

end;

procedure TTestTypeScriptParser.TestDeclareTypeTuple;

Var
  T : TJSTypeDeclaration;
  U : TJSTupleTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = [string , number]');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSTupleTypeDef,T.TypeDef.ClassType);
  U:=TJSTupleTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2,U.TypeCount);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[0].ClassType);
  R:=U.Types[0] as TJSTypeReference;
  AssertEquals('Name type 0',Unicodestring('string'),R.Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, U.Types[1].ClassType);
  R:=U.Types[1] as TJSTypeReference;
  AssertEquals('Name type 1',Unicodestring('number'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArray;
Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = string[]');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSArrayTypeDef,T.TypeDef.ClassType);
  A:=TJSArrayTypeDef(T.TypeDef);
  AssertNotNull('have base type',A.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, A.BaseType.ClassType);
  R:=A.BaseType as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('string'),R.Name);
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
  AssertEquals('Name',Unicodestring('a'),T.Name);
  A:=TJSArrayTypeDef(CheckClass(T.TypeDef,TJSArrayTypeDef,'Type def class'));
  U:=TJSUnionTypeDef(CheckClass(A.BaseType,TJSUnionTypeDef,'base type Union class '));
  AssertEquals('Union Count',2,U.TypeCount);
  R:=TJSTypeReference(CheckClass(U.Types[0],TJSTypeReference,'Union class element 0'));
  AssertEquals('Name base type',Unicodestring('string'),R.Name);
  R:=TJSTypeReference(CheckClass(U.Types[1],TJSTypeReference,'Union class element 1'));
  AssertEquals('Name base type',Unicodestring('number'),R.Name);
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
  AssertEquals('Name',Unicodestring('a'),T.Name);
  A:=TJSArrayTypeDef(CheckClass(T.TypeDef,TJSArrayTypeDef,'Type def class'));
  F:=TJSArrowFunctionTypeDef(CheckClass(A.BaseType,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',F.aFunction);
  R:=TJSTypeReference(CheckClass(F.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name',Unicodestring('string'),R.Name);
  AssertEquals('function argument count',0,F.aFunction.TypedParams.Count);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrayGeneric;

Var
  T : TJSTypeDeclaration;
  A : TJSArrayTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type a = Array<string>');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSArrayTypeDef,T.TypeDef.ClassType);
  A:=TJSArrayTypeDef(T.TypeDef);
  AssertNotNull('have base type',A.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, A.BaseType.ClassType);
  R:=A.BaseType as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('string'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGeneric;
Var
  T : TJSTypeDeclaration;
  G : TJSGenericTypeRef;
  R : TJSTypeReference;

begin
  StartTS('type a = Shell<string>');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('a'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type def class',TJSGenericTypeRef,T.TypeDef.ClassType);
  G:=TJSGenericTypeRef(T.TypeDef);
  AssertNotNull('have base type',G.BaseType);
  AssertEquals('Correct type of base type',TJSTypeReference, G.BaseType.ClassType);
  R:=G.BaseType as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('Shell'),R.Name);
  AssertEquals('Correct number of parameters',1, G.ParamTypeCount);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, G.ParamTypes[0].Classtype);
  R:=G.ParamTypes[0] as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('string'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeGenericGeneric;
Var
  T : TJSTypeDeclaration;
  G : TJSTupleTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type Pair<T> = [T,T]');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('Pair'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct number of parameters',1, T.TypeParams.Count);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, T.TypeParams[0].Node.Classtype);
  R:=T.TypeParams[0].Node as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('T'),R.Name);
  AssertEquals('Correct type def class',TJSTupleTypeDef,T.TypeDef.ClassType);
  G:=TJSTupleTypeDef(T.TypeDef);
  AssertEquals('Correct number of parameters',2, G.TypeCount);
  AssertEquals('Correct type of parameter 0',TJSTypeReference, G.Types[0].Classtype);
  R:=G.Types[0] as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('T'),R.Name);
  AssertEquals('Correct type of parameter 1',TJSTypeReference, G.Types[1].Classtype);
  R:=G.Types[1] as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('T'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeObject;

Var
  T : TJSTypeDeclaration;
  O : TJSObjectTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type TreeItem = {left : TreeItem, right : TreeItem};');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('TreeItem'),T.Name);
  AssertNotNull('Have type def',T.TypeDef);
  AssertEquals('Correct type of typedef',TJSObjectTypeDef, T.TypeDef.ClassType);
  O:=TJSObjectTypeDef(T.TypeDef);
  AssertEquals('Correct number of elements',2, O.ElementCount);
  AssertEquals('Correct name of element 0',UnicodeString('left'), O.Elements[0].Name);
  AssertEquals('Correct type of element 0',TJSTypeReference, O.Elements[0].ElementType.ClassType);
  R:=O.Elements[0].ElementType as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('TreeItem'),R.Name);
  AssertEquals('Correct name of element 0',UnicodeString('right'), O.Elements[1].Name);
  AssertEquals('Correct type of element 1',TJSTypeReference, O.Elements[1].ElementType.ClassType);
  R:=O.Elements[1].ElementType as TJSTypeReference;
  AssertEquals('Name base type',Unicodestring('TreeItem'),R.Name);
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
  AssertEquals('Name',Unicodestring('CallBack'),T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name',Unicodestring('void'),R.Name);
  AssertEquals('function argument count',1,A.aFunction.TypedParams.Count);
  P:=A.aFunction.TypedParams.Params[0];
  AssertEquals('param name',Unicodestring('data'),P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param type'));
  AssertEquals('param type name',Unicodestring('t'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareTypeArrowFunctionShort;
Var
  T : TJSTypeDeclaration;
  A : TJSArrowFunctionTypeDef;
  R : TJSTypeReference;

begin
  StartTS('type CallBack = () => void;');
  T:=GetFirstType;
  AssertEquals('Name',Unicodestring('CallBack'),T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name',Unicodestring('void'),R.Name);
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
  AssertEquals('Name',Unicodestring('CallBack'),T.Name);
  A:=TJSArrowFunctionTypeDef(CheckClass(T.TypeDef,TJSArrowFunctionTypeDef,'Have arrow function'));
  AssertNotNull('Have function definition',A.aFunction);
  R:=TJSTypeReference(CheckClass(A.aFunction.ResultType,TJSTypeReference,'Have function result'));
  AssertEquals('result type name',Unicodestring('void'),R.Name);
  AssertEquals('function argument count',2,A.aFunction.TypedParams.Count);
  P:=A.aFunction.TypedParams.Params[0];
  AssertEquals('param name',Unicodestring('data'),P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param 0 type'));
  AssertEquals('param type name',Unicodestring('t'),R.Name);
  P:=A.aFunction.TypedParams.Params[1];
  AssertEquals('param name',Unicodestring('context'),P.Name);
  R:=TJSTypeReference(CheckClass(P.Node,TJSTypeReference,'Have param 1 type'));
  AssertEquals('param type name',Unicodestring('t2'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareEnum;

Var
  E : TJSEnumDeclaration;
  ET : TJSEnumTypeDef;

begin
  StartTS('enum Colors {red,blue,green};');
  E:=GetFirstEnum;
  AssertEquals('Name',Unicodestring('Colors'),E.Name);
  ET:=TJSEnumTypeDef(CheckClass(E.EnumDef,TJSEnumTypeDef,'Have enum definition'));
  AssertEquals('Member count',3,ET.NameCount);
  AssertEquals('Member 0',UnicodeString('red'),ET.Names[0]);
  AssertEquals('Member 1',UnicodeString('blue'),ET.Names[1]);
  AssertEquals('Member 2',UnicodeString('green'),ET.Names[2]);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceEmpty;

Var
  Decl : TJSInterfaceDeclaration;

begin
  StartTS('interface Empty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name',Unicodestring('Empty'),Decl.Name);
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
  AssertEquals('First type node is type declaration',True,GetTypes.Nodes[0].IsAmbient);
  AssertNotNull('Have declaration');
  AssertEquals('Name',Unicodestring('Empty'),Decl.Name);
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
  AssertEquals('Name',Unicodestring('Empty'),Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
  AssertEquals('Extends count',1,Decl.Extends.Count);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[0].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name',UnicodeString('ParentEmpty'),Lit.Value.AsString);
end;

procedure TTestTypeScriptParser.TestDeclareInterfaceExtendsTwoEmpty;
Var
  Decl : TJSInterfaceDeclaration;
  Lit : TJSLiteral;

begin
  StartTS('interface Empty extends ParentEmpty,MoreEmpty {};');
  Decl:=TJSInterfaceDeclaration(CheckClass(GetFirstInterface,TJSInterfaceDeclaration,'Interface'));
  AssertNotNull('Have declaration');
  AssertEquals('Name',Unicodestring('Empty'),Decl.Name);
  AssertNotNull('Extends',Decl.Extends);
  AssertEquals('Extends count',2,Decl.Extends.Count);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[0].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name',UnicodeString('ParentEmpty'),Lit.Value.AsString);
  Lit:=TJSLiteral(CheckClass(Decl.Extends.Nodes[1].Node,TJSLiteral,'Literal 0 '));
  AssertNotNull('Lit has value',Lit.Value);
  AssertEquals('Extends name',UnicodeString('MoreEmpty'),Lit.Value.AsString);
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
  AssertEquals('Name',Unicodestring('Friend'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name',UnicodeString('name'),Prop.Name);
  AssertEquals('Property optional',False,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name',UnicodeString('string'),R.Name);
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
  AssertEquals('Name',Unicodestring('Friend'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name',UnicodeString('name'),Prop.Name);
  AssertEquals('Property optional',True,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name',UnicodeString('string'),R.Name);
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
  AssertEquals('Name',Unicodestring('Friend'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name',UnicodeString('name'),Prop.Name);
  AssertEquals('Property optional',False,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name',UnicodeString('any'),R.Name);
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
  AssertEquals('Name',Unicodestring('Friend'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  Prop:=TJSPropertyDeclaration(CheckClass('Property class',TJSPropertyDeclaration,Decl.Elements[0]));
  AssertEquals('Property name',UnicodeString('name'),Prop.Name);
  AssertEquals('Property optional',True,Prop.Optional);
  R:=TJSTypeReference(CheckClass('Property type',TJSTypeReference,Prop.ElementType));
  AssertEquals('Property type name',UnicodeString('any'),R.Name);
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
  AssertEquals('Name',Unicodestring('JQuery'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name',UnicodeString('text'),M.Name);
  AssertEquals('Method optional',False,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name',UnicodeString('string'),R.Name);
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
  AssertEquals('Name',Unicodestring('JQuery'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name',UnicodeString('text'),M.Name);
  AssertEquals('Method optional',False,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name',UnicodeString('any'),R.Name);
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
  AssertEquals('Name',Unicodestring('JQuery'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name',UnicodeString(''),M.Name);
  AssertEquals('Method optional',False,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name',UnicodeString('JQuery'),R.Name);
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
  AssertEquals('Name',Unicodestring('JQuery'),Decl.Name);
  AssertNull('Extends',Decl.Extends);
  AssertEquals('Member count',1,Decl.ElementCount);
  M:=TJSMethodDeclaration(CheckClass('Method class',TJSMethodDeclaration,Decl.Elements[0]));
  AssertEquals('Method name',UnicodeString('text'),M.Name);
  AssertEquals('Method optional',False,M.Optional);
  AssertNotNull('Have function def',M.FuncDef);
  R:=TJSTypeReference(CheckClass('Result type',TJSTypeReference,M.FuncDef.ResultType));
  AssertEquals('Property type name',UnicodeString('this'),R.Name);
end;

procedure TTestTypeScriptParser.TestDeclareClassEmpty;
Var
  Decl : TJSClassDeclaration;
  M : TJSMethodDeclaration;
  R : TJSTypeReference;

begin
  StartTS('declare class JQuery {};');
  Decl:=TJSClassDeclaration(CheckClass('Class',TJSClassDeclaration,GetFirstClass));
  AssertNotNull('Have declaration');
  AssertEquals('Name',Unicodestring('JQuery'),Decl.Name);
  AssertEquals('Extends','',Decl.Extends);
  CheckMembers(Decl.Members,0,0,0,0,0,0,0);
end;

initialization
  RegisterTest(TTestTypeScriptParser);
end.

