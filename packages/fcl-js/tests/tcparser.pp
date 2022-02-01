unit tcparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, jstoken, jsParser, jstree, jsbase;

type

  { TTestJSParser }

  { TTestBaseJSParser }

  TTestBaseJSParser = class(TTestCase)
  Private
    FSource : TStringStream;
    FParser : TJSParser;
    FSE : TJSSourceElements;
    FToFree: TJSElement;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure CreateParser(Const ASource : string; aVersion : TECMAVersion = TECMAVersion.ecma5; aIsTypeScript : Boolean = False);
    Function CheckClass(E : TJSElement; C : TJSElementClass; Const aMsg : String = '') : TJSElement;
    Function CheckClass(Const aMsg : String; aExpectedClass : TJSElementClass; aActual : TJSElement) : TJSElement;
    Procedure AssertEquals(Const AMessage : String; Expected, Actual : TJSToken); overload;
    Procedure AssertEquals(Const AMessage : String; Expected, Actual : TJSType); overload;
    Procedure AssertEquals(Const AMessage : String; Expected, Actual : TJSVarType); overload;
    Procedure AssertEquals(Const AMessage : String; Expected, Actual : TKeyOptionality); overload;
    Procedure AssertIdentifier(Msg : String; El : TJSElement; Const AName : TJSString);
    procedure AssertEquals(Const AMessage : String; aExpected : AnsiString; aActual : TJSString); overload;
    Function  GetSourceElements : TJSSourceElements;
    Function  GetVars : TJSElementNodes;
    Function  GetStatements : TJSElementNodes;
    Function  GetFunctions : TJSElementNodes;
    Function  GetFirstFunction : TJSFunctionDeclarationStatement;
    Function  GetClasses : TJSElementNodes;
    Function  GetFirstClass(isAmbient : Boolean = false) : TJSClassDeclaration;
    Function  GetFirstStatement : TJSElement;
    Function  GetFirstVar : TJSElement;
    Function  GetModules : TJSElementNodes;
    Function  GetFirstModule : TJSModuleDeclaration;
    Function  GetNameSpaces : TJSElementNodes;
    Function  GetFirstNameSpace : TJSNamespaceDeclaration;
    Function  GetTypes : TJSElementNodes;
    Function  GetFirstType : TJSTypeDeclaration;
    Function  GetEnums : TJSElementNodes;
    Function  GetFirstEnum: TJSEnumDeclaration;
    Function  GetExpressionStatement : TJSExpressionStatement;
    Function  GetInterfaces : TJSElementNodes;
    Function  GetFirstInterface : TJSInterfaceDeclaration;
  end;

  TTestJSParser= class(TTestBaseJSParser)
  private
  published
    procedure TestEmpty;
    procedure TestSimple;
    procedure TestSimpleExpressionNumericalLiteral;
    procedure TestSimpleExpressionStringLiteral;
    procedure TestSimpleExpressionBooleanLiteralFalse;
    procedure TestSimpleExpressionBooleanLiteralTrue;
    procedure TestSimpleExpressionIdentifier;
    procedure TestSimpleExpressionNull;
    procedure TestAssignExpressionNumerical;
    procedure TestAssignExpressionNull;
    procedure TestAssignExpressionString;
    procedure TestAssignExpressionBooleanFalse;
    procedure TestAssignExpressionBooleanTrue;
    procedure TestAssignExpressionIdent;
    procedure TestAssignExpressionPlus;
    procedure TestAssignExpressionMinus;
    procedure TestAssignExpressionDiv;
    procedure TestAssignExpressionMul;
    procedure TestAssignExpressionMod;
    procedure TestAssignExpressionAnd;
    procedure TestAssignExpressionOr;
    procedure TestAssignExpressionXOr;
    procedure TestAssignExpressionLShift;
    procedure TestAssignExpressionRShift;
    procedure TestAssignExpressionURShift;
    procedure TestExpressionPlus;
    procedure TestExpressionSub;
    procedure TestExpressionMul;
    procedure TestExpressionDiv;
    procedure TestExpressionMod;
    procedure TestExpressionLShift;
    procedure TestExpressionRShift;
    procedure TestExpressionURShift;
    procedure TestExpressionPostPlusPlus;
    procedure TestExpressionPostMinusMinus;
    procedure TestExpressionPreMinusMinus;
    procedure TestExpressionPrePlusPlus;
    procedure TestExpressionPrecedenceMulPlus;
    procedure TestExpressionPrecedencePlusMul;
    procedure TestExpressionPrecedenceMulMinus;
    procedure TestExpressionPrecedenceMinusMul;
    procedure TestExpressionPrecedenceDivPlus;
    procedure TestExpressionPrecedencePlusDiv;
    procedure TestExpressionPrecedenceModPlus;
    procedure TestExpressionPrecedencePlusMod;
    procedure TestExpressionPrecedencePlusPostPlusPlus;
    procedure TestExpressionPrecedencePlusPostMinusMinus;
    procedure TestExpressionPrecedenceMulPostMinusMinus;
    procedure TestExpressionPrecedenceMulPostPlusPlus;
    procedure TestExpressionPrecedenceMulPreMinusMinus;
    procedure TestExpressionPrecedenceMulPrePlusPlus;
    procedure TestExpressionPrecedencePlusPreMinusMinus;
    procedure TestExpressionPrecedencePlusPrePlusPlus;
    procedure TestExpressionPrecedencePlusInv;
    procedure TestExpressionPrecedenceMulInv;
    procedure TestExpressionPrecedenceMulNot;
    procedure TestExpressionPrecedencePlusNot;
    procedure TestExpressionPrecedenceBraceMulPlus;
    procedure TestExpressionPrecedenceBracePlusMul;
    procedure TestExpressionFunction;
    procedure TestFunctionCallNoArgs;
    procedure TestAwaitFunctionCallNoArgs;
    procedure TestFunctionCallOneArg;
    procedure TestFunctionCallTwoArgs;
    procedure TestObjectGeneratorFunction;
    procedure TestArrayExpressionNumericalArgs;
    procedure TestArrayExpressionStringArgs;
    procedure TestArrayExpressionIdentArgs;
    Procedure TestVarDeclarationSimple;
    Procedure TestVarDeclarationInit;
    Procedure TestLetDeclarationSimple;
    procedure TestVarDeclarationDouble;
    procedure TestVarDeclarationSimpleInit;
    procedure TestConstDeclarationSimpleInit;
    procedure TestVarDeclarationDoubleInit;
    procedure TestDebuggerStatement;
    procedure TestBlockEmpty;
    procedure TestBlockEmptyStatement;
    procedure TestBlockSimpleStatement;
    procedure TestFunctionDeclarationEmpty;
    procedure TestFunctionDeclarationAsync;
    procedure TestFunctionDeclarationWithArgs;
    procedure TestFunctionDeclarationWithSpreadArgs;
    procedure TestFunctionDeclarationWithBody;
    procedure TestIfSimple;
    procedure TestIfElseSimple;
    procedure TestIfEmptyBlock;
    procedure TestIfEmptyBlockElse;
    procedure TestWhileSimple;
    procedure TestWhileBlock;
    procedure TestDoWhileSimple;
    procedure TestDoWhileBlock;
    procedure TestForEmpty;
    procedure TestForEmptyBody;
    procedure TestForSimpleBody;
    procedure TestTryCatch;
    procedure TestTryCatchFinally;
    procedure TestTryFinally;
    procedure TestThrow;
    procedure TestReturn;
    procedure TestAssignment;
    procedure TestNew;
    procedure TestLabeledStatement;
    procedure TestContinue;
    procedure TestContinueTarget;
    procedure TestBreak;
    procedure TestBreakTarget;
    procedure TestSwitchEmpty;
    procedure TestSwitchOne;
    procedure TestSwitchTwo;
    procedure TestSwitchTwoDefault;
    Procedure TestImportModule;
    Procedure TestImportImportedDefault;
    Procedure TestImportNamespaceImport;
    Procedure TestImportImportedDefaultAndNamespaceImport;
    Procedure TestImportNamedImport;
    Procedure TestImportNamedImportAlias;
    Procedure TestImport2NamedImports;
    Procedure TestImport2NamedImportAlias;
    Procedure TestImport2NamedImportsComma;
    Procedure TestImportDefaultAndNamedImport;
    Procedure TestExportAll;
    Procedure TestExportAllFrom;
    Procedure TestExportExportNameFrom;
    Procedure TestExportExportName;
    Procedure TestExportExportNameAlias;
    Procedure TestExportVar;
    Procedure TestExportLet;
    Procedure TestExportConst;
    Procedure TestExportFunction;
    Procedure TestExportDefaultAssignment;
    Procedure TestExportDefaultFunction;
    Procedure TestExportDefaultAsyncFunction;
    Procedure TestClass;
    Procedure TestClassExtends;
    Procedure TestClassWithMethod;
    procedure TestClassExpression;
    procedure TestLetClassExpression;
  end;

implementation

uses typinfo;

{ ----------------------------------------------------------------------
  TTestBaseJSParser
  ----------------------------------------------------------------------}

procedure TTestBaseJSParser.SetUp;
begin
  FParser:=Nil;
  FSource:=Nil;
end;

procedure TTestBaseJSParser.TearDown;
begin
  FreeAndNil(FToFree);
  FreeAndNil(FParser);
  FReeAndNil(FSource);
end;

Procedure TTestBaseJSParser.CreateParser(Const ASource: string; aVersion : TECMAVersion = TECMAVersion.ecma5; aIsTypeScript : Boolean = False);
begin
  FSource:=TStringStream.Create(ASource);
  FParser:=TJSParser.Create(FSource,aVersion,aIsTypescript);
end;

function TTestBaseJSParser.CheckClass(E: TJSElement; C: TJSElementClass; const aMsg: String): TJSElement;
begin
  AssertNotNull(aMsg+': Not null element',E);
  AssertNotNull(aMsg+': Not null class',C);
  AssertEquals(aMsg,C,E.ClassType);
  Result:=E;
end;

function TTestBaseJSParser.CheckClass(const aMsg: String; aExpectedClass: TJSElementClass; aActual: TJSElement): TJSElement;
begin
  Result:=CheckClass(aActual,aExpectedClass,aMsg);
end;

procedure TTestBaseJSParser.AssertEquals(const AMessage: String; Expected, Actual: TJSToken);
Var
  NE,NA : String;

begin
  NE:=GetEnumName(TypeInfo(TJSToken),Ord(Expected));
  NA:=GetEnumName(TypeInfo(TJSToken),Ord(Actual));
  AssertEquals(AMessage,NE,NA);
end;

Function TTestBaseJSParser.GetSourceElements: TJSSourceElements;

Var
  E : TJSElement;
  FB : TJSFunctionBody;

begin
  If Not Assigned(FSE) then
    begin
    AssertNotNull('Parser assigned',FParser);
    E:=FParser.Parse;
    CheckClass(E,TJSFunctionBody);
    FB:=TJSFunctionBody(E);
    AssertNotNull(FB.A);
    CheckClass(FB.A,TJSSourceElements);
    FSE:=TJSSourceElements(FB.A);
    FToFree:=E;
    end;
  Result:=FSE;
end;

Function TTestBaseJSParser.GetVars: TJSElementNodes;
begin
  Result:=GetSourceElements.Vars;
end;

Function TTestBaseJSParser.GetStatements: TJSElementNodes;
begin
  Result:=GetSourceElements.Statements;
end;

Function TTestBaseJSParser.GetFunctions: TJSElementNodes;
begin
  Result:=GetSourceElements.Functions;
end;

function TTestBaseJSParser.GetFirstFunction: TJSFunctionDeclarationStatement;
Var
  aFunctions : TJSElementNodes;

begin
  aFunctions:=GetFunctions;
  AssertTrue('Have functions ',aFunctions.Count>0);
  AssertNotNull('have first function node',aFunctions.Nodes[0].Node);
  AssertEquals('First function node is function declaration',TJSFunctionDeclarationStatement,aFunctions.Nodes[0].Node.ClassType);
  Result:=TJSFunctionDeclarationStatement(aFunctions.Nodes[0].Node);
end;

function TTestBaseJSParser.GetClasses: TJSElementNodes;
begin
  Result:=GetSourceElements.Classes;
end;

function TTestBaseJSParser.GetFirstClass(isAmbient : Boolean = false): TJSClassDeclaration;

Var
  aClasses : TJSElementNodes;

begin
  aClasses:=GetClasses;
  AssertTrue('Have classes ',aClasses.Count>0);
  AssertNotNull('have first class node',aClasses.Nodes[0].Node);
  if IsAmbient then
    AssertEquals('First class node is ambientclass declaration',TJSAmbientClassDeclaration,aClasses.Nodes[0].Node.ClassType)
  else
    AssertEquals('First class node is class declaration',TJSClassDeclaration,aClasses.Nodes[0].Node.ClassType);
  Result:=TJSClassDeclaration(aClasses.Nodes[0].Node);
end;

Procedure TTestBaseJSParser.AssertEquals(Const AMessage: String; Expected,
  Actual: TJSType);

Var
  NE,NA : String;

begin
  NE:=GetEnumName(TypeInfo(TJSType),Ord(Expected));
  NA:=GetEnumName(TypeInfo(TJSType),Ord(Actual));
  AssertEquals(AMessage,NE,NA);
end;

procedure TTestBaseJSParser.AssertEquals(const AMessage: String; Expected, Actual: TJSVarType);

Var
  NE,NA : String;

begin
  NE:=GetEnumName(TypeInfo(TJSVarType),Ord(Expected));
  NA:=GetEnumName(TypeInfo(TJSVarType),Ord(Actual));
  AssertEquals(AMessage,NE,NA);
end;

procedure TTestBaseJSParser.AssertEquals(const AMessage: String; Expected, Actual: TKeyOptionality);
Var
  NE,NA : String;

begin
  NE:=GetEnumName(TypeInfo(TKeyOptionality),Ord(Expected));
  NA:=GetEnumName(TypeInfo(TKeyOptionality),Ord(Actual));
  AssertEquals(AMessage,NE,NA);
end;

Procedure TTestBaseJSParser.AssertIdentifier(Msg: String; El: TJSElement;
  Const AName: TJSString);

Var
  L : TJSPrimaryExpressionIdent;
  S1,S2 : TJSString;
begin
  AssertNotNull(Msg+' have TJSPrimaryExpressionIdent element',El);
  CheckClass(El,TJSPrimaryExpressionIdent);
  L:=TJSPrimaryExpressionIdent(el);
  S1:=L.Name;
  S2:=Aname;
  AssertEquals(Msg+'Identifier has correct name',S2,S1);
end;

procedure TTestBaseJSParser.AssertEquals(const AMessage: String; aExpected: AnsiString; aActual: TJSString);
begin
  AssertEquals(AMessage,UTF8Decode(aExpected),aActual);
end;

Function TTestBaseJSParser.GetFirstStatement: TJSElement;

Var
  E : TJSElementNodes;
begin
  E:=GetStatements;
  AssertNotNull('Have statements',E);
  AssertTrue('1 statement',1<=E.Count);
  Result:=E.Nodes[0].Node;
  AssertNotNull('First statement assigned',Result);
end;

Function TTestBaseJSParser.GetFirstVar: TJSElement;
Var
  E : TJSElementNodes;
begin
  E:=GetVars;
  AssertNotNull('Have statements',E);
  // Writeln('Count : ',E.Count);
  If (E.Count=0) then
    Fail('Zero variables defined');
  Result:=E.Nodes[0].Node;
  AssertNotNull('First variable declaration',Result);
end;

function TTestBaseJSParser.GetModules: TJSElementNodes;
begin
  Result:=GetSourceElements.Modules;
end;

function TTestBaseJSParser.GetFirstModule: TJSModuleDeclaration;
Var
  E : TJSElementNodes;

begin
  E:=GetModules;
  AssertNotNull('Have modules',E);
  AssertEquals('1 statement',1,E.Count);
  AssertEquals('First module node is module declaration',TJSModuleDeclaration,E.Nodes[0].Node.ClassType);
  Result:=E.Nodes[0].Node as TJSModuleDeclaration;
end;

function TTestBaseJSParser.GetNameSpaces: TJSElementNodes;
begin
  Result:=GetSourceElements.NameSpaces;
end;

function TTestBaseJSParser.GetFirstNameSpace: TJSNamespaceDeclaration;

Var
  E : TJSElementNodes;

begin
  E:=GetNameSpaces;
  AssertNotNull('Have namespaces',E);
  AssertEquals('1 namespace',1,E.Count);
  AssertEquals('First module node is namespace declaration',TJSNamespaceDeclaration,E.Nodes[0].Node.ClassType);
  Result:=E.Nodes[0].Node as TJSNamespaceDeclaration;
end;

function TTestBaseJSParser.GetTypes: TJSElementNodes;
begin
  Result:=GetSourceElements.Types;
end;

function TTestBaseJSParser.GetFirstType: TJSTypeDeclaration;
Var
  E : TJSElementNodes;

begin
  E:=GetTypes;
  AssertNotNull('Have types',E);
  AssertEquals('1 type',1,E.Count);
  AssertEquals('First type node is type declaration',TJSTypeDeclaration,E.Nodes[0].Node.ClassType);
  Result:=(E.Nodes[0].Node as TJSTypeDeclaration);
end;

function TTestBaseJSParser.GetEnums: TJSElementNodes;
begin
  Result:=GetSourceElements.Enums;
end;

function TTestBaseJSParser.GetFirstEnum: TJSEnumDeclaration;
Var
  E : TJSElementNodes;

begin
  E:=GetEnums;
  AssertNotNull('Have enums',E);
  AssertEquals('1 type',1,E.Count);
  AssertEquals('First type node is enum declaration',TJSEnumDeclaration,E.Nodes[0].Node.ClassType);
  Result:=(E.Nodes[0].Node as TJSEnumDeclaration);
end;

Function TTestBaseJSParser.GetExpressionStatement: TJSExpressionStatement;

Var
  N : TJSElement;
begin
  N:=GetFirstStatement;
  CheckClass(N,TJSExpressionStatement);
  Result:=TJSExpressionStatement(N);
end;

function TTestBaseJSParser.GetInterfaces: TJSElementNodes;
begin
  Result:=GetSourceElements.Interfaces;
end;

function TTestBaseJSParser.GetFirstInterface: TJSInterfaceDeclaration;
Var
  E : TJSElementNodes;

begin
  E:=GetInterfaces;
  AssertNotNull('Have interfaces',E);
  AssertEquals('1 interfaces',1,E.Count);
  AssertEquals('First interface node is interface declaration',TJSInterfaceDeclaration,E.Nodes[0].Node.ClassType);
  Result:=(E.Nodes[0].Node as TJSInterfaceDeclaration);
end;



{ ----------------------------------------------------------------------
  TTestJSParser
  ----------------------------------------------------------------------}





procedure TTestJSParser.TestSimple;

Var
  E : TJSElementNodes;
  N : TJSElement;
  X : TJSExpressionStatement;

begin
  CreateParser('1;');
  E:=GetStatements;
  AssertNotNull('Have statements',E);
  AssertEquals('1 statement',1,E.Count);
  N:=E.Nodes[0].Node;
  AssertNotNull('First statement assigned',N);
  AssertNotNull('First statement assigned',N);
  CheckClass(N,TJSExpressionStatement);
  X:=TJSExpressionStatement(N);
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLiteral);
end;

procedure TTestJSParser.TestSimpleExpressionNumericalLiteral;
Var
  X : TJSExpressionStatement;

begin
  CreateParser('1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLiteral);
  AssertNotNull('Expression value assigned',TJSLiteral(X.A).Value);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(X.A).Value.ValueType);
  AssertEquals('Expression value correct', 1.0,TJSLiteral(X.A).Value.AsNumber);
end;

procedure TTestJSParser.TestSimpleExpressionStringLiteral;

Var
  X : TJSExpressionStatement;

begin
  CreateParser('"string";');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLiteral);
  AssertNotNull('Expression value assigned',TJSLiteral(X.A).Value);
  AssertEquals('Expression value type correct', jstString,TJSLiteral(X.A).Value.ValueType);
  AssertEquals('Expression value correct', 'string',TJSLiteral(X.A).Value.AsString);
end;

procedure TTestJSParser.TestSimpleExpressionBooleanLiteralFalse;

Var
  X : TJSExpressionStatement;

begin
  CreateParser('false;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLiteral);
  AssertNotNull('Expression value assigned',TJSLiteral(X.A).Value);
  AssertEquals('Expression value type correct', jstBoolean,TJSLiteral(X.A).Value.ValueType);
  AssertEquals('Expression value correct', False, TJSLiteral(X.A).Value.AsBoolean);
end;

procedure TTestJSParser.TestSimpleExpressionIdentifier;

Var
  X : TJSExpressionStatement;

begin
  CreateParser('Something;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSPrimaryExpressionIdent);
  AssertEquals('Expression value assigned','Something',TJSPrimaryExpressionIdent(X.A).Name);
end;

procedure TTestJSParser.TestSimpleExpressionNull;

Var
  X : TJSExpressionStatement;
begin
    CreateParser('null;');
    X:=GetExpressionStatement;
    AssertNotNull('Expression statement assigned',X.A);
    CheckClass(X.A,TJSLiteral);
    AssertNotNull('Expression value assigned',TJSLiteral(X.A).Value);
    AssertEquals('Expression value type correct', jstNull,TJSLiteral(X.A).Value.ValueType);
    AssertEquals('Expression value correct', True, TJSLiteral(X.A).Value.IsNull);
end;

procedure TTestJSParser.TestAssignExpressionNumerical;

Var
  X : TJSExpressionStatement;
  SA : TJSSimpleAssignStatement;
begin
  CreateParser('a=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionString;

Var
  X : TJSExpressionStatement;
  SA : TJSSimpleAssignStatement;
begin
  CreateParser('a="string";');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstString,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 'string', TJSLiteral(SA.Expr).Value.AsString);
end;

procedure TTestJSParser.TestAssignExpressionBooleanFalse;

Var
  X : TJSExpressionStatement;
  SA : TJSSimpleAssignStatement;
begin
  CreateParser('a=false;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstBoolean,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', False, TJSLiteral(SA.Expr).Value.AsBoolean);
end;

procedure TTestJSParser.TestAssignExpressionBooleanTrue;
Var
  X : TJSExpressionStatement;
  SA : TJSSimpleAssignStatement;
begin
  CreateParser('a=true;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstBoolean,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', True, TJSLiteral(SA.Expr).Value.AsBoolean);
end;

procedure TTestJSParser.TestAssignExpressionNull;

Var
  X : TJSExpressionStatement;
  SA : TJSSimpleAssignStatement;
begin
  CreateParser('a=null;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNull,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', True, TJSLiteral(SA.Expr).Value.IsNull);
end;

procedure TTestJSParser.TestAssignExpressionIdent;
Var
  X : TJSExpressionStatement;
  SA : TJSSimpleAssignStatement;
begin
  CreateParser('a=b;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSPrimaryExpressionIdent);
  AssertEquals('Expression value type correct', 'b',TJSPrimaryExpressionIdent(SA.Expr).Name);
end;

procedure TTestJSParser.TestAssignExpressionPlus;

Var
  X : TJSExpressionStatement;
  SA : TJSAddEqAssignStatement;

begin
  CreateParser('a+=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAddEqAssignStatement);
  SA:=TJSAddEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionMinus;

Var
  X : TJSExpressionStatement;
  SA : TJSSubEqAssignStatement;

begin
  CreateParser('a-=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSubEqAssignStatement);
  SA:=TJSSubEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionMul;

Var
  X : TJSExpressionStatement;
  SA : TJSMulEqAssignStatement;

begin
  CreateParser('a*=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMulEqAssignStatement);
  SA:=TJSMulEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionDiv;

Var
  X : TJSExpressionStatement;
  SA : TJSDivEqAssignStatement;

begin
  CreateParser('a/=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSDivEqAssignStatement);
  SA:=TJSDivEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionMod;

Var
  X : TJSExpressionStatement;
  SA : TJSModEqAssignStatement;

begin
  CreateParser('a%=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSModEqAssignStatement);
  SA:=TJSModEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionAnd;

Var
  X : TJSExpressionStatement;
  SA : TJSAndEqAssignStatement;

begin
  CreateParser('a&=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAndEqAssignStatement);
  SA:=TJSAndEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionOr;

Var
  X : TJSExpressionStatement;
  SA : TJSOrEqAssignStatement;

begin
  CreateParser('a|=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSOrEqAssignStatement);
  SA:=TJSOrEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionXOr;

Var
  X : TJSExpressionStatement;
  SA : TJSXOrEqAssignStatement;

begin
  CreateParser('a^=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSXOrEqAssignStatement);
  SA:=TJSXOrEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionLShift;

Var
  X : TJSExpressionStatement;
  SA : TJSLShiftEqAssignStatement;

begin
  CreateParser('a<<=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLShiftEqAssignStatement);
  SA:=TJSLShiftEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionRShift;

Var
  X : TJSExpressionStatement;
  SA : TJSRShiftEqAssignStatement;

begin
  CreateParser('a>>=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSRShiftEqAssignStatement);
  SA:=TJSRShiftEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestAssignExpressionURShift;

Var
  X : TJSExpressionStatement;
  SA : TJSURShiftEqAssignStatement;

begin
  CreateParser('a>>>=1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSURShiftEqAssignStatement);
  SA:=TJSURShiftEqAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  CheckClass(SA.EXPR,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(SA.Expr).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(SA.Expr).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPlus;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;

begin
  CreateParser('1+2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionSub;
Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionMinus;

begin
  CreateParser('1 - 2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionMinus);
  E:=TJSAdditiveExpressionMinus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionMul;

Var
  X : TJSExpressionStatement;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('1*2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionDiv;
Var
  X : TJSExpressionStatement;
  E : TJSMultiplicativeExpressionDiv;

begin
  CreateParser('1/2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionDiv);
  E:=TJSMultiplicativeExpressionDiv(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionMod;
Var
  X : TJSExpressionStatement;
  E : TJSMultiplicativeExpressionMod;

begin
  CreateParser('1%2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMod);
  E:=TJSMultiplicativeExpressionMod(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionLShift;
Var
  X : TJSExpressionStatement;
  E : TJSLShiftExpression;

begin
  CreateParser('1 << 2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLShiftExpression);
  E:=TJSLShiftExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionRShift;
Var
  X : TJSExpressionStatement;
  E : TJSRShiftExpression;

begin
  CreateParser('1 >> 2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSRShiftExpression);
  E:=TJSRShiftExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionURShift;
Var
  X : TJSExpressionStatement;
  E : TJSURShiftExpression;

begin
  CreateParser('1 >>> 2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSURShiftExpression);
  E:=TJSURShiftExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSLiteral);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Expression left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression left operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
  AssertNotNull('Expression right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Expression left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 2.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPostPlusPlus;
Var
  X : TJSExpressionStatement;
  E : TJSUnaryPostPlusPlusExpression;

begin
  CreateParser('1++;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSUnaryPostPlusPlusExpression);
  E:=TJSUnaryPostPlusPlusExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Expression  operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPostMinusMinus;
Var
  X : TJSExpressionStatement;
  E : TJSUnaryPostMinusMinusExpression;

begin
  CreateParser('1--;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSUnaryPostMinusMinusExpression);
  E:=TJSUnaryPostMinusMinusExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Expression  operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrePlusPlus;

Var
  X : TJSExpressionStatement;
  E : TJSUnaryPrePlusPlusExpression;

begin
  CreateParser('++1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSUnaryPrePlusPlusExpression);
  E:=TJSUnaryPrePlusPlusExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Expression  operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
end;


procedure TTestJSParser.TestExpressionPreMinusMinus;

Var
  X : TJSExpressionStatement;
  E : TJSUnaryPreMinusMinusExpression;

begin
  CreateParser('--1;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSUnaryPreMinusMinusExpression);
  E:=TJSUnaryPreMinusMinusExpression(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Expression  operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Expression operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression operand value correct', 1.0, TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulPlus;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;
  R : TJSMultiplicativeExpressionMul;
begin
  CreateParser('2 * 3 + 4;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSMultiplicativeExpressionMul);
  R:=TJSMultiplicativeExpressionMul(E.A);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  AssertNotNull('Multiplication right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Multiplication right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Multiplication right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Expression right operand value correct', 4.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceBraceMulPlus;

Var
  X : TJSExpressionStatement;
  E : TJSMultiplicativeExpressionMul;
  R : TJSAdditiveExpressionPlus;

begin
  CreateParser('2 * (3 + 4);');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSAdditiveExpressionPlus);
  R:=TJSAdditiveExpressionPlus(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  AssertNotNull('Multiplication right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(E.A).Value.AsNumber);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication right operand value correct', 3.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(R.B).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Addition right operand value correct', 4.0,TJSLiteral(R.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceBracePlusMul;

Var
  X : TJSExpressionStatement;
  E : TJSMultiplicativeExpressionMul;
  R : TJSAdditiveExpressionPlus;

begin
  CreateParser('(3 + 4)*2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSAdditiveExpressionPlus);
  R:=TJSAdditiveExpressionPlus(E.A);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  AssertNotNull('Multiplication right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(E.B).Value.AsNumber);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication right operand value correct', 3.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(R.B).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Addition right operand value correct', 4.0,TJSLiteral(R.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionFunction;
Var
  X : TJSExpressionStatement;
  A  : TJSSimpleAssignStatement;
begin
  CreateParser('a = function () {};');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSSimpleAssignStatement);
  A:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Have left operand',A.LHS);
  CheckClass(A.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Correct name for assignment LHS ','a',TJSPrimaryExpressionIdent(A.LHS).Name);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusMul;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;
  R : TJSMultiplicativeExpressionMul;
begin
  CreateParser('4 + 2 * 3;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSMultiplicativeExpressionMul);
  R:=TJSMultiplicativeExpressionMul(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  AssertNotNull('Multiplication right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Multiplication right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Multiplication right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulMinus;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionMinus;
  R : TJSMultiplicativeExpressionMul;

begin
  CreateParser('2 * 3 - 4;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionMinus);
  E:=TJSAdditiveExpressionMinus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSMultiplicativeExpressionMul);
  R:=TJSMultiplicativeExpressionMul(E.A);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  AssertNotNull('Multiplication right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Multiplication right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Multiplication right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('subtraction right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('subtraction right operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('subtraction right operand value correct', 4.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMinusMul;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionMinus;
  R : TJSMultiplicativeExpressionMul;
begin
  CreateParser('4 - 2 * 3;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionMinus);
  E:=TJSAdditiveExpressionMinus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSMultiplicativeExpressionMul);
  R:=TJSMultiplicativeExpressionMul(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  AssertNotNull('Multiplication right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Multiplication right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Multiplication right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Subtraction left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Subtraction left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Subtraction left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceDivPlus;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;
  R : TJSMultiplicativeExpressionDiv;
begin
  CreateParser('2 / 3 + 4;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSMultiplicativeExpressionDiv);
  R:=TJSMultiplicativeExpressionDiv(E.A);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('Div left operand assigned',R.A);
  AssertNotNull('Div right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Div left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Div left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Div right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Div right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Addition right operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Addition right operand value correct', 4.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusDiv;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;
  R : TJSMultiplicativeExpressionDiv;

begin
  CreateParser('4 + 2 / 3;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSMultiplicativeExpressionDiv);
  R:=TJSMultiplicativeExpressionDiv(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Div left operand assigned',R.A);
  AssertNotNull('Div right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Div left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Div left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Div right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Div right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceModPlus;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;
  R : TJSMultiplicativeExpressionMod;
begin
  CreateParser('2 % 3 + 4;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.A,TJSMultiplicativeExpressionMod);
  R:=TJSMultiplicativeExpressionMod(E.A);
  CheckClass(E.B,TJSLiteral);
  AssertNotNull('mod left operand assigned',R.A);
  AssertNotNull('mod right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('mod left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('mod left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('mod right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('mod right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.B).Value);
  AssertEquals('Addition right operand type correct', jstNumber, TJSLiteral(E.B).Value.ValueType);
  AssertEquals('Addition right operand value correct', 4.0,TJSLiteral(E.B).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusMod;

Var
  X : TJSExpressionStatement;
  E : TJSAdditiveExpressionPlus;
  R : TJSMultiplicativeExpressionMod;

begin
  CreateParser('4 + 2 % 3;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSMultiplicativeExpressionMod);
  R:=TJSMultiplicativeExpressionMod(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Mod left operand assigned',R.A);
  AssertNotNull('Mod right operand assigned',R.B);
  CheckClass(R.A,TJSLiteral);
  CheckClass(R.B,TJSLiteral);
  AssertEquals('Mod left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Mod left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertEquals('Mod right operand type correct', jstNumber, TJSLiteral(R.B).Value.ValueType);
  AssertEquals('Mod right operand value correct', 3.0, TJSLiteral(R.B).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusPostPlusPlus;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryPostPlusPlusExpression;
  E : TJSAdditiveExpressionPlus;

begin
  CreateParser('4 + 2++;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPostPlusPlusExpression);
  R:=TJSUnaryPostPlusPlusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('++ operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('++ operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('++ operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusPostMinusMinus;
Var
  X : TJSExpressionStatement;
  R : TJSUnaryPostMinusMinusExpression;
  E : TJSAdditiveExpressionPlus;

begin
  CreateParser('4 + 2--;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPostMinusMinusExpression);
  R:=TJSUnaryPostMinusMinusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('-- operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('-- operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('-- operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulPostPlusPlus;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryPostPlusPlusExpression;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('4 * 2++;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPostPlusPlusExpression);
  R:=TJSUnaryPostPlusPlusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('++operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('++ operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('++ operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Multiplication left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulPostMinusMinus;
Var
  X : TJSExpressionStatement;
  R : TJSUnaryPostMinusMinusExpression;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('4 * 2--;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPostMinusMinusExpression);
  R:=TJSUnaryPostMinusMinusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('-- operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('-- operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('-- operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Multiplication left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusPrePlusPlus;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryPrePlusPlusExpression;
  E : TJSAdditiveExpressionPlus;

begin
  CreateParser('4 + ++2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPrePlusPlusExpression);
  R:=TJSUnaryPrePlusPlusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('++ operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('++ operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusInv;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryInvExpression;
  E : TJSAdditiveExpressionPlus;

begin
    CreateParser('4 + ~2;');
    X:=GetExpressionStatement;
    AssertNotNull('Expression statement assigned',X.A);
    CheckClass(X.A,TJSAdditiveExpressionPlus);
    E:=TJSAdditiveExpressionPlus(X.A);
    AssertNotNull('Expression left operand assigned',E.A);
    AssertNotNull('Expression right operand assigned',E.B);
    CheckClass(E.B,TJSUnaryInvExpression);
    R:=TJSUnaryInvExpression(E.B);
    CheckClass(E.A,TJSLiteral);
    AssertNotNull('Multiplication left operand assigned',R.A);
    CheckClass(R.A,TJSLiteral);
    AssertEquals('inv operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
    AssertEquals('inv operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
    AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
    AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
    AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulInv;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryInvExpression;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('4 * ~2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryInvExpression);
  R:=TJSUnaryInvExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('Inv operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Inv operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Multiplication left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusNot;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryNotExpression;
  E : TJSAdditiveExpressionPlus;

begin
    CreateParser('4 + !2;');
    X:=GetExpressionStatement;
    AssertNotNull('Expression statement assigned',X.A);
    CheckClass(X.A,TJSAdditiveExpressionPlus);
    E:=TJSAdditiveExpressionPlus(X.A);
    AssertNotNull('Expression left operand assigned',E.A);
    AssertNotNull('Expression right operand assigned',E.B);
    CheckClass(E.B,TJSUnaryNotExpression);
    R:=TJSUnaryNotExpression(E.B);
    CheckClass(E.A,TJSLiteral);
    AssertNotNull('Multiplication left operand assigned',R.A);
    CheckClass(R.A,TJSLiteral);
    AssertEquals('Not operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
    AssertEquals('Not operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
    AssertNotNull('Addition left operand value assigned',TJSLiteral(E.A).Value);
    AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
    AssertEquals('Addition left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestFunctionCallNoArgs;

Var
  X : TJSExpressionStatement;
  C : TJSCallExpression;

begin
  CreateParser('abc();');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSCallExpression);
  C:=TJSCallExpression(X.A);
  AssertEquals('No arguments',0,C.Args.Elements.Count);
  AssertNotNull('Call function expression',C.Expr);
  CheckClass(C.Expr,TJSPrimaryExpressionIdent);
  AssertEquals('Function name correct','abc',TJSPrimaryExpressionIdent(C.Expr).Name);
end;

procedure TTestJSParser.TestAwaitFunctionCallNoArgs;

Var
  X : TJSExpressionStatement;
  W : TJSAwaitExpression;
  C : TJSCallExpression;

begin
  CreateParser('await abc();',MinAwaitVersion);
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSAwaitExpression);
  W:=TJSAwaitExpression(X.A);
  CheckClass(W.A,TJSCallExpression);
  C:=TJSCallExpression(W.A);
  AssertEquals('No arguments',0,C.Args.Elements.Count);
  AssertNotNull('Call function expression',C.Expr);
  CheckClass(C.Expr,TJSPrimaryExpressionIdent);
  AssertEquals('Function name correct','abc',TJSPrimaryExpressionIdent(C.Expr).Name);
end;


procedure TTestJSParser.TestFunctionCallOneArg;

Var
  X : TJSExpressionStatement;
  C : TJSCallExpression;
  E : TJSelement;

begin
  CreateParser('abc(d);');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSCallExpression);
  C:=TJSCallExpression(X.A);
  AssertNotNull('Call function expression',C.Expr);
  CheckClass(C.Expr,TJSPrimaryExpressionIdent);
  AssertEquals('Function name correct','abc',TJSPrimaryExpressionIdent(C.Expr).Name);
  AssertEquals('1 argument',1,C.Args.Elements.Count);
  E:=C.Args.Elements[0].Expr;
  AssertNotNull('First argument expression',E);
  CheckClass(E,TJSPrimaryExpressionIdent);
  AssertEquals('First argument name correct','d',TJSPrimaryExpressionIdent(E).Name);
end;

procedure TTestJSParser.TestFunctionCallTwoArgs;

Var
  X : TJSExpressionStatement;
  C : TJSCallExpression;
  E : TJSelement;

begin
  CreateParser('abc(d,e);');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSCallExpression);
  C:=TJSCallExpression(X.A);
  AssertNotNull('Call function expression',C.Expr);
  CheckClass(C.Expr,TJSPrimaryExpressionIdent);
  AssertEquals('Function name correct','abc',TJSPrimaryExpressionIdent(C.Expr).Name);
  AssertEquals('2 arguments',2,C.Args.Elements.Count);
  E:=C.Args.Elements[0].Expr;
  AssertNotNull('First argument expression',E);
  CheckClass(E,TJSPrimaryExpressionIdent);
  AssertEquals('First argument name correct','d',TJSPrimaryExpressionIdent(E).Name);
  E:=C.Args.Elements[1].Expr;
  AssertNotNull('Second argument expression',E);
  CheckClass(E,TJSPrimaryExpressionIdent);
  AssertEquals('Second argument name correct','e',TJSPrimaryExpressionIdent(E).Name);
end;

procedure TTestJSParser.TestObjectGeneratorFunction;
Var
    X : TJSExpressionStatement;
    SA : TJSSimpleAssignStatement;
    Obj : TJSObjectLiteral;

begin
  CreateParser('a = {* g() { } };',MinGeneratorVersion);
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSSimpleAssignStatement);
  SA:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Assignment LHS assigned',SA.LHS);
  CheckClass(SA.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Expression LHS name correct', 'a',TJSPrimaryExpressionIdent(SA.LHS).Name);
  AssertNotNull('Assignment Expression assigned',SA.Expr);
  AssertEquals('Assignment Expression assigned',TJSObjectLiteral,SA.Expr.ClassType);
  Obj:=TJSObjectLiteral(SA.Expr);
  AssertEquals('Object element count',1,Obj.Elements.Count);
  AssertEquals('Object element name','g',Obj.Elements[0].Name);
  AssertEquals('Object element expression',TJSFunctionDeclarationStatement,Obj.Elements[0].Expr.ClassType);
  AssertTrue('Generator',TJSFunctionDeclarationStatement(Obj.Elements[0].Expr).IsGenerator);
end;

procedure TTestJSParser.TestArrayExpressionNumericalArgs;
Var
  X : TJSExpressionStatement;
  B : TJSBracketMemberExpression;

begin
  CreateParser('A[1];');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSBracketMemberExpression);
  B:=TJSBracketMemberExpression(X.A);
  CheckClass(B.Name,TJSLiteral);
  AssertEquals('Member name operand type correct', jstNumber, TJSLiteral(B.Name).Value.ValueType);
  AssertEquals('Member name operand value correct', 1.0, TJSLiteral(B.Name).Value.AsNumber);
  CheckClass(B.Mexpr,TJSPrimaryExpressionIdent);
  AssertEquals('Array name correct','A',TJSPrimaryExpressionIdent(B.Mexpr).Name);
end;

procedure TTestJSParser.TestArrayExpressionStringArgs;
Var
  X : TJSExpressionStatement;
  B : TJSBracketMemberExpression;

begin
  CreateParser('A["propname"];');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSBracketMemberExpression);
  B:=TJSBracketMemberExpression(X.A);
  CheckClass(B.Name,TJSLiteral);
  AssertEquals('Member name operand type correct', jstString, TJSLiteral(B.Name).Value.ValueType);
  AssertEquals('Member name operand value correct', 'propname', TJSLiteral(B.Name).Value.AsString);
  CheckClass(B.Mexpr,TJSPrimaryExpressionIdent);
  AssertEquals('Array name correct','A',TJSPrimaryExpressionIdent(B.Mexpr).Name);
end;

procedure TTestJSParser.TestArrayExpressionIdentArgs;

Var
  X : TJSExpressionStatement;
  B : TJSBracketMemberExpression;

begin
  CreateParser('A[B];');
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSBracketMemberExpression);
  B:=TJSBracketMemberExpression(X.A);
  CheckClass(B.Name,TJSPrimaryExpressionIdent);
  AssertEquals('Member name identifier correct', 'B', TJSPrimaryExpressionIdent(B.Name).Name);
  CheckClass(B.Mexpr,TJSPrimaryExpressionIdent);
  AssertEquals('Array name correct','A',TJSPrimaryExpressionIdent(B.Mexpr).Name);
end;

Procedure TTestJSParser.TestVarDeclarationSimple;

Var
  X : TJSELement;
  V : TJSVarDeclaration;
begin
  CreateParser('var a;');
  X:=GetFirstVar;
  AssertNotNull('Variable statement assigned',(X));
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
  AssertEquals('correct variable type', vtVar, V.VarType);
  AssertEquals('variable name correct registered', 'a', V.Name);
  AssertNull('No initialization expression', V.Init);
end;

procedure TTestJSParser.TestVarDeclarationInit;
Var
  X : TJSELement;
  V : TJSVarDeclaration;
begin
  CreateParser('var a = 0;');
  X:=GetFirstVar;
  AssertNotNull('Variable statement assigned',(X));
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
  AssertEquals('correct variable type', vtVar, V.VarType);
  AssertEquals('variable name correct registered', 'a', V.Name);
  AssertNotNull('initialization expression', V.Init);
  CheckClass(V.Init,TJSLiteral);
  AssertEquals('Init value correct', 0, TJSLiteral(V.init).Value.AsNumber);
end;

procedure TTestJSParser.TestLetDeclarationSimple;
Var
  X : TJSELement;
  V : TJSVarDeclaration;
begin
  CreateParser('let a;',minLetVersion);
  X:=GetFirstVar;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
  AssertEquals('correct variable type', vtLet, V.VarType);
//  AssertNotNull('Variable statement assigned',(X));
  AssertEquals('variable name correct registered', 'a', V.Name);
  AssertNull('No initialization expression', V.Init);
end;

procedure TTestJSParser.TestVarDeclarationDouble;

Var
  X : TJSELement;
  V : TJSVarDeclaration;

begin
  CreateParser('var a, b ;');
  AssertEquals('2 variables declared',2,GetVars.Count);
  X:=GetFirstVar;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
  AssertEquals('correct variable type', vtVar, V.VarType);
//  AssertNotNull('Variable statement assigned',(X));
  AssertEquals('variable name correct registered', 'a', V.name);
  X:=GetVars.Nodes[1].Node;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
  AssertEquals('correct variable type', vtVar, V.VarType);
  AssertEquals('variable name correct registered', 'b', V.Name);
  AssertNull('No initialization expression', V.Init);
end;

procedure TTestJSParser.TestVarDeclarationSimpleInit;

Var
  X : TJSELement;
  V : TJSVarDeclaration;
begin
  CreateParser('var a = b;');
  X:=GetFirstVar;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
//  AssertNotNull('Variable statement assigned',(X));
  AssertEquals('variable name correct registered', 'a', V.Name);
  AssertNotNull('Initialization expression present', V.Init);
  CheckClass(V.Init,TJSPrimaryExpressionIdent);
  AssertEquals('Member name identifier correct', 'b', TJSPrimaryExpressionIdent(V.init).Name);
end;

procedure TTestJSParser.TestConstDeclarationSimpleInit;
Var
  X : TJSELement;
  V : TJSVarDeclaration;
begin
  CreateParser('const a = 1;',MinLetVersion);
  X:=GetFirstVar;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
//  AssertNotNull('Variable statement assigned',(X));
  AssertEquals('variable name correct registered', 'a', V.Name);
  AssertNotNull('Initialization expression present', V.Init);
  CheckClass(V.Init,TJSLiteral);
  AssertEquals('Expression value type correct', jstNumber,TJSLiteral(V.Init).Value.ValueType);
  AssertEquals('Expression value correct', 1.0, TJSLiteral(V.Init).Value.AsNumber);
end;

procedure TTestJSParser.TestVarDeclarationDoubleInit;

Var
  X : TJSELement;
  V : TJSVarDeclaration;
begin
  CreateParser('var a, c = b;');
  AssertEquals('2 variables declared',2,GetVars.Count);
  X:=GetFirstVar;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
//  AssertNotNull('Variable statement assigned',(X));
  AssertEquals('variable name correct registered', 'a', V.Name);
  AssertNull('No initialization expression', V.Init);
  X:=GetVars.Nodes[1].Node;
  CheckClass(X,TJSVarDeclaration);
  V:=TJSVarDeclaration(X);
  AssertEquals('variable name correct registered', 'c', V.Name);
  AssertNotNull('No initialization expression', V.Init);
  CheckClass(V.Init,TJSPrimaryExpressionIdent);
  AssertEquals('Member name identifier correct', 'b', TJSPrimaryExpressionIdent(V.init).Name);
end;

procedure TTestJSParser.TestDebuggerStatement;
Var
  E : TJSSourceElements;
  X : TJSElement;

begin
  CreateParser('debugger',MinDebuggerVersion);
  E:=GetSourceElements;
  AssertEquals('1 statement in block',1,E.Statements.Count);
  X:=E.Statements.Nodes[0].Node;
  CheckClass(X,TJSDebuggerStatement);
end;

procedure TTestJSParser.TestBlockEmpty;

Var
  E : TJSSourceElements;
  X : TJSElement;

begin
  CreateParser('{}');
  E:=GetSourceElements;
  AssertEquals('1 statement in block',1,E.Statements.Count);
  X:=E.Statements.Nodes[0].Node;
  CheckClass(X,TJSEmptyBlockStatement);
end;

procedure TTestJSParser.TestBlockEmptyStatement;

Var
  E : TJSSourceElements;
  X : TJSElement;

begin
  CreateParser('{;}');
  E:=GetSourceElements;
  AssertEquals('1 statement in block',1,E.Statements.Count);
  X:=E.Statements.Nodes[0].Node;
  CheckClass(X,TJSEmptyStatement);
end;

procedure TTestJSParser.TestBlockSimpleStatement;

Var
  E : TJSSourceElements;
  X : TJSElement;

begin
  CreateParser('{a;}');
  E:=GetSourceElements;
  AssertEquals('1 statement in block',1,E.Statements.Count);
  X:=E.Statements.Nodes[0].Node;
  CheckClass(X,TJSExpressionStatement);
  CheckNotNull(TJSExpressionStatement(X).A);
  CheckClass(TJSExpressionStatement(X).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name','a',TJSPrimaryExpressionIdent(TJSExpressionStatement(X).A).Name)
end;

procedure TTestJSParser.TestFunctionDeclarationEmpty;

Var
  E : TJSSourceElements;
  N : TJSElement;
  FD : TJSFunctionDeclarationStatement;

begin
  CreateParser('function a () {}');
  E:=GetSourceElements;
  AssertEquals('1 function defined',1,E.functions.Count);
  N:=E.Functions.Nodes[0].Node;
  AssertNotNull('Function element defined ',N);
  CheckClass(N,TJSFunctionDeclarationStatement);
  FD:=TJSFunctionDeclarationStatement(N);
  AssertNotNull('Function definition assigned',FD.AFunction);
  AssertFalse('Async function ',FD.AFunction.IsAsync);
  AssertEquals('Function name OK','a',FD.AFunction.Name);
  AssertNotNull('Function body assigned', FD.AFunction.Body);
  AssertEquals('No parameters',0,FD.AFunction.TypedParams.Count);
  N:=FD.AFunction.Body;
  CheckClass(N,TJSFunctionBody);
  AssertNotNull('Function body has element',TJSFunctionBody(N).A);
  CheckClass(TJSFunctionBody(N).A,  TJSSourceElements);
  E:=TJSSourceElements(TJSFunctionBody(N).A);
  AssertEquals('0 statement in functionbody elements',0,E.Statements.Count);
//  TJSEmptyBlockStatement
end;

procedure TTestJSParser.TestFunctionDeclarationAsync;
Var
  E : TJSSourceElements;
  N : TJSElement;
  FD : TJSFunctionDeclarationStatement;

begin
  CreateParser('async function a () {}',MinAsyncVersion);
  E:=GetSourceElements;
  AssertEquals('1 function defined',1,E.functions.Count);
  N:=E.Functions.Nodes[0].Node;
  AssertNotNull('Function element defined ',N);
  CheckClass(N,TJSFunctionDeclarationStatement);
  FD:=TJSFunctionDeclarationStatement(N);
  AssertNotNull('Function definition assigned',FD.AFunction);
  AssertTrue('Async function ',FD.AFunction.IsAsync);
  AssertEquals('Function name OK','a',FD.AFunction.Name);
  AssertNotNull('Function body assigned', FD.AFunction.Body);
  AssertEquals('No parameters',0,FD.AFunction.TypedParams.Count);
  N:=FD.AFunction.Body;
  CheckClass(N,TJSFunctionBody);
  AssertNotNull('Function body has element',TJSFunctionBody(N).A);
  CheckClass(TJSFunctionBody(N).A,  TJSSourceElements);
  E:=TJSSourceElements(TJSFunctionBody(N).A);
  AssertEquals('0 statement in functionbody elements',0,E.Statements.Count);
end;

procedure TTestJSParser.TestFunctionDeclarationWithArgs;

Var
  E : TJSSourceElements;
  N : TJSElement;
  FD : TJSFunctionDeclarationStatement;

begin
  CreateParser('function a (b,c) {}');
  E:=GetSourceElements;
  AssertEquals('1 function defined',1,E.functions.Count);
  N:=E.Functions.Nodes[0].Node;
  AssertNotNull('Function element defined ',N);
  CheckClass(N,TJSFunctionDeclarationStatement);
  FD:=TJSFunctionDeclarationStatement(N);
  AssertNotNull('Function definition assigned',FD.AFunction);
  AssertEquals('Function name OK','a',FD.AFunction.Name);
  AssertNotNull('Function body assigned', FD.AFunction.Body);
  AssertEquals('2 parameters',2,FD.AFunction.TypedParams.Count);
  AssertEquals('1st parameter','b',FD.AFunction.TypedParams[0].Name);
  AssertEquals('2nd parameter','c',FD.AFunction.TypedParams[1].Name);
  N:=FD.AFunction.Body;
  CheckClass(N,TJSFunctionBody);
  AssertNotNull('Function body has element',TJSFunctionBody(N).A);
  CheckClass(TJSFunctionBody(N).A,  TJSSourceElements);
  E:=TJSSourceElements(TJSFunctionBody(N).A);
  AssertEquals('0 statement in functionbody elements',0,E.Statements.Count);
//  TJSEmptyBlockStatement
end;

procedure TTestJSParser.TestFunctionDeclarationWithSpreadArgs;
Var
  E : TJSSourceElements;
  N : TJSElement;
  FD : TJSFunctionDeclarationStatement;

begin
  CreateParser('function a (...b,c) {}');
  E:=GetSourceElements;
  AssertEquals('1 function defined',1,E.functions.Count);
  N:=E.Functions.Nodes[0].Node;
  AssertNotNull('Function element defined ',N);
  CheckClass(N,TJSFunctionDeclarationStatement);
  FD:=TJSFunctionDeclarationStatement(N);
  AssertNotNull('Function definition assigned',FD.AFunction);
  AssertEquals('Function name OK','a',FD.AFunction.Name);
  AssertNotNull('Function body assigned', FD.AFunction.Body);
  AssertEquals('2 parameters',2,FD.AFunction.TypedParams.Count);
  AssertEquals('1st parameter','b',FD.AFunction.TypedParams[0].Name);
  AssertEquals('1st parameter','b',FD.AFunction.TypedParams.Names[0]);
  AssertTrue('1st parameter spread',FD.AFunction.TypedParams.Params[0].IsSpread);
  AssertEquals('2nd parameter','c',FD.AFunction.TypedParams[1].Name);
  N:=FD.AFunction.Body;
  CheckClass(N,TJSFunctionBody);
  AssertNotNull('Function body has element',TJSFunctionBody(N).A);
  CheckClass(TJSFunctionBody(N).A,  TJSSourceElements);
  E:=TJSSourceElements(TJSFunctionBody(N).A);
  AssertEquals('0 statement in functionbody elements',0,E.Statements.Count);
//  TJSEmptyBlockStatement
end;

procedure TTestJSParser.TestFunctionDeclarationWithBody;

Var
  E : TJSSourceElements;
  N : TJSElement;
  FD : TJSFunctionDeclarationStatement;

begin
  CreateParser('function a () { b; }');
  E:=GetSourceElements;
  AssertEquals('1 function defined',1,E.functions.Count);
  N:=E.Functions.Nodes[0].Node;
  AssertNotNull('Function element defined ',N);
  CheckClass(N,TJSFunctionDeclarationStatement);
  FD:=TJSFunctionDeclarationStatement(N);
  AssertNotNull('Function definition assigned',FD.AFunction);
  AssertEquals('Function name OK','a',FD.AFunction.Name);
  AssertNotNull('Function body assigned', FD.AFunction.Body);
  AssertEquals('2 parameters',0,FD.AFunction.TypedParams.Count);
  N:=FD.AFunction.Body;
  CheckClass(N,TJSFunctionBody);
  AssertNotNull('Function body has element',TJSFunctionBody(N).A);
  CheckClass(TJSFunctionBody(N).A,  TJSSourceElements);
  E:=TJSSourceElements(TJSFunctionBody(N).A);
  AssertEquals('1 statement in functionbody elements',1,E.Statements.Count);
  N:=E.Statements.Nodes[0].Node;
  CheckClass(N,TJSExpressionStatement);
  CheckNotNull(TJSExpressionStatement(N).A);
  CheckClass(TJSExpressionStatement(N).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(N).A).Name);
//  TJSEmptyBlockStatement
end;

procedure TTestJSParser.TestIfSimple;

Var
  E : TJSElement;
  I : TJSIfStatement;

begin
  CreateParser('if (a) b;');
  E:=GetFirstStatement;
  CheckClass(E,TJSIfStatement);
  I:=TJSIfStatement(E);
  AssertNotNull('Statement condition assigned',I.Cond);
  CheckClass(I.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(I.Cond).Name);
  AssertNull('Statement false branch assigned',I.BFalse);
  AssertNotNull('Statement true branch assigned',I.Btrue);
  CheckClass(I.Btrue,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(I.BTrue).A);
  CheckClass(TJSExpressionStatement(I.BTrue).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 2','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(I.Btrue).A).Name);
end;

procedure TTestJSParser.TestIfEmptyBlock;

Var
  E : TJSElement;
  I : TJSIfStatement;

begin
  CreateParser('if (a) {}');
  E:=GetFirstStatement;
  CheckClass(E,TJSIfStatement);
  I:=TJSIfStatement(E);
  AssertNotNull('Statement condition assigned',I.Cond);
  CheckClass(I.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(I.Cond).Name);
  AssertNull('Statement false branch assigned',I.BFalse);
  AssertNotNull('Statement true branch assigned',I.Btrue);
  CheckClass(I.Btrue,TJSEmptyBlockStatement);
end;

procedure TTestJSParser.TestIfEmptyBlockElse;

Var
  E : TJSElement;
  I : TJSIfStatement;

begin
  CreateParser('if (a) {} else b;');
  E:=GetFirstStatement;
  CheckClass(E,TJSIfStatement);
  I:=TJSIfStatement(E);
  AssertNotNull('Statement condition assigned',I.Cond);
  CheckClass(I.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(I.Cond).Name);
  AssertNotNull('Statement false branch assigned',I.BFalse);
  AssertNotNull('Statement true branch assigned',I.Btrue);
  CheckClass(I.Btrue,TJSEmptyBlockStatement);
end;

procedure TTestJSParser.TestWhileSimple;
Var
  E : TJSElement;
  W : TJSWhileStatement;

begin
  CreateParser('while (a) b;');
  E:=GetFirstStatement;
  CheckClass(E,TJSWhileStatement);
  W:=TJSWhileStatement(E);
  AssertNotNull('Statement condition assigned',W.Cond);
  CheckClass(W.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(W.Cond).Name);
  AssertNotNull('Statement condition assigned',W.body);
  CheckClass(W.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(W.Body).A);
  CheckClass(TJSExpressionStatement(W.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(W.Body).A).Name);
end;

procedure TTestJSParser.TestWhileBlock;

Var
  E : TJSElement;
  W : TJSWhileStatement;
//  B : TJSBlockStatement;

begin
  CreateParser('while (a) {b;}');
  E:=GetFirstStatement;
  CheckClass(E,TJSWhileStatement);
  W:=TJSWhileStatement(E);
  AssertNotNull('Statement condition assigned',W.Cond);
  CheckClass(W.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(W.Cond).Name);
  AssertNotNull('Statement condition assigned',W.body);
  CheckClass(W.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(W.Body).A);
  CheckClass(TJSExpressionStatement(W.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(W.Body).A).Name);
end;

procedure TTestJSParser.TestDoWhileSimple;

Var
  E : TJSElement;
  W : TJSDoWhileStatement;
//  B : TJSBlockStatement;

begin
  CreateParser('do b; while (a);');
  E:=GetFirstStatement;
  CheckClass(E,TJSDoWhileStatement);
  W:=TJSDoWhileStatement(E);
  AssertNotNull('Statement condition assigned',W.Cond);
  CheckClass(W.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(W.Cond).Name);
  AssertNotNull('Statement condition assigned',W.body);
  CheckClass(W.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(W.Body).A);
  CheckClass(TJSExpressionStatement(W.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(W.Body).A).Name);
end;

procedure TTestJSParser.TestDoWhileBlock;

Var
  E : TJSElement;
  W : TJSDoWhileStatement;
//  B : TJSBlockStatement;

begin
  CreateParser('do {b;} while (a);');
  E:=GetFirstStatement;
  CheckClass(E,TJSDoWhileStatement);
  W:=TJSDoWhileStatement(E);
  AssertNotNull('Statement condition assigned',W.Cond);
  CheckClass(W.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(W.Cond).Name);
  AssertNotNull('Statement condition assigned',W.body);
  CheckClass(W.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(W.Body).A);
  CheckClass(TJSExpressionStatement(W.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(W.Body).A).Name);
end;

procedure TTestJSParser.TestForEmpty;

Var
  E : TJSElement;
  F : TJSForStatement;

begin
  CreateParser('for (;;) a;');
  E:=GetFirstStatement;
  CheckClass(E,TJSForStatement);
  F:=TJSForStatement(E);
  AssertNull('Statement condition not assigned',F.Cond);
  AssertNull('Statement init not assigned',F.Init);
  AssertNull('Statement step not assigned',F.Incr);
  CheckClass(F.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(F.Body).A);
  CheckClass(TJSExpressionStatement(F.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(TJSExpressionStatement(F.Body).A).Name);
end;

procedure TTestJSParser.TestForEmptyBody;

Var
  E : TJSElement;
  F : TJSForStatement;

begin
  CreateParser('for (;;) {a;}');
  E:=GetFirstStatement;
  CheckClass(E,TJSForStatement);
  F:=TJSForStatement(E);
  AssertNull('Statement condition not assigned',F.Cond);
  AssertNull('Statement init not assigned',F.Init);
  AssertNull('Statement step not assigned',F.Incr);
  CheckClass(F.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(F.Body).A);
  CheckClass(TJSExpressionStatement(F.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(TJSExpressionStatement(F.Body).A).Name);
end;

procedure TTestJSParser.TestForSimpleBody;

Var
  E : TJSElement;
  F : TJSForStatement;

begin
  CreateParser('for (a;b;c) {d;}');
  E:=GetFirstStatement;
  CheckClass(E,TJSForStatement);
  F:=TJSForStatement(E);
  AssertNotNull('Statement condition not assigned',F.Cond);
  AssertNotNull('Statement init not assigned',F.Init);
  AssertNotNull('Statement step not assigned',F.Incr);
  CheckClass(F.Init,TJSPrimaryExpressionIdent);
  AssertNotNull('Expression statement expression',TJSPrimaryExpressionIdent(F.Init));
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(F.Init).Name);
  CheckClass(F.Incr,TJSPrimaryExpressionIdent);
  AssertNotNull('Expression statement expression',TJSPrimaryExpressionIdent(F.Incr));
  AssertEquals('Name 2','c',TJSPrimaryExpressionIdent(F.Incr).Name);
  CheckClass(F.Cond,TJSPrimaryExpressionIdent);
  AssertNotNull('Expression statement expression',TJSPrimaryExpressionIdent(F.Cond));
  AssertEquals('Name 3','b',TJSPrimaryExpressionIdent(F.cond).Name);
  CheckClass(F.Body,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(F.Body).A);
  CheckClass(TJSExpressionStatement(F.Body).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 4','d',TJSPrimaryExpressionIdent(TJSExpressionStatement(F.Body).A).Name);
end;

procedure TTestJSParser.TestTryCatch;

Var
  E : TJSElement;
  T : TJSTryCatchStatement;

begin
  CreateParser('try {a;} catch (e) {b;}');
  E:=GetFirstStatement;
  CheckClass(E,TJSTryCatchStatement);
  T:=TJSTryCatchStatement(E);
  CheckClass(T.Block,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(T.Block).A);
  CheckClass(TJSExpressionStatement(T.Block).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.Block).A).Name);
  CheckClass(T.BCatch,TJSExpressionStatement);
  AssertEquals('Except object identifier name','e',T.Ident);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(T.BCatch).A);
  CheckClass(TJSExpressionStatement(T.BCatch).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 2','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.BCatch).A).Name);
  AssertNull('No Finally expression',T.BFinally);
end;

procedure TTestJSParser.TestTryCatchFinally;

Var
  E : TJSElement;
  T : TJSTryCatchFinallyStatement;

begin
  CreateParser('try {a;} catch (e) {b;} finally {c;}');
  E:=GetFirstStatement;
  CheckClass(E,TJSTryCatchFinallyStatement);
  T:=TJSTryCatchFinallyStatement(E);
  CheckClass(T.Block,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(T.Block).A);
  CheckClass(TJSExpressionStatement(T.Block).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.Block).A).Name);
  AssertEquals('Except object identifier name','e',T.Ident);
  CheckClass(T.BCatch,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(T.BCatch).A);
  CheckClass(TJSExpressionStatement(T.BCatch).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 2','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.BCatch).A).Name);
  AssertNotNull('Finally expression',T.BFinally);
  CheckClass(T.BFinally,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(T.BFinally).A);
  CheckClass(TJSExpressionStatement(T.BFinally).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 3','c',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.BFinally).A).Name);
end;

procedure TTestJSParser.TestTryFinally;

Var
  E : TJSElement;
  T : TJSTryFinallyStatement;

begin
  CreateParser('try {a;} finally {c;}');
  E:=GetFirstStatement;
  CheckClass(E,TJSTryFinallyStatement);
  T:=TJSTryFinallyStatement(E);
  CheckClass(T.Block,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(T.Block).A);
  CheckClass(TJSExpressionStatement(T.Block).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.Block).A).Name);
  AssertNull('No catch',T.BCatch);
  AssertNotNull('Finally expression',T.BFinally);
  AssertNotNull('Finally expression',TJSExpressionStatement(T.BFinally).A);
  CheckClass(TJSExpressionStatement(T.BFinally).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 2','c',TJSPrimaryExpressionIdent(TJSExpressionStatement(T.BFinally).A).Name);
end;

procedure TTestJSParser.TestThrow;
Var
  E : TJSElement;
  T : TJSThrowStatement;

begin
  CreateParser('throw a;');
  E:=GetFirstStatement;
  CheckClass(E,TJSThrowStatement);
  T:=TJSThrowStatement(E);
  AssertNotNull('Have throw object',T.A);
  CheckClass(T.A,TJSPrimaryExpressionIdent);
  AssertEquals('Correct identifier','a',TJSPrimaryExpressionIdent(T.A).Name);
end;

procedure TTestJSParser.TestReturn;

Var
  E : TJSSourceElements;
  N : TJSElement;
  FD : TJSFunctionDeclarationStatement;

begin
  CreateParser('function a () { return b; }');
  E:=GetSourceElements;
  AssertEquals('1 function defined',1,E.functions.Count);
  N:=E.Functions.Nodes[0].Node;
  AssertNotNull('Function element defined ',N);
  CheckClass(N,TJSFunctionDeclarationStatement);
  FD:=TJSFunctionDeclarationStatement(N);
  AssertNotNull('Function definition assigned',FD.AFunction);
  AssertEquals('Function name OK','a',FD.AFunction.Name);
  AssertNotNull('Function body assigned', FD.AFunction.Body);
  AssertEquals('No parameters',0,FD.AFunction.TypedParams.Count);
  N:=FD.AFunction.Body;
  CheckClass(N,TJSFunctionBody);
  AssertNotNull('Function body has element',TJSFunctionBody(N).A);
  CheckClass(TJSFunctionBody(N).A,  TJSSourceElements);
  E:=TJSSourceElements(TJSFunctionBody(N).A);
  AssertEquals('1 statement in functionbody elements',1,E.Statements.Count);
end;

procedure TTestJSParser.TestAssignment;
Var
  E : TJSElement;
  ES : TJSExpressionStatement;
  A : TJSSimpleAssignStatement;

begin
  CreateParser('a=b;');
  E:=GetFirstStatement;
  CheckClass(E,TJSExpressionStatement);
  ES:=TJSExpressionStatement(E);
  CheckClass(ES.A,TJSSimpleAssignStatement);
  A:=TJSSimpleAssignStatement(ES.A);
  AssertNotNull('Have LHS',A.LHS);
  CheckClass(A.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Have LHS name','a',TJSPrimaryExpressionIdent(A.LHS).Name);
  CheckClass(A.Expr,TJSPrimaryExpressionIdent);
  AssertEquals('Have RHS name','b',TJSPrimaryExpressionIdent(A.Expr).Name);
end;

procedure TTestJSParser.TestNew;
Var
  E : TJSElement;
  ES : TJSExpressionStatement;
  A : TJSSimpleAssignStatement;
  N : TJSNewMemberExpression;
  L : TJSLiteral;

begin
  CreateParser('a = new b(123)');
  E:=GetFirstStatement;
  CheckClass(E,TJSExpressionStatement);
  ES:=TJSExpressionStatement(E);
  CheckClass(ES.A,TJSSimpleAssignStatement);
  A:=TJSSimpleAssignStatement(ES.A);
  CheckClass(A.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Have LHS name','a',TJSPrimaryExpressionIdent(A.LHS).Name);
  CheckClass(A.Expr,TJSNewMemberExpression);
  N:=TJSNewMemberExpression(A.Expr);
  AssertNotNull('Have LHS name',N.Mexpr);
  CheckClass(N.Mexpr,TJSPrimaryExpressionIdent);
  AssertEquals('Have LHS name','b',TJSPrimaryExpressionIdent(N.Mexpr).Name);
  AssertNotNull('Have arguments',N.Args);
  AssertEquals('One argument',1,N.Args.Elements.Count);
  AssertNotNull('Have argument 0',N.Args.Elements[0].Expr);
  CheckClass(N.Args.Elements[0].Expr,TJSLiteral);
  L:=TJSLiteral(N.Args.Elements[0].Expr);
  AssertNotNull('Expression value assigned',L.Value);
  AssertEquals('Expression value type correct', jstNumber,L.Value.ValueType);
  AssertEquals('Expression value correct', 123,L.Value.AsNumber);

end;

procedure TTestJSParser.TestLabeledStatement;
Var
  E : TJSElement;
  ES : TJSExpressionStatement;
  A : TJSSimpleAssignStatement;
  N : TJSNewMemberExpression;
  L : TJSLiteral;
  LS : TJSLabeledStatement;

begin
  CreateParser('loc: a = new b(123)');
  E:=GetFirstStatement;
  CheckClass(E,TJSLabeledStatement);
  LS:=TJSLabeledStatement(E);
  AssertNotNull('Have label',LS.TheLabel);
  AssertEquals('Have correct label','loc',LS.TheLabel.Name);
  CheckClass(LS.A,TJSExpressionStatement);
  ES:=TJSExpressionStatement(LS.A);
  CheckClass(ES.A,TJSSimpleAssignStatement);
  A:=TJSSimpleAssignStatement(ES.A);
  CheckClass(A.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Have LHS name','a',TJSPrimaryExpressionIdent(A.LHS).Name);
  CheckClass(A.Expr,TJSNewMemberExpression);
  N:=TJSNewMemberExpression(A.Expr);
  AssertNotNull('Have LHS name',N.Mexpr);
  CheckClass(N.Mexpr,TJSPrimaryExpressionIdent);
  AssertEquals('Have LHS name','b',TJSPrimaryExpressionIdent(N.Mexpr).Name);
  AssertNotNull('Have arguments',N.Args);
  AssertEquals('One argument',1,N.Args.Elements.Count);
  AssertNotNull('Have argument 0',N.Args.Elements[0].Expr);
  CheckClass(N.Args.Elements[0].Expr,TJSLiteral);
  L:=TJSLiteral(N.Args.Elements[0].Expr);
  AssertNotNull('Expression value assigned',L.Value);
  AssertEquals('Expression value type correct', jstNumber,L.Value.ValueType);
  AssertEquals('Expression value correct', 123,L.Value.AsNumber);
end;

procedure TTestJSParser.TestContinue;

Var
  E : TJSElement;
  C : TJSContinueStatement;

begin
  CreateParser('while (true) continue;');
  E:=GetFirstStatement;
  CheckClass(E,TJSWhileStatement);
  E:=TJSWhileStatement(E).Body;
  CheckClass(E,TJSContinueStatement);
  C:=TJSContinueStatement(E);
  AssertEquals('Have correct (empty) label','',C.TargetName);
end;

procedure TTestJSParser.TestContinueTarget;

Var
  E : TJSElement;
  C : TJSContinueStatement;

begin
  CreateParser('a: while (true) continue a;');
  E:=GetFirstStatement;
  CheckClass(E,TJSLabeledStatement);
  E:=TJSLabeledStatement(E).A;
  CheckClass(E,TJSWhileStatement);
  E:=TJSWhileStatement(E).Body;
  CheckClass(E,TJSContinueStatement);
  C:=TJSContinueStatement(E);
  AssertEquals('Have correct  label','a',C.TargetName);
end;

procedure TTestJSParser.TestBreakTarget;
Var
  E : TJSElement;
  C : TJSBreakStatement;

begin
  CreateParser('a: while (true) break a;');
  E:=GetFirstStatement;
  CheckClass(E,TJSLabeledStatement);
  E:=TJSLabeledStatement(E).A;
  CheckClass(E,TJSWhileStatement);
  E:=TJSWhileStatement(E).Body;
  CheckClass(E,TJSBreakStatement);
  C:=TJSBreakStatement(E);
  AssertEquals('Have correct  label','a',C.TargetName);
end;

procedure TTestJSParser.TestSwitchEmpty;
Var
  E : TJSElement;
  S : TJSSwitchStatement;
begin
  CreateParser('switch (a) {}');
  E:=GetFirstStatement;
  CheckClass(E,TJSSwitchStatement);
  S:=TJSSwitchStatement(E);
  AssertNotNull('Have condition',S.Cond);
  AssertNull('Have no default',S.TheDefault);
  AssertIdentifier('Case condition',S.Cond,'a');
  S:=TJSSwitchStatement(E);
  AssertEquals('No cases',0,S.Cases.Count)
end;

procedure TTestJSParser.TestSwitchOne;
Var
  E : TJSElement;
  S : TJSSwitchStatement;
  C : TJSCaseElement;
begin
  CreateParser('switch (a) { case c : {}}');
  E:=GetFirstStatement;
  CheckClass(E,TJSSwitchStatement);
  S:=TJSSwitchStatement(E);
  AssertNotNull('Have condition',S.Cond);
  AssertNull('Have no default',S.TheDefault);
  AssertIdentifier('Case condition',S.Cond,'a');
  S:=TJSSwitchStatement(E);
  AssertEquals('1 case',1,S.Cases.Count);
  C:=TJSCaseElement(S.Cases[0]);
  AssertIdentifier('Case expression',C.Expr,'c');
  CheckClass(C.Body,TJSEmptyBlockStatement);
end;

procedure TTestJSParser.TestSwitchTwo;
Var
  E : TJSElement;
  S : TJSSwitchStatement;
  C : TJSCaseElement;
begin
  CreateParser('switch (a) { case c: {}'+sLineBreak+' case d: {}}');
  E:=GetFirstStatement;
  CheckClass(E,TJSSwitchStatement);
  S:=TJSSwitchStatement(E);
  AssertNotNull('Have condition',S.Cond);
  AssertNull('Have no default',S.TheDefault);
  AssertIdentifier('Case condition',S.Cond,'a');
  S:=TJSSwitchStatement(E);
  AssertEquals('2 cases',2,S.Cases.Count);
  C:=TJSCaseElement(S.Cases[0]);
  AssertIdentifier('Case expression',C.Expr,'c');
  CheckClass(C.Body,TJSEmptyBlockStatement);
  C:=TJSCaseElement(S.Cases[1]);
  AssertIdentifier('Case expression',C.Expr,'d');
  CheckClass(C.Body,TJSEmptyBlockStatement);
end;

procedure TTestJSParser.TestSwitchTwoDefault;
Var
  E : TJSElement;
  S : TJSSwitchStatement;
  C : TJSCaseElement;
begin
  CreateParser('switch (a) { case c: {} case d: {} default: {}}');
  E:=GetFirstStatement;
  CheckClass(E,TJSSwitchStatement);
  S:=TJSSwitchStatement(E);
  AssertNotNull('Have condition',S.Cond);
  AssertNotNull('Have default',S.TheDefault);
  AssertIdentifier('Case condition',S.Cond,'a');
  S:=TJSSwitchStatement(E);
  AssertEquals('2 cases',3,S.Cases.Count);
  C:=TJSCaseElement(S.Cases[0]);
  AssertIdentifier('Case expression',C.Expr,'c');
  CheckClass(C.Body,TJSEmptyBlockStatement);
  C:=TJSCaseElement(S.Cases[1]);
  AssertIdentifier('Case expression',C.Expr,'d');
  CheckClass(C.Body,TJSEmptyBlockStatement);
  C:=TJSCaseElement(S.Cases[2]);
  CheckClass(C.Body,TJSEmptyBlockStatement);
  AssertSame('Default',C,S.TheDefault);
end;

procedure TTestJSParser.TestImportModule;

Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;

begin
  CreateParser('import "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertFalse('Named imports',Imp.HaveNamedImports);
end;

procedure TTestJSParser.TestImportImportedDefault;

Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;

begin
  CreateParser('import A from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','A',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertFalse('Named imports',Imp.HaveNamedImports);
end;

procedure TTestJSParser.TestImportNamespaceImport;
Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;

begin
  CreateParser('import * as A from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','A',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertFalse('Named imports',Imp.HaveNamedImports);
end;

procedure TTestJSParser.TestImportImportedDefaultAndNamespaceImport;
Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;

begin
  CreateParser('import A, * as B from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','A',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','B',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertFalse('Named imports',Imp.HaveNamedImports);
end;

procedure TTestJSParser.TestImportNamedImport;

Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;
  NamedImp : TJSNamedImportElement;

begin
  CreateParser('import {A} from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertTrue('Named imports',Imp.HaveNamedImports);
  AssertEquals('Named import count',1,Imp.NamedImports.Count);
  NamedImp:=Imp.NamedImports[0];
  AssertEquals('Named import name','A',NamedImp.Name);
  AssertEquals('Named import alias','',NamedImp.Alias);
end;

procedure TTestJSParser.TestImportNamedImportAlias;
Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;
  NamedImp : TJSNamedImportElement;

begin
  CreateParser('import {A as C} from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertTrue('Named imports',Imp.HaveNamedImports);
  AssertEquals('Named import count',1,Imp.NamedImports.Count);
  NamedImp:=Imp.NamedImports[0];
  AssertEquals('Named import name','A',NamedImp.Name);
  AssertEquals('Named import alias','C',NamedImp.Alias);
end;

procedure TTestJSParser.TestImport2NamedImports;

Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;
  NamedImp : TJSNamedImportElement;

begin
  CreateParser('import {A, B} from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertTrue('Named imports',Imp.HaveNamedImports);
  AssertEquals('Named import count',2,Imp.NamedImports.Count);
  NamedImp:=Imp.NamedImports[0];
  AssertEquals('Named import name 1','A',NamedImp.Name);
  AssertEquals('Named import alias 1','',NamedImp.Alias);
  NamedImp:=Imp.NamedImports[1];
  AssertEquals('Named import name 2','B',NamedImp.Name);
  AssertEquals('Named import alias 2','',NamedImp.Alias);
end;

procedure TTestJSParser.TestImport2NamedImportAlias;
Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;
  NamedImp : TJSNamedImportElement;

begin
  CreateParser('import {A as C, B as D} from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertTrue('Named imports',Imp.HaveNamedImports);
  AssertEquals('Named import count',2,Imp.NamedImports.Count);
  NamedImp:=Imp.NamedImports[0];
  AssertEquals('Named import name 1','A',NamedImp.Name);
  AssertEquals('Named import alias 1','C',NamedImp.Alias);
  NamedImp:=Imp.NamedImports[1];
  AssertEquals('Named import name 2','B',NamedImp.Name);
  AssertEquals('Named import alias 2','D',NamedImp.Alias);
end;

procedure TTestJSParser.TestImport2NamedImportsComma;
Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;
  NamedImp : TJSNamedImportElement;

begin
  CreateParser('import {A, B, } from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertTrue('Named imports',Imp.HaveNamedImports);
  AssertEquals('Named import count',2,Imp.NamedImports.Count);
  NamedImp:=Imp.NamedImports[0];
  AssertEquals('Named import name 1','A',NamedImp.Name);
  AssertEquals('Named import alias 1','',NamedImp.Alias);
  NamedImp:=Imp.NamedImports[1];
  AssertEquals('Named import name 2','B',NamedImp.Name);
  AssertEquals('Named import alias 2','',NamedImp.Alias);
end;

procedure TTestJSParser.TestImportDefaultAndNamedImport;
Var
  E : TJSElement;
  Imp : TJSImportStatement absolute E;
  NamedImp : TJSNamedImportElement;

begin
  CreateParser('import D, {A} from "a.js"',MinImportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSImportStatement);
  AssertEquals('DefaultImportedName','D',Imp.DefaultBinding);
  AssertEquals('NamespaceImport','',Imp.NameSpaceImport);
  AssertEquals('Modulename','a.js',Imp.ModuleName);
  AssertTrue('Named imports',Imp.HaveNamedImports);
  AssertEquals('Named import count',1,Imp.NamedImports.Count);
  NamedImp:=Imp.NamedImports[0];
  AssertEquals('Named import name','A',NamedImp.Name);
  AssertEquals('Named import alias','',NamedImp.Alias);
end;

procedure TTestJSParser.TestExportAll;

Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;

begin
  CreateParser('export *',MinExportVersion);
  E:=GetFirstStatement;
  AssertFalse('Default',Exp.IsDefault);
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','*',Exp.NameSpaceExport);
  AssertNull('Declaration',Exp.Declaration);
  AssertEquals('ModuleName','',Exp.ModuleName);
  AssertFalse('ExportNames',Exp.HaveExportNames);
end;

procedure TTestJSParser.TestExportAllFrom;

Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;

begin
  CreateParser('export * from "a.js"',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','*',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNull('Declaration',Exp.Declaration);
  AssertEquals('ModuleName','a.js',Exp.ModuleName);
  AssertFalse('ExportNames',Exp.HaveExportNames);
end;

procedure TTestJSParser.TestExportExportName;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  El : TJSExportNameElement;

begin
  CreateParser('export { a }',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNull('Declaration',Exp.Declaration);
  AssertEquals('ModuleName','',Exp.ModuleName);
  AssertTrue('ExportNames',Exp.HaveExportNames);
  AssertEquals('ExportNames count',1,Exp.ExportNames.Count);
  El:=Exp.ExportNames[0];
  AssertEquals('ExportNames[0].Name','a',El.Name);
  AssertEquals('ExportNames[0].Name','',El.Alias);
end;

procedure TTestJSParser.TestExportExportNameAlias;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  El : TJSExportNameElement;

begin
  CreateParser('export { a as b }',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNull('Declaration',Exp.Declaration);
  AssertEquals('ModuleName','',Exp.ModuleName);
  AssertTrue('ExportNames',Exp.HaveExportNames);
  AssertEquals('ExportNames count',1,Exp.ExportNames.Count);
  El:=Exp.ExportNames[0];
  AssertEquals('ExportNames[0].Name','a',El.Name);
  AssertEquals('ExportNames[0].Name','b',El.Alias);
end;

procedure TTestJSParser.TestExportVar;

Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  V : TJSVariableStatement;

begin
  CreateParser('export var a = 1',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSVariableStatement);
  V:=TJSVariableStatement(Exp.Declaration);
  AssertEquals('var type',vtVar,V.varType);
  CheckClass(V.VarDecl,TJSVarDeclaration);
  AssertEquals('Variable name','a',TJSVarDeclaration(V.VarDecl).Name);
end;

procedure TTestJSParser.TestExportLet;

Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  V : TJSVariableStatement;

begin
  CreateParser('export let a = 1',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSVariableStatement);
  V:=TJSVariableStatement(Exp.Declaration);
  AssertEquals('var type',vtLet,V.varType);
  CheckClass(V.VarDecl,TJSVarDeclaration);
  AssertEquals('Variable name','a',TJSVarDeclaration(V.VarDecl).Name);
end;

procedure TTestJSParser.TestExportConst;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  V : TJSVariableStatement;

begin
  CreateParser('export const a = 1',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSVariableStatement);
  V:=TJSVariableStatement(Exp.Declaration);
  AssertEquals('var type',vtConst,V.varType);
  CheckClass(V.VarDecl,TJSVarDeclaration);
  AssertEquals('Variable name','a',TJSVarDeclaration(V.VarDecl).Name);
end;

procedure TTestJSParser.TestExportFunction;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  F : TJSFunctionDeclarationStatement;

begin
  CreateParser('export function a () { return 1; } ',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertFalse('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSFunctionDeclarationStatement);
  F:=TJSFunctionDeclarationStatement(Exp.Declaration);
  AssertNotNull('Have function', F.AFunction);
  AssertEquals('Variable name','a',F.AFunction.Name);
end;

procedure TTestJSParser.TestExportDefaultAssignment;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  A : TJSAssignStatement;

begin
  CreateParser('export default a = 1',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertTrue('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSSimpleAssignStatement);
  A:=TJSSimpleAssignStatement(Exp.Declaration);
  CheckClass(A.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Operator token',jstoken.tjsASSIGN,A.OperatorToken);
  AssertEquals('Variable name','a',TJSPrimaryExpressionIdent(A.LHS).Name);
end;

procedure TTestJSParser.TestExportDefaultFunction;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  F : TJSFunctionDeclarationStatement;

begin
  CreateParser('export default function a () { return 1; } ',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertTrue('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSFunctionDeclarationStatement);
  F:=TJSFunctionDeclarationStatement(Exp.Declaration);
  AssertNotNull('Have function', F.AFunction);
  AssertEquals('Variable name','a',F.AFunction.Name);
  AssertFalse('Async',F.AFunction.IsAsync);
end;

procedure TTestJSParser.TestExportDefaultAsyncFunction;
Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  F : TJSFunctionDeclarationStatement;

begin
  CreateParser('export default async function a () { return 1; } ',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertTrue('Default',Exp.IsDefault);
  AssertNotNull('Declaration',Exp.Declaration);
  CheckClass(Exp.Declaration,TJSFunctionDeclarationStatement);
  F:=TJSFunctionDeclarationStatement(Exp.Declaration);
  AssertNotNull('Have function', F.AFunction);
  AssertEquals('Variable name','a',F.AFunction.Name);
  AssertTrue('Async',F.AFunction.IsAsync);
end;

procedure TTestJSParser.TestClass;
begin
  CreateParser('class Rectangle {  } ',MinClassVersion);
  AssertEquals('class name correct','Rectangle',GetFirstClass.Name);
  AssertNull('class extends name correct',GetFirstClass.Extends);
end;

procedure TTestJSParser.TestClassExtends;

Var
  Ext: TJSTypeReference;

begin
  CreateParser('class Rectangle extends Shape {  function myMethod () { return null; } } ',MinClassVersion);
  AssertEquals('class name correct','Rectangle',GetFirstClass.Name);
  Ext:=TJSTypeReference(CheckClass('Extends is type ref',TJSTypeReference,GetFirstClass.Extends));
  AssertEquals('class extends name correct','Shape',Ext.Name);
end;

procedure TTestJSParser.TestClassWithMethod;
begin
  CreateParser('class Rectangle {  function myMethod () { return null; } } ',MinClassVersion);
  AssertEquals('class name correct','Rectangle',GetFirstClass.Name);
  AssertNull('class extends name correct',GetFirstClass.Extends);
  AssertNotNull('Have members',GetFirstClass.Members);
  AssertEquals('Have functions',1,GetFirstClass.Members.Functions.Count);
end;

procedure TTestJSParser.TestClassExpression;

Var
  X : TJSExpressionStatement;
  A  : TJSSimpleAssignStatement;
begin
  CreateParser('a = class Rectangle {  } ',MinClassVersion);
  X:=GetExpressionStatement;
  CheckClass(X.A,TJSSimpleAssignStatement);
  A:=TJSSimpleAssignStatement(X.A);
  AssertNotNull('Have left operand',A.LHS);
  CheckClass(A.LHS,TJSPrimaryExpressionIdent);
  AssertEquals('Correct name for assignment LHS ','a',TJSPrimaryExpressionIdent(A.LHS).Name);
end;

procedure TTestJSParser.TestLetClassExpression;
Var
  aVars : TJSVariableStatement;
  aVarDecl : TJSVarDeclaration;
  aClass : TJSClassDeclaration;
begin
  CreateParser('let a = class Rectangle {  } ',MinClassVersion);
  AssertEquals('class name correct',TJSVariableStatement, GetFirstStatement.ClassType);
  aVars:=TJSVariableStatement(GetFirstStatement);
  AssertEquals('First class node is var declaration',TJSVarDeclaration,aVars.VarDecl.ClassType);
  aVarDecl:=TJSVarDeclaration(aVars.VarDecl);
  AssertNotNull('Var declaration has init',aVarDecl.Init);
  AssertEquals('Init is class declaration',TJSClassDeclaration,aVarDecl.Init.ClassType);
  aClass:=TJSClassDeclaration(aVarDecl.Init);
  AssertEquals('class name correct','Rectangle',aClass.Name);
end;


procedure TTestJSParser.TestExportExportNameFrom;

Var
  E : TJSElement;
  Exp : TJSExportStatement absolute E;
  El : TJSExportNameElement;

begin
  CreateParser('export { a } from "a.js"',MinExportVersion);
  E:=GetFirstStatement;
  CheckClass(E,TJSExportStatement);
  AssertEquals('NameSpaceExport','',Exp.NameSpaceExport);
  AssertNull('Declaration',Exp.Declaration);
  AssertEquals('ModuleName','a.js',Exp.ModuleName);
  AssertTrue('ExportNames',Exp.HaveExportNames);
  AssertEquals('ExportNames count',1,Exp.ExportNames.Count);
  El:=Exp.ExportNames[0];
  AssertEquals('ExportNames[0].Name','a',El.Name);
  AssertEquals('ExportNames[0].Name','',El.Alias);
end;

procedure TTestJSParser.TestBreak;
Var
  E : TJSElement;
  C : TJSBreakStatement;

begin
  CreateParser('while (true) break;');
  E:=GetFirstStatement;
  CheckClass(E,TJSWhileStatement);
  E:=TJSWhileStatement(E).Body;
  CheckClass(E,TJSBreakStatement);
  C:=TJSBreakStatement(E);
  AssertEquals('Have correct (empty) label','',C.TargetName);
end;

procedure TTestJSParser.TestIfElseSimple;

Var
  E : TJSElement;
  I : TJSIfStatement;

begin
  CreateParser('if (a) b; else c;');
  E:=GetFirstStatement;
  CheckClass(E,TJSIfStatement);
  I:=TJSIfStatement(E);
  AssertNotNull('Statement condition assigned',I.Cond);
  CheckClass(I.Cond,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','a',TJSPrimaryExpressionIdent(I.Cond).Name);
  AssertNotNull('Statement condition assigned',I.Btrue);
  CheckClass(I.Btrue,TJSExpressionStatement);
  AssertNotNull('Expression statement expression',TJSExpressionStatement(I.BTrue).A);
  CheckClass(TJSExpressionStatement(I.BTrue).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','b',TJSPrimaryExpressionIdent(TJSExpressionStatement(I.Btrue).A).Name);
  AssertNotNull('Else Statement condition assigned',I.BFalse);
  CheckClass(I.BFalse,TJSExpressionStatement);
  AssertNotNull('Else statement expression',TJSExpressionStatement(I.BFalse).A);
  CheckClass(TJSExpressionStatement(I.BFalse).A,TJSPrimaryExpressionIdent);
  AssertEquals('Name 1','c',TJSPrimaryExpressionIdent(TJSExpressionStatement(I.BFalse).A).Name);
end;


procedure TTestJSParser.TestExpressionPrecedenceMulNot;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryNotExpression;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('4 * !2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryNotExpression);
  R:=TJSUnaryNotExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('Not operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Not operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Multiplication left operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedencePlusPreMinusMinus;
Var
  X : TJSExpressionStatement;
  R : TJSUnaryPreMinusMinusExpression;
  E : TJSAdditiveExpressionPlus;

begin
  CreateParser('4 + --2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSAdditiveExpressionPlus);
  E:=TJSAdditiveExpressionPlus(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPreMinusMinusExpression);
  R:=TJSUnaryPreMinusMinusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression right operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulPrePlusPlus;

Var
  X : TJSExpressionStatement;
  R : TJSUnaryPrePlusPlusExpression;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('4 * ++2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPrePlusPlusExpression);
  R:=TJSUnaryPrePlusPlusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression right operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestExpressionPrecedenceMulPreMinusMinus;
Var
  X : TJSExpressionStatement;
  R : TJSUnaryPreMinusMinusExpression;
  E : TJSMultiplicativeExpressionMul;

begin
  CreateParser('4 * --2;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSMultiplicativeExpressionMul);
  E:=TJSMultiplicativeExpressionMul(X.A);
  AssertNotNull('Expression left operand assigned',E.A);
  AssertNotNull('Expression right operand assigned',E.B);
  CheckClass(E.B,TJSUnaryPreMinusMinusExpression);
  R:=TJSUnaryPreMinusMinusExpression(E.B);
  CheckClass(E.A,TJSLiteral);
  AssertNotNull('Multiplication left operand assigned',R.A);
  CheckClass(R.A,TJSLiteral);
  AssertEquals('Multiplication left operand type correct', jstNumber, TJSLiteral(R.A).Value.ValueType);
  AssertEquals('Multiplication left operand value correct', 2.0, TJSLiteral(R.A).Value.AsNumber);
  AssertNotNull('Addition right operand value assigned',TJSLiteral(E.A).Value);
  AssertEquals('Addition left operand type correct', jstNumber, TJSLiteral(E.A).Value.ValueType);
  AssertEquals('Expression right operand value correct', 4.0,TJSLiteral(E.A).Value.AsNumber);
end;

procedure TTestJSParser.TestSimpleExpressionBooleanLiteralTrue;

Var
  X : TJSExpressionStatement;

begin
  CreateParser('true;');
  X:=GetExpressionStatement;
  AssertNotNull('Expression statement assigned',X.A);
  CheckClass(X.A,TJSLiteral);
  AssertNotNull('Expression value assigned',TJSLiteral(X.A).Value);
  AssertEquals('Expression value type correct', jstBoolean,TJSLiteral(X.A).Value.ValueType);
  AssertEquals('Expression value correct', True, TJSLiteral(X.A).Value.AsBoolean);
end;


procedure TTestJSParser.TestEmpty;

Var
  E : TJSElement;
  FB : TJSFunctionBody;
  SE : TJSSourceElements;

begin
  CreateParser('var a;');
  E:=FParser.Parse;
  try
    CheckClass(E,TJSFunctionBody);
    FB:=TJSFunctionBody(E);
    AssertNotNull(FB.A);
    CheckClass(FB.A,TJSSourceElements);
    SE:=TJSSourceElements(FB.A);
    AssertEquals('1 variable declaration ',1,SE.Vars.Count);
    CheckClass(FB.A,TJSSourceElements);
  finally
    E.Free;
  end;
end;



initialization

  RegisterTest(TTestJSParser); 
end.

