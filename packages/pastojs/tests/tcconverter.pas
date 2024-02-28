{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
    ./testpas2js --suite=TTestExpressionConverter.TestVariable
}
unit tcconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fppas2js, jsbase, jstree, pastree;

type

  { TTestConverter }

  TTestConverter = class(TTestCase)
  private
    FAC: TPasElement;
    FConverter: TPasToJSConverter;
    FRes: TJSElement;
    FSource: TPasElement;
    FOwnedElements: TFPList;
    procedure OnAddEl(El: TPasElement; Arg: pointer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property AttemptConvert : TPasElement Read FAC Write FAC;
    Procedure TryConvert;
    Function Convert(AElement : TPasElement; AClass : TJSElementClass) : TJSElement;
    Property Converter : TPasToJSConverter Read FConverter;
    Procedure AddEl(El: TPasElement);
    Procedure AddElWithChildren(El: TPasElement);
    Function CreateElement(aClass: TPTreeElement; aParent: TPasElement = nil): TPasElement;
    Function CreatePrimitiveExpr(aParent: TPasElement; Kind: TPasExprKind; const Value: string): TPrimitiveExpr;
    Property TheSource : TPasElement Read FSource Write FSource;
    Property TheResult : TJSElement Read FRes Write FRes;
  Public
    Class procedure AssertEquals(Const Msg : String; AExpected, AActual : TJSType); overload;
    Class procedure AssertLiteral(Const Msg : String; Lit : TJSElement; AType : TJSType);
    Class procedure AssertLiteral(Const Msg : String; Lit : TJSElement; AValue : Boolean);
    Class procedure AssertLiteral(Const Msg : String; Lit : TJSElement; AValue : TJSString);
    Class procedure AssertLiteral(Const Msg : String; Lit : TJSElement; AValue : TJSNumber);
    Class procedure AssertIdentifier(Const Msg : String; Ident : TJSElement; AName : String);
    Class Procedure AssertAssignStatement(Const Msg: String; El: TJSElement; LHS: String='a'; RHS: String='b');
    Class Procedure AssertEmptyBlockStatement(Const Msg: String; El: TJSElement);
    class Function AssertListStatement(Const Msg: String; El: TJSElement) : TJSStatementList;
    class Function AssertElement(Const Msg: String; AClass : TJSElementClass; El: TJSElement) : TJSElement;
    Function CreateLiteral(AValue : String) : TPasExpr;
    Function CreateLiteral(AValue : Double) : TPasExpr;
    Function CreateIdent(AName : String) : TPrimitiveExpr;
    Function CreateAssignStatement(LHS: String = 'a';RHS : String = 'b'): TPasImplAssign;
    Function CreateParamsExpr(Kind: TPasExprKind; aParent: TPasElement = nil) : TParamsExpr;
    Function CreateFunctionCall(AName : String; Params : Array of String) : TParamsExpr;
    Function CreateCondition: TPasExpr;
    Function CreateVariable(aName: String; aParent: TPasElement = nil) : TPasVariable;
    Function CreateBinary(AOpCode: TExprOpCode; aParent: TPasElement = nil) : TBinaryExpr;
    Function CreateBoolConstExpr(Value: boolean): TBoolConstExpr;
    Function CreateUnaryExpr(AOpCode: TExprOpCode): TUnaryExpr;
  end;

  { TTestTestConverter }

  TTestTestConverter = class(TTestConverter)
  published
    procedure TestEmpty;
  end;

  { TTestExpressionConverter }

  TTestExpressionConverter = Class(TTestConverter)
  Protected
    Function TestLiteralExpression(AElement : TPasElement; AClass : TJSElementClass) : TJSLIteral;
    Function TestUnaryExpression(AElement : TPasElement; AClass : TJSElementClass) : TJSUnary;
    Function TestBinaryExpression(AElement : TPasElement; AClass : TJSElementClass) : TJSBinary;
  Published
    Procedure TestPrimitiveString;
    Procedure TestPrimitiveNumber;
    Procedure TestPrimitiveNil;
    Procedure TestPrimitiveBoolTrue;
    Procedure TestPrimitiveBoolFalse;
    Procedure TestPrimitiveIdent;
    Procedure TestUnaryMinus;
    Procedure TestUnaryPlus;
    Procedure TestBinaryPlus;
    Procedure TestBinaryMinus;
    Procedure TestBinaryMultiply;
    Procedure TestBinaryDivision;
    Procedure TestBinaryDiv;
    Procedure TestBinaryMod;
    Procedure TestBinarySHL;
    Procedure TestBinarySHR;
    Procedure TestBinaryEqual;
    Procedure TestBinaryNotEqual;
    Procedure TestBinaryLessThan;
    Procedure TestBinaryLessThanEqual;
    Procedure TestBinaryGreater;
    Procedure TestBinaryGreaterThanEqual;
    Procedure TestBinaryIs;
    Procedure TestBinaryPower;
    Procedure TestCallExpressionNone;
    Procedure TestCallExpressionOne;
    Procedure TestCallExpressionTwo;
    Procedure TestMemberExpressionArrayOneDim;
    Procedure TestMemberExpressionArrayTwoDim;
    Procedure TestVariable;
    Procedure TestArrayVariable;
  end;

  { TTestStatementConverter }

  TTestStatementConverter = Class(TTestConverter)
  private
  Published
    Procedure TestRaiseStatement;
    Procedure TestAssignStatement;
    Procedure TestIfStatement;
    Procedure TestIfStatementFull;
    Procedure TestIfStatementElse;
    Procedure TestWhileStatementEmpty;
    Procedure TestWhileStatement;
    Procedure TestSimpleStatement;
    Procedure TestRepeatUntilStatementEmpty;
    Procedure TestRepeatUntilStatementOne;
    Procedure TestRepeatUntilStatementTwo;
    Procedure TestRepeatUntilStatementThree;
    Procedure TestForLoopUp;
    Procedure TestForLoopDown;
    Procedure TestBeginEndBlockEmpty;
    Procedure TestBeginEndBlockStatementOne;
    Procedure TestBeginEndBlockStatementTwo;
    Procedure TestBeginEndBlockStatementThree;
    Procedure TestWithStatementEmpty;
    Procedure TestWithStatementOne;
    Procedure TestWithStatementTwo;
    Procedure TestTryFinallyStatement;
    Procedure TestTryExceptStatement;
    Procedure TestTryExceptStatementOnE;
    Procedure TestReRaise;
    Procedure TestVariableStatement;
  end;

implementation

uses typinfo;

{ TTestStatementConverter }

Procedure TTestStatementConverter.TestRaiseStatement;

Var
  R : TPasImplRaise;
  E : TJSThrowStatement;

begin
  R:=TPasImplRaise(CreateElement(TPasImplRaise));
  R.ExceptObject:=CreateIdent('e');
  E:=TJSThrowStatement(Convert(R,TJSThrowStatement));
  AssertIdentifier('Raise exception object',E.A,'e');
end;


Procedure TTestStatementConverter.TestAssignStatement;

Var
  R : TPasImplAssign;
  E : TJSSimpleAssignStatement;

begin
  R:=CreateAssignStatement('a','b');
  E:=TJSSimpleAssignStatement(Convert(R,TJSSimpleAssignStatement));
  AssertAssignStatement('a = b assignment',E);
end;


Procedure TTestStatementConverter.TestIfStatement;

Var
  R : TPasImplIfElse;
  E : TJSIfStatement;

begin
  // If a then ;
  R:=TPasImplIfElse(CreateElement(TPasImplIfElse));
  R.ConditionExpr:=CreateCondition;
  E:=TJSIfStatement(Convert(R,TJSIfStatement));
  AssertNull('If branch is empty',E.BTrue);
  AssertNull('No else branch',E.BFalse);
  AssertIdentifier('Left hand side OK',E.Cond,'a');
end;

Procedure TTestStatementConverter.TestIfStatementFull;

Var
  R : TPasImplIfElse;
  E : TJSIfStatement;

begin
  // If a then a:=b;
  R:=TPasImplIfElse(CreateElement(TPasImplIfElse));
  R.ConditionExpr:=CreateCondition;
  R.IfBranch:=CreateAssignStatement;
  E:=TJSIfStatement(Convert(R,TJSIfStatement));
  AssertIdentifier('Conditional expression',E.Cond,'a');
  AssertAssignStatement('If branch',E.btrue);
  AssertNull('No else branch',E.bfalse);
end;

Procedure TTestStatementConverter.TestIfStatementElse;
Var
  R : TPasImplIfElse;
  E : TJSIfStatement;

begin
  // If a then a:=b else a:=e;
  R:=TPasImplIfElse(CreateElement(TPasImplIfElse));
  R.ConditionExpr:=CreateCondition;
  R.IfBranch:=CreateAssignStatement;
  R.ElseBranch:=CreateAssignStatement('a','e');
  E:=TJSIfStatement(Convert(R,TJSIfStatement));
  AssertIdentifier('Conditional expression',E.Cond,'a');
  AssertAssignStatement('If branch',E.btrue);
  AssertAssignStatement('else branch',E.bfalse,'a','e');
end;

Procedure TTestStatementConverter.TestWhileStatementEmpty;
Var
  R : TPasImplWhileDo;
  E : TJSWhileStatement;

begin
  // While a do ;
  R:=TPasImplWhileDo(CreateElement(TPasImplWhileDo));
  R.ConditionExpr:=CreateCondition;
  E:=TJSWhileStatement(Convert(R,TJSWhileStatement));
  AssertIdentifier('Conditional expression',E.Cond,'a');
  AssertEquals('No statement, empty block statement',TJSEmptyBlockStatement,E.body.ClassType);
end;

Procedure TTestStatementConverter.TestWhileStatement;
Var
  R : TPasImplWhileDo;
  E : TJSWhileStatement;

begin
  // While a do b:=c;
  R:=TPasImplWhileDo(CreateElement(TPasImplWhileDo));
  R.Body:=CreateAssignStatement('b','c');
  R.ConditionExpr:=CreateCondition;
  E:=TJSWhileStatement(Convert(R,TJSWhileStatement));
  AssertIdentifier('Conditional expression',E.Cond,'a');
  AssertAssignStatement('While Block is assignment',E.body,'b','c');
end;

Procedure TTestStatementConverter.TestSimpleStatement;

Var
  R : TPasImplSimple;
  E : TJSExpressionStatement;
  C : TJSCallExpression;

begin
  R:=TPasImplSimple(CreateElement(TPasImplSimple));
  R.Expr:=CreateFunctionCall('a',['b']);
  E:=TJSExpressionStatement(Convert(R,TJSExpressionStatement));
  AssertNotNull('Have call node',E.A);
  AssertEquals('Have call expression',TJSCallExpression,E.A.ClassType);
  C:=TJSCallExpression(E.A);
  AssertIdentifier('Call expression',C.Expr,'a');
end;

Procedure TTestStatementConverter.TestRepeatUntilStatementEmpty;

Var
  R : TPasImplRepeatUntil;
  E : TJSWhileStatement;

begin
  // Repeat until a;
  R:=TPasImplRepeatUntil(CreateElement(TPasImplRepeatUntil));
  R.ConditionExpr:=CreateCondition;
  E:=TJSDoWhileStatement(Convert(R,TJSDoWhileStatement));
  AssertNotNull('Have condition',E.Cond);
  AssertEquals('Correct condition class',TJSUnaryNotExpression,E.Cond.ClassType);
  AssertIdentifier('Conditional expression',TJSUnaryNotExpression(E.Cond).A,'a');
  AssertNotNull('Have body',E.Body);
  AssertEquals('No statement, empty block statement',TJSEmptyBlockStatement,E.body.ClassType);
end;

Procedure TTestStatementConverter.TestRepeatUntilStatementOne;

Var
  R : TPasImplRepeatUntil;
  E : TJSWhileStatement;
  L : TJSStatementList;

begin
  // Repeat b:=c; until a;
  R:=TPasImplRepeatUntil(CreateElement(TPasImplRepeatUntil));
  R.ConditionExpr:=CreateCondition;
  AddEl(R.AddAssign(CreateIdent('b'),CreateIdent('c')));
  E:=TJSDoWhileStatement(Convert(R,TJSDoWhileStatement));
  AssertNotNull('Have condition',E.Cond);
  AssertEquals('Correct condition class',TJSUnaryNotExpression,E.Cond.ClassType);
  AssertIdentifier('Conditional expression',TJSUnaryNotExpression(E.Cond).A,'a');
  AssertNotNull('Have body',E.Body);
  AssertEquals('List statement, List statement',TJSStatementList,E.body.ClassType);
  L:=TJSStatementList(E.Body);
  AssertAssignStatement('First List statement is assignment',L.A,'b','c');
  AssertNull('No second statement',L.B);
end;

Procedure TTestStatementConverter.TestRepeatUntilStatementTwo;

Var
  R : TPasImplRepeatUntil;
  E : TJSWhileStatement;
  L : TJSStatementList;

begin
  // Repeat b:=c; d:=e; until a;
  R:=TPasImplRepeatUntil(CreateElement(TPasImplRepeatUntil));
  R.ConditionExpr:=CreateCondition;
  AddEl(R.AddAssign(CreateIdent('b'),CreateIdent('c')));
  AddEl(R.AddAssign(CreateIdent('d'),CreateIdent('e')));
  E:=TJSDoWhileStatement(Convert(R,TJSDoWhileStatement));
  AssertNotNull('Have condition',E.Cond);
  AssertEquals('Correct condition class',TJSUnaryNotExpression,E.Cond.ClassType);
  AssertIdentifier('Conditional expression',TJSUnaryNotExpression(E.Cond).A,'a');
  L:=AssertListStatement('Multiple statements',E.Body);
  AssertAssignStatement('First List statement is assignment',L.A,'b','c');
  AssertAssignStatement('Second List statement is assignment',L.B,'d','e');
end;

Procedure TTestStatementConverter.TestRepeatUntilStatementThree;

Var
  R : TPasImplRepeatUntil;
  E : TJSWhileStatement;
  L : TJSStatementList;

begin
  // Repeat b:=c; d:=e; f:=g; until a;
  R:=TPasImplRepeatUntil(CreateElement(TPasImplRepeatUntil));
  R.ConditionExpr:=CreateCondition;
  AddEl(R.AddAssign(CreateIdent('b'),CreateIdent('c')));
  AddEl(R.AddAssign(CreateIdent('d'),CreateIdent('e')));
  AddEl(R.AddAssign(CreateIdent('f'),CreateIdent('g')));
  E:=TJSDoWhileStatement(Convert(R,TJSDoWhileStatement));
  AssertNotNull('Have condition',E.Cond);
  AssertEquals('Correct condition class',TJSUnaryNotExpression,E.Cond.ClassType);
  AssertIdentifier('Conditional expression',TJSUnaryNotExpression(E.Cond).A,'a');
  AssertNotNull('Have body',E.Body);
  L:=AssertListStatement('Multiple statements',E.Body);
  AssertAssignStatement('First List statement is assignment',L.A,'b','c');
  L:=AssertListStatement('Second statement is again list',L.B);
  AssertAssignStatement('Second List statement is assignment',L.A,'d','e');
  AssertAssignStatement('third List statement is assignment',L.B,'f','g');
end;

Procedure TTestStatementConverter.TestForLoopUp;

Var
  F : TPasImplForLoop;
  ForSt: TJSForStatement;
  L : TJSStatementList;
  VD : TJSVarDeclaration;
  A : TJSSimpleAssignStatement;
  I : TJSUnaryPostPlusPlusExpression;
  Cond : TJSRelationalExpressionLE;
  VS: TJSVariableStatement;
  LoopEndVar, LoopVar: String;
  VDL: TJSVariableDeclarationList;

begin
  // For I:=1 to 100 do a:=b;
  F:=TPasImplForLoop(CreateElement(TPasImplForLoop));
  F.Variable:=CreateVariable('I',F);
  F.VariableName:=CreateIdent('I');
  F.StartExpr:=CreateLiteral(1);
  F.EndExpr:=CreateLiteral(100);
  F.Body:=CreateAssignStatement();
  ForSt:=TJSForStatement(Convert(F,TJSForStatement));
  // Should be
  //   for(var $l=1, $end=100; $l<=$end2; $l++){
  //     I=$l;
  //     a=b;
  //   }
  LoopVar:=Pas2JSBuiltInNames[pbivnLoop];
  LoopEndVar:=Pas2JSBuiltInNames[pbivnLoopEnd];

  // "var $l=1, $end=100"
  VS:=TJSVariableStatement(AssertElement('For init is var '+LoopEndVar,TJSVariableStatement,ForSt.Init));
  VDL:=TJSVariableDeclarationList(AssertElement('For init var has comma',TJSVariableDeclarationList,VS.VarDecl));
  VD:=TJSVarDeclaration(AssertElement('var '+LoopVar,TJSVarDeclaration,VDL.A));
  AssertEquals('Correct name for '+LoopVar,LoopVar,String(VD.Name));
  AssertLiteral('Correct start value',VD.Init,1);
  VD:=TJSVarDeclaration(AssertElement('var '+LoopEndVar,TJSVarDeclaration,VDL.B));
  AssertEquals('Correct name for '+LoopEndVar,LoopEndVar,String(VD.Name));
  AssertLiteral('Correct end value',VD.Init,100);

  // $l<=$end
  Cond:=TJSRelationalExpressionLE(AssertElement('Condition is <= expression',TJSRelationalExpressionLE,ForSt.Cond));
  AssertIdentifier('Cond LHS is '+LoopVar,Cond.A,LoopVar);
  AssertIdentifier('Cond RHS is '+LoopEndVar,Cond.B,LoopEndVar);

  // $l++
  I:=TJSUnaryPostPlusPlusExpression(AssertElement('Increment is ++ statement',TJSUnaryPostPlusPlusExpression,ForSt.Incr));
  AssertIdentifier('++ on correct variable name',I.A,LoopVar);

  // body
  L:=TJSStatementList(AssertElement('For body ist list',TJSStatementList,ForSt.Body));

  // I:=$l
  A:=TJSSimpleAssignStatement(AssertElement('I:='+LoopVar,TJSSimpleAssignStatement,L.A));
  AssertIdentifier('Init statement LHS is loop variable',A.LHS,'i');
  AssertIdentifier('Init statement RHS is '+LoopVar,A.Expr,LoopVar);

  AssertAssignStatement('Correct body',L.B);
end;

Procedure TTestStatementConverter.TestForLoopDown;
Var
  F : TPasImplForLoop;
  ForSt: TJSForStatement;
  L : TJSStatementList;
  VD : TJSVarDeclaration;
  A : TJSSimpleAssignStatement;
  I : TJSUnaryPostMinusMinusExpression;
  Cond: TJSRelationalExpressionGE;
  VS: TJSVariableStatement;
  LoopEndVar, LoopVar: String;
  VDL: TJSVariableDeclarationList;

begin
  // For I:=100 downto 1 do a:=b;
  F:=TPasImplForLoop(CreateElement(TPasImplForLoop));
  F.Variable:=CreateVariable('I',F);
  F.VariableName:=CreateIdent('I');
  F.StartExpr:=CreateLiteral(100);
  F.EndExpr:=CreateLiteral(1);
  F.LoopType:=ltDown;
  F.Body:=CreateAssignStatement();
  ForSt:=TJSForStatement(Convert(F,TJSForStatement));
  // Should be
  //   for(var $l=100, $end=1; $l>=$end; $l--){
  //     I=$l;
  //     a=b;
  //   }
  LoopVar:=Pas2JSBuiltInNames[pbivnLoop];
  LoopEndVar:=Pas2JSBuiltInNames[pbivnLoopEnd];

  // "var $l=100, $end=1"
  VS:=TJSVariableStatement(AssertElement('For init is var '+LoopEndVar,TJSVariableStatement,ForSt.Init));
  VDL:=TJSVariableDeclarationList(AssertElement('For init var has comma',TJSVariableDeclarationList,VS.VarDecl));
  VD:=TJSVarDeclaration(AssertElement('var '+LoopVar,TJSVarDeclaration,VDL.A));
  AssertEquals('Correct name for '+LoopVar,LoopVar,String(VD.Name));
  AssertLiteral('Correct start value',VD.Init,100);
  VD:=TJSVarDeclaration(AssertElement('var '+LoopEndVar,TJSVarDeclaration,VDL.B));
  AssertEquals('Correct name for '+LoopEndVar,LoopEndVar,String(VD.Name));
  AssertLiteral('Correct end value',VD.Init,1);

  // $l>=$end
  Cond:=TJSRelationalExpressionGE(AssertElement('Condition is >= expression',TJSRelationalExpressionGE,ForSt.Cond));
  AssertIdentifier('Cond LHS is '+LoopVar,Cond.A,LoopVar);
  AssertIdentifier('Cond RHS is '+LoopEndVar,Cond.B,LoopEndVar);

  // $l--
  I:=TJSUnaryPostMinusMinusExpression(AssertElement('Increment is -- statement',TJSUnaryPostMinusMinusExpression,ForSt.Incr));
  AssertIdentifier('-- on correct variable name',I.A,LoopVar);

  // body
  L:=TJSStatementList(AssertElement('For body ist list',TJSStatementList,ForSt.Body));

  // I:=$l
  A:=TJSSimpleAssignStatement(AssertElement('I:='+LoopVar,TJSSimpleAssignStatement,L.A));
  AssertIdentifier('Init statement LHS is loop variable',A.LHS,'i');
  AssertIdentifier('Init statement RHS is '+LoopVar,A.Expr,LoopVar);

  AssertAssignStatement('Correct body',L.B);
end;

Procedure TTestStatementConverter.TestBeginEndBlockEmpty;
Var
  R : TPasImplBeginBlock;

begin
  // begin end;
  R:=TPasImplBeginBlock(CreateElement(TPasImplBeginBlock));
  Convert(R,TJSEmptyBlockStatement);
end;

Procedure TTestStatementConverter.TestBeginEndBlockStatementOne;

Var
  R : TPasImplBeginBlock;
  L : TJSStatementList;

begin
  // begin a:=bend;
  R:=TPasImplBeginBlock(CreateElement(TPasImplBeginBlock));
  AddEl(R.AddAssign(CreateIdent('a'),CreateIdent('b')));
  L:=TJSStatementList(Convert(R,TJSStatementList));
  AssertNull('No second statement',L.B);
  AssertAssignStatement('First List statement is assignment',L.A,'a','b');
end;

Procedure TTestStatementConverter.TestBeginEndBlockStatementTwo;

Var
  R : TPasImplBeginBlock;
  L : TJSStatementList;

begin
  // begin a:=b; c:=d; end;
  R:=TPasImplBeginBlock(CreateElement(TPasImplBeginBlock));
  AddEl(R.AddAssign(CreateIdent('a'),CreateIdent('b')));
  AddEl(R.AddAssign(CreateIdent('c'),CreateIdent('d')));
  L:=TJSStatementList(Convert(R,TJSStatementList));
  AssertAssignStatement('First List statement is assignment',L.A,'a','b');
  AssertAssignStatement('Second List statement is assignment',L.B,'c','d');
end;

Procedure TTestStatementConverter.TestBeginEndBlockStatementThree;
Var
  R : TPasImplBeginBlock;
  L : TJSStatementList;

begin
  // begin a:=b; c:=d; end;
  R:=TPasImplBeginBlock(CreateElement(TPasImplBeginBlock));
  AddEl(R.AddAssign(CreateIdent('a'),CreateIdent('b')));
  AddEl(R.AddAssign(CreateIdent('c'),CreateIdent('d')));
  AddEl(R.AddAssign(CreateIdent('e'),CreateIdent('f')));
  L:=TJSStatementList(Convert(R,TJSStatementList));
  AssertAssignStatement('First List statement is assignment',L.A,'a','b');
  L:=AssertListStatement('Second statement is again list',L.B);
  AssertAssignStatement('Second List statement is assignment',L.A,'c','d');
  AssertAssignStatement('third List statement is assignment',L.B,'e','f');
end;

Procedure TTestStatementConverter.TestWithStatementEmpty;
Var
  W : TPasImplWithDo;
  El : TJSWithStatement;

begin
  // With A do ;
  W:=TPasImplWithDo(CreateElement(TPasImplWithDo));
  W.Expressions.Add(CreateIdent('a'));
  El:=TJSWithStatement(Convert(W,TJSWithStatement));
  AssertIdentifier('Correct with expression',EL.A,'a');
  AssertEmptyBlockStatement('Empty with',El.B);
end;

Procedure TTestStatementConverter.TestWithStatementOne;

Var
  W : TPasImplWithDo;
  El : TJSWithStatement;

begin
  // With A do b:=c;
  W:=TPasImplWithDo(CreateElement(TPasImplWithDo));
  W.Expressions.Add(CreateIdent('a'));
  W.Body:=CreateAssignStatement('b','c');
  El:=TJSWithStatement(Convert(W,TJSWithStatement));
  AssertIdentifier('Correct with expression',EL.A,'a');
  AssertAssignStatement('Correct assignment',EL.B,'b','c');
end;

Procedure TTestStatementConverter.TestWithStatementTwo;
Var
  W : TPasImplWithDo;
  El : TJSWithStatement;

begin
  // With A,D do b:=c;
  W:=TPasImplWithDo(CreateElement(TPasImplWithDo));
  W.Expressions.Add(CreateIdent('a'));
  W.Expressions.Add(CreateIdent('d'));
  W.Body:=CreateAssignStatement('b','c');
  El:=TJSWithStatement(Convert(W,TJSWithStatement));
  AssertIdentifier('Correct with expression',EL.A,'a');
  El:=TJSWithStatement(AssertElement('Have second with statement',TJSWithStatement,EL.B));
  AssertIdentifier('Correct with expression',EL.A,'d');
  AssertAssignStatement('Correct assignment',El.B,'b','c');
end;

Procedure TTestStatementConverter.TestTryFinallyStatement;

Var
  T : TPasImplTry;
  F : TPasImplTryFinally;
  El : TJSTryFinallyStatement;
  L : TJSStatementList;

begin
  // Try a:=B finally b:=c end;
  T:=TPasImplTry(CreateElement(TPasImplTry));
  T.AddElement(CreateAssignStatement('a','b'));
  F:=T.AddFinally;
  AddEl(F);
  F.AddElement(CreateAssignStatement('b','c'));
  El:=TJSTryFinallyStatement(Convert(T,TJSTryFinallyStatement));
  L:=AssertListStatement('try..finally block is statement list',EL.Block);
  AssertAssignStatement('Correct assignment in try..finally block',L.A,'a','b');
  AssertNull('No second statement',L.B);
  L:=AssertListStatement('try..finally block is statement list',El.BFinally);
  AssertAssignStatement('Correct assignment in finally..end block',L.A,'b','c');
  AssertNull('No second statement',L.B);
end;

Procedure TTestStatementConverter.TestTryExceptStatement;
Var
  T : TPasImplTry;
  F : TPasImplTryExcept;
  El : TJSTryCatchStatement;
  L : TJSStatementList;
  ExceptObjName: String;

begin
  // Try a:=b except b:=c end;
  (*
    Becomes:
    try {
     a=b;
    } catch ($e) {
      b = c;
    }
  *)
  T:=TPasImplTry(CreateElement(TPasImplTry));
  T.AddElement(CreateAssignStatement('a','b'));
  F:=T.AddExcept;
  AddElWithChildren(F);
  F.AddElement(CreateAssignStatement('b','c'));
  // Convert
  El:=TJSTryCatchStatement(Convert(T,TJSTryCatchStatement));
  // check "catch(exceptobject)"
  ExceptObjName:=lowercase(Pas2JSBuiltInNames[pbivnExceptObject]);
  AssertEquals('Correct exception object name',ExceptObjName,String(El.Ident));
  // check "a=b;"
  L:=AssertListStatement('try..except block is statement list',El.Block);
  AssertAssignStatement('Correct assignment in try..except block',L.A,'a','b');
  AssertNull('No second statement',L.B);
  // check "b=c;'
  L:=AssertListStatement('try..except block is statement list',El.BCatch);
  AssertAssignStatement('Correct assignment in except..end block',L.A,'b','c');
  AssertNull('No second statement',L.B);
end;

Procedure TTestStatementConverter.TestTryExceptStatementOnE;

Var
  T : TPasImplTry;
  F : TPasImplTryExcept;
  O : TPasImplExceptOn;
  El : TJSTryCatchStatement;
  L : TJSStatementList;
  I : TJSIfStatement;
  IC : TJSCallExpression;
  D: TJSDotMemberExpression;
  ExObj: TJSElement;
  VS: TJSVariableStatement;
  V: TJSVarDeclaration;
  ExceptObjName: String;

begin
  // Try a:=B except on E : exception do  b:=c end;
  (*
    Becomes:
    try {
     a=b;
    } catch (exceptobject) {
      if (exception.isPrototypeOf(exceptobject)) {
        var e = exceptobject;
        b = c;
      }
    }
  *)
  T:=TPasImplTry(CreateElement(TPasImplTry));
  T.AddElement(CreateAssignStatement('a','b'));
  F:=T.AddExcept;
  AddElWithChildren(F);
  O:=F.AddExceptOn('E','Exception');
  AddElWithChildren(O);
  O.Body:=CreateAssignStatement('b','c');
  // Convert
  El:=TJSTryCatchStatement(Convert(T,TJSTryCatchStatement));
  // check "catch(exceptobject)"
  ExceptObjName:=lowercase(Pas2JSBuiltInNames[pbivnExceptObject]);
  AssertEquals('Correct exception object name',ExceptObjName,String(El.Ident));
  // check "if"
  I:=TJSIfStatement(AssertElement('On block is if',TJSIfStatement,El.BCatch));
  // check if condition "exception.isPrototypeOf(exceptobject)"
  IC:=TJSCallExpression(AssertElement('If condition is call expression',TJSCallExpression,I.Cond));
  D:=TJSDotMemberExpression(AssertElement('exception.isPrototypeOf is dot member expression',TJSDotMemberExpression,IC.Expr));
  Assertidentifier('left side of exception.isPrototypeOf',D.MExpr,'exception');
  AssertEquals('right side of exception.isPrototypeOf','isPrototypeOf',String(D.Name));
  AssertNotNull('args of exception.isPrototypeOf(exceptobject)',IC.Args);
  AssertEquals('args of exception.isPrototypeOf(exceptobject)',1,IC.Args.Elements.Count);
  ExObj:=IC.Args.Elements.Elements[0].Expr;
  Assertidentifier('arg of exception.isPrototypeOf(exceptobject)',ExObj,ExceptObjName);
  // check statement "var e = exceptobject;"
  L:=AssertListStatement('On block is always a list',I.BTrue);
  writeln('TTestStatementConverter.TestTryExceptStatementOnE ',L.A.ClassName);
  VS:=TJSVariableStatement(AssertElement('First statement in list is a var statement',TJSVariableStatement,L.A));
  V:=TJSVarDeclaration(AssertElement('var declaration e=ExceptObject',TJSVarDeclaration,VS.VarDecl));
  AssertEquals('Variable name is identifier in On A : Ex do','e',String(V.Name));
  Assertidentifier('Variable init is exception object',V.Init,ExceptObjName);
  // check "b = c;"
  AssertAssignStatement('Original assignment in second statement',L.B,'b','c');
end;

Procedure TTestStatementConverter.TestReRaise;
Var
  T : TPasImplTry;
  F : TPasImplTryExcept;
  O : TPasImplExceptOn;
  El : TJSTryCatchStatement;
  L : TJSStatementList;
  I : TJSIfStatement;
  IC : TJSCallExpression;
  R : TJSThrowStatement;
  V : TJSVarDeclaration;
  D: TJSDotMemberExpression;
  ExObj: TJSElement;
  VS: TJSVariableStatement;
  ExceptObjName: String;

begin
  // Try a:=B except on E : exception do raise; end;
  (*
    Becomes:
    try {
     a=b;
    } catch ($e) {
      if (exception.isPrototypeOf($e)) {
        var e = $e;
        throw $e;
      }
    }
  *)
  T:=TPasImplTry(CreateElement(TPasImplTry));
  T.AddElement(CreateAssignStatement('a','b'));
  F:=T.AddExcept;
  AddEl(F);
  O:=F.AddExceptOn('E','Exception');
  AddElWithChildren(O);
  O.Body:=TPasImplRaise(CreateElement(TPasImplRaise));
  // Convert
  El:=TJSTryCatchStatement(Convert(T,TJSTryCatchStatement));
  // check "catch(exceptobject)"
  ExceptObjName:=lowercase(Pas2JSBuiltInNames[pbivnExceptObject]);
  AssertEquals('Correct exception object name',ExceptObjName,String(El.Ident));
  // check "if"
  I:=TJSIfStatement(AssertElement('On block is if',TJSIfStatement,El.BCatch));
  // check if condition "exception.isPrototypeOf(exceptobject)"
  IC:=TJSCallExpression(AssertElement('If condition is call expression',TJSCallExpression,I.Cond));
  D:=TJSDotMemberExpression(AssertElement('exception.isPrototypeOf is dot member expression',TJSDotMemberExpression,IC.Expr));
  Assertidentifier('left side of exception.isPrototypeOf',D.MExpr,'exception');
  AssertEquals('right side of exception.isPrototypeOf','isPrototypeOf',String(D.Name));
  AssertNotNull('args of exception.isPrototypeOf(ExceptObject)',IC.Args);
  AssertEquals('args of exception.isPrototypeOf(ExceptObject)',1,IC.Args.Elements.Count);
  ExObj:=IC.Args.Elements.Elements[0].Expr;
  Assertidentifier('arg of exception.isPrototypeOf(ExceptObject)',ExObj,ExceptObjName);
  // check statement "var e = exceptobject;"
  L:=AssertListStatement('On block is always a list',I.BTrue);
  writeln('TTestStatementConverter.TestTryExceptStatementOnE ',L.A.ClassName);
  VS:=TJSVariableStatement(AssertElement('First statement in list is a var statement',TJSVariableStatement,L.A));
  V:=TJSVarDeclaration(AssertElement('var declaration e=ExceptObject',TJSVarDeclaration,VS.VarDecl));
  AssertEquals('Variable name is identifier in On A : Ex do','e',String(V.Name));
  Assertidentifier('Variable init is exception object',V.Init,ExceptObjName);
  R:=TJSThrowStatement(AssertElement('On block is throw statement',TJSThrowStatement,L.B));
  Assertidentifier('R expression is original exception ',R.A,ExceptObjName);
end;

Procedure TTestStatementConverter.TestVariableStatement;

Var
  S : TPasSection;
  V : TPasVariable;
  L : TJSStatementList;
  JV : TJSVariableStatement;
  JVD : TJSVarDeclaration;
begin
  S:=TPasSection(CreateElement(TPasSection));
  V:=CreateVariable('A',Nil);
  S.Declarations.Add(V);
  S.Variables.Add(V);
  L:=TJSStatementList(Convert(S,TJSStatementList));
  JV:=TJSVariableStatement(AssertElement('Variable statement',TJSVariableStatement,L.A));
  JVD:=TJSVarDeclaration(AssertElement('Variable declaration',TJSVarDeclaration,JV.VarDecl));
  AssertEquals('Correct variable name','a',String(JVD.Name));
end;

{ TTestExpressionConverter }

function TTestExpressionConverter.TestLiteralExpression(AElement: TPasElement;
  AClass: TJSElementClass): TJSLIteral;

Var
  E : TJSElement;

begin
  E:=Convert(AElement,AClass);
  if not (E is TJSLiteral) then
    Fail('Do not have literal class, but: '+E.ClassName);
  Result:=TJSLiteral(E);
end;

function TTestExpressionConverter.TestUnaryExpression(AElement: TPasElement;
  AClass: TJSElementClass): TJSUnary;

Var
  E : TJSElement;

begin
  E:=Convert(AElement,AClass);
  AssertNotNull('Convert returned a result',E);
  if not (E is TJSUnary) then
    Fail('Do not have unary class, but: '+E.ClassName);
  AssertEquals('TTestExpressionConverter.TestUnaryExpression: wrong class',AClass.ClassName,E.ClassName);
  Result:=TJSUnary(E);
end;

function TTestExpressionConverter.TestBinaryExpression(AElement: TPasElement;
  AClass: TJSElementClass): TJSBinary;

Var
  E : TJSElement;

begin
  E:=Convert(AElement,AClass);
  if not (E is TJSBinary) then
    Fail('Do not have literal class, but: '+E.ClassName);
  Result:=TJSBinary(E);
end;

procedure TTestExpressionConverter.TestPrimitiveString;

Var
  S : TPrimitiveExpr;
  E : TJSLiteral;

begin
  S:=CreatePrimitiveExpr(Nil,pekString,'''me''');
  E:=TestLiteralExpression(S,TJSLiteral);
  AssertEquals('Correct literal type',jstString,E.Value.ValueType);
  AssertEquals('Correct literal value','me',String(E.Value.AsString));
end;

procedure TTestExpressionConverter.TestPrimitiveNumber;
Var
  S : TPrimitiveExpr;
  E : TJSLiteral;

begin
  S:=CreatePrimitiveExpr(Nil,pekNumber,'1.23');
  E:=TestLiteralExpression(S,TJSLiteral);
  AssertEquals('Correct literal type',jstNumber,E.Value.ValueType);
  AssertEquals('Correct literal value',1.23,E.Value.AsNumber);
end;

procedure TTestExpressionConverter.TestPrimitiveNil;

Var
  S : TNilExpr;
  E : TJSLiteral;

begin
  S:=TNilExpr(CreateElement(TNilExpr));
  E:=TestLiteralExpression(S,TJSLiteral);
  AssertEquals('Correct literal type',jstNull,E.Value.ValueType);
  AssertEquals('Correct literal value',True,E.Value.IsNull);
end;

procedure TTestExpressionConverter.TestPrimitiveBoolTrue;
Var
  S : TBoolConstExpr;
  E : TJSLiteral;

begin
  S:=CreateBoolConstExpr(True);
  E:=TestLiteralExpression(S,TJSLiteral);
  AssertEquals('Correct literal type',jstBoolean,E.Value.ValueType);
  AssertEquals('Correct literal value',True,E.Value.AsBoolean);
end;

procedure TTestExpressionConverter.TestPrimitiveBoolFalse;

Var
  S : TBoolConstExpr;
  E : TJSLiteral;

begin
  S:=CreateBoolConstExpr(False);
  E:=TestLiteralExpression(S,TJSLiteral);
  AssertEquals('Correct literal type',jstBoolean,E.Value.ValueType);
  AssertEquals('Correct literal value',False,E.Value.AsBoolean);
end;

procedure TTestExpressionConverter.TestPrimitiveIdent;

Var
  Id : TPrimitiveExpr;
  Res : TJSPrimaryExpressionIdent;
begin
  Id:=CreatePrimitiveExpr(Nil,pekIdent,'a');
  Res:=TJSPrimaryExpressionIdent(Convert(Id,TJSPrimaryExpressionIdent));
  AssertEquals('Correct identifier name','a',String(Res.Name));
end;

procedure TTestExpressionConverter.TestUnaryMinus;
Var
  U : TUnaryExpr;
  E : TJSUnaryMinusExpression;

begin
  U:=CreateUnaryExpr(eopSubtract);
  U.Operand:=CreateLiteral(1.23);
  E:=TJSUnaryMinusExpression(TestUnaryExpression(U,TJSUnaryMinusExpression));
  AssertLiteral('Correct literal for minus',E.A,1.23)
end;

procedure TTestExpressionConverter.TestUnaryPlus;
Var
  U : TUnaryExpr;
  E : TJSUnaryPlusExpression;

begin
  U:=CreateUnaryExpr(eopAdd);
  U.Operand:=CreateLiteral(1.23);
  E:=TJSUnaryPlusExpression(TestUnaryExpression(U,TJSUnaryPlusExpression));
  AssertLiteral('Correct literal for plus',E.A,1.23)
end;

procedure TTestExpressionConverter.TestBinaryPlus;
Var
  B : TBinaryExpr;
  E : TJSAdditiveExpressionPlus;

begin
  B:=CreateBinary(eopAdd);
  B.left:=CreateLiteral(1.23);
  B.Right:=CreateLiteral(3.45);
  E:=TJSAdditiveExpressionPlus(TestBinaryExpression(B,TJSAdditiveExpressionPlus));
  AssertLiteral('Correct left literal for addition',E.A,1.23);
  AssertLiteral('Correct right literal for addition',E.B,3.45);
end;

procedure TTestExpressionConverter.TestBinaryMinus;
Var
  B : TBinaryExpr;
  E : TJSAdditiveExpressionMinus;

begin
  B:=CreateBinary(eopSubtract);
  B.left:=CreateLiteral(1.23);
  B.Right:=CreateLiteral(3.45);
  E:=TJSAdditiveExpressionMinus(TestBinaryExpression(B,TJSAdditiveExpressionMinus));
  AssertLiteral('Correct left literal for subtract',E.A,1.23);
  AssertLiteral('Correct right literal for subtract',E.B,3.45);
end;

procedure TTestExpressionConverter.TestBinaryMultiply;
Var
  B : TBinaryExpr;
  E : TJSMultiplicativeExpressionMul;

begin
  B:=CreateBinary(eopMultiply);
  B.left:=CreateLiteral(1.23);
  B.Right:=CreateLiteral(3.45);
  E:=TJSMultiplicativeExpressionMul(TestBinaryExpression(B,TJSMultiplicativeExpressionMul));
  AssertLiteral('Correct left literal for multiplication',E.A,1.23);
  AssertLiteral('Correct right literal for multiplication',E.B,3.45);
end;

procedure TTestExpressionConverter.TestBinaryDivision;

Var
  B : TBinaryExpr;
  E : TJSMultiplicativeExpressionDiv;

begin
  B:=CreateBinary(eopDivide);
  B.left:=CreateLiteral(1.23);
  B.Right:=CreateLiteral(3.45);
  E:=TJSMultiplicativeExpressionDiv(TestBinaryExpression(B,TJSMultiplicativeExpressionDiv));
  AssertLiteral('Correct left literal for division',E.A,1.23);
  AssertLiteral('Correct right literal for division',E.B,3.45);
end;

procedure TTestExpressionConverter.TestBinaryDiv;

Var
  B : TBinaryExpr;
  E : TJSMultiplicativeExpressionDiv;
  C: TJSCallExpression;
  Args: TJSArguments;
begin
  B:=CreateBinary(eopDiv);
  B.left:=CreateLiteral(1.23);
  B.Right:=CreateLiteral(3.45);
  C:=TJSCallExpression(Convert(B,TJSCallExpression));
  Args:=TJSArguments(AssertElement('Math.floor param',TJSArguments,C.Args));
  E:=TJSMultiplicativeExpressionDiv(AssertElement('param',TJSMultiplicativeExpressionDiv,Args.Elements.Elements[0].Expr));
  AssertLiteral('Correct left literal for div',E.A,1.23);
  AssertLiteral('Correct right literal for div',E.B,3.45);
end;

procedure TTestExpressionConverter.TestBinaryMod;
Var
  B : TBinaryExpr;
  E : TJSMultiplicativeExpressionMod;

begin
  B:=CreateBinary(eopMod);
  B.left:=CreateLiteral(1.23);
  B.Right:=CreateLiteral(3.45);
  E:=TJSMultiplicativeExpressionMod(TestBinaryExpression(B,TJSMultiplicativeExpressionMod));
  AssertLiteral('Correct left literal for mod',E.A,1.23);
  AssertLiteral('Correct right literal for mod',E.B,3.45);
end;

procedure TTestExpressionConverter.TestBinarySHL;
Var
  B : TBinaryExpr;
  E : TJSLShiftExpression;

begin
  B:=CreateBinary(eopSHL);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSLShiftExpression(TestBinaryExpression(B,TJSLShiftExpression));
  AssertLiteral('Correct left literal for shl',E.A,13);
  AssertLiteral('Correct right literal for shl',E.B,3);
end;

procedure TTestExpressionConverter.TestBinarySHR;
Var
  B : TBinaryExpr;
  E : TJSURShiftExpression;

begin
  B:=CreateBinary(eopSHR);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSURShiftExpression(TestBinaryExpression(B,TJSURShiftExpression));
  AssertLiteral('Correct left literal for shr',E.A,13);
  AssertLiteral('Correct right literal for shr',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryEqual;
Var
  B : TBinaryExpr;
  E : TJSEqualityExpressionSEQ;

begin
  B:=CreateBinary(eopEqual);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSEqualityExpressionSEQ(TestBinaryExpression(B,TJSEqualityExpressionSEQ));
  AssertLiteral('Correct left literal for equal',E.A,13);
  AssertLiteral('Correct right literal for equal',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryNotEqual;
Var
  B : TBinaryExpr;
  E : TJSEqualityExpressionSNE;

begin
  B:=CreateBinary(eopNotEqual);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSEqualityExpressionSNE(TestBinaryExpression(B,TJSEqualityExpressionSNE));
  AssertLiteral('Correct left literal for not equal',E.A,13);
  AssertLiteral('Correct right literal for not equal',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryLessThan;

Var
  B : TBinaryExpr;
  E : TJSRelationalExpressionLT;

begin
  B:=CreateBinary(eopLessThan);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSRelationalExpressionLT(TestBinaryExpression(B,TJSRelationalExpressionLT));
  AssertLiteral('Correct left literal for less than',E.A,13);
  AssertLiteral('Correct right literal for less than',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryLessThanEqual;

Var
  B : TBinaryExpr;
  E : TJSRelationalExpressionLE;

begin
  B:=CreateBinary(eopLessThanEqual);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSRelationalExpressionLE(TestBinaryExpression(B,TJSRelationalExpressionLE));
  AssertLiteral('Correct left literal for less than or equal',E.A,13);
  AssertLiteral('Correct right literal for less than or equal',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryGreater;
Var
  B : TBinaryExpr;
  E : TJSRelationalExpressionGT;

begin
  B:=CreateBinary(eopGreaterThan);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSRelationalExpressionGT(TestBinaryExpression(B,TJSRelationalExpressionGT));
  AssertLiteral('Correct left literal for greater than',E.A,13);
  AssertLiteral('Correct right literal for greater than',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryGreaterThanEqual;

Var
  B : TBinaryExpr;
  E : TJSRelationalExpressionGE;

begin
  B:=CreateBinary(eopGreaterThanEqual);
  B.left:=CreateLiteral(13);
  B.Right:=CreateLiteral(3);
  E:=TJSRelationalExpressionGE(TestBinaryExpression(B,TJSRelationalExpressionGE));
  AssertLiteral('Correct left literal for greater than or equal',E.A,13);
  AssertLiteral('Correct right literal for greater than or equal',E.B,3);
end;

procedure TTestExpressionConverter.TestBinaryIs;
Var
  B : TBinaryExpr;
  E : TJSRelationalExpressionInstanceOf;

begin
  B:=CreateBinary(eopIs);
  B.left:=CreateIdent('a');
  B.Right:=CreateIdent('b');
  E:=TJSRelationalExpressionInstanceOf(TestBinaryExpression(B,TJSRelationalExpressionInstanceOf));
  AssertIdentifier('Correct left literal for is',E.A,'a');
  AssertIdentifier('Correct right literal for is',E.B,'b');
end;

procedure TTestExpressionConverter.TestBinaryPower;
Var
  B : TBinaryExpr;
  E : TJSPowerExpression;

begin
  B:=CreateBinary(eopPower);
  B.left:=CreateIdent('a');
  B.Right:=CreateIdent('b');
  E:=TJSPowerExpression(TestBinaryExpression(B,TJSPowerExpression));
  AssertIdentifier('Correct left literal for power',E.A,'a');
  AssertIdentifier('Correct right literal for power',E.B,'b');
end;

procedure TTestExpressionConverter.TestCallExpressionNone;
Var
  B : TParamsExpr;
  E : TJSCallExpression;

begin
  // a();
  B:=CreateFunctionCall('a',[]);
  E:=TJSCallExpression(Convert(B,TJSCallExpression));
  AssertIdentifier('Correct left literal for is',E.Expr,'a');
  AssertNull('No arguments',E.Args);
//  AssertEquals('No arguments',0,E.Args.Elements.Count);
end;

procedure TTestExpressionConverter.TestCallExpressionOne;

Var
  B : TParamsExpr;
  E : TJSCallExpression;

begin
  // a(b);
  B:=CreateFunctionCall('a',['b']);
  E:=TJSCallExpression(Convert(B,TJSCallExpression));
  AssertIdentifier('Correct left literal for is',E.Expr,'a');
  AssertNotNull('have arguments',E.Args);
  AssertEquals('Argument count',1,E.Args.Elements.Count);
  AssertIdentifier('Argument 1 identifier',E.Args.Elements[0].Expr,'b');
end;

procedure TTestExpressionConverter.TestCallExpressionTwo;
Var
  B : TParamsExpr;
  E : TJSCallExpression;

begin
  // a(b,c);
  B:=CreateFunctionCall('a',['b','c']);
  E:=TJSCallExpression(Convert(B,TJSCallExpression));
  AssertIdentifier('Correct left literal for is',E.Expr,'a');
  AssertNotNull('have arguments',E.Args);
  AssertEquals('Argument count',2,E.Args.Elements.Count);
  AssertIdentifier('Argument 1 identifier',E.Args.Elements[0].Expr,'b');
  AssertIdentifier('Argument 2 identifier',E.Args.Elements[1].Expr,'c');
end;

procedure TTestExpressionConverter.TestMemberExpressionArrayOneDim;

Var
  B : TParamsExpr;
  E : TJSBracketMemberExpression;

begin
  // a[b];
  B:=CreateParamsExpr(pekArrayParams);
  B.Value:=CreateIdent('a');
  B.AddParam(CreateIdent('b'));
  E:=TJSBracketMemberExpression(Convert(B,TJSBracketMemberExpression));
  AssertIdentifier('Correct array name',E.MExpr,'a');
  AssertIdentifier('Correct array member name',E.Name,'b');
end;

procedure TTestExpressionConverter.TestMemberExpressionArrayTwoDim;
Var
  B : TParamsExpr;
begin
  // a[b,c];
  B:=CreateParamsExpr(pekArrayParams);
  B.Value:=CreateIdent('a');
  B.AddParam(CreateIdent('b'));
  B.AddParam(CreateIdent('c'));
  AttemptConvert:=B;
  AssertException('Pascal element not supported: TParamsExpr:TParamsExpr: Cannot convert 2-dim arrays',EPas2JS,@TryConvert);
end;

procedure TTestExpressionConverter.TestVariable;

Var
  VD : TJSVarDeclaration;
  R :TPasVariable;
begin
  R:=CreateVariable('A',Nil);
  VD:=TJSVarDeclaration(Convert(R,TJSVarDeclaration));
  AssertEquals('Correct name, lowercased','a',String(VD.Name));
  AssertNotNull('No init',VD.Init);
end;

procedure TTestExpressionConverter.TestArrayVariable;
Var
  VD : TJSVarDeclaration;
  R :TPasVariable;
  A : TJSArrayLiteral;

begin
  R:=CreateVariable('A',Nil);
  R.VarType:=TPasArrayType.Create('myarray',Nil);
  AddEl(R.VarType);
  VD:=TJSVarDeclaration(Convert(R,TJSVarDeclaration));
  AssertEquals('Correct name, lowercased','a',String(VD.Name));
  A:=TJSArrayLiteral(AssertElement('Init is array literal',TJSArrayLiteral,VD.Init));
  AssertEquals('No elements',0,A.Elements.Count);
end;

procedure TTestTestConverter.TestEmpty;
begin
  AssertNotNull('Have converter',Converter);
end;

procedure TTestConverter.OnAddEl(El: TPasElement; Arg: pointer);
begin
  //writeln('TTestConverter.OnAddEl ',El.Name,':',El.ClassName);
  if FOwnedElements.IndexOf(El)<0 then
    FOwnedElements.Add(El);
  if Arg=nil then ;
end;

procedure TTestConverter.SetUp;
begin
  FOwnedElements:=TFPList.Create;
  FConverter:=TPasToJSConverter.Create;
  FConverter.Globals:=TPasToJSConverterGlobals.Create(FConverter);
end;

procedure TTestConverter.TearDown;
var
  i: Integer;
begin
  for i:=0 to FOwnedElements.Count-1 do
    TPasElement(FOwnedElements[i]).Free;
  FreeAndNil(FOwnedElements);
  FSource:=nil;
  FreeAndNil(FRes);
  FreeAndNil(FConverter);
end;

procedure TTestConverter.TryConvert;
begin
  Convert(FAC,TJSElement);
end;

function TTestConverter.Convert(AElement: TPasElement; AClass: TJSElementClass
  ): TJSElement;
begin
  FSource:=AElement;
  Result:=FConverter.ConvertPasElement(AElement,nil);
  FRes:=Result;
  if (AClass<>Nil) then
    begin
    AssertNotNull('Have conversion result',Result);
    AssertEquals('Conversion result has correct class',AClass,Result.ClassType);
    end;
end;

procedure TTestConverter.AddEl(El: TPasElement);
begin
  FOwnedElements.Add(El);
end;

procedure TTestConverter.AddElWithChildren(El: TPasElement);
begin
  El.ForEachCall(@OnAddEl,nil);
end;

function TTestConverter.CreateElement(aClass: TPTreeElement;
  aParent: TPasElement): TPasElement;
begin
  Result:=aClass.Create('',aParent);
  AddEl(Result);
end;

function TTestConverter.CreatePrimitiveExpr(aParent: TPasElement;
  Kind: TPasExprKind; const Value: string): TPrimitiveExpr;
begin
  Result:=TPrimitiveExpr.Create(aParent,Kind,Value);
  AddEl(Result);
end;

class procedure TTestConverter.AssertEquals(const Msg: String; AExpected,
  AActual: TJSType);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TJSType),Ord(AExpected)),
                   GetEnumName(TypeInfo(TJSType),Ord(AActual)));
end;

class procedure TTestConverter.AssertLiteral(const Msg: String;
  Lit: TJSElement; AType: TJSType);
begin
  AssertNotNull(Msg+': Have instance',Lit);
  AssertEquals(Msg+': Correct class',TJSLIteral,Lit.ClassType);
  AssertEquals(Msg+': Correct value type',AType,TJSLIteral(Lit).Value.ValueType);
end;

class procedure TTestConverter.AssertLiteral(const Msg: String;
  Lit: TJSElement; AValue: Boolean);
begin
  AssertLiteral(Msg,Lit,jstBoolean);
  AssertEquals(Msg+': Correct value',AValue,TJSLiteral(Lit).Value.AsBoolean);
end;

class procedure TTestConverter.AssertLiteral(const Msg: String;
  Lit: TJSElement; AValue: TJSString);
begin
  AssertLiteral(Msg,Lit,jstString);
  AssertEquals(Msg+': Correct value',String(AValue),String(TJSLiteral(Lit).Value.AsString));
end;

class procedure TTestConverter.AssertLiteral(const Msg: String;
  Lit: TJSElement; AValue: TJSNumber);
begin
  AssertLiteral(Msg,Lit,jstNumber);
  AssertEquals(Msg+': Correct value',AValue,TJSLiteral(Lit).Value.AsNumber);
end;

class procedure TTestConverter.AssertIdentifier(const Msg: String;
  Ident: TJSElement; AName: String);
begin
  AssertNotNull(Msg+': Have instance',Ident);
  AssertEquals(Msg+': Correct class',TJSPrimaryExpressionIdent,Ident.ClassType);
  AssertEquals(Msg+': Correct name',AName,String(TJSPrimaryExpressionIdent(Ident).Name));
end;

function TTestConverter.CreateLiteral(AValue: String): TPasExpr;
begin
  Result:=CreatePrimitiveExpr(Nil,pekString,AValue);
end;

function TTestConverter.CreateLiteral(AValue: Double): TPasExpr;

Var
  S : String;

begin
  Str(AValue,S);
  Result:=CreatePrimitiveExpr(Nil,pekNumber,Trim(S));
end;

function TTestConverter.CreateIdent(AName: String): TPrimitiveExpr;
begin
  Result:=CreatePrimitiveExpr(Nil,pekIdent,AName);
end;

function TTestConverter.CreateCondition: TPasExpr;

begin
  Result:=CreateIdent('a');
end;

function TTestConverter.CreateVariable(aName: String; aParent: TPasElement
  ): TPasVariable;
begin
  Result:=TPasVariable.Create(aName,aParent);
  AddEl(Result);
end;

function TTestConverter.CreateBinary(AOpCode: TExprOpCode; aParent: TPasElement
  ): TBinaryExpr;
begin
  Result:=TBinaryExpr.Create(aParent,pekBinary,AOpCode);
  AddEl(Result);
end;

function TTestConverter.CreateBoolConstExpr(Value: boolean): TBoolConstExpr;
begin
  Result:=TBoolConstExpr.Create(Nil,pekBoolConst,Value);
  AddEl(Result);
end;

function TTestConverter.CreateUnaryExpr(AOpCode: TExprOpCode): TUnaryExpr;
begin
  Result:=TUnaryExpr.Create(Nil,pekUnary,AOpCode);
  AddEl(Result);
end;

function TTestConverter.CreateAssignStatement(LHS: String; RHS: String
  ): TPasImplAssign;

begin
  Result:=TPasImplAssign(CreateElement(TPasImplAssign));
  Result.Left:=CreateIdent(LHS);
  Result.Right:=CreateIdent(RHS);
end;

function TTestConverter.CreateParamsExpr(Kind: TPasExprKind;
  aParent: TPasElement): TParamsExpr;
begin
  Result:=TParamsExpr.Create(aParent,Kind);
  AddEl(Result);
end;

function TTestConverter.CreateFunctionCall(AName: String;
  Params: array of String): TParamsExpr;

Var
  I : Integer;

begin
  Result:=TParamsExpr.Create(Nil,pekFuncParams,eopNone);
  AddEl(Result);
  Result.Value:=CreateIdent(AName);
  For I:=Low(Params) to High(Params) do
    Result.AddParam(TPasExpr(CreateIdent(Params[I])));
end;

class procedure TTestConverter.AssertAssignStatement(const Msg: String;
  El: TJSElement; LHS: String; RHS: String);

begin
  AssertNotNull(Msg+': have statement',EL);
  If not (El is TJSSimpleAssignStatement) then
    Fail(Msg+': statement is not assign statement but is '+El.ClassName);
  AssertIdentifier(Msg+': left hand side ('+LHS+')',TJSAssignStatement(EL).LHS,LHS);
  AssertIdentifier(Msg+': left hand side ('+LHS+')',TJSAssignStatement(EL).Expr,RHS);
end;

class procedure TTestConverter.AssertEmptyBlockStatement(const Msg: String;
  El: TJSElement);
begin
  AssertNotNull(Msg+': have statement',EL);
  If not (El is TJSEmptyBlockStatement) then
    Fail(Msg+': statement is not empty block statement but is'+El.ClassName);
end;

class function TTestConverter.AssertListStatement(const Msg: String;
  El: TJSElement): TJSStatementList;
begin
  AssertNotNull(Msg+': have statement',EL);
  If not (El is TJSStatementList) then
    Fail(Msg+': statement is not a list statement but is'+El.ClassName);
  Result:=TJSStatementList(El);
end;

class function TTestConverter.AssertElement(const Msg: String;
  AClass: TJSElementClass; El: TJSElement): TJSElement;
begin
  AssertNotNull(Msg+': have element',El);
  if not (El is ACLass) then
    Fail(Msg+': is not of class '+AClass.ClassName+' but is '+EL.ClassName);
  Result:=El;
end;

Initialization
  RegisterTests([TTestTestConverter,TTestExpressionConverter,TTestStatementConverter]);
end.

