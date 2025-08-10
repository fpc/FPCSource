unit utcExprParsAggr;

{$mode objfpc}
{$h+}
interface

uses
  Classes, SysUtils, math, punit, fpexprpars;

procedure RegisterTests(aTop : PSuite);

implementation

uses typinfo;

type
  TAggregateNode = Class(TFPExprNode)
  Public
    InitCount : Integer;
    UpdateCount : Integer;
    Class Function IsAggregate: Boolean; override;
    Function NodeType: TResultType; override;
    Procedure InitAggregate; override;
    Procedure UpdateAggregate; override;
    procedure GetNodeValue(var Result: TFPExpressionResult); override;
  end;

  TVarCallback = class
    procedure GetVar(var Result: TFPExpressionResult; constref AName: ShortString);
  end;

var
  VarCallBack : TVarCallback;
  FVarValue : Integer;
  FLeft : TAggregateNode;
  FRight : TAggregateNode;
  FFunction : TFPExprIdentifierDef;
  FFunction2 : TFPExprIdentifierDef;


procedure TVarCallback.GetVar(var Result: TFPExpressionResult; constref AName: ShortString);
begin
  Result.ResultType:=FFunction2.ResultType;
  Case Result.ResultType of
    rtInteger : Result.ResInteger:=FVarValue;
    rtFloat : Result.ResFloat:=FVarValue / 2;
    rtCurrency : Result.ResCurrency:=FVarValue / 2;
  end;
end;

procedure AssertEquals(Msg: String; AExpected, AActual: TResultType); overload;
begin
  AssertEquals(Msg, ResultTypeName(AExpected), ResultTypeName(AActual));
end;


function SuiteSetup: TTestString;
begin
  Result := '';
  FVarValue:=0;
  VarCallBack:=TVarCallback.Create;
  FFunction:=TFPExprIdentifierDef.Create(Nil);
  FFunction.Name:='Count';
  FFunction2:=TFPExprIdentifierDef.Create(Nil);
  FFunction2.Name:='MyVar';
  FFunction2.ResultType:=rtInteger;
  FFunction2.IdentifierType:=itVariable;
  FFunction2.OnGetVariableValue:=@VarCallBack.GetVar;
  FLeft:=TAggregateNode.Create;
  FRight:=TAggregateNode.Create;
end;

function SuiteTearDown: TTestString;
begin
  Result := '';
  FreeAndNil(VarCallBack);
  FreeAndNil(FFunction);
  FreeAndNil(FFunction2);
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
end;

function TestParserAggregate_TestIsAggregate: TTestString;
begin
  Result:='';
  AssertEquals('ExprNode',False,TFPExprNode.IsAggregate);
  AssertEquals('TAggregateExpr',True,TAggregateExpr.IsAggregate);
  AssertEquals('TAggregateExpr',False,TFPBinaryOperation.IsAggregate);
end;

function TestParserAggregate_TestHasAggregate: TTestString;
Var
  N :  TFPExprNode;
begin
  Result:='';
  N:=TFPExprNode.Create;
  try
    AssertEquals('ExprNode',False,N.HasAggregate);
  finally
    N.Free;
  end;
  N:=TAggregateExpr.Create;
  try
    AssertEquals('TAggregateExpr',True,N.HasAggregate);
  finally
    N.Free;
  end;
end;

function TestParserAggregate_TestBinaryAggregate: TTestString;
Var
  B :  TFPBinaryOperation;
begin
  Result:='';
  B:=TFPBinaryOperation.Create(Fleft,TFPConstExpression.CreateInteger(1));
  try
    FLeft:=Nil;
    AssertEquals('Binary',True,B.HasAggregate);
  finally
    B.Free;
    FLeft:=TAggregateNode.Create; // Recreate for next test
  end;
  B:=TFPBinaryOperation.Create(TFPConstExpression.CreateInteger(1),FRight);
  try
    FRight:=Nil;
    AssertEquals('Binary',True,B.HasAggregate);
  finally
    B.Free;
    FRight:=TAggregateNode.Create; // Recreate for next test
  end;
end;

function TestParserAggregate_TestUnaryAggregate: TTestString;
Var
  B : TFPUnaryOperator;
begin
  Result:='';
  B:=TFPUnaryOperator.Create(Fleft);
  try
    FLeft:=Nil;
    AssertEquals('Unary',True,B.HasAggregate);
  finally
    B.Free;
    FLeft:=TAggregateNode.Create; // Recreate for next test
  end;
end;

function TestParserAggregate_TestCountAggregate: TTestString;
Var
  C : TAggregateCount;
  I : Integer;
  R : TFPExpressionResult;
begin
  Result:='';
  FFunction.ResultType:=rtInteger;
  FFunction.ParameterTypes:='';
  C:=TAggregateCount.CreateFunction(FFunction,Nil);
  try
    C.Check;
    C.InitAggregate;
    For I:=1 to 11 do
      C.UpdateAggregate;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtInteger,R.ResultType);
    AssertEquals('Correct value',11,R.ResInteger);
  finally
    C.Free;
  end;
end;

function TestParserAggregate_TestSumAggregate: TTestString;
Var
  C : TAggregateSum;
  V : TFPExprVariable;
  I : Integer;
  R : TFPExpressionResult;
  A : TExprArgumentArray;
begin
  Result:='';
  FFunction.ResultType:=rtInteger;
  FFunction.ParameterTypes:='I';
  FFunction.Name:='SUM';
  FFunction2.ResultType:=rtInteger;
  C:=Nil;
  V:=TFPExprVariable.CreateIdentifier(FFunction2);
  try
    SetLength(A,1);
    A[0]:=V;
    C:=TAggregateSum.CreateFunction(FFunction,A);
    C.Check;
    C.InitAggregate;
    For I:=1 to 10 do
      begin
      FVarValue:=I;
      C.UpdateAggregate;
      end;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtInteger,R.ResultType);
    AssertEquals('Correct value',55,R.ResInteger);
  finally
    C.Free;
  end;
end;

function TestParserAggregate_TestSumAggregate2: TTestString;
Var
  C : TAggregateSum;
  V : TFPExprVariable;
  I : Integer;
  R : TFPExpressionResult;
  A : TExprArgumentArray;
begin
  Result:='';
  FFunction.ResultType:=rtFloat;
  FFunction.ParameterTypes:='F';
  FFunction.Name:='SUM';
  FFunction2.ResultType:=rtFloat;
  C:=Nil;
  V:=TFPExprVariable.CreateIdentifier(FFunction2);
  try
    SetLength(A,1);
    A[0]:=V;
    C:=TAggregateSum.CreateFunction(FFunction,A);
    C.Check;
    C.InitAggregate;
    For I:=1 to 10 do
      begin
      FVarValue:=I;
      C.UpdateAggregate;
      end;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtFloat,R.ResultType);
    AssertEquals('Correct value',55/2,R.ResFloat,0.1);
  finally
    C.Free;
  end;
end;

function TestParserAggregate_TestSumAggregate3: TTestString;
Var
  C : TAggregateSum;
  V : TFPExprVariable;
  I : Integer;
  R : TFPExpressionResult;
  A : TExprArgumentArray;
begin
  Result:='';
  FFunction.ResultType:=rtCurrency;
  FFunction.ParameterTypes:='F';
  FFunction.Name:='SUM';
  FFunction2.ResultType:=rtCurrency;
  C:=Nil;
  V:=TFPExprVariable.CreateIdentifier(FFunction2);
  try
    SetLength(A,1);
    A[0]:=V;
    C:=TAggregateSum.CreateFunction(FFunction,A);
    C.Check;
    C.InitAggregate;
    For I:=1 to 10 do
      begin
      FVarValue:=I;
      C.UpdateAggregate;
      end;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtCurrency,R.ResultType);
    AssertEquals('Correct value',55/2,R.ResCurrency,0.1);
  finally
    C.Free;
  end;
end;

function TestParserAggregate_TestAvgAggregate: TTestString;
Var
  C : TAggregateAvg;
  V : TFPExprVariable;
  I : Integer;
  R : TFPExpressionResult;
  A : TExprArgumentArray;
begin
  Result:='';
  FFunction.ResultType:=rtInteger;
  FFunction.ParameterTypes:='F';
  FFunction.Name:='AVG';
  FFunction2.ResultType:=rtInteger;
  C:=Nil;
  V:=TFPExprVariable.CreateIdentifier(FFunction2);
  try
    SetLength(A,1);
    A[0]:=V;
    C:=TAggregateAvg.CreateFunction(FFunction,A);
    C.Check;
    C.InitAggregate;
    For I:=1 to 10 do
      begin
      FVarValue:=I;
      C.UpdateAggregate;
      end;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtFloat,R.ResultType);
    AssertEquals('Correct value',5.5,R.ResFloat,0.1);
  finally
    C.Free;
  end;
end;

function TestParserAggregate_TestAvgAggregate2: TTestString;
Var
  C : TAggregateAvg;
  V : TFPExprVariable;
  I : Integer;
  R : TFPExpressionResult;
  A : TExprArgumentArray;
begin
  Result:='';
  FFunction.ResultType:=rtInteger;
  FFunction.ParameterTypes:='F';
  FFunction.Name:='AVG';
  FFunction2.ResultType:=rtFloat;
  C:=Nil;
  V:=TFPExprVariable.CreateIdentifier(FFunction2);
  try
    SetLength(A,1);
    A[0]:=V;
    C:=TAggregateAvg.CreateFunction(FFunction,A);
    C.Check;
    C.InitAggregate;
    For I:=1 to 10 do
      begin
      FVarValue:=I;
      C.UpdateAggregate;
      end;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtFloat,R.ResultType);
    AssertEquals('Correct value',5.5/2,R.ResFloat,0.1);
  finally
    C.Free;
  end;
end;

function TestParserAggregate_TestAvgAggregate3: TTestString;
Var
  C : TAggregateAvg;
  V : TFPExprVariable;
  R : TFPExpressionResult;
  A : TExprArgumentArray;
begin
  Result:='';
  FFunction.ResultType:=rtInteger;
  FFunction.ParameterTypes:='F';
  FFunction.Name:='AVG';
  FFunction2.ResultType:=rtFloat;
  C:=Nil;
  V:=TFPExprVariable.CreateIdentifier(FFunction2);
  try
    SetLength(A,1);
    A[0]:=V;
    C:=TAggregateAvg.CreateFunction(FFunction,A);
    C.Check;
    C.InitAggregate;
    C.GetNodeValue(R);
    AssertEquals('Correct type',rtFloat,R.ResultType);
    AssertEquals('Correct value',0.0,R.ResFloat,0.1);
  finally
    C.Free;
  end;
end;

{ TAggregateNode }

class function TAggregateNode.IsAggregate: Boolean;
begin
  Result:=True
end;

function TAggregateNode.NodeType: TResultType;
begin
  Result:=rtInteger;
end;

procedure TAggregateNode.InitAggregate;
begin
  inherited InitAggregate;
  inc(InitCount)
end;

procedure TAggregateNode.UpdateAggregate;
begin
  inherited UpdateAggregate;
  inc(UpdateCount);
end;

procedure TAggregateNode.GetNodeValue(var Result: TFPExpressionResult);
begin
  Result.ResultType:=rtInteger;
  Result.ResInteger:=updateCount;
end;

procedure RegisterTests(aTop: PSuite);

var
  lSuite : PSuite;

begin
  lSuite:=AddSuite('TParserAggregateTests', @SuiteSetup, @SuiteTearDown, aTop);
  AddTest('TestIsAggregate', @TestParserAggregate_TestIsAggregate, lSuite);
  AddTest('TestHasAggregate', @TestParserAggregate_TestHasAggregate, lSuite);
  AddTest('TestBinaryAggregate', @TestParserAggregate_TestBinaryAggregate, lSuite);
  AddTest('TestUnaryAggregate', @TestParserAggregate_TestUnaryAggregate, lSuite);
  AddTest('TestCountAggregate', @TestParserAggregate_TestCountAggregate, lSuite);
  AddTest('TestSumAggregate', @TestParserAggregate_TestSumAggregate, lSuite);
  AddTest('TestSumAggregate2', @TestParserAggregate_TestSumAggregate2, lSuite);
  AddTest('TestSumAggregate3', @TestParserAggregate_TestSumAggregate3, lSuite);
  AddTest('TestAvgAggregate', @TestParserAggregate_TestAvgAggregate, lSuite);
  AddTest('TestAvgAggregate2', @TestParserAggregate_TestAvgAggregate2, lSuite);
  AddTest('TestAvgAggregate3', @TestParserAggregate_TestAvgAggregate3, lSuite);
end;

end.
