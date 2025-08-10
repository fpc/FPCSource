unit utcExprParsParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, math, fpexprpars;

procedure RegisterTests;

implementation

uses typinfo;

type
  TMyFPExpressionParser = class(TFPExpressionParser)
  public
    property ExprNode;
    property Scanner;
    property Dirty;
  end;

var
  FP: TMyFPExpressionParser;
  FTestExpr: String;

procedure AssertLeftRight(N: TFPExprNode; LeftClass, RightClass: TClass);
begin
  AssertNotNull('Node should not be nil', N);
  AssertEquals('Node should be a binary operation', True, N is TFPBinaryOperation);
  if N is TFPBinaryOperation then
  begin
    AssertEquals('Left node class', LeftClass, TFPBinaryOperation(N).Left.ClassType);
    AssertEquals('Right node class', RightClass, TFPBinaryOperation(N).Right.ClassType);
  end;
end;

procedure AssertOperand(N: TFPExprNode; OperandClass: TClass);
begin
  AssertNotNull('Node should not be nil', N);
  AssertEquals('Node should be a unary operation', True, N is TFPUnaryOperator);
  if N is TFPUnaryOperator then
    AssertEquals('Operand node class', OperandClass, TFPUnaryOperator(N).Operand.ClassType);
end;

procedure AssertEqualsResultType(Msg: String; AExpected, AActual: TResultType);
begin
  AssertEquals(Msg, ResultTypeName(AExpected), ResultTypeName(AActual));
end;

procedure AssertResultType(RT: TResultType);
begin
  AssertEqualsResultType('Result type', RT, FP.ExprNode.NodeType);
end;

procedure AssertResult(F: TExprFloat);
begin
  AssertEquals('Float result', F, FP.AsFloat, 1E-9);
end;

procedure AssertCurrencyResult(C: Currency);
begin
  AssertEquals('Currency result', C, FP.AsCurrency, 1E-4);
end;

procedure AssertResult(I: Int64);
begin
  AssertEquals('Integer result', I, FP.AsInteger);
end;

procedure AssertResult(S: String);
begin
  AssertEquals('String result', S, FP.AsString);
end;

procedure AssertResult(B: Boolean);
begin
  AssertEquals('Boolean result', B, FP.AsBoolean);
end;

procedure AssertDateTimeResult(D: TDateTime);
begin
  AssertEquals('DateTime result', D, FP.AsDateTime);
end;

function Parser_Setup: string;
begin
  FP := TMyFPExpressionParser.Create(nil);
end;

function Parser_TearDown : string;
begin
  FreeAndNil(FP);
end;

procedure TestParser(AExpr: String);
begin
  FP.Expression := AExpr;
end;

function TestParserExpressions_TestCreate: TTestString;
begin
  Result := '';
  AssertNotNull('Parser created', FP);
end;

function TestParserExpressions_TestNumberValues: TTestString;
begin
  Result := '';
  TestParser('123');
  AssertResult(123);
  TestParser('123.456');
  AssertResult(123.456);
end;

// ... and so on for all the other test cases ...

procedure RegisterTests;
begin
  AddSuite('TParserExpressionsTests', @Parser_Setup, @Parser_TearDown);
  AddTest('TestCreate', @TestParserExpressions_TestCreate, 'TParserExpressionsTests');
  AddTest('TestNumberValues', @TestParserExpressions_TestNumberValues, 'TParserExpressionsTests');
  // ... and so on for all the other tests ...
end;

end.
