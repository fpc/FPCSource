unit utcexprparsops;

{$mode objfpc}
{$h+}
interface

uses
  Classes, SysUtils, math, punit, fpexprpars;

procedure RegisterTests(aTop : PSuite);

implementation

uses typinfo;

Type
  TMyDestroyNode = Class(TFPConstExpression)
  Public
    Destructor Destroy; override;
  end;

Var
  FCheckNode : TFPExprNode;
  FDestroyCalled : Integer;

procedure DoCheck;
begin
  if Assigned(FCheckNode) then
    FCheckNode.Check;
end;

procedure AssertNodeType(Msg: String; AClass: TClass;  ANode: TFPExprNode);
begin
  AssertNotNull(Msg+': Not null',ANode);
  AssertEquals(Msg+': Class OK',AClass,ANode.ClassType);
end;

procedure AssertEquals(Msg: String; AResultType: TResultType;  ANode: TFPExprNode); overload;
begin
  AssertNotNull(Msg+': Node not null',ANode);
  AssertEquals(Msg,ResultTypeName(AResultType),ResultTypeName(Anode.NodeType));
end;

procedure AssertEquals(Msg: String; AExpected, AActual: TResultType); overload;
begin
  AssertEquals(Msg,ResultTypeName(AExpected),ResultTypeName(AActual));
end;

function CreateIntNode(AInteger: Integer): TFPExprNode;
begin
  Result:=TFPConstExpression.CreateInteger(AInteger);
end;

function CreateFloatNode(AFloat: TExprFloat): TFPExprNode;
begin
  Result:=TFPConstExpression.CreateFloat(AFloat);
end;

function CreateStringNode(Astring: String): TFPExprNode;
begin
  Result:=TFPConstExpression.CreateString(AString);
end;

function CreateDateTimeNode(ADateTime: TDateTime): TFPExprNode;
begin
  Result:=TFPConstExpression.CreateDateTime(ADateTime);
end;

function CreateBoolNode(ABoolean: Boolean): TFPExprNode;
begin
  Result:=TFPConstExpression.CreateBoolean(ABoolean);
end;

procedure AssertNodeOK(FN: TFPExprNode);
Var
  B : Boolean;
  Msg : String;
begin
  AssertNotNull('Node to test OK',FN);
  B:=False;
  try
    FN.Check;
    B:=True;
  except
    On E : Exception do
      Msg:=E.Message;
  end;
  If Not B then
    Fail(Format('Node %s not OK: %s',[FN.ClassName,Msg]));
end;

procedure AssertNodeNotOK(const MSg : String; FN: TFPExprNode);
begin
  FCheckNode:=FN;
  AssertException(Msg,EExprParser,@DoCheck);
end;

destructor TMyDestroyNode.Destroy;
begin
  Inc(FDestroyCalled);
  inherited Destroy;
end;

function SuiteSetup: TTestString;
begin
  Result := '';
  FCheckNode := Nil;
  FDestroyCalled:=0;
end;

function SuiteTearDown: TTestString;
begin
  Result := '';
end;

Function TestIfOperation_TestCreateInteger : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateIntNode(1),CreateIntNode(2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',1,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
  FN:=TIfOperation.Create(CreateBoolNode(False),CreateIntNode(1),CreateIntNode(2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',2,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBoolean : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',True,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBoolean2 : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(False),CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',False,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateString : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateStringNode('a'),CreateStringNode('b'));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','a',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
  FN:=TIfOperation.Create(CreateBoolNode(False),CreateStringNode('a'),CreateStringNode('b'));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','b',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateFloat : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateFloatNode(1.23),CreateFloatNode(2.34));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtFloat,FN.NodeType);
    AssertEquals('Correct result',1.23,FN.NodeValue.ResFloat);
  finally
    FN.Free;
  end;
  FN:=TIfOperation.Create(CreateBoolNode(False),CreateFloatNode(1.23),CreateFloatNode(2.34));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtFloat,FN.NodeType);
    AssertEquals('Correct result',2.34,FN.NodeValue.ResFloat);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateDateTime : TTestString;
var
  FN : TIfOperation;
  D1,D2 : TDateTime;
begin
  Result:='';
  D1:=Now;
  D2:=D1-1;
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateDateTimeNode(D1),CreateDateTimeNode(D2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtDateTime,FN.NodeType);
    AssertEquals('Correct result',D1,FN.NodeValue.ResDateTime);
  finally
    FN.Free;
  end;
  FN:=TIfOperation.Create(CreateBoolNode(False),CreateDateTimeNode(D1),CreateDateTimeNode(D2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtDateTime,FN.NodeType);
    AssertEquals('Correct result',D2,FN.NodeValue.ResDateTime);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBooleanInteger : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateBoolNode(True),CreateIntNode(2));
  try
    AssertNodeNotOK('No type promotion for boolean',FN);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBooleanInteger2 : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateIntNode(2),CreateBoolNode(True));
  try
    AssertNodeNotOK('No type promotion for boolean',FN);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBooleanString : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateBoolNode(True),CreateStringNode('b'));
  try
    AssertNodeNotOK('No type promotion for boolean',FN);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBooleanString2 : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateStringNode('b'),CreateBoolNode(True));
  try
    AssertNodeNotOK('No type promotion for boolean',FN);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBooleanDateTime : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateBoolNode(True),CreateDateTimeNode(Now));
  try
    AssertNodeNotOK('No type promotion for boolean',FN);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestCreateBooleanDateTime2 : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FN:=TIfOperation.Create(CreateBoolNode(True),CreateDateTimeNode(Now),CreateBoolNode(True));
  try
    AssertNodeNotOK('No type promotion for boolean',FN);
  finally
    FN.Free;
  end;
end;

Function TestIfOperation_TestDestroy : TTestString;
var
  FN : TIfOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TIfOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for all nodes',3,FDestroyCalled);
end;

function CreateArgs(  Args: array of const): TExprArgumentArray;

Var
  I : Integer;

begin
  Result:=Default(TExprArgumentArray);
  SetLength(Result,High(Args)-Low(Args)+1);
  For I:=Low(Args) to High(Args) do
    Result[I]:=Args[i].VObject as TFPExprNode;
end;


Function TestCaseOperation_TestCreateOne : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(False)]));
  try
    AssertNodeNotOK('Too little arguments',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestCreateTwo : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(False),CreateBoolNode(False)]));
  try
    AssertNodeNotOK('Too little arguments',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestCreateThree : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(False),CreateBoolNode(False),CreateBoolNode(False)]));
  try
    AssertNodeNotOK('Too little arguments',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestCreateOdd : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(False),CreateBoolNode(False),
                                          CreateBoolNode(False),CreateBoolNode(False),
                                          CreateBoolNode(False)]));
  try
    AssertNodeNotOK('Odd number of arguments',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestCreateNoExpression : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(False),
                                          CreateBoolNode(False),
                                          TFPBinaryOrOperation.Create(CreateBoolNode(False),CreateBoolNode(False)),
                                          CreateBoolNode(False)]));
  try
    AssertNodeNotOK('Label is not a constant expression',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestCreateWrongLabel : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateBoolNode(False),
                                        CreateIntNode(1),CreateBoolNode(False),
                                        CreateBoolNode(True),CreateBoolNode(False)]));
  try
    AssertNodeNotOK('Wrong label type',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestCreateWrongValue : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateBoolNode(False),
                                          CreateIntNode(1),CreateBoolNode(False),
                                          CreateIntNode(2),CreateIntNode(1)]));
  try
    AssertNodeNotOK('Wrong value',FN);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestIntegerTag : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateStringNode('many'),
                                          CreateIntNode(1),CreateStringNode('one'),
                                          CreateIntNode(2),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','one',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestIntegerTagDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateStringNode('many'),
                                        CreateIntNode(1),CreateStringNode('one'),
                                        CreateIntNode(2),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','many',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestStringTag : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateStringNode('many'),
                                        CreateIntNode(1),CreateStringNode('one'),
                                        CreateIntNode(2),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','many',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestStringTagDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateStringNode('many'),CreateIntNode(3),
                                        CreateStringNode('one'),CreateIntNode(1),
                                        CreateStringNode('two'),CreateIntNode(2)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',3,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestFloatTag : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateFloatNode(1.0),CreateStringNode('many'),
                                        CreateFloatNode(1.0),CreateStringNode('one'),
                                        CreateFloatNode(2.0),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','one',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestFloatTagDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateFloatNode(1.0),CreateStringNode('many'),
                                          CreateFloatNode(1.0),CreateStringNode('one'),
                                          CreateFloatNode(2.0),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','one',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestBooleanTag : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(True),CreateStringNode('unknown'),
                                        CreateBoolNode(True),CreateStringNode('one'),
                                        CreateBoolNode(False),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','one',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestBooleanTagDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateBoolNode(True),CreateStringNode('unknown'),
                                        CreateBoolNode(False),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','unknown',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestDateTimeTag : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateDateTimeNode(Date),CreateStringNode('later'),
                                        CreateDateTimeNode(Date),CreateStringNode('today'),
                                        CreateDateTimeNode(Date+1),CreateStringNode('tomorrow')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','today',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestDateTimeTagDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateDateTimeNode(Date),CreateStringNode('later'),
                                          CreateDateTimeNode(Date),CreateStringNode('today'),
                                          CreateDateTimeNode(Date+1),CreateStringNode('tomorrow')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','today',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestIntegerValue : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateIntNode(0),
                                          CreateIntNode(1),CreateIntNode(-1),
                                          CreateIntNode(2),CreateIntNode(-2)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',-1,FN.NodeValue.ResInteger);  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestIntegerValueDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateIntNode(0),
                                        CreateIntNode(1),CreateIntNode(-1),
                                        CreateIntNode(2),CreateIntNode(-2)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',0,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestStringValue : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateStringNode('many'),
                                        CreateIntNode(1),CreateStringNode('one'),
                                        CreateIntNode(2),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','one',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestStringValueDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateStringNode('many'),
                                        CreateIntNode(1),CreateStringNode('one'),
                                        CreateIntNode(2),CreateStringNode('two')]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtString,FN.NodeType);
    AssertEquals('Correct result','many',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestFloatValue : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateFloatNode(0.0),
                                        CreateIntNode(1),CreateFloatNode(2.0),
                                        CreateIntNode(2),CreateFloatNode(1.0)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtFloat,FN.NodeType);
    AssertEquals('Correct result',2.0,FN.NodeValue.ResFloat);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestFloatValueDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateFloatNode(0.0),
                                        CreateIntNode(1),CreateFloatNode(2.0),
                                        CreateIntNode(2),CreateFloatNode(1.0)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtFloat,FN.NodeType);
    AssertEquals('Correct result',0.0,FN.NodeValue.ResFloat);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestBooleanValue : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateBoolNode(False),
                                        CreateIntNode(1),CreateBoolNode(True),
                                        CreateIntNode(2),CreateBoolNode(False)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',True,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestBooleanValueDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateBoolNode(False),
                                        CreateIntNode(1),CreateBoolNode(True),
                                        CreateIntNode(2),CreateBoolNode(False)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',False,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestDateTimeValue : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(1),CreateDateTimeNode(Date+1),
                                        CreateIntNode(1),CreateDateTimeNode(Date),
                                        CreateIntNode(2),CreateDateTimeNode(Date-1)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtDateTime,FN.NodeType);
    AssertEquals('Correct result',Date,FN.NodeValue.ResDateTime);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestDateTimeValueDefault : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([CreateIntNode(3),CreateDateTimeNode(Date+1),
                                        CreateIntNode(1),CreateDateTimeNode(Date),
                                        CreateIntNode(2),CreateDateTimeNode(Date-1)]));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtDateTime,FN.NodeType);
    AssertEquals('Correct result',Date+1,FN.NodeValue.ResDateTime);
  finally
    FN.Free;
  end;
end;

Function TestCaseOperation_TestDestroy : TTestString;
var
  FN : TCaseOperation;
begin
  Result:='';
  FN:=TCaseOperation.Create(CreateArgs([TMyDestroyNode.CreateInteger(1),
                                        TMyDestroyNode.CreateInteger(1),
                                        TMyDestroyNode.CreateInteger(2),
                                        TMyDestroyNode.CreateInteger(3)]));
  FreeAndNil(FN);
  AssertEquals('Destroy called for operand',4,FDestroyCalled)
end;


procedure RegisterTests(aTop: PSuite);
var
  lSuite,lParent : PSuite;
begin
  lParent:=AddSuite('ExprParserOps',@SuiteSetup, @SuiteTearDown, aTop);
  lSuite:=AddSuite('TIfOperation', @SuiteSetup, @SuiteTearDown,lParent,True);
  AddTest('TestCreateInteger', @TestIfOperation_TestCreateInteger, lSuite);
  AddTest('TestCreateBoolean', @TestIfOperation_TestCreateBoolean, lSuite);
  AddTest('TestCreateBoolean2', @TestIfOperation_TestCreateBoolean2, lSuite);
  AddTest('TestCreateString', @TestIfOperation_TestCreateString, lSuite);
  AddTest('TestCreateFloat', @TestIfOperation_TestCreateFloat, lSuite);
  AddTest('TestCreateDateTime', @TestIfOperation_TestCreateDateTime, lSuite);
  AddTest('TestCreateBooleanInteger', @TestIfOperation_TestCreateBooleanInteger, lSuite);
  AddTest('TestCreateBooleanInteger2', @TestIfOperation_TestCreateBooleanInteger2, lSuite);
  AddTest('TestCreateBooleanString', @TestIfOperation_TestCreateBooleanString, lSuite);
  AddTest('TestCreateBooleanString2', @TestIfOperation_TestCreateBooleanString2, lSuite);
  AddTest('TestCreateBooleanDateTime', @TestIfOperation_TestCreateBooleanDateTime, lSuite);
  AddTest('TestCreateBooleanDateTime2', @TestIfOperation_TestCreateBooleanDateTime2, lSuite);
  AddTest('TestDestroy', @TestIfOperation_TestDestroy, lSuite);

  lSuite:=AddSuite('TCaseOperationTests', @SuiteSetup, @SuiteTearDown,lParent,true);
  AddTest('TestCreateOne', @TestCaseOperation_TestCreateOne, lSuite);
  AddTest('TestCreateTwo', @TestCaseOperation_TestCreateTwo, lSuite);
  AddTest('TestCreateThree', @TestCaseOperation_TestCreateThree, lSuite);
  AddTest('TestCreateOdd', @TestCaseOperation_TestCreateOdd, lSuite);
  AddTest('TestCreateNoExpression', @TestCaseOperation_TestCreateNoExpression, lSuite);
  AddTest('TestCreateWrongLabel', @TestCaseOperation_TestCreateWrongLabel, lSuite);
  AddTest('TestCreateWrongValue', @TestCaseOperation_TestCreateWrongValue, lSuite);
  AddTest('TestIntegerTag', @TestCaseOperation_TestIntegerTag, lSuite);
  AddTest('TestIntegerTagDefault', @TestCaseOperation_TestIntegerTagDefault, lSuite);
  AddTest('TestStringTag', @TestCaseOperation_TestStringTag, lSuite);
  AddTest('TestStringTagDefault', @TestCaseOperation_TestStringTagDefault, lSuite);
  AddTest('TestFloatTag', @TestCaseOperation_TestFloatTag, lSuite);
  AddTest('TestFloatTagDefault', @TestCaseOperation_TestFloatTagDefault, lSuite);
  AddTest('TestBooleanTag', @TestCaseOperation_TestBooleanTag, lSuite);
  AddTest('TestBooleanTagDefault', @TestCaseOperation_TestBooleanTagDefault, lSuite);
  AddTest('TestDateTimeTag', @TestCaseOperation_TestDateTimeTag, lSuite);
  AddTest('TestDateTimeTagDefault', @TestCaseOperation_TestDateTimeTagDefault, lSuite);
  AddTest('TestIntegerValue', @TestCaseOperation_TestIntegerValue, lSuite);
  AddTest('TestIntegerValueDefault', @TestCaseOperation_TestIntegerValueDefault, lSuite);
  AddTest('TestStringValue', @TestCaseOperation_TestStringValue, lSuite);
  AddTest('TestStringValueDefault', @TestCaseOperation_TestStringValueDefault, lSuite);
  AddTest('TestFloatValue', @TestCaseOperation_TestFloatValue, lSuite);
  AddTest('TestFloatValueDefault', @TestCaseOperation_TestFloatValueDefault, lSuite);
  AddTest('TestBooleanValue', @TestCaseOperation_TestBooleanValue, lSuite);
  AddTest('TestBooleanValueDefault', @TestCaseOperation_TestBooleanValueDefault, lSuite);
  AddTest('TestDateTimeValue', @TestCaseOperation_TestDateTimeValue, lSuite);
  AddTest('TestDateTimeValueDefault', @TestCaseOperation_TestDateTimeValueDefault, lSuite);
  AddTest('TestDestroy', @TestCaseOperation_TestDestroy, lSuite);

end;

end.
