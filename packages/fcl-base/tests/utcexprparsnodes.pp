unit utcExprParsNodes;

{$mode objfpc}
{$h+}
interface

uses
  Classes, SysUtils, punit, fpexprpars;

procedure RegisterTests(aTop : PSuite);

procedure AssertEquals(Msg: String; AResultType: TResultType;  ANode: TFPExprNode); overload;
procedure AssertEquals(Msg: String; AExpected, AActual: TResultType); overload;
procedure AssertNodeType(Msg: String; AClass: TClass;  ANode: TFPExprNode);


implementation

Type
  TMyDestroyNode = Class(TFPConstExpression)
  Public
    Destructor Destroy; override;
  end;

Var
  FCheckNode : TFPExprNode;
  FDestroyCalled : Integer;
  
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

procedure DoCheck;
begin
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
  AssertEquals(Msg,AResultType,Anode.NodeType);
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

function CreateBoolNode(ABoolean: Boolean): TFPExprNode;
begin
  Result:=TFPConstExpression.CreateBoolean(ABoolean);
end;

destructor TMyDestroyNode.Destroy;
begin
  Inc(FDestroyCalled);
  inherited Destroy;
end;

Function TestDestroyNode_TestDestroy : TTestString;
Var
  FN : TMyDestroyNode;
begin
  Result:='';
  FDestroyCalled := 0;
  AssertEquals('Destroy not called yet',0,FDestroyCalled);
  FN:=TMyDestroyNode.CreateInteger(1);
  FN.Free;
  AssertEquals('Destroy called',1,FDestroyCalled)
end;

Function TestConstExprNode_TestCreateInteger : TTestString;
var
  FN : TFPConstExpression;
begin
  Result:='';
  FN:=TFPConstExpression.CreateInteger(1);
  try
    AssertEquals('Correct type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',1,FN.ConstValue.ResInteger);
    AssertEquals('Correct result',1,FN.NodeValue.ResInteger);
    AssertEquals('AsString ok','1',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestConstExprNode_TestCreateFloat : TTestString;
Var
  FN : TFPConstExpression;
  F : Double;
  C : Integer;
begin
  Result:='';
  FN:=TFPConstExpression.CreateFloat(2.34);
  try
    AssertEquals('Correct type',rtFloat,FN.NodeType);
    AssertEquals('Correct result',2.34,FN.ConstValue.ResFloat);
    AssertEquals('Correct result',2.34,FN.NodeValue.ResFloat);
    Val(FN.AsString,F,C);
    AssertEquals('Correct conversion',0,C);
    AssertEquals('AsString ok',2.34,F,0.001);
  finally
    FN.Free;
  end;
end;

Function TestConstExprNode_TestCreateBoolean : TTestString;
var
  FN : TFPConstExpression;
begin
  Result:='';
  FN:=TFPConstExpression.CreateBoolean(True);
  try
    AssertEquals('Correct type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',True,FN.ConstValue.ResBoolean);
    AssertEquals('Correct result',True,FN.NodeValue.ResBoolean);
    AssertEquals('AsString ok','True',FN.AsString);
  finally
    FN.Free;
  end;
  FN:=TFPConstExpression.CreateBoolean(False);
  try
    AssertEquals('AsString ok','False',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestConstExprNode_TestCreateDateTime : TTestString;
Var
  FN : TFPConstExpression;
  D : TDateTime;
  S : String;
begin
  Result:='';
  D:=Now;
  FN:=TFPConstExpression.CreateDateTime(D);
  try
    AssertEquals('Correct type',rtDateTime,FN.NodeType);
    AssertEquals('Correct result',D,FN.ConstValue.ResDateTime);
    AssertEquals('Correct result',D,FN.NodeValue.ResDateTime);
    S:=''''+FormatDateTime('cccc',D)+'''';
    AssertEquals('AsString ok',S,FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestConstExprNode_TestCreateString : TTestString;
Var
  FN : TFPConstExpression;
  S : String;
begin
  Result:='';
  S:='Ohlala';
  FN:=TFPConstExpression.CreateString(S);
  try
    AssertEquals('Correct type',rtString,FN.NodeType);
    AssertEquals('Correct result',S,FN.ConstValue.ResString);
    AssertEquals('Correct result',S,FN.NodeValue.ResString);
    AssertEquals('AsString ok',''''+S+'''',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestNegateExprNode_TestCreateInteger : TTestString;
var
  FN : TFPNegateOperation;
begin
  Result:='';
  FN:=TFPNegateOperation.Create(CreateIntNode(23));
  try
    AssertEquals('Negate has correct type',rtInteger,FN.NodeType);
    AssertEquals('Negate has correct result',-23,FN.NodeValue.Resinteger);
    AssertEquals('Negate has correct string','-23',FN.AsString);
    AssertNodeOK(FN);
  finally
    FN.Free;
  end;
end;

Function TestNegateExprNode_TestCreateFloat : TTestString;
var
  FN : TFPNegateOperation;
  S : String;
begin
  Result:='';
  FN:=TFPNegateOperation.Create(CreateFloatNode(1.23));
  try
    AssertEquals('Negate has correct type',rtFloat,FN.NodeType);
    AssertEquals('Negate has correct result',-1.23,FN.NodeValue.ResFloat);
    Str(TExprFloat(-1.23),S);
    AssertEquals('Negate has correct string',S,FN.AsString);
    AssertNodeOK(FN);
  finally
    FN.Free;
  end;
end;

Function TestNegateExprNode_TestCreateOther1 : TTestString;
var
  FN : TFPNegateOperation;
begin
  Result:='';
  FN:=TFPNegateOperation.Create(TFPConstExpression.CreateString('1.23'));
  try
    AssertNodeNotOK('Negate does not accept string',FN);
  finally
    FN.Free;
  end;
end;

Function TestNegateExprNode_TestCreateOther2 : TTestString;
var
  FN : TFPNegateOperation;
begin
  Result:='';
  FN:=TFPNegateOperation.Create(TFPConstExpression.CreateBoolean(True));
  try
    AssertNodeNotOK('Negate does not accept boolean',FN)
  finally
    FN.Free;
  end;
end;

Function TestNegateExprNode_TestDestroy : TTestString;
var
  FN : TFPNegateOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPNegateOperation.Create(TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Operand Destroy called',1,FDestroyCalled)
end;

Function TestBinaryAndNode_TestCreateInteger : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FN:=TFPBinaryAndOperation.Create(CreateIntNode(3),CreateIntNode(2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',2,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestBinaryAndNode_TestCreateBoolean : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FN:=TFPBinaryAndOperation.Create(CreateBoolNode(True),CreateBoolNode(True));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',True,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestBinaryAndNode_TestCreateBooleanInteger : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FN:=TFPBinaryAndOperation.Create(CreateBoolNode(True),CreateIntNode(0));
  try
    AssertNodeNotOK('Different node types',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryAndNode_TestCreateString : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FN:=TFPBinaryAndOperation.Create(CreateStringNode('True'),CreateStringNode('True'));
  try
    AssertNodeNotOK('String node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryAndNode_TestCreateFloat : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FN:=TFPBinaryAndOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.23));
  try
    AssertNodeNotOK('float node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryAndNode_TestCreateDateTime : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FN:=TFPBinaryAndOperation.Create(CreateDateTimeNode(Now),CreateDateTimeNode(Now));
  try
    AssertNodeNotOK('DateTime node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryAndNode_TestDestroy : TTestString;
var
  FN : TFPBinaryAndOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPBinaryAndOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled)
end;

Function TestNotNode_TestCreateInteger : TTestString;
var
  FN : TFPNotNode;
begin
  Result:='';
  FN:=TFPNotNode.Create(CreateIntNode(3));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',Not(Int64(3)),FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestNotNode_TestCreateBoolean : TTestString;
var
  FN : TFPNotNode;
begin
  Result:='';
  FN:=TFPNotNode.Create(CreateBoolNode(True));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',False,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestNotNode_TestCreateString : TTestString;
var
  FN : TFPNotNode;
begin
  Result:='';
  FN:=TFPNotNode.Create(CreateStringNode('True'));
  try
    AssertNodeNotOK('String node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestNotNode_TestCreateFloat : TTestString;
var
  FN : TFPNotNode;
begin
  Result:='';
  FN:=TFPNotNode.Create(CreateFloatNode(1.23));
  try
    AssertNodeNotOK('String node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestNotNode_TestCreateDateTime : TTestString;
var
  FN : TFPNotNode;
begin
  Result:='';
  FN:=TFPNotNode.Create(CreateDateTimeNode(Now));
  try
    AssertNodeNotOK('String node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestNotNode_TestDestroy : TTestString;
var
  FN : TFPNotNode;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPNotNode.Create(TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for operand',1,FDestroyCalled)
end;

Function TestBinaryOrNode_TestCreateInteger : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FN:=TFPBinaryOrOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',3,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestBinaryOrNode_TestCreateBoolean : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FN:=TFPBinaryOrOperation.Create(CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',True,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestBinaryOrNode_TestCreateBooleanInteger : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FN:=TFPBinaryOrOperation.Create(CreateBoolNode(True),CreateIntNode(0));
  try
    AssertNodeNotOK('Different node types',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryOrNode_TestCreateString : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FN:=TFPBinaryOrOperation.Create(CreateStringNode('True'),CreateStringNode('True'));
  try
    AssertNodeNotOK('String node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryOrNode_TestCreateFloat : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FN:=TFPBinaryOrOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.23));
  try
    AssertNodeNotOK('float node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryOrNode_TestCreateDateTime : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FN:=TFPBinaryOrOperation.Create(CreateDateTimeNode(Now),CreateDateTimeNode(Now));
  try
    AssertNodeNotOK('DateTime node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryOrNode_TestDestroy : TTestString;
var
  FN : TFPBinaryOrOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPBinaryOrOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled)
end;

Function TestBinaryXorNode_TestCreateInteger : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FN:=TFPBinaryXorOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtInteger,FN.NodeType);
    AssertEquals('Correct result',3,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TestBinaryXorNode_TestCreateBoolean : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FN:=TFPBinaryXorOperation.Create(CreateBoolNode(True),CreateBoolNode(True));
  try
    AssertNodeOK(FN);
    AssertEquals('Correct node type',rtBoolean,FN.NodeType);
    AssertEquals('Correct result',False,FN.NodeValue.ResBoolean);
  finally
    FN.Free;
  end;
end;

Function TestBinaryXorNode_TestCreateBooleanInteger : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FN:=TFPBinaryXorOperation.Create(CreateBoolNode(True),CreateIntNode(0));
  try
    AssertNodeNotOK('Different node types',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryXorNode_TestCreateString : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FN:=TFPBinaryXorOperation.Create(CreateStringNode('True'),CreateStringNode('True'));
  try
    AssertNodeNotOK('String node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryXorNode_TestCreateFloat : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FN:=TFPBinaryXorOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.23));
  try
    AssertNodeNotOK('float node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryXorNode_TestCreateDateTime : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FN:=TFPBinaryXorOperation.Create(CreateDateTimeNode(Now),CreateDateTimeNode(Now));
  try
    AssertNodeNotOK('DateTime node type',FN);
  finally
    FN.Free;
  end;
end;

Function TestBinaryXorNode_TestDestroy : TTestString;
var
  FN : TFPBinaryXorOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPBinaryXorOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled)
end;

procedure TestEqualNode(B: TFPBooleanResultOperation; AResult: Boolean);
begin
  AssertEquals(Format('Test %s(%s,%s) result',[B.ClassName,B.Left.AsString,B.Right.AsString]),AResult,B.NodeValue.resBoolean);
end;

Function TestEqualNode_TestCreateIntegerEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateIntNode(1),CreateIntNode(1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateIntegerUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateIntNode(2),CreateIntNode(1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateFloatEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.23));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateFloatUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.34));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateStringEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateStringNode('now'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateStringUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateStringNode('now'),CreateStringNode('then'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateBooleanEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateBoolNode(True),CreateBoolNode(True));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateBooleanUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateBoolNode(False),CreateBoolNode(True));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateDateTimeEqual : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestCreateDateTimeUnEqual : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D-1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestDestroy : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPEqualOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled)
end;

Function TestEqualNode_TestWrongTypes1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateIntNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestWrongTypes2 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateDateTimeNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestWrongTypes3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateFloatNode(1.3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestWrongTypes4 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateBoolNode(False),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestWrongTypes5 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateFloatNode(1),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestEqualNode_TestAsString : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPEqualOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 = 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateIntegerEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateIntNode(1),CreateIntNode(1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateIntegerUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateIntNode(2),CreateIntNode(1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateFloatEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.23));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateFloatUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateFloatNode(1.23),CreateFloatNode(1.34));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateStringEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateStringNode('now'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateStringUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateStringNode('now'),CreateStringNode('then'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateBooleanEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateBoolNode(True),CreateBoolNode(True));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateBooleanUnEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateBoolNode(False),CreateBoolNode(True));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateDateTimeEqual : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPUnEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestCreateDateTimeUnEqual : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPUnEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D-1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestDestroy : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPUnEqualOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled)
end;

Function TestUnEqualNode_TestWrongTypes1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateIntNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestWrongTypes2 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateDateTimeNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestWrongTypes3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateFloatNode(1.3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestWrongTypes4 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateBoolNode(False),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestWrongTypes5 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateFloatNode(1),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestUnEqualNode_TestAsString : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPUnEqualOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 <> 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestCreateDateTimeSmaller : TTestString;
Var
   FN : TFPBooleanResultOperation;
   D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPLessThanOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D+1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestCreateDateTimeLarger : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPLessThanOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D-1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestCreateStringEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateStringNode('now'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestCreateStringSmaller : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateStringNode('now'),CreateStringNode('then'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestCreateStringLarger : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateStringNode('then'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestWrongTypes1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateIntNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestWrongTypes2 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateDateTimeNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestWrongTypes3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateFloatNode(1.3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestWrongTypes4 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateBoolNode(False),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestWrongTypes5 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateFloatNode(1.23),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestNoBoolean1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateBoolNode(False),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestNoBoolean2 : TTestString;
 var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateIntNode(1),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestNoBoolean3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateBoolNode(False),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanNode_TestAsString : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 < 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestCreateDateTimeSmaller : TTestString;
Var
   FN : TFPBooleanResultOperation;
   D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPLessThanEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D+1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestCreateDateTimeLarger : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPLessThanEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D-1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestCreateStringEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateStringNode('now'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestCreateStringSmaller : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateStringNode('now'),CreateStringNode('then'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestCreateStringLarger : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateStringNode('then'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestWrongTypes1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateIntNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestWrongTypes2 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateDateTimeNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestWrongTypes3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateFloatNode(1.3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestWrongTypes4 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateBoolNode(False),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestWrongTypes5 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateFloatNode(1.23),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestNoBoolean1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateBoolNode(False),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestNoBoolean2 : TTestString;
 var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateIntNode(1),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestNoBoolean3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateBoolNode(False),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLessThanEqualNode_TestAsString : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPLessThanEqualOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 <= 2',FN.AsString);
  finally
    FN.Free;
  end;
end;


Function TestLargerThanNode_TestCreateDateTimeSmaller : TTestString;
Var
   FN : TFPBooleanResultOperation;
   D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPGreaterThanOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D+1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestCreateDateTimeLarger : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPGreaterThanOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D-1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestCreateStringEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateStringNode('now'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestCreateStringSmaller : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateStringNode('now'),CreateStringNode('then'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestCreateStringLarger : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateStringNode('then'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestWrongTypes1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateIntNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestWrongTypes2 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateDateTimeNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestWrongTypes3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateFloatNode(1.3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestWrongTypes4 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateBoolNode(False),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestWrongTypes5 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateFloatNode(1.23),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestNoBoolean1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateBoolNode(False),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestNoBoolean2 : TTestString;
 var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateIntNode(1),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestNoBoolean3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateBoolNode(False),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanNode_TestAsString : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 > 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestCreateDateTimeSmaller : TTestString;
Var
   FN : TFPBooleanResultOperation;
   D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPGreaterThanEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D+1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestCreateDateTimeLarger : TTestString;
Var
  FN : TFPBooleanResultOperation;
  D : TDateTime;
begin
  Result:='';
  D:=Now;
  FN:=TFPGreaterThanEqualOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(D-1));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestCreateStringEqual : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateStringNode('now'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestCreateStringSmaller : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateStringNode('now'),CreateStringNode('then'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,False);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestCreateStringLarger : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateStringNode('then'),CreateStringNode('now'));
  try
    AssertNodeOk(FN);
    AssertEquals('Boolean result',rtBoolean,FN.NodeType);
    TestEqualNode(FN,True);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestWrongTypes1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateIntNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestWrongTypes2 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateDateTimeNode(3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestWrongTypes3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateFloatNode(1.3),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestWrongTypes4 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateBoolNode(False),CreateStringNode('1.23'));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestWrongTypes5 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateFloatNode(1.23),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestNoBoolean1 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateBoolNode(False),CreateIntNode(1));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestNoBoolean2 : TTestString;
 var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateIntNode(1),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestNoBoolean3 : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateBoolNode(False),CreateBoolNode(False));
  try
    AssertNodeNotOk('Wrong Types',FN);
  finally
    FN.Free;
  end;
end;

Function TestLargerThanEqualNode_TestAsString : TTestString;
var
  FN : TFPBooleanResultOperation;
begin
  Result:='';
  FN:=TFPGreaterThanEqualOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 >= 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TTestAddNode_TestCreateInteger : TTestString;
var
  FN : TFPAddOperation;
begin
  Result:='';
  FN:=TFPAddOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Add has correct type',rtInteger,FN.NodeType);
    AssertEquals('Add has correct result',3,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TTestAddNode_TestCreateFloat : TTestString;
var
  FN : TFPAddOperation;
begin
  Result:='';
  FN:=TFPAddOperation.Create(CreateFloatNode(1.23),CreateFloatNode(4.56));
  try
    AssertEquals('Add has correct type',rtFloat,FN.NodeType);
    AssertEquals('Add has correct result',5.79,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestAddNode_TestCreateDateTime : TTestString;
var
  FN : TFPAddOperation;
  D,T : TDateTime;
begin
  Result:='';
  D:=Date;
  T:=Time;
  FN:=TFPAddOperation.Create(CreateDateTimeNode(D),CreateDateTimeNode(T));
  try
    AssertEquals('Add has correct type',rtDateTime,FN.NodeType);
    AssertEquals('Add has correct result',D+T,FN.NodeValue.ResDateTime);
  finally
    FN.Free;
  end;
end;

Function TTestAddNode_TestCreateString : TTestString;
var
  FN : TFPAddOperation;
begin
  Result:='';
  FN:=TFPAddOperation.Create(CreateStringNode('alo'),CreateStringNode('ha'));
  try
    AssertEquals('Add has correct type',rtString,FN.NodeType);
    AssertEquals('Add has correct result','aloha',FN.NodeValue.ResString);
  finally
    FN.Free;
  end;
end;

Function TTestAddNode_TestCreateBoolean : TTestString;
var
  FN : TFPAddOperation;
begin
  Result:='';
  FN:=TFPAddOperation.Create(CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeNotOK('No boolean addition',FN);
  finally
    FN.Free;
  end;
end;

Function TTestAddNode_TestDestroy : TTestString;
var
  FN : TFPAddOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPAddOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled);
end;

Function TTestAddNode_TestAsString : TTestString;
var
  FN : TFPAddOperation;
begin
  Result:='';
  FN:=TFPAddOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 + 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TTestSubtractNode_TestCreateInteger : TTestString;
var
  FN : TFPSubtractOperation;
begin
  Result:='';
  FN:=TFPSubtractOperation.Create(CreateIntNode(4),CreateIntNode(1));
  try
    AssertEquals('Subtract has correct type',rtInteger,FN.NodeType);
    AssertEquals('Subtract has correct result',3,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TTestSubtractNode_TestCreateFloat : TTestString;
var
  FN : TFPSubtractOperation;
begin
  Result:='';
  FN:=TFPSubtractOperation.Create(CreateFloatNode(4.56),CreateFloatNode(1.23));
  try
    AssertEquals('Subtract has correct type',rtFloat,FN.NodeType);
    AssertEquals('Subtract has correct result',3.33,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestSubtractNode_TestCreateDateTime : TTestString;
var
  FN : TFPSubtractOperation;
  D,T : TDateTime;
begin
  Result:='';
  D:=Date;
  T:=Time;
  FN:=TFPSubtractOperation.Create(CreateDateTimeNode(D+T),CreateDateTimeNode(T));
  try
    AssertEquals('Subtract has correct type',rtDateTime,FN.NodeType);
    AssertEquals('Subtract has correct result',D,FN.NodeValue.ResDateTime);
  finally
    FN.Free;
  end;
end;

Function TTestSubtractNode_TestCreateString : TTestString;
var
  FN : TFPSubtractOperation;
begin
  Result:='';
  FN:=TFPSubtractOperation.Create(CreateStringNode('alo'),CreateStringNode('ha'));
  try
    AssertNodeNotOK('No string Subtract',FN);
  finally
    FN.Free;
  end;
end;

Function TTestSubtractNode_TestCreateBoolean : TTestString;
var
  FN : TFPSubtractOperation;
begin
  Result:='';
  FN:=TFPSubtractOperation.Create(CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeNotOK('No boolean Subtract',FN);
  finally
    FN.Free;
  end;
end;

Function TTestSubtractNode_TestDestroy : TTestString;
var
  FN : TFPSubtractOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPSubtractOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled);
end;

Function TTestSubtractNode_TestAsString : TTestString;
var
  FN : TFPSubtractOperation;
begin
  Result:='';
  FN:=TFPSubtractOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 - 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TTestMultiplyNode_TestCreateInteger : TTestString;
var
  FN : TFPMultiplyOperation;
begin
  Result:='';
  FN:=TFPMultiplyOperation.Create(CreateIntNode(4),CreateIntNode(2));
  try
    AssertEquals('multiply has correct type',rtInteger,FN.NodeType);
    AssertEquals('multiply has correct result',8,FN.NodeValue.ResInteger);
  finally
    FN.Free;
  end;
end;

Function TTestMultiplyNode_TestCreateFloat : TTestString;
var
  FN : TFPMultiplyOperation;
begin
  Result:='';
  FN:=TFPMultiplyOperation.Create(CreateFloatNode(2.0),CreateFloatNode(1.23));
  try
    AssertEquals('multiply has correct type',rtFloat,FN.NodeType);
    AssertEquals('multiply has correct result',2.46,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestMultiplyNode_TestCreateDateTime : TTestString;
var
  FN : TFPMultiplyOperation;
  D,T : TDateTime;
begin
  Result:='';
  D:=Date;
  T:=Time;
  FN:=TFPMultiplyOperation.Create(CreateDateTimeNode(D+T),CreateDateTimeNode(T));
  try
    AssertNodeNotOK('No datetime multiply',FN);
  finally
    FN.Free;
  end;
end;

Function TTestMultiplyNode_TestCreateString : TTestString;
var
  FN : TFPMultiplyOperation;
begin
  Result:='';
  FN:=TFPMultiplyOperation.Create(CreateStringNode('alo'),CreateStringNode('ha'));
  try
    AssertNodeNotOK('No string multiply',FN);
  finally
    FN.Free;
  end;
end;

Function TTestMultiplyNode_TestCreateBoolean : TTestString;
var
  FN : TFPMultiplyOperation;
begin
  Result:='';
  FN:=TFPMultiplyOperation.Create(CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeNotOK('No boolean multiply',FN);
  finally
    FN.Free;
  end;
end;

Function TTestMultiplyNode_TestDestroy : TTestString;
var
  FN : TFPMultiplyOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPMultiplyOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled);
end;

Function TTestMultiplyNode_TestAsString : TTestString;
var
  FN : TFPMultiplyOperation;
begin
  Result:='';
  FN:=TFPMultiplyOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 * 2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TTestPowerNode_TestCreateInteger : TTestString;
var
  FN : TFPPowerOperation;
begin
  Result:='';
  FN:=TFPPowerOperation.Create(CreateIntNode(4),CreateIntNode(2));
  try
    AssertEquals('Power has correct type',rtfloat,FN.NodeType);
    AssertEquals('Power has correct result',16.0,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestPowerNode_TestCreateFloat : TTestString;
var
  FN : TFPPowerOperation;
begin
  Result:='';
  FN:=TFPPowerOperation.Create(CreateFloatNode(2.0),CreateFloatNode(3.0));
  try
    AssertEquals('Power has correct type',rtFloat,FN.NodeType);
    AssertEquals('Power has correct result',8.0,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestPowerNode_TestCreateDateTime : TTestString;
var
  FN : TFPPowerOperation;
  D,T : TDateTime;
begin
  Result:='';
  D:=Date;
  T:=Time;
  FN:=TFPPowerOperation.Create(CreateDateTimeNode(D+T),CreateDateTimeNode(T));
  try
    AssertNodeNotOK('No datetime Power',FN);
  finally
    FN.Free;
  end;
end;

Function TTestPowerNode_TestCreateString : TTestString;
var
  FN : TFPPowerOperation;
begin
  Result:='';
  FN:=TFPPowerOperation.Create(CreateStringNode('alo'),CreateStringNode('ha'));
  try
    AssertNodeNotOK('No string Power',FN);
  finally
    FN.Free;
  end;
end;

Function TTestPowerNode_TestCreateBoolean : TTestString;
var
  FN : TFPPowerOperation;
begin
  Result:='';
  FN:=TFPPowerOperation.Create(CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeNotOK('No boolean Power',FN);
  finally
    FN.Free;
  end;
end;

Function TTestPowerNode_TestDestroy : TTestString;
var
  FN : TFPPowerOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPPowerOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled);
end;

Function TTestPowerNode_TestAsString : TTestString;
var
  FN : TFPPowerOperation;
begin
  Result:='';
  FN:=TFPPowerOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1^2',FN.AsString);
  finally
    FN.Free;
  end;
end;

Function TTestDivideNode_TestCreateInteger : TTestString;
var
  FN : TFPDivideOperation;
begin
  Result:='';
  FN:=TFPDivideOperation.Create(CreateIntNode(4),CreateIntNode(2));
  try
    AssertEquals('Divide has correct type',rtfloat,FN.NodeType);
    AssertEquals('Divide has correct result',2.0,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestDivideNode_TestCreateFloat : TTestString;
var
  FN : TFPDivideOperation;
begin
  Result:='';
  FN:=TFPDivideOperation.Create(CreateFloatNode(9.0),CreateFloatNode(3.0));
  try
    AssertEquals('Divide has correct type',rtFloat,FN.NodeType);
    AssertEquals('Divide has correct result',3.0,FN.NodeValue.ResFloat, 1e-9);
  finally
    FN.Free;
  end;
end;

Function TTestDivideNode_TestCreateDateTime : TTestString;
var
  FN : TFPDivideOperation;
  D,T : TDateTime;
begin
  Result:='';
  D:=Date;
  T:=Time;
  FN:=TFPDivideOperation.Create(CreateDateTimeNode(D+T),CreateDateTimeNode(T));
  try
    AssertNodeNotOK('No datetime division',FN);
  finally
    FN.Free;
  end;
end;

Function TTestDivideNode_TestCreateString : TTestString;
var
  FN : TFPDivideOperation;
begin
  Result:='';
  FN:=TFPDivideOperation.Create(CreateStringNode('alo'),CreateStringNode('ha'));
  try
    AssertNodeNotOK('No string division',FN);
  finally
    FN.Free;
  end;
end;

Function TTestDivideNode_TestCreateBoolean : TTestString;
var
  FN : TFPDivideOperation;
begin
  Result:='';
  FN:=TFPDivideOperation.Create(CreateBoolNode(True),CreateBoolNode(False));
  try
    AssertNodeNotOK('No boolean division',FN);
  finally
    FN.Free;
  end;
end;

Function TTestDivideNode_TestDestroy : TTestString;
var
  FN : TFPDivideOperation;
begin
  Result:='';
  FDestroyCalled := 0;
  FN:=TFPDivideOperation.Create(TMyDestroyNode.CreateInteger(1),TMyDestroyNode.CreateInteger(1));
  FN.Free;
  AssertEquals('Destroy called for left and right nodes',2,FDestroyCalled);
end;

Function TTestDivideNode_TestAsString : TTestString;
var
  FN : TFPDivideOperation;
begin
  Result:='';
  FN:=TFPDivideOperation.Create(CreateIntNode(1),CreateIntNode(2));
  try
    AssertEquals('Asstring works ok','1 / 2',FN.AsString);
  finally
    FN.Free;
  end;
end;


procedure RegisterTests(aTop: PSuite);
var
  lParentSuite,lSuite : PSuite;
begin
  lParentSuite:=AddSuite('TExprParsNodes', @SuiteSetup, @SuiteTearDown, aTop);
  AddTest('TestDestroyNode', @TestDestroyNode_TestDestroy, 'TExprParsNodes');

  lSuite:=AddSuite('TestConstExprNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestConstCreateInteger', @TestConstExprNode_TestCreateInteger, lSuite);
  AddTest('TestConstCreateFloat', @TestConstExprNode_TestCreateFloat, lSuite);
  AddTest('TestConstCreateBoolean', @TestConstExprNode_TestCreateBoolean, lSuite);
  AddTest('TestConstCreateDateTime', @TestConstExprNode_TestCreateDateTime, lSuite);
  AddTest('TestConstCreateString', @TestConstExprNode_TestCreateString, lSuite);

  lSuite:=AddSuite('TestNegateExprNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestNegateCreateInteger', @TestNegateExprNode_TestCreateInteger, lSuite);
  AddTest('TestNegateCreateFloat', @TestNegateExprNode_TestCreateFloat, lSuite);
  AddTest('TestNegateCreateOther1', @TestNegateExprNode_TestCreateOther1, lSuite);
  AddTest('TestNegateCreateOther2', @TestNegateExprNode_TestCreateOther2, lSuite);
  AddTest('TestNegateDestroy', @TestNegateExprNode_TestDestroy, lSuite);

  lSuite:=AddSuite('TestBinaryAndNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestBinaryAndCreateInteger', @TestBinaryAndNode_TestCreateInteger, lSuite);
  AddTest('TestBinaryAndCreateBoolean', @TestBinaryAndNode_TestCreateBoolean, lSuite);
  AddTest('TestBinaryAndCreateBooleanInteger', @TestBinaryAndNode_TestCreateBooleanInteger, lSuite);
  AddTest('TestBinaryAndCreateString', @TestBinaryAndNode_TestCreateString, lSuite);
  AddTest('TestBinaryAndCreateFloat', @TestBinaryAndNode_TestCreateFloat, lSuite);
  AddTest('TestBinaryAndCreateDateTime', @TestBinaryAndNode_TestCreateDateTime, lSuite);
  AddTest('TestBinaryAndDestroy', @TestBinaryAndNode_TestDestroy, lSuite);

  lSuite:=AddSuite('TestNotNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestNotCreateInteger', @TestNotNode_TestCreateInteger, lSuite);
  AddTest('TestNotCreateBoolean', @TestNotNode_TestCreateBoolean, lSuite);
  AddTest('TestNotCreateString', @TestNotNode_TestCreateString, lSuite);
  AddTest('TestNotCreateFloat', @TestNotNode_TestCreateFloat, lSuite);
  AddTest('TestNotCreateDateTime', @TestNotNode_TestCreateDateTime, lSuite);
  AddTest('TestNotDestroy', @TestNotNode_TestDestroy, lSuite);

  lSuite:=AddSuite('TestBinaryOrNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestBinaryOrCreateInteger', @TestBinaryOrNode_TestCreateInteger, lSuite);
  AddTest('TestBinaryOrCreateBoolean', @TestBinaryOrNode_TestCreateBoolean, lSuite);
  AddTest('TestBinaryOrCreateBooleanInteger', @TestBinaryOrNode_TestCreateBooleanInteger, lSuite);
  AddTest('TestBinaryOrCreateString', @TestBinaryOrNode_TestCreateString, lSuite);
  AddTest('TestBinaryOrCreateFloat', @TestBinaryOrNode_TestCreateFloat, lSuite);
  AddTest('TestBinaryOrCreateDateTime', @TestBinaryOrNode_TestCreateDateTime, lSuite);
  AddTest('TestBinaryOrDestroy', @TestBinaryOrNode_TestDestroy, lSuite);
  AddTest('TestBinaryXorCreateInteger', @TestBinaryXorNode_TestCreateInteger, lSuite);
  AddTest('TestBinaryXorCreateBoolean', @TestBinaryXorNode_TestCreateBoolean, lSuite);
  AddTest('TestBinaryXorCreateBooleanInteger', @TestBinaryXorNode_TestCreateBooleanInteger, lSuite);
  AddTest('TestBinaryXorCreateString', @TestBinaryXorNode_TestCreateString, lSuite);
  AddTest('TestBinaryXorCreateFloat', @TestBinaryXorNode_TestCreateFloat, lSuite);
  AddTest('TestBinaryXorCreateDateTime', @TestBinaryXorNode_TestCreateDateTime, lSuite);
  AddTest('TestBinaryXorDestroy', @TestBinaryXorNode_TestDestroy, lSuite);

  lSuite:=AddSuite('TestEqualNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestEqualCreateIntegerEqual', @TestEqualNode_TestCreateIntegerEqual, lSuite);
  AddTest('TestEqualCreateIntegerUnEqual', @TestEqualNode_TestCreateIntegerUnEqual, lSuite);
  AddTest('TestEqualCreateFloatEqual', @TestEqualNode_TestCreateFloatEqual, lSuite);
  AddTest('TestEqualCreateFloatUnEqual', @TestEqualNode_TestCreateFloatUnEqual, lSuite);
  AddTest('TestEqualCreateStringEqual', @TestEqualNode_TestCreateStringEqual, lSuite);
  AddTest('TestEqualCreateStringUnEqual', @TestEqualNode_TestCreateStringUnEqual, lSuite);
  AddTest('TestEqualCreateBooleanEqual', @TestEqualNode_TestCreateBooleanEqual, lSuite);
  AddTest('TestEqualCreateBooleanUnEqual', @TestEqualNode_TestCreateBooleanUnEqual, lSuite);
  AddTest('TestEqualCreateDateTimeEqual', @TestEqualNode_TestCreateDateTimeEqual, lSuite);
  AddTest('TestEqualCreateDateTimeUnEqual', @TestEqualNode_TestCreateDateTimeUnEqual, lSuite);
  AddTest('TestEqualDestroy', @TestEqualNode_TestDestroy, lSuite);
  AddTest('TestEqualWrongTypes1', @TestEqualNode_TestWrongTypes1, lSuite);
  AddTest('TestEqualWrongTypes2', @TestEqualNode_TestWrongTypes2, lSuite);
  AddTest('TestEqualWrongTypes3', @TestEqualNode_TestWrongTypes3, lSuite);
  AddTest('TestEqualWrongTypes4', @TestEqualNode_TestWrongTypes4, lSuite);
  AddTest('TestEqualWrongTypes5', @TestEqualNode_TestWrongTypes5, lSuite);
  AddTest('TestEqualAsString', @TestEqualNode_TestAsString, lSuite);

  lSuite:=AddSuite('TestUnEqualNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestUnEqualCreateIntegerEqual', @TestUnEqualNode_TestCreateIntegerEqual, lSuite);
  AddTest('TestUnEqualCreateIntegerUnEqual', @TestUnEqualNode_TestCreateIntegerUnEqual, lSuite);
  AddTest('TestUnEqualCreateFloatEqual', @TestUnEqualNode_TestCreateFloatEqual, lSuite);
  AddTest('TestUnEqualCreateFloatUnEqual', @TestUnEqualNode_TestCreateFloatUnEqual, lSuite);
  AddTest('TestUnEqualCreateStringEqual', @TestUnEqualNode_TestCreateStringEqual, lSuite);
  AddTest('TestUnEqualCreateStringUnEqual', @TestUnEqualNode_TestCreateStringUnEqual, lSuite);
  AddTest('TestUnEqualCreateBooleanEqual', @TestUnEqualNode_TestCreateBooleanEqual, lSuite);
  AddTest('TestUnEqualCreateBooleanUnEqual', @TestUnEqualNode_TestCreateBooleanUnEqual, lSuite);
  AddTest('TestUnEqualCreateDateTimeEqual', @TestUnEqualNode_TestCreateDateTimeEqual, lSuite);
  AddTest('TestUnEqualCreateDateTimeUnEqual', @TestUnEqualNode_TestCreateDateTimeUnEqual, lSuite);
  AddTest('TestUnEqualDestroy', @TestUnEqualNode_TestDestroy, lSuite);
  AddTest('TestUnEqualWrongTypes1', @TestUnEqualNode_TestWrongTypes1, lSuite);
  AddTest('TestUnEqualWrongTypes2', @TestUnEqualNode_TestWrongTypes2, lSuite);
  AddTest('TestUnEqualWrongTypes3', @TestUnEqualNode_TestWrongTypes3, lSuite);
  AddTest('TestUnEqualWrongTypes4', @TestUnEqualNode_TestWrongTypes4, lSuite);
  AddTest('TestUnEqualWrongTypes5', @TestUnEqualNode_TestWrongTypes5, lSuite);
  AddTest('TestUnEqualAsString', @TestUnEqualNode_TestAsString, lSuite);

  lSuite:=AddSuite('TestLessThanNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestLessThanCreateDateTimeSmaller',@TestLessThanNode_TestCreateDateTimeSmaller, lSuite);
  AddTest('TestLessThanCreateDateTimeLarger',@TestLessThanNode_TestCreateDateTimeLarger, lSuite);
  AddTest('TestLessThanCreateStringEqual',@TestLessThanNode_TestCreateStringEqual, lSuite);
  AddTest('TestLessThanCreateStringSmaller',@TestLessThanNode_TestCreateStringSmaller, lSuite);
  AddTest('TestLessThanCreateStringLarger',@TestLessThanNode_TestCreateStringLarger, lSuite);
  AddTest('TestLessThanWrongTypes1',@TestLessThanNode_TestWrongTypes1, lSuite);
  AddTest('TestLessThanWrongTypes2',@TestLessThanNode_TestWrongTypes2, lSuite);
  AddTest('TestLessThanWrongTypes3',@TestLessThanNode_TestWrongTypes3, lSuite);
  AddTest('TestLessThanWrongTypes4',@TestLessThanNode_TestWrongTypes4, lSuite);
  AddTest('TestLessThanWrongTypes5',@TestLessThanNode_TestWrongTypes5, lSuite);
  AddTest('TestLessThanNoBoolean1',@TestLessThanNode_TestNoBoolean1, lSuite);
  AddTest('TestLessThanNoBoolean2',@TestLessThanNode_TestNoBoolean2, lSuite);
  AddTest('TestLessThanNoBoolean3',@TestLessThanNode_TestNoBoolean3, lSuite);
  AddTest('TestLessThanAsString',@TestLessThanNode_TestAsString, lSuite);

  lSuite:=AddSuite('TestLessThanEqualNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestLessThanEqualCreateDateTimeSmaller',@TestLessThanEqualNode_TestCreateDateTimeSmaller, lSuite);
  AddTest('TestLessThanEqualCreateDateTimeLarger',@TestLessThanEqualNode_TestCreateDateTimeLarger, lSuite);
  AddTest('TestLessThanEqualCreateStringEqual',@TestLessThanEqualNode_TestCreateStringEqual, lSuite);
  AddTest('TestLessThanEqualCreateStringSmaller',@TestLessThanEqualNode_TestCreateStringSmaller, lSuite);
  AddTest('TestLessThanEqualCreateStringLarger',@TestLessThanEqualNode_TestCreateStringLarger, lSuite);
  AddTest('TestLessThanEqualWrongTypes1',@TestLessThanEqualNode_TestWrongTypes1, lSuite);
  AddTest('TestLessThanEqualWrongTypes2',@TestLessThanEqualNode_TestWrongTypes2, lSuite);
  AddTest('TestLessThanEqualWrongTypes3',@TestLessThanEqualNode_TestWrongTypes3, lSuite);
  AddTest('TestLessThanEqualWrongTypes4',@TestLessThanEqualNode_TestWrongTypes4, lSuite);
  AddTest('TestLessThanEqualWrongTypes5',@TestLessThanEqualNode_TestWrongTypes5, lSuite);
  AddTest('TestLessThanEqualNoBoolean1',@TestLessThanEqualNode_TestNoBoolean1, lSuite);
  AddTest('TestLessThanEqualNoBoolean2',@TestLessThanEqualNode_TestNoBoolean2, lSuite);
  AddTest('TestLessThanEqualNoBoolean3',@TestLessThanEqualNode_TestNoBoolean3, lSuite);
  AddTest('TestLessThanEqualAsString',@TestLessThanEqualNode_TestAsString, lSuite);

  lSuite:=AddSuite('TestLargerThanNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestLargerThanCreateDateTimeSmaller',@TestLargerThanNode_TestCreateDateTimeSmaller, lSuite);
  AddTest('TestLargerThanCreateDateTimeLarger',@TestLargerThanNode_TestCreateDateTimeLarger, lSuite);
  AddTest('TestLargerThanCreateStringEqual',@TestLargerThanNode_TestCreateStringEqual, lSuite);
  AddTest('TestLargerThanCreateStringSmaller',@TestLargerThanNode_TestCreateStringSmaller, lSuite);
  AddTest('TestLargerThanCreateStringLarger',@TestLargerThanNode_TestCreateStringLarger, lSuite);
  AddTest('TestLargerThanWrongTypes1',@TestLargerThanNode_TestWrongTypes1, lSuite);
  AddTest('TestLargerThanWrongTypes2',@TestLargerThanNode_TestWrongTypes2, lSuite);
  AddTest('TestLargerThanWrongTypes3',@TestLargerThanNode_TestWrongTypes3, lSuite);
  AddTest('TestLargerThanWrongTypes4',@TestLargerThanNode_TestWrongTypes4, lSuite);
  AddTest('TestLargerThanWrongTypes5',@TestLargerThanNode_TestWrongTypes5, lSuite);
  AddTest('TestLargerThanNoBoolean1',@TestLargerThanNode_TestNoBoolean1, lSuite);
  AddTest('TestLargerThanNoBoolean2',@TestLargerThanNode_TestNoBoolean2, lSuite);
  AddTest('TestLargerThanNoBoolean3',@TestLargerThanNode_TestNoBoolean3, lSuite);
  AddTest('TestLargerThanAsString',@TestLargerThanNode_TestAsString, lSuite);

  lSuite:=AddSuite('TestLargerThanEqualNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestLargerThanEqualCreateDateTimeSmaller',@TestLargerThanEqualNode_TestCreateDateTimeSmaller, lSuite);
  AddTest('TestLargerThanEqualCreateDateTimeLarger',@TestLargerThanEqualNode_TestCreateDateTimeLarger, lSuite);
  AddTest('TestLargerThanEqualCreateStringEqual',@TestLargerThanEqualNode_TestCreateStringEqual, lSuite);
  AddTest('TestLargerThanEqualCreateStringSmaller',@TestLargerThanEqualNode_TestCreateStringSmaller, lSuite);
  AddTest('TestLargerThanEqualCreateStringLarger',@TestLargerThanEqualNode_TestCreateStringLarger, lSuite);
  AddTest('TestLargerThanEqualWrongTypes1',@TestLargerThanEqualNode_TestWrongTypes1, lSuite);
  AddTest('TestLargerThanEqualWrongTypes2',@TestLargerThanEqualNode_TestWrongTypes2, lSuite);
  AddTest('TestLargerThanEqualWrongTypes3',@TestLargerThanEqualNode_TestWrongTypes3, lSuite);
  AddTest('TestLargerThanEqualWrongTypes4',@TestLargerThanEqualNode_TestWrongTypes4, lSuite);
  AddTest('TestLargerThanEqualWrongTypes5',@TestLargerThanEqualNode_TestWrongTypes5, lSuite);
  AddTest('TestLargerThanEqualNoBoolean1',@TestLargerThanEqualNode_TestNoBoolean1, lSuite);
  AddTest('TestLargerThanEqualNoBoolean2',@TestLargerThanEqualNode_TestNoBoolean2, lSuite);
  AddTest('TestLargerThanEqualNoBoolean3',@TestLargerThanEqualNode_TestNoBoolean3, lSuite);
  AddTest('TestLargerThanEqualAsString',@TestLargerThanEqualNode_TestAsString, lSuite);

  lSuite:=AddSuite('TTestAddNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestAddNode_CreateInteger', @TTestAddNode_TestCreateInteger, lSuite);
  AddTest('TestAddNode_CreateFloat', @TTestAddNode_TestCreateFloat, lSuite);
  AddTest('TestAddNode_CreateDateTime', @TTestAddNode_TestCreateDateTime, lSuite);
  AddTest('TestAddNode_CreateString', @TTestAddNode_TestCreateString, lSuite);
  AddTest('TestAddNode_CreateBoolean', @TTestAddNode_TestCreateBoolean, lSuite);
  AddTest('TestAddNode_Destroy', @TTestAddNode_TestDestroy, lSuite);
  AddTest('TestAddNode_AsString', @TTestAddNode_TestAsString, lSuite);

  lSuite:=AddSuite('TTestSubtractNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestSubtractNode_CreateInteger', @TTestSubtractNode_TestCreateInteger, lSuite);
  AddTest('TestSubtractNode_CreateFloat', @TTestSubtractNode_TestCreateFloat, lSuite);
  AddTest('TestSubtractNode_CreateDateTime', @TTestSubtractNode_TestCreateDateTime, lSuite);
  AddTest('TestSubtractNode_CreateString', @TTestSubtractNode_TestCreateString, lSuite);
  AddTest('TestSubtractNode_CreateBoolean', @TTestSubtractNode_TestCreateBoolean, lSuite);
  AddTest('TestSubtractNode_Destroy', @TTestSubtractNode_TestDestroy, lSuite);
  AddTest('TestSubtractNode_AsString', @TTestSubtractNode_TestAsString, lSuite);

  lSuite:=AddSuite('TTestMultiplyNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestMultiplyNode_CreateInteger', @TTestMultiplyNode_TestCreateInteger, lSuite);
  AddTest('TestMultiplyNode_CreateFloat', @TTestMultiplyNode_TestCreateFloat, lSuite);
  AddTest('TestMultiplyNode_CreateDateTime', @TTestMultiplyNode_TestCreateDateTime, lSuite);
  AddTest('TestMultiplyNode_CreateString', @TTestMultiplyNode_TestCreateString, lSuite);
  AddTest('TestMultiplyNode_CreateBoolean', @TTestMultiplyNode_TestCreateBoolean, lSuite);
  AddTest('TestMultiplyNode_Destroy', @TTestMultiplyNode_TestDestroy, lSuite);
  AddTest('TestMultiplyNode_AsString', @TTestMultiplyNode_TestAsString, lSuite);

  lSuite:=AddSuite('TTestPowerNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestPowerNode_CreateInteger', @TTestPowerNode_TestCreateInteger, lSuite);
  AddTest('TestPowerNode_CreateFloat', @TTestPowerNode_TestCreateFloat, lSuite);
  AddTest('TestPowerNode_CreateDateTime', @TTestPowerNode_TestCreateDateTime, lSuite);
  AddTest('TestPowerNode_CreateString', @TTestPowerNode_TestCreateString, lSuite);
  AddTest('TestPowerNode_CreateBoolean', @TTestPowerNode_TestCreateBoolean, lSuite);
  AddTest('TestPowerNode_Destroy', @TTestPowerNode_TestDestroy, lSuite);
  AddTest('TestPowerNode_AsString', @TTestPowerNode_TestAsString, lSuite);

  lSuite:=AddSuite('TTestDivideNode', @SuiteSetup, @SuiteTearDown,lParentSuite,True);
  AddTest('TestDivideNode_CreateInteger', @TTestDivideNode_TestCreateInteger, lSuite);
  AddTest('TestDivideNode_CreateFloat', @TTestDivideNode_TestCreateFloat, lSuite);
  AddTest('TestDivideNode_CreateDateTime', @TTestDivideNode_TestCreateDateTime, lSuite);
  AddTest('TestDivideNode_CreateString', @TTestDivideNode_TestCreateString, lSuite);
  AddTest('TestDivideNode_CreateBoolean', @TTestDivideNode_TestCreateBoolean, lSuite);
  AddTest('TestDivideNode_Destroy', @TTestDivideNode_TestDestroy, lSuite);
  AddTest('TestDivideNode_AsString', @TTestDivideNode_TestAsString, lSuite);

end;

end.
