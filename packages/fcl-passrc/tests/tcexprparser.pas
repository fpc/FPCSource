unit tcexprparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit,  testregistry, tcbaseparser, pastree;

type

  { TTestExpressions }

  TTestExpressions= class(TTestParser)
  private
    FLeft: TPAsExpr;
    FRight: TPAsExpr;
    FTheExpr: TPasExpr;
    FVariables : TStringList;
    procedure AssertLeftPrecedence(AInnerLeft: Integer; AInnerOp: TExprOpCode;
      AInnerRight: Integer; AOuterOp: TexprOpCode; AOuterRight: Integer);
    procedure AssertRightPrecedence(AOuterLeft: Integer; AOuterOp: TExprOpCode;
      AInnerLeft: Integer; AInnerOp: TexprOpCode; AInnerRight: Integer);
    procedure DeclareVar(const AVarType: String; const AVarName: String = 'a');
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure SetExpression(Const AExpression : String);
    Procedure ParseExpression;
    Procedure ParseExpression(Const AExpression : String);
    Function AssertBinaryExpr(Const Msg : String; Op : TExprOpCode; Out ALeft,ARight : TPasExpr) : TBinaryExpr;
    Function AssertBinaryExpr(Const Msg : String; AExpr : TPasExpr; Op : TExprOpCode; Out ALeft,ARight : TPasExpr) : TBinaryExpr;
    Function AssertUnaryExpr(Const Msg : String; Op : TExprOpCode; Out AOperand : TPasExpr) : TUnaryExpr;
    Function AssertUnaryExpr(Const Msg : String; AExpr: TPasExpr; Op : TExprOpCode; Out AOperand : TPasExpr) : TUnaryExpr;
    Property TheExpr : TPasExpr read FTheExpr;
    Property Theleft : TPAsExpr Read FLeft;
    Property TheRight : TPAsExpr Read FRight;
  published
    {
      TPasExprKind = (pekRange,
       pekListOfExp, );
    }
    procedure TestPrimitiveInteger;
    procedure TestPrimitiveIntegerHex;
    procedure TestPrimitiveIntegerOctal;
    procedure TestPrimitiveIntegerBinary;
    procedure TestPrimitiveDouble;
    procedure TestPrimitiveString;
    procedure TestPrimitiveIdent;
    procedure TestPrimitiveBooleanFalse;
    procedure TestPrimitiveBooleanTrue;
    procedure TestPrimitiveNil;
    procedure TestPrimitiveSet;
    procedure TestPrimitiveChar;
    procedure TestPrimitiveControlChar;
    procedure TestPrimitiveSetEmpty;
    procedure TestPrimitiveSelf;
    Procedure TestInherited;
    Procedure TestInheritedFunction;
    Procedure TestUnaryMinus;
    Procedure TestUnaryMinusWhiteSpace;
    Procedure TestUnaryAddress;
    Procedure TestUnaryNot;
    Procedure TestUnaryDeref;
    Procedure TestBinaryAdd;
    Procedure TestBinarySubtract;
    Procedure TestBinaryMultiply;
    Procedure TestBinaryDivision;
    Procedure TestBinaryPower;
    Procedure TestBinaryMod;
    Procedure TestBinaryDiv;
    procedure TestBinaryShl;
    procedure TestBinaryShr;
    Procedure TestBinarySymmetricalDifference;
    Procedure TestBinaryAnd;
    Procedure TestBinaryOr;
    Procedure TestBinaryXOr;
    Procedure TestBinaryIn;
    Procedure TestBinaryIs;
    Procedure TestBinaryAs;
    Procedure TestBinaryEquals;
    Procedure TestBinaryDiffers;
    Procedure TestBinaryLessThan;
    Procedure TestBinaryLessThanEqual;
    Procedure TestBinaryLargerThan;
    Procedure TestBinaryLargerThanEqual;
    procedure TestBinaryFullIdent;
    Procedure TestArrayElement;
    Procedure TestArrayElement2Dims;
    Procedure TestFunctionCall;
    Procedure TestFunctionCall2args;
    Procedure TestFunctionCallNoArgs;
    Procedure TestRange;
    Procedure TestBracketsTotal;
    Procedure TestBracketsLeft;
    Procedure TestBracketsRight;
    Procedure TestPrecedenceLeftToRight;
    Procedure TestPrecedenceLeftToRightMinus;
    Procedure TestPrecedenceLeftToRightMultiply;
    Procedure TestPrecedenceLeftToRightDivision;
    Procedure TestPrecedenceLeftToRightPlusMinus;
    Procedure TestPrecedenceLeftToRightMinusPlus;
    Procedure TestPrecedenceLeftToRightMultiplyDivision;
    Procedure TestPrecedenceLeftToRightDivisionMultiply;
    Procedure TestPrecedencePlusMultiply;
    Procedure TestPrecedencePlusDivide;
    Procedure TestPrecedenceMinusMultiply;
    Procedure TestPrecedenceMinusDivide;
    Procedure TestPrecedencePlusOr;
    Procedure TestPrecedenceAndOr;
    Procedure TestPrecedenceAndNot;
    Procedure TestPrecedencePlusAnd;
    Procedure TestPrecedenceMinusOr;
    Procedure TestPrecedenceMinusAnd;
    Procedure TestPrecedenceMultiplyOr;
    Procedure TestPrecedenceMultiplyAnd;
    Procedure TestPrecedencePlusDiv;
    Procedure TestPrecedencePlusMod;
    Procedure TestPrecedenceMultiplyDiv;
    Procedure TestPrecedenceDivMultiply;
    Procedure TestTypeCast;
    Procedure TestCreate;
  end;

implementation

procedure TTestExpressions.DeclareVar(const AVarType: String;
const AVarName: String = 'a');
begin
  FVariables.Add(AVarName+' : '+AVarType+';');
end;

procedure TTestExpressions.TestPrimitiveInteger;
begin
  ParseExpression('1');
  AssertExpression('Simple integer',theExpr,pekNumber,'1');
end;

procedure TTestExpressions.TestPrimitiveIntegerHex;
begin
  ParseExpression('$FF');
  AssertExpression('Simple integer',theExpr,pekNumber,'$FF');
end;

procedure TTestExpressions.TestPrimitiveIntegerOctal;
begin
  ParseExpression('&777');
  AssertExpression('Simple integer',theExpr,pekNumber,'&777');
end;

procedure TTestExpressions.TestPrimitiveIntegerBinary;
begin
  ParseExpression('%10101010');
  AssertExpression('Simple integer',theExpr,pekNumber,'%10101010');
end;

procedure TTestExpressions.TestPrimitiveDouble;
begin
  ParseExpression('1.2');
  AssertExpression('Simple double',theExpr,pekNumber,'1.2');
end;

procedure TTestExpressions.TestPrimitiveString;
begin
  DeclareVar('string');
  ParseExpression('''123''');
  AssertExpression('Simple string',theExpr,pekString,'''123''');
end;

procedure TTestExpressions.TestPrimitiveIdent;
begin
  DeclareVar('integer','a');
  DeclareVar('integer','b');
  ParseExpression('b');
  AssertExpression('Simple identifier',theExpr,pekIdent,'b');
end;

procedure TTestExpressions.TestBinaryFullIdent;
begin
  DeclareVar('integer','a');
  DeclareVar('record x,y : integer; end','b');
  ParseExpression('b.x');
  AssertBinaryExpr('sub identifier',eopSubIdent,Fleft,FRight);
  AssertExpression('Simple identifier',Theleft,pekIdent,'b');
  AssertExpression('Simple identifier',Theright,pekIdent,'x');
end;

procedure TTestExpressions.TestArrayElement;

Var
  P : TParamsExpr;

begin
  DeclareVar('integer','a');
  DeclareVar('array[1..2] of integer','b');
  ParseExpression('b[1]');
  P:=TParamsExpr(AssertExpression('Simple identifier',theExpr,pekArrayParams,TParamsExpr));
  AssertExpression('Name of array',P.Value,pekIdent,'b');
  AssertEquals('One dimension',1,Length(p.params));
  AssertExpression('Simple identifier',p.params[0],pekNumber,'1');
end;

procedure TTestExpressions.TestArrayElement2Dims;
Var
  P : TParamsExpr;

begin
  DeclareVar('integer','a');
  DeclareVar('array[1..2,1..2] of integer','b');
  ParseExpression('b[1,2]');
  P:=TParamsExpr(AssertExpression('Simple identifier',theExpr,pekArrayParams,TParamsExpr));
  AssertExpression('Name of array',P.Value,pekIdent,'b');
  AssertEquals('Two dimensions',2,Length(p.params));
  AssertExpression('Simple identifier',p.params[0],pekNumber,'1');
  AssertExpression('Simple identifier',p.params[1],pekNumber,'2');
end;

procedure TTestExpressions.TestFunctionCall;
Var
  P : TParamsExpr;

begin
  DeclareVar('integer','a');
  ParseExpression('Random(10)');
  P:=TParamsExpr(AssertExpression('Simple identifier',theExpr,pekFuncParams,TParamsExpr));
  AssertExpression('Name of function',P.Value,pekIdent,'Random');
  AssertEquals('1 argument',1,Length(p.params));
  AssertExpression('Simple identifier',p.params[0],pekNumber,'10');
end;

procedure TTestExpressions.TestFunctionCall2args;
Var
  P : TParamsExpr;

begin
  DeclareVar('integer','a');
  ParseExpression('Random(10,12)');
  P:=TParamsExpr(AssertExpression('Simple identifier',theExpr,pekFuncParams,TParamsExpr));
  AssertExpression('Name of function',P.Value,pekIdent,'Random');
  AssertEquals('2 argument',2,Length(p.params));
  AssertExpression('Simple identifier 1',p.params[0],pekNumber,'10');
  AssertExpression('Simple identifier 2',p.params[1],pekNumber,'12');
end;

procedure TTestExpressions.TestFunctionCallNoArgs;

Var
  P : TParamsExpr;

begin
  DeclareVar('integer','a');
  ParseExpression('Random()');
  P:=TParamsExpr(AssertExpression('Simple identifier',theExpr,pekFuncParams,TParamsExpr));
  AssertExpression('Name of function',P.Value,pekIdent,'Random');
  AssertEquals('0 arguments',0,Length(p.params));
end;

procedure TTestExpressions.TestRange;

Var
  B : TBinaryExpr;

begin
  DeclareVar('boolean','a');
  DeclareVar('byte','b');
  ParseExpression('b in 0..10');
  AssertBinaryExpr('Simple binary In',eopIn,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekIdent,'b');
  B:=TBinaryExpr(AssertExpression('Right is range',TheRight,pekRange,TBinaryExpr));
  AssertExpression('Left is 0',B.Left,pekNumber,'0');
  AssertExpression('Right is 10',B.Right,pekNumber,'10');
end;

procedure TTestExpressions.TestBracketsTotal;
begin
  DeclareVar('integer','a');
  ParseExpression('(3+4)');
  AssertBinaryExpr('simple binary add',eopAdd,FLeft,FRight);
  AssertExpression('Inner Left is 3',TheLeft,pekNumber,'3');
  AssertExpression('Inner Right is 4',TheRight,pekNumber,'4');
end;

procedure TTestExpressions.TestBracketsLeft;
begin
  DeclareVar('integer','a');
  ParseExpression('2*(3+4)');
  AssertRightPrecedence(2,eopMultiply,3,eopAdd,4);
end;

procedure TTestExpressions.TestBracketsRight;
begin
  DeclareVar('integer','a');
  ParseExpression('(2*3)+4');
  AssertLeftPrecedence(2,eopMultiply,3,eopAdd,4);
end;

procedure TTestExpressions.TestPrecedenceLeftToRight;
begin
  ParseExpression('1+2+3');
  AssertLeftPrecedence(1,eopAdd,2,eopAdd,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightMinus;
begin
  ParseExpression('1-2-3');
  AssertLeftPrecedence(1,eopSubtract,2,eopSubtract,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightMultiply;
begin
  ParseExpression('1*2*3');
  AssertLeftPrecedence(1,eopMultiply,2,eopMultiply,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightDivision;
begin
  ParseExpression('1/2/3');
  AssertLeftPrecedence(1,eopDivide,2,eopDivide,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightPlusMinus;
begin
  ParseExpression('1+2-3');
  AssertLeftPrecedence(1,eopAdd,2,eopSubtract,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightMinusPlus;
begin
  ParseExpression('1-2+3');
  AssertLeftPrecedence(1,eopSubtract,2,eopAdd,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightMultiplyDivision;
begin
  ParseExpression('1*2/3');
  AssertLeftPrecedence(1,eopMultiply,2,eopDivide,3);
end;

procedure TTestExpressions.TestPrecedenceLeftToRightDivisionMultiply;
begin
  ParseExpression('1/2*3');
  AssertLeftPrecedence(1,eopDivide,2,eopMultiply,3);
end;

procedure TTestExpressions.TestPrecedencePlusMultiply;
begin
  ParseExpression('1+2*3');
  AssertRightPrecedence(1,eopAdd,2,eopMultiply,3);
end;

procedure TTestExpressions.TestPrecedencePlusDivide;
begin
  ParseExpression('1+2/3');
  AssertRightPrecedence(1,eopAdd,2,eopDivide,3);
end;

procedure TTestExpressions.TestPrecedenceMinusMultiply;
begin
  ParseExpression('1-2*3');
  AssertRightPrecedence(1,eopsubtract,2,eopMultiply,3);
end;

procedure TTestExpressions.TestPrecedenceMinusDivide;
begin
  ParseExpression('1-2/3');
  AssertRightPrecedence(1,eopsubtract,2,eopDivide,3);
end;

procedure TTestExpressions.TestPrecedencePlusOr;
begin
  ParseExpression('1 or 2 + 3');
  AssertLeftPrecedence(1,eopor,2,eopAdd,3);
end;

procedure TTestExpressions.TestPrecedenceAndOr;
begin
  ParseExpression('1 or 2 and 3');
  AssertRightPrecedence(1,eopor,2,eopAnd,3);
end;

procedure TTestExpressions.TestPrecedenceAndNot;
begin
  ParseExpression('Not 1 and 3');
  AssertBinaryExpr('Simple binary and',eopAnd,FLeft,FRight);
  AssertExpression('Outer right is 3',TheRight,pekNumber,'3');
  AssertUnaryExpr('Left is Unary not ',TheLeft,eopNot,FRight);
  AssertExpression('Inner Right is 1',TheRight,pekNumber,'1');
end;

procedure TTestExpressions.TestPrecedencePlusAnd;
begin
  ParseExpression('1 + 2 and 3');
  AssertRightPrecedence(1,eopAdd,2,eopAnd,3);
end;

procedure TTestExpressions.TestPrecedenceMinusOr;
begin
  ParseExpression('1 or 2 - 3');
  AssertLeftPrecedence(1,eopOr,2,eopSubtract,3);
end;

procedure TTestExpressions.TestPrecedenceMinusAnd;
begin
  ParseExpression('1 - 2 and 3');
  AssertRightPrecedence(1,eopSubtract,2,eopand,3);
end;

procedure TTestExpressions.TestPrecedenceMultiplyOr;
begin
  ParseExpression('1 or 2 * 3');
  AssertRightPrecedence(1,eopOr,2,eopMultiply,3);
end;

procedure TTestExpressions.TestPrecedenceMultiplyAnd;
begin
  ParseExpression('1 * 2 and 3');
  AssertLeftPrecedence(1,eopMultiply,2,eopAnd,3);
end;

procedure TTestExpressions.TestPrecedencePlusDiv;
begin
  ParseExpression('1+2 div 3');
  AssertRightPrecedence(1,eopAdd,2,eopDiv,3);
end;

procedure TTestExpressions.TestPrecedencePlusMod;
begin
  ParseExpression('1+2 mod 3');
  AssertRightPrecedence(1,eopAdd,2,eopMod,3);
end;

procedure TTestExpressions.AssertLeftPrecedence(AInnerLeft : Integer; AInnerOp : TExprOpCode; AInnerRight : Integer; AOuterOp : TexprOpCode; AOuterRight: Integer);

begin
  AssertBinaryExpr('Outer expression',AOuterOp,FLeft,FRight);
  AssertExpression('Outer right constant',TheRight,pekNumber,intToStr(AOuterRight));
  AssertBinaryExpr('Inner (left) expression',TheLeft,AInnerOp,FLeft,FRight);
  AssertExpression('Inner Left constant',TheLeft,pekNumber,IntToStr(AInnerLeft));
  AssertExpression('Inner Right constant',TheRight,pekNumber,IntToStr(AInnerRight));
end;


procedure TTestExpressions.AssertRightPrecedence(AOuterLeft : Integer; AOuterOp : TExprOpCode; AInnerLeft : Integer; AInnerOp : TexprOpCode; AInnerRight: Integer);

begin
  AssertBinaryExpr('Outer expression',AOuterOp,FLeft,FRight);
  AssertExpression('Outer left constant',TheLeft,pekNumber,intToStr(AOuterLeft));
  AssertBinaryExpr('Inner (right) expression',TheRight,AInnerOp,FLeft,FRight);
  AssertExpression('Inner Left constant',TheLeft,pekNumber,IntToStr(AInnerLeft));
  AssertExpression('Inner Right constant',TheRight,pekNumber,IntToStr(AInnerRight));
end;

procedure TTestExpressions.TestPrecedenceMultiplyDiv;
begin
  ParseExpression('1 * 2 div 3');
  AssertLeftPrecedence(1,eopMultiply,2,eopDiv,3);
end;

procedure TTestExpressions.TestPrecedenceDivMultiply;
begin
  ParseExpression('1 div 2 * 3');
  AssertLeftPrecedence(1,eopDiv,2,eopMultiply,3);
end;

procedure TTestExpressions.TestTypeCast;
begin
  DeclareVar('TSDOBaseDataObjectClass');
  ParseExpression('TSDOBaseDataObjectClass(Self.ClassType).Create');
end;

procedure TTestExpressions.TestCreate;
begin
  DeclareVar('ESDOSerializationException');
  ParseExpression('ESDOSerializationException.CreateFmt(SERR_InvalidDataTypeInContext,[IntToStr(Ord(AOwner^.DataType))])');
end;


procedure TTestExpressions.TestUnaryMinus;
begin
  DeclareVar('integer','a');
  DeclareVar('integer','b');
  ParseExpression('-b');
  AssertUnaryExpr('Simple minus unary',eopSubtract,FLeft);
  AssertExpression('Simple identifier',theLeft,pekIdent,'b');
end;

procedure TTestExpressions.TestUnaryMinusWhiteSpace;
begin
  DeclareVar('integer','a');
  DeclareVar('integer','b');
  ParseExpression('- b');
  AssertUnaryExpr('Simple minus unary',eopSubtract,FLeft);
  AssertExpression('Simple identifier',theLeft,pekIdent,'b');
end;

procedure TTestExpressions.TestUnaryAddress;
begin
  DeclareVar('integer','a');
  DeclareVar('integer','b');
  ParseExpression('@b');
  AssertUnaryExpr('Simple address unary',eopAddress,FLeft);
  AssertExpression('Simple identifier',theLeft,pekIdent,'b');
end;

procedure TTestExpressions.TestUnaryNot;
begin
  DeclareVar('boolean','a');
  DeclareVar('boolean','b');
  ParseExpression('not b');
  AssertUnaryExpr('Simple address unary',eopNot,FLeft);
  AssertExpression('Simple identifier',theLeft,pekIdent,'b');
end;

procedure TTestExpressions.TestUnaryDeref;
begin
  DeclareVar('integer','a');
  DeclareVar('pinteger','b');
  ParseExpression('b^');
  AssertUnaryExpr('Simple address unary',eopDeref,FLeft);
  AssertExpression('Simple identifier',theLeft,pekIdent,'b');
end;

procedure TTestExpressions.TestBinaryAdd;
begin
  ParseExpression('1+2');
  AssertBinaryExpr('Simple binary add',eopAdd,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinarySubtract;
begin
  ParseExpression('1-2');
  AssertBinaryExpr('Simple binary subtract',eopSubtract,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryMultiply;
begin
  ParseExpression('1*2');
  AssertBinaryExpr('Simple binary multiply',eopMultiply,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryDivision;
begin
  DeclareVar('double');
  ParseExpression('1/2');
  AssertBinaryExpr('Simple binary division',eopDivide,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryPower;
begin
  DeclareVar('double');
  ParseExpression('1**2');
  AssertBinaryExpr('Simple binary power',eopPower,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryMod;
begin
  ParseExpression('1 mod 2');
  AssertBinaryExpr('Simple binary mod',eopMod,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryDiv;
begin
  ParseExpression('1 div 2');
  AssertBinaryExpr('Simple binary div',eopDiv,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryShl;
begin
  ParseExpression('1 shl 2');
  AssertBinaryExpr('Simple binary shl',eopShl,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinaryShr;
begin
  ParseExpression('1 shr 2');
  AssertBinaryExpr('Simple binary shr',eopShr,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is 2',TheRight,pekNumber,'2');
end;

procedure TTestExpressions.TestBinarySymmetricalDifference;
begin
  DeclareVar('Set of Byte','a');
  DeclareVar('Set of Byte','b');
  DeclareVar('Set of Byte','c');
  ParseExpression('b >< c');
  AssertBinaryExpr('Simple binary smmetrical difference',eopSymmetricalDifference,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryAnd;
begin
  DeclareVar('boolean','a');
  DeclareVar('boolean','b');
  DeclareVar('boolean','b');
  ParseExpression('b and c');
  AssertBinaryExpr('Simple binary and',eopAnd,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekIdent,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryOr;
begin
  DeclareVar('boolean','a');
  DeclareVar('boolean','b');
  DeclareVar('boolean','b');
  ParseExpression('b or c');
  AssertBinaryExpr('Simple binary or',eopOr,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekIdent,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryXOr;
begin
  DeclareVar('boolean','a');
  DeclareVar('boolean','b');
  DeclareVar('boolean','b');
  ParseExpression('b xor c');
  AssertBinaryExpr('Simple binary xor',eopxOr,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekIdent,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryIn;
begin
  DeclareVar('boolean','a');
  ParseExpression('1 in [1,2,3]');
  AssertBinaryExpr('Simple binary In',eopIn,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekNumber,'1');
  AssertExpression('Right is array set',TheRight,pekSet,TParamsExpr);
end;

procedure TTestExpressions.TestBinaryIs;
begin
  DeclareVar('boolean','a');
  DeclareVar('TObject','b');
  ParseExpression('b is TObject');
  AssertBinaryExpr('Simple binary Is',eopIs,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekident,'b');
  AssertExpression('Right is TObject',TheRight,pekIdent,'TObject');
end;

procedure TTestExpressions.TestBinaryAs;
begin
  DeclareVar('TObject','a');
  DeclareVar('TObject','b');
  ParseExpression('b as TObject');
  AssertBinaryExpr('Simple binary As',eopAs,FLeft,FRight);
  AssertExpression('Left is 1',TheLeft,pekident,'b');
  AssertExpression('Right is TObject',TheRight,pekIdent,'TObject');
end;

procedure TTestExpressions.TestBinaryEquals;
begin
  DeclareVar('boolean','a');
  DeclareVar('integer','b');
  DeclareVar('integer','c');
  ParseExpression('b=c');
  AssertBinaryExpr('Simple binary equals',eopEqual,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryDiffers;
begin
  DeclareVar('boolean','a');
  DeclareVar('integer','b');
  DeclareVar('integer','c');
  ParseExpression('b<>c');
  AssertBinaryExpr('Simple binary differs',eopNotEqual,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryLessThan;
begin
  DeclareVar('boolean','a');
  DeclareVar('integer','b');
  DeclareVar('integer','c');
  ParseExpression('b<c');
  AssertBinaryExpr('Simple binary less than',eopLessThan,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryLessThanEqual;
begin
  DeclareVar('boolean','a');
  DeclareVar('integer','b');
  DeclareVar('integer','c');
  ParseExpression('b<=c');
  AssertBinaryExpr('Simple binary less than or equal',eopLessThanEqual,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryLargerThan;
begin
  DeclareVar('boolean','a');
  DeclareVar('integer','b');
  DeclareVar('integer','c');
  ParseExpression('b>c');
  AssertBinaryExpr('Simple binary larger than ',eopGreaterThan,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestBinaryLargerThanEqual;
begin
  DeclareVar('boolean','a');
  DeclareVar('integer','b');
  DeclareVar('integer','c');
  ParseExpression('b>=c');
  AssertBinaryExpr('Simple binary larger than or equal',eopGreaterThanEqual,FLeft,FRight);
  AssertExpression('Left is b',TheLeft,pekident,'b');
  AssertExpression('Right is c',TheRight,pekIdent,'c');
end;

procedure TTestExpressions.TestPrimitiveBooleanFalse;
begin
  DeclareVar('boolean','a');
  ParseExpression('False');
  AssertExpression('Simple boolean',theExpr,pekBoolConst,TBoolConstExpr);
  AssertEquals('Boolean false',False,TBoolConstExpr(TheExpr).Value);
end;

procedure TTestExpressions.TestPrimitiveBooleanTrue;
begin
  DeclareVar('boolean','a');
  ParseExpression('True');
  AssertExpression('Simple boolean',theExpr,pekBoolConst,TBoolConstExpr);
  AssertEquals('Boolean true',True,TBoolConstExpr(TheExpr).Value);
end;

procedure TTestExpressions.TestPrimitiveNil;
begin
  DeclareVar('pointer','a');
  ParseExpression('Nil');
  AssertExpression('Nil expr',theExpr,pekNil,TNilExpr);
end;

procedure TTestExpressions.TestPrimitiveSet;

Var
  P : TParamsExpr;
begin
  DeclareVar('set of byte','a');
  ParseExpression('[1,2,3]');
  P:=TParamsExpr(AssertExpression('Set expr',theExpr,pekSet,TParamsExpr));
  AssertEquals('Element count',3,Length(P.Params));
  AssertExpression('Element 1 in set',P.Params[0],pekNumber,'1');
  AssertExpression('Element 2 in set',P.Params[1],pekNumber,'2');
  AssertExpression('Element 3 in set',P.Params[2],pekNumber,'3');
end;

procedure TTestExpressions.TestPrimitiveChar;
begin
  DeclareVar('char');
  ParseExpression('#32');
  AssertExpression('Simple string',theExpr,pekString,'#32');
end;

procedure TTestExpressions.TestPrimitiveControlChar;
begin
  DeclareVar('char');
  ParseExpression('^M');
  AssertExpression('Simple string',theExpr,pekString,'^M');
end;

procedure TTestExpressions.TestPrimitiveSetEmpty;

Var
  P : TParamsExpr;
begin
  DeclareVar('set of byte','a');
  ParseExpression('[]');
  P:=TParamsExpr(AssertExpression('Set expr',theExpr,pekSet,TParamsExpr));
  AssertEquals('Element count',0,Length(P.Params));
end;

procedure TTestExpressions.TestPrimitiveSelf;

begin
  DeclareVar('pointer','a');
  ParseExpression('Self');
  AssertExpression('Inherited expr',theExpr,pekSelf,TSelfExpr);
end;

procedure TTestExpressions.TestInherited;

begin
  DeclareVar('pointer','a');
  ParseExpression('inherited');
  AssertExpression('Inherited expr',theExpr,pekInherited,TInheritedExpr);
end;

procedure TTestExpressions.TestInheritedFunction;

begin
  DeclareVar('pointer','a');
  ParseExpression('inherited myfunction');
  AssertBinaryExpr('Inherited expr',eopNone,Fleft,FRight);
  AssertExpression('Inherited expr',theleft,pekInherited,TInheritedExpr);
  AssertExpression('Inherited expr',theright,pekIdent,'myfunction');
end;

procedure TTestExpressions.SetUp;
begin
  Inherited;
  FVariables:=TStringList.Create;
end;

procedure TTestExpressions.TearDown;

begin
  FreeAndNil(FVariables);
  Inherited;
end;

procedure TTestExpressions.SetExpression(const AExpression: String);

Var
  I : Integer;

begin
  StartProgram('afile');
  if FVariables.Count=0 then
    DeclareVar('integer');
  Add('Var');
  For I:=0 to FVariables.Count-1 do
    Add('  '+Fvariables[I]);
  Add('begin');
  Add('  a:='+AExpression+';');
end;

procedure TTestExpressions.ParseExpression;
begin
  ParseModule;
  AssertEquals('Have program',TPasProgram,Module.ClassType);
  AssertNotNull('Have program section',PasProgram.ProgramSection);
  AssertNotNull('Have initialization section',PasProgram.InitializationSection);
  AssertEquals('Have initialization statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertNotNull('Have initialization statement',PasProgram.InitializationSection.Elements[0]);
  AssertEquals('Assignment statement',TPasImplAssign,TObject(PasProgram.InitializationSection.Elements[0]).ClassType);
  FTheExpr:=TPasImplAssign(PasProgram.InitializationSection.Elements[0]).right;
  AssertNotNull('Have assignment expression',FTheExpr);
end;

procedure TTestExpressions.ParseExpression(const AExpression: String);
begin
  SetExpression(AExpression);
  ParseExpression;
end;

function TTestExpressions.AssertBinaryExpr(const Msg: String; Op: TExprOpCode;
  out ALeft, ARight: TPasExpr): TBinaryExpr;
begin
  Result:=AssertBinaryExpr(Msg,TheExpr,Op,ALeft,ARight);
end;

function TTestExpressions.AssertBinaryExpr(const Msg: String; AExpr: TPasExpr;
  Op: TExprOpCode; out ALeft, ARight: TPasExpr): TBinaryExpr;
begin
  AssertExpression(Msg+' is binary',AExpr,pekBinary,TBinaryExpr);
  Result:=AExpr as TBinaryExpr;
  AssertEquals(Msg+' opcode OK',Op,Result.OpCode);
  ALeft:=Result.Left;
  ARight:=Result.Right;
  AssertNotNull('Have left',ALeft);
  AssertNotNull('Have right',ARight);
end;

function TTestExpressions.AssertUnaryExpr(const Msg: String; Op: TExprOpCode;
  out AOperand : TPasExpr): TUnaryExpr;
begin
  Result:=AssertUnaryExpr(Msg,TheExpr,OP,AOperand);
end;

function TTestExpressions.AssertUnaryExpr(const Msg: String; AExpr: TPasExpr;
  Op: TExprOpCode; out AOperand: TPasExpr): TUnaryExpr;
begin
  AssertExpression(Msg+' is unary',AExpr,pekUnary,TUnaryExpr);
  Result:=AExpr as TUnaryExpr;
  AssertEquals(Msg+' opcode OK',Op,Result.OpCode);
  AOperand:=Result.Operand;
  AssertNotNull('Have left',AOperand);
end;

initialization

  RegisterTest(TTestExpressions);
end.

