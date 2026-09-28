{
  h2pas test suite: #define constants and macros.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcMacros;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestConstMacros }

  TTestConstMacros = class(TH2PasTestCase)
  protected
    // Converts "#define aName aValue" and checks for the constant "aName = aPascal;".
    procedure CheckConst(const aName, aValue, aPascal: string);
  published
    procedure TestEmptyDefine;
    procedure TestDecimal;
    procedure TestHexadecimal;
    procedure TestOctal;
    procedure TestZero;
    procedure TestZeroSuffix;
    procedure TestIntegerDivision;
    procedure TestFloatDivision;
    procedure TestIntegerSuffix;
    procedure TestNegative;
    procedure TestFloat;
    procedure TestExponent;
    procedure TestString;
    procedure TestCharLiteral;
    procedure TestAlias;
    procedure TestOr;
    procedure TestAnd;
    procedure TestShl;
    procedure TestShr;
    procedure TestAdd;
    procedure TestSubtract;
    procedure TestMultiply;
    procedure TestBitwiseNot;
    procedure TestLogicalNot;
    procedure TestConstBlock;
    procedure TestLineCommentAfterDefine;
    procedure TestBlockCommentAfterDefine;
  end;

  { TTestFunctionMacros }

  TTestFunctionMacros = class(TH2PasTestCase)
  published
    procedure TestCastExpression;
    procedure TestCallExpression;
    procedure TestParameterMacroInterface;
    procedure TestParameterMacroBody;
    procedure TestTernary;
    procedure TestDivision;
    procedure TestDeref;
    procedure TestDot;
    procedure TestIndex;
    procedure TestAddress;
    procedure TestEqual;
    procedure TestNotEqual;
    procedure TestLess;
    procedure TestGreaterEqual;
    procedure TestPointerCast;
    procedure TestNoParameters;
    procedure TestIndentationAfterMacros;
    procedure TestCompactIndentationAfterMacros;
  end;

implementation


procedure TTestConstMacros.CheckConst(const aName, aValue, aPascal: string);

begin
  Convert(['#define '+aName+' '+aValue]);
  AssertConverted;
  AssertInterface('#define '+aName+' '+aValue,['const',aName+' = '+aPascal+';']);
end;


procedure TTestConstMacros.TestEmptyDefine;

begin
  Convert(['#define EMPTY']);
  AssertConverted;
  AssertInterface('define without value becomes a conditional define',['{$define EMPTY}']);
end;


procedure TTestConstMacros.TestDecimal;

begin
  CheckConst('DEC','42','42');
end;


procedure TTestConstMacros.TestHexadecimal;

begin
  CheckConst('HEX','0x1F','$1F');
end;


procedure TTestConstMacros.TestOctal;

begin
  CheckConst('OCT','0755','&755');
end;


procedure TTestConstMacros.TestZero;

begin
  CheckConst('ZERO','0','0');
end;


procedure TTestConstMacros.TestZeroSuffix;

begin
  CheckConst('ZEROL','0L','0');
end;


procedure TTestConstMacros.TestIntegerDivision;

begin
  CheckConst('DIV_E','(6 / 2)','6 div 2');
end;


procedure TTestConstMacros.TestFloatDivision;

begin
  CheckConst('FDIV_E','(6.0 / 2)','6.0/2');
end;


procedure TTestConstMacros.TestIntegerSuffix;

begin
  CheckConst('SUF','0x1FUL','$1F');
end;


procedure TTestConstMacros.TestNegative;

begin
  CheckConst('NEG','-1','-(1)');
end;


procedure TTestConstMacros.TestFloat;

begin
  CheckConst('FLT','3.14','3.14');
end;


procedure TTestConstMacros.TestExponent;

begin
  CheckConst('EXPF','1.5e10','1.5e10');
end;


procedure TTestConstMacros.TestString;

begin
  CheckConst('STRC','"abc"','''abc''');
end;


procedure TTestConstMacros.TestCharLiteral;

begin
  CheckConst('CHC','''x''','''x''');
end;


procedure TTestConstMacros.TestAlias;

begin
  CheckConst('ALIAS','OTHER','OTHER');
end;


procedure TTestConstMacros.TestOr;

begin
  CheckConst('OR_E','(1 | 2)','1 or 2');
end;


procedure TTestConstMacros.TestAnd;

begin
  CheckConst('AND_E','(3 & 1)','3 and 1');
end;


procedure TTestConstMacros.TestShl;

begin
  CheckConst('SHL_E','(1 << 4)','1 shl 4');
end;


procedure TTestConstMacros.TestShr;

begin
  CheckConst('SHR_E','(256 >> 2)','256 shr 2');
end;


procedure TTestConstMacros.TestAdd;

begin
  CheckConst('ADD_E','(DEC + 1)','DEC+1');
end;


procedure TTestConstMacros.TestSubtract;

begin
  CheckConst('SUB_E','(DEC - 1)','DEC-1');
end;


procedure TTestConstMacros.TestMultiply;

begin
  CheckConst('MUL_E','(2 * 3)','2*3');
end;


procedure TTestConstMacros.TestBitwiseNot;

begin
  CheckConst('NOT_E','(~1)','not (1)');
end;


procedure TTestConstMacros.TestLogicalNot;

begin
  CheckConst('NOT_E','(!1)','not (1)');
end;


procedure TTestConstMacros.TestConstBlock;

begin
  Convert(['#define A 1','#define B 2']);
  AssertConverted;
  AssertInterface('consecutive constants share one const block',['const','A = 1;','B = 2;']);
end;


procedure TTestConstMacros.TestLineCommentAfterDefine;

begin
  Convert(['#define CMT 7 // the comment']);
  AssertConverted;
  AssertInterface('line comment of a define follows the constant',['CMT = 7; { the comment }']);
end;


procedure TTestConstMacros.TestBlockCommentAfterDefine;

begin
  Convert(['#define A 8 /* block */']);
  AssertConverted;
  AssertInterface('block comment of a define is kept',['{ block }','const','A = 8;']);
end;


procedure TTestFunctionMacros.TestCastExpression;

begin
  Convert(['#define CAST_E ((int)5)']);
  AssertConverted;
  AssertInterface('typecast value gives a function with the cast type',['{ was #define dname def_expr }','function CAST_E : longint;']);
  AssertImplementation('typecast body',['function CAST_E : longint;','begin','CAST_E:=longint(5);','end;']);
end;


procedure TTestFunctionMacros.TestCallExpression;

begin
  Convert(['#define CALLM foo(1, 2)']);
  AssertConverted;
  AssertInterface('call value gives a function',['function CALLM : longint; { return type might be wrong }']);
  AssertImplementation('call body',['begin','CALLM:=foo(1,2);','end;']);
end;


procedure TTestFunctionMacros.TestParameterMacroInterface;

begin
  Convert(['#define PAR2(a, b) ((a) * (b))']);
  AssertConverted;
  AssertInterface('macro with parameters becomes a function',
    ['{ was #define dname(params) para_def_expr }','{ argument types are unknown }',
     '{ return type might be wrong }','function PAR2(a,b : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestParameterMacroBody;

begin
  Convert(['#define PAR2(a, b) ((a) * (b))']);
  AssertConverted;
  AssertImplementation('macro with parameters body',['function PAR2(a,b : longint) : longint;','begin','PAR2:=a*b;','end;']);
end;


procedure TTestFunctionMacros.TestTernary;

begin
  Convert(['#define TERN(a) ((a) ? 1 : 2)']);
  AssertConverted;
  AssertImplementation('ternary operator becomes an if statement on a local',
    ['var','if_local1 : longint;','(* result types are not known *)','begin',
     'if a then','if_local1:=1','else','if_local1:=2;','TERN:=if_local1;','end;']);
end;


procedure TTestFunctionMacros.TestDivision;

begin
  Convert(['#define HALF(a) ((a) / 2)','#define FHALF(a) ((a) / 2.0)','#define CHALF(a) ((double)(a) / 2)']);
  AssertConverted;
  AssertImplementation('integer division in a macro',['HALF:=a div 2;']);
  AssertImplementation('float literal division in a macro',['FHALF:=a/2.0;']);
  AssertImplementation('division of a float cast in a macro',['CHALF:=(double(a))/2;']);
end;


procedure TTestFunctionMacros.TestDeref;

begin
  Convert(['#define FIELD(p) ((p)->field)']);
  AssertConverted;
  AssertImplementation('-> becomes ^.',['FIELD:=p^.field;']);
end;


procedure TTestFunctionMacros.TestDot;

begin
  Convert(['#define DOT(p) ((p).field)']);
  AssertConverted;
  AssertImplementation('member access',['DOT:=p.field;']);
end;


procedure TTestFunctionMacros.TestIndex;

begin
  Convert(['#define IDX(a) (a[3])']);
  AssertConverted;
  AssertImplementation('array index',['IDX:=a[3];']);
end;


procedure TTestFunctionMacros.TestAddress;

begin
  Convert(['#define ADDR(a) (&a)']);
  AssertConverted;
  AssertImplementation('address operator',['ADDR:=@(a);']);
end;


procedure TTestFunctionMacros.TestEqual;

begin
  Convert(['#define CMP(a,b) ((a) == (b))']);
  AssertConverted;
  AssertImplementation('== becomes =',['CMP:=a=b;']);
end;


procedure TTestFunctionMacros.TestNotEqual;

begin
  Convert(['#define NE(a,b) ((a) != (b))']);
  AssertConverted;
  AssertImplementation('!= becomes <>',['NE:=a<>b;']);
end;


procedure TTestFunctionMacros.TestLess;

begin
  Convert(['#define LT_E(a,b) ((a) < (b))']);
  AssertConverted;
  AssertImplementation('less than',['LT_E:=a<b;']);
end;


procedure TTestFunctionMacros.TestGreaterEqual;

begin
  Convert(['#define GE_E(a,b) ((a) >= (b))']);
  AssertConverted;
  AssertImplementation('greater or equal',['GE_E:=a>=b;']);
end;


procedure TTestFunctionMacros.TestPointerCast;

begin
  Convert(['#define PCAST(p) ((char *)(p))']);
  AssertConverted;
  AssertInterface('pointer cast gives the result type',['function PCAST(p : longint) : pansichar;']);
  AssertImplementation('pointer cast body',['PCAST:=pansichar(p);']);
end;


procedure TTestFunctionMacros.TestNoParameters;

begin
  Convert(['#define F() (foo)']);
  AssertConverted;
  AssertInterface('macro with an empty parameter list',['function F : longint;']);
  AssertImplementation('macro with an empty parameter list body',['F:=foo;']);
end;


procedure TTestFunctionMacros.TestIndentationAfterMacros;

begin
  Convert(['#define F(a) a','#define CAST_E ((int)5)','#define G 1','int f(void);','typedef int t;']);
  AssertConverted;
  AssertEquals('no indentation warning','',Trim(ToolOutput));
  AssertRawLine('const block after macros','  const');
  AssertRawLine('constant after macros','    G = 1;');
  AssertRawLine('function after macros','  function f:longint;');
  AssertRawLine('type block after macros','  type');
  AssertRawLine('type after macros','    t = longint;');
end;


procedure TTestFunctionMacros.TestCompactIndentationAfterMacros;

begin
  Convert(['#define F(a) a','#define CAST_E ((int)5)','#define G 1','int f(void);','typedef int t;'],['-c']);
  AssertConverted;
  AssertEquals('no indentation warning','',Trim(ToolOutput));
  AssertRawLine('-c const block after macros','const');
  AssertRawLine('-c constant after macros','  G = 1;');
  AssertRawLine('-c function after macros','function f:longint;');
  AssertRawLine('-c type block after macros','type');
end;


initialization
  RegisterTests('H2Pas',[TTestConstMacros,TTestFunctionMacros]);
end.
