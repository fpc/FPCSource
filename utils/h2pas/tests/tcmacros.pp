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
    procedure TestFloatSuffixes;
    procedure TestFloatWithoutDigits;
    procedure TestFloatsCompile;
    procedure TestString;
    procedure TestCharLiteral;
    procedure TestCharEscapes;
    procedure TestOctalAndHexEscapes;
    procedure TestStringEscapes;
    procedure TestQuotesInStrings;
    procedure TestEscapesCompile;
    procedure TestAdjacentStrings;
    procedure TestAdjacentStringsWithEscapes;
    procedure TestAdjacentStringsCompile;
    procedure TestAlias;
    procedure TestSelfReferencingDefine;
    procedure TestSelfReferencingDefineStripped;
    procedure TestSelfReferencingDefineCompiles;
    procedure TestOr;
    procedure TestAnd;
    procedure TestShl;
    procedure TestShr;
    procedure TestAdd;
    procedure TestSubtract;
    procedure TestMultiply;
    procedure TestBitwiseNot;
    procedure TestLogicalNot;
    procedure TestUnparenthesizedExpression;
    procedure TestOperatorPrecedence;
    procedure TestLineContinuation;
    procedure TestMultiLineContinuation;
    procedure TestContinuedValueOnNextLine;
    procedure TestUnparenthesizedCompiles;
    procedure TestParenthesizedProduct;
    procedure TestParenthesizedProductPrecedence;
    procedure TestParenthesizedProductGrouping;
    procedure TestParenthesizedProductCompiles;
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
    procedure TestParenthesizedParameter;
    procedure TestParenthesizedParameterOperators;
    procedure TestParenthesizedParameterPrecedence;
    procedure TestParenthesizedParameterGrouping;
    procedure TestParenthesizedParameterCast;
    procedure TestCastToTypeKept;
    procedure TestPointerCastToNamedType;
    procedure TestDoublePointerCast;
    procedure TestDoublePointerCastToBaseType;
    procedure TestPointerCastTypesDeclared;
    procedure TestPointerCastPrefix;
    procedure TestPointerCastsCompile;
    procedure TestProductInParameterMacro;
    procedure TestDereference;
    procedure TestDereferenceOfCast;
    procedure TestDoubleDereference;
    procedure TestDereferenceInExpression;
    procedure TestFunctionPointerCall;
    procedure TestParenthesizedNameTimesName;
    procedure TestUnparenthesizedBody;
    procedure TestUnparenthesizedTernary;
    procedure TestContinuedBody;
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
    procedure TestStatementMacro;
    procedure TestContinuedStatementMacro;
    procedure TestSideEffectMacros;
    procedure TestCallMacrosKept;
    procedure TestStatementMacroStripped;
    procedure TestStatementMacrosCompile;
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


procedure TTestConstMacros.TestFloatSuffixes;

begin
  Convert(['#define A 1.5f','#define B 2.5L','#define C 1e5f','#define D .5e-3F']);
  AssertConverted;
  AssertInterface('float suffixes are removed',['A = 1.5;','B = 2.5;','C = 1e5;','D = 0.5e-3;']);
end;


procedure TTestConstMacros.TestFloatWithoutDigits;

begin
  Convert(['#define A .5','#define B 1.','#define C 1.e3']);
  AssertConverted;
  AssertInterface('missing digits around the point are added',['A = 0.5;','B = 1.0;','C = 1.0e3;']);
end;


procedure TTestConstMacros.TestFloatsCompile;

begin
  Convert(['#define A 1.5f','#define B .5','#define C 1.','#define D 1e5f','#define E (1.0f/3)'],['-d']);
  AssertConverted;
  AssertInterface('division with a float suffix',['E = 1.0/3;']);
  AssertCompiles;
end;


procedure TTestConstMacros.TestString;

begin
  CheckConst('STRC','"abc"','''abc''');
end;


procedure TTestConstMacros.TestCharLiteral;

begin
  CheckConst('CHC','''x''','''x''');
end;


procedure TTestConstMacros.TestCharEscapes;

begin
  Convert(['#define NL ''\n''','#define TB ''\t''','#define CR ''\r''','#define NUL ''\0''','#define BSL ''\\''']);
  AssertConverted;
  AssertInterface('character escapes become character codes',['NL = #10;','TB = #9;','CR = #13;','NUL = #0;','BSL = ''\'';']);
end;


procedure TTestConstMacros.TestOctalAndHexEscapes;

begin
  Convert(['#define HX ''\x41''','#define OC ''\101''','#define ESC "\033[0m"']);
  AssertConverted;
  AssertInterface('hexadecimal and octal escapes',['HX = #65;','OC = #65;','ESC = #27''[0m'';']);
end;


procedure TTestConstMacros.TestStringEscapes;

begin
  Convert(['#define S "a\tb\n"','#define E ""']);
  AssertConverted;
  AssertInterface('escapes inside a string',['S = ''a''#9''b''#10;','E = '''';']);
end;


procedure TTestConstMacros.TestQuotesInStrings;

begin
  Convert(['#define DQ "say \"hi\""','#define AP "it''s"','#define SQ ''\''''']);
  AssertConverted;
  AssertInterface('escaped double quote',['DQ = ''say "hi"'';']);
  AssertInterface('apostrophe is doubled',['AP = ''it''''s'';']);
  AssertInterface('escaped apostrophe',['SQ = '''''''';']);
end;


procedure TTestConstMacros.TestEscapesCompile;

begin
  Convert(['#define NL ''\n''','#define S "a\tb\n"','#define E ""','#define DQ "say \"hi\""','#define AP "it''s"',
           '#define SQ ''\''''','#define ESC "\033[0m"','#define BSL ''\\'''],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestAdjacentStrings;

begin
  Convert(['#define S "ab" "cd"','#define V "" "x"','#define W ("p" \','  "q")']);
  AssertConverted;
  AssertInterface('adjacent strings are joined',['S = ''abcd'';','V = ''x'';','W = ''pq'';']);
end;


procedure TTestConstMacros.TestAdjacentStringsWithEscapes;

begin
  Convert(['#define T "a\n" "b" "c\t"','#define U "it''" "s"']);
  AssertConverted;
  AssertInterface('adjacent strings with character codes',['T = ''a''#10''bc''#9;']);
  AssertInterface('adjacent strings with an apostrophe',['U = ''it''''s'';']);
end;


procedure TTestConstMacros.TestAdjacentStringsCompile;

begin
  Convert(['#define S "ab" "cd"','#define T "a\n" "b" "c\t"','#define U "it''" "s"','#define V "" "x"'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestAlias;

begin
  CheckConst('ALIAS','OTHER','OTHER');
end;


procedure TTestConstMacros.TestSelfReferencingDefine;

begin
  Convert(['#define X X','#define Y (Y)','#define foo FOO','#define Z 1']);
  AssertConverted;
  AssertOutput('define of its own name is ignored',['(* self-referencing #define X ignored *)']);
  AssertOutput('parenthesized own name is ignored',['(* self-referencing #define Y ignored *)']);
  AssertOutput('own name in another case is ignored',['(* self-referencing #define foo ignored *)']);
  AssertNotOutput('no constant for a define of its own name','X = X;');
  AssertInterface('other defines are converted',['Z = 1;']);
end;


procedure TTestConstMacros.TestSelfReferencingDefineStripped;

begin
  Convert(['#define X X','#define Z 1'],['-S']);
  AssertConverted;
  AssertNotOutput('-S drops the comment','self-referencing');
  AssertInterface('other defines are converted',['Z = 1;']);
end;


procedure TTestConstMacros.TestSelfReferencingDefineCompiles;

begin
  Convert(['#define X X','#define Y (Y)','#define Z 1'],['-d']);
  AssertConverted;
  AssertCompiles;
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


procedure TTestConstMacros.TestUnparenthesizedExpression;

begin
  CheckConst('ONE_LINE','1 + 2','1+2');
end;


procedure TTestConstMacros.TestOperatorPrecedence;

begin
  CheckConst('MASK','1 << 4 | 1 << 2','(1 shl 4) or (1 shl 2)');
end;


procedure TTestConstMacros.TestLineContinuation;

begin
  Convert(['#define LONGDEF 1 + \','  2']);
  AssertConverted;
  AssertInterface('define continued on the next line',['LONGDEF = 1+2;']);
end;


procedure TTestConstMacros.TestMultiLineContinuation;

begin
  Convert(['#define CONT 1 + \','   2 + \','   3']);
  AssertConverted;
  AssertInterface('define continued over three lines',['CONT = (1+2)+3;']);
end;


procedure TTestConstMacros.TestContinuedValueOnNextLine;

begin
  Convert(['#define PLAIN_CONT \','  42']);
  AssertConverted;
  AssertInterface('value on the continuation line',['PLAIN_CONT = 42;']);
end;


procedure TTestConstMacros.TestUnparenthesizedCompiles;

begin
  Convert(['#define A 1','#define B 1 << 4 | A','#define C B + \','  2 * A','#define D 1.0 / 3'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestParenthesizedProduct;

begin
  CheckConst('N1','(X * 2)','X*2');
end;


procedure TTestConstMacros.TestParenthesizedProductPrecedence;

begin
  Convert(['#define N2 (X * 2 + 1)','#define N3 (X * 2 / 3)','#define N5 (X * 2 == 4)','#define N6 (X * 2 << 1)']);
  AssertConverted;
  AssertInterface('product before an addition',['N2 = (X*2)+1;']);
  AssertInterface('product before a division',['N3 = (X*2) div 3;']);
  AssertInterface('product before a comparison',['N5 = (X*2)=4;']);
  AssertInterface('product before a shift',['N6 = (X*2) shl 1;']);
end;


procedure TTestConstMacros.TestParenthesizedProductGrouping;

begin
  Convert(['#define N4 (X * (2 + 1))','#define N7 (X * -1)']);
  AssertConverted;
  AssertInterface('parentheses on the right operand are kept',['N4 = X*(2+1);']);
  AssertInterface('negative right operand',['N7 = X*(-(1));']);
end;


procedure TTestConstMacros.TestParenthesizedProductCompiles;

begin
  Convert(['#define X 3','#define N1 (X * 2)','#define N2 (X * 2 + 1)','#define N4 (X * (2 + 1))'],['-d']);
  AssertConverted;
  AssertCompiles;
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


procedure TTestFunctionMacros.TestParenthesizedParameter;

begin
  Convert(['#define PAR1(a) ((a) + 1)']);
  AssertConverted;
  AssertInterface('parenthesized parameter is no typecast',['function PAR1(a : longint) : longint;']);
  AssertImplementation('parenthesized parameter body',['PAR1:=a+1;']);
end;


procedure TTestFunctionMacros.TestParenthesizedParameterOperators;

begin
  Convert(['#define M1(a) (a) - 1','#define M6(a) (a) & 1','#define M10(a) (a) - -1','#define M11(a,b) ((a) + (b))']);
  AssertConverted;
  AssertImplementation('minus after a parenthesized parameter',['M1:=a-1;']);
  AssertImplementation('and after a parenthesized parameter',['M6:=a and 1;']);
  AssertImplementation('minus before a negative number',['M10:=a-(-(1));']);
  AssertImplementation('two parenthesized parameters',['M11:=a+b;']);
end;


procedure TTestFunctionMacros.TestParenthesizedParameterPrecedence;

begin
  Convert(['#define M2(a,x) x * (a) - 1','#define M3(a,x) (a) - 1 * x','#define M4(a,x) x - (a) - 1']);
  AssertConverted;
  AssertImplementation('higher precedence on the left',['M2:=(x*a)-1;']);
  AssertImplementation('higher precedence on the right',['M3:=a-(1*x);']);
  AssertImplementation('left associative',['M4:=(x-a)-1;']);
end;


procedure TTestFunctionMacros.TestParenthesizedParameterGrouping;

begin
  Convert(['#define M7(a,x) x * ((a) - 1)','#define N6(a,x,y) y * ((x) * (a) - 1)']);
  AssertConverted;
  AssertImplementation('explicit parentheses are kept',['M7:=x*(a-1);']);
  AssertImplementation('explicit parentheses around a re-associated operation',['N6:=y*((x*a)-1);']);
end;


procedure TTestFunctionMacros.TestParenthesizedParameterCast;

begin
  Convert(['#define M8(a) ((int)(a) + 1)','#define M9(a) -(a) + 1']);
  AssertConverted;
  AssertImplementation('cast of a parenthesized parameter',['M8:=(longint(a))+1;']);
  AssertImplementation('negation of a parenthesized parameter',['M9:=(-(a))+1;']);
end;


procedure TTestFunctionMacros.TestCastToTypeKept;

begin
  Convert(['#define NOPARAM(x) ((mytype) + 1)']);
  AssertConverted;
  AssertInterface('cast to a name that is no parameter gives the result type',['function NOPARAM(x : longint) : mytype;']);
  AssertImplementation('cast to a name that is no parameter',['NOPARAM:=mytype(+(1));']);
end;


procedure TTestFunctionMacros.TestPointerCastToNamedType;

begin
  Convert(['#define P1(p) ((foo *)(p))','#define P2(p) ((foo *) p)']);
  AssertConverted;
  AssertInterface('pointer cast to a named type gives the result type',['function P1(p : longint) : Pfoo;']);
  AssertImplementation('pointer cast to a named type',['P1:=Pfoo(p);']);
  AssertImplementation('pointer cast to a named type without parentheses around the operand',['P2:=Pfoo(p);']);
end;


procedure TTestFunctionMacros.TestDoublePointerCast;

begin
  Convert(['#define C2(p) ((foo **)(p))','#define C7(p) ((foo ***)(p))']);
  AssertConverted;
  AssertInterface('double pointer cast gives the result type',['function C2(p : longint) : PPfoo;']);
  AssertImplementation('double pointer cast',['C2:=PPfoo(p);']);
  AssertImplementation('triple pointer cast',['C7:=PPPfoo(p);']);
end;


procedure TTestFunctionMacros.TestDoublePointerCastToBaseType;

begin
  Convert(['#define C3(p) ((char **)(p))','#define C4(p) ((void **)(p))','#define C5(p) ((struct s **)(p))']);
  AssertConverted;
  AssertImplementation('double pointer cast to char',['C3:=PPansichar(p);']);
  AssertImplementation('double pointer cast to void',['C4:=Ppointer(p);']);
  AssertImplementation('double pointer cast to a struct',['C5:=PPs(p);']);
end;


procedure TTestFunctionMacros.TestPointerCastTypesDeclared;

begin
  Convert(['typedef struct { int a; } foo;','#define C1(p) ((foo *)(p))','#define C2(p) ((foo **)(p))']);
  AssertConverted;
  AssertInterface('pointer types of casts are declared after their target',['foo = record','a : longint;','end;','Pfoo = ^foo;','PPfoo = ^Pfoo;']);
end;


procedure TTestFunctionMacros.TestPointerCastPrefix;

begin
  Convert(['typedef struct { int a; } foo;','#define C2(p) ((foo **)(p))'],['-p','-T']);
  AssertConverted;
  AssertInterface('-p -T double pointer cast',['function C2(p : longint) : PPfoo;']);
  AssertInterface('-p -T pointer types of casts',['Pfoo = ^Tfoo;','Tfoo = record','a : longint;','end;','PPfoo = ^Pfoo;']);
end;


procedure TTestFunctionMacros.TestPointerCastsCompile;

begin
  Convert(['typedef struct { int a; } foo;','#define C1(p) ((foo *)(p))','#define C2(p) ((foo **)(p))',
           '#define C3(p) ((char **)(p))','#define C4(p) ((void **)(p))','#define C5(p) ((int *)(p))'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctionMacros.TestDereference;

begin
  Convert(['#define D1(p) (*(p))','#define D6(p) ((*p))']);
  AssertConverted;
  AssertImplementation('dereference of a parenthesized name',['D1:=p^;']);
  AssertImplementation('dereference between parentheses',['D6:=p^;']);
end;


procedure TTestFunctionMacros.TestDereferenceOfCast;

begin
  Convert(['typedef struct { int a; } foo;','#define D2(p) (*(foo **)(p))','#define D9(p) (*(int *)(p) + 1)']);
  AssertConverted;
  AssertImplementation('dereference of a pointer cast',['D2:=(PPfoo(p))^;']);
  AssertImplementation('dereference of a cast in an addition',['D9:=((Plongint(p))^)+1;']);
  AssertInterface('pointer type of the dereferenced cast is declared',['Pfoo = ^foo;','PPfoo = ^Pfoo;']);
end;


procedure TTestFunctionMacros.TestDoubleDereference;

begin
  Convert(['#define D3(pp) (**pp)']);
  AssertConverted;
  AssertImplementation('double dereference',['D3:=(pp^)^;']);
end;


procedure TTestFunctionMacros.TestDereferenceInExpression;

begin
  Convert(['#define D4(p) (*p + 1)','#define D5(a,p) ((a) * *p)','#define D8(x,p) (x * *p)']);
  AssertConverted;
  AssertImplementation('dereference before an addition',['D4:=(p^)+1;']);
  AssertImplementation('product of a parenthesized parameter and a dereference',['D5:=a*(p^);']);
  AssertImplementation('product of a name and a dereference',['D8:=x*(p^);']);
end;


procedure TTestFunctionMacros.TestFunctionPointerCall;

begin
  Convert(['#define D7(fp) ((*fp)(1, 2))']);
  AssertConverted;
  AssertImplementation('call through a function pointer with its arguments',['D7:=fp(1, 2);']);
  AssertEquals('no output to the console','',Trim(ToolOutput));
end;


procedure TTestFunctionMacros.TestParenthesizedNameTimesName;

begin
  Convert(['#define M1(a,b) ((a) * (b))','#define M2(a,b) ((a) * b + 1)','#define M3 ((X) * Y)']);
  AssertConverted;
  AssertImplementation('parenthesized parameters',['M1:=a*b;']);
  AssertImplementation('parenthesized parameter times a name',['M2:=(a*b)+1;']);
  AssertInterface('parenthesized name times a name',['M3 = X*Y;']);
end;


procedure TTestFunctionMacros.TestProductInParameterMacro;

begin
  Convert(['#define M5(a,x,y) y * (x * (a) - 1)','#define T1(a) (X * a ? 1 : 2)']);
  AssertConverted;
  AssertImplementation('product of a name with a parenthesized parameter',['M5:=y*((x*a)-1);']);
  AssertImplementation('product in a ternary condition',['if X*a then']);
end;


procedure TTestFunctionMacros.TestUnparenthesizedBody;

begin
  Convert(['#define F(a) a + 1','#define CMP(a,b) a == b']);
  AssertConverted;
  AssertImplementation('macro body without parentheses',['F:=a+1;']);
  AssertImplementation('comparison without parentheses',['CMP:=a=b;']);
end;


procedure TTestFunctionMacros.TestUnparenthesizedTernary;

begin
  Convert(['#define T(a) a ? 1 : 2']);
  AssertConverted;
  AssertImplementation('ternary without parentheses',['if a then','if_local1:=1','else','if_local1:=2;','T:=if_local1;']);
end;


procedure TTestFunctionMacros.TestContinuedBody;

begin
  Convert(['#define MACRO_CONT(a) ((a) * \','  2)']);
  AssertConverted;
  AssertImplementation('macro body continued on the next line',['MACRO_CONT:=a*2;']);
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
  AssertInterface('pointer cast gives the result type',['function PCAST(p : longint) : Pansichar;']);
  AssertImplementation('pointer cast body',['PCAST:=Pansichar(p);']);
end;


procedure TTestFunctionMacros.TestStatementMacro;

begin
  Convert(['#define SWAP(a,b) do { int t = a; a = b; b = t; } while (0)','#define BEGIN_DECLS {','#define A 1']);
  AssertConverted;
  AssertOutput('statement macro is ignored',['(* macro SWAP with statements or side effects ignored *)']);
  AssertOutput('macro with a brace is ignored',['(* macro BEGIN_DECLS with statements or side effects ignored *)']);
  AssertNotOutput('no function for a statement macro','function SWAP');
  AssertInterface('next define is converted',['A = 1;']);
end;


procedure TTestFunctionMacros.TestContinuedStatementMacro;

begin
  Convert(['#define L(x) \','  do { x; \','  } while (0)','#define B 2','int after;']);
  AssertConverted;
  AssertOutput('continued statement macro is ignored',['(* macro L with statements or side effects ignored *)']);
  AssertInterface('declarations after the macro are converted',['B = 2;','var','after : longint;cvar;public;']);
end;


procedure TTestFunctionMacros.TestSideEffectMacros;

begin
  Convert(['#define M(a) ((a)++, (a))','#define N(g) ((g)->have ? ((g)->have--, (g)->pos) : 0)',
           '#define SET(c, v) (c) = (v)','#define ADD(c, v) (c) += (v)']);
  AssertConverted;
  AssertOutput('comma operator and increment',['(* macro M with statements or side effects ignored *)']);
  AssertOutput('decrement',['(* macro N with statements or side effects ignored *)']);
  AssertOutput('assignment',['(* macro SET with statements or side effects ignored *)']);
  AssertOutput('compound assignment',['(* macro ADD with statements or side effects ignored *)']);
end;


procedure TTestFunctionMacros.TestCallMacrosKept;

begin
  Convert(['#define C(x) f(x, 2)','#define S "a;b{"','#define K(x) (x) /* a; b */','#define E(a,b) ((a) == (b))',
           '#define LE(a,b) ((a) <= (b))','#define D (1 - -1)']);
  AssertConverted;
  AssertNotOutput('macros without statements or side effects are converted','side effects ignored');
  AssertImplementation('comma between call arguments',['C:=f(x,2);']);
  AssertInterface('semicolon and brace inside a string',['S = ''a;b{'';']);
  AssertImplementation('semicolon inside a comment',['K:=x;']);
end;


procedure TTestFunctionMacros.TestStatementMacroStripped;

begin
  Convert(['#define SWAP(a,b) do { a = b; } while (0)','#define A 1'],['-S']);
  AssertConverted;
  AssertNotOutput('-S drops the comment','side effects ignored');
  AssertInterface('next define is converted',['A = 1;']);
end;


procedure TTestFunctionMacros.TestStatementMacrosCompile;

begin
  Convert(['#define SWAP(a,b) do { int t = a; a = b; b = t; } while (0)','#define L(x) \','  do { x; \','  } while (0)',
           '#define M(a) ((a)++, (a))','#define A 1','int after;'],['-d']);
  AssertConverted;
  AssertCompiles;
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
