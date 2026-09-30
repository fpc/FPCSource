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
    procedure TestDefineOfEmptyDefine;
    procedure TestDefineOfEmptyDefineCompiles;
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
    procedure TestCharLiteralArithmetic;
    procedure TestCharLiteralComparison;
    procedure TestCharLiteralKeptAlone;
    procedure TestCharLiteralsCompile;
    procedure TestCharEscapes;
    procedure TestOctalAndHexEscapes;
    procedure TestStringEscapes;
    procedure TestQuotesInStrings;
    procedure TestEscapesCompile;
    procedure TestAdjacentStrings;
    procedure TestAdjacentStringsWithEscapes;
    procedure TestAdjacentStringsCompile;
    procedure TestAlias;
    procedure TestAliasOfLaterEnumMember;
    procedure TestRepeatedDefine;
    procedure TestEnumMemberInConstantExpression;
    procedure TestEnumMemberInMacroBody;
    procedure TestEnumArithmeticCompiles;
    procedure TestRedefinedAfterUndef;
    procedure TestDefinesInConditionalBranches;
    procedure TestRedefinedDefinesCompile;
    procedure TestAliasOfLaterDefine;
    procedure TestAliasChainBeforeUse;
    procedure TestAliasesOfLaterNamesCompile;
    procedure TestTypeMacros;
    procedure TestTypeMacrosPrefixes;
    procedure TestTypeMacrosCompile;
    procedure TestTypeMacroOfDeclaredType;
    procedure TestDefineNameCaseClash;
    procedure TestDefineNameCaseClashCompiles;
    procedure TestKeywordDefines;
    procedure TestKeywordDefinesStripped;
    procedure TestKeywordDefinesCompile;
    procedure TestKeywordNameDefines;
    procedure TestEmptyBodyMacro;
    procedure TestReservedWordMacroNames;
    procedure TestZconfDefinesCompile;
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
    procedure TestAdditionBindsTighterThanShift;
    procedure TestComparisonBindsTighterThanBitwise;
    procedure TestBitwisePrecedence;
    procedure TestXorAndMod;
    procedure TestLogicalOperators;
    procedure TestOperatorsCompile;
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
    procedure TestTernaryWithComparison;
    procedure TestNestedTernary;
    procedure TestTernaryValueCondition;
    procedure TestTernaryValueConditionCompiles;
    procedure TestLogicalMacros;
    procedure TestLogicalNotOfValue;
    procedure TestLogicalNotOfComparison;
    procedure TestBitwiseNotKept;
    procedure TestNotsCompile;
    procedure TestTernaryAndLogicalCompile;
    procedure TestContinuedBody;
    procedure TestDeref;
    procedure TestDot;
    procedure TestIndex;
    procedure TestAddress;
    procedure TestEqual;
    procedure TestNotEqual;
    procedure TestLess;
    procedure TestGreaterEqual;
    procedure TestComparisonMacroIsBoolean;
    procedure TestCombinedComparisonsAreBoolean;
    procedure TestArithmeticMacroIsLongint;
    procedure TestBooleanMacrosCompile;
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
    procedure TestFunctionAlias;
    procedure TestFunctionAliasWithLibraryName;
    procedure TestFunctionAliasDynamic;
    procedure TestFunctionAliasWrapper;
    procedure TestFunctionAliasOfFunctionWithBody;
    procedure TestFunctionAliasVarargs;
    procedure TestWrapperMacro;
    procedure TestWrapperMacroOtherArguments;
    procedure TestWrapperMacroArgumentCount;
    procedure TestFunctionAliasBeforeFunction;
    procedure TestFunctionAliasesCompile;
    procedure TestMacroResultDereference;
    procedure TestMacroResultNestedCast;
    procedure TestMacroResultAddress;
    procedure TestMacroResultCall;
    procedure TestMacroResultCallDynamic;
    procedure TestMacroResultCallBeforeFunction;
    procedure TestMacroResultCompiles;
    procedure TestMacroParamFromCall;
    procedure TestMacroParamFromPointerCast;
    procedure TestMacroParamOtherUse;
    procedure TestMacroParamConflictingTypes;
    procedure TestMacroParamVarargs;
    procedure TestMacroParamNameOfType;
    procedure TestMacroParamsCompile;
    procedure TestVoidCallMacro;
    procedure TestVoidCallMacroDynamic;
    procedure TestVoidCastMacro;
    procedure TestValueCallMacro;
    procedure TestTernaryInCallArguments;
    procedure TestVoidCallMacrosCompile;
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


procedure TTestConstMacros.TestDefineOfEmptyDefine;

begin
  Convert(['#define SQLITE_APICALL','#define SQLITE_STDCALL SQLITE_APICALL','#define DEEPER SQLITE_STDCALL',
           '#define PAREN (SQLITE_APICALL)','#define other 1','#define ALIAS other']);
  AssertConverted;
  AssertOutput('a define of an empty define is empty',
    ['{$define SQLITE_APICALL}','{$define SQLITE_STDCALL}','{$define DEEPER}','{$define PAREN}']);
  AssertNotOutput('no constant of an empty define','SQLITE_STDCALL =');
  AssertInterface('a define of a constant stays a constant',['other = 1;','ALIAS = other;']);
end;


procedure TTestConstMacros.TestDefineOfEmptyDefineCompiles;

begin
  Convert(['#define SQLITE_APICALL','#define SQLITE_STDCALL SQLITE_APICALL','#ifdef SQLITE_STDCALL','int x;','#endif'],['-d']);
  AssertConverted;
  AssertCompiles;
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


procedure TTestConstMacros.TestCharLiteralArithmetic;

begin
  Convert(['#define FOURCC (((''n''<<24) | (''c''<<16)) | ''x'')','#define DIFF (''a'' - ''A'')','#define NEXT (''\n'' + 1)']);
  AssertConverted;
  AssertInterface('character literals in bit operations',['FOURCC = ((ord(''n'') shl 24) or (ord(''c'') shl 16)) or ord(''x'');']);
  AssertInterface('difference of two characters',['DIFF = ord(''a'')-ord(''A'');']);
  AssertInterface('character code in an addition',['NEXT = ord(#10)+1;']);
end;


procedure TTestConstMacros.TestCharLiteralComparison;

begin
  Convert(['#define ISA(c) ((c) == ''a'')','#define SAME (''a'' == ''b'')']);
  AssertConverted;
  AssertImplementation('a value compared with a character',['ISA:=c=ord(''a'');']);
  AssertInterface('two characters are compared directly',['SAME = ''a''=''b'';']);
end;


procedure TTestConstMacros.TestCharLiteralKeptAlone;

begin
  Convert(['#define CH ''a''','#define NL ''\n''','#define S "a"']);
  AssertConverted;
  AssertInterface('a character literal alone stays a character',['CH = ''a'';','NL = #10;','S = ''a'';']);
end;


procedure TTestConstMacros.TestCharLiteralsCompile;

begin
  Convert(['#define FOURCC (((''n''<<24) | (''c''<<16)) | ''x'')','#define DIFF (''a'' - ''A'')','#define NEXT (''\n'' + 1)',
           '#define ISA(c) ((c) == ''a'')','#define SAME (''a'' == ''b'')','#define UP(c) ((c) - ''a'' + ''A'')'],['-d']);
  AssertConverted;
  AssertCompiles;
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


procedure TTestConstMacros.TestEnumMemberInConstantExpression;

begin
  Convert(['typedef enum { GPG_ERR_NO_ERROR = 0, GPG_ERR_CODE_DIM = 65536 } gpg_err_code_t;',
           '#define GPG_ERR_CODE_MASK (GPG_ERR_CODE_DIM - 1)','#define NEG (-GPG_ERR_CODE_DIM)','#define ALIAS GPG_ERR_NO_ERROR'],['-d']);
  AssertConverted;
  AssertInterface('an enum member in arithmetic is its ordinal value',
    ['GPG_ERR_CODE_MASK = ord(GPG_ERR_CODE_DIM)-1;','NEG = -(ord(GPG_ERR_CODE_DIM));']);
  AssertInterface('an enum member alone keeps its type',['ALIAS = GPG_ERR_NO_ERROR;']);
end;


procedure TTestConstMacros.TestEnumMemberInMacroBody;

begin
  Convert(['typedef enum { IDN2_NFC_INPUT = 1, IDN2_NONTRANSITIONAL = 8 } idn2_flags;',
           'int idn2_to_ascii_4i(const char *i, int l, char *o, int f);',
           '#define idna_to_ascii_4i(i, l, o, f) idn2_to_ascii_4i(i, l, o, f | IDN2_NFC_INPUT | IDN2_NONTRANSITIONAL)',
           '#define IS_NFC(f) ((f) == IDN2_NFC_INPUT)'],['-d']);
  AssertConverted;
  AssertImplementation('enum members in an or',
    ['idna_to_ascii_4i:=idn2_to_ascii_4i(i,l,o,(f or ord(IDN2_NFC_INPUT)) or ord(IDN2_NONTRANSITIONAL));']);
  AssertImplementation('an enum member in a comparison',['IS_NFC:=f=ord(IDN2_NFC_INPUT);']);
end;


procedure TTestConstMacros.TestEnumArithmeticCompiles;

begin
  Convert(['typedef enum { GPG_ERR_NO_ERROR = 0, GPG_ERR_CODE_DIM = 65536 } gpg_err_code_t;',
           '#define GPG_ERR_CODE_MASK (GPG_ERR_CODE_DIM - 1)','#define NEG (-GPG_ERR_CODE_DIM)','#define ALIAS GPG_ERR_NO_ERROR',
           'typedef enum { IDN2_NFC_INPUT = 1, IDN2_NONTRANSITIONAL = 8 } idn2_flags;',
           'int idn2_to_ascii_4i(const char *i, int l, char *o, int f);',
           '#define idna_to_ascii_4i(i, l, o, f) idn2_to_ascii_4i(i, l, o, f | IDN2_NFC_INPUT | IDN2_NONTRANSITIONAL)',
           '#define IS_NFC(f) ((f) == IDN2_NFC_INPUT)'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestRepeatedDefine;

begin
  Convert(['#define MAD_F_FRACBITS 28','#define MAD_F_SCALEBITS MAD_F_FRACBITS','int x;','#define MAD_F_SCALEBITS MAD_F_FRACBITS'],['-d']);
  AssertConverted;
  AssertEquals('the constant is written once',1,CountOf('MAD_F_SCALEBITS = MAD_F_FRACBITS;'));
  AssertOutput('the repeated define is left out',['(* #define MAD_F_SCALEBITS ignored, defined before *)']);
end;


procedure TTestConstMacros.TestRedefinedAfterUndef;

begin
  Convert(['#define PCRE2_LOCAL_WIDTH 8','#undef PCRE2_LOCAL_WIDTH','#define PCRE2_LOCAL_WIDTH 16'],['-d']);
  AssertConverted;
  AssertInterface('the first definition is the constant',['PCRE2_LOCAL_WIDTH = 8;']);
  AssertNotOutput('the second definition is left out','PCRE2_LOCAL_WIDTH = 16');
end;


procedure TTestConstMacros.TestDefinesInConditionalBranches;

begin
  Convert(['#ifdef WIDE','#define BUILD_TYPE 1','#else','#define BUILD_TYPE 2','#endif'],['-d']);
  AssertConverted;
  AssertInterface('a define in each branch of a condition',
    ['{$ifdef WIDE}','const','BUILD_TYPE = 1;','{$else}','const','BUILD_TYPE = 2;','{$endif}']);
  AssertNotOutput('no define is left out','defined before');
end;


procedure TTestConstMacros.TestRedefinedDefinesCompile;

begin
  Convert(['#define MAD_F_FRACBITS 28','#define MAD_F_SCALEBITS MAD_F_FRACBITS','#define MAD_F_SCALEBITS MAD_F_FRACBITS',
           '#define PCRE2_LOCAL_WIDTH 8','#undef PCRE2_LOCAL_WIDTH','#define PCRE2_LOCAL_WIDTH 16',
           '#define SQR(x) ((x)*(x))','#define SQR(x) ((x)*(x))','#define FLAG','#define FLAG 1'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestAliasOfLaterEnumMember;

begin
  Convert(['#define CAIRO_FONT_TYPE_ATSUI CAIRO_FONT_TYPE_QUARTZ',
           'typedef enum _cairo_font_type { CAIRO_FONT_TYPE_TOY, CAIRO_FONT_TYPE_QUARTZ } cairo_font_type_t;'],['-d']);
  AssertConverted;
  AssertInterface('the alias of an enum member follows the enum',
    ['cairo_font_type_t = _cairo_font_type;','const','CAIRO_FONT_TYPE_ATSUI = CAIRO_FONT_TYPE_QUARTZ;']);
end;


procedure TTestConstMacros.TestAliasOfLaterDefine;

begin
  Convert(['#define LZMA_VERSION_STABILITY LZMA_VERSION_STABILITY_STABLE','#define LZMA_VERSION_STABILITY_ALPHA 0',
           '#define LZMA_VERSION_STABILITY_STABLE 2','int after(void);'],['-d']);
  AssertConverted;
  AssertInterface('the alias follows the define it names',
    ['LZMA_VERSION_STABILITY_STABLE = 2;','LZMA_VERSION_STABILITY = LZMA_VERSION_STABILITY_STABLE;',
     'function after:longint;cdecl;external;']);
end;


procedure TTestConstMacros.TestAliasChainBeforeUse;

begin
  Convert(['#define A1 B1','#define B1 C1','#define C1 7','struct s { char buf[A1]; };'],['-d']);
  AssertConverted;
  AssertInterface('a chain of aliases in the order of their declarations, before their use',
    ['C1 = 7;','B1 = C1;','A1 = B1;','type','s = record','buf : array[0..(A1)-1] of ansichar;']);
end;


procedure TTestConstMacros.TestAliasesOfLaterNamesCompile;

const
  Header : array[0..5] of string = (
    '#define CAIRO_FONT_TYPE_ATSUI CAIRO_FONT_TYPE_QUARTZ',
    'typedef enum _cairo_font_type { CAIRO_FONT_TYPE_TOY, CAIRO_FONT_TYPE_QUARTZ } cairo_font_type_t;',
    '#define A1 B1','#define B1 C1','#define C1 7','struct s { char buf[A1]; };');

begin
  Convert(Header,['-d']);
  AssertConverted;
  AssertCompiles;
  Convert(Header,['-d','-1']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestAlias;

begin
  CheckConst('ALIAS','OTHER','OTHER');
end;


procedure TTestConstMacros.TestTypeMacros;

begin
  Convert(['#define z_off_t off_t','#define T int','#define U unsigned long','#define LL long long','#define SZ size_t',
           '#define ALIAS OTHER']);
  AssertConverted;
  AssertInterface('a define of a type is a type alias',
    ['type','z_off_t = PtrInt;','T = longint;','U = dword;','LL = int64;','SZ = SizeUInt;']);
  AssertInterface('a define of another name stays a constant',['const','ALIAS = OTHER;']);
end;


procedure TTestConstMacros.TestTypeMacrosPrefixes;

begin
  Convert(['#define z_off_t off_t','#define T int','z_off_t f(T a, T *b);'],['-d','-T','-C']);
  AssertConverted;
  AssertInterface('type macros under -T -C',['Tz_off_t = coff_t;','TT = cint;','PT = ^TT;']);
  AssertInterface('declarations use the type macros',['function f(a:TT; b:PT):Tz_off_t;cdecl;external;']);
end;


procedure TTestConstMacros.TestTypeMacrosCompile;

begin
  Convert(['#define z_off_t off_t','#define T int','#define U unsigned long','#define SZ size_t',
           'z_off_t f(T a, U *b, SZ c);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestTypeMacroOfDeclaredType;

begin
  Convert(['#define z_off_t long','typedef int myint;','#define z_off64_t z_off_t','#define MI myint']);
  AssertConverted;
  AssertInterface('define of a type macro',['z_off64_t = z_off_t;']);
  AssertInterface('define of a typedef',['MI = myint;']);
  AssertNotOutput('no constant of a type','const');
end;


procedure TTestConstMacros.TestDefineNameCaseClash;

begin
  Convert(['#define ZLIB_VERSION "1.2.11"','#define zlib_version zlibVersion()','#define Zlib_Version(a) (a)']);
  AssertConverted;
  AssertInterface('the first define',['ZLIB_VERSION = ''1.2.11'';']);
  AssertOutput('a define that differs in case only is ignored',['(* #define zlib_version ignored, the Pascal name of ZLIB_VERSION *)']);
  AssertOutput('a macro that differs in case only is ignored',['(* #define Zlib_Version ignored, the Pascal name of ZLIB_VERSION *)']);
  AssertNotOutput('no function of the same Pascal name','function zlib_version');
end;


procedure TTestConstMacros.TestDefineNameCaseClashCompiles;

begin
  Convert(['#define ZLIB_VERSION "1.2.11"','char *zlibVersion(void);','#define zlib_version zlibVersion()'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestKeywordDefines;

begin
  Convert(['#define SQLITE_EXTERN extern','#define API __declspec(dllexport)',
           '#define ATTR __attribute__((visibility("default"))) extern','#define CST const /* c */','#define N 1']);
  AssertConverted;
  AssertOutput('storage class',['(* macro SQLITE_EXTERN with declaration keywords ignored *)']);
  AssertOutput('__declspec',['(* macro API with declaration keywords ignored *)']);
  AssertOutput('attribute and storage class',['(* macro ATTR with declaration keywords ignored *)']);
  AssertOutput('qualifier with a comment',['(* macro CST with declaration keywords ignored *)']);
  AssertInterface('other defines are converted',['N = 1;']);
end;


procedure TTestConstMacros.TestKeywordDefinesStripped;

begin
  Convert(['#define SQLITE_EXTERN extern','#define N 1'],['-S']);
  AssertConverted;
  AssertNotOutput('-S drops the comment','SQLITE_EXTERN');
  AssertInterface('other defines are converted',['N = 1;']);
end;


procedure TTestConstMacros.TestKeywordDefinesCompile;

begin
  Convert(['#define SQLITE_EXTERN extern','#define API __declspec(dllexport)','#define CST const','#define N 1'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestConstMacros.TestKeywordNameDefines;

begin
  Convert(['#define FAR','#define NEAR far','#define CDECL __cdecl','#define N 1']);
  AssertConverted;
  AssertOutput('define of FAR',['(* define of the keyword FAR ignored *)']);
  AssertOutput('define of NEAR',['(* define of the keyword NEAR ignored *)']);
  AssertOutput('define of CDECL',['(* define of the keyword CDECL ignored *)']);
  AssertInterface('other defines are converted',['N = 1;']);
end;


procedure TTestConstMacros.TestEmptyBodyMacro;

begin
  Convert(['#define Z_ARG(args) ()','#define N 1']);
  AssertConverted;
  AssertOutput('macro with the body ()',['(* macro Z_ARG with an empty body ignored *)']);
  AssertNotOutput('no function for the macro','function Z_ARG');
  AssertInterface('other defines are converted',['N = 1;']);
end;


procedure TTestConstMacros.TestReservedWordMacroNames;

begin
  Convert(['#define OF(args) args','#define in(x) ((x)+1)','#define end 3']);
  AssertConverted;
  AssertInterface('macro named OF',['function _OF(args : longint) : longint;']);
  AssertImplementation('body of the macro named OF',['_OF:=args;']);
  AssertInterface('macro named in',['function _in(x : longint) : longint;']);
  AssertInterface('constant named end',['_end = 3;']);
end;


procedure TTestConstMacros.TestZconfDefinesCompile;

begin
  Convert(['#define OF(args) args','#define Z_ARG(args) ()','#define FAR','#define NEAR far','#define in(x) ((x)+1)',
           '#define end 3','#define N 1'],['-d']);
  AssertConverted;
  AssertCompiles;
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
  CheckConst('NOT_E','(!1)','1=0');
end;


procedure TTestConstMacros.TestUnparenthesizedExpression;

begin
  CheckConst('ONE_LINE','1 + 2','1+2');
end;


procedure TTestConstMacros.TestOperatorPrecedence;

begin
  CheckConst('MASK','1 << 4 | 1 << 2','(1 shl 4) or (1 shl 2)');
end;


procedure TTestConstMacros.TestAdditionBindsTighterThanShift;

begin
  Convert(['#define A1 (1 + 2 << 3)','#define A2 (1 << 2 + 3)','#define A3 (8 >> 1 - 1)']);
  AssertConverted;
  AssertInterface('+ and - bind tighter than shifts',['A1 = (1+2) shl 3;','A2 = 1 shl (2+3);','A3 = 8 shr (1-1);']);
end;


procedure TTestConstMacros.TestComparisonBindsTighterThanBitwise;

begin
  Convert(['#define B1 (1 < 2 & 3 > 2)','#define B2 (1 == 1 | 2 != 2)','#define B3 (1 < 2 == 3 > 2)']);
  AssertConverted;
  AssertInterface('comparisons bind tighter than & and |',['B1 = (1<2) and (3>2);','B2 = (1=1) or (2<>2);']);
  AssertInterface('relational operators bind tighter than equality',['B3 = (1<2)=(3>2);']);
end;


procedure TTestConstMacros.TestBitwisePrecedence;

begin
  Convert(['#define C1 (1 | 2 ^ 3 & 4)','#define C2 (1 & 2 | 3)']);
  AssertConverted;
  AssertInterface('& binds tighter than ^, ^ tighter than |',['C1 = 1 or (2 xor (3 and 4));','C2 = (1 and 2) or 3;']);
end;


procedure TTestConstMacros.TestXorAndMod;

begin
  Convert(['#define X1 (5 ^ 3)','#define M1 (7 % 3 * 2)','#define M2 (1 + 7 % 3)']);
  AssertConverted;
  AssertInterface('^ becomes xor',['X1 = 5 xor 3;']);
  AssertInterface('% becomes mod with the precedence of *',['M1 = (7 mod 3)*2;','M2 = 1+(7 mod 3);']);
end;


procedure TTestConstMacros.TestLogicalOperators;

begin
  Convert(['#define L1 (1 && 0)','#define L2 (1 < 2 || 3 < 2)','#define L3 (1 || 2 && 3)']);
  AssertConverted;
  AssertInterface('&& of values compares them to 0',['L1 = (1<>0) and (0<>0);']);
  AssertInterface('|| of comparisons',['L2 = (1<2) or (3<2);']);
  AssertInterface('&& binds tighter than ||',['L3 = (1<>0) or ((2<>0) and (3<>0));']);
end;


procedure TTestConstMacros.TestOperatorsCompile;

begin
  Convert(['#define A1 (1 + 2 << 3)','#define A2 (1 << 2 + 3)','#define B1 (1 < 2 & 3 > 2)','#define B3 (1 < 2 == 3 > 2)',
           '#define C1 (1 | 2 ^ 3 & 4)','#define M1 (7 % 3 * 2)','#define L1 (1 && 0)','#define L3 (1 || 2 && 3)'],['-d']);
  AssertConverted;
  AssertCompiles;
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
     'if a<>0 then','if_local1:=1','else','if_local1:=2;','TERN:=if_local1;','end;']);
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
  AssertInterface('pointer cast to a named type gives the result type',['function P1(p : pointer) : Pfoo;']);
  AssertImplementation('pointer cast to a named type',['P1:=Pfoo(p);']);
  AssertImplementation('pointer cast to a named type without parentheses around the operand',['P2:=Pfoo(p);']);
end;


procedure TTestFunctionMacros.TestDoublePointerCast;

begin
  Convert(['#define C2(p) ((foo **)(p))','#define C7(p) ((foo ***)(p))']);
  AssertConverted;
  AssertInterface('double pointer cast gives the result type',['function C2(p : pointer) : PPfoo;']);
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
  AssertInterface('pointer types of casts precede their target record',['Pfoo = ^foo;','PPfoo = ^Pfoo;','foo = record','a : longint;','end;']);
end;


procedure TTestFunctionMacros.TestPointerCastPrefix;

begin
  Convert(['typedef struct { int a; } foo;','#define C2(p) ((foo **)(p))'],['-p','-T']);
  AssertConverted;
  AssertInterface('-p -T double pointer cast',['function C2(p : pointer) : PPfoo;']);
  AssertInterface('-p -T pointer types of casts',['Pfoo = ^Tfoo;','PPfoo = ^Pfoo;','Tfoo = record','a : longint;','end;']);
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
  AssertImplementation('product in a ternary condition',['if (X*a)<>0 then']);
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
  AssertImplementation('ternary without parentheses',['if a<>0 then','if_local1:=1','else','if_local1:=2;','T:=if_local1;']);
end;


procedure TTestFunctionMacros.TestTernaryWithComparison;

begin
  Convert(['#define MAX(a,b) ((a)>(b)?(a):(b))','#define ROW(pass) ((pass)>2?(8>>(((pass)-1)>>1)):8)']);
  AssertConverted;
  AssertImplementation('comparison is the condition of the ternary',
    ['if a>b then','if_local1:=a','else','if_local1:=b;','MAX:=if_local1;']);
  AssertImplementation('comparison with a constant',
    ['if pass>2 then','if_local1:=8 shr ((pass-1) shr 1)','else','if_local1:=8;','ROW:=if_local1;']);
end;


procedure TTestFunctionMacros.TestNestedTernary;

begin
  Convert(['#define NEST(a) ((a)>1?1:(a)<0?2:3)']);
  AssertConverted;
  AssertImplementation('the ternary is right associative',
    ['if a<0 then','if_local1:=2','else','if_local1:=3;','if a>1 then','if_local2:=1','else','if_local2:=if_local1;',
     'NEST:=if_local2;']);
end;


procedure TTestFunctionMacros.TestTernaryValueCondition;

begin
  Convert(['#define T1(a) ((a) & 0x80 ? 1 : 2)','#define T2(a,b) (((a) > 0) & ((b) < 2) ? 1 : 2)']);
  AssertConverted;
  AssertImplementation('a value as condition is compared to 0',['if (a and $80)<>0 then']);
  AssertImplementation('a combination of comparisons is kept',['if (a>0) and (b<2) then']);
end;


procedure TTestFunctionMacros.TestTernaryValueConditionCompiles;

begin
  Convert(['#define T1(a) ((a) & 0x80 ? 1 : 2)','#define T2(a) ((a) ? 1 : 2)','#define T3(a,b) ((a) > (b) ? (a) : (b))'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctionMacros.TestLogicalMacros;

begin
  Convert(['#define LA(a,b) ((a) && (b))','#define LM(a,b) ((a) && (b) > 1)','#define LO(a,b) ((a) > 0 || (b) < 2)']);
  AssertConverted;
  AssertInterface('logical macros return boolean',['function LA(a,b : longint) : boolean;']);
  AssertImplementation('&& of values',['LA:=(a<>0) and (b<>0);']);
  AssertImplementation('&& of a value and a comparison',['LM:=(a<>0) and (b>1);']);
  AssertImplementation('|| of comparisons',['LO:=(a>0) or (b<2);']);
end;


procedure TTestFunctionMacros.TestLogicalNotOfValue;

begin
  Convert(['#define N3(a) (!(a))','#define N5(a) (!(a) ? 1 : 2)','#define N6(a,b) (!(a) && (b))']);
  AssertConverted;
  AssertInterface('! of a value returns boolean',['function N3(a : longint) : boolean;']);
  AssertImplementation('! of a value is a comparison to 0',['N3:=a=0;']);
  AssertImplementation('! as ternary condition',['if a=0 then']);
  AssertImplementation('! in a logical and',['N6:=(a=0) and (b<>0);']);
end;


procedure TTestFunctionMacros.TestLogicalNotOfComparison;

begin
  Convert(['#define N4(a,b) (!((a) > (b)))']);
  AssertConverted;
  AssertInterface('! of a comparison returns boolean',['function N4(a,b : longint) : boolean;']);
  AssertImplementation('! of a comparison is not',['N4:= not (a>b);']);
end;


procedure TTestFunctionMacros.TestBitwiseNotKept;

begin
  Convert(['#define N7(a) (~(a) & 0xff)']);
  AssertConverted;
  AssertInterface('~ keeps an integer result',['function N7(a : longint) : longint;']);
  AssertImplementation('~ is a bitwise not',['N7:=( not (a)) and $ff;']);
end;


procedure TTestFunctionMacros.TestNotsCompile;

begin
  Convert(['#define N1 (!1)','#define N2 (~1)','#define N3(a) (!(a))','#define N4(a,b) (!((a) > (b)))',
           '#define N5(a) (!(a) ? 1 : 2)','#define N6(a,b) (!(a) && (b))','#define N7(a) (~(a) & 0xff)'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctionMacros.TestTernaryAndLogicalCompile;

begin
  Convert(['#define MAX(a,b) ((a)>(b)?(a):(b))','#define NEST(a) ((a)>1?1:(a)<0?2:3)','#define LA(a,b) ((a) && (b))',
           '#define LO(a,b) ((a) > 0 || (b) < 2)','#define XO(a,b) ((a) ^ (b))','#define MO(a,b) ((a) % (b))'],['-d']);
  AssertConverted;
  AssertCompiles;
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


procedure TTestFunctionMacros.TestComparisonMacroIsBoolean;

begin
  Convert(['#define GE(a, b) ((a) >= (b))','#define EQ(a,b) (a == b)','#define V 3','#define HAVE(h) (V >= (h))']);
  AssertConverted;
  AssertInterface('comparison macro returns boolean',['function GE(a,b : longint) : boolean;']);
  AssertInterface('equality macro returns boolean',['function EQ(a,b : longint) : boolean;']);
  AssertInterface('comparison with a constant returns boolean',['function HAVE(h : longint) : boolean;']);
  AssertNotOutput('the result type of a comparison is known','return type might be wrong');
end;


procedure TTestFunctionMacros.TestCombinedComparisonsAreBoolean;

begin
  Convert(['#define BOTH(a,b) (((a) > 0) & ((b) < 2))','#define EITHER(a,b) (((a) != 0) | ((b) <= 2))']);
  AssertConverted;
  AssertInterface('and of comparisons',['function BOTH(a,b : longint) : boolean;']);
  AssertInterface('or of comparisons',['function EITHER(a,b : longint) : boolean;']);
  AssertImplementation('and of comparisons body',['BOTH:=(a>0) and (b<2);']);
end;


procedure TTestFunctionMacros.TestArithmeticMacroIsLongint;

begin
  Convert(['#define ADD(a,b) ((a) + (b))','#define MIX(a,b) (((a) > 0) | (b))','#define CAST(a) ((int)((a) > 0))']);
  AssertConverted;
  AssertInterface('arithmetic macro keeps longint',['function ADD(a,b : longint) : longint;']);
  AssertInterface('or of a comparison and a value keeps longint',['function MIX(a,b : longint) : longint;']);
  AssertInterface('cast result type is kept',['function CAST(a : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestBooleanMacrosCompile;

begin
  Convert(['#define GE(a, b) ((a) >= (b))','#define EQ(a,b) (a == b)','#define V 3','#define HAVE(h) (V >= (h))',
           '#define BOTH(a,b) (((a) > 0) & ((b) < 2))','#define ADD(a,b) ((a) + (b))'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctionMacros.TestPointerCast;

begin
  Convert(['#define PCAST(p) ((char *)(p))']);
  AssertConverted;
  AssertInterface('pointer cast gives the result type',['function PCAST(p : pointer) : Pansichar;']);
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
  Convert(['#define FM(a) a','#define CAST_E ((int)5)','#define G 1','int f(void);','typedef int t;']);
  AssertConverted;
  AssertEquals('no indentation warning','',Trim(ToolOutput));
  AssertRawLine('const block after macros','  const');
  AssertRawLine('constant after macros','    G = 1;');
  AssertRawLine('function after macros','  function f:longint;');
  AssertRawLine('type block after macros','  type');
  AssertRawLine('type after macros','    t = longint;');
end;


const
  AliasHeader : array[0..3] of string = (
    'typedef long XML_Size;',
    'XML_Size XML_GetCurrentLineNumber(void *parser);',
    '#define XML_GetErrorLineNumber XML_GetCurrentLineNumber',
    'void reset(void);');

procedure TTestFunctionMacros.TestFunctionAlias;

begin
  Convert(AliasHeader,['-d']);
  AssertConverted;
  AssertInterface('an alias of an external function imports the same symbol',
    ['function XML_GetErrorLineNumber(parser:pointer):XML_Size;cdecl;external name ''XML_GetCurrentLineNumber'';']);
  AssertNotOutput('no constant for the alias','XML_GetErrorLineNumber =');
end;


procedure TTestFunctionMacros.TestFunctionAliasWithLibraryName;

begin
  Convert(AliasHeader,['-D','-l','libexpat']);
  AssertConverted;
  AssertInterface('an alias with the library name',
    ['function XML_GetErrorLineNumber(parser:pointer):XML_Size;cdecl;external External_library name ''XML_GetCurrentLineNumber'';']);
end;


procedure TTestFunctionMacros.TestFunctionAliasDynamic;

begin
  Convert(AliasHeader,['-P','-l','libexpat.so']);
  AssertConverted;
  AssertInterface('an alias is a procedure variable',['XML_GetErrorLineNumber : function(parser:pointer):XML_Size;cdecl;']);
  AssertImplementation('the alias is loaded from the symbol of the function',
    ['pointer(XML_GetErrorLineNumber):=GetProcAddress(hlib,''XML_GetCurrentLineNumber'');']);
end;


procedure TTestFunctionMacros.TestFunctionAliasWrapper;

begin
  Convert(AliasHeader);
  AssertConverted;
  AssertInterface('an alias of a function to implement',['function XML_GetErrorLineNumber(parser:pointer):XML_Size;']);
  AssertImplementation('the alias calls the function',
    ['function XML_GetErrorLineNumber(parser:pointer):XML_Size;','begin',
     'XML_GetErrorLineNumber:=XML_GetCurrentLineNumber(parser);','end;']);
end;


procedure TTestFunctionMacros.TestFunctionAliasOfFunctionWithBody;

begin
  Convert(['static inline int twice(int a) { return a * 2; }','#define twice2 twice'],['-d']);
  AssertConverted;
  AssertInterface('an alias of a function with a body is no import',['function twice2(a:longint):longint;']);
  AssertNotOutput('the alias imports nothing','external name');
  AssertImplementation('the alias calls the function',['function twice2(a:longint):longint;','begin','twice2:=twice(a);','end;']);
end;


procedure TTestFunctionMacros.TestFunctionAliasVarargs;

begin
  Convert(['int logf_(const char *fmt, ...);','#define logf2 logf_'],['-d']);
  AssertConverted;
  AssertInterface('an alias of a varargs function',['function logf2(fmt:Pansichar):longint;cdecl;varargs;external name ''logf_'';']);
  Convert(['int logf_(const char *fmt, ...);','#define logf2 logf_']);
  AssertConverted;
  AssertImplementation('the array of const variant passes the arguments',['logf2:=logf_(fmt,args);']);
  AssertImplementation('the variant without arguments',['logf2:=logf_(fmt);']);
end;


procedure TTestFunctionMacros.TestWrapperMacro;

begin
  Convert(['int crc(int seed, const char *buf, int len);','#define crc_alias(s, b, l) crc(s, b, l)',
           '#define crc_paren(s, b, l) (crc((s), (b), (l)))','void reset(void);','#define reset2() reset()'],['-d']);
  AssertConverted;
  AssertInterface('a macro that passes its parameters is an alias',
    ['function crc_alias(seed:longint; buf:Pansichar; len:longint):longint;cdecl;external name ''crc'';']);
  AssertInterface('with parentheses around the call and the arguments',
    ['function crc_paren(seed:longint; buf:Pansichar; len:longint):longint;cdecl;external name ''crc'';']);
  AssertInterface('a macro without parameters',['procedure reset2;cdecl;external name ''reset'';']);
end;


procedure TTestFunctionMacros.TestWrapperMacroOtherArguments;

begin
  Convert(['int crc(int seed, const char *buf, int len);','#define swapped(s, b, l) crc(b, s, l)',
           '#define extra(s, b) crc(s, b, 0)'],['-d']);
  AssertConverted;
  AssertInterface('parameters in another order give a macro function',['function swapped(s : Pansichar; b,l : longint) : longint;']);
  AssertInterface('an argument that is no parameter gives a macro function',['function extra(s : longint; b : Pansichar) : longint;']);
  AssertNotOutput('no alias','external name');
end;


procedure TTestFunctionMacros.TestWrapperMacroArgumentCount;

begin
  Convert(['int one(int a);','#define two(a, b) one(a, b)'],['-d']);
  AssertConverted;
  AssertNotOutput('a call with another number of arguments is no alias','external name');
end;


procedure TTestFunctionMacros.TestFunctionAliasBeforeFunction;

begin
  Convert(['#define early later','int later(void);'],['-d']);
  AssertConverted;
  AssertInterface('an alias before its function stays a constant',['early = later;']);
end;


procedure TTestFunctionMacros.TestFunctionAliasesCompile;

begin
  Convert(['typedef long XML_Size;','XML_Size XML_GetCurrentLineNumber(void *parser);',
           '#define XML_GetErrorLineNumber XML_GetCurrentLineNumber','int crc(int seed, const char *buf, int len);',
           '#define crc_alias(s, b, l) crc(s, b, l)','void reset(void);','#define reset2() reset()',
           'static inline int twice(int a) { return a * 2; }','#define twice2 twice','int logf_(const char *fmt, ...);',
           '#define logf2 logf_'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctionMacros.TestMacroResultDereference;

begin
  Convert(['#define XML_GetUserData(parser) (*(void **)(parser))','#define GETI(p) (*(int *)(p))'],['-d']);
  AssertConverted;
  AssertInterface('the dereference of a void pointer pointer is a pointer',
    ['{ was #define dname(params) para_def_expr }','function XML_GetUserData(parser : pointer) : pointer;']);
  AssertInterface('the dereference of an int pointer is an int',
    ['{ was #define dname(params) para_def_expr }','function GETI(p : pointer) : longint;']);
  AssertNotOutput('the result types are known','return type might be wrong');
end;


procedure TTestFunctionMacros.TestMacroResultNestedCast;

begin
  Convert(['#define CAST(x) ((unsigned char *)(x) + 1)'],['-d']);
  AssertConverted;
  AssertInterface('the type of a cast inside the body',['{ was #define dname(params) para_def_expr }','function CAST(x : pointer) : Pbyte;']);
end;


procedure TTestFunctionMacros.TestMacroResultAddress;

begin
  Convert(['#define ADDR(x) (&(x))'],['-d']);
  AssertConverted;
  AssertInterface('an address is a pointer',['{ argument types are unknown }','function ADDR(x : longint) : pointer;']);
end;


procedure TTestFunctionMacros.TestMacroResultCall;

begin
  Convert(['typedef struct s *sp;','sp mk(int a, int b);','char *getname(int a);','char **names(void);',
           '#define MK1(a) mk(a, 0)','#define NAME1(a) (getname((a)+1))','#define FIRST(a) (*names())'],['-d']);
  AssertConverted;
  AssertInterface('the result type of the called function',['{ was #define dname(params) para_def_expr }','function MK1(a : longint) : sp;']);
  AssertInterface('a called function that returns a pointer',
    ['{ argument types are unknown }','function NAME1(a : longint) : Pansichar;']);
  AssertInterface('the dereference of the result of a call',
    ['{ argument types are unknown }','function FIRST(a : longint) : Pansichar;']);
  AssertNotOutput('the result types are known','return type might be wrong');
end;


procedure TTestFunctionMacros.TestMacroResultCallDynamic;

begin
  Convert(['char *getname(int a);','#define NAME1(a) getname(a + 1)'],['-P','-l','libx.so']);
  AssertConverted;
  AssertInterface('the result type of a function loaded at run time',
    ['{ argument types are unknown }','function NAME1(a : longint) : Pansichar;']);
end;


procedure TTestFunctionMacros.TestMacroResultCallBeforeFunction;

begin
  Convert(['#define LATER(a) later(a, 1)','char *later(int a, int b);'],['-d']);
  AssertConverted;
  AssertInterface('a function declared after the macro gives no type',
    ['{ return type might be wrong }','function LATER(a : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestMacroResultCompiles;

begin
  Convert(['typedef struct s *sp;','sp mk(int a, int b);','char *getname(int a);','char **names(void);',
           '#define MK1(a) mk(a, 0)','#define NAME1(a) (getname((a)+1))','#define FIRST(a) (*names())',
           '#define XML_GetUserData(parser) (*(void **)(parser))','#define GETI(p) (*(int *)(p))',
           '#define CAST(x) ((unsigned char *)(x) + 1)'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


const
  ZlibMacroHeader : array[0..5] of string = (
    'typedef struct z_stream_s { int avail_in; } z_stream;',
    'typedef z_stream *z_streamp;',
    'int deflateInit_(z_streamp strm, int level, const char *version, int stream_size);',
    'int inflateBackInit_(z_streamp strm, int windowBits, unsigned char *window, const char *version, int stream_size);',
    '#define deflateInit(strm, level) deflateInit_((strm), (level), "1.3", (int)sizeof(z_stream))',
    '#define inflateBackInit(strm, windowBits, window) inflateBackInit_((strm), (windowBits), (window), "1.3", (int)sizeof(z_stream))');

procedure TTestFunctionMacros.TestMacroParamFromCall;

begin
  Convert(ZlibMacroHeader,['-d']);
  AssertConverted;
  AssertInterface('a parameter passed to a declared function has the type of the argument',
    ['{ was #define dname(params) para_def_expr }','function deflateInit(strm : z_streamp; level : longint) : longint;']);
  AssertInterface('parameters of different types',
    ['function inflateBackInit(strm : z_streamp; windowBits : longint; window : Pbyte) : longint;']);
  AssertNotOutput('the argument types are known','argument types are unknown');
end;


procedure TTestFunctionMacros.TestMacroParamFromPointerCast;

begin
  Convert(['#define XML_GetUserData(parser) (*(void **)(parser))'],['-d']);
  AssertConverted;
  AssertInterface('a parameter cast to a pointer is a pointer',['function XML_GetUserData(parser : pointer) : pointer;']);
  AssertImplementation('the body is unchanged',['XML_GetUserData:=(Ppointer(parser))^;']);
end;


procedure TTestFunctionMacros.TestMacroParamOtherUse;

begin
  Convert(['int f(char *a);','#define MIXED(p) (*(int *)(p) + p)','#define SUM(p) f((p) + 1)'],['-d']);
  AssertConverted;
  AssertInterface('a parameter that is also used otherwise has no type',
    ['{ argument types are unknown }','{ return type might be wrong }','function MIXED(p : longint) : longint;']);
  AssertInterface('a parameter in an argument expression has no type',
    ['{ argument types are unknown }','function SUM(p : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestMacroParamConflictingTypes;

begin
  Convert(['int fi(int a);','int fp(char *a);','#define BOTH(x) (fi(x) + fp(x))','#define TWICE(x) (fi(x) - fi(x))'],['-d']);
  AssertConverted;
  AssertInterface('arguments of different types give no type',['{ argument types are unknown }','{ return type might be wrong }',
    'function BOTH(x : longint) : longint;']);
  AssertInterface('arguments of the same type give that type',['{ was #define dname(params) para_def_expr }',
    '{ return type might be wrong }','function TWICE(x : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestMacroParamVarargs;

begin
  Convert(['int pr(const char *f, ...);','#define PRX(f, x) pr(f, x)'],['-d']);
  AssertConverted;
  AssertInterface('a parameter passed as variable argument has no type',
    ['{ argument types are unknown }','function PRX(f : Pansichar; x : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestMacroParamNameOfType;

begin
  Convert(['typedef int sp;','int g(sp a, int b);','#define G1(sp) g(sp, 1)'],['-d']);
  AssertConverted;
  AssertInterface('a parameter with the name of its type has no type',['{ argument types are unknown }','function G1(sp : longint) : longint;']);
end;


procedure TTestFunctionMacros.TestMacroParamsCompile;

begin
  Convert(ZlibMacroHeader,['-d']);
  AssertConverted;
  AssertCompiles;
  Convert(['#define XML_GetUserData(parser) (*(void **)(parser))','int fi(int a);','#define TWICE(x) (fi(x) - fi(x))'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


const
  VoidCallHeader : array[0..6] of string = (
    'void nothing(int a);',
    'int some(int a);',
    '#define NOTHING1(a) nothing(a+1)',
    '#define NOTHING2(a) (nothing((a)*2))',
    '#define CASTV(a) ((void)nothing(a+1))',
    '#define CASTI(a) (void)some((a))',
    '#define TERN(a) nothing(a ? 1 : 2)');

procedure TTestFunctionMacros.TestVoidCallMacro;

begin
  Convert(VoidCallHeader,['-d']);
  AssertConverted;
  AssertInterface('a macro that calls a procedure is a procedure',
    ['{ argument types are unknown }','procedure NOTHING1(a : longint);']);
  AssertInterface('with parentheses around the call',['{ argument types are unknown }','procedure NOTHING2(a : longint);']);
  AssertImplementation('the body calls the procedure',['procedure NOTHING1(a : longint);','begin','nothing(a+1);','end;']);
  AssertNotOutput('no result type','return type might be wrong');
end;


procedure TTestFunctionMacros.TestVoidCallMacroDynamic;

begin
  Convert(['void nothing(int a);','#define NOTHING1(a) nothing(a+1)'],['-P','-l','libx.so']);
  AssertConverted;
  AssertInterface('a macro that calls a procedure variable is a procedure',
    ['{ argument types are unknown }','procedure NOTHING1(a : longint);']);
  AssertImplementation('the body calls the procedure variable',['begin','nothing(a+1);','end;']);
end;


procedure TTestFunctionMacros.TestVoidCastMacro;

begin
  Convert(VoidCallHeader,['-d']);
  AssertConverted;
  AssertInterface('a void cast of a procedure call is a procedure',
    ['{ argument types are unknown }','procedure CASTV(a : longint);']);
  AssertInterface('a void cast of a function call is a procedure',
    ['{ was #define dname(params) para_def_expr }','procedure CASTI(a : longint);']);
  AssertImplementation('the body calls the function without cast',['procedure CASTI(a : longint);','begin','some(a);','end;']);
end;


procedure TTestFunctionMacros.TestValueCallMacro;

begin
  Convert(['void *mem(int a);','#define MEM1(a) mem(a+1)'],['-d']);
  AssertConverted;
  AssertInterface('a macro that calls a function stays a function',
    ['{ argument types are unknown }','function MEM1(a : longint) : pointer;']);
end;


procedure TTestFunctionMacros.TestTernaryInCallArguments;

begin
  Convert(['int some(int a);','#define TERN3(a) some(a ? 1 : 2)'],['-d']);
  AssertConverted;
  AssertImplementation('a conditional in a call argument is assigned before the call',
    ['begin','if a<>0 then','if_local1:=1','else','if_local1:=2;','TERN3:=some(if_local1);','end;']);
end;


procedure TTestFunctionMacros.TestVoidCallMacrosCompile;

begin
  Convert(VoidCallHeader,['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctionMacros.TestCompactIndentationAfterMacros;

begin
  Convert(['#define FM(a) a','#define CAST_E ((int)5)','#define G 1','int f(void);','typedef int t;'],['-c']);
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
