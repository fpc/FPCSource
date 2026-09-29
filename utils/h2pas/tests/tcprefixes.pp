{
  h2pas test suite: the T type prefix (-t, -T) and the P pointer prefix (-p).
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcPrefixes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TPrefixTestCase }

  TPrefixTestCase = class(TH2PasTestCase)
  protected
    // Converts the prefix sample header with aOptions.
    procedure ConvertSample(const aOptions: array of string);
  end;

  { TTestNoPrefix }

  TTestNoPrefix = class(TPrefixTestCase)
  published
    procedure TestTypes;
    procedure TestParams;
    procedure TestHeaderPointerTypes;
    procedure TestUnitCompiles;
  end;

  { TTestTypePrefix }

  TTestTypePrefix = class(TPrefixTestCase)
  published
    procedure TestTypes;
    procedure TestHeaderPointerTypes;
    procedure TestParams;
    procedure TestParamsImplementation;
    procedure TestUnitCompiles;
    procedure TestBaseTypesNotPrefixed;
    procedure TestCTypesNotPrefixed;
    procedure TestUnderscoreCTypesNotPrefixed;
    procedure TestUnnamedParams;
    procedure TestVariableType;
    procedure TestFunctionPointerArgType;
    procedure TestUnderscoreTypes;
    procedure TestUnderscoreAliasSkipped;
    procedure TestUnderscoreUnnamedParams;
    procedure TestUnderscoreIdentifiers;
    procedure TestUnderscoreParamType;
    procedure TestUnderscoreParams;
    procedure TestUnderscorePointerNames;
    procedure TestUnderscoreUnitCompiles;
    procedure TestFunctionPointerParamType;
  end;

  { TTestPointerPrefix }

  TTestPointerPrefix = class(TPrefixTestCase)
  published
    procedure TestTypes;
    procedure TestParams;
    procedure TestNoPointerForProcType;
    procedure TestStructKeywordParams;
    procedure TestSystemPointerTypesNotDeclared;
    procedure TestPointerResult;
    procedure TestSelfPointerMember;
    procedure TestPointerTypedef;
    procedure TestVarParamsWin;
    procedure TestCTypes;
    procedure TestPointerTypesDeclaredOnce;
    procedure TestUnitCompiles;
  end;

  { TTestCombinedPrefixes }

  TTestCombinedPrefixes = class(TPrefixTestCase)
  published
    procedure TestPointerAndTypeTypes;
    procedure TestPointerAndTypeParams;
    procedure TestPointerAndUnderscoreTypes;
    procedure TestPointerAndUnderscoreParams;
    procedure TestPointerAndUnderscoreAlias;
    procedure TestPointerAndUnderscoreDeclaredOnce;
    procedure TestPointerAndUnderscoreCompiles;
  end;

  { TTestPointerToPointer }

  TTestPointerToPointer = class(TH2PasTestCase)
  protected
    // Converts the pointer to pointer sample header with aOptions and -d.
    procedure ConvertSample(const aOptions: array of string);
  published
    procedure TestDeclaredAfterTarget;
    procedure TestSystemTypesNotDeclared;
    procedure TestPointerToBaseType;
    procedure TestPointerPrefix;
    procedure TestUnderscorePrefix;
    procedure TestTypePrefix;
    procedure TestCompiles;
  end;

implementation

const
  PrefixHeader : array[0..5] of string = (
    'typedef struct _point { int x; int y; } point;',
    'typedef int myint;',
    'struct _rect { point *tl; struct _point br; };',
    'typedef enum { red, green } color;',
    'void doit(point *p, myint *m, struct _rect *r, int);',
    'typedef void (*callback_t)(int code);'
  );


procedure TPrefixTestCase.ConvertSample(const aOptions: array of string);

begin
  Convert(PrefixHeader,aOptions);
  AssertConverted;
end;


procedure TTestNoPrefix.TestTypes;

begin
  ConvertSample([]);
  AssertInterface('tagged struct',['_point = record','x : longint;','y : longint;','end;','point = _point;']);
  AssertInterface('base type typedef',['myint = longint;']);
  AssertInterface('struct members',['_rect = record','tl : ^point;','br : _point;','end;']);
  AssertInterface('enum',['color = (red,green);']);
  AssertInterface('procedure type',['callback_t = procedure (code:longint);cdecl;']);
end;


procedure TTestNoPrefix.TestParams;

begin
  ConvertSample([]);
  AssertInterface('pointer parameters use P plus the C name',['procedure doit(p:Ppoint; m:Pmyint; r:P_rect; _para4:longint);']);
end;


procedure TTestNoPrefix.TestHeaderPointerTypes;

begin
  ConvertSample([]);
  AssertInterface('pointer to typedef follows the typedef',['point = _point;','Ppoint = ^point;']);
  AssertInterface('pointer to base type typedef follows the typedef',['myint = longint;','Pmyint = ^myint;']);
  AssertInterface('pointer to struct tag precedes the struct',['P_rect = ^_rect;','_rect = record']);
  AssertNotOutput('no header pointer list','Type');
end;


procedure TTestNoPrefix.TestUnitCompiles;

begin
  ConvertSample(['-d']);
  AssertCompiles;
end;


procedure TTestTypePrefix.TestTypes;

begin
  ConvertSample(['-t']);
  AssertInterface('-t prefixes the struct tag and the typedef',
    ['T_point = record','x : longint;','y : longint;','end;','Tpoint = T_point;']);
  AssertInterface('-t prefixes a base type typedef',['Tmyint = longint;']);
  AssertInterface('-t prefixes member types',['T_rect = record','tl : ^Tpoint;','br : T_point;','end;']);
  AssertInterface('-t prefixes the enum type',['Tcolor = (red,green);']);
  AssertInterface('-t prefixes a procedure type',['Tcallback_t = procedure (code:longint);cdecl;']);
end;


procedure TTestTypePrefix.TestHeaderPointerTypes;

begin
  ConvertSample(['-t']);
  AssertOutput('-t pointer to typedef points to the T type',['Ppoint = ^Tpoint;']);
  AssertOutput('-t pointer to base type typedef points to the T type',['Pmyint = ^Tmyint;']);
  AssertOutput('-t pointer to struct tag points to the T type',['P_rect = ^T_rect;']);
end;


procedure TTestTypePrefix.TestParams;

begin
  ConvertSample(['-t']);
  AssertInterface('-t parameters use the declared pointer types',['procedure doit(p:Ppoint; m:Pmyint; r:P_rect; _para4:longint);']);
  AssertNotOutput('-t pointer names have no T','PT');
end;


procedure TTestTypePrefix.TestParamsImplementation;

begin
  Convert(['typedef int myint;','myint *f(myint *a);'],['-t']);
  AssertConverted;
  AssertImplementation('-t stub uses the declared pointer types',['function f(a:Pmyint):Pmyint;']);
end;


procedure TTestTypePrefix.TestUnitCompiles;

begin
  ConvertSample(['-t','-d']);
  AssertCompiles;
end;


procedure TTestTypePrefix.TestBaseTypesNotPrefixed;

begin
  Convert(['typedef int myint;','void f(int a, float b, char *c);'],['-t']);
  AssertConverted;
  AssertInterface('-t does not prefix Pascal base types',['procedure f(a:longint; b:single; c:Pansichar);']);
end;


procedure TTestTypePrefix.TestCTypesNotPrefixed;

begin
  Convert(['struct s { int a; unsigned long b; };','int g;','void f(int *p, char *c, unsigned int u);'],['-t','-C']);
  AssertConverted;
  AssertInterface('-t does not prefix ctypes field types',['Ts = record','a : cint;','b : culong;','end;']);
  AssertInterface('-t does not prefix ctypes variable types',['g : cint;cvar;public;']);
  AssertInterface('-t keeps the ctypes pointer types',['procedure f(p:pcint; c:pcchar; u:cuint);']);
end;


procedure TTestTypePrefix.TestUnderscoreCTypesNotPrefixed;

begin
  Convert(['typedef int _myint;','struct s { short a; _myint b; };'],['-T','-C']);
  AssertConverted;
  AssertInterface('-T does not prefix ctypes but prefixes typedefs',['Ts = record','a : cshort;','b : Tmyint;','end;']);
  AssertInterface('-T typedef of a ctypes type',['Tmyint = cint;']);
end;


procedure TTestTypePrefix.TestUnnamedParams;

begin
  ConvertSample(['-t']);
  AssertTrue('-t keeps the underscore of unnamed parameters',Pos('; _para4:longint);',InterfacePart)>0);
end;


procedure TTestTypePrefix.TestVariableType;

begin
  Convert(['typedef int myint;','extern myint v;'],['-t']);
  AssertConverted;
  AssertInterface('-t prefixes variable types',['v : Tmyint;cvar;external;']);
end;


procedure TTestTypePrefix.TestFunctionPointerArgType;

begin
  Convert(['typedef int myint;','typedef int (*binop)(myint a);'],['-t']);
  AssertConverted;
  AssertInterface('-t prefixes procedure type and argument types',['Tbinop = function (a:Tmyint):longint;cdecl;']);
end;


procedure TTestTypePrefix.TestUnderscoreTypes;

begin
  ConvertSample(['-T']);
  AssertInterface('-T removes the leading underscore of the struct tag',['Tpoint = record','x : longint;','y : longint;','end;']);
  AssertInterface('-T prefixes a base type typedef',['Tmyint = longint;']);
  AssertInterface('-T member types',['Trect = record','tl : ^Tpoint;','br : Tpoint;','end;']);
  AssertInterface('-T prefixes the enum type',['Tcolor = (red,green);']);
  AssertInterface('-T prefixes a procedure type',['Tcallback_t = procedure (code:longint);cdecl;']);
end;


procedure TTestTypePrefix.TestUnderscoreAliasSkipped;

begin
  ConvertSample(['-T']);
  AssertNotOutput('-T drops the alias that would equal the tag','Tpoint = Tpoint;');
end;


procedure TTestTypePrefix.TestUnderscoreUnnamedParams;

begin
  ConvertSample(['-T']);
  AssertTrue('-T names unnamed parameters without underscore',Pos('; para4:longint);',InterfacePart)>0);
  AssertNotOutput('-T writes no _para names','_para4');
end;


procedure TTestTypePrefix.TestUnderscoreIdentifiers;

begin
  Convert(['typedef struct _s { struct _s *next; _u f; } s;'],['-T']);
  AssertConverted;
  AssertInterface('-T removes underscores of referenced types',['Ts = record','next : ^Ts;','f : Tu;','end;']);
end;


procedure TTestTypePrefix.TestUnderscoreParamType;

begin
  Convert(['typedef int _myint;','extern _myint v;','void f(_myint a);'],['-T']);
  AssertConverted;
  AssertInterface('-T typedef',['Tmyint = longint;']);
  AssertInterface('-T variable type',['v : Tmyint;cvar;external;']);
  AssertInterface('-T parameter type',['procedure f(a:Tmyint);']);
end;


procedure TTestTypePrefix.TestUnderscoreParams;

begin
  ConvertSample(['-T']);
  AssertInterface('-T parameters use the declared pointer types',['procedure doit(p:Ppoint; m:Pmyint; r:Prect; para4:longint);']);
  AssertNotOutput('-T pointer names have no T','PT');
end;


procedure TTestTypePrefix.TestUnderscorePointerNames;

begin
  ConvertSample(['-T']);
  AssertInterface('-T pointer to a struct tag without underscore',['Prect = ^Trect;','Trect = record']);
  AssertNotOutput('-T refers to no type with an underscore','T_rect');
end;


procedure TTestTypePrefix.TestUnderscoreUnitCompiles;

begin
  ConvertSample(['-T','-d']);
  AssertCompiles;
end;


procedure TTestTypePrefix.TestFunctionPointerParamType;

begin
  Convert(['void f(void (*vcb)(void));','void g(void (*)(void));'],['-T']);
  AssertConverted;
  AssertInterface('-T prefixes the procedural type of a parameter',['Tf_vcb = procedure ;cdecl;']);
  AssertInterface('-T parameter uses the prefixed type',['procedure f(vcb:Tf_vcb);']);
  AssertInterface('-T unnamed parameter type',['Tg_para1 = procedure ;cdecl;']);
  AssertInterface('-T unnamed parameter',['procedure g(para1:Tg_para1);']);
end;


procedure TTestPointerPrefix.TestTypes;

begin
  ConvertSample(['-p']);
  AssertInterface('-p declares a pointer type before the struct tag and the typedef',
    ['P_point = ^_point;','_point = record','x : longint;','y : longint;','end;','point = _point;','Ppoint = ^point;']);
  AssertInterface('-p declares a pointer type for a base type typedef',['Pmyint = ^myint;','myint = longint;']);
  AssertInterface('-p uses the pointer type in members',['P_rect = ^_rect;','_rect = record','tl : Ppoint;','br : _point;','end;']);
  AssertInterface('-p declares a pointer type for an enum',['Pcolor = ^color;','color = (red,green);']);
end;


procedure TTestPointerPrefix.TestParams;

begin
  ConvertSample(['-p']);
  AssertInterface('-p pointer parameters',['procedure doit(p:Ppoint; m:Pmyint; r:P_rect; _para4:longint);']);
end;


procedure TTestPointerPrefix.TestNoPointerForProcType;

begin
  ConvertSample(['-p']);
  AssertInterface('-p procedure type',['callback_t = procedure (code:longint);cdecl;']);
  AssertNotOutput('-p declares no pointer to a procedure type','Pcallback_t');
end;


procedure TTestPointerPrefix.TestStructKeywordParams;

begin
  Convert(['void f(struct s *p, union u *q);'],['-p']);
  AssertConverted;
  AssertInterface('-p pointers to struct and union tags',['procedure f(p:Ps; q:Pu);']);
  AssertOutput('-p declares the struct pointer type',['Ps = ^s;']);
  AssertOutput('-p declares the union pointer type',['Pu = ^u;']);
end;


procedure TTestPointerPrefix.TestSystemPointerTypesNotDeclared;

begin
  Convert(['void f(int *a, char *b, double *c, short *d);'],['-p']);
  AssertConverted;
  AssertInterface('-p pointers to base types',['procedure f(a:Plongint; b:Pansichar; c:Pdouble; d:Psmallint);']);
  AssertNotOutput('Plongint comes from the system unit','Plongint =');
  AssertNotOutput('Pansichar comes from the system unit','Pansichar =');
  AssertNotOutput('Pdouble comes from the system unit','Pdouble =');
  AssertNotOutput('Psmallint comes from the system unit','Psmallint =');
end;


procedure TTestPointerPrefix.TestPointerResult;

begin
  Convert(['char *name(void);','void *data(void);'],['-p']);
  AssertConverted;
  AssertInterface('-p pointer result',['function name:Pansichar;']);
  AssertInterface('-p untyped pointer result',['function data:pointer;']);
  Convert(['int **pp(void);'],['-p']);
  AssertConverted;
  AssertInterface('-p pointer to pointer result',['function pp:PPlongint;']);
end;


procedure TTestPointerPrefix.TestSelfPointerMember;

begin
  Convert(['struct s { struct s *next; };'],['-p']);
  AssertConverted;
  AssertInterface('-p pointer member to the struct itself',['Ps = ^s;','s = record','next : Ps;','end;']);
end;


procedure TTestPointerPrefix.TestPointerTypedef;

begin
  Convert(['typedef char *string_t;'],['-p']);
  AssertConverted;
  AssertInterface('-p pointer typedef',['Pstring_t = ^string_t;','string_t = Pansichar;']);
end;


procedure TTestPointerPrefix.TestVarParamsWin;

begin
  Convert(['typedef struct { int a; } rec;','void f(rec *r);'],['-p','-v']);
  AssertConverted;
  AssertInterface('-v wins over -p for typed pointers',['procedure f(var r:rec);']);
end;


procedure TTestPointerPrefix.TestCTypes;

begin
  Convert(['void f(int *a, long *b, char *c, unsigned int *d);'],['-p','-C']);
  AssertConverted;
  AssertInterface('-p with -C uses the ctypes pointer types',['procedure f(a:pcint; b:pclong; c:pcchar; d:pcuint);']);
end;


procedure TTestPointerPrefix.TestPointerTypesDeclaredOnce;

begin
  ConvertSample(['-p']);
  AssertEquals('-p declares Ppoint once',1,CountOf('Ppoint = ^point;'));
  AssertEquals('-p declares P_rect once',1,CountOf('P_rect = ^_rect;'));
  AssertNotOutput('-p writes no header pointer list','Type');
end;


procedure TTestPointerPrefix.TestUnitCompiles;

begin
  ConvertSample(['-p','-d']);
  AssertCompiles;
end;


procedure TTestCombinedPrefixes.TestPointerAndTypeTypes;

begin
  ConvertSample(['-p','-t']);
  AssertInterface('-p -t struct tag and typedef',
    ['P_point = ^T_point;','T_point = record','x : longint;','y : longint;','end;','Tpoint = T_point;','Ppoint = ^Tpoint;']);
  AssertInterface('-p -t base type typedef',['Pmyint = ^Tmyint;','Tmyint = longint;']);
  AssertInterface('-p -t members',['P_rect = ^T_rect;','T_rect = record','tl : Ppoint;','br : T_point;','end;']);
  AssertInterface('-p -t enum',['Pcolor = ^Tcolor;','Tcolor = (red,green);']);
  AssertInterface('-p -t procedure type',['Tcallback_t = procedure (code:longint);cdecl;']);
end;


procedure TTestCombinedPrefixes.TestPointerAndTypeParams;

begin
  ConvertSample(['-p','-t']);
  AssertInterface('-p -t parameters',['procedure doit(p:Ppoint; m:Pmyint; r:P_rect; _para4:longint);']);
end;


procedure TTestCombinedPrefixes.TestPointerAndUnderscoreTypes;

begin
  ConvertSample(['-p','-T']);
  AssertInterface('-p -T struct',['Ppoint = ^Tpoint;','Tpoint = record','x : longint;','y : longint;','end;']);
  AssertInterface('-p -T base type typedef',['Pmyint = ^Tmyint;','Tmyint = longint;']);
  AssertInterface('-p -T members',['Prect = ^Trect;','Trect = record','tl : Ppoint;','br : Tpoint;','end;']);
  AssertInterface('-p -T enum',['Pcolor = ^Tcolor;','Tcolor = (red,green);']);
  AssertInterface('-p -T procedure type',['Tcallback_t = procedure (code:longint);cdecl;']);
end;


procedure TTestCombinedPrefixes.TestPointerAndUnderscoreParams;

begin
  ConvertSample(['-p','-T']);
  AssertInterface('-p -T parameters',['procedure doit(p:Ppoint; m:Pmyint; r:Prect; para4:longint);']);
end;


procedure TTestCombinedPrefixes.TestPointerAndUnderscoreAlias;

begin
  Convert(['typedef struct _node { int value; struct _node *next; } node, *pnode;'],['-p','-T']);
  AssertConverted;
  AssertInterface('-p -T struct with pointer alias',
    ['Pnode = ^Tnode;','Tnode = record','value : longint;','next : Pnode;','end;','Tpnode = Pnode;','Ppnode = ^Tpnode;']);
end;


procedure TTestCombinedPrefixes.TestPointerAndUnderscoreDeclaredOnce;

begin
  ConvertSample(['-p','-T']);
  AssertEquals('-p -T declares Prect once',1,CountOf('Prect = ^Trect;'));
  AssertNotOutput('-p -T writes no header pointer list','Type');
end;


procedure TTestCombinedPrefixes.TestPointerAndUnderscoreCompiles;

begin
  ConvertSample(['-p','-T','-d']);
  AssertCompiles;
end;


const
  PointerToPointerHeader : array[0..3] of string = (
    'struct s { int a; };',
    'typedef struct _node { int b; } node;',
    'void g(struct s **pp, node **np, node ***npp, int **ip, char **cp, unsigned short **wp);',
    'node **getn(void);'
  );


procedure TTestPointerToPointer.ConvertSample(const aOptions: array of string);

var
  lOptions: array of string;
  lIndex: integer;

begin
  lOptions:=[];
  SetLength(lOptions,Length(aOptions)+1);
  for lIndex:=0 to Length(aOptions)-1 do
    lOptions[lIndex]:=aOptions[lIndex];
  lOptions[Length(aOptions)]:='-d';
  Convert(PointerToPointerHeader,lOptions);
  AssertConverted;
end;


procedure TTestPointerToPointer.TestDeclaredAfterTarget;

begin
  ConvertSample([]);
  AssertInterface('pointer to pointer to a struct follows the pointer',['Ps = ^s;','PPs = ^Ps;','s = record','a : longint;','end;']);
  AssertInterface('pointer to pointer to a typedef follows the pointer',
    ['node = _node;','Pnode = ^node;','PPnode = ^Pnode;','PPPnode = ^PPnode;']);
  AssertInterface('parameters',['procedure g(pp:PPs; np:PPnode; npp:PPPnode; ip:PPlongint; cp:PPansichar;','wp:PPword);cdecl;external;']);
  AssertInterface('pointer to pointer result',['function getn:PPnode;cdecl;external;']);
end;


procedure TTestPointerToPointer.TestSystemTypesNotDeclared;

begin
  ConvertSample([]);
  AssertNotOutput('PPlongint comes from the system unit','PPlongint =');
  AssertNotOutput('PPansichar comes from the system unit','PPansichar =');
end;


procedure TTestPointerToPointer.TestPointerToBaseType;

begin
  ConvertSample([]);
  AssertOutput('pointer to a system pointer type is in the header pointer list',['Type','PPword = ^Pword;']);
end;


procedure TTestPointerToPointer.TestPointerPrefix;

begin
  ConvertSample(['-p']);
  AssertInterface('-p pointer to pointer to a struct',['Ps = ^s;','PPs = ^Ps;','s = record','a : longint;','end;']);
  AssertInterface('-p pointer to pointer to a typedef',['Pnode = ^node;','PPnode = ^Pnode;','PPPnode = ^PPnode;']);
  AssertEquals('-p declares PPnode once',1,CountOf('PPnode = ^Pnode;'));
end;


procedure TTestPointerToPointer.TestUnderscorePrefix;

begin
  ConvertSample(['-T']);
  AssertInterface('-T pointer to pointer to a struct',['Ps = ^Ts;','PPs = ^Ps;']);
  AssertInterface('-T pointer to pointer to a typedef',['Pnode = ^Tnode;','PPnode = ^Pnode;','PPPnode = ^PPnode;']);
end;


procedure TTestPointerToPointer.TestTypePrefix;

begin
  ConvertSample(['-t']);
  AssertInterface('-t pointer to pointer to a typedef',['Tnode = T_node;','Pnode = ^Tnode;','PPnode = ^Pnode;']);
  AssertInterface('-t parameters',['procedure g(pp:PPs; np:PPnode; npp:PPPnode;']);
end;


procedure TTestPointerToPointer.TestCompiles;

const
  OptionSets : array[0..4] of string = ('','-p','-t','-T','-p -T');

var
  lSet: string;
  lOptions: TStringArray;

begin
  for lSet in OptionSets do
    begin
    lOptions:=[];
    if lSet<>'' then
      lOptions:=lSet.Split(' ');
    ConvertSample(lOptions);
    AssertCompiles;
    end;
end;


initialization
  RegisterTests('H2Pas',[TTestNoPrefix,TTestTypePrefix,TTestPointerPrefix,TTestCombinedPrefixes,TTestPointerToPointer]);
end.
