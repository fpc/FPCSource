{
  h2pas test suite: function prototypes, function bodies and variables.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcDeclarations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestFunctions }

  TTestFunctions = class(TH2PasTestCase)
  published
    procedure TestVoidParamList;
    procedure TestEmptyParamList;
    procedure TestFunctionResult;
    procedure TestLongInputLine;
    procedure TestCommentsInArgumentList;
    procedure TestFunctionNamesDifferingInCase;
    procedure TestPointerToLaterStruct;
    procedure TestPointerToUndeclaredStruct;
    procedure TestPointersToLaterStructsCompile;
    procedure TestFunctionNamesDifferingInCaseDynamic;
    procedure TestFunctionNameOfMacroInOtherCase;
    procedure TestFunctionPointerArgumentResult;
    procedure TestNestedFunctionPointerResult;
    procedure TestImplementationStub;
    procedure TestNamedParams;
    procedure TestUnnamedParams;
    procedure TestReservedWordParams;
    procedure TestPointerParams;
    procedure TestPointerToPointerParam;
    procedure TestOpenArrayParam;
    procedure TestConstParam;
    procedure TestFunctionPointerParams;
    procedure TestUnnamedFunctionPointerParam;
    procedure TestNestedFunctionPointerParam;
    procedure TestFunctionPointerParamCompiles;
    procedure TestEllipsis;
    procedure TestEllipsisExternal;
    procedure TestEllipsisStdcall;
    procedure TestEllipsisProcedureTypes;
    procedure TestEllipsisDynLib;
    procedure TestFarNearPointers;
    procedure TestParamListWraps;
    procedure TestVoidPointerResult;
    procedure TestPointerResultImplementation;
    procedure TestPointerResultInterface;
    procedure TestPointerResultUnitCompiles;
    procedure TestStructPointerResult;
    procedure TestStructPointerResultStub;
    procedure TestStructPointerResultCompiles;
    procedure TestExternFunction;
    procedure TestUnionPointerParam;
    procedure TestEnumParam;
    procedure TestPointerParamDeclaresPointerType;
    procedure TestPointerParamExternalType;
    procedure TestPointerParamUnitCompiles;
    procedure TestReferenceParams;
    procedure TestUnnamedReferenceParam;
    procedure TestReferenceResult;
    procedure TestReferenceInProcedureType;
    procedure TestReferencesCompile;
    procedure TestQualifiersIgnored;
    procedure TestAttributesIgnored;
    procedure TestMicrosoftCallingConventions;
    procedure TestStaticInlineFunction;
    procedure TestStaticInlineNotExternal;
    procedure TestStaticInlineCompiles;
    procedure TestPointerResultReturnsNil;
    procedure TestPointerResultReturnsNilCompiles;
    procedure TestArrayParams;
    procedure TestArrayParamsCompile;
    procedure TestFunctionPointerResult;
    procedure TestFunctionPointerResultWithFunctionPointerArg;
    procedure TestFunctionPointerResultCompiles;
    procedure TestFunctionPointerArrayParam;
    procedure TestFunctionPointerArrayParamCompiles;
    procedure TestStaticPrototypeIgnored;
    procedure TestStaticPrototypeNotImported;
    procedure TestExternFunctionBody;
    procedure TestExternFunctionBodyNotImported;
    procedure TestExternFunctionBodyCompiles;
    procedure TestFunctionBody;
    procedure TestWhileStatement;
  end;

  { TTestVariables }

  TTestVariables = class(TH2PasTestCase)
  published
    procedure TestExternVariable;
    procedure TestPublicVariable;
    procedure TestArrayVariable;
    procedure TestPointerVariable;
    procedure TestUnknownTypeVariable;
    procedure TestMultipleVariables;
    procedure TestMultipleExternVariables;
    procedure TestProcedureTypeVariable;
    procedure TestFunctionPointerArrayVariable;
    procedure TestFunctionPointerArrayVariablesCompile;
    procedure TestPointerToFunctionPointer;
    procedure TestAnonymousStructVariable;
    procedure TestAnonymousUnionAndEnumVariables;
    procedure TestAnonymousVariablesCompile;
    procedure TestStaticVariable;
    procedure TestStaticVariablesCompile;
    procedure TestPointersAfterFunctionResultOfFunctionPointer;
    procedure TestFunctionPointerVariableResult;
    procedure TestFunctionPointerVariableArgument;
    procedure TestFunctionPointerResultsCompile;
    procedure TestVariableNameOfDefineInOtherCase;
    procedure TestPointerToArrayVariable;
    procedure TestPointerToArrayArgument;
    procedure TestDefineNameOfVariableInOtherCase;
    procedure TestVariableNameOfTypeInOtherCase;
    procedure TestEnumMemberNameOfDefineInOtherCase;
    procedure TestNamesDifferingInCaseCompile;
  end;

implementation


procedure TTestFunctions.TestVoidParamList;

begin
  Convert(['void f(void);']);
  AssertConverted;
  AssertInterface('void function without arguments is a procedure',['procedure f;']);
end;


procedure TTestFunctions.TestEmptyParamList;

begin
  Convert(['void f();']);
  AssertConverted;
  AssertInterface('empty parameter list gives no parameters',['procedure f;']);
end;


const
  LaterStructHeader : array[0..5] of string = (
    'struct pci_device { int x; };',
    'const struct pci_agp_info *pci_device_get_agp_info(struct pci_device *dev);',
    'extern struct pci_state *gstate;',
    'struct pci_agp_info { unsigned int fast_writes:1; int rate; };',
    'struct pci_state { int s; };',
    'struct snd_shm_area *snd_shm_area_create(int shmid, void *ptr);');

procedure TTestFunctions.TestPointerToLaterStruct;

begin
  Convert(LaterStructHeader,['-d']);
  AssertConverted;
  AssertInterface('a struct declared after the function that points to it moves before the function',
    ['Ppci_agp_info = ^pci_agp_info;','pci_agp_info = record','flag0 : word;','rate : longint;','end;',
     'function pci_device_get_agp_info(dev:Ppci_device):Ppci_agp_info;cdecl;external;']);
  AssertInterface('and before a variable that points to it',['pci_state = record','s : longint;','end;','var',
    'gstate : ^pci_state;cvar;external;']);
end;


procedure TTestFunctions.TestPointerToUndeclaredStruct;

begin
  Convert(['struct snd_shm_area *snd_shm_area_create(int shmid, void *ptr);'],['-d']);
  AssertConverted;
  AssertInterface('a struct without declaration is an empty record before the function',
    ['snd_shm_area = record','{undefined structure}','end;','Psnd_shm_area = ^snd_shm_area;',
     'function snd_shm_area_create(shmid:longint; ptr:pointer):Psnd_shm_area;cdecl;external;']);
end;


procedure TTestFunctions.TestPointersToLaterStructsCompile;

begin
  Convert(LaterStructHeader,['-d']);
  AssertConverted;
  AssertCompiles;
  Convert(LaterStructHeader,['-d','-1']);
  AssertConverted;
  AssertCompiles;
  Convert(LaterStructHeader,['-P','-l','libx.so']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestFunctionNamesDifferingInCase;

begin
  Convert(['int rl_vi_fWord(int a, int b);','int rl_vi_fword(int a, int b);','#define CALLW(a) rl_vi_fword(a, 0)'],['-d']);
  AssertConverted;
  AssertInterface('the first function keeps its name',['function rl_vi_fWord(a:longint; b:longint):longint;cdecl;external;']);
  AssertInterface('the second function has an underscore and imports its C name',
    ['function rl_vi_fword_(a:longint; b:longint):longint;cdecl;external name ''rl_vi_fword'';']);
  AssertImplementation('a macro calls the renamed function',['CALLW:=rl_vi_fword_(a,0);']);
end;


procedure TTestFunctions.TestFunctionNamesDifferingInCaseDynamic;

begin
  Convert(['int rl_vi_fWord(int a, int b);','int rl_vi_fword(int a, int b);'],['-P','-l','libx.so']);
  AssertConverted;
  AssertInterface('the procedure variable has an underscore',['rl_vi_fword_ : function(a:longint; b:longint):longint;cdecl;']);
  AssertImplementation('it is loaded from the C name',['pointer(rl_vi_fword_):=GetProcAddress(hlib,''rl_vi_fword'');']);
  Convert(['int rl_vi_fWord(int a, int b);','int rl_vi_fword(int a, int b);'],['-D','-l','libx.so']);
  AssertConverted;
  AssertInterface('the library import of the C name',
    ['function rl_vi_fword_(a:longint; b:longint):longint;cdecl;external External_library name ''rl_vi_fword'';']);
end;


procedure TTestFunctions.TestFunctionNameOfMacroInOtherCase;

begin
  Convert(['#define LZMA_VERSION_STRING(a) (a)','const char *lzma_version_string(void);'],['-d']);
  AssertConverted;
  AssertInterface('a function after a macro of that name in other case has an underscore',
    ['function lzma_version_string_:Pansichar;cdecl;external name ''lzma_version_string'';']);
end;


procedure TTestFunctions.TestCommentsInArgumentList;

const
  Header : array[0..2] of string = (
    'struct s { int x; };',
    'extern struct s *make(int /* width */, int /* height */);',
    'typedef struct t { int y; } t_t;');

begin
  Convert(Header,['-d']);
  AssertConverted;
  AssertNotOutput('no section marker after the comments',#6);
  AssertInterface('the comments precede the function',
    ['{ width  }  { height  }','function make(_para1:longint; _para2:longint):Ps;cdecl;external;']);
  AssertCompiles;
  Convert(Header,['-d','-1']);
  AssertConverted;
  AssertNotOutput('no section marker after the comments with -1',#6);
  AssertInterface('the comments stay with the function with -1',
    ['{ width  }  { height  }','function make(_para1:longint; _para2:longint):Ps;cdecl;external;']);
  AssertCompiles;
end;


procedure TTestFunctions.TestLongInputLine;

var
  lLine : AnsiString;
  i : integer;

begin
  lLine:='int longargs(int a0';
  for i:=1 to 400 do
    lLine:=lLine+', int argument_number_'+IntToStr(i);
  lLine:=lLine+'); int after(void);';
  Convert([lLine],['-d']);
  AssertConverted;
  AssertOutput('the declaration of a line longer than the input buffer',['argument_number_400:longint):longint;cdecl;external;']);
  AssertInterface('the declaration after it on the same line',['function after:longint;cdecl;external;']);
end;


procedure TTestFunctions.TestFunctionResult;

begin
  Convert(['int f(void);']);
  AssertConverted;
  AssertInterface('int result gives a longint function',['function f:longint;']);
end;


procedure TTestFunctions.TestImplementationStub;

begin
  Convert(['int f(void);']);
  AssertConverted;
  AssertImplementation('non-extern prototype gets an implementation stub',
    ['function f:longint;','begin','{ You must implement this function }','end;']);
end;


procedure TTestFunctions.TestNamedParams;

begin
  Convert(['int add(int a, int b);']);
  AssertConverted;
  AssertInterface('named parameters keep their names',['function add(a:longint; b:longint):longint;']);
end;


procedure TTestFunctions.TestUnnamedParams;

begin
  Convert(['void f(int, char);']);
  AssertConverted;
  AssertInterface('unnamed parameters get _paraN names',['procedure f(_para1:longint; _para2:ansichar);']);
end;


procedure TTestFunctions.TestReservedWordParams;

begin
  Convert(['void f(int type, int record);']);
  AssertConverted;
  AssertInterface('Pascal reserved words get an underscore prefix',['procedure f(_type:longint; _record:longint);']);
end;


procedure TTestFunctions.TestPointerParams;

begin
  Convert(['void f(int *a, char *s, void *p);']);
  AssertConverted;
  AssertInterface('pointer parameters use P types',['procedure f(a:Plongint; s:Pansichar; p:pointer);']);
end;


procedure TTestFunctions.TestPointerToPointerParam;

begin
  Convert(['void f(char **argv);']);
  AssertConverted;
  AssertInterface('pointer to pointer parameter gives PP type',['procedure f(argv:PPansichar);']);
end;


procedure TTestFunctions.TestOpenArrayParam;

begin
  Convert(['void f(char *list[]);']);
  AssertConverted;
  AssertInterface('open array parameter becomes a pointer',['procedure f(list:PPansichar);']);
end;


procedure TTestFunctions.TestConstParam;

begin
  Convert(['void f(const char *s);']);
  AssertConverted;
  AssertOutput('const qualifier is reported as ignored',['(* Const before declarator ignored *)']);
  AssertInterface('const pointer parameter',['procedure f(s:Pansichar);']);
end;


procedure TTestFunctions.TestFunctionPointerParams;

begin
  Convert(['void f(int (*cb)(int x), void (*vcb)(void));']);
  AssertConverted;
  AssertInterface('function pointer parameters get named procedural types',
    ['type','f_cb = function (x:longint):longint;cdecl;','f_vcb = procedure ;cdecl;']);
  AssertInterface('parameters use the named types',['procedure f(cb:f_cb; vcb:f_vcb);']);
  AssertImplementation('stub uses the named types',['procedure f(cb:f_cb; vcb:f_vcb);']);
end;


procedure TTestFunctions.TestUnnamedFunctionPointerParam;

begin
  Convert(['void g(void (*)(void));']);
  AssertConverted;
  AssertInterface('unnamed function pointer parameter type',['g__para1 = procedure ;cdecl;']);
  AssertInterface('unnamed function pointer parameter',['procedure g(_para1:g__para1);']);
end;


procedure TTestFunctions.TestNestedFunctionPointerParam;

begin
  Convert(['int h(int (*cmp)(void (*deep)(void), int), char *s);']);
  AssertConverted;
  AssertInterface('function pointer parameter of a function pointer parameter is declared first',
    ['h_cmp_deep = procedure ;cdecl;','h_cmp = function (deep:h_cmp_deep; _para2:longint):longint;cdecl;']);
  AssertInterface('outer parameter',['function h(cmp:h_cmp; s:Pansichar):longint;']);
end;


procedure TTestFunctions.TestFunctionPointerParamCompiles;

begin
  Convert(['void f(void (*vcb)(void), int (*icb)(int a));',
           'int h(int (*cmp)(void (*deep)(void), int), char *s);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestEllipsis;

begin
  Convert(['int f(const char *fmt, ...);','void g(...);']);
  AssertConverted;
  AssertOutput('array of const needs objfpc mode',['{$mode objfpc}','unit output;']);
  AssertInterface('stub with array of const and overload without it',
    ['function f(fmt:Pansichar; args:array of const):longint;','function f(fmt:Pansichar):longint;']);
  AssertInterface('stub with only an ellipsis',['procedure g(args:array of const);','procedure g();']);
  AssertImplementation('stub with array of const',['function f(fmt:Pansichar; args:array of const):longint;']);
  AssertImplementation('stub without array of const',['function f(fmt:Pansichar):longint;']);
  AssertCompiles;
end;


procedure TTestFunctions.TestEllipsisExternal;

begin
  Convert(['int f(const char *fmt, ...);','void g(...);'],['-d']);
  AssertConverted;
  AssertInterface('external ellipsis function is varargs',['function f(fmt:Pansichar):longint;cdecl;varargs;external;']);
  AssertInterface('external function with only an ellipsis',['procedure g();cdecl;varargs;external;']);
  AssertNotOutput('external ellipsis function has no array of const','array of const');
  AssertNotOutput('external ellipsis function needs no objfpc mode','{$mode objfpc}');
  AssertCompiles;
end;


procedure TTestFunctions.TestEllipsisStdcall;

begin
  Convert(['int STDCALL f(const char *fmt, ...);'],['-w']);
  AssertConverted;
  AssertInterface('ellipsis function is cdecl despite STDCALL',
    ['function f(fmt:Pansichar):longint;cdecl;varargs;external External_library name ''f'';']);
end;


procedure TTestFunctions.TestEllipsisProcedureTypes;

begin
  Convert(['typedef int (*logfn)(const char *fmt, ...);','struct s { int (*pf)(const char *, ...); };',
           'extern int (*gvf)(const char *fmt, ...);','void setlog(int (*fn)(const char *, ...));'],['-d']);
  AssertConverted;
  AssertInterface('procedure type typedef is varargs',['logfn = function (fmt:Pansichar):longint;cdecl;varargs;']);
  AssertInterface('record field is varargs',['pf : function (_para1:Pansichar):longint;cdecl;varargs;']);
  AssertInterface('variable is varargs',['gvf : function (fmt:Pansichar):longint;cdecl;varargs;cvar;external;']);
  AssertInterface('parameter type is varargs',['setlog_fn = function (_para1:Pansichar):longint;cdecl;varargs;']);
  AssertCompiles;
end;


procedure TTestFunctions.TestEllipsisDynLib;

begin
  Convert(['int f(const char *fmt, ...);'],['-P']);
  AssertConverted;
  AssertEquals('-P declares one variable',1,CountOf('f : function'));
  AssertInterface('-P variable is varargs',['f : function(fmt:Pansichar):longint;cdecl;varargs;']);
  AssertCompiles;
end;


procedure TTestFunctions.TestFarNearPointers;

begin
  Convert(['void f(int far *a, int near *b, int huge *c);']);
  AssertConverted;
  AssertOutput('far is reported as ignored',['(* far ignored *)']);
  AssertOutput('near is reported as ignored',['(* near ignored *)']);
  AssertOutput('huge is reported as ignored',['(* huge ignored *)']);
  AssertInterface('far, near and huge pointers are plain pointers',['procedure f(a:Plongint; b:Plongint; c:Plongint);']);
end;


procedure TTestFunctions.TestParamListWraps;

begin
  Convert(['void f(int a, int b, int c, int d, int e, int f, int g);']);
  AssertConverted;
  AssertInterface('parameter list wraps after every fifth parameter',
    ['procedure f(a:longint; b:longint; c:longint; d:longint; e:longint;','f:longint; g:longint);']);
end;


procedure TTestFunctions.TestVoidPointerResult;

begin
  Convert(['void *alloc(int n);']);
  AssertConverted;
  AssertInterface('void pointer result',['function alloc(n:longint):pointer;']);
  AssertImplementation('void pointer result in the stub',['function alloc(n:longint):pointer;']);
end;


procedure TTestFunctions.TestPointerResultImplementation;

begin
  Convert(['char *name(void);','int **pp(void);']);
  AssertConverted;
  AssertImplementation('pointer result in the stub uses a P type',['function name:Pansichar;']);
  AssertImplementation('pointer to pointer result in the stub uses a PP type',['function pp:PPlongint;']);
end;


procedure TTestFunctions.TestPointerResultInterface;

begin
  Convert(['char *name(void);','int **pp(void);']);
  AssertConverted;
  AssertInterface('pointer result in the interface uses a P type',['function name:Pansichar;']);
  AssertInterface('pointer to pointer result in the interface uses a PP type',['function pp:PPlongint;']);
end;


procedure TTestFunctions.TestPointerResultUnitCompiles;

begin
  Convert(['typedef struct { int a; } rec;','rec *getrec(int i);','char *name(void);'],['-d']);
  AssertConverted;
  AssertInterface('pointer to a declared type as result',['function getrec(i:longint):Prec;cdecl;external;']);
  AssertCompiles;
end;


procedure TTestFunctions.TestStructPointerResult;

begin
  Convert(['struct s1 { int a; };','struct s1 *f(struct s1 *p);','union u1 *g(void);','enum e1 h(int a);'],['-d']);
  AssertConverted;
  AssertInterface('function returning a struct pointer',['function f(p:Ps1):Ps1;cdecl;external;']);
  AssertInterface('function returning a union pointer',['function g:Pu1;cdecl;external;']);
  AssertInterface('function returning an enum',['function h(a:longint):e1;cdecl;external;']);
end;


procedure TTestFunctions.TestStructPointerResultStub;

begin
  Convert(['struct s1 *f(struct s1 *p);']);
  AssertConverted;
  AssertImplementation('stub of a function returning a struct pointer',
    ['function f(p:Ps1):Ps1;','begin','{ You must implement this function }','end;']);
end;


procedure TTestFunctions.TestStructPointerResultCompiles;

begin
  Convert(['struct s1 { int a; };','struct s1 *f(struct s1 *p);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestExternFunction;

begin
  Convert(['extern int f(int a);']);
  AssertConverted;
  AssertInterface('extern function is cdecl',['function f(a:longint):longint;cdecl;']);
  AssertNotImplementation('extern function has no stub','function f');
end;


procedure TTestFunctions.TestUnionPointerParam;

begin
  Convert(['void f(union u1 *q);']);
  AssertConverted;
  AssertInterface('pointer to union parameter',['procedure f(q:Pu1);']);
end;


procedure TTestFunctions.TestEnumParam;

begin
  Convert(['void f(enum e1 e);']);
  AssertConverted;
  AssertInterface('enum parameter uses the enum name',['procedure f(e:e1);']);
end;


procedure TTestFunctions.TestPointerParamDeclaresPointerType;

begin
  Convert(['typedef struct { int a; } rec;','void f(rec *r);']);
  AssertConverted;
  AssertInterface('pointer type used by a parameter precedes its target record',
    ['type','Prec = ^rec;','rec = record','a : longint;','end;']);
  AssertNotOutput('pointer to a declared type is not in the header pointer list','Type');
  AssertInterface('parameter uses the pointer type',['procedure f(r:Prec);']);
end;


procedure TTestFunctions.TestPointerParamExternalType;

begin
  Convert(['void f(mytype *p);']);
  AssertConverted;
  AssertOutput('pointer to a type not declared in the header is in the header pointer list',
    ['Type','Pmytype = ^mytype;']);
  AssertInterface('parameter uses the pointer type',['procedure f(p:Pmytype);']);
end;


procedure TTestFunctions.TestPointerParamUnitCompiles;

begin
  Convert(['typedef struct { int a; } rec;','void f(rec *r);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestReferenceParams;

begin
  Convert(['typedef struct { int a; } rec;','void f(int &r, rec &s, const rec &c, int *&pr);']);
  AssertConverted;
  AssertInterface('C++ references become var parameters',
    ['procedure f(var r:longint; var s:rec; var c:rec; var pr:Plongint);']);
  AssertImplementation('stub with var parameters',['procedure f(var r:longint; var s:rec; var c:rec; var pr:Plongint);']);
end;


procedure TTestFunctions.TestUnnamedReferenceParam;

begin
  Convert(['void h(int &);']);
  AssertConverted;
  AssertInterface('unnamed reference parameter',['procedure h(var _para1:longint);']);
end;


procedure TTestFunctions.TestReferenceResult;

begin
  Convert(['int &g(int &a);'],['-d']);
  AssertConverted;
  AssertInterface('function returning a reference returns a pointer',['function g(var a:longint):Plongint;cdecl;external;']);
end;


procedure TTestFunctions.TestReferenceInProcedureType;

begin
  Convert(['typedef void (*cb)(int &x);','struct holder { int &ref; };']);
  AssertConverted;
  AssertInterface('reference parameter of a procedure type',['cb = procedure (var x:longint);cdecl;']);
  AssertInterface('reference field becomes a pointer',['ref : ^longint;']);
end;


procedure TTestFunctions.TestReferencesCompile;

begin
  Convert(['typedef struct { int a; } rec;','void f(int &r, rec &s, int *&pr);','int &g(int &a);','void h(int &);',
           'typedef void (*cb)(int &x);','struct holder { int &ref; };'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestQualifiersIgnored;

begin
  Convert(['extern volatile int v;','extern const volatile unsigned long * __restrict p;',
           'void f(char * restrict a, const char *__restrict b, volatile int *c);'],['-d']);
  AssertConverted;
  AssertInterface('volatile is ignored',['v : longint;cvar;external;']);
  AssertInterface('__restrict is ignored',['p : ^dword;cvar;external;']);
  AssertInterface('restrict in parameters is ignored',['procedure f(a:Pansichar; b:Pansichar; c:Plongint);cdecl;external;']);
end;


procedure TTestFunctions.TestAttributesIgnored;

begin
  Convert(['int f(int a) __attribute__((deprecated, nonnull(1)));',
           'extern int __attribute__((visibility("default"))) g(void);',
           '__declspec(dllimport) int h(void);','int k(void) __asm__("k64");'],['-d']);
  AssertConverted;
  AssertInterface('__attribute__ after the declaration',['function f(a:longint):longint;cdecl;external;']);
  AssertInterface('__attribute__ before the name',['function g:longint;cdecl;external;']);
  AssertInterface('__declspec',['function h:longint;cdecl;external;']);
  AssertInterface('__asm__',['function k:longint;cdecl;external;']);
end;


procedure TTestFunctions.TestMicrosoftCallingConventions;

begin
  Convert(['int __stdcall ws(int a);','int __cdecl cs(int a);'],['-d']);
  AssertConverted;
  AssertInterface('__stdcall is stdcall without -w',['function ws(a:longint):longint;stdcall;external;']);
  AssertInterface('__cdecl is cdecl',['function cs(a:longint):longint;cdecl;external;']);
end;


procedure TTestFunctions.TestStaticInlineFunction;

begin
  Convert(['static inline int twice(int a) { return a * 2; }','static __inline__ void nothing(void) { return; }']);
  AssertConverted;
  AssertInterface('static inline function',['function twice(a:longint):longint;']);
  AssertImplementation('return with a value',['function twice(a:longint):longint;','begin','exit(a*2);','end;']);
  AssertImplementation('return without a value',['procedure nothing;','begin','exit;','end;']);
end;


procedure TTestFunctions.TestStaticInlineNotExternal;

begin
  Convert(['static inline int twice(int a) { return a * 2; }'],['-D','-l','mylib']);
  AssertConverted;
  AssertInterface('function with a body is not external',['function twice(a:longint):longint;']);
  AssertNotOutput('function with a body is not imported','external External_library name ''twice''');
  AssertImplementation('function with a body is implemented',['exit(a*2);']);
end;


procedure TTestFunctions.TestStaticInlineCompiles;

begin
  Convert(['static inline int twice(int a) { return a * 2; }','static inline void nothing(void) { return; }',
           'int imported(int a);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestPointerResultReturnsNil;

begin
  Convert(['static char *name(int a) { return NULL; }','static int *p(void) { return (0); }',
           'static void (*pick(void))(int) { return 0; }','static int zero(void) { return 0; }']);
  AssertConverted;
  AssertImplementation('NULL result of a pointer function',['function name(a:longint):Pansichar;','begin','exit(nil);','end;']);
  AssertImplementation('0 result of a pointer function',['function p:Plongint;','begin','exit(nil);','end;']);
  AssertImplementation('0 result of a function pointer function',['function pick:pick_result;','begin','exit(nil);','end;']);
  AssertImplementation('0 result of an integer function',['function zero:longint;','begin','exit(0);','end;']);
end;


procedure TTestFunctions.TestPointerResultReturnsNilCompiles;

begin
  Convert(['static char *name(int a) { return NULL; }','static int *p(void) { return (0); }',
           'static void (*pick(void))(int) { return 0; }','static void **pp(void) { return 0; }'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestArrayParams;

begin
  Convert(['struct s { int a; };','void f(struct s arr[4], int m[3][4], char *names[8], int n[]);'],['-d']);
  AssertConverted;
  AssertInterface('array parameters are pointers',['procedure f(arr:Ps; m:pointer; names:PPansichar; n:Plongint);cdecl;external;']);
end;


procedure TTestFunctions.TestArrayParamsCompile;

begin
  Convert(['struct s { int a; };','void f(struct s arr[4], int m[3][4], char *names[8]);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestFunctionPointerResult;

begin
  Convert(['void (*signal(int sig, void (*func)(int)))(int);','int (*getop(char c))(int a, int b);']);
  AssertConverted;
  AssertInterface('function pointer result gets a named procedural type',
    ['type','signal_func = procedure (_para1:longint);cdecl;','signal_result = procedure (_para1:longint);cdecl;']);
  AssertInterface('function uses the result type',['function signal(sig:longint; func:signal_func):signal_result;']);
  AssertInterface('function result type of a function pointer',
    ['getop_result = function (a:longint; b:longint):longint;cdecl;','function getop(c:ansichar):getop_result;']);
end;


procedure TTestFunctions.TestFunctionPointerResultWithFunctionPointerArg;

begin
  Convert(['void (*reg(void))(int (*cb)(void));']);
  AssertConverted;
  AssertInterface('function pointer argument of the result type is declared first',
    ['reg_result_cb = function :longint;cdecl;','reg_result = procedure (cb:reg_result_cb);cdecl;','function reg:reg_result;']);
end;


procedure TTestFunctions.TestFunctionPointerResultCompiles;

begin
  Convert(['void (*signal(int sig, void (*func)(int)))(int);','int (*getop(char c))(int a, int b);',
           'void (*reg(void))(int (*cb)(void));'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestFunctionPointerArrayParam;

begin
  Convert(['void f(void (*cbs[4])(int));']);
  AssertConverted;
  AssertInterface('array of function pointers parameter is a pointer to a named element type',
    ['f_cbs = procedure (_para1:longint);cdecl;','Pf_cbs = ^f_cbs;','procedure f(cbs:Pf_cbs);']);
end;


procedure TTestFunctions.TestFunctionPointerArrayParamCompiles;

begin
  Convert(['void f(void (*cbs[4])(int));','void g(int (*ops[])(int a));'],['-d','-T']);
  AssertConverted;
  AssertInterface('element type under -T',['Tf_cbs = procedure (para1:longint);cdecl;','Pf_cbs = ^Tf_cbs;']);
  AssertInterface('open array of function pointers parameter',['Tg_ops = function (a:longint):longint;cdecl;','Pg_ops = ^Tg_ops;','procedure g(ops:Pg_ops);']);
  AssertCompiles;
end;


procedure TTestFunctions.TestStaticPrototypeIgnored;

begin
  Convert(['static int helper(int a);','int imported(int a);'],['-d']);
  AssertConverted;
  AssertOutput('static prototype is reported as ignored',['(* static function helper ignored *)']);
  AssertNotOutput('static prototype is not declared','function helper(');
  AssertInterface('other prototypes are declared',['function imported(a:longint):longint;cdecl;external;']);
end;


procedure TTestFunctions.TestStaticPrototypeNotImported;

begin
  Convert(['static int helper(int a);','int imported(int a);'],['-D','-l','mylib']);
  AssertConverted;
  AssertNotOutput('static prototype is not imported','name ''helper''');
  AssertInterface('other prototypes are imported',
    ['function imported(a:longint):longint;cdecl;external External_library name ''imported'';']);
  AssertCompiles;
end;


procedure TTestFunctions.TestExternFunctionBody;

begin
  Convert(['extern int twice(int a) { return a * 2; }','extern inline void nothing(void) { return; }'],['-d']);
  AssertConverted;
  AssertInterface('extern function with a body is not external',['function twice(a:longint):longint;','procedure nothing;']);
  AssertNotOutput('extern function with a body has no external directive','cdecl;external');
  AssertImplementation('extern function with a body is implemented',
    ['function twice(a:longint):longint;','begin','exit(a*2);','end;','procedure nothing;','begin','exit;','end;']);
end;


procedure TTestFunctions.TestExternFunctionBodyNotImported;

begin
  Convert(['extern int twice(int a) { return a * 2; }'],['-D','-l','mylib']);
  AssertConverted;
  AssertNotOutput('extern function with a body is not imported','name ''twice''');
  AssertImplementation('extern function with a body is implemented',['exit(a*2);']);
end;


procedure TTestFunctions.TestExternFunctionBodyCompiles;

begin
  Convert(['extern int twice(int a) { return a * 2; }','extern inline void nothing(void) { return; }'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestFunctionBody;

begin
  Convert(['int f(int a) { a; }']);
  AssertConverted;
  AssertInterface('function with body is declared',['function f(a:longint):longint;']);
  AssertImplementation('body statements are copied',['function f(a:longint):longint;','begin','a;','end;']);
end;


procedure TTestFunctions.TestWhileStatement;

begin
  Convert(['int f(int a) { while (a) a; }']);
  AssertConverted;
  AssertImplementation('while statement',['begin','while a do','begin','a;','end;','end;']);
end;


procedure TTestVariables.TestExternVariable;

begin
  Convert(['extern int ev;']);
  AssertConverted;
  AssertInterface('extern variable is an external cvar',['var','ev : longint;cvar;external;']);
end;


procedure TTestVariables.TestPublicVariable;

begin
  Convert(['int iv;']);
  AssertConverted;
  AssertInterface('variable definition is a public cvar',['var','iv : longint;cvar;public;']);
end;


procedure TTestVariables.TestArrayVariable;

begin
  Convert(['extern int buf[16];']);
  AssertConverted;
  AssertInterface('array variable',['buf : array[0..15] of longint;cvar;external;']);
end;


procedure TTestVariables.TestPointerVariable;

begin
  Convert(['extern int *pv;']);
  AssertConverted;
  AssertInterface('pointer variable',['pv : ^longint;cvar;external;']);
end;


procedure TTestVariables.TestUnknownTypeVariable;

begin
  Convert(['double d;']);
  AssertConverted;
  AssertInterface('unknown type names are copied',['d : double;cvar;public;']);
end;


procedure TTestVariables.TestMultipleVariables;

begin
  Convert(['int iv, jv;']);
  AssertConverted;
  AssertInterface('each declarator becomes a variable',['var','iv : longint;cvar;public;','jv : longint;cvar;public;']);
end;


procedure TTestVariables.TestMultipleExternVariables;

begin
  Convert(['extern int a, *b, c[4];'],['-d']);
  AssertConverted;
  AssertInterface('plain, pointer and array declarators',
    ['a : longint;cvar;external;','b : ^longint;cvar;external;','c : array[0..3] of longint;cvar;external;']);
  AssertCompiles;
end;


procedure TTestVariables.TestProcedureTypeVariable;

begin
  Convert(['extern void (*gv)(void);','extern int (*gf)(int a);'],['-d']);
  AssertConverted;
  AssertInterface('procedure variable is cdecl',['gv : procedure ;cdecl;cvar;external;']);
  AssertInterface('function variable is cdecl',['gf : function (a:longint):longint;cdecl;cvar;external;']);
  AssertCompiles;
end;


procedure TTestVariables.TestFunctionPointerArrayVariable;

begin
  Convert(['extern void (*handlers[4])(int);','int (*ops[2][3])(int a), plain;']);
  AssertConverted;
  AssertInterface('array of function pointers variable gets a named element type',
    ['handlers_element = procedure (_para1:longint);cdecl;','var','handlers : array[0..3] of handlers_element;cvar;external;']);
  AssertInterface('two-dimensional array of function pointers',
    ['ops_element = function (a:longint):longint;cdecl;','var','ops : array[0..1] of array[0..2] of ops_element;cvar;public;',
     'plain : longint;cvar;public;']);
end;


procedure TTestVariables.TestFunctionPointerArrayVariablesCompile;

begin
  Convert(['extern void (*handlers[4])(int);','int (*ops[2][3])(int a), plain;'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestVariables.TestPointerToFunctionPointer;

begin
  Convert(['extern void (**pp)(int);','void h(void (**out)(int));','struct s { int (**tab)(void); };',
           'typedef void (**ppf)(int);'],['-d']);
  AssertConverted;
  AssertInterface('pointer to function pointer variable',
    ['pp_element = procedure (_para1:longint);cdecl;','var','pp : ^pp_element;cvar;external;']);
  AssertInterface('pointer to function pointer parameter',['h_out = procedure (_para1:longint);cdecl;','Ph_out = ^h_out;',
    'procedure h(_out:Ph_out);']);
  AssertInterface('pointer to function pointer member',['s_tab = function :longint;cdecl;','s = record','tab : ^s_tab;']);
  AssertInterface('pointer to function pointer typedef',['ppf_element = procedure (_para1:longint);cdecl;','ppf = ^ppf_element;']);
  AssertCompiles;
end;


procedure TTestVariables.TestAnonymousStructVariable;

begin
  Convert(['struct { int a; } v;']);
  AssertConverted;
  AssertInterface('variable of an anonymous struct',['var','v : record','a : longint;','end;cvar;public;']);
end;


procedure TTestVariables.TestAnonymousUnionAndEnumVariables;

begin
  Convert(['union { int i; float f; } u;','enum { E1, E2 } e;']);
  AssertConverted;
  AssertInterface('variable of an anonymous union',['u : record','case longint of']);
  AssertInterface('variable of an anonymous enum',['e : (E1,E2);cvar;public;']);
end;


procedure TTestVariables.TestAnonymousVariablesCompile;

begin
  Convert(['struct { int a; } v;','union { int i; float f; } u;','enum { E1, E2 } e;'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestVariables.TestStaticVariable;

begin
  Convert(['static int counter;','static const char *names[4];','int pub;']);
  AssertConverted;
  AssertInterface('static variable is a plain variable',['var','counter : longint;']);
  AssertNotOutput('static variable is not public','counter : longint;cvar');
  AssertInterface('static array variable',['names : array[0..3] of ^ansichar;']);
  AssertInterface('other variables stay public',['pub : longint;cvar;public;']);
end;


procedure TTestVariables.TestStaticVariablesCompile;

begin
  Convert(['static int counter;','static const char *names[4];','int pub;'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestFunctions.TestFunctionPointerArgumentResult;

begin
  Convert(['void reg(int (*(*cb)(int))(char *s), int n);'],['-d']);
  AssertConverted;
  AssertInterface('the function pointer result of a function pointer argument is a named type',
    ['reg_cb_result = function (s:Pansichar):longint;cdecl;','reg_cb = function (_para1:longint):reg_cb_result;cdecl;']);
  AssertInterface('the argument uses the named type',['procedure reg(cb:reg_cb; n:longint);cdecl;external;']);
end;


procedure TTestFunctions.TestNestedFunctionPointerResult;

begin
  Convert(['void (*(*getter(int k))(int))(void);'],['-d']);
  AssertConverted;
  AssertInterface('the result of a function pointer result is a named type',
    ['getter_result_result = procedure ;cdecl;','getter_result = function (_para1:longint):getter_result_result;cdecl;']);
  AssertInterface('the function uses the named result',['function getter(k:longint):getter_result;cdecl;external;']);
end;


procedure TTestVariables.TestFunctionPointerVariableResult;

begin
  Convert(['extern int (*(*gfi)(int))(char *s);'],['-d']);
  AssertConverted;
  AssertInterface('the function pointer result of a function pointer variable is a named type',
    ['gfi_result = function (s:Pansichar):longint;cdecl;']);
  AssertInterface('the variable uses the named type',['gfi : function (_para1:longint):gfi_result;cdecl;cvar;external;']);
end;


procedure TTestVariables.TestFunctionPointerVariableArgument;

begin
  Convert(['extern void (*gcb)(void (*f)(int));'],['-d']);
  AssertConverted;
  AssertInterface('a function pointer argument of a function pointer variable is a named type',
    ['gcb_f = procedure (_para1:longint);cdecl;']);
  AssertInterface('the variable uses the named type',['gcb : procedure (f:gcb_f);cdecl;cvar;external;']);
end;


procedure TTestVariables.TestFunctionPointerResultsCompile;

const
  Header : array[0..5] of string = (
    'extern void (*(*gfp)(int))(void);',
    'extern int (*(*gfi)(int))(char *s);',
    'extern void (*gcb)(void (*f)(int));',
    'typedef int (*(*fi_t)(int))(char *s);',
    'void reg(int (*(*cb)(int))(char *s), int n);',
    'void (*(*getter(int k))(int))(void);');

begin
  Convert(Header,['-d']);
  AssertConverted;
  AssertCompiles;
  Convert(Header,['-d','-1']);
  AssertConverted;
  AssertCompiles;
  Convert(Header,['-P','-l','libx.so']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestVariables.TestPointerToArrayVariable;

begin
  Convert(['extern int (*grid)[16];'],['-d']);
  AssertConverted;
  AssertInterface('the array a variable points to is a named type',['grid_array = array[0..15] of longint;']);
  AssertInterface('the variable points to it',['grid : ^grid_array;cvar;external;']);
end;


procedure TTestVariables.TestPointerToArrayArgument;

begin
  Convert(['void fill(int (*m)[3], int n);'],['-d']);
  AssertConverted;
  AssertInterface('the array an argument points to is a named type',['fill_m = array[0..2] of longint;']);
  AssertInterface('the argument is a pointer to it',['procedure fill(m:Pfill_m; n:longint);cdecl;external;']);
end;


procedure TTestVariables.TestVariableNameOfDefineInOtherCase;

begin
  Convert(['#define MAD_AUTHOR "Underbit"','extern char const mad_author[];','char mad_title[8];'],['-d']);
  AssertConverted;
  AssertInterface('the define keeps its name',['MAD_AUTHOR = ''Underbit'';']);
  AssertInterface('an external variable with an underscore imports its C name',
    ['mad_author_ : ^ansichar;external name ''mad_author'';']);
end;


procedure TTestVariables.TestDefineNameOfVariableInOtherCase;

begin
  Convert(['int mad_count;','#define MAD_COUNT 3'],['-d']);
  AssertConverted;
  AssertInterface('the variable keeps its name',['mad_count : longint;cvar;public;']);
  AssertOutput('the define is left out',['(* #define MAD_COUNT ignored, the Pascal name of mad_count *)']);
  AssertNotOutput('no constant','MAD_COUNT = 3');
end;


procedure TTestVariables.TestVariableNameOfTypeInOtherCase;

begin
  Convert(['typedef struct { int x; } FUNMAP;','extern FUNMAP **funmap;','int keycount;','int KeyCount;'],['-d']);
  AssertConverted;
  AssertInterface('a variable with the name of a type in other case has an underscore',
    ['funmap_ : ^PFUNMAP;external name ''funmap'';']);
  AssertInterface('a public variable with an underscore exports its C name',
    ['keycount : longint;cvar;public;','KeyCount_ : longint;public name ''KeyCount'';']);
end;


procedure TTestVariables.TestEnumMemberNameOfDefineInOtherCase;

begin
  Convert(['#define XKB_KEY_Up 0xff52','enum xkb_key_direction { XKB_KEY_UP, XKB_KEY_DOWN = XKB_KEY_UP + 1 };'],['-d']);
  AssertConverted;
  AssertInterface('an enum member with the name of a define in other case has an underscore',
    ['xkb_key_direction = (XKB_KEY_UP_,XKB_KEY_DOWN := ord(XKB_KEY_UP_)+1);']);
end;


procedure TTestVariables.TestNamesDifferingInCaseCompile;

begin
  Convert(['#define MAD_AUTHOR "Underbit"','extern char const mad_author[];','int mad_count;','#define MAD_COUNT 3',
           'int rl_vi_fWord(int a, int b);','int rl_vi_fword(int a, int b);','#define CALLW(a) rl_vi_fword(a, 0)',
           'typedef struct { int x; } FUNMAP;','extern FUNMAP **funmap;','char pubname;','int PUBNAME(void);',
           '#define XKB_KEY_Up 0xff52','enum xkb_key_direction { XKB_KEY_UP, XKB_KEY_DOWN = XKB_KEY_UP + 1 };'],['-d']);
  AssertConverted;
  AssertInterface('a public variable before a function of that name in other case',['pubname : ansichar;cvar;public;']);
  AssertCompiles;
  Convert(['int rl_vi_fWord(int a, int b);','int rl_vi_fword(int a, int b);'],['-P','-l','libx.so']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestVariables.TestPointersAfterFunctionResultOfFunctionPointer;

begin
  Convert(['extern int (*(*gfp)(int))(char *s);','struct after { int *p; };','extern int *gp;'],['-d']);
  AssertConverted;
  AssertInterface('a member pointer after a function pointer result is written with ^',['p : ^longint;']);
  AssertInterface('a variable pointer after a function pointer result is written with ^',['gp : ^longint;cvar;external;']);
end;


initialization
  RegisterTests('H2Pas',[TTestFunctions,TTestVariables]);
end.
