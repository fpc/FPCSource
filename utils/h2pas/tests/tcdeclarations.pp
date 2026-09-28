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
  AssertInterface('pointer type used by a parameter follows the declaration of its target',
    ['type','rec = record','a : longint;','end;','Prec = ^rec;']);
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


initialization
  RegisterTests('H2Pas',[TTestFunctions,TTestVariables]);
end.
