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
    procedure TestEllipsis;
    procedure TestEllipsisExternal;
    procedure TestFarNearPointers;
    procedure TestParamListWraps;
    procedure TestVoidPointerResult;
    procedure TestPointerResultImplementation;
    procedure TestExternFunction;
    procedure TestUnionPointerParam;
    procedure TestEnumParam;
    procedure TestPointerParamDeclaresPointerType;
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
  AssertInterface('function pointer parameters become procedural types',
    ['procedure f(cb:function (x:longint):longint; vcb:procedure );']);
end;


procedure TTestFunctions.TestEllipsis;

begin
  Convert(['int f(const char *fmt, ...);']);
  AssertConverted;
  AssertInterface('ellipsis becomes array of const',['function f(fmt:Pansichar; args:array of const):longint;']);
end;


procedure TTestFunctions.TestEllipsisExternal;

begin
  Convert(['int f(const char *fmt, ...);'],['-d']);
  AssertConverted;
  AssertInterface('external ellipsis function with array of const',
    ['function f(fmt:Pansichar; args:array of const):longint;cdecl;external;']);
  AssertInterface('external ellipsis function overload without the variable arguments',
    ['function f(fmt:Pansichar):longint;cdecl;external;']);
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
  AssertOutput('pointer type used by a parameter is declared in the header',['Type','Prec = ^rec;']);
  AssertInterface('parameter uses the pointer type',['procedure f(r:Prec);']);
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


initialization
  RegisterTests('H2Pas',[TTestFunctions,TTestVariables]);
end.
