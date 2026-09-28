{
  h2pas test suite: typedefs and enumerations.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcTypedefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestTypedefs }

  TTestTypedefs = class(TH2PasTestCase)
  published
    procedure TestSimpleTypedef;
    procedure TestFunctionPointerArrayTypedef;
    procedure TestFunctionPointerArrayTypedefCompiles;
    procedure TestNamedTypeTypedef;
    procedure TestAnonymousStruct;
    procedure TestTaggedStruct;
    procedure TestPointerAlias;
    procedure TestTaggedStructPointerTypedef;
    procedure TestUntaggedStructPointerTypedef;
    procedure TestEnumPointerTypedef;
    procedure TestInlineTypePointerTypedefsCompile;
    procedure TestStructTagAlias;
    procedure TestStructTagAliasPointer;
    procedure TestStructTagAliasPrefix;
    procedure TestAnonymousUnion;
    procedure TestCharPointer;
    procedure TestArray;
    procedure TestOpenArray;
    procedure TestFunctionPointer;
    procedure TestProcedurePointer;
    procedure TestVoidArgProcedurePointer;
    procedure TestVoidArgFunctionPointer;
    procedure TestVoidPointerArg;
    procedure TestFunctionPointerArg;
    procedure TestFunctionType;
    procedure TestFunctionTypeWithoutParentheses;
    procedure TestFunctionTypeUse;
    procedure TestFunctionTypePrefix;
    procedure TestGenericTypedef;
    procedure TestTypedefsShareTypeBlock;
  end;

  { TTestEnums }

  TTestEnums = class(TH2PasTestCase)
  published
    procedure TestAnonymousEnum;
    procedure TestTaggedEnum;
    procedure TestNamedEnum;
    procedure TestEnumValues;
    procedure TestEnumExpressionValue;
    procedure TestEnumMemberNotPrefixed;
    procedure TestReservedWordEnumMembers;
    procedure TestReservedWordEnumConstants;
    procedure TestReservedWordEnumMembersCompile;
    procedure TestEnumValueUsesMember;
    procedure TestEnumValueUsesMemberCompiles;
    procedure TestEnumConstantUsesMember;
  end;

implementation


procedure TTestTypedefs.TestSimpleTypedef;

begin
  Convert(['typedef int myint;']);
  AssertConverted;
  AssertInterface('typedef of a base type',['type','myint = longint;']);
end;


procedure TTestTypedefs.TestFunctionPointerArrayTypedef;

begin
  Convert(['typedef void (*tbl[4])(int);']);
  AssertConverted;
  AssertInterface('array of function pointers gets a named element type',
    ['tbl_element = procedure (_para1:longint);cdecl;','tbl = array[0..3] of tbl_element;']);
end;


procedure TTestTypedefs.TestFunctionPointerArrayTypedefCompiles;

begin
  Convert(['typedef void (*tbl[4])(int);','typedef int (*ops[2][3])(int a);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestTypedefs.TestNamedTypeTypedef;

begin
  Convert(['typedef foo bar;']);
  AssertConverted;
  AssertInterface('typedef of a type name',['bar = foo;']);
end;


procedure TTestTypedefs.TestAnonymousStruct;

begin
  Convert(['typedef struct { int x; } anon_t;']);
  AssertConverted;
  AssertInterface('anonymous struct takes the typedef name',['anon_t = record','x : longint;','end;']);
end;


procedure TTestTypedefs.TestTaggedStruct;

begin
  Convert(['typedef struct tag3 { int x; } t3;']);
  AssertConverted;
  AssertInterface('tagged struct is declared under its tag, the typedef is an alias',
    ['tag3 = record','x : longint;','end;','t3 = tag3;']);
end;


procedure TTestTypedefs.TestPointerAlias;

begin
  Convert(['typedef struct { int x; } anon_t, *panon_t;']);
  AssertConverted;
  AssertInterface('second declarator is a pointer alias',['anon_t = record','x : longint;','end;','panon_t = ^anon_t;']);
end;


procedure TTestTypedefs.TestTaggedStructPointerTypedef;

begin
  Convert(['typedef union u { int i; } *pu;','typedef struct s { int a; } *ps, sarr[2];']);
  AssertConverted;
  AssertInterface('tagged union is declared under its tag',
    ['u = record','case longint of','0 : ( i : longint );','end;','pu = ^u;']);
  AssertInterface('tagged struct with pointer and array declarators',
    ['s = record','a : longint;','end;','ps = ^s;','sarr = array[0..1] of s;']);
end;


procedure TTestTypedefs.TestUntaggedStructPointerTypedef;

begin
  Convert(['typedef struct { int a; } *pt;']);
  AssertConverted;
  AssertInterface('untagged struct of a pointer typedef gets a record name',
    ['pt_record = record','a : longint;','end;','pt = ^pt_record;']);
end;


procedure TTestTypedefs.TestEnumPointerTypedef;

begin
  Convert(['typedef enum e { A, B } *pe;','typedef enum { C, D } *pf;']);
  AssertConverted;
  AssertInterface('tagged enum of a pointer typedef',['e = (A,B);','pe = ^e;']);
  AssertInterface('untagged enum of a pointer typedef gets an enum name',['pf_enum = (C,D);','pf = ^pf_enum;']);
end;


procedure TTestTypedefs.TestInlineTypePointerTypedefsCompile;

begin
  Convert(['typedef union u { int i; } *pu;','typedef struct s { int a; } *ps, sarr[2];',
           'typedef struct { int a; } *pt;','typedef enum e { A, B } *pe;'],['-d','-T','-p']);
  AssertConverted;
  AssertInterface('pointer typedef under -T -p',['Ppt_record = ^Tpt_record;','Tpt_record = record','a : longint;','end;','Tpt = Ppt_record;']);
  AssertCompiles;
end;


procedure TTestTypedefs.TestStructTagAlias;

begin
  Convert(['typedef struct tag4 t4;']);
  AssertConverted;
  AssertInterface('typedef name is the alias of the tag',['type','t4 = tag4;']);
end;


procedure TTestTypedefs.TestStructTagAliasPointer;

begin
  Convert(['struct tag4 { int a; };','typedef struct tag4 t4;','void f(t4 *p);'],['-d']);
  AssertConverted;
  AssertInterface('pointer to the alias follows the alias',['t4 = tag4;','Pt4 = ^t4;']);
  AssertInterface('parameter uses the pointer to the alias',['procedure f(p:Pt4);']);
  AssertCompiles;
end;


procedure TTestTypedefs.TestStructTagAliasPrefix;

begin
  Convert(['typedef struct _tag4 t4;'],['-T']);
  AssertConverted;
  AssertInterface('-T alias of a tag',['Tt4 = Ttag4;']);
end;


procedure TTestTypedefs.TestAnonymousUnion;

begin
  Convert(['typedef union { int a; long b; } uu_t;']);
  AssertConverted;
  AssertInterface('anonymous union typedef',
    ['uu_t = record','case longint of','0 : ( a : longint );','1 : ( b : longint );','end;']);
end;


procedure TTestTypedefs.TestCharPointer;

begin
  Convert(['typedef char *string_t;']);
  AssertConverted;
  AssertInterface('pointer typedef',['string_t = ^ansichar;']);
end;


procedure TTestTypedefs.TestArray;

begin
  Convert(['typedef int vec3[3];']);
  AssertConverted;
  AssertInterface('array typedef',['vec3 = array[0..2] of longint;']);
end;


procedure TTestTypedefs.TestOpenArray;

begin
  Convert(['typedef XrmHashTable XrmSearchList[];']);
  AssertConverted;
  AssertInterface('open array typedef becomes a pointer',['XrmSearchList = ^XrmHashTable;']);
end;


procedure TTestTypedefs.TestFunctionPointer;

begin
  Convert(['typedef int (*binop)(int a, int b);']);
  AssertConverted;
  AssertInterface('function pointer typedef',['binop = function (a:longint; b:longint):longint;cdecl;']);
end;


procedure TTestTypedefs.TestProcedurePointer;

begin
  Convert(['typedef void (*cb)();']);
  AssertConverted;
  AssertInterface('procedure pointer typedef without arguments',['cb = procedure ;cdecl;']);
end;


procedure TTestTypedefs.TestVoidArgProcedurePointer;

begin
  Convert(['typedef void (*cb)(void);']);
  AssertConverted;
  AssertInterface('(void) gives a procedure type without parameters',['cb = procedure ;cdecl;']);
end;


procedure TTestTypedefs.TestVoidArgFunctionPointer;

begin
  Convert(['typedef int (*cb)(void);']);
  AssertConverted;
  AssertInterface('(void) gives a function type without parameters',['cb = function :longint;cdecl;']);
end;


procedure TTestTypedefs.TestVoidPointerArg;

begin
  Convert(['typedef void (*cb1)(void *);','typedef void (*cb2)(void *p);'],['-d']);
  AssertConverted;
  AssertInterface('unnamed void pointer argument is kept',['cb1 = procedure (_para1:pointer);cdecl;']);
  AssertInterface('named void pointer argument is kept',['cb2 = procedure (p:pointer);cdecl;']);
  AssertCompiles;
end;


procedure TTestTypedefs.TestFunctionPointerArg;

begin
  Convert(['typedef void (*cb)(void (*inner)(int x));'],['-d']);
  AssertConverted;
  AssertInterface('function pointer argument gets a named type before the typedef',
    ['cb_inner = procedure (x:longint);cdecl;','cb = procedure (inner:cb_inner);cdecl;']);
  AssertCompiles;
end;


procedure TTestTypedefs.TestFunctionType;

begin
  Convert(['typedef int (func_t)(int);','typedef void (vfunc_t)(void);']);
  AssertConverted;
  AssertInterface('function type becomes a procedural type',['func_t = function (_para1:longint):longint;cdecl;']);
  AssertInterface('procedure type becomes a procedural type',['vfunc_t = procedure ;cdecl;']);
end;


procedure TTestTypedefs.TestFunctionTypeWithoutParentheses;

begin
  Convert(['typedef int func2_t(int a, int b);']);
  AssertConverted;
  AssertInterface('function type without parentheses',['func2_t = function (a:longint; b:longint):longint;cdecl;']);
end;


procedure TTestTypedefs.TestFunctionTypeUse;

begin
  Convert(['typedef int (func_t)(int);','typedef int func2_t(int a, int b);',
           'void usef(func_t *f, func_t g, func2_t *w);','extern func_t *fp;','struct cb { func_t *handler; };'],['-d']);
  AssertConverted;
  AssertInterface('pointer to a function type is the procedural type',['procedure usef(f:func_t; g:func_t; w:func2_t);cdecl;external;']);
  AssertInterface('variable of a pointer to a function type',['fp : func_t;cvar;external;']);
  AssertInterface('field of a pointer to a function type',['handler : func_t;']);
  AssertNotOutput('no pointer type to a function type','Pfunc_t');
  AssertCompiles;
end;


procedure TTestTypedefs.TestFunctionTypePrefix;

begin
  Convert(['typedef int (func_t)(int);','typedef int func2_t(int a, int b);',
           'void usef(func_t *f, func2_t *w);','struct cb { func_t *handler; };'],['-d','-p','-T']);
  AssertConverted;
  AssertInterface('-p -T function type',['Tfunc_t = function (para1:longint):longint;cdecl;']);
  AssertInterface('-p -T pointer to a function type',['procedure usef(f:Tfunc_t; w:Tfunc2_t);cdecl;external;']);
  AssertInterface('-p -T field of a pointer to a function type',['handler : Tfunc_t;']);
  AssertNotOutput('-p -T declares no pointer to a function type','Pfunc2_t');
  AssertCompiles;
end;


procedure TTestTypedefs.TestGenericTypedef;

begin
  Convert(['typedef unknowntype;']);
  AssertConverted;
  AssertInterface('typedef without a type becomes a pointer',['(* generic typedef *)','unknowntype = pointer;']);
end;


procedure TTestTypedefs.TestTypedefsShareTypeBlock;

begin
  Convert(['typedef int a_t;','typedef long b_t;']);
  AssertConverted;
  AssertEquals('consecutive typedefs share one type block',1,CountOf('type'));
end;


procedure TTestEnums.TestAnonymousEnum;

begin
  Convert(['typedef enum { red, green, blue } color;']);
  AssertConverted;
  AssertInterface('anonymous enum takes the typedef name',['color = (red,green,blue);']);
end;


procedure TTestEnums.TestTaggedEnum;

begin
  Convert(['typedef enum e5 { A5, B5 } e5_t;']);
  AssertConverted;
  AssertInterface('tagged enum is declared under its tag, the typedef is an alias',['e5 = (A5,B5);','e5_t = e5;']);
end;


procedure TTestEnums.TestNamedEnum;

begin
  Convert(['enum e6 { A6, B6 };']);
  AssertConverted;
  AssertInterface('named enum declaration',['type','e6 = (A6,B6);']);
end;


procedure TTestEnums.TestEnumValues;

begin
  Convert(['enum e6 { A6 = 1, B6, C6 = 10, D6 };']);
  AssertConverted;
  AssertInterface('explicit enum values',['e6 = (A6 := 1,B6,C6 := 10,D6);']);
end;


procedure TTestEnums.TestEnumExpressionValue;

begin
  Convert(['enum e7 { A7 = X, B7 };']);
  AssertConverted;
  AssertInterface('enum value given by an identifier',['e7 = (A7 := X,B7);']);
end;


procedure TTestEnums.TestEnumMemberNotPrefixed;

begin
  Convert(['typedef enum { red, green } color;'],['-T']);
  AssertConverted;
  AssertInterface('enum members keep their names under -T',['Tcolor = (red,green);']);
end;


procedure TTestEnums.TestReservedWordEnumMembers;

begin
  Convert(['enum e { nil, uses, other };']);
  AssertConverted;
  AssertInterface('reserved word enum members get an underscore prefix',['e = (_nil,_uses,other);']);
end;


procedure TTestEnums.TestReservedWordEnumConstants;

begin
  Convert(['enum e { nil, uses = nil + 1 };','#define X (uses)'],['-e']);
  AssertConverted;
  AssertInterface('reserved word enum constants get an underscore prefix',['_nil = 0;','_uses = _nil+1;']);
  AssertInterface('references to the constants use the prefix',['X = _uses;']);
end;


procedure TTestEnums.TestReservedWordEnumMembersCompile;

begin
  Convert(['enum e { nil, uses, other };','struct s { int in; int end; int of; int set; int file; int xor; };',
           'void f(int begin, int with, int then);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestEnums.TestEnumValueUsesMember;

begin
  Convert(['enum e { A = 4, B = A + 1, C = A | B };','enum f { X = B + 1, Y };']);
  AssertConverted;
  AssertInterface('members in a value are converted with ord',['e = (A := 4,B := ord(A)+1,C := ord(A) or ord(B));']);
  AssertInterface('members of another enum are converted with ord',['f = (X := ord(B)+1,Y);']);
end;


procedure TTestEnums.TestEnumValueUsesMemberCompiles;

begin
  Convert(['enum e { A = 4, B = A + 1, C = A | B };','enum f { X = B + 1, Y };','#define M (A)'],['-d']);
  AssertConverted;
  AssertInterface('member outside an enum value is not converted',['M = A;']);
  AssertCompiles;
end;


procedure TTestEnums.TestEnumConstantUsesMember;

begin
  Convert(['enum e { A = 4, B = A + 1 };'],['-e']);
  AssertConverted;
  AssertInterface('enum constants use members directly',['A = 4;','B = A+1;']);
end;


initialization
  RegisterTests('H2Pas',[TTestTypedefs,TTestEnums]);
end.
