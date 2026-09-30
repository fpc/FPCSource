{
  h2pas test suite: structs, unions and bit fields.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcStructs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestStructs }

  TTestStructs = class(TH2PasTestCase)
  published
    procedure TestNamedStruct;
    procedure TestForwardStruct;
    procedure TestMultipleDeclarators;
    procedure TestCharArrayMember;
    procedure TestLargeArrayMember;
    procedure TestMultiDimArrayMember;
    procedure TestMacroSizedArrayMember;
    procedure TestFlexibleArrayMember;
    procedure TestFlexibleArrayMemberKinds;
    procedure TestFlexibleArrayMembersCompile;
    procedure TestSelfPointerMember;
    procedure TestSelfPointerInFunctionPointerMember;
    procedure TestSelfPointerInFunctionPointerMemberCompiles;
    procedure TestStructKeywordMember;
    procedure TestFunctionPointerMember;
    procedure TestFunctionPointerArrayMember;
    procedure TestFunctionPointerArrayMemberCompiles;
    procedure TestNoCdeclAfterProcedureType;
    procedure TestFunctionPointerMemberArguments;
    procedure TestFunctionPointerMemberResult;
    procedure TestFunctionPointerMemberArgumentsOneTypeSection;
    procedure TestFunctionPointerMemberArgumentsCompile;
    procedure TestNestedUnionMember;
    procedure TestNestedStructMember;
    procedure TestPointerToTaggedStructMember;
    procedure TestPointerToAnonymousStructMember;
    procedure TestTaggedStructMember;
    procedure TestPointerToAnonymousUnionMember;
    procedure TestNestedPointerToStructMembers;
    procedure TestPointerToStructMembersCompile;
    procedure TestDoublePointerMembers;
    procedure TestDoublePointerMembersPrefix;
    procedure TestDoublePointerVariableAndTypedef;
    procedure TestDoublePointersCompile;
    procedure TestReservedWordMember;
    procedure TestMoreReservedWordMembers;
    procedure TestConstMember;
    procedure TestUnion;
    procedure TestBitFields;
    procedure TestBitFieldConstants;
    procedure TestBitFieldAccessors;
    procedure TestBitFieldAccessorBodies;
    procedure TestBitFieldNamedLikeParameter;
    procedure TestBitFieldNamedLikeReservedWord;
    procedure TestWideBitFields;
    procedure TestBitFieldWiderThan32;
    procedure TestBitFieldGroupFull;
    procedure TestBitFieldDoesNotFit;
    procedure TestBitFieldsCompile;
    procedure TestBitFieldSetterClearsField;
    procedure TestVolatileMember;
    procedure TestDefinesInStruct;
    procedure TestDefinesInEnum;
    procedure TestDefinesInBracesCompile;
    procedure TestSelfContainedUnitCompiles;
  end;

implementation


procedure TTestStructs.TestNamedStruct;

begin
  Convert(['struct s { int a; long b; };']);
  AssertConverted;
  AssertInterface('named struct becomes a record type',['type','s = record','a : longint;','b : longint;','end;']);
end;


procedure TTestStructs.TestForwardStruct;

begin
  Convert(['struct fwd;']);
  AssertConverted;
  AssertInterface('forward struct becomes an empty record',['fwd = record','{undefined structure}','end;']);
end;


procedure TTestStructs.TestMultipleDeclarators;

begin
  Convert(['struct s { int x, y; };']);
  AssertConverted;
  AssertInterface('each declarator of a member becomes a field',['s = record','x : longint;','y : longint;','end;']);
end;


procedure TTestStructs.TestLargeArrayMember;

begin
  Convert(['struct s { char buf[40000]; int big[0x10000]; };'],['-d']);
  AssertConverted;
  AssertInterface('an array size above 32767 is a constant bound',
    ['buf : array[0..39999] of ansichar;','big : array[0..65535] of longint;']);
  AssertCompiles;
end;


procedure TTestStructs.TestCharArrayMember;

begin
  Convert(['struct s { char name[32]; };']);
  AssertConverted;
  AssertInterface('array member with a constant size',['name : array[0..31] of ansichar;']);
end;


procedure TTestStructs.TestMultiDimArrayMember;

begin
  Convert(['struct s { int grid[3][4]; };']);
  AssertConverted;
  AssertInterface('two-dimensional array member',['grid : array[0..2] of array[0..3] of longint;']);
end;


procedure TTestStructs.TestMacroSizedArrayMember;

begin
  Convert(['struct s { int dyn[SIZE]; };']);
  AssertConverted;
  AssertInterface('array member sized by an identifier',['dyn : array[0..(SIZE)-1] of longint;']);
end;


procedure TTestStructs.TestFlexibleArrayMember;

begin
  Convert(['struct s { int n; char data[]; };','struct v { int n; char *p; };']);
  AssertConverted;
  AssertInterface('flexible array member is an array of one element',
    ['s = record','n : longint;','data : array[0..0] of ansichar;','end;']);
  AssertInterface('pointer member stays a pointer',['v = record','n : longint;','p : ^ansichar;','end;']);
end;


procedure TTestStructs.TestFlexibleArrayMemberKinds;

begin
  Convert(['struct t { int n; char *names[]; };','struct u { int n; int m[][4]; };',
           'typedef struct { int n; double vals[]; } w;','void f(char *list[]);']);
  AssertConverted;
  AssertInterface('flexible array of pointers',['names : array[0..0] of ^ansichar;']);
  AssertInterface('flexible array of arrays',['m : array[0..0] of array[0..3] of longint;']);
  AssertInterface('flexible array member in a typedef',['w = record','n : longint;','vals : array[0..0] of double;','end;']);
  AssertInterface('open array parameter stays a pointer',['procedure f(list:PPansichar);']);
end;


procedure TTestStructs.TestFlexibleArrayMembersCompile;

begin
  Convert(['struct s { int n; struct s *next; char data[]; };','struct t { int n; char *names[]; };',
           'typedef struct { int n; double vals[]; } w;'],['-d','-T','-p']);
  AssertConverted;
  AssertInterface('flexible array member under -T -p',['next : Ps;','data : array[0..0] of ansichar;']);
  AssertCompiles;
end;


procedure TTestStructs.TestSelfPointerMember;

begin
  Convert(['struct s { struct s *next; };']);
  AssertConverted;
  AssertInterface('pointer to the struct itself',['next : ^s;']);
end;


procedure TTestStructs.TestSelfPointerInFunctionPointerMember;

begin
  Convert(['struct s { int a; void (*cb)(struct s *p); };','typedef struct t { void (*cb)(struct t *p); } t_t;']);
  AssertConverted;
  AssertInterface('pointer type precedes the record that uses it',
    ['Ps = ^s;','s = record','a : longint;','cb : procedure (p:Ps);cdecl;','end;']);
  AssertInterface('pointer type precedes a typedef record',['Pt = ^t;','t = record','cb : procedure (p:Pt);cdecl;','end;','t_t = t;']);
end;


procedure TTestStructs.TestSelfPointerInFunctionPointerMemberCompiles;

begin
  Convert(['struct s { int a; void (*cb)(struct s *p); };','typedef struct t { void (*cb)(struct t *p); } t_t;',
           'void f(struct s *p, t_t *q);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestStructs.TestStructKeywordMember;

begin
  Convert(['struct s { struct other o; union u v; enum e w; };']);
  AssertConverted;
  AssertInterface('struct, union and enum members use the tag names',['o : other;','v : u;','w : e;']);
end;


procedure TTestStructs.TestFunctionPointerMember;

begin
  Convert(['struct s { int (*fn)(int); };']);
  AssertConverted;
  AssertInterface('function pointer member is a cdecl procedural type',['fn : function (_para1:longint):longint;cdecl;']);
end;


procedure TTestStructs.TestFunctionPointerArrayMember;

begin
  Convert(['struct s { void (*cb[4])(int); int n; };','typedef struct { int (*ops[2])(void); } t;']);
  AssertConverted;
  AssertInterface('array of function pointers member gets a named element type',
    ['s_cb = procedure (_para1:longint);cdecl;','s = record','cb : array[0..3] of s_cb;','n : longint;','end;']);
  AssertInterface('element type in a typedef of an anonymous struct',
    ['t_ops = function :longint;cdecl;','t = record','ops : array[0..1] of t_ops;','end;']);
end;


procedure TTestStructs.TestFunctionPointerArrayMemberCompiles;

begin
  Convert(['struct s { void (*cb[4])(int); int n; };','typedef struct { int (*ops[2])(void); } t;',
           'struct { void (*cb[2])(int); } v;'],['-d']);
  AssertConverted;
  AssertInterface('element type of a member of an anonymous struct variable',['v_cb = procedure (_para1:longint);cdecl;']);
  AssertCompiles;
end;


procedure TTestStructs.TestNoCdeclAfterProcedureType;

begin
  Convert(['typedef int (*binop)(int a);','struct s { int m; int (*fn)(int); int n; };'],['-d']);
  AssertConverted;
  AssertInterface('only the function pointer field is cdecl',
    ['s = record','m : longint;','fn : function (_para1:longint):longint;cdecl;','n : longint;','end;']);
  AssertCompiles;
end;


procedure TTestStructs.TestNestedUnionMember;

begin
  Convert(['struct s { union { int i; float f; } u; };']);
  AssertConverted;
  AssertInterface('anonymous union member becomes a variant record',
    ['u : record','case longint of','0 : ( i : longint );','1 : ( f : single );','end;']);
end;


procedure TTestStructs.TestNestedStructMember;

begin
  Convert(['struct s { struct { int x, y; } pt; };']);
  AssertConverted;
  AssertInterface('anonymous struct member becomes a nested record',['pt : record','x : longint;','y : longint;','end;']);
end;


procedure TTestStructs.TestReservedWordMember;

begin
  Convert(['struct s { int type; int label; };']);
  AssertConverted;
  AssertInterface('reserved word members get an underscore prefix',['_type : longint;','_label : longint;']);
end;


procedure TTestStructs.TestMoreReservedWordMembers;

begin
  Convert(['struct s { int in; int end; int of; int set; int file; int unit; int xor; };']);
  AssertConverted;
  AssertInterface('all Pascal reserved words get an underscore prefix',
    ['_in : longint;','_end : longint;','_of : longint;','_set : longint;','_file : longint;','_unit : longint;',
     '_xor : longint;']);
end;


procedure TTestStructs.TestConstMember;

begin
  Convert(['struct s { const char *cs; };']);
  AssertConverted;
  AssertOutput('const qualifier is reported as ignored',['(* Const before declarator ignored *)']);
  AssertInterface('const pointer member',['cs : ^ansichar;']);
end;


procedure TTestStructs.TestUnion;

begin
  Convert(['union u2 { int i; float f; char c[4]; };']);
  AssertConverted;
  AssertInterface('union becomes a variant record',
    ['u2 = record','case longint of','0 : ( i : longint );','1 : ( f : single );',
     '2 : ( c : array[0..3] of ansichar );','end;']);
end;


procedure TTestStructs.TestBitFields;

begin
  Convert(['struct bits { unsigned int fa : 1; unsigned int fb : 3; int c; unsigned int fd : 4; };']);
  AssertConverted;
  AssertInterface('bit fields are grouped in flag fields',
    ['bits = record','flag0 : word;','c : longint;','flag1 : word;','end;']);
end;


procedure TTestStructs.TestBitFieldConstants;

begin
  Convert(['struct bits { unsigned int fa : 1; unsigned int fb : 3; int c; unsigned int fd : 4; };']);
  AssertConverted;
  AssertInterface('bit masks and positions are constants',
    ['const','bm_bits_fa = $1;','bp_bits_fa = 0;','bm_bits_fb = $E;','bp_bits_fb = 1;',
     'bm_bits_fd = $F;','bp_bits_fd = 0;']);
end;


procedure TTestStructs.TestBitFieldAccessors;

begin
  Convert(['struct bits { unsigned int fa : 1; unsigned int fb : 3; };']);
  AssertConverted;
  AssertInterface('bit fields get getter and setter declarations',
    ['function fa(var __rec : bits) : dword;','procedure set_fa(var __rec : bits; __fa : dword);',
     'function fb(var __rec : bits) : dword;','procedure set_fb(var __rec : bits; __fb : dword);']);
end;


procedure TTestStructs.TestBitFieldAccessorBodies;

begin
  Convert(['struct bits { unsigned int fa : 1; unsigned int fb : 3; };']);
  AssertConverted;
  AssertImplementation('bit field getter body',
    ['function fa(var __rec : bits) : dword;','begin','fa:=(__rec.flag0 and bm_bits_fa) shr bp_bits_fa;','end;']);
  AssertImplementation('bit field setter body',
    ['procedure set_fb(var __rec : bits; __fb : dword);','begin',
     '__rec.flag0:=(__rec.flag0 and not bm_bits_fb) or ((__fb shl bp_bits_fb) and bm_bits_fb);','end;']);
end;


procedure TTestStructs.TestBitFieldNamedLikeParameter;

begin
  Convert(['struct bits { unsigned int a : 1; unsigned int b : 3; };'],['-d']);
  AssertConverted;
  AssertInterface('getter of a field named a',['function a(var __rec : bits) : dword;']);
  AssertCompiles;
end;


procedure TTestStructs.TestBitFieldNamedLikeReservedWord;

begin
  Convert(['struct track { unsigned int type : 1; unsigned int end : 3; };'],['-d']);
  AssertConverted;
  AssertInterface('the getter of a field named like a reserved word',['function _type(var __rec : track) : dword;']);
  AssertInterface('the setter keeps its prefix',['procedure set_type(var __rec : track; __type : dword);']);
  AssertImplementation('the getter body',['_end:=(__rec.flag0 and bm_track_end) shr bp_track_end;']);
  AssertCompiles;
end;


procedure TTestStructs.TestWideBitFields;

begin
  Convert(['struct wide { unsigned fa : 10; unsigned fb : 10; };']);
  AssertConverted;
  AssertInterface('more than 16 bits use a dword flag field',['wide = record','flag0 : dword;','end;']);
  AssertInterface('second bit field mask and position',['bm_wide_fb = $FFC00;','bp_wide_fb = 10;']);
end;


procedure TTestStructs.TestBitFieldWiderThan32;

begin
  Convert(['struct s { unsigned long long x : 40; unsigned int y : 3; };']);
  AssertConverted;
  AssertInterface('a bit field wider than 32 bits uses a qword flag',['s = record','flag0 : qword;','flag1 : word;','end;']);
  AssertInterface('mask of the wide bit field',['bm_s_x = $FFFFFFFFFF;']);
  AssertInterface('getter of the wide bit field',['function x(var __rec : s) : qword;']);
end;


procedure TTestStructs.TestBitFieldGroupFull;

begin
  Convert(['struct s { unsigned int a : 1; unsigned int b : 31; unsigned int c : 2; };']);
  AssertConverted;
  AssertInterface('a full 32 bit group is followed by a new flag',['s = record','flag0 : dword;','flag1 : word;','end;']);
  AssertImplementation('bit field in the first flag',['b:=(__rec.flag0 and bm_s_b) shr bp_s_b;']);
  AssertImplementation('bit field in the second flag',['c:=(__rec.flag1 and bm_s_c) shr bp_s_c;']);
end;


procedure TTestStructs.TestBitFieldDoesNotFit;

begin
  Convert(['struct s { unsigned int a : 30; unsigned int b : 10; };']);
  AssertConverted;
  AssertInterface('a bit field that does not fit starts a new flag',['s = record','flag0 : dword;','flag1 : word;','end;']);
  AssertInterface('position in the new flag',['bm_s_b = $3FF;','bp_s_b = 0;']);
  AssertImplementation('bit field in the new flag',['b:=(__rec.flag1 and bm_s_b) shr bp_s_b;']);
end;


procedure TTestStructs.TestBitFieldsCompile;

begin
  Convert(['struct w { unsigned long long x : 40; unsigned int y : 3; };',
           'struct f { unsigned int a : 1; unsigned int b : 31; unsigned int c : 2; };'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestStructs.TestBitFieldSetterClearsField;

begin
  Convert(['struct s { unsigned long long x : 40; unsigned int a : 1; unsigned int b : 3; };'],['-d']);
  AssertConverted;
  AssertImplementation('setter of a qword flag clears the field',
    ['procedure set_x(var __rec : s; __x : qword);','begin',
     '__rec.flag0:=(__rec.flag0 and not bm_s_x) or ((__x shl bp_s_x) and bm_s_x);','end;']);
  AssertImplementation('setter in the second flag clears the field',
    ['__rec.flag1:=(__rec.flag1 and not bm_s_b) or ((__b shl bp_s_b) and bm_s_b);']);
  AssertCompiles;
end;


procedure TTestStructs.TestVolatileMember;

begin
  Convert(['struct s { volatile int flag; int (*fn)(void volatile **p); };']);
  AssertConverted;
  AssertInterface('volatile member',['flag : longint;']);
  AssertInterface('volatile in a procedure type',['fn : function (p:Ppointer):longint;cdecl;']);
end;


procedure TTestStructs.TestDefinesInStruct;

begin
  Convert(['typedef struct {','  int a;','#define FLAG_A 1','#define FLAG_B(x) ((x)&2)','  int b;','} s_t, *ps_t;',
           'int after(void);'],['-d']);
  AssertConverted;
  AssertInterface('the record is complete',['s_t = record','a : longint;','b : longint;','end;','ps_t = ^s_t;']);
  AssertInterface('the constant follows the record',['ps_t = ^s_t;','const','FLAG_A = 1;']);
  AssertInterface('the macro follows the record',['function FLAG_B(x : longint) : longint;']);
  AssertInterface('the declarations after the record',['function after:longint;cdecl;external;']);
end;


procedure TTestStructs.TestDefinesInEnum;

begin
  Convert(['enum XML_Status {','  XML_STATUS_ERROR = 0,','#define XML_STATUS_ERROR XML_STATUS_ERROR','  XML_STATUS_OK = 1,',
           '#define XML_STATUS_OK XML_STATUS_OK','  XML_STATUS_SUSPENDED = 2','};','#define LIMIT 3']);
  AssertConverted;
  AssertInterface('the enum is complete',['XML_Status = (XML_STATUS_ERROR := 0,XML_STATUS_OK := 1,']);
  AssertOutput('the defines follow the enum',
    ['(* self-referencing #define XML_STATUS_ERROR ignored *)','(* self-referencing #define XML_STATUS_OK ignored *)']);
  AssertInterface('defines after the enum',['LIMIT = 3;']);
end;


procedure TTestStructs.TestDefinesInBracesCompile;

begin
  Convert(['typedef struct {','  int a;','#define FLAG_A 1','  int b;','} s_t;','enum e {','  E1 = 0,','#define E1 E1','  E2 = 1','};',
           'int after(void);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestStructs.TestSelfContainedUnitCompiles;

begin
  Convert(['struct rec { int a; long b; unsigned short c; float d; char name[8]; };',
           'union un { int i; float f; };',
           'enum kind { ka, kb = 3, kc };',
           'extern int counter;',
           'int add(int a, int b);',
           'void reset(void);'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestStructs.TestPointerToTaggedStructMember;

begin
  Convert(['struct info { int n; struct cons { int col; unsigned char op; } *aCons; struct cons *again; };',
           'void f(struct cons *c);'],['-d']);
  AssertConverted;
  AssertInterface('a struct defined in a member is a record type of its own',
    ['Pcons = ^cons;','cons = record','col : longint;','op : byte;','end;']);
  AssertInterface('the member points to the record type',['aCons : ^cons;','again : ^cons;']);
  AssertInterface('the tag is known after the struct',['procedure f(c:Pcons);cdecl;external;']);
  AssertNotOutput('no pointer to an inline record','^record');
end;


procedure TTestStructs.TestPointerToAnonymousStructMember;

begin
  Convert(['struct info { struct { int x; } *anon; };'],['-d']);
  AssertConverted;
  AssertInterface('an anonymous struct with a pointer declarator is named after the member',
    ['info_anon = record','x : longint;','end;']);
  AssertInterface('the member points to it',['anon : ^info_anon;']);
end;


procedure TTestStructs.TestTaggedStructMember;

begin
  Convert(['struct info2 { struct inner { int col; } in; struct { int y; } an; };'],['-d']);
  AssertConverted;
  AssertInterface('a tagged struct member is a record type of its own',['inner = record','col : longint;','end;']);
  AssertInterface('the member uses it, an anonymous struct member stays nested',
    ['_in : inner;','an : record','y : longint;','end;']);
end;


procedure TTestStructs.TestPointerToAnonymousUnionMember;

begin
  Convert(['struct info2 { union { int i; float f; } *pu; };'],['-d']);
  AssertConverted;
  AssertInterface('an anonymous union with a pointer declarator is a variant record type',
    ['info2_pu = record','case longint of','0 : ( i : longint );','1 : ( f : single );','end;']);
  AssertInterface('the member points to it',['pu : ^info2_pu;']);
end;


procedure TTestStructs.TestNestedPointerToStructMembers;

begin
  Convert(['typedef struct info { int n; struct { int a; struct deep { int z; } *d; } *list; } info;'],['-d']);
  AssertConverted;
  AssertInterface('the innermost struct comes first',
    ['deep = record','z : longint;','end;','info_list = record','a : longint;','d : ^deep;','end;',
     'info = record','n : longint;','list : ^info_list;','end;']);
end;


procedure TTestStructs.TestPointerToStructMembersCompile;

begin
  Convert(['struct info { int n; struct cons { int col; unsigned char op; } *aCons; struct { int x; } *anon;',
           '  struct cons *again; union { int i; float f; } *pu; struct inner { int c; } in; };',
           'typedef struct t { struct { int a; struct deep { int z; } *d; } *list; } t;',
           'void f(struct cons *c);'],['-d']);
  AssertConverted;
  AssertCompiles;
  Convert(['struct info { struct cons { int col; } *aCons; struct { int x; } *anon; };'],['-d','-1']);
  AssertConverted;
  AssertCompiles;
end;


const
  DoublePointerHeader : array[0..2] of string = (
    'typedef struct sqlite3_value sqlite3_value;',
    'struct cons { int col; };',
    'struct info { struct cons **pp; int **ip; char ***cpp; sqlite3_value **apSqlParam; int **parr[2]; void **vp; };');

procedure TTestStructs.TestDoublePointerMembers;

begin
  Convert(DoublePointerHeader,['-d']);
  AssertConverted;
  AssertInterface('a pointer to a pointer is a pointer to the named pointer type',
    ['pp : ^Pcons;','ip : ^Plongint;','cpp : ^PPansichar;','apSqlParam : ^Psqlite3_value;','parr : array[0..1] of ^Plongint;',
     'vp : ^pointer;']);
  AssertInterface('the named pointer types are declared',['Pcons = ^cons;']);
  AssertNotOutput('no pointer to a pointer written with ^^','^^');
end;


procedure TTestStructs.TestDoublePointerMembersPrefix;

begin
  Convert(DoublePointerHeader,['-d','-T']);
  AssertConverted;
  AssertInterface('the named pointer type with -T',['pp : ^Pcons;','ip : ^Plongint;']);
  AssertInterface('the pointer type points to the T type',['Pcons = ^Tcons;']);
end;


procedure TTestStructs.TestDoublePointerVariableAndTypedef;

begin
  Convert(['extern int **gip;','typedef int **ipp_t;','typedef struct cons { int c; } **cpp_t;'],['-d']);
  AssertConverted;
  AssertInterface('a variable',['gip : ^Plongint;cvar;external;']);
  AssertInterface('a typedef',['ipp_t = ^Plongint;']);
  AssertInterface('a typedef of a struct',['cpp_t = ^Pcons;']);
end;


procedure TTestStructs.TestDoublePointersCompile;

begin
  Convert(DoublePointerHeader,['-d']);
  AssertConverted;
  AssertCompiles;
  Convert(DoublePointerHeader,['-d','-T','-1']);
  AssertConverted;
  AssertCompiles;
  Convert(['extern int **gip;','typedef int **ipp_t;','typedef struct cons { int c; } **cpp_t;'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


const
  ModuleHeader : array[0..2] of string = (
    'typedef struct m {',
    '  int (*find)(int n, void (*cb)(int), void (**pcb)(int, char *));',
    '  void (*(*sym)(void *h))(void); } m;');

procedure TTestStructs.TestFunctionPointerMemberArguments;

begin
  Convert(ModuleHeader,['-d']);
  AssertConverted;
  AssertInterface('function pointer arguments of a function pointer member are named types',
    ['m_find_cb = procedure (_para1:longint);cdecl;','m_find_pcb = procedure (_para1:longint; _para2:Pansichar);cdecl;',
     'Pm_find_pcb = ^m_find_pcb;']);
  AssertInterface('the member uses the named types',
    ['find : function (n:longint; cb:m_find_cb; pcb:Pm_find_pcb):longint;cdecl;']);
  AssertNotOutput('no pointer to an anonymous procedure type','Pprocedure');
end;


procedure TTestStructs.TestFunctionPointerMemberResult;

begin
  Convert(ModuleHeader,['-d']);
  AssertConverted;
  AssertInterface('the function pointer result of a function pointer member is a named type',
    ['m_sym_result = procedure ;cdecl;']);
  AssertInterface('the member uses the named result type',['sym : function (h:pointer):m_sym_result;cdecl;']);
end;


procedure TTestStructs.TestFunctionPointerMemberArgumentsOneTypeSection;

begin
  Convert(ModuleHeader,['-d','-1']);
  AssertConverted;
  AssertInterface('the named types precede the record',['m_sym_result = procedure ;cdecl;','m = record']);
  AssertCompiles;
end;


procedure TTestStructs.TestFunctionPointerMemberArgumentsCompile;

begin
  Convert(ModuleHeader,['-d']);
  AssertConverted;
  AssertCompiles;
end;


initialization
  RegisterTest('H2Pas',TTestStructs);
end.
