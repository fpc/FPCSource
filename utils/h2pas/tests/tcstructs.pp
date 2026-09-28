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
    procedure TestMultiDimArrayMember;
    procedure TestMacroSizedArrayMember;
    procedure TestSelfPointerMember;
    procedure TestStructKeywordMember;
    procedure TestFunctionPointerMember;
    procedure TestNoCdeclAfterProcedureType;
    procedure TestNestedUnionMember;
    procedure TestNestedStructMember;
    procedure TestReservedWordMember;
    procedure TestConstMember;
    procedure TestUnion;
    procedure TestBitFields;
    procedure TestBitFieldConstants;
    procedure TestBitFieldAccessors;
    procedure TestBitFieldAccessorBodies;
    procedure TestBitFieldNamedLikeParameter;
    procedure TestWideBitFields;
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


procedure TTestStructs.TestSelfPointerMember;

begin
  Convert(['struct s { struct s *next; };']);
  AssertConverted;
  AssertInterface('pointer to the struct itself',['next : ^s;']);
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
     '__rec.flag0:=__rec.flag0 or ((__fb shl bp_bits_fb) and bm_bits_fb);','end;']);
end;


procedure TTestStructs.TestBitFieldNamedLikeParameter;

begin
  Convert(['struct bits { unsigned int a : 1; unsigned int b : 3; };'],['-d']);
  AssertConverted;
  AssertInterface('getter of a field named a',['function a(var __rec : bits) : dword;']);
  AssertCompiles;
end;


procedure TTestStructs.TestWideBitFields;

begin
  Convert(['struct wide { unsigned fa : 10; unsigned fb : 10; };']);
  AssertConverted;
  AssertInterface('more than 16 bits use a longint flag field',['wide = record','flag0 : longint;','end;']);
  AssertInterface('second bit field mask and position',['bm_wide_fb = $FFC00;','bp_wide_fb = 10;']);
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


initialization
  RegisterTest('H2Pas',TTestStructs);
end.
