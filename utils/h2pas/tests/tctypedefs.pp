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
    procedure TestNamedTypeTypedef;
    procedure TestAnonymousStruct;
    procedure TestTaggedStruct;
    procedure TestPointerAlias;
    procedure TestAnonymousUnion;
    procedure TestCharPointer;
    procedure TestArray;
    procedure TestOpenArray;
    procedure TestFunctionPointer;
    procedure TestProcedurePointer;
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
  end;

implementation


procedure TTestTypedefs.TestSimpleTypedef;

begin
  Convert(['typedef int myint;']);
  AssertConverted;
  AssertInterface('typedef of a base type',['type','myint = longint;']);
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


initialization
  RegisterTests('H2Pas',[TTestTypedefs,TTestEnums]);
end.
