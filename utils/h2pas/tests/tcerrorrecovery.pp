{
  h2pas test suite: conversion continues after syntax errors in the header.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcErrorRecovery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestErrorRecovery }

  TTestErrorRecovery = class(TH2PasTestCase)
  protected
    // Fails unless h2pas reported a syntax error, exited normally and wrote the unit.
    procedure AssertRecovered;
  published
    procedure TestTypedefWithTwoNames;
    procedure TestTaggedTypedefWithTwoNames;
    procedure TestUnionTypedefWithTwoNames;
    procedure TestTypedefWithMacroArguments;
    procedure TestRepeatedTypedefErrors;
    procedure TestMemberError;
    procedure TestMacroDecoratedPrototypes;
    procedure TestRecoveredUnitCompiles;
    procedure TestIndentationAfterError;
    procedure TestCommentBracketsInErrorText;
  end;

implementation


procedure TTestErrorRecovery.AssertRecovered;

begin
  AssertTrue('h2pas reports the syntax error',Pos('syntax error',ToolOutput)>0);
  AssertEquals('h2pas exit code',0,ToolExitCode);
  AssertTrue('h2pas reports no internal error',Pos('Internal error',ToolOutput)=0);
  AssertTrue('h2pas reports no indentation warning',Pos('decrease the indentation',ToolOutput)=0);
  AssertTrue('h2pas writes the unit',RawOutput.Count>0);
end;


procedure TTestErrorRecovery.TestTypedefWithTwoNames;

begin
  Convert(['typedef struct { int a; } FAR ZEXPORT;','int x;']);
  AssertRecovered;
  AssertOutput('typedef without a usable name is reported',['(* typedef without name at line 1 ignored *)']);
  AssertInterface('declaration after the error',['var','x : longint;cvar;public;']);
end;


procedure TTestErrorRecovery.TestTaggedTypedefWithTwoNames;

begin
  Convert(['typedef struct s { int a; } X Y;','int z;']);
  AssertRecovered;
  AssertInterface('tagged struct is declared under its tag',['s = record','a : longint;','end;']);
  AssertInterface('declaration after the error',['z : longint;cvar;public;']);
end;


procedure TTestErrorRecovery.TestUnionTypedefWithTwoNames;

begin
  Convert(['typedef union { int a; int b; } X Y;','int z;']);
  AssertRecovered;
  AssertInterface('declaration after the error',['z : longint;cvar;public;']);
end;


procedure TTestErrorRecovery.TestTypedefWithMacroArguments;

begin
  Convert(['typedef int OF((int a));','int y;']);
  AssertRecovered;
  AssertInterface('declaration after the error',['y : longint;cvar;public;']);
end;


procedure TTestErrorRecovery.TestRepeatedTypedefErrors;

begin
  Convert(['typedef struct { int a; } A1 B1;','typedef struct { int b; } A2 B2;',
           'typedef struct { int c; } A3 B3;','typedef struct { int d; } A4 B4;','int w;']);
  AssertRecovered;
  AssertEquals('each failing typedef is reported',4,CountOf('typedef without name'));
  AssertInterface('declaration after the errors',['w : longint;cvar;public;']);
end;


procedure TTestErrorRecovery.TestMemberError;

begin
  Convert(['struct s { int a b; int c; };','int z;']);
  AssertRecovered;
  AssertInterface('members after the error',['s = record','c : longint;','end;']);
end;


procedure TTestErrorRecovery.TestMacroDecoratedPrototypes;

begin
  Convert(['typedef struct z_stream_s { int avail_in; } z_stream;',
           'typedef z_stream FAR *z_streamp;',
           'ZEXTERN int ZEXPORT deflate OF((z_streamp strm, int flush));',
           'ZEXTERN int ZEXPORT deflateEnd OF((z_streamp strm));',
           'typedef struct gz_header_s { int text; } gz_header;',
           'int after(void);']);
  AssertRecovered;
  AssertInterface('struct before the errors',['z_stream_s = record','avail_in : longint;','end;']);
  AssertInterface('struct between the errors',['gz_header_s = record','text : longint;','end;']);
  AssertInterface('function after the errors',['function after:longint;']);
end;


procedure TTestErrorRecovery.TestRecoveredUnitCompiles;

begin
  Convert(['typedef struct { int a; } FAR ZEXPORT;','typedef struct s { int a; } X Y;',
           'struct t { int a b; int c; };','extern int z;'],['-d']);
  AssertRecovered;
  AssertCompiles;
end;


procedure TTestErrorRecovery.TestIndentationAfterError;

begin
  Convert(['int bad bad;','int f(void);','typedef int t;']);
  AssertRecovered;
  AssertRawLine('function after an error has the base indentation','  function f:longint;');
  AssertRawLine('type block after an error has the base indentation','  type');
end;


procedure TTestErrorRecovery.TestCommentBracketsInErrorText;

begin
  Convert(['#define JMETHOD(type,methodname,arglist) type (*methodname) arglist','int x;',
           '#define wl_cast(ptr) (__typeof__(ptr))((char *)(ptr) - 1) @','int y;'],['-d']);
  AssertRecovered;
  AssertOutput('an opening comment bracket in the C text is split',
    ['#define JMETHOD(type,methodname,arglist) type ( *methodname) arglist']);
  AssertOutput('a closing comment bracket in the C text is split',
    ['#define wl_cast(ptr) (__typeof__(ptr))((char * )(ptr) - 1) @']);
  AssertInterface('the declarations after the errors',['x : longint;cvar;public;']);
  AssertCompiles;
end;


initialization
  RegisterTest('H2Pas',TTestErrorRecovery);
end.
