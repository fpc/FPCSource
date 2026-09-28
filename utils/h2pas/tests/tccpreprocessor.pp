{
  h2pas test suite: running the C preprocessor on the input file (-E, -Ec, -Eo, -Ek).
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcCPreprocessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestCPreprocessor }

  TTestCPreprocessor = class(TH2PasTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestDecorationMacroExpanded;
    procedure TestDefinesKept;
    procedure TestMacroExpandedInEnum;
    procedure TestSystemIncludeDropped;
    procedure TestLocalIncludeDropped;
    procedure TestKeepOtherFile;
    procedure TestPreprocessorOptions;
    procedure TestLineNumbersOfTheHeader;
    procedure TestCommentsKept;
    procedure TestTemporaryFilesRemoved;
    procedure TestMissingPreprocessor;
    procedure TestPreprocessedUnitCompiles;
  end;

implementation


procedure TTestCPreprocessor.SetUp;

begin
  inherited SetUp;
  if ExeSearch('gcc',GetEnvironmentVariable('PATH'))='' then
    Ignore('gcc not found');
end;


procedure TTestCPreprocessor.TestDecorationMacroExpanded;

begin
  Convert(['#define API','API int f(void);'],['-d','-E']);
  AssertConverted;
  AssertInterface('an empty macro before a declaration is expanded',['function f:longint;cdecl;external;']);
end;


procedure TTestCPreprocessor.TestDefinesKept;

begin
  Convert(['#define ANSWER 42','#define TWICE(a) ((a)*2)'],['-E']);
  AssertConverted;
  AssertInterface('constant define is kept',['const','ANSWER = 42;']);
  AssertInterface('macro define is kept',['function TWICE(a : longint) : longint;']);
end;


procedure TTestCPreprocessor.TestMacroExpandedInEnum;

begin
  Convert(['#define FOURCC(a,b) ((a<<8)|b)','enum e { X = FOURCC(1,2) };'],['-E']);
  AssertConverted;
  AssertInterface('a macro used in an enum value is expanded',['e = (X := (1 shl 8) or 2);']);
end;


procedure TTestCPreprocessor.TestSystemIncludeDropped;

begin
  Convert(['#include <stddef.h>','size_t g(void);'],['-d','-E']);
  AssertConverted;
  AssertInterface('the declaration of the header is converted',['function g:SizeUInt;cdecl;external;']);
  AssertNotOutput('the text of the system header is dropped','ptrdiff_t');
end;


procedure TTestCPreprocessor.TestLocalIncludeDropped;

begin
  WriteWorkFile('other.h',Lines(['#define OTHER 1','typedef int other_t;']));
  Convert(['#include "other.h"','int f(void);'],['-d','-E']);
  AssertConverted;
  AssertInterface('the declaration of the header is converted',['function f:longint;cdecl;external;']);
  AssertNotOutput('the text of the included header is dropped','other_t');
  AssertNotOutput('the defines of the included header are dropped','OTHER');
end;


procedure TTestCPreprocessor.TestKeepOtherFile;

begin
  WriteWorkFile('other.h',Lines(['#define OTHER 1','typedef int other_t;']));
  Convert(['#include "other.h"','int f(void);'],['-d','-E','-Ek','other.h']);
  AssertConverted;
  AssertInterface('the define of the kept file',['const','OTHER = 1;']);
  AssertInterface('the typedef of the kept file',['type','other_t = longint;']);
  AssertInterface('the declaration of the header itself',['function f:longint;cdecl;external;']);
end;


procedure TTestCPreprocessor.TestPreprocessorOptions;

begin
  Convert(['#ifdef WIN','int w(void);','#else','int u(void);','#endif'],['-d','-Eo','-DWIN']);
  AssertConverted;
  AssertInterface('the option selects the branch',['function w:longint;cdecl;external;']);
  AssertNotOutput('the other branch is dropped','function u');
end;


procedure TTestCPreprocessor.TestLineNumbersOfTheHeader;

begin
  Convert(['#include <stddef.h>','','','','int f(int x) y;'],['-d','-E']);
  AssertTrue('the error line is the line in the header: '+ToolOutput,Pos('at line 5 error',ToolOutput)>0);
end;


procedure TTestCPreprocessor.TestCommentsKept;

begin
  Convert(['/* doc comment */','int f(void);'],['-d','-E']);
  AssertConverted;
  AssertOutput('comments of the header are kept',['{ doc comment }']);
end;


procedure TTestCPreprocessor.TestTemporaryFilesRemoved;

begin
  Convert(['int f(void);'],['-d','-E']);
  AssertConverted;
  AssertFalse('the preprocessor output is removed',FileExists(WorkDir+'output.i'));
  AssertFalse('the filtered header is removed',FileExists(WorkDir+'output.pre.h'));
end;


procedure TTestCPreprocessor.TestMissingPreprocessor;

begin
  Convert(['int f(void);'],['-d','-Ec','no_such_cpp']);
  AssertTrue('h2pas fails without the preprocessor',ToolExitCode<>0);
  AssertTrue('the missing preprocessor is reported: '+ToolOutput,Pos('C preprocessor no_such_cpp not found',ToolOutput)>0);
end;


procedure TTestCPreprocessor.TestPreprocessedUnitCompiles;

begin
  WriteWorkFile('other.h',Lines(['#define OTHER 1','typedef int other_t;']));
  Convert(['#include <stddef.h>','#include "other.h"','#define API','#define FOURCC(a,b) ((a<<8)|b)',
           'API int f(other_t a);','enum e { X = FOURCC(1,2) };','size_t g(void);'],['-d','-E','-Ek','other.h']);
  AssertConverted;
  AssertCompiles;
end;


initialization
  RegisterTest('H2Pas',TTestCPreprocessor);
end.
