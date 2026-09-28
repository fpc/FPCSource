{
  h2pas test suite: comments and preprocessor directives.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcPreprocessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestComments }

  TTestComments = class(TH2PasTestCase)
  published
    procedure TestBlockComment;
    procedure TestLineComment;
    procedure TestMultiLineComment;
    procedure TestCommentBracesRemoved;
  end;

  { TTestPreprocessor }

  TTestPreprocessor = class(TH2PasTestCase)
  published
    procedure TestIfdef;
    procedure TestIfdefElse;
    procedure TestIf;
    procedure TestBlockKeywordAfterConditional;
    procedure TestElif;
    procedure TestElifAfterIfdef;
    procedure TestElifStripInfo;
    procedure TestElifCompiles;
    procedure TestIfLogicalOperators;
    procedure TestIfComparisonPrecedence;
    procedure TestIfDefinedWithoutParentheses;
    procedure TestIfArithmetic;
    procedure TestIfNumbers;
    procedure TestIfComment;
    procedure TestIfContinuation;
    procedure TestIfUntranslatable;
    procedure TestIfndef;
    procedure TestIfdefComment;
    procedure TestElifCondition;
    procedure TestIfCompiles;
    procedure TestUndef;
    procedure TestDirectiveOnlyHeader;
    procedure TestEmptyHeader;
    procedure TestInclude;
    procedure TestSystemInclude;
    procedure TestPragma;
    procedure TestError;
    procedure TestLineInfoSkipped;
    procedure TestExternCRemoved;
    procedure TestExternCEndRemoved;
    procedure TestCPlusPlusBlockSkipped;
    procedure TestCPlusPlusElseKept;
  end;

implementation


procedure TTestComments.TestBlockComment;

begin
  Convert(['/* block comment */','int x;']);
  AssertConverted;
  AssertOutput('block comment',['{ block comment }']);
end;


procedure TTestComments.TestLineComment;

begin
  Convert(['// line comment','int x;']);
  AssertConverted;
  AssertOutput('line comment',['{ line comment }']);
end;


procedure TTestComments.TestMultiLineComment;

begin
  Convert(['/* multi','   line */','int x;']);
  AssertConverted;
  AssertOutput('multi-line comment',['{ multi','line }']);
end;


procedure TTestComments.TestCommentBracesRemoved;

begin
  Convert(['/* a {b} c */','int x;']);
  AssertConverted;
  AssertOutput('braces inside comments are removed',['{ a b c }']);
end;


procedure TTestPreprocessor.TestIfdef;

begin
  Convert(['#ifdef FOO','int foo1(void);','#endif']);
  AssertConverted;
  AssertInterface('#ifdef',['{$ifdef FOO}','function foo1:longint;','{$endif}']);
end;


procedure TTestPreprocessor.TestIfdefElse;

begin
  Convert(['#ifdef FOO','int foo1(void);','#else','int foo2(void);','#endif']);
  AssertConverted;
  AssertInterface('#else',['{$ifdef FOO}','function foo1:longint;','{$else}','function foo2:longint;','{$endif}']);
end;


procedure TTestPreprocessor.TestIf;

begin
  Convert(['#if FOO','int x;','#endif']);
  AssertConverted;
  AssertInterface('#if',['{$if FOO}','var','x : longint;cvar;public;','{$endif}']);
end;


procedure TTestPreprocessor.TestBlockKeywordAfterConditional;

begin
  Convert(['#ifdef A','typedef int a_t;','#endif','typedef int b_t;',
           '#ifdef B','extern int x;','#else','extern int y;','#endif','extern int z;'],['-d']);
  AssertConverted;
  AssertInterface('type keyword is repeated after #endif',['{$endif}','type','b_t = longint;']);
  AssertInterface('var keyword is repeated after #else',['{$else}','var','y : longint;cvar;external;']);
  AssertInterface('var keyword is repeated after the second #endif',['{$endif}','var','z : longint;cvar;external;']);
  AssertCompiles;
end;


procedure TTestPreprocessor.TestUndef;

begin
  Convert(['#undef FOO','int x;']);
  AssertConverted;
  AssertOutput('#undef',['{$undef FOO}']);
end;


procedure TTestPreprocessor.TestElif;

begin
  Convert(['#if A','int x;','#elif B','int y;','#else','int z;','#endif']);
  AssertConverted;
  AssertInterface('#elif becomes $elseif',
    ['{$if A}','var','x : longint;cvar;public;','{$elseif B}','var','y : longint;cvar;public;',
     '{$else}','var','z : longint;cvar;public;','{$endif}']);
  AssertNotOutput('#elif is no $else','{$else B}');
  AssertNotOutput('#elif gets no comment','was #elif');
end;


procedure TTestPreprocessor.TestElifAfterIfdef;

begin
  Convert(['#ifdef A','int x;','#elif B','int y;','#endif']);
  AssertConverted;
  AssertInterface('#elif after #ifdef',['{$ifdef A}','var','x : longint;cvar;public;','{$elseif B}']);
end;


procedure TTestPreprocessor.TestElifStripInfo;

begin
  Convert(['#if A','int x;','#elif B','int y;','#endif'],['-S']);
  AssertConverted;
  AssertInterface('-S #elif',['{$elseif B}']);
end;


procedure TTestPreprocessor.TestElifCompiles;

begin
  Convert(['#ifdef A','extern int x;','#elif defined(B)','extern int y;','#elif 1','extern int z;','#endif'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestPreprocessor.TestIfLogicalOperators;

begin
  Convert(['#if defined(A) && B','int x;','#endif','#if !defined(A) || (B > 1)','int y;','#endif']);
  AssertConverted;
  AssertOutput('&& becomes and',['{$if defined(A) and B}']);
  AssertOutput('! and || become not and or',['{$if not defined(A) or (B > 1)}']);
  AssertNotOutput('no C operators remain','&&');
end;


procedure TTestPreprocessor.TestIfComparisonPrecedence;

begin
  Convert(['#if A == 1 && B != 2','int x;','#endif','#if V << 2 | 1 >= 4','int y;','#endif']);
  AssertConverted;
  AssertOutput('comparisons are parenthesized',['{$if (A = 1) and (B <> 2)}']);
  AssertOutput('C precedence is kept',['{$if (V shl 2) or (1 >= 4)}']);
end;


procedure TTestPreprocessor.TestIfDefinedWithoutParentheses;

begin
  Convert(['#if defined A && !defined B','int x;','#endif']);
  AssertConverted;
  AssertOutput('defined without parentheses',['{$if defined(A) and not defined(B)}']);
end;


procedure TTestPreprocessor.TestIfArithmetic;

begin
  Convert(['#if V % 3 == 0','int x;','#endif','#if V / 2 > 1','int y;','#endif']);
  AssertConverted;
  AssertOutput('% becomes mod',['{$if (V mod 3) = 0}']);
  AssertOutput('/ becomes div',['{$if (V div 2) > 1}']);
end;


procedure TTestPreprocessor.TestIfNumbers;

begin
  Convert(['#if (V & 0x10) != 0 && V < 010L','int x;','#endif']);
  AssertConverted;
  AssertOutput('hexadecimal and octal numbers',['{$if ((V and $10) <> 0) and (V < &10)}']);
end;


procedure TTestPreprocessor.TestIfComment;

begin
  Convert(['#if defined(A) /* comment */','int x;','#endif','#if defined(B) // comment','int y;','#endif']);
  AssertConverted;
  AssertOutput('block comment is removed',['{$if defined(A)}']);
  AssertOutput('line comment is removed',['{$if defined(B)}']);
end;


procedure TTestPreprocessor.TestIfContinuation;

begin
  Convert(['#if defined(A) && \','    defined(B)','int x;','#endif']);
  AssertConverted;
  AssertOutput('continued condition',['{$if defined(A) and defined(B)}','var','x : longint;cvar;public;']);
end;


procedure TTestPreprocessor.TestIfUntranslatable;

begin
  Convert(['#if A ? B : C','int x;','#endif','#if MACRO(1)','int y;','#endif']);
  AssertConverted;
  AssertOutput('ternary is copied',['{$if A ? B : C}']);
  AssertOutput('macro call is copied',['{$if MACRO(1)}']);
end;


procedure TTestPreprocessor.TestIfndef;

begin
  Convert(['#ifndef GUARD_H /* guard */','int x;','#endif']);
  AssertConverted;
  AssertOutput('#ifndef',['{$ifndef GUARD_H}']);
end;


procedure TTestPreprocessor.TestIfdefComment;

begin
  Convert(['#ifdef FOO // comment','int x;','#endif']);
  AssertConverted;
  AssertOutput('#ifdef without comment',['{$ifdef FOO}']);
end;


procedure TTestPreprocessor.TestElifCondition;

begin
  Convert(['#if 0','int x;','#elif defined(X) || defined(Y)','int y;','#endif']);
  AssertConverted;
  AssertOutput('#elif condition is translated',['{$elseif defined(X) or defined(Y)}']);
end;


procedure TTestPreprocessor.TestIfCompiles;

begin
  Convert(['#if defined(A) && !defined(B)','extern int x;','#endif',
           '#if (1 << 4) == 16 && 7 % 3 == 1','extern int y;','#endif',
           '#if !defined(A) || 0x10 > 010','extern int z;','#endif',
           '#ifndef GUARD_H','extern int w;','#elif defined(A) || \','  defined(B)','extern int v;','#endif'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestPreprocessor.TestDirectiveOnlyHeader;

begin
  Convert(['#undef FOO']);
  AssertConverted;
  AssertOutput('header with only a directive',['{$undef FOO}']);
end;


procedure TTestPreprocessor.TestEmptyHeader;

begin
  Convert(['/* nothing */'],['-d']);
  AssertConverted;
  AssertOutput('empty header gives an empty unit',['unit output;','interface']);
  AssertCompiles;
end;


procedure TTestPreprocessor.TestInclude;

begin
  Convert(['#include "other.h"','int x;']);
  AssertConverted;
  AssertOutput('#include',['{$include "other.h"}']);
end;


procedure TTestPreprocessor.TestSystemInclude;

begin
  Convert(['#include <stdio.h>','int x;']);
  AssertConverted;
  AssertOutput('#include of a system header',['{$include <stdio.h>}']);
end;


procedure TTestPreprocessor.TestPragma;

begin
  Convert(['#pragma pack(1)','int x;']);
  AssertConverted;
  AssertOutput('#pragma is reported as unsupported',['(** unsupported pragma#pragma pack(1)*)']);
end;


procedure TTestPreprocessor.TestError;

begin
  Convert(['#error some error','int x;']);
  AssertConverted;
  AssertOutput('#error',['{$error some error}']);
end;


procedure TTestPreprocessor.TestLineInfoSkipped;

begin
  Convert(['# 1 "file.h"','int after(void);']);
  AssertConverted;
  AssertNotOutput('line markers of the preprocessor are skipped','file.h');
  AssertInterface('declaration after a line marker',['function after:longint;']);
end;


procedure TTestPreprocessor.TestExternCRemoved;

begin
  Convert(['#ifdef __cplusplus','extern "C" {','#endif','int after(void);']);
  AssertConverted;
  AssertOutput('extern "C" block start is removed',['{ C++ extern C conditional removed }']);
  AssertInterface('declaration after extern "C"',['function after:longint;']);
end;


procedure TTestPreprocessor.TestExternCEndRemoved;

begin
  Convert(['int before(void);','#ifdef __cplusplus','}','#endif']);
  AssertConverted;
  AssertOutput('extern "C" block end is removed',['{ C++ end of extern C conditional removed }']);
end;


procedure TTestPreprocessor.TestCPlusPlusBlockSkipped;

begin
  Convert(['#ifdef __cplusplus','class X { };','#endif','int after(void);']);
  AssertConverted;
  AssertNotOutput('C++ code is skipped','class');
  AssertNotOutput('C++ conditional is not copied','{$ifdef');
  AssertInterface('declaration after a C++ block',['function after:longint;']);
end;


procedure TTestPreprocessor.TestCPlusPlusElseKept;

begin
  Convert(['#ifdef __cplusplus','class X;','#else','int c_only(void);','#endif']);
  AssertConverted;
  AssertInterface('else part of a C++ conditional is kept',['function c_only:longint;']);
  AssertNotOutput('else of a C++ conditional is not copied','{$else}');
end;


initialization
  RegisterTests('H2Pas',[TTestComments,TTestPreprocessor]);
end.
