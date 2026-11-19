{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Shared test cases for byte scanner syntax highlighters

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.syntaxfixture;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Types, fpcunit, testregistry,
  syntax.highlighter, syntax.table;

const
  UTF8Sample = 'café Ελληνικά 日本語 😀';
  LargeInputLimitMs = 750;

type
  TSyntaxFixture = record
    // Space separated language names, the first one is the main name
    Names: string;
    // A short program
    Code: string;
    // A keyword occurring in Code, Literal and Comment
    Keyword: string;
    // A literal containing Keyword and a comment start
    Literal: string;
    // A comment containing Keyword
    Comment: string;
  end;

  { TSyntaxCheckTestCase }

  TSyntaxCheckTestCase = class(TTestCase)
  protected
    FTokens: TSyntaxTokenArray;
    // Highlighter registered for aLanguage
    function CreateHighlighter(const aLanguage: string): TSyntaxHighlighter;
    // Check that aTokens are not empty and recompose aSource
    procedure AssertRecovered(const aLabel, aSource: string; const aTokens: TSyntaxTokenArray);
    // Highlight aSource as aLanguage into FTokens, checking the recomposition
    function Highlight(const aLanguage, aSource: string): TSyntaxTokenArray;
    // Kind of the token in FTokens containing the 1-based byte offset aOffset
    function KindAt(aOffset: SizeInt): TSyntaxHighlightKind;
    // Check the kind of the token at the aOccurrence-th occurrence of aNeedle in aSource
    procedure AssertRole(const aLanguage, aSource, aNeedle: string; aKind: TSyntaxHighlightKind; aOccurrence: Integer = 1);
    // Check that aSource has a token aNeedle of kind aKind
    procedure AssertToken(const aLanguage, aSource, aNeedle: string; aKind: TSyntaxHighlightKind);
    // Check that aSource is highlighted and recomposed, the highlighting within LargeInputLimitMs
    procedure AssertFast(const aLanguage, aSource: string);
  end;

  { TSyntaxFixtureTestCase }

  TSyntaxFixtureTestCase = class(TSyntaxCheckTestCase)
  protected
    // The language fixture tested by the generic tests
    class function Fixture: TSyntaxFixture; virtual; abstract;
  published
    procedure TestAliasesRegistered;
    procedure TestRobustInputs;
    procedure TestUTF8Boundaries;
    procedure TestKeyword;
    procedure TestLiteralShieldsKeyword;
    procedure TestCommentShieldsKeyword;
    procedure TestBytePermutations;
    procedure TestLargeInput;
  end;

// A fixture record from its parts
function MakeFixture(const aNames, aCode, aKeyword, aLiteral, aComment: string): TSyntaxFixture;

implementation

function MakeFixture(const aNames, aCode, aKeyword, aLiteral, aComment: string): TSyntaxFixture;

begin
  Result.Names := aNames;
  Result.Code := aCode;
  Result.Keyword := aKeyword;
  Result.Literal := aLiteral;
  Result.Comment := aComment;
end;


{ TSyntaxCheckTestCase }

function TSyntaxCheckTestCase.CreateHighlighter(const aLanguage: string): TSyntaxHighlighter;

var
  lClass: TSyntaxHighlighterClass;
begin
  lClass := TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(aLanguage);
  AssertNotNull('highlighter registered for ' + aLanguage, lClass);
  Result := lClass.Create;
end;


procedure TSyntaxCheckTestCase.AssertRecovered(const aLabel, aSource: string; const aTokens: TSyntaxTokenArray);

var
  I: Integer;
  lJoined: string;
  lPos, lLen: SizeInt;
begin
  lJoined := '';
  SetLength(lJoined, Length(aSource));
  lPos := 1;
  for I := 0 to High(aTokens) do
    begin
    lLen := Length(aTokens[I].Text);
    AssertTrue(aLabel + ': token ' + IntToStr(I) + ' is not empty', lLen > 0);
    AssertTrue(aLabel + ': tokens do not overrun the source', lLen <= Length(lJoined) - lPos + 1);
    Move(aTokens[I].Text[1], lJoined[lPos], lLen);
    Inc(lPos, lLen);
    end;
  AssertEquals(aLabel + ': tokens cover the whole source', Length(aSource) + 1, lPos);
  AssertTrue(aLabel + ': tokens recompose the source', lJoined = aSource);
end;


function TSyntaxCheckTestCase.Highlight(const aLanguage, aSource: string): TSyntaxTokenArray;

var
  lHighlighter: TSyntaxHighlighter;
begin
  lHighlighter := CreateHighlighter(aLanguage);
  try
    FTokens := lHighlighter.Execute(aSource);
  finally
    lHighlighter.Free;
  end;
  AssertRecovered(aLanguage + ' ' + aSource, aSource, FTokens);
  Result := FTokens;
end;


function TSyntaxCheckTestCase.KindAt(aOffset: SizeInt): TSyntaxHighlightKind;

var
  I: Integer;
  lPos: SizeInt;
begin
  lPos := 1;
  for I := 0 to High(FTokens) do
    begin
    if (aOffset >= lPos) and (aOffset < lPos + Length(FTokens[I].Text)) then
      Exit(FTokens[I].Kind);
    Inc(lPos, Length(FTokens[I].Text));
    end;
  Result := shInvalid;
end;


procedure TSyntaxCheckTestCase.AssertRole(const aLanguage, aSource, aNeedle: string; aKind: TSyntaxHighlightKind; aOccurrence: Integer);

var
  I: Integer;
  lPos, lFrom: SizeInt;
begin
  Highlight(aLanguage, aSource);
  lPos := 0;
  lFrom := 1;
  for I := 1 to aOccurrence do
    begin
    lPos := Pos(aNeedle, aSource, lFrom);
    lFrom := lPos + Length(aNeedle);
    end;
  AssertTrue(aLanguage + ': ' + aNeedle + ' occurs in ' + aSource, lPos > 0);
  AssertEquals(aLanguage + ': kind of ' + aNeedle + ' in ' + aSource, aKind.ToString, KindAt(lPos).ToString);
end;


procedure TSyntaxCheckTestCase.AssertToken(const aLanguage, aSource, aNeedle: string; aKind: TSyntaxHighlightKind);

var
  I: Integer;
  lFound: boolean;
begin
  Highlight(aLanguage, aSource);
  lFound := False;
  for I := 0 to High(FTokens) do
    if (FTokens[I].Text = aNeedle) and (FTokens[I].Kind = aKind) then
      lFound := True;
  AssertTrue(aLanguage + ': token ' + aNeedle + ' of kind ' + aKind.ToString + ' in ' + aSource, lFound);
end;


procedure TSyntaxCheckTestCase.AssertFast(const aLanguage, aSource: string);

var
  lHighlighter: TSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
  lStarted, lElapsed: QWord;
begin
  lHighlighter := CreateHighlighter(aLanguage);
  try
    lStarted := GetTickCount64;
    lTokens := lHighlighter.Execute(aSource);
    lElapsed := GetTickCount64 - lStarted;
  finally
    lHighlighter.Free;
  end;
  AssertTrue(aLanguage + ': ' + IntToStr(Length(aSource)) + ' bytes highlighted in ' + IntToStr(lElapsed)
             + ' ms, limit ' + IntToStr(LargeInputLimitMs), lElapsed < LargeInputLimitMs);
  AssertRecovered(aLanguage + ' large input', aSource, lTokens);
end;


{ TSyntaxFixtureTestCase }

procedure TSyntaxFixtureTestCase.TestAliasesRegistered;

var
  lName, lMain: string;
  lClass, lMainClass: TSyntaxHighlighterClass;
begin
  lMain := LanguageNames(Fixture.Names)[0];
  lMainClass := TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(lMain);
  AssertNotNull(lMain + ' is registered', lMainClass);
  for lName in LanguageNames(Fixture.Names) do
    begin
    lClass := TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(lName);
    AssertNotNull(lName + ' is registered', lClass);
    AssertEquals(lName + ' uses the same highlighter as ' + lMain, lMainClass.ClassName, lClass.ClassName);
    end;
end;


procedure TSyntaxFixtureTestCase.TestRobustInputs;

var
  I: Integer;
  lName, lBytes, lLiteral: string;
  lFixture: TSyntaxFixture;
  lPos: SizeInt;
begin
  lFixture := Fixture;
  lBytes := '';
  SetLength(lBytes, 256);
  for lName in LanguageNames(lFixture.Names) do
    begin
    Highlight(lName, lFixture.Code);
    Highlight(lName, '');
    for I := 0 to 255 do
      lBytes[I + 1] := Chr(I);
    Highlight(lName, lBytes);
    for I := 0 to 255 do
      lBytes[I + 1] := Chr(255 - I);
    Highlight(lName, lBytes);
    lLiteral := lFixture.Literal;
    lPos := Pos('"', lLiteral);
    if lPos = 0 then
      lPos := Pos('(', lLiteral);
    Insert(UTF8Sample, lLiteral, lPos + 1);
    Highlight(lName, lLiteral + #13#10 + lFixture.Comment + ' ' + UTF8Sample);
    Highlight(lName, '"' + UTF8Sample);
    Highlight(lName, '''' + UTF8Sample);
    Highlight(lName, '/* ' + UTF8Sample);
    Highlight(lName, '(* ' + UTF8Sample);
    Highlight(lName, '<!-- ' + UTF8Sample);
    Highlight(lName, '--[==[ ' + UTF8Sample);
    Highlight(lName, '<# ' + UTF8Sample);
    Highlight(lName, lFixture.Comment);
    Highlight(lName, lFixture.Code + #0 + UTF8Sample);
    end;
end;


procedure TSyntaxFixtureTestCase.TestUTF8Boundaries;

var
  I: Integer;
  lFixture: TSyntaxFixture;
  lLiteral: string;
  lPos: SizeInt;
begin
  lFixture := Fixture;
  lLiteral := lFixture.Literal;
  lPos := Pos('"', lLiteral);
  if lPos = 0 then
    lPos := Pos('(', lLiteral);
  Insert(UTF8Sample, lLiteral, lPos + 1);
  Highlight(LanguageNames(lFixture.Names)[0], lLiteral + #13#10 + lFixture.Comment + ' ' + UTF8Sample);
  for I := 0 to High(FTokens) do
    AssertFalse('token ' + IntToStr(I) + ' does not start inside a UTF-8 sequence', FTokens[I].Text[1] in [#128..#191]);
end;


procedure TSyntaxFixtureTestCase.TestKeyword;

var
  lFixture: TSyntaxFixture;
begin
  lFixture := Fixture;
  Highlight(LanguageNames(lFixture.Names)[0], lFixture.Code);
  if lFixture.Keyword <> '' then
    AssertTrue(lFixture.Keyword + ' is a keyword in ' + lFixture.Code,
               KindAt(Pos(lFixture.Keyword, lFixture.Code)) in [shKeyword, shKey]);
end;


procedure TSyntaxFixtureTestCase.TestLiteralShieldsKeyword;

var
  lFixture: TSyntaxFixture;
begin
  lFixture := Fixture;
  Highlight(LanguageNames(lFixture.Names)[0], lFixture.Literal);
  if lFixture.Keyword <> '' then
    AssertFalse(lFixture.Keyword + ' is neither keyword nor comment in ' + lFixture.Literal,
                KindAt(Pos(lFixture.Keyword, lFixture.Literal)) in [shKeyword, shComment]);
end;


procedure TSyntaxFixtureTestCase.TestCommentShieldsKeyword;

var
  lFixture: TSyntaxFixture;
begin
  lFixture := Fixture;
  Highlight(LanguageNames(lFixture.Names)[0], lFixture.Comment);
  if lFixture.Keyword <> '' then
    AssertEquals(lFixture.Keyword + ' is part of the comment ' + lFixture.Comment, shComment.ToString,
                 KindAt(Pos(lFixture.Keyword, lFixture.Comment)).ToString);
end;


procedure TSyntaxFixtureTestCase.TestBytePermutations;

const
  Prefixes: array[0..7] of string = ('', '"', '/*', '(*', '[#include "', 'r###"', '--[==[', '$(');
var
  I, J: Integer;
  lBytes: string;
begin
  lBytes := '';
  SetLength(lBytes, 256);
  for I := 0 to 63 do
    begin
    for J := 0 to 255 do
      lBytes[J + 1] := Chr((J * 73 + I * 19) mod 256);
    Highlight(LanguageNames(Fixture.Names)[0], Prefixes[I mod 8] + lBytes);
    end;
end;


procedure TSyntaxFixtureTestCase.TestLargeInput;

var
  lFixture: TSyntaxFixture;
  lSource, lLarge: string;
  lPos, lLen: SizeInt;
begin
  lFixture := Fixture;
  lSource := lFixture.Code + #10 + lFixture.Literal + #10 + lFixture.Comment + #10;
  lLarge := '';
  SetLength(lLarge, 1024 * 1024);
  lPos := 1;
  while lPos <= Length(lLarge) do
    begin
    lLen := Length(lSource);
    if lLen > Length(lLarge) - lPos + 1 then
      lLen := Length(lLarge) - lPos + 1;
    Move(lSource[1], lLarge[lPos], lLen);
    Inc(lPos, lLen);
    end;
  AssertFast(LanguageNames(lFixture.Names)[0], lLarge);
end;

end.
