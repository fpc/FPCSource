{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown text scanner tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UTest.Markdown.Scanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MarkDown.Elements, MarkDown.Scanner;

type
  TMyMarkDownTextScanner = Class(TMarkDownTextScanner)
  Public
    Property Cursor;
    Property LineNo;
  end;

  { TTestMarkDownTextScanner }

  TTestMarkDownTextScanner = class(TTestCase)
  private
    FScanner: TMyMarkDownTextScanner;
  protected
    procedure InitScanner(const aText : string; aLineNo : Integer);
    procedure SetUp; override;
    procedure TearDown; override;
    property Scanner : TMyMarkDownTextScanner Read FScanner;
  published
    procedure TestCreate;
    procedure TestEOF;
    procedure TestPeek;
    procedure TestPeekNext;
    procedure TestPeekPrevious;
    procedure TestPeekEndRun;
    procedure TestNextChar;
    procedure TestLocation;
    procedure TestBookmark;
    procedure TestPeekLen;
    procedure TestPeekWhile;
    procedure TestPeekUntil;
    procedure TestPeekRun;
    procedure TestNextChars;
    procedure TestNextEquals;
    procedure TestHas;
    procedure TestSkipWhitespace;
    procedure TestFindMatchingOccurrence;
    procedure TestFindMatchingOccurrenceExclude;
  end;

implementation

{ TTestMarkDownTextScanner }

procedure TTestMarkDownTextScanner.InitScanner(const aText: string; aLineNo: Integer);
begin
  FreeAndNil(FScanner);
  FScanner:=TMyMarkDownTextScanner.Create(aText, aLineNo);
end;

procedure TTestMarkDownTextScanner.SetUp;
begin
  InitScanner('abcde', 1);
end;

procedure TTestMarkDownTextScanner.TearDown;
begin
  FreeAndNil(FScanner);
end;

procedure TTestMarkDownTextScanner.TestCreate;
begin
  AssertNotNull('Scanner should be created', FScanner);
  AssertEquals('Initial cursor should be 1', 1, FScanner.Cursor);
  AssertEquals('Initial line number should be set', 1, FScanner.LineNo);
end;

procedure TTestMarkDownTextScanner.TestEOF;

begin
  // Test with the default scanner
  AssertFalse('Should not be EOF at start', FScanner.EOF);
  FScanner.NextChars(5);
  AssertTrue('Should be EOF after reading all chars', FScanner.EOF);

  // Test with an empty string
  InitScanner('', 1);
  AssertTrue('Empty scanner should be at EOF', Scanner.EOF);
end;

procedure TTestMarkDownTextScanner.TestPeek;
begin
  AssertEquals('Peek should return first char', 'a', FScanner.Peek);
  FScanner.NextChar;
  AssertEquals('Peek should return second char', 'b', FScanner.Peek);
  FScanner.NextChars(4); // Move to end
  AssertTrue('Should be at EOF', FScanner.EOF);
  AssertEquals('Peek at EOF should return #0', #0, FScanner.Peek);
end;

procedure TTestMarkDownTextScanner.TestPeekNext;
begin
  AssertEquals('PeekNext should see the second char', 'b', FScanner.PeekNext);
  FScanner.NextChars(3);
  AssertEquals('PeekNext should see the last char', 'e', FScanner.PeekNext);
  FScanner.NextChar;
  AssertEquals('PeekNext at the last char should be #0', #0, FScanner.PeekNext);
  FScanner.NextChar;
  AssertTrue('Should be at EOF', FScanner.EOF);
  AssertEquals('PeekNext at EOF should be #0', #0, FScanner.PeekNext);
end;

procedure TTestMarkDownTextScanner.TestPeekPrevious;
begin
  AssertEquals('PeekPrevious at start should be #0', #0, FScanner.PeekPrevious);
  FScanner.NextChar;
  AssertEquals('PeekPrevious at second char should be "a"', 'a', FScanner.PeekPrevious);
  FScanner.NextChars(4);
  AssertEquals('PeekPrevious at EOF should be "e"', 'e', FScanner.PeekPrevious);
end;

procedure TTestMarkDownTextScanner.TestPeekEndRun;

begin
  InitScanner('a--b-c', 1);
  AssertEquals('PeekEndRun on single char', '-', Scanner.PeekEndRun);
  Scanner.NextChar; // Cursor at '-'
  AssertEquals('PeekEndRun on run start', 'b', Scanner.PeekEndRun);
  Scanner.NextChars(3); // Cursor at '-'
  AssertEquals('PeekEndRun on single char before another', 'c', Scanner.PeekEndRun);
  Scanner.NextChars(2); // Cursor at EOF
  AssertEquals('PeekEndRun at EOF', #0, Scanner.PeekEndRun);
end;

procedure TTestMarkDownTextScanner.TestNextChar;
begin
  AssertEquals('NextChar should return "a"', 'a', FScanner.NextChar);
  AssertEquals('Cursor should advance to 2', 2, FScanner.Cursor);
  AssertEquals('Peek should now be "b"', 'b', FScanner.Peek);
  FScanner.NextChars(4); // To EOF
  AssertEquals('NextChar at EOF should return #0', #0, FScanner.NextChar);
end;

procedure TTestMarkDownTextScanner.TestLocation;
var
  Pos: TPosition;
begin
  InitScanner('a'#10'b', 5); // Start at line 5
  Pos := Scanner.Location;
  AssertEquals('Initial line should be 5', 5, Pos.Line);
  AssertEquals('Initial col should be 1', 1, Pos.Col);

  Scanner.NextChar; // consume 'a'
  Pos := Scanner.Location;
  AssertEquals('Line should still be 5', 5, Pos.Line);
  AssertEquals('Col should be 2', 2, Pos.Col);

  Scanner.NextChar; // consume #10
  Pos := Scanner.Location;
  AssertEquals('Line should be 6 after newline', 6, Pos.Line);
  AssertEquals('Col should be 1 after newline', 1, Pos.Col);
end;

procedure TTestMarkDownTextScanner.TestBookmark;
begin
  FScanner.NextChars(2); // Cursor at 'c'
  AssertEquals('Cursor should be at position 3', 3, FScanner.Cursor);
  FScanner.Bookmark;
  FScanner.NextChars(2); // Cursor at 'e'
  AssertEquals('Cursor should be at position 5', 5, FScanner.Cursor);
  FScanner.GotoBookmark;
  AssertEquals('Cursor should be back at position 3', 3, FScanner.Cursor);
  AssertEquals('Peek should be "c" after goto bookmark', 'c', FScanner.Peek);
end;

procedure TTestMarkDownTextScanner.TestPeekLen;
begin
  AssertEquals('PeekLen(3) should be "abc"', 'abc', FScanner.PeekLen(3));
  AssertEquals('Cursor should not move after PeekLen', 1, FScanner.Cursor);
  FScanner.NextChars(3);
  AssertEquals('PeekLen(5) past EOF should be "de"', 'de', FScanner.PeekLen(5));
  AssertEquals('Cursor should not move after PeekLen past EOF', 4, FScanner.Cursor);
end;

procedure TTestMarkDownTextScanner.TestPeekWhile;
begin
  AssertEquals('PeekWhile should get "ab"', 'ab', FScanner.PeekWhile(['a', 'b']));
  AssertEquals('Cursor should not move after PeekWhile', 1, FScanner.Cursor);
  AssertEquals('PeekWhile with no match should be empty', '', FScanner.PeekWhile(['x', 'y']));
end;

procedure TTestMarkDownTextScanner.TestPeekUntil;

begin
  InitScanner('abc*def', 1);
  AssertEquals('PeekUntil should get "abc"', 'abc', Scanner.PeekUntil(['*']));
  AssertEquals('Cursor should not move after PeekUntil', 1, Scanner.Cursor);
  AssertEquals('PeekUntil with no match should be empty', '', Scanner.PeekUntil(['z']));
end;

procedure TTestMarkDownTextScanner.TestPeekRun;

begin
  InitScanner('a***b', 1);
  Scanner.NextChar;
  AssertEquals('PeekRun should find "***"', '***', Scanner.PeekRun(False));
  AssertEquals('Cursor should not move after PeekRun', 2, Scanner.Cursor);

  // Test checkBefore
  AssertEquals('PeekRun with checkBefore=true and different prev char should succeed', '***', Scanner.PeekRun(True));
  Scanner.NextChar; // cursor at second '*'
  AssertEquals('PeekRun with checkBefore=true and same prev char should fail', '', Scanner.PeekRun(True));
end;

procedure TTestMarkDownTextScanner.TestNextChars;
begin
  AssertEquals('NextChars(3) should return "abc"', 'abc', FScanner.NextChars(3));
  AssertEquals('Cursor should be at 4 after NextChars(3)', 4, FScanner.Cursor);
  AssertEquals('NextChars(5) past EOF should return "de"', 'de', FScanner.NextChars(5));
  AssertTrue('Should be at EOF after reading past end', FScanner.EOF);
end;

procedure TTestMarkDownTextScanner.TestNextEquals;

begin
  InitScanner('---abc', 1);
  AssertEquals('NextEquals should consume "---"', '---', Scanner.NextEquals);
  AssertEquals('Cursor should be at 4 after NextEquals', 4, Scanner.Cursor);
  AssertEquals('Peek should be "a"', 'a', Scanner.Peek);
end;

procedure TTestMarkDownTextScanner.TestHas;
begin
  AssertTrue('Has("ab") should be true at start', FScanner.Has('ab'));
  AssertFalse('Has("ac") should be false at start', FScanner.Has('ac'));
  AssertEquals('Cursor should not move after Has', 1, FScanner.Cursor);
end;

procedure TTestMarkDownTextScanner.TestSkipWhitespace;

begin
  InitScanner('  ab', 1);
  Scanner.SkipWhitespace;
  AssertEquals('Cursor should be at 3 after skipping whitespace', 3, Scanner.Cursor);
  AssertEquals('Peek should be "a"', 'a', Scanner.Peek);
end;

procedure TTestMarkDownTextScanner.TestFindMatchingOccurrence;

begin
  InitScanner('some **bold** text, not ***this***', 1);
  AssertTrue('Should find "**" in " **bold** "', Scanner.FindMatchingOccurrence('**'));
  Scanner.Cursor:=25;
  AssertFalse('Should not find "**" in "***this***" because of surrounding *', Scanner.FindMatchingOccurrence('**'));
end;

procedure TTestMarkDownTextScanner.TestFindMatchingOccurrenceExclude;
begin
  InitScanner('a `code` and not a \`backtick`', 1);
  AssertTrue('Should find `code`', Scanner.FindMatchingOccurrence('`', '\'));
  Scanner.Cursor := 18; // Move cursor to "and not a "
  AssertFalse('Should not find `backtick` because it is escaped by \', Scanner.FindMatchingOccurrence('`', '\'));
end;

initialization
  RegisterTest(TTestMarkDownTextScanner);
end.

