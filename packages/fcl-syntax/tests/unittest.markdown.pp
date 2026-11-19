{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.markdown;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.markdown, unittest.syntaxfixture;

type

  { TTestMarkdownHighlighter }

  TTestMarkdownHighlighter = class(TSyntaxCheckTestCase)
  published
    procedure TestRegistered;
    procedure TestHeading;
    procedure TestEmphasis;
    procedure TestCodeSpan;
    procedure TestLink;
    procedure TestQuote;
    procedure TestListMarks;
    procedure TestRule;
    procedure TestFence;
    procedure TestWikiLink;
    procedure TestTag;
    procedure TestTableBar;
    procedure TestEscape;
    procedure TestUnderscoreInWord;
    procedure TestNotAHeading;
    procedure TestUnmatchedMarks;
    procedure TestTildeFence;
    procedure TestPermutations;
    procedure TestBracketRunFast;
    procedure TestBacktickRunFast;
    procedure TestStarRunFast;
    procedure TestStarWordRunFast;
    procedure TestTagRunFast;
    procedure TestLinkRunFast;
    procedure TestStrikeRunFast;
    procedure TestFenceRunFast;
  end;

implementation

const
  Doc = '# A heading' + #10 + 'Some **strong** and *soft* words, `code` and [a link](https://example.com).' + #10 + '> a quote' + #10
    + '- a list item' + #10 + '12. a numbered one' + #10 + '- [x] done' + #10 + '---' + #10 + '```pascal' + #10 + '**not strong** in a fence'
    + #10 + '```' + #10 + 'after the fence **strong**' + #10
    + 'a [[Wiki Page|wiki link]] and <b>tag</b> | bar | and \* escaped and snake_case_name';

procedure TTestMarkdownHighlighter.TestRegistered;

begin
  AssertNotNull('markdown is registered as md', TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass('md'));
  AssertNotNull('markdown is registered as markdown', TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass('markdown'));
end;


procedure TTestMarkdownHighlighter.TestHeading;

begin
  AssertRole('markdown', Doc, '# A heading', shSection);
end;


procedure TTestMarkdownHighlighter.TestEmphasis;

begin
  AssertRole('markdown', Doc, '**strong**', shKeyword);
  AssertRole('markdown', Doc, '*soft*', shAttribute);
end;


procedure TTestMarkdownHighlighter.TestCodeSpan;

begin
  AssertRole('markdown', Doc, '`code`', shStrings);
end;


procedure TTestMarkdownHighlighter.TestLink;

begin
  AssertRole('markdown', Doc, '[a link]', shKey);
  AssertRole('markdown', Doc, '(https://example.com)', shStrings);
end;


procedure TTestMarkdownHighlighter.TestQuote;

begin
  AssertRole('markdown', Doc, '> a quote', shComment);
end;


procedure TTestMarkdownHighlighter.TestListMarks;

begin
  AssertRole('markdown', Doc, '- a list', shSymbol);
  AssertRole('markdown', Doc, '12.', shSymbol);
  AssertRole('markdown', Doc, '[x]', shSymbol);
end;


procedure TTestMarkdownHighlighter.TestRule;

begin
  AssertRole('markdown', Doc, '---', shSymbol);
end;


procedure TTestMarkdownHighlighter.TestFence;

begin
  AssertRole('markdown', Doc, '**not strong**', shRawString);
  AssertRole('markdown', Doc, '**strong**', shKeyword, 2);
end;


procedure TTestMarkdownHighlighter.TestWikiLink;

begin
  AssertRole('markdown', Doc, '[[Wiki Page|wiki link]]', shKey);
end;


procedure TTestMarkdownHighlighter.TestTag;

begin
  AssertRole('markdown', Doc, '<b>', shDirective);
end;


procedure TTestMarkdownHighlighter.TestTableBar;

begin
  AssertRole('markdown', Doc, '|', shSymbol, 2);
end;


procedure TTestMarkdownHighlighter.TestEscape;

begin
  AssertRole('markdown', Doc, '\*', shEscape);
end;


procedure TTestMarkdownHighlighter.TestUnderscoreInWord;

begin
  AssertRole('markdown', Doc, '_case_', shDefault);
end;


procedure TTestMarkdownHighlighter.TestNotAHeading;

begin
  AssertRole('markdown', '#not a heading', '#not', shDefault);
end;


procedure TTestMarkdownHighlighter.TestUnmatchedMarks;

begin
  AssertRole('markdown', '* not emphasis *', '* not', shSymbol);
  AssertRole('markdown', 'a * b * c', '* b', shDefault);
  AssertRole('markdown', 'unclosed ``code and **strong', '**strong', shDefault);
end;


procedure TTestMarkdownHighlighter.TestTildeFence;

begin
  AssertRole('markdown', '~~~' + #10 + '# inside' + #10 + '~~~' + #10 + '# outside', '# inside', shRawString);
  AssertRole('markdown', '~~~' + #10 + '# inside' + #10 + '~~~' + #10 + '# outside', '# outside', shSection);
end;


procedure TTestMarkdownHighlighter.TestPermutations;

var
  I: Integer;
begin
  for I := 0 to 63 do
    Highlight('markdown', Copy('`*_[!<|~\#>-1', I mod 14 + 1, 1) + StringOfChar(Chr(I * 3 mod 256), 3) + Doc);
end;


procedure TTestMarkdownHighlighter.TestBracketRunFast;

begin
  AssertFast('markdown', StringOfChar('[', 1024 * 1024));
end;


procedure TTestMarkdownHighlighter.TestBacktickRunFast;

begin
  AssertFast('markdown', StringOfChar('`', 1024 * 1024));
end;


procedure TTestMarkdownHighlighter.TestStarRunFast;

begin
  AssertFast('markdown', StringOfChar('*', 1024 * 1024));
end;


procedure TTestMarkdownHighlighter.TestStarWordRunFast;

begin
  AssertFast('markdown', DupeString('*a', 512 * 1024));
end;


procedure TTestMarkdownHighlighter.TestTagRunFast;

begin
  AssertFast('markdown', DupeString('<a', 512 * 1024));
end;


procedure TTestMarkdownHighlighter.TestLinkRunFast;

begin
  AssertFast('markdown', DupeString('[x](', 256 * 1024));
end;


procedure TTestMarkdownHighlighter.TestStrikeRunFast;

begin
  AssertFast('markdown', DupeString('~~a', 300 * 1024));
end;


procedure TTestMarkdownHighlighter.TestFenceRunFast;

begin
  AssertFast('markdown', DupeString('```' + #10, 256 * 1024));
end;

initialization
  RegisterTest(TTestMarkdownHighlighter);
end.
