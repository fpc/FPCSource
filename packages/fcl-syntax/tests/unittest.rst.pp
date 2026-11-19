{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    reStructuredText highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.rst;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.rst, unittest.syntaxfixture;

type

  { TTestRstHighlighter }

  TTestRstHighlighter = class(TSyntaxCheckTestCase)
  published
    procedure TestRegistered;
    procedure TestSection;
    procedure TestInlineMarkup;
    procedure TestRole;
    procedure TestReferences;
    procedure TestListMarks;
    procedure TestField;
    procedure TestDirective;
    procedure TestCodeBlock;
    procedure TestComment;
    procedure TestTarget;
    procedure TestLiteralBlock;
    procedure TestTable;
    procedure TestPlainText;
    procedure TestEscape;
    procedure TestUnmatchedMarks;
    procedure TestMathDirective;
    procedure TestPermutations;
    procedure TestStarRunFast;
    procedure TestStarWordRunFast;
    procedure TestBackquoteWordRunFast;
    procedure TestBarWordRunFast;
    procedure TestLiteralWordRunFast;
    procedure TestColonRunFast;
    procedure TestColonWordRunFast;
    procedure TestBracketWordRunFast;
    procedure TestExplicitRunFast;
    procedure TestLiteralBlockRunFast;
    procedure TestTitleRunFast;
  end;

implementation

const
  Doc = 'A Title' + #10 + '=======' + #10 + '' + #10 + 'The **barye** is *one* unit, ``literal`` and :math:`1\,\mathrm{Ba}` here.'
    + #10 + '- a list item with a `link <https://example.com>`_ and a ref_ and [1]_' + #10 + '#. numbered' + #10 + ':Author: Tony' + #10
    + '.. note:: a |sub| here' + #10 + '.. code-block:: pascal' + #10 + '   :linenos:' + #10 + '' + #10 + '   **not strong** in code'
    + #10 + '' + #10 + 'back **out**' + #10 + '.. a comment' + #10 + '   still the comment' + #10 + '.. _target: https://example.org'
    + #10 + 'Literal next::' + #10 + '' + #10 + '    *raw* here' + #10 + '' + #10 + '+-----+-----+' + #10 + '| a   | b   |' + #10
    + 'snake_case_name and 2*3*4 and \*escaped';
  MathDoc = '.. math::' + #10 + '' + #10 + '   E = mc^2' + #10 + 'after';

procedure TTestRstHighlighter.TestRegistered;

begin
  AssertNotNull('rst is registered as rst', TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass('rst'));
  AssertNotNull('rst is registered as restructuredtext', TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass('restructuredtext'));
end;


procedure TTestRstHighlighter.TestSection;

begin
  AssertRole('rst', Doc, 'A Title', shSection);
  AssertRole('rst', Doc, '=======', shSymbol);
end;


procedure TTestRstHighlighter.TestInlineMarkup;

begin
  AssertRole('rst', Doc, '**barye**', shKeyword);
  AssertRole('rst', Doc, '*one*', shAttribute);
  AssertRole('rst', Doc, '``literal``', shStrings);
  AssertRole('rst', Doc, '**out**', shKeyword);
end;


procedure TTestRstHighlighter.TestRole;

begin
  AssertRole('rst', Doc, ':math:', shDirective);
  AssertRole('rst', Doc, '`1\,\mathrm{Ba}`', shStrings);
end;


procedure TTestRstHighlighter.TestReferences;

begin
  AssertRole('rst', Doc, '`link <https://example.com>`_', shKey);
  AssertRole('rst', Doc, 'ref_', shKey);
  AssertRole('rst', Doc, '[1]_', shKey);
  AssertRole('rst', Doc, '|sub|', shKey);
end;


procedure TTestRstHighlighter.TestListMarks;

begin
  AssertRole('rst', Doc, '- ', shSymbol);
  AssertRole('rst', Doc, '#.', shSymbol);
end;


procedure TTestRstHighlighter.TestField;

begin
  AssertRole('rst', Doc, ':Author:', shAttribute);
end;


procedure TTestRstHighlighter.TestDirective;

begin
  AssertRole('rst', Doc, '.. note::', shDirective);
end;


procedure TTestRstHighlighter.TestCodeBlock;

begin
  AssertRole('rst', Doc, '.. code-block::', shDirective);
  AssertRole('rst', Doc, ' pascal', shStrings);
  AssertRole('rst', Doc, ':linenos:', shAttribute);
  AssertRole('rst', Doc, '**not strong**', shRawString);
end;


procedure TTestRstHighlighter.TestComment;

begin
  AssertRole('rst', Doc, '.. a comment', shComment);
  AssertRole('rst', Doc, '   still the comment', shComment);
end;


procedure TTestRstHighlighter.TestTarget;

begin
  AssertRole('rst', Doc, '.. _target:', shKey);
  AssertRole('rst', Doc, ' https://example.org', shStrings);
end;


procedure TTestRstHighlighter.TestLiteralBlock;

begin
  AssertRole('rst', Doc, '    *raw* here', shRawString);
end;


procedure TTestRstHighlighter.TestTable;

begin
  AssertRole('rst', Doc, '+-----+-----+', shSymbol);
  AssertRole('rst', Doc, '|', shSymbol, 3);
end;


procedure TTestRstHighlighter.TestPlainText;

begin
  AssertRole('rst', Doc, 'snake_case_name', shDefault);
  AssertRole('rst', Doc, '*3*', shDefault);
end;


procedure TTestRstHighlighter.TestEscape;

begin
  AssertRole('rst', Doc, '\*', shEscape);
end;


procedure TTestRstHighlighter.TestUnmatchedMarks;

begin
  AssertRole('rst', 'a * b * c', '* b', shDefault);
  AssertRole('rst', 'unclosed ``code and **strong', '**strong', shDefault);
end;


procedure TTestRstHighlighter.TestMathDirective;

begin
  AssertRole('rst', MathDoc, '   E = mc^2', shRawString);
  AssertRole('rst', MathDoc, 'after', shDefault);
end;


procedure TTestRstHighlighter.TestPermutations;

var
  I: Integer;
begin
  for I := 0 to 63 do
    Highlight('rst', Copy('`*_[|:.\=-+#>(', I mod 15 + 1, 1) + StringOfChar(Chr(I * 3 mod 256), 3) + Doc);
end;


procedure TTestRstHighlighter.TestStarRunFast;

begin
  AssertFast('rst', StringOfChar('*', 1024 * 1024));
end;


procedure TTestRstHighlighter.TestStarWordRunFast;

begin
  AssertFast('rst', DupeString(' *a', 350 * 1024));
end;


procedure TTestRstHighlighter.TestBackquoteWordRunFast;

begin
  AssertFast('rst', DupeString(' `a', 350 * 1024));
end;


procedure TTestRstHighlighter.TestBarWordRunFast;

begin
  AssertFast('rst', DupeString(' |a', 350 * 1024));
end;


procedure TTestRstHighlighter.TestLiteralWordRunFast;

begin
  AssertFast('rst', DupeString(' ``a', 256 * 1024));
end;


procedure TTestRstHighlighter.TestColonRunFast;

begin
  AssertFast('rst', StringOfChar(':', 1024 * 1024));
end;


procedure TTestRstHighlighter.TestColonWordRunFast;

begin
  AssertFast('rst', DupeString(':a', 512 * 1024));
end;


procedure TTestRstHighlighter.TestBracketWordRunFast;

begin
  AssertFast('rst', DupeString('[a', 512 * 1024));
end;


procedure TTestRstHighlighter.TestExplicitRunFast;

begin
  AssertFast('rst', DupeString('.. ' + #10, 256 * 1024));
end;


procedure TTestRstHighlighter.TestLiteralBlockRunFast;

begin
  AssertFast('rst', DupeString('a::' + #10, 256 * 1024));
end;


procedure TTestRstHighlighter.TestTitleRunFast;

begin
  AssertFast('rst', DupeString('a' + #10 + '==' + #10, 256 * 1024));
end;

initialization
  RegisterTest(TTestRstHighlighter);
end.
