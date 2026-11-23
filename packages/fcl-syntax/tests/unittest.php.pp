{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    PHP highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.php;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.php, unittest.syntaxfixture;

type

  { TTestPhpHighlighter }

  TTestPhpHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestNowdoc;
    procedure TestAttribute;
    procedure TestComments;
  end;

implementation

class function TTestPhpHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('php',
    '<?php function f($x) { return $x + 42; }',
    'function', '"function //"', '// function');
end;


procedure TTestPhpHighlighter.TestNowdoc;

begin
  AssertRole('php', '<<<''TEXT''' + #10 + 'function #' + #10 + 'TEXT;', 'function', shStrings);
end;


procedure TTestPhpHighlighter.TestAttribute;

begin
  AssertRole('php', '#[Example] function f() {}', '#[Example', shAttribute);
end;


procedure TTestPhpHighlighter.TestComments;

begin
  AssertRole('php', '# FUNCTION' + #10 + 'function', 'FUNCTION', shComment);
  AssertRole('php', '# FUNCTION' + #10 + 'function', 'function', shKeyword);
  AssertRole('php', '/* Function */ function', 'Function', shComment);
  AssertRole('php', '/* Function */ function', 'function', shKeyword);
  AssertRole('php', 'function f() {} //', '//', shComment);
end;

initialization
  RegisterTest(TTestPhpHighlighter);
end.
