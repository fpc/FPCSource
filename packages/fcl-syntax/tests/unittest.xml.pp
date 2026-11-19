{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    XML highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.xml;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.xml, unittest.syntaxfixture;

type

  { TTestXmlHighlighter }

  TTestXmlHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestCommentInAttribute;
    procedure TestCData;
    procedure TestEntity;
  end;

implementation

class function TTestXmlHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('xml svg xsd lpi lpk lps',
    '<root name="value">text &amp; more</root>',
    'root', '<tag name="root <!--"/>', '<!-- root -->');
end;


procedure TTestXmlHighlighter.TestCommentInAttribute;

begin
  AssertRole('xml', '<r a="<!-- x -->">text</r>', '<!--', shStrings);
end;


procedure TTestXmlHighlighter.TestCData;

begin
  AssertRole('xml', '<![CDATA[<tag>]]>', '<tag>', shRawString);
end;


procedure TTestXmlHighlighter.TestEntity;

begin
  AssertRole('xml', '<root name="value">text &amp; more</root>', '&amp;', shEscape);
end;

initialization
  RegisterTest(TTestXmlHighlighter);
end.
