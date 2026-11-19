{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    LFM highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.lfm;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.lfm, unittest.syntaxfixture;

type

  { TTestLfmHighlighter }

  TTestLfmHighlighter = class(TSyntaxCheckTestCase)
  published
    procedure TestRegistered;
    procedure TestEmpty;
    procedure TestObjectHeader;
    procedure TestInheritedAndInline;
    procedure TestEnd;
    procedure TestPropertyName;
    procedure TestDottedPropertyName;
    procedure TestStrings;
    procedure TestCharacterCodes;
    procedure TestNumbers;
    procedure TestSet;
    procedure TestCollection;
    procedure TestListContinuation;
    procedure TestBinaryData;
    procedure TestBooleans;
    procedure TestUnfinishedInputs;
    procedure TestCategory;
  end;

implementation

const
  Form = 'object Form1: TForm1' + #10
    + '  Left = 395' + #10
    + '  Caption = ''It''''s main'' + #13#$0A' + #10
    + '  Anchors = [akTop, akLeft]' + #10
    + '  Font.Height = -11' + #10
    + '  Visible = True' + #10
    + '  object Button1: TButton' + #10
    + '    Width = 75' + #10
    + '  end' + #10
    + 'end';

procedure TTestLfmHighlighter.TestRegistered;

var
  lName: string;
begin
  for lName in ['lfm', 'dfm', 'xfm', 'fmx'] do
    AssertNotNull(lName + ' is registered', TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(lName));
end;


procedure TTestLfmHighlighter.TestEmpty;

begin
  AssertEquals('empty source gives no tokens', 0, Length(Highlight('lfm', '')));
end;


procedure TTestLfmHighlighter.TestObjectHeader;

begin
  AssertToken('lfm', Form, 'object', shKeyword);
  AssertToken('lfm', Form, 'Form1', shAttribute);
  AssertToken('lfm', Form, 'TForm1', shAttribute);
  AssertToken('lfm', Form, ':', shSymbol);
end;


procedure TTestLfmHighlighter.TestInheritedAndInline;

begin
  AssertToken('lfm', 'inherited Form2: TForm2', 'inherited', shKeyword);
  AssertToken('lfm', 'inline Frame1: TFrame1', 'inline', shKeyword);
  AssertToken('lfm', 'inline Frame1: TFrame1', 'TFrame1', shAttribute);
end;


procedure TTestLfmHighlighter.TestEnd;

begin
  AssertRole('lfm', Form, 'end', shKeyword);
  AssertRole('lfm', Form, 'end', shKeyword, 2);
end;


procedure TTestLfmHighlighter.TestPropertyName;

begin
  AssertToken('lfm', Form, 'Left', shKey);
  AssertToken('lfm', Form, '=', shOperator);
end;


procedure TTestLfmHighlighter.TestDottedPropertyName;

begin
  AssertToken('lfm', Form, 'Font.Height', shKey);
end;


procedure TTestLfmHighlighter.TestStrings;

begin
  AssertToken('lfm', Form, '''It''''s main''', shStrings);
  AssertToken('lfm', Form, '+', shOperator);
end;


procedure TTestLfmHighlighter.TestCharacterCodes;

begin
  AssertToken('lfm', Form, '#13', shCharacters);
  AssertToken('lfm', Form, '#$0A', shCharacters);
end;


procedure TTestLfmHighlighter.TestNumbers;

begin
  AssertToken('lfm', Form, '395', shNumbers);
  AssertToken('lfm', Form, '-11', shNumbers);
  AssertToken('lfm', 'Ratio = 1.5E-3', '1.5E-3', shNumbers);
  AssertToken('lfm', 'Color = $00FF00', '$00FF00', shNumbers);
end;


procedure TTestLfmHighlighter.TestSet;

begin
  AssertToken('lfm', Form, '[', shSymbol);
  AssertToken('lfm', Form, 'akTop', shDefault);
end;


procedure TTestLfmHighlighter.TestCollection;

begin
  AssertToken('lfm', 'Columns = <' + #10 + '  item' + #10 + '    Width = 10' + #10 + '  end>', 'item', shKeyword);
  AssertToken('lfm', 'Columns = <' + #10 + '  item' + #10 + '    Width = 10' + #10 + '  end>', 'Width', shKey);
end;


procedure TTestLfmHighlighter.TestListContinuation;

begin
  AssertRole('lfm', 'Items.Strings = (' + #10 + '  ''one''' + #10 + '  Two' + #10 + ')', 'Two', shDefault);
end;


procedure TTestLfmHighlighter.TestBinaryData;

begin
  AssertToken('lfm', 'Picture.Data = {' + #10 + '  0A0B' + #10 + '  0C0D}', '{' + #10 + '  0A0B' + #10 + '  0C0D}', shComment);
end;


procedure TTestLfmHighlighter.TestBooleans;

begin
  AssertToken('lfm', Form, 'True', shKeyword);
end;


procedure TTestLfmHighlighter.TestUnfinishedInputs;

begin
  Highlight('lfm', '''unfinished' + #10 + 'end');
  Highlight('lfm', 'Data = {0A0B');
  Highlight('lfm', '#');
  Highlight('lfm', '#$');
  Highlight('lfm', 'X = -');
  Highlight('lfm', 'X = 1e');
  Highlight('lfm', UTF8Sample);
end;


procedure TTestLfmHighlighter.TestCategory;

begin
  Highlight('lfm', 'object X: TX');
  AssertTrue('tokens have the LFM category', FTokens[0].HasCategory(TLfmSyntaxHighlighter.CategoryLfm));
end;

initialization
  RegisterTest(TTestLfmHighlighter);
end.
