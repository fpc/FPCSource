{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown ANSI renderer tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UTest.Markdown.ANSIRender;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Registry,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
{$ENDIF}
  Markdown.Utils, Markdown.Elements, Markdown.Render, Markdown.Parser, Markdown.ANSIRender;

type

  { TTestANSIRender }

  TTestANSIRender = class(TTestCase)
  private
    FRenderer : TMarkDownANSIRenderer;
    procedure DoHeadingNumber(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aNumber : TMarkdownString);
  protected
    function Render(const aMarkdown : TMarkdownString) : TMarkdownString;
    procedure CheckContains(const aMsg, aExpected, aActual : TMarkdownString);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPlainDocument;
    procedure TestNonAsciiText;
    procedure TestNonAsciiStyledText;
    procedure TestDefinitionList;
    procedure TestAlert;
    procedure TestFootnotes;
    procedure TestTableCaption;
    procedure TestFigure;
    procedure TestCommentWritesNothing;
    procedure TestHeadingNumber;
    procedure TestSetextHeading;
    procedure TestEmptyLinkWritesTarget;
  end;

implementation

uses
  Markdown.Processors;

{ TTestANSIRender }

procedure TTestANSIRender.SetUp;

begin
  FRenderer:=TMarkDownANSIRenderer.Create(Nil);
  FRenderer.UseColor:=False;
  FRenderer.Hyperlinks:=False;
end;


procedure TTestANSIRender.TearDown;

begin
  FreeAndNil(FRenderer);
end;


procedure TTestANSIRender.DoHeadingNumber(aRenderer: TMarkdownRenderer; aBlock: TMarkdownBlock; out aNumber: TMarkdownString);

begin
  aNumber:='2.3';
end;


function TTestANSIRender.Render(const aMarkdown: TMarkdownString): TMarkdownString;

var
  lSource : TStringList;
  lDoc : TMarkdownDocument;
begin
  lSource:=TStringList.Create;
  try
    lSource.Text:=MarkdownToString(aMarkdown);
    lDoc:=TMarkdownParser.FastParse(lSource,MarkdownDocExtensions);
    try
      Result:=FRenderer.RenderToString(lDoc);
    finally
      lDoc.Free;
    end;
  finally
    lSource.Free;
  end;
end;


procedure TTestANSIRender.CheckContains(const aMsg, aExpected, aActual: TMarkdownString);

begin
  AssertTrue(aMsg+': expected "'+aExpected+'" in "'+aActual+'"',Pos(aExpected,aActual)>0);
end;


procedure TTestANSIRender.TestPlainDocument;

begin
  CheckContains('Paragraph text','Some text.',Render('# Title'#10#10'Some text.'));
end;


procedure TTestANSIRender.TestNonAsciiText;

begin
  CheckContains('UTF-8 text is kept',UnicodeToMarkdown('Grüße 日本語'),Render(UnicodeToMarkdown('Grüße 日本語')));
end;


procedure TTestANSIRender.TestNonAsciiStyledText;

var
  lOutput : TMarkdownString;

begin
  FRenderer.UseColor:=True;
  lOutput:=Render(UnicodeToMarkdown('**Grüße**'));
  CheckContains('Bold UTF-8 text is kept',UnicodeToMarkdown('Grüße'),lOutput);
  CheckContains('Bold escape sequence',#27'[',lOutput);
end;


procedure TTestANSIRender.TestDefinitionList;

begin
  CheckContains('Term and indented definition','Term'+sLineBreak+'    Definition',Render('Term'#10': Definition'));
end;


procedure TTestANSIRender.TestAlert;

begin
  FRenderer.AlertTitles[atNote]:='Remark';
  CheckContains('Title and indented content','Remark'+sLineBreak+'  Text',Render('> [!NOTE]'#10'> Text'));
end;


procedure TTestANSIRender.TestFootnotes;

var
  S : TMarkdownString;
begin
  S:=Render('Text.[^n]'#10#10'[^n]: The note.');
  CheckContains('Reference','Text.[1]',S);
  CheckContains('Note at the end','[1] The note.',S);
end;


procedure TTestANSIRender.TestTableCaption;

begin
  CheckContains('Caption above the table','Values'+sLineBreak+'+---+',Render('| a |'#10'|---|'#10'| 1 |'#10#10'Table: Values'));
end;


procedure TTestANSIRender.TestFigure;

begin
  CheckContains('Figure line','[Figure: A figure]',Render('![A figure](pic.png)'));
end;


procedure TTestANSIRender.TestCommentWritesNothing;

begin
  AssertEquals('No comment text',0,Pos('hidden',Render('<!-- hidden -->'#10#10'A <!-- also hidden --> b')));
end;


procedure TTestANSIRender.TestHeadingNumber;

begin
  FRenderer.OnHeadingNumber:=@DoHeadingNumber;
  CheckContains('Number before heading','2.3 Title',Render('# Title'));
end;


procedure TTestANSIRender.TestSetextHeading;

begin
  FRenderer.OnHeadingNumber:=@DoHeadingNumber;
  CheckContains('Setext heading rendered as heading','2.3 Title',Render('Title'#10'====='));
end;


procedure TTestANSIRender.TestEmptyLinkWritesTarget;

begin
  CheckContains('Target as text','See other.md.',Render('See [](other.md).'));
end;

initialization
  RegisterTest(TTestANSIRender);
end.
