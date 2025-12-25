{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown LaTeX renderer tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *********************************************************************}
unit UTest.Markdown.LaTeXRender;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MarkDown.Elements,
  MarkDown.LatexRender;

type

  { TTestLaTeXRender }

  TTestLaTeXRender = Class(TTestCase)
  private
    FLaTeXRenderer : TMarkDownLaTeXRenderer;
    FDocument: TMarkDownDocument;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    function CreateTextBlock(aParent: TMarkdownBlock; const aText,aTextNode: string; aNodeStyle : TNodeStyles=[]): TMarkDownTextBlock;
    function CreateParagraphBlock(const aTextNode: string): TMarkdownBlock;
    function CreateQuotedBlock(const aTextNode: string): TMarkdownBlock;
    function CreateHeadingBlock(const aTextNode: string; aLevel : integer): TMarkdownBlock;
    function CreateListBlock(aOrdered : boolean; const aListItemText : string): TMarkDownListBlock;
    function CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: string): TMarkDownListItemBlock;
    function AppendTextNode(aBlock: TMarkDownTextBlock; const aText: string; aNodeStyle : TNodeStyles) : TMarkDownTextNode;
    procedure TestRender(const aLaTeX : string);
    Property Renderer : TMarkDownLaTeXRenderer Read FLaTeXRenderer;
    Property Document : TMarkDownDocument Read FDocument;
  Published
    procedure TestHookup;
    procedure TestEmpty;
    procedure TestEmptyNoEnvelope;
    procedure TestEmptyTitle;
    procedure TestTextBlockEmpty;
    procedure TestTextBlockText;
    procedure TestTextBlockTextEscaping;
    procedure TestTextBlockTextStrong;
    procedure TestTextBlockTextEmph;
    procedure TestTextBlockTextDelete;
    procedure TestTextBlockTextStrongEmph;
    procedure TestPragraphBlockEmpty;
    procedure TestPragraphBlockText;
    procedure TestQuotedBlockEmpty;
    procedure TestQuotedBlockText;
    procedure TestHeadingBlockEmpty;
    procedure TestHeadingBlockText;
    procedure TestHeadingBlockTextLevel2;
    procedure TestUnorderedListEmpty;
    procedure TestUnorderedListOneItem;
  end;

implementation

{ TTestLaTeXRender }

procedure TTestLaTeXRender.SetUp;

begin
  FLaTeXRenderer:=TMarkDownLaTeXRenderer.Create(Nil);
  FDocument:=TMarkDownDocument.Create(Nil,1);
end;


procedure TTestLaTeXRender.TearDown;

begin
  FreeAndNil(FDocument);
  FreeAndNil(FLaTeXRenderer);
end;


function TTestLaTeXRender.CreateTextBlock(aParent: TMarkdownBlock; const aText, aTextNode: string; aNodeStyle: TNodeStyles): TMarkDownTextBlock;

begin
  Result:=TMarkDownTextBlock.Create(aParent,1,aText);
  if aTextNode<>'' then
    AppendTextNode(Result,aTextNode,aNodeStyle);
end;

function TTestLaTeXRender.CreateParagraphBlock(const aTextNode: string): TMarkdownBlock;

begin
  Result:=TMarkDownParagraphBlock.Create(FDocument,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestLaTeXRender.CreateQuotedBlock(const aTextNode: string): TMarkdownBlock;

begin
  Result:=TMarkDownQuoteBlock.Create(FDocument,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestLaTeXRender.CreateHeadingBlock(const aTextNode: string; aLevel: integer): TMarkdownBlock;

begin
  Result:=TMarkDownHeadingBlock.Create(FDocument,1,aLevel);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestLaTeXRender.CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: string): TMarkDownListItemBlock;

var
  lPar : TMarkDownParagraphBlock;
begin
  Result:=TMarkDownListItemBlock.Create(aParent,1);
  lPar:=TMarkDownParagraphBlock.Create(Result,1);
  CreateTextBlock(lPar,'',aText);
end;


function TTestLaTeXRender.CreateListBlock(aOrdered: boolean; const aListItemText: string): TMarkDownListBlock;

begin
  Result:=TMarkDownListBlock.Create(FDocument,1);
  Result.ordered:=aOrdered;
  if aListItemText<>'' then
    CreateListItemBlock(Result,aListItemText);
end;

function TTestLaTeXRender.AppendTextNode(aBlock: TMarkDownTextBlock; const aText: string; aNodeStyle: TNodeStyles): TMarkDownTextNode;

var
  p : TPosition;
  t : TMarkdownTextNode;

begin
  if aBlock.Nodes=Nil then
    aBlock.Nodes:=TMarkDownTextNodeList.Create(True);
  p.col:=Length(aBlock.Text);
  p.line:=1;
  t:=TMarkDownTextNode.Create(p,nkText);
  t.addText(aText);
  t.active:=False;
  T.Styles:=aNodeStyle;
  aBlock.Nodes.Add(t);
  Result:=T;
end;

procedure TTestLaTeXRender.TestRender(const aLaTeX: string);

var
  L : TStrings;

begin
  L:=TstringList.Create;
  try
    L.SkipLastLineBreak:=True;
    Renderer.RenderDocument(FDocument,L);
    assertEquals('Correct latex: ',aLaTeX,L.Text);
  finally
    L.Free;
  end;
end;


procedure TTestLaTeXRender.TestHookup;

begin
  AssertNotNull('Have renderer',FLaTeXRenderer);
  AssertNotNull('Have document',FDocument);
  AssertEquals('Have empty document',0,FDocument.blocks.Count);
end;


procedure TTestLaTeXRender.TestEmpty;

begin
  Renderer.Options:=[loEnvelope];
  TestRender('\documentclass{article}'+sLineBreak+
             '\usepackage[utf8]{inputenc}'+sLineBreak+
             '\usepackage{graphicx}'+sLineBreak+
             '\usepackage{hyperref}'+sLineBreak+
             '\usepackage{ulem}'+sLineBreak+
             '\begin{document}'+sLineBreak+
             '\end{document}');
end;


procedure TTestLaTeXRender.TestEmptyNoEnvelope;

begin
  Renderer.Options:=[];
  TestRender('');
end;


procedure TTestLaTeXRender.TestEmptyTitle;

begin
  Renderer.Options:=[loEnvelope];
  Renderer.Title:='a';
  TestRender('\documentclass{article}'+sLineBreak+
             '\usepackage[utf8]{inputenc}'+sLineBreak+
             '\usepackage{graphicx}'+sLineBreak+
             '\usepackage{hyperref}'+sLineBreak+
             '\usepackage{ulem}'+sLineBreak+
             '\title{a}'+sLineBreak+
             '\begin{document}'+sLineBreak+
             '\maketitle'+sLineBreak+
             '\end{document}');
end;


procedure TTestLaTeXRender.TestTextBlockEmpty;

begin
  CreateTextBlock(Document,'a','');
  TestRender('');
end;


procedure TTestLaTeXRender.TestTextBlockText;

begin
  CreateTextBlock(Document,'a','a');
  TestRender('a');
end;

procedure TTestLaTeXRender.TestTextBlockTextEscaping;

begin
  CreateTextBlock(Document,'a','# $ % ^ & _ { } ~ \');
  // Expected: \# \$ \% \textasciicircum{} \& \_ \{ \} \textasciitilde{} \textbackslash{}
  TestRender('\# \$ \% \textasciicircum{} \& \_ \{ \} \textasciitilde{} \textbackslash{}');
end;


procedure TTestLaTeXRender.TestTextBlockTextStrong;

begin
  CreateTextBlock(Document,'a','a',[nsStrong]);
  TestRender('\textbf{a}');
end;


procedure TTestLaTeXRender.TestTextBlockTextEmph;

begin
  CreateTextBlock(Document,'a','a',[nsEmph]);
  TestRender('\textit{a}');
end;


procedure TTestLaTeXRender.TestTextBlockTextDelete;

begin
  CreateTextBlock(Document,'a','a',[nsDelete]);
  TestRender('\sout{a}');
end;


procedure TTestLaTeXRender.TestTextBlockTextStrongEmph;

begin
  CreateTextBlock(Document,'a','a',[nsStrong,nsEmph]);
  TestRender('\textbf{\textit{a}}');
end;


procedure TTestLaTeXRender.TestPragraphBlockEmpty;

begin
  CreateParagraphBlock('');
  TestRender(sLineBreak); // Blank line
end;

procedure TTestLaTeXRender.TestQuotedBlockEmpty;

begin
  CreateQuotedBlock('');
  TestRender('\begin{quote}'+sLineBreak+'\end{quote}');
end;

procedure TTestLaTeXRender.TestUnorderedListEmpty;

begin
  CreateListBlock(false,'');
  TestRender('\begin{itemize}'+sLineBreak+'\end{itemize}');
end;

procedure TTestLaTeXRender.TestPragraphBlockText;

begin
  CreateParagraphBlock('a');
  TestRender('a'+sLineBreak);
end;

procedure TTestLaTeXRender.TestQuotedBlockText;

begin
  CreateQuotedBlock('a');
  TestRender('\begin{quote}'+sLineBreak+'a\end{quote}');
end;

procedure TTestLaTeXRender.TestHeadingBlockEmpty;

begin
  CreateHeadingBlock('',1);
  TestRender('\section*{}');
end;

procedure TTestLaTeXRender.TestHeadingBlockText;

begin
  CreateHeadingBlock('a',1);
  TestRender('\section*{a}');
end;

procedure TTestLaTeXRender.TestHeadingBlockTextLevel2;

begin
  CreateHeadingBlock('a',2);
  TestRender('\subsection*{a}');
end;

procedure TTestLaTeXRender.TestUnorderedListOneItem;

begin
  CreateListBlock(false,'a');
  // ListItem appends '\item ' then renders children.
  // Children = Paragraph 'a'. Plain paragraph renders children (text 'a').
  // Item renderer now adds a newline.
  // List renderer adds \begin{itemize}\n ... \end{itemize}\n
  TestRender('\begin{itemize}'+sLineBreak+'\item a'+sLineBreak+'\end{itemize}');
end;


initialization
  Registertest(TTestLaTeXRender);
end.
