{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown HTML renderer tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UTest.Markdown.HTMLRender;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MarkDown.Elements,
  MarkDown.HtmlRender;

type

  { TTestHTMLRender }

  TTestHTMLRender = Class(TTestCase)
  private
    FHTMLRenderer : TMarkDownHTMLRenderer;
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
    procedure TestRender(const aHTML : string);
    Property Renderer : TMarkDownHTMLRenderer Read FHTMLRenderer;
    Property Document : TMarkDownDocument Read FDocument;
  Published
    procedure TestHookup;
    procedure TestEmpty;
    procedure TestEmptyNoEnvelope;
    procedure TestEmptyTitle;
    procedure TestEmptyHead;
    procedure TestTextBlockEmpty;
    procedure TestTextBlockText;
    procedure TestTextBlockTextStrong;
    procedure TestTextBlockTextEmph;
    procedure TestTextBlockTextDelete;
    procedure TestTextBlockTextStrongEmph;
    procedure TestTextBlockTextStrongEmphSplit1;
    procedure TestTextBlockTextStrongEmphSplit2;
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

{ TTestHTMLRender }

procedure TTestHTMLRender.SetUp;

begin
  FHTMLRenderer:=TMarkDownHTMLRenderer.Create(Nil);
  FDocument:=TMarkDownDocument.Create(Nil,1);
end;


procedure TTestHTMLRender.TearDown;

begin
  FreeAndNil(FDocument);
  FreeAndNil(FHTMLRenderer);
end;


function TTestHTMLRender.CreateTextBlock(aParent: TMarkdownBlock; const aText, aTextNode: string; aNodeStyle: TNodeStyles): TMarkDownTextBlock;

begin
  Result:=TMarkDownTextBlock.Create(aParent,1,aText);
  if aTextNode<>'' then
    AppendTextNode(Result,aTextNode,aNodeStyle);
end;

function TTestHTMLRender.CreateParagraphBlock(const aTextNode: string): TMarkdownBlock;

begin
  Result:=TMarkDownParagraphBlock.Create(FDocument,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestHTMLRender.CreateQuotedBlock(const aTextNode: string): TMarkdownBlock;

begin
  Result:=TMarkDownQuoteBlock.Create(FDocument,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestHTMLRender.CreateHeadingBlock(const aTextNode: string; aLevel: integer): TMarkdownBlock;

begin
  Result:=TMarkDownHeadingBlock.Create(FDocument,1,aLevel);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestHTMLRender.CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: string): TMarkDownListItemBlock;

var
  lPar : TMarkDownParagraphBlock;
begin
  Result:=TMarkDownListItemBlock.Create(aParent,1);
  lPar:=TMarkDownParagraphBlock.Create(Result,1);
  CreateTextBlock(lPar,'',aText);
end;


function TTestHTMLRender.CreateListBlock(aOrdered: boolean; const aListItemText: string): TMarkDownListBlock;

begin
  Result:=TMarkDownListBlock.Create(FDocument,1);
  Result.ordered:=aOrdered;
  if aListItemText<>'' then
    CreateListItemBlock(Result,aListItemText);
end;

function TTestHTMLRender.AppendTextNode(aBlock: TMarkDownTextBlock; const aText: string; aNodeStyle: TNodeStyles): TMarkDownTextNode;

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

procedure TTestHTMLRender.TestRender(const aHTML: string);

var
  L : TStrings;

begin
  L:=TstringList.Create;
  try
    L.SkipLastLineBreak:=True;
    Renderer.RenderDocument(FDocument,L);
    assertEquals('Correct html: ',aHTML,L.Text);
  finally
    L.Free;
  end;
end;


procedure TTestHTMLRender.TestHookup;

begin
  AssertNotNull('Have renderer',FHTMLRenderer);
  AssertNotNull('Have document',FDocument);
  AssertEquals('Have empty document',0,FDocument.blocks.Count);
end;


procedure TTestHTMLRender.TestEmpty;

begin
  Renderer.Options:=[hoEnvelope];
  TestRender('<!DOCTYPE html>'+sLineBreak+'<html>'+sLineBreak+'<body>'+sLineBreak+'</body>'+sLineBreak+'</html>');
end;


procedure TTestHTMLRender.TestEmptyNoEnvelope;

begin
  Renderer.Options:=[];
  TestRender('');
end;


procedure TTestHTMLRender.TestEmptyTitle;

begin
  Renderer.Options:=[hoEnvelope,hoHead];
  Renderer.Title:='a';
  TestRender('<!DOCTYPE html>'+sLineBreak+'<html>'+sLineBreak
             +'<head>'+sLineBreak+'<title>a</title>'+sLineBreak+'</head>'+sLineBreak
             +'<body>'+sLineBreak+'</body>'+sLineBreak+'</html>');
end;


procedure TTestHTMLRender.TestEmptyHead;

begin
  Renderer.Options:=[hoEnvelope,hoHead];
  Renderer.Head.Add('<meta charset="UTF8">');
  TestRender('<!DOCTYPE html>'+sLineBreak+'<html>'+sLineBreak
             +'<head>'+sLineBreak+'<meta charset="UTF8">'+sLineBreak+'</head>'+sLineBreak
             +'<body>'+sLineBreak+'</body>'+sLineBreak+'</html>');
end;

procedure TTestHTMLRender.TestTextBlockEmpty;

begin
  CreateTextBlock(Document,'a','');
  TestRender('');
end;


procedure TTestHTMLRender.TestTextBlockText;

begin
  CreateTextBlock(Document,'a','a');
  TestRender('a');
end;


procedure TTestHTMLRender.TestTextBlockTextStrong;

begin
  CreateTextBlock(Document,'a','a',[nsStrong]);
  TestRender('<b>a</b>');
end;


procedure TTestHTMLRender.TestTextBlockTextEmph;

begin
  CreateTextBlock(Document,'a','a',[nsEmph]);
  TestRender('<i>a</i>');
end;


procedure TTestHTMLRender.TestTextBlockTextDelete;

begin
  CreateTextBlock(Document,'a','a',[nsDelete]);
  TestRender('<del>a</del>');
end;


procedure TTestHTMLRender.TestTextBlockTextStrongEmph;

begin
  CreateTextBlock(Document,'a','a',[nsStrong,nsEmph]);
  TestRender('<b><i>a</i></b>');
end;


procedure TTestHTMLRender.TestTextBlockTextStrongEmphSplit1;

var
  lBlock : TMarkDownTextBlock;

begin
  lBlock:=CreateTextBlock(Document,'a','a ',[nsStrong]);
  AppendTextNode(lBlock,'b',[nsStrong,nsemph]);
  TestRender('<b>a <i>b</i></b>');
end;


procedure TTestHTMLRender.TestTextBlockTextStrongEmphSplit2;

var
  lBlock : TMarkDownTextBlock;
begin
  lBlock:=CreateTextBlock(Document,'a','a',[nsEmph,nsStrong]);
  AppendTextNode(lBlock,' b',[nsStrong]);
  TestRender('<b><i>a</i> b</b>');
end;


procedure TTestHTMLRender.TestPragraphBlockEmpty;

begin
  CreateParagraphBlock('');
  TestRender('<p></p>');
end;


procedure TTestHTMLRender.TestPragraphBlockText;

begin
  CreateParagraphBlock('a');
  TestRender('<p>a</p>');
end;


procedure TTestHTMLRender.TestQuotedBlockEmpty;

begin
  CreateQuotedBlock('');
  TestRender('<blockquote>'+sLineBreak+'</blockquote>');
end;


procedure TTestHTMLRender.TestQuotedBlockText;

begin
  CreateQuotedBlock('a');
  TestRender('<blockquote>'+sLineBreak+'a</blockquote>');
end;


procedure TTestHTMLRender.TestHeadingBlockEmpty;

begin
  CreateHeadingBlock('',1);
  TestRender('<h1></h1>');
end;


procedure TTestHTMLRender.TestHeadingBlockText;

begin
  CreateHeadingBlock('a',1);
  TestRender('<h1>a</h1>');
end;


procedure TTestHTMLRender.TestHeadingBlockTextLevel2;

begin
  CreateHeadingBlock('a',2);
  TestRender('<h2>a</h2>');
end;


procedure TTestHTMLRender.TestUnorderedListEmpty;

begin
  CreateListBlock(false,'');
  TestRender('<ul>'+sLineBreak+'</ul>');
end;


procedure TTestHTMLRender.TestUnorderedListOneItem;

begin
  CreateListBlock(false,'a');
  TestRender('<ul>'+sLineBreak+'<li>a</li>'+sLineBreak+'</ul>');
end;


initialization
  Registertest(TTestHTMLRender);
end.

