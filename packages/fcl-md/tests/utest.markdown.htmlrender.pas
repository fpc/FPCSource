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
  Markdown.Render,
  Markdown.Parser,
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
    procedure TestAssignedHead;
    procedure TestTextBlockEmpty;
    procedure TestTextBlockText;
    procedure TestTextBlockTextStrong;
    procedure TestTextBlockTextEmph;
    procedure TestTextBlockTextDelete;
    procedure TestTextBlockTextStrongEmph;
    procedure TestTextBlockTextStrongEmphSplit1;
    procedure TestTextBlockTextStrongEmphSplit2;
    procedure TestTextBlockTextNestedEmph;
    procedure TestTextBlockImage;
    procedure TestTextBlockLinkWithChildren;
    procedure TestTextBlockLineBreak;
    procedure TestTextBlockTextEscaping;
    procedure TestTextBlockAttributeEscaping;
    procedure TestCodeBlock;
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

  { TMyParagraphBlock }

  // A block class without its own renderer
  TMyParagraphBlock = class(TMarkdownParagraphBlock);

  { TTestHTMLRenderExtensions }

  TTestHTMLRenderExtensions = Class(TTestCase)
  private
    FRenderer : TMarkDownHTMLRenderer;
    procedure DoMarker(aRenderer : TMarkdownRenderer; const aName, aArgument, aValue : String);
    procedure DoResolveLink(aRenderer : TMarkdownRenderer; var aHref, aText : String; out aHandled : Boolean);
    procedure DoHeadingNumber(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aNumber : String);
    procedure DoCaptionNumber(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aLabel : String);
  protected
    function Render(const aMarkdown : String; aOptions : TMarkdownOptions = MarkdownDocExtensions) : String;
    procedure CheckRender(const aMarkdown, aHTML : String);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
  Published
    procedure TestHeadingAttributes;
    procedure TestSetextHeadingID;
    procedure TestCodeAttributes;
    procedure TestCommentWritesNothing;
    procedure TestMarkerEvent;
    procedure TestDefinitionListTight;
    procedure TestDefinitionListLoose;
    procedure TestAlert;
    procedure TestAlertTitle;
    procedure TestFootnotes;
    procedure TestTableCaption;
    procedure TestTableHeaderCells;
    procedure TestFigure;
    procedure TestEmptyLinkWritesTarget;
    procedure TestResolveLink;
    procedure TestHeadingNumber;
    procedure TestRendererFallback;
  end;

implementation

uses
  Markdown.Processors;

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


procedure TTestHTMLRender.TestAssignedHead;

var
  lHead : TStrings;

begin
  lHead:=TStringList.Create;
  try
    lHead.Add('<meta charset="UTF8">');
    Renderer.Options:=[hoEnvelope,hoHead];
    Renderer.Head:=lHead;
    TestRender('<!DOCTYPE html>'+sLineBreak+'<html>'+sLineBreak
               +'<head>'+sLineBreak+'<meta charset="UTF8">'+sLineBreak+'</head>'+sLineBreak
               +'<body>'+sLineBreak+'</body>'+sLineBreak+'</html>');
  finally
    lHead.Free;
  end;
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


procedure TTestHTMLRender.TestTextBlockTextNestedEmph;

var
  lBlock : TMarkDownTextBlock;
  lNode : TMarkDownTextNode;

begin
  lBlock:=CreateTextBlock(Document,'a','a ',[nsEmph]);
  lNode:=AppendTextNode(lBlock,'b',[nsEmph]);
  lNode.AddStyle(nsEmph);
  AppendTextNode(lBlock,' c',[nsEmph]);
  TestRender('<i>a <i>b</i> c</i>');
end;


procedure TTestHTMLRender.TestTextBlockImage;

var
  lBlock : TMarkDownTextBlock;
  lNode : TMarkDownTextNode;

begin
  lBlock:=CreateTextBlock(Document,'a','');
  lNode:=AppendTextNode(lBlock,'alt text',[]);
  lNode.Kind:=nkImg;
  lNode.Attrs.Add('src','i.png');
  lNode.Attrs.Add('alt','alt text');
  TestRender('<img src="i.png" alt="alt text">');
end;


procedure TTestHTMLRender.TestTextBlockLinkWithChildren;

var
  lBlock : TMarkDownTextBlock;
  lNode : TMarkDownTextNode;

begin
  lBlock:=CreateTextBlock(Document,'a','');
  lNode:=AppendTextNode(lBlock,'a ',[]);
  lNode.Kind:=nkURI;
  lNode.Attrs.Add('href','u');
  lNode.Children.AddTextNode(lNode.Pos,nkText,'b').AddStyle(nsEmph);
  lNode.Children.AddTextNode(lNode.Pos,nkText,' c');
  TestRender('<a href="u">a <i>b</i> c</a>');
end;


procedure TTestHTMLRender.TestTextBlockLineBreak;

var
  lBlock : TMarkDownTextBlock;

begin
  lBlock:=CreateTextBlock(Document,'a','a');
  lBlock.Nodes.AddTextNode(lBlock.Nodes[0].Pos,nkLineBreak,'');
  AppendTextNode(lBlock,'b',[]);
  TestRender('a<br />b');
end;


procedure TTestHTMLRender.TestTextBlockTextEscaping;

begin
  CreateTextBlock(Document,'a','5 < 7 & "q" > 3');
  TestRender('5 &lt; 7 &amp; &quot;q&quot; &gt; 3');
end;


procedure TTestHTMLRender.TestTextBlockAttributeEscaping;

var
  lBlock : TMarkDownTextBlock;
  lNode : TMarkDownTextNode;

begin
  lBlock:=CreateTextBlock(Document,'a','');
  lNode:=AppendTextNode(lBlock,'a',[]);
  lNode.Kind:=nkURI;
  lNode.Attrs.Add('href','http://x/?p=1&q=2');
  TestRender('<a href="http://x/?p=1&amp;q=2">a</a>');
end;


procedure TTestHTMLRender.TestCodeBlock;

var
  lCode : TMarkDownCodeBlock;

begin
  lCode:=TMarkDownCodeBlock.Create(Document,1);
  CreateTextBlock(lCode,'a','a');
  CreateTextBlock(lCode,'b','b');
  TestRender(sLineBreak+'<pre><code>a'+sLineBreak+'b'+sLineBreak+'</code></pre>');
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


{ TTestHTMLRenderExtensions }

procedure TTestHTMLRenderExtensions.SetUp;

begin
  FRenderer:=TMarkDownHTMLRenderer.Create(Nil);
end;


procedure TTestHTMLRenderExtensions.TearDown;

begin
  FreeAndNil(FRenderer);
end;


procedure TTestHTMLRenderExtensions.DoMarker(aRenderer: TMarkdownRenderer; const aName, aArgument, aValue: String);

begin
  aRenderer.WriteRaw('<a id="'+aName+'-'+aArgument+'-'+aValue+'"></a>');
end;


procedure TTestHTMLRenderExtensions.DoResolveLink(aRenderer: TMarkdownRenderer; var aHref, aText: String; out aHandled: Boolean);

begin
  aHandled:=False;
  if aHref='chapter.md#intro' then
    aHref:='chapter.html#intro';
  if aText='' then
    aText:='section 1.2';
end;


procedure TTestHTMLRenderExtensions.DoHeadingNumber(aRenderer: TMarkdownRenderer; aBlock: TMarkdownBlock; out aNumber: String);

begin
  aNumber:='1.'+IntToStr(aBlock.Line);
end;


procedure TTestHTMLRenderExtensions.DoCaptionNumber(aRenderer: TMarkdownRenderer; aBlock: TMarkdownBlock; out aLabel: String);

begin
  if aBlock is TMarkdownTableBlock then
    aLabel:='Table 2.1'
  else
    aLabel:='Figure 3.4';
end;


function TTestHTMLRenderExtensions.Render(const aMarkdown: String; aOptions: TMarkdownOptions): String;

var
  lSource : TStringList;
  lDoc : TMarkdownDocument;
begin
  lSource:=TStringList.Create;
  try
    lSource.Text:=aMarkdown;
    lDoc:=TMarkdownParser.FastParse(lSource,aOptions);
    try
      Result:=FRenderer.RenderHTML(lDoc);
    finally
      lDoc.Free;
    end;
  finally
    lSource.Free;
  end;
end;


procedure TTestHTMLRenderExtensions.CheckRender(const aMarkdown, aHTML: String);

begin
  AssertEquals('Correct html',aHTML,Render(aMarkdown));
end;


procedure TTestHTMLRenderExtensions.TestHeadingAttributes;

begin
  CheckRender('## Title {#t .c1 .c2 data-x=1}','<h2 id="t" class="c1 c2" data-x="1">Title</h2>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestSetextHeadingID;

begin
  CheckRender('A title'#10'===','<h1 id="a-title">A title</h1>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestCodeAttributes;

begin
  CheckRender('```pascal {#lst .numbered}'#10'begin'#10'```',
              sLineBreak+'<pre id="lst" class="numbered"><code class="language-pascal">begin'+sLineBreak+'</code></pre>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestCommentWritesNothing;

begin
  CheckRender('<!-- block -->'#10#10'Text <!-- inline: marker --> here',
              '<p>Text  here</p>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestMarkerEvent;

begin
  FRenderer.OnMarker:=@DoMarker;
  CheckRender('<!-- index[x]: Block -->'#10#10'A<!-- index: Inline --> b',
              '<a id="index-x-Block"></a><p>A<a id="index--Inline"></a> b</p>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestDefinitionListTight;

begin
  CheckRender('Term'#10': Definition'#10': Second',
              '<dl>'+sLineBreak+'<dt>Term</dt>'+sLineBreak+'<dd>Definition</dd>'+sLineBreak+
              '<dd>Second</dd>'+sLineBreak+'</dl>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestDefinitionListLoose;

begin
  CheckRender('Term'#10#10': Definition',
              '<dl>'+sLineBreak+'<dt>Term</dt>'+sLineBreak+'<dd>'+sLineBreak+'<p>Definition</p>'+sLineBreak+
              '</dd>'+sLineBreak+'</dl>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestAlert;

begin
  CheckRender('> [!WARNING]'#10'> Careful',
              '<div class="alert alert-warning">'+sLineBreak+'<p class="alert-title">Warning</p>'+sLineBreak+
              '<p>Careful</p>'+sLineBreak+'</div>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestAlertTitle;

begin
  FRenderer.AlertTitles[atNote]:='Remark';
  AssertTrue('Note title changed',Pos('<p class="alert-title">Remark</p>',Render('> [!NOTE]'#10'> x'))>0);
end;


procedure TTestHTMLRenderExtensions.TestFootnotes;

begin
  CheckRender('Text.[^n]'#10#10'[^n]: The note.',
              '<p>Text.<sup><a href="#fn-n" id="fnref-n">1</a></sup></p>'+sLineBreak+
              '<section class="footnotes">'+sLineBreak+'<ol>'+sLineBreak+'<li id="fn-n">'+sLineBreak+
              '<p>The note.</p>'+sLineBreak+
              '<a href="#fnref-n" class="footnote-backref">&#8617;</a>'+sLineBreak+'</li>'+sLineBreak+
              '</ol>'+sLineBreak+'</section>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestTableCaption;

var
  lHTML : String;
begin
  FRenderer.OnCaptionNumber:=@DoCaptionNumber;
  lHTML:=Render('| a |'#10'|---|'#10'| 1 |'#10#10'Table: Values {#tab}');
  AssertTrue('Table with id',Pos('<table id="tab">',lHTML)=1);
  AssertTrue('Caption with label',Pos('<caption>Table 2.1: Values</caption>',lHTML)>0);
end;


procedure TTestHTMLRenderExtensions.TestTableHeaderCells;

var
  lHTML : String;
begin
  lHTML:=Render('| a |'#10'|---|'#10'| 1 |');
  AssertTrue('Header cell is th: '+lHTML,Pos('<th>a</th>',lHTML)>0);
  AssertTrue('Body cell is td: '+lHTML,Pos('<td>1</td>',lHTML)>0);
end;


procedure TTestHTMLRenderExtensions.TestFigure;

begin
  FRenderer.OnCaptionNumber:=@DoCaptionNumber;
  CheckRender('![A *figure*](pic.png){#fig width=50%}',
              '<figure id="fig">'+sLineBreak+'<img src="pic.png" alt="A figure" width="50%">'+sLineBreak+
              '<figcaption>Figure 3.4: A <i>figure</i></figcaption>'+sLineBreak+'</figure>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestEmptyLinkWritesTarget;

begin
  CheckRender('See [](other.md#x).','<p>See <a href="other.md#x">other.md#x</a>.</p>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestResolveLink;

begin
  FRenderer.OnResolveLink:=@DoResolveLink;
  CheckRender('See [](chapter.md#intro) and [text](chapter.md#intro).',
              '<p>See <a href="chapter.html#intro">section 1.2</a> and <a href="chapter.html#intro">text</a>.</p>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestHeadingNumber;

begin
  FRenderer.OnHeadingNumber:=@DoHeadingNumber;
  CheckRender('# Title','<h1 id="title">1.1 Title</h1>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestRendererFallback;

var
  lDoc : TMarkdownDocument;
  lPar : TMyParagraphBlock;
  lText : TMarkdownTextBlock;
  lPos : TPosition;
begin
  lDoc:=TMarkdownDocument.Create(Nil,1);
  try
    lPar:=TMyParagraphBlock.Create(lDoc,1);
    lText:=TMarkdownTextBlock.Create(lPar,1,'x');
    lText.Nodes:=TMarkdownTextNodeList.Create(True);
    lPos.Line:=1;
    lPos.Col:=1;
    lText.Nodes.AddTextNode(lPos,nkText,'x');
    AssertEquals('Rendered by the paragraph renderer','<p>x</p>'+sLineBreak,FRenderer.RenderHTML(lDoc));
  finally
    lDoc.Free;
  end;
end;

initialization
  Registertest(TTestHTMLRender);
  Registertest(TTestHTMLRenderExtensions);
end.

