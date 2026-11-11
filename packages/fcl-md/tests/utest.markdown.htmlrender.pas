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
{$codepage utf8}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Registry,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
{$ENDIF}
  MarkDown.Elements,
  Markdown.Render,
  Markdown.Parser,
  Markdown.Utils, MarkDown.HtmlRender;

type

  { TTestHTMLRender }

  TTestHTMLRender = Class(TTestCase)
  private
    FHTMLRenderer : TMarkDownHTMLRenderer;
    FDocument: TMarkDownDocument;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    function CreateTextBlock(aParent: TMarkdownBlock; const aText,aTextNode: TMarkdownString; aNodeStyle : TNodeStyles=[]): TMarkDownTextBlock;
    function CreateParagraphBlock(const aTextNode: TMarkdownString): TMarkdownBlock;
    function CreateQuotedBlock(const aTextNode: TMarkdownString): TMarkdownBlock;
    function CreateHeadingBlock(const aTextNode: TMarkdownString; aLevel : integer): TMarkdownBlock;
    function CreateListBlock(aOrdered : boolean; const aListItemText : TMarkdownString): TMarkDownListBlock;
    function CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: TMarkdownString): TMarkDownListItemBlock;
    function AppendTextNode(aBlock: TMarkDownTextBlock; const aText: TMarkdownString; aNodeStyle : TNodeStyles) : TMarkDownTextNode;
    procedure TestRender(const aHTML : TMarkdownString);
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

  { TNoteDirectiveRenderer }

  // Writes note directives as aside and em elements
  TNoteDirectiveRenderer = class(THTMLDirectiveRenderer)
  public
    procedure RenderBlock(aBlock : TMarkdownDirectiveBlock); override;
    procedure RenderInline(aTextRenderer : THTMLMarkdownTextRenderer; aNode : TMarkdownTextNode); override;
  end;

  { TTestHTMLRenderExtensions }

  TTestHTMLRenderExtensions = Class(TTestCase)
  private
    FRenderer : TMarkDownHTMLRenderer;
    procedure DoMarker(aRenderer : TMarkdownRenderer; const aName, aArgument, aValue : TMarkdownString);
    procedure DoResolveLink(aRenderer : TMarkdownRenderer; var aHref, aText : TMarkdownString; out aHandled : Boolean);
    procedure DoHeadingNumber(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aNumber : TMarkdownString);
    procedure DoCaptionNumber(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aLabel : TMarkdownString);
    procedure DoResolveWikiLink(aRenderer : TMarkdownRenderer; const aTarget, aFragment : TMarkdownString;
                                var aHref, aClass : TMarkdownString; var aExists : Boolean);
    procedure DoResolveWikiLinkNoHref(aRenderer : TMarkdownRenderer; const aTarget, aFragment : TMarkdownString;
                                      var aHref, aClass : TMarkdownString; var aExists : Boolean);
  protected
    function Render(const aMarkdown : TMarkdownString; aOptions : TMarkdownOptions = MarkdownDocExtensions) : TMarkdownString;
    procedure CheckRender(const aMarkdown, aHTML : TMarkdownString);
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
  Published
    procedure TestHeadingAttributes;
    procedure TestNonAsciiText;
    procedure TestNonAsciiEntity;
    procedure TestNonAsciiLink;
    procedure TestNonAsciiLinkReference;
    procedure TestNonAsciiToStrings;
    procedure TestSetextHeadingID;
    procedure TestCodeAttributes;
    procedure TestCommentWritesNothing;
    procedure TestMarkerEvent;
    procedure TestDefinitionListTight;
    procedure TestDefinitionListLoose;
    procedure TestAlert;
    procedure TestAlertTitle;
    procedure TestSuccessAlert;
    procedure TestAdmonition;
    procedure TestAdmonitionDefaultTitle;
    procedure TestFootnotes;
    procedure TestTableCaption;
    procedure TestTableHeaderCells;
    procedure TestFigure;
    procedure TestEmptyLinkWritesTarget;
    procedure TestResolveLink;
    procedure TestHeadingNumber;
    procedure TestRendererFallback;
    procedure TestTaskListTight;
    procedure TestTaskListLoose;
    procedure TestTaskListOrdered;
    procedure TestDirectiveContainer;
    procedure TestDirectiveNested;
    procedure TestDirectiveEndsTable;
    procedure TestLinkTextIsURL;
    procedure TestAutolinkTrailingPunctuation;
    procedure TestNumberWithDotOnly;
    procedure TestDirectiveLeaf;
    procedure TestDirectiveInline;
    procedure TestDirectiveInlineNameOnlyIsText;
    procedure TestDirectiveAttributesAsData;
    procedure TestDirectiveRegisteredRenderer;
    procedure TestDirectiveUnregisteredRenderer;
    procedure TestWikiLinkDefault;
    procedure TestWikiLinkResolver;
    procedure TestWikiLinkWithoutHref;
    procedure TestRawHTMLEscaped;
    procedure TestEmojiShortcodes;
  end;

implementation

uses
  Markdown.Processors, Markdown.Emoji;

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


function TTestHTMLRender.CreateTextBlock(aParent: TMarkdownBlock; const aText, aTextNode: TMarkdownString; aNodeStyle: TNodeStyles): TMarkDownTextBlock;

begin
  Result:=TMarkDownTextBlock.Create(aParent,1,aText);
  if aTextNode<>'' then
    AppendTextNode(Result,aTextNode,aNodeStyle);
end;

function TTestHTMLRender.CreateParagraphBlock(const aTextNode: TMarkdownString): TMarkdownBlock;

begin
  Result:=TMarkDownParagraphBlock.Create(FDocument,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestHTMLRender.CreateQuotedBlock(const aTextNode: TMarkdownString): TMarkdownBlock;

begin
  Result:=TMarkDownQuoteBlock.Create(FDocument,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestHTMLRender.CreateHeadingBlock(const aTextNode: TMarkdownString; aLevel: integer): TMarkdownBlock;

begin
  Result:=TMarkDownHeadingBlock.Create(FDocument,1,aLevel);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestHTMLRender.CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: TMarkdownString): TMarkDownListItemBlock;

var
  lPar : TMarkDownParagraphBlock;
begin
  Result:=TMarkDownListItemBlock.Create(aParent,1);
  lPar:=TMarkDownParagraphBlock.Create(Result,1);
  CreateTextBlock(lPar,'',aText);
end;


function TTestHTMLRender.CreateListBlock(aOrdered: boolean; const aListItemText: TMarkdownString): TMarkDownListBlock;

begin
  Result:=TMarkDownListBlock.Create(FDocument,1);
  Result.ordered:=aOrdered;
  if aListItemText<>'' then
    CreateListItemBlock(Result,aListItemText);
end;

function TTestHTMLRender.AppendTextNode(aBlock: TMarkDownTextBlock; const aText: TMarkdownString; aNodeStyle: TNodeStyles): TMarkDownTextNode;

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

procedure TTestHTMLRender.TestRender(const aHTML: TMarkdownString);

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


procedure TTestHTMLRenderExtensions.DoMarker(aRenderer: TMarkdownRenderer; const aName, aArgument, aValue: TMarkdownString);

begin
  aRenderer.WriteRaw('<a id="'+aName+'-'+aArgument+'-'+aValue+'"></a>');
end;


procedure TTestHTMLRenderExtensions.DoResolveLink(aRenderer: TMarkdownRenderer; var aHref, aText: TMarkdownString; out aHandled: Boolean);

begin
  aHandled:=False;
  if aHref='chapter.md#intro' then
    aHref:='chapter.html#intro';
  if aText='' then
    aText:='section 1.2';
end;


procedure TTestHTMLRenderExtensions.DoHeadingNumber(aRenderer: TMarkdownRenderer; aBlock: TMarkdownBlock; out aNumber: TMarkdownString);

begin
  aNumber:='1.'+IntToStr(aBlock.Line);
end;


procedure TTestHTMLRenderExtensions.DoCaptionNumber(aRenderer: TMarkdownRenderer; aBlock: TMarkdownBlock; out aLabel: TMarkdownString);

begin
  if aBlock is TMarkdownTableBlock then
    aLabel:='Table 2.1'
  else
    aLabel:='Figure 3.4';
end;


function TTestHTMLRenderExtensions.Render(const aMarkdown: TMarkdownString; aOptions: TMarkdownOptions): TMarkdownString;

var
  lSource : TStringList;
  lDoc : TMarkdownDocument;
begin
  lSource:=TStringList.Create;
  try
    lSource.Text:=MarkdownToString(aMarkdown);
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


procedure TTestHTMLRenderExtensions.CheckRender(const aMarkdown, aHTML: TMarkdownString);

begin
  AssertEquals('Correct html',aHTML,Render(aMarkdown));
end;


procedure TTestHTMLRenderExtensions.TestNonAsciiText;

begin
  CheckRender(UnicodeToMarkdown('Ünïcödé *日本語* 😀'),UnicodeToMarkdown('<p>Ünïcödé <i>日本語</i> 😀</p>'+sLineBreak));
end;


procedure TTestHTMLRenderExtensions.TestNonAsciiEntity;

begin
  CheckRender('caf&eacute; &#x1F600;',UnicodeToMarkdown('<p>café 😀</p>'+sLineBreak));
end;


procedure TTestHTMLRenderExtensions.TestNonAsciiLink;

begin
  CheckRender(UnicodeToMarkdown('[Grüße](/grüße)'),UnicodeToMarkdown('<p><a href="/gr%C3%BC%C3%9Fe">Grüße</a></p>'+sLineBreak));
end;


procedure TTestHTMLRenderExtensions.TestNonAsciiLinkReference;

begin
  CheckRender(UnicodeToMarkdown('[über]: /u'#10#10'[ÜBER]'),UnicodeToMarkdown('<p><a href="/u">ÜBER</a></p>'+sLineBreak));
end;


procedure TTestHTMLRenderExtensions.TestNonAsciiToStrings;

var
  lSource,lDest : TStringList;
  lDoc : TMarkdownDocument;

begin
  lDest:=Nil;
  lDoc:=Nil;
  lSource:=TStringList.Create;
  try
    lDest:=TStringList.Create;
    lSource.Text:=MarkdownToString(UnicodeToMarkdown('Grüße 日本語'));
    lDoc:=TMarkdownParser.FastParse(lSource,[]);
    FRenderer.RenderDocument(lDoc,lDest);
    AssertEquals('One line',1,lDest.Count);
    AssertEquals('Native string of the HTML',MarkdownToString(UnicodeToMarkdown('<p>Grüße 日本語</p>')),lDest[0]);
  finally
    lDoc.Free;
    lDest.Free;
    lSource.Free;
  end;
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


procedure TTestHTMLRenderExtensions.TestSuccessAlert;

begin
  CheckRender('> [!SUCCESS]'#10'> It worked',
              '<div class="alert alert-success">'+sLineBreak+'<p class="alert-title">Success</p>'+sLineBreak+
              '<p>It worked</p>'+sLineBreak+'</div>'+sLineBreak);
end;


procedure TTestHTMLRenderExtensions.TestAdmonition;

begin
  AssertEquals('Admonition with inline title',
               '<div class="alert alert-caution">'+sLineBreak+'<p class="alert-title">Do <i>not</i> &amp; stop</p>'+sLineBreak+
               '<p>Careful</p>'+sLineBreak+'</div>'+sLineBreak,
               Render('!!! danger "Do *not* & stop"'#10'    Careful',MarkdownDocExtensions+[mdoAdmonitions]));
end;


procedure TTestHTMLRenderExtensions.TestAdmonitionDefaultTitle;

begin
  AssertEquals('Admonition with default title',
               '<div class="alert alert-tip">'+sLineBreak+'<p class="alert-title">Tip</p>'+sLineBreak+
               '<p>Body</p>'+sLineBreak+'</div>'+sLineBreak,
               Render('!!! hint'#10'    Body',MarkdownDocExtensions+[mdoAdmonitions]));
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
  lHTML : TMarkdownString;
begin
  FRenderer.OnCaptionNumber:=@DoCaptionNumber;
  lHTML:=Render('| a |'#10'|---|'#10'| 1 |'#10#10'Table: Values {#tab}');
  AssertTrue('Table with id',Pos('<table id="tab">',lHTML)=1);
  AssertTrue('Caption with label',Pos('<caption>Table 2.1: Values</caption>',lHTML)>0);
end;


procedure TTestHTMLRenderExtensions.TestTableHeaderCells;

var
  lHTML : TMarkdownString;
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


procedure TTestHTMLRenderExtensions.TestEmojiShortcodes;

begin
  TEmojiRegistry.Register('htmltestemoji','<E>');
  TEmojiRegistry.RegisterMarkup('htmltesticon','<i class="icon"></i>');
  try
    AssertEquals('Text escaped, markup as is','<p>&lt;E&gt; <i class="icon"></i> :unknown:</p>'+sLineBreak,
                 Render(':htmltestemoji: :htmltesticon: :unknown:'));
  finally
    TEmojiRegistry.UnRegister('htmltestemoji');
    TEmojiRegistry.UnRegister('htmltesticon');
  end;
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

procedure TTestHTMLRenderExtensions.DoResolveWikiLink(aRenderer: TMarkdownRenderer; const aTarget, aFragment: TMarkdownString;
  var aHref, aClass: TMarkdownString; var aExists: Boolean);

begin
  aHref:='/wiki/'+StringReplace(aTarget,' ','_',[rfReplaceAll])+'.html';
  if aFragment<>'' then
    aHref:=aHref+'#'+aFragment;
  aExists:=aTarget<>'Missing';
  if aTarget='Styled' then
    aClass:='internal';
end;


procedure TTestHTMLRenderExtensions.DoResolveWikiLinkNoHref(aRenderer: TMarkdownRenderer; const aTarget, aFragment: TMarkdownString;
  var aHref, aClass: TMarkdownString; var aExists: Boolean);

begin
  aHref:='';
end;


procedure TTestHTMLRenderExtensions.TestTaskListTight;

begin
  AssertEquals('Correct html',
               '<ul class="contains-task-list">'+sLineBreak+
               '<li class="task-list-item"><input type="checkbox" class="task-list-item-checkbox" disabled="" /> todo</li>'+sLineBreak+
               '<li class="task-list-item"><input type="checkbox" class="task-list-item-checkbox" disabled="" checked="" /> done</li>'+sLineBreak+
               '<li>plain</li>'+sLineBreak+
               '</ul>'+sLineBreak,
               Render('- [ ] todo'#10'- [x] done'#10'- plain',[mdoTaskLists]));
end;


procedure TTestHTMLRenderExtensions.TestTaskListLoose;

begin
  AssertEquals('Correct html',
               '<ul class="contains-task-list">'+sLineBreak+
               '<li class="task-list-item">'+sLineBreak+
               '<p><input type="checkbox" class="task-list-item-checkbox" disabled="" /> todo</p>'+sLineBreak+
               '<p>more</p>'+sLineBreak+
               '</li>'+sLineBreak+
               '<li class="task-list-item">'+sLineBreak+
               '<p><input type="checkbox" class="task-list-item-checkbox" disabled="" checked="" /> done</p>'+sLineBreak+
               '</li>'+sLineBreak+
               '</ul>'+sLineBreak,
               Render('- [ ] todo'#10#10'  more'#10'- [x] done',[mdoTaskLists]));
end;


procedure TTestHTMLRenderExtensions.TestTaskListOrdered;

begin
  AssertEquals('Correct html',
               '<ol start="3" class="contains-task-list">'+sLineBreak+
               '<li class="task-list-item"><input type="checkbox" class="task-list-item-checkbox" disabled="" checked="" /> done</li>'+sLineBreak+
               '</ol>'+sLineBreak,
               Render('3. [x] done',[mdoTaskLists]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveContainer;

begin
  AssertEquals('Correct html',
               '<div data-directive="note" id="n" class="c1 c2" data-level="2">'+sLineBreak+
               '<p data-directive-label="">The <i>title</i></p>'+sLineBreak+
               '<p>Text</p>'+sLineBreak+
               '</div>'+sLineBreak,
               Render(':::note[The *title*]{#n .c1 .c2 level=2}'#10'Text'#10':::',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveEndsTable;

var
  lHTML : TMarkdownString;

begin
  lHTML:=Render(':::note'#10'| a |'#10'| --- |'#10'| b |'#10':::'#10'after',MarkdownDocExtensions+[mdoDirectives]);
  AssertTrue('table inside the directive: '+lHTML,Pos('<div data-directive="note">'+sLineBreak+'<table>',lHTML)>0);
  AssertTrue('fence is not a table row: '+lHTML,Pos(':::',lHTML)=0);
  AssertTrue('text after the directive: '+lHTML,Pos('</div>'+sLineBreak+'<p>after</p>',lHTML)>0);
end;


procedure TTestHTMLRenderExtensions.TestLinkTextIsURL;

begin
  AssertEquals('Correct html','<p><a href="https://x.org">https://x.org</a></p>'+sLineBreak,
               Render('[https://x.org](https://x.org)'));
  AssertEquals('Correct html','<p>See <a href="http://www.x.org">www.x.org</a>.</p>'+sLineBreak,
               Render('See [www.x.org](http://www.x.org).'));
end;


procedure TTestHTMLRenderExtensions.TestAutolinkTrailingPunctuation;

begin
  AssertEquals('Correct html','<p>Visit <a href="https://x.org">https://x.org</a>.</p>'+sLineBreak,
               Render('Visit https://x.org.'));
end;


procedure TTestHTMLRenderExtensions.TestNumberWithDotOnly;

begin
  AssertEquals('Correct html','<p>1.</p>'+sLineBreak,Render('1.'));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveNested;

begin
  AssertEquals('Correct html',
               '<div data-directive="outer">'+sLineBreak+
               '<div data-directive="inner">'+sLineBreak+
               '<p>a</p>'+sLineBreak+
               '</div>'+sLineBreak+
               '</div>'+sLineBreak,
               Render('::::outer'#10':::inner'#10'a'#10':::'#10'::::',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveLeaf;

begin
  AssertEquals('Correct html',
               '<div data-directive="video" data-src="clip.mp4">My clip</div>'+sLineBreak,
               Render('::video[My clip]{src=clip.mp4}',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveInline;

begin
  AssertEquals('Correct html',
               '<p>Press <span data-directive="kbd" class="key">Ctrl <b>C</b></span> now</p>'+sLineBreak,
               Render('Press :kbd[Ctrl **C**]{.key} now',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveInlineNameOnlyIsText;

begin
  AssertEquals('Correct html','<p>unit:SysUtils &amp; a:b</p>'+sLineBreak,Render('unit:SysUtils & a:b',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveAttributesAsData;

begin
  AssertEquals('Correct html',
               '<p><span data-directive="x" data-onclick="alert(1)" data-style="a&quot;b">y</span></p>'+sLineBreak,
               Render(':x[y]{onclick=alert(1) Style=''a"b''}',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveRegisteredRenderer;

begin
  FRenderer.RegisterDirectiveRenderer('note',TNoteDirectiveRenderer);
  AssertEquals('Correct html',
               '<aside class="note">'+sLineBreak+
               '<p>See <em class="note">this</em> and <span data-directive="other">that</span></p>'+sLineBreak+
               '</aside>'+sLineBreak,
               Render(':::note'#10'See :note[this] and :other[that]'#10':::',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestDirectiveUnregisteredRenderer;

begin
  FRenderer.RegisterDirectiveRenderer('note',TNoteDirectiveRenderer);
  FRenderer.RegisterDirectiveRenderer('note',Nil);
  AssertEquals('Default renderer class',THTMLDirectiveRenderer,FRenderer.FindDirectiveRenderer('note'));
  AssertEquals('Correct html','<div data-directive="note"></div>'+sLineBreak,Render('::note',[mdoDirectives]));
end;


procedure TTestHTMLRenderExtensions.TestWikiLinkDefault;

begin
  AssertEquals('Correct html',
               '<p>See <a href="Main%20Page#The%20intro">the intro</a> and <a href="FPC">FPC</a></p>'+sLineBreak,
               Render('See [[Main Page#The intro|the intro]] and [[FPC]]',[mdoWikiLinks]));
end;


procedure TTestHTMLRenderExtensions.TestWikiLinkResolver;

begin
  FRenderer.OnResolveWikiLink:=@DoResolveWikiLink;
  AssertEquals('Correct html',
               '<p><a href="/wiki/Main_Page.html#Intro">Main Page#Intro</a> '+
               '<a href="/wiki/Missing.html" class="missing">Missing</a> '+
               '<a href="/wiki/Styled.html" class="internal"><i>S</i></a></p>'+sLineBreak,
               Render('[[Main Page#Intro]] [[Missing]] [[Styled|*S*]]',[mdoWikiLinks]));
end;


procedure TTestHTMLRenderExtensions.TestWikiLinkWithoutHref;

begin
  FRenderer.OnResolveWikiLink:=@DoResolveWikiLinkNoHref;
  AssertEquals('Correct html','<p><span>Page</span></p>'+sLineBreak,Render('[[Page]]',[mdoWikiLinks]));
end;


procedure TTestHTMLRenderExtensions.TestRawHTMLEscaped;

var
  lHTML : TMarkdownString;
begin
  lHTML:=Render('<div onclick="x">'#10#10'<b>bold</b> <script>alert(1)</script> &lt;i&gt;',
                MarkdownDocExtensions+[mdoDirectives,mdoWikiLinks,mdoTaskLists]);
  AssertEquals('No div tag',0,Pos('<div',lHTML));
  AssertEquals('No b tag',0,Pos('<b>',lHTML));
  AssertEquals('No script tag',0,Pos('<script',lHTML));
  AssertEquals('No i tag',0,Pos('<i>',lHTML));
  AssertTrue('Block tag as text',Pos('&lt;div onclick=&quot;x&quot;&gt;',lHTML)>0);
  AssertTrue('Inline tag as text',Pos('&lt;script&gt;alert(1)&lt;/script&gt;',lHTML)>0);
end;

{ TNoteDirectiveRenderer }

procedure TNoteDirectiveRenderer.RenderBlock(aBlock: TMarkdownDirectiveBlock);

begin
  AppendNL('<aside class="note">');
  Renderer.RenderChildren(aBlock);
  AppendNL('</aside>');
end;


procedure TNoteDirectiveRenderer.RenderInline(aTextRenderer: THTMLMarkdownTextRenderer; aNode: TMarkdownTextNode);

begin
  Append('<em class="note">');
  aTextRenderer.RenderChildren(aNode);
  Append('</em>');
end;

initialization
  Registertest(TTestHTMLRender);
  Registertest(TTestHTMLRenderExtensions);
end.

