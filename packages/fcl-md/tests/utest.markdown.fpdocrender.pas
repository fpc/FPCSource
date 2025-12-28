{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown FPDoc renderer tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit UTest.Markdown.FPDocRender;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  MarkDown.Elements,
  MarkDown.FPDocRender;

type

  { TTestFPDocRender }

  TTestFPDocRender = Class(TTestCase)
  private
    FFPDocRenderer : TMarkDownFPDocRenderer;
    FDocument: TMarkDownDocument;
    FParent : TMarkDownBlock;
  Public
    const
      cIndent    = '        ';
      cNLIndent  = sLineBreak+cIndent;
      cNLIndent2  = sLineBreak+cIndent+'  ';
      cNLIndent4  = sLineBreak+cIndent+'    ';

    Procedure SetUp; override;
    Procedure TearDown; override;
    Procedure StartDoc;
    function CreateTextBlock(aParent: TMarkdownBlock; const aText,aTextNode: string; aNodeStyle : TNodeStyles=[]): TMarkDownTextBlock;
    function CreateParagraphBlock(const aTextNode: string): TMarkdownBlock;
    function CreateQuotedBlock(const aTextNode: string): TMarkdownBlock;
    function CreateHeadingBlock(const aTextNode: string; aLevel : integer): TMarkdownBlock;
    function CreateListBlock(aOrdered : boolean; const aListItemText : string): TMarkDownListBlock;
    function CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: string): TMarkDownListItemBlock;
    function AppendTextNode(aBlock: TMarkDownTextBlock; const aText: string; aNodeStyle : TNodeStyles) : TMarkDownTextNode;
    procedure TestRender(const aFPDoc : string; aEnvelope : boolean = False);
    Property Renderer : TMarkDownFPDocRenderer Read FFPDocRenderer;
    Property Document : TMarkDownDocument Read FDocument;
  Published
    procedure TestHookup;
    procedure TestEmpty;
    procedure TestEmptyPackageName;
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
    procedure TestUnorderedListEmpty;
    procedure TestUnorderedListOneItem;
  end;

implementation

{ TTestFPDocRender }

procedure TTestFPDocRender.SetUp;

begin
  FFPDocRenderer:=TMarkDownFPDocRenderer.Create(Nil);
  FDocument:=TMarkDownDocument.Create(Nil,1);
  FParent:=FDocument;
end;


procedure TTestFPDocRender.TearDown;

begin
  FParent:=nil;
  FreeAndNil(FDocument);
  FreeAndNil(FFPDocRenderer);
end;

procedure TTestFPDocRender.StartDoc;

begin
  CreateHeadingBlock('unit1',1);
  CreateHeadingBlock('a',2);
  CreateHeadingBlock('descr',3);
end;


function TTestFPDocRender.CreateTextBlock(aParent: TMarkdownBlock; const aText, aTextNode: string; aNodeStyle: TNodeStyles): TMarkDownTextBlock;

begin
  Result:=TMarkDownTextBlock.Create(aParent,1,aText);
  if aTextNode<>'' then
    AppendTextNode(Result,aTextNode,aNodeStyle);
end;

function TTestFPDocRender.CreateParagraphBlock(const aTextNode: string): TMarkdownBlock;

begin
  Result:=TMarkDownParagraphBlock.Create(FParent,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestFPDocRender.CreateQuotedBlock(const aTextNode: string): TMarkdownBlock;

begin
  Result:=TMarkDownQuoteBlock.Create(FParent,1);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestFPDocRender.CreateHeadingBlock(const aTextNode: string; aLevel: integer): TMarkdownBlock;

begin
  Result:=TMarkDownHeadingBlock.Create(FParent,1,aLevel);
  if aTextNode<>'' then
    CreateTextBlock(Result,aTextNode,aTextNode);
end;

function TTestFPDocRender.CreateListItemBlock(aParent: TMarkDownContainerBlock; const aText: string): TMarkDownListItemBlock;

var
  lPar : TMarkDownParagraphBlock;
begin
  Result:=TMarkDownListItemBlock.Create(aParent,1);
  lPar:=TMarkDownParagraphBlock.Create(Result,1);
  CreateTextBlock(lPar,'',aText);
end;


function TTestFPDocRender.CreateListBlock(aOrdered: boolean; const aListItemText: string): TMarkDownListBlock;

begin
  Result:=TMarkDownListBlock.Create(FParent,1);
  Result.ordered:=aOrdered;
  if aListItemText<>'' then
    CreateListItemBlock(Result,aListItemText);
end;

function TTestFPDocRender.AppendTextNode(aBlock: TMarkDownTextBlock; const aText: string; aNodeStyle: TNodeStyles): TMarkDownTextNode;

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

procedure TTestFPDocRender.TestRender(const aFPDoc: string; aEnvelope: boolean);

const
  prefix = '<?xml version="1.0" encoding="utf-8"?>'+sLineBreak
             +'<fpdoc-descriptions>'+sLineBreak
             +'  <package>'+sLineBreak
             +'    <module name="unit1">'+sLineBreak
             +'      <element name="a">'+sLineBreak;

  prefixEmpty = prefix
             + '        <descr/>'+sLineBreak;

  prefixContent = prefix
             + '        <descr>';
  Suffix    = '      </element>'+sLineBreak
            + '    </module>'+sLineBreak
            + '  </package>'+sLineBreak
            + '</fpdoc-descriptions>';

  SuffixContent = '</descr>'+sLineBreak
            + Suffix;
  SuffixEmpty = Suffix;

var
  L : TStrings;
  lFPDoc: string;

begin
  L:=TstringList.Create;
  try
    L.SkipLastLineBreak:=True;
    Renderer.RenderDocument(FDocument,L);
    if not aEnvelope then
      lFPDoc:=aFPDoc
    else
      begin
      if aFPDoc='' then
        lFPDoc:=PrefixEmpty+SuffixEmpty
      else
        lFPDoc:=PrefixContent+aFpdoc+SuffixContent
      end;
    assertEquals('Correct FPDoc: ',lFPDoc,L.Text);
  finally
    L.Free;
  end;
end;


procedure TTestFPDocRender.TestHookup;

begin
  AssertNotNull('Have renderer',FFPDocRenderer);
  AssertNotNull('Have document',FDocument);
  AssertEquals('Have empty document',0,FDocument.blocks.Count);
end;


procedure TTestFPDocRender.TestEmpty;

begin
  TestRender('<?xml version="1.0" encoding="utf-8"?>'+sLineBreak
             +'<fpdoc-descriptions>'+sLineBreak
             +'  <package/>'+sLineBreak
             +'</fpdoc-descriptions>');
end;

procedure TTestFPDocRender.TestEmptyPackageName;

begin
  Renderer.PackageName:='a';
  TestRender('<?xml version="1.0" encoding="utf-8"?>'+sLineBreak
             +'<fpdoc-descriptions>'+sLineBreak
             +'  <package name="a"/>'+sLineBreak
             +'</fpdoc-descriptions>');
end;


procedure TTestFPDocRender.TestTextBlockEmpty;

begin
  StartDoc;
  CreateTextBlock(Document,'a','');
  TestRender('',True);
end;


procedure TTestFPDocRender.TestTextBlockText;

begin
  StartDoc;
  CreateTextBlock(Document,'a','a');
  TestRender('a',True);
end;


procedure TTestFPDocRender.TestTextBlockTextStrong;

begin
  StartDoc;
  CreateTextBlock(Document,'a','a',[nsStrong]);
  TestRender(cNlIndent2+'<b>a</b>'+CnlIndent,True);
end;


procedure TTestFPDocRender.TestTextBlockTextEmph;

begin
  StartDoc;
  CreateTextBlock(Document,'a','a',[nsEmph]);
  TestRender(cNlIndent2+'<i>a</i>'+cNlIndent,True);
end;


procedure TTestFPDocRender.TestTextBlockTextDelete;

begin
  StartDoc;
  CreateTextBlock(Document,'a','a',[nsDelete]);
  TestRender(cNlIndent2+'<u>a</u>'+cNlindent,True);
end;


procedure TTestFPDocRender.TestTextBlockTextStrongEmph;

begin
  StartDoc;
  CreateTextBlock(Document,'a','a',[nsStrong,nsEmph]);
  TestRender(cNlIndent2+'<b>'+cNLIndent4+'<i>a</i>'+cNlIndent2+'</b>'+cNlIndent,True);
end;


procedure TTestFPDocRender.TestTextBlockTextStrongEmphSplit1;

var
  lBlock : TMarkDownTextBlock;

begin
  StartDoc;
  lBlock:=CreateTextBlock(Document,'a','a ',[nsStrong]);
  AppendTextNode(lBlock,'b',[nsStrong,nsemph]);
  TestRender(cNlIndent2+'<b>a <i>b</i>'+cNlIndent2+'</b>'+cNlIndent,True);
end;


procedure TTestFPDocRender.TestTextBlockTextStrongEmphSplit2;

var
  lBlock : TMarkDownTextBlock;
begin
  StartDoc;
  lBlock:=CreateTextBlock(Document,'a','a',[nsEmph,nsStrong]);
  AppendTextNode(lBlock,' b',[nsStrong]);
  TestRender(cNlIndent2+'<b>'+cNlIndent4+'<i>a</i> b</b>'+cNlIndent,True);
end;


procedure TTestFPDocRender.TestPragraphBlockEmpty;

begin
  StartDoc;
  CreateParagraphBlock('');
  TestRender(cNlIndent2+'<p/>'+cNlIndent,true);
end;


procedure TTestFPDocRender.TestPragraphBlockText;

begin
  StartDoc;
  CreateParagraphBlock('a');
  TestRender(cNlIndent2+'<p>a</p>'+cNlIndent,true);
end;


procedure TTestFPDocRender.TestQuotedBlockEmpty;

begin
  StartDoc;
  CreateQuotedBlock('');
  TestRender(cNlIndent2+'<remark/>'+cNlIndent,true);
end;


procedure TTestFPDocRender.TestQuotedBlockText;

begin
  StartDoc;
  CreateQuotedBlock('a');
  TestRender(cNlIndent2+'<remark>a</remark>'+cNlIndent,true);
end;

procedure TTestFPDocRender.TestUnorderedListEmpty;

begin
  StartDoc;
  CreateListBlock(false,'');
  TestRender(cNlIndent2+'<ul/>'+cNlIndent,True);
end;


procedure TTestFPDocRender.TestUnorderedListOneItem;

begin
  StartDoc;
  CreateListBlock(false,'a');
  TestRender(cNlIndent2+'<ul>'+cNlIndent4+'<li>a</li>'+cNlIndent2+'</ul>'+cNlIndent,True);
end;


initialization
  Registertest(TTestFPDocRender);
end.

