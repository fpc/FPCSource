{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown Inline text processing tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit UTest.Markdown.InlineText;


{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Registry, System.Contnrs,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry, contnrs,
{$ENDIF}
  Markdown.Utils, Markdown.Elements, Markdown.Scanner, Markdown.InlineText, Markdown.Emoji;

type

  { TTestInlineTextProcessor }

  TTestInlineTextProcessor = class(TTestCase)
  private
    FScanner: TMarkDownTextScanner;
    FNodes: TMarkDownTextNodeList;
    FProcessor: TInlineTextProcessor;
    FEntities: TFPStringHashTable;

    procedure DumpNodes;
    procedure SetupProcessor(const AText: TMarkdownString; awsMode: TWhitespaceMode = wsTrim);
    function NodeAsText(AIndex: Integer): TMarkDownTextNode;
    function NodeAsNamed(AIndex: Integer; AExpectedKind: TTextNodeKind): TMarkDownTextNode;
    class procedure AssertEquals(const aMsg: TMarkdownString; aExpected,aActual : TTextNodeKind); overload;
    class procedure AssertEquals(const aMsg: TMarkdownString; aExpected,aActual : TNodeStyle); overload;
    class procedure AssertEquals(const aMsg: TMarkdownString; aExpected,aActual : TNodeStyles); overload;
    class procedure CheckStyleList(const aMsg: TMarkdownString; const aExpected : Array of TNodeStyle; aNode : TMarkdownTextNode);
    function DoEmojiLookup(const aName : TMarkdownString; out aReplacement : TMarkdownString) : Boolean;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleText;
    procedure TestNonAsciiEmphasis;
    procedure TestNonAsciiIntrawordEmphasis;
    procedure TestBackslashEscapes;
    procedure TestBareAmpersand;
    procedure TestNumericCharacterReference;
    procedure TestNumericCharacterReferenceAboveBMP;
    procedure TestCodeSpans;
    procedure TestEmphasisAndStrong;
    procedure TestEmphasisAndStrongInOne;
    procedure TestEmphasisAndStrongInOneSplit;
    procedure TestEmphasisAndStrongInOneSplit2;
    procedure TestStrikethroughGFM;
    procedure TestAutoLinks;
    procedure TestInlineLink;
    procedure TestInlineImage;
    procedure TestUnmatchedBrackets;
    procedure TestUnmatchedBracketsAfterStrong;
    procedure TestUnclosedBracket;
    procedure TestUnclosedImageBracket;
    procedure TestUnmatchedEmphasis;
    procedure TestNestedEmphasis;
    procedure TestInlineLinkWithMarkup;
    procedure TestInlineLinkWithNestedBrackets;
    procedure TestInlineImageWithMarkup;
    procedure TestHardLineBreakSpaces;
    procedure TestHardLineBreakBackslash;
    procedure TestTextIsNotEscaped;
    procedure TestInlineLinkUrlIsNotEscaped;
    procedure TestEmptyLinkText;
    procedure TestInlineComment;
    procedure TestInlineCommentMarker;
    procedure TestInlineCommentSpansLines;
    procedure TestEmptyComment;
    procedure TestUnclosedCommentIsText;
    procedure TestCommentOptionOff;
    procedure TestFootnoteReference;
    procedure TestFootnoteReferenceWithSpaceIsText;
    procedure TestFootnoteOptionOff;
    procedure TestReferenceLink;
    procedure TestReferenceImage;
    procedure TestReferenceLinkWithoutDocument;
    procedure TestInlineDirective;
    procedure TestInlineDirectiveIDAndClasses;
    procedure TestInlineDirectiveNameOnly;
    procedure TestInlineDirectiveAfterColonIsText;
    procedure TestInlineDirectiveDigitIsText;
    procedure TestInlineDirectiveEscapedIsText;
    procedure TestInlineDirectiveInvalidAttributes;
    procedure TestInlineDirectiveUnclosedLabel;
    procedure TestInlineDirectiveNestedLabel;
    procedure TestInlineDirectiveInEmphasis;
    procedure TestInlineDirectiveOptionOff;
    procedure TestWikiLink;
    procedure TestWikiLinkFragmentAndLabel;
    procedure TestWikiLinkFragmentOnly;
    procedure TestWikiLinkLabelMarkup;
    procedure TestWikiLinkEmptyIsText;
    procedure TestWikiLinkUnclosedIsText;
    procedure TestWikiLinkLineBreakIsText;
    procedure TestWikiLinkOptionOff;
    procedure TestEmoji;
    procedure TestEmojiCaseInsensitive;
    procedure TestEmojiMarkup;
    procedure TestEmojiUnknownIsText;
    procedure TestEmojiTimeIsText;
    procedure TestEmojiInCodeSpan;
    procedure TestEmojiLookup;
    procedure TestEmojiBeforeDirective;
    procedure TestEmojiInDirectiveLabel;
    procedure TestEmojiDefaults;
    procedure TestEmojiWithoutResolver;
  end;

implementation

uses {$IFDEF FPC_DOTTEDUNITS}System.TypInfo{$ELSE}typinfo{$ENDIF};

{ TTestInlineTextProcessor }

procedure TTestInlineTextProcessor.SetUp;
begin
  TEmojiRegistry.Register('testemoji','[E]');
  TEmojiRegistry.RegisterMarkup('testicon','<i class="icon"></i>');
end;

procedure TTestInlineTextProcessor.TearDown;
begin
  TEmojiRegistry.UnRegister('testemoji');
  TEmojiRegistry.UnRegister('testicon');
  TEmojiRegistry.OnLookup:=Nil;
  FProcessor.Free;
  FScanner.Free;
  FNodes.Free;
  FEntities.Free;
end;

function TTestInlineTextProcessor.DoEmojiLookup(const aName: TMarkdownString; out aReplacement: TMarkdownString): Boolean;
begin
  Result:=(aName='looked_up');
  if Result then
    aReplacement:='[L]'
  else
    aReplacement:='';
end;

procedure TTestInlineTextProcessor.SetupProcessor(const AText: TMarkdownString; awsMode: TWhitespaceMode = wsTrim);
begin
  FScanner := TMarkDownTextScanner.Create(AText, 1);
  FNodes := TMarkDownTextNodeList.Create(True);
  FEntities := TFPStringHashTable.Create; // Assuming no custom entities for now
  FProcessor := TInlineTextProcessor.Create(FScanner, FNodes, FEntities, awsMode);
end;

function TTestInlineTextProcessor.NodeAsText(AIndex: Integer): TMarkDownTextNode;
begin
  AssertEquals('Node at index ' + IntToStr(AIndex) + ' should be text', nkText, FNodes[AIndex].Kind);
  Result := FNodes[AIndex];
end;

function TTestInlineTextProcessor.NodeAsNamed(AIndex: Integer; AExpectedKind: TTextNodeKind): TMarkDownTextNode;
begin
  AssertEquals('Node at index ' + IntToStr(AIndex) + ' should have kind ' + GetEnumName(TypeInfo(TTextNodeKind), Ord(AExpectedKind)), AExpectedKind, FNodes[AIndex].Kind);
  Result := FNodes[AIndex];
end;

class procedure TTestInlineTextProcessor.AssertEquals(const aMsg: TMarkdownString; aExpected, aActual: TTextNodeKind);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TTextNodeKind),Ord(aExpected)),GetEnumName(TypeInfo(TTextNodeKind),Ord(aActual)));
end;

class procedure TTestInlineTextProcessor.AssertEquals(const aMsg: TMarkdownString; aExpected, aActual: TNodeStyle);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TNodeStyle),Ord(aExpected)),GetEnumName(TypeInfo(TNodeStyle),Ord(aActual)));
end;

class procedure TTestInlineTextProcessor.AssertEquals(const aMsg: TMarkdownString; aExpected, aActual: TNodeStyles);
begin
  AssertEquals(aMsg,SetToString(PTypeInfo(TypeInfo(TNodeStyles)),Integer(aExpected),False),
                    SetToString(PTypeInfo(TypeInfo(TNodeStyles)),Integer(aActual),False));
end;

class procedure TTestInlineTextProcessor.CheckStyleList(const aMsg: TMarkdownString; const aExpected: array of TNodeStyle; aNode: TMarkdownTextNode);
var
  I : Integer;
begin
  AssertEquals(aMsg+': style count',Length(aExpected),Length(aNode.StyleList));
  for I:=0 to Length(aExpected)-1 do
    AssertEquals(aMsg+': style at '+IntToStr(I),aExpected[I],aNode.StyleList[I]);
end;

procedure TTestInlineTextProcessor.TestSimpleText;
begin
  SetupProcessor('This is a simple text.');
  FProcessor.Process(True);
  AssertEquals('Should have one text node', 1, FNodes.Count);
  AssertEquals('Text content mismatch', 'This is a simple text.', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.TestNonAsciiEmphasis;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor(UnicodeToMarkdown('*café* 😀'));
  FProcessor.Process(True);
  AssertEquals('Should have 2 nodes', 2, FNodes.Count);
  Node:=NodeAsText(0);
  AssertEquals('Emphasis content', UnicodeToMarkdown('café'), Node.NodeText);
  AssertEquals('Style', [nsEmph], Node.Styles);
  Node:=NodeAsText(1);
  AssertEquals('Text after emphasis', UnicodeToMarkdown(' 😀'), Node.NodeText);
  AssertEquals('Style', [], Node.Styles);
end;

procedure TTestInlineTextProcessor.TestNonAsciiIntrawordEmphasis;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor(UnicodeToMarkdown('é*ü*é'));
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before emphasis', UnicodeToMarkdown('é'), NodeAsText(0).NodeText);
  Node:=NodeAsText(1);
  AssertEquals('Emphasis content', UnicodeToMarkdown('ü'), Node.NodeText);
  AssertEquals('Style', [nsEmph], Node.Styles);
  AssertEquals('Text after emphasis', UnicodeToMarkdown('é'), NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestBackslashEscapes;
begin
  SetupProcessor('This is \*not\* emphasis.');
  FProcessor.Process(True);
  AssertEquals('Should have one text node for escaped chars', 1, FNodes.Count);
  AssertEquals('Escaped character was not handled correctly', 'This is *not* emphasis.', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.TestBareAmpersand;
begin
  // Regression: a bare '&' with no following ';' must not crash. HandleEntityInner
  // used to index lEntity[1] on an empty entity name, causing an access violation.
  SetupProcessor('Concepts & data types');
  FProcessor.Process(True);
  AssertEquals('Should have one text node', 1, FNodes.Count);
  AssertEquals('Bare ampersand is kept as text', 'Concepts & data types', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.TestNumericCharacterReference;
begin
  SetupProcessor('x &#65; y');
  FProcessor.Process(True);
  AssertEquals('Should have one text node', 1, FNodes.Count);
  AssertEquals('Numeric character reference should be decoded', 'x A y', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.TestNumericCharacterReferenceAboveBMP;
begin
  SetupProcessor('x &#x1F600; &#128512; y');
  FProcessor.Process(True);
  AssertEquals('Should have one text node', 1, FNodes.Count);
  AssertEquals('Both spellings decode to the same character',
               'x '+UTF8Encode(#$D83D#$DE00)+' '+UTF8Encode(#$D83D#$DE00)+' y', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.DumpNodes;

begin
  FProcessor.DumpNodes;
end;

procedure TTestInlineTextProcessor.TestCodeSpans;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('Use the `printf()` function.');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes for a code span', 3, FNodes.Count);
  AssertEquals('Text before code span', 'Use the ', NodeAsText(0).NodeText);
  Node:=NodeAsNamed(1,nkCode);
  AssertEquals('Code span content', 'printf()', Node.NodeText);
  AssertEquals('Text after code span', ' function.', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestEmphasisAndStrong;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('*emphasis* and **strong**');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes for emphasis and strong', 3, FNodes.Count);
  // *emphasis*
  Node := NodeAsText(0);
  AssertEquals('Emphasis content', 'emphasis', Node.NodeText);
  AssertEquals('Style', [nsEmph], Node.Styles);

  Node:=NodeAsText(1);
  AssertEquals('Connector text', ' and ', Node.NodeText);
  AssertEquals('Style', [], Node.Styles);

  // **strong**
  Node := NodeAsText(2);
  AssertEquals('Style', [nsStrong], Node.Styles);
  AssertEquals('Strong content', 'strong', Node.NodeText);
end;

procedure TTestInlineTextProcessor.TestEmphasisAndStrongInOne;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('***emphasis and strong***');
  FProcessor.Process(True);
  AssertEquals('Should have 1 node for emphasis and strong', 1, FNodes.Count);
  Node := NodeAsText(0);
  AssertEquals('Emphasis content', 'emphasis and strong', Node.NodeText);
  AssertEquals('Style', [nsStrong,nsEmph], Node.Styles);
end;

procedure TTestInlineTextProcessor.TestEmphasisAndStrongInOneSplit;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('***strong** and emphasis*');
  FProcessor.Process(True);
  AssertEquals('Should have 2 nodes for emphasis and strong', 2, FNodes.Count);
  Node := NodeAsText(0);
  AssertEquals('content', 'strong', Node.NodeText);
  AssertEquals('Style', [nsStrong,nsEmph], Node.Styles);
  Node := NodeAsText(1);
  AssertEquals('Emphasis content', ' and emphasis', Node.NodeText);
  AssertEquals('Style', [nsEmph], Node.Styles);
end;

procedure TTestInlineTextProcessor.TestEmphasisAndStrongInOneSplit2;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('*emphasis and **strong***');
  FProcessor.Process(True);
  AssertEquals('Should have 2 nodes for emphasis and strong', 2, FNodes.Count);
  Node := NodeAsText(0);
  AssertEquals('content', 'emphasis and ', Node.NodeText);
  AssertEquals('Style', [nsEmph], Node.Styles);
  Node := NodeAsText(1);
  AssertEquals('Emphasis content', 'strong', Node.NodeText);
  AssertEquals('Style', [nsStrong,nsEmph], Node.Styles);
end;

procedure TTestInlineTextProcessor.TestStrikethroughGFM;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('This is ~~deleted~~ text.');
  FProcessor.GFMExtensions := True;
  FProcessor.Process(True);

  AssertEquals('Should have 3 nodes for strikethrough', 3, FNodes.Count);
  AssertEquals('Text before', 'This is ', NodeAsText(0).NodeText);

  Node := NodeAsText(1);
  AssertEquals('Strikethrough style', [nsDelete], Node.Styles);
  AssertEquals('Strikethrough content', 'deleted', Node.NodeText);

  Node := NodeAsText(2);
  AssertEquals('Text after', ' text.', Node.NodeText);
end;

procedure TTestInlineTextProcessor.TestAutoLinks;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('See <https://www.example.com>.');
  FProcessor.Process(True);

  AssertEquals('Should have 3 nodes for autolink', 3, FNodes.Count);
  AssertEquals('Text before', 'See ', NodeAsText(0).NodeText);

  Node := NodeAsNamed(1, nkURI);
  AssertEquals('href attribute', 'https://www.example.com', Node.attrs['href']);
  AssertEquals('Autolink text content', 'https://www.example.com', Node.NodeText);

  AssertEquals('Text after', '.', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestInlineLink;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('A [link](https://example.com "Title").');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes for inline link', 3, FNodes.Count);
  AssertEquals('Text before', 'A ', NodeAsText(0).NodeText);

  Node := NodeAsNamed(1, nkURI);
  AssertEquals('Link href', 'https://example.com', Node.attrs['href']);
  AssertEquals('Link title', 'Title', Node.attrs['title']);
  AssertEquals('Link text content', 'link', Node.NodeText);
  Node := NodeAsNamed(2, nkText);
  AssertEquals('Final text content', '.', Node.NodeText);
end;

procedure TTestInlineTextProcessor.TestInlineImage;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('An image ![alt text](/path/img.jpg "title").');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes for inline image', 3, FNodes.Count);
  Node:=NodeAsText(0);
  AssertEquals('Text before', 'An image ', Node.NodeText);
  Node := NodeAsNamed(1, nkImg);
  AssertEquals('Image src', '/path/img.jpg', Node.attrs['src']);
  AssertEquals('Image alt', 'alt text', Node.attrs['alt']);
  AssertEquals('Image title', 'title', Node.attrs['title']);
  Node:=NodeAsText(2);
  AssertEquals('Text after', '.', Node.NodeText);
end;

procedure TTestInlineTextProcessor.TestUnmatchedBrackets;
begin
  SetupProcessor('A [M] bracket.');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before', 'A ', NodeAsText(0).NodeText);
  AssertEquals('Opening bracket kept', '[M', NodeAsText(1).NodeText);
  AssertEquals('Text after', '] bracket.', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestUnmatchedBracketsAfterStrong;
begin
  SetupProcessor('**Level:** [M]');
  FProcessor.Process(True);
  AssertEquals('Should have 4 nodes', 4, FNodes.Count);
  AssertEquals('Strong text', 'Level:', NodeAsText(0).NodeText);
  AssertEquals('Strong style', [nsStrong], NodeAsText(0).Styles);
  AssertEquals('Space between', ' ', NodeAsText(1).NodeText);
  AssertEquals('Opening bracket kept', '[M', NodeAsText(2).NodeText);
  AssertEquals('Closing bracket', ']', NodeAsText(3).NodeText);
end;

procedure TTestInlineTextProcessor.TestUnclosedBracket;
begin
  SetupProcessor('An [unclosed bracket');
  FProcessor.Process(True);
  AssertEquals('Should have 2 nodes', 2, FNodes.Count);
  AssertEquals('Text before', 'An ', NodeAsText(0).NodeText);
  AssertEquals('Opening bracket kept', '[unclosed bracket', NodeAsText(1).NodeText);
end;

procedure TTestInlineTextProcessor.TestUnclosedImageBracket;
begin
  SetupProcessor('An ![unclosed image');
  FProcessor.Process(True);
  AssertEquals('Should have 2 nodes', 2, FNodes.Count);
  AssertEquals('Text before', 'An ', NodeAsText(0).NodeText);
  AssertEquals('Image bracket kept', '![unclosed image', NodeAsText(1).NodeText);
end;

procedure TTestInlineTextProcessor.TestUnmatchedEmphasis;
begin
  SetupProcessor('a * b');
  FProcessor.Process(True);
  AssertEquals('Should have 2 nodes', 2, FNodes.Count);
  AssertEquals('Text before', 'a ', NodeAsText(0).NodeText);
  AssertEquals('Asterisk kept', '* b', NodeAsText(1).NodeText);
end;

procedure TTestInlineTextProcessor.TestNestedEmphasis;
begin
  SetupProcessor('*a *b* c*');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before', 'a ', NodeAsText(0).NodeText);
  CheckStyleList('Outer emphasis', [nsEmph], NodeAsText(0));
  AssertEquals('Nested text', 'b', NodeAsText(1).NodeText);
  CheckStyleList('Nested emphasis', [nsEmph,nsEmph], NodeAsText(1));
  AssertEquals('Text after', ' c', NodeAsText(2).NodeText);
  CheckStyleList('Outer emphasis resumed', [nsEmph], NodeAsText(2));
end;

procedure TTestInlineTextProcessor.TestInlineLinkWithMarkup;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('A [a *b* c](u) tail.');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before', 'A ', NodeAsText(0).NodeText);
  Node := NodeAsNamed(1, nkURI);
  AssertEquals('Link href', 'u', Node.attrs['href']);
  AssertEquals('Link text before markup', 'a ', Node.NodeText);
  AssertTrue('Link has children', Node.HasChildren);
  AssertEquals('Link child count', 2, Node.Children.Count);
  AssertEquals('Emphasized child text', 'b', Node.Children[0].NodeText);
  CheckStyleList('Emphasized child', [nsEmph], Node.Children[0]);
  AssertEquals('Last child text', ' c', Node.Children[1].NodeText);
  CheckStyleList('Last child', [], Node.Children[1]);
  AssertEquals('Text after', ' tail.', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestInlineLinkWithNestedBrackets;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('A [a [b] c](u).');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  Node := NodeAsNamed(1, nkURI);
  AssertEquals('Link href', 'u', Node.attrs['href']);
  AssertEquals('Link text before bracket', 'a ', Node.NodeText);
  AssertEquals('Link child count', 2, Node.Children.Count);
  AssertEquals('Opening bracket kept', '[b', Node.Children[0].NodeText);
  AssertEquals('Closing bracket kept', '] c', Node.Children[1].NodeText);
  AssertEquals('Text after', '.', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestInlineImageWithMarkup;
var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('An ![a *b* c](i.png "T").');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before', 'An ', NodeAsText(0).NodeText);
  Node := NodeAsNamed(1, nkImg);
  AssertEquals('Image src', 'i.png', Node.attrs['src']);
  AssertEquals('Image alt', 'a b c', Node.attrs['alt']);
  AssertEquals('Image title', 'T', Node.attrs['title']);
  AssertFalse('Image has no children', Node.HasChildren);
  AssertEquals('Text after', '.', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestHardLineBreakSpaces;
begin
  SetupProcessor('a  '#10'b');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before', 'a', NodeAsText(0).NodeText);
  AssertEquals('Line break has no text', '', NodeAsNamed(1, nkLineBreak).NodeText);
  AssertEquals('Text after', #10'b', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestHardLineBreakBackslash;
begin
  SetupProcessor('a\'#10'b');
  FProcessor.Process(True);
  AssertEquals('Should have 3 nodes', 3, FNodes.Count);
  AssertEquals('Text before', 'a', NodeAsText(0).NodeText);
  AssertEquals('Line break has no text', '', NodeAsNamed(1, nkLineBreak).NodeText);
  AssertEquals('Text after', #10'b', NodeAsText(2).NodeText);
end;

procedure TTestInlineTextProcessor.TestTextIsNotEscaped;
begin
  SetupProcessor('5 < 7 & "q" > 3');
  FProcessor.Process(True);
  AssertEquals('Should have one text node', 1, FNodes.Count);
  AssertEquals('Text is kept unescaped', '5 < 7 & "q" > 3', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.TestInlineLinkUrlIsNotEscaped;
begin
  SetupProcessor('[a](http://x/?p=1&q=2)');
  FProcessor.Process(True);
  AssertEquals('Should have one node', 1, FNodes.Count);
  AssertEquals('Link href', 'http://x/?p=1&q=2', NodeAsNamed(0, nkURI).attrs['href']);
end;

procedure TTestInlineTextProcessor.TestEmptyLinkText;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('See [](#target).');
  FProcessor.Process(True);
  Node := NodeAsNamed(1, nkURI);
  AssertEquals('Link target', '#target', Node.Attrs['href']);
  AssertEquals('Link has no text', '', Node.NodeText);
  AssertFalse('Link has no children', Node.HasChildren);
end;


procedure TTestInlineTextProcessor.TestInlineComment;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('a <!-- remark --> b');
  FProcessor.Comments:=True;
  FProcessor.Process(True);
  AssertEquals('Three nodes', 3, FNodes.Count);
  Node := NodeAsNamed(1, nkComment);
  AssertEquals('Comment text', ' remark ', Node.NodeText);
  AssertFalse('Plain comment is no marker', Node.HasAttrs and Node.Attrs.Contains('marker'));
end;


procedure TTestInlineTextProcessor.TestInlineCommentMarker;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('x<!-- index[msgnr]: 1000 -->');
  FProcessor.Comments:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(1, nkComment);
  AssertEquals('Marker name', 'index', Node.Attrs['marker']);
  AssertEquals('Marker argument', 'msgnr', Node.Attrs['argument']);
  AssertEquals('Marker value', '1000', Node.Attrs['value']);
end;


procedure TTestInlineTextProcessor.TestInlineCommentSpansLines;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('a <!-- one'#10'two --> b');
  FProcessor.Comments:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(1, nkComment);
  AssertEquals('Comment over two lines', ' one'#10'two ', Node.NodeText);
end;


procedure TTestInlineTextProcessor.TestEmptyComment;

begin
  SetupProcessor('a <!--> b');
  FProcessor.Comments:=True;
  FProcessor.Process(True);
  AssertEquals('Empty comment', '', NodeAsNamed(1, nkComment).NodeText);
  AssertEquals('Text after', ' b', NodeAsText(2).NodeText);
end;


procedure TTestInlineTextProcessor.TestUnclosedCommentIsText;

begin
  SetupProcessor('a <!-- b');
  FProcessor.Comments:=True;
  FProcessor.Process(True);
  AssertEquals('Literal text', 'a <!-- b', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestCommentOptionOff;

begin
  SetupProcessor('a <!-- b --> c');
  FProcessor.Process(True);
  AssertEquals('Literal text without option', 'a <!-- b --> c', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestFootnoteReference;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('Text.[^speed] More');
  FProcessor.Footnotes:=True;
  FProcessor.Process(True);
  AssertEquals('Three nodes', 3, FNodes.Count);
  Node := NodeAsNamed(1, nkFootnoteRef);
  AssertEquals('Label', 'speed', Node.Attrs['label']);
  AssertEquals('Text after', ' More', NodeAsText(2).NodeText);
end;


procedure TTestInlineTextProcessor.TestFootnoteReferenceWithSpaceIsText;

begin
  SetupProcessor('Text [^a b] more');
  FProcessor.Footnotes:=True;
  FProcessor.Process(True);
  AssertEquals('Literal text', 'Text [^a b] more', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestFootnoteOptionOff;

var
  I : Integer;
begin
  SetupProcessor('Text[^a]');
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No footnote reference without option', FNodes[I].Kind=nkFootnoteRef);
end;


procedure TTestInlineTextProcessor.TestReferenceLink;

var
  lDoc : TMarkdownDocument;
  Node: TMarkDownTextNode;
begin
  lDoc:=TMarkdownDocument.Create(Nil,1);
  try
    lDoc.AddLinkRef('Foo','/url','The title');
    SetupProcessor('A [link][foo] and [foo].');
    FProcessor.LinkReferences:=lDoc;
    FProcessor.Process(True);
    Node := NodeAsNamed(1, nkURI);
    AssertEquals('Full reference target', '/url', Node.Attrs['href']);
    AssertEquals('Full reference title', 'The title', Node.Attrs['title']);
    AssertEquals('Full reference text', 'link', Node.NodeText);
    Node := NodeAsNamed(3, nkURI);
    AssertEquals('Shortcut reference target', '/url', Node.Attrs['href']);
    AssertEquals('Shortcut reference text', 'foo', Node.NodeText);
  finally
    lDoc.Free;
  end;
end;


procedure TTestInlineTextProcessor.TestReferenceImage;

var
  lDoc : TMarkdownDocument;
  Node: TMarkDownTextNode;
begin
  lDoc:=TMarkdownDocument.Create(Nil,1);
  try
    lDoc.AddLinkRef('logo','logo.png','');
    SetupProcessor('![The logo][logo]');
    FProcessor.LinkReferences:=lDoc;
    FProcessor.Process(True);
    Node := NodeAsNamed(0, nkImg);
    AssertEquals('Image source', 'logo.png', Node.Attrs['src']);
    AssertEquals('Image alt', 'The logo', Node.Attrs['alt']);
  finally
    lDoc.Free;
  end;
end;


procedure TTestInlineTextProcessor.TestReferenceLinkWithoutDocument;

begin
  SetupProcessor('[foo]');
  FProcessor.Process(True);
  AssertEquals('Literal text', '[foo]', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestInlineDirective;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('An :abbr[HTML]{title="Hyper Text"} tag');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  AssertEquals('Three nodes', 3, FNodes.Count);
  Node := NodeAsNamed(1, nkDirective);
  AssertEquals('Name', 'abbr', Node.Name);
  AssertEquals('Label', 'HTML', Node.Children.PlainText);
  AssertEquals('Attribute', 'Hyper Text', Node.Attrs['title']);
  AssertEquals('No text of its own', '', Node.NodeText);
  AssertEquals('Text after', ' tag', NodeAsText(2).NodeText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveIDAndClasses;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor(':x[a]{#i .c1 .c2 flag}');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(0, nkDirective);
  AssertEquals('ID', 'i', Node.Attrs['id']);
  AssertEquals('Classes', 'c1 c2', Node.Attrs['class']);
  AssertTrue('Bare key', Node.Attrs.Contains('flag'));
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveNameOnly;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('a :br b');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  AssertEquals('Three nodes', 3, FNodes.Count);
  Node := NodeAsNamed(1, nkDirective);
  AssertEquals('Name', 'br', Node.Name);
  AssertEquals('Source as text', ':br', Node.NodeText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveAfterColonIsText;

var
  I : Integer;
begin
  SetupProcessor('a ::x');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No directive after a colon', FNodes[I].Kind=nkDirective);
  AssertEquals('Literal text', 'a ::x', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveDigitIsText;

var
  I : Integer;
begin
  SetupProcessor('at 10:30 or a:-b');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No directive without a letter', FNodes[I].Kind=nkDirective);
  AssertEquals('Literal text', 'at 10:30 or a:-b', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveEscapedIsText;

var
  I : Integer;
begin
  SetupProcessor('a \:x[b]');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No directive after escape', FNodes[I].Kind=nkDirective);
  AssertEquals('Literal text', 'a :x[b]', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveInvalidAttributes;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor(':x[a]{=b}');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(0, nkDirective);
  AssertEquals('Label', 'a', Node.Children.PlainText);
  AssertFalse('No attributes', Node.HasAttrs);
  AssertEquals('Attributes left as text', 'a{=b}', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveUnclosedLabel;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor(':x[a');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(0, nkDirective);
  AssertEquals('Name only', ':x', Node.NodeText);
  AssertEquals('Bracket left as text', ':x[a', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveNestedLabel;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor(':x[see [link](u) here] after');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(0, nkDirective);
  AssertEquals('Label text', 'see link here', Node.Children.PlainText);
  AssertEquals('Link in label', 'nkURI', GetEnumName(TypeInfo(TTextNodeKind), Ord(Node.Children[1].Kind)));
  AssertEquals('Text after', ' after', NodeAsText(1).NodeText);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveInEmphasis;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('*a :x[b] c*');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(1, nkDirective);
  AssertTrue('Directive emphasized', nsEmph in Node.Styles);
  AssertTrue('Label emphasized', nsEmph in Node.Children[0].Styles);
end;


procedure TTestInlineTextProcessor.TestInlineDirectiveOptionOff;

var
  I : Integer;
begin
  SetupProcessor('a :x[b]');
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No directive without option', FNodes[I].Kind=nkDirective);
end;


procedure TTestInlineTextProcessor.TestWikiLink;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('[[Main Page]]');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  AssertEquals('One node', 1, FNodes.Count);
  Node := NodeAsNamed(0, nkWikiLink);
  AssertEquals('Target', 'Main Page', Node.Attrs['target']);
  AssertFalse('No fragment', Node.Attrs.Contains('fragment'));
  AssertEquals('Target as text', 'Main Page', Node.NodeText);
  AssertFalse('No label', Node.HasChildren);
end;


procedure TTestInlineTextProcessor.TestWikiLinkFragmentAndLabel;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('See [[ Page # Sec | The label ]].');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  AssertEquals('Three nodes', 3, FNodes.Count);
  Node := NodeAsNamed(1, nkWikiLink);
  AssertEquals('Target', 'Page', Node.Attrs['target']);
  AssertEquals('Fragment', 'Sec', Node.Attrs['fragment']);
  AssertEquals('Label', 'The label', Node.Children.PlainText);
  AssertEquals('No text of its own', '', Node.NodeText);
  AssertEquals('Text after', '.', NodeAsText(2).NodeText);
end;


procedure TTestInlineTextProcessor.TestWikiLinkFragmentOnly;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('[[#Sec]]');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(0, nkWikiLink);
  AssertEquals('Empty target', '', Node.Attrs['target']);
  AssertEquals('Fragment', 'Sec', Node.Attrs['fragment']);
  AssertEquals('Source as text', '#Sec', Node.NodeText);
end;


procedure TTestInlineTextProcessor.TestWikiLinkLabelMarkup;

var
  Node: TMarkDownTextNode;
begin
  SetupProcessor('[[Page|a **b**]]');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  Node := NodeAsNamed(0, nkWikiLink);
  AssertEquals('Two label nodes', 2, Node.Children.Count);
  AssertTrue('Strong in label', nsStrong in Node.Children[1].Styles);
end;


procedure TTestInlineTextProcessor.TestWikiLinkEmptyIsText;

var
  I : Integer;
begin
  SetupProcessor('[[ ]] and [[|x]]');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No wiki link without target', FNodes[I].Kind=nkWikiLink);
  AssertEquals('Literal text', '[[ ]] and [[|x]]', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestWikiLinkUnclosedIsText;

begin
  SetupProcessor('[[Page');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  AssertEquals('Literal text', '[[Page', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestWikiLinkLineBreakIsText;

var
  I : Integer;
begin
  SetupProcessor('[[Pa'#10'ge]]');
  FProcessor.WikiLinks:=True;
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No wiki link over a line end', FNodes[I].Kind=nkWikiLink);
end;


procedure TTestInlineTextProcessor.TestWikiLinkOptionOff;

var
  I : Integer;
begin
  SetupProcessor('[[Page]]');
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('No wiki link without option', FNodes[I].Kind=nkWikiLink);
end;


procedure TTestInlineTextProcessor.TestEmoji;

begin
  SetupProcessor('a :testemoji: b');
  FProcessor.Process(True);
  AssertEquals('Shortcode replaced', 'a [E] b', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiCaseInsensitive;

begin
  SetupProcessor(':TestEmoji:');
  FProcessor.Process(True);
  AssertEquals('Name looked up in lowercase', '[E]', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiMarkup;

var
  I : Integer;
  lFound : Boolean;
begin
  SetupProcessor('a :testicon: b');
  FProcessor.Process(True);
  lFound:=False;
  for I:=0 to FNodes.Count-1 do
    if FNodes[I].Kind=nkRaw then
      begin
      lFound:=True;
      AssertEquals('Markup', '<i class="icon"></i>', FNodes[I].NodeText);
      end;
  AssertTrue('Markup becomes a raw node', lFound);
end;


procedure TTestInlineTextProcessor.TestEmojiUnknownIsText;

begin
  SetupProcessor('a :no_such_name: b');
  FProcessor.Process(True);
  AssertEquals('Unknown shortcode stays as written', 'a :no_such_name: b', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiTimeIsText;

begin
  SetupProcessor('Note: at 10:30, see http://x.org');
  FProcessor.Process(True);
  AssertEquals('Colons that are no shortcode stay', 'Note: at 10:30, see http://x.org', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiInCodeSpan;

begin
  SetupProcessor('`:testemoji:`');
  FProcessor.Process(True);
  AssertEquals('Code span keeps the shortcode', ':testemoji:', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiLookup;

begin
  TEmojiRegistry.OnLookup:=@DoEmojiLookup;
  SetupProcessor(':looked_up: :not_looked_up:');
  FProcessor.Process(True);
  AssertEquals('OnLookup resolves unregistered names', '[L] :not_looked_up:', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiBeforeDirective;

var
  I : Integer;
begin
  SetupProcessor('a :testemoji: b');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  for I:=0 to FNodes.Count-1 do
    AssertFalse('Shortcode is no directive', FNodes[I].Kind=nkDirective);
  AssertEquals('Shortcode replaced', 'a [E] b', FNodes.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiInDirectiveLabel;

begin
  SetupProcessor(':span[x :testemoji:]{.c}');
  FProcessor.Directives:=True;
  FProcessor.Process(True);
  AssertEquals('Label replaced', 'x [E]', NodeAsNamed(0, nkDirective).Children.PlainText);
end;


procedure TTestInlineTextProcessor.TestEmojiDefaults;

var
  lText : TMarkdownString;
begin
  TEmojiRegistry.RegisterDefaults;
  AssertTrue('smile registered', TEmojiRegistry.Find('smile',lText));
  AssertEquals('smile is U+1F604', #$F0#$9F#$98#$84, lText);
  AssertTrue('All defaults registered', TEmojiRegistry.Count>=147);
end;


procedure TTestInlineTextProcessor.TestEmojiWithoutResolver;

var
  lResolver : TEmojiResolver;
begin
  lResolver:=TInlineTextProcessor.EmojiResolver;
  TInlineTextProcessor.EmojiResolver:=Nil;
  try
    SetupProcessor('a :testemoji: b');
    FProcessor.Process(True);
    AssertEquals('No replacement without resolver', 'a :testemoji: b', FNodes.PlainText);
  finally
    TInlineTextProcessor.EmojiResolver:=lResolver;
  end;
end;

initialization
  RegisterTest(TTestInlineTextProcessor);
end.


