{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown block parser tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UTest.Markdown.Parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Contnrs,
  Markdown.Elements, Markdown.Parser, Markdown.InlineText, Markdown.Scanner;

type
  { TBlockTestCase }
  // Helper base class to avoid boilerplate code
  TBlockTestCase = class(TTestCase)
  private
    FDoc: TMarkDownDocument;
    FParser: TMarkDownParser;
    FStrings: TStringList;
    procedure CheckTextnodeText(const aMsg: string; aBlock: TMarkDownBlock; const aText: string);
  protected
    procedure SetupParser(const AText: String);
    procedure CheckBlockText(const aMsg: string; aBlock: TMarkDownBlock; const aText : string; aInParagraph: Boolean);
    function GetBlock(AIndex: Integer): TMarkDownBlock;
    property Doc: TMarkDownDocument read FDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TTestParagraphs }
  TTestParagraphs = class(TBlockTestCase)
  published
    procedure TestSimpleParagraph;
    procedure TestMultipleParagraphs;
  end;

  { TTestHeadings }
  TTestHeadings = class(TBlockTestCase)
  published
    procedure TestATXHeading;
    procedure TestSetextHeadings;
  end;

  { TTestCodeBlocks }
  TTestCodeBlocks = class(TBlockTestCase)
  published
    procedure TestIndentedCodeBlock;
    procedure TestFencedCodeBlock;
    procedure TestNormalFencedCodeBlockWithInfoString;
    procedure TestFencedCodeBlockWithInfoString;
    procedure TestNestedCodeBlock;
  end;

  { TTestInlineProcessorClass : replaces @ to show that the class var is honoured }

  TTestInlineProcessor = class(TInlineTextProcessor)
  protected
    procedure HandleTextCore; override;
  end;

  { TTestInlineProcessorHook }

  TTestInlineProcessorHook = class(TBlockTestCase)
  Public
    procedure TearDown; override;
  published
    procedure TestDefaultInlineTextProcessorClass;
  end;

  { TTestBlockQuotes }
  TTestBlockQuotes = class(TBlockTestCase)
  published
    procedure TestSimpleQuote;
    procedure TestNestedQuote;
    procedure TestLazy;
    // Code blocks in a quote have the quote marker removed from each of their lines.
    procedure TestQuoteFencedCode;
    procedure TestQuoteFencedCodeThenText;
    procedure TestQuoteIndentedCode;
    procedure TestBlankLineEndsQuote;
    procedure TestNestedQuoteBlankLine;
    procedure TestQuoteUnorderedList;
    procedure TestQuoteOrderedListContinuation;
    procedure TestQuoteNestedList;
    procedure TestQuoteListThenParagraph;
    procedure TestQuoteListLazy;
  end;

  { TTestLists }
  TTestLists = class(TBlockTestCase)
  private
    function TestList(const Msg, Source: String; aListCount, aListItemCount: Integer): TMarkDownListBlock;
  published
    procedure TestUnorderedList;
    procedure TestOrderedList;
    procedure TestNestedList;
    procedure TestNestedList2;
    procedure TestNestedList3;
    procedure TestNestedList4;
    procedure TestNestedList5;
    procedure TestNestedList6;
    // A non-indented block after a blank line ends the list (CommonMark), it is
    // not absorbed as a loose continuation of the last item.
    procedure TestOrderedListThenParagraph;
    procedure TestUnorderedListThenParagraph;
    procedure TestOrderedListThenHeading;
    // These must keep working (the fix must not be over-eager):
    procedure TestLooseListPreserved;
    procedure TestLazyContinuationPreserved;
    // Indentation of item content is counted from the column where the item content
    // starts, not from the left margin.
    procedure TestItemContentIndentParagraph;
    procedure TestItemContentIndentCodeBlock;
    procedure TestItemContentIndentThematicBreak;
    procedure TestItemContentIndentWideMarker;
    procedure TestItemTwoParagraphsAreLoose;
    // Blocks starting inside an indented item are recognised and lose that indentation.
    procedure TestItemHeading;
    procedure TestItemQuote;
    procedure TestItemQuoteList;
    procedure TestItemFencedCode;
    procedure TestListThenThematicBreak;
    procedure TestSpacedThematicBreakEndsList;
  end;

  { TTestThematicBreaks }
  TTestThematicBreaks = class(TBlockTestCase)
  published
    procedure TestAsteriskBreak;
    procedure TestUnderscoreBreak;
  end;

  { TTestTables }
  TTestTables = class(TBlockTestCase)
  published
    procedure TestSimpleTable;
  end;

  { TTestFrontmatter }
  TTestFrontmatter = class(TBlockTestCase)
  published
    procedure TestYAMLFrontmatter;
    procedure TestTOMLFrontmatter;
    procedure TestJSONFrontmatter;
    procedure TestFrontmatterNotOnLine1;
    procedure TestFrontmatterWithContent;
    procedure TestNoFrontmatter;
    procedure TestEmptyFrontmatter;
    procedure TestUnclosedFrontmatter;
  end;

  { TExtensionTestCase }

  TExtensionTestCase = class(TBlockTestCase)
  private
    FMessages : TStringList;
    procedure DoMessage(Sender : TObject; aLevel : TMarkdownMessageLevel; aLine : Integer; const aMessage : String);
  protected
    procedure SetupExt(const aText : String; aOptions : TMarkdownOptions = MarkdownDocExtensions);
    function TextBlockOf(aBlock : TMarkdownBlock) : TMarkdownTextBlock;
    function PlainOf(aBlock : TMarkdownBlock) : String;
    // Parser messages as level:line:text
    property Messages : TStringList read FMessages;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TTestComments }

  TTestComments = class(TExtensionTestCase)
  published
    procedure TestCommentBlock;
    procedure TestCommentBlockMultiLine;
    procedure TestCommentBlockTextAfterClose;
    procedure TestCommentBlockMarker;
    procedure TestCommentDoesNotInterruptParagraph;
    procedure TestPlainCommentIsNoMarker;
    procedure TestMarkerInsideEmphasis;
    procedure TestCommentBetweenListItems;
    procedure TestMarkersInDocumentOrder;
    procedure TestCommentOptionOff;
  end;

  { TTestAttributes }

  TTestAttributes = class(TExtensionTestCase)
  published
    procedure TestATXHeading;
    procedure TestATXHeadingClosingHashes;
    procedure TestSetextHeading;
    procedure TestFencedCode;
    procedure TestAnchorRegistered;
    procedure TestDuplicateID;
    procedure TestInvalidSpecStaysText;
    procedure TestAttributesOptionOff;
  end;

  { TTestHeadingIDs }

  TTestHeadingIDs = class(TExtensionTestCase)
  published
    procedure TestAutoID;
    procedure TestAutoIDCollision;
    procedure TestAutoIDExplicitClash;
    procedure TestAutoIDUnicode;
    procedure TestAutoIDSetext;
    procedure TestHeadingIDsOptionOff;
  end;

  { TTestDefinitionLists }

  TTestDefinitionLists = class(TExtensionTestCase)
  published
    procedure TestSimple;
    procedure TestMultipleDefinitions;
    procedure TestMultiParagraphDefinition;
    procedure TestBlankLineBeforeDefinition;
    procedure TestTermIsLastParagraphLine;
    procedure TestSecondTermJoinsList;
    procedure TestLazyContinuation;
    procedure TestNoTerm;
    procedure TestDefinitionListInListItem;
    procedure TestDefinitionListsOptionOff;
  end;

  { TTestAlerts }

  TTestAlerts = class(TExtensionTestCase)
  published
    procedure TestNote;
    procedure TestCaseInsensitive;
    procedure TestMarkerOnlyLine;
    procedure TestExtraTextStaysQuote;
    procedure TestUnknownTypeStaysQuote;
    procedure TestAlertWithCodeAndDefinitionList;
    procedure TestAlertAfterList;
    procedure TestAlertsOptionOff;
  end;

  { TTestFootnotes }

  TTestFootnotes = class(TExtensionTestCase)
  published
    procedure TestDefinition;
    procedure TestNumberingByFirstReference;
    procedure TestRepeatedReference;
    procedure TestContinuationLines;
    procedure TestUndefinedReference;
    procedure TestUnusedDefinition;
    procedure TestDuplicateLabel;
    procedure TestDefinitionInsideQuote;
    procedure TestFootnotesOptionOff;
  end;

  { TTestCaptions }

  TTestCaptions = class(TExtensionTestCase)
  published
    procedure TestTableCaptionAfter;
    procedure TestTableCaptionBefore;
    procedure TestTableCaptionID;
    procedure TestCaptionNotNextToTable;
    procedure TestFigure;
    procedure TestFigureAttributes;
    procedure TestImageWithTextIsNoFigure;
    procedure TestCaptionsOptionOff;
  end;

  { TTestLinkReferences }

  TTestLinkReferences = class(TExtensionTestCase)
  private
    function FirstNode : TMarkdownTextNode;
  published
    procedure TestDefinition;
    procedure TestFullReference;
    procedure TestCollapsedReference;
    procedure TestShortcutReference;
    procedure TestCaseInsensitiveLabel;
    procedure TestUndefinedLabelStaysText;
    procedure TestDefinitionDoesNotInterruptParagraph;
    procedure TestLinkReferencesOptionOff;
  end;

  { TTestTransformRegistry }

  TTestTransformRegistry = class(TTestCase)
  published
    procedure TestDefaultTransforms;
  end;

implementation

{ TBlockTestCase }

procedure TBlockTestCase.SetUp;
begin
  inherited SetUp;
  FStrings := TStringList.Create;
  FParser := TMarkDownParser.Create(nil);
end;

procedure TBlockTestCase.TearDown;
begin
  FDoc.Free;
  FParser.Free;
  FStrings.Free;
  inherited TearDown;
end;

procedure TBlockTestCase.SetupParser(const AText: String);

begin
  FStrings.Text := AText;
  FDoc := FParser.Parse(FStrings);
//  FDoc.Dump('');
  AssertNotNull('Document should be parsed', FDoc);
end;

procedure TBlockTestCase.CheckBlockText(Const aMsg : string; aBlock: TMarkDownBlock; const aText : String; aInParagraph: Boolean);
var
  lBlock : TMarkDownBlock;
begin
  lBlock:=aBlock;
  AssertTrue(aMsg+': Have child',lBlock.ChildCount>0);
  if aInParagraph then
    begin
    lBlock:=lBlock[0];
    AssertEquals(aMsg+': child is para',TMarkDownParagraphBlock,lBlock.ClassType);
    AssertTrue(aMsg+': Paragrapg Has child',lBlock.ChildCount>0);
    end;
  lBlock:=lBlock[0];
  CheckTextnodeText(aMsg,lBlock,aText);
end;

procedure TBlockTestCase.CheckTextnodeText(const aMsg : string; aBlock : TMarkDownBlock; const aText : string);

var
  lText : TMarkDownTextBlock absolute aBlock;
  lTextNode : TMarkDownTextNode;
  lCount : Integer;
begin
  AssertEquals(aMsg+': block is text',TMarkDownTextBlock,aBlock.ClassType);
  lCount:=lText.Nodes.Count;
  AssertTrue(aMsg+' text nodes',lCount>0);
  lTextNode:=lText.Nodes[0];
  AssertEquals(aMsg+' text node text',aText,lTextNode.NodeText);
end;

function TBlockTestCase.GetBlock(AIndex: Integer): TMarkDownBlock;
begin
  AssertTrue('Block index out of bounds', AIndex < FDoc.Blocks.Count);
  Result := FDoc.Blocks[AIndex];
end;

{ TTestParagraphs }

procedure TTestParagraphs.TestSimpleParagraph;
var
  Block: TMarkDownParagraphBlock;
begin
  SetupParser('This is a simple paragraph.');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownParagraphBlock;
  AssertNotNull('Block should be a paragraph', Block);
  AssertTrue('Should be a plain paragraph', Block.isPlainPara);
end;

procedure TTestParagraphs.TestMultipleParagraphs;
begin
  SetupParser('First paragraph.'#10#10'Second paragraph.');
  AssertEquals('Document should have 2 blocks', 2, Doc.Blocks.Count);
  AssertTrue('First block should be a paragraph', GetBlock(0) is TMarkDownParagraphBlock);
  AssertTrue('Second block should be a paragraph', GetBlock(1) is TMarkDownParagraphBlock);
end;

{ TTestHeadings }

procedure TTestHeadings.TestATXHeading;
var
  Block: TMarkDownHeadingBlock;
begin
  SetupParser('# A Level 1 Heading');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownHeadingBlock;
  AssertNotNull('Block should be a heading', Block);
  AssertEquals('Heading level should be 1', 1, Block.Level);
end;

procedure TTestHeadings.TestSetextHeadings;
var
  Block: TMarkDownParagraphBlock;
begin
  SetupParser('A Level 2 Heading'#10'-----------------');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownParagraphBlock;
  AssertNotNull('Block should be a paragraph (used for setext)', Block);
  AssertEquals('Header property should be 2 for setext', 2, Block.Header);
end;

{ TTestCodeBlocks }

procedure TTestCodeBlocks.TestIndentedCodeBlock;
var
  Block: TMarkDownCodeBlock;
begin
  SetupParser('    a = 1;'#10'    b = 2;');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownCodeBlock;
  AssertNotNull('Block should be a code block', Block);
  AssertFalse('Should not be a fenced code block', Block.Fenced);
end;

procedure TTestCodeBlocks.TestFencedCodeBlock;
var
  Block: TMarkDownCodeBlock;
begin
  SetupParser('```'#10'code here'#10'```');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownCodeBlock;
  AssertNotNull('Block should be a code block', Block);
  AssertTrue('Should be a fenced code block', Block.Fenced);
end;

procedure TTestCodeBlocks.TestNormalFencedCodeBlockWithInfoString;
var
  Block: TMarkDownCodeBlock;
begin
  SetupParser('```pascal'#10'var i: Integer;'#10'```');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownCodeBlock;
  AssertNotNull('Block should be a code block', Block);
  AssertTrue('Should be a fenced code block', Block.Fenced);
  AssertEquals('Language info string incorrect', 'pascal', Block.Lang);
end;


procedure TTestCodeBlocks.TestFencedCodeBlockWithInfoString;
var
  Block: TMarkDownCodeBlock;
begin
  SetupParser('~~~ pascal'#10'var i: Integer;'#10'~~~');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownCodeBlock;
  AssertNotNull('Block should be a code block', Block);
  AssertTrue('Should be a fenced code block', Block.Fenced);
  AssertEquals('Language info string incorrect', 'pascal', Block.Lang);
end;

procedure TTestCodeBlocks.TestNestedCodeBlock;
var
  lList : TMarkDownListBlock;
  lItem : TMarkDownListItemBlock;
  Block: TMarkDownCodeBlock;

begin
  SetupParser('* List'#10'   ```'#10'code here'#10'```');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  lList := GetBlock(0) as TMarkDownListBlock;
  AssertEquals('List should have 1 blocks', 1, lList.Blocks.Count);
  lItem := lList.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('List item should have 2 blocks', 2, lItem.Blocks.Count);
  AssertEquals('First list item is paragraph block', TMarkDownParagraphBlock, lItem.Blocks[0].ClassType);
  AssertEquals('Second list item is code block', TMarkDownCodeBlock, lItem.Blocks[1].ClassType);
  Block := lItem.Blocks[1] as TMarkDownCodeBlock;
  AssertTrue('Should be a fenced code block', Block.Fenced);
end;

{ TTestBlockQuotes }

procedure TTestBlockQuotes.TestSimpleQuote;
var
  Block: TMarkDownQuoteBlock;
begin
  SetupParser('> This is a quote.');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Block := GetBlock(0) as TMarkDownQuoteBlock;
  AssertNotNull('Block should be a quote block', Block);
end;

procedure TTestBlockQuotes.TestNestedQuote;
var
  OuterQuote, InnerQuote: TMarkDownQuoteBlock;
begin
  SetupParser('> First level'#10'>> Second level');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  AssertEquals('Outer block should be a quote', TMarkDownQuoteBlock,GetBlock(0).ClassType);
  OuterQuote :=GetBlock(0)  as TMarkDownQuoteBlock;
  AssertEquals('Outer quote should have 2 blocks inside', 2, OuterQuote.Blocks.Count); // Para and another quote
  AssertEquals('First inner block is a paragraph', TMarkDownParagraphBlock,OuterQuote.Blocks[0].ClassType);
  AssertEquals('Second inner block should be a quote', TMarkDownQuoteBlock,OuterQuote.Blocks[1].ClassType);
  InnerQuote :=OuterQuote.Blocks[1] as TMarkDownQuoteBlock;
  AssertEquals('Outer quote should have 1 block inside', 1, InnerQuote.Blocks.Count); // Para and another quote
  AssertEquals('First inner block is a paragraph', TMarkDownParagraphBlock,InnerQuote.Blocks[0].ClassType);
end;

procedure TTestBlockQuotes.TestLazy;
var
  OuterQuote: TMarkDownQuoteBlock;
begin
  SetupParser('> First level'#10'Continues');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  AssertEquals('Outer block should be a quote', TMarkDownQuoteBlock,GetBlock(0).ClassType);
  OuterQuote :=GetBlock(0)  as TMarkDownQuoteBlock;
  AssertEquals('Outer quote should have 1 blocks inside', 1, OuterQuote.Blocks.Count); // Para and another quote
  AssertEquals('First inner block is a paragraph', TMarkDownParagraphBlock,OuterQuote.Blocks[0].ClassType);
end;

procedure TTestBlockQuotes.TestQuoteFencedCode;
var
  Quote: TMarkDownQuoteBlock;
  Code: TMarkDownCodeBlock;
begin
  SetupParser('> ```pascal'#10'> begin end.'#10'> ```');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote should have 1 block', 1, Quote.Blocks.Count);
  AssertTrue('Quote block is a code block', Quote.Blocks[0] is TMarkDownCodeBlock);
  Code := Quote.Blocks[0] as TMarkDownCodeBlock;
  AssertEquals('Code block language', 'pascal', Code.Lang);
  AssertEquals('The closing fence ends the block', 1, Code.Blocks.Count);
  AssertEquals('The quote marker is stripped from the code',
               'begin end.', (Code.Blocks[0] as TMarkDownTextBlock).Text);
end;


procedure TTestBlockQuotes.TestQuoteFencedCodeThenText;
var
  Quote: TMarkDownQuoteBlock;
begin
  SetupParser('> ```'#10'> code'#10'> ```'#10'> after');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote should have 2 blocks', 2, Quote.Blocks.Count);
  AssertTrue('First quote block is a code block', Quote.Blocks[0] is TMarkDownCodeBlock);
  AssertTrue('Text after the fence stays in the quote', Quote.Blocks[1] is TMarkDownParagraphBlock);
end;


procedure TTestBlockQuotes.TestQuoteIndentedCode;
var
  Quote: TMarkDownQuoteBlock;
  Code: TMarkDownCodeBlock;
begin
  SetupParser('> text'#10'>'#10'>     code one'#10'>     code two');
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote should have 2 blocks', 2, Quote.Blocks.Count);
  AssertTrue('Second quote block is a code block', Quote.Blocks[1] is TMarkDownCodeBlock);
  Code := Quote.Blocks[1] as TMarkDownCodeBlock;
  AssertEquals('Both lines belong to one code block', 2, Code.Blocks.Count);
  AssertEquals('Second code line', 'code two', (Code.Blocks[1] as TMarkDownTextBlock).Text);
end;


procedure TTestBlockQuotes.TestBlankLineEndsQuote;

begin
  SetupParser('> text'#10#10'after');
  AssertEquals('Quote and paragraph', 2, Doc.Blocks.Count);
  AssertTrue('First block is a quote', GetBlock(0) is TMarkDownQuoteBlock);
  AssertTrue('Second block is a paragraph', GetBlock(1) is TMarkDownParagraphBlock);
end;


procedure TTestBlockQuotes.TestNestedQuoteBlankLine;
var
  Quote: TMarkDownQuoteBlock;
begin
  SetupParser('> > a'#10'>'#10'> b');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Outer quote has the inner quote and a paragraph', 2, Quote.Blocks.Count);
  AssertTrue('First block is the inner quote', Quote.Blocks[0] is TMarkDownQuoteBlock);
  AssertTrue('Second block is a paragraph', Quote.Blocks[1] is TMarkDownParagraphBlock);
end;


procedure TTestBlockQuotes.TestQuoteUnorderedList;
var
  Quote: TMarkDownQuoteBlock;
begin
  SetupParser('> - a'#10'> - b');
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote should have 1 block', 1, Quote.Blocks.Count);
  AssertTrue('Quote block is a list', Quote.Blocks[0] is TMarkDownListBlock);
  AssertEquals('Both items in one list', 2, Quote.Blocks[0].ChildCount);
end;


procedure TTestBlockQuotes.TestQuoteOrderedListContinuation;
var
  Quote: TMarkDownQuoteBlock;
  List: TMarkDownListBlock;
begin
  SetupParser('> 1. one'#10'>    more'#10'> 2. two');
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote should have 1 block', 1, Quote.Blocks.Count);
  List := Quote.Blocks[0] as TMarkDownListBlock;
  AssertEquals('Both items in one list', 2, List.Blocks.Count);
  AssertEquals('Continuation line stays in the first item', 1, List.Blocks[0].ChildCount);
end;


procedure TTestBlockQuotes.TestQuoteNestedList;
var
  Quote: TMarkDownQuoteBlock;
  List: TMarkDownListBlock;
begin
  SetupParser('> - a'#10'>   - nested'#10'> - b');
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  List := Quote.Blocks[0] as TMarkDownListBlock;
  AssertEquals('Two items in the outer list', 2, List.Blocks.Count);
  AssertEquals('First item has a paragraph and a list', 2, List.Blocks[0].ChildCount);
  AssertTrue('Nested list in the first item', List.Blocks[0].Children[1] is TMarkDownListBlock);
end;


procedure TTestBlockQuotes.TestQuoteListThenParagraph;
var
  Quote: TMarkDownQuoteBlock;
begin
  SetupParser('> - a'#10'>'#10'> text');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote has a list and a paragraph', 2, Quote.Blocks.Count);
  AssertTrue('First block is a list', Quote.Blocks[0] is TMarkDownListBlock);
  AssertTrue('Second block is a paragraph', Quote.Blocks[1] is TMarkDownParagraphBlock);
end;


procedure TTestBlockQuotes.TestQuoteListLazy;
var
  Quote: TMarkDownQuoteBlock;
begin
  SetupParser('> - a'#10'lazy');
  AssertEquals('Lazy line stays in the quote', 1, Doc.Blocks.Count);
  Quote := GetBlock(0) as TMarkDownQuoteBlock;
  AssertEquals('Quote should have 1 block', 1, Quote.Blocks.Count);
  AssertEquals('One item', 1, Quote.Blocks[0].ChildCount);
end;


{ TTestInlineProcessorHook }

procedure TTestInlineProcessor.HandleTextCore;

begin
  if Scanner.Peek='@' then
    begin
    Scanner.NextChar;
    Nodes.AddText(Scanner.Location,'[at]');
    end
  else
    inherited HandleTextCore;
end;


procedure TTestInlineProcessorHook.TearDown;

begin
  TMarkDownParser.DefaultInlineTextProcessorClass:=Nil;
  inherited TearDown;
end;


procedure TTestInlineProcessorHook.TestDefaultInlineTextProcessorClass;

var
  Text: TMarkDownTextBlock;

begin
  TMarkDownParser.DefaultInlineTextProcessorClass:=TTestInlineProcessor;
  SetupParser('a @ b');
  Text := (Doc.Blocks[0] as TMarkDownParagraphBlock).Blocks[0] as TMarkDownTextBlock;
  AssertEquals('Text has one node', 1, Text.Nodes.Count);
  AssertEquals('The installed processor produced the text', 'a [at] b', Text.Nodes[0].NodeText);
end;


{ TTestLists }

procedure TTestLists.TestUnorderedList;
var
  List: TMarkDownListBlock;
  ListItem: TMarkDownListItemBlock;
begin
  SetupParser('* Item 1'#10'* Item 2');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  List := GetBlock(0) as TMarkDownListBlock;
  AssertNotNull('Block should be a list', List);
  AssertFalse('List should be unordered', List.Ordered);
  AssertEquals('List should have 2 items', 2, List.Blocks.Count);
  // Check first list item and its contents
  AssertTrue('First item should be a list item block', List.Blocks[0] is TMarkDownListItemBlock);
  ListItem := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('First list item should contain one inner block', 1, ListItem.Blocks.Count);
  AssertTrue('Inner block of first list item should be a paragraph', ListItem.Blocks[0] is TMarkDownParagraphBlock);
  CheckBlockText('First block',ListItem,'Item 1',True);
  // Check second list item and its contents
  AssertTrue('Second item should be a list item block', List.Blocks[1] is TMarkDownListItemBlock);
  ListItem := List.Blocks[1] as TMarkDownListItemBlock;
  AssertEquals('Second list item should contain one inner block', 1, ListItem.Blocks.Count);
  AssertTrue('Inner block of second list item should be a paragraph', ListItem.Blocks[0] is TMarkDownParagraphBlock);
  CheckBlockText('Second block',ListItem,'Item 2',True);
end;

procedure TTestLists.TestOrderedList;
var
  List: TMarkDownListBlock;
  ListItem: TMarkDownListItemBlock;
begin
  SetupParser('1. First item'#10'2. Second item');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  List := GetBlock(0) as TMarkDownListBlock;
  AssertNotNull('Block should be a list', List);
  AssertTrue('List should be ordered', List.Ordered);
  AssertEquals('List should have 2 items', 2, List.Blocks.Count);
  ListItem := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('First list item should contain one inner block', 1, ListItem.Blocks.Count);
  AssertTrue('Inner block of first list item should be a paragraph', ListItem.Blocks[0] is TMarkDownParagraphBlock);
  CheckBlockText('First block',ListItem,'First item',True);
  ListItem := List.Blocks[1] as TMarkDownListItemBlock;
  AssertEquals('Second list item should contain one inner block', 1, ListItem.Blocks.Count);
  AssertTrue('Inner block of second list item should be a paragraph', ListItem.Blocks[0] is TMarkDownParagraphBlock);
  CheckBlockText('First block',ListItem,'Second item',True);
end;

procedure TTestLists.TestNestedList;
var
  OuterList, InnerList: TMarkDownListBlock;
  OuterItem: TMarkDownListItemBlock;
begin
  SetupParser('* Level 1'#10'  * Level 2');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  OuterList := GetBlock(0) as TMarkDownListBlock;
  AssertNotNull('Outer block should be a list', OuterList);
  AssertEquals('Outer list should have 1 item', 1, OuterList.Blocks.Count);

  OuterItem := OuterList.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Outer item should contain 2 blocks (para, list)', 2, OuterItem.Blocks.Count);

  InnerList := OuterItem.Blocks[1] as TMarkDownListBlock;
  AssertNotNull('Inner block should be a list', InnerList);
end;

function TTestLists.TestList(const Msg,Source : String; aListCount,aListItemCount:  Integer) : TMarkDownListBlock;

begin
  SetupParser(Source);
  AssertEquals(Msg+': Block count',aListCount,FDoc.Blocks.Count);
  AssertTrue(Msg+': First block is list',FDoc.Blocks[0] is TMarkDownListBlock);
  Result:=FDoc.Blocks[0] as TMarkDownListBlock;
  AssertEquals(Msg+': First list item count',aListItemCount,Result.Blocks.Count);

end;

procedure TTestLists.TestNestedList2;

var
  OuterList, InnerList: TMarkDownListBlock;
  OuterItem1, OuterItem2: TMarkDownListItemBlock;

begin
  //
  OuterList:=TestList(
    'Basic nested unordered list',
    '* First item'#10 +
    '  * Sub item 1'#10 +
    '  * Sub item 2'#10 +
    '* Second item',
    1,2
  );
  // Should have 1 top-level list
  // Outer list should have 2 items
  if OuterList.Blocks.Count <> 2 then
    Fail('Outer list block count not 2');
  if not (OuterList.Blocks[0] is TMarkDownListItemBlock) then
    Fail('Outer list block 0 not item');
  if not (OuterList.Blocks[1] is TMarkDownListItemBlock) then
    Fail('Outer list block 1 not item');

  OuterItem1 := OuterList.Blocks[0] as TMarkDownListItemBlock;
  OuterItem2 := OuterList.Blocks[1] as TMarkDownListItemBlock;

  // First outer item should contain paragraph + nested list
  if OuterItem1.Blocks.Count <> 2 then
    Fail('Item 1 block count not 2');
  if not (OuterItem1.Blocks[0] is TMarkDownParagraphBlock) then
    Fail('Item 1 block 0 not paragraph');
  if not (OuterItem1.Blocks[1] is TMarkDownListBlock) then
    Fail('Item 1 block 1 not list');

  InnerList := OuterItem1.Blocks[1] as TMarkDownListBlock;

    // Inner list should have 2 items
  if InnerList.Blocks.Count <> 2 then
    Fail('Item 1 - Inner list block count not 2');
  // Second outer item should contain only a paragraph
  if OuterItem2.Blocks.Count <> 1 then
    Fail('Item Inner list block item count not 1');
  if not (OuterItem2.Blocks[0] is TMarkDownParagraphBlock) then
    Fail('Item Inner list block item content not paragraph ');

end;

procedure TTestLists.TestNestedList3;
var
  List1, List2, List3: TMarkDownListBlock;
  Item1, Item2: TMarkDownListItemBlock;
begin
  // Test 2: Deep nesting (3 levels)
  List1:=TestList(
    'Deep nested list (3 levels)',
    '* Level 1'#10 +
    '  * Level 2'#10 +
    '    * Level 3',
    1,1
  );
  if List1.Blocks.Count <> 1 then
    Fail('List 1 must have 1 item');

  Item1 := List1.Blocks[0] as TMarkDownListItemBlock;
  if Item1.Blocks.Count <> 2 then
    Fail('List 1 item 1 has 2 blocks');

  List2 := Item1.Blocks[1] as TMarkDownListBlock;
  if List2.Blocks.Count <> 1 then
    Fail('List 1 item 1 has 1 list sub');

  Item2 := List2.Blocks[0] as TMarkDownListItemBlock;
  if Item2.Blocks.Count <> 2 then
    // paragraph + nested list
    Fail('List 2 item 1 has 2 blocks');

  List3 := Item2.Blocks[1] as TMarkDownListBlock;
  if List3.Blocks.Count <> 1 then // deepest level has 1 item
    Fail('List 3 item 1 has 1 block');

end;

procedure TTestLists.TestNestedList4;
var
  OuterList, InnerList: TMarkDownListBlock;
  OuterItem: TMarkDownListItemBlock;
begin
  // Test 3: Ordered nested list
  OuterList:=TestList(
    'Ordered nested list',
    '1. First item'#10 +
    '   1. Sub item 1'#10 +
    '   2. Sub item 2'#10 +
    '2. Second item',
    1,2
  );
  if Not OuterList.Ordered then
    Fail('Outer must be ordered');
  if OuterList.Blocks.Count <> 2 then
    Fail('Outer has 2 items');

  OuterItem := OuterList.Blocks[0] as TMarkDownListItemBlock;
  if OuterItem.Blocks.Count <> 2 then
    Fail('Outer item 1 has 2 children');

  InnerList := OuterItem.Blocks[1] as TMarkDownListBlock;
  if not InnerList.Ordered then
    Fail('Inner is unordered');

  if InnerList.Blocks.Count <> 2 then
    Fail('Inner list has 2 items');

end;

procedure TTestLists.TestNestedList5;
var
  OuterList, InnerList: TMarkDownListBlock;
  OuterItem: TMarkDownListItemBlock;
begin
  // Test 4: Mixed nesting (unordered containing ordered)
  OuterList:=TestList(
    'Mixed nested list (unordered -> ordered)',
    '* First item'#10 +
    '  1. Sub item 1'#10 +
    '  2. Sub item 2',
    1,1
  );
  if OuterList.Ordered then
    Fail('Outer list must be unordered');
  if OuterList.Blocks.Count <> 1 then
    Fail('Outer list must have 1 item');
  OuterItem := OuterList.Blocks[0] as TMarkDownListItemBlock;
  if OuterItem.Blocks.Count <> 2 then
    Fail('Outer item must have 2 children');

  InnerList := OuterItem.Blocks[1] as TMarkDownListBlock;
  if not InnerList.Ordered then
    Fail('Inner list must be ordered');
  if InnerList.Blocks.Count <> 2 then
    Fail('Inner list must have 2 items');
end;

procedure TTestLists.TestNestedList6;
var
  OuterList, InnerList: TMarkDownListBlock;
  OuterItem: TMarkDownListItemBlock;
begin
   OuterList:=TestList(
     'Multiple consecutive nested items',
     '* First item'#10 +
     '  * Sub item 1'#10 +
     '  * Sub item 2'#10 +
     '  * Sub item 3'#10 +
     '  * Sub item 4'#10 +
     '* Second item',
     1,2
   );
  OuterItem := OuterList.Blocks[0] as TMarkDownListItemBlock;
  if OuterItem.Blocks.Count <> 2 then
    Fail('Outer item should have 2 children');
  InnerList := OuterItem.Blocks[1] as TMarkDownListBlock;
  if not InnerList.Blocks.Count = 4 then
    Fail('Inner list item should have 4 items');
end;

procedure TTestLists.TestOrderedListThenParagraph;
var
  List: TMarkDownListBlock;
begin
  // The blank line + non-indented paragraph ends the ordered list; "After." is a
  // top-level paragraph, not a second paragraph of the last item.
  SetupParser('1. one'#10'2. two'#10#10'After.');
  AssertEquals('Document should have 2 blocks (list + paragraph)', 2, Doc.Blocks.Count);
  AssertTrue('First block is a list', Doc.Blocks[0] is TMarkDownListBlock);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertEquals('List should have exactly 2 items', 2, List.Blocks.Count);
  AssertTrue('Second block is a paragraph', Doc.Blocks[1] is TMarkDownParagraphBlock);
  CheckBlockText('Trailing paragraph', Doc.Blocks[1], 'After.', False);
end;

procedure TTestLists.TestUnorderedListThenParagraph;
var
  List: TMarkDownListBlock;
begin
  SetupParser('* one'#10'* two'#10#10'After.');
  AssertEquals('Document should have 2 blocks (list + paragraph)', 2, Doc.Blocks.Count);
  AssertTrue('First block is a list', Doc.Blocks[0] is TMarkDownListBlock);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertEquals('List should have exactly 2 items', 2, List.Blocks.Count);
  AssertTrue('Second block is a paragraph', Doc.Blocks[1] is TMarkDownParagraphBlock);
  CheckBlockText('Trailing paragraph', Doc.Blocks[1], 'After.', False);
end;

procedure TTestLists.TestOrderedListThenHeading;
var
  List: TMarkDownListBlock;
begin
  // An interrupting block (a heading) at column 0 also ends the list.
  SetupParser('1. one'#10'2. two'#10#10'# Heading');
  AssertEquals('Document should have 2 blocks (list + heading)', 2, Doc.Blocks.Count);
  AssertTrue('First block is a list', Doc.Blocks[0] is TMarkDownListBlock);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertEquals('List should have exactly 2 items', 2, List.Blocks.Count);
  AssertTrue('Second block is a heading', Doc.Blocks[1] is TMarkDownHeadingBlock);
end;

procedure TTestLists.TestLooseListPreserved;
var
  List: TMarkDownListBlock;
begin
  // A blank line between two markers makes the list loose; it stays ONE list.
  SetupParser('1. one'#10#10'2. two');
  AssertEquals('Document should have 1 block (the loose list)', 1, Doc.Blocks.Count);
  AssertTrue('Block is a list', Doc.Blocks[0] is TMarkDownListBlock);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertEquals('Loose list should still have 2 items', 2, List.Blocks.Count);
end;

procedure TTestLists.TestLazyContinuationPreserved;
var
  List: TMarkDownListBlock;
  Item: TMarkDownListItemBlock;
begin
  // No blank line: "continued" is a lazy continuation of the first item, so the
  // document is still a single list and "continued" did not leak out as a block.
  SetupParser('1. one'#10'continued'#10'2. two');
  AssertEquals('Document should have 1 block (the list)', 1, Doc.Blocks.Count);
  AssertTrue('Block is a list', Doc.Blocks[0] is TMarkDownListBlock);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertEquals('List should have 2 items', 2, List.Blocks.Count);
  Item := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('First item should have a single paragraph (with the lazy line)', 1, Item.Blocks.Count);
end;

procedure TTestLists.TestItemContentIndentParagraph;
var
  List: TMarkDownListBlock;
  Item: TMarkDownListItemBlock;
begin
  // Content column is 4, so a continuation indented 4 is item content, not code.
  SetupParser('-   one'#10#10'    continued');
  AssertEquals('Document should have 1 block (the list)', 1, Doc.Blocks.Count);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertEquals('List should have 1 item', 1, List.Blocks.Count);
  Item := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 2 paragraphs', 2, Item.Blocks.Count);
  AssertTrue('First item block is a paragraph', Item.Blocks[0] is TMarkDownParagraphBlock);
  AssertTrue('Second item block is a paragraph', Item.Blocks[1] is TMarkDownParagraphBlock);
end;


procedure TTestLists.TestItemContentIndentCodeBlock;
var
  List: TMarkDownListBlock;
  Item: TMarkDownListItemBlock;
begin
  // Content column is 4, so a code block inside the item starts at column 8.
  SetupParser('-   one'#10#10'        code');
  AssertEquals('Document should have 1 block (the list)', 1, Doc.Blocks.Count);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  Item := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 2 blocks', 2, Item.Blocks.Count);
  AssertTrue('Second item block is a code block', Item.Blocks[1] is TMarkDownCodeBlock);
end;


procedure TTestLists.TestItemContentIndentThematicBreak;
var
  List: TMarkDownListBlock;
  Item: TMarkDownListItemBlock;
begin
  SetupParser('-   one'#10#10'    ---'#10#10'    two');
  AssertEquals('Document should have 1 block (the list)', 1, Doc.Blocks.Count);
  List := Doc.Blocks[0] as TMarkDownListBlock;
  Item := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 3 blocks', 3, Item.Blocks.Count);
  AssertTrue('Second item block is a thematic break', Item.Blocks[1] is TMarkDownThematicBreakBlock);
end;


procedure TTestLists.TestItemContentIndentWideMarker;
var
  List: TMarkDownListBlock;
  Item: TMarkDownListItemBlock;
begin
  // Content column is 2, so a continuation indented 4 is still item content.
  SetupParser('- one'#10#10'    continued');
  List := Doc.Blocks[0] as TMarkDownListBlock;
  Item := List.Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 2 paragraphs', 2, Item.Blocks.Count);
  AssertTrue('Second item block is a paragraph', Item.Blocks[1] is TMarkDownParagraphBlock);
end;


procedure TTestLists.TestItemTwoParagraphsAreLoose;
var
  List: TMarkDownListBlock;
begin
  SetupParser('-   one'#10#10'    continued');
  List := Doc.Blocks[0] as TMarkDownListBlock;
  AssertTrue('An item holding two paragraphs makes the list loose', List.Loose);
end;


procedure TTestLists.TestItemHeading;
var
  Item: TMarkDownListItemBlock;
  Heading: TMarkDownHeadingBlock;
begin
  SetupParser('-   one'#10#10'    ## Heading');
  Item := (Doc.Blocks[0] as TMarkDownListBlock).Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 2 blocks', 2, Item.Blocks.Count);
  AssertTrue('Second item block is a heading', Item.Blocks[1] is TMarkDownHeadingBlock);
  Heading := Item.Blocks[1] as TMarkDownHeadingBlock;
  AssertEquals('Heading level', 2, Heading.Level);
  AssertEquals('The markers are not part of the heading text', 'Heading',
               (Heading.Blocks[0] as TMarkDownTextBlock).Text);
end;


procedure TTestLists.TestItemQuote;
var
  Item: TMarkDownListItemBlock;
begin
  SetupParser('-   one'#10#10'    > quoted');
  Item := (Doc.Blocks[0] as TMarkDownListBlock).Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 2 blocks', 2, Item.Blocks.Count);
  AssertTrue('Second item block is a quote', Item.Blocks[1] is TMarkDownQuoteBlock);
end;


procedure TTestLists.TestItemQuoteList;
var
  List: TMarkDownListBlock;
  Quote: TMarkDownQuoteBlock;
begin
  SetupParser('- item'#10'  > quoted'#10'  > - inner'#10'- next');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  List := GetBlock(0) as TMarkDownListBlock;
  AssertEquals('Two items in the outer list', 2, List.Blocks.Count);
  Quote := List.Blocks[0].Children[1] as TMarkDownQuoteBlock;
  AssertEquals('Quote has a paragraph and a list', 2, Quote.Blocks.Count);
  AssertTrue('List inside the quote', Quote.Blocks[1] is TMarkDownListBlock);
end;


procedure TTestLists.TestListThenThematicBreak;

begin
  SetupParser('- a'#10#10'---');
  AssertEquals('List and thematic break', 2, Doc.Blocks.Count);
  AssertEquals('One item', 1, GetBlock(0).ChildCount);
  AssertTrue('Second block is a thematic break', GetBlock(1) is TMarkDownThematicBreakBlock);
end;


procedure TTestLists.TestSpacedThematicBreakEndsList;

begin
  SetupParser('- a'#10'- - -');
  AssertEquals('List and thematic break', 2, Doc.Blocks.Count);
  AssertTrue('Second block is a thematic break', GetBlock(1) is TMarkDownThematicBreakBlock);
end;


procedure TTestLists.TestItemFencedCode;
var
  Item: TMarkDownListItemBlock;
  Code: TMarkDownCodeBlock;
begin
  SetupParser('-   one'#10#10'    ```pascal'#10'    begin end.'#10'    ```');
  Item := (Doc.Blocks[0] as TMarkDownListBlock).Blocks[0] as TMarkDownListItemBlock;
  AssertEquals('Item should have 2 blocks', 2, Item.Blocks.Count);
  AssertTrue('Second item block is a code block', Item.Blocks[1] is TMarkDownCodeBlock);
  Code := Item.Blocks[1] as TMarkDownCodeBlock;
  AssertEquals('Code block language', 'pascal', Code.Lang);
  AssertEquals('The closing fence ends the block', 1, Code.Blocks.Count);
  AssertEquals('The item indentation is stripped from the code',
               'begin end.', (Code.Blocks[0] as TMarkDownTextBlock).Text);
end;


{ TTestThematicBreaks }

procedure TTestThematicBreaks.TestAsteriskBreak;
begin
  SetupParser('***');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  AssertTrue('Block should be a thematic break', GetBlock(0) is TMarkDownThematicBreakBlock);
end;

procedure TTestThematicBreaks.TestUnderscoreBreak;
begin
  SetupParser('___');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  AssertTrue('Block should be a thematic break', GetBlock(0) is TMarkDownThematicBreakBlock);
end;

{ TTestTables }

procedure TTestTables.TestSimpleTable;
var
  Table: TMarkDownTableBlock;
  HeaderRow, BodyRow: TMarkDownTableRowBlock;
begin
  SetupParser(
    '| Header 1 | Header 2 |'#10 +
    '|----------|----------|'#10 +
    '| Cell 1   | Cell 2   |'
  );
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  Table := GetBlock(0) as TMarkDownTableBlock;
  AssertNotNull('Block should be a table', Table);
  AssertEquals('Table should have 2 rows', 2, Table.Blocks.Count);
  AssertEquals('Table should have 2 columns', 2, Length(Table.Columns));

  HeaderRow := Table.Blocks[0] as TMarkDownTableRowBlock;
  AssertNotNull('First row should be a table row', HeaderRow);
  AssertEquals('Header row should have 2 cells', 2, HeaderRow.Blocks.Count);
  CheckTextnodeText('Header row, Cell 1',HeaderRow.Blocks[0],'Header 1');
  CheckTextnodeText('Header row, Cell 2',HeaderRow.Blocks[1],'Header 2');

  BodyRow := Table.Blocks[1] as TMarkDownTableRowBlock;
  AssertNotNull('Second row should be a table row', BodyRow);
  AssertEquals('Body row should have 2 cells', 2, BodyRow.Blocks.Count);
  CheckTextnodeText('Body Row 1, Cell 1',BodyRow.Blocks[0],'Cell 1');
  CheckTextnodeText('Body Row 1, Cell 2',BodyRow.Blocks[1],'Cell 2');
end;


{ TTestFrontmatter }

procedure TTestFrontmatter.TestYAMLFrontmatter;
var
  Block: TMarkdownFrontmatterBlock;
begin
  SetupParser('---'#10'title: Hello'#10'---'#10#10'# Content');
  AssertNotNull('Document frontmatter should exist', Doc.Frontmatter);
  Block := Doc.Frontmatter;
  AssertEquals('Frontmatter type should be YAML', Ord(fmtYAML), Ord(Block.FrontMatterType));
  AssertEquals('Frontmatter content count', 1, Block.Content.Count);
  AssertEquals('Frontmatter content', 'title: Hello', Block.Content[0]);
  AssertTrue('Document should have blocks after frontmatter', Doc.Blocks.Count > 1);
end;

procedure TTestFrontmatter.TestTOMLFrontmatter;
var
  Block: TMarkdownFrontmatterBlock;
begin
  SetupParser('+++'#10'title = "Hello"'#10'+++');
  AssertNotNull('Document frontmatter should exist', Doc.Frontmatter);
  Block := Doc.Frontmatter;
  AssertEquals('Frontmatter type should be TOML', Ord(fmtTOML), Ord(Block.FrontMatterType));
  AssertEquals('Frontmatter content count', 1, Block.Content.Count);
  AssertEquals('Frontmatter content', 'title = "Hello"', Block.Content[0]);
end;

procedure TTestFrontmatter.TestJSONFrontmatter;
var
  Block: TMarkdownFrontmatterBlock;
begin
  SetupParser(';;;'#10'{"title":"Hello"}'#10';;;');
  AssertNotNull('Document frontmatter should exist', Doc.Frontmatter);
  Block := Doc.Frontmatter;
  AssertEquals('Frontmatter type should be JSON', Ord(fmtJSON), Ord(Block.FrontMatterType));
  AssertEquals('Frontmatter content count', 1, Block.Content.Count);
  AssertEquals('Frontmatter content', '{"title":"Hello"}', Block.Content[0]);
end;

procedure TTestFrontmatter.TestFrontmatterNotOnLine1;
begin
  SetupParser(#10'---'#10'title: Test'#10'---');
  AssertNull('Document should have no frontmatter', Doc.Frontmatter);
end;

procedure TTestFrontmatter.TestFrontmatterWithContent;
var
  lBlock: TMarkdownBlock;
begin
  SetupParser('---'#10'title: Test'#10'---'#10#10'Hello world');
  AssertNotNull('Document frontmatter should exist', Doc.Frontmatter);
  AssertEquals('Document should have 2 blocks', 2, Doc.Blocks.Count);
  lBlock := Doc.Blocks[1];
  AssertTrue('Second block should be a paragraph', lBlock is TMarkdownParagraphBlock);
end;

procedure TTestFrontmatter.TestNoFrontmatter;
begin
  SetupParser('# Just a heading');
  AssertNull('Document should have no frontmatter', Doc.Frontmatter);
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  AssertTrue('Block should be a heading', Doc.Blocks[0] is TMarkdownHeadingBlock);
end;

procedure TTestFrontmatter.TestEmptyFrontmatter;
begin
  SetupParser('---'#10'---'#10#10'Content');
  AssertNotNull('Document frontmatter should exist', Doc.Frontmatter);
  AssertEquals('Frontmatter content should be empty', 0, Doc.Frontmatter.Content.Count);
end;

procedure TTestFrontmatter.TestUnclosedFrontmatter;
begin
  SetupParser('---'#10'title: Test'#10'more content');
  AssertNotNull('Document frontmatter should exist', Doc.Frontmatter);
  AssertEquals('Frontmatter content count', 2, Doc.Frontmatter.Content.Count);
  AssertEquals('Frontmatter line 1', 'title: Test', Doc.Frontmatter.Content[0]);
  AssertEquals('Frontmatter line 2', 'more content', Doc.Frontmatter.Content[1]);
end;

{ TExtensionTestCase }

procedure TExtensionTestCase.SetUp;

begin
  inherited SetUp;
  FMessages:=TStringList.Create;
end;


procedure TExtensionTestCase.TearDown;

begin
  FreeAndNil(FMessages);
  inherited TearDown;
end;


procedure TExtensionTestCase.DoMessage(Sender: TObject; aLevel: TMarkdownMessageLevel; aLine: Integer; const aMessage: String);

begin
  FMessages.Add(Format('%d:%d:%s',[Ord(aLevel),aLine,aMessage]));
end;


procedure TExtensionTestCase.SetupExt(const aText: String; aOptions: TMarkdownOptions);

begin
  FParser.Options:=aOptions;
  FParser.OnMessage:=@DoMessage;
  SetupParser(aText);
end;


function TExtensionTestCase.TextBlockOf(aBlock: TMarkdownBlock): TMarkdownTextBlock;

var
  lBlock : TMarkdownBlock;
begin
  lBlock:=aBlock;
  while Assigned(lBlock) and not (lBlock is TMarkdownTextBlock) and (lBlock.ChildCount>0) do
    lBlock:=lBlock.Children[0];
  AssertTrue('Found a text block',lBlock is TMarkdownTextBlock);
  Result:=TMarkdownTextBlock(lBlock);
end;


function TExtensionTestCase.PlainOf(aBlock: TMarkdownBlock): String;

begin
  Result:=TextBlockOf(aBlock).Nodes.PlainText;
end;

{ TTestComments }

procedure TTestComments.TestCommentBlock;

var
  lComment : TMarkdownCommentBlock;
begin
  SetupExt('<!-- hello -->');
  AssertEquals('One block',1,Doc.Blocks.Count);
  AssertEquals('Comment block',TMarkdownCommentBlock,GetBlock(0).ClassType);
  lComment:=TMarkdownCommentBlock(GetBlock(0));
  AssertEquals('Comment text',' hello ',lComment.Text);
  AssertFalse('Not a marker',lComment.IsMarker);
end;


procedure TTestComments.TestCommentBlockMultiLine;

begin
  SetupExt('<!-- first'#10'second -->'#10'After');
  AssertEquals('Two blocks',2,Doc.Blocks.Count);
  AssertEquals('Comment text spans lines',' first'#10'second ',TMarkdownCommentBlock(GetBlock(0)).Text);
  AssertEquals('Paragraph after comment',TMarkdownParagraphBlock,GetBlock(1).ClassType);
end;


procedure TTestComments.TestCommentBlockTextAfterClose;

begin
  SetupExt('<!-- a --> trailing'#10'Next');
  AssertEquals('Two blocks',2,Doc.Blocks.Count);
  AssertEquals('Comment block',TMarkdownCommentBlock,GetBlock(0).ClassType);
  AssertEquals('Next line is a paragraph','Next',PlainOf(GetBlock(1)));
end;


procedure TTestComments.TestCommentBlockMarker;

var
  lComment : TMarkdownCommentBlock;
begin
  SetupExt('<!-- index[msgnr]: 1000 -->');
  lComment:=TMarkdownCommentBlock(GetBlock(0));
  AssertTrue('Is marker',lComment.IsMarker);
  AssertEquals('Marker name','index',lComment.MarkerName);
  AssertEquals('Marker argument','msgnr',lComment.MarkerArgument);
  AssertEquals('Marker value','1000',lComment.MarkerValue);
  AssertEquals('Document has one marker',1,Doc.Markers.Count);
  AssertSame('Marker block',lComment,Doc.Markers[0].Block);
  AssertNull('Block marker has no node',Doc.Markers[0].Node);
  AssertEquals('Block marker node index',-1,Doc.Markers[0].NodeIndex);
end;


procedure TTestComments.TestCommentDoesNotInterruptParagraph;

var
  lMarker : TMarkdownMarker;
begin
  SetupExt('Some text'#10'<!-- index: Tokens!Comments -->');
  AssertEquals('One paragraph',1,Doc.Blocks.Count);
  AssertEquals('Paragraph',TMarkdownParagraphBlock,GetBlock(0).ClassType);
  AssertEquals('Comment has no visible text','Some text',Trim(PlainOf(GetBlock(0))));
  AssertEquals('One marker',1,Doc.Markers.Count);
  lMarker:=Doc.Markers[0];
  AssertNotNull('Inline marker has a node',lMarker.Node);
  AssertEquals('Inline marker node kind',Ord(nkComment),Ord(lMarker.Node.Kind));
  AssertEquals('Inline marker value','Tokens!Comments',lMarker.Value);
  AssertEquals('Inline marker block is the text block',TMarkdownTextBlock,lMarker.Block.ClassType);
end;


procedure TTestComments.TestPlainCommentIsNoMarker;

begin
  SetupExt('<!-- just a remark -->'#10#10'Text <!-- another remark --> here');
  AssertEquals('No markers',0,Doc.Markers.Count);
end;


procedure TTestComments.TestMarkerInsideEmphasis;

var
  lMarker : TMarkdownMarker;
begin
  SetupExt('*with <!-- index: emph --> marker*');
  AssertEquals('One marker',1,Doc.Markers.Count);
  lMarker:=Doc.Markers[0];
  AssertEquals('Marker value','emph',lMarker.Value);
  AssertTrue('Marker node is emphasized',nsEmph in lMarker.Node.Styles);
end;


procedure TTestComments.TestCommentBetweenListItems;

begin
  SetupExt('- a'#10#10'<!-- index: between -->'#10#10'- b');
  AssertEquals('List, comment, list',3,Doc.Blocks.Count);
  AssertEquals('First list',TMarkdownListBlock,GetBlock(0).ClassType);
  AssertEquals('Comment',TMarkdownCommentBlock,GetBlock(1).ClassType);
  AssertEquals('Second list',TMarkdownListBlock,GetBlock(2).ClassType);
  AssertEquals('Marker collected',1,Doc.Markers.Count);
end;


procedure TTestComments.TestMarkersInDocumentOrder;

begin
  SetupExt('<!-- a: 1 -->'#10#10'x <!-- b: 2 --> y'#10#10'<!-- c: 3 -->');
  AssertEquals('Three markers',3,Doc.Markers.Count);
  AssertEquals('First','a',Doc.Markers[0].Name);
  AssertEquals('Second','b',Doc.Markers[1].Name);
  AssertEquals('Third','c',Doc.Markers[2].Name);
end;


procedure TTestComments.TestCommentOptionOff;

begin
  SetupExt('<!-- hello -->',[]);
  AssertEquals('Paragraph without option',TMarkdownParagraphBlock,GetBlock(0).ClassType);
  AssertEquals('No markers without option',0,Doc.Markers.Count);
end;

{ TTestAttributes }

procedure TTestAttributes.TestATXHeading;

var
  lHeading : TMarkdownHeadingBlock;
begin
  SetupExt('## Title {#my-id .c1 .c2 key=value other="quoted value"}');
  lHeading:=GetBlock(0) as TMarkdownHeadingBlock;
  AssertEquals('Heading id','my-id',lHeading.ID);
  AssertEquals('Class count',2,Length(lHeading.Classes));
  AssertEquals('First class','c1',lHeading.Classes[0]);
  AssertEquals('Second class','c2',lHeading.Classes[1]);
  AssertEquals('key attribute','value',lHeading.Attrs.Values['key']);
  AssertEquals('quoted attribute','quoted value',lHeading.Attrs.Values['other']);
  AssertEquals('Heading text without attributes','Title',PlainOf(lHeading));
end;


procedure TTestAttributes.TestATXHeadingClosingHashes;

var
  lHeading : TMarkdownHeadingBlock;
begin
  SetupExt('## Title ## {#t}');
  lHeading:=GetBlock(0) as TMarkdownHeadingBlock;
  AssertEquals('Heading id','t',lHeading.ID);
  AssertEquals('Heading text','Title',PlainOf(lHeading));
end;


procedure TTestAttributes.TestSetextHeading;

var
  lPar : TMarkdownParagraphBlock;
begin
  SetupExt('Title {#st}'#10'=====');
  lPar:=GetBlock(0) as TMarkdownParagraphBlock;
  AssertEquals('Setext level',1,lPar.Header);
  AssertEquals('Setext id','st',lPar.ID);
  AssertEquals('Setext text','Title',PlainOf(lPar));
end;


procedure TTestAttributes.TestFencedCode;

var
  lCode : TMarkdownCodeBlock;
begin
  SetupExt('```pascal {#lst-hello .numbered title="hello.pp"}'#10'begin'#10'```');
  lCode:=GetBlock(0) as TMarkdownCodeBlock;
  AssertEquals('Language stays first word','pascal',lCode.Lang);
  AssertEquals('Code id','lst-hello',lCode.ID);
  AssertEquals('Code class','numbered',lCode.Classes[0]);
  AssertEquals('Code title','hello.pp',lCode.Attrs.Values['title']);
end;


procedure TTestAttributes.TestAnchorRegistered;

begin
  SetupExt('# A {#first}'#10#10'```'#10'x'#10'``` ');
  AssertSame('Anchor maps to heading',GetBlock(0),Doc.FindAnchor('first'));
end;


procedure TTestAttributes.TestDuplicateID;

begin
  SetupExt('# A {#x}'#10#10'# B {#x}');
  AssertEquals('One message',1,Messages.Count);
  AssertTrue('Message names the id',Pos('"x"',Messages[0])>0);
  AssertTrue('Message is an error on line 3',Messages[0].StartsWith(IntToStr(Ord(mlError))+':3:'));
  AssertSame('First registration wins',GetBlock(0),Doc.FindAnchor('x'));
end;


procedure TTestAttributes.TestInvalidSpecStaysText;

var
  lHeading : TMarkdownHeadingBlock;
begin
  SetupExt('## Title {not valid}',MarkdownDocExtensions-[mdoHeadingIds]);
  lHeading:=GetBlock(0) as TMarkdownHeadingBlock;
  AssertEquals('No id','',lHeading.ID);
  AssertEquals('Text keeps braces','Title {not valid}',PlainOf(lHeading));
end;


procedure TTestAttributes.TestAttributesOptionOff;

var
  lHeading : TMarkdownHeadingBlock;
begin
  SetupExt('## Title {#id}',[]);
  lHeading:=GetBlock(0) as TMarkdownHeadingBlock;
  AssertEquals('No id without option','',lHeading.ID);
  AssertEquals('Text keeps specification','Title {#id}',PlainOf(lHeading));
end;

{ TTestHeadingIDs }

procedure TTestHeadingIDs.TestAutoID;

begin
  SetupExt('# Hello, *World*!');
  AssertEquals('Automatic id','hello-world',GetBlock(0).ID);
  AssertSame('Automatic id registered',GetBlock(0),Doc.FindAnchor('hello-world'));
end;


procedure TTestHeadingIDs.TestAutoIDCollision;

begin
  SetupExt('# Intro'#10'# Intro'#10'# Intro');
  AssertEquals('First','intro',GetBlock(0).ID);
  AssertEquals('Second','intro-1',GetBlock(1).ID);
  AssertEquals('Third','intro-2',GetBlock(2).ID);
  AssertEquals('Collisions between automatic ids are not reported',0,Messages.Count);
end;


procedure TTestHeadingIDs.TestAutoIDExplicitClash;

begin
  SetupExt('# Foo'#10'# Bar {#foo}');
  AssertEquals('Explicit id kept','foo',GetBlock(1).ID);
  AssertEquals('Automatic id gets suffix','foo-1',GetBlock(0).ID);
  AssertEquals('Clash reported',1,Messages.Count);
end;


procedure TTestHeadingIDs.TestAutoIDUnicode;

begin
  SetupExt('# Café über_alles 2');
  AssertEquals('Unicode letters kept','café-über_alles-2',GetBlock(0).ID);
end;


procedure TTestHeadingIDs.TestAutoIDSetext;

begin
  SetupExt('Some Title'#10'----');
  AssertEquals('Setext automatic id','some-title',GetBlock(0).ID);
end;


procedure TTestHeadingIDs.TestHeadingIDsOptionOff;

begin
  SetupExt('# Intro',MarkdownDocExtensions-[mdoHeadingIds]);
  AssertEquals('No automatic id','',GetBlock(0).ID);
end;

{ TTestDefinitionLists }

procedure TTestDefinitionLists.TestSimple;

var
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('Term'#10': Definition');
  AssertEquals('One block',1,Doc.Blocks.Count);
  lList:=GetBlock(0) as TMarkdownDefinitionListBlock;
  AssertFalse('Tight list',lList.Loose);
  AssertEquals('Term and definition',2,lList.ChildCount);
  AssertEquals('Term',TMarkdownDefinitionTermBlock,lList.Children[0].ClassType);
  AssertEquals('Term text','Term',PlainOf(lList.Children[0]));
  AssertEquals('Definition',TMarkdownDefinitionBlock,lList.Children[1].ClassType);
  AssertEquals('Definition text','Definition',PlainOf(lList.Children[1]));
end;


procedure TTestDefinitionLists.TestMultipleDefinitions;

var
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('Term'#10': One'#10': Two');
  lList:=GetBlock(0) as TMarkdownDefinitionListBlock;
  AssertEquals('Term and two definitions',3,lList.ChildCount);
  AssertEquals('First definition','One',PlainOf(lList.Children[1]));
  AssertEquals('Second definition','Two',PlainOf(lList.Children[2]));
  AssertFalse('Tight list',lList.Loose);
end;


procedure TTestDefinitionLists.TestMultiParagraphDefinition;

var
  lList : TMarkdownDefinitionListBlock;
  lDef : TMarkdownBlock;
begin
  SetupExt('Term'#10':   Definition, first paragraph.'#10#10'    Second paragraph, indented 4.'#10#10'After');
  AssertEquals('List and paragraph',2,Doc.Blocks.Count);
  lList:=GetBlock(0) as TMarkdownDefinitionListBlock;
  lDef:=lList.Children[1];
  AssertEquals('Content indentation',4,lDef.ContentIndentation);
  AssertEquals('Two paragraphs in definition',2,lDef.ChildCount);
  AssertEquals('Second paragraph','Second paragraph, indented 4.',PlainOf(lDef.Children[1]));
  AssertTrue('Loose list',lList.Loose);
  AssertEquals('Paragraph after the list','After',PlainOf(GetBlock(1)));
end;


procedure TTestDefinitionLists.TestBlankLineBeforeDefinition;

var
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('Term'#10#10': Definition');
  lList:=GetBlock(0) as TMarkdownDefinitionListBlock;
  AssertEquals('Term text','Term',PlainOf(lList.Children[0]));
  AssertTrue('Blank line makes the list loose',lList.Loose);
end;


procedure TTestDefinitionLists.TestTermIsLastParagraphLine;

var
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('Para line'#10'Term'#10': Definition');
  AssertEquals('Paragraph and list',2,Doc.Blocks.Count);
  AssertEquals('Earlier lines stay in the paragraph','Para line',PlainOf(GetBlock(0)));
  lList:=GetBlock(1) as TMarkdownDefinitionListBlock;
  AssertEquals('Term is the last line','Term',PlainOf(lList.Children[0]));
end;


procedure TTestDefinitionLists.TestSecondTermJoinsList;

var
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('Term'#10': One'#10#10'Second term'#10': Two'#10': Three');
  AssertEquals('One list',1,Doc.Blocks.Count);
  lList:=GetBlock(0) as TMarkdownDefinitionListBlock;
  AssertEquals('Two terms and three definitions',5,lList.ChildCount);
  AssertEquals('Second term',TMarkdownDefinitionTermBlock,lList.Children[2].ClassType);
  AssertEquals('Second term text','Second term',PlainOf(lList.Children[2]));
end;


procedure TTestDefinitionLists.TestLazyContinuation;

var
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('Term'#10': Definition line'#10'lazy line');
  lList:=GetBlock(0) as TMarkdownDefinitionListBlock;
  AssertEquals('Lazy line continues the definition','Definition line'#10'lazy line',TextBlockOf(lList.Children[1]).Text);
end;


procedure TTestDefinitionLists.TestNoTerm;

begin
  SetupExt(': not a definition');
  AssertEquals('Paragraph',TMarkdownParagraphBlock,GetBlock(0).ClassType);
end;


procedure TTestDefinitionLists.TestDefinitionListInListItem;

var
  lItem : TMarkdownBlock;
  lList : TMarkdownDefinitionListBlock;
begin
  SetupExt('- item'#10'  Term'#10'  : Definition'#10#10'- next');
  lItem:=GetBlock(0).Children[0];
  AssertEquals('Item has paragraph and definition list',2,lItem.ChildCount);
  lList:=lItem.Children[1] as TMarkdownDefinitionListBlock;
  AssertEquals('Term in item','Term',PlainOf(lList.Children[0]));
  AssertEquals('Definition in item','Definition',PlainOf(lList.Children[1]));
  AssertEquals('Second item kept',2,GetBlock(0).ChildCount);
end;


procedure TTestDefinitionLists.TestDefinitionListsOptionOff;

begin
  SetupExt('Term'#10': Definition',[]);
  AssertEquals('One paragraph',1,Doc.Blocks.Count);
  AssertEquals('Paragraph',TMarkdownParagraphBlock,GetBlock(0).ClassType);
end;

{ TTestAlerts }

procedure TTestAlerts.TestNote;

var
  lAlert : TMarkdownAlertBlock;
begin
  SetupExt('> [!NOTE]'#10'> This is a note');
  AssertEquals('Alert block',TMarkdownAlertBlock,GetBlock(0).ClassType);
  lAlert:=TMarkdownAlertBlock(GetBlock(0));
  AssertEquals('Alert type',Ord(atNote),Ord(lAlert.AlertType));
  AssertEquals('Marker line removed','This is a note',PlainOf(lAlert));
end;


procedure TTestAlerts.TestCaseInsensitive;

begin
  SetupExt('> [!warning]'#10'> Careful');
  AssertEquals('Alert block',TMarkdownAlertBlock,GetBlock(0).ClassType);
  AssertEquals('Alert type',Ord(atWarning),Ord(TMarkdownAlertBlock(GetBlock(0)).AlertType));
end;


procedure TTestAlerts.TestMarkerOnlyLine;

var
  lAlert : TMarkdownAlertBlock;
begin
  SetupExt('> [!TIP]'#10'>'#10'> Paragraph');
  lAlert:=GetBlock(0) as TMarkdownAlertBlock;
  AssertEquals('Marker paragraph removed',1,lAlert.ChildCount);
  AssertEquals('Remaining paragraph','Paragraph',PlainOf(lAlert));
end;


procedure TTestAlerts.TestExtraTextStaysQuote;

begin
  SetupExt('> [!NOTE] extra'#10'> Text');
  AssertEquals('Ordinary quote',TMarkdownQuoteBlock,GetBlock(0).ClassType);
end;


procedure TTestAlerts.TestUnknownTypeStaysQuote;

begin
  SetupExt('> [!FOO]'#10'> Text');
  AssertEquals('Ordinary quote',TMarkdownQuoteBlock,GetBlock(0).ClassType);
end;


procedure TTestAlerts.TestAlertWithCodeAndDefinitionList;

var
  lAlert : TMarkdownAlertBlock;
begin
  SetupExt('> [!CAUTION]'#10'> Careful:'#10'>'#10'> ```pascal'#10'> writeln;'#10'> ```'#10'>'#10'> Term'#10'> : Definition');
  lAlert:=GetBlock(0) as TMarkdownAlertBlock;
  AssertEquals('Alert type',Ord(atCaution),Ord(lAlert.AlertType));
  AssertEquals('Paragraph, code, definition list',3,lAlert.ChildCount);
  AssertEquals('Code block',TMarkdownCodeBlock,lAlert.Children[1].ClassType);
  AssertEquals('Code line','writeln;',TMarkdownTextBlock(lAlert.Children[1].Children[0]).Text);
  AssertEquals('Definition list',TMarkdownDefinitionListBlock,lAlert.Children[2].ClassType);
  AssertEquals('Definition','Definition',PlainOf(lAlert.Children[2].Children[1]));
end;


procedure TTestAlerts.TestAlertAfterList;

begin
  SetupExt('- item'#10#10'> [!IMPORTANT]'#10'> Text');
  AssertEquals('List and alert',2,Doc.Blocks.Count);
  AssertEquals('Alert block',TMarkdownAlertBlock,GetBlock(1).ClassType);
  AssertEquals('Alert text','Text',PlainOf(GetBlock(1)));
end;


procedure TTestAlerts.TestAlertsOptionOff;

begin
  SetupExt('> [!NOTE]'#10'> Text',[]);
  AssertEquals('Ordinary quote',TMarkdownQuoteBlock,GetBlock(0).ClassType);
end;

{ TTestFootnotes }

procedure TTestFootnotes.TestDefinition;

var
  lNote : TMarkdownFootnoteBlock;
begin
  SetupExt('Text[^a]'#10#10'[^a]: The note.');
  lNote:=GetBlock(1) as TMarkdownFootnoteBlock;
  AssertEquals('Label','a',lNote.FootnoteLabel);
  AssertSame('Registered',lNote,Doc.FindFootnoteDef('a'));
  AssertEquals('Note text','The note.',Trim(PlainOf(lNote)));
  AssertEquals('Number',1,lNote.Number);
end;


procedure TTestFootnotes.TestNumberingByFirstReference;

var
  lNodes : TMarkdownTextNodeList;
begin
  SetupExt('x[^b] y[^a]'#10#10'[^a]: A'#10#10'[^b]: B');
  AssertEquals('b is first',1,Doc.FindFootnoteDef('b').Number);
  AssertEquals('a is second',2,Doc.FindFootnoteDef('a').Number);
  AssertEquals('Two footnotes',2,Doc.Footnotes.Count);
  AssertSame('Footnotes in number order',Doc.FindFootnoteDef('b'),Doc.Footnotes[0]);
  lNodes:=TextBlockOf(GetBlock(0)).Nodes;
  AssertEquals('Reference node',Ord(nkFootnoteRef),Ord(lNodes[1].Kind));
  AssertEquals('Reference number','1',lNodes[1].Attrs['number']);
end;


procedure TTestFootnotes.TestRepeatedReference;

var
  lNodes : TMarkdownTextNodeList;
begin
  SetupExt('x[^a] y[^a]'#10#10'[^a]: A');
  lNodes:=TextBlockOf(GetBlock(0)).Nodes;
  AssertEquals('First reference index','1',lNodes[1].Attrs['refindex']);
  AssertEquals('Second reference index','2',lNodes[3].Attrs['refindex']);
  AssertEquals('Same number','1',lNodes[3].Attrs['number']);
end;


procedure TTestFootnotes.TestContinuationLines;

var
  lNote : TMarkdownFootnoteBlock;
begin
  SetupExt('x[^a]'#10#10'[^a]: First line'#10'    second line'#10#10'    Second paragraph'#10#10'After');
  lNote:=GetBlock(1) as TMarkdownFootnoteBlock;
  AssertEquals('Two paragraphs',2,lNote.ChildCount);
  AssertEquals('Second paragraph','Second paragraph',PlainOf(lNote.Children[1]));
  AssertEquals('Paragraph after note','After',PlainOf(GetBlock(2)));
end;


procedure TTestFootnotes.TestUndefinedReference;

var
  lNodes : TMarkdownTextNodeList;
begin
  SetupExt('Ref to [^missing] here.');
  AssertEquals('Undefined reference reported',1,Messages.Count);
  AssertTrue('Message is an error',Messages[0].StartsWith(IntToStr(Ord(mlError))+':'));
  lNodes:=TextBlockOf(GetBlock(0)).Nodes;
  AssertEquals('Rendered as literal text','Ref to [^missing] here.',lNodes.PlainText);
end;


procedure TTestFootnotes.TestUnusedDefinition;

begin
  SetupExt('[^unused]: Nobody refers to me.');
  AssertEquals('Unused definition reported',1,Messages.Count);
  AssertTrue('Message is a warning',Messages[0].StartsWith(IntToStr(Ord(mlWarning))+':1:'));
  AssertEquals('Not numbered',0,(GetBlock(0) as TMarkdownFootnoteBlock).Number);
  AssertEquals('Not in footnote list',0,Doc.Footnotes.Count);
end;


procedure TTestFootnotes.TestDuplicateLabel;

begin
  SetupExt('x[^a]'#10#10'[^a]: One'#10#10'[^a]: Two');
  AssertTrue('Duplicate reported',Messages.Count>=1);
  AssertTrue('Duplicate is an error on line 5',Messages[0].StartsWith(IntToStr(Ord(mlError))+':5:'));
end;


procedure TTestFootnotes.TestDefinitionInsideQuote;

var
  lQuote : TMarkdownBlock;
begin
  SetupExt('> Quote with note[^q].'#10'>'#10'> [^q]: Footnote in quote.');
  lQuote:=GetBlock(0);
  AssertEquals('Quote',TMarkdownQuoteBlock,lQuote.ClassType);
  AssertEquals('Paragraph and footnote',2,lQuote.ChildCount);
  AssertEquals('Footnote in quote',TMarkdownFootnoteBlock,lQuote.Children[1].ClassType);
  AssertEquals('Footnote numbered',1,TMarkdownFootnoteBlock(lQuote.Children[1]).Number);
  AssertEquals('No messages',0,Messages.Count);
end;


procedure TTestFootnotes.TestFootnotesOptionOff;

begin
  SetupExt('x[^a]'#10#10'[^a]: A',[]);
  AssertEquals('Two paragraphs',2,Doc.Blocks.Count);
  AssertEquals('Definition is a paragraph',TMarkdownParagraphBlock,GetBlock(1).ClassType);
end;

{ TTestCaptions }

const
  cTable = '| a | b |'#10'|---|---|'#10'| 1 | 2 |';

procedure TTestCaptions.TestTableCaptionAfter;

var
  lTable : TMarkdownTableBlock;
begin
  SetupExt(cTable+#10#10'Table: The *caption*');
  AssertEquals('Caption merged',1,Doc.Blocks.Count);
  lTable:=GetBlock(0) as TMarkdownTableBlock;
  AssertNotNull('Caption',lTable.Caption);
  AssertEquals('Caption text','The caption',lTable.Caption.PlainText);
end;


procedure TTestCaptions.TestTableCaptionBefore;

var
  lTable : TMarkdownTableBlock;
begin
  SetupExt('Table: Before'#10#10+cTable);
  AssertEquals('Caption merged',1,Doc.Blocks.Count);
  lTable:=GetBlock(0) as TMarkdownTableBlock;
  AssertEquals('Caption text','Before',lTable.Caption.PlainText);
end;


procedure TTestCaptions.TestTableCaptionID;

var
  lTable : TMarkdownTableBlock;
begin
  SetupExt(cTable+#10#10'Table: Caption {#tab-x .wide}');
  lTable:=GetBlock(0) as TMarkdownTableBlock;
  AssertEquals('Table id','tab-x',lTable.ID);
  AssertEquals('Table class','wide',lTable.Classes[0]);
  AssertEquals('Caption without attributes','Caption',lTable.Caption.PlainText);
  AssertSame('Table anchor',lTable,Doc.FindAnchor('tab-x'));
end;


procedure TTestCaptions.TestCaptionNotNextToTable;

begin
  SetupExt('Table: alone'#10#10'Text');
  AssertEquals('Two paragraphs',2,Doc.Blocks.Count);
  AssertEquals('Caption stays a paragraph','Table: alone',PlainOf(GetBlock(0)));
end;


procedure TTestCaptions.TestFigure;

var
  lFigure : TMarkdownFigureBlock;
begin
  SetupExt('![A *nice* picture](pic.png)');
  AssertEquals('Figure',TMarkdownFigureBlock,GetBlock(0).ClassType);
  lFigure:=TMarkdownFigureBlock(GetBlock(0));
  AssertNotNull('Image node',lFigure.Image);
  AssertEquals('Image source','pic.png',lFigure.Image.Attrs['src']);
  AssertEquals('Caption from alt text','A nice picture',lFigure.Caption.PlainText);
  AssertTrue('Caption keeps emphasis',nsEmph in lFigure.Caption[1].Styles);
end;


procedure TTestCaptions.TestFigureAttributes;

var
  lFigure : TMarkdownFigureBlock;
begin
  SetupExt('![Caption](file.png){#fig-x width=80%}');
  lFigure:=GetBlock(0) as TMarkdownFigureBlock;
  AssertEquals('Figure id','fig-x',lFigure.ID);
  AssertEquals('Figure width','80%',lFigure.Attrs.Values['width']);
  AssertEquals('Attribute text removed','',TextBlockOf(lFigure).Nodes.PlainText.Replace('Caption',''));
  AssertSame('Figure anchor',lFigure,Doc.FindAnchor('fig-x'));
end;


procedure TTestCaptions.TestImageWithTextIsNoFigure;

begin
  SetupExt('See ![x](a.png) here');
  AssertEquals('Paragraph',TMarkdownParagraphBlock,GetBlock(0).ClassType);
end;


procedure TTestCaptions.TestCaptionsOptionOff;

begin
  SetupExt(cTable+#10#10'Table: caption'#10#10'![x](a.png)',[]);
  AssertEquals('Table, caption paragraph, image paragraph',3,Doc.Blocks.Count);
  AssertEquals('Image stays in a paragraph',TMarkdownParagraphBlock,GetBlock(2).ClassType);
end;

{ TTestLinkReferences }

function TTestLinkReferences.FirstNode: TMarkdownTextNode;

begin
  Result:=TextBlockOf(GetBlock(0)).Nodes[0];
end;


procedure TTestLinkReferences.TestDefinition;

var
  lRef : TMarkdownLinkReference;
begin
  SetupExt('[foo]: /url "The title"');
  AssertEquals('No blocks',0,Doc.Blocks.Count);
  lRef:=Doc.FindLinkRef('foo');
  AssertNotNull('Definition registered',lRef);
  AssertEquals('URL','/url',lRef.URL);
  AssertEquals('Title','The title',lRef.Title);
end;


procedure TTestLinkReferences.TestFullReference;

var
  lNode : TMarkdownTextNode;
begin
  SetupExt('[some text][foo]'#10#10'[foo]: /url "T"');
  lNode:=FirstNode;
  AssertEquals('Link node',Ord(nkURI),Ord(lNode.Kind));
  AssertEquals('Link target','/url',lNode.Attrs['href']);
  AssertEquals('Link title','T',lNode.Attrs['title']);
  AssertEquals('Link text','some text',lNode.NodeText+lNode.Children.PlainText);
end;


procedure TTestLinkReferences.TestCollapsedReference;

var
  lNode : TMarkdownTextNode;
begin
  SetupExt('[foo][] after'#10#10'[foo]: /url');
  lNode:=FirstNode;
  AssertEquals('Link node',Ord(nkURI),Ord(lNode.Kind));
  AssertEquals('Link text','foo',lNode.NodeText+lNode.Children.PlainText);
  AssertEquals('Brackets consumed','foo after',TextBlockOf(GetBlock(0)).Nodes.PlainText);
end;


procedure TTestLinkReferences.TestShortcutReference;

var
  lNode : TMarkdownTextNode;
begin
  SetupExt('[foo] after'#10#10'[foo]: /url');
  lNode:=FirstNode;
  AssertEquals('Link node',Ord(nkURI),Ord(lNode.Kind));
  AssertEquals('Link target','/url',lNode.Attrs['href']);
end;


procedure TTestLinkReferences.TestCaseInsensitiveLabel;

begin
  SetupExt('[FOO   Bar]'#10#10'[foo bar]: /url');
  AssertEquals('Link node',Ord(nkURI),Ord(FirstNode.Kind));
end;


procedure TTestLinkReferences.TestUndefinedLabelStaysText;

begin
  SetupExt('[nothing] here');
  AssertEquals('Literal text','[nothing] here',TextBlockOf(GetBlock(0)).Nodes.PlainText);
end;


procedure TTestLinkReferences.TestDefinitionDoesNotInterruptParagraph;

begin
  SetupExt('para'#10'[foo]: /url');
  AssertEquals('One paragraph',1,Doc.Blocks.Count);
  AssertNull('Not a definition',Doc.FindLinkRef('foo'));
end;


procedure TTestLinkReferences.TestLinkReferencesOptionOff;

begin
  SetupExt('[foo]'#10#10'[foo]: /url',[]);
  AssertEquals('Two paragraphs',2,Doc.Blocks.Count);
  AssertNull('No definition',Doc.FindLinkRef('foo'));
end;

{ TTestTransformRegistry }

procedure TTestTransformRegistry.TestDefaultTransforms;

var
  lAll : TMarkdownTransformClassArray;
  lNames : Array of String;
  I : Integer;
begin
  lNames:=['alerts','captions','footnotes','headingids','markers'];
  lAll:=TMarkdownTransformFactory.Instance.All;
  AssertTrue('At least the default transforms',Length(lAll)>=Length(lNames));
  for I:=0 to Length(lNames)-1 do
    begin
    AssertNotNull('Registered: '+lNames[I],TMarkdownTransformFactory.Instance.FindTransform(lNames[I]));
    AssertTrue('Order: '+lNames[I],lAll[I]=TMarkdownTransformFactory.Instance.FindTransform(lNames[I]));
    end;
end;

initialization
  RegisterTests('Parser',[TTestParagraphs, TTestHeadings, TTestCodeBlocks,
                          TTestBlockQuotes, TTestLists, TTestThematicBreaks,
                          TTestTables, TTestFrontmatter, TTestInlineProcessorHook]);
  RegisterTests('Extensions',[TTestComments, TTestAttributes, TTestHeadingIDs, TTestDefinitionLists,
                              TTestAlerts, TTestFootnotes, TTestCaptions, TTestLinkReferences,
                              TTestTransformRegistry]);
end.

