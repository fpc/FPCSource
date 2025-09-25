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
  Markdown.Elements, Markdown.Parser;

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
    procedure TestFencedCodeBlockWithInfoString;
  end;

  { TTestBlockQuotes }
  TTestBlockQuotes = class(TBlockTestCase)
  published
    procedure TestSimpleQuote;
    procedure TestNestedQuote;
    procedure TestLazy;
  end;

  { TTestLists }
  TTestLists = class(TBlockTestCase)
  published
    procedure TestUnorderedList;
    procedure TestOrderedList;
    procedure TestNestedList;
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

{ TTestThematicBreaks }

procedure TTestThematicBreaks.TestAsteriskBreak;
begin
  SetupParser('***');
  AssertEquals('Document should have 1 block', 1, Doc.Blocks.Count);
  AssertTrue('Block should be a thematic break', GetBlock(0) is TMarkDownThematicBreakBlock);
end;

procedure TTestThematicBreaks.TestUnderscoreBreak;
begin
  SetupParser('---');
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


initialization
  RegisterTests('Parser',[TTestParagraphs, TTestHeadings, TTestCodeBlocks,
                          TTestBlockQuotes, TTestLists, TTestThematicBreaks,
                          TTestTables]);
end.

