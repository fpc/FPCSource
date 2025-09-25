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

interface

uses
  Classes, SysUtils, fpcunit, testregistry, contnrs,
  Markdown.Elements, Markdown.Scanner, Markdown.InlineText;

type

  { TTestInlineTextProcessor }

  TTestInlineTextProcessor = class(TTestCase)
  private
    FScanner: TMarkDownTextScanner;
    FNodes: TMarkDownTextNodeList;
    FProcessor: TInlineTextProcessor;
    FEntities: TFPStringHashTable;

    procedure DumpNodes;
    procedure SetupProcessor(const AText: AnsiString; awsMode: TWhitespaceMode = wsTrim);
    function NodeAsText(AIndex: Integer): TMarkDownTextNode;
    function NodeAsNamed(AIndex: Integer; AExpectedKind: TTextNodeKind): TMarkDownTextNode;
    class procedure AssertEquals(const aMsg: string; aExpected,aActual : TTextNodeKind); overload;
    class procedure AssertEquals(const aMsg: string; aExpected,aActual : TNodeStyle); overload;
    class procedure AssertEquals(const aMsg: string; aExpected,aActual : TNodeStyles); overload;

  protected
    procedure TearDown; override;
  published
    procedure TestSimpleText;
    procedure TestBackslashEscapes;
    procedure TestCodeSpans;
    procedure TestEmphasisAndStrong;
    procedure TestEmphasisAndStrongInOne;
    procedure TestEmphasisAndStrongInOneSplit;
    procedure TestEmphasisAndStrongInOneSplit2;
    procedure TestStrikethroughGFM;
    procedure TestAutoLinks;
    procedure TestInlineLink;
    procedure TestInlineImage;
  end;

implementation

uses typinfo;

{ TTestInlineTextProcessor }

procedure TTestInlineTextProcessor.TearDown;
begin
  FProcessor.Free;
  FScanner.Free;
  FNodes.Free;
  FEntities.Free;
end;

procedure TTestInlineTextProcessor.SetupProcessor(const AText: AnsiString; awsMode: TWhitespaceMode = wsTrim);
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

class procedure TTestInlineTextProcessor.AssertEquals(const aMsg: string; aExpected, aActual: TTextNodeKind);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TTextNodeKind),Ord(aExpected)),GetEnumName(TypeInfo(TTextNodeKind),Ord(aActual)));
end;

class procedure TTestInlineTextProcessor.AssertEquals(const aMsg: string; aExpected, aActual: TNodeStyle);
begin
  AssertEquals(aMsg,GetEnumName(TypeInfo(TNodeStyle),Ord(aExpected)),GetEnumName(TypeInfo(TNodeStyle),Ord(aActual)));
end;

class procedure TTestInlineTextProcessor.AssertEquals(const aMsg: string; aExpected, aActual: TNodeStyles);
begin
  AssertEquals(aMsg,SetToString(PTypeInfo(TypeInfo(TNodeStyles)),Integer(aExpected),False),
                    SetToString(PTypeInfo(TypeInfo(TNodeStyles)),Integer(aActual),False));
end;

procedure TTestInlineTextProcessor.TestSimpleText;
begin
  SetupProcessor('This is a simple text.');
  FProcessor.Process(True);
  AssertEquals('Should have one text node', 1, FNodes.Count);
  AssertEquals('Text content mismatch', 'This is a simple text.', NodeAsText(0).NodeText);
end;

procedure TTestInlineTextProcessor.TestBackslashEscapes;
begin
  SetupProcessor('This is \*not\* emphasis.');
  FProcessor.Process(True);
  AssertEquals('Should have one text node for escaped chars', 1, FNodes.Count);
  AssertEquals('Escaped character was not handled correctly', 'This is *not* emphasis.', NodeAsText(0).NodeText);
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

initialization
  RegisterTest(TTestInlineTextProcessor);
end.


