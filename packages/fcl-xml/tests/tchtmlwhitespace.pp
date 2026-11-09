{
    This file is part of the Free Component Library

    Tests for the line break at the start of pre and textarea in the HTML reader and writer (issue #32512).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tchtmlwhitespace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM, dom_html, sax_html, htmwrite;

type

  { TTestHTMLLeadingLineBreak }

  TTestHTMLLeadingLineBreak = class(TTestCase)
  private
    FDoc: THTMLDocument;
    procedure Parse(const aBody: String);
    function Element(const aTag: String; aIndex: Integer = 0): TDOMElement;
    function FirstText(const aTag: String; aIndex: Integer = 0): String;
    function Written: String;
    procedure ReadWritten;
    procedure AssertText(const aMsg, aExpected, aActual: String);
  protected
    procedure TearDown; override;
  published
    procedure TestReporterExample;
    procedure TestParagraphKeepsLineBreak;
    procedure TestPreCRLF;
    procedure TestPreCR;
    procedure TestOnlyOneLineBreakRemoved;
    procedure TestPreOnlyLineBreak;
    procedure TestPreStartsWithText;
    procedure TestPreStartsWithEntity;
    procedure TestPreStartsWithElement;
    procedure TestTextareaLF;
    procedure TestTextareaCRLF;
    procedure TestTwoPreElements;
    procedure TestWritePreLeadingLF;
    procedure TestWritePreLeadingCR;
    procedure TestWritePreNoLeadingLF;
    procedure TestWriteTextareaLeadingLF;
    procedure TestWriteParagraphLeadingLF;
    procedure TestWritePreStartsWithElement;
    procedure TestRoundTripPre;
    procedure TestRoundTripTextarea;
  end;

implementation


// s with every CR LF and CR replaced by LF
function Normalized(const s: String): String;

begin
  Result:=StringReplace(StringReplace(s,#13#10,#10,[rfReplaceAll]),#13,#10,[rfReplaceAll]);
end;


// s with CR and LF written as \r and \n
function Visible(const s: String): String;

begin
  Result:=StringReplace(StringReplace(s,#13,'\r',[rfReplaceAll]),#10,'\n',[rfReplaceAll]);
end;


procedure TTestHTMLLeadingLineBreak.Parse(const aBody: String);

var
  lStream: TStringStream;

begin
  FreeAndNil(FDoc);
  lStream:=TStringStream.Create('<html><body>'+aBody+'</body></html>');
  try
    ReadHTMLFile(FDoc,lStream);
  finally
    lStream.Free;
  end;
end;


function TTestHTMLLeadingLineBreak.Element(const aTag: String; aIndex: Integer): TDOMElement;

var
  lList: TDOMNodeList;

begin
  lList:=FDoc.GetElementsByTagName(aTag);
  AssertTrue('Element '+aTag+' present',aIndex<lList.Count);
  Result:=lList[aIndex] as TDOMElement;
end;


function TTestHTMLLeadingLineBreak.FirstText(const aTag: String; aIndex: Integer): String;

var
  lNode: TDOMNode;

begin
  lNode:=Element(aTag,aIndex).FirstChild;
  if (lNode<>nil) and (lNode.NodeType=TEXT_NODE) then
    Result:=UTF8Encode(lNode.NodeValue)
  else
    Result:='';
end;


function TTestHTMLLeadingLineBreak.Written: String;

var
  lStream: TStringStream;

begin
  lStream:=TStringStream.Create('');
  try
    WriteHTMLFile(FDoc,lStream);
    Result:=lStream.DataString;
  finally
    lStream.Free;
  end;
end;


procedure TTestHTMLLeadingLineBreak.ReadWritten;

var
  lStream: TStringStream;

begin
  lStream:=TStringStream.Create(Written);
  try
    FreeAndNil(FDoc);
    ReadHTMLFile(FDoc,lStream);
  finally
    lStream.Free;
  end;
end;


procedure TTestHTMLLeadingLineBreak.AssertText(const aMsg, aExpected, aActual: String);

begin
  AssertEquals(aMsg,Visible(aExpected),Visible(aActual));
end;


procedure TTestHTMLLeadingLineBreak.TearDown;

begin
  FreeAndNil(FDoc);
  inherited TearDown;
end;


procedure TTestHTMLLeadingLineBreak.TestReporterExample;

begin
  Parse('<p style="white-space:pre">'#10'    Lorem'#10'  </p>'+
        '<pre>'#10'    Lorem'#10'  </pre>'+
        '<textarea>'#10'abc</textarea>');
  AssertText('p keeps the leading line break',#10'    Lorem'#10'  ',FirstText('p'));
  AssertText('pre drops the leading line break','    Lorem'#10'  ',FirstText('pre'));
  AssertText('textarea drops the leading line break','abc',FirstText('textarea'));
end;


procedure TTestHTMLLeadingLineBreak.TestParagraphKeepsLineBreak;

begin
  Parse('<div>'#10'text</div>');
  AssertText('div keeps the leading line break',#10'text',FirstText('div'));
end;


procedure TTestHTMLLeadingLineBreak.TestPreCRLF;

begin
  Parse('<pre>'#13#10'line</pre>');
  AssertText('CR LF after pre is removed as one line break','line',FirstText('pre'));
end;


procedure TTestHTMLLeadingLineBreak.TestPreCR;

begin
  Parse('<pre>'#13'line</pre>');
  AssertText('A lone CR after pre is removed','line',FirstText('pre'));
end;


procedure TTestHTMLLeadingLineBreak.TestOnlyOneLineBreakRemoved;

begin
  Parse('<pre>'#10#10'line</pre>');
  AssertText('Only the first line break is removed',#10'line',FirstText('pre'));
end;


procedure TTestHTMLLeadingLineBreak.TestPreOnlyLineBreak;

begin
  Parse('<pre>'#10'</pre><p>after</p>');
  AssertEquals('pre with only a line break has no children',0,Element('pre').ChildNodes.Count);
  AssertText('Next element is read normally','after',FirstText('p'));
end;


procedure TTestHTMLLeadingLineBreak.TestPreStartsWithText;

begin
  Parse('<pre>line'#10'next</pre>');
  AssertText('Text not starting with a line break is unchanged','line'#10'next',FirstText('pre'));
end;


procedure TTestHTMLLeadingLineBreak.TestPreStartsWithEntity;

begin
  Parse('<pre>&amp;'#10'x</pre>');
  AssertText('Line break after a leading entity is kept','&'#10'x',UTF8Encode(Element('pre').TextContent));
end;


procedure TTestHTMLLeadingLineBreak.TestPreStartsWithElement;

begin
  Parse('<pre><b>bold</b>'#10'x</pre>');
  AssertText('Line break after a leading child element is kept','bold'#10'x',UTF8Encode(Element('pre').TextContent));
end;


procedure TTestHTMLLeadingLineBreak.TestTextareaLF;

begin
  Parse('<textarea>'#10'first'#10'second</textarea>');
  AssertText('textarea drops only the first line break','first'#10'second',FirstText('textarea'));
end;


procedure TTestHTMLLeadingLineBreak.TestTextareaCRLF;

begin
  Parse('<textarea>'#13#10'abc</textarea>');
  AssertText('CR LF after textarea is removed','abc',FirstText('textarea'));
end;


procedure TTestHTMLLeadingLineBreak.TestTwoPreElements;

begin
  Parse('<pre>'#10'one</pre>'#10'<pre>'#10'two</pre>');
  AssertText('First pre','one',FirstText('pre',0));
  AssertText('Second pre','two',FirstText('pre',1));
end;


procedure TTestHTMLLeadingLineBreak.TestWritePreLeadingLF;

begin
  Parse('<pre>'#10#10'x</pre>');
  AssertText('pre text read back with one line break',#10'x',FirstText('pre'));
  AssertTrue('Writer adds a line break before a leading line break in pre: '+Visible(Written),
    Pos('<pre>'+LineEnding+LineEnding+'x</pre>',Written)>0);
end;


procedure TTestHTMLLeadingLineBreak.TestWritePreLeadingCR;

var
  lPre: TDOMElement;

begin
  Parse('<pre>x</pre>');
  lPre:=Element('pre');
  TDOMText(lPre.FirstChild).Data:=#13'y';
  AssertTrue('Writer adds a line break before a leading CR in pre: '+Visible(Written),
    Pos('<pre>'+LineEnding+LineEnding+'y</pre>',Written)>0);
end;


procedure TTestHTMLLeadingLineBreak.TestWritePreNoLeadingLF;

begin
  Parse('<pre>x'#10'y</pre>');
  AssertTrue('Writer adds nothing when pre text does not start with a line break: '+Visible(Written),
    Pos('<pre>x'+LineEnding+'y</pre>',Written)>0);
end;


procedure TTestHTMLLeadingLineBreak.TestWriteTextareaLeadingLF;

begin
  Parse('<textarea>'#10#10'abc</textarea>');
  AssertTrue('Writer adds a line break before a leading line break in textarea: '+Visible(Written),
    Pos('<textarea>'+LineEnding+LineEnding+'abc</textarea>',Written)>0);
end;


procedure TTestHTMLLeadingLineBreak.TestWriteParagraphLeadingLF;

begin
  Parse('<p>'#10'text</p>');
  AssertTrue('Writer adds nothing for a p: '+Visible(Written),Pos('<p>'+LineEnding+'text</p>',Written)>0);
end;


procedure TTestHTMLLeadingLineBreak.TestWritePreStartsWithElement;

begin
  Parse('<pre><b>bold</b>'#10'x</pre>');
  AssertTrue('Writer adds nothing when pre starts with an element: '+Visible(Written),
    Pos('<pre><b>bold</b>'+LineEnding+'x</pre>',Written)>0);
end;


procedure TTestHTMLLeadingLineBreak.TestRoundTripPre;

begin
  Parse('<pre>'#10#10'    code'#10'</pre>');
  ReadWritten;
  AssertText('pre text survives writing and reading',#10'    code'#10,Normalized(FirstText('pre')));
  ReadWritten;
  AssertText('pre text survives a second round trip',#10'    code'#10,Normalized(FirstText('pre')));
end;


procedure TTestHTMLLeadingLineBreak.TestRoundTripTextarea;

begin
  Parse('<textarea>'#10#10'value</textarea>');
  ReadWritten;
  AssertText('textarea text survives writing and reading',#10'value',Normalized(FirstText('textarea')));
end;


initialization
  RegisterTest(TTestHTMLLeadingLineBreak);
end.
