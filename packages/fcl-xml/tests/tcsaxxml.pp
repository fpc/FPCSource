{
    This file is part of the Free Component Library

    Tests for "!" and ">" inside tags (issue #41159) and comments in the SAX XML reader.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsaxxml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM, SAX, sax_xml;

type

  { TTestSAXXMLTagChars }

  TTestSAXXMLTagChars = class(TTestCase)
  private
    FDoc: TXMLDocument;
    FEvents: String;
    procedure Parse(const aXML: String);
    procedure ParseEvents(const aXML: String);
    procedure DoCharacters(Sender: TObject; const ch: PSAXChar; aStart, aLength: Integer);
    procedure DoComment(Sender: TObject; const ch: PSAXChar; aStart, aLength: Integer);
    function RootChild(aIndex: Integer): TDOMNode;
    function Attr(aIndex: Integer; const aName: String): String;
  protected
    procedure TearDown; override;
  published
    procedure TestIssueExample;
    procedure TestExclamationInAttribute;
    procedure TestGreaterThanInAttribute;
    procedure TestGreaterThanInSingleQuotedAttribute;
    procedure TestCommentAndCData;
    procedure TestExclamationInText;
    procedure TestExclamationAfterBufferBoundary;
    procedure TestDocumentStartsWithComment;
    procedure TestCommentAfterRoot;
    procedure TestNestedComment;
    procedure TestNoEmptyTextEvents;
  end;

implementation

procedure TTestSAXXMLTagChars.TearDown;

begin
  FreeAndNil(FDoc);
end;


procedure TTestSAXXMLTagChars.Parse(const aXML: String);

var
  lStream: TStringStream;

begin
  FreeAndNil(FDoc);
  lStream:=TStringStream.Create(aXML);
  try
    ReadXMLFile(FDoc,lStream);
  finally
    lStream.Free;
  end;
  AssertNotNull('Document element',FDoc.DocumentElement);
end;


procedure TTestSAXXMLTagChars.ParseEvents(const aXML: String);

var
  lReader: TSAXXMLReader;
  lStream: TStringStream;

begin
  FEvents:='';
  lReader:=TSAXXMLReader.Create;
  lStream:=TStringStream.Create(aXML);
  try
    lReader.OnCharacters:=@DoCharacters;
    lReader.OnComment:=@DoComment;
    lReader.ParseStream(lStream);
  finally
    lStream.Free;
    lReader.Free;
  end;
end;


procedure TTestSAXXMLTagChars.DoCharacters(Sender: TObject; const ch: PSAXChar; aStart, aLength: Integer);

var
  lText: SAXString;

begin
  SetString(lText,ch+aStart,aLength);
  FEvents:=FEvents+'[text:'+String(lText)+']';
end;


procedure TTestSAXXMLTagChars.DoComment(Sender: TObject; const ch: PSAXChar; aStart, aLength: Integer);

var
  lText: SAXString;

begin
  SetString(lText,ch+aStart,aLength);
  FEvents:=FEvents+'[comment:'+String(lText)+']';
end;


function TTestSAXXMLTagChars.RootChild(aIndex: Integer): TDOMNode;

begin
  AssertTrue('Root has child '+IntToStr(aIndex),FDoc.DocumentElement.ChildNodes.Count>aIndex);
  Result:=FDoc.DocumentElement.ChildNodes[aIndex];
end;


function TTestSAXXMLTagChars.Attr(aIndex: Integer; const aName: String): String;

var
  lNode: TDOMNode;

begin
  lNode:=RootChild(aIndex);
  AssertTrue('Child '+IntToStr(aIndex)+' is an element',lNode is TDOMElement);
  AssertTrue('Child '+IntToStr(aIndex)+' has attribute '+aName,TDOMElement(lNode).HasAttribute(aName));
  Result:=String(TDOMElement(lNode).GetAttribute(aName));
end;


procedure TTestSAXXMLTagChars.TestIssueExample;

begin
  Parse('<root><tag attr="[field]" bad_attr="[!field]"/></root>');
  AssertEquals('Element with "!" in an attribute is kept','tag',String(RootChild(0).NodeName));
  AssertEquals('attr value','[field]',Attr(0,'attr'));
  AssertEquals('bad_attr value','[!field]',Attr(0,'bad_attr'));
end;


procedure TTestSAXXMLTagChars.TestExclamationInAttribute;

begin
  Parse('<root><tag a="[!x]"/></root>');
  AssertEquals('Attribute value with "!"','[!x]',Attr(0,'a'));
end;


procedure TTestSAXXMLTagChars.TestGreaterThanInAttribute;

begin
  Parse('<root><tag b="a>b"/></root>');
  AssertEquals('Attribute value with ">"','a>b',Attr(0,'b'));
end;


procedure TTestSAXXMLTagChars.TestGreaterThanInSingleQuotedAttribute;

begin
  Parse('<root><tag b=''x>y!z''/></root>');
  AssertEquals('Single-quoted attribute value with ">" and "!"','x>y!z',Attr(0,'b'));
end;


procedure TTestSAXXMLTagChars.TestCommentAndCData;

begin
  Parse('<root><tag a="[!x]" b="a>b"/><!--c--><![CDATA[d]]></root>');
  AssertEquals('a value','[!x]',Attr(0,'a'));
  AssertEquals('b value','a>b',Attr(0,'b'));
  AssertEquals('Second child is a comment',COMMENT_NODE,RootChild(1).NodeType);
  AssertEquals('Comment text','c',String(RootChild(1).NodeValue));
  AssertEquals('CDATA text','d',String(RootChild(2).NodeValue));
  AssertEquals('Root has three children',3,FDoc.DocumentElement.ChildNodes.Count);
  ParseEvents('<root><tag a="[!x]" b="a>b"/><!--c--><![CDATA[d]]></root>');
  AssertEquals('Comment and CDATA events after a tag with "!" and ">"','[comment:c][text:d]',FEvents);
end;


procedure TTestSAXXMLTagChars.TestExclamationInText;

begin
  Parse('<root>hi!<tag/></root>');
  AssertEquals('Text with "!"','hi!',String(RootChild(0).NodeValue));
  AssertEquals('Element after the text','tag',String(RootChild(1).NodeName));
end;


procedure TTestSAXXMLTagChars.TestExclamationAfterBufferBoundary;

var
  lValue: String;
  i: Integer;

begin
  for i:=1000 to 1030 do
    begin
    lValue:=StringOfChar('x',i)+'!y';
    Parse('<root><tag a="'+lValue+'"/></root>');
    AssertEquals('Attribute value with "!" at offset '+IntToStr(i),lValue,Attr(0,'a'));
    end;
end;


procedure TTestSAXXMLTagChars.TestDocumentStartsWithComment;

begin
  ParseEvents('<!--first--><root><tag a="1"/></root>');
  AssertEquals('Comment at the start of the document','[comment:first]',FEvents);
  Parse('<!--first--><root><tag a="1"/></root>');
  AssertEquals('Attribute after a leading comment','1',Attr(0,'a'));
  AssertEquals('Document starts with a comment node',COMMENT_NODE,FDoc.FirstChild.NodeType);
  AssertEquals('Comment before the root','first',String(FDoc.FirstChild.NodeValue));
  AssertTrue('Root follows the comment',FDoc.FirstChild.NextSibling=FDoc.DocumentElement);
end;


procedure TTestSAXXMLTagChars.TestCommentAfterRoot;

begin
  Parse('<root/><!--last-->');
  AssertTrue('Comment follows the root',Assigned(FDoc.DocumentElement.NextSibling));
  AssertEquals('Comment after the root is a comment node',COMMENT_NODE,FDoc.DocumentElement.NextSibling.NodeType);
  AssertEquals('Comment after the root','last',String(FDoc.DocumentElement.NextSibling.NodeValue));
end;


procedure TTestSAXXMLTagChars.TestNestedComment;

var
  lInner: TDOMNode;

begin
  Parse('<root><a>x<!--in-->y</a><!--out--></root>');
  lInner:=RootChild(0);
  AssertEquals('Element a has three children',3,lInner.ChildNodes.Count);
  AssertEquals('Comment inside a',COMMENT_NODE,lInner.ChildNodes[1].NodeType);
  AssertEquals('Comment text inside a','in',String(lInner.ChildNodes[1].NodeValue));
  AssertEquals('Comment after a',COMMENT_NODE,RootChild(1).NodeType);
  AssertEquals('Comment text after a','out',String(RootChild(1).NodeValue));
end;


procedure TTestSAXXMLTagChars.TestNoEmptyTextEvents;

begin
  ParseEvents('<!--a--><root><!--b--><![CDATA[]]><x/><!--c--></root>');
  AssertEquals('No empty characters events','[comment:a][comment:b][comment:c]',FEvents);
  Parse('<root><!--b--><x/></root>');
  AssertEquals('No empty text node before a comment',2,FDoc.DocumentElement.ChildNodes.Count);
end;


initialization
  RegisterTest(TTestSAXXMLTagChars);
end.
