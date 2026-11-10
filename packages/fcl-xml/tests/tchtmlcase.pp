{
    This file is part of the Free Component Library

    Tests for case-insensitive names (issue #16973) and element classes in HTML documents.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tchtmlcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM, dom_html, sax_html;

type

  { TTestHTMLCase }

  TTestHTMLCase = class(TTestCase)
  private
    FDoc: THTMLDocument;
    procedure Parse(const aBody: String);
    function Image: TDOMElement;
  protected
    procedure TearDown; override;
  published
    procedure TestGetNamedItem;
    procedure TestGetAttribute;
    procedure TestHasAttribute;
    procedure TestGetAttributeNode;
    procedure TestSetAttributeReplaces;
    procedure TestSetNewAttribute;
    procedure TestRemoveAttribute;
    procedure TestRemoveNamedItem;
    procedure TestCreateElement;
    procedure TestCreateAttribute;
    procedure TestXMLStaysCaseSensitive;
    procedure TestParsedElementClasses;
    procedure TestParsedImageProperties;
    procedure TestParsedBody;
    procedure TestFragmentElementClass;
  end;

implementation

procedure TTestHTMLCase.TearDown;

begin
  FreeAndNil(FDoc);
end;


procedure TTestHTMLCase.Parse(const aBody: String);

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


function TTestHTMLCase.Image: TDOMElement;

var
  lList: TDOMNodeList;

begin
  Parse('<IMG SRC="a.png">');
  lList:=FDoc.GetElementsByTagName('img');
  AssertEquals('One img element',1,lList.Count);
  Result:=lList[0] as TDOMElement;
end;


procedure TTestHTMLCase.TestGetNamedItem;

var
  lImg: TDOMElement;
  lNode: TDOMNode;

begin
  lImg:=Image;
  lNode:=lImg.Attributes.GetNamedItem('SRC');
  AssertNotNull('GetNamedItem(''SRC'') finds the attribute',lNode);
  AssertEquals('Value of SRC','a.png',String(lNode.NodeValue));
  AssertNotNull('GetNamedItem(''src'') finds the attribute',lImg.Attributes.GetNamedItem('src'));
  AssertNull('GetNamedItem of an unknown attribute',lImg.Attributes.GetNamedItem('ALT'));
end;


procedure TTestHTMLCase.TestGetAttribute;

var
  lImg: TDOMElement;

begin
  lImg:=Image;
  AssertEquals('GetAttribute(''Src'')','a.png',String(lImg.GetAttribute('Src')));
  AssertEquals('GetAttribute(''SRC'')','a.png',String(lImg.GetAttribute('SRC')));
  AssertEquals('AttribStrings[''sRC'']','a.png',String(lImg['sRC']));
end;


procedure TTestHTMLCase.TestHasAttribute;

begin
  AssertTrue('hasAttribute(''SRC'')',Image.hasAttribute('SRC'));
  AssertFalse('hasAttribute(''ALT'')',Image.hasAttribute('ALT'));
end;


procedure TTestHTMLCase.TestGetAttributeNode;

var
  lAttr: TDOMAttr;

begin
  lAttr:=Image.GetAttributeNode('sRc');
  AssertNotNull('GetAttributeNode(''sRc'')',lAttr);
  AssertEquals('Attribute name is stored in lowercase','src',String(lAttr.Name));
end;


procedure TTestHTMLCase.TestSetAttributeReplaces;

var
  lImg: TDOMElement;

begin
  lImg:=Image;
  lImg.SetAttribute('SRC','b.png');
  AssertEquals('SetAttribute(''SRC'') replaces src, no second attribute',1,Integer(lImg.Attributes.Length));
  AssertEquals('New value of src','b.png',String(lImg.GetAttribute('src')));
end;


procedure TTestHTMLCase.TestSetNewAttribute;

var
  lImg: TDOMElement;

begin
  lImg:=Image;
  lImg.SetAttribute('ALT','x');
  AssertEquals('Two attributes after SetAttribute(''ALT'')',2,Integer(lImg.Attributes.Length));
  AssertNotNull('New attribute found as alt',lImg.Attributes.GetNamedItem('alt'));
  AssertEquals('New attribute name is stored in lowercase','alt',String(lImg.GetAttributeNode('ALT').Name));
  lImg.SetAttribute('Alt','y');
  AssertEquals('SetAttribute(''Alt'') replaces alt',2,Integer(lImg.Attributes.Length));
  AssertEquals('Value of alt','y',String(lImg.GetAttribute('ALT')));
  AssertEquals('src is still found','a.png',String(lImg.GetAttribute('SRC')));
end;


procedure TTestHTMLCase.TestRemoveAttribute;

var
  lImg: TDOMElement;

begin
  lImg:=Image;
  lImg.RemoveAttribute('SRC');
  AssertFalse('RemoveAttribute(''SRC'') removes src',lImg.hasAttribute('src'));
  AssertEquals('No attributes left',0,Integer(lImg.Attributes.Length));
end;


procedure TTestHTMLCase.TestRemoveNamedItem;

var
  lImg: TDOMElement;

begin
  lImg:=Image;
  lImg.Attributes.RemoveNamedItem('Src').Free;
  AssertEquals('RemoveNamedItem(''Src'') removes src',0,Integer(lImg.Attributes.Length));
end;


procedure TTestHTMLCase.TestCreateElement;

var
  lDiv: THTMLElement;

begin
  FDoc:=THTMLDocument.Create;
  lDiv:=FDoc.CreateElement('DIV');
  FDoc.AppendChild(lDiv);
  AssertEquals('CreateElement(''DIV'').TagName','div',String(lDiv.TagName));
  AssertTrue('CreateElement(''DIV'') uses the div class',lDiv is THTMLDivElement);
end;


procedure TTestHTMLCase.TestCreateAttribute;

var
  lImg: TDOMElement;
  lAttr: TDOMAttr;

begin
  lImg:=Image;
  lAttr:=FDoc.CreateAttribute('TITLE');
  AssertEquals('CreateAttribute(''TITLE'').Name','title',String(lAttr.Name));
  lAttr.Value:='t';
  lImg.SetAttributeNode(lAttr);
  AssertEquals('Attribute set with SetAttributeNode found as Title','t',String(lImg.GetAttribute('Title')));
  AssertEquals('src is still found','a.png',String(lImg.GetAttribute('SRC')));
end;


procedure TTestHTMLCase.TestXMLStaysCaseSensitive;

var
  lDoc: TXMLDocument;
  lEl: TDOMElement;

begin
  lDoc:=TXMLDocument.Create;
  try
    lEl:=lDoc.CreateElement('Item');
    lDoc.AppendChild(lEl);
    lEl.SetAttribute('Name','x');
    AssertEquals('XML element name keeps its case','Item',String(lEl.TagName));
    AssertEquals('XML attribute found with the same case','x',String(lEl.GetAttribute('Name')));
    AssertEquals('XML attribute not found with another case','',String(lEl.GetAttribute('name')));
    AssertNull('XML GetNamedItem with another case',lEl.Attributes.GetNamedItem('NAME'));
    lEl.SetAttribute('name','y');
    AssertEquals('XML attributes differing in case are separate',2,Integer(lEl.Attributes.Length));
  finally
    lDoc.Free;
  end;
end;


procedure TTestHTMLCase.TestParsedElementClasses;

var
  lDiv, lAnchor, lUnknown: TDOMNode;

begin
  Parse('<DIV><A HREF="x.html">x</A><custom-tag>y</custom-tag></DIV>');
  AssertTrue('html element class',FDoc.DocumentElement is THTMLHtmlElement);
  lDiv:=FDoc.GetElementsByTagName('div')[0];
  AssertTrue('div element class',lDiv is THTMLDivElement);
  AssertEquals('div tag name','div',String(lDiv.NodeName));
  lAnchor:=FDoc.GetElementsByTagName('a')[0];
  AssertTrue('a element class',lAnchor is THTMLAnchorElement);
  lUnknown:=FDoc.GetElementsByTagName('custom-tag')[0];
  AssertNotNull('unknown tag is parsed',lUnknown);
  AssertTrue('unknown tag is a THTMLElement',lUnknown is THTMLElement);
end;


procedure TTestHTMLCase.TestParsedImageProperties;

var
  lImg: TDOMElement;

begin
  Parse('<IMG SRC="a.png" ALT="pic">');
  lImg:=FDoc.GetElementsByTagName('img')[0] as TDOMElement;
  AssertTrue('img element class',lImg is THTMLImageElement);
  AssertEquals('Src property','a.png',String(THTMLImageElement(lImg).Src));
  AssertEquals('Alt property','pic',String(THTMLImageElement(lImg).Alt));
end;


procedure TTestHTMLCase.TestParsedBody;

begin
  Parse('<p>text</p>');
  AssertTrue('Body property returns the parsed body element',FDoc.Body is THTMLBodyElement);
  AssertTrue('p element class',FDoc.GetElementsByTagName('p')[0] is THTMLParagraphElement);
end;


procedure TTestHTMLCase.TestFragmentElementClass;

var
  lStream: TStringStream;
  lDiv: THTMLElement;

begin
  FDoc:=THTMLDocument.Create;
  FDoc.AppendChild(FDoc.CreateElement('html'));
  lDiv:=FDoc.CreateElement('div');
  FDoc.DocumentElement.AppendChild(lDiv);
  lStream:=TStringStream.Create('<TABLE><TR><TD>1</TD></TR></TABLE>');
  try
    ReadHTMLFragment(lDiv,lStream);
  finally
    lStream.Free;
  end;
  AssertTrue('table in a fragment',FDoc.GetElementsByTagName('table')[0] is THTMLTableElement);
  AssertTrue('td in a fragment',FDoc.GetElementsByTagName('td')[0] is THTMLTableCellElement);
end;


initialization
  RegisterTest(TTestHTMLCase);
end.
