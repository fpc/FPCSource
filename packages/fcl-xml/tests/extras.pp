{**********************************************************************

    This file is part of the Free Component Library (FCL)

    DOM Test cases which are missing from w3.org test suite
    Copyright (c) 2008 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit extras;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, DOM, xmlread, xmlwrite, domunit, testregistry;

implementation

type
  TDOMTestExtra = class(TDOMTestBase)
  published
    procedure attr_ownership01;
    procedure attr_ownership02;
    procedure attr_ownership03;
    procedure attr_ownership04;
    procedure attr_ownership05;
    procedure replacesamechild;
    procedure insertbeforefirst;
    procedure nsFixup1;
    procedure nsFixup2;
    procedure nsFixup3;
    procedure nsFixup4;
    procedure nsFixup5;
    procedure nsFixup6;
    procedure nsFixup7;
    procedure nsFixup8;
  end;

{ TDOMTestExtra }

// verify that an attribute created by Element.SetAttribute()
// has its OwnerElement assigned properly
procedure TDOMTestExtra.attr_ownership01;
var
  doc: TDOMDocument;
  el: TDOMElement;
  attr: TDOMAttr;
  attrOwner: TDOMElement;
begin
  LoadStringData(doc, '<doc/>');
  el := doc.CreateElement('element1');
  el.SetAttribute('newAttr', 'newValue');
  attr := el.GetAttributeNode('newAttr');
  AssertNotNull('attribute', attr);
  attrOwner := attr.OwnerElement;
  AssertEquals('ownerElement', el, attrOwner);
  AssertTrue('specified', attr.Specified);
end;

// verify that an attribute created by Element.SetAttributeNS()
// has its OwnerElement assigned properly
procedure TDOMTestExtra.attr_ownership02;
var
  doc: TDOMDocument;
  el: TDOMElement;
  attr: TDOMAttr;
  attrOwner: TDOMElement;
begin
  LoadStringData(doc, '<doc/>');
  el := doc.CreateElement('element1');
  el.SetAttributeNS('http://www.freepascal.org', 'fpc:newAttr', 'newValue');
  attr := el.GetAttributeNodeNS('http://www.freepascal.org', 'newAttr');
  AssertNotNull('attribute', attr);
  attrOwner := attr.OwnerElement;
  AssertEquals('ownerElement', el, attrOwner);
  AssertTrue('specified', attr.Specified);
end;

// verify that NamedNodeMap.SetNamedItem() resets OwnerElement
// of the attribute being replaced
procedure TDOMTestExtra.attr_ownership03;
var
  doc: TDOMDocument;
  el: TDOMElement;
  attr, attr2: TDOMAttr;
  retNode: TDOMNode;
begin
  LoadStringData(doc, '<doc/>');
  el := doc.CreateElement('element1');
  attr := doc.CreateAttribute('newAttr');
  el.SetAttributeNode(attr);
  AssertEquals('ownerElement_before', el, attr.OwnerElement);
  attr2 := doc.CreateAttribute('newAttr');
  retNode := el.Attributes.SetNamedItem(attr2);
  AssertSame('retNode', attr, retNode);
  AssertNull('ownerElement_after', attr.OwnerElement);
  AssertEquals('ownerElement2', el, attr2.OwnerElement);
end;

// verify that NamedNodeMap.SetNamedItemNS() resets OwnerElement
// of the attribute being replaced
procedure TDOMTestExtra.attr_ownership04;
var
  doc: TDOMDocument;
  el: TDOMElement;
  attr, attr2: TDOMAttr;
  retNode: TDOMNode;
begin
  LoadStringData(doc, '<doc/>');
  el := doc.CreateElement('element1');
  attr := doc.CreateAttributeNS('http://www.freepascal.org', 'fpc:newAttr');
  el.SetAttributeNodeNS(attr);
  AssertEquals('ownerElement_before', el, attr.OwnerElement);
  attr2 := doc.CreateAttributeNS('http://www.freepascal.org', 'fpc:newAttr');
  retNode := el.Attributes.SetNamedItemNS(attr2);
  AssertSame('retNode', attr, retNode);
  AssertNull('ownerElement_after', attr.OwnerElement);
  AssertEquals('ownerElement2', el, attr2.OwnerElement);
end;


// verify that Element.removeAttributeNode() resets ownerElement
// of the attribute being removed
procedure TDOMTestExtra.attr_ownership05;
var
  doc: TDOMDocument;
  el: TDOMElement;
  attr: TDOMAttr;
begin
  LoadStringData(doc, '<doc/>');
  el := doc.CreateElement('element1');
  attr := doc.CreateAttributeNS('http://www.freepascal.org', 'fpc:newAttr');
  el.SetAttributeNodeNS(attr);
  AssertEquals('ownerElement_before', el, attr.OwnerElement);
  el.RemoveAttributeNode(attr);
  AssertNull('ownerElement_after', attr.ownerElement);
end;

// verify that replacing a node by itself does not remove it from the tree
// (specs say this is implementation-dependent, but guess that means either
//  no-op or raising an exception, not removal).
procedure TDOMTestExtra.replacesamechild;
var
  doc: TDOMDocument;
  root, el, prev, next: TDOMNode;
begin
  LoadStringData(doc, '<root><child1/><child2/><child3/></root>');
  root := doc.DocumentElement;
  el := root.ChildNodes[1];
  prev := el.PreviousSibling;
  next := el.NextSibling;
  AssertEqualsW('prev_name_before', 'child1', prev.NodeName);
  AssertEqualsW('next_name_before', 'child3', next.NodeName);
  root.replaceChild(el, el);
  prev := el.PreviousSibling;
  next := el.NextSibling;
  AssertNotNull('prev_after', prev);
  AssertNotNull('prev_after', next);
  AssertEqualsW('prev_name_after', 'child1', prev.NodeName);
  AssertEqualsW('next_name_after', 'child3', next.NodeName);
end;

// verify that inserting a node before the first child sets
// both refnode.previoussibling and newnode.nextsibling properties
procedure TDOMTestExtra.insertbeforefirst;
var
  doc: TDOMDocument;
  root, refchild, newchild: TDOMNode;
begin
  LoadStringData(doc, '<root><child1/><child2/><child3/></root>');
  root := doc.DocumentElement;
  refchild := root.FirstChild;
  newchild := doc.CreateElement('new');
  root.insertbefore(newchild, refchild);
  AssertEquals('prev', refchild.previoussibling, newchild);
  AssertEquals('next', newchild.nextsibling, refchild);
  AssertEquals('child', root.firstchild, newchild);
end;

const
  nsURI1 = 'http://www.example.com/ns1';
  nsURI2 = 'http://www.example.com/ns2';
  svgNS = 'http://www.w3.org/2000/svg';
  xlinkNS = 'http://www.w3.org/1999/xlink';
  xmlnsNS = 'http://www.w3.org/2000/xmlns/';

// verify the namespace fixup with two nested elements
// (same localName, different nsURI, and no prefixes)
procedure TDOMTestExtra.nsFixup1;
var
  domImpl: TDOMImplementation;
  origDoc: TDOMDocument;
  parsedDoc: TDOMDocument;
  docElem: TDOMElement;
  el: TDOMElement;
  stream: TStringStream;
  list: TDOMNodeList;
begin
  FParser.Options.Namespaces := True;
  domImpl := GetImplementation;
  origDoc := domImpl.createDocument(nsURI1, 'test', nil);
  GC(origDoc);
  docElem := origDoc.documentElement;
  el := origDoc.CreateElementNS(nsURI2, 'test');
  docElem.AppendChild(el);

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  LoadStringData(parsedDoc, stream.DataString);

  docElem := parsedDoc.documentElement;
  assertEqualsW('docElemLocalName', 'test', docElem.localName);
  assertEqualsW('docElemNS', nsURI1, docElem.namespaceURI);

  list := docElem.GetElementsByTagNameNS(nsURI2, '*');
  assertEquals('ns2_elementCount', 1, list.Length);
  el := TDOMElement(list[0]);
  assertEqualsW('ns2_nodeName', 'test', el.nodeName);
end;

// verify the namespace fixup with two nested elements
// (same localName, different nsURI, different prefixes)
procedure TDOMTestExtra.nsFixup2;
var
  domImpl: TDOMImplementation;
  origDoc: TDOMDocument;
  parsedDoc: TDOMDocument;
  docElem: TDOMElement;
  el: TDOMElement;
  stream: TStringStream;
  list: TDOMNodeList;
begin
  FParser.Options.Namespaces := True;
  domImpl := GetImplementation;
  origDoc := domImpl.createDocument(nsURI1, 'a:test', nil);
  GC(origDoc);
  docElem := origDoc.documentElement;
  el := origDoc.CreateElementNS(nsURI2, 'b:test');
  docElem.AppendChild(el);

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  LoadStringData(parsedDoc, stream.DataString);

  docElem := parsedDoc.documentElement;
  assertEqualsW('docElemLocalName', 'test', docElem.localName);
  assertEqualsW('docElemNS', nsURI1, docElem.namespaceURI);

  list := docElem.GetElementsByTagNameNS(nsURI2, '*');
  assertEquals('ns2_elementCount', 1, list.Length);
  el := TDOMElement(list[0]);
  assertEqualsW('ns2_nodeName', 'b:test', el.nodeName);
end;

// verify the namespace fixup with two nested elements and an attribute
// attribute's prefix must change to that of document element
procedure TDOMTestExtra.nsFixup3;
var
  domImpl: TDOMImplementation;
  origDoc: TDOMDocument;
  parsedDoc: TDOMDocument;
  docElem: TDOMElement;
  el: TDOMElement;
  stream: TStringStream;
  list: TDOMNodeList;
  attr: TDOMAttr;
begin
  FParser.Options.Namespaces := True;
  domImpl := GetImplementation;
  origDoc := domImpl.createDocument(nsURI1, 'a:test', nil);
  GC(origDoc);
  docElem := origDoc.documentElement;
  el := origDoc.CreateElementNS(nsURI2, 'b:test');
  docElem.AppendChild(el);
  el.SetAttributeNS(nsURI1, 'test:attr', 'test value');

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  LoadStringData(parsedDoc, stream.DataString);

  docElem := parsedDoc.documentElement;
  assertEqualsW('docElemLocalName', 'test', docElem.localName);
  assertEqualsW('docElemNS', nsURI1, docElem.namespaceURI);

  list := docElem.GetElementsByTagNameNS(nsURI2, '*');
  assertEquals('ns2_elementCount', 1, list.Length);
  el := TDOMElement(list[0]);
  attr := el.GetAttributeNodeNS(nsURI1, 'attr');
  assertEqualsW('attr_nodeName', 'a:attr', attr.nodeName);
end;


// First child of aNode that is an element, nil if there is none
function FirstChildElement(aNode: TDOMNode): TDOMElement;
var
  lChild: TDOMNode;
begin
  Result := nil;
  lChild := aNode.FirstChild;
  while Assigned(lChild) and (lChild.NodeType <> ELEMENT_NODE) do
    lChild := lChild.NextSibling;
  if Assigned(lChild) then
    Result := TDOMElement(lChild);
end;

// a level 1 element with a level 2 attribute inside a default namespace (issue #22299):
// the default namespace is undeclared and the attribute's prefix is declared
procedure TDOMTestExtra.nsFixup4;
var
  domImpl: TDOMImplementation;
  origDoc: TDOMDocument;
  parsedDoc: TDOMDocument;
  el: TDOMElement;
  stream: TStringStream;
begin
  FParser.Options.Namespaces := True;
  domImpl := GetImplementation;
  origDoc := domImpl.createDocument(svgNS, 'svg', nil);
  GC(origDoc);
  el := origDoc.CreateElement('script');
  origDoc.documentElement.AppendChild(el);
  el.SetAttribute('type', 'text/ecmascript');
  el.SetAttributeNS(xlinkNS, 'xlink:href', 'cool-script.js');

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  assertTrue('xmlns:xlink declared', Pos('xmlns:xlink="'+xlinkNS+'"', stream.DataString) > 0);
  assertTrue('default namespace undeclared', Pos('xmlns=""', stream.DataString) > 0);
  LoadStringData(parsedDoc, stream.DataString);

  assertEqualsW('docElemNS', svgNS, parsedDoc.documentElement.namespaceURI);
  el := FirstChildElement(parsedDoc.documentElement);
  assertNotNull('script element', el);
  assertEqualsW('script_localName', 'script', el.localName);
  assertEqualsW('script_NS', '', el.namespaceURI);
  assertEqualsW('script_type', 'text/ecmascript', el.GetAttribute('type'));
  assertEqualsW('script_href', 'cool-script.js', el.GetAttributeNS(xlinkNS, 'href'));
end;

// a document with only level 1 nodes is written without any namespace declarations
procedure TDOMTestExtra.nsFixup5;
var
  origDoc: TXMLDocument;
  root, el: TDOMElement;
  stream: TStringStream;
begin
  origDoc := TXMLDocument.Create;
  GC(origDoc);
  root := origDoc.CreateElement('root');
  origDoc.AppendChild(root);
  root.SetAttribute('a', '1');
  el := origDoc.CreateElement('child');
  root.AppendChild(el);
  el.SetAttribute('b', '2');

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  assertEquals('no namespace declarations', 0, Pos('xmlns', stream.DataString));
  assertTrue('root written', Pos('<root a="1">', stream.DataString) > 0);
  assertTrue('child written', Pos('<child b="2"/>', stream.DataString) > 0);
end;

// a level 1 element with a level 2 attribute outside any default namespace
// declares the attribute's prefix, but does not write a needless xmlns=""
procedure TDOMTestExtra.nsFixup6;
var
  origDoc: TXMLDocument;
  parsedDoc: TDOMDocument;
  root: TDOMElement;
  stream: TStringStream;
begin
  FParser.Options.Namespaces := True;
  origDoc := TXMLDocument.Create;
  GC(origDoc);
  root := origDoc.CreateElement('root');
  origDoc.AppendChild(root);
  root.SetAttributeNS(xlinkNS, 'xlink:href', 'target');

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  assertTrue('xmlns:xlink declared', Pos('xmlns:xlink="'+xlinkNS+'"', stream.DataString) > 0);
  assertEquals('no xmlns=""', 0, Pos('xmlns=""', stream.DataString));
  LoadStringData(parsedDoc, stream.DataString);
  assertEqualsW('root_href', 'target', parsedDoc.documentElement.GetAttributeNS(xlinkNS, 'href'));
end;

// a level 1 element declaring its own default namespace inside namespaced content
// keeps that single declaration
procedure TDOMTestExtra.nsFixup7;
var
  domImpl: TDOMImplementation;
  origDoc: TDOMDocument;
  parsedDoc: TDOMDocument;
  el: TDOMElement;
  stream: TStringStream;
begin
  FParser.Options.Namespaces := True;
  domImpl := GetImplementation;
  origDoc := domImpl.createDocument(nsURI1, 'test', nil);
  GC(origDoc);
  el := origDoc.CreateElement('inner');
  origDoc.documentElement.AppendChild(el);
  el.SetAttributeNS(xmlnsNS, 'xmlns', nsURI2);

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  assertEquals('no xmlns=""', 0, Pos('xmlns=""', stream.DataString));
  LoadStringData(parsedDoc, stream.DataString);
  el := FirstChildElement(parsedDoc.documentElement);
  assertEqualsW('inner_NS', nsURI2, el.namespaceURI);
end;

// a level 1 element under a prefixed namespace (no default namespace in scope) is written unchanged
procedure TDOMTestExtra.nsFixup8;
var
  domImpl: TDOMImplementation;
  origDoc: TDOMDocument;
  parsedDoc: TDOMDocument;
  el: TDOMElement;
  stream: TStringStream;
begin
  FParser.Options.Namespaces := True;
  domImpl := GetImplementation;
  origDoc := domImpl.createDocument(nsURI1, 'a:test', nil);
  GC(origDoc);
  el := origDoc.CreateElement('plain');
  origDoc.documentElement.AppendChild(el);

  stream := TStringStream.Create('');
  GC(stream);
  writeXML(origDoc, stream);
  assertTrue('plain written without declarations', Pos('<plain/>', stream.DataString) > 0);
  LoadStringData(parsedDoc, stream.DataString);
  el := FirstChildElement(parsedDoc.documentElement);
  assertEqualsW('plain_NS', '', el.namespaceURI);
end;


initialization
  RegisterTest(TDOMTestExtra);

end.

