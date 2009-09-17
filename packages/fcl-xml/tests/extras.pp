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
  SysUtils, Classes, DOM, xmlread, domunit, testregistry;

implementation

type
  TDOMTestExtra = class(TDOMTestBase)
  published
    procedure attr_ownership01;
    procedure attr_ownership02;
    procedure attr_ownership03;
    procedure attr_ownership04;
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




initialization
  RegisterTest(TDOMTestExtra);

end.

