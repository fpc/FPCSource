{**********************************************************************

    This file is part of the Free Component Library (FCL)

    Test cases for TXMLReader class and its descendants
    Copyright (c) 2012 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    Using tests from Mono test suite, written by
      Jason Diamond (jason@injektilo.org)
      Martin Willemoes Hansen (mwh@sysrq.dk)
      (C) 2001, 2002 Jason Diamond  http://injektilo.org/    

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit readertest;
{$mode objfpc}{$h+}

interface
implementation

uses
  sysutils, fpcunit, testregistry, xmlutils, readerunit, XmlReader;

type
  TXMLReaderTest = class(TXMLReaderTestBase)
  public
    procedure TestInitialState(r: TXMLReader);
    procedure TestReadEmptySource(r: TXMLReader);
    procedure TestRead(r: TXMLReader);
    procedure TestReadAttributeValue(r: TXMLReader);
    procedure TestReadAttributeValue2(r: TXMLReader);
    procedure TestDTDProperties(r: TXMLReader);
    procedure TestReadStringFromElement(r: TXMLReader);
    procedure TestEmptyElement(r: TXMLReader);
    procedure TestNestedEmptyTag(r: TXMLReader);
    procedure TestNestedText(r: TXMLReader);
    procedure TestEmptyElementWithAttributes(r: TXMLReader);
    procedure TestPIBeforeRoot(r: TXMLReader);
    procedure TestCommentBeforeRoot(r: TXMLReader);
    procedure TestCDATA(r: TXMLReader);
    procedure TestChildElementInNamespace(r: TXMLReader);
    procedure TestMoveToElementFromAttribute(r: TXMLReader);
    procedure TestMoveToElementFromElement(r: TXMLReader);
    procedure TestMoveToNextAttribute(r: TXMLReader);
    procedure TestResolveEntity(r: TXMLReader);
    procedure TestLineInfo(r: TXMLReader);
  published
    procedure InitialState;
    procedure ReadEmptySource;
    procedure Read;
    procedure ReadAttributeValue;
    procedure ReadAttributeValue2;
    procedure DTDProperties;
    procedure ReadStringFromElement;
    procedure EmptyElement;
    procedure NestedEmptyTag;
    procedure NestedText;
    procedure EmptyElementWithAttributes;
    procedure PIBeforeRoot;
    procedure CommentBeforeRoot;
    procedure CDATA;
    procedure ChildElementInNamespace;
    procedure MoveToElementFromAttribute;
    procedure MoveToElementFromElement;
    procedure MoveToNextAttribute;
    procedure ResolveEntity;
    procedure LineInfo;
  end;

const
  xml1 = '<root attr1=''value1''><child /></root>';
  xml2 = '<root><foo/><bar>test.</bar></root>';
  xml3 = '<root>  test of <b>mixed</b> string.<![CDATA[ cdata string.]]></root>';


procedure TXMLReaderTest.TestInitialState(r: TXMLReader);
begin
  AssertEquals('Depth', 0, r.Depth);
  AssertEquals('EOF', False, r.EOF);
  AssertEquals('HasValue', False, r.HasValue);
  AssertEqualsW('LocalName', '', r.LocalName);
  AssertEquals('NodeType', ntNone, r.NodeType);
  AssertEquals('ReadState', rsInitial, r.ReadState);
end;

procedure TXMLReaderTest.InitialState;
begin
  DoTest(xml1, @TestInitialState);
end;

// TODO: this should fail only when conformancelevel=document
procedure TXMLReaderTest.TestReadEmptySource(r: TXMLReader);
begin
  try
    r.Read;
    Fail('Read empty source succeeded');
  except
  end;
end;

procedure TXMLReaderTest.ReadEmptySource;
begin
  DoTest('', @TestReadEmptySource);
end;

procedure TXMLReaderTest.TestRead(r: TXMLReader);
begin
  r.Read;
  AssertEquals('<root>.NodeType', ntElement, r.NodeType);
  AssertEqualsW('<root>.Name', 'root', r.Name);
  AssertEquals('<root>.ReadState', rsInteractive, r.ReadState);
  AssertEquals('<root>.Depth', 0, r.Depth);

  // move to 'child'
  r.Read;
  AssertEquals('<child/>.Depth', 1, r.Depth);
  AssertEquals('<child/>.NodeType', ntElement, r.NodeType);
  AssertEqualsW('<child/>.Name', 'child', r.Name);

  { this differs from .net; we don't support EmptyElement }
  r.Read;
  AssertEquals('<child/>.EndDepth', 1, r.Depth);
  AssertEquals('<child/>.EndNodeType', ntEndElement, r.NodeType);
  AssertEqualsW('<child/>.EndName', 'child', r.Name);

  r.Read;
  AssertEquals('</root>.Depth', 0, r.Depth);
  AssertEquals('</root>.NodeType', ntEndElement, r.NodeType);
  AssertEqualsW('</root>.Name', 'root', r.Name);

  r.Read;
  AssertTrue('end.EOF', r.EOF);
  AssertEquals('end.NodeType', ntNone, r.NodeType);
end;

procedure TXMLReaderTest.Read;
begin
  DoTest(xml1, @TestRead);
end;

procedure TXMLReaderTest.TestReadAttributeValue(r: TXMLReader);
begin
  r.Read;       // root
  AssertTrue('#1',r.MoveToFirstAttribute);
  AssertNodeValues('#2', r,
    ntAttribute,
    1, 'attr', '', 'attr', '', 'value', true, 1, true);
  AssertTrue('#3',r.ReadAttributeValue);
  AssertNodeValues('#4', r,
    ntText,
    2, '', '', '', '', 'value', true, 1, true);
  AssertTrue('#5',r.MoveToElement);
  AssertNodeValues('#6', r,
    ntElement,
    0, 'root', '', 'root', '', '', false, 1, true);
end;

procedure TXMLReaderTest.ReadAttributeValue;
begin
  DoTest('<root attr="value"/>', @TestReadAttributeValue);
end;

procedure TXMLReaderTest.TestReadAttributeValue2(r: TXMLReader);
begin
  r.Read;       // DTD
  r.Read;       // root
  AssertTrue('#1',r.MoveToFirstAttribute);
  AssertNodeValues('#2', r,
    ntAttribute,
    1, 'attr', '', 'attr', '', 'ax1y1x1b', true, 1, true);
  AssertTrue('#3',r.ReadAttributeValue);
  AssertNodeValues('#4', r,
    ntText, 2,
    '', '', '', '', 'a', true, 1, true);

  AssertTrue('#5', r.ReadAttributeValue);
  AssertNodeValues('#6', r,
    ntEntityReference, 2,
    'e1','','e1','', '', false, 1, true);

  r.ResolveEntity;
  AssertEquals('#6a', 2, r.Depth);
  AssertEquals('#6b', ntEntityReference, r.nodeType);

  AssertTrue('#6c', r.ReadAttributeValue);
  AssertNodeValues('#6d', r,
    ntEntityReference, 3,
    'e2', '', 'e2', '', '', false, 1, true);

  // Don't resolve it and advance to 'y1' textnode
  AssertTrue('#6e', r.ReadAttributeValue);
  AssertNodeValues('#6f', r,
    ntText, 3,
    '', '', '', '', 'y1', true, 1, true);

  AssertTrue('#6g', r.ReadAttributeValue);
  AssertNodeValues('#6h', r,
    ntEntityReference, 3,
    'e2', '', 'e2', '', '', false, 1, true);

  r.ResolveEntity;

  AssertTrue('#6i', r.ReadAttributeValue);
  AssertNodeValues('#6j', r,
    ntText, 4,
    '','','','','x1', true, 1, true);

  AssertTrue('#6k', r.ReadAttributeValue);
  AssertNodeValues('#6l', r,
    ntEndEntity, 3,
    'e2','','e2','', '', false, 1, true);

  AssertTrue('#7', r.ReadAttributeValue);
  AssertNodeValues('#8', r,
    ntEndEntity, 2,
    'e1','','e1','', '', false, 1, true);

  AssertTrue('#9', r.ReadAttributeValue);

  AssertNodeValues('#10', r,
    ntText, 2,
    '', '', '', '', 'b', true, 1, true);
  AssertFalse('#11', r.ReadAttributeValue);

  AssertTrue('#12',r.MoveToElement);
  AssertNodeValues('#13', r,
    ntElement, 0,
    'root', '', 'root', '', '', false, 1, true);

end;

procedure TXMLReaderTest.ReadAttributeValue2;
const
  xml = '<!DOCTYPE root ['+
        '<!ELEMENT root ANY>'+
        '<!ENTITY e2 "x1">'+
        '<!ENTITY e1 "&e2;y1&e2;">]>'+
        '<root attr="a&e1;b"/>';
begin
  DoTest(xml, @TestReadAttributeValue2);
end;

procedure TXMLReaderTest.TestDTDProperties(r: TXMLReader);
begin
  r.Read;
  AssertNodeValues('#DTD',r,
    ntDocumentType, 0,
    'root', '', 'root', '', '<!ELEMENT root ANY><!ENTITY e2 "x1"><!ENTITY e1 "&e2;y1&e2;">', True, 0, False);
  r.Read;
  AssertNodeValues('#root',r,
    ntElement, 0,
    'root','', 'root', '', '', False,
    1, True);
end;

procedure TXMLReaderTest.DTDProperties;
const
  xml = '<!DOCTYPE root ['+
        '<!ELEMENT root ANY>'+
        '<!ENTITY e2 "x1">'+
        '<!ENTITY e1 "&e2;y1&e2;">]>'+
        '<root attr="a&e1;b"/>';
begin
  DoTest(xml, @TestDTDProperties);
end;

procedure TXMLReaderTest.TestReadStringFromElement(r: TXMLReader);
var
  s: XMLString;
begin
  r.Read;
  s := r.ReadString;
  AssertEqualsW('readString.1.ret_val', '  test of ', s);
  AssertEqualsW('readString.1.Name', 'b', r.Name);
  s := r.ReadString;
  AssertEqualsW('readString.2.ret_val', 'mixed', s);
  AssertEquals('readString.2.nodeType', ntEndElement, r.NodeType);
  s := r.ReadString; // does not advance
  AssertEqualsW('readString.3.ret_val', '',s);
  AssertEquals('readString.3.nodeType', ntEndElement, r.NodeType);
  r.Read;
  AssertEquals('readString.4.nodeType', ntText, r.NodeType);
  AssertEqualsW('readString.4.Value', ' string.', r.Value);
  s := r.ReadString;
  AssertEqualsW('readString.5.ret_val', ' string. cdata string.', s);
  AssertEquals('readString.5.nodeType', ntEndElement, r.NodeType);
end;

procedure TXMLReaderTest.ReadStringFromElement;
begin
  DoTest(xml3, @TestReadStringFromElement);
end;

procedure TXMLReaderTest.TestEmptyElement(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r, ntElement, 0,
    'foo', '', 'foo', '', // name, prefix, localname, nsuri
    '', 0);
  AssertNode('#2', r, ntEndElement, 0,
    'foo', '', 'foo', '',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.EmptyElement;
begin
  DoTest('<foo/>', @TestEmptyElement);
end;

procedure TXMLReaderTest.TestNestedEmptyTag(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
    ntElement, 0,
    'foo', '', 'foo', '',
    '', 0);
  AssertNode('#2', r,
    ntElement, 1,
    'bar', '', 'bar', '',
    '', 0);
  AssertNode('#3', r,
    ntEndElement, 1,
    'bar', '', 'bar', '',
    '', 0);
  AssertNode('#4', r,
    ntEndElement, 0,
    'foo', '', 'foo', '',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.NestedEmptyTag;
begin
  DoTest('<foo><bar/></foo>', @TestNestedEmptyTag);
end;

procedure TXMLReaderTest.TestNestedText(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
    ntElement, 0,
    'foo', '', 'foo', '',
    '', 0);
  AssertNode('#2', r,
    ntText, 1,
    '', '', '', '',
    'bar', 0);
  AssertNode('#3', r,
    ntEndElement, 0,
    'foo', '', 'foo', '',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.NestedText;
begin
  DoTest('<foo>bar</foo>', @TestNestedText);
end;

procedure TXMLReaderTest.TestEmptyElementWithAttributes(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
    ntElement, 0,
    'foo', '', 'foo', '',
    '', 4);
  AssertAttribute(r,
    'bar', '', 'bar', '',
    'baz');
  AssertAttribute(r,
    'quux', '', 'quux', '',
    'quuux');
  AssertAttribute(r,                    // non-existing attribute
    'notexist', '', 'notexist', '',
    '');
  AssertAttribute(r,                    // non-existing prefixed attribute
    'x:bar', 'x', 'bar', 'urn:xfoo',
    '');
  AssertAttribute(r,
    'x:foo', 'x', 'foo', 'urn:xfoo',
    'x-foo');
  AssertNode('#2', r,
    ntEndElement, 0,
    'foo', '', 'foo', '',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.EmptyElementWithAttributes;
begin
  DoTest('<foo bar="baz" quux=''quuux'' x:foo=''x-foo'' xmlns:x = ''urn:xfoo'' />',
    @TestEmptyElementWithAttributes);
end;

procedure TXMLReaderTest.TestPIBeforeRoot(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
    ntProcessingInstruction, 0,
    'foo', '', 'foo', '',
    'bar', 0);
  AssertNode('#2', r,
    ntElement, 0,
    'baz', '', 'baz', '',
    '', 0);
  AssertNode('#3', r,
    ntEndElement, 0,
    'baz', '', 'baz', '',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.PIBeforeRoot;
begin
  DoTest('<?foo bar?><baz/>', @TestPIBeforeRoot);
end;

procedure TXMLReaderTest.TestCommentBeforeRoot(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
    ntComment, 0,
    '','','','',
    'foo', 0);
  AssertNode('#2', r,
    ntElement, 0,
    'bar', '', 'bar', '',
    '', 0);
  AssertNode('#3', r,
    ntEndElement, 0,
    'bar', '', 'bar', '',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.CommentBeforeRoot;
begin
  DoTest('<!--foo--><bar/>', @TestCommentBeforeRoot);
end;

procedure TXMLReaderTest.TestCDATA(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
   ntElement, 0,
   'foo', '', 'foo', '',
   '', 0);
  AssertNode('#2', r,
    ntCDATA, 1,
    '','','','',
    '<>&', 0);
  AssertNode('#3', r,
    ntEndElement, 0,
    'foo', '', 'foo', '',
    '',0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.CDATA;
begin
  DoTest('<foo><![CDATA[<>&]]></foo>', @TestCDATA);
end;

procedure TXMLReaderTest.ChildElementInNamespace;
begin
  DoTest('<foo:bar xmlns:foo=''http://foo/''><baz:quux xmlns:baz=''http://baz/'' /></foo:bar>', @TestChildElementInNamespace);
end;

procedure TXMLReaderTest.TestChildElementInNamespace(r: TXMLReader);
begin
  AssertStartDocument(r);

  AssertNode('#1', r,
    ntElement, 0,
    'foo:bar', 'foo', 'bar', 'http://foo/',
    '', 1);

  AssertAttribute(r,
    'xmlns:foo', 'xmlns', 'foo', 'http://www.w3.org/2000/xmlns/',
    'http://foo/');

  AssertEqualsW('http://foo/', r.LookupNamespace('foo'));

  AssertNode('#2', r,
    ntElement, 1,
    'baz:quux', 'baz', 'quux', 'http://baz/',
    '', 1);

  AssertAttribute(r,
    'xmlns:baz', 'xmlns', 'baz', 'http://www.w3.org/2000/xmlns/',
    'http://baz/');

  AssertEqualsW('http://foo/', r.LookupNamespace ('foo'));
  AssertEqualsW('http://baz/', r.LookupNamespace ('baz'));

  AssertNode('#3', r,
    ntEndElement, 1,
    'baz:quux', 'baz', 'quux', 'http://baz/',
    '', 0);

  AssertNode('#4', r,
    ntEndElement, 0,
    'foo:bar', 'foo', 'bar', 'http://foo/',
    '', 0);

  AssertEqualsW('http://foo/', r.LookupNamespace('foo'));
  AssertNull('', r.LookupNamespace('baz'));

  AssertEndDocument(r);
end;

procedure TXMLReaderTest.TestMoveToElementFromAttribute(r: TXMLReader);
begin
  AssertTrue(r.Read);
  AssertEquals(ntElement, r.NodeType);
  AssertTrue(r.MoveToFirstAttribute);
  AssertEquals(ntAttribute, r.NodeType);
  AssertTrue(r.MoveToElement);
  AssertEquals(ntElement, r.NodeType);
end;

procedure TXMLReaderTest.MoveToElementFromAttribute;
begin
  DoTest('<foo bar="baz" />', @TestMoveToElementFromAttribute);
end;

procedure TXMLReaderTest.TestMoveToElementFromElement(r: TXMLReader);
begin
  AssertTrue(r.Read);
  AssertEquals(ntElement, r.NodeType);
  AssertFalse(r.MoveToElement);
  AssertEquals(ntElement, r.NodeType);
end;

procedure TXMLReaderTest.MoveToElementFromElement;
begin
  DoTest('<foo bar="baz" />', @TestMoveToElementFromElement);
end;

// TODO: moveToFirstAttribute_negative
//       moveToNextAttribute_negative

procedure TXMLReaderTest.TestMoveToNextAttribute(r: TXMLReader);
begin
  AssertStartDocument(r);
  AssertNode('#1', r,
    ntElement, 0,
    'foo', '', 'foo', '',
    '', 2);
  AssertAttribute(r,
    'bar', '', 'bar', '',
    'baz');
  AssertAttribute(r,
    'quux', '', 'quux', '',
    'quuux');
  AssertTrue(r.MoveToNextAttribute);
  AssertEqualsW('bar', r.Name);
  AssertEqualsW('baz', r.Value);
  AssertTrue(r.MoveToNextAttribute);
  AssertEqualsW('quux', r.Name);
  AssertEqualsW('quuux', r.Value);
  AssertFalse(r.MoveToNextAttribute);
  AssertTrue(r.MoveToElement);
  AssertNodeValues('#1', r,
    ntElement, 0,
    'foo','','foo','',
    '', False, 2, True);
  AssertNode('#2', r,
    ntEndElement, 0,
    'foo','','foo','',
    '', 0);
  AssertEndDocument(r);
end;

procedure TXMLReaderTest.MoveToNextAttribute;
begin
  DoTest('<foo bar="baz" quux=''quuux''/>', @TestMoveToNextAttribute);
end;

procedure TXMLReaderTest.TestResolveEntity(r: TXMLReader);
begin
  r.Read;       // DTD
  r.Read;       // root
  r.Read;       // &ent;
  AssertEquals('#1', ntEntityReference, r.NodeType);
  AssertEquals('#2', 1, r.Depth);
  r.ResolveEntity;
  // stays on entity reference
  AssertEquals('#3', ntEntityReference, r.NodeType);
  AssertEquals('#3a', 1, r.Depth);
  r.Read;
  // now advances to replacement text
  AssertEquals('#4', ntText, r.NodeType);
  AssertEquals('#5', 2, r.Depth);
  AssertEqualsW('#6', 'entity string', r.Value);
  r.Read;
  AssertEquals('#7', ntEndEntity, r.NodeType);
  AssertEquals('#8', 1, r.Depth);
  AssertEqualsW('#9', '', r.Value);

  r.Read;     // &ent2;
  AssertEquals('#10', ntEntityReference, r.NodeType);
  AssertEquals('#11', 1, r.Depth);
  r.ResolveEntity;
  // stays on entity reference
  AssertEquals('#12', ntEntityReference, r.NodeType);
  AssertEquals('#12a', 1, r.Depth);
  // now advances to element node
  r.Read;
  AssertEquals('#13', ntElement, r.NodeType);
  AssertEquals('#14', 2, r.Depth);
end;

procedure TXMLReaderTest.ResolveEntity;
const
  xml='<!DOCTYPE root [<!ELEMENT root (#PCDATA|foo)*>'+
      '<!ENTITY ent "entity string">'+
      '<!ENTITY ent2 "<foo/><foo/>">]>'+
      '<root>&ent;&ent2;</root>';
begin
  DoTest(xml, @TestResolveEntity);
end;

procedure TXMLReaderTest.TestLineInfo(r: TXMLReader);
var
  aux: IXmlLineInfo;
begin
  if not Supports(r, IXmlLineInfo, aux) then
    Exit;
  AssertEquals('#1a', 0, aux.LineNumber);
  AssertEquals('#1b', 0, aux.LinePosition);
  r.MoveToContent;
  AssertEquals('#2a', 1, aux.LineNumber);
  AssertEquals('#2b', 2, aux.LinePosition);
  r.Read;
  AssertEquals('#3a', 1, aux.LineNumber);
  AssertEquals('#3b', 7, aux.LinePosition);
  r.Read;
  r.Read;
//  r.ReadOuterXml;
  AssertEquals('#4a', 1, aux.LineNumber);
  AssertEquals('#4b', 22, aux.LinePosition);
  r.Read;
  r.Read;
//  r.ReadInnerXml;
  AssertEquals('#5a', 1, aux.LineNumber);
  AssertEquals('#5b', 34, aux.LinePosition);
  r.Read;     { now at EOF }
  AssertEquals('#6a', 1, aux.LineNumber);
  AssertEquals('#6b', 38, aux.LinePosition);
  r.Close;
  AssertEquals('#7a', 0, aux.LineNumber);
  AssertEquals('#7b', 0, aux.LinePosition);
end;

procedure TXMLReaderTest.LineInfo;
begin
  DoTest('<all><hello></hello><bug></bug></all>', @TestLineInfo);
end;

initialization
  RegisterTest(TXMLReaderTest);

end.
