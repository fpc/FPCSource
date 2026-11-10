{
    This file is part of the Free Component Library
    Tests for DOM level 3 TypeInfo of attributes and elements (issue #24301).
    See the file COPYING.FPC, included in this distribution, for details about the copyright.
 **********************************************************************}
unit tcxmltypeinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM, XMLRead;

type

  { TTypeInfoDocument }

  TTypeInfoDocument = class(TXMLDocument)
  private
    FElementType: TDOMTypeInfo;
  public
    constructor Create; override;
    destructor Destroy; override;
    // Return FElementType for elements, the inherited type otherwise
    function GetSchemaTypeInfo(aNode: TDOMNode): TDOMTypeInfo; override;
  end;

  { TTestXMLTypeInfo }

  TTestXMLTypeInfo = class(TTestCase)
  private
    FDoc: TXMLDocument;
    procedure Parse(const aXML: String);
    function Attr(const aName: String): TDOMAttr;
    procedure AssertDTDType(const aAttr, aType: String);
    procedure AssertNoType(const aMsg: String; aInfo: TDOMTypeInfo);
  protected
    procedure TearDown; override;
  published
    procedure TestIDAttribute;
    procedure TestCDATAAttribute;
    procedure TestTokenAttributes;
    procedure TestEnumerationAttribute;
    procedure TestNotationAttribute;
    procedure TestDefaultedAttribute;
    procedure TestUndeclaredAttribute;
    procedure TestAttributeOfElementWithoutAttlist;
    procedure TestAttributeWithoutDTD;
    procedure TestXmlIdAttribute;
    procedure TestDetachedAttribute;
    procedure TestElement;
    procedure TestSharedInstance;
    procedure TestIsDerivedFrom;
    procedure TestDocumentHook;
  end;

implementation

const
  cDTDNamespace = 'http://www.w3.org/TR/REC-xml';
  cDTD = '<!DOCTYPE root ['+
         '<!NOTATION gif SYSTEM "image/gif">'+
         '<!NOTATION png SYSTEM "image/png">'+
         '<!ELEMENT root (e*)>'+
         '<!ELEMENT e EMPTY>'+
         '<!ELEMENT plain EMPTY>'+
         '<!ATTLIST e key ID #IMPLIED name CDATA #IMPLIED'+
         ' ref IDREF #IMPLIED refs IDREFS #IMPLIED tok NMTOKEN #IMPLIED toks NMTOKENS #IMPLIED'+
         ' color (red|green) #IMPLIED fmt NOTATION (gif|png) #IMPLIED fixed CDATA #FIXED "f">'+
         ']>';


constructor TTypeInfoDocument.Create;

begin
  inherited Create;
  FElementType:=TDOMTypeInfo.Create('urn:test','elementType');
end;


destructor TTypeInfoDocument.Destroy;

begin
  inherited Destroy;
  FreeAndNil(FElementType);
end;


function TTypeInfoDocument.GetSchemaTypeInfo(aNode: TDOMNode): TDOMTypeInfo;

begin
  if aNode is TDOMElement then
    Result:=FElementType
  else
    Result:=inherited GetSchemaTypeInfo(aNode);
end;


procedure TTestXMLTypeInfo.TearDown;

begin
  FreeAndNil(FDoc);
end;


procedure TTestXMLTypeInfo.Parse(const aXML: String);

var
  lParser: TDOMParser;
  lSource: TXMLInputSource;

begin
  FreeAndNil(FDoc);
  lParser:=TDOMParser.Create;
  lSource:=TXMLInputSource.Create(aXML);
  try
    lParser.Options.Namespaces:=True;
    lParser.Parse(lSource,FDoc);
  finally
    lSource.Free;
    lParser.Free;
  end;
end;


function TTestXMLTypeInfo.Attr(const aName: String): TDOMAttr;

begin
  Result:=TDOMElement(FDoc.DocumentElement.FirstChild).GetAttributeNode(aName);
  AssertNotNull('Attribute '''+aName+''' exists',Result);
end;


procedure TTestXMLTypeInfo.AssertDTDType(const aAttr, aType: String);

var
  lInfo: TDOMTypeInfo;

begin
  lInfo:=Attr(aAttr).SchemaTypeInfo;
  AssertNotNull('Type info of '''+aAttr+'''',lInfo);
  AssertEquals('Type name of '''+aAttr+'''',aType,String(lInfo.TypeName));
  AssertEquals('Type namespace of '''+aAttr+'''',cDTDNamespace,String(lInfo.TypeNamespace));
end;


procedure TTestXMLTypeInfo.AssertNoType(const aMsg: String; aInfo: TDOMTypeInfo);

begin
  AssertNotNull(aMsg+': type info is assigned',aInfo);
  AssertEquals(aMsg+': empty type name','',String(aInfo.TypeName));
  AssertEquals(aMsg+': empty type namespace','',String(aInfo.TypeNamespace));
end;


procedure TTestXMLTypeInfo.TestIDAttribute;

begin
  Parse(cDTD+'<root><e key="k1"/></root>');
  AssertDTDType('key','ID');
end;


procedure TTestXMLTypeInfo.TestCDATAAttribute;

begin
  Parse(cDTD+'<root><e name="first"/></root>');
  AssertDTDType('name','CDATA');
end;


procedure TTestXMLTypeInfo.TestTokenAttributes;

begin
  Parse(cDTD+'<root><e key="k1" ref="k1" refs="k1 k1" tok="t" toks="t u"/></root>');
  AssertDTDType('ref','IDREF');
  AssertDTDType('refs','IDREFS');
  AssertDTDType('tok','NMTOKEN');
  AssertDTDType('toks','NMTOKENS');
end;


procedure TTestXMLTypeInfo.TestEnumerationAttribute;

begin
  Parse(cDTD+'<root><e color="red"/></root>');
  AssertDTDType('color','ENUMERATION');
end;


procedure TTestXMLTypeInfo.TestNotationAttribute;

begin
  Parse(cDTD+'<root><e fmt="gif"/></root>');
  AssertDTDType('fmt','NOTATION');
end;


procedure TTestXMLTypeInfo.TestDefaultedAttribute;

begin
  Parse(cDTD+'<root><e name="first"/></root>');
  AssertFalse('The fixed attribute is not specified',Attr('fixed').Specified);
  AssertDTDType('fixed','CDATA');
end;


procedure TTestXMLTypeInfo.TestUndeclaredAttribute;

begin
  Parse(cDTD+'<root><e name="first" extra="x"/></root>');
  AssertNoType('Attribute missing from the ATTLIST',Attr('extra').SchemaTypeInfo);
  AssertDTDType('name','CDATA');
end;


procedure TTestXMLTypeInfo.TestAttributeOfElementWithoutAttlist;

begin
  Parse(cDTD+'<root><plain name="x"/></root>');
  AssertNoType('Attribute of an element without ATTLIST',Attr('name').SchemaTypeInfo);
end;


procedure TTestXMLTypeInfo.TestAttributeWithoutDTD;

begin
  Parse('<root><e key="k1" name="first"/></root>');
  AssertNoType('Attribute in a document without DTD',Attr('key').SchemaTypeInfo);
end;


procedure TTestXMLTypeInfo.TestXmlIdAttribute;

begin
  Parse('<root><e xml:id="x1"/></root>');
  AssertDTDType('xml:id','ID');
end;


procedure TTestXMLTypeInfo.TestDetachedAttribute;

var
  lAttr: TDOMAttr;

begin
  Parse(cDTD+'<root><e name="first"/></root>');
  lAttr:=FDoc.CreateAttribute('name');
  try
    AssertNoType('Attribute without owner element',lAttr.SchemaTypeInfo);
  finally
    lAttr.Free;
  end;
end;


procedure TTestXMLTypeInfo.TestElement;

begin
  Parse(cDTD+'<root><e name="first"/></root>');
  AssertNoType('Element in a document with DTD',FDoc.DocumentElement.SchemaTypeInfo);
end;


procedure TTestXMLTypeInfo.TestSharedInstance;

begin
  Parse(cDTD+'<root><e key="k1" name="first"/></root>');
  AssertSame('CDATA attributes share one type instance',Attr('name').SchemaTypeInfo,Attr('fixed').SchemaTypeInfo);
  AssertTrue('ID and CDATA types differ',Attr('key').SchemaTypeInfo<>Attr('name').SchemaTypeInfo);
end;


procedure TTestXMLTypeInfo.TestIsDerivedFrom;

var
  lInfo: TDOMTypeInfo;

begin
  Parse(cDTD+'<root><e key="k1"/></root>');
  lInfo:=Attr('key').SchemaTypeInfo;
  AssertFalse('DTD type does not derive from itself',lInfo.IsDerivedFrom(cDTDNamespace,'ID',DERIVATION_RESTRICTION));
  AssertFalse('DTD type does not derive from CDATA',lInfo.IsDerivedFrom(cDTDNamespace,'CDATA',
    DERIVATION_RESTRICTION or DERIVATION_EXTENSION or DERIVATION_UNION or DERIVATION_LIST));
end;


procedure TTestXMLTypeInfo.TestDocumentHook;

var
  lEl: TDOMElement;

begin
  FreeAndNil(FDoc);
  FDoc:=TTypeInfoDocument.Create;
  lEl:=FDoc.CreateElement('root');
  FDoc.AppendChild(lEl);
  lEl.SetAttribute('name','x');
  AssertEquals('Element type name from the document descendant','elementType',String(lEl.SchemaTypeInfo.TypeName));
  AssertEquals('Element type namespace from the document descendant','urn:test',String(lEl.SchemaTypeInfo.TypeNamespace));
  AssertNoType('Attribute type from the inherited implementation',lEl.GetAttributeNode('name').SchemaTypeInfo);
end;


initialization
  RegisterTest(TTestXMLTypeInfo);
end.
