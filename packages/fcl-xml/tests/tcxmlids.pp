{
    This file is part of the Free Component Library

    Tests for the ID table of documents read by the XML reader (issue #24032).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcxmlids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM, XMLRead, XMLReader;

type

  { TTestXMLIDs }

  TTestXMLIDs = class(TTestCase)
  private
    FDoc: TXMLDocument;
    FErrors: TStringList;
    procedure DoError(E: EXMLReadError);
    procedure Parse(const aXML: String; aValidate: Boolean = False);
    procedure AssertID(const aID, aElement: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDTDIDNotValidating;
    procedure TestDTDIDValidating;
    procedure TestDTDIDNormalized;
    procedure TestDuplicateIDNotValidating;
    procedure TestDuplicateIDValidating;
    procedure TestXmlIdWithoutDTD;
    procedure TestXmlIdNormalized;
    procedure TestXmlIdWithoutNamespaces;
    procedure TestPlainIdIsNotAnID;
    procedure TestCDATAAttributeIsNotAnID;
  end;

implementation

const
  cDTD = '<!DOCTYPE root ['+
         '<!ELEMENT root (e*)>'+
         '<!ELEMENT e EMPTY>'+
         '<!ATTLIST e key ID #IMPLIED name CDATA #IMPLIED>'+
         ']>';


procedure TTestXMLIDs.SetUp;

begin
  FErrors:=TStringList.Create;
end;


procedure TTestXMLIDs.TearDown;

begin
  FreeAndNil(FDoc);
  FreeAndNil(FErrors);
end;


procedure TTestXMLIDs.DoError(E: EXMLReadError);

begin
  FErrors.Add(E.Message);
end;


procedure TTestXMLIDs.Parse(const aXML: String; aValidate: Boolean);

var
  lParser: TDOMParser;
  lSource: TXMLInputSource;

begin
  FreeAndNil(FDoc);
  FErrors.Clear;
  lParser:=TDOMParser.Create;
  lSource:=TXMLInputSource.Create(aXML);
  try
    lParser.Options.Validate:=aValidate;
    lParser.Options.Namespaces:=True;
    lParser.OnError:=@DoError;
    lParser.Parse(lSource,FDoc);
  finally
    lSource.Free;
    lParser.Free;
  end;
end;


procedure TTestXMLIDs.AssertID(const aID, aElement: String);

var
  lEl: TDOMElement;

begin
  lEl:=FDoc.GetElementById(aID);
  AssertNotNull('GetElementById('''+aID+''') finds an element',lEl);
  AssertEquals('Element found for ID '''+aID+'''',aElement,String(lEl.GetAttribute('name')));
end;


procedure TTestXMLIDs.TestDTDIDNotValidating;

begin
  Parse(cDTD+'<root><e key="k1" name="first"/><e key="k2" name="second"/></root>');
  AssertID('k1','first');
  AssertID('k2','second');
  AssertNull('Unknown ID',FDoc.GetElementById('k3'));
end;


procedure TTestXMLIDs.TestDTDIDValidating;

begin
  Parse(cDTD+'<root><e key="k1" name="first"/><e key="k2" name="second"/></root>',True);
  AssertEquals('No validation errors','',FErrors.Text);
  AssertID('k1','first');
  AssertID('k2','second');
end;


procedure TTestXMLIDs.TestDTDIDNormalized;

begin
  Parse(cDTD+'<root><e key="  k1  " name="first"/></root>');
  AssertID('k1','first');
end;


procedure TTestXMLIDs.TestDuplicateIDNotValidating;

begin
  Parse(cDTD+'<root><e key="k1" name="first"/><e key="k1" name="second"/></root>');
  AssertEquals('No error for a duplicate ID without validation','',FErrors.Text);
  AssertID('k1','first');
end;


procedure TTestXMLIDs.TestDuplicateIDValidating;

begin
  Parse(cDTD+'<root><e key="k1" name="first"/><e key="k1" name="second"/></root>',True);
  AssertEquals('One error for a duplicate ID when validating',1,FErrors.Count);
  AssertTrue('Error reports the duplicate ID: '+FErrors.Text,Pos('not unique',FErrors[0])>0);
end;


procedure TTestXMLIDs.TestXmlIdWithoutDTD;

begin
  Parse('<root><e xml:id="x1" name="first"/><e xml:id="x2" name="second"/></root>');
  AssertID('x1','first');
  AssertID('x2','second');
end;


procedure TTestXMLIDs.TestXmlIdNormalized;

var
  lEl: TDOMElement;

begin
  Parse('<root><e xml:id=" x1  " name="first"/></root>');
  AssertID('x1','first');
  lEl:=FDoc.GetElementById('x1');
  AssertEquals('xml:id value is normalized','x1',String(lEl.GetAttribute('xml:id')));
end;


procedure TTestXMLIDs.TestXmlIdWithoutNamespaces;

var
  lParser: TDOMParser;
  lSource: TXMLInputSource;

begin
  lParser:=TDOMParser.Create;
  lSource:=TXMLInputSource.Create('<root><e xml:id="x1" name="first"/></root>');
  try
    lParser.Options.Namespaces:=False;
    lParser.Parse(lSource,FDoc);
  finally
    lSource.Free;
    lParser.Free;
  end;
  AssertID('x1','first');
end;


procedure TTestXMLIDs.TestPlainIdIsNotAnID;

begin
  Parse('<root><e id="p1" name="first"/></root>');
  AssertNull('An attribute named id without a DTD is not an ID',FDoc.GetElementById('p1'));
end;


procedure TTestXMLIDs.TestCDATAAttributeIsNotAnID;

begin
  Parse(cDTD+'<root><e name="first"/></root>');
  AssertNull('A CDATA attribute is not an ID',FDoc.GetElementById('first'));
end;


initialization
  RegisterTest(TTestXMLIDs);
end.
