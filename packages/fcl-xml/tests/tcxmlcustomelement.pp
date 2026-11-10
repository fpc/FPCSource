{
    This file is part of the Free Component Library
    Tests for TDOMElement descendants created, cloned and imported by a document (issue #25114).
    See the file COPYING.FPC, included in this distribution, for details about the copyright.
 **********************************************************************}
unit tcxmlcustomelement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DOM;

type

  { TCustomElement }

  TCustomElement = class(TDOMElement)
  private
    FTag: Integer;
    FNote: String;
    FPadding: array[0..63] of Byte;
  protected
    procedure CopyFrom(aSource: TDOMElement); override;
  public
    destructor Destroy; override;
    // Integer value copied by CloneNode
    property Tag: Integer read FTag write FTag;
    // String value copied by CloneNode
    property Note: String read FNote write FNote;
  end;

  { TLargerElement }

  TLargerElement = class(TCustomElement)
  private
    FMore: array[0..255] of Byte;
  end;

  { TTestCustomElement }

  TTestCustomElement = class(TTestCase)
  private
    FDoc: TXMLDocument;
    FOther: TXMLDocument;
    function NewCustom(const aName: String; aTag: Integer): TCustomElement;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultClass;
    procedure TestCreateElementWithClass;
    procedure TestCreateElementNSWithClass;
    procedure TestInvalidName;
    procedure TestCloneShallow;
    procedure TestCloneDeep;
    procedure TestCloneNS;
    procedure TestImportNode;
    procedure TestImportNodeNS;
    procedure TestFreeUnattached;
    procedure TestUnattachedReleasedWithDocument;
    procedure TestAttachedReleasedWithDocument;
    procedure TestTwoLargeSizes;
  end;

implementation

var
  DestroyCount: Integer;


procedure TCustomElement.CopyFrom(aSource: TDOMElement);

begin
  inherited CopyFrom(aSource);
  if aSource is TCustomElement then
  begin
    FTag:=TCustomElement(aSource).FTag;
    FNote:=TCustomElement(aSource).FNote;
  end;
end;


destructor TCustomElement.Destroy;

begin
  Inc(DestroyCount);
  inherited Destroy;
end;


procedure TTestCustomElement.SetUp;

begin
  DestroyCount:=0;
  FDoc:=TXMLDocument.Create;
  FOther:=TXMLDocument.Create;
end;


procedure TTestCustomElement.TearDown;

begin
  FreeAndNil(FOther);
  FreeAndNil(FDoc);
end;


function TTestCustomElement.NewCustom(const aName: String; aTag: Integer): TCustomElement;

begin
  Result:=TCustomElement(FDoc.CreateElement(aName,TCustomElement));
  Result.Tag:=aTag;
  Result.Note:='note '+IntToStr(aTag);
end;


procedure TTestCustomElement.TestDefaultClass;

begin
  AssertEquals('One-argument CreateElement class',TDOMElement,FDoc.CreateElement('a').ClassType);
end;


procedure TTestCustomElement.TestCreateElementWithClass;

var
  lEl: TCustomElement;

begin
  lEl:=NewCustom('a',1);
  AssertEquals('Element class',TCustomElement,lEl.ClassType);
  AssertEquals('Tag name','a',String(lEl.TagName));
  AssertSame('Owner document',FDoc,lEl.OwnerDocument);
  AssertEquals('Field value',1,lEl.Tag);
  AssertEquals('Managed field value','note 1',lEl.Note);
end;


procedure TTestCustomElement.TestCreateElementNSWithClass;

var
  lEl: TDOMElement;

begin
  lEl:=FDoc.CreateElementNS('urn:x','p:a',TCustomElement);
  AssertEquals('Element class',TCustomElement,lEl.ClassType);
  AssertEquals('Namespace','urn:x',String(lEl.NamespaceURI));
  AssertEquals('Local name','a',String(lEl.LocalName));
end;


procedure TTestCustomElement.TestInvalidName;

var
  lRaised: Boolean;

begin
  lRaised:=False;
  try
    FDoc.CreateElement('1a',TCustomElement);
  except
    on E: EDOMError do
      lRaised:=E.Code=INVALID_CHARACTER_ERR;
  end;
  AssertTrue('Invalid tag name raises INVALID_CHARACTER_ERR',lRaised);
end;


procedure TTestCustomElement.TestCloneShallow;

var
  lEl: TCustomElement;
  lClone: TDOMNode;

begin
  lEl:=NewCustom('a',7);
  FDoc.AppendChild(lEl);
  lEl.SetAttribute('x','1');
  lEl.AppendChild(FDoc.CreateTextNode('text'));
  lClone:=lEl.CloneNode(False);
  AssertEquals('Clone class',TCustomElement,lClone.ClassType);
  AssertEquals('Clone field',7,TCustomElement(lClone).Tag);
  AssertEquals('Clone managed field','note 7',TCustomElement(lClone).Note);
  AssertEquals('Clone attribute','1',String(TDOMElement(lClone).GetAttribute('x')));
  AssertFalse('Shallow clone has no children',lClone.HasChildNodes);
  lClone.Free;
  AssertEquals('Freeing the clone runs the destructor',1,DestroyCount);
end;


procedure TTestCustomElement.TestCloneDeep;

var
  lRoot, lChild: TCustomElement;
  lClone: TDOMNode;

begin
  lRoot:=NewCustom('root',1);
  lChild:=NewCustom('child',2);
  FDoc.AppendChild(lRoot);
  lRoot.AppendChild(lChild);
  lClone:=lRoot.CloneNode(True);
  try
    AssertEquals('Clone class',TCustomElement,lClone.ClassType);
    AssertNotNull('Deep clone has a child',lClone.FirstChild);
    AssertEquals('Child clone class',TCustomElement,lClone.FirstChild.ClassType);
    AssertEquals('Child clone field',2,TCustomElement(lClone.FirstChild).Tag);
    AssertTrue('Child clone is a new node',lClone.FirstChild<>lChild);
  finally
    lClone.Free;
  end;
end;


procedure TTestCustomElement.TestCloneNS;

var
  lEl: TDOMElement;
  lClone: TDOMNode;

begin
  lEl:=FDoc.CreateElementNS('urn:x','p:a',TCustomElement);
  FDoc.AppendChild(lEl);
  lClone:=lEl.CloneNode(False);
  try
    AssertEquals('Clone class',TCustomElement,lClone.ClassType);
    AssertEquals('Clone namespace','urn:x',String(lClone.NamespaceURI));
  finally
    lClone.Free;
  end;
end;


procedure TTestCustomElement.TestImportNode;

var
  lEl: TCustomElement;
  lImported: TDOMNode;

begin
  lEl:=NewCustom('a',3);
  FDoc.AppendChild(lEl);
  lEl.AppendChild(NewCustom('b',4));
  lImported:=FOther.ImportNode(lEl,True);
  FOther.AppendChild(lImported);
  AssertEquals('Imported class',TCustomElement,lImported.ClassType);
  AssertSame('Imported owner document',FOther,lImported.OwnerDocument);
  AssertEquals('Imported field',3,TCustomElement(lImported).Tag);
  AssertEquals('Imported managed field','note 3',TCustomElement(lImported).Note);
  AssertEquals('Imported child class',TCustomElement,lImported.FirstChild.ClassType);
  AssertEquals('Imported child field',4,TCustomElement(lImported.FirstChild).Tag);
end;


procedure TTestCustomElement.TestImportNodeNS;

var
  lEl: TDOMElement;
  lImported: TDOMNode;

begin
  lEl:=FDoc.CreateElementNS('urn:x','p:a',TCustomElement);
  lImported:=FOther.ImportNode(lEl,False);
  FOther.AppendChild(lImported);
  AssertEquals('Imported class',TCustomElement,lImported.ClassType);
  AssertEquals('Imported namespace','urn:x',String(lImported.NamespaceURI));
end;


procedure TTestCustomElement.TestFreeUnattached;

var
  lEl: TCustomElement;

begin
  lEl:=NewCustom('a',1);
  lEl.Free;
  AssertEquals('Destructor ran once',1,DestroyCount);
  lEl:=NewCustom('b',2);
  AssertEquals('Reused node has the new field value',2,lEl.Tag);
  FreeAndNil(FDoc);
  AssertEquals('Second node destroyed with the document',2,DestroyCount);
end;


procedure TTestCustomElement.TestUnattachedReleasedWithDocument;

begin
  NewCustom('a',1);
  NewCustom('b',2);
  FreeAndNil(FDoc);
  AssertEquals('Unattached nodes destroyed with the document',2,DestroyCount);
end;


procedure TTestCustomElement.TestAttachedReleasedWithDocument;

var
  lRoot: TCustomElement;
  i: Integer;

begin
  lRoot:=NewCustom('root',0);
  FDoc.AppendChild(lRoot);
  for i:=1 to 100 do
    lRoot.AppendChild(NewCustom('e',i));
  FreeAndNil(FDoc);
  AssertEquals('All nodes destroyed with the document',101,DestroyCount);
end;


procedure TTestCustomElement.TestTwoLargeSizes;

var
  lSmall, lLarge: TDOMElement;

begin
  lSmall:=FDoc.CreateElement('a',TCustomElement);
  lLarge:=FDoc.CreateElement('b',TLargerElement);
  FDoc.AppendChild(lSmall);
  lSmall.AppendChild(lLarge);
  AssertEquals('First class',TCustomElement,lSmall.ClassType);
  AssertEquals('Second class',TLargerElement,lLarge.ClassType);
  AssertEquals('Larger clone class',TLargerElement,lLarge.CloneNode(False).ClassType);
  FreeAndNil(FDoc);
  AssertEquals('Nodes of both sizes destroyed with the document',3,DestroyCount);
end;


initialization
  RegisterTest(TTestCustomElement);
end.
