{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 Sebastian Guenther (sguenther@gmx.de)

    Implementation of DOM level 1 interfaces

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 more or less DOM level 1 conformant class library for FreePascal
}

{$MODE objfpc}
{$H+}

unit DOM;

interface

uses sysutils, classes;

type

  TDOMImplementation = class;
  TDOMDocumentFragment = class;
  TDOMDocument = class;
  TDOMNode = class;
  TDOMNodeList = class;
  TDOMNamedNodeMap = class;
  TDOMCharacterData = class;
  TDOMAttr = class;
  TDOMElement = class;
  TDOMText = class;
  TDOMComment = class;
  TDOMCDATASection = class;
  TDOMDocumentType = class;
  TDOMNotation = class;
  TDOMEntity = class;
  TDOMEntityReference = class;
  TDOMProcessingInstruction = class;


// -------------------------------------------------------
//   DOMString
// -------------------------------------------------------

  DOMString = String;  // *** should be WideString  /sg


// -------------------------------------------------------
//   DOMException
// -------------------------------------------------------

const
  INDEX_SIZE_ERR              = 1;
  DOMSTRING_SIZE_ERR          = 2;
  HIERARCHY_REQUEST_ERR       = 3;
  WRONG_DOCUMENT_ERR          = 4;
  INVALID_CHARACTER_ERR       = 5;
  NO_DATA_ALLOWED_ERR         = 6;
  NO_MODIFICATION_ALLOWED_ERR = 7;
  NOT_FOUND_ERR               = 8;
  NOT_SUPPORTED_ERR           = 9;
  INUSE_ATTRIBUTE_ERR         = 10;

type
  EDOMError = class(Exception)
  protected
    constructor Create(ACode: Integer; const ASituation: String);
  public
    Code: Integer;
  end;

  EDOMIndexSize = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMHierarchyRequest = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMWrongDocument = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotFound = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNotSupported = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInUseAttribute = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;


// -------------------------------------------------------
//   Node
// -------------------------------------------------------

const
  ELEMENT_NODE = 1;
  ATTRIBUTE_NODE = 2;
  TEXT_NODE = 3;
  CDATA_SECTION_NODE = 4;
  ENTITY_REFERENCE_NODE = 5;
  ENTITY_NODE = 6;
  PROCESSING_INSTRUCTION_NODE = 7;
  COMMENT_NODE = 8;
  DOCUMENT_NODE = 9;
  DOCUMENT_TYPE_NODE = 10;
  DOCUMENT_FRAGMENT_NODE = 11;
  NOTATION_NODE = 12;

type

  TRefClass = class
  protected
    RefCounter: LongInt;
  public
    constructor Create;
    function AddRef: LongInt; virtual;
    function Release: LongInt; virtual;
  end;

  TDOMNode = class
  protected
    FNodeName, FNodeValue: DOMString;
    FNodeType: Integer;
    FParentNode: TDOMNode;
    FPreviousSibling, FNextSibling: TDOMNode;
    FOwnerDocument: TDOMDocument;

    function  FGetNodeValue: DOMString; virtual;
    procedure FSetNodeValue(AValue: DOMString); virtual;
    function  FGetFirstChild: TDOMNode; virtual;
    function  FGetLastChild: TDOMNode; virtual;
    function  FGetAttributes: TDOMNamedNodeMap; virtual;

    constructor Create(AOwner: TDOMDocument);
  public
    // Free NodeList with TDOMNodeList.Release!
    function GetChildNodes: TDOMNodeList; virtual;

    property NodeName: DOMString read FNodeName;
    property NodeValue: DOMString read FGetNodeValue write FSetNodeValue;
    property NodeType: Integer read FNodeType;
    property ParentNode: TDOMNode read FParentNode;
    property FirstChild: TDOMNode read FGetFirstChild;
    property LastChild: TDOMNode read FGetLastChild;
    property ChildNodes: TDOMNodeList read GetChildNodes;
    property PreviousSibling: TDOMNode read FPreviousSibling;
    property NextSibling: TDOMNode read FNextSibling;
    property Attributes: TDOMNamedNodeMap read FGetAttributes;
    property OwnerDocument: TDOMDocument read FOwnerDocument;

    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; virtual;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; virtual;
    function AppendChild(NewChild: TDOMNode): TDOMNode; virtual;
    function HasChildNodes: Boolean; virtual;
    function CloneNode(deep: Boolean): TDOMNode;

    // Extensions to DOM interface:
    function FindNode(const ANodeName: DOMString): TDOMNode;
  end;


  { The following class is an implementation specific extension, it is just an
    extended implementation of TDOMNode, the generic DOM::Node interface
    implementation. (Its main purpose is to save memory in a big node tree) }

  TDOMNode_WithChildren = class(TDOMNode)
  protected
    FFirstChild, FLastChild: TDOMNode;
    function FGetFirstChild: TDOMNode; override;
    function FGetLastChild: TDOMNode; override;
  public
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; override;
    function AppendChild(NewChild: TDOMNode): TDOMNode; override;
    function HasChildNodes: Boolean; override;
  end;

// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

  TDOMNodeList = class(TRefClass)
  protected
    node: TDOMNode;
    filter: DOMString;
    UseFilter: Boolean;
    constructor Create(ANode: TDOMNode; AFilter: DOMString);
    function FGetCount: LongWord;
    function FGetItem(index: LongWord): TDOMNode;
  public
    property Item[index: LongWord]: TDOMNode read FGetItem;
    property Count: LongWord read FGetCount;
  end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

  TDOMNamedNodeMap = class(TList)
  protected
    OwnerDocument: TDOMDocument;
    function  FGetItem(index: LongWord): TDOMNode;
    procedure FSetItem(index: LongWord; AItem: TDOMNode);
    function  FGetLength: LongWord;

    constructor Create(AOwner: TDOMDocument);
  public
    function GetNamedItem(const name: DOMString): TDOMNode;
    function SetNamedItem(arg: TDOMNode): TDOMNode;
    function RemoveNamedItem(const name: DOMString): TDOMNode;
    property Item[index: LongWord]: TDOMNode read FGetItem write FSetItem;
    property Length: LongWord read FGetLength;
  end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

  TDOMCharacterData = class(TDOMNode)
  protected
    function FGetLength: LongWord;
  public
    property Data: DOMString read FNodeValue;
    property Length: LongWord read FGetLength;
    function  SubstringData(offset, count: LongWord): DOMString;
    procedure AppendData(const arg: DOMString);
    procedure InsertData(offset: LongWord; const arg: DOMString);
    procedure DeleteData(offset, count: LongWord);
    procedure ReplaceData(offset, count: LongWord; const arg: DOMString);
  end;


// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

  TDOMImplementation = class
  public
    function HasFeature(const feature, version: DOMString): Boolean;
  end;


// -------------------------------------------------------
//   DocumentFragment
// -------------------------------------------------------

  TDOMDocumentFragment = class(TDOMNode_WithChildren)
  protected
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

  TDOMDocument = class(TDOMNode_WithChildren)
  protected
    FDocType: TDOMDocumentType;
    FImplementation: TDOMImplementation;
    FDocumentElement: TDOMElement;
  public
    property DocType: TDOMDocumentType read FDocType;
    property Impl: TDOMImplementation read FImplementation;
    property DocumentElement: TDOMElement read FDocumentElement;

    function CreateElement(const tagName: DOMString): TDOMElement; virtual;
    function CreateDocumentFragment: TDOMDocumentFragment;
    function CreateTextNode(const data: DOMString): TDOMText;
    function CreateComment(const data: DOMString): TDOMComment;
    function CreateCDATASection(const data: DOMString): TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; virtual;
    function CreateAttribute(const name: DOMString): TDOMAttr; virtual;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference;
      virtual;
    // Free NodeList with TDOMNodeList.Release!
    function GetElementsByTagName(const tagname: DOMString): TDOMNodeList;

    // Extensions to DOM interface:
    constructor Create; virtual;
    function CreateEntity(const data: DOMString): TDOMEntity;
    procedure SetDocumentElement(ADocumentElement: TDOMElement);
  end;

  TXMLDocument = class(TDOMDocument)
  public
    function CreateCDATASection(const data: DOMString): TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; virtual;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference;
      virtual;

    // Extensions to DOM interface:
    XMLVersion, Encoding: String;
  end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

  TDOMAttr = class(TDOMNode_WithChildren)
  protected
    FSpecified: Boolean;
    AttrOwner: TDOMNamedNodeMap;
    function  FGetNodeValue: DOMString; override;
    procedure FSetNodeValue(AValue: DOMString); override;

    constructor Create(AOwner: TDOMDocument);
  public
    property Name: DOMString read FNodeName;
    property Specified: Boolean read FSpecified;
    property Value: DOMString read FNodeValue write FSetNodeValue;
  end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

  TDOMElement = class(TDOMNode_WithChildren)
  protected
    FAttributes: TDOMNamedNodeMap;
    function FGetAttributes: TDOMNamedNodeMap; override;

    constructor Create(AOwner: TDOMDocument); virtual;
  public
    property  TagName: DOMString read FNodeName;
    function  GetAttribute(const name: DOMString): DOMString;
    procedure SetAttribute(const name, value: DOMString);
    procedure RemoveAttribute(const name: DOMString);
    function  GetAttributeNode(const name: DOMString): TDOMAttr;
    procedure SetAttributeNode(NewAttr: TDOMAttr);
    function  RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
    // Free NodeList with TDOMNodeList.Release!
    function  GetElementsByTagName(const name: DOMString): TDOMNodeList;
    procedure Normalize;
  end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

  TDOMText = class(TDOMCharacterData)
  protected
    constructor Create(AOwner: TDOMDocument);
  public
    function SplitText(offset: LongWord): TDOMText;
  end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

  TDOMComment = class(TDOMCharacterData)
  protected
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

  TDOMCDATASection = class(TDOMText)
  protected
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

  TDOMDocumentType = class(TDOMNode)
  protected
    FEntities, FNotations: TDOMNamedNodeMap;

    constructor Create(AOwner: TDOMDocument);
  public
    property Name: DOMString read FNodeName;
    property Entities: TDOMNamedNodeMap read FEntities;
    property Notations: TDOMNamedNodeMap read FEntities;
  end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

  TDOMNotation = class(TDOMNode)
  protected
    FPublicID, FSystemID: DOMString;

    constructor Create(AOwner: TDOMDocument);
  public
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
  end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

  TDOMEntity = class(TDOMNode_WithChildren)
  protected
    FPublicID, FSystemID, FNotationName: DOMString;

    constructor Create(AOwner: TDOMDocument);
  public
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property NotationName: DOMString read FNotationName;
  end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

  TDOMEntityReference = class(TDOMNode_WithChildren)
  protected
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

  TDOMProcessingInstruction = class(TDOMNode)
  protected
    constructor Create(AOwner: TDOMDocument);
  public
    property Target: DOMString read FNodeName;
    property Data: DOMString read FNodeValue;
  end;




// =======================================================
// =======================================================

implementation


constructor TRefClass.Create;
begin
  inherited Create;
  RefCounter := 1;
end;

function TRefClass.AddRef: LongInt;
begin
  Inc(RefCounter);
  Result := RefCounter;
end;

function TRefClass.Release: LongInt;
begin
  Dec(RefCounter);
  Result := RefCounter;
  if RefCounter <= 0 then Free;
end;


// -------------------------------------------------------
//   DOM Exception
// -------------------------------------------------------

constructor EDOMError.Create(ACode: Integer; const ASituation: String);
begin
  Code := ACode;
  inherited Create(Self.ClassName + ' in ' + ASituation);
end;

constructor EDOMIndexSize.Create(const ASituation: String);    // 1
begin
  inherited Create(INDEX_SIZE_ERR, ASituation);
end;

constructor EDOMHierarchyRequest.Create(const ASituation: String);    // 3
begin
  inherited Create(HIERARCHY_REQUEST_ERR, ASituation);
end;

constructor EDOMWrongDocument.Create(const ASituation: String);    // 4
begin
  inherited Create(WRONG_DOCUMENT_ERR, ASituation);
end;

constructor EDOMNotFound.Create(const ASituation: String);    // 8
begin
  inherited Create(NOT_FOUND_ERR, ASituation);
end;

constructor EDOMNotSupported.Create(const ASituation: String);    // 9
begin
  inherited Create(NOT_SUPPORTED_ERR, ASituation);
end;

constructor EDOMInUseAttribute.Create(const ASituation: String);    // 10
begin
  inherited Create(INUSE_ATTRIBUTE_ERR, ASituation);
end;


// -------------------------------------------------------
//   Node
// -------------------------------------------------------

constructor TDOMNode.Create(AOwner: TDOMDocument);
begin
  FOwnerDocument := AOwner;
  inherited Create;
end;

function TDOMNode.FGetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMNode.FSetNodeValue(AValue: DOMString);
begin
  FNodeValue := AValue;
end;

function TDOMNode.GetChildNodes: TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, '*');
end;

function TDOMNode.FGetFirstChild: TDOMNode; begin Result := nil end;
function TDOMNode.FGetLastChild: TDOMNode; begin Result := nil end;
function TDOMNode.FGetAttributes: TDOMNamedNodeMap; begin Result := nil end;

function TDOMNode.InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
end;

function TDOMNode.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.ReplaceChild');
end;

function TDOMNode.RemoveChild(OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.RemoveChild');
end;

function TDOMNode.AppendChild(NewChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.AppendChild');
end;

function TDOMNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

function TDOMNode.CloneNode(deep: Boolean): TDOMNode;
begin
  Result := nil;
end;

function TDOMNode.FindNode(const ANodeName: DOMString): TDOMNode;
var
  child: TDOMNode;
begin
  child := FirstChild;
  while child <> nil do begin
    if child.NodeName = ANodeName then begin
      Result := child;
      exit;
    end;
    child := child.NextSibling;
  end;
  Result := nil;
end;


function TDOMNode_WithChildren.FGetFirstChild: TDOMNode;
begin
  Result := FFirstChild;
end;

function TDOMNode_WithChildren.FGetLastChild: TDOMNode;
begin
  Result := FLastChild;
end;

function TDOMNode_WithChildren.InsertBefore(NewChild, RefChild: TDOMNode):
  TDOMNode;
var
  i: Integer;
begin
  if RefChild = nil then begin
    AppendChild(NewChild);
    exit(NewChild);
  end;

  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.InsertBefore');

  if RefChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');

  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
    raise EDOMNotSupported.Create('NodeWC.InsertBefore for DocumentFragment');

  NewChild.FNextSibling := RefChild;
  if RefChild = FFirstChild then
    FFirstChild := NewChild
  else
    RefChild.FPreviousSibling.FNextSibling := NewChild;

  RefChild.FPreviousSibling := NewChild;

  Result := NewChild;
end;

function TDOMNode_WithChildren.ReplaceChild(NewChild, OldChild: TDOMNode):
  TDOMNode;
begin
  InsertBefore(NewChild, OldChild);
  RemoveChild(OldChild);
  Result := NewChild;
end;

function TDOMNode_WithChildren.RemoveChild(OldChild: TDOMNode):
  TDOMNode;
begin
  if OldChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.RemoveChild');

  if OldChild = FFirstChild then
    FFirstChild := nil
  else
    OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;

  if OldChild = FLastChild then
    FLastChild := nil
  else
    OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;
end;

function TDOMNode_WithChildren.AppendChild(NewChild: TDOMNode): TDOMNode;
var
  parent: TDOMNode;
begin
  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.AppendChild');

  parent := Self;
  while parent <> nil do begin
    if parent = NewChild then
      raise EDOMHierarchyRequest.Create('NodeWC.AppendChild (cycle in tree)');
    parent := parent.ParentNode;
  end;

  if NewChild.FParentNode = Self then
    RemoveChild(NewChild);

  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then begin
    raise EDOMNotSupported.Create('NodeWC.AppendChild for DocumentFragments');
  end else begin
    if FLastChild = nil then
      FFirstChild := NewChild
    else begin
      FLastChild.FNextSibling := NewChild;
      NewChild.FPreviousSibling := FLastChild;
    end;
    FLastChild := NewChild;
    NewChild.FParentNode := Self;
  end;
  Result := NewChild;
end;


function TDOMNode_WithChildren.HasChildNodes: Boolean;
begin
  Result := FFirstChild <> nil;
end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------


constructor TDOMNodeList.Create(ANode: TDOMNode; AFilter: DOMString);
begin
  inherited Create;
  node := ANode;
  filter := AFilter;
  UseFilter := filter <> '*';
end;

function TDOMNodeList.FGetCount: LongWord;
var
  child: TDOMNode;
begin
  Result := 0;
  child := node.FirstChild;
  while child <> nil do begin
    if (not UseFilter) or (child.NodeName = filter) then
      Inc(Result);
    child := child.NextSibling;
  end;
end;

function TDOMNodeList.FGetItem(index: LongWord): TDOMNode;
var
  child: TDOMNode;
begin
  Result := nil;
  if index < 0 then exit;
  child := node.FirstChild;
  while child <> nil do begin
    if index = 0 then begin
      Result := child;
      break;
    end;
    if (not UseFilter) or (child.NodeName = filter) then
      Dec(index);
    child := child.NextSibling;
  end;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner: TDOMDocument);
begin
  inherited Create;
  OwnerDocument := AOwner;
end;

function TDOMNamedNodeMap.FGetItem(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(Items[index]);
end;

procedure TDOMNamedNodeMap.FSetItem(index: LongWord; AItem: TDOMNode);
begin
  Items[index] := AItem;
end;

function TDOMNamedNodeMap.FGetLength: LongWord;
begin
  Result := LongWord(Count);
end;

function TDOMNamedNodeMap.GetNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Item[i].NodeName = name then
      exit(Item[i]);
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItem(arg: TDOMNode): TDOMNode;
var
  i: Integer;
begin
  if arg.FOwnerDocument <> OwnerDocument then
    raise EDOMWrongDocument.Create('NamedNodeMap.SetNamedItem');

  if arg.NodeType = ATTRIBUTE_NODE then begin
    if TDOMAttr(arg).AttrOwner <> nil then
      raise EDOMInUseAttribute.Create('NamedNodeMap.SetNamedItem');
    TDOMAttr(arg).AttrOwner := Self;
  end;

  for i := 0 to Count - 1 do
    if Item[i].NodeName = arg.NodeName then begin
      Result := Item[i];
      Item[i] := arg;
      exit;
    end;
  Add(arg);
  Result := nil;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Item[i].NodeName = name then begin
      Result := Item[i];
      Result.FParentNode := nil;
      exit;
    end;
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

function TDOMCharacterData.FGetLength: LongWord;
begin
  Result := system.Length(FNodeValue);
end;

function TDOMCharacterData.SubstringData(offset, count: LongWord): DOMString;
begin
  if (offset < 0) or (offset > Length) or (count < 0) then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  Result := Copy(FNodeValue, offset + 1, count);
end;

procedure TDOMCharacterData.AppendData(const arg: DOMString);
begin
  FNodeValue := FNodeValue + arg;
end;

procedure TDOMCharacterData.InsertData(offset: LongWord; const arg: DOMString);
begin
  if (offset < 0) or (offset > Length) then
    raise EDOMIndexSize.Create('CharacterData.InsertData');

  FNodeValue := Copy(FNodeValue, 1, offset) + arg +
    Copy(FNodeValue, offset + 1, Length);
end;

procedure TDOMCharacterData.DeleteData(offset, count: LongWord);
begin
  if (offset < 0) or (offset > Length) or (count < 0) then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');

  FNodeValue := Copy(FNodeValue, 1, offset) +
    Copy(FNodeValue, offset + count + 1, Length);
end;

procedure TDOMCharacterData.ReplaceData(offset, count: LongWord; const arg: DOMString);
begin
  DeleteData(offset, count);
  InsertData(offset, arg);
end;


// -------------------------------------------------------
//   DocumentFragmet
// -------------------------------------------------------

constructor TDOMDocumentFragment.Create(AOwner: TDOMDocument);
begin
  FNodeType := DOCUMENT_FRAGMENT_NODE;
  FNodeName := '#document-fragment';
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

function TDOMImplementation.HasFeature(const feature, version: DOMString):
  Boolean;
begin
  Result := False;
end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

constructor TDOMDocument.Create;
begin
  FNodeType := DOCUMENT_NODE;
  FNodeName := '#document';
  inherited Create(nil);
  FOwnerDocument := Self;
end;

procedure TDOMDocument.SetDocumentElement(ADocumentElement: TDOMElement);
begin
  FDocumentElement := ADocumentElement;
end;

function TDOMDocument.CreateElement(const tagName: DOMString): TDOMElement;
begin
  Result := TDOMElement.Create(Self);
  Result.FNodeName := tagName;
end;

function TDOMDocument.CreateDocumentFragment: TDOMDocumentFragment;
begin
  Result := TDOMDocumentFragment.Create(Self);
end;

function TDOMDocument.CreateTextNode(const data: DOMString): TDOMText;
begin
  Result := TDOMText.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateComment(const data: DOMString): TDOMComment;
begin
  Result := TDOMComment.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateCDATASection');
end;

function TDOMDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateProcessingInstruction');
end;

function TDOMDocument.CreateAttribute(const name: DOMString): TDOMAttr;
begin
  Result := TDOMAttr.Create(Self);
  Result.FNodeName := name;
end;

function TDOMDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
end;

function TDOMDocument.CreateEntity(const data: DOMString): TDOMEntity;
begin
  Result := TDOMEntity.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.GetElementsByTagName(const tagname: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, tagname);
end;


function TXMLDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  Result := TDOMCDATASection.Create(Self);
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  Result := TDOMProcessingInstruction.Create(Self);
  Result.FNodeName := target;
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  Result := TDOMEntityReference.Create(Self);
  Result.FNodeName := name;
end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

constructor TDOMAttr.Create(AOwner: TDOMDocument);
begin
  FNodeType := ATTRIBUTE_NODE;
  inherited Create(AOwner);
end;

function TDOMAttr.FGetNodeValue: DOMString;
var
  child: TDOMNode;
begin
  if FFirstChild = nil then
    Result := ''
  else begin
    Result := '';
    child := FFirstChild;
    while child <> nil do begin
      if child.NodeType = ENTITY_REFERENCE_NODE then
        Result := Result + '&' + child.NodeName + ';'
      else
        Result := Result + child.NodeValue;
      child := child.NextSibling;
    end;
  end;
end;

procedure TDOMAttr.FSetNodeValue(AValue: DOMString);
var
  tn: TDOMText;
begin
  FSpecified := True;
  tn := TDOMText.Create(FOwnerDocument);
  tn.FNodeValue := AValue;
  if FFirstChild <> nil then
    ReplaceChild(tn, FFirstChild)
  else
    AppendChild(tn);
end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

constructor TDOMElement.Create(AOwner: TDOMDocument);
begin
  FNodeType := ELEMENT_NODE;
  inherited Create(AOwner);
  FAttributes := TDOMNamedNodeMap.Create(AOwner);
end;

function TDOMElement.FGetAttributes: TDOMNamedNodeMap;
begin
  Result := FAttributes;
end;

function TDOMElement.GetAttribute(const name: DOMString): DOMString;
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes.Item[i].NodeName = name then begin
      Result := FAttributes.Item[i].NodeValue;
      exit;
    end;
  Result := '';
end;

procedure TDOMElement.SetAttribute(const name, value: DOMString);
var
  i: Integer;
  attr: TDOMAttr;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes.Item[i].NodeName = name then begin
      FAttributes.Item[i].NodeValue := value;
      exit;
    end;
  attr := TDOMAttr.Create(FOwnerDocument);
  attr.FNodeName := name;
  attr.NodeValue := value;
  FAttributes.Add(attr);
end;

procedure TDOMElement.RemoveAttribute(const name: DOMString);
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes.Item[i].NodeName = name then begin
      FAttributes.Delete(i);
      FAttributes.Item[i].Free;
      exit;
    end;
end;

function TDOMElement.GetAttributeNode(const name: DOMString): TDOMAttr;
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes.Item[i].NodeName = name then
      exit(TDOMAttr(FAttributes.Item[i]));
  Result := nil;
end;

procedure TDOMElement.SetAttributeNode(NewAttr: TDOMAttr);
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes.Item[i].NodeName = NewAttr.NodeName then begin
      FAttributes.Item[i].Free;
      FAttributes.Item[i] := NewAttr;
      exit;
    end;
end;

function TDOMElement.RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
var
  i: Integer;
  node: TDOMNode;
begin
  for i := 0 to FAttributes.Count - 1 do begin
    node := FAttributes.Item[i];
    if node = OldAttr then begin
      FAttributes.Delete(i);
      exit(TDOMAttr(node));
    end;
  end;
end;

function TDOMElement.GetElementsByTagName(const name: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, name);
end;

procedure TDOMElement.Normalize;
begin
end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

constructor TDOMText.Create(AOwner: TDOMDocument);
begin
  FNodeType := TEXT_NODE;
  FNodeName := '#text';
  inherited Create(AOwner);
end;

function TDOMText.SplitText(offset: LongWord): TDOMText;
var
  nt: TDOMText;
begin
  if offset > Length then
    raise EDOMIndexSize.Create('Text.SplitText');

  nt := TDOMText.Create(FOwnerDocument);
  nt.FNodeValue := Copy(FNodeValue, offset + 1, Length);
  FNodeValue := Copy(FNodeValue, 1, offset);
  FParentNode.InsertBefore(nt, FNextSibling);
end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

constructor TDOMComment.Create(AOwner: TDOMDocument);
begin
  FNodeType := COMMENT_NODE;
  FNodeName := '#comment';
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

constructor TDOMCDATASection.Create(AOwner: TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeType := CDATA_SECTION_NODE;
  FNodeName := '#cdata-section';
end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

constructor TDOMDocumentType.Create(AOwner: TDOMDocument);
begin
  FNodeType := DOCUMENT_TYPE_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

constructor TDOMNotation.Create(AOwner: TDOMDocument);
begin
  FNodeType := NOTATION_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

constructor TDOMEntity.Create(AOwner: TDOMDocument);
begin
  FNodeType := ENTITY_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

constructor TDOMEntityReference.Create(AOwner: TDOMDocument);
begin
  FNodeType := ENTITY_REFERENCE_NODE;
  inherited Create(AOwner);
end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

constructor TDOMProcessingInstruction.Create(AOwner: TDOMDocument);
begin
  FNodeType := PROCESSING_INSTRUCTION_NODE;
  inherited Create(AOwner);
end;


end.


{
  $Log$
  Revision 1.9  2000-01-06 23:55:22  peter
    * childnodes property as that is used instead of getchildnodes
      in the apps

  Revision 1.8  2000/01/06 01:20:36  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:11  peter
    * moved to packages dir

  Revision 1.6  1999/12/05 22:00:10  sg
  * Bug workaround for problem with "exit(<some string type>)"

  Revision 1.5  1999/07/12 12:19:49  michael
  + More fixes from Sebastian Guenther

  Revision 1.4  1999/07/11 20:20:11  michael
  + Fixes from Sebastian Guenther

  Revision 1.3  1999/07/10 21:48:26  michael
  + Made domelement constructor virtual, needs overriding in thtmlelement

  Revision 1.2  1999/07/09 21:05:49  michael
  + fixes from Guenther Sebastian

  Revision 1.1  1999/07/09 08:35:09  michael
  + Initial implementation by Sebastian Guenther

}
