{
    This file is part of the Free Component Library

    Implementation of DOM interfaces
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
    Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru    

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This unit provides classes which implement the interfaces defined in the
  DOM (Document Object Model) specification.
  The current state is:
  DOM Level 1  -  Almost completely implemented
  DOM Level 2  -  Partially implemented

  Specification used for this implementation:

  "Document Object Model (DOM) Level 2 Specification Version 1.0
   W3C Recommendation 11 November, 2000"
   http://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113
}


unit DOM;

{$ifdef fpc}
{$MODE objfpc}{$H+}
{$endif}

interface

{off $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  SysUtils, Classes, AVL_Tree;

// -------------------------------------------------------
//   DOMException
// -------------------------------------------------------

const

  // DOM Level 1 exception codes:

  INDEX_SIZE_ERR              = 1;  // index or size is negative, or greater than the allowed value
  DOMSTRING_SIZE_ERR          = 2;  // Specified range of text does not fit into a DOMString
  HIERARCHY_REQUEST_ERR       = 3;  // node is inserted somewhere it does not belong
  WRONG_DOCUMENT_ERR          = 4;  // node is used in a different document than the one that created it (that does not support it)
  INVALID_CHARACTER_ERR       = 5;  // invalid or illegal character is specified, such as in a name
  NO_DATA_ALLOWED_ERR         = 6;  // data is specified for a node which does not support data
  NO_MODIFICATION_ALLOWED_ERR = 7;  // an attempt is made to modify an object where modifications are not allowed
  NOT_FOUND_ERR               = 8;  // an attempt is made to reference a node in a context where it does not exist
  NOT_SUPPORTED_ERR           = 9;  // implementation does not support the type of object requested
  INUSE_ATTRIBUTE_ERR         = 10;  // an attempt is made to add an attribute that is already in use elsewhere

  // DOM Level 2 exception codes:

  INVALID_STATE_ERR           = 11;  // an attempt is made to use an object that is not, or is no longer, usable
  SYNTAX_ERR                  = 12;  // invalid or illegal string specified
  INVALID_MODIFICATION_ERR    = 13;  // an attempt is made to modify the type of the underlying object
  NAMESPACE_ERR               = 14;  // an attempt is made to create or change an object in a way which is incorrect with regard to namespaces
  INVALID_ACCESS_ERR          = 15;  // parameter or operation is not supported by the underlying object

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

  DOMString = WideString;
  DOMPChar = PWideChar;

  EDOMError = class(Exception)
  public
    Code: Integer;
    constructor Create(ACode: Integer; const ASituation: String);
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

  EDOMInvalidState = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMSyntax = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidModification = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMNamespace = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;

  EDOMInvalidAccess = class(EDOMError)
  public
    constructor Create(const ASituation: String);
  end;


  TRefClass = class
  protected
    RefCounter: LongInt;
  public
    constructor Create;
    function AddRef: LongInt; virtual;
    function Release: LongInt; virtual;
  end;

  // NodeType has been moved from field to a function.
  // This lowers memory usage and also obsoletes most constructors.

  TDOMNode = class
  protected
    FNodeName, FNodeValue: DOMString;
    FParentNode: TDOMNode;
    FPreviousSibling, FNextSibling: TDOMNode;
    FOwnerDocument: TDOMDocument;

    function  GetNodeValue: DOMString; virtual;
    procedure SetNodeValue(const AValue: DOMString); virtual;
    function  GetFirstChild: TDOMNode; virtual;
    function  GetLastChild: TDOMNode; virtual;
    function  GetAttributes: TDOMNamedNodeMap; virtual;
    function GetRevision: Integer;
    function GetNodeType: Integer; virtual; abstract;
  public
    constructor Create(AOwner: TDOMDocument);

    // Free NodeList with TDOMNodeList.Release!
    function GetChildNodes: TDOMNodeList; virtual;  // why virtual?

    property NodeName: DOMString read FNodeName;
    property NodeValue: DOMString read GetNodeValue write SetNodeValue;
    property NodeType: Integer read GetNodeType;
    property ParentNode: TDOMNode read FParentNode;
    property FirstChild: TDOMNode read GetFirstChild;
    property LastChild: TDOMNode read GetLastChild;
    property ChildNodes: TDOMNodeList read GetChildNodes;
    property PreviousSibling: TDOMNode read FPreviousSibling;
    property NextSibling: TDOMNode read FNextSibling;
    property Attributes: TDOMNamedNodeMap read GetAttributes;
    // DOM 2: is now nil for documents and unused DocTypes
    property OwnerDocument: TDOMDocument read FOwnerDocument;

    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; virtual;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; virtual;
    function AppendChild(NewChild: TDOMNode): TDOMNode; virtual;
    function HasChildNodes: Boolean; virtual;
    function CloneNode(deep: Boolean): TDOMNode; overload;

    // DOM level 2
    (*
    function Supports(const Feature, Version: DOMString): Boolean;
    *)
    function HasAttributes: Boolean; virtual;
    procedure Normalize;               // moved from TDOMElement

    (*
    // TODO: What is that Java NULL for strings ???
    // always '' for nodes other than ELEMENT and ATTRIBUTE
    // as well as for nodes created with DOM 1 methods
    property NamespaceURI: DOMString read GetNamespaceURI;

    // Prefix may only be changed if it was specified at creation time.
    property Prefix: DOMString read FPrefix (write SetPrefix?);
    property LocalName: DOMString read FLocalName;
    *)
    // Extensions to DOM interface:
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; virtual;
    function FindNode(const ANodeName: DOMString): TDOMNode; virtual;
  end;


  { The following class is an implementation specific extension, it is just an
    extended implementation of TDOMNode, the generic DOM::Node interface
    implementation. (Its main purpose is to save memory in a big node tree) }

  TDOMNode_WithChildren = class(TDOMNode)
  protected
    FFirstChild, FLastChild: TDOMNode;
    FChildNodeTree: TAVLTree;
    function GetFirstChild: TDOMNode; override;
    function GetLastChild: TDOMNode; override;
    procedure CloneChildren(ACopy: TDOMNode; ACloneOwner: TDOMDocument);
    procedure AddToChildNodeTree(NewNode: TDOMNode);
    procedure RemoveFromChildNodeTree(OldNode: TDOMNode);
  public
    destructor Destroy; override;
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    function RemoveChild(OldChild: TDOMNode): TDOMNode; override;
    function AppendChild(NewChild: TDOMNode): TDOMNode; override;
    function HasChildNodes: Boolean; override;
    function FindNode(const ANodeName: DOMString): TDOMNode; override;
  end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

  TDOMNodeList = class(TRefClass)
  protected
    node: TDOMNode;
    filter: DOMString;
    FNamespaceFilter: DOMString;
    UseFilter: Boolean;
    FRevision: Integer;
    FList: TList;
    function GetCount: LongWord;
    function GetItem(index: LongWord): TDOMNode;
    procedure BuildList;
  public
    constructor Create(ANode: TDOMNode); overload;
    constructor Create(ANode: TDOMNode; const AFilter: DOMString); overload;
    constructor Create(ANode: TDOMNode; const nsURI, localName: DOMString); overload;
    destructor Destroy; override;
    property Item[index: LongWord]: TDOMNode read GetItem;
    property Count: LongWord read GetCount;
  end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

  TDOMNamedNodeMap = class(TList)
  protected
    // FIX: track ownership by element, in order to implement DOM2 Attr.OwnerElement
    FOwnerElement: TDOMNode;
    function GetItem(index: LongWord): TDOMNode;
    function GetLength: LongWord;
  public
    // FIX: ownership; see above
    constructor Create(AOwner: TDOMNode);
    destructor Destroy; override;

    function GetNamedItem(const name: DOMString): TDOMNode;
    function SetNamedItem(arg: TDOMNode): TDOMNode;
    function RemoveNamedItem(const name: DOMString): TDOMNode;
    // Introduced in DOM Level 2:
    function getNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
    function setNamedItemNS(arg: TDOMNode): TDOMNode;
    function removeNamedItemNS(const namespaceURI,localName: DOMString): TDOMNode;

    // FIX: made readonly. Reason: Anyone was allowed to insert any node without any checking.  
    property Item[index: LongWord]: TDOMNode read GetItem {write SetItem}; default;
    property Length: LongWord read GetLength;
  end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

  TDOMCharacterData = class(TDOMNode)
  protected
    function  GetLength: LongWord;
  public
    property Data: DOMString read FNodeValue;
    property Length: LongWord read GetLength;
    function SubstringData(offset, count: LongWord): DOMString;
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

    // Introduced in DOM Level 2:

    function CreateDocumentType(const QualifiedName, PublicID,
      SystemID: DOMString): TDOMDocumentType;
    function CreateDocument(const NamespaceURI, QualifiedName: DOMString;
      doctype: TDOMDocumentType): TDOMDocument;
  end;


// -------------------------------------------------------
//   DocumentFragment
// -------------------------------------------------------

  TDOMDocumentFragment = class(TDOMNode_WithChildren)
  protected
    function GetNodeType: Integer; override;
  public
    constructor Create(AOwner: TDOMDocument);
  end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

  TDOMDocument = class(TDOMNode_WithChildren)
  protected
    FRevision: Integer;
    FImplementation: TDOMImplementation;
    function GetDocumentElement: TDOMElement;
    function GetDocType: TDOMDocumentType;
    function GetNodeType: Integer; override;
  public
    property DocType: TDOMDocumentType read GetDocType;
    property Impl: TDOMImplementation read FImplementation;
    property DocumentElement: TDOMElement read GetDocumentElement;

    function CreateElement(const tagName: DOMString): TDOMElement; virtual;
    function CreateElementBuf(Buf: DOMPChar; Length: Integer): TDOMElement;
    function CreateDocumentFragment: TDOMDocumentFragment;
    function CreateTextNode(const data: DOMString): TDOMText;
    function CreateTextNodeBuf(Buf: DOMPChar; Length: Integer): TDOMText;
    function CreateComment(const data: DOMString): TDOMComment;
    function CreateCommentBuf(Buf: DOMPChar; Length: Integer): TDOMComment;
    function CreateCDATASection(const data: DOMString): TDOMCDATASection;
      virtual;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; virtual;
    function CreateAttribute(const name: DOMString): TDOMAttr;
    function CreateAttributeBuf(Buf: DOMPChar; Length: Integer): TDOMAttr;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference;
      virtual;
    // Free NodeList with TDOMNodeList.Release!
    function GetElementsByTagName(const tagname: DOMString): TDOMNodeList;

    // DOM level 2 methods
    function ImportNode(ImportedNode: TDOMNode; Deep: Boolean): TDOMNode;
    function CreateElementNS(const NamespaceURI, QualifiedName: DOMString): TDOMElement;
    function CreateAttributeNS(const NamespaceURI, QualifiedName: DOMString): TDOMAttr;
    function GetElementsByTagNameNS(const namespaceURI, localName: DOMString): TDOMNodeList;
    function GetElementById(const ElementID: DOMString): TDOMElement;
    // Extensions to DOM interface:
    constructor Create;
    function CreateEntity(const data: DOMString): TDOMEntity;
  end;

  TXMLDocument = class(TDOMDocument)
  public
    // These fields are extensions to the DOM interface:
    XMLVersion, Encoding, StylesheetType, StylesheetHRef: DOMString;

    function CreateCDATASection(const data: DOMString): TDOMCDATASection; override;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; override;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference; override;
  end;


// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

  TDOMAttr = class(TDOMNode_WithChildren)
  protected
    FSpecified: Boolean;
    FOwnerElement: TDOMElement;
    function  GetNodeValue: DOMString; override;
    function GetNodeType: Integer; override;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Name: DOMString read FNodeName;
    property Specified: Boolean read FSpecified;
    property Value: DOMString read GetNodeValue write SetNodeValue;
    // Introduced in DOM level 2:
    property OwnerElement: TDOMElement read FOwnerElement;
  end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

  TDOMElement = class(TDOMNode_WithChildren)
  private
    FAttributes: TDOMNamedNodeMap;
  protected
    function GetNodeType: Integer; override;
    function GetAttributes: TDOMNamedNodeMap; override;
  public
    destructor Destroy; override;
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property  TagName: DOMString read FNodeName;
    function  GetAttribute(const name: DOMString): DOMString;
    procedure SetAttribute(const name, value: DOMString);
    procedure RemoveAttribute(const name: DOMString);
    function  GetAttributeNode(const name: DOMString): TDOMAttr;
    // FIX: Changed to a function, as per DOM 2
    function SetAttributeNode(NewAttr: TDOMAttr): TDOMAttr;
    function  RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
    // Free NodeList with TDOMNodeList.Release!
    function  GetElementsByTagName(const name: DOMString): TDOMNodeList;

    // Introduced in DOM Level 2:
    function GetAttributeNS(const namespaceURI, localName: DOMString): DOMString;
    procedure SetAttributeNS(const namespaceURI, qualifiedName, value: DOMString); // raises (DOMException)
    procedure RemoveAttributeNS(const namespaceURI, localName: DOMString);         // raises(DOMException);
    function GetAttributeNodeNS(const namespaceURI, localName: DOMString): TDOMAttr;
    function SetAttributeNodeNS(newAttr: TDOMAttr): TDOMAttr;                      // raises(DOMException);
    function GetElementsByTagNameNS(const namespaceURI, localName: DOMString): TDOMNodeList;
    function hasAttribute(const name: DOMString): Boolean;
    function hasAttributeNS(const namespaceURI, localName: DOMString): Boolean;
    function HasAttributes: Boolean; override;

    property AttribStrings[const Name: DOMString]: DOMString
      read GetAttribute write SetAttribute; default;
  end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

  TDOMText = class(TDOMCharacterData)
  protected
    function GetNodeType: Integer; override;
  public
    constructor Create(AOwner: TDOMDocument);
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    function SplitText(offset: LongWord): TDOMText;
  end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

  TDOMComment = class(TDOMCharacterData)
  protected
    function GetNodeType: Integer; override;
  public
    constructor Create(AOwner: TDOMDocument);
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

  TDOMCDATASection = class(TDOMText)
  protected
    function GetNodeType: Integer; override;
  public
    constructor Create(AOwner: TDOMDocument);
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

  TDOMDocumentType = class(TDOMNode)
  protected
    FPublicID: DOMString;
    FSystemID: DOMString;
    FInternalSubset: DOMString;
    FEntities, FNotations: TDOMNamedNodeMap;
    function GetEntities: TDOMNamedNodeMap;
    function GetNotations: TDOMNamedNodeMap;
    function GetNodeType: Integer; override;
  public
    destructor Destroy; override;
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Name: DOMString read FNodeName;
    property Entities: TDOMNamedNodeMap read GetEntities;
    property Notations: TDOMNamedNodeMap read GetNotations;
  // Introduced in DOM Level 2:
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property InternalSubset: DOMString read FInternalSubset;
  end;


// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

  TDOMNotation = class(TDOMNode)
  protected
    FPublicID, FSystemID: DOMString;
    function GetNodeType: Integer; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
  end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

  TDOMEntity = class(TDOMNode_WithChildren)
  protected
    FPublicID, FSystemID, FNotationName: DOMString;
    function GetNodeType: Integer; override;
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
    function GetNodeType: Integer; override;
  end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

  TDOMProcessingInstruction = class(TDOMNode)
  protected
    function GetNodeType: Integer; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
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

constructor EDOMInvalidState.Create(const ASituation: String);    // 11
begin
  inherited Create(INVALID_STATE_ERR, ASituation);
end;

constructor EDOMSyntax.Create(const ASituation: String);    // 12
begin
  inherited Create(SYNTAX_ERR, ASituation);
end;

constructor EDOMInvalidModification.Create(const ASituation: String);    // 13
begin
  inherited Create(INVALID_MODIFICATION_ERR, ASituation);
end;

constructor EDOMNamespace.Create(const ASituation: String);    // 14
begin
  inherited Create(NAMESPACE_ERR, ASituation);
end;

constructor EDOMInvalidAccess.Create(const ASituation: String);    // 15
begin
  inherited Create(INVALID_ACCESS_ERR, ASituation);
end;


// -------------------------------------------------------
//   Node
// -------------------------------------------------------

constructor TDOMNode.Create(AOwner: TDOMDocument);
begin
  FOwnerDocument := AOwner;
  inherited Create;
end;

function TDOMNode.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMNode.SetNodeValue(const AValue: DOMString);
begin
  FNodeValue := AValue;
end;

function TDOMNode.GetChildNodes: TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self);
end;

function TDOMNode.GetFirstChild: TDOMNode;
begin
  Result := nil;
end;

function TDOMNode.GetLastChild: TDOMNode;
begin
  Result := nil;
end;

function TDOMNode.GetAttributes: TDOMNamedNodeMap;
begin
  Result := nil;
end;

function TDOMNode.InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
  Result:=nil;
end;

function TDOMNode.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.ReplaceChild');
  Result:=nil;
end;

function TDOMNode.RemoveChild(OldChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.RemoveChild');
  Result:=nil;
end;

function TDOMNode.AppendChild(NewChild: TDOMNode): TDOMNode;
begin
  raise EDOMHierarchyRequest.Create('Node.AppendChild');
  Result:=nil;
end;

function TDOMNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

function TDOMNode.CloneNode(deep: Boolean): TDOMNode;
begin
  Result := CloneNode(deep, FOwnerDocument);
end;

function TDOMNode.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  raise EDOMNotSupported.Create('CloneNode not implemented for ' + ClassName);
  Result:=nil;
end;

function TDOMNode.FindNode(const ANodeName: DOMString): TDOMNode;
var
  child: TDOMNode;
begin
  child := FirstChild;
  while Assigned(child) do
  begin
    if child.NodeName = ANodeName then
    begin
      Result := child;
      exit;
    end;
    child := child.NextSibling;
  end;
  Result := nil;
end;

function TDOMNode.GetRevision: Integer;
begin
  Result := FOwnerDocument.FRevision;
end;

function TDOMNode.HasAttributes: Boolean;
begin
  Result := False;
end;

// DONE: moved to TDOMNode and implemented
procedure TDOMNode.Normalize;
var
  Child, tmp: TDOMNode;
  Txt: TDOMText;
begin
  Child := FirstChild;
  Txt := nil;

  while Assigned(Child) do
  begin
    if Child.NodeType = TEXT_NODE then
    begin
      if Assigned(Txt) then
      begin
        tmp := Child.NextSibling;
        Txt.AppendData(Child.nodeValue);
        RemoveChild(Child);
        Child := tmp;
      end
      else
      begin
        Txt := TDOMText(Child);
        Child := Child.NextSibling;
      end
    end
    else
    begin
      Child := Child.NextSibling;
      Txt := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

function CompareDOMStrings(const s1, s2: DOMPChar; l1, l2: integer): integer;
var i: integer;
begin
  Result:=l1-l2;
  i:=0;
  while (i<l1) and (Result=0) do begin
    Result:=ord(s1[i])-ord(s2[i]);
    inc(i);
  end;
end;

function CompareDOMNodeWithDOMNode(Node1, Node2: Pointer): integer;
begin
  Result:=CompareDOMStrings(DOMPChar(TDOMNode(Node1).NodeName),
                            DOMPChar(TDOMNode(Node2).NodeName),
                            length(TDOMNode(Node1).NodeName),
                            length(TDOMNode(Node2).NodeName)
                            );
end;

function CompareDOMStringWithDOMNode(AKey, ANode: Pointer): integer;
begin
  Result:=CompareDOMStrings(DOMPChar(AKey),
                            DOMPChar(TDOMNode(ANode).NodeName),
                            length(DOMString(AKey)),
                            length(TDOMNode(ANode).NodeName)
                            );
end;


function TDOMNode_WithChildren.GetFirstChild: TDOMNode;
begin
  Result := FFirstChild;
end;

function TDOMNode_WithChildren.GetLastChild: TDOMNode;
begin
  Result := FLastChild;
end;

destructor TDOMNode_WithChildren.Destroy;
var
  child, next: TDOMNode;
begin
  if FChildNodeTree<>nil then begin
    FChildNodeTree.Free;
    FChildNodeTree:=nil;
  end;
  
  child := FFirstChild;
  while Assigned(child) do
  begin
    next := child.NextSibling;
    child.Free;
    child := next;
  end;
  inherited Destroy;
end;

function TDOMNode_WithChildren.InsertBefore(NewChild, RefChild: TDOMNode):
  TDOMNode;
var
  Tmp: TDOMNode;
begin
  Result := NewChild;

  if not Assigned(RefChild) then
  begin
    AppendChild(NewChild);
    exit;
  end;

  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.InsertBefore');

  if RefChild.ParentNode <> Self then
    raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  // DONE: Implemented InsertBefore for DocumentFragments (except ChildNodeTree)
  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
  begin
    // Is fragment empty?
    Tmp := NewChild.FirstChild;
    if not Assigned(Tmp) then
      Exit;
    // reparent nodes
    while Assigned(Tmp) do
    begin
      Tmp.FParentNode := Self;
      Tmp := Tmp.NextSibling;
    end;

    if (RefChild = nil) or (RefChild.FPreviousSibling = nil) then
    begin  // insert at the beginning  <- AppendChild ??? ->
      if Assigned(FirstChild) then
        FirstChild.FPreviousSibling := NewChild.LastChild;
      NewChild.LastChild.FNextSibling := FirstChild;
      if not Assigned(FLastChild) then
        FLastChild := NewChild.LastChild;
      FFirstChild := NewChild.FirstChild;
    end
    else  // insert to the middle
    begin
      NewChild.LastChild.FNextSibling := RefChild;
      NewChild.FirstChild.FPreviousSibling := RefChild.FPreviousSibling;
      RefChild.FPreviousSibling.FNextSibling := NewChild.FirstChild;
      RefChild.FPreviousSibling.FPreviousSibling := NewChild.LastChild;
    end;
    // finally, detach nodes from the fragment
    TDOMDocumentFragment(NewChild).FFirstChild := nil;
    TDOMDocumentFragment(NewChild).FLastChild := nil;
    // TODO: ChildNodeTree...
    Exit;
  end;

  NewChild.FNextSibling := RefChild;
  if RefChild = FFirstChild then
    FFirstChild := NewChild
  else
  begin
    RefChild.FPreviousSibling.FNextSibling := NewChild;
    NewChild.FPreviousSibling := RefChild.FPreviousSibling;
  end;

  RefChild.FPreviousSibling := NewChild;
  NewChild.FParentNode := Self;
  AddToChildNodeTree(NewChild);
end;

function TDOMNode_WithChildren.ReplaceChild(NewChild, OldChild: TDOMNode):
  TDOMNode;
begin
//  Inc(FOwnerDocument.FRevision); // invalidate nodelists (will happen anyway)

  RemoveFromChildNodeTree(OldChild);
  InsertBefore(NewChild, OldChild);
  if Assigned(OldChild) then
    RemoveChild(OldChild);
  Result := NewChild;
end;

function TDOMNode_WithChildren.RemoveChild(OldChild: TDOMNode):
  TDOMNode;
begin
  if OldChild.ParentNode <> Self then
    // DOM 2: must raise NOT_FOUND_ERR
    raise EDOMHierarchyRequest.Create('NodeWC.RemoveChild');

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  if OldChild = FFirstChild then
    FFirstChild := FFirstChild.NextSibling
  else
    OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;

  if OldChild = FLastChild then
    FLastChild := FLastChild.FPreviousSibling
  else
    OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;

  RemoveFromChildNodeTree(OldChild);
  // For sanity, make sure removed child does not contain references to nowhere
  {
  OldChild.FPreviousSibling := nil;
  OldChild.FNextSibling := nil;
  OldChild.FParentNode := nil;
  }
  // DOM level 2: Must return removed node
  OldChild.Free;
  Result:=nil;
end;

function TDOMNode_WithChildren.AppendChild(NewChild: TDOMNode): TDOMNode;
var
  Parent: TDOMNode;
  Tmp: TDOMNode;
begin
  if NewChild.FOwnerDocument <> FOwnerDocument then
    raise EDOMWrongDocument.Create('NodeWC.AppendChild');

  Parent := Self;
  while Assigned(Parent) do
  begin
    if Parent = NewChild then
      raise EDOMHierarchyRequest.Create('NodeWC.AppendChild (cycle in tree)');
    Parent := Parent.ParentNode;
  end;

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  if NewChild.FParentNode = Self then
    RemoveChild(NewChild);

  // DONE: supported AppendChild for DocumentFragments (except ChildNodeList)
  if NewChild.NodeType = DOCUMENT_FRAGMENT_NODE then
  begin
    Result := NewChild;
    Tmp := NewChild.FirstChild;
    // Is fragment empty?
    if Tmp = nil then
      Exit;
    // reparent nodes  
    while Assigned(Tmp) do
    begin
      Tmp.FParentNode := Self;
      Tmp := Tmp.NextSibling;
    end;

    if Assigned(FLastChild) then
      FLastChild.FNextSibling := NewChild.FirstChild;
    NewChild.FirstChild.FPreviousSibling := LastChild;
    if not Assigned(FFirstChild) then
      FFirstChild := NewChild.FirstChild;
    FLastChild := NewChild.LastChild;
    // detach nodes from fragment
    TDOMDocumentFragment(NewChild).FFirstChild := nil;
    TDOMDocumentFragment(NewChild).FLastChild := nil;
    // TODO: ChildNodeList...
    Exit;
  end
  else
  begin
    if Assigned(FFirstChild) then
    begin
      FLastChild.FNextSibling := NewChild;
      NewChild.FPreviousSibling := FLastChild;
    end else
      FFirstChild := NewChild;
    FLastChild := NewChild;
    NewChild.FParentNode := Self;
  end;
  AddToChildNodeTree(NewChild);
  Result := NewChild;
end;

function TDOMNode_WithChildren.HasChildNodes: Boolean;
begin
  Result := Assigned(FFirstChild);
end;


function TDOMNode_WithChildren.FindNode(const ANodeName: DOMString): TDOMNode;
var AVLNode: TAVLTreeNode;
begin
  Result:=nil;
  if FChildNodeTree<>nil then begin
    AVLNode:=FChildNodeTree.FindKey(DOMPChar(ANodeName),
                                    @CompareDOMStringWithDOMNode);
    if AVLNode<>nil then
      Result:=TDOMNode(AVLNode.Data);
  end;
end;


procedure TDOMNode_WithChildren.CloneChildren(ACopy: TDOMNode;
  ACloneOwner: TDOMDocument);
var
  node: TDOMNode;
begin
  node := FirstChild;
  while Assigned(node) do
  begin
    ACopy.AppendChild(node.CloneNode(True, ACloneOwner));
    node := node.NextSibling;
  end;
end;


procedure TDOMNode_WithChildren.AddToChildNodeTree(NewNode: TDOMNode);
begin
  if FChildNodeTree=nil then
    FChildNodeTree:=TAVLTree.Create(@CompareDOMNodeWithDOMNode);
  if FChildNodeTree.Find(NewNode)=nil then
    FChildNodeTree.Add(NewNode);
end;

procedure TDOMNode_WithChildren.RemoveFromChildNodeTree(OldNode: TDOMNode);
begin
  if FChildNodeTree<>nil then
    FChildNodeTree.Remove(OldNode);
end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

constructor TDOMNodeList.Create(ANode: TDOMNode);
begin
  inherited Create;
  node := ANode;
  FRevision := ANode.GetRevision-1;   // force BuildList at first access
  FList := TList.Create;
end;

constructor TDOMNodeList.Create(ANode: TDOMNode; const AFilter: DOMString);
begin
  Create(ANode);
  filter := AFilter;
  UseFilter := filter <> '*';
end;

constructor TDOMNodeList.Create(ANode: TDOMNode; const nsURI, localName: DOMString);
begin
  Create(ANode);
  filter := localName;
  FNamespaceFilter := nsURI;
  // TODO: UseFilter ?
end;

destructor TDOMNodeList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

// TODO: namespace support here
// TODO: when called by GetElementByTagName, must build RECURSIVE list of ELEMENTS
procedure TDOMNodeList.BuildList;
var
  Child: TDOMNode;
begin
  FList.Clear;
  FRevision := node.GetRevision; // refresh

  Child := node.FirstChild;
  if UseFilter then
  begin
    while Assigned(Child) do
    begin
      if Child.NodeName = filter then
        FList.Add(Child);
      Child := Child.NextSibling;
    end;
  end
  else
  begin
    while Assigned(Child) do
    begin
      FList.Add(Child);
      Child := Child.NextSibling;
    end;
  end;
end;

function TDOMNodeList.GetCount: LongWord;
begin
  if FRevision <> node.GetRevision then
    BuildList;

  Result := FList.Count;
end;

function TDOMNodeList.GetItem(index: LongWord): TDOMNode;
begin
  if FRevision <> node.GetRevision then
    BuildList;

  if index < LongWord(FList.Count) then
    Result := TDOMNode(FList[index])
  else
    Result := nil;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner: TDOMNode);
begin
  inherited Create;
  FOwnerElement := AOwner;
end;

// TODO: should this be in overriden Clear()?
destructor TDOMNamedNodeMap.Destroy;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    Item[I].Free;
  inherited Destroy;
end;

function TDOMNamedNodeMap.GetItem(index: LongWord): TDOMNode;
begin
  Result := TDOMNode(Items[index]);
end;

function TDOMNamedNodeMap.GetLength: LongWord;
begin
  Result := Count;
end;

function TDOMNamedNodeMap.GetNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Item[i];
    if Result.NodeName = name then
      exit;
  end;
  Result := nil;
end;

function TDOMNamedNodeMap.GetNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
begin
  // TODO: implement TDOMNamedNodeMap.GetNamedItemNS
  raise EDOMNotSupported.Create('TDOMNamedNodeMap.GetNamedItemNS');
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItem(arg: TDOMNode): TDOMNode;
var
  i: Integer;
  AttrOwner: TDOMElement;
begin
  // FIX: attribute ownership
  if arg.FOwnerDocument <> FOwnerElement.FOwnerDocument then
    raise EDOMWrongDocument.Create('NamedNodeMap.SetNamedItem');

  if arg.NodeType = ATTRIBUTE_NODE then
  begin
    AttrOwner := TDOMAttr(arg).ownerElement;
    // FIX: allow setting items which have the same owner
    if Assigned(AttrOwner) and (AttrOwner <> FOwnerElement) then
      raise EDOMInUseAttribute.Create('NamedNodeMap.SetNamedItem');
    TDOMAttr(arg).FOwnerElement := TDOMElement(FOwnerElement);
  end;

  for i := 0 to Count - 1 do
    if Item[i].NodeName = arg.NodeName then
    begin
      Result := Item[i];
      Items[i] := arg;
      exit;
    end;
  Add(arg);
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItemNS(arg: TDOMNode): TDOMNode;
begin
  // TODO: implement TDOMNamedNodeMap.SetNamedItemNS
  Result := nil;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const name: DOMString): TDOMNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Item[i].NodeName = name then
    begin
      Result := Item[i];
      // DONE: delete item from list
      Delete(I);
      // DONE: attribute ownership
      if Result.NodeType = ATTRIBUTE_NODE then
        TDOMAttr(Result).FOwnerElement := nil;
      Result.FParentNode := nil;  // ??? should it ever be assigned for Attrs, Notations or Entities?
      exit;
    end;
  raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;

function TDOMNamedNodeMap.RemoveNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
begin
  // TODO: Implement TDOMNamedNodeMap.RemoveNamedItemNS
  Result := nil;
end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

function TDOMCharacterData.GetLength: LongWord;
begin
  Result := system.Length(FNodeValue);
end;

function TDOMCharacterData.SubstringData(offset, count: LongWord): DOMString;
begin
  if (offset > Length) then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  Result := Copy(FNodeValue, offset + 1, count);
end;

procedure TDOMCharacterData.AppendData(const arg: DOMString);
begin
  FNodeValue := FNodeValue + arg;
end;

procedure TDOMCharacterData.InsertData(offset: LongWord; const arg: DOMString);
begin
  if (offset > Length) then
    raise EDOMIndexSize.Create('CharacterData.InsertData');
  // TODO: use System.Insert?
  FNodeValue := Copy(FNodeValue, 1, offset) + arg +
    Copy(FNodeValue, offset + 1, Length);
end;

procedure TDOMCharacterData.DeleteData(offset, count: LongWord);
begin
  if (offset > Length) then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');
  // TODO: use System.Delete?
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
  FNodeName := '#document-fragment';
  inherited Create(AOwner);
end;

function TDOMDocumentFragment.GetNodeType: Integer;
begin
  Result := DOCUMENT_FRAGMENT_NODE;
end;

// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

function TDOMImplementation.HasFeature(const feature, version: DOMString):
  Boolean;
begin
  // very basic
  if (feature = 'XML') then
  begin
    if (version = '') or (version = '1.0') then
      Result := True
    else
      Result := False;
  end
  else
    Result := False;
end;

function TDOMImplementation.CreateDocumentType(const QualifiedName, PublicID,
  SystemID: DOMString): TDOMDocumentType;
begin
  // PublicID impossible without SystemID
  // QualifiedName -> TDomDocumentType.Name

  // !!!: Implement this method (easy to do)
  raise EDOMNotSupported.Create('DOMImplementation.CreateDocumentType');
  if (QualifiedName='') and (PublicID='') and (SystemID='') then ;
  Result:=nil;
end;

function TDOMImplementation.CreateDocument(const NamespaceURI,
  QualifiedName: DOMString; doctype: TDOMDocumentType): TDOMDocument;
begin
  // QualifiedName and NamespaceURI are for root element, not document itself
  // like that:
{
var
  Root: TDOMNode;
begin
  Result := TDOMDocument.Create;
  Result.DocType := doctype;
  Root := Result.CreateElementNS(NamespaceURI, QualifiedName);
  Result.AppendChild(Root);
end;
}

  // !!!: Implement this method (easy to do)
  raise EDOMNotSupported.Create('DOMImplementation.CreateDocument');
  if (NamespaceURI='') and (QualifiedName='') and (doctype=nil) then ;
  Result:=nil;
end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

constructor TDOMDocument.Create;
begin
  FNodeName := '#document';
  inherited Create(nil);
  // TODO: DOM lvl 2 states that Document should be unowned. Any dependencies?  
  FOwnerDocument := Self;
end;

function TDOMDocument.GetNodeType: Integer;
begin
  Result := DOCUMENT_NODE;
end;

function TDOMDocument.GetDocumentElement: TDOMElement;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) do
  begin
    if node.NodeType = ELEMENT_NODE then
    begin
      Result := TDOMElement(node);
      exit;
    end;
    node := node.NextSibling;
  end;
  Result := nil;
end;

function TDOMDocument.GetDocType: TDOMDocumentType;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) do
  begin
    if node.NodeType = DOCUMENT_TYPE_NODE then
    begin
      Result := TDOMDocumentType(node);
      exit;
    end;
    node := node.NextSibling;
  end;
  Result := nil;
end;

function TDOMDocument.CreateElement(const tagName: DOMString): TDOMElement;
begin
  Result := TDOMElement.Create(Self);
  Result.FNodeName := tagName;
end;

function TDOMDocument.CreateElementBuf(Buf: DOMPChar; Length: Integer): TDOMElement;
begin
  Result := TDOMElement.Create(Self);
  SetString(Result.FNodeName, Buf, Length);
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

function TDOMDocument.CreateTextNodeBuf(Buf: DOMPChar; Length: Integer): TDOMText;
begin
  Result := TDOMText.Create(Self);
  SetString(Result.FNodeValue, Buf, Length);
end;


function TDOMDocument.CreateComment(const data: DOMString): TDOMComment;
begin
  Result := TDOMComment.Create(Self);
  Result.FNodeValue := data;
end;

function TDOMDocument.CreateCommentBuf(Buf: DOMPChar; Length: Integer): TDOMComment;
begin
  Result := TDOMComment.Create(Self);
  SetString(Result.FNodeValue, Buf, Length);
end;

function TDOMDocument.CreateCDATASection(const data: DOMString):
  TDOMCDATASection;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateCDATASection');
  Result:=nil;
end;

function TDOMDocument.CreateProcessingInstruction(const target,
  data: DOMString): TDOMProcessingInstruction;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateProcessingInstruction');
  Result:=nil;
end;

function TDOMDocument.CreateAttribute(const name: DOMString): TDOMAttr;
begin
  Result := TDOMAttr.Create(Self);
  Result.FNodeName := name;
end;

function TDOMDocument.CreateAttributeBuf(Buf: DOMPChar; Length: Integer): TDOMAttr;
begin
  Result := TDOMAttr.Create(Self);
  SetString(Result.FNodeName, Buf, Length);
end;

function TDOMDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
  Result:=nil;
end;

function TDOMDocument.CreateEntity(const data: DOMString): TDOMEntity;
begin
  Result := TDOMEntity.Create(Self);
  Result.FNodeName := data;
end;

function TDOMDocument.GetElementsByTagName(const tagname: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, tagname);
end;

function TDOMDocument.GetElementsByTagNameNS(const namespaceURI, localName: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, namespaceURI, localName);
end;

function TDOMDocument.CreateAttributeNS(const NamespaceURI,
  QualifiedName: DOMString): TDOMAttr;
begin
  // TODO: Implement TDOMDocument.CreateAttributeNS
  raise EDOMNotSupported.Create('TDOMDocument.CreateAttributeNS');
  Result := nil;
end;

function TDOMDocument.CreateElementNS(const NamespaceURI,
  QualifiedName: DOMString): TDOMElement;
begin
  // TODO: Implement TDOMDocument.CreateElementNS
  raise EDOMNotSupported.Create('TDOMDocument.CreateElementNS');
  Result := nil;
end;

function TDOMDocument.GetElementById(
  const ElementID: DOMString): TDOMElement;
begin
  // TODO: Implement TDOMDocument.GetElementById
  raise EDOMNotSupported.Create('TDOMDocument.GetElementById');
  Result := nil;
end;

function TDOMDocument.ImportNode(ImportedNode: TDOMNode;
  Deep: Boolean): TDOMNode;
begin
  // TODO: Implement TDOMDocument.ImportNode
  raise EDOMNotSupported.Create('TDOMDocument.ImportNode');
  Result := nil;
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

function TDOMAttr.GetNodeType: Integer;
begin
  Result := ATTRIBUTE_NODE;
end;

function TDOMAttr.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateAttribute(FNodeName);
  TDOMAttr(Result).FSpecified := FSpecified;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMAttr.GetNodeValue: DOMString;
var
  child: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FFirstChild) then
  begin
    child := FFirstChild;
    while Assigned(child) do
    begin
      if child.NodeType = ENTITY_REFERENCE_NODE then
      // TODO: here we must substitute entity value
        Result := Result + '&' + child.NodeName + ';'
      else
        Result := Result + child.NodeValue;
      child := child.NextSibling;
    end;
  end;
end;

procedure TDOMAttr.SetNodeValue(const AValue: DOMString);
var
  tn: TDOMText;
begin
  FSpecified := True;
  tn := FOwnerDocument.CreateTextNode(AValue);
  // TODO: if we have EntityRefs children, they should be deleted...
  if Assigned(FFirstChild) then
    ReplaceChild(tn, FFirstChild)
  else
    AppendChild(tn);
end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

function TDOMElement.GetNodeType: Integer;
begin
  Result := ELEMENT_NODE;
end;

destructor TDOMElement.Destroy;
begin
  // FIX: Attribute nodes are now freed by TDOMNamedNodeMap.Destroy
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

function TDOMElement.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
var
  i: Integer;
begin
  Result := ACloneOwner.CreateElement(FNodeName);
  if FAttributes<>nil then
  begin
    TDOMElement(Result).GetAttributes;
    for i := 0 to FAttributes.Count - 1 do
      TDOMElement(Result).FAttributes.Add(FAttributes[i].CloneNode(True, ACloneOwner));
  end;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

function TDOMElement.GetAttributes: TDOMNamedNodeMap;
begin
  if FAttributes=nil then
    // FIX: ownership
    FAttributes := TDOMNamedNodeMap.Create(Self);
  Result := FAttributes;
end;

function TDOMElement.GetAttribute(const name: DOMString): DOMString;
var
  Attr: TDOMNode;
begin
  // DONE: delegated to TNamedNodeMap
  SetLength(Result, 0);
  if Assigned(FAttributes) then
  begin
    Attr := FAttributes.GetNamedItem(name);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

function TDOMElement.GetAttributeNS(const namespaceURI, localName: DOMString): DOMString;
var
  Attr: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FAttributes) then
  begin
    Attr := FAttributes.GetNamedItemNS(namespaceURI, localName);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

procedure TDOMElement.SetAttribute(const name, value: DOMString);
var
  attr: TDOMAttr;
begin
  GetAttributes;
  // DONE: delegate to TNamedNodeMap
  Attr := FAttributes.GetNamedItem(name) as TDOMAttr;
  if Assigned(Attr) then
    Attr.NodeValue := value
  else
  begin
    attr := FOwnerDocument.CreateAttribute(name);
    attr.NodeValue := value;
    FAttributes.Add(attr);
  end;
end;

procedure TDOMElement.RemoveAttribute(const name: DOMString);
var
  i: Integer;
begin
  // (note) cannot call NamedNodeMap.RemoveNamedItem because it can raise NOT_FOUND_ERR
  // and we should not raise it.
  if FAttributes=nil then exit;
  for i := 0 to FAttributes.Count - 1 do
    if FAttributes[i].NodeName = name then
    begin
      FAttributes[i].Free;
      FAttributes.Delete(i);
      exit;
    end;
end;

procedure TDOMElement.RemoveAttributeNS(const namespaceURI,
  localName: DOMString);
begin
  // TODO: Implement TDOMElement.RemoveAttributeNS
  raise EDOMNotSupported.Create('TDOMElement.RemoveAttributeNS'); 
end;

procedure TDOMElement.SetAttributeNS(const namespaceURI, qualifiedName,
  value: DOMString);
var
  Attr: TDOMAttr;
begin
  GetAttributes;
  Attr := FAttributes.GetNamedItemNS(namespaceURI, qualifiedName) as TDOMAttr;
  if Assigned(Attr) then
    Attr.NodeValue := value
  else
  begin
    attr := FOwnerDocument.CreateAttributeNS(namespaceURI, qualifiedName);
    attr.NodeValue := value;
    FAttributes.Add(attr);
  end;
end;

function TDOMElement.GetAttributeNode(const name: DOMString): TDOMAttr;
begin
  // DONE: delegated to TNamedNodeMap.GetNamedItem
  if Assigned(FAttributes) then
    Result := FAttributes.GetNamedItem(name) as TDOMAttr
  else
    Result := nil;
end;

function TDOMElement.GetAttributeNodeNS(const namespaceURI, localName: DOMString): TDOMAttr;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.GetNamedItemNS(namespaceURI, localName) as TDOMAttr
  else
    Result := nil;
end;

function TDOMElement.SetAttributeNode(NewAttr: TDOMAttr): TDOMAttr;
begin
  // FIX #1: FAttributes must be created if none
  // FIX #2: if no such attribute present, it should be added
  // FIX #3: All delegated to TDOMNamedNodeMap
  GetAttributes;
  Result := FAttributes.SetNamedItem(NewAttr) as TDOMAttr;

  // here goes inconsistency with DOM 2 - same as in TDOMNode.RemoveChild
  Result.Free;
  Result := nil;
end;

function TDOMElement.SetAttributeNodeNS(NewAttr: TDOMAttr): TDOMAttr;
begin
  GetAttributes;
  Result := FAttributes.SetNamedItemNS(NewAttr) as TDOMAttr;

  // here goes inconsistency with DOM 2 - same as in TDOMNode.RemoveChild
  Result.Free;
  Result := nil;
end;


function TDOMElement.RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
begin
  Result:=nil;
  if FAttributes=nil then exit;
  // TODO: DOM 2: must raise NOT_FOUND_ERR if OldAttr is not ours.
  //       -- but what is the purpose of return value then?
  // TODO: delegate to TNamedNodeMap?  Nope, it does not have such method
  // (note) one way around is to remove by name
  if FAttributes.Remove(OldAttr) > -1 then
    Result := OldAttr;
end;

function TDOMElement.GetElementsByTagName(const name: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, name);
end;

function TDOMElement.GetElementsByTagNameNS(const namespaceURI, localName: DOMString): TDOMNodeList;
begin
  Result := TDOMNodeList.Create(Self, namespaceURI, localName);
end;

function TDOMElement.hasAttribute(const name: DOMString): Boolean;
begin
  Result := Assigned(FAttributes) and
    Assigned(FAttributes.GetNamedItem(name));
end;

function TDOMElement.hasAttributeNS(const namespaceURI, localName: DOMString): Boolean;
begin
  Result := Assigned(FAttributes) and
    Assigned(FAttributes.getNamedItemNS(namespaceURI, localName));
end;

function TDOMElement.HasAttributes: Boolean;
begin
  Result := Assigned(FAttributes);
end;

// -------------------------------------------------------
//   Text
// -------------------------------------------------------

constructor TDOMText.Create(AOwner: TDOMDocument);
begin
  FNodeName := '#text';
  inherited Create(AOwner);
end;

function TDOMText.GetNodeType: Integer;
begin
  Result := TEXT_NODE;
end;

function TDOMText.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateTextNode(FNodeValue); {Data?}
  // ignore deep because text cannot have children
end;

function TDOMText.SplitText(offset: LongWord): TDOMText;
begin
  if offset > Length then
    raise EDOMIndexSize.Create('Text.SplitText');

  Result := TDOMText.Create(FOwnerDocument);
  Result.FNodeValue := Copy(FNodeValue, offset + 1, Length);
  FNodeValue := Copy(FNodeValue, 1, offset);
  FParentNode.InsertBefore(Result, FNextSibling);
end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

constructor TDOMComment.Create(AOwner: TDOMDocument);
begin
  FNodeName := '#comment';
  inherited Create(AOwner);
end;

function TDOMComment.GetNodeType: Integer;
begin
  Result := COMMENT_NODE;
end;

function TDOMComment.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateComment(FNodeValue);
end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

constructor TDOMCDATASection.Create(AOwner: TDOMDocument);
begin
  inherited Create(AOwner);
  FNodeName := '#cdata-section';
end;

function TDOMCDATASection.GetNodeType: Integer;
begin
  Result := CDATA_SECTION_NODE;
end;

function TDOMCDATASection.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateCDATASection(FNodeValue);
end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

function TDOMDocumentType.GetNodeType: Integer;
begin
  Result := DOCUMENT_TYPE_NODE;
end;

destructor TDOMDocumentType.Destroy;
begin
  FEntities.Free;
  FNotations.Free;
  inherited Destroy;
end;

function TDOMDocumentType.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMDocumentType.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  // ignore deep - DocumentType cannot have children
  // TODO: Clone Entities and Notations 
end;

function TDOMDocumentType.GetEntities: TDOMNamedNodeMap;
begin
  if FEntities = nil then
    FEntities := TDOMNamedNodeMap.Create(Self);
  Result := FEntities;
end;

function TDOMDocumentType.GetNotations: TDOMNamedNodeMap;
begin
  if FNotations = nil then
    FNotations := TDOMNamedNodeMap.Create(Self);
  Result := FNotations;
end;

// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

function TDOMNotation.GetNodeType: Integer;
begin
  Result := NOTATION_NODE;
end;

function TDOMNotation.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMNotation.Create(ACloneOwner);
  Result.FNodeName := FNodeName;
  TDOMNotation(Result).FPublicID := PublicID;
  TDOMNotation(Result).FSystemID := SystemID;
  // notation cannot have children, ignore Deep
end;


// -------------------------------------------------------
//   Entity
// -------------------------------------------------------

function TDOMEntity.GetNodeType: Integer;
begin
  Result := ENTITY_NODE;
end;

// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

function TDOMEntityReference.GetNodeType: Integer;
begin
  Result := ENTITY_REFERENCE_NODE;
end;

// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

function TDOMProcessingInstruction.CloneNode(deep: Boolean;
  ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateProcessingInstruction(Target, Data);
end;

function TDOMProcessingInstruction.GetNodeType: Integer;
begin
  Result := PROCESSING_INSTRUCTION_NODE;
end;


end.
