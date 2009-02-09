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

uses
  SysUtils, Classes, AVL_Tree, xmlutils;

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

// URIs of predefined namespaces
  stduri_xml = 'http://www.w3.org/XML/1998/namespace';
  stduri_xmlns = 'http://www.w3.org/2000/xmlns/';


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

  TSetOfChar = set of Char;
  DOMString = WideString;
  DOMPChar = PWideChar;
  PDOMString = ^DOMString;

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

{ NodeType, NodeName and NodeValue had been moved from fields to functions.
  This lowers memory usage and also obsoletes most constructors,
  at a slight performance penalty. However, NodeName and NodeValue are
  accessible via fields using specialized properties of descendant classes,
  e.g. TDOMElement.TagName, TDOMCharacterData.Data etc.}

  TNodeFlagEnum = (
    nfReadonly,
    nfRecycled,
    nfLevel2,
    nfIgnorableWS,
    nfSpecified
  );
  TNodeFlags = set of TNodeFlagEnum;

  TDOMNode = class
  protected
    FFlags: TNodeFlags;
    FParentNode: TDOMNode;
    FPreviousSibling, FNextSibling: TDOMNode;
    FOwnerDocument: TDOMDocument;

    function  GetNodeName: DOMString; virtual; abstract;
    function  GetNodeValue: DOMString; virtual;
    procedure SetNodeValue(const AValue: DOMString); virtual;
    function  GetFirstChild: TDOMNode; virtual;
    function  GetLastChild: TDOMNode; virtual;
    function  GetAttributes: TDOMNamedNodeMap; virtual;
    function GetRevision: Integer;
    function GetNodeType: Integer; virtual; abstract;
    function GetTextContent: DOMString; virtual;
    procedure SetTextContent(const AValue: DOMString); virtual;
    function GetLocalName: DOMString; virtual;
    function GetNamespaceURI: DOMString; virtual;
    function GetPrefix: DOMString; virtual;
    procedure SetPrefix(const Value: DOMString); virtual;
    function GetOwnerDocument: TDOMDocument; virtual;
    procedure SetReadOnly(Value: Boolean);
    procedure Changing;
  public
    constructor Create(AOwner: TDOMDocument);
    destructor Destroy; override;

    // Free NodeList with TDOMNodeList.Release!
    function GetChildNodes: TDOMNodeList;

    property NodeName: DOMString read GetNodeName;
    property NodeValue: DOMString read GetNodeValue write SetNodeValue;
    property NodeType: Integer read GetNodeType;
    property ParentNode: TDOMNode read FParentNode;
    property FirstChild: TDOMNode read GetFirstChild;
    property LastChild: TDOMNode read GetLastChild;
    property ChildNodes: TDOMNodeList read GetChildNodes;
    property PreviousSibling: TDOMNode read FPreviousSibling;
    property NextSibling: TDOMNode read FNextSibling;
    property Attributes: TDOMNamedNodeMap read GetAttributes;
    property OwnerDocument: TDOMDocument read GetOwnerDocument;

    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; virtual;
    function DetachChild(OldChild: TDOMNode): TDOMNode; virtual;
    function RemoveChild(OldChild: TDOMNode): TDOMNode;
    function AppendChild(NewChild: TDOMNode): TDOMNode;
    function HasChildNodes: Boolean; virtual;
    function CloneNode(deep: Boolean): TDOMNode; overload;

    // DOM level 2
    function IsSupported(const Feature, Version: DOMString): Boolean;
    function HasAttributes: Boolean; virtual;
    procedure Normalize; virtual;

    property NamespaceURI: DOMString read GetNamespaceURI;
    property LocalName: DOMString read GetLocalName;
    property Prefix: DOMString read GetPrefix write SetPrefix;
    // DOM level 3
    property TextContent: DOMString read GetTextContent write SetTextContent;
    // Extensions to DOM interface:
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; virtual;
    function FindNode(const ANodeName: DOMString): TDOMNode; virtual;
    function CompareName(const name: DOMString): Integer; virtual;
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
    procedure FreeChildren;
    function GetTextContent: DOMString; override;
    procedure SetTextContent(const AValue: DOMString); override;
  public
    destructor Destroy; override;
    function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; override;
    function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; override;
    function DetachChild(OldChild: TDOMNode): TDOMNode; override;
    function HasChildNodes: Boolean; override;
    function FindNode(const ANodeName: DOMString): TDOMNode; override;
  end;


// -------------------------------------------------------
//   NodeList
// -------------------------------------------------------

  TDOMNodeList = class(TRefClass)
  protected
    FNode: TDOMNode;
    FRevision: Integer;
    FList: TFPList;
    function GetCount: LongWord;
    function GetItem(index: LongWord): TDOMNode;
    procedure BuildList; virtual;
  public
    constructor Create(ANode: TDOMNode);
    destructor Destroy; override;
    property Item[index: LongWord]: TDOMNode read GetItem; default;
    property Count: LongWord read GetCount;
    property Length: LongWord read GetCount;
  end;

  { an extension to DOM interface, used to build recursive lists of elements }

  TDOMElementList = class(TDOMNodeList)
  protected
    filter: DOMString;
    FNamespaceFilter: DOMString;
    UseFilter: Boolean;
    procedure BuildList; override;
  public
    constructor Create(ANode: TDOMNode; const AFilter: DOMString); overload;
    constructor Create(ANode: TDOMNode; const nsURI, localName: DOMString); overload;
  end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

  TDOMNamedNodeMap = class(TObject)
  protected
    FOwner: TDOMNode;
    FNodeType: Integer;
    FList: TFPList;
    function GetItem(index: LongWord): TDOMNode;
    function GetLength: LongWord;
    function Find(const name: DOMString; out Index: LongWord): Boolean;
    function InternalRemove(const name: DOMString): TDOMNode;
    function ValidateInsert(arg: TDOMNode): Integer;
  public
    constructor Create(AOwner: TDOMNode; ANodeType: Integer);
    destructor Destroy; override;

    function GetNamedItem(const name: DOMString): TDOMNode;
    function SetNamedItem(arg: TDOMNode): TDOMNode;
    function RemoveNamedItem(const name: DOMString): TDOMNode;
    // Introduced in DOM Level 2:
    function getNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
    function setNamedItemNS(arg: TDOMNode): TDOMNode;
    function removeNamedItemNS(const namespaceURI,localName: DOMString): TDOMNode;

    // FIX: made readonly. Reason: Anyone was allowed to insert any node without any checking.
    property Item[index: LongWord]: TDOMNode read GetItem; default;
    property Length: LongWord read GetLength;
  end;


// -------------------------------------------------------
//   CharacterData
// -------------------------------------------------------

  TDOMCharacterData = class(TDOMNode)
  private
    FNodeValue: DOMString;
  protected
    function  GetLength: LongWord;
    function GetNodeValue: DOMString; override;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    property Data: DOMString read FNodeValue write SetNodeValue;
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
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------
  // TODO: to be replaced by more suitable container
  TNamespaces = array of DOMString;

  TDOMDocument = class(TDOMNode_WithChildren)
  protected
    FIDList: THashTable;
    FRevision: Integer;
    FXML11: Boolean;
    FImplementation: TDOMImplementation;
    FNamespaces: TNamespaces;
    FNames: THashTable;
    function GetDocumentElement: TDOMElement;
    function GetDocType: TDOMDocumentType;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
    function GetTextContent: DOMString; override;
    function GetOwnerDocument: TDOMDocument; override;
    procedure SetTextContent(const value: DOMString); override;
    procedure RemoveID(Elem: TDOMElement);
  public
    function IndexOfNS(const nsURI: DOMString; AddIfAbsent: Boolean = False): Integer;
    property DocType: TDOMDocumentType read GetDocType;
    property Impl: TDOMImplementation read FImplementation;
    property DocumentElement: TDOMElement read GetDocumentElement;

    function CreateElement(const tagName: DOMString): TDOMElement; virtual;
    function CreateElementBuf(Buf: DOMPChar; Length: Integer): TDOMElement;
    function CreateDocumentFragment: TDOMDocumentFragment;
    function CreateTextNode(const data: DOMString): TDOMText;
    function CreateTextNodeBuf(Buf: DOMPChar; Length: Integer; IgnWS: Boolean): TDOMText;
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
    function CreateElementNS(const nsURI, QualifiedName: DOMString): TDOMElement;
    function CreateAttributeNS(const nsURI, QualifiedName: DOMString): TDOMAttr;
    function GetElementsByTagNameNS(const nsURI, alocalName: DOMString): TDOMNodeList;
    function GetElementById(const ElementID: DOMString): TDOMElement;
    // Extensions to DOM interface:
    constructor Create;
    destructor Destroy; override;
    function AddID(Attr: TDOMAttr): Boolean;
    property Names: THashTable read FNames;
  end;

  TXMLDocument = class(TDOMDocument)
  private
    FXMLVersion: DOMString;
    procedure SetXMLVersion(const aValue: DOMString);
  public
    // These fields are extensions to the DOM interface:
    Encoding, StylesheetType, StylesheetHRef: DOMString;

    function CreateCDATASection(const data: DOMString): TDOMCDATASection; override;
    function CreateProcessingInstruction(const target, data: DOMString):
      TDOMProcessingInstruction; override;
    function CreateEntityReference(const name: DOMString): TDOMEntityReference; override;
    property XMLVersion: DOMString read FXMLVersion write SetXMLVersion;
  end;

  // This limits number of namespaces per document to 65535,
  // and prefix length to 65535, too.
  // I believe that higher values may only be found in deliberately malformed documents.
  TNamespaceInfo = packed record
    NSIndex: Word;
    PrefixLen: Word;
    QName: PHashItem;
  end;

// -------------------------------------------------------
//   Attr
// -------------------------------------------------------

  TAttrDataType = (
    dtCdata,
    dtId,
    dtIdRef,
    dtIdRefs,
    dtEntity,
    dtEntities,
    dtNmToken,
    dtNmTokens,
    dtNotation
  );

  TDOMNode_NS = class(TDOMNode_WithChildren)
  protected
    FNSI: TNamespaceInfo;
    function GetNodeName: DOMString; override;
    function GetLocalName: DOMString; override;
    function GetNamespaceURI: DOMString; override;
    function GetPrefix: DOMString; override;
    procedure SetPrefix(const Value: DOMString); override;
  public
    function CompareName(const AName: DOMString): Integer; override;
    property NSI: TNamespaceInfo read FNSI;
  end;

  TDOMAttr = class(TDOMNode_NS)
  protected
    FOwnerElement: TDOMElement;
    FDataType: TAttrDataType;
    function  GetNodeValue: DOMString; override;
    function GetNodeType: Integer; override;
    function GetSpecified: Boolean;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Name: DOMString read GetNodeName;
    property Specified: Boolean read GetSpecified;
    property Value: DOMString read GetNodeValue write SetNodeValue;
    property OwnerElement: TDOMElement read FOwnerElement;
    // extensions
    property DataType: TAttrDataType read FDataType;
  end;


// -------------------------------------------------------
//   Element
// -------------------------------------------------------

  TDOMElement = class(TDOMNode_NS)
  protected
    FAttributes: TDOMNamedNodeMap;
    function GetNodeType: Integer; override;
    function GetAttributes: TDOMNamedNodeMap; override;
  public
    destructor Destroy; override;
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    procedure Normalize; override;
    property  TagName: DOMString read GetNodeName;
    function  GetAttribute(const name: DOMString): DOMString;
    procedure SetAttribute(const name, value: DOMString);
    procedure RemoveAttribute(const name: DOMString);
    function  GetAttributeNode(const name: DOMString): TDOMAttr;
    function SetAttributeNode(NewAttr: TDOMAttr): TDOMAttr;
    function RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
    // Free NodeList with TDOMNodeList.Release!
    function  GetElementsByTagName(const name: DOMString): TDOMNodeList;

    // Introduced in DOM Level 2:
    function GetAttributeNS(const nsURI, aLocalName: DOMString): DOMString;
    procedure SetAttributeNS(const nsURI, qualifiedName, value: DOMString);
    procedure RemoveAttributeNS(const nsURI, aLocalName: DOMString);
    function GetAttributeNodeNS(const nsURI, aLocalName: DOMString): TDOMAttr;
    function SetAttributeNodeNS(newAttr: TDOMAttr): TDOMAttr;
    function GetElementsByTagNameNS(const nsURI, aLocalName: DOMString): TDOMNodeList;
    function hasAttribute(const name: DOMString): Boolean;
    function hasAttributeNS(const nsURI, aLocalName: DOMString): Boolean;
    function HasAttributes: Boolean; override;
    // extension
    property AttribStrings[const Name: DOMString]: DOMString
      read GetAttribute write SetAttribute; default;
  end;


// -------------------------------------------------------
//   Text
// -------------------------------------------------------

  TDOMText = class(TDOMCharacterData)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
    procedure SetNodeValue(const aValue: DOMString); override;
  public
    function  CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    function SplitText(offset: LongWord): TDOMText;
    function IsElementContentWhitespace: Boolean;
  end;


// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

  TDOMComment = class(TDOMCharacterData)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

  TDOMCDATASection = class(TDOMText)
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   DocumentType
// -------------------------------------------------------

  TDOMDocumentType = class(TDOMNode)
  protected
    FName: DOMString;
    FPublicID: DOMString;
    FSystemID: DOMString;
    FInternalSubset: DOMString;
    FEntities, FNotations: TDOMNamedNodeMap;
    function GetEntities: TDOMNamedNodeMap;
    function GetNotations: TDOMNamedNodeMap;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    destructor Destroy; override;
    property Name: DOMString read FName;
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
    FName: DOMString;
    FPublicID, FSystemID: DOMString;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
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
    FName: DOMString;
    FPublicID, FSystemID, FNotationName: DOMString;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; aCloneOwner: TDOMDocument): TDOMNode; override;
    property PublicID: DOMString read FPublicID;
    property SystemID: DOMString read FSystemID;
    property NotationName: DOMString read FNotationName;
  end;


// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

  TDOMEntityReference = class(TDOMNode_WithChildren)
  protected
    FName: DOMString;
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
  end;


// -------------------------------------------------------
//   ProcessingInstruction
// -------------------------------------------------------

  TDOMProcessingInstruction = class(TDOMNode)
  private
    FTarget: DOMString;
    FNodeValue: DOMString;
  protected
    function GetNodeType: Integer; override;
    function GetNodeName: DOMString; override;
    function GetNodeValue: DOMString; override;
    procedure SetNodeValue(const AValue: DOMString); override;
  public
    function CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode; overload; override;
    property Target: DOMString read FTarget;
    property Data: DOMString read FNodeValue write SetNodeValue;
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

destructor TDOMNode.Destroy;
begin
  if Assigned(FParentNode) then
    FParentNode.DetachChild(Self);
  inherited Destroy;
end;

function TDOMNode.GetNodeValue: DOMString;
begin
  Result := '';
end;

procedure TDOMNode.SetNodeValue(const AValue: DOMString);
begin
  // do nothing
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
  Changing;  // merely to comply with core3/nodeinsertbefore14
  raise EDOMHierarchyRequest.Create('Node.InsertBefore');
  Result:=nil;
end;

function TDOMNode.ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode;
begin
  Changing;  // merely to comply with core3/nodereplacechild21
  raise EDOMHierarchyRequest.Create('Node.ReplaceChild');
  Result:=nil;
end;

function TDOMNode.DetachChild(OldChild: TDOMNode): TDOMNode;
begin
  // OldChild isn't in our child list
  raise EDOMNotFound.Create('Node.RemoveChild');
  Result:=nil;
end;

function TDOMNode.RemoveChild(OldChild: TDOMNode): TDOMNode;
begin
  DetachChild(OldChild);
  OldChild.Free;
  Result:=nil;
end;

function TDOMNode.AppendChild(NewChild: TDOMNode): TDOMNode;
begin
  Result := InsertBefore(NewChild, nil);
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
// !! CreateFmt() does not set Code property !!
  raise EDOMNotSupported.Create(Format('Cloning/importing of %s is not supported', [ClassName]));
  Result:=nil;
end;

function TDOMNode.FindNode(const ANodeName: DOMString): TDOMNode;
begin
  // FIX: we have no children, hence cannot find anything
  Result := nil;
end;

function TDOMNode.GetRevision: Integer;
begin
  Result := FOwnerDocument.FRevision;
end;

function TDOMNode.IsSupported(const Feature, Version: DOMString): Boolean;
begin
  Result := FOwnerDocument.Impl.HasFeature(Feature, Version);
end;

function TDOMNode.HasAttributes: Boolean;
begin
  Result := False;
end;

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
      tmp := Child.NextSibling;
      if TDOMText(Child).Data <> '' then
      begin
        if Assigned(Txt) then
        begin
          Txt.AppendData(TDOMText(Child).Data);
          // TODO: maybe should be smarter
          Exclude(Txt.FFlags, nfIgnorableWS);
        end
        else
        begin
          Txt := TDOMText(Child);
          Child := Child.NextSibling;
          Continue;
        end;
      end;
      Child.Free;
      Child := tmp;
    end
    else
    begin
      Child.Normalize;  // should be recursive!
      Child := Child.NextSibling;
      Txt := nil;
    end;
  end;
end;

function TDOMNode.GetTextContent: DOMString;
begin
  Result := NodeValue;
end;

procedure TDOMNode.SetTextContent(const AValue: DOMString);
begin
  SetNodeValue(AValue);
end;

function TDOMNode.GetNamespaceURI: DOMString;
begin
  Result := '';
end;

function TDOMNode.GetLocalName: DOMString;
begin
  Result := '';
end;

function TDOMNode.GetPrefix: DOMString;
begin
  Result := '';
end;

procedure TDOMNode.SetPrefix(const Value: DOMString);
begin
  // do nothing, override for Elements and Attributes
end;

function TDOMNode.GetOwnerDocument: TDOMDocument;
begin
  Result := FOwnerDocument;
end;

procedure TDOMNode.SetReadOnly(Value: Boolean);
var
  child: TDOMNode;
  attrs: TDOMNamedNodeMap;
  I: Integer;
begin
  if Value then
    Include(FFlags, nfReadOnly)
  else
    Exclude(FFlags, nfReadOnly);
  child := FirstChild;
  while Assigned(child) do
  begin
    child.SetReadOnly(Value);
    child := child.NextSibling;
  end;
  attrs := Attributes;
  if Assigned(attrs) then
  begin
    for I := 0 to attrs.Length-1 do
      attrs[I].SetReadOnly(Value);
  end;
end;

procedure TDOMNode.Changing;
begin
  if nfReadOnly in FFlags then
    raise EDOMError.Create(NO_MODIFICATION_ALLOWED_ERR, 'Node.CheckReadOnly');
end;

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

// generic version (slow)
function TDOMNode.CompareName(const name: DOMString): Integer;
var
  SelfName: DOMString;
begin
  SelfName := NodeName;
  Result := CompareDOMStrings(DOMPChar(name), DOMPChar(SelfName), Length(name), Length(SelfName));
end;


//------------------------------------------------------------------------------

function CompareDOMNodeWithDOMNode(Node1, Node2: Pointer): integer;
begin
  Result := TDOMNode(Node2).CompareName(TDOMNode(Node1).NodeName);
end;

function CompareDOMStringWithDOMNode(AKey, ANode: Pointer): integer;
begin
  Result := TDOMNode(ANode).CompareName(PDOMString(AKey)^);
end;

type
  TNodeTypeEnum = ELEMENT_NODE..NOTATION_NODE;
  TNodeTypeSet = set of TNodeTypeEnum;

const
  stdChildren = [TEXT_NODE, ENTITY_REFERENCE_NODE, PROCESSING_INSTRUCTION_NODE,
                 COMMENT_NODE, CDATA_SECTION_NODE, ELEMENT_NODE];

  ValidChildren: array [TNodeTypeEnum] of TNodeTypeSet = (
   stdChildren, { element }
   [TEXT_NODE, ENTITY_REFERENCE_NODE], { attribute }
   [], { text }
   [], { cdata }
   stdChildren, { ent ref }
   stdChildren, { entity }
   [], { pi }
   [], { comment }
   [ELEMENT_NODE, DOCUMENT_TYPE_NODE, PROCESSING_INSTRUCTION_NODE, COMMENT_NODE], { document }
   [], { doctype }
   stdChildren, { fragment }
   []  { notation }
  );

function TDOMNode_WithChildren.GetFirstChild: TDOMNode;
begin
  Result := FFirstChild;
end;

function TDOMNode_WithChildren.GetLastChild: TDOMNode;
begin
  Result := FLastChild;
end;

destructor TDOMNode_WithChildren.Destroy;
begin
  FreeChildren;
  FreeAndNil(FChildNodeTree);  
  inherited Destroy;
end;

function TDOMNode_WithChildren.InsertBefore(NewChild, RefChild: TDOMNode):
  TDOMNode;
var
  Tmp: TDOMNode;
  NewChildType: Integer;
begin
  Result := NewChild;
  NewChildType := NewChild.NodeType;

  Changing;
  if NewChild.FOwnerDocument <> FOwnerDocument then
  begin
    if (NewChildType <> DOCUMENT_TYPE_NODE) or
    (NewChild.FOwnerDocument <> nil) then
      raise EDOMWrongDocument.Create('NodeWC.InsertBefore');
  end;

  if Assigned(RefChild) and (RefChild.ParentNode <> Self) then
    raise EDOMNotFound.Create('NodeWC.InsertBefore');

  // TODO: skip checking Fragments as well? (Fragment itself cannot be in the tree)  
  if not (NewChildType in [TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE, PROCESSING_INSTRUCTION_NODE]) and (NewChild.FirstChild <> nil) then
  begin
    Tmp := Self;
    while Assigned(Tmp) do
    begin
      if Tmp = NewChild then
        raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore (cycle in tree)');
      Tmp := Tmp.ParentNode;
    end;
  end;
  if NewChild = RefChild then    // inserting node before itself is a no-op
    Exit;

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  if NewChildType = DOCUMENT_FRAGMENT_NODE then
  begin
    Tmp := NewChild.FirstChild;
    if Assigned(Tmp) then
    begin
      while Assigned(Tmp) do
      begin
        if not (Tmp.NodeType in ValidChildren[NodeType]) then
          raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');
        Tmp := Tmp.NextSibling;
      end;
    
      while Assigned(TDOMDocumentFragment(NewChild).FFirstChild) do
        InsertBefore(TDOMDocumentFragment(NewChild).FFirstChild, RefChild);
    end;
    Exit;
  end;

  if not (NewChildType in ValidChildren[NodeType]) then
    raise EDOMHierarchyRequest.Create('NodeWC.InsertBefore');

  if Assigned(NewChild.FParentNode) then
    NewChild.FParentNode.DetachChild(NewChild);

  NewChild.FNextSibling := RefChild;
  if RefChild = nil then  // append to the end
  begin
    if Assigned(FFirstChild) then
    begin
      FLastChild.FNextSibling := NewChild;
      NewChild.FPreviousSibling := FLastChild;
    end else
      FFirstChild := NewChild;
    FLastChild := NewChild;
  end
  else   // insert before RefChild
  begin
    if RefChild = FFirstChild then
      FFirstChild := NewChild
    else
    begin
      RefChild.FPreviousSibling.FNextSibling := NewChild;
      NewChild.FPreviousSibling := RefChild.FPreviousSibling;
    end;
    RefChild.FPreviousSibling := NewChild;
  end;
  NewChild.FParentNode := Self;
  AddToChildNodeTree(NewChild);
end;

function TDOMNode_WithChildren.ReplaceChild(NewChild, OldChild: TDOMNode):
  TDOMNode;
begin
  RemoveFromChildNodeTree(OldChild);
  InsertBefore(NewChild, OldChild);
  if Assigned(OldChild) then
    RemoveChild(OldChild);
  // TODO: per DOM spec, must return OldChild, but OldChild is destroyed
  Result := NewChild;
end;

function TDOMNode_WithChildren.DetachChild(OldChild: TDOMNode): TDOMNode;
begin
  Changing;

  if OldChild.ParentNode <> Self then
    raise EDOMNotFound.Create('NodeWC.RemoveChild');

  Inc(FOwnerDocument.FRevision); // invalidate nodelists

  if OldChild = FFirstChild then
    FFirstChild := FFirstChild.FNextSibling
  else
    OldChild.FPreviousSibling.FNextSibling := OldChild.FNextSibling;

  if OldChild = FLastChild then
    FLastChild := FLastChild.FPreviousSibling
  else
    OldChild.FNextSibling.FPreviousSibling := OldChild.FPreviousSibling;

  RemoveFromChildNodeTree(OldChild);
  // Make sure removed child does not contain references to nowhere
  OldChild.FPreviousSibling := nil;
  OldChild.FNextSibling := nil;
  OldChild.FParentNode := nil;
  Result := OldChild;
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
    AVLNode:=FChildNodeTree.FindKey(Pointer(@ANodeName),
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

procedure TDOMNode_WithChildren.FreeChildren;
var
  child, next: TDOMNode;
begin
  if Assigned(FChildNodeTree) then
    FChildNodeTree.Clear;
  child := FFirstChild;
  while Assigned(child) do
  begin
    next := child.NextSibling;
    child.FParentNode := nil;
    child.Destroy;   // we know it's not nil, so save a call
    child := next;
  end;
  FFirstChild := nil;
  FLastChild := nil;
end;

function TDOMNode_WithChildren.GetTextContent: DOMString;
var
  child: TDOMNode;
begin
  Result := '';
  child := FFirstChild;
  // TODO: probably very slow, optimization needed
  while Assigned(child) do
  begin
    case child.NodeType of
      TEXT_NODE: if not (nfIgnorableWS in child.FFlags) then
        Result := Result + TDOMText(child).Data;
      COMMENT_NODE, PROCESSING_INSTRUCTION_NODE: ; // ignored
    else
      Result := Result + child.TextContent;
    end;
    child := child.NextSibling;
  end;
end;

procedure TDOMNode_WithChildren.SetTextContent(const AValue: DOMString);
begin
  Changing;
  FreeChildren;
  if AValue <> '' then
    AppendChild(FOwnerDocument.CreateTextNode(AValue));
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
  FNode := ANode;
  FRevision := ANode.GetRevision-1;   // force BuildList at first access
  FList := TFPList.Create;
end;

destructor TDOMNodeList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TDOMNodeList.BuildList;
var
  Child: TDOMNode;
begin
  FList.Clear;
  FRevision := FNode.GetRevision; // refresh

  Child := FNode.FirstChild;
  while Assigned(Child) do
  begin
    FList.Add(Child);
    Child := Child.NextSibling;
  end;
end;

function TDOMNodeList.GetCount: LongWord;
begin
  if FRevision <> FNode.GetRevision then
    BuildList;

  Result := FList.Count;
end;

function TDOMNodeList.GetItem(index: LongWord): TDOMNode;
begin
  if FRevision <> FNode.GetRevision then
    BuildList;

  if index < LongWord(FList.Count) then
    Result := TDOMNode(FList.List^[index])
  else
    Result := nil;
end;

{ TDOMElementList }

constructor TDOMElementList.Create(ANode: TDOMNode; const AFilter: DOMString);
begin
  inherited Create(ANode);
  filter := AFilter;
  UseFilter := filter <> '*';
end;

constructor TDOMElementList.Create(ANode: TDOMNode; const nsURI, localName: DOMString);
begin
  inherited Create(ANode);
  filter := localName;
  FNamespaceFilter := nsURI;
  UseFilter := (filter <> '*') and (FNamespaceFilter <> '*');
end;

// TODO: namespace support here
procedure TDOMElementList.BuildList;
var
  Child: TDOMNode;
begin
  FList.Clear;
  FRevision := FNode.GetRevision; // refresh

  Child := FNode.FirstChild;
  while Assigned(Child) and (Child <> FNode) do
  begin
    if (Child.NodeType = ELEMENT_NODE) and (not UseFilter or (TDOMElement(Child).TagName = filter)) then
          FList.Add(Child);
    // recursive track node hierarchy  
    if Assigned(Child.FirstChild) then
      Child := Child.FirstChild
    else
      if Assigned(Child.NextSibling) then
        Child := Child.NextSibling
      else
      begin
         Child := Child.ParentNode;
         while Assigned(Child) and (Child <> FNode) and not Assigned(Child.NextSibling) do
           Child := Child.ParentNode;
         if Assigned(Child) and (Child <> FNode) then
            Child := Child.NextSibling;
      end;
  end;
end;


// -------------------------------------------------------
//   NamedNodeMap
// -------------------------------------------------------

constructor TDOMNamedNodeMap.Create(AOwner: TDOMNode; ANodeType: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FNodeType := ANodeType;
  FList := TFPList.Create;
end;

destructor TDOMNamedNodeMap.Destroy;
var
  I: Integer;
begin
  for I := FList.Count-1 downto 0 do
    TDOMNode(FList[I]).Free;
  FList.Free;
  inherited Destroy;
end;

function TDOMNamedNodeMap.GetItem(index: LongWord): TDOMNode;
begin
  if index < LongWord(FList.Count) then
    Result := TDOMNode(FList.List^[index])
  else
    Result := nil;
end;

function TDOMNamedNodeMap.GetLength: LongWord;
begin
  Result := FList.Count;
end;

function TDOMNamedNodeMap.Find(const name: DOMString; out Index: LongWord): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := TDOMNode(FList.List^[I]).CompareName(name);
    if C > 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TDOMNamedNodeMap.GetNamedItem(const name: DOMString): TDOMNode;
var
  i: Cardinal;
begin
  if Find(name, i) then
    Result := TDOMNode(FList.List^[i])
  else
    Result := nil;
end;

function TDOMNamedNodeMap.GetNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
begin
  // TODO: implement TDOMNamedNodeMap.GetNamedItemNS
  raise EDOMNotSupported.Create('TDOMNamedNodeMap.GetNamedItemNS');
    Result := nil;
end;

function TDOMNamedNodeMap.ValidateInsert(arg: TDOMNode): Integer;
var
  AttrOwner: TDOMNode;
begin
  Result := 0;
  if nfReadOnly in FOwner.FFlags then
    Result := NO_MODIFICATION_ALLOWED_ERR
  else if arg.FOwnerDocument <> FOwner.FOwnerDocument then
    Result := WRONG_DOCUMENT_ERR
  else if arg.NodeType <> FNodeType then
    Result := HIERARCHY_REQUEST_ERR
  else if (FNodeType = ATTRIBUTE_NODE) then
  begin
    AttrOwner := TDOMAttr(arg).ownerElement;
    if Assigned(AttrOwner) and (AttrOwner <> FOwner) then
      Result := INUSE_ATTRIBUTE_ERR;
  end;
end;

function TDOMNamedNodeMap.SetNamedItem(arg: TDOMNode): TDOMNode;
var
  i: Cardinal;
  Exists: Boolean;
  res: Integer;
begin
  res := ValidateInsert(arg);
  if res <> 0 then
    raise EDOMError.Create(res, 'NamedNodeMap.SetNamedItem');

  if FNodeType = ATTRIBUTE_NODE then
  begin
    TDOMAttr(arg).FOwnerElement := TDOMElement(FOwner);
    Exists := Find(TDOMAttr(arg).Name, i); // optimization
  end
  else
    Exists := Find(arg.NodeName, i);

  if Exists then
  begin
    Result := TDOMNode(FList.List^[i]);
    if FNodeType = ATTRIBUTE_NODE then
      TDOMAttr(Result).FOwnerElement := nil;
    FList.List^[i] := arg;
    exit;
  end;
  FList.Insert(i, arg);
  Result := nil;
end;

function TDOMNamedNodeMap.SetNamedItemNS(arg: TDOMNode): TDOMNode;
var
  res: Integer;
begin
  // TODO: implement TDOMNamedNodeMap.SetNamedItemNS
  res := ValidateInsert(arg);
  if res <> 0 then
    raise EDOMError.Create(res, 'NamedNodeMap.SetNamedItemNS');

    Result := nil;
end;

function TDOMNamedNodeMap.InternalRemove(const name: DOMString): TDOMNode;
var
  i: Cardinal;
begin
  Result := nil;
  if Find(name, i) then
  begin
    Result := TDOMNode(FList.List^[i]);
    FList.Delete(I);
    if Result.NodeType = ATTRIBUTE_NODE then
      TDOMAttr(Result).FOwnerElement := nil;
  end;
end;

function TDOMNamedNodeMap.RemoveNamedItem(const name: DOMString): TDOMNode;
begin
  if nfReadOnly in FOwner.FFlags then
    raise EDOMError.Create(NO_MODIFICATION_ALLOWED_ERR, 'NamedNodeMap.RemoveNamedItem');
  Result := InternalRemove(name);
  if Result = nil then
    raise EDOMNotFound.Create('NamedNodeMap.RemoveNamedItem');
end;

function TDOMNamedNodeMap.RemoveNamedItemNS(const namespaceURI, localName: DOMString): TDOMNode;
begin
  if nfReadOnly in FOwner.FFlags then
    raise EDOMError.Create(NO_MODIFICATION_ALLOWED_ERR, 'NamedNodeMap.RemoveNamedItemNS');
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

function TDOMCharacterData.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMCharacterData.SetNodeValue(const AValue: DOMString);
begin
  Changing;
  FNodeValue := AValue;
end;

function TDOMCharacterData.SubstringData(offset, count: LongWord): DOMString;
begin
  if offset > Length then
    raise EDOMIndexSize.Create('CharacterData.SubstringData');
  Result := Copy(FNodeValue, offset + 1, count);
end;

procedure TDOMCharacterData.AppendData(const arg: DOMString);
begin
  Changing;
  FNodeValue := FNodeValue + arg;
end;

procedure TDOMCharacterData.InsertData(offset: LongWord; const arg: DOMString);
begin
  Changing;
  if offset > Length then
    raise EDOMIndexSize.Create('CharacterData.InsertData');
  Insert(arg, FNodeValue, offset+1);
end;

procedure TDOMCharacterData.DeleteData(offset, count: LongWord);
begin
  Changing;
  if offset > Length then
    raise EDOMIndexSize.Create('CharacterData.DeleteData');
  Delete(FNodeValue, offset+1, count);
end;

procedure TDOMCharacterData.ReplaceData(offset, count: LongWord; const arg: DOMString);
begin
  DeleteData(offset, count);
  InsertData(offset, arg);
end;


// -------------------------------------------------------
//   DocumentFragmet
// -------------------------------------------------------

function TDOMDocumentFragment.GetNodeType: Integer;
begin
  Result := DOCUMENT_FRAGMENT_NODE;
end;

function TDOMDocumentFragment.GetNodeName: DOMString;
begin
  Result := '#document-fragment';
end;

function TDOMDocumentFragment.CloneNode(deep: Boolean; aCloneOwner: TDOMDocument): TDOMNode;
begin
  Result := aCloneOwner.CreateDocumentFragment;
  if deep then
    CloneChildren(Result, aCloneOwner);
end;

// -------------------------------------------------------
//   DOMImplementation
// -------------------------------------------------------

{ if nsIdx = -1, checks only the name. Otherwise additionally checks if the prefix is
  valid for standard namespace specified by nsIdx. 
  Non-negative return value is Pos(':', QName), negative is DOM error code. }
function CheckQName(const QName: DOMString; nsIdx: Integer; Xml11: Boolean): Integer;
var
  I, L: Integer;
begin
  if not IsXmlName(QName, Xml11) then
  begin
    Result := -INVALID_CHARACTER_ERR;
    Exit;
  end;

  L := Length(QName);
  Result := Pos(WideChar(':'), QName);
  if Result > 0 then
  begin
    for I := Result+1 to L-1 do  // check for second colon (Use IndexWord?)
      if QName[I] = ':' then
      begin
        Result := -NAMESPACE_ERR;
        Exit;
      end;
    // Name validity has already been checked by IsXmlName() call above.  
    // So just check that colon isn't first or last char, and that it is follwed by NameStartChar.
    if ((Result = 1) or (Result = L) or not IsXmlName(@QName[Result+1], 1, Xml11)) then
    begin
      Result := -NAMESPACE_ERR;
      Exit;
    end;
  end;
  if nsIdx < 0 then Exit;
  // QName contains prefix, but no namespace
  if ((nsIdx = 0) and (Result > 0)) or
  // Bad usage of 'http://www.w3.org/2000/xmlns/'
  ((((L = 5) or (Result = 6)) and (Pos(WideString('xmlns'), QName) = 1)) <> (nsIdx = 2)) or
  // Bad usage of 'http://www.w3.org/XML/1998/namespace'
  ((Result = 4) and (Pos(WideString('xml'), QName) = 1) and (nsIdx <> 1)) then
    Result := -NAMESPACE_ERR;
end;

function TDOMImplementation.HasFeature(const feature, version: DOMString):
  Boolean;
var
  s: string;
begin
  s := feature;   // force Ansi, features do not contain non-ASCII chars
  Result := SameText(s, 'XML') and ((version = '') or (version = '1.0'));
end;

function TDOMImplementation.CreateDocumentType(const QualifiedName, PublicID,
  SystemID: DOMString): TDOMDocumentType;
begin
  // DONE: Implemented
  Result := TDOMDocumentType.Create(nil);
  Result.FName := QualifiedName;

  // DOM does not restrict PublicID without SystemID (unlike XML spec)
  Result.FPublicID := PublicID;
  Result.FSystemID := SystemID;
end;

function TDOMImplementation.CreateDocument(const NamespaceURI,
  QualifiedName: DOMString; doctype: TDOMDocumentType): TDOMDocument;
var
  Root: TDOMNode;
begin
  // TODO: This method is not usable yet due to CreateElementNS...
  Result := TXMLDocument.Create;
  Result.FImplementation := Self;
  try
    if Assigned(doctype) then
    begin
      if Assigned(doctype.OwnerDocument) then
        raise EDOMWrongDocument.Create('Implementation.CreateDocument');
      Doctype.FOwnerDocument := Result;
      Result.AppendChild(doctype);
    end;
    Root := Result.CreateElementNS(NamespaceURI, QualifiedName);
    Result.AppendChild(Root);
  except
    Result.Free;
    raise;
  end;
end;


// -------------------------------------------------------
//   Document
// -------------------------------------------------------

constructor TDOMDocument.Create;
begin
  inherited Create(nil);
  FOwnerDocument := Self;
  FNames := THashTable.Create(256, True);
  SetLength(FNamespaces, 3);
  // Namespace #0 should always be an empty string
  FNamespaces[1] := stduri_xml;
  FNamespaces[2] := stduri_xmlns;
end;

destructor TDOMDocument.Destroy;
begin
  FreeAndNil(FIDList);   // set to nil before starting destroying chidlren
  inherited Destroy;
  FNames.Free;
end;

function TDOMDocument.AddID(Attr: TDOMAttr): Boolean;
var
  ID: DOMString;
  Exists: Boolean;
  p: PHashItem;
begin
  if FIDList = nil then
    FIDList := THashTable.Create(256, False);

  ID := Attr.Value;
  p := FIDList.FindOrAdd(DOMPChar(ID), Length(ID), Exists);
  if not Exists then
  begin
    p^.Data := Attr.OwnerElement;
    Result := True;
  end
  else
    Result := False;
end;

// This shouldn't be called if document has no IDs,
// or when it is being destroyed
// TODO: This could be much faster if removing ID happens
// upon modification of corresponding attribute value.

type
  TempRec = record
    Element: TDOMElement;
    Entry: PHashItem;
  end;

function CheckID(Entry: PHashItem; arg: Pointer): Boolean;
begin
  if Entry^.Data = TempRec(arg^).Element then
  begin
    TempRec(arg^).Entry := Entry;
    Result := False;
  end
  else
    Result := True;
end;

procedure TDOMDocument.RemoveID(Elem: TDOMElement);
var
  hr: TempRec;
begin
  hr.Element := Elem;
  hr.Entry := nil;
  FIDList.ForEach(@CheckID, @hr);
  if Assigned(hr.Entry) then
    FIDList.Remove(hr.Entry);
end;

function TDOMDocument.GetNodeType: Integer;
begin
  Result := DOCUMENT_NODE;
end;

function TDOMDocument.GetNodeName: DOMString;
begin
  Result := '#document';
end;

function TDOMDocument.GetTextContent: DOMString;
begin
  Result := '';
end;

procedure TDOMDocument.SetTextContent(const value: DOMString);
begin
  // Document ignores setting TextContent
end;

function TDOMDocument.GetOwnerDocument: TDOMDocument;
begin
  Result := nil;
end;

function TDOMDocument.GetDocumentElement: TDOMElement;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) and (node.NodeType <> ELEMENT_NODE) do
    node := node.NextSibling;
  Result := TDOMElement(node);
end;

function TDOMDocument.GetDocType: TDOMDocumentType;
var
  node: TDOMNode;
begin
  node := FFirstChild;
  while Assigned(node) and (node.NodeType <> DOCUMENT_TYPE_NODE) do
    node := node.NextSibling;
  Result := TDOMDocumentType(node);
end;

function TDOMDocument.CreateElement(const tagName: DOMString): TDOMElement;
begin
  if not IsXmlName(tagName, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'DOMDocument.CreateElement');
  Result := TDOMElement.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(tagName), Length(tagName));
  // TODO: attach default attributes
end;

function TDOMDocument.CreateElementBuf(Buf: DOMPChar; Length: Integer): TDOMElement;
begin
  Result := TDOMElement.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(Buf, Length);
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

function TDOMDocument.CreateTextNodeBuf(Buf: DOMPChar; Length: Integer; IgnWS: Boolean): TDOMText;
begin
  Result := TDOMText.Create(Self);
  SetString(Result.FNodeValue, Buf, Length);
  if IgnWS then
    Include(Result.FFlags, nfIgnorableWS);
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
  if not IsXmlName(name, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'DOMDocument.CreateAttribute');
  Result := TDOMAttr.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(name), Length(name));
  Include(Result.FFlags, nfSpecified);
end;

function TDOMDocument.CreateAttributeBuf(Buf: DOMPChar; Length: Integer): TDOMAttr;
begin
  Result := TDOMAttr.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(buf, Length);
  Include(Result.FFlags, nfSpecified);
end;

function TDOMDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
begin
  raise EDOMNotSupported.Create('DOMDocument.CreateEntityReference');
  Result:=nil;
end;

function TDOMDocument.GetElementsByTagName(const tagname: DOMString): TDOMNodeList;
begin
  Result := TDOMElementList.Create(Self, tagname);
end;

function TDOMDocument.GetElementsByTagNameNS(const nsURI, aLocalName: DOMString): TDOMNodeList;
begin
  Result := TDOMElementList.Create(Self, nsURI, aLocalName);
end;

function TDOMDocument.CreateAttributeNS(const nsURI,
  QualifiedName: DOMString): TDOMAttr;
begin
  // TODO: Implement TDOMDocument.CreateAttributeNS
  raise EDOMNotSupported.Create('TDOMDocument.CreateAttributeNS');
  Result := nil;
end;

function TDOMDocument.CreateElementNS(const nsURI,
  QualifiedName: DOMString): TDOMElement;
begin
  // TODO: Implement TDOMDocument.CreateElementNS
  raise EDOMNotSupported.Create('TDOMDocument.CreateElementNS');
  Result := nil;
end;

function TDOMDocument.GetElementById(const ElementID: DOMString): TDOMElement;
begin
  Result := nil;
  if Assigned(FIDList) then
    Result := TDOMElement(FIDList.Get(DOMPChar(ElementID), Length(ElementID)));
end;

function TDOMDocument.ImportNode(ImportedNode: TDOMNode;
  Deep: Boolean): TDOMNode;
begin
  Result := ImportedNode.CloneNode(Deep, Self);
end;

function TDOMDocument.IndexOfNS(const nsURI: DOMString; AddIfAbsent: Boolean): Integer;
var
  I: Integer;
begin
  // TODO: elaborate implementation
  for I := 0 to Length(FNamespaces)-1 do
    if FNamespaces[I] = nsURI then
    begin
      Result := I;
      Exit;
    end;
  if AddIfAbsent then
  begin
    Result := Length(FNamespaces);
    SetLength(FNamespaces, Result+1);
    FNamespaces[Result] := nsURI;
  end
  else
    Result := -1;
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
  if not IsXmlName(target, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'XMLDocument.CreateProcessingInstruction');
  Result := TDOMProcessingInstruction.Create(Self);
  Result.FTarget := target;
  Result.FNodeValue := data;
end;

function TXMLDocument.CreateEntityReference(const name: DOMString):
  TDOMEntityReference;
var
  dType: TDOMDocumentType;
  ent: TDOMEntity;
begin
  if not IsXmlName(name, FXML11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'XMLDocument.CreateEntityReference');
  Result := TDOMEntityReference.Create(Self);
  Result.FName := name;
  dType := DocType;
  if Assigned(dType) then
  begin
    TDOMNode(ent) := dType.Entities.GetNamedItem(name);
    if Assigned(ent) then
      ent.CloneChildren(Result, Self);
  end;
  Result.SetReadOnly(True);
end;

procedure TXMLDocument.SetXMLVersion(const aValue: DOMString);
begin
  FXMLVersion := aValue;
  FXML11 := (aValue = '1.1');
end;

{ TDOMNode_NS }

function TDOMNode_NS.GetNodeName: DOMString;
begin
  // Because FNSI.QName is not set by the TDOMNode itself, but is set by
  // other classes/functions, it is necessary to check if FNSQ.QName is
  // assigned.
  if assigned(FNSI.QName) then
    Result := FNSI.QName^.Key
  else
    Result := '';
end;

function TDOMNode_NS.GetLocalName: DOMString;
begin
  if nfLevel2 in FFlags then
    Result := Copy(FNSI.QName^.Key, FNSI.PrefixLen+1, MaxInt)
  else
    Result := '';
end;

function TDOMNode_NS.GetNamespaceURI: DOMString;
begin
  Result := FOwnerDocument.FNamespaces[FNSI.NSIndex];
end;

function TDOMNode_NS.GetPrefix: DOMString;
begin
  if FNSI.PrefixLen < 2 then
    Result := ''
  else
    Result := Copy(FNSI.QName^.Key, 1, FNSI.PrefixLen-1);
end;

procedure TDOMNode_NS.SetPrefix(const Value: DOMString);
var
  NewName: DOMString;
begin
  Changing;
  if not IsXmlName(Value, FOwnerDocument.FXml11) then
    raise EDOMError.Create(INVALID_CHARACTER_ERR, 'Node.SetPrefix');

  if (Pos(WideChar(':'), Value) > 0) or not (nfLevel2 in FFlags) or
    ((Value = 'xml') and (FNSI.NSIndex <> 1)) or
    ((ClassType = TDOMAttr) and  // BAD!
    ((Value = 'xmlns') and (FNSI.NSIndex <> 2)) or (FNSI.QName^.Key = 'xmlns')) then
    raise EDOMNamespace.Create('Node.SetPrefix');

  // TODO: rehash properly
  NewName := Value + ':' + Copy(FNSI.QName^.Key, FNSI.PrefixLen+1, MaxInt);
  FNSI.QName := FOwnerDocument.FNames.FindOrAdd(DOMPChar(NewName), Length(NewName));
  FNSI.PrefixLen := Length(Value)+1;
end;

function TDOMNode_NS.CompareName(const AName: DOMString): Integer;
begin
  Result := CompareDOMStrings(DOMPChar(AName), DOMPChar(NodeName), Length(AName), Length(NodeName));
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
  // Cloned attribute is always specified and carries its children
  Result := ACloneOwner.CreateAttribute(NodeName);
  TDOMAttr(Result).FDataType := FDataType;
  CloneChildren(Result, ACloneOwner);
end;

function TDOMAttr.GetNodeValue: DOMString;
begin
  Result := GetTextContent;
  if FDataType <> dtCdata then
    NormalizeSpaces(Result);
end;

procedure TDOMAttr.SetNodeValue(const AValue: DOMString);
begin
  SetTextContent(AValue);
  Include(FFlags, nfSpecified);
end;

function TDOMAttr.GetSpecified: Boolean;
begin
  Result := nfSpecified in FFlags;
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
  if Assigned(FOwnerDocument.FIDList) then
    FOwnerDocument.RemoveID(Self);
  // FIX: Attribute nodes are now freed by TDOMNamedNodeMap.Destroy
  FreeAndNil(FAttributes);
  inherited Destroy;
end;

function TDOMElement.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
var
  i: Integer;
begin
  Result := ACloneOwner.CreateElement(NodeName);
  if Assigned(FAttributes) then
  begin
    for i := 0 to FAttributes.Length - 1 do
      TDOMElement(Result).SetAttributeNode(TDOMAttr(FAttributes[i].CloneNode(True, ACloneOwner)));
  end;
  if deep then
    CloneChildren(Result, ACloneOwner);
end;

procedure TDOMElement.Normalize;
var
  I: Integer;
begin
  if Assigned(FAttributes) then
    for I := 0 to FAttributes.Length - 1 do
      FAttributes[I].Normalize;
  inherited Normalize;    
end;

function TDOMElement.GetAttributes: TDOMNamedNodeMap;
begin
  if FAttributes=nil then
    FAttributes := TDOMNamedNodeMap.Create(Self, ATTRIBUTE_NODE);
  Result := FAttributes;
end;

function TDOMElement.GetAttribute(const name: DOMString): DOMString;
var
  Attr: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FAttributes) then
  begin
    Attr := FAttributes.GetNamedItem(name);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

function TDOMElement.GetAttributeNS(const nsURI, aLocalName: DOMString): DOMString;
var
  Attr: TDOMNode;
begin
  SetLength(Result, 0);
  if Assigned(FAttributes) then
  begin
    Attr := FAttributes.GetNamedItemNS(nsURI, aLocalName);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

procedure TDOMElement.SetAttribute(const name, value: DOMString);
var
  I: Cardinal;
  attr: TDOMAttr;
begin
  Changing;
  if Attributes.Find(name, I) then
    Attr := FAttributes[I] as TDOMAttr
  else
  begin
    Attr := FOwnerDocument.CreateAttribute(name);
    Attr.FOwnerElement := Self;
    FAttributes.FList.Insert(I, Attr);
  end;
  attr.NodeValue := value;
end;

procedure TDOMElement.RemoveAttribute(const name: DOMString);
begin
  Changing;
// (note) NamedNodeMap.RemoveNamedItem can raise NOT_FOUND_ERR and we should not.
  if Assigned(FAttributes) then
    FAttributes.InternalRemove(name).Free;
end;

procedure TDOMElement.RemoveAttributeNS(const nsURI,
  aLocalName: DOMString);
begin
  Changing;
  // TODO: Implement TDOMElement.RemoveAttributeNS
  raise EDOMNotSupported.Create('TDOMElement.RemoveAttributeNS');
end;

procedure TDOMElement.SetAttributeNS(const nsURI, qualifiedName,
  value: DOMString);
var
  Attr: TDOMAttr;
begin
  Attr := Attributes.GetNamedItemNS(nsURI, qualifiedName) as TDOMAttr;
  if attr = nil then
  begin
    attr := FOwnerDocument.CreateAttributeNS(nsURI, qualifiedName);
    // TODO 5: keep sorted!
    FAttributes.FList.Add(attr);
  end;
  attr.NodeValue := value;
end;

function TDOMElement.GetAttributeNode(const name: DOMString): TDOMAttr;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.GetNamedItem(name) as TDOMAttr
  else
    Result := nil;
end;

function TDOMElement.GetAttributeNodeNS(const nsURI, aLocalName: DOMString): TDOMAttr;
begin
  if Assigned(FAttributes) then
    Result := FAttributes.GetNamedItemNS(nsURI, aLocalName) as TDOMAttr
  else
    Result := nil;
end;

function TDOMElement.SetAttributeNode(NewAttr: TDOMAttr): TDOMAttr;
begin
  Result := Attributes.SetNamedItem(NewAttr) as TDOMAttr;

  // TODO -cConformance: here goes inconsistency with DOM 2 - same as in TDOMNode.RemoveChild
  if Assigned(Result) and (Result <> NewAttr) then
  begin
    Result.Free;
    Result := nil;
  end;  
end;

function TDOMElement.SetAttributeNodeNS(NewAttr: TDOMAttr): TDOMAttr;
begin
  Result := Attributes.SetNamedItemNS(NewAttr) as TDOMAttr;

  // TODO -cConformance: here goes inconsistency with DOM 2 - same as in TDOMNode.RemoveChild
  if Assigned(Result) and (Result <> NewAttr) then
  begin
    Result.Free;
    Result := nil;
  end;  
end;


function TDOMElement.RemoveAttributeNode(OldAttr: TDOMAttr): TDOMAttr;
begin
  Changing;
  Result:=nil;
  // TODO: DOM 2: must raise NOT_FOUND_ERR if OldAttr is not ours.
  //       -- but what is the purpose of return value then?
  // TODO: delegate to TNamedNodeMap?  Nope, it does not have such method
  // (note) one way around is to remove by name
  if Assigned(FAttributes) and (FAttributes.FList.Remove(OldAttr) > -1) then
  begin
    Result := OldAttr;
  end
  else
    raise EDOMNotFound.Create('Element.RemoveAttributeNode');
end;

function TDOMElement.GetElementsByTagName(const name: DOMString): TDOMNodeList;
begin
  Result := TDOMElementList.Create(Self, name);
end;

function TDOMElement.GetElementsByTagNameNS(const nsURI, aLocalName: DOMString): TDOMNodeList;
begin
  Result := TDOMElementList.Create(Self, nsURI, aLocalName);
end;

function TDOMElement.hasAttribute(const name: DOMString): Boolean;
begin
  Result := Assigned(FAttributes) and
    Assigned(FAttributes.GetNamedItem(name));
end;

function TDOMElement.hasAttributeNS(const nsURI, aLocalName: DOMString): Boolean;
begin
  Result := Assigned(FAttributes) and
    Assigned(FAttributes.getNamedItemNS(nsURI, aLocalName));
end;

function TDOMElement.HasAttributes: Boolean;
begin
  Result := Assigned(FAttributes) and (FAttributes.Length > 0);
end;

// -------------------------------------------------------
//   Text
// -------------------------------------------------------

function TDOMText.GetNodeType: Integer;
begin
  Result := TEXT_NODE;
end;

function TDOMText.GetNodeName: DOMString;
begin
  Result := '#text';
end;

procedure TDOMText.SetNodeValue(const aValue: DOMString);
begin
  inherited SetNodeValue(aValue);
  // TODO: may analyze aValue, but this will slow things down...
  Exclude(FFlags, nfIgnorableWS);
end;

function TDOMText.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateTextNode(FNodeValue);
end;

function TDOMText.SplitText(offset: LongWord): TDOMText;
begin
  if offset > Length then
    raise EDOMIndexSize.Create('Text.SplitText');

  Result := TDOMText.Create(FOwnerDocument);
  Result.FNodeValue := Copy(FNodeValue, offset + 1, Length);
  Result.FFlags := FFlags * [nfIgnorableWS];
  FNodeValue := Copy(FNodeValue, 1, offset);
  if Assigned(FParentNode) then
    FParentNode.InsertBefore(Result, FNextSibling);
end;

function TDOMText.IsElementContentWhitespace: Boolean;
begin
  Result := nfIgnorableWS in FFlags;
end;

// -------------------------------------------------------
//   Comment
// -------------------------------------------------------

function TDOMComment.GetNodeType: Integer;
begin
  Result := COMMENT_NODE;
end;

function TDOMComment.GetNodeName: DOMString;
begin
  Result := '#comment';
end;

function TDOMComment.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateComment(FNodeValue);
end;


// -------------------------------------------------------
//   CDATASection
// -------------------------------------------------------

function TDOMCDATASection.GetNodeType: Integer;
begin
  Result := CDATA_SECTION_NODE;
end;

function TDOMCDATASection.GetNodeName: DOMString;
begin
  Result := '#cdata-section';
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

function TDOMDocumentType.GetNodeName: DOMString;
begin
  Result := FName;
end;

destructor TDOMDocumentType.Destroy;
begin
  FEntities.Free;
  FNotations.Free;
  inherited Destroy;
end;

function TDOMDocumentType.GetEntities: TDOMNamedNodeMap;
begin
  if FEntities = nil then
    FEntities := TDOMNamedNodeMap.Create(Self, ENTITY_NODE);
  Result := FEntities;
end;

function TDOMDocumentType.GetNotations: TDOMNamedNodeMap;
begin
  if FNotations = nil then
    FNotations := TDOMNamedNodeMap.Create(Self, NOTATION_NODE);
  Result := FNotations;
end;

// -------------------------------------------------------
//   Notation
// -------------------------------------------------------

function TDOMNotation.GetNodeType: Integer;
begin
  Result := NOTATION_NODE;
end;

function TDOMNotation.GetNodeName: DOMString;
begin
  Result := FName;
end;

function TDOMNotation.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMNotation.Create(ACloneOwner);
  TDOMNotation(Result).FName := FName;
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

function TDOMEntity.GetNodeName: DOMString;
begin
  Result := FName;
end;

function TDOMEntity.CloneNode(deep: Boolean; aCloneOwner: TDOMDocument): TDOMNode;
begin
  Result := TDOMEntity.Create(aCloneOwner);
  TDOMEntity(Result).FName := FName;
  TDOMEntity(Result).FSystemID := FSystemID;
  TDOMEntity(Result).FPublicID := FPublicID;
  TDOMEntity(Result).FNotationName := FNotationName;
  if deep then
    CloneChildren(Result, aCloneOwner);
  Result.SetReadOnly(True);
end;

// -------------------------------------------------------
//   EntityReference
// -------------------------------------------------------

function TDOMEntityReference.GetNodeType: Integer;
begin
  Result := ENTITY_REFERENCE_NODE;
end;

function TDOMEntityReference.GetNodeName: DOMString;
begin
  Result := FName;
end;

function TDOMEntityReference.CloneNode(deep: Boolean; ACloneOwner: TDOMDocument): TDOMNode;
begin
  Result := ACloneOwner.CreateEntityReference(FName);
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

function TDOMProcessingInstruction.GetNodeName: DOMString;
begin
  Result := FTarget;
end;

function TDOMProcessingInstruction.GetNodeValue: DOMString;
begin
  Result := FNodeValue;
end;

procedure TDOMProcessingInstruction.SetNodeValue(const AValue: DOMString);
begin
  Changing;
  FNodeValue := AValue;
end;

end.
