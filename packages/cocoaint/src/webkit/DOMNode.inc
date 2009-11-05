{ Parsed from Webkit.framework DOMNode.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMNODE_PAS_T}
{$define DOMNODE_PAS_T}

{ Constants }

const
  DOM_ELEMENT_NODE = 1;
  DOM_ATTRIBUTE_NODE = 2;
  DOM_TEXT_NODE = 3;
  DOM_CDATA_SECTION_NODE = 4;
  DOM_ENTITY_REFERENCE_NODE = 5;
  DOM_ENTITY_NODE = 6;
  DOM_PROCESSING_INSTRUCTION_NODE = 7;
  DOM_COMMENT_NODE = 8;
  DOM_DOCUMENT_NODE = 9;
  DOM_DOCUMENT_TYPE_NODE = 10;
  DOM_DOCUMENT_FRAGMENT_NODE = 11;
  DOM_NOTATION_NODE = 12;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMNODE_PAS_R}
{$define DOMNODE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMNODE_PAS_F}
{$define DOMNODE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMNODE_PAS_S}
{$define DOMNODE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMNode = objcclass;
  DOMNodePointer = ^DOMNode;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNODE_PAS_C}
{$define DOMNODE_PAS_C}

{ DOMNode }
  DOMNode = objcclass(DOMObject)
    
  public
    class function alloc: DOMNode; message 'alloc';

    function nodeName: NSString; message 'nodeName';
    procedure setNodeValue (newValue: NSString); message 'setNodeValue:';
    function nodeValue: NSString; message 'nodeValue';
    function nodeType: cushort; message 'nodeType';
    function parentNode: DOMNode; message 'parentNode';
    function childNodes: DOMNodeList; message 'childNodes';
    function firstChild: DOMNode; message 'firstChild';
    function lastChild: DOMNode; message 'lastChild';
    function previousSibling: DOMNode; message 'previousSibling';
    function nextSibling: DOMNode; message 'nextSibling';
    function attributes: DOMNamedNodeMap; message 'attributes';
    function ownerDocument: DOMDocument; message 'ownerDocument';
    function namespaceURI: NSString; message 'namespaceURI';
    procedure setPrefix (newValue: NSString); message 'setPrefix:';
    function prefix: NSString; message 'prefix';
    function localName: NSString; message 'localName';
    procedure setTextContent (newValue: NSString); message 'setTextContent:';
    function textContent: NSString; message 'textContent';
    function insertBefore_refChild(newChild: DOMNode; refChild: DOMNode): DOMNode; message 'insertBefore:refChild:';
    function replaceChild_oldChild(newChild: DOMNode; oldChild: DOMNode): DOMNode; message 'replaceChild:oldChild:';
    function removeChild(oldChild: DOMNode): DOMNode; message 'removeChild:';
    function appendChild(newChild: DOMNode): DOMNode; message 'appendChild:';
    function hasChildNodes: Boolean; message 'hasChildNodes';
    function cloneNode(deep: Boolean): DOMNode; message 'cloneNode:';
    procedure normalize; message 'normalize';
    function isSupported_version(feature: NSString; version_: NSString): Boolean; message 'isSupported:version:';
    function hasAttributes: Boolean; message 'hasAttributes';
    function isSameNode(other: DOMNode): Boolean; message 'isSameNode:';
    function isEqualNode(other: DOMNode): Boolean; message 'isEqualNode:';

    { Category: DOMNodeDeprecated }
    function insertBefore(newChild: DOMNode; refChild: DOMNode): DOMNode; message 'insertBefore:newChild:';
    function replaceChild(newChild: DOMNode; oldChild: DOMNode): DOMNode; message 'replaceChild:newChild:';
    function isSupported(feature: NSString; version_: NSString): Boolean; message 'isSupported:feature:';

    { Category: DOMNodeExtensions }
    function boundingBox: NSRect; message 'boundingBox';
    function lineBoxRects: NSArray; message 'lineBoxRects';

    { Category: DOMEventTarget }
  end; external;

{$endif}
{$endif}
