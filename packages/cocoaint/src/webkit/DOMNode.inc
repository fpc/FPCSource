{ Parsed from WebKit.framework DOMNode.h }


{$ifdef TYPES}
type
  DOMNodePtr = ^DOMNode;
{$endif}

{$ifdef TYPES}
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
  DOM_DOCUMENT_POSITION_DISCONNECTED = $01;
  DOM_DOCUMENT_POSITION_PRECEDING = $02;
  DOM_DOCUMENT_POSITION_FOLLOWING = $04;
  DOM_DOCUMENT_POSITION_CONTAINS = $08;
  DOM_DOCUMENT_POSITION_CONTAINED_BY = $10;
  DOM_DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = $20;
{$endif}

{$ifdef CLASSES}

type
  DOMNode = objcclass external (DOMObject, DOMEventTargetProtocol)
  public
    function nodeName: NSString; message 'nodeName';
    procedure setNodeValue(newValue: NSString); message 'setNodeValue:';
    function nodeValue: NSString; message 'nodeValue';
    function nodeType: cushort; message 'nodeType';
    function parentNode: DOMNode; message 'parentNode';
    function childNodes: DOMNodeList; message 'childNodes';
    function firstChild: DOMNode; message 'firstChild';
    function lastChild: DOMNode; message 'lastChild';
    function previousSibling: DOMNode; message 'previousSibling';
    function nextSibling: DOMNode; message 'nextSibling';
    function ownerDocument: DOMDocument; message 'ownerDocument';
    function namespaceURI: NSString; message 'namespaceURI';
    procedure setPrefix(newValue: NSString); message 'setPrefix:';
    function prefix: NSString; message 'prefix';
    function localName: NSString; message 'localName';
    function attributes: DOMNamedNodeMap; message 'attributes';
    function baseURI: NSString; message 'baseURI';
    procedure setTextContent(newValue: NSString); message 'setTextContent:';
    function textContent: NSString; message 'textContent';
    function parentElement: DOMElement; message 'parentElement';
    function isContentEditable: ObjCBOOL; message 'isContentEditable';
    function insertBefore_refChild (newChild: DOMNode; refChild: DOMNode): DOMNode; message 'insertBefore:refChild:'; { available in 10_5 }
    function replaceChild_oldChild (newChild: DOMNode; oldChild: DOMNode): DOMNode; message 'replaceChild:oldChild:'; { available in 10_5 }
    function removeChild (oldChild: DOMNode): DOMNode; message 'removeChild:';
    function appendChild (newChild: DOMNode): DOMNode; message 'appendChild:';
    function hasChildNodes: ObjCBOOL; message 'hasChildNodes';
    function cloneNode (deep: ObjCBOOL): DOMNode; message 'cloneNode:';
    procedure normalize; message 'normalize';
    function isSupported_version (feature: NSString; version: NSString): ObjCBOOL; message 'isSupported:version:'; { available in 10_5 }
    function hasAttributes: ObjCBOOL; message 'hasAttributes';
    function isSameNode (other: DOMNode): ObjCBOOL; message 'isSameNode:'; { available in 10_5 }
    function isEqualNode (other: DOMNode): ObjCBOOL; message 'isEqualNode:'; { available in 10_5 }
    function lookupPrefix (namespaceURI_: NSString): NSString; message 'lookupPrefix:'; { available in 10_5 }
    function isDefaultNamespace (namespaceURI_: NSString): ObjCBOOL; message 'isDefaultNamespace:'; { available in 10_5 }
    function lookupNamespaceURI (prefix_: NSString): NSString; message 'lookupNamespaceURI:'; { available in 10_5 }
    function compareDocumentPosition (other: DOMNode): cushort; message 'compareDocumentPosition:'; { available in 10_6 }
    function contains (other: DOMNode): ObjCBOOL; message 'contains:'; { available in 10_5 }

    { Adopted protocols }
    procedure addEventListener_listener (type_: NSString; useCapture: ObjCBOOL); message 'addEventListener:listener:'; deprecated 'in 10_4, 10_5';
    procedure addEventListener_listener_useCapture (type_: NSString; listener: DOMEventListenerProtocol; useCapture: ObjCBOOL); message 'addEventListener:listener:useCapture:'; { available in 10_5 }
    function dispatchEvent (event: DOMEvent): ObjCBOOL; message 'dispatchEvent:';
    procedure removeEventListener_listener (type_: NSString; useCapture: ObjCBOOL); message 'removeEventListener:listener:'; deprecated 'in 10_4, 10_5';
    procedure removeEventListener_listener_useCapture (type_: NSString; listener: DOMEventListenerProtocol; useCapture: ObjCBOOL); message 'removeEventListener:listener:useCapture:'; { available in 10_5 }
  end;


type
  DOMNodeDeprecated = objccategory external (DOMNode)
    function insertBefore (newChild: DOMNode): DOMNode; message 'insertBefore:'; deprecated 'in 10_4, 10_5';
    function replaceChild (newChild: DOMNode): DOMNode; message 'replaceChild:'; deprecated 'in 10_4, 10_5';
    function isSupported (feature: NSString): ObjCBOOL; message 'isSupported:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

