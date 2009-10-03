{ Parsed from Webkit.framework DOMElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMELEMENT_PAS_T}
{$define DOMELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMELEMENT_PAS_R}
{$define DOMELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMELEMENT_PAS_F}
{$define DOMELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMELEMENT_PAS_S}
{$define DOMELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMElement = objcclass;
  DOMElementPointer = ^DOMElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMELEMENT_PAS_C}
{$define DOMELEMENT_PAS_C}

{ DOMElement }
  DOMElement = objcclass(DOMNode)
    
  public
    class function alloc: DOMElement; message 'alloc';

    function tagName: NSString; message 'tagName';
    function style: DOMCSSStyleDeclaration; message 'style';
    function offsetLeft: cint; message 'offsetLeft';
    function offsetTop: cint; message 'offsetTop';
    function offsetWidth: cint; message 'offsetWidth';
    function offsetHeight: cint; message 'offsetHeight';
    function offsetParent: DOMElement; message 'offsetParent';
    function clientWidth: cint; message 'clientWidth';
    function clientHeight: cint; message 'clientHeight';
    function scrollWidth: cint; message 'scrollWidth';
    function scrollHeight: cint; message 'scrollHeight';
    function getAttribute(name: NSStringPointer): NSString; message 'getAttribute:';
    procedure setAttribute_value(name: NSStringPointer; value: NSStringPointer); message 'setAttribute:value:';
    procedure removeAttribute(name: NSStringPointer); message 'removeAttribute:';
    function getAttributeNode(name: NSStringPointer): DOMAttr; message 'getAttributeNode:';
    function setAttributeNode(newAttr: DOMAttr): DOMAttr; message 'setAttributeNode:';
    function removeAttributeNode(oldAttr: DOMAttr): DOMAttr; message 'removeAttributeNode:';
    function getElementsByTagName(name: NSStringPointer): DOMNodeList; message 'getElementsByTagName:';
    function getAttributeNS_localName(namespaceURI_: NSStringPointer; localName_: NSStringPointer): NSString; message 'getAttributeNS:localName:';
    procedure setAttributeNS_qualifiedName_value(namespaceURI_: NSStringPointer; qualifiedName: NSStringPointer; value: NSStringPointer); message 'setAttributeNS:qualifiedName:value:';
    procedure removeAttributeNS_localName(namespaceURI_: NSStringPointer; localName_: NSStringPointer); message 'removeAttributeNS:localName:';
    function getElementsByTagNameNS_localName(namespaceURI_: NSStringPointer; localName_: NSStringPointer): DOMNodeList; message 'getElementsByTagNameNS:localName:';
    function getAttributeNodeNS_localName(namespaceURI_: NSStringPointer; localName_: NSStringPointer): DOMAttr; message 'getAttributeNodeNS:localName:';
    function setAttributeNodeNS(newAttr: DOMAttr): DOMAttr; message 'setAttributeNodeNS:';
    function hasAttribute(name: NSStringPointer): Boolean; message 'hasAttribute:';
    function hasAttributeNS_localName(namespaceURI_: NSStringPointer; localName_: NSStringPointer): Boolean; message 'hasAttributeNS:localName:';
    procedure focus; message 'focus';
    procedure blur; message 'blur';
    procedure scrollIntoView(alignWithTop: Boolean); message 'scrollIntoView:';
    procedure scrollIntoViewIfNeeded(centerIfNeeded: Boolean); message 'scrollIntoViewIfNeeded:';

    { Category: DOMElementDeprecated }
    procedure setAttribute(name: NSStringPointer; value: NSStringPointer); message 'setAttribute:name:';
    function getAttributeNS(namespaceURI_: NSStringPointer; localName_: NSStringPointer): NSString; message 'getAttributeNS:namespaceURI:';
    procedure setAttributeNS(namespaceURI_: NSStringPointer; qualifiedName: NSStringPointer; value: NSStringPointer); message 'setAttributeNS:namespaceURI:qualifiedName:';
    procedure removeAttributeNS(namespaceURI_: NSStringPointer; localName_: NSStringPointer); message 'removeAttributeNS:namespaceURI:';
    function getElementsByTagNameNS(namespaceURI_: NSStringPointer; localName_: NSStringPointer): DOMNodeList; message 'getElementsByTagNameNS:namespaceURI:';
    function getAttributeNodeNS(namespaceURI_: NSStringPointer; localName_: NSStringPointer): DOMAttr; message 'getAttributeNodeNS:namespaceURI:';
    function hasAttributeNS(namespaceURI_: NSStringPointer; localName_: NSStringPointer): Boolean; message 'hasAttributeNS:namespaceURI:';

    { Category: DOMElementAppKitExtensions }
    function image: NSImage; message 'image';
  end; external;

{$endif}
{$endif}
