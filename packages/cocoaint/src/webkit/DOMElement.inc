{ Parsed from Webkit.framework DOMElement.h }

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
  DOMElementPtr = DOMElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMELEMENT_PAS_C}
{$define DOMELEMENT_PAS_C}

{ DOMElement }
  DOMElement = objcclass external (DOMNode)
    
  public
    function tagName: NSString; message 'tagName';
    function style: DOMCSSStyleDeclaration; message 'style';
    function offsetLeft: cint; message 'offsetLeft';
    function offsetTop: cint; message 'offsetTop';
    function offsetWidth: cint; message 'offsetWidth';
    function offsetHeight: cint; message 'offsetHeight';
    function offsetParent: DOMElement; message 'offsetParent';
    function clientLeft: cint; message 'clientLeft';
    function clientTop: cint; message 'clientTop';
    function clientWidth: cint; message 'clientWidth';
    function clientHeight: cint; message 'clientHeight';
    procedure setScrollLeft (newValue: cint); message 'setScrollLeft:';
    function scrollLeft: cint; message 'scrollLeft';
    procedure setScrollTop (newValue: cint); message 'setScrollTop:';
    function scrollTop: cint; message 'scrollTop';
    function scrollWidth: cint; message 'scrollWidth';
    function scrollHeight: cint; message 'scrollHeight';
    function firstElementChild: DOMElement; message 'firstElementChild';
    function lastElementChild: DOMElement; message 'lastElementChild';
    function previousElementSibling: DOMElement; message 'previousElementSibling';
    function nextElementSibling: DOMElement; message 'nextElementSibling';
    function childElementCount: cuint; message 'childElementCount';
    function innerText: NSString; message 'innerText';
    function getAttribute(name: NSString): NSString; message 'getAttribute:';
    procedure setAttribute_value(name: NSString; value: NSString); message 'setAttribute:value:';
    procedure removeAttribute(name: NSString); message 'removeAttribute:';
    function getAttributeNode(name: NSString): DOMAttr; message 'getAttributeNode:';
    function setAttributeNode(newAttr: DOMAttr): DOMAttr; message 'setAttributeNode:';
    function removeAttributeNode(oldAttr: DOMAttr): DOMAttr; message 'removeAttributeNode:';
    function getElementsByTagName(name: NSString): DOMNodeList; message 'getElementsByTagName:';
    function getAttributeNS_localName(namespaceURI_: NSString; localName_: NSString): NSString; message 'getAttributeNS:localName:';
    procedure setAttributeNS_qualifiedName_value(namespaceURI_: NSString; qualifiedName: NSString; value: NSString); message 'setAttributeNS:qualifiedName:value:';
    procedure removeAttributeNS_localName(namespaceURI_: NSString; localName_: NSString); message 'removeAttributeNS:localName:';
    function getElementsByTagNameNS_localName(namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:localName:';
    function getAttributeNodeNS_localName(namespaceURI_: NSString; localName_: NSString): DOMAttr; message 'getAttributeNodeNS:localName:';
    function setAttributeNodeNS(newAttr: DOMAttr): DOMAttr; message 'setAttributeNodeNS:';
    function hasAttribute(name: NSString): Boolean; message 'hasAttribute:';
    function hasAttributeNS_localName(namespaceURI_: NSString; localName_: NSString): Boolean; message 'hasAttributeNS:localName:';
    procedure focus; message 'focus';
    procedure blur; message 'blur';
    procedure scrollIntoView(alignWithTop: Boolean); message 'scrollIntoView:';
    function contains(element: DOMElement): Boolean; message 'contains:';
    procedure scrollIntoViewIfNeeded(centerIfNeeded: Boolean); message 'scrollIntoViewIfNeeded:';
    procedure scrollByLines(lines: cint); message 'scrollByLines:';
    procedure scrollByPages(pages: cint); message 'scrollByPages:';
    function getElementsByClassName(name: NSString): DOMNodeList; message 'getElementsByClassName:';
    function querySelector(selectors: NSString): DOMElement; message 'querySelector:';
    function querySelectorAll(selectors: NSString): DOMNodeList; message 'querySelectorAll:';
  end;

{ DOMElementDeprecatedCategory }
  DOMElementDeprecatedCategory = objccategory external (DOMElement)
    procedure setAttribute(name: NSString; value: NSString); message 'setAttribute:name:'; deprecated 'in_webkit_version_3 and later';
    function getAttributeNS(namespaceURI_: NSString; localName_: NSString): NSString; message 'getAttributeNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    procedure setAttributeNS(namespaceURI_: NSString; qualifiedName: NSString; value: NSString); message 'setAttributeNS:namespaceURI:qualifiedName:'; deprecated 'in_webkit_version_3 and later';
    procedure removeAttributeNS(namespaceURI_: NSString; localName_: NSString); message 'removeAttributeNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function getElementsByTagNameNS(namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function getAttributeNodeNS(namespaceURI_: NSString; localName_: NSString): DOMAttr; message 'getAttributeNodeNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function hasAttributeNS(namespaceURI_: NSString; localName_: NSString): Boolean; message 'hasAttributeNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
