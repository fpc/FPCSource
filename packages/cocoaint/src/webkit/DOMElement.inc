{ Parsed from WebKit.framework DOMElement.h }


{$ifdef TYPES}
type
  DOMElementPtr = ^DOMElement;
{$endif}

{$ifdef TYPES}
const
  DOM_ALLOW_KEYBOARD_INPUT = 1;
{$endif}

{$ifdef CLASSES}

type
  DOMElement = objcclass external (DOMNode)
  public
    function tagName: NSString; message 'tagName';
    function style: DOMCSSStyleDeclaration; message 'style';
    function offsetLeft: cint; message 'offsetLeft';
    function offsetTop: cint; message 'offsetTop';
    function offsetWidth: cint; message 'offsetWidth';
    function offsetHeight: cint; message 'offsetHeight';
    function clientLeft: cint; message 'clientLeft';
    function clientTop: cint; message 'clientTop';
    function clientWidth: cint; message 'clientWidth';
    function clientHeight: cint; message 'clientHeight';
    procedure setScrollLeft(newValue: cint); message 'setScrollLeft:';
    function scrollLeft: cint; message 'scrollLeft';
    procedure setScrollTop(newValue: cint); message 'setScrollTop:';
    function scrollTop: cint; message 'scrollTop';
    function scrollWidth: cint; message 'scrollWidth';
    function scrollHeight: cint; message 'scrollHeight';
    function offsetParent: DOMElement; message 'offsetParent';
    procedure setClassName(newValue: NSString); message 'setClassName:';
    function className: NSString; message 'className';
    function firstElementChild: DOMElement; message 'firstElementChild';
    function lastElementChild: DOMElement; message 'lastElementChild';
    function previousElementSibling: DOMElement; message 'previousElementSibling';
    function nextElementSibling: DOMElement; message 'nextElementSibling';
    function childElementCount: cuint; message 'childElementCount';
    function innerText: NSString; message 'innerText';
    function getAttribute (name: NSString): NSString; message 'getAttribute:';
    procedure setAttribute_value (name: NSString; value: NSString); message 'setAttribute:value:'; { available in 10_5 }
    procedure removeAttribute (name: NSString); message 'removeAttribute:';
    function getAttributeNode (name: NSString): DOMAttr; message 'getAttributeNode:';
    function setAttributeNode (newAttr: DOMAttr): DOMAttr; message 'setAttributeNode:';
    function removeAttributeNode (oldAttr: DOMAttr): DOMAttr; message 'removeAttributeNode:';
    function getElementsByTagName (name: NSString): DOMNodeList; message 'getElementsByTagName:';
    function getAttributeNS_localName (namespaceURI_: NSString; localName_: NSString): NSString; message 'getAttributeNS:localName:'; { available in 10_5 }
    procedure setAttributeNS_qualifiedName_value (namespaceURI_: NSString; qualifiedName: NSString; value: NSString); message 'setAttributeNS:qualifiedName:value:'; { available in 10_5 }
    procedure removeAttributeNS_localName (namespaceURI_: NSString; localName_: NSString); message 'removeAttributeNS:localName:'; { available in 10_5 }
    function getElementsByTagNameNS_localName (namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:localName:'; { available in 10_5 }
    function getAttributeNodeNS_localName (namespaceURI_: NSString; localName_: NSString): DOMAttr; message 'getAttributeNodeNS:localName:'; { available in 10_5 }
    function setAttributeNodeNS (newAttr: DOMAttr): DOMAttr; message 'setAttributeNodeNS:';
    function hasAttribute (name: NSString): ObjCBOOL; message 'hasAttribute:';
    function hasAttributeNS_localName (namespaceURI_: NSString; localName_: NSString): ObjCBOOL; message 'hasAttributeNS:localName:'; { available in 10_5 }
    procedure focus; message 'focus'; { available in 10_6 }
    procedure blur; message 'blur'; { available in 10_6 }
    procedure scrollIntoView (alignWithTop: ObjCBOOL); message 'scrollIntoView:'; { available in 10_5 }
    procedure scrollIntoViewIfNeeded (centerIfNeeded: ObjCBOOL); message 'scrollIntoViewIfNeeded:'; { available in 10_5 }
    procedure scrollByLines (lines: cint); message 'scrollByLines:'; { available in 10_5 }
    procedure scrollByPages (pages: cint); message 'scrollByPages:'; { available in 10_5 }
    function getElementsByClassName (name: NSString): DOMNodeList; message 'getElementsByClassName:'; { available in 10_6 }
    function querySelector (selectors: NSString): DOMElement; message 'querySelector:'; { available in 10_6 }
    function querySelectorAll (selectors: NSString): DOMNodeList; message 'querySelectorAll:'; { available in 10_6 }
    procedure webkitRequestFullScreen (flags: cushort); message 'webkitRequestFullScreen:'; { available in 10_6 }
  end;


type
  DOMElementDeprecated = objccategory external (DOMElement)
    procedure setAttribute (name: NSString); message 'setAttribute:'; deprecated 'in 10_4, 10_5';
    function getAttributeNS (namespaceURI_: NSString): NSString; message 'getAttributeNS:'; deprecated 'in 10_4, 10_5';
    procedure setAttributeNS_qualifiedName (namespaceURI_: NSString; value: NSString); message 'setAttributeNS:qualifiedName:'; deprecated 'in 10_4, 10_5';
    procedure removeAttributeNS (namespaceURI_: NSString); message 'removeAttributeNS:'; deprecated 'in 10_4, 10_5';
    function getElementsByTagNameNS (namespaceURI_: NSString): DOMNodeList; message 'getElementsByTagNameNS:'; deprecated 'in 10_4, 10_5';
    function getAttributeNodeNS (namespaceURI_: NSString): DOMAttr; message 'getAttributeNodeNS:'; deprecated 'in 10_4, 10_5';
    function hasAttributeNS (namespaceURI_: NSString): ObjCBOOL; message 'hasAttributeNS:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

