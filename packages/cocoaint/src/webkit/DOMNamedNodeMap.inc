{ Parsed from Webkit.framework DOMNamedNodeMap.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMNAMEDNODEMAP_PAS_T}
{$define DOMNAMEDNODEMAP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMNAMEDNODEMAP_PAS_R}
{$define DOMNAMEDNODEMAP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMNAMEDNODEMAP_PAS_F}
{$define DOMNAMEDNODEMAP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMNAMEDNODEMAP_PAS_S}
{$define DOMNAMEDNODEMAP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMNamedNodeMap = objcclass;
  DOMNamedNodeMapPointer = ^DOMNamedNodeMap;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNAMEDNODEMAP_PAS_C}
{$define DOMNAMEDNODEMAP_PAS_C}

{ DOMNamedNodeMap }
  DOMNamedNodeMap = objcclass(DOMObject)
    
  public
    class function alloc: DOMNamedNodeMap; message 'alloc';

    function length: cuint; message 'length';
    function getNamedItem(name: NSStringPointer): DOMNode; message 'getNamedItem:';
    function setNamedItem(node: DOMNode): DOMNode; message 'setNamedItem:';
    function removeNamedItem(name: NSStringPointer): DOMNode; message 'removeNamedItem:';
    function item(index: cuint): DOMNode; message 'item:';
    function getNamedItemNS_localName(namespaceURI: NSStringPointer; localName: NSStringPointer): DOMNode; message 'getNamedItemNS:localName:';
    function setNamedItemNS(node: DOMNode): DOMNode; message 'setNamedItemNS:';
    function removeNamedItemNS_localName(namespaceURI: NSStringPointer; localName: NSStringPointer): DOMNode; message 'removeNamedItemNS:localName:';

    { Category: DOMNamedNodeMapDeprecated }
    function getNamedItemNS(namespaceURI: NSStringPointer; localName: NSStringPointer): DOMNode; message 'getNamedItemNS:namespaceURI:';
    function removeNamedItemNS(namespaceURI: NSStringPointer; localName: NSStringPointer): DOMNode; message 'removeNamedItemNS:namespaceURI:';
  end; external;

{$endif}
{$endif}
