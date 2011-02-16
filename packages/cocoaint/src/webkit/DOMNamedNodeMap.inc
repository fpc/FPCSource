{ Parsed from Webkit.framework DOMNamedNodeMap.h }

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
  DOMNamedNodeMapPtr = DOMNamedNodeMapPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNAMEDNODEMAP_PAS_C}
{$define DOMNAMEDNODEMAP_PAS_C}

{ DOMNamedNodeMap }
  DOMNamedNodeMap = objcclass external (DOMObject)
    
  public
    function length: cuint; message 'length';
    function getNamedItem(name: NSString): DOMNode; message 'getNamedItem:';
    function setNamedItem(node: DOMNode): DOMNode; message 'setNamedItem:';
    function removeNamedItem(name: NSString): DOMNode; message 'removeNamedItem:';
    function item(index: cuint): DOMNode; message 'item:';
    function getNamedItemNS_localName(namespaceURI: NSString; localName: NSString): DOMNode; message 'getNamedItemNS:localName:';
    function setNamedItemNS(node: DOMNode): DOMNode; message 'setNamedItemNS:';
    function removeNamedItemNS_localName(namespaceURI: NSString; localName: NSString): DOMNode; message 'removeNamedItemNS:localName:';
  end;

{ DOMNamedNodeMapDeprecatedCategory }
  DOMNamedNodeMapDeprecatedCategory = objccategory external (DOMNamedNodeMap)
    function getNamedItemNS(namespaceURI: NSString; localName: NSString): DOMNode; message 'getNamedItemNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function removeNamedItemNS(namespaceURI: NSString; localName: NSString): DOMNode; message 'removeNamedItemNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
