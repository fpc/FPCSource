{ Parsed from WebKit.framework DOMNamedNodeMap.h }


{$ifdef TYPES}
type
  DOMNamedNodeMapPtr = ^DOMNamedNodeMap;
{$endif}

{$ifdef CLASSES}

type
  DOMNamedNodeMap = objcclass external (DOMObject)
  public
    function length: cuint; message 'length';
    function getNamedItem (name: NSString): DOMNode; message 'getNamedItem:';
    function setNamedItem (node: DOMNode): DOMNode; message 'setNamedItem:';
    function removeNamedItem (name: NSString): DOMNode; message 'removeNamedItem:';
    function item (index: cuint): DOMNode; message 'item:';
    function getNamedItemNS_localName (namespaceURI: NSString; localName: NSString): DOMNode; message 'getNamedItemNS:localName:'; { available in 10_5 }
    function setNamedItemNS (node: DOMNode): DOMNode; message 'setNamedItemNS:';
    function removeNamedItemNS_localName (namespaceURI: NSString; localName: NSString): DOMNode; message 'removeNamedItemNS:localName:'; { available in 10_5 }
  end;


type
  DOMNamedNodeMapDeprecated = objccategory external (DOMNamedNodeMap)
    function getNamedItemNS (namespaceURI: NSString): DOMNode; message 'getNamedItemNS:'; deprecated 'in 10_4, 10_5';
    function removeNamedItemNS (namespaceURI: NSString): DOMNode; message 'removeNamedItemNS:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

