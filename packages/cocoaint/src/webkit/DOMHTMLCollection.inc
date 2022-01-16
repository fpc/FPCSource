{ Parsed from WebKit.framework DOMHTMLCollection.h }


{$ifdef TYPES}
type
  DOMHTMLCollectionPtr = ^DOMHTMLCollection;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLCollection = objcclass external (DOMObject)
  public
    function length: cuint; message 'length';
    function item (index: cuint): DOMNode; message 'item:';
    function namedItem (name: NSString): DOMNode; message 'namedItem:';
    function tags (name: NSString): DOMNodeList; message 'tags:'; { available in 10_6 }
  end;
{$endif}

