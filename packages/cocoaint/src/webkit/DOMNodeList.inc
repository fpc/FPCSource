{ Parsed from WebKit.framework DOMNodeList.h }


{$ifdef TYPES}
type
  DOMNodeListPtr = ^DOMNodeList;
{$endif}

{$ifdef CLASSES}

type
  DOMNodeList = objcclass external (DOMObject)
  public
    function length: cuint; message 'length';
    function item (index: cuint): DOMNode; message 'item:';
  end;
{$endif}

