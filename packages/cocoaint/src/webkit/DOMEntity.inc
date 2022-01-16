{ Parsed from WebKit.framework DOMEntity.h }


{$ifdef TYPES}
type
  DOMEntityPtr = ^DOMEntity;
{$endif}

{$ifdef CLASSES}

type
  DOMEntity = objcclass external (DOMNode)
  public
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
    function notationName: NSString; message 'notationName';
  end;
{$endif}

