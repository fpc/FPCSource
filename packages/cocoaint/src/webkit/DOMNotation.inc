{ Parsed from WebKit.framework DOMNotation.h }


{$ifdef TYPES}
type
  DOMNotationPtr = ^DOMNotation;
{$endif}

{$ifdef CLASSES}

type
  DOMNotation = objcclass external (DOMNode)
  public
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
  end;
{$endif}

