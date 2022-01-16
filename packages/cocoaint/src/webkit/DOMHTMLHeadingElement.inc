{ Parsed from WebKit.framework DOMHTMLHeadingElement.h }


{$ifdef TYPES}
type
  DOMHTMLHeadingElementPtr = ^DOMHTMLHeadingElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLHeadingElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;
{$endif}

