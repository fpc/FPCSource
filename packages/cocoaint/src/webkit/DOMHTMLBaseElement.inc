{ Parsed from WebKit.framework DOMHTMLBaseElement.h }


{$ifdef TYPES}
type
  DOMHTMLBaseElementPtr = ^DOMHTMLBaseElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLBaseElement = objcclass external (DOMHTMLElement)
  public
    procedure setHref(newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setTarget(newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
  end;
{$endif}

