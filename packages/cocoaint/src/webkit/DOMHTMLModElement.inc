{ Parsed from WebKit.framework DOMHTMLModElement.h }


{$ifdef TYPES}
type
  DOMHTMLModElementPtr = ^DOMHTMLModElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLModElement = objcclass external (DOMHTMLElement)
  public
    procedure setCite(newValue: NSString); message 'setCite:';
    function cite: NSString; message 'cite';
    procedure setDateTime(newValue: NSString); message 'setDateTime:';
    function dateTime: NSString; message 'dateTime';
  end;
{$endif}

