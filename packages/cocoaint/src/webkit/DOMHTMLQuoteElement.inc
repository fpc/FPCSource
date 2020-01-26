{ Parsed from WebKit.framework DOMHTMLQuoteElement.h }


{$ifdef TYPES}
type
  DOMHTMLQuoteElementPtr = ^DOMHTMLQuoteElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLQuoteElement = objcclass external (DOMHTMLElement)
  public
    procedure setCite(newValue: NSString); message 'setCite:';
    function cite: NSString; message 'cite';
  end;
{$endif}

