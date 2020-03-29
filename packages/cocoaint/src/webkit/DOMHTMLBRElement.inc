{ Parsed from WebKit.framework DOMHTMLBRElement.h }


{$ifdef TYPES}
type
  DOMHTMLBRElementPtr = ^DOMHTMLBRElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLBRElement = objcclass external (DOMHTMLElement)
  public
    procedure setClear(newValue: NSString); message 'setClear:';
    function clear: NSString; message 'clear';
  end;
{$endif}

