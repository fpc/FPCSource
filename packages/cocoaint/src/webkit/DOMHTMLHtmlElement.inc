{ Parsed from WebKit.framework DOMHTMLHtmlElement.h }


{$ifdef TYPES}
type
  DOMHTMLHtmlElementPtr = ^DOMHTMLHtmlElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLHtmlElement = objcclass external (DOMHTMLElement)
  public
    procedure setVersion(newValue: NSString); message 'setVersion:';
    function version: NSString; message 'version';
  end;
{$endif}

