{ Parsed from WebKit.framework DOMHTMLTitleElement.h }


{$ifdef TYPES}
type
  DOMHTMLTitleElementPtr = ^DOMHTMLTitleElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTitleElement = objcclass external (DOMHTMLElement)
  public
    procedure setText(newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
  end;
{$endif}

