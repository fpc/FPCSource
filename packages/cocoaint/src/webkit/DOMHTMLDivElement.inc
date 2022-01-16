{ Parsed from WebKit.framework DOMHTMLDivElement.h }


{$ifdef TYPES}
type
  DOMHTMLDivElementPtr = ^DOMHTMLDivElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLDivElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;
{$endif}

