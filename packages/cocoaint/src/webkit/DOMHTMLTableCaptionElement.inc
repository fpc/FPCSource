{ Parsed from WebKit.framework DOMHTMLTableCaptionElement.h }


{$ifdef TYPES}
type
  DOMHTMLTableCaptionElementPtr = ^DOMHTMLTableCaptionElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTableCaptionElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;
{$endif}

