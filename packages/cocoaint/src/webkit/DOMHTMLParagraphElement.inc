{ Parsed from WebKit.framework DOMHTMLParagraphElement.h }


{$ifdef TYPES}
type
  DOMHTMLParagraphElementPtr = ^DOMHTMLParagraphElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLParagraphElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;
{$endif}

