{ Parsed from WebKit.framework DOMHTMLMarqueeElement.h }


{$ifdef TYPES}
type
  DOMHTMLMarqueeElementPtr = ^DOMHTMLMarqueeElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLMarqueeElement = objcclass external (DOMHTMLElement)
  public
    procedure start; message 'start';
    procedure stop; message 'stop';
  end;
{$endif}

