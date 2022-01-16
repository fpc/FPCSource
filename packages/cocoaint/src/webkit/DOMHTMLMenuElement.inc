{ Parsed from WebKit.framework DOMHTMLMenuElement.h }


{$ifdef TYPES}
type
  DOMHTMLMenuElementPtr = ^DOMHTMLMenuElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLMenuElement = objcclass external (DOMHTMLElement)
  public
    procedure setCompact(newValue: ObjCBOOL); message 'setCompact:';
    function compact: ObjCBOOL; message 'compact';
  end;
{$endif}

