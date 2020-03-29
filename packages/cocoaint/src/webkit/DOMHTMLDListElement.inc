{ Parsed from WebKit.framework DOMHTMLDListElement.h }


{$ifdef TYPES}
type
  DOMHTMLDListElementPtr = ^DOMHTMLDListElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLDListElement = objcclass external (DOMHTMLElement)
  public
    procedure setCompact(newValue: ObjCBOOL); message 'setCompact:';
    function compact: ObjCBOOL; message 'compact';
  end;
{$endif}

