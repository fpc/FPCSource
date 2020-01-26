{ Parsed from WebKit.framework DOMHTMLDirectoryElement.h }


{$ifdef TYPES}
type
  DOMHTMLDirectoryElementPtr = ^DOMHTMLDirectoryElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLDirectoryElement = objcclass external (DOMHTMLElement)
  public
    procedure setCompact(newValue: ObjCBOOL); message 'setCompact:';
    function compact: ObjCBOOL; message 'compact';
  end;
{$endif}

