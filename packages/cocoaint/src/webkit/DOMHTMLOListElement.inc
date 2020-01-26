{ Parsed from WebKit.framework DOMHTMLOListElement.h }


{$ifdef TYPES}
type
  DOMHTMLOListElementPtr = ^DOMHTMLOListElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLOListElement = objcclass external (DOMHTMLElement)
  public
    procedure setCompact(newValue: ObjCBOOL); message 'setCompact:';
    function compact: ObjCBOOL; message 'compact';
    procedure setStart(newValue: cint); message 'setStart:';
    function start: cint; message 'start';
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
  end;
{$endif}

