{ Parsed from WebKit.framework DOMHTMLMapElement.h }


{$ifdef TYPES}
type
  DOMHTMLMapElementPtr = ^DOMHTMLMapElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLMapElement = objcclass external (DOMHTMLElement)
  public
    function areas: DOMHTMLCollection; message 'areas';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
  end;
{$endif}

