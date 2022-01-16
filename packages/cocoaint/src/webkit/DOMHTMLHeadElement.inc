{ Parsed from WebKit.framework DOMHTMLHeadElement.h }


{$ifdef TYPES}
type
  DOMHTMLHeadElementPtr = ^DOMHTMLHeadElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLHeadElement = objcclass external (DOMHTMLElement)
  public
    procedure setProfile(newValue: NSString); message 'setProfile:';
    function profile: NSString; message 'profile';
  end;
{$endif}

