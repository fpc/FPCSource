{ Parsed from WebKit.framework DOMHTMLStyleElement.h }


{$ifdef TYPES}
type
  DOMHTMLStyleElementPtr = ^DOMHTMLStyleElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLStyleElement = objcclass external (DOMHTMLElement)
  public
    procedure setDisabled(newValue: ObjCBOOL); message 'setDisabled:';
    function disabled: ObjCBOOL; message 'disabled';
    procedure setMedia(newValue: NSString); message 'setMedia:';
    function media: NSString; message 'media';
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
    function sheet: DOMStyleSheet; message 'sheet';
  end;
{$endif}

