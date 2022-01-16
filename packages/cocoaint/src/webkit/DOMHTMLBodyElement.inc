{ Parsed from WebKit.framework DOMHTMLBodyElement.h }


{$ifdef TYPES}
type
  DOMHTMLBodyElementPtr = ^DOMHTMLBodyElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLBodyElement = objcclass external (DOMHTMLElement)
  public
    procedure setALink(newValue: NSString); message 'setALink:';
    function aLink: NSString; message 'aLink';
    procedure setBackground(newValue: NSString); message 'setBackground:';
    function background: NSString; message 'background';
    procedure setBgColor(newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setLink(newValue: NSString); message 'setLink:';
    function link: NSString; message 'link';
    procedure setText(newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
    procedure setVLink(newValue: NSString); message 'setVLink:';
    function vLink: NSString; message 'vLink';
  end;
{$endif}

