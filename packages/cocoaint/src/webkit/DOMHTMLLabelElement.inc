{ Parsed from WebKit.framework DOMHTMLLabelElement.h }


{$ifdef TYPES}
type
  DOMHTMLLabelElementPtr = ^DOMHTMLLabelElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLLabelElement = objcclass external (DOMHTMLElement)
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setHtmlFor(newValue: NSString); message 'setHtmlFor:';
    function htmlFor: NSString; message 'htmlFor';
    procedure setAccessKey(newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
  end;
{$endif}

