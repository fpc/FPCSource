{ Parsed from WebKit.framework DOMHTMLLIElement.h }


{$ifdef TYPES}
type
  DOMHTMLLIElementPtr = ^DOMHTMLLIElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLLIElement = objcclass external (DOMHTMLElement)
  public
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
    procedure setValue(newValue: cint); message 'setValue:';
    function value: cint; message 'value';
  end;
{$endif}

