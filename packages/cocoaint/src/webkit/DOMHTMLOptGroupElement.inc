{ Parsed from WebKit.framework DOMHTMLOptGroupElement.h }


{$ifdef TYPES}
type
  DOMHTMLOptGroupElementPtr = ^DOMHTMLOptGroupElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLOptGroupElement = objcclass external (DOMHTMLElement)
  public
    procedure setDisabled(newValue: ObjCBOOL); message 'setDisabled:';
    function disabled: ObjCBOOL; message 'disabled';
    procedure setLabel(newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label';
  end;
{$endif}

