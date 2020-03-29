{ Parsed from WebKit.framework DOMHTMLFrameSetElement.h }


{$ifdef TYPES}
type
  DOMHTMLFrameSetElementPtr = ^DOMHTMLFrameSetElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLFrameSetElement = objcclass external (DOMHTMLElement)
  public
    procedure setCols(newValue: NSString); message 'setCols:';
    function cols: NSString; message 'cols';
    procedure setRows(newValue: NSString); message 'setRows:';
    function rows: NSString; message 'rows';
  end;
{$endif}

