{ Parsed from WebKit.framework DOMRGBColor.h }


{$ifdef TYPES}
type
  DOMRGBColorPtr = ^DOMRGBColor;
{$endif}

{$ifdef CLASSES}

type
  DOMRGBColor = objcclass external (DOMObject)
  public
    function red: DOMCSSPrimitiveValue; message 'red';
    function green: DOMCSSPrimitiveValue; message 'green';
    function blue: DOMCSSPrimitiveValue; message 'blue';
    function alpha: DOMCSSPrimitiveValue; message 'alpha';
    function color: NSColor; message 'color';
  end;
{$endif}

