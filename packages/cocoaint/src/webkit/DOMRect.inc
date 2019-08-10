{ Parsed from WebKit.framework DOMRect.h }


{$ifdef TYPES}
type
  DOMRectPtr = ^DOMRect;
{$endif}

{$ifdef CLASSES}

type
  DOMRect = objcclass external (DOMObject)
  public
    function top: DOMCSSPrimitiveValue; message 'top';
    function right: DOMCSSPrimitiveValue; message 'right';
    function bottom: DOMCSSPrimitiveValue; message 'bottom';
    function left: DOMCSSPrimitiveValue; message 'left';
  end;
{$endif}

