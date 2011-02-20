{ Parsed from Webkit.framework DOMRect.h }

{$ifdef TYPES}
{$ifndef DOMRECT_PAS_T}
{$define DOMRECT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMRECT_PAS_R}
{$define DOMRECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMRECT_PAS_F}
{$define DOMRECT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMRECT_PAS_S}
{$define DOMRECT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMRect = objcclass;
  DOMRectPointer = ^DOMRect;
  DOMRectPtr = DOMRectPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMRECT_PAS_C}
{$define DOMRECT_PAS_C}

{ DOMRect }
  DOMRect = objcclass external (DOMObject)
    
  public
    function top: DOMCSSPrimitiveValue; message 'top';
    function right: DOMCSSPrimitiveValue; message 'right';
    function bottom: DOMCSSPrimitiveValue; message 'bottom';
    function left: DOMCSSPrimitiveValue; message 'left';
  end;

{$endif}
{$endif}
