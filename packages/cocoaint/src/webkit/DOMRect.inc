{ Parsed from Webkit.framework DOMRect.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMRECT_PAS_C}
{$define DOMRECT_PAS_C}

{ DOMRect }
  DOMRect = objcclass(DOMObject)
    
  public
    class function alloc: DOMRect; message 'alloc';

    function top: DOMCSSPrimitiveValue; message 'top';
    function right: DOMCSSPrimitiveValue; message 'right';
    function bottom: DOMCSSPrimitiveValue; message 'bottom';
    function left: DOMCSSPrimitiveValue; message 'left';
  end; external;

{$endif}
{$endif}
