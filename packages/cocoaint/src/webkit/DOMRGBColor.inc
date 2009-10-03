{ Parsed from Webkit.framework DOMRGBColor.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMRGBCOLOR_PAS_T}
{$define DOMRGBCOLOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMRGBCOLOR_PAS_R}
{$define DOMRGBCOLOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMRGBCOLOR_PAS_F}
{$define DOMRGBCOLOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMRGBCOLOR_PAS_S}
{$define DOMRGBCOLOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMRGBColor = objcclass;
  DOMRGBColorPointer = ^DOMRGBColor;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMRGBCOLOR_PAS_C}
{$define DOMRGBCOLOR_PAS_C}

{ DOMRGBColor }
  DOMRGBColor = objcclass(DOMObject)
    
  public
    class function alloc: DOMRGBColor; message 'alloc';

    function red: DOMCSSPrimitiveValue; message 'red';
    function green: DOMCSSPrimitiveValue; message 'green';
    function blue: DOMCSSPrimitiveValue; message 'blue';
    function alpha: DOMCSSPrimitiveValue; message 'alpha';
    function color: NSColor; message 'color';
  end; external;

{$endif}
{$endif}
