{ Parsed from Webkit.framework DOMHTMLBaseFontElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMHTMLBASEFONTELEMENT_PAS_T}
{$define DOMHTMLBASEFONTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBASEFONTELEMENT_PAS_R}
{$define DOMHTMLBASEFONTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBASEFONTELEMENT_PAS_F}
{$define DOMHTMLBASEFONTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBASEFONTELEMENT_PAS_S}
{$define DOMHTMLBASEFONTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLBaseFontElement = objcclass;
  DOMHTMLBaseFontElementPointer = ^DOMHTMLBaseFontElement;
  DOMHTMLBaseFontElementPtr = DOMHTMLBaseFontElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBASEFONTELEMENT_PAS_C}
{$define DOMHTMLBASEFONTELEMENT_PAS_C}

{ DOMHTMLBaseFontElement }
  DOMHTMLBaseFontElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLBaseFontElement; message 'alloc';

    procedure setColor (newValue: NSString); message 'setColor:';
    function color: NSString; message 'color';
    procedure setFace (newValue: NSString); message 'setFace:';
    function face: NSString; message 'face';
    procedure setSize (newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
  end; external;

{$endif}
{$endif}
