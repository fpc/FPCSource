{ Parsed from Webkit.framework DOMHTMLFontElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLFONTELEMENT_PAS_T}
{$define DOMHTMLFONTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLFONTELEMENT_PAS_R}
{$define DOMHTMLFONTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLFONTELEMENT_PAS_F}
{$define DOMHTMLFONTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLFONTELEMENT_PAS_S}
{$define DOMHTMLFONTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLFontElement = objcclass;
  DOMHTMLFontElementPointer = ^DOMHTMLFontElement;
  DOMHTMLFontElementPtr = DOMHTMLFontElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFONTELEMENT_PAS_C}
{$define DOMHTMLFONTELEMENT_PAS_C}

{ DOMHTMLFontElement }
  DOMHTMLFontElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setColor (newValue: NSString); message 'setColor:';
    function color: NSString; message 'color';
    procedure setFace (newValue: NSString); message 'setFace:';
    function face: NSString; message 'face';
    procedure setSize (newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
  end;

{$endif}
{$endif}
