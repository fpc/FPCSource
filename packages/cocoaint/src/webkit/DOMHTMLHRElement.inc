{ Parsed from Webkit.framework DOMHTMLHRElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLHRELEMENT_PAS_T}
{$define DOMHTMLHRELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHRELEMENT_PAS_R}
{$define DOMHTMLHRELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHRELEMENT_PAS_F}
{$define DOMHTMLHRELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHRELEMENT_PAS_S}
{$define DOMHTMLHRELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHRElement = objcclass;
  DOMHTMLHRElementPointer = ^DOMHTMLHRElement;
  DOMHTMLHRElementPtr = DOMHTMLHRElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHRELEMENT_PAS_C}
{$define DOMHTMLHRELEMENT_PAS_C}

{ DOMHTMLHRElement }
  DOMHTMLHRElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setNoShade (newValue: Boolean); message 'setNoShade:';
    function noShade: Boolean; message 'noShade';
    procedure setSize (newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end;

{$endif}
{$endif}
