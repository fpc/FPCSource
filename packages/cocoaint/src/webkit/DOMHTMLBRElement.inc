{ Parsed from Webkit.framework DOMHTMLBRElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLBRELEMENT_PAS_T}
{$define DOMHTMLBRELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBRELEMENT_PAS_R}
{$define DOMHTMLBRELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBRELEMENT_PAS_F}
{$define DOMHTMLBRELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBRELEMENT_PAS_S}
{$define DOMHTMLBRELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLBRElement = objcclass;
  DOMHTMLBRElementPointer = ^DOMHTMLBRElement;
  DOMHTMLBRElementPtr = DOMHTMLBRElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBRELEMENT_PAS_C}
{$define DOMHTMLBRELEMENT_PAS_C}

{ DOMHTMLBRElement }
  DOMHTMLBRElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setClear (newValue: NSString); message 'setClear:';
    function clear: NSString; message 'clear';
  end;

{$endif}
{$endif}
