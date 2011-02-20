{ Parsed from Webkit.framework DOMHTMLQuoteElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLQUOTEELEMENT_PAS_T}
{$define DOMHTMLQUOTEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLQUOTEELEMENT_PAS_R}
{$define DOMHTMLQUOTEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLQUOTEELEMENT_PAS_F}
{$define DOMHTMLQUOTEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLQUOTEELEMENT_PAS_S}
{$define DOMHTMLQUOTEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLQuoteElement = objcclass;
  DOMHTMLQuoteElementPointer = ^DOMHTMLQuoteElement;
  DOMHTMLQuoteElementPtr = DOMHTMLQuoteElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLQUOTEELEMENT_PAS_C}
{$define DOMHTMLQUOTEELEMENT_PAS_C}

{ DOMHTMLQuoteElement }
  DOMHTMLQuoteElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCite (newValue: NSString); message 'setCite:';
    function cite: NSString; message 'cite';
  end;

{$endif}
{$endif}
