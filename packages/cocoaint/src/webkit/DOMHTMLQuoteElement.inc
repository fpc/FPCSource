{ Parsed from Webkit.framework DOMHTMLQuoteElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLQUOTEELEMENT_PAS_C}
{$define DOMHTMLQUOTEELEMENT_PAS_C}

{ DOMHTMLQuoteElement }
  DOMHTMLQuoteElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLQuoteElement; message 'alloc';

    procedure setCite (newValue: NSString); message 'setCite:';
    function cite: NSString; message 'cite';
  end; external;

{$endif}
{$endif}
