{ Parsed from Webkit.framework DOMHTMLTableCaptionElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLTABLECAPTIONELEMENT_PAS_T}
{$define DOMHTMLTABLECAPTIONELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTABLECAPTIONELEMENT_PAS_R}
{$define DOMHTMLTABLECAPTIONELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTABLECAPTIONELEMENT_PAS_F}
{$define DOMHTMLTABLECAPTIONELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTABLECAPTIONELEMENT_PAS_S}
{$define DOMHTMLTABLECAPTIONELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTableCaptionElement = objcclass;
  DOMHTMLTableCaptionElementPointer = ^DOMHTMLTableCaptionElement;
  DOMHTMLTableCaptionElementPtr = DOMHTMLTableCaptionElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLECAPTIONELEMENT_PAS_C}
{$define DOMHTMLTABLECAPTIONELEMENT_PAS_C}

{ DOMHTMLTableCaptionElement }
  DOMHTMLTableCaptionElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;

{$endif}
{$endif}
