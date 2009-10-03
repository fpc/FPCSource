{ Parsed from Webkit.framework DOMHTMLTableCaptionElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLECAPTIONELEMENT_PAS_C}
{$define DOMHTMLTABLECAPTIONELEMENT_PAS_C}

{ DOMHTMLTableCaptionElement }
  DOMHTMLTableCaptionElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLTableCaptionElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end; external;

{$endif}
{$endif}
