{ Parsed from Webkit.framework DOMHTMLTitleElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMHTMLTITLEELEMENT_PAS_T}
{$define DOMHTMLTITLEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTITLEELEMENT_PAS_R}
{$define DOMHTMLTITLEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTITLEELEMENT_PAS_F}
{$define DOMHTMLTITLEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTITLEELEMENT_PAS_S}
{$define DOMHTMLTITLEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTitleElement = objcclass;
  DOMHTMLTitleElementPointer = ^DOMHTMLTitleElement;
  DOMHTMLTitleElementPtr = DOMHTMLTitleElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTITLEELEMENT_PAS_C}
{$define DOMHTMLTITLEELEMENT_PAS_C}

{ DOMHTMLTitleElement }
  DOMHTMLTitleElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLTitleElement; message 'alloc';

    procedure setText (newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
  end; external;

{$endif}
{$endif}
