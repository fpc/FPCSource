{ Parsed from Webkit.framework DOMHTMLTitleElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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
