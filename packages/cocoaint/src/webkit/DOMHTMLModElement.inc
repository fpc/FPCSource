{ Parsed from Webkit.framework DOMHTMLModElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLMODELEMENT_PAS_T}
{$define DOMHTMLMODELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLMODELEMENT_PAS_R}
{$define DOMHTMLMODELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLMODELEMENT_PAS_F}
{$define DOMHTMLMODELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLMODELEMENT_PAS_S}
{$define DOMHTMLMODELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLModElement = objcclass;
  DOMHTMLModElementPointer = ^DOMHTMLModElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMODELEMENT_PAS_C}
{$define DOMHTMLMODELEMENT_PAS_C}

{ DOMHTMLModElement }
  DOMHTMLModElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLModElement; message 'alloc';

    procedure setCite (newValue: NSString); message 'setCite:';
    function cite: NSString; message 'cite';
    procedure setDateTime (newValue: NSString); message 'setDateTime:';
    function dateTime: NSString; message 'dateTime';
  end; external;

{$endif}
{$endif}
