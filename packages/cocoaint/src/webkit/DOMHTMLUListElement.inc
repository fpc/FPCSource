{ Parsed from Webkit.framework DOMHTMLUListElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLULISTELEMENT_PAS_T}
{$define DOMHTMLULISTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLULISTELEMENT_PAS_R}
{$define DOMHTMLULISTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLULISTELEMENT_PAS_F}
{$define DOMHTMLULISTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLULISTELEMENT_PAS_S}
{$define DOMHTMLULISTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLUListElement = objcclass;
  DOMHTMLUListElementPointer = ^DOMHTMLUListElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLULISTELEMENT_PAS_C}
{$define DOMHTMLULISTELEMENT_PAS_C}

{ DOMHTMLUListElement }
  DOMHTMLUListElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLUListElement; message 'alloc';

    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end; external;

{$endif}
{$endif}
