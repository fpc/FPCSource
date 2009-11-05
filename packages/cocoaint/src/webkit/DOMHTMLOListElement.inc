{ Parsed from Webkit.framework DOMHTMLOListElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLOLISTELEMENT_PAS_T}
{$define DOMHTMLOLISTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLOLISTELEMENT_PAS_R}
{$define DOMHTMLOLISTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLOLISTELEMENT_PAS_F}
{$define DOMHTMLOLISTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLOLISTELEMENT_PAS_S}
{$define DOMHTMLOLISTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLOListElement = objcclass;
  DOMHTMLOListElementPointer = ^DOMHTMLOListElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOLISTELEMENT_PAS_C}
{$define DOMHTMLOLISTELEMENT_PAS_C}

{ DOMHTMLOListElement }
  DOMHTMLOListElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLOListElement; message 'alloc';

    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end; external;

{$endif}
{$endif}
