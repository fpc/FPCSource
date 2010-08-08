{ Parsed from Webkit.framework DOMHTMLPreElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLPREELEMENT_PAS_T}
{$define DOMHTMLPREELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLPREELEMENT_PAS_R}
{$define DOMHTMLPREELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLPREELEMENT_PAS_F}
{$define DOMHTMLPREELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLPREELEMENT_PAS_S}
{$define DOMHTMLPREELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLPreElement = objcclass;
  DOMHTMLPreElementPointer = ^DOMHTMLPreElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLPREELEMENT_PAS_C}
{$define DOMHTMLPREELEMENT_PAS_C}

{ DOMHTMLPreElement }
  DOMHTMLPreElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLPreElement; message 'alloc';
  end; external;

{$endif}
{$endif}
