{ Parsed from Webkit.framework DOMHTMLMenuElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLMENUELEMENT_PAS_T}
{$define DOMHTMLMENUELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLMENUELEMENT_PAS_R}
{$define DOMHTMLMENUELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLMENUELEMENT_PAS_F}
{$define DOMHTMLMENUELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLMENUELEMENT_PAS_S}
{$define DOMHTMLMENUELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLMenuElement = objcclass;
  DOMHTMLMenuElementPointer = ^DOMHTMLMenuElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMENUELEMENT_PAS_C}
{$define DOMHTMLMENUELEMENT_PAS_C}

{ DOMHTMLMenuElement }
  DOMHTMLMenuElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLMenuElement; message 'alloc';
  end; external;

{$endif}
{$endif}
