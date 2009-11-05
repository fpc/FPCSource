{ Parsed from Webkit.framework DOMHTMLFieldSetElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLFIELDSETELEMENT_PAS_T}
{$define DOMHTMLFIELDSETELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLFIELDSETELEMENT_PAS_R}
{$define DOMHTMLFIELDSETELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLFIELDSETELEMENT_PAS_F}
{$define DOMHTMLFIELDSETELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLFIELDSETELEMENT_PAS_S}
{$define DOMHTMLFIELDSETELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLFieldSetElement = objcclass;
  DOMHTMLFieldSetElementPointer = ^DOMHTMLFieldSetElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFIELDSETELEMENT_PAS_C}
{$define DOMHTMLFIELDSETELEMENT_PAS_C}

{ DOMHTMLFieldSetElement }
  DOMHTMLFieldSetElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLFieldSetElement; message 'alloc';

    function form: DOMHTMLFormElement; message 'form';
  end; external;

{$endif}
{$endif}
