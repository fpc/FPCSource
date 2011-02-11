{ Parsed from Webkit.framework DOMHTMLFieldSetElement.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:20 CET 2011 }


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
  DOMHTMLFieldSetElementPtr = DOMHTMLFieldSetElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFIELDSETELEMENT_PAS_C}
{$define DOMHTMLFIELDSETELEMENT_PAS_C}

{ DOMHTMLFieldSetElement }
  DOMHTMLFieldSetElement = objcclass external (DOMHTMLElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
  end;

{$endif}
{$endif}
