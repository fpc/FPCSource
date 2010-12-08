{ Parsed from Webkit.framework DOMHTMLHeadingElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMHTMLHEADINGELEMENT_PAS_T}
{$define DOMHTMLHEADINGELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHEADINGELEMENT_PAS_R}
{$define DOMHTMLHEADINGELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHEADINGELEMENT_PAS_F}
{$define DOMHTMLHEADINGELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHEADINGELEMENT_PAS_S}
{$define DOMHTMLHEADINGELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHeadingElement = objcclass;
  DOMHTMLHeadingElementPointer = ^DOMHTMLHeadingElement;
  DOMHTMLHeadingElementPtr = DOMHTMLHeadingElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHEADINGELEMENT_PAS_C}
{$define DOMHTMLHEADINGELEMENT_PAS_C}

{ DOMHTMLHeadingElement }
  DOMHTMLHeadingElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLHeadingElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end; external;

{$endif}
{$endif}
