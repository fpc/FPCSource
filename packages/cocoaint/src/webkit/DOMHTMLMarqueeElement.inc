{ Parsed from Webkit.framework DOMHTMLMarqueeElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLMARQUEEELEMENT_PAS_T}
{$define DOMHTMLMARQUEEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLMARQUEEELEMENT_PAS_R}
{$define DOMHTMLMARQUEEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLMARQUEEELEMENT_PAS_F}
{$define DOMHTMLMARQUEEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLMARQUEEELEMENT_PAS_S}
{$define DOMHTMLMARQUEEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLMarqueeElement = objcclass;
  DOMHTMLMarqueeElementPointer = ^DOMHTMLMarqueeElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMARQUEEELEMENT_PAS_C}
{$define DOMHTMLMARQUEEELEMENT_PAS_C}

{ DOMHTMLMarqueeElement }
  DOMHTMLMarqueeElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLMarqueeElement; message 'alloc';

    procedure start; message 'start';
    procedure stop; message 'stop';
  end; external;

{$endif}
{$endif}
