{ Parsed from Webkit.framework DOMHTMLMarqueeElement.h }

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
  DOMHTMLMarqueeElementPtr = DOMHTMLMarqueeElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMARQUEEELEMENT_PAS_C}
{$define DOMHTMLMARQUEEELEMENT_PAS_C}

{ DOMHTMLMarqueeElement }
  DOMHTMLMarqueeElement = objcclass external (DOMHTMLElement)
    
  public
    procedure start; message 'start';
    procedure stop; message 'stop';
  end;

{$endif}
{$endif}
