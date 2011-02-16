{ Parsed from Webkit.framework DOMHTMLUListElement.h }

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
  DOMHTMLUListElementPtr = DOMHTMLUListElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLULISTELEMENT_PAS_C}
{$define DOMHTMLULISTELEMENT_PAS_C}

{ DOMHTMLUListElement }
  DOMHTMLUListElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCompact (newValue: Boolean); message 'setCompact:';
    function compact: Boolean; message 'compact';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end;

{$endif}
{$endif}
