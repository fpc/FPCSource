{ Parsed from Webkit.framework DOMHTMLOListElement.h }

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
  DOMHTMLOListElementPtr = DOMHTMLOListElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOLISTELEMENT_PAS_C}
{$define DOMHTMLOLISTELEMENT_PAS_C}

{ DOMHTMLOListElement }
  DOMHTMLOListElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCompact (newValue: Boolean); message 'setCompact:';
    function compact: Boolean; message 'compact';
    procedure setStart (newValue: cint); message 'setStart:';
    function start: cint; message 'start';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end;

{$endif}
{$endif}
