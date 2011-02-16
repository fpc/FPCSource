{ Parsed from Webkit.framework DOMHTMLModElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLMODELEMENT_PAS_T}
{$define DOMHTMLMODELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLMODELEMENT_PAS_R}
{$define DOMHTMLMODELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLMODELEMENT_PAS_F}
{$define DOMHTMLMODELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLMODELEMENT_PAS_S}
{$define DOMHTMLMODELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLModElement = objcclass;
  DOMHTMLModElementPointer = ^DOMHTMLModElement;
  DOMHTMLModElementPtr = DOMHTMLModElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMODELEMENT_PAS_C}
{$define DOMHTMLMODELEMENT_PAS_C}

{ DOMHTMLModElement }
  DOMHTMLModElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCite (newValue: NSString); message 'setCite:';
    function cite: NSString; message 'cite';
    procedure setDateTime (newValue: NSString); message 'setDateTime:';
    function dateTime: NSString; message 'dateTime';
  end;

{$endif}
{$endif}
