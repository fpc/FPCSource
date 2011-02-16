{ Parsed from Webkit.framework DOMHTMLMenuElement.h }

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
  DOMHTMLMenuElementPtr = DOMHTMLMenuElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMENUELEMENT_PAS_C}
{$define DOMHTMLMENUELEMENT_PAS_C}

{ DOMHTMLMenuElement }
  DOMHTMLMenuElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCompact (newValue: Boolean); message 'setCompact:';
    function compact: Boolean; message 'compact';
  end;

{$endif}
{$endif}
