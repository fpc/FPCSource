{ Parsed from Webkit.framework DOMHTMLHeadElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLHEADELEMENT_PAS_T}
{$define DOMHTMLHEADELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHEADELEMENT_PAS_R}
{$define DOMHTMLHEADELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHEADELEMENT_PAS_F}
{$define DOMHTMLHEADELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHEADELEMENT_PAS_S}
{$define DOMHTMLHEADELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHeadElement = objcclass;
  DOMHTMLHeadElementPointer = ^DOMHTMLHeadElement;
  DOMHTMLHeadElementPtr = DOMHTMLHeadElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHEADELEMENT_PAS_C}
{$define DOMHTMLHEADELEMENT_PAS_C}

{ DOMHTMLHeadElement }
  DOMHTMLHeadElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setProfile (newValue: NSString); message 'setProfile:';
    function profile: NSString; message 'profile';
  end;

{$endif}
{$endif}
