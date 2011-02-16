{ Parsed from Webkit.framework DOMHTMLHtmlElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLHTMLELEMENT_PAS_T}
{$define DOMHTMLHTMLELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHTMLELEMENT_PAS_R}
{$define DOMHTMLHTMLELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHTMLELEMENT_PAS_F}
{$define DOMHTMLHTMLELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHTMLELEMENT_PAS_S}
{$define DOMHTMLHTMLELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHtmlElement = objcclass;
  DOMHTMLHtmlElementPointer = ^DOMHTMLHtmlElement;
  DOMHTMLHtmlElementPtr = DOMHTMLHtmlElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHTMLELEMENT_PAS_C}
{$define DOMHTMLHTMLELEMENT_PAS_C}

{ DOMHTMLHtmlElement }
  DOMHTMLHtmlElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setVersion (newValue: NSString); message 'setVersion:';
    function version: NSString; message 'version';
  end;

{$endif}
{$endif}
