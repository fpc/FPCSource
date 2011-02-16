{ Parsed from Webkit.framework DOMHTMLDirectoryElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_T}
{$define DOMHTMLDIRECTORYELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_R}
{$define DOMHTMLDIRECTORYELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_F}
{$define DOMHTMLDIRECTORYELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_S}
{$define DOMHTMLDIRECTORYELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLDirectoryElement = objcclass;
  DOMHTMLDirectoryElementPointer = ^DOMHTMLDirectoryElement;
  DOMHTMLDirectoryElementPtr = DOMHTMLDirectoryElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_C}
{$define DOMHTMLDIRECTORYELEMENT_PAS_C}

{ DOMHTMLDirectoryElement }
  DOMHTMLDirectoryElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCompact (newValue: Boolean); message 'setCompact:';
    function compact: Boolean; message 'compact';
  end;

{$endif}
{$endif}
