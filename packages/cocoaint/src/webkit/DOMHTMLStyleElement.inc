{ Parsed from Webkit.framework DOMHTMLStyleElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_T}
{$define DOMHTMLSTYLEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_R}
{$define DOMHTMLSTYLEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_F}
{$define DOMHTMLSTYLEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_S}
{$define DOMHTMLSTYLEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLStyleElement = objcclass;
  DOMHTMLStyleElementPointer = ^DOMHTMLStyleElement;
  DOMHTMLStyleElementPtr = DOMHTMLStyleElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_C}
{$define DOMHTMLSTYLEELEMENT_PAS_C}

{ DOMHTMLStyleElement }
  DOMHTMLStyleElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setDisabled (newValue: Boolean); message 'setDisabled:';
    function disabled: Boolean; message 'disabled';
    procedure setMedia (newValue: NSString); message 'setMedia:';
    function media: NSString; message 'media';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    function sheet: DOMStyleSheet; message 'sheet';
  end;

{$endif}
{$endif}
