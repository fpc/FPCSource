{ Parsed from Webkit.framework DOMHTMLFormElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLFORMELEMENT_PAS_T}
{$define DOMHTMLFORMELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLFORMELEMENT_PAS_R}
{$define DOMHTMLFORMELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLFORMELEMENT_PAS_F}
{$define DOMHTMLFORMELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLFORMELEMENT_PAS_S}
{$define DOMHTMLFORMELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLFormElement = objcclass;
  DOMHTMLFormElementPointer = ^DOMHTMLFormElement;
  DOMHTMLFormElementPtr = DOMHTMLFormElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFORMELEMENT_PAS_C}
{$define DOMHTMLFORMELEMENT_PAS_C}

{ DOMHTMLFormElement }
  DOMHTMLFormElement = objcclass external (DOMHTMLElement)
    
  public
    function elements: DOMHTMLCollection; message 'elements';
    function length: cint; message 'length';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setAcceptCharset (newValue: NSString); message 'setAcceptCharset:';
    function acceptCharset: NSString; message 'acceptCharset';
    procedure setAction (newValue: NSString); message 'setAction:';
    function action: NSString; message 'action';
    procedure setEncoding (newValue: NSString); message 'setEncoding:';
    function encoding: NSString; message 'encoding';
    procedure setEnctype (newValue: NSString); message 'setEnctype:';
    function enctype: NSString; message 'enctype';
    procedure setMethod (newValue: NSString); message 'setMethod:';
    function method: NSString; message 'method';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    procedure submit; message 'submit';
    procedure reset; message 'reset';
  end;

{$endif}
{$endif}
