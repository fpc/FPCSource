{ Parsed from Webkit.framework DOMHTMLFormElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFORMELEMENT_PAS_C}
{$define DOMHTMLFORMELEMENT_PAS_C}

{ DOMHTMLFormElement }
  DOMHTMLFormElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLFormElement; message 'alloc';

    function elements: DOMHTMLCollection; message 'elements';
    function length: cint; message 'length';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setAcceptCharset (newValue: NSString); message 'setAcceptCharset:';
    function acceptCharset: NSString; message 'acceptCharset';
    procedure setAction (newValue: NSString); message 'setAction:';
    function action: NSString; message 'action';
    procedure setEnctype (newValue: NSString); message 'setEnctype:';
    function enctype: NSString; message 'enctype';
    procedure setMethod (newValue: NSString); message 'setMethod:';
    function method: NSString; message 'method';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    procedure submit; message 'submit';
    procedure reset; message 'reset';
  end; external;

{$endif}
{$endif}
