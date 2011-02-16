{ Parsed from Webkit.framework DOMHTMLButtonElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_T}
{$define DOMHTMLBUTTONELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_R}
{$define DOMHTMLBUTTONELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_F}
{$define DOMHTMLBUTTONELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_S}
{$define DOMHTMLBUTTONELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLButtonElement = objcclass;
  DOMHTMLButtonElementPointer = ^DOMHTMLButtonElement;
  DOMHTMLButtonElementPtr = DOMHTMLButtonElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_C}
{$define DOMHTMLBUTTONELEMENT_PAS_C}

{ DOMHTMLButtonElement }
  DOMHTMLButtonElement = objcclass external (DOMHTMLElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setDisabled (newValue: Boolean); message 'setDisabled:';
    function disabled: Boolean; message 'disabled';
    procedure setAutofocus (newValue: Boolean); message 'setAutofocus:';
    function autofocus: Boolean; message 'autofocus';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function willValidate: Boolean; message 'willValidate';
    procedure click; message 'click';
  end;

{$endif}
{$endif}
