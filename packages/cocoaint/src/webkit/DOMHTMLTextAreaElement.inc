{ Parsed from Webkit.framework DOMHTMLTextAreaElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_T}
{$define DOMHTMLTEXTAREAELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_R}
{$define DOMHTMLTEXTAREAELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_F}
{$define DOMHTMLTEXTAREAELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_S}
{$define DOMHTMLTEXTAREAELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTextAreaElement = objcclass;
  DOMHTMLTextAreaElementPointer = ^DOMHTMLTextAreaElement;
  DOMHTMLTextAreaElementPtr = DOMHTMLTextAreaElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_C}
{$define DOMHTMLTEXTAREAELEMENT_PAS_C}

{ DOMHTMLTextAreaElement }
  DOMHTMLTextAreaElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setDefaultValue (newValue: NSString); message 'setDefaultValue:';
    function defaultValue: NSString; message 'defaultValue';
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setCols (newValue: cint); message 'setCols:';
    function cols: cint; message 'cols';
    procedure setDisabled (newValue: Boolean); message 'setDisabled:';
    function disabled: Boolean; message 'disabled';
    procedure setAutofocus (newValue: Boolean); message 'setAutofocus:';
    function autofocus: Boolean; message 'autofocus';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setReadOnly (newValue: Boolean); message 'setReadOnly:';
    function readOnly: Boolean; message 'readOnly';
    procedure setRows (newValue: cint); message 'setRows:';
    function rows: cint; message 'rows';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function willValidate: Boolean; message 'willValidate';
    procedure setSelectionStart (newValue: cint); message 'setSelectionStart:';
    function selectionStart: cint; message 'selectionStart';
    procedure setSelectionEnd (newValue: cint); message 'setSelectionEnd:';
    function selectionEnd: cint; message 'selectionEnd';
    procedure select; message 'select';
    procedure setSelectionRange_end(start: cint; end_: cint); message 'setSelectionRange:end:';
  end;

{$endif}
{$endif}
