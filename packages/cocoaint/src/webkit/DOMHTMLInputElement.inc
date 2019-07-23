{ Parsed from Webkit.framework DOMHTMLInputElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLINPUTELEMENT_PAS_T}
{$define DOMHTMLINPUTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLINPUTELEMENT_PAS_R}
{$define DOMHTMLINPUTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLINPUTELEMENT_PAS_F}
{$define DOMHTMLINPUTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLINPUTELEMENT_PAS_S}
{$define DOMHTMLINPUTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLInputElement = objcclass;
  DOMHTMLInputElementPointer = ^DOMHTMLInputElement;
  DOMHTMLInputElementPtr = DOMHTMLInputElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLINPUTELEMENT_PAS_C}
{$define DOMHTMLINPUTELEMENT_PAS_C}

{ DOMHTMLInputElement }
  DOMHTMLInputElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setDefaultValue (newValue: NSString); message 'setDefaultValue:';
    function defaultValue: NSString; message 'defaultValue';
    procedure setDefaultChecked (newValue: ObjCBOOL); message 'setDefaultChecked:';
    function defaultChecked: ObjCBOOL; message 'defaultChecked';
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccept (newValue: NSString); message 'setAccept:';
    function accept: NSString; message 'accept';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setChecked (newValue: ObjCBOOL); message 'setChecked:';
    function checked: ObjCBOOL; message 'checked';
    procedure setDisabled (newValue: ObjCBOOL); message 'setDisabled:';
    function disabled: ObjCBOOL; message 'disabled';
    procedure setAutofocus (newValue: ObjCBOOL); message 'setAutofocus:';
    function autofocus: ObjCBOOL; message 'autofocus';
    procedure setMaxLength (newValue: cint); message 'setMaxLength:';
    function maxLength: cint; message 'maxLength';
    procedure setMultiple (newValue: ObjCBOOL); message 'setMultiple:';
    function multiple: ObjCBOOL; message 'multiple';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setReadOnly (newValue: ObjCBOOL); message 'setReadOnly:';
    function readOnly: ObjCBOOL; message 'readOnly';
    procedure setSize (newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setUseMap (newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function willValidate: ObjCBOOL; message 'willValidate';
    procedure setIndeterminate (newValue: ObjCBOOL); message 'setIndeterminate:';
    function indeterminate: ObjCBOOL; message 'indeterminate';
    procedure setSelectionStart (newValue: cint); message 'setSelectionStart:';
    function selectionStart: cint; message 'selectionStart';
    procedure setSelectionEnd (newValue: cint); message 'setSelectionEnd:';
    function selectionEnd: cint; message 'selectionEnd';
    function altDisplayString: NSString; message 'altDisplayString';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
    function files: DOMFileList; message 'files';
    procedure select; message 'select';
    procedure click; message 'click';
    procedure setSelectionRange_end(start: cint; end_: cint); message 'setSelectionRange:end:';
  end;

{$endif}
{$endif}
