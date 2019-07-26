{ Parsed from WebKit.framework DOMHTMLTextAreaElement.h }


{$ifdef TYPES}
type
  DOMHTMLTextAreaElementPtr = ^DOMHTMLTextAreaElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTextAreaElement = objcclass external (DOMHTMLElement)
  public
    procedure setAutofocus(newValue: ObjCBOOL); message 'setAutofocus:';
    function autofocus: ObjCBOOL; message 'autofocus';
    procedure setCols(newValue: cint); message 'setCols:';
    function cols: cint; message 'cols';
    procedure setDisabled(newValue: ObjCBOOL); message 'setDisabled:';
    function disabled: ObjCBOOL; message 'disabled';
    function form: DOMHTMLFormElement; message 'form';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setReadOnly(newValue: ObjCBOOL); message 'setReadOnly:';
    function readOnly: ObjCBOOL; message 'readOnly';
    procedure setRows(newValue: cint); message 'setRows:';
    function rows: cint; message 'rows';
    function type_: NSString; message 'type';
    procedure setDefaultValue(newValue: NSString); message 'setDefaultValue:';
    function defaultValue: NSString; message 'defaultValue';
    procedure setValue(newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function willValidate: ObjCBOOL; message 'willValidate';
    procedure setSelectionStart(newValue: cint); message 'setSelectionStart:';
    function selectionStart: cint; message 'selectionStart';
    procedure setSelectionEnd(newValue: cint); message 'setSelectionEnd:';
    function selectionEnd: cint; message 'selectionEnd';
    procedure setAccessKey(newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure select; message 'select';
    procedure setSelectionRange_end (start: cint; end_: cint); message 'setSelectionRange:end:'; { available in 10_5 }
  end;
{$endif}

