{ Parsed from WebKit.framework DOMHTMLInputElement.h }


{$ifdef TYPES}
type
  DOMHTMLInputElementPtr = ^DOMHTMLInputElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLInputElement = objcclass external (DOMHTMLElement)
  public
    procedure setAccept(newValue: NSString); message 'setAccept:';
    function accept: NSString; message 'accept';
    procedure setAlt(newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setAutofocus(newValue: ObjCBOOL); message 'setAutofocus:';
    function autofocus: ObjCBOOL; message 'autofocus';
    procedure setDefaultChecked(newValue: ObjCBOOL); message 'setDefaultChecked:';
    function defaultChecked: ObjCBOOL; message 'defaultChecked';
    procedure setChecked(newValue: ObjCBOOL); message 'setChecked:';
    function checked: ObjCBOOL; message 'checked';
    procedure setDisabled(newValue: ObjCBOOL); message 'setDisabled:';
    function disabled: ObjCBOOL; message 'disabled';
    function form: DOMHTMLFormElement; message 'form';
    procedure setFiles(newValue: DOMFileList); message 'setFiles:';
    function files: DOMFileList; message 'files';
    procedure setIndeterminate(newValue: ObjCBOOL); message 'setIndeterminate:';
    function indeterminate: ObjCBOOL; message 'indeterminate';
    procedure setMaxLength(newValue: cint); message 'setMaxLength:';
    function maxLength: cint; message 'maxLength';
    procedure setMultiple(newValue: ObjCBOOL); message 'setMultiple:';
    function multiple: ObjCBOOL; message 'multiple';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setReadOnly(newValue: ObjCBOOL); message 'setReadOnly:';
    function readOnly: ObjCBOOL; message 'readOnly';
    procedure setSize(newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
    procedure setSrc(newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType(newValue: NSString); message 'setType:';
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
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setUseMap(newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    procedure setAccessKey(newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    function altDisplayString: NSString; message 'altDisplayString';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
    procedure select; message 'select';
    procedure setSelectionRange_end (start: cint; end_: cint); message 'setSelectionRange:end:'; { available in 10_5 }
    procedure click; message 'click';
  end;
{$endif}

