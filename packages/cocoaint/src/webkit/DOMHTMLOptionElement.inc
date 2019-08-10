{ Parsed from WebKit.framework DOMHTMLOptionElement.h }


{$ifdef TYPES}
type
  DOMHTMLOptionElementPtr = ^DOMHTMLOptionElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLOptionElement = objcclass external (DOMHTMLElement)
  public
    procedure setDisabled(newValue: ObjCBOOL); message 'setDisabled:';
    function disabled: ObjCBOOL; message 'disabled';
    function form: DOMHTMLFormElement; message 'form';
    procedure setLabel(newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label';
    procedure setDefaultSelected(newValue: ObjCBOOL); message 'setDefaultSelected:';
    function defaultSelected: ObjCBOOL; message 'defaultSelected';
    procedure setSelected(newValue: ObjCBOOL); message 'setSelected:';
    function selected: ObjCBOOL; message 'selected';
    procedure setValue(newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function text: NSString; message 'text';
    function index: cint; message 'index';
  end;
{$endif}

