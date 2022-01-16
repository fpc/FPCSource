{ Parsed from WebKit.framework DOMHTMLParamElement.h }


{$ifdef TYPES}
type
  DOMHTMLParamElementPtr = ^DOMHTMLParamElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLParamElement = objcclass external (DOMHTMLElement)
  public
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
    procedure setValue(newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    procedure setValueType(newValue: NSString); message 'setValueType:';
    function valueType: NSString; message 'valueType';
  end;
{$endif}

