{ Parsed from WebKit.framework DOMHTMLFormElement.h }


{$ifdef TYPES}
type
  DOMHTMLFormElementPtr = ^DOMHTMLFormElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLFormElement = objcclass external (DOMHTMLElement)
  public
    procedure setAcceptCharset(newValue: NSString); message 'setAcceptCharset:';
    function acceptCharset: NSString; message 'acceptCharset';
    procedure setAction(newValue: NSString); message 'setAction:';
    function action: NSString; message 'action';
    procedure setEnctype(newValue: NSString); message 'setEnctype:';
    function enctype: NSString; message 'enctype';
    procedure setEncoding(newValue: NSString); message 'setEncoding:';
    function encoding: NSString; message 'encoding';
    procedure setMethod(newValue: NSString); message 'setMethod:';
    function method: NSString; message 'method';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setTarget(newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    function elements: DOMHTMLCollection; message 'elements';
    function length: cint; message 'length';
    procedure submit; message 'submit';
    procedure reset; message 'reset';
  end;
{$endif}

