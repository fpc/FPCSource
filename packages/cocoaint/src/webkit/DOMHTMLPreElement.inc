{ Parsed from WebKit.framework DOMHTMLPreElement.h }


{$ifdef TYPES}
type
  DOMHTMLPreElementPtr = ^DOMHTMLPreElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLPreElement = objcclass external (DOMHTMLElement)
  public
    procedure setWidth(newValue: cint); message 'setWidth:';
    function width: cint; message 'width';
    procedure setWrap(newValue: ObjCBOOL); message 'setWrap:';
    function wrap: ObjCBOOL; message 'wrap';
  end;
{$endif}

