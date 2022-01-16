{ Parsed from WebKit.framework DOMHTMLIFrameElement.h }


{$ifdef TYPES}
type
  DOMHTMLIFrameElementPtr = ^DOMHTMLIFrameElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLIFrameElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setFrameBorder(newValue: NSString); message 'setFrameBorder:';
    function frameBorder: NSString; message 'frameBorder';
    procedure setHeight(newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setLongDesc(newValue: NSString); message 'setLongDesc:';
    function longDesc: NSString; message 'longDesc';
    procedure setMarginHeight(newValue: NSString); message 'setMarginHeight:';
    function marginHeight: NSString; message 'marginHeight';
    procedure setMarginWidth(newValue: NSString); message 'setMarginWidth:';
    function marginWidth: NSString; message 'marginWidth';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setScrolling(newValue: NSString); message 'setScrolling:';
    function scrolling: NSString; message 'scrolling';
    procedure setSrc(newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setWidth(newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
    function contentDocument: DOMDocument; message 'contentDocument';
    function contentWindow: DOMAbstractView; message 'contentWindow';
  end;
{$endif}

