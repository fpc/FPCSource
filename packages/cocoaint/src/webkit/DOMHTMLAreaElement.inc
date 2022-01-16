{ Parsed from WebKit.framework DOMHTMLAreaElement.h }


{$ifdef TYPES}
type
  DOMHTMLAreaElementPtr = ^DOMHTMLAreaElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLAreaElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlt(newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setCoords(newValue: NSString); message 'setCoords:';
    function coords: NSString; message 'coords';
    procedure setHref(newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setNoHref(newValue: ObjCBOOL); message 'setNoHref:';
    function noHref: ObjCBOOL; message 'noHref';
    procedure setShape(newValue: NSString); message 'setShape:';
    function shape: NSString; message 'shape';
    procedure setTarget(newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    procedure setAccessKey(newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    function hashName: NSString; message 'hashName';
    function host: NSString; message 'host';
    function hostname: NSString; message 'hostname';
    function pathname: NSString; message 'pathname';
    function port: NSString; message 'port';
    function protocol: NSString; message 'protocol';
    function search: NSString; message 'search';
    function absoluteLinkURL: NSURL; message 'absoluteLinkURL';
  end;
{$endif}

