{ Parsed from WebKit.framework DOMHTMLDocument.h }


{$ifdef TYPES}
type
  DOMHTMLDocumentPtr = ^DOMHTMLDocument;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLDocument = objcclass external (DOMDocument)
  public
    function embeds: DOMHTMLCollection; message 'embeds';
    function plugins: DOMHTMLCollection; message 'plugins';
    function scripts: DOMHTMLCollection; message 'scripts';
    function width: cint; message 'width';
    function height: cint; message 'height';
    procedure setDir(newValue: NSString); message 'setDir:';
    function dir: NSString; message 'dir';
    procedure setDesignMode(newValue: NSString); message 'setDesignMode:';
    function designMode: NSString; message 'designMode';
    function compatMode: NSString; message 'compatMode';
    procedure setBgColor(newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setFgColor(newValue: NSString); message 'setFgColor:';
    function fgColor: NSString; message 'fgColor';
    procedure setAlinkColor(newValue: NSString); message 'setAlinkColor:';
    function alinkColor: NSString; message 'alinkColor';
    procedure setLinkColor(newValue: NSString); message 'setLinkColor:';
    function linkColor: NSString; message 'linkColor';
    procedure setVlinkColor(newValue: NSString); message 'setVlinkColor:';
    function vlinkColor: NSString; message 'vlinkColor';
    procedure open; message 'open';
    procedure close; message 'close';
    procedure write (text: NSString); message 'write:';
    procedure writeln (text: NSString); message 'writeln:';
    procedure clear; message 'clear'; { available in 10_6 }
    procedure captureEvents; message 'captureEvents'; { available in 10_5 }
    procedure releaseEvents; message 'releaseEvents'; { available in 10_5 }
  end;
{$endif}

