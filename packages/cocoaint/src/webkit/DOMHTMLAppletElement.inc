{ Parsed from WebKit.framework DOMHTMLAppletElement.h }


{$ifdef TYPES}
type
  DOMHTMLAppletElementPtr = ^DOMHTMLAppletElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLAppletElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt(newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setArchive(newValue: NSString); message 'setArchive:';
    function archive: NSString; message 'archive';
    procedure setCode(newValue: NSString); message 'setCode:';
    function code: NSString; message 'code';
    procedure setCodeBase(newValue: NSString); message 'setCodeBase:';
    function codeBase: NSString; message 'codeBase';
    procedure setHeight(newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setHspace(newValue: cint); message 'setHspace:';
    function hspace: cint; message 'hspace';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setObject(newValue: NSString); message 'setObject:';
    function object_: NSString; message 'object';
    procedure setVspace(newValue: cint); message 'setVspace:';
    function vspace: cint; message 'vspace';
    procedure setWidth(newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end;
{$endif}

