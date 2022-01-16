{ Parsed from WebKit.framework DOMHTMLElement.h }


{$ifdef TYPES}
type
  DOMHTMLElementPtr = ^DOMHTMLElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLElement = objcclass external (DOMElement)
  public
    procedure setIdName(newValue: NSString); message 'setIdName:';
    function idName: NSString; message 'idName';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setLang(newValue: NSString); message 'setLang:';
    function lang: NSString; message 'lang';
    procedure setDir(newValue: NSString); message 'setDir:';
    function dir: NSString; message 'dir';
    procedure setTabIndex(newValue: cint); message 'setTabIndex:';
    function tabIndex: cint; message 'tabIndex';
    procedure setAccessKey(newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setInnerHTML(newValue: NSString); message 'setInnerHTML:';
    function innerHTML: NSString; message 'innerHTML';
    procedure setInnerText(newValue: NSString); message 'setInnerText:';
    function innerText: NSString; message 'innerText';
    procedure setOuterHTML(newValue: NSString); message 'setOuterHTML:';
    function outerHTML: NSString; message 'outerHTML';
    procedure setOuterText(newValue: NSString); message 'setOuterText:';
    function outerText: NSString; message 'outerText';
    function children: DOMHTMLCollection; message 'children';
    procedure setContentEditable(newValue: NSString); message 'setContentEditable:';
    function contentEditable: NSString; message 'contentEditable';
    function isContentEditable: ObjCBOOL; message 'isContentEditable';
    function titleDisplayString: NSString; message 'titleDisplayString';
    procedure click; message 'click'; { available in 10_8 }
  end;
{$endif}

