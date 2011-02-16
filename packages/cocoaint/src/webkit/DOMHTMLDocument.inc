{ Parsed from Webkit.framework DOMHTMLDocument.h }

{$ifdef TYPES}
{$ifndef DOMHTMLDOCUMENT_PAS_T}
{$define DOMHTMLDOCUMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLDOCUMENT_PAS_R}
{$define DOMHTMLDOCUMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLDOCUMENT_PAS_F}
{$define DOMHTMLDOCUMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLDOCUMENT_PAS_S}
{$define DOMHTMLDOCUMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLDocument = objcclass;
  DOMHTMLDocumentPointer = ^DOMHTMLDocument;
  DOMHTMLDocumentPtr = DOMHTMLDocumentPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDOCUMENT_PAS_C}
{$define DOMHTMLDOCUMENT_PAS_C}

{ DOMHTMLDocument }
  DOMHTMLDocument = objcclass external (DOMDocument)
    
  public
    function embeds: DOMHTMLCollection; message 'embeds';
    function plugins: DOMHTMLCollection; message 'plugins';
    function scripts: DOMHTMLCollection; message 'scripts';
    function width: cint; message 'width';
    function height: cint; message 'height';
    procedure setDir (newValue: NSString); message 'setDir:';
    function dir: NSString; message 'dir';
    procedure setDesignMode (newValue: NSString); message 'setDesignMode:';
    function designMode: NSString; message 'designMode';
    function compatMode: NSString; message 'compatMode';
    function activeElement: DOMElement; message 'activeElement';
    procedure setBgColor (newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setFgColor (newValue: NSString); message 'setFgColor:';
    function fgColor: NSString; message 'fgColor';
    procedure setAlinkColor (newValue: NSString); message 'setAlinkColor:';
    function alinkColor: NSString; message 'alinkColor';
    procedure setLinkColor (newValue: NSString); message 'setLinkColor:';
    function linkColor: NSString; message 'linkColor';
    procedure setVlinkColor (newValue: NSString); message 'setVlinkColor:';
    function vlinkColor: NSString; message 'vlinkColor';
    procedure open; message 'open';
    procedure close; message 'close';
    procedure write(text: NSString); message 'write:';
    procedure writeln(text: NSString); message 'writeln:';
    procedure clear; message 'clear';
    procedure captureEvents; message 'captureEvents';
    procedure releaseEvents; message 'releaseEvents';
    function hasFocus: Boolean; message 'hasFocus';
  end;

{$endif}
{$endif}
