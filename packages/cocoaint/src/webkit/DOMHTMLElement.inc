{ Parsed from Webkit.framework DOMHTMLElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLELEMENT_PAS_T}
{$define DOMHTMLELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLELEMENT_PAS_R}
{$define DOMHTMLELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLELEMENT_PAS_F}
{$define DOMHTMLELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLELEMENT_PAS_S}
{$define DOMHTMLELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLElement = objcclass;
  DOMHTMLElementPointer = ^DOMHTMLElement;
  DOMHTMLElementPtr = DOMHTMLElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLELEMENT_PAS_C}
{$define DOMHTMLELEMENT_PAS_C}

{ DOMHTMLElement }
  DOMHTMLElement = objcclass external (DOMElement)
    
  public
    procedure setIdName (newValue: NSString); message 'setIdName:';
    function idName: NSString; message 'idName';
    procedure setTitle (newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setLang (newValue: NSString); message 'setLang:';
    function lang: NSString; message 'lang';
    procedure setDir (newValue: NSString); message 'setDir:';
    function dir: NSString; message 'dir';
    procedure setClassName (newValue: NSString); message 'setClassName:';
    function className: NSString; message 'className';
    procedure setTabIndex (newValue: cint); message 'setTabIndex:';
    function tabIndex: cint; message 'tabIndex';
    procedure setInnerHTML (newValue: NSString); message 'setInnerHTML:';
    function innerHTML: NSString; message 'innerHTML';
    procedure setInnerText (newValue: NSString); message 'setInnerText:';
    function innerText: NSString; message 'innerText';
    procedure setOuterHTML (newValue: NSString); message 'setOuterHTML:';
    function outerHTML: NSString; message 'outerHTML';
    procedure setOuterText (newValue: NSString); message 'setOuterText:';
    function outerText: NSString; message 'outerText';
    function children: DOMHTMLCollection; message 'children';
    procedure setContentEditable (newValue: NSString); message 'setContentEditable:';
    function contentEditable: NSString; message 'contentEditable';
    function isContentEditable: Boolean; message 'isContentEditable';
    function titleDisplayString: NSString; message 'titleDisplayString';
  end;

{$endif}
{$endif}
