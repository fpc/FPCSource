{ Parsed from Webkit.framework DOMHTMLElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLELEMENT_PAS_C}
{$define DOMHTMLELEMENT_PAS_C}

{ DOMHTMLElement }
  DOMHTMLElement = objcclass(DOMElement)
    
  public
    class function alloc: DOMHTMLElement; message 'alloc';

    procedure setIdName (newValue: NSString); message 'setIdName:';
    function idName: NSString; message 'idName';
    procedure setTitle (newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setLang (newValue: NSString); message 'setLang:';
    function lang: NSString; message 'lang';
    procedure setDir (newValue: NSString); message 'setDir:';
    function dir: NSString; message 'dir';
    procedure setClassName (newValue: NSString); message 'setClassName:';
    function className_: NSString; message 'className_';
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
  end; external;

{$endif}
{$endif}
