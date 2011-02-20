{ Parsed from Webkit.framework DOMHTMLIFrameElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLIFRAMEELEMENT_PAS_T}
{$define DOMHTMLIFRAMEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLIFRAMEELEMENT_PAS_R}
{$define DOMHTMLIFRAMEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLIFRAMEELEMENT_PAS_F}
{$define DOMHTMLIFRAMEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLIFRAMEELEMENT_PAS_S}
{$define DOMHTMLIFRAMEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLIFrameElement = objcclass;
  DOMHTMLIFrameElementPointer = ^DOMHTMLIFrameElement;
  DOMHTMLIFrameElementPtr = DOMHTMLIFrameElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLIFRAMEELEMENT_PAS_C}
{$define DOMHTMLIFRAMEELEMENT_PAS_C}

{ DOMHTMLIFrameElement }
  DOMHTMLIFrameElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setFrameBorder (newValue: NSString); message 'setFrameBorder:';
    function frameBorder: NSString; message 'frameBorder';
    procedure setHeight (newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setLongDesc (newValue: NSString); message 'setLongDesc:';
    function longDesc: NSString; message 'longDesc';
    procedure setMarginHeight (newValue: NSString); message 'setMarginHeight:';
    function marginHeight: NSString; message 'marginHeight';
    procedure setMarginWidth (newValue: NSString); message 'setMarginWidth:';
    function marginWidth: NSString; message 'marginWidth';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setScrolling (newValue: NSString); message 'setScrolling:';
    function scrolling: NSString; message 'scrolling';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
    function contentDocument: DOMDocument; message 'contentDocument';
    function contentWindow: DOMAbstractView; message 'contentWindow';
  end;

{$endif}
{$endif}
