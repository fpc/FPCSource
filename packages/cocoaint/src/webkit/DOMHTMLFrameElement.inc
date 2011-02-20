{ Parsed from Webkit.framework DOMHTMLFrameElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLFRAMEELEMENT_PAS_T}
{$define DOMHTMLFRAMEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLFRAMEELEMENT_PAS_R}
{$define DOMHTMLFRAMEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLFRAMEELEMENT_PAS_F}
{$define DOMHTMLFRAMEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLFRAMEELEMENT_PAS_S}
{$define DOMHTMLFRAMEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLFrameElement = objcclass;
  DOMHTMLFrameElementPointer = ^DOMHTMLFrameElement;
  DOMHTMLFrameElementPtr = DOMHTMLFrameElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFRAMEELEMENT_PAS_C}
{$define DOMHTMLFRAMEELEMENT_PAS_C}

{ DOMHTMLFrameElement }
  DOMHTMLFrameElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setFrameBorder (newValue: NSString); message 'setFrameBorder:';
    function frameBorder: NSString; message 'frameBorder';
    procedure setLongDesc (newValue: NSString); message 'setLongDesc:';
    function longDesc: NSString; message 'longDesc';
    procedure setMarginHeight (newValue: NSString); message 'setMarginHeight:';
    function marginHeight: NSString; message 'marginHeight';
    procedure setMarginWidth (newValue: NSString); message 'setMarginWidth:';
    function marginWidth: NSString; message 'marginWidth';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setNoResize (newValue: Boolean); message 'setNoResize:';
    function noResize: Boolean; message 'noResize';
    procedure setScrolling (newValue: NSString); message 'setScrolling:';
    function scrolling: NSString; message 'scrolling';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    function contentDocument: DOMDocument; message 'contentDocument';
    function contentWindow: DOMAbstractView; message 'contentWindow';
    procedure setLocation (newValue: NSString); message 'setLocation:';
    function location: NSString; message 'location';
    function width: cint; message 'width';
    function height: cint; message 'height';
  end;

{$endif}
{$endif}
