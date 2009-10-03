{ Parsed from Webkit.framework DOMHTMLFrameElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFRAMEELEMENT_PAS_C}
{$define DOMHTMLFRAMEELEMENT_PAS_C}

{ DOMHTMLFrameElement }
  DOMHTMLFrameElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLFrameElement; message 'alloc';

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
    procedure setScrolling (newValue: NSString); message 'setScrolling:';
    function scrolling: NSString; message 'scrolling';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    function contentDocument: DOMDocument; message 'contentDocument';
  end; external;

{$endif}
{$endif}
