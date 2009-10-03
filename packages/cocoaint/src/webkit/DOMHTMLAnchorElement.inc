{ Parsed from Webkit.framework DOMHTMLAnchorElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLANCHORELEMENT_PAS_T}
{$define DOMHTMLANCHORELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLANCHORELEMENT_PAS_R}
{$define DOMHTMLANCHORELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLANCHORELEMENT_PAS_F}
{$define DOMHTMLANCHORELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLANCHORELEMENT_PAS_S}
{$define DOMHTMLANCHORELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLAnchorElement = objcclass;
  DOMHTMLAnchorElementPointer = ^DOMHTMLAnchorElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLANCHORELEMENT_PAS_C}
{$define DOMHTMLANCHORELEMENT_PAS_C}

{ DOMHTMLAnchorElement }
  DOMHTMLAnchorElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLAnchorElement; message 'alloc';

    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setCharset (newValue: NSString); message 'setCharset:';
    function charset: NSString; message 'charset';
    procedure setCoords (newValue: NSString); message 'setCoords:';
    function coords: NSString; message 'coords';
    procedure setHref (newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setHreflang (newValue: NSString); message 'setHreflang:';
    function hreflang: NSString; message 'hreflang';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setRel (newValue: NSString); message 'setRel:';
    function rel: NSString; message 'rel';
    procedure setRev (newValue: NSString); message 'setRev:';
    function rev: NSString; message 'rev';
    procedure setShape (newValue: NSString); message 'setShape:';
    function shape: NSString; message 'shape';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    function absoluteLinkURL: NSURL; message 'absoluteLinkURL';
    procedure blur; message 'blur';
    procedure focus; message 'focus';
  end; external;

{$endif}
{$endif}
