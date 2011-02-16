{ Parsed from Webkit.framework DOMHTMLAnchorElement.h }

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
  DOMHTMLAnchorElementPtr = DOMHTMLAnchorElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLANCHORELEMENT_PAS_C}
{$define DOMHTMLANCHORELEMENT_PAS_C}

{ DOMHTMLAnchorElement }
  DOMHTMLAnchorElement = objcclass external (DOMHTMLElement)
    
  public
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
    function hashName: NSString; message 'hashName';
    function host: NSString; message 'host';
    function hostname: NSString; message 'hostname';
    function pathname: NSString; message 'pathname';
    function port: NSString; message 'port';
    function protocol: NSString; message 'protocol';
    function search: NSString; message 'search';
    function text: NSString; message 'text';
    function absoluteLinkURL: NSURL; message 'absoluteLinkURL';
  end;

{$endif}
{$endif}
