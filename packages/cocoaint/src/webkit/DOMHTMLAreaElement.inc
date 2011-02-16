{ Parsed from Webkit.framework DOMHTMLAreaElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLAREAELEMENT_PAS_T}
{$define DOMHTMLAREAELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLAREAELEMENT_PAS_R}
{$define DOMHTMLAREAELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLAREAELEMENT_PAS_F}
{$define DOMHTMLAREAELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLAREAELEMENT_PAS_S}
{$define DOMHTMLAREAELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLAreaElement = objcclass;
  DOMHTMLAreaElementPointer = ^DOMHTMLAreaElement;
  DOMHTMLAreaElementPtr = DOMHTMLAreaElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLAREAELEMENT_PAS_C}
{$define DOMHTMLAREAELEMENT_PAS_C}

{ DOMHTMLAreaElement }
  DOMHTMLAreaElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setCoords (newValue: NSString); message 'setCoords:';
    function coords: NSString; message 'coords';
    procedure setHref (newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setNoHref (newValue: Boolean); message 'setNoHref:';
    function noHref: Boolean; message 'noHref';
    procedure setShape (newValue: NSString); message 'setShape:';
    function shape: NSString; message 'shape';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    function hashName: NSString; message 'hashName';
    function host: NSString; message 'host';
    function hostname: NSString; message 'hostname';
    function pathname: NSString; message 'pathname';
    function port: NSString; message 'port';
    function protocol: NSString; message 'protocol';
    function search: NSString; message 'search';
    function absoluteLinkURL: NSURL; message 'absoluteLinkURL';
  end;

{$endif}
{$endif}
