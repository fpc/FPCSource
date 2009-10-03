{ Parsed from Webkit.framework DOMHTMLAreaElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLAREAELEMENT_PAS_C}
{$define DOMHTMLAREAELEMENT_PAS_C}

{ DOMHTMLAreaElement }
  DOMHTMLAreaElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLAreaElement; message 'alloc';

    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setCoords (newValue: NSString); message 'setCoords:';
    function coords: NSString; message 'coords';
    procedure setHref (newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setShape (newValue: NSString); message 'setShape:';
    function shape: NSString; message 'shape';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    function absoluteLinkURL: NSURL; message 'absoluteLinkURL';
  end; external;

{$endif}
{$endif}
