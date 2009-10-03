{ Parsed from Webkit.framework DOMHTMLLinkElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLLINKELEMENT_PAS_T}
{$define DOMHTMLLINKELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLLINKELEMENT_PAS_R}
{$define DOMHTMLLINKELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLLINKELEMENT_PAS_F}
{$define DOMHTMLLINKELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLLINKELEMENT_PAS_S}
{$define DOMHTMLLINKELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLLinkElement = objcclass;
  DOMHTMLLinkElementPointer = ^DOMHTMLLinkElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLLINKELEMENT_PAS_C}
{$define DOMHTMLLINKELEMENT_PAS_C}

{ DOMHTMLLinkElement }
  DOMHTMLLinkElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLLinkElement; message 'alloc';

    procedure setCharset (newValue: NSString); message 'setCharset:';
    function charset: NSString; message 'charset';
    procedure setHref (newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setHreflang (newValue: NSString); message 'setHreflang:';
    function hreflang: NSString; message 'hreflang';
    procedure setMedia (newValue: NSString); message 'setMedia:';
    function media: NSString; message 'media';
    procedure setRel (newValue: NSString); message 'setRel:';
    function rel: NSString; message 'rel';
    procedure setRev (newValue: NSString); message 'setRev:';
    function rev: NSString; message 'rev';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    function absoluteLinkURL: NSURL; message 'absoluteLinkURL';
  end; external;

{$endif}
{$endif}
