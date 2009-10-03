{ Parsed from Webkit.framework DOMHTMLImageElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLIMAGEELEMENT_PAS_T}
{$define DOMHTMLIMAGEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLIMAGEELEMENT_PAS_R}
{$define DOMHTMLIMAGEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLIMAGEELEMENT_PAS_F}
{$define DOMHTMLIMAGEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLIMAGEELEMENT_PAS_S}
{$define DOMHTMLIMAGEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLImageElement = objcclass;
  DOMHTMLImageElementPointer = ^DOMHTMLImageElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLIMAGEELEMENT_PAS_C}
{$define DOMHTMLIMAGEELEMENT_PAS_C}

{ DOMHTMLImageElement }
  DOMHTMLImageElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLImageElement; message 'alloc';

    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setBorder (newValue: NSString); message 'setBorder:';
    function border: NSString; message 'border';
    procedure setLongDesc (newValue: NSString); message 'setLongDesc:';
    function longDesc: NSString; message 'longDesc';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setUseMap (newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    function altDisplayString: NSString; message 'altDisplayString';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
  end; external;

{$endif}
{$endif}
