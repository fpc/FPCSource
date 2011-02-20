{ Parsed from Webkit.framework DOMHTMLImageElement.h }

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
  DOMHTMLImageElementPtr = DOMHTMLImageElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLIMAGEELEMENT_PAS_C}
{$define DOMHTMLIMAGEELEMENT_PAS_C}

{ DOMHTMLImageElement }
  DOMHTMLImageElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setBorder (newValue: NSString); message 'setBorder:';
    function border: NSString; message 'border';
    procedure setHeight (newValue: cint); message 'setHeight:';
    function height: cint; message 'height';
    procedure setHspace (newValue: cint); message 'setHspace:';
    function hspace: cint; message 'hspace';
    procedure setIsMap (newValue: Boolean); message 'setIsMap:';
    function isMap: Boolean; message 'isMap';
    procedure setLongDesc (newValue: NSString); message 'setLongDesc:';
    function longDesc: NSString; message 'longDesc';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setUseMap (newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    procedure setVspace (newValue: cint); message 'setVspace:';
    function vspace: cint; message 'vspace';
    procedure setWidth (newValue: cint); message 'setWidth:';
    function width: cint; message 'width';
    function complete: Boolean; message 'complete';
    procedure setLowsrc (newValue: NSString); message 'setLowsrc:';
    function lowsrc: NSString; message 'lowsrc';
    function naturalHeight: cint; message 'naturalHeight';
    function naturalWidth: cint; message 'naturalWidth';
    function x: cint; message 'x';
    function y: cint; message 'y';
    function altDisplayString: NSString; message 'altDisplayString';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
  end;

{$endif}
{$endif}
