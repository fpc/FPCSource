{ Parsed from WebKit.framework DOMHTMLImageElement.h }


{$ifdef TYPES}
type
  DOMHTMLImageElementPtr = ^DOMHTMLImageElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLImageElement = objcclass external (DOMHTMLElement)
  public
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt(newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setBorder(newValue: NSString); message 'setBorder:';
    function border: NSString; message 'border';
    procedure setHeight(newValue: cint); message 'setHeight:';
    function height: cint; message 'height';
    procedure setHspace(newValue: cint); message 'setHspace:';
    function hspace: cint; message 'hspace';
    procedure setIsMap(newValue: ObjCBOOL); message 'setIsMap:';
    function isMap: ObjCBOOL; message 'isMap';
    procedure setLongDesc(newValue: NSString); message 'setLongDesc:';
    function longDesc: NSString; message 'longDesc';
    procedure setSrc(newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setUseMap(newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    procedure setVspace(newValue: cint); message 'setVspace:';
    function vspace: cint; message 'vspace';
    procedure setWidth(newValue: cint); message 'setWidth:';
    function width: cint; message 'width';
    function complete: ObjCBOOL; message 'complete';
    procedure setLowsrc(newValue: NSString); message 'setLowsrc:';
    function lowsrc: NSString; message 'lowsrc';
    function naturalHeight: cint; message 'naturalHeight';
    function naturalWidth: cint; message 'naturalWidth';
    function x: cint; message 'x';
    function y: cint; message 'y';
    function altDisplayString: NSString; message 'altDisplayString';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
  end;
{$endif}

