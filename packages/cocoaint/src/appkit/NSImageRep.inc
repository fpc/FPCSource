{ Parsed from Appkit.framework NSImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSIMAGEREP_PAS_H}
{$define NSIMAGEREP_PAS_H}
type
  NSImageRepPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSIMAGEREP_PAS_T}
{$define NSIMAGEREP_PAS_T}

{ Constants }

const
  NSImageRepMatchesDevice = 0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSIMAGEREP_PAS_R}
{$define NSIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSIMAGEREP_PAS_F}
{$define NSIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSIMAGEREP_PAS_S}
{$define NSIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSImageRep = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSIMAGEREP_PAS_C}
{$define NSIMAGEREP_PAS_C}

{ NSImageRep }
  NSImageRep = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __repFlags: bitpacked record
        hasAlpha: 0..1;
        isOpaque: 0..1;
        cacheParamsComputed: 0..1;
        cacheAlphaComputed: 0..1;
        loadState: 0..((1 shl 2)-1);
        keepCacheWindow: 0..1;
        reserved: 0..1;
        bitsPerSample: 0..((1 shl 8)-1);
        gsaved: 0..((1 shl 16)-1);
      end;
    __colorSpaceName: NSString;
    __size: NSSize;
    __pixelsWide: cint;
    __pixelsHigh: cint;
    
  public
    class function alloc: NSImageRep; message 'alloc';

    function draw: Boolean; message 'draw';
    function drawAtPoint(point: NSPoint): Boolean; message 'drawAtPoint:';
    function drawInRect(rect: NSRect): Boolean; message 'drawInRect:';
    procedure setSize(aSize: NSSize); message 'setSize:';
    function size: NSSize; message 'size';
    procedure setAlpha(flag: Boolean); message 'setAlpha:';
    function hasAlpha: Boolean; message 'hasAlpha';
    procedure setOpaque(flag: Boolean); message 'setOpaque:';
    function isOpaque: Boolean; message 'isOpaque';
    procedure setColorSpaceName(string_: NSString); message 'setColorSpaceName:';
    function colorSpaceName: NSString; message 'colorSpaceName';
    procedure setBitsPerSample(anInt: NSInteger); message 'setBitsPerSample:';
    function bitsPerSample: NSInteger; message 'bitsPerSample';
    procedure setPixelsWide(anInt: NSInteger); message 'setPixelsWide:';
    function pixelsWide: NSInteger; message 'pixelsWide';
    procedure setPixelsHigh(anInt: NSInteger); message 'setPixelsHigh:';
    function pixelsHigh: NSInteger; message 'pixelsHigh';
    class procedure registerImageRepClass(imageRepClass: Pobjc_class); message 'registerImageRepClass:';
    class procedure unregisterImageRepClass(imageRepClass: Pobjc_class); message 'unregisterImageRepClass:';
    class function registeredImageRepClasses: NSArray; message 'registeredImageRepClasses';
    class function imageRepClassForFileType(type_: NSString): Pobjc_class; message 'imageRepClassForFileType:';
    class function imageRepClassForPasteboardType(type_: NSString): Pobjc_class; message 'imageRepClassForPasteboardType:';
    class function imageRepClassForType(type_: NSString): Pobjc_class; message 'imageRepClassForType:';
    class function imageRepClassForData(data: NSData): Pobjc_class; message 'imageRepClassForData:';
    class function canInitWithData(data: NSData): Boolean; message 'canInitWithData:';
    class function imageUnfilteredFileTypes: NSArray; message 'imageUnfilteredFileTypes';
    class function imageUnfilteredPasteboardTypes: NSArray; message 'imageUnfilteredPasteboardTypes';
    class function imageFileTypes: NSArray; message 'imageFileTypes';
    class function imagePasteboardTypes: NSArray; message 'imagePasteboardTypes';
    class function imageUnfilteredTypes: NSArray; message 'imageUnfilteredTypes';
    class function imageTypes: NSArray; message 'imageTypes';
    class function canInitWithPasteboard(pasteboard: NSPasteboard): Boolean; message 'canInitWithPasteboard:';
    class function imageRepsWithContentsOfFile(filename: NSString): NSArray; message 'imageRepsWithContentsOfFile:';
    class function imageRepWithContentsOfFile(filename: NSString): id; message 'imageRepWithContentsOfFile:';
    class function imageRepsWithContentsOfURL(url: NSURL): NSArray; message 'imageRepsWithContentsOfURL:';
    class function imageRepWithContentsOfURL(url: NSURL): id; message 'imageRepWithContentsOfURL:';
    class function imageRepsWithPasteboard(pasteboard: NSPasteboard): NSArray; message 'imageRepsWithPasteboard:';
    class function imageRepWithPasteboard(pasteboard: NSPasteboard): id; message 'imageRepWithPasteboard:';
  end; external;

{$endif}
{$endif}
