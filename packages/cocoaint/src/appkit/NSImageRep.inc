{ Parsed from AppKit.framework NSImageRep.h }


{$ifdef TYPES}
type
  NSImageRepPtr = ^NSImageRep;
{$endif}

{$ifdef TYPES}
const
  NSImageRepMatchesDevice = 0;
{$endif}

{$ifdef CLASSES}

type
  NSImageRep = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __repFlags: bitpacked record
      case byte of
        0: (_anonBitField___repFlags0: cuint);
        1: (
          hasAlpha: 0..1;
          isOpaque: 0..1;
          cacheParamsComputed: cuint;
          cacheAlphaComputed: cuint;
          loadState: 0..((1 shl 2)-1);
          keepCacheWindow: cuint;
          reserved: 0..1;
          bitsPerSample: 0..((1 shl 8)-1);
          gsaved: 0..((1 shl 16)-1);
        );
      end;
    _colorSpaceName: NSString;
    _size: NSSize;
    _pixelsWide: cint;
    _pixelsHigh: cint;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function draw: ObjCBOOL; message 'draw';
    function drawAtPoint (point: NSPoint): ObjCBOOL; message 'drawAtPoint:';
    function drawInRect (rect: NSRect): ObjCBOOL; message 'drawInRect:';
    function drawInRect_fromRect_operation_fraction_respectFlipped_hints (dstSpacePortionRect: NSRect; srcSpacePortionRect: NSRect; op: NSCompositingOperation; requestedAlpha: CGFloat; respectContextIsFlipped: ObjCBOOL; hints: NSDictionary): ObjCBOOL; message 'drawInRect:fromRect:operation:fraction:respectFlipped:hints:'; { available in 10_6 }
    procedure setSize(newValue: NSSize); message 'setSize:';
    function size: NSSize; message 'size';
    procedure setAlpha(newValue: ObjCBOOL); message 'setAlpha:';
    function hasAlpha: ObjCBOOL; message 'hasAlpha';
    procedure setOpaque(newValue: ObjCBOOL); message 'setOpaque:';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure setColorSpaceName(newValue: NSString); message 'setColorSpaceName:';
    function colorSpaceName: NSString; message 'colorSpaceName';
    procedure setBitsPerSample(newValue: NSInteger); message 'setBitsPerSample:';
    function bitsPerSample: NSInteger; message 'bitsPerSample';
    procedure setPixelsWide(newValue: NSInteger); message 'setPixelsWide:';
    function pixelsWide: NSInteger; message 'pixelsWide';
    procedure setPixelsHigh(newValue: NSInteger); message 'setPixelsHigh:';
    function pixelsHigh: NSInteger; message 'pixelsHigh';
    class procedure registerImageRepClass (imageRepClass: pobjc_class); message 'registerImageRepClass:';
    class procedure unregisterImageRepClass (imageRepClass: pobjc_class); message 'unregisterImageRepClass:';
    class function registeredImageRepClasses: NSArray; message 'registeredImageRepClasses';
    class function imageRepClassForFileType (type_: NSString): pobjc_class; message 'imageRepClassForFileType:'; deprecated 'in 10_0, 10_10, "Use +imageRepClassForType: instead"';
    class function imageRepClassForPasteboardType (type_: NSString): pobjc_class; message 'imageRepClassForPasteboardType:'; deprecated 'in 10_0, 10_10, "Use +imageRepClassForType: instead"';
    class function imageRepClassForType (type_: NSString): pobjc_class; message 'imageRepClassForType:'; { available in 10_5 }
    class function imageRepClassForData (data: NSData): pobjc_class; message 'imageRepClassForData:';
    class function canInitWithData (data: NSData): ObjCBOOL; message 'canInitWithData:';
    class function imageUnfilteredFileTypes: NSArray; message 'imageUnfilteredFileTypes'; deprecated 'in 10_0, 10_10, "Use +imageUnfilteredTypes instead"';
    class function imageUnfilteredPasteboardTypes: NSArray; message 'imageUnfilteredPasteboardTypes'; deprecated 'in 10_0, 10_10, "Use +imageUnfilteredTypes instead"';
    class function imageFileTypes: NSArray; message 'imageFileTypes'; deprecated 'in 10_0, 10_10, "Use +imageTypes instead"';
    class function imagePasteboardTypes: NSArray; message 'imagePasteboardTypes'; deprecated 'in 10_0, 10_10, "Use +imageTypes instead"';
    class function imageUnfilteredTypes: NSArray; message 'imageUnfilteredTypes'; { available in 10_5 }
    class function imageTypes: NSArray; message 'imageTypes'; { available in 10_5 }
    class function canInitWithPasteboard (pasteboard: NSPasteboard): ObjCBOOL; message 'canInitWithPasteboard:';
    class function imageRepsWithContentsOfFile (filename: NSString): NSArray; message 'imageRepsWithContentsOfFile:';
    class function imageRepWithContentsOfFile (filename: NSString): id; message 'imageRepWithContentsOfFile:';
    class function imageRepsWithContentsOfURL (url: NSURL): NSArray; message 'imageRepsWithContentsOfURL:';
    class function imageRepWithContentsOfURL (url: NSURL): id; message 'imageRepWithContentsOfURL:';
    class function imageRepsWithPasteboard (pasteboard: NSPasteboard): NSArray; message 'imageRepsWithPasteboard:';
    class function imageRepWithPasteboard (pasteboard: NSPasteboard): id; message 'imageRepWithPasteboard:';
    function CGImageForProposedRect_context_hints (proposedDestRect: NSRectPtr; context: NSGraphicsContext; hints: NSDictionary): CGImageRef; message 'CGImageForProposedRect:context:hints:'; { available in 10_6 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSImageRepRegistryDidChangeNotification: NSString; cvar; external;
{$endif}

