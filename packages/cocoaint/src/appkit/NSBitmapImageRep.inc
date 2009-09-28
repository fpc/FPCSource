{ Parsed from Appkit.framework NSBitmapImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSBITMAPIMAGEREP_PAS_H}
{$define NSBITMAPIMAGEREP_PAS_H}
type
  NSBitmapImageRepPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSBITMAPIMAGEREP_PAS_T}
{$define NSBITMAPIMAGEREP_PAS_T}

{ Constants }

const
  NSTIFFCompressionNone = 1;
  NSTIFFCompressionCCITTFAX3 = 3;
  NSTIFFCompressionCCITTFAX4 = 4;
  NSTIFFCompressionLZW = 5;
  NSTIFFCompressionJPEG = 6;
  NSTIFFCompressionNEXT = 32766;
  NSTIFFCompressionPackBits = 32773;
  NSTIFFCompressionOldJPEG = 32865;

const
  NSTIFFFileType = 0;
  NSBMPFileType = 1;
  NSGIFFileType = 2;
  NSJPEGFileType = 3;
  NSPNGFileType = 4;
  NSJPEG2000FileType = 5;

const
  NSImageRepLoadStatusUnknownType = -1;
  NSImageRepLoadStatusReadingHeader = -2;
  NSImageRepLoadStatusWillNeedAllData = -3;
  NSImageRepLoadStatusInvalidData = -4;
  NSImageRepLoadStatusUnexpectedEOF = -5;
  NSImageRepLoadStatusCompleted = -6;

const
  NSAlphaFirstBitmapFormat = 1  shl  0;
  NSAlphaNonpremultipliedBitmapFormat = 1  shl  1;
  NSFloatingPointSamplesBitmapFormat = 1  shl  2;

{ Types }
type
  NSTIFFCompression = NSUInteger;
  NSBitmapImageFileType = NSUInteger;
  NSImageRepLoadStatus = NSInteger;
  NSBitmapFormat = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBITMAPIMAGEREP_PAS_R}
{$define NSBITMAPIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBITMAPIMAGEREP_PAS_F}
{$define NSBITMAPIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBITMAPIMAGEREP_PAS_S}
{$define NSBITMAPIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBitmapImageRep = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBITMAPIMAGEREP_PAS_C}
{$define NSBITMAPIMAGEREP_PAS_C}

{ NSBitmapImageRep }
  NSBitmapImageRep = objcclass(NSImageRep)
  private
    __moreRepFlags: bitpacked record
        bitsPerPixel: cuint;
        isPlanar: 0..1;
        explicitPlanes: 0..1;
        isUnpacked: 0..1;
        dataLoaded: 0..1;
        numColors: cuint;
        memory: 0..((1 shl 2)-1);
        compressionFactor: 0..((1 shl 14)-1);
        imageNumber: 0..((1 shl 8)-1);
        bitmapFormat: 0..((1 shl 3)-1);
        cgImageIsPrimary: 0..1;
        compression: 0..((1 shl 20)-1);
      end;
    __bytesPerRow: cuint;
    __data: char;
    __tiffData: NSData;
    __properties: id;
    
  public
    class function alloc: NSBitmapImageRep; message 'alloc';

    function initWithFocusedViewRect(rect: NSRect): id; message 'initWithFocusedViewRect:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(planes: pchar; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; rBytes: NSInteger; pBits: NSInteger): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(planes: pchar; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; bitmapFormat_: NSBitmapFormat; rBytes: NSInteger; pBits: NSInteger): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:';
    function initWithCGImage(CGImage_: CGImageRef): id; message 'initWithCGImage:';
    function initWithCIImage(ciImage: CIImagePointer): id; message 'initWithCIImage:';
    class function imageRepsWithData(data: NSData): NSArray; message 'imageRepsWithData:';
    class function imageRepWithData(data: NSData): id; message 'imageRepWithData:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function bitmapData: Pointer; message 'bitmapData';
    procedure getBitmapDataPlanes(data: pchar); message 'getBitmapDataPlanes:';
    function isPlanar: Boolean; message 'isPlanar';
    function samplesPerPixel: NSInteger; message 'samplesPerPixel';
    function bitsPerPixel: NSInteger; message 'bitsPerPixel';
    function bytesPerRow: NSInteger; message 'bytesPerRow';
    function bytesPerPlane: NSInteger; message 'bytesPerPlane';
    function numberOfPlanes: NSInteger; message 'numberOfPlanes';
    function bitmapFormat: NSBitmapFormat; message 'bitmapFormat';
    procedure getCompression_factor(compression: NSTIFFCompressionPointer; factor: psingle); message 'getCompression:factor:';
    procedure setCompression_factor(compression: NSTIFFCompression; factor: single); message 'setCompression:factor:';
    function TIFFRepresentation: NSData; message 'TIFFRepresentation';
    function TIFFRepresentationUsingCompression_factor(comp: NSTIFFCompression; factor: single): NSData; message 'TIFFRepresentationUsingCompression:factor:';
    class function TIFFRepresentationOfImageRepsInArray(array_: NSArray): NSData; message 'TIFFRepresentationOfImageRepsInArray:';
    class function TIFFRepresentationOfImageRepsInArray_usingCompression_factor(array_: NSArray; comp: NSTIFFCompression; factor: single): NSData; message 'TIFFRepresentationOfImageRepsInArray:usingCompression:factor:';
    class procedure getTIFFCompressionTypes_count(list: NSTIFFCompressionPointer; numTypes: NSIntegerPointer); message 'getTIFFCompressionTypes:count:';
    class function localizedNameForTIFFCompressionType(compression: NSTIFFCompression): NSString; message 'localizedNameForTIFFCompressionType:';
    function canBeCompressedUsing(compression: NSTIFFCompression): Boolean; message 'canBeCompressedUsing:';
    procedure colorizeByMappingGray_toColor_blackMapping_whiteMapping(midPoint: CGFloat; midPointColor: NSColor; shadowColor: NSColor; lightColor: NSColor); message 'colorizeByMappingGray:toColor:blackMapping:whiteMapping:';
    function initForIncrementalLoad: id; message 'initForIncrementalLoad';
    function incrementalLoadFromData_complete(data: NSData; complete: Boolean): NSInteger; message 'incrementalLoadFromData:complete:';
    procedure setColor_atX_y(color: NSColor; x: NSInteger; y: NSInteger); message 'setColor:atX:y:';
    function colorAtX_y(x: NSInteger; y: NSInteger): NSColor; message 'colorAtX:y:';
    procedure getPixel_atX_y(p: NSUInteger; x: NSInteger; y: NSInteger); message 'getPixel:atX:y:';
    procedure setPixel_atX_y(p: NSUInteger; x: NSInteger; y: NSInteger); message 'setPixel:atX:y:';
    function CGImage: CGImageRef; message 'CGImage';

    { Category: NSBitmapImageFileTypeExtensions }
    class function representationOfImageRepsInArray_usingType_properties(imageReps: NSArray; storageType: NSBitmapImageFileType; properties: NSDictionary): NSData; message 'representationOfImageRepsInArray:usingType:properties:';
    function representationUsingType_properties(storageType: NSBitmapImageFileType; properties: NSDictionary): NSData; message 'representationUsingType:properties:';
    procedure setProperty_withValue(property_: NSString; value: id); message 'setProperty:withValue:';
    function valueForProperty(property_: NSString): id; message 'valueForProperty:';
  end; external;

{$endif}
{$endif}
