{ Parsed from Appkit.framework NSBitmapImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

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
  NSAlphaFirstBitmapFormat = 1  shl  0;
  NSAlphaNonpremultipliedBitmapFormat = 1  shl  1;
  NSFloatingPointSamplesBitmapFormat = 1  shl  2;

{ Types }
type
  NSTIFFCompression = culong;
  NSBitmapImageFileType = culong;
  NSImageRepLoadStatus = clong;
  NSBitmapFormat = culong;

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
{$ifndef NSBITMAPIMAGEREP_PAS_T}
{$define NSBITMAPIMAGEREP_PAS_T}

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
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(var planes: char; width: clong; height: clong; bps: clong; spp: clong; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; rBytes: clong; pBits: clong): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(var planes: char; width: clong; height: clong; bps: clong; spp: clong; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; bitmapFormat_: NSBitmapFormat; rBytes: clong; pBits: clong): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:';
    function initWithCGImage(CGImage_: CGImageRef): id; message 'initWithCGImage:';
    function initWithCIImage(var ciImage: CIImage): id; message 'initWithCIImage:';
    class function imageRepsWithData(data: NSData): NSArray; message 'imageRepsWithData:';
    class function imageRepWithData_initWithData(data: NSData): id; message 'imageRepWithData:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function bitmapData: Pointer; message 'bitmapData';
    procedure getBitmapDataPlanes(var data: char); message 'getBitmapDataPlanes:';
    function isPlanar: Boolean; message 'isPlanar';
    function samplesPerPixel: clong; message 'samplesPerPixel';
    function bitsPerPixel: clong; message 'bitsPerPixel';
    function bytesPerRow: clong; message 'bytesPerRow';
    function bytesPerPlane: clong; message 'bytesPerPlane';
    function numberOfPlanes: clong; message 'numberOfPlanes';
    function bitmapFormat: NSBitmapFormat; message 'bitmapFormat';
    procedure getCompression_factor(var compression: NSTIFFCompression; var factor: single); message 'getCompression:factor:';
    procedure setCompression_factor(compression: NSTIFFCompression; factor: single); message 'setCompression:factor:';
    function TIFFRepresentation: NSData; message 'TIFFRepresentation';
    function TIFFRepresentationUsingCompression_factor(comp: NSTIFFCompression; factor: single): NSData; message 'TIFFRepresentationUsingCompression:factor:';
    class function TIFFRepresentationOfImageRepsInArray(array_: NSArray): NSData; message 'TIFFRepresentationOfImageRepsInArray:';
    class function TIFFRepresentationOfImageRepsInArray_usingCompression_factor(array_: NSArray; comp: NSTIFFCompression; factor: single): NSData; message 'TIFFRepresentationOfImageRepsInArray:usingCompression:factor:';
    class procedure getTIFFCompressionTypes_count(var list: NSTIFFCompression; var numTypes: clong); message 'getTIFFCompressionTypes:count:';
    class function localizedNameForTIFFCompressionType(compression: NSTIFFCompression): NSString; message 'localizedNameForTIFFCompressionType:';
    function canBeCompressedUsing(compression: NSTIFFCompression): Boolean; message 'canBeCompressedUsing:';
    procedure colorizeByMappingGray_toColor_blackMapping_whiteMapping(midPoint: CGFloat; midPointColor: NSColor; shadowColor: NSColor; lightColor: NSColor); message 'colorizeByMappingGray:toColor:blackMapping:whiteMapping:';
    function initForIncrementalLoad: id; message 'initForIncrementalLoad';
    function incrementalLoadFromData_complete(data: NSData; complete: Boolean): clong; message 'incrementalLoadFromData:complete:';
    procedure setColor_atX_y(color: NSColor; x: clong; y: clong); message 'setColor:atX:y:';
    function colorAtX_y(x: clong; y: clong): NSColor; message 'colorAtX:y:';
    procedure getPixel_atX_y(p: culong; x: clong; y: clong); message 'getPixel:atX:y:';
    procedure setPixel_atX_y(p: culong; x: clong; y: clong); message 'setPixel:atX:y:';
    function CGImage: CGImageRef; message 'CGImage';

    { Category: NSBitmapImageFileTypeExtensions }
    class function representationOfImageRepsInArray_usingType_properties(imageReps: NSArray; storageType: NSBitmapImageFileType; properties: NSDictionary): NSData; message 'representationOfImageRepsInArray:usingType:properties:';
    function representationUsingType_properties(storageType: NSBitmapImageFileType; properties: NSDictionary): NSData; message 'representationUsingType:properties:';
    procedure setProperty_withValue(property_: NSString; value: id); message 'setProperty:withValue:';
    function valueForProperty(property_: NSString): id; message 'valueForProperty:';
  end; external;

{$endif}
{$endif}
