{ Parsed from AppKit.framework NSBitmapImageRep.h }


{$ifdef TYPES}
type
  NSBitmapImageRepPtr = ^NSBitmapImageRep;
{$endif}

{$ifdef TYPES}
type
  NSTIFFCompression = NSUInteger;
  NSTIFFCompressionPtr = ^NSTIFFCompression;

const
  NSTIFFCompressionNone = 1;
  NSTIFFCompressionCCITTFAX3 = 3;
  NSTIFFCompressionCCITTFAX4 = 4;
  NSTIFFCompressionLZW = 5;
  NSTIFFCompressionJPEG = 6;
  NSTIFFCompressionNEXT = 32766;
  NSTIFFCompressionPackBits = 32773;
  NSTIFFCompressionOldJPEG = 32865;

type
  NSBitmapImageFileType = NSUInteger;
  NSBitmapImageFileTypePtr = ^NSBitmapImageFileType;

const
  NSTIFFFileType = 0;
  NSBMPFileType = 1;
  NSGIFFileType = 2;
  NSJPEGFileType = 3;
  NSPNGFileType = 4;
  NSJPEG2000FileType = 5;

type
  NSImageRepLoadStatus = NSInteger;
  NSImageRepLoadStatusPtr = ^NSImageRepLoadStatus;

const
  NSImageRepLoadStatusUnknownType = -1;
  NSImageRepLoadStatusReadingHeader = -2;
  NSImageRepLoadStatusWillNeedAllData = -3;
  NSImageRepLoadStatusInvalidData = -4;
  NSImageRepLoadStatusUnexpectedEOF = -5;
  NSImageRepLoadStatusCompleted = -6;

type
  NSBitmapFormat = NSUInteger;
  NSBitmapFormatPtr = ^NSBitmapFormat;

const
  NSAlphaFirstBitmapFormat = 1 shl 0;
  NSAlphaNonpremultipliedBitmapFormat = 1 shl 1;
  NSFloatingPointSamplesBitmapFormat = 1 shl 2;
  NS16BitLittleEndianBitmapFormat = 1 shl 8 { available in 10_10 };
  NS32BitLittleEndianBitmapFormat = 1 shl 9 { available in 10_10 };
  NS16BitBigEndianBitmapFormat = 1 shl 10 { available in 10_10 };
  NS32BitBigEndianBitmapFormat = 1 shl 11 { available in 10_10 };
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSImageCompressionMethod: NSString; cvar; external;
  NSImageCompressionFactor: NSString; cvar; external;
  NSImageDitherTransparency: NSString; cvar; external;
  NSImageRGBColorTable: NSString; cvar; external;
  NSImageInterlaced: NSString; cvar; external;
  NSImageColorSyncProfileData: NSString; cvar; external;
  NSImageFrameCount: NSString; cvar; external;
  NSImageCurrentFrame: NSString; cvar; external;
  NSImageCurrentFrameDuration: NSString; cvar; external;
  NSImageLoopCount: NSString; cvar; external;
  NSImageGamma: NSString; cvar; external;
  NSImageProgressive: NSString; cvar; external;
  NSImageEXIFData: NSString; cvar; external;
  NSImageFallbackBackgroundColor: NSString { available in 10_5 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSBitmapImageRep = objcclass external (NSImageRep, NSSecureCodingProtocol)
  private
    _moreRepFlags: bitpacked record
      case byte of
        0: (_anonBitField__moreRepFlags0: cuint);
        1: (
          bitsPerPixel: 0..((1 shl 8)-1);
          isPlanar: 0..1;
          explicitPlanes: 0..1;
          imageSourceIsIndexed: 0..1;
          dataLoaded: 0..1;
          colorModel: 0..((1 shl 4)-1);
          tierTwoInfoIsLoaded: 0..1;
          respectO: 0..1;
          compressionFactor: 0..((1 shl 14)-1);
          imageNumber: 0..((1 shl 8)-1);
          bitmapFormat: 0..((1 shl 3)-1);
          cgImageIsPrimary: 0..1;
          compression: 0..((1 shl 20)-1);
        );
      end;
    _bytesPerRow: cuint;
    _dataObj: id;
    _tiffData: NSData;
    _properties: id;
  public
    function initWithFocusedViewRect (rect: NSRect): instancetype; message 'initWithFocusedViewRect:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel (planes: PChar; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: ObjCBOOL; isPlanar: ObjCBOOL; colorSpaceName_: NSString; rBytes: NSInteger; pBits: NSInteger): instancetype; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel (planes: PChar; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: ObjCBOOL; isPlanar: ObjCBOOL; colorSpaceName_: NSString; bitmapFormat: NSBitmapFormat; rBytes: NSInteger; pBits: NSInteger): instancetype; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:';
    function initWithCGImage (cgImage: CGImageRef): instancetype; message 'initWithCGImage:'; { available in 10_5 }
    function initWithCIImage (ciImage_: CIImage): instancetype; message 'initWithCIImage:'; { available in 10_5 }
    class function imageRepsWithData (data: NSData): NSArray; message 'imageRepsWithData:';
    class function imageRepWithData (data: NSData): instancetype; message 'imageRepWithData:';
    function initWithData (data: NSData): instancetype; message 'initWithData:';
    function bitmapData: PChar; message 'bitmapData';
    procedure getBitmapDataPlanes (data: PChar); message 'getBitmapDataPlanes:';
    function isPlanar: ObjCBOOL; message 'isPlanar';
    function samplesPerPixel: NSInteger; message 'samplesPerPixel';
    function bitsPerPixel: NSInteger; message 'bitsPerPixel';
    function bytesPerRow: NSInteger; message 'bytesPerRow';
    function bytesPerPlane: NSInteger; message 'bytesPerPlane';
    function numberOfPlanes: NSInteger; message 'numberOfPlanes';
    function bitmapFormat: NSBitmapFormat; message 'bitmapFormat';
    procedure getCompression_factor (compression: NSTIFFCompressionPtr; factor: psingle); message 'getCompression:factor:';
    procedure setCompression_factor (compression: NSTIFFCompression; factor: single); message 'setCompression:factor:';
    function TIFFRepresentation: NSData; message 'TIFFRepresentation';
    function TIFFRepresentationUsingCompression_factor (comp: NSTIFFCompression; factor: single): NSData; message 'TIFFRepresentationUsingCompression:factor:';
    class function TIFFRepresentationOfImageRepsInArray (array_: NSArray): NSData; message 'TIFFRepresentationOfImageRepsInArray:';
    class function TIFFRepresentationOfImageRepsInArray_usingCompression_factor (array_: NSArray; comp: NSTIFFCompression; factor: single): NSData; message 'TIFFRepresentationOfImageRepsInArray:usingCompression:factor:';
    class procedure getTIFFCompressionTypes_count (list: NSTIFFCompressionPtr; numTypes: NSIntegerPtr); message 'getTIFFCompressionTypes:count:';
    class function localizedNameForTIFFCompressionType (compression: NSTIFFCompression): NSString; message 'localizedNameForTIFFCompressionType:';
    function canBeCompressedUsing (compression: NSTIFFCompression): ObjCBOOL; message 'canBeCompressedUsing:';
    procedure colorizeByMappingGray_toColor_blackMapping_whiteMapping (midPoint: CGFloat; midPointColor: NSColor; shadowColor: NSColor; lightColor: NSColor); message 'colorizeByMappingGray:toColor:blackMapping:whiteMapping:';
    function initForIncrementalLoad: instancetype; message 'initForIncrementalLoad';
    function incrementalLoadFromData_complete (data: NSData; complete: ObjCBOOL): NSInteger; message 'incrementalLoadFromData:complete:';
    procedure setColor_atX_y (color: NSColor; x: NSInteger; y: NSInteger); message 'setColor:atX:y:';
    function colorAtX_y (x: NSInteger; y: NSInteger): NSColor; message 'colorAtX:y:';
    procedure getPixel_atX_y (p: NSUIntegerPtr { variable size array of NSUInteger }; x: NSInteger; y: NSInteger); message 'getPixel:atX:y:';
    procedure setPixel_atX_y (p: NSUIntegerPtr { variable size array of NSUInteger }; x: NSInteger; y: NSInteger); message 'setPixel:atX:y:';
    function CGImage: CGImageRef; message 'CGImage';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function bitmapImageRepByConvertingToColorSpace_renderingIntent (targetSpace: NSColorSpace; renderingIntent: NSColorRenderingIntent): NSBitmapImageRep; message 'bitmapImageRepByConvertingToColorSpace:renderingIntent:'; { available in 10_6 }
    function bitmapImageRepByRetaggingWithColorSpace (newSpace: NSColorSpace): NSBitmapImageRep; message 'bitmapImageRepByRetaggingWithColorSpace:'; { available in 10_6 }

    { Adopted protocols }
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSBitmapImageFileTypeExtensions = objccategory external (NSBitmapImageRep)
    class function representationOfImageRepsInArray_usingType_properties (imageReps: NSArray; storageType: NSBitmapImageFileType; properties: NSDictionary): NSData; message 'representationOfImageRepsInArray:usingType:properties:';
    function representationUsingType_properties (storageType: NSBitmapImageFileType; properties: NSDictionary): NSData; message 'representationUsingType:properties:';
    procedure setProperty_withValue (property_: NSString; value: id); message 'setProperty:withValue:';
    function valueForProperty (property_: NSString): id; message 'valueForProperty:';
  end;
{$endif}

