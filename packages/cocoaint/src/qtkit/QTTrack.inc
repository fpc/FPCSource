{ Parsed from QTKit.framework QTTrack.h }


{ Types from QTTrack }
{$ifdef TYPES}


{$endif}


{ Types from QTTrack_VisualSupport }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  QTTrackPtr = ^QTTrack;
{$endif}

{$ifdef CLASSES}

type
  QTTrack = objcclass external (NSObject)
  private
    _flags: clong;
    _trackHelper: QTTrackHelper;
    _movie: QTMovie;
    _children: QTInvalidationSet;
    _cachedTrackID: cint32;
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
    _reserved4: clong;
  public
    {$if  not defined(__LP64__)}
    class function trackWithQuickTimeTrack_error (track_: Track; errorPtr: NSErrorPtr): id; message 'trackWithQuickTimeTrack:error:';
    function initWithQuickTimeTrack_error (track_: Track; errorPtr: NSErrorPtr): id; message 'initWithQuickTimeTrack:error:';
    {$endif}
    function movie: QTMovie; message 'movie';
    function media: QTMedia; message 'media';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setEnabled (enabled: ObjCBOOL); message 'setEnabled:';
    function volume: single; message 'volume';
    procedure setVolume (volume: single); message 'setVolume:';
    function trackAttributes: NSDictionary; message 'trackAttributes';
    procedure setTrackAttributes (attributes: NSDictionary); message 'setTrackAttributes:';
    function attributeForKey (attributeKey: NSString): id; message 'attributeForKey:';
    procedure setAttribute_forKey (value: id; attributeKey: NSString); message 'setAttribute:forKey:';
    {$if  not defined(__LP64__)}
    function quickTimeTrack: Track; message 'quickTimeTrack';
    {$endif}
    procedure insertSegmentOfTrack_timeRange_atTime (track: QTTrack; range: QTTimeRange; time: QTTime); message 'insertSegmentOfTrack:timeRange:atTime:';
    procedure insertSegmentOfTrack_fromRange_scaledToRange (track: QTTrack; srcRange: QTTimeRange; dstRange: QTTimeRange); message 'insertSegmentOfTrack:fromRange:scaledToRange:';
    procedure insertEmptySegmentAt (range: QTTimeRange); message 'insertEmptySegmentAt:';
    procedure deleteSegment (segment: QTTimeRange); message 'deleteSegment:';
    procedure scaleSegment_newDuration (segment: QTTimeRange; newDuration: QTTime); message 'scaleSegment:newDuration:';
    procedure addImage_forDuration_withAttributes (image: NSImage; duration: QTTime; attributes: NSDictionary); message 'addImage:forDuration:withAttributes:';
  end;


type
  QTTrack_VisualSupport = objccategory external (QTTrack)
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    function apertureModeDimensionsForMode (mode: NSString): NSSize; message 'apertureModeDimensionsForMode:';
    procedure setApertureModeDimensions_forMode (dimensions: NSSize; mode: NSString); message 'setApertureModeDimensions:forMode:';
    procedure generateApertureModeDimensions; message 'generateApertureModeDimensions';
    procedure removeApertureModeDimensions; message 'removeApertureModeDimensions';
    {$endif}
    {$if ((defined(MAC_OS_X_VERSION_10_7)) and ((MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_7)))}
    function commonMetadata: NSArray; message 'commonMetadata'; deprecated 'available in 10.7 and later but deprecated in 10.9';
    function availableMetadataFormats: NSArray; message 'availableMetadataFormats'; deprecated 'available in 10.7 and later but deprecated in 10.9';
    function metadataForFormat (format: NSString): NSArray; message 'metadataForFormat:'; deprecated 'available in 10.7 and later but deprecated in 10.9';
    {$endif}
  end;
{$endif}

