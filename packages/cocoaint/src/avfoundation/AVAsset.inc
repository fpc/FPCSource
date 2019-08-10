{ Parsed from AVFoundation.framework AVAsset.h }


{ Types from AVAsset_AVAssetReferenceRestrictions }
{$ifdef TYPES}

type
  AVAssetReferenceRestrictions = NSUInteger;
type
  AVAssetReferenceRestrictionsPtr = ^AVAssetReferenceRestrictions;

const
  AVAssetReferenceRestrictionForbidNone = 0;
  AVAssetReferenceRestrictionForbidRemoteReferenceToLocal = 1 shl 0;
  AVAssetReferenceRestrictionForbidLocalReferenceToRemote = 1 shl 1;
  AVAssetReferenceRestrictionForbidCrossSiteReference = 1 shl 2;
  AVAssetReferenceRestrictionForbidLocalReferenceToLocal = 1 shl 3;
  AVAssetReferenceRestrictionForbidAll = $FFFF;
{$endif}


{ Types from AVAssetUsability }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  AVAssetPtr = ^AVAsset;
  AVURLAssetPtr = ^AVURLAsset;
{$endif}

{$ifdef CLASSES}

type
  AVAsset = objcclass external (NSObject, NSCopyingProtocol, AVAsynchronousKeyValueLoadingProtocol)
  private
    _asset: AVAssetInternal;
  public
    class function assetWithURL (URL: NSURL): id; message 'assetWithURL:';
    function duration: CMTime; message 'duration';
    function preferredRate: single; message 'preferredRate';
    function preferredVolume: single; message 'preferredVolume';
    function preferredTransform: CGAffineTransform; message 'preferredTransform';
    function naturalSize: CGSize; message 'naturalSize';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure loadValuesAsynchronouslyForKeys_completionHandler (keys: NSArray; handler: OpaqueCBlock); message 'loadValuesAsynchronouslyForKeys:completionHandler:';
    function statusOfValueForKey_error (key: NSString; outError: NSErrorPtr): AVKeyValueStatus; message 'statusOfValueForKey:error:';
  end;


type
  AVAssetAsynchronousLoading = objccategory external (AVAsset)
    function providesPreciseDurationAndTiming: ObjCBOOL; message 'providesPreciseDurationAndTiming';
    procedure cancelLoading; message 'cancelLoading';
  end;

type
  AVAsset_AVAssetReferenceRestrictions = objccategory external name 'AVAssetReferenceRestrictions' (AVAsset)
    function referenceRestrictions: AVAssetReferenceRestrictions; message 'referenceRestrictions';
  end;

type
  AVAssetTrackInspection = objccategory external (AVAsset)
    function tracks: NSArray; message 'tracks';
    function trackWithTrackID (trackID: CMPersistentTrackID): AVAssetTrack; message 'trackWithTrackID:';
    function tracksWithMediaType (mediaType: NSString): NSArray; message 'tracksWithMediaType:';
    function tracksWithMediaCharacteristic (mediaCharacteristic: NSString): NSArray; message 'tracksWithMediaCharacteristic:';
    function trackGroups: NSArray; message 'trackGroups';
  end;

type
  AVAssetMetadataReading = objccategory external (AVAsset)
    function creationDate: AVMetadataItem; message 'creationDate';
    function lyrics: NSString; message 'lyrics';
    function commonMetadata: NSArray; message 'commonMetadata';
    function metadata: NSArray; message 'metadata';
    function availableMetadataFormats: NSArray; message 'availableMetadataFormats';
    function metadataForFormat (format: NSString): NSArray; message 'metadataForFormat:';
  end;

type
  AVAssetChapterInspection = objccategory external (AVAsset)
    function availableChapterLocales: NSArray; message 'availableChapterLocales';
    function chapterMetadataGroupsWithTitleLocale_containingItemsWithCommonKeys (locale: NSLocale; commonKeys: NSArray): NSArray; message 'chapterMetadataGroupsWithTitleLocale:containingItemsWithCommonKeys:'; { available in 10_7, 4_3 }
    function chapterMetadataGroupsBestMatchingPreferredLanguages (preferredLanguages: NSArray): NSArray; message 'chapterMetadataGroupsBestMatchingPreferredLanguages:'; { available in 10_8, 6_0 }
  end;

type
  AVAssetMediaSelection = objccategory external (AVAsset)
    function availableMediaCharacteristicsWithMediaSelectionOptions: NSArray; message 'availableMediaCharacteristicsWithMediaSelectionOptions';
    function mediaSelectionGroupForMediaCharacteristic (mediaCharacteristic: NSString): AVMediaSelectionGroup; message 'mediaSelectionGroupForMediaCharacteristic:'; { available in 10_8, 5_0 }
  end;

type
  AVAssetProtectedContent = objccategory external (AVAsset)
    function hasProtectedContent: ObjCBOOL; message 'hasProtectedContent';
  end;

type
  AVAssetUsability = objccategory external (AVAsset)
    function isPlayable: ObjCBOOL; message 'isPlayable';
    function isExportable: ObjCBOOL; message 'isExportable';
    function isReadable: ObjCBOOL; message 'isReadable';
    function isComposable: ObjCBOOL; message 'isComposable';
    {$if defined(TARGET_OS_IPHONE)}
    function isCompatibleWithSavedPhotosAlbum: ObjCBOOL; message 'isCompatibleWithSavedPhotosAlbum';
    {$endif}
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVURLAssetPreferPreciseDurationAndTimingKey: NSString { available in 10_7, 4_0 }; cvar; external;
  AVURLAssetReferenceRestrictionsKey: NSString { available in 10_7, 5_0 }; cvar; external;
  AVURLAssetHTTPCookiesKey: NSString { available in 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVURLAsset = objcclass external (AVAsset)
  private
    _URLAsset: AVURLAssetInternal;
  public
    class function audiovisualTypes: NSArray; message 'audiovisualTypes'; { available in 10_7, 5_0 }
    class function audiovisualMIMETypes: NSArray; message 'audiovisualMIMETypes'; { available in 10_7, 5_0 }
    class function isPlayableExtendedMIMEType (extendedMIMEType: NSString): ObjCBOOL; message 'isPlayableExtendedMIMEType:'; { available in 10_7, 5_0 }
    class function URLAssetWithURL_options (URL: NSURL; options: NSDictionary): AVURLAsset; message 'URLAssetWithURL:options:';
    function initWithURL_options (URL: NSURL; options: NSDictionary): instancetype; message 'initWithURL:options:';
    function URL: NSURL; message 'URL';
  end;


type
  AVURLAssetURLHandling = objccategory external (AVURLAsset)
    function resourceLoader: AVAssetResourceLoader; message 'resourceLoader';
  end;

type
  AVAssetCompositionUtility = objccategory external (AVURLAsset)
    function compatibleTrackForCompositionTrack (compositionTrack: AVCompositionTrack): AVAssetTrack; message 'compatibleTrackForCompositionTrack:';
  end;
{$endif}

