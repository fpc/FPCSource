{ Parsed from AVFoundation.framework AVAssetTrack.h }


{$ifdef TYPES}
type
  AVAssetTrackPtr = ^AVAssetTrack;
{$endif}

{$ifdef CLASSES}

type
  AVAssetTrack = objcclass external (NSObject, NSCopyingProtocol, AVAsynchronousKeyValueLoadingProtocol)
  private
    _track: AVAssetTrackInternal;
  public
    function asset: AVAsset; message 'asset';
    function trackID: CMPersistentTrackID; message 'trackID';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure loadValuesAsynchronouslyForKeys_completionHandler (keys: NSArray; handler: OpaqueCBlock); message 'loadValuesAsynchronouslyForKeys:completionHandler:';
    function statusOfValueForKey_error (key: NSString; outError: NSErrorPtr): AVKeyValueStatus; message 'statusOfValueForKey:error:';
  end;


type
  AVAssetTrackBasicPropertiesAndCharacteristics = objccategory external (AVAssetTrack)
    function mediaType: NSString; message 'mediaType';
    function formatDescriptions: NSArray; message 'formatDescriptions';
    function isPlayable: ObjCBOOL; message 'isPlayable';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    function isSelfContained: ObjCBOOL; message 'isSelfContained';
    function totalSampleDataLength: clonglong; message 'totalSampleDataLength';
    function hasMediaCharacteristic (mediaCharacteristic: NSString): ObjCBOOL; message 'hasMediaCharacteristic:';
  end;

type
  AVAssetTrackTemporalProperties = objccategory external (AVAssetTrack)
    function timeRange: CMTimeRange; message 'timeRange';
    function naturalTimeScale: CMTimeScale; message 'naturalTimeScale';
    function estimatedDataRate: single; message 'estimatedDataRate';
  end;

type
  AVAssetTrackLanguageProperties = objccategory external (AVAssetTrack)
    function languageCode: NSString; message 'languageCode';
    function extendedLanguageTag: NSString; message 'extendedLanguageTag';
  end;

type
  AVAssetTrackPropertiesForVisualCharacteristic = objccategory external (AVAssetTrack)
    function naturalSize: CGSize; message 'naturalSize';
    function preferredTransform: CGAffineTransform; message 'preferredTransform';
  end;

type
  AVAssetTrackPropertiesForAudibleCharacteristic = objccategory external (AVAssetTrack)
    function preferredVolume: single; message 'preferredVolume';
  end;

type
  AVAssetTrackPropertiesForFrameBasedCharacteristic = objccategory external (AVAssetTrack)
    function nominalFrameRate: single; message 'nominalFrameRate';
    function minFrameDuration: CMTime; message 'minFrameDuration';
    function requiresFrameReordering: ObjCBOOL; message 'requiresFrameReordering';
  end;

type
  AVAssetTrackSegments = objccategory external (AVAssetTrack)
    function segments: NSArray; message 'segments';
    function segmentForTrackTime (trackTime: CMTime): AVAssetTrackSegment; message 'segmentForTrackTime:';
    function samplePresentationTimeForTrackTime (trackTime: CMTime): CMTime; message 'samplePresentationTimeForTrackTime:';
  end;

type
  AVAssetTrackMetadataReading = objccategory external (AVAssetTrack)
    function commonMetadata: NSArray; message 'commonMetadata';
    function metadata: NSArray; message 'metadata';
    function availableMetadataFormats: NSArray; message 'availableMetadataFormats';
    function metadataForFormat (format: NSString): NSArray; message 'metadataForFormat:';
  end;

type
  AVAssetTrackTrackAssociations = objccategory external (AVAssetTrack)
    function availableTrackAssociationTypes: NSArray; message 'availableTrackAssociationTypes';
    function associatedTracksOfType (trackAssociationType: NSString): NSArray; message 'associatedTracksOfType:'; { available in 10_9, 7_0 }
  end;
{$endif}

{$if  not defined(TARGET_OS_IPHONE)}
{$ifdef CLASSES}

type
  AVAssetTrackSampleCursorProvision = objccategory external (AVAssetTrack)
    function canProvideSampleCursors: ObjCBOOL; message 'canProvideSampleCursors';
    function makeSampleCursorWithPresentationTimeStamp (presentationTimeStamp: CMTime): AVSampleCursor; message 'makeSampleCursorWithPresentationTimeStamp:'; { available in 10_10 }
    function makeSampleCursorAtFirstSampleInDecodeOrder: AVSampleCursor; message 'makeSampleCursorAtFirstSampleInDecodeOrder'; { available in 10_10 }
    function makeSampleCursorAtLastSampleInDecodeOrder: AVSampleCursor; message 'makeSampleCursorAtLastSampleInDecodeOrder'; { available in 10_10 }
  end;
{$endif}

{$endif}
