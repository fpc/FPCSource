{ Parsed from AVFoundation.framework AVComposition.h }


{$ifdef TYPES}
type
  AVCompositionPtr = ^AVComposition;
  AVMutableCompositionPtr = ^AVMutableComposition;
{$endif}

{$ifdef CLASSES}

type
  AVComposition = objcclass external (AVAsset, NSMutableCopyingProtocol)
  private
    _priv: AVCompositionInternal;
  public
    function tracks: NSArray; message 'tracks';
    function naturalSize: CGSize; message 'naturalSize';

    { Adopted protocols }
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  AVMutableComposition = objcclass external (AVComposition)
  private
    _mutablePriv: AVMutableCompositionInternal;
  public
    function tracks: NSArray; message 'tracks';
    procedure setNaturalSize(newValue: CGSize); message 'setNaturalSize:';
    function naturalSize: CGSize; message 'naturalSize';
    class function composition: AVMutableComposition; message 'composition';
  end;


type
  AVMutableCompositionCompositionLevelEditing = objccategory external (AVMutableComposition)
    function insertTimeRange_ofAsset_atTime_error (timeRange: CMTimeRange; asset: AVAsset; startTime: CMTime; outError: NSErrorPtr): ObjCBOOL; message 'insertTimeRange:ofAsset:atTime:error:';
    procedure insertEmptyTimeRange (timeRange: CMTimeRange); message 'insertEmptyTimeRange:';
    procedure removeTimeRange (timeRange: CMTimeRange); message 'removeTimeRange:';
    procedure scaleTimeRange_toDuration (timeRange: CMTimeRange; duration_: CMTime); message 'scaleTimeRange:toDuration:';
  end;

type
  AVMutableCompositionTrackLevelEditing = objccategory external (AVMutableComposition)
    function addMutableTrackWithMediaType_preferredTrackID (mediaType: NSString; preferredTrackID: CMPersistentTrackID): AVMutableCompositionTrack; message 'addMutableTrackWithMediaType:preferredTrackID:';
    procedure removeTrack (track: AVCompositionTrack); message 'removeTrack:';
    function mutableTrackCompatibleWithTrack (track: AVAssetTrack): AVMutableCompositionTrack; message 'mutableTrackCompatibleWithTrack:';
  end;
{$endif}

