{ Parsed from AVFoundation.framework AVCompositionTrack.h }


{$ifdef TYPES}
type
  AVCompositionTrackPtr = ^AVCompositionTrack;
  AVMutableCompositionTrackPtr = ^AVMutableCompositionTrack;
{$endif}

{$ifdef CLASSES}

type
  AVCompositionTrack = objcclass external (AVAssetTrack)
  private
    _priv: AVCompositionTrackInternal;
  public
    function segments: NSArray; message 'segments';
  end;

type
  AVMutableCompositionTrack = objcclass external (AVCompositionTrack)
  private
    _mutablePriv: AVMutableCompositionTrackInternal;
  public
    procedure setNaturalTimeScale(newValue: CMTimeScale); message 'setNaturalTimeScale:';
    function naturalTimeScale: CMTimeScale; message 'naturalTimeScale';
    procedure setLanguageCode(newValue: NSString); message 'setLanguageCode:';
    function languageCode: NSString; message 'languageCode';
    procedure setExtendedLanguageTag(newValue: NSString); message 'setExtendedLanguageTag:';
    function extendedLanguageTag: NSString; message 'extendedLanguageTag';
    procedure setPreferredTransform(newValue: CGAffineTransform); message 'setPreferredTransform:';
    function preferredTransform: CGAffineTransform; message 'preferredTransform';
    procedure setPreferredVolume(newValue: single); message 'setPreferredVolume:';
    function preferredVolume: single; message 'preferredVolume';
    procedure setSegments(newValue: NSArray); message 'setSegments:';
    function segments: NSArray; message 'segments';
    function insertTimeRange_ofTrack_atTime_error (timeRange: CMTimeRange; track: AVAssetTrack; startTime: CMTime; error: NSErrorPtr): ObjCBOOL; message 'insertTimeRange:ofTrack:atTime:error:';
    function insertTimeRanges_ofTracks_atTime_error (timeRanges: NSArray; tracks: NSArray; startTime: CMTime; error: NSErrorPtr): ObjCBOOL; message 'insertTimeRanges:ofTracks:atTime:error:'; { available in 10_8, 5_0 }
    procedure insertEmptyTimeRange (timeRange: CMTimeRange); message 'insertEmptyTimeRange:';
    procedure removeTimeRange (timeRange: CMTimeRange); message 'removeTimeRange:';
    procedure scaleTimeRange_toDuration (timeRange: CMTimeRange; duration: CMTime); message 'scaleTimeRange:toDuration:';
    function validateTrackSegments_error (trackSegments: NSArray; error: NSErrorPtr): ObjCBOOL; message 'validateTrackSegments:error:';
  end;
{$endif}

