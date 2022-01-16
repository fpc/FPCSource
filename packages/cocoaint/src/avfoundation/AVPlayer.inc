{ Parsed from AVFoundation.framework AVPlayer.h }


{ Types from AVPlayerItemControl }
{$ifdef TYPES}

type
  AVPlayerActionAtItemEnd = NSInteger;
type
  AVPlayerActionAtItemEndPtr = ^AVPlayerActionAtItemEnd;

const
  AVPlayerActionAtItemEndAdvance = 0;
  AVPlayerActionAtItemEndPause = 1;
  AVPlayerActionAtItemEndNone = 2;
{$endif}


{$ifdef TYPES}
type
  AVPlayerPtr = ^AVPlayer;
  AVQueuePlayerPtr = ^AVQueuePlayer;
{$endif}

{$ifdef TYPES}
type
  AVPlayerStatus = NSInteger;
  AVPlayerStatusPtr = ^AVPlayerStatus;

const
  AVPlayerStatusUnknown = 0;
  AVPlayerStatusReadyToPlay = 1;
  AVPlayerStatusFailed = 2;
{$endif}

{$ifdef CLASSES}

type
  AVPlayer = objcclass external (NSObject)
  private
    _player: AVPlayerInternal;
  public
    class function playerWithURL (URL: NSURL): id; message 'playerWithURL:';
    class function playerWithPlayerItem (item: AVPlayerItem): id; message 'playerWithPlayerItem:';
    function initWithURL (URL: NSURL): instancetype; message 'initWithURL:';
    function initWithPlayerItem (item: AVPlayerItem): instancetype; message 'initWithPlayerItem:';
    function status: AVPlayerStatus; message 'status';
    function error: NSError; message 'error';
  end;


type
  AVPlayerPlaybackControl = objccategory external (AVPlayer)
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure play; message 'play';
    procedure pause; message 'pause';
  end;

type
  AVPlayerItemControl = objccategory external (AVPlayer)
    function currentItem: AVPlayerItem; message 'currentItem';
    procedure replaceCurrentItemWithPlayerItem (item: AVPlayerItem); message 'replaceCurrentItemWithPlayerItem:';
    procedure setActionAtItemEnd(newValue: AVPlayerActionAtItemEnd); message 'setActionAtItemEnd:';
    function actionAtItemEnd: AVPlayerActionAtItemEnd; message 'actionAtItemEnd';
  end;

type
  AVPlayerTimeControl = objccategory external (AVPlayer)
    function currentTime: CMTime; message 'currentTime';
    procedure seekToDate (date: NSDate); message 'seekToDate:';
    procedure seekToDate_completionHandler (date: NSDate; completionHandler: OpaqueCBlock); message 'seekToDate:completionHandler:'; { available in 10_7, 5_0 }
    procedure seekToTime (time: CMTime); message 'seekToTime:';
    procedure seekToTime_toleranceBefore_toleranceAfter (time: CMTime; toleranceBefore: CMTime; toleranceAfter: CMTime); message 'seekToTime:toleranceBefore:toleranceAfter:';
    procedure seekToTime_completionHandler (time: CMTime; completionHandler: OpaqueCBlock); message 'seekToTime:completionHandler:'; { available in 10_7, 5_0 }
    procedure seekToTime_toleranceBefore_toleranceAfter_completionHandler (time: CMTime; toleranceBefore: CMTime; toleranceAfter: CMTime; completionHandler: OpaqueCBlock); message 'seekToTime:toleranceBefore:toleranceAfter:completionHandler:'; { available in 10_7, 5_0 }
  end;

type
  AVPlayerAdvancedRateControl = objccategory external (AVPlayer)
    procedure setRate_time_atHostTime (rate: single; itemTime: CMTime; hostClockTime: CMTime); message 'setRate:time:atHostTime:'; { available in 10_8, 6_0 }
    procedure prerollAtRate_completionHandler (rate: single; completionHandler: OpaqueCBlock); message 'prerollAtRate:completionHandler:'; { available in 10_8, 6_0 }
    procedure cancelPendingPrerolls; message 'cancelPendingPrerolls'; { available in 10_8, 6_0 }
    procedure setMasterClock(newValue: CMClockRef); message 'setMasterClock:';
    function masterClock: CMClockRef; message 'masterClock';
  end;

type
  AVPlayerTimeObservation = objccategory external (AVPlayer)
    function addPeriodicTimeObserverForInterval_queue_usingBlock (interval: CMTime; queue: dispatch_queue_t; block: OpaqueCBlock): id; message 'addPeriodicTimeObserverForInterval:queue:usingBlock:';
    function addBoundaryTimeObserverForTimes_queue_usingBlock (times: NSArray; queue: dispatch_queue_t; block: OpaqueCBlock): id; message 'addBoundaryTimeObserverForTimes:queue:usingBlock:';
    procedure removeTimeObserver (observer: id); message 'removeTimeObserver:';
  end;

type
  AVPlayerMediaControl = objccategory external (AVPlayer)
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
    procedure setMuted(newValue: ObjCBOOL); message 'setMuted:';
    function isMuted: ObjCBOOL; message 'isMuted';
    procedure setClosedCaptionDisplayEnabled(newValue: ObjCBOOL); message 'setClosedCaptionDisplayEnabled:';
    function isClosedCaptionDisplayEnabled: ObjCBOOL; message 'isClosedCaptionDisplayEnabled';
  end;

type
  AVPlayerAutomaticMediaSelection = objccategory external (AVPlayer)
    procedure setAppliesMediaSelectionCriteriaAutomatically(newValue: ObjCBOOL); message 'setAppliesMediaSelectionCriteriaAutomatically:';
    function appliesMediaSelectionCriteriaAutomatically: ObjCBOOL; message 'appliesMediaSelectionCriteriaAutomatically';
    procedure setMediaSelectionCriteria_forMediaCharacteristic (criteria: AVPlayerMediaSelectionCriteria; mediaCharacteristic: NSString); message 'setMediaSelectionCriteria:forMediaCharacteristic:'; { available in 10_9, 7_0 }
    function mediaSelectionCriteriaForMediaCharacteristic (mediaCharacteristic: NSString): AVPlayerMediaSelectionCriteria; message 'mediaSelectionCriteriaForMediaCharacteristic:'; { available in 10_9, 7_0 }
  end;

type
  AVPlayerAudioDeviceSupport = objccategory external (AVPlayer)
    procedure setAudioOutputDeviceUniqueID(newValue: NSString); message 'setAudioOutputDeviceUniqueID:';
    function audioOutputDeviceUniqueID: NSString; message 'audioOutputDeviceUniqueID';
  end;
{$endif}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef CLASSES}

type
  AVPlayerExternalPlaybackSupport = objccategory external (AVPlayer)
    procedure setAllowsExternalPlayback(newValue: ObjCBOOL); message 'setAllowsExternalPlayback:';
    function allowsExternalPlayback: ObjCBOOL; message 'allowsExternalPlayback';
    function isExternalPlaybackActive: ObjCBOOL; message 'isExternalPlaybackActive';
    procedure setUsesExternalPlaybackWhileExternalScreenIsActive(newValue: ObjCBOOL); message 'setUsesExternalPlaybackWhileExternalScreenIsActive:';
    function usesExternalPlaybackWhileExternalScreenIsActive: ObjCBOOL; message 'usesExternalPlaybackWhileExternalScreenIsActive';
    procedure setExternalPlaybackVideoGravity(newValue: NSString); message 'setExternalPlaybackVideoGravity:';
    function externalPlaybackVideoGravity: NSString; message 'externalPlaybackVideoGravity';
  end;

type
  AVPlayerAirPlaySupport = objccategory external (AVPlayer)
    procedure setAllowsAirPlayVideo(newValue: ObjCBOOL); message 'setAllowsAirPlayVideo:';
    function allowsAirPlayVideo: ObjCBOOL; message 'allowsAirPlayVideo';
    function isAirPlayVideoActive: ObjCBOOL; message 'isAirPlayVideoActive';
    procedure setUsesAirPlayVideoWhileAirPlayScreenIsActive(newValue: ObjCBOOL); message 'setUsesAirPlayVideoWhileAirPlayScreenIsActive:';
    function usesAirPlayVideoWhileAirPlayScreenIsActive: ObjCBOOL; message 'usesAirPlayVideoWhileAirPlayScreenIsActive';
  end;

type
  AVPlayerProtectedContent = objccategory external (AVPlayer)
    function outputObscuredDueToInsufficientExternalProtection: ObjCBOOL; message 'outputObscuredDueToInsufficientExternalProtection';
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  AVQueuePlayer = objcclass external (AVPlayer)
  private
    _queuePlayer: AVQueuePlayerInternal;
  public
    class function queuePlayerWithItems (items: NSArray): id; message 'queuePlayerWithItems:';
    function initWithItems (items: NSArray): instancetype; message 'initWithItems:';
    function items: NSArray; message 'items';
    procedure advanceToNextItem; message 'advanceToNextItem';
    function canInsertItem_afterItem (item: AVPlayerItem; afterItem: AVPlayerItem): ObjCBOOL; message 'canInsertItem:afterItem:';
    procedure insertItem_afterItem (item: AVPlayerItem; afterItem: AVPlayerItem); message 'insertItem:afterItem:';
    procedure removeItem (item: AVPlayerItem); message 'removeItem:';
    procedure removeAllItems; message 'removeAllItems';
  end;
{$endif}

