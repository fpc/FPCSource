{ Parsed from CoreMediaIO.framework CMIOHardwareStream.h }

{$if  not (defined(__CMIOHardwareStream_h__))}

{$ifdef TYPES}
type
  CMIOStreamID = CMIOObjectID;
  CMIOStreamIDPtr = ^CMIOStreamID;
  CMIODeviceStreamQueueAlteredProc = procedure (streamID: CMIOStreamID; token: pointer; refCon: pointer); cdecl;

type
  CMIOStreamDeck = record
    mStatus: UInt32;
    mState: UInt32;
    mState2: UInt32;
  end;
type
  CMIOStreamDeckPtr = ^CMIOStreamDeck;

const
  kCMIOStreamClassID = 'astr';
  kCMIOStreamUnknown = kCMIOObjectUnknown;
const
  kCMIODeckStatusBusy = 1;
  kCMIODeckStatusLocal = 2;
  kCMIODeckStatusNotThreaded = 3;
  kCMIODeckStatusTapeInserted = 4;
  kCMIODeckStatusOpcode = 5;
  kCMIODeckStatusSearchingForDevice = 6;
  kCMIODeckStatusNoDevice = 7;
const
  kCMIODeckStateStop = 0;
  kCMIODeckStatePlay = 1;
  kCMIODeckStatePause = 2;
  kCMIODeckStatePlaySlow = 3;
  kCMIODeckStateReverseSlow = 4;
  kCMIODeckStatePlayReverse = 5;
  kCMIODeckStateFastForward = 6;
  kCMIODeckStateFastRewind = 7;
const
  kCMIODeckShuttleReverseHighSpeed = -10;
  kCMIODeckShuttleReverseFastest = -9;
  kCMIODeckShuttleReverseFaster = -8;
  kCMIODeckShuttleReverseFast = -7;
  kCMIODeckShuttleReverse1x = -6;
  kCMIODeckShuttleReverseSlow3 = -5;
  kCMIODeckShuttleReverseSlow2 = -4;
  kCMIODeckShuttleReverseSlow1 = -3;
  kCMIODeckShuttleReverseSlowest = -2;
  kCMIODeckShuttlePlayPreviousFrame = -1;
  kCMIODeckShuttlePause = 0;
  kCMIODeckShuttlePlayNextFrame = 1;
  kCMIODeckShuttlePlaySlowest = 2;
  kCMIODeckShuttlePlaySlow1 = 3;
  kCMIODeckShuttlePlaySlow2 = 4;
  kCMIODeckShuttlePlaySlow3 = 5;
  kCMIODeckShuttlePlay1x = 6;
  kCMIODeckShuttlePlayFast = 7;
  kCMIODeckShuttlePlayFaster = 8;
  kCMIODeckShuttlePlayFastest = 9;
  kCMIODeckShuttlePlayHighSpeed = 10;
const
  kCMIOStreamPropertyDirection = 'sdir';
  kCMIOStreamPropertyTerminalType = 'term';
  kCMIOStreamPropertyStartingChannel = 'schn';
  kCMIOStreamPropertyLatency = 'ltnc';
  kCMIOStreamPropertyFormatDescription = 'pft ';
  kCMIOStreamPropertyFormatDescriptions = 'pfta';
  kCMIOStreamPropertyStillImage = 'stmg';
  kCMIOStreamPropertyStillImageFormatDescriptions = 'stft';
  kCMIOStreamPropertyFrameRate = 'nfrt';
  kCMIOStreamPropertyMinimumFrameRate = 'mfrt';
  kCMIOStreamPropertyFrameRates = 'nfr#';
  kCMIOStreamPropertyFrameRateRanges = 'frrg';
  kCMIOStreamPropertyNoDataTimeoutInMSec = 'pmn1';
  kCMIOStreamPropertyDeviceSyncTimeoutInMSec = 'pmn2';
  kCMIOStreamPropertyNoDataEventCount = 'pmn3';
  kCMIOStreamPropertyOutputBufferUnderrunCount = 'pmou';
  kCMIOStreamPropertyOutputBufferRepeatCount = 'pmor';
  kCMIOStreamPropertyOutputBufferQueueSize = 'pmoq';
  kCMIOStreamPropertyOutputBuffersRequiredForStartup = 'pmos';
  kCMIOStreamPropertyOutputBuffersNeededForThrottledPlayback = 'miff';
  kCMIOStreamPropertyFirstOutputPresentationTimeStamp = 'popt';
  kCMIOStreamPropertyEndOfData = 'pmed';
  kCMIOStreamPropertyClock = 'pmcl';
  kCMIOStreamPropertyCanProcessDeckCommand = 'pdcd';
  kCMIOStreamPropertyDeck = 'deck';
  kCMIOStreamPropertyDeckFrameNumber = 'tcod';
  kCMIOStreamPropertyDeckDropness = 'drop';
  kCMIOStreamPropertyDeckThreaded = 'thrd';
  kCMIOStreamPropertyDeckLocal = 'locl';
  kCMIOStreamPropertyDeckCueing = 'cuec';
  kCMIOStreamPropertyInitialPresentationTimeStampForLinkedAndSyncedAudio = 'ipls';
  kCMIOStreamPropertyScheduledOutputNotificationProc = 'sonp';
  kCMIOStreamPropertyPreferredFormatDescription = 'prfd';
  kCMIOStreamPropertyPreferredFrameRate = 'prfr';
{$endif}

{$ifdef FUNCTIONS}
function CMIOStreamCopyBufferQueue(streamID: CMIOStreamID; queueAlteredProc: CMIODeviceStreamQueueAlteredProc; queueAlteredRefCon: pointer; queue: CMSimpleQueueRef): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamDeckPlay(streamID: CMIOStreamID): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamDeckStop(streamID: CMIOStreamID): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamDeckJog(streamID: CMIOStreamID; speed: SInt32): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamDeckCueTo(streamID: CMIOStreamID; frameNumber: UInt64; playOnCue: Boolean): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamClockCreate(allocator: CFAllocatorRef; clockName: CFStringRef; sourceIdentifier: pointer; getTimeCallMinimumInterval: CMTime; numberOfEventsForRateSmoothing: UInt32; numberOfAveragesForRateSmoothing: UInt32; clock: CFTypeRefPtr): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamClockPostTimingEvent(eventTime: CMTime; hostTime: UInt64; resynchronize: Boolean; clock: CFTypeRef): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamClockInvalidate(clock: CFTypeRef): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOStreamClockConvertHostTimeToDeviceTime(hostTime: UInt64; clock: CFTypeRef): CMTime; cdecl; external; { available in 10.7 and later }
{$endif}

{$ifdef TYPES}
type
  CMIOStreamScheduledOutputNotificationProc = procedure (sequenceNumberOfBufferThatWasOutput: UInt64; outputHostTime: UInt64; scheduledOutputNotificationRefCon: pointer); cdecl;

type
  CMIOStreamScheduledOutputNotificationProcAndRefCon = record
    scheduledOutputNotificationProc: CMIOStreamScheduledOutputNotificationProc;
    scheduledOutputNotificationRefCon: pointer;
  end;
type
  CMIOStreamScheduledOutputNotificationProcAndRefConPtr = ^CMIOStreamScheduledOutputNotificationProcAndRefCon;
{$endif}

{$endif}
