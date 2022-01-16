{ Parsed from CoreMedia.framework CMSync.h }

{$ifndef __CMSYNC__}

{$ifdef TYPES}
type
  CMClockRef = OpaqueType;
  CMTimebaseRef = OpaqueType;
  CMClockOrTimebaseRef = CFTypeRef;
  CMClockOrTimebaseRefPtr = ^CMClockOrTimebaseRef;

const
  kCMClockError_MissingRequiredParameter = -12745;
  kCMClockError_InvalidParameter = -12746;
  kCMClockError_AllocationFailed = -12747;
  kCMClockError_UnsupportedOperation = -12756;
const
  kCMTimebaseError_MissingRequiredParameter = -12748;
  kCMTimebaseError_InvalidParameter = -12749;
  kCMTimebaseError_AllocationFailed = -12750;
  kCMTimebaseError_TimerIntervalTooShort = -12751;
  kCMTimebaseError_ReadOnly = -12757;
const
  kCMSyncError_MissingRequiredParameter = -12752;
  kCMSyncError_InvalidParameter = -12753;
  kCMSyncError_AllocationFailed = -12754;
  kCMSyncError_RateMustBeNonZero = -12755;
{$endif}

{$ifdef FUNCTIONS}
function CMClockGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMClockGetHostTimeClock: CMClockRef; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMClockConvertHostTimeToSystemUnits(hostTime: CMTime): cuint64; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMClockMakeHostTimeFromSystemUnits(hostTime: cuint64): CMTime; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMClockGetTime(clock: CMClockRef): CMTime; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMClockGetAnchorTime(clock: CMClockRef; outClockTime: CMTimePtr; outReferenceClockTime: CMTimePtr): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMClockMightDrift(clock: CMClockRef; otherClock: CMClockRef): Boolean; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
procedure CMClockInvalidate(clock: CMClockRef); cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseCreateWithMasterClock(allocator: CFAllocatorRef; masterClock: CMClockRef; timebaseOut: CMTimebaseRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseCreateWithMasterTimebase(allocator: CFAllocatorRef; masterTimebase: CMTimebaseRef; timebaseOut: CMTimebaseRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetMasterTimebase(timebase: CMTimebaseRef): CMTimebaseRef; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetMasterClock(timebase: CMTimebaseRef): CMClockRef; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetMaster(timebase: CMTimebaseRef): CMClockOrTimebaseRef; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetUltimateMasterClock(timebase: CMTimebaseRef): CMClockRef; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetTime(timebase: CMTimebaseRef): CMTime; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetTimeWithTimeScale(timebase: CMTimebaseRef; timescale: CMTimeScale; method: CMTimeRoundingMethod): CMTime; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetTime(timebase: CMTimebaseRef; time: CMTime): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetAnchorTime(timebase: CMTimebaseRef; timebaseTime: CMTime; immediateMasterTime: CMTime): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetRate(timebase: CMTimebaseRef): Float64; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetTimeAndRate(timebase: CMTimebaseRef; outTime: CMTimePtr; outRate: Float64Ptr): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetRate(timebase: CMTimebaseRef; rate: Float64): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetRateAndAnchorTime(timebase: CMTimebaseRef; rate: Float64; timebaseTime: CMTime; immediateMasterTime: CMTime): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseGetEffectiveRate(timebase: CMTimebaseRef): Float64; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseAddTimer(timebase: CMTimebaseRef; timer: CFRunLoopTimerRef; runloop: CFRunLoopRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseRemoveTimer(timebase: CMTimebaseRef; timer: CFRunLoopTimerRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetTimerNextFireTime(timebase: CMTimebaseRef; timer: CFRunLoopTimerRef; fireTime: CMTime; flags: cuint32): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetTimerToFireImmediately(timebase: CMTimebaseRef; timer: CFRunLoopTimerRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseAddTimerDispatchSource(timebase: CMTimebaseRef; timerSource: dispatch_source_t): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseRemoveTimerDispatchSource(timebase: CMTimebaseRef; timerSource: dispatch_source_t): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetTimerDispatchSourceNextFireTime(timebase: CMTimebaseRef; timerSource: dispatch_source_t; fireTime: CMTime; flags: cuint32): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseSetTimerDispatchSourceToFireImmediately(timebase: CMTimebaseRef; timerSource: dispatch_source_t): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMSyncGetRelativeRate(ofClockOrTimebase: CMClockOrTimebaseRef; relativeToClockOrTimebase: CMClockOrTimebaseRef): Float64; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMSyncGetRelativeRateAndAnchorTime(ofClockOrTimebase: CMClockOrTimebaseRef; relativeToClockOrTimebase: CMClockOrTimebaseRef; outRelativeRate: Float64Ptr; outOfClockOrTimebaseAnchorTime: CMTimePtr; outRelativeToClockOrTimebaseAnchorTime: CMTimePtr): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMSyncConvertTime(time: CMTime; fromClockOrTimebase: CMClockOrTimebaseRef; toClockOrTimebase: CMClockOrTimebaseRef): CMTime; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMSyncMightDrift(clockOrTimebase1: CMClockOrTimebaseRef; clockOrTimebase2: CMClockOrTimebaseRef): Boolean; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMSyncGetTime(clockOrTimebase: CMClockOrTimebaseRef): CMTime; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
function CMTimebaseNotificationBarrier(timebase: CMTimebaseRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimebaseNotification_EffectiveRateChanged: CFStringRef { available starting in __MAC_10_8, __IPHONE_6_0 }; cvar; external;
  kCMTimebaseNotification_TimeJumped: CFStringRef { available starting in __MAC_10_8, __IPHONE_6_0 }; cvar; external;
  kCMTimebaseNotificationKey_EventTime: CFStringRef { available starting in __MAC_10_9, __IPHONE_7_0 }; cvar; external;
{$endif}

{$endif}
