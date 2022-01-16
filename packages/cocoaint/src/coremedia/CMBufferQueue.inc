{ Parsed from CoreMedia.framework CMBufferQueue.h }

{$ifndef CMBUFFERQUEUE_H}

{$ifdef TYPES}
const
  kCMBufferQueueError_AllocationFailed = -12760;
  kCMBufferQueueError_RequiredParameterMissing = -12761;
  kCMBufferQueueError_InvalidCMBufferCallbacksStruct = -12762;
  kCMBufferQueueError_EnqueueAfterEndOfData = -12763;
  kCMBufferQueueError_QueueIsFull = -12764;
  kCMBufferQueueError_BadTriggerDuration = -12765;
  kCMBufferQueueError_CannotModifyQueueFromTriggerCallback = -12766;
  kCMBufferQueueError_InvalidTriggerCondition = -12767;
  kCMBufferQueueError_InvalidTriggerToken = -12768;
  kCMBufferQueueError_InvalidBuffer = -12769;

type
  CMBufferQueueRef = OpaqueType;
  CMBufferRef = CFTypeRef;
  CMBufferRefPtr = ^CMBufferRef;
  CMBufferGetTimeCallback = function (buf: CMBufferRef; refcon: pointer): CMTime; cdecl;
  CMBufferGetBooleanCallback = function (buf: CMBufferRef; refcon: pointer): Boolean; cdecl;
  CMBufferCompareCallback = function (buf1: CMBufferRef; buf2: CMBufferRef; refcon: pointer): CFComparisonResult; cdecl;
  CMBufferGetSizeCallback = function (buf: CMBufferRef; refcon: pointer): size_t; cdecl;

type
  CMBufferCallbacks = record
    version: cuint32;
    refcon: pointer;
    getDecodeTimeStamp: CMBufferGetTimeCallback;
    getPresentationTimeStamp: CMBufferGetTimeCallback;
    getDuration: CMBufferGetTimeCallback;
    isDataReady: CMBufferGetBooleanCallback;
    compare: CMBufferCompareCallback;
    dataBecameReadyNotification: CFStringRef;
    getSize: CMBufferGetSizeCallback;
  end;
type
  CMBufferCallbacksPtr = ^CMBufferCallbacks;
{$endif}

{$ifdef FUNCTIONS}
function CMBufferQueueGetCallbacksForUnsortedSampleBuffers: CMBufferCallbacksPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetCallbacksForSampleBuffersSortedByOutputPTS: CMBufferCallbacksPtr; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_3 }
function CMBufferQueueCreate(allocator: CFAllocatorRef; capacity: CMItemCount; callbacks: CMBufferCallbacksPtr; queueOut: CMBufferQueueRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueEnqueue(queue: CMBufferQueueRef; buf: CMBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueDequeueAndRetain(queue: CMBufferQueueRef): CMBufferRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueDequeueIfDataReadyAndRetain(queue: CMBufferQueueRef): CMBufferRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetHead(queue: CMBufferQueueRef): CMBufferRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueIsEmpty(queue: CMBufferQueueRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueMarkEndOfData(queue: CMBufferQueueRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueContainsEndOfData(queue: CMBufferQueueRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueIsAtEndOfData(queue: CMBufferQueueRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueReset(queue: CMBufferQueueRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetBufferCount(queue: CMBufferQueueRef): CMItemCount; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetDuration(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetMinDecodeTimeStamp(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetFirstDecodeTimeStamp(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetMinPresentationTimeStamp(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetFirstPresentationTimeStamp(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetMaxPresentationTimeStamp(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetEndPresentationTimeStamp(queue: CMBufferQueueRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueGetTotalSize(queue: CMBufferQueueRef): size_t; cdecl; external; { available starting in __MAC_10_10,__IPHONE_7_1 }
{$endif}

{$ifdef TYPES}
type
  CMBufferQueueTriggerToken = OpaqueType;
  CMBufferQueueTriggerCallback = procedure (triggerRefcon: pointer; triggerToken: CMBufferQueueTriggerToken); cdecl;

const
  kCMBufferQueueTrigger_WhenDurationBecomesLessThan = 1;
  kCMBufferQueueTrigger_WhenDurationBecomesLessThanOrEqualTo = 2;
  kCMBufferQueueTrigger_WhenDurationBecomesGreaterThan = 3;
  kCMBufferQueueTrigger_WhenDurationBecomesGreaterThanOrEqualTo = 4;
  kCMBufferQueueTrigger_WhenMinPresentationTimeStampChanges = 5;
  kCMBufferQueueTrigger_WhenMaxPresentationTimeStampChanges = 6;
  kCMBufferQueueTrigger_WhenDataBecomesReady = 7;
  kCMBufferQueueTrigger_WhenEndOfDataReached = 8;
  kCMBufferQueueTrigger_WhenReset = 9;
  kCMBufferQueueTrigger_WhenBufferCountBecomesLessThan = 10;
  kCMBufferQueueTrigger_WhenBufferCountBecomesGreaterThan = 11;

type
  CMBufferQueueTriggerCondition = cint32;
  CMBufferQueueTriggerConditionPtr = ^CMBufferQueueTriggerCondition;
{$endif}

{$ifdef FUNCTIONS}
function CMBufferQueueInstallTrigger(queue: CMBufferQueueRef; triggerCallback: CMBufferQueueTriggerCallback; triggerRefcon: pointer; triggerCondition: CMBufferQueueTriggerCondition; triggerTime: CMTime; triggerTokenOut: CMBufferQueueTriggerToken): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueInstallTriggerWithIntegerThreshold(queue: CMBufferQueueRef; triggerCallback: CMBufferQueueTriggerCallback; triggerRefcon: pointer; triggerCondition: CMBufferQueueTriggerCondition; triggerThreshold: CMItemCount; triggerTokenOut: CMBufferQueueTriggerToken): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueRemoveTrigger(queue: CMBufferQueueRef; triggerToken: CMBufferQueueTriggerToken): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBufferQueueTestTrigger(queue: CMBufferQueueRef; triggerToken: CMBufferQueueTriggerToken): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CMBufferValidationCallback = function (queue: CMBufferQueueRef; buf: CMBufferRef; validationRefCon: pointer): OSStatus; cdecl;
{$endif}

{$ifdef FUNCTIONS}
function CMBufferQueueSetValidationCallback(queue: CMBufferQueueRef; validationCallback: CMBufferValidationCallback; validationRefCon: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$endif}
