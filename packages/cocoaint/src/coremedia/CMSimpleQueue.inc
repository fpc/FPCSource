{ Parsed from CoreMedia.framework CMSimpleQueue.h }

{$ifndef CMSIMPLEQUEUE_H}

{$ifdef TYPES}
const
  kCMSimpleQueueError_AllocationFailed = -12770;
  kCMSimpleQueueError_RequiredParameterMissing = -12771;
  kCMSimpleQueueError_ParameterOutOfRange = -12772;
  kCMSimpleQueueError_QueueIsFull = -12773;

type
  CMSimpleQueueRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function CMSimpleQueueGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueCreate(allocator: CFAllocatorRef; capacity: cint32; queueOut: CMSimpleQueueRef): OSStatus; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueEnqueue(queue: CMSimpleQueueRef; element: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueDequeue(queue: CMSimpleQueueRef): pointer; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueGetHead(queue: CMSimpleQueueRef): pointer; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueReset(queue: CMSimpleQueueRef): OSStatus; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueGetCapacity(queue: CMSimpleQueueRef): cint32; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
function CMSimpleQueueGetCount(queue: CMSimpleQueueRef): cint32; cdecl; external; { available starting in __MAC_10_7, __IPHONE_5_0 }
{$endif}

{$endif}
