{ Parsed from CoreMedia.framework CMBlockBuffer.h }

{$ifndef CMBLOCKBUFFER_H}

{$ifdef TYPES}
const
  kCMBlockBufferNoErr = 0;
  kCMBlockBufferStructureAllocationFailedErr = -12700;
  kCMBlockBufferBlockAllocationFailedErr = -12701;
  kCMBlockBufferBadCustomBlockSourceErr = -12702;
  kCMBlockBufferBadOffsetParameterErr = -12703;
  kCMBlockBufferBadLengthParameterErr = -12704;
  kCMBlockBufferBadPointerParameterErr = -12705;
  kCMBlockBufferEmptyBBufErr = -12706;
  kCMBlockBufferUnallocatedBlockErr = -12707;
  kCMBlockBufferInsufficientSpaceErr = -12708;
const
  kCMBlockBufferAssureMemoryNowFlag = 1 shl 0;
  kCMBlockBufferAlwaysCopyDataFlag = 1 shl 1;
  kCMBlockBufferDontOptimizeDepthFlag = 1 shl 2;
  kCMBlockBufferPermitEmptyReferenceFlag = 1 shl 3;

type
  CMBlockBufferFlags = cuint32;
  CMBlockBufferFlagsPtr = ^CMBlockBufferFlags;
  CMBlockBufferRef = OpaqueType;

type
  CMBlockBufferCustomBlockSource = record
    version: cuint32;
    AllocateBlock: function (refCon: pointer; sizeInBytes: size_t): pointer; cdecl;
    FreeBlock: procedure (refCon: pointer; doomedMemoryBlock: pointer; sizeInBytes: size_t); cdecl;
    refCon: pointer;
  end;
type
  CMBlockBufferCustomBlockSourcePtr = ^CMBlockBufferCustomBlockSource;

const
  kCMBlockBufferCustomBlockSourceVersion = 0;
{$endif}

{$ifdef FUNCTIONS}
function CMBlockBufferCreateEmpty(structureAllocator: CFAllocatorRef; subBlockCapacity: cuint32; flags: CMBlockBufferFlags; newBBufOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferCreateWithMemoryBlock(structureAllocator: CFAllocatorRef; memoryBlock: pointer; blockLength: size_t; blockAllocator: CFAllocatorRef; customBlockSource: CMBlockBufferCustomBlockSourcePtr; offsetToData: size_t; dataLength: size_t; flags: CMBlockBufferFlags; newBBufOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferCreateWithBufferReference(structureAllocator: CFAllocatorRef; targetBuffer: CMBlockBufferRef; offsetToData: size_t; dataLength: size_t; flags: CMBlockBufferFlags; newBBufOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferCreateContiguous(structureAllocator: CFAllocatorRef; sourceBuffer: CMBlockBufferRef; blockAllocator: CFAllocatorRef; customBlockSource: CMBlockBufferCustomBlockSourcePtr; offsetToData: size_t; dataLength: size_t; flags: CMBlockBufferFlags; newBBufOut: CMBlockBufferRef): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferAppendMemoryBlock(theBuffer: CMBlockBufferRef; memoryBlock: pointer; blockLength: size_t; blockAllocator: CFAllocatorRef; customBlockSource: CMBlockBufferCustomBlockSourcePtr; offsetToData: size_t; dataLength: size_t; flags: CMBlockBufferFlags): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferAppendBufferReference(theBuffer: CMBlockBufferRef; targetBBuf: CMBlockBufferRef; offsetToData: size_t; dataLength: size_t; flags: CMBlockBufferFlags): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferAssureBlockMemory(theBuffer: CMBlockBufferRef): OSStatus; cdecl; external;
function CMBlockBufferAccessDataBytes(theBuffer: CMBlockBufferRef; offset: size_t; length: size_t; temporaryBlock: pointer; returnedPointer: PChar): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferCopyDataBytes(theSourceBuffer: CMBlockBufferRef; offsetToData: size_t; dataLength: size_t; destination: pointer): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferReplaceDataBytes(sourceBytes: pointer; destinationBuffer: CMBlockBufferRef; offsetIntoDestination: size_t; dataLength: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferFillDataBytes(fillByte: char; destinationBuffer: CMBlockBufferRef; offsetIntoDestination: size_t; dataLength: size_t): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferGetDataPointer(theBuffer: CMBlockBufferRef; offset: size_t; lengthAtOffset: size_tPtr; totalLength: size_tPtr; dataPointer: PChar): OSStatus; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferGetDataLength(theBuffer: CMBlockBufferRef): size_t; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferIsRangeContiguous(theBuffer: CMBlockBufferRef; offset: size_t; length: size_t): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMBlockBufferIsEmpty(theBuffer: CMBlockBufferRef): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$endif}
