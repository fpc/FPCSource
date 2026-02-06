{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    functions for heap management in the data segment

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{ Do not use standard memory manager }
{ $define HAS_MEMORYMANAGER}

{ Memory manager }
const
  MemoryManager: TMemoryManager = (
    NeedLock: false;  // Obsolete
    GetMem: {$ifndef FPC_NO_DEFAULT_HEAP}@SysGetMem{$else}nil{$endif};
    FreeMem: {$ifndef FPC_NO_DEFAULT_HEAP}@SysFreeMem{$else}nil{$endif};
    FreeMemSize: {$ifndef FPC_NO_DEFAULT_HEAP}@SysFreeMemSize{$else}nil{$endif};
    AllocMem: {$ifndef FPC_NO_DEFAULT_HEAP}@SysAllocMem{$else}nil{$endif};
    ReAllocMem: {$ifndef FPC_NO_DEFAULT_HEAP}@SysReAllocMem{$else}nil{$endif};
    MemSize: {$ifndef FPC_NO_DEFAULT_HEAP}@SysMemSize{$else}nil{$endif};
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: {$ifndef FPC_NO_DEFAULT_HEAP}@SysGetHeapStatus{$else}nil{$endif};
    GetFPCHeapStatus: {$ifndef FPC_NO_DEFAULT_HEAP}@SysGetFPCHeapStatus{$else}nil{$endif};
  ); {$ifdef FPC_NO_DEFAULT_HEAP} public name 'FPC_SYSTEM_MEMORYMANAGER'; {$endif}

{*****************************************************************************
                             Memory Manager
*****************************************************************************}

procedure GetMemoryManager(var MemMgr:TMemoryManager);
begin
  MemMgr := MemoryManager;
end;


procedure SetMemoryManager(const MemMgr:TMemoryManager);
begin
  MemoryManager := MemMgr;
end;

function IsMemoryManagerSet:Boolean;
begin
{$if defined(HAS_MEMORYMANAGER) or defined(FPC_NO_DEFAULT_HEAP)}
  Result:=false;
{$else not FPC_NO_DEFAULT_HEAP}
  IsMemoryManagerSet := (MemoryManager.GetMem<>@SysGetMem)
    or (MemoryManager.FreeMem<>@SysFreeMem);
{$endif HAS_MEMORYMANAGER or FPC_NO_DEFAULT_HEAP}
end;

{$ifdef FPC_HAS_FEATURE_HEAP}
procedure GetMem(Out p:pointer;Size:ptruint);
begin
  p := MemoryManager.GetMem(Size);
end;

procedure GetMemory(Out p:pointer;Size:ptruint);
begin
  GetMem(p,size);
end;

procedure FreeMem(p:pointer;Size:ptruint);
begin
  MemoryManager.FreeMemSize(p,Size);
end;

procedure FreeMemory(p:pointer;Size:ptruint);
begin
  FreeMem(p,size);
end;


function GetHeapStatus:THeapStatus;
begin
  Result:=MemoryManager.GetHeapStatus();
end;


function GetFPCHeapStatus:TFPCHeapStatus;
begin
  Result:=MemoryManager.GetFPCHeapStatus();
end;


function MemSize(p:pointer):ptruint;
begin
  MemSize := MemoryManager.MemSize(p);
end;


{ Delphi style }
function FreeMem(p:pointer):ptruint;
begin
  FreeMem := MemoryManager.FreeMem(p);
end;

function FreeMemory(p:pointer):ptruint; cdecl;
begin
  FreeMemory := FreeMem(p);
end;

function GetMem(size:ptruint):pointer;
begin
  GetMem := MemoryManager.GetMem(Size);
end;

function GetMemory(size:ptruint):pointer; cdecl;
begin
  GetMemory := GetMem(size);
end;

function AllocMem(Size:ptruint):pointer;
begin
  AllocMem := MemoryManager.AllocMem(size);
end;


function ReAllocMem(var p:pointer;Size:ptruint):pointer;
begin
  ReAllocMem := MemoryManager.ReAllocMem(p,size);
end;

function ReAllocMemory(p:pointer;Size:ptruint):pointer; cdecl;
begin
  ReAllocMemory := ReAllocMem(p,size);
end;


{ Needed for calls from Assembler }
function fpc_getmem(size:ptruint):pointer;compilerproc;[public,alias:'FPC_GETMEM'];
begin
  fpc_GetMem := MemoryManager.GetMem(size);
end;

procedure fpc_freemem(p:pointer);compilerproc;[public,alias:'FPC_FREEMEM'];
begin
  MemoryManager.FreeMem(p);
end;

{$ifndef HAS_MEMORYMANAGER}
type

{
  We use 'fixed' size chunks for small allocations,
  os chunks with variable sized blocks for bigger allocations,
  and (almost) directly use os chunks for huge allocations.

  * a block is an area allocated by user
  * a chunk is a block plus our bookkeeping
  * an os chunk is a collection of chunks

  Memory layout:
    fixed:                 < CommonHeader >   [ ... user data ... ]
    variable:  [ VarHeader < CommonHeader > ] [ ... user data ... ]
    huge:        HugeChunk < CommonHeader >   [ ... user data ... ]

  When all chunks in an os chunk are free, we keep a few around
  but otherwise it will be freed to the OS.
}

{$ifdef ENDIAN_LITTLE}
  {$define HEAP_INC_USE_SETS} { Potentially better codegen than “or 1 shl” etc. (at least on x86). Can be adapted for big endian, too, but I have no such platform to test. }
{$endif ENDIAN_LITTLE}

  HeapInc = object
  const
    { Alignment requirement for blocks. All fixed sizes (among other things) are assumed to be divisible. }
    Alignment = 2 * sizeof(pointer);

    { Fixed chunk sizes are:
      ┌──── step = 16 ────┐┌─── step = 32 ────┐┌──── step = 48 ───┐┌ step 64 ┐
      16, 32, 48, 64, 80, 96, 128, 160, 192, 224, 272, 320, 368, 416, 480, 544
      #0  #1  #2  #3  #4  #5  #6   #7   #8   #9   #10  #11  #12  #13  #14  #15 }
    MinFixedHeaderAndPayload = 16;
    MaxFixedHeaderAndPayload = 544;
    FixedSizesCount = 16;
    FixedSizes: array[0 .. FixedSizesCount - 1] of uint16 = (16, 32, 48, 64, 80, 96, 128, 160, 192, 224, 272, 320, 368, 416, 480, 544);
    SizeMinus1Div16ToIndex: array[0 .. (MaxFixedHeaderAndPayload - 1) div 16] of uint8 =
      {  16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256, 272, 288, 304, 320, 336, 352, 368, 384, 400, 416, 432, 448, 464, 480, 496, 512, 528, 544 }
      (   0,  1,  2,  3,  4,  5,   6,   6,   7,   7,   8,   8,   9,   9,  10,  10,  10,  11,  11,  11,  12,  12,  12,  13,  13,  13,  14,  14,  14,  14,  15,  15,  15,  15);

    class function SizeMinus1ToIndex(sizeMinus1: SizeUint): SizeUint; static; inline; { sizeMinus1 + 1 ≤ MaxFixedHeaderAndPayload }
    class function IndexToSize(sizeIndex: SizeUint): SizeUint; static; inline;

  const
    OSChunkVarSizeQuant = 64 * 1024;
    FixedArenaSizeQuant = 4 * 1024;
    MinFixedArenaSize = 8 * 1024;
    MaxFixedArenaSize = 64 * 1024;
    MaxKeptFixedArenas = 4;

    { Bin index that corresponds to the minimum size ChooseFixedArenaSize can return.
      = VarSizeToBinIndex(MinFixedArenaSize - MinFixedArenaSize shr (FirstVarRangeP2 - FirstVarStepP2), roundUp := true) or so.
      Pure functions could be useful to calculate these with less hardcoding... }
    MinArenaBinIndex = 96;
    { Maximum size that can be added by AllocVar(isArena := true) to the original request.
      BinIndexToVarSize(MinArenaBinIndex) - 1 is possibly the (minimal) ideal value because it guarantees that
      there will be no tail left unusable for further arenas, but can be arbitrary. }
    MaxArenaOverallocation = 7968 - 1;

  { Adjustable part ends here~ }

  const
    SizeIndexBits = 1 + trunc(ln(FixedSizesCount - 1) /  ln(2));
    SizeIndexMask = 1 shl SizeIndexBits - 1;
    FixedBitPos = {$if SizeIndexBits >= 4} SizeIndexBits {$else} 4 {$endif}; { Variable chunks use 4 low bits for used / last / prev. free / fixed arena. }
    FixedFlag = 1 shl FixedBitPos;
    FixedArenaOffsetShift = {$if FixedBitPos + 1 >= 5} FixedBitPos + 1 {$else} 5 {$endif}; { VarSizeQuant is expected to be 2^5. }

    UsedFlag = 1 shl 0;
    LastFlag = 1 shl 1;
    PrevIsFreeFlag = 1 shl 2;
    FixedArenaFlag = 1 shl 3;
    VarSizeQuant = 1 shl FixedArenaOffsetShift; {$if VarSizeQuant <> 32} {$error Should in principle work but explanations below assume exactly 32. :)} {$endif}
    VarSizeMask = uint32(-VarSizeQuant);
    HugeHeader = 0; { Special header value for huge chunks. FixedFlag must be 0, and the value must be impossible for a variable chunk. 0 turns out to be suitable. :) }

    { Variable chunk sizes, not counting extra MaxFixedHeaderAndPayload added to each of these:

      32 sizes in the range +1 ..   1 024 (2^10) rounded up to the multiple of     32 = 2^ 5, + 0,                max =     1 024 =            %100 0000 0000
      32 sizes in the range +1 ..   2 048 (2^11) rounded up to the multiple of     64 = 2^ 6, + 1024,             max =     3 072 =           %1100 0000 0000
      32 sizes in the range +1 ..   4 096 (2^12) rounded up to the multiple of    128 = 2^ 7, + 1024 + 2048,      max =     7 168 =         %1 1100 0000 0000
      32 sizes in the range +1 ..   8 192 (2^13) rounded up to the multiple of    256 = 2^ 8, + 2^10 + .. + 2^12, max =    15 360 =        %11 1100 0000 0000
      32 sizes in the range +1 ..  16 384 (2^14) rounded up to the multiple of    512 = 2^ 9, + 2^10 + .. + 2^13, max =    31 744 =       %111 1100 0000 0000
      32 sizes in the range +1 ..  32 768 (2^15) rounded up to the multiple of  1 024 = 2^10, + 2^10 + .. + 2^14, max =    64 512 =      %1111 1100 0000 0000
      32 sizes in the range +1 ..  65 536 (2^16) rounded up to the multiple of  2 048 = 2^11, + 2^10 + .. + 2^15, max =   130 048 =    %1 1111 1100 0000 0000
      32 sizes in the range +1 .. 131 072 (2^17) rounded up to the multiple of  4 096 = 2^12, + 2^10 + .. + 2^16, max =   261 120 =   %11 1111 1100 0000 0000
      32 sizes in the range +1 .. 262 144 (2^18) rounded up to the multiple of  8 192 = 2^13, + 2^10 + .. + 2^17, max =   523 264 =  %111 1111 1100 0000 0000
      32 sizes in the range +1 .. 524 288 (2^19) rounded up to the multiple of 16 384 = 2^14, + 2^10 + .. + 2^18, max = 1 047 552 = %1111 1111 1100 0000 0000 }

    FirstVarRangeP2 = 10;
    FirstVarStepP2 = FixedArenaOffsetShift; {$if FirstVarStepP2 <> 5} {$error :|} {$endif}
    VarSizeClassesCount = 10;
    VarSizesPerClass = 32;
    VarSizesCount = VarSizeClassesCount * VarSizesPerClass;
    L0BinSize = 32;

    { Minimum size of the chunk that can be added to varFree.
      Medium chunks can be smaller than this, all the way down to MinAnyVarHeaderAndPayload defined later in terms of things it must fit;
      they aren’t visible for varFree searches but are visible for merging with freed neighbors. }
    MinSearchableVarHeaderAndPayload = (MaxFixedHeaderAndPayload + 1 shl FirstVarStepP2 + VarSizeQuant - 1) and -VarSizeQuant;
    MaxVarHeaderAndPayload = (MaxFixedHeaderAndPayload + (1 shl VarSizeClassesCount - 1) shl FirstVarRangeP2) and -VarSizeQuant; {$if MaxVarHeaderAndPayload <> MaxFixedHeaderAndPayload + 1047552} {$error does not match the explanation above :D} {$endif}

    class function VarSizeToBinIndex(size: SizeUint; roundUp: boolean): SizeUint; static; inline; { roundUp is constant. }
    class function BinIndexToVarSize(binIndex: SizeUint): SizeUint; static; inline;

  type
    { Common header of any memory chunk, residing immediately to the left of the ~payload~ (block).

      Fixed chunk header, assuming SizeIndexBits = 4:
      h[0:3] = size index (= h and SizeIndexMask)
      h[4] = 1 (h and FixedFlag <> 0)
      h[5:31] — offset in the FixedArena (= h shr FixedArenaOffsetShift)

      Variable chunk header, assuming SizeIndexBits = 4:
      h[0] = used flag (h and UsedFlag <> 0)
      h[1] = last flag (h and LastFlag <> 0)
      h[2] = previous is free flag (h and PrevIsFreeFlag <> 0)
      h[3] = fixed arena flag (h and FixedArenaFlag <> 0)
      h[4] = 0 (h and FixedFlag = 0)
      h[5:31] = size, rounded up to 32 (VarSizeQuant), shr 5; in other words, size = h and VarSizeMask.

      Huge chunk header:
      h[4] = 0 (h and FixedFlag = 0)
      h[0:31] = HugeHeader }

    pCommonHeader = ^CommonHeader;
    CommonHeader = record
      h: uint32;
    end;

    pThreadState = ^ThreadState;

    { Chunk that has been freed. Reuses the now-uninteresting payload, so payload must always fit its size.
      Used for fixed freelists and cross-thread to-free queue. }
    pFreeChunk = ^FreeChunk;
    FreeChunk = record
      next: pFreeChunk;
    end;

    OSChunkBase = object { Shared between OSChunk and HugeChunk. }
      size: SizeUint; { Full size asked from SysOSAlloc. }
    end;

    pOSChunk = ^OSChunk;
    OSChunk = object(OSChunkBase) { Common header for all OS chunks. }
      prev, next: pointer; { pOSChunk, but used for different subtypes. }
    end;

    pFreeOSChunk = ^FreeOSChunk;
    FreeOSChunk = object(OSChunk)
    end;

  {$ifndef HAS_SYSOSFREE}
    FreeOSChunkList = object
      first, last: pFreeOSChunk;
      function Get(minSize, maxSize: SizeUint): pOSChunk;
    end;
  {$endif not HAS_SYSOSFREE}

    pFixedArena = ^FixedArena;
    FixedArena = record
      { Allocated with AllocVar(isArena := true), so has VarHeader to the left.

        Data starts at FixedArenaDataOffset and spans for “maxSize” (virtual value, does not exist directly) bytes, of which:
        — first “formattedSize” are either allocated (“used”; counted in usedSizeMinus1) or in the freelist (firstFreeChunk; size = “formattedSize” - (usedSizeMinus1 + 1)),
        — the rest “maxSize” - “formattedSize” are yet unallocated space.

        This design, together with tracking free chunks per FixedArena rather than per fixed size, trivializes reusing the fixed arenas.
        Chopping all available space at once would get rid of the “unallocated space” entity, but is a lot of potentially wasted work:
        https://gitlab.com/freepascal.org/fpc/source/-/issues/40447.

        Values are multiples of the chunk size instead of counts (could be chunksUsed, chunksFormatted, chunksMax) to save on multiplications.
        Moreover, instead of “maxSize” from the explanation above, almostFullThreshold is used, which is such a value that the chunk is full if usedSizeMinus1 - chunk size >= almostFullThreshold.
        maxSize = RoundUp(almostFullThreshold + chunk size + 1, chunk size).
        Reasons are, calculating almostFullThreshold does not require division, and it is more convenient (in terms of code generation) for AllocFixed / FreeFixed.

        “formattedSize” is a virtual value, too; it equals to usedSizeMinus1 + 1 + <total size of the freelist> and is used only when said freelist is empty, so is in practice int32(usedSizeMinus1) + 1 (see AllocFixed). }

      firstFreeChunk: pFreeChunk;
      usedSizeMinus1, almostFullThreshold: uint32;
      prev, next: pFixedArena;
    end;

    pVarOSChunk = ^VarOSChunk;
    VarOSChunk = object(OSChunk)
    {$ifdef FPC_HAS_FEATURE_THREADING}
      threadState: pThreadState; { Protected with gs.lock. Nil if orphaned. }
    {$endif}
    end;

    pVarHeader = ^VarHeader;
    VarHeader = record
      { Negative offset from the end of this VarHeader to owning VarOSChunk, friendlier to x86 LEA instruction than the more obvious positive variant.
        Truly required only under FPC_HAS_FEATURE_THREADING and could be removed otherwise, bringing the variable header size to the same 4 bytes as fixed headers,
        but this would require some redesign (reintroducing FirstFlag removed in https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/1027
        or some other way to detect the first chunk) and does not matter enough to bother.
        Moreover, accessing VarOSChunk could have been useful beyond multithreading, it just so happens it isn’t. }
      ofsToOs: int32;

      { Assumed to indeed match chunk’s CommonHeader, i.e. that there is no padding after this field.
        Otherwise must be accessed as pCommonHeader(pointer(varHdr) + (VarHeaderSize - CommonHeaderSize))^ :D. }
      ch: CommonHeader;
    end;

    { Reuses the payload of variable chunks whose ch.h and UsedFlag = 0, so variable chunk payload must always fit its size. }
    pFreeVarChunk = ^FreeVarChunk;
    FreeVarChunk = record
      prev, next: pFreeVarChunk;
      binIndex: uint32;
    end;

    { Placed at the end of the free variable chunks that have occupied chunks to the right, thus immediately to the left of such an occupied chunk. }
    pFreeVarTail = ^FreeVarTail;
    FreeVarTail = record
      size: uint32;
    end;

    pHugeChunk = ^HugeChunk;
    HugeChunk = object(OSChunkBase)
    end;

  {$ifdef HEAP_INC_USE_SETS}
    Set32 = set of 0 .. 31;
  {$endif HEAP_INC_USE_SETS}

    VarFreeMap = object
      { Two-level bitfield that allows to search for minimal-size fits (up to the quantization) using up to two “Bsf”s.
        Bit 1 in L1 means that the corresponding cell of L0 is non-0.
        Bit 1 in L0 means that the corresponding cell of bins is non-nil. }
      L1: uint32;
      L0: array[0 .. (VarSizesCount + L0BinSize - 1) div L0BinSize - 1] of uint32;
      bins: array[0 .. VarSizesCount - 1] of pFreeVarChunk;
      { As an optimization, Add.binIndex can also be a size (will be rounded down), assuming VarSizesCount <= MinSearchableVarHeaderAndPayload. }
    {$if VarSizesCount > MinSearchableVarHeaderAndPayload} {$error assumption above does not hold} {$endif}
      procedure Add(c: pFreeVarChunk; binIndex: SizeUint);
      procedure Remove(c: pFreeVarChunk);
      function Find(binIndex: SizeUint): pFreeVarChunk;
      function FindSmaller(binIndex: SizeUint): pFreeVarChunk;
    end;

    ThreadState = object
      emptyArenas: pFixedArena; { Empty fixed arenas to be reused instead of slower AllocVar. Singly linked list, “prev”s are garbage. }
      nEmptyArenas: SizeUint; { # of items in emptyArenas. }
    {$ifdef HAS_SYSOSFREE}
      freeOS1: pFreeOSChunk; { Just one cached empty OS chunk so that borderline (free + alloc) × N scenarios don’t lead to N OS allocations. }
    {$else HAS_SYSOSFREE}
      freeOS: FreeOSChunkList; { Completely empty OS chunks. }
    {$endif HAS_SYSOSFREE}
    {$ifdef FPC_HAS_FEATURE_THREADING}
      toFree: pFreeChunk; { Free requests from other threads, atomic. }
    {$endif}

      used, maxUsed, allocated, maxAllocated: SizeUint; { “maxUsed” and “maxAllocated” include gs.hugeUsed; “used” and “allocated” don’t. }

      varOS: pVarOSChunk;

      { Fixed arenas with at least 1 free chunk (including unformatted space), but not completely empty.
        Fixed arenas that become completely empty are moved to emptyArenas, completely full are... not present in any list. }
      partialArenas: array[0 .. FixedSizesCount - 1] of pFixedArena;

      { Only to calculate preferable new fixed arena sizes...
        (Updated infrequently, as opposed to possible “usedPerArena”. When a new arena is required, all existing arenas of its size are full.) }
      allocatedByFullArenas: array[0 .. FixedSizesCount - 1] of uint32; { SizeUint is not obligatory, overflow is tolerable. }

      varFree: VarFreeMap;

    {$if defined(SUPPORT_INIT_HEAP_PROCESS_WIDE) and defined(HAS_SYSOSFREE)}
      prev, next: pThreadState; { For gs.threads. }
    {$endif SUPPORT_INIT_HEAP_PROCESS_WIDE and HAS_SYSOSFREE}

    {$ifdef DEBUG_HEAP_INC}
      procedure Dump(var f: text);
    {$endif}

      function ChooseFixedArenaSize(sizeIndex: SizeUint): SizeUint;
      function AllocFixed(size: SizeUint): pointer; {$ifndef DEBUG_HEAP_INC} inline; {$endif}
      function FreeFixed(p: pointer): SizeUint; {$ifndef DEBUG_HEAP_INC} inline; {$endif}
      procedure FreeEmptyArenas;
    {$ifdef HAS_SYSOSFREE}
      procedure ReplaceFreeOS1(&with: pFreeOSChunk); inline;
    {$endif HAS_SYSOSFREE}

      function GetOSChunk(minSize, maxSize: SizeUint): pOSChunk; {$if defined(HAS_SYSOSFREE) or not defined(FPC_HAS_FEATURE_THREADING)} inline; {$endif}
      function AllocateOSChunk(minSize, maxSize: SizeUint): pOSChunk;

      function AllocVar(size: SizeUint; isArena: boolean): pointer;
      function FreeVar(p: pointer): SizeUint;
      function TryResizeVar(p: pointer; size: SizeUint): pointer;

      class function AddToHugeUsed(delta: SizeInt): SizeUint; static;
      function AllocHuge(size: SizeUint): pointer;
      function FreeHuge(p: pointer): SizeUint;
      function TryResizeHuge(p: pointer; size: SizeUint): pointer;
      procedure UpdateMaxStats(hugeUsed: SizeUint);

    {$ifdef FPC_HAS_FEATURE_THREADING}
      procedure PushToFree(p: pFreeChunk);
      procedure FlushToFree;
      class procedure FreeToFreeList(tf: pFreeChunk); static;

      procedure Orphan;
      procedure AdoptArena(arena: pFixedArena);
      procedure AdoptVarOwner(p: pointer); { Adopts the OS chunk that contains p. Must be performed under gs.lock. }

    {$ifndef FPC_SECTION_THREADVARS}
      procedure FixupSelfPtr;
    {$endif ndef FPC_SECTION_THREADVARS}
    {$endif FPC_HAS_FEATURE_THREADING}
    end;

    GlobalState = object
    const
      LockInitializedProcessWide = -1; { Special lockUse value to support DLL_PROCESS_ATTACH / DLL_PROCESS_DETACH-like mechanism. }
    var
      hugeUsed: SizeUint; { Same as non-existing “hugeAllocated” as huge chunks don’t have free space.
                            Atomic, but can be read unprotected if unreliability is tolerable.
                            Huge chunks don’t have thread affinity, so are tracked here. Presently, this value is added to all memory statistics.
                            Not a good idea and makes multithreaded statistics a strange and unreliable mix, but alternatives are even worse. }

    {$ifdef FPC_HAS_FEATURE_THREADING}
      lock: TRTLCriticalSection;
      lockUse: int32;
    {$ifdef SUPPORT_INIT_HEAP_PROCESS_WIDE}
      askedForProcessWideLockInitialization: boolean;
    {$endif SUPPORT_INIT_HEAP_PROCESS_WIDE}

      { Like ThreadState.varFree but over orphaned OS chunks. Protected by gs.lock. }
      varFree: VarFreeMap;
    {$if not defined(HAS_SYSOSFREE)}
      freeOS: FreeOSChunkList;
    {$elseif defined(SUPPORT_INIT_HEAP_PROCESS_WIDE)}
      threads: pThreadState;
    {$endif not HAS_SYSOSFREE | SUPPORT_INIT_HEAP_PROCESS_WIDE}
    {$endif FPC_HAS_FEATURE_THREADING}
    end;

    class function AllocFailed: pointer; static;

  class var
    gs: GlobalState;
{$ifdef FPC_HAS_FEATURE_THREADING}
  class threadvar
{$endif FPC_HAS_FEATURE_THREADING}
    thisTs: ThreadState;

  const
    CommonHeaderSize = sizeof(CommonHeader);
  {$if MinFixedHeaderAndPayload < CommonHeaderSize + sizeof(FreeChunk)} {$error MinFixedHeaderAndPayload does not fit CommonHeader + FreeChunk.} {$endif}
    FixedArenaDataOffset = (sizeof(FixedArena) + CommonHeaderSize + Alignment - 1) and -Alignment - CommonHeaderSize;
    VarHeaderSize = sizeof(VarHeader);
    FreeVarTailSize = sizeof(FreeVarTail);
    VarOSChunkDataOffset = (sizeof(VarOSChunk) + VarHeaderSize + Alignment - 1) and -Alignment - VarHeaderSize;
    HugeChunkDataOffset = (sizeof(HugeChunk) + CommonHeaderSize + Alignment - 1) and -Alignment - CommonHeaderSize;
    MinAnyVarHeaderAndPayload = (sizeof(VarHeader) + sizeof(FreeVarChunk) + sizeof(FreeVarTail) + VarSizeQuant - 1) and -VarSizeQuant;
  end;

  class function HeapInc.SizeMinus1ToIndex(sizeMinus1: SizeUint): SizeUint;
  begin
    result := SizeMinus1Div16ToIndex[sizeMinus1 div 16];
  end;

  class function HeapInc.IndexToSize(sizeIndex: SizeUint): SizeUint;
  begin
    result := FixedSizes[sizeIndex];
  end;

  class function HeapInc.VarSizeToBinIndex(size: SizeUint; roundUp: boolean): SizeUint;
  var
    binClassIndex: SizeUint;
  begin
    { Honestly can’t explain the code, it’s made from and equivalent to https://gitlab.com/freepascal.org/fpc/source/-/blob/6ed0a74f54327e07f33e66ddcc4fb5eb8d808dd8/rtl/inc/heap.inc#L503.
      Basic idea is that size class breakpoints of the form %11..1100_0000_0000 are converted to %11..11 for simple BSR by adding %11_1111_1111 = 1 shl FirstVarRangeP2 - 1,
      but the rest, in particular applying roundUp, is magic that is somehow equivalent to the original but uses 1/3 of its instructions. }
    inc(size, 1 shl FirstVarRangeP2 - MaxFixedHeaderAndPayload - ord(roundUp));
    binClassIndex := BsrDWord(uint32(size) or 1); { Off by +FirstVarRangeP2. “or 1” is not required logically, just triggers node_not_zero optimization. }
    result := binClassIndex * VarSizesPerClass + size shr (binClassIndex - (FirstVarRangeP2 - FirstVarStepP2)) - (FirstVarRangeP2 * VarSizesPerClass + 1 shl (FirstVarRangeP2 - FirstVarStepP2) + ord(not roundUp));
  end;

  class function HeapInc.BinIndexToVarSize(binIndex: SizeUint): SizeUint;
  begin
    { Same. }
    result := (1 shl (FirstVarRangeP2 - FirstVarStepP2) + 1 + binIndex mod VarSizesPerClass) shl (binIndex div VarSizesPerClass + FirstVarStepP2) - (1 shl FirstVarRangeP2 - MaxFixedHeaderAndPayload);
  end;

{$ifndef HAS_SYSOSFREE}
  function HeapInc.FreeOSChunkList.Get(minSize, maxSize: SizeUint): pOSChunk;
  var
    prev, next: pFreeOSChunk;
  begin
    result := first;
    while Assigned(result) and not ((result^.size >= minSize) and (result^.size <= maxSize)) do
      result := result^.next;
    if not Assigned(result) then
      exit;

    prev := result^.prev;
    next := result^.next;
    if Assigned(prev) then
      prev^.next := next
    else
      first := next;
    if Assigned(next) then
      next^.prev := prev
    else
      last := prev;
  end;
{$endif not HAS_SYSOSFREE}

  procedure HeapInc.VarFreeMap.Add(c: pFreeVarChunk; binIndex: SizeUint);
  var
    next: pFreeVarChunk;
    iL0: SizeUint;
    vL0 {$ifdef HEAP_INC_USE_SETS}, vL1 {$endif}: uint32;
  begin
    if binIndex >= VarSizesCount then
      if binIndex >= MaxVarHeaderAndPayload then { Large sizes go to the last bin, assuming searches never search for more than MaxVarHeaderAndPayload. }
        binIndex := VarSizesCount - 1
      else
        binIndex := VarSizeToBinIndex(binIndex, false);

    next := bins[binIndex];
    c^.prev := nil;
    c^.next := next;
    c^.binIndex := binIndex;
    bins[binIndex] := c;
    if Assigned(next) then
      next^.prev := c
    else
    begin
      iL0 := binIndex div L0BinSize;
      vL0 := L0[iL0];
    {$ifdef HEAP_INC_USE_SETS}
      if vL0 = 0 then
      begin
        vL1 := L1;
        Include(Set32(vL1), iL0);
        L1 := vL1;
      end;
      Include(Set32(vL0), binIndex mod L0BinSize);
      L0[iL0] := vL0;
    {$else}
      if vL0 = 0 then
        L1 := L1 or uint32(1) shl iL0;
      L0[iL0] := vL0 or uint32(1) shl (binIndex mod L0BinSize);
    {$endif}
    end;
  end;

  procedure HeapInc.VarFreeMap.Remove(c: pFreeVarChunk);
  var
    prev, next: pFreeVarChunk;
    binIndex, iL0: SizeUint;
    v: uint32;
  begin
    prev := c^.prev;
    next := c^.next;
    if Assigned(next) then
      next^.prev := prev;
    if Assigned(prev) then
      prev^.next := next
    else
    begin
      binIndex := c^.binIndex;
      bins[binIndex] := next;
      if not Assigned(next) then
      begin
        iL0 := binIndex div L0BinSize;
      {$ifdef HEAP_INC_USE_SETS}
        v := L0[iL0];
        Exclude(Set32(v), binIndex mod L0BinSize);
        L0[iL0] := v;
        if v = 0 then
        begin
          v := L1;
          Exclude(Set32(v), iL0);
          L1 := v;
        end;
      {$else}
        v := L0[iL0] xor (uint32(1) shl (binIndex mod L0BinSize));
        L0[iL0] := v;
        if v = 0 then
          L1 := L1 xor (uint32(1) shl iL0);
      {$endif}
      end;
    end;
  end;

  function HeapInc.VarFreeMap.Find(binIndex: SizeUint): pFreeVarChunk;
  var
    mask: uint32;
  begin
    result := bins[binIndex];
    if Assigned(result) then
      exit;
    mask := L0[binIndex div L0BinSize] shr (binIndex mod L0BinSize); { Logically should be “1 + binIndex mod L0BinSize” but the bit that represents the binIndex-th bin is 0 anyway. }
    if mask <> 0 then
      exit(bins[binIndex + BsfDWord(mask or 1 shl (L0BinSize - 1))]); { In these two BsfDWords, ensuring the highest bit to be set is just an optimization, as long as the supposed alternative from https://gitlab.com/freepascal.org/fpc/source/-/issues/41179 does not work. }
    mask := L1 and (SizeUint(-2) shl (binIndex div L0BinSize));
    if mask <> 0 then
    begin
      binIndex := BsfDWord(mask or 1 shl (L0BinSize - 1)); { Index at L0. }
      result := bins[binIndex * L0BinSize + BsfDWord(L0[binIndex] or 1 shl (L0BinSize - 1))]; { Careful, this time “or 1 shl (L0BinSize - 1)” is NOT an optimization: unsynchronized gs.varFree.Find can read zero from L0[binIndex]. }
    end;
  end;

  function HeapInc.VarFreeMap.FindSmaller(binIndex: SizeUint): pFreeVarChunk;
  var
    mask: uint32;
  begin
    mask := L0[binIndex div L0BinSize] and (uint32(1) shl (binIndex mod L0BinSize) - 1);
    if mask <> 0 then
      exit(bins[binIndex and SizeUint(-L0BinSize) + BsrDWord(mask or 1)]);
    result := nil;
    mask := L1 and (uint32(1) shl (binIndex div L0BinSize) - 1);
    if mask <> 0 then
    begin
      binIndex := BsrDWord(mask or 1);
      result := bins[binIndex * L0BinSize + BsrDWord(L0[binIndex] or 1)];
    end;
  end;

{$ifdef DEBUG_HEAP_INC}
  procedure HeapInc.ThreadState.Dump(var f: text);
  var
    i: SizeInt;
    fix: pFixedArena;
    fr: pFreeOSChunk;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    tf: pFreeChunk;
  {$endif}
    vf: pFreeVarChunk;
    vOs: pVarOSChunk;
    p: pointer;
    needLE, anything: boolean;

    procedure MaybeLE;
    begin
      if needLE then
        writeln(f);
      needLE := false;
    end;

    procedure DumpVarFree(const varFree: VarFreeMap; const name: shortstring);
    var
      i: SizeInt;
    begin
      if varFree.L1 = 0 then
        exit;
      MaybeLE;
      write(f, name, LineEnding, 'L1:');
      for i := 0 to VarSizesCount div L0BinSize - 1 do
        if varFree.L1 shr i and 1 <> 0 then
        begin
          write(f, ' #', i, ' ', BinIndexToVarSize(i * L0BinSize), '-');
          if i = VarSizesCount div L0BinSize - 1 then
            write(f, 'inf')
          else
            write(f, BinIndexToVarSize((i + 1) * L0BinSize) - 1);
        end;
      writeln(f);
      write(f, 'L0 (bins):');
      for i := 0 to VarSizesCount - 1 do
      begin
        if varFree.L0[SizeUint(i) div L0BinSize] shr (SizeUint(i) mod L0BinSize) and 1 <> 0 then
        begin
          write(f, ' #', i, ' ', BinIndexToVarSize(i), '-');
          if i = VarSizesCount - 1 then
            write(f, 'inf')
          else
            write(f, BinIndexToVarSize(i + 1) - 1);
        end;
        if Assigned(varFree.bins[i]) then
        begin
          write(f, ' (');
          vf := varFree.bins[i];
          repeat
            if Assigned(vf^.prev) then write(f, ' ');
            write(f, HexStr(PtrUint(vf), 1 + BsrQWord(PtrUint(vf)) div 4), ':', pVarHeader(vf)[-1].ch.h and VarSizeMask);
            vf := vf^.next;
          until not Assigned(vf);
          write(f, ')');
        end;
      end;
      writeln(f);
      needLE := true;
    end;

  begin
    writeln(f, 'used = ', used, ', allocated = ', allocated, ', hugeUsed = ', gs.hugeUsed, ', maxUsed = ', maxUsed, ', maxAllocated = ', maxAllocated);
    needLE := true;
    anything := false;
    for i := 0 to FixedSizesCount - 1 do
    begin
      if not Assigned(partialArenas[i]) and (allocatedByFullArenas[i] = 0) then
        continue;
      MaybeLE;
      anything := true;
      write(f, 'Size #', i, ' (', IndexToSize(i), '):');
      if allocatedByFullArenas[i] <> 0 then
        write(f, ' allocatedByFullArenas = ', allocatedByFullArenas[i]);
      if Assigned(partialArenas[i]) then
      begin
        writeln(f);
        fix := partialArenas[i];
        repeat
          writeln(f, 'arena size = ', pVarHeader(fix)[-1].ch.h and VarSizeMask - VarHeaderSize - FixedArenaDataOffset, ', usedSizeMinus1 = ', fix^.usedSizeMinus1, ', almostFullThreshold = ', fix^.almostFullThreshold);
          fix := fix^.next;
        until not Assigned(fix);
      end
      else if allocatedByFullArenas[i] <> 0 then
        writeln(f);
    end;
    needLE := needLE or anything;
    if nEmptyArenas <> 0 then
    begin
      MaybeLE;
      writeln(f, 'nEmptyArenas = ', nEmptyArenas);
      needLE := true;
    end;
    vOs := varOS;
    while Assigned(vOs) do
    begin
      MaybeLE;
      writeln(f, 'Var OS chunk, size ', vOs^.size);
      p := pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize);
      repeat
        write(f, HexStr(p), ': size = ', pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask, ', ofsToOs = ', pVarHeader(p - VarHeaderSize)^.ofsToOs);
        if pVarHeader(p - VarHeaderSize)^.ch.h and UsedFlag <> 0 then
          write(f, ', used')
        else
        begin
          write(f, ', f r e e');
          if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag = 0 then
            write(f, ' (tail ', pFreeVarTail(p + pVarHeader(p - VarHeaderSize)^.ch.h - VarHeaderSize - FreeVarTailSize)^.size, ')');
        end;
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          write(f, ', last');
        if pVarHeader(p - VarHeaderSize)^.ch.h and PrevIsFreeFlag <> 0 then
          write(f, ', prev. is free');
        if pVarHeader(p - VarHeaderSize)^.ch.h and FixedArenaFlag <> 0 then
          write(f, ', fixed arena');
        writeln(f);
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          break;
        if pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask = 0 then
        begin
          writeln(f,'ERROR: block size is zero, cannot find the next block!');
          break;
        end;
        p := p + pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask;
        if p > ((pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize)) + vOs^.size) then
        begin
          writeln(f,'ERROR: previous tail points to ',HexStr(p),' which is outside of this OS chunk!');
          break;
        end;
      until false;
      needLE := true;
      vOs := vOs^.next;
    end;
    fr := {$ifdef HAS_SYSOSFREE} freeOS1 {$else} freeOS.first {$endif};
    if Assigned(fr) then
    begin
      MaybeLE;
      repeat
        writeln(f, 'Free OS: ', HexStr(fr), ', size = ', fr^.size);
      {$ifndef HAS_SYSOSFREE} fr := fr^.next; {$endif}
      until {$ifdef HAS_SYSOSFREE} true {$else} not Assigned(fr) {$endif};
      needLE := true;
    end;
    DumpVarFree(varFree, 'varFree');
  {$ifdef FPC_HAS_FEATURE_THREADING}
    DumpVarFree(gs.varFree, 'Orphaned varFree');
    tf := toFree;
    if Assigned(tf) then
    begin
      MaybeLE;
      write(f, 'To-free:');
      repeat
        if pCommonHeader(pointer(tf) - CommonHeaderSize)^.h and FixedFlag <> 0 then
          write(f, ' f ', CommonHeaderSize + SysMemSize(tf))
        else
          write(f, ' v ', VarHeaderSize + SysMemSize(tf));
        tf := tf^.next;
      until not Assigned(tf);
      writeln(f);
    end;
  {$endif FPC_HAS_FEATURE_THREADING}
  end;
{$endif DEBUG_HEAP_INC}

  function HeapInc.ThreadState.ChooseFixedArenaSize(sizeIndex: SizeUint): SizeUint;
  begin
    result := (allocatedByFullArenas[sizeIndex] div 8 + (FixedArenaSizeQuant - 1)) and SizeUint(-FixedArenaSizeQuant); { 12.5% of memory allocated by the size. }
    if result < MinFixedArenaSize then
      result := MinFixedArenaSize;
    if result > MaxFixedArenaSize then
      result := MaxFixedArenaSize;
    dec(result, result shr (FirstVarRangeP2 - FirstVarStepP2)); { Prettier fit into OS chunks. }
  end;

  function HeapInc.ThreadState.AllocFixed(size: SizeUint): pointer;
  var
    sizeIndex, sizeUp, statv: SizeUint;
    usedSizeMinus1: int32;
    arena, nextArena: pFixedArena;
  begin
    sizeIndex := SizeMinus1ToIndex(size + (CommonHeaderSize - 1));

    arena := partialArenas[sizeIndex];
    if not Assigned(arena) then
    begin
    {$ifdef FPC_HAS_FEATURE_THREADING}
      if Assigned(toFree) then
      begin
        FlushToFree;
        arena := partialArenas[sizeIndex];
      end;
      if not Assigned(arena) then
    {$endif FPC_HAS_FEATURE_THREADING}
      begin
        arena := emptyArenas;
        if Assigned(arena) then
        begin
          emptyArenas := arena^.next;
          dec(nEmptyArenas);
        end else
        begin
          arena := AllocVar(ChooseFixedArenaSize(sizeIndex), true);
          if not Assigned(arena) then
            exit(nil);
          { Size index of the first chunk in the arena is used to determine if it can be reused. Set a purposely mismatching value for freshly allocated arena. }
          pCommonHeader(pointer(arena) + FixedArenaDataOffset)^.h := uint32(not sizeIndex);
        end;
        if pCommonHeader(pointer(arena) + FixedArenaDataOffset)^.h and SizeIndexMask = uint32(sizeIndex) then
          { Lucky! Just don’t reset the chunk and use its old freelist. }
        else
        begin
          arena^.firstFreeChunk := nil;
          arena^.usedSizeMinus1 := uint32(-1);
          arena^.almostFullThreshold := pVarHeader(arena)[-1].ch.h and VarSizeMask - 2 * IndexToSize(sizeIndex) - (VarHeaderSize + FixedArenaDataOffset); { available space - 2 * chunk size. }
        end;

        { Add arena to partialArenas[sizeIndex], which is nil. Careful: AllocVar above should not call FlushToFree, or this assumption might be violated. }
        arena^.prev := nil;
        arena^.next := nil;
        partialArenas[sizeIndex] := arena;
      end;
    end;

    sizeUp := IndexToSize(sizeIndex); { Not reusing the “size” variable saved a register at the time of writing this comment. }
    inc(used, sizeUp);

    { arena from partialArenas has either free chunk or free unformatted space for a new chunk. }
    usedSizeMinus1 := int32(arena^.usedSizeMinus1);
    arena^.usedSizeMinus1 := uint32(usedSizeMinus1 + int32(sizeUp));
    result := arena^.firstFreeChunk;
    if Assigned(result) then
    begin
      { This branch is much more likely (when compiling FPC: 9×), so comes first. }
      arena^.firstFreeChunk := pFreeChunk(result)^.next;
      if usedSizeMinus1 < int32(arena^.almostFullThreshold) then { Arena is still not full? Uses usedSizeMinus1 value before adding sizeUp, as assumed by almostFullThreshold. }
        exit;
    end else
    begin
      { Freelist is empty, so “formattedSize” = usedSizeMinus1 + 1. This “+ 1” is folded into constants. }
      result := pointer(arena) + (FixedArenaDataOffset + CommonHeaderSize + 1) + usedSizeMinus1;
      pCommonHeader(result - CommonHeadersize)^.h := uint32(int32(sizeIndex) + int32(usedSizeMinus1 shl FixedArenaOffsetShift) +
        (FixedFlag + (FixedArenaDataOffset + CommonHeaderSize + 1) shl FixedArenaOffsetShift) { ← const });
      if usedSizeMinus1 < int32(arena^.almostFullThreshold) then { Arena is still not full? }
        exit;
    end;

    { Arena became full. This is unlikely, so instead of the “if”, the check is duplicated in both branches above. (Saves a jump from the “then” branch above.) }
    inc(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h and VarSizeMask);
    { Remove arena from partialArenas[sizeIndex]. (It was first.) }
    nextArena := arena^.next;
    partialArenas[sizeIndex] := nextArena;
    if Assigned(nextArena) then
      nextArena^.prev := nil;
    { And since this is unlikely, it won’t hurt to update maxUsed (unlike doing it in the common path). }
    statv := used + gs.hugeUsed;
    if statv > maxUsed then
      maxUsed := statv;
  end;

  function HeapInc.ThreadState.FreeFixed(p: pointer): SizeUint;
  var
    sizeIndex: SizeUint;
    usedSizeMinus1: int32;
    arena, prevArena, nextArena: pFixedArena;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    ts: pThreadState;
  {$endif FPC_HAS_FEATURE_THREADING}
  begin
    arena := p - pCommonHeader(p - CommonHeaderSize)^.h shr FixedArenaOffsetShift;

  {$ifdef FPC_HAS_FEATURE_THREADING}
    { This can be checked without blocking; <arena>.threadState can only change from one value not equal to @self to another value not equal to @self. }
    if pVarOSChunk(pointer(arena) + pVarHeader(arena)[-1].ofsToOs)^.threadState <> @self then
    begin
      EnterCriticalSection(gs.lock);
      ts := pVarOSChunk(pointer(arena) + pVarHeader(arena)[-1].ofsToOs)^.threadState;
      if Assigned(ts) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy its threadState.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := IndexToSize(pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask) - CommonHeaderSize;
        ts^.PushToFree(p);
        LeaveCriticalSection(gs.lock);
        exit;
      end;
      AdoptVarOwner(arena); { ...And continue! }
      LeaveCriticalSection(gs.lock);
    end;
  {$endif FPC_HAS_FEATURE_THREADING}

    pFreeChunk(p)^.next := arena^.firstFreeChunk;
    arena^.firstFreeChunk := p;
    sizeIndex := pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask;
    result := IndexToSize(sizeIndex);
    dec(used, result);

    usedSizeMinus1 := int32(arena^.usedSizeMinus1) - int32(result);
    arena^.usedSizeMinus1 := uint32(usedSizeMinus1);
    dec(result, CommonHeaderSize);

    { “(usedSizeMinus1 = -1) or (usedSizeMinus1 >= arena^.almostFullThreshold)” as 1 comparison. }
    if uint32(usedSizeMinus1) >= arena^.almostFullThreshold then
      if usedSizeMinus1 <> -1 then
      begin
        dec(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h and VarSizeMask);
        { Add arena to partialArenas[sizeIndex]. }
        nextArena := partialArenas[sizeIndex];
        arena^.prev := nil;
        arena^.next := nextArena;
        if Assigned(nextArena) then
          nextArena^.prev := arena;
        partialArenas[sizeIndex] := arena;
      end else
      begin
        { Remove arena from partialArenas[sizeIndex], add to emptyArenas (maybe). }
        prevArena := arena^.prev;
        nextArena := arena^.next;
        if Assigned(prevArena) then
          prevArena^.next := nextArena
        else
          partialArenas[sizeIndex] := nextArena;
        if Assigned(nextArena) then
          nextArena^.prev := prevArena;

        if nEmptyArenas < MaxKeptFixedArenas then
        begin
          arena^.next := emptyArenas;
          emptyArenas := arena;
          inc(nEmptyArenas);
        end else
          FreeVar(arena);
      end;
  end;

  procedure HeapInc.ThreadState.FreeEmptyArenas;
  var
    arena: pFixedArena;
  begin
    while nEmptyArenas > 0 do
    begin
      arena := emptyArenas;
      emptyArenas := arena^.next;
      dec(nEmptyArenas);
      FreeVar(arena);
    end;
  end;

{$ifdef HAS_SYSOSFREE}
  procedure HeapInc.ThreadState.ReplaceFreeOS1(&with: pFreeOSChunk);
  begin
    if Assigned(freeOS1) then
    begin
      dec(allocated, freeOS1^.size);
      SysOSFree(freeOS1, freeOS1^.size);
    end;
    freeOS1 := &with;
  end;
{$endif HAS_SYSOSFREE}

  function HeapInc.ThreadState.GetOSChunk(minSize, maxSize: SizeUint): pOSChunk;
{$if defined(FPC_HAS_FEATURE_THREADING) and not defined(HAS_SYSOSFREE)}
  var
    statv: SizeUint;
{$endif FPC_HAS_FEATURE_THREADING and not HAS_SYSOSFREE}
  begin
  {$ifdef HAS_SYSOSFREE}
    result := freeOS1;
    if Assigned(result) then
      if (result^.size >= minSize) and (result^.size <= maxSize) then
        freeOS1 := nil
      else
        result := nil;
  {$else HAS_SYSOSFREE}
    result := freeOS.Get(minSize, maxSize);
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if not Assigned(result) and Assigned(gs.freeOS.first) then { Racing precheck. }
    begin
      EnterCriticalSection(gs.lock);
      result := gs.freeOS.Get(minSize, maxSize);
      LeaveCriticalSection(gs.lock);
      if Assigned(result) then
      begin
        statv := allocated + result^.size;
        allocated := statv;
        inc(statv, gs.hugeUsed);
        if statv > maxAllocated then
          maxAllocated := statv;
      end;
    end;
  {$endif FPC_HAS_FEATURE_THREADING}
  {$endif HAS_SYSOSFREE}
  end;

  function HeapInc.ThreadState.AllocateOSChunk(minSize, maxSize: SizeUint): pOSChunk;
  var
    query, statv: SizeUint;
  begin
    query := used div 16 + minSize div 2; { Base: 6.25% of the memory used, so if GrowHeapSize2 = 1 Mb, 1 Mb OS allocations start at 16 Mb used. }
    if query > maxSize then { Limit by maxSize (usually GrowHeapSize2). }
      query := maxSize;
    if query < minSize then { But of course allocate at least the amount requested. Also triggers if maxSize was wrong (smaller than minSize). }
      query := minSize;
    query := (query + (OSChunkVarSizeQuant - 1)) and SizeUint(-OSChunkVarSizeQuant); { Quantize. }
    result := SysOSAlloc(query);
    if not Assigned(result) and (query > minSize) then
    begin
      query := minSize;
      result := SysOSAlloc(query);
    end;
    if not Assigned(result) then
      exit(AllocFailed);
    result^.size := query;
    statv := allocated + query;
    allocated := statv;
    inc(statv, gs.hugeUsed);
    if statv > maxAllocated then
      maxAllocated := statv;
  end;

  function HeapInc.ThreadState.AllocVar(size: SizeUint; isArena: boolean): pointer;
  var
    fv: pFreeVarChunk absolute result;
    tailFv: pFreeVarChunk;
    osChunk, osNext: pVarOSChunk;
    binIndex, vSizeFlags, statv: SizeUint;
  begin
    { Search varFree for (roughly) smallest chunk ≥ size. }
    binIndex := VarSizeToBinIndex(size + VarHeaderSize, true);
    { Round the size up to the bin size.
      Can do without that, but not doing that will often mean the inability to reuse the hole for the same size because varFree rounds up for searches and down for additions. }
    size := BinIndexToVarSize(binIndex);
    repeat { break = found fv or osChunk. }
      fv := varFree.Find(binIndex);
      if Assigned(fv) then
        break;

      { If allocating arena, try to allocate less than requested, within arena size limitations. }
      if isArena and (binIndex > MinArenaBinIndex) then
      begin
        fv := varFree.FindSmaller(binIndex);
        if Assigned(fv) and (fv^.binIndex >= MinArenaBinIndex) then
        begin
          size := pVarHeader(fv)[-1].ch.h and VarSizeMask; { Use the entire chunk. }
          break;
        end;
        fv := nil;
      end;

      { Try reusing empty OS chunk. }
      osChunk := pVarOSChunk(GetOSChunk(VarOSChunkDataOffset + size, GrowHeapSize2));
      if Assigned(osChunk) then
        break;

      { If there are empty arenas, free them and retry. }
      if nEmptyArenas > 0 then
      begin
        FreeEmptyArenas;
        continue;
      end;

    {$ifdef FPC_HAS_FEATURE_THREADING}
      { Try reusing an orphaned chunk. }
      fv := gs.varFree.Find(binIndex); { Preliminary search without blocking, assuming varFree.Find doesn’t do anything that can go wrong. }
      if Assigned(fv) then
      begin
        EnterCriticalSection(gs.lock);
        fv := gs.varFree.Find(binIndex); { True search. }
        if Assigned(fv) then
          AdoptVarOwner(fv); { Moves fv to own varFree. }
        LeaveCriticalSection(gs.lock);
        if Assigned(fv) then
          break;
      end;
    {$endif FPC_HAS_FEATURE_THREADING}

      osChunk := pVarOSChunk(AllocateOSChunk(VarOSChunkDataOffset + size, GrowHeapSize2));
      if Assigned(osChunk) then
        break;
      exit; { (nil) as fv is nil and mapped to result. }
    until false;
    if not Assigned(fv) then
    begin
    {$ifdef FPC_HAS_FEATURE_THREADING}
      osChunk^.threadState := @self;
    {$endif}

      { Add osChunk to varOS. }
      osNext := varOS;
      osChunk^.prev := nil;
      osChunk^.next := osNext;
      if Assigned(osNext) then
        osNext^.prev := osChunk;
      varOS := osChunk;

      { Format new free var chunk spanning the entire osChunk. FreeVarTail is not required. }
      fv := pointer(osChunk) + (VarOSChunkDataOffset + VarHeaderSize);
      pVarHeader(result - VarHeaderSize)^.ofsToOs := -(VarOSChunkDataOffset + VarHeaderSize);
      pVarHeader(result - VarHeaderSize)^.ch.h := (uint32(osChunk^.size) - VarOSChunkDataOffset) and VarSizeMask + LastFlag;
    end else
      varFree.Remove(fv);

    { Result will be allocated at the beginning of fv; maybe format the remainder and add it back to varFree. }
    vSizeFlags := pVarHeader(fv)[-1].ch.h - size; { Inherits LastFlag. }
    if
      { Allow over-allocating arenas by up to MaxArenaOverallocation.
        Harmless check if not an arena, assuming (MaxArenaOverallocation and -VarSizeQuant + VarSizeQuant) >= MinSearchableVarHeaderAndPayload. }
      (vSizeFlags >= MaxArenaOverallocation and -VarSizeQuant + VarSizeQuant) or
      { Allow leaving a non-searchable tail if non-last.
        “vSizeFlags >= MinAnyVarHeaderAndPayload” if non-last, “vSizeFlags >= MinSearchableVarHeaderAndPayload” if last. }
      not isArena and (vSizeFlags >= MinAnyVarHeaderAndPayload + (MinSearchableVarHeaderAndPayload - MinAnyVarHeaderAndPayload) div LastFlag * (vSizeFlags and LastFlag)) then
    begin
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) + UsedFlag;
      tailFv := result + size; { fv (result) = allocated block, tailFv = remainder. }
      pVarHeader(pointer(tailFv) - VarHeaderSize)^.ofsToOs := pVarHeader(result - VarHeaderSize)^.ofsToOs - int32(size);
      pVarHeader(pointer(tailFv) - VarHeaderSize)^.ch.h := vSizeFlags;
      { Chunk to the right retains its PrevFreeFlag. }
      if vSizeFlags and LastFlag = 0 then
        pFreeVarTail(pointer(tailFv) + vSizeFlags - (VarHeaderSize + FreeVarTailSize))^.size := vSizeFlags;
      if vSizeFlags >= MinSearchableVarHeaderAndPayload then
        varFree.Add(tailFv, vSizeFlags); { Rounding down, so not masking is ok. }
    end else
    begin
      { Use the entire chunk. }
      inc(vSizeFlags, size);
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(vSizeFlags) + UsedFlag;
      if vSizeFlags and LastFlag = 0 then
        dec(pVarHeader(result + vSizeFlags - VarHeaderSize)^.ch.h, PrevIsFreeFlag);
      size := vSizeFlags and VarSizeMask;
    end;

    if isArena then
      inc(pVarHeader(result)[-1].ch.h, FixedArenaFlag) { Arenas aren’t counted in “used” directly. }
    else
      inc(used, size);

    { Update maxUsed regardless. }
    statv := used + gs.hugeUsed;
    if statv > maxUsed then
      maxUsed := statv;
  end;

  function HeapInc.ThreadState.FreeVar(p: pointer): SizeUint;
  var
    fSizeFlags, hNext, prevSize: SizeUint;
    osChunk, osPrev, osNext: pVarOSChunk;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    pts: ^pThreadState;
  {$endif FPC_HAS_FEATURE_THREADING}
  {$ifndef HAS_SYSOSFREE}
    freeOsNext: pFreeOSChunk;
    fOs: ^FreeOSChunkList;
  {$endif not HAS_SYSOSFREE}
  begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
    pts := @pVarOSChunk(p + pVarHeader(p - VarHeaderSize)^.ofsToOs)^.threadState;
    if pts^ <> @self then
    begin
      EnterCriticalSection(gs.lock);
      if Assigned(pts^) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy its threadState.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask - VarHeaderSize;
        pts^^.PushToFree(p);
        LeaveCriticalSection(gs.lock);
        exit;
      end;
      AdoptVarOwner(p); { ...And continue! }
      LeaveCriticalSection(gs.lock);
    end;
  {$endif FPC_HAS_FEATURE_THREADING}

    fSizeFlags := pVarHeader(p - VarHeaderSize)^.ch.h;
    result := fSizeFlags and VarSizeMask;
    if fSizeFlags and FixedArenaFlag = 0 then
      dec(used, result)
    else
      dec(fSizeFlags, FixedArenaFlag);

    { If next/prev are free, remove them from varFree and merge with f — (f)uture (f)ree chunk that starts at p and has fSizeFlags. }
    if fSizeFlags and LastFlag = 0 then
    begin
      hNext := pVarHeader(p + result - VarHeaderSize)^.ch.h;
      if uint32(hNext) and UsedFlag = 0 then
      begin
        inc(fSizeFlags, hNext); { Inherit LastFlag, other p2 flags must be 0. }
        if hNext >= MinSearchableVarHeaderAndPayload then { Logically “hNext and VarSizeMask”. }
          varFree.Remove(p + result);
        { Chunk to the right retains its PrevFreeFlag. }
      end;
    end;

    if fSizeFlags and PrevIsFreeFlag <> 0 then
    begin
      prevSize := pFreeVarTail(p - (VarHeaderSize + FreeVarTailSize))^.size;
      dec(p, prevSize);
      fSizeFlags := fSizeFlags - PrevIsFreeFlag + prevSize;
      if uint32(prevSize) >= MinSearchableVarHeaderAndPayload then
        varFree.Remove(p);
    end;

    { Turn p into a free chunk and add it back to varFree...
      unless it spans the entire OS chunk, in which case instead move the chunk from varOS to freeOS1 / freeOS. }
    if (fSizeFlags and LastFlag = 0) or (pVarHeader(p - VarHeaderSize)^.ofsToOs <> -(VarOSChunkDataOffset + VarHeaderSize)) then
    begin
      dec(fSizeFlags, UsedFlag);
      pVarHeader(p - VarHeaderSize)^.ch.h := fSizeFlags;
      varFree.Add(p, fSizeFlags);
      if fSizeFlags and LastFlag = 0 then
      begin
        pVarHeader(p + fSizeFlags - VarHeaderSize)^.ch.h := pVarHeader(p + fSizeFlags - VarHeaderSize)^.ch.h or PrevIsFreeFlag; { Could have it already. }
        pFreeVarTail(p + fSizeFlags - (VarHeaderSize + FreeVarTailSize))^.size := fSizeFlags;
      end;
    end else
    begin
      osChunk := p - (VarOSChunkDataOffset + VarHeaderSize);

      { Remove osChunk from varOS. }
      osPrev := osChunk^.prev;
      osNext := osChunk^.next;
      if Assigned(osPrev) then
        osPrev^.next := osNext
      else
        varOS := osNext;
      if Assigned(osNext) then
        osNext^.prev := osPrev;

    {$ifdef HAS_SYSOSFREE}
      ReplaceFreeOS1(pFreeOSChunk(osChunk)); { Move to freeOS1, discarding old freeOS1. }
    {$else HAS_SYSOSFREE}
      fOs := @freeOS;
      { Share if huge. }
    {$ifdef FPC_HAS_FEATURE_THREADING}
      if osChunk^.size > GrowHeapSize2 then
      begin
        fOs := @gs.freeOS;
        EnterCriticalSection(gs.lock);
      end;
    {$endif FPC_HAS_FEATURE_THREADING}
      { Add to fOs. }
      freeOsNext := fOs^.first;
      osChunk^.prev := nil;
      osChunk^.next := freeOsNext;
      if Assigned(freeOsNext) then
        freeOsNext^.prev := osChunk
      else
        fOs^.last := pFreeOSChunk(osChunk);
      fOs^.first := pFreeOSChunk(osChunk);
    {$ifdef FPC_HAS_FEATURE_THREADING}
      if fOs <> @freeOS then
      begin
        dec(allocated, osChunk^.size); { gs.freeOS aren’t counted anywhere, for now. }
        LeaveCriticalSection(gs.lock);
      end;
    {$endif FPC_HAS_FEATURE_THREADING}
    {$endif HAS_SYSOSFREE}
    end;
    dec(result, VarHeaderSize);
  end;

  function HeapInc.ThreadState.TryResizeVar(p: pointer; size: SizeUint): pointer;
  var
    ar: pointer absolute result;
    fv, fp: pointer;
    arSizeFlags, prevSize2, maxFv, minFragment, fSizeFlags, hNext, hNext2, oldph: uint32;
    prevSize, binIndex, oldpsize, statv, moveSize: SizeUint;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    pts: ^pThreadState;
  {$endif FPC_HAS_FEATURE_THREADING}
  begin
    result := nil;
    if (size > GrowHeapSize2) { Assuming GrowHeapSize2 is never larger than 3.999 Gb, this prevents overflow on adding headers and allows uint32(size) to tune for x64. }
      or (uint32(size) <= MaxFixedHeaderAndPayload - CommonHeaderSize)
    then
      exit;

  {$ifdef FPC_HAS_FEATURE_THREADING}
    pts := @pVarOSChunk(p + pVarHeader(p - VarHeaderSize)^.ofsToOs)^.threadState;
    if pts^ <> @self then
    begin
      if Assigned(pts^) then { Pretest to avoid acquiring the lock. }
        exit;
      EnterCriticalSection(gs.lock);
      if Assigned(pts^) then
      begin
        LeaveCriticalSection(gs.lock);
        exit;
      end;
      AdoptVarOwner(p); { ...And continue! }
      LeaveCriticalSection(gs.lock);
    end;
  {$endif FPC_HAS_FEATURE_THREADING}

    { Round the size up, but only if supported by VarSizeToBinIndex: chunks can be reallocated to the sizes larger than MaxVarHeaderAndPayload. }
    if uint32(size) <= MaxVarHeaderAndPayload - VarHeaderSize then
    begin
      binIndex := VarSizeToBinIndex(size + VarHeaderSize, true);
      size := BinIndexToVarSize(binIndex);
    end else
      size := uint32(uint32(size) + (VarHeaderSize + VarSizeQuant - 1)) and uint32(-VarSizeQuant); { Just do the strictly necessary quantization... }

    { ar + arSizeFlags (from “around”) is the chunk made from p and its adjacent free chunks. }
    ar := p;
    arSizeFlags := pVarHeader(ar - VarHeaderSize)^.ch.h;

    if arSizeFlags and LastFlag = 0 then
    begin
      hNext := pVarHeader(ar + arSizeFlags and VarSizeMask - VarHeaderSize)^.ch.h;
      if hNext and UsedFlag = 0 then
        inc(arSizeFlags, hNext); { Inherit LastFlag, other flags must be 0. }
    end;

    if arSizeFlags and PrevIsFreeFlag <> 0 then
    begin
      prevSize := pFreeVarTail(ar - (VarHeaderSize + FreeVarTailSize))^.size;
      dec(ar, prevSize);
      inc(arSizeFlags, prevSize);
    end;

    if uint32(size) > arSizeFlags then { “ar” has no way to fit the new chunk. }
      exit(nil);

    { Check if there is a better place... }
    maxFv := arSizeFlags div 4 * 3;
    if (uint32(size) <= MaxVarHeaderAndPayload) and (uint32(size) < maxFv) then { Pretest the condition on a “CONSIDERABLY” better fv below, maybe it’s not going to happen no matter what. }
    begin
      fv := varFree.Find(binIndex);
      if Assigned(fv)
        { fv may be one of the chunks around; in this case, ignore it. Checked as unsigned(fv - ar) < arSize. }
        and (PtrUint(PtrInt(PtrUint(fv)) - PtrInt(PtrUint(ar))) >= arSizeFlags) { Logically “arSizeFlags and VarSizeMask”. }
        { To justify moving FAR, better place should be CONSIDERABLY better: say, <75% of the ar. }
        and (pVarHeader(fv)[-1].ch.h < maxFv) { Ignore masking, this is a rough check anyway. }
      then
        exit(nil);
    end;

    { So p will be placed inside “ar” after all. It is either moved to the beginning of “ar” or stays in place.

      There might be no choice but to move: reallocating A in
      [free 1,000][A 1,000][free 1,000]
      to 2,500 bytes has to move, resulting in
      [A 2,500][free 500].

      But if there is a choice, moving might be or not be worth it. If we have
      [free 5,000][A 1,000][free 5,000]
      then moving will give
      [A 1,000][free 10,000]
      and that’s the point — [free 10,000] is better than 2 × [free 5,000]. But if we have
      [free 64][A 1,000][free 9,936]
      then moving for the sake of defragmenting these 64 bytes is definitely a waste of time.

      So if there is a choice, moving is performed if fragments on BOTH sides are larger than 1/8 (12.5%) of the (new) size. }

    oldph := pVarHeader(p)[-1].ch.h;
    if arSizeFlags and PrevIsFreeFlag <> 0 then
    begin
      prevSize2 := pFreeVarTail(p - (VarHeaderSize + FreeVarTailSize))^.size;
      { Consider (not) moving... }
      dec(arSizeFlags, prevSize2); { Temporarily (or not) remove prevSize from arSizeFlags. This corresponds to the size available without moving. }
      minFragment := uint32(size) div 8;
      if (arSizeFlags < uint32(size)) { Size does not fit without moving? }
        or (prevSize2 >= minFragment) and (uint32(arSizeFlags - uint32(size)) >= minFragment) { There are large enough fragments on both sides? }
      then
      begin
        if prevSize2 >= MinSearchableVarHeaderAndPayload then
          varFree.Remove(ar);
        inc(arSizeFlags, prevSize2 - PrevIsFreeFlag); { Add prevSize back, and remove PrevIsFreeFlag. }
        moveSize := oldph and VarSizeMask;
        if uint32(size) < uint32(moveSize) then
          moveSize := uint32(size);
        { Might overwrite old p header, so must use oldph from now on.
          Otherwise is safe: won’t overwrite the header of the free chunk after p, won’t be performed in the no-op case. }
        Move(p^, ar^, moveSize - VarHeaderSize);
      end else
        { Not moving; finish the removal of the previous chunk from “ar”. arSizeFlags is already decreased by prevSize, and keeps PrevIsFreeFlag. }
        ar := p; { Same as inc(ar, prevSize2). }
    end;

    oldpsize := oldph and VarSizeMask;
    if (uint32(size) = uint32(oldpsize)) and (ar = p) then
      { TryResizeVar was a no-op, and with some explicit efforts we managed to write nothing by this point,
        so we use our last chance to get out. }
      exit;

    { Remove the free chunk after p. }
    if oldph and LastFlag = 0 then
    begin
      hNext2 := pVarHeader(p + oldpsize - VarHeaderSize)^.ch.h;
      if (hNext2 and UsedFlag = 0) and (hNext2 >= MinSearchableVarHeaderAndPayload) then
        varFree.Remove(p + oldpsize);
    end;
    dec(used, oldpsize);

    { Format the free chunk after ar, or its absence. }
    fSizeFlags := uint32(arSizeFlags - uint32(size)) and (VarSizeMask or LastFlag);
    if fSizeFlags >= uint32(MinAnyVarHeaderAndPayload + (MinSearchableVarHeaderAndPayload - MinAnyVarHeaderAndPayload) div LastFlag * (fSizeFlags and LastFlag)) then
    begin
      dec(arSizeFlags, fSizeFlags);
      fp := ar + size;
      pVarHeader(fp)[-1].ofsToOs := pVarHeader(ar)[-1].ofsToOs - int32(size);
      pVarHeader(fp)[-1].ch.h := fSizeFlags;
      if fSizeFlags and LastFlag = 0 then
      begin
        pFreeVarTail(fp + fSizeFlags - (VarHeaderSize + FreeVarTailSize))^.size := fSizeFlags;
        pVarHeader(fp + fSizeFlags)[-1].ch.h := pVarHeader(fp + fSizeFlags)[-1].ch.h or PrevIsFreeFlag; { May have had it already. }
      end;
      if fSizeFlags >= MinSearchableVarHeaderAndPayload then
        varFree.Add(fp, fSizeFlags);
    end
    else if arSizeFlags and LastFlag = 0 then
      pVarHeader(ar + arSizeFlags and VarSizeMask)[-1].ch.h := pVarHeader(ar + arSizeFlags and VarSizeMask)[-1].ch.h and uint32(not PrevIsFreeFlag); { May not have had it already. }

    pVarHeader(ar)[-1].ch.h := arSizeFlags;

    statv := used + arSizeFlags and VarSizeMask;
    used := statv;
    inc(statv, gs.hugeUsed);
    if statv > maxUsed then
      maxUsed := statv;
  end;

  { If SysOSFree is available, huge chunks aren’t cached by any means.
    If SysOSFree is not available, there’s no choice but to cache them.
    Caching is done directly into gs.freeOS if FPC_HAS_FEATURE_THREADING, otherwise ThreadState.freeOS. }

  class function HeapInc.ThreadState.AddToHugeUsed(delta: SizeInt): SizeUint;
  begin
  {$if not defined(FPC_HAS_FEATURE_THREADING)}
    result := SizeUint(SizeInt(gs.hugeUsed) + delta);
    gs.hugeUsed := result;
  {$elseif not defined(VER3_2)}
    result := AtomicIncrement(gs.hugeUsed, SizeUint(delta));
  {$elseif sizeof(SizeInt) = sizeof(int64)}
    result := SizeUint(delta + InterlockedExchangeAdd64(SizeInt(gs.hugeUsed), delta));
  {$else}
    result := SizeUint(delta + InterlockedExchangeAdd(SizeInt(gs.hugeUsed), delta));
  {$endif}
  end;

  function HeapInc.ThreadState.AllocHuge(size: SizeUint): pointer;
  var
    userSize: SizeUint;
  begin
    userSize := size;
    size := (size + (HugeChunkDataOffset + CommonHeaderSize + OSChunkVarSizeQuant - 1)) and SizeUint(-OSChunkVarSizeQuant);
    if size < userSize then { Overflow. }
      exit(AllocFailed);
  {$ifdef HAS_SYSOSFREE}
    result := SysOSAlloc(size);
    if not Assigned(result) then
      exit(AllocFailed);
    pHugeChunk(result)^.size := size;
  {$else HAS_SYSOSFREE}
    result := GetOSChunk(size, High(SizeUint));
    if not Assigned(result) then
    begin
      result := AllocateOSChunk(size, High(SizeUint));
      if not Assigned(result) then
        exit; { AllocateOSChunk throws an error if required. }
    end;
    size := pOSChunk(result)^.size;
    dec(allocated, size); { After GetOSChunk* chunk size is counted in “allocated”; don’t count. }
  {$endif HAS_SYSOSFREE}
    pCommonHeader(result + HugeChunkDataOffset)^.h := HugeHeader;
    inc(result, HugeChunkDataOffset + CommonHeaderSize);
    UpdateMaxStats(AddToHugeUsed(size));
  end;

  function HeapInc.ThreadState.FreeHuge(p: pointer): SizeUint;
  {$ifndef HAS_SYSOSFREE}
  var
    fOs: ^FreeOSChunkList;
    osPrev: pOSChunk;
  {$endif ndef HAS_SYSOSFREE}
  begin
    dec(p, HugeChunkDataOffset + CommonHeaderSize);
    result := pHugeChunk(p)^.size;
    AddToHugeUsed(-SizeInt(result));
  {$ifndef HAS_SYSOSFREE} { But you’d better have SysOSFree... }
  {$ifdef FPC_HAS_FEATURE_THREADING}
    fOs := @gs.freeOS; { gs.freeOS aren’t counted anywhere (for now). }
    EnterCriticalSection(gs.lock);
  {$else FPC_HAS_FEATURE_THREADING}
    fOs := @freeOS;
    inc(allocated, result); { ThreadState.freeOS are counted in ThreadState.allocated. But since “size” (= result) is just moved from “hugeUsed” to “allocated”, it won’t affect maximums. }
  {$endif FPC_HAS_FEATURE_THREADING}
    { Turn p into FreeOSChunk and add to fOs; add to the end to reduce the chance for this chunk to be reused
      (other OS chunks are added to the beginning and searched from the beginning). }
    osPrev := fOs^.last;
    pFreeOSChunk(p)^.prev := osPrev;
    pFreeOSChunk(p)^.next := nil;
    if Assigned(osPrev) then
      osPrev^.next := p
    else
      fOs^.first := p;
    fOs^.last := p;
  {$ifdef FPC_HAS_FEATURE_THREADING} LeaveCriticalSection(gs.lock); {$endif}
  {$endif ndef HAS_SYSOSFREE}
  {$ifdef HAS_SYSOSFREE} SysOSFree(p, result); {$endif}
    dec(result, HugeChunkDataOffset + CommonHeaderSize);
  end;

  function HeapInc.ThreadState.TryResizeHuge(p: pointer; size: SizeUint): pointer;
  var
    userSize, oldSize: SizeUint;
  begin
    userSize := size;
    size := (size + (HugeChunkDataOffset + CommonHeaderSize + OSChunkVarSizeQuant - 1)) and SizeUint(-OSChunkVarSizeQuant);
    if (size < userSize) or { Overflow. }
      (size < GrowHeapSize2 div 4) { Limit on shrinking huge chunks. }
    then
      exit(nil);
    oldSize := pHugeChunk(p - (HugeChunkDataOffset + CommonHeaderSize))^.size;
    if size = oldSize then
      exit(p);
  {$ifdef FPC_SYSTEM_HAS_SYSOSREALLOC}
    result := SysOSRealloc(p - (HugeChunkDataOffset + CommonHeaderSize), oldSize, size);
    if Assigned(result) then
    begin
      UpdateMaxStats(AddToHugeUsed(SizeInt(size) - SizeInt(oldSize)));
      pHugeChunk(result)^.size := size;
      inc(result, HugeChunkDataOffset + CommonHeaderSize);
    end;
  {$else FPC_SYSTEM_HAS_SYSOSREALLOC}
    result := nil; { Just don’t. Note shrinking 20 Mb to 19 will require temporary 39 because of this. }
  {$endif FPC_SYSTEM_HAS_SYSOSREALLOC}
  end;

  procedure HeapInc.ThreadState.UpdateMaxStats(hugeUsed: SizeUint);
  var
    statv: SizeUint;
  begin
    statv := used + hugeUsed;
    if statv > maxUsed then
      maxUsed := statv;
    statv := allocated + hugeUsed;
    if statv > maxAllocated then
      maxAllocated := statv;
  end;

{$ifdef FPC_HAS_FEATURE_THREADING}
  procedure HeapInc.ThreadState.PushToFree(p: pFreeChunk);
  var
    nx: pFreeChunk;
  begin
    repeat
      nx := toFree;
      p^.next := nx;
      WriteBarrier; { Write p after p^.next. }
    until {$ifdef VER3_2} InterlockedCompareExchange {$else} AtomicCmpExchange {$endif} (toFree, p, nx) = nx;
  end;

  procedure HeapInc.ThreadState.FlushToFree;
  begin
    FreeToFreeList({$ifdef VER3_2} InterlockedExchange {$else} AtomicExchange {$endif} (toFree, nil));
  end;

  class procedure HeapInc.ThreadState.FreeToFreeList(tf: pFreeChunk);
  var
    nx: pFreeChunk;
  begin
    while Assigned(tf) do
    begin
      ReadDependencyBarrier; { Read tf^.next after tf. }
      nx := tf^.next;
      SysFreeMem(tf);
      tf := nx;
    end;
  end;

  procedure HeapInc.ThreadState.Orphan;
  var
    vOs: pVarOSChunk;
    p: pointer;
    h: uint32;
  {$ifndef HAS_SYSOSFREE}
    lastFree, nextFree: pFreeOSChunk;
  {$endif not HAS_SYSOSFREE}
  begin
    if gs.lockUse <> 0 then
      EnterCriticalSection(HeapInc.gs.lock);
    if assigned(toFree) then
      FlushToFree; { Performing it under gs.lock guarantees there will be no new toFree requests. }
    FreeEmptyArenas; { Has to free all empty arenas, otherwise the chunk that contains only empty arenas can leak. }

{$if not defined(HAS_SYSOSFREE)}
    { Prepend freeOS to gs.freeOS. }
    lastFree := freeOS.last;
    if Assigned(lastFree) then
    begin
      nextFree := gs.freeOS.first;
      lastFree^.next := nextFree;
      if Assigned(nextFree) then
        nextFree^.prev := lastFree
      else
        gs.freeOS.last := lastFree;
      gs.freeOS.first := freeOS.first;
      freeOS.first := nil;
      freeOS.last := nil;
    end;
{$elseif defined(SUPPORT_INIT_HEAP_PROCESS_WIDE)}
    { Remove from gs.threads, if present. }
    if Assigned(prev) then
      prev^.next := next
    else if @self = gs.threads then { if prev = nil, then this ThreadState is either absent from gs.threads or is its first item. }
      gs.threads := next;
    if Assigned(next) then
      next^.prev := prev;
    prev := nil;
    next := nil;
{$endif not HAS_SYSOSFREE | defined(SUPPORT_INIT_HEAP_PROCESS_WIDE)}
    vOs := varOS;
    while Assigned(vOs) do
    begin
      vOs^.threadState := nil;
      p := pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize);
      repeat
        h := pVarHeader(p - VarHeaderSize)^.ch.h;
        { bail out if the next pointer points outside of the allocated OS chunk,
          or if the size is zero, so the next block would point to ourselves }
        if (h and VarSizeMask = 0) or
          ((p + (h and VarSizeMask)) > (pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize)) + vOs^.size) then
        begin
          RunError(203);
        end;
        if (h and UsedFlag = 0) and (h >= MinSearchableVarHeaderAndPayload) then
          gs.varFree.Add(p, pFreeVarChunk(p)^.binIndex);
        inc(p, h and VarSizeMask);
      until h and LastFlag <> 0;
      vOs := vOs^.next;
    end;
    varOS := nil;
    if gs.lockUse <> 0 then
      LeaveCriticalSection(gs.lock);

{$ifdef HAS_SYSOSFREE}
    ReplaceFreeOS1(nil); { Does not require gs.lock. }
{$endif HAS_SYSOSFREE}
  end;

  procedure HeapInc.ThreadState.AdoptArena(arena: pFixedArena);
  var
    sizeIndex: SizeUint;
    nextArena: pFixedArena;
  begin
    sizeIndex := pCommonHeader(pointer(arena) + FixedArenaDataOffset)^.h and SizeIndexMask;
    inc(used, arena^.usedSizeMinus1 + 1); { maxUsed is updated at the end of AdoptVarOwner. }

    { Orphan frees all empty arenas, so adopted arena can’t be empty. }
    if arena^.usedSizeMinus1 < arena^.almostFullThreshold + IndexToSize(sizeIndex) then
    begin
      { Add arena to partialArenas[sizeIndex]. }
      nextArena := partialArenas[sizeIndex];
      arena^.prev := nil;
      arena^.next := nextArena;
      if Assigned(nextArena) then
        nextArena^.prev := arena;
      partialArenas[sizeIndex] := arena;
    end else
      inc(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h and VarSizeMask);
  end;

  procedure HeapInc.ThreadState.AdoptVarOwner(p: pointer);
  var
    statv: SizeUint;
    h: uint32;
    vOs, osNext: pVarOSChunk;
  begin
    vOs := p + pVarHeader(p)[-1].ofsToOs;
    vOs^.threadState := @self;

    { Add OS chunk to varOS. }
    vOs^.prev := nil;
    osNext := varOS;
    vOs^.next := osNext;
    if Assigned(osNext) then
      osNext^.prev := vOs;
    varOS := vOs;

    statv := allocated + vOs^.size;
    allocated := statv;
    inc(statv, gs.hugeUsed);
    if statv > maxAllocated then
      maxAllocated := statv;

    p := pointer(vOs) + VarOSChunkDataOffset + VarHeaderSize;
    repeat
      h := pVarHeader(p - VarHeaderSize)^.ch.h;
      if h and UsedFlag = 0 then
      begin
        if h >= MinSearchableVarHeaderAndPayload then
        begin
          gs.varFree.Remove(p);
          varFree.Add(p, pFreeVarChunk(p)^.binIndex);
        end;
      end
      else if h and FixedArenaFlag <> 0 then
        AdoptArena(p)
      else
        inc(used, h and VarSizeMask); { maxUsed is updated after the loop. }
      inc(p, h and VarSizeMask);
    until h and LastFlag <> 0;

    statv := used + gs.hugeUsed;
    if statv > maxUsed then
      maxUsed := statv;
  end;

{$ifndef FPC_SECTION_THREADVARS}
  procedure HeapInc.ThreadState.FixupSelfPtr;
  var
    vOs: pVarOSChunk;
  begin
    vOs := varOS;
    while Assigned(vOs) do
    begin
      vOs^.threadState := @self;
      vOs := vOs^.next;
    end;
  {$if defined(SUPPORT_INIT_HEAP_PROCESS_WIDE) and defined(HAS_SYSOSFREE)}
    { Not sure if required... }
    if Assigned(prev) then
      prev^.next := @self
    else
      gs.threads := @self;
    if Assigned(next) then
      next^.prev := @self;
  {$endif SUPPORT_INIT_HEAP_PROCESS_WIDE and HAS_SYSOSFREE}
  end;
{$endif ndef FPC_SECTION_THREADVARS}
{$endif FPC_HAS_FEATURE_THREADING}

  class function HeapInc.AllocFailed: pointer;
  begin
    if not ReturnNilIfGrowHeapFails then
      HandleError(204);
    result := nil;
  end;

function SysGetFPCHeapStatus:TFPCHeapStatus;
var
  ts: HeapInc.pThreadState;
  hugeUsed: SizeUint;
begin
  ts := @HeapInc.thisTs;
  hugeUsed := HeapInc.gs.hugeUsed;
  ts^.UpdateMaxStats(hugeUsed); { Cheat to avoid clearly implausible values like current > max. }
  result.MaxHeapSize := ts^.maxAllocated;
  result.MaxHeapUsed := ts^.maxUsed;
  result.CurrHeapSize := hugeUsed + ts^.allocated;
  result.CurrHeapUsed := hugeUsed + ts^.used;
  result.CurrHeapFree := result.CurrHeapSize - result.CurrHeapUsed;
end;

function SysGetHeapStatus :THeapStatus;
var
  fhs: TFPCHeapStatus;
begin
  fhs := SysGetFPCHeapStatus;
  FillChar((@result)^, sizeof(result), 0);
  result.TotalAllocated   := fhs.CurrHeapUsed;
  result.TotalFree        := fhs.CurrHeapSize - fhs.CurrHeapUsed;
  result.TotalAddrSpace   := fhs.CurrHeapSize;
end;

function SysGetMem(size : ptruint):pointer;
var
  ts: HeapInc.pThreadState;
begin
  ts := @HeapInc.thisTs;
  if size <= HeapInc.MaxFixedHeaderAndPayload - HeapInc.CommonHeaderSize then
    exit(ts^.AllocFixed(size));
{$ifdef FPC_HAS_FEATURE_THREADING}
  if Assigned(ts^.toFree) then
    ts^.FlushToFree;
{$endif}
  if (size < GrowHeapSize2 div 2) { Approximate idea on the max size of the variable chunk. Approximate because size does not include headers but GrowHeapSize2 does. }
    and (size <= HeapInc.MaxVarHeaderAndPayload - HeapInc.VarHeaderSize) then
    result := ts^.AllocVar(size, false)
  else
    result := ts^.AllocHuge(size);
end;

function SysFreeMem(p: pointer): ptruint;
var
  ts: HeapInc.pThreadState;
begin
  if Assigned(p) then
    begin
      ts := @HeapInc.thisTs;
      if HeapInc.pCommonHeader(p - HeapInc.CommonHeaderSize)^.h and HeapInc.FixedFlag <> 0 then
        result := ts^.FreeFixed(p)
      else if HeapInc.pCommonHeader(p - HeapInc.CommonHeaderSize)^.h <> HeapInc.HugeHeader then
        result := ts^.FreeVar(p)
      else
        result := ts^.FreeHuge(p);
    end
  else
    result := 0;
end;

function SysMemSize(p: pointer): ptruint;
var
  h: uint32;
begin
  if not Assigned(p) then
    exit(0);
  h := HeapInc.pCommonHeader(p - HeapInc.CommonHeaderSize)^.h;
  if h and HeapInc.FixedFlag <> 0 then
    result := HeapInc.IndexToSize(h and HeapInc.SizeIndexMask) - HeapInc.CommonHeaderSize
  else if h <> HeapInc.HugeHeader then
    result := HeapInc.pVarHeader(p - HeapInc.VarHeaderSize)^.ch.h and uint32(HeapInc.VarSizeMask) - HeapInc.VarHeaderSize
  else
    result := HeapInc.pHugeChunk(p - (HeapInc.HugeChunkDataOffset + HeapInc.CommonHeaderSize))^.size - (HeapInc.HugeChunkDataOffset + HeapInc.CommonHeaderSize);
end;

function SysReAllocMem(var p: pointer; size: ptruint):pointer;
var
  ts: HeapInc.pThreadState;
  h: uint32;
  oldOrCopySize: SizeUint;
  newp: pointer;
begin
  result := p; { Use as old p value until freed etc. }
  if (size = 0) or not Assigned(result) then { Special cases; check at once. }
  begin
    if size = 0 then
    begin
      SysFreeMem(result);
      result := nil;
    end else
      result := SysGetMem(size);
    p := result;
    exit;
  end;
  h := HeapInc.pCommonHeader(result - HeapInc.CommonHeaderSize)^.h;
  if h and HeapInc.FixedFlag <> 0 then
  begin
    if (size <= HeapInc.MaxFixedHeaderAndPayload - HeapInc.CommonHeaderSize)
      and (h and HeapInc.SizeIndexMask = HeapInc.SizeMinus1ToIndex(size + (HeapInc.CommonHeaderSize - 1))) then
      exit;
  end else
  begin
    ts := @HeapInc.thisTs;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if Assigned(ts^.toFree) then
      ts^.FlushToFree;
  {$endif FPC_HAS_FEATURE_THREADING}
    if h <> HeapInc.HugeHeader then
      newp := ts^.TryResizeVar(result, size)
    else
      newp := ts^.TryResizeHuge(result, size);
    if Assigned(newp) then
    begin
      p := newp;
      exit(newp);
    end;
  end;

  { Generic fallback: GetMem + Move + FreeMem. }
  oldOrCopySize := SysMemSize(result);
  newp := SysGetMem(size);
  if not Assigned(newp) then
  begin
    { Don’t fail if shrinking. If growing failed, return nil, but keep the old p. }
    if size > oldOrCopySize then
      result := nil;
    exit;
  end;
  p := newp;
  if oldOrCopySize > size then
    oldOrCopySize := size;
  Move(result^, newp^, oldOrCopySize);
  SysFreeMem(result);
  result := newp;
end;

Function SysFreeMemSize(p: pointer; size: ptruint):ptruint;
begin
  { can't free partial blocks, ignore size }
  result := SysFreeMem(p);
end;

function SysAllocMem(size: ptruint): pointer;
begin
  result := SysGetMem(size);
  if Assigned(result) then
    FillChar(result^, SysMemSize(result), 0);
end;

{*****************************************************************************
                                 InitHeap
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_THREADING}
{$ifdef SUPPORT_INIT_HEAP_PROCESS_WIDE}
{ DeferInitHeapProcessWide / DoneHeapProcessWide are meant to support DLL_PROCESS_ATTACH / DLL_PROCESS_DETACH.
  Otherwise InitHeapThread + FinalizeHeap called per thread do their best with refcounting... }
procedure DeferInitHeapProcessWide;
begin
  HeapInc.gs.askedForProcessWideLockInitialization := true; { Called before InitSystemThreads, cannot initialize the lock... }
end;

procedure DoneHeapProcessWide;
{$ifdef HAS_SYSOSFREE}
var
  thisTs, nextTs, ts: HeapInc.pThreadState;
  stolenTf: HeapInc.pFreeChunk;
{$endif HAS_SYSOSFREE}
begin
  if HeapInc.gs.lockUse <> HeapInc.gs.LockInitializedProcessWide then
    exit;
{$ifdef HAS_SYSOSFREE}
  { We need to free all lingering data of all threads: to-free lists, empty arenas, “freeOS1”s.

    For each particular thread ts, this is what ts^.Orphan does, but ts^.Orphan can’t be (easily) called from another thread:
    it calls ts^.FlushToFree which calls SysFreeMem which is hardcoded to work with HeapInc.thisTs.
    It’s not worth redesigning, or the common case of SysFreeMem that must indeed work with HeapInc.thisTs will be slower.

    So we steal and zero ts^.toFree (ts^.FlushToFree is the only thing that prevents ts^.Orphan from working from threads other than ts), call ts^.Orphan,
    then manually complete toFree requests on our behalf.

    This entire thing is just to handle the case of unloading a DLL before terminating the thread that used this DLL
    (https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/1173). }

  thisTs := @HeapInc.thisTs;
  nextTs := HeapInc.gs.threads;
  while Assigned(nextTs) do
  begin
    ts := nextTs;
    nextTs := ts^.next;
    if ts = thisTs then { Used for executing toFrees and is orphaned the last. }
      continue;
    stolenTf := ts^.toFree;
    ts^.toFree := nil;
    ts^.Orphan;
    HeapInc.ThreadState.FreeToFreeList(stolenTf);
  end;
  thisTs^.Orphan;
{$endif HAS_SYSOSFREE}
  HeapInc.gs.lockUse := 0;
  DoneCriticalSection(HeapInc.gs.lock);
end;
{$endif SUPPORT_INIT_HEAP_PROCESS_WIDE}

{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeapThread;
{$if defined(SUPPORT_INIT_HEAP_PROCESS_WIDE) and defined(HAS_SYSOSFREE)}
var
  ts, next: HeapInc.pThreadState;
{$endif SUPPORT_INIT_HEAP_PROCESS_WIDE and HAS_SYSOSFREE}
begin
{$ifdef SUPPORT_INIT_HEAP_PROCESS_WIDE}
  if (HeapInc.gs.lockUse = 0) and HeapInc.gs.askedForProcessWideLockInitialization then
  begin
    HeapInc.gs.lockUse := HeapInc.gs.LockInitializedProcessWide;
    InitCriticalSection(HeapInc.gs.lock);
  end else
{$endif SUPPORT_INIT_HEAP_PROCESS_WIDE}
  if (HeapInc.gs.lockUse >= 0) and ({$ifdef VER3_2} InterlockedIncrement {$else} AtomicIncrement {$endif} (HeapInc.gs.lockUse) = 1) then
    InitCriticalSection(HeapInc.gs.lock);

{$if defined(SUPPORT_INIT_HEAP_PROCESS_WIDE) and defined(HAS_SYSOSFREE)}
  { Add to gs.threads. }
  ts := @HeapInc.thisTs;
  EnterCriticalSection(HeapInc.gs.lock);
  next := HeapInc.gs.threads;
  { Check if already in gs.threads; this function can in principle be called twice on the same thread (or at least I did call redundant InitThread for some time...). }
  if not Assigned(ts^.prev) and (ts <> next) then
  begin
    ts^.next := next;
    if Assigned(next) then
      next^.prev := ts;
    HeapInc.gs.threads := ts;
  end;
  LeaveCriticalSection(HeapInc.gs.lock);
{$endif SUPPORT_INIT_HEAP_PROCESS_WIDE and HAS_SYSOSFREE}
end;
{$endif FPC_HAS_FEATURE_THREADING}

procedure InitHeap; public name '_FPC_InitHeap';
begin
  { we cannot initialize the locks here yet, thread support is
    not loaded yet }
end;

procedure RelocateHeap;
begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  if HeapInc.gs.lockUse <> 0 then
    exit;
  InitHeapThread; { Initializes the lock and sets lockUse = 1 (or maybe LockInitializedProcessWide). }
{$ifndef FPC_SECTION_THREADVARS}
  { threadState pointers still point to main thread's thisTs, but they
    have a reference to the global main thisTs, fix them to point
    to the main thread specific variable.
    even if section threadvars are used, this shouldn't cause problems as threadState pointers simply
    do not change but we do not need it }
  HeapInc.thisTs.FixupSelfPtr;
{$endif FPC_SECTION_THREADVARS}
  if MemoryManager.RelocateHeap <> nil then
    MemoryManager.RelocateHeap();
{$endif FPC_HAS_FEATURE_THREADING}
end;

procedure FinalizeHeap;
begin
  { Do not try to do anything if the heap manager already reported an error }
  if (errorcode=203) or (errorcode=204) then
    exit;
{$if defined(FPC_HAS_FEATURE_THREADING)}
  HeapInc.thisTs.Orphan;
  if (HeapInc.gs.lockUse > 0) and ({$ifdef VER3_2} InterlockedDecrement {$else} AtomicDecrement {$endif} (HeapInc.gs.lockUse) = 0) then
    DoneCriticalSection(HeapInc.gs.lock);
{$elseif defined(HAS_SYSOSFREE)}
  HeapInc.thisTs.ReplaceFreeOS1(nil);
{$endif FPC_HAS_FEATURE_THREADING | defined(HAS_SYSOSFREE)}
end;

{$endif ndef HAS_MEMORYMANAGER}
{$endif FPC_HAS_FEATURE_HEAP}
