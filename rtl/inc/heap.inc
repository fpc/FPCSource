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
{$if not defined(FPC_NO_DEFAULT_MEMORYMANAGER)}
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
  );
{$elseif not defined(FPC_IN_HEAPMGR)}
const
  MemoryManager: TMemoryManager = (
    NeedLock: false;  // Obsolete
    GetMem: nil;
    FreeMem: nil;
    FreeMemSize: nil;
    AllocMem: nil;
    ReAllocMem: nil;
    MemSize: nil;
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: nil;
    GetFPCHeapStatus: nil;
  );public name 'FPC_SYSTEM_MEMORYMANAGER';
{$endif FPC_IN_HEAPMGR}

{*****************************************************************************
                             Memory Manager
*****************************************************************************}

{$ifndef FPC_IN_HEAPMGR}
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
{$if defined(HAS_MEMORYMANAGER) or defined(FPC_NO_DEFAULT_MEMORYMANAGER)}
  Result:=false;
{$else not FPC_NO_DEFAULT_MEMORYMANAGER}
  IsMemoryManagerSet := (MemoryManager.GetMem<>@SysGetMem)
    or (MemoryManager.FreeMem<>@SysFreeMem);
{$endif HAS_MEMORYMANAGER or FPC_NO_DEFAULT_MEMORYMANAGER}
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
{$endif FPC_HAS_FEATURE_HEAP}
{$endif FPC_IN_HEAPMGR}

{$if (defined(FPC_HAS_FEATURE_HEAP) or defined(FPC_IN_HEAPMGR)) and not defined(HAS_MEMORYMANAGER)}

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

  { Adjustable part ends here~ }

  const
    SizeIndexBits = 1 + trunc(ln(FixedSizesCount - 1) /  ln(2));
    SizeIndexMask = 1 shl SizeIndexBits - 1;
    FixedBitPos = {$if SizeIndexBits >= 3} SizeIndexBits {$else} 3 {$endif}; { Variable chunks use 3 low bits for used / last / fixed arena. }
    FixedFlag = 1 shl FixedBitPos;
    FixedArenaOffsetShift = {$if FixedBitPos + 1 >= 5} FixedBitPos + 1 {$else} 5 {$endif}; { VarSizeQuant is expected to be 2^5. }

    UsedFlag = 1 shl 0;
    LastFlag = 1 shl 1;
    FixedArenaFlag = 1 shl 2;
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
    MinEmptyVarHeaderAndPayload = (MaxFixedHeaderAndPayload + 1 shl FirstVarStepP2 + VarSizeQuant - 1) and -VarSizeQuant;
    MaxVarHeaderAndPayload = (MaxFixedHeaderAndPayload + (1 shl VarSizeClassesCount - 1) shl FirstVarRangeP2) and -VarSizeQuant; {$if MaxVarHeaderAndPayload <> MaxFixedHeaderAndPayload + 1047552} {$error does not match the explanation above :D} {$endif}

    class function VarSizeToBinIndex(size: SizeUint; roundUp: boolean): SizeUint; static;
  {$ifdef DEBUG_HEAP_INC}
    class function BinIndexToVarSize(binIndex: SizeUint): SizeUint; static;
  {$endif DEBUG_HEAP_INC}

  type
    { Common header of any memory chunk, residing immediately to the left of the ~payload~ (block).

      Fixed chunk header, assuming SizeIndexBits = 4:
      h[0:3] = size index (= h and SizeIndexMask)
      h[4] = 1 (h and FixedFlag <> 0)
      h[5:31] — offset in the FixedArena (= h shr FixedArenaOffsetShift)

      Variable chunk header, assuming SizeIndexBits = 4:
      h[0] = used flag (h and UsedFlag <> 0)
      h[1] = last flag (h and LastFlag <> 0)
      h[2] = fixed arena flag (h and FixedArenaFlag <> 0)
      h[3] = unused
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

    FreeOSChunkList = object
      first, last: pFreeOSChunk;
    {$ifdef HAS_SYSOSFREE}
      n: SizeUint;
    {$endif}

      function Get(minSize, maxSize: SizeUint): pOSChunk;
    {$ifdef HAS_SYSOSFREE}
      function FreeOne: SizeUint;
      procedure FreeAll;
    {$endif}
    end;

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
    end;

    pVarHeader = ^VarHeader;
    VarHeader = record
    {$ifdef FPC_HAS_FEATURE_THREADING}
      threadState: pThreadState; { Protected with gs.lock. Nil if orphaned. }
    {$endif}
      prevSize: uint32; { Always 0 for the first chunk. }
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

    pHugeChunk = ^HugeChunk;
    HugeChunk = object(OSChunkBase)
    end;

    NonZeroDWord = 1 .. High(uint32); { MAYBE IT WILL WORK ONE DAY (https://gitlab.com/freepascal.org/fpc/source/-/issues/41179). }
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
      procedure Add(c: pFreeVarChunk; binIndex: SizeUint);
      procedure Remove(c: pFreeVarChunk);
    end;

    ThreadState = object
      emptyArenas: pFixedArena; { Empty fixed arenas to be reused instead of slower AllocVar. Singly linked list, “prev”s are garbage. }
      nEmptyArenas: SizeUint; { # of items in emptyArenas. }
      freeOS: FreeOSChunkList; { Completely empty OS chunks. }
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
      allocatedByFullArenas: array[0 .. FixedSizesCount - 1] of SizeUint;

      varFree: VarFreeMap;

    {$ifdef DEBUG_HEAP_INC}
      procedure Dump(var f: text);
    {$endif}

      function ChooseFixedArenaSize(sizeIndex: SizeUint): SizeUint;
      function AllocFixed(size: SizeUint): pointer; inline;
      function FreeFixed(p: pointer): SizeUint; inline;

      function GetOSChunk(minSize, maxSize: SizeUint): pOSChunk;
      function AllocateOSChunk(minSize, maxSize: SizeUint): pOSChunk;

      function AllocVar(size: SizeUint; isArena: boolean): pointer;
      function FreeVar(p: pointer): SizeUint;
      function TryResizeVar(p: pointer; size: SizeUint): pointer;

      function AllocHuge(size: SizeUint): pointer;
      function FreeHuge(p: pointer): SizeUint;
      function TryResizeHuge(p: pointer; size: SizeUint): pointer;
      procedure UpdateMaxStats(hugeUsed: SizeUint);

    {$ifdef FPC_HAS_FEATURE_THREADING}
      procedure PushToFree(p: pFreeChunk);
      procedure FlushToFree;

      procedure Orphan;
      procedure AdoptArena(arena: pFixedArena);
      procedure AdoptVarOwner(p: pointer); { Adopts the OS chunk that contains p. Must be performed under gs.lock. }
      class procedure ChangeThreadState(vOs: pVarOSChunk; ts: pThreadState); static;

    {$ifndef FPC_SECTION_THREADVARS}
      procedure FixupSelfPtr;
    {$endif ndef FPC_SECTION_THREADVARS}
    {$endif FPC_HAS_FEATURE_THREADING}
    end;

    GlobalState = record
      hugeUsed: SizeUint; { Same as non-existing “hugeAllocated” as huge chunks don’t have free space.
                            Protected by gs.lock, but can be read unprotected if unreliability is tolerable.
                            Huge chunks don’t have thread affinity, so are tracked here. Presently, this value is added to all memory statistics.
                            Not a good idea and makes multithreaded statistics a strange and unreliable mix, but alternatives are even worse. }

    {$ifdef FPC_HAS_FEATURE_THREADING}
      lock: TRTLCriticalSection;
      lockUse: int32;

      { Data from dead threads (“orphaned”), protected by gs.lock. }
      varOS: pVarOSChunk;
    {$ifndef HAS_SYSOSFREE}
      freeOS: FreeOSChunkList;
    {$endif not HAS_SYSOSFREE}
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
    VarOSChunkDataOffset = (sizeof(VarOSChunk) + VarHeaderSize + Alignment - 1) and -Alignment - VarHeaderSize;
    HugeChunkDataOffset = (sizeof(HugeChunk) + CommonHeaderSize + Alignment - 1) and -Alignment - CommonHeaderSize;
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
    maxv, binClassIndex: SizeUint;
  begin
    if size >= MaxVarHeaderAndPayload then { Large sizes go to the last bin, assuming searches never search for more than MaxVarHeaderAndPayload. }
      exit(VarSizeClassesCount * VarSizesPerClass - 1);
    dec(size, MaxFixedHeaderAndPayload);
    binClassIndex := SizeUint(BsrDWord(NonZeroDWord(size)) - FirstVarRangeP2);
    if SizeInt(binClassIndex) < 0 then binClassIndex := 0;
    maxv := (SizeUint(2) shl binClassIndex - 1) shl FirstVarRangeP2;
    if size <= maxv then
    begin
      maxv := maxv shr 1; { Turn into “minv” to be subtracted from size. If size > maxv, “minv” is maxv. :) }
      maxv := maxv and SizeUint(-SizeInt(1 shl FirstVarRangeP2));
      dec(SizeInt(binClassIndex)); { Compensate inc(binClassIndex) below, so in the end, it is increased if size > maxv. All of this prevents having an “else” branch with its extra jump. }
    end;
    dec(size, maxv);
    inc(SizeInt(binClassIndex));
    result := binClassIndex * VarSizesPerClass + SizeUint(size - 1) shr (FirstVarStepP2 + binClassIndex);
    if not roundUp and (size and SizeUint(SizeUint(1) shl (FirstVarStepP2 + binClassIndex) - 1) <> 0) then
      dec(result);
  end;

{$ifdef DEBUG_HEAP_INC}
  class function HeapInc.BinIndexToVarSize(binIndex: SizeUint): SizeUint;
  begin
    result := binIndex div VarSizesPerClass;
    result := MaxFixedHeaderAndPayload + (SizeUint(1) shl result - 1) shl FirstVarRangeP2 + (1 + binIndex mod VarSizesPerClass) shl (FirstVarStepP2 + result);
  end;
{$endif DEBUG_HEAP_INC}

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
  {$ifdef HAS_SYSOSFREE} dec(n); {$endif}
  end;

{$ifdef HAS_SYSOSFREE}
  function HeapInc.FreeOSChunkList.FreeOne: SizeUint;
  var
    best, prev: pFreeOSChunk;
  begin
    { Presently: the last one (which means LRU, as they are pushed to the beginning). }
    best := last;
    prev := best^.prev;
    if Assigned(prev) then
      prev^.next := nil
    else
      first := nil;
    last := prev;
    dec(n);
    result := best^.size;
    SysOSFree(best, best^.size);
  end;

  procedure HeapInc.FreeOSChunkList.FreeAll;
  var
    cur, next: pFreeOSChunk;
  begin
    cur := first;
    first := nil;
    last := nil;
    n := 0;
    while Assigned(cur) do
    begin
      next := cur^.next;
      SysOSFree(cur, cur^.size);
      cur := next;
    end;
  end;
{$endif HAS_SYSOSFREE}

  procedure HeapInc.VarFreeMap.Add(c: pFreeVarChunk; binIndex: SizeUint);
  var
    next: pFreeVarChunk;
    iL0: SizeUint;
    vL0 {$ifdef HEAP_INC_USE_SETS}, vL1 {$endif}: uint32;
  begin
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
        write(f, HexStr(p), ': ',
          'prevSize = ', pVarHeader(p - VarHeaderSize)^.prevSize, ', size = ', pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask);
        if pVarHeader(p - VarHeaderSize)^.ch.h and UsedFlag <> 0 then
          write(f, ', used')
        else
          write(f, ', f r e e');
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          write(f, ', last');
        if pVarHeader(p - VarHeaderSize)^.ch.h and FixedArenaFlag <> 0 then
          write(f, ', fixed arena');
        writeln(f);
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          break;
        p := p + pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask;
      until false;
      needLE := true;
      vOs := vOs^.next;
    end;
    fr := freeOS.first;
    if Assigned(fr) then
    begin
      MaybeLE;
      repeat
        writeln(f, 'Free OS: ', HexStr(fr), ', size = ', fr^.size);
        fr := fr^.next;
      until not Assigned(fr);
      needLE := true;
    end;
    if varFree.L1 <> 0 then
    begin
      MaybeLE;
      write(f, 'L1:');
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
            write(f, pVarHeader(vf)[-1].ch.h and VarSizeMask);
            vf := vf^.next;
          until not Assigned(vf);
          write(f, ')');
        end;
      end;
      writeln(f);
      needLE := true;
    end;
  {$ifdef FPC_HAS_FEATURE_THREADING}
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
    dec(result, VarHeaderSize + VarSizeQuant); { Prettier fit into OS chunks. }
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
        if pCommonHeader(pointer(arena) + FixedArenaDataOffset)^.h and SizeIndexMask = sizeIndex then
          { Lucky! Just don’t reset the chunk and use its old freelist. }
        else
        begin
          arena^.firstFreeChunk := nil;
          arena^.usedSizeMinus1 := uint32(-1);
          arena^.almostFullThreshold := pVarHeader(arena)[-1].ch.h and VarSizeMask - 2 * IndexToSize(sizeIndex) - (VarHeaderSize + FixedArenaDataOffset); { available space - 2 * chunk size. }
        end;

        { Add arena to partialArenas[sizeIndex]. }
        nextArena := partialArenas[sizeIndex];
        arena^.prev := nil;
        arena^.next := nextArena;
        if Assigned(nextArena) then
          nextArena^.prev := arena;
        partialArenas[sizeIndex] := arena;
      end;
    end;

    sizeUp := IndexToSize(sizeIndex); { Not reusing the “size” variable saved a register at the time of writing this comment. }
    statv := used + sizeUp;
    used := statv;
    inc(statv, gs.hugeUsed);
    if statv > maxUsed then
      maxUsed := statv;

    { arena from partialArenas has either free chunk or free unformatted space for a new chunk. }
    usedSizeMinus1 := int32(arena^.usedSizeMinus1);
    result := arena^.firstFreeChunk;
    if not Assigned(result) then
    begin
      { Freelist is empty, so “formattedSize” = usedSizeMinus1 + 1. This “+ 1” is folded into constants. }
      result := pointer(arena) + (FixedArenaDataOffset + CommonHeaderSize + 1) + usedSizeMinus1;
      pCommonHeader(result - CommonHeadersize)^.h := uint32(int32(sizeIndex) + int32(usedSizeMinus1 shl FixedArenaOffsetShift) +
        (FixedFlag + (FixedArenaDataOffset + CommonHeaderSize + 1) shl FixedArenaOffsetShift) { ← const });
    end else
      arena^.firstFreeChunk := pFreeChunk(result)^.next;
    arena^.usedSizeMinus1 := uint32(usedSizeMinus1 + int32(sizeUp));
    if usedSizeMinus1 >= int32(arena^.almostFullThreshold) then { Uses usedSizeMinus1 value before adding sizeUp, as assumed by almostFullThreshold. }
    begin
      inc(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h); { Without masking with VarSizeMask, ch.h has parasite bits, but they don’t matter as long as they are unchanged, so the same value will be subtracted. }
      { Remove arena from partialArenas[sizeIndex]. (It was first.) }
      nextArena := arena^.next;
      partialArenas[sizeIndex] := nextArena;
      if Assigned(nextArena) then
        nextArena^.prev := nil;
    end;
  end;

  function HeapInc.ThreadState.FreeFixed(p: pointer): SizeUint;
  var
    sizeIndex: SizeUint;
    usedSizeMinus1: int32;
    arena, prevArena, nextArena: pFixedArena;
  begin
    arena := p - pCommonHeader(p - CommonHeaderSize)^.h shr FixedArenaOffsetShift;

  {$ifdef FPC_HAS_FEATURE_THREADING}
    { This can be checked without blocking; <arena>.threadState can only change from one value not equal to @self to another value not equal to @self. }
    if pVarHeader(arena)[-1].threadState <> @self then
    begin
      EnterCriticalSection(gs.lock);
      if Assigned(pVarHeader(arena)[-1].threadState) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy its threadState.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := IndexToSize(pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask) - CommonHeaderSize;
        pVarHeader(arena)[-1].threadState^.PushToFree(p);
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
        dec(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h);
        { Add arena to partialArenas[sizeIndex]. }
        nextArena := partialArenas[sizeIndex];
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

  function HeapInc.ThreadState.GetOSChunk(minSize, maxSize: SizeUint): pOSChunk;
{$if defined(FPC_HAS_FEATURE_THREADING) and not defined(HAS_SYSOSFREE)}
  var
    statv: SizeUint;
{$endif FPC_HAS_FEATURE_THREADING and not HAS_SYSOSFREE}
  begin
    result := freeOS.Get(minSize, maxSize);
    if Assigned(result) then
      exit;
  {$if defined(FPC_HAS_FEATURE_THREADING) and not defined(HAS_SYSOSFREE)}
    if Assigned(gs.freeOS.first) then { Racing precheck. }
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
        exit;
      end;
    end;
  {$endif FPC_HAS_FEATURE_THREADING and not HAS_SYSOSFREE}
    result := AllocateOSChunk(minSize, maxSize);
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
    fv: pFreeVarChunk;
    osChunk, osNext: pVarOSChunk;
    binIndex, vSizeFlags, statv: SizeUint;
    mask: uint32;
  begin
    size := (size + (VarHeaderSize + VarSizeQuant - 1)) and SizeUint(-VarSizeQuant);

  {$if (MaxFixedHeaderAndPayload - CommonHeaderSize + 1 + VarHeaderSize + VarSizeQuant - 1) div VarSizeQuant * VarSizeQuant < MinEmptyVarHeaderAndPayload}
    { Chunk will get freed one day. As a result, it might turn into a free chunk of the same size.
      Consequently, it must not be smaller than MinEmptyVarHeaderAndPayload.
      This can be a dead case depending on the constants, which is checked by the enclosing compile-time check. :)
      Also applies to TryResizeVar. }
    if size < MinEmptyVarHeaderAndPayload then
      size := MinEmptyVarHeaderAndPayload;
  {$endif}

  {$ifdef FPC_HAS_FEATURE_THREADING}
    if Assigned(toFree) then
      FlushToFree;
  {$endif}

    { Search varFree for (roughly) smallest chunk ≥ size. }
    binIndex := VarSizeToBinIndex(size, true);
    fv := varFree.bins[binIndex];
    osChunk := nil; { If remains nil, fv comes from varFree and must be removed. }
    if not Assigned(fv) then
    begin
      mask := varFree.L0[binIndex div L0BinSize] shr (binIndex mod L0BinSize); { Logically should be “1 + binIndex mod L0BinSize” but the bit that represents the binIndex-th bin is 0 anyway. }
      if mask <> 0 then
        fv := varFree.bins[binIndex + BsfDWord(NonZeroDWord(mask))]
      else
      begin
        mask := varFree.L1 and (SizeUint(-2) shl (binIndex div L0BinSize));
        if mask <> 0 then
        begin
          binIndex := BsfDWord(NonZeroDWord(mask)); { Index at L0. }
          fv := varFree.bins[binIndex * L0BinSize + BsfDWord(NonZeroDWord(varFree.L0[binIndex]))];
        end else
        begin
          { No such a chunk, allocate a new one. }
          osChunk := pVarOSChunk(GetOSChunk(VarOSChunkDataOffset + size, GrowHeapSize2));
          if not Assigned(osChunk) then
            exit(nil);

          { Add osChunk to varOS. }
          osNext := varOS;
          osChunk^.prev := nil;
          osChunk^.next := osNext;
          if Assigned(osNext) then
            osNext^.prev := osChunk;
          varOS := osChunk;

          { Format new free var chunk spanning the entire osChunk. }
          fv := pointer(osChunk) + (VarOSChunkDataOffset + VarHeaderSize);
          pVarHeader(pointer(fv) - VarHeaderSize)^.prevSize := 0;
        {$ifdef FPC_HAS_FEATURE_THREADING}
          pVarHeader(pointer(fv) - VarHeaderSize)^.threadState := @self;
        {$endif}
          pVarHeader(pointer(fv) - VarHeaderSize)^.ch.h := (uint32(osChunk^.size) - VarOSChunkDataOffset) and VarSizeMask + LastFlag;
        end;
      end;
    end;
    if not Assigned(osChunk) then
      varFree.Remove(fv);

    { Result will be allocated at the beginning of fv; maybe format the remainder and add it back to varFree. }
    result := fv;
    vSizeFlags := pVarHeader(fv)[-1].ch.h - size; { Inherits LastFlag. }
    if vSizeFlags >= MinEmptyVarHeaderAndPayload then { Logically “vSizeFlags and VarSizeMask” but here it’s okay to not mask. }
    begin
      inc(pointer(fv), size); { result = allocated block, fv = remainder. }
      pVarHeader(pointer(fv) - VarHeaderSize)^.prevSize := size;
    {$ifdef FPC_HAS_FEATURE_THREADING}
      pVarHeader(pointer(fv) - VarHeaderSize)^.threadState := @self;
    {$endif}
      pVarHeader(pointer(fv) - VarHeaderSize)^.ch.h := vSizeFlags;
      if vSizeFlags and LastFlag = 0 then
        pVarHeader(pointer(fv) + vSizeFlags - VarHeaderSize)^.prevSize := vSizeFlags; { All flags are 0. }

      varFree.Add(fv, VarSizeToBinIndex(vSizeFlags and VarSizeMask, false));

      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) + UsedFlag;
    end else
    begin
      { Use the entire chunk. }
      inc(vSizeFlags, size);
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(vSizeFlags) + UsedFlag;
      size := vSizeFlags and VarSizeMask;
    end;

    if isArena then
      inc(pVarHeader(result)[-1].ch.h, FixedArenaFlag) { Arenas aren’t counted in “used” directly. }
    else
    begin
      statv := used + size;
      used := statv;
      inc(statv, gs.hugeUsed);
      if statv > maxUsed then
        maxUsed := statv;
    end;
  end;

  function HeapInc.ThreadState.FreeVar(p: pointer): SizeUint;
  var
    p2: pointer;
    fSizeFlags, prevSize, hPrev, hNext: SizeUint;
    osChunk, osPrev, osNext: pVarOSChunk;
    freeOsNext: pFreeOSChunk;
  begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if pVarHeader(p - VarHeaderSize)^.threadState <> @self then
    begin
      EnterCriticalSection(gs.lock);
      if Assigned(pVarHeader(p - VarHeaderSize)^.threadState) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy its threadState.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask - VarHeaderSize;
        pVarHeader(p - VarHeaderSize)^.threadState^.PushToFree(p);
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

    { If next/prev are free, remove them from varFree and merge with f — (f)uture (f)ree chunk that starts at p, has fSizeFlags,
      and conveniently always inherits prevSize of its final location. }
    if fSizeFlags and LastFlag = 0 then
    begin
      p2 := p + result;
      hNext := pVarHeader(p2 - VarHeaderSize)^.ch.h;
      if uint32(hNext) and UsedFlag = 0 then
      begin
        inc(fSizeFlags, hNext); { Inherit LastFlag, other p2 flags must be 0. }
        varFree.Remove(p2);
      end;
    end;

    prevSize := pVarHeader(p - VarHeaderSize)^.prevSize;
    if prevSize <> 0 then
    begin
      p2 := p - prevSize;
      hPrev := pVarHeader(p2 - VarHeaderSize)^.ch.h;
      if uint32(hPrev) and UsedFlag = 0 then
      begin
        p := p2;
        inc(fSizeFlags, hPrev); { All p2 flags must be 0. }
        varFree.Remove(p2);
      end;
    end;

    { Turn p into a free chunk and add it back to varFree...
      unless it spans the entire OS chunk, in which case instead move the chunk from varOS to freeOS. }
    if (fSizeFlags and LastFlag = 0) or (pVarHeader(p - VarHeaderSize)^.prevSize <> 0) then
    begin
      dec(fSizeFlags, UsedFlag);
      if fSizeFlags and LastFlag = 0 then
        pVarHeader(p + fSizeFlags - VarHeaderSize)^.prevSize := fSizeFlags; { All fSizeFlags flags are 0. }

      pVarHeader(p - VarHeaderSize)^.ch.h := fSizeFlags;

      varFree.Add(p, VarSizeToBinIndex(fSizeFlags and VarSizeMask, false));
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

      { Instantly free if huge. }
    {$ifdef HAS_SYSOSFREE}
      if osChunk^.size > GrowHeapSize2 then
      begin
        dec(allocated, osChunk^.size);
        SysOSFree(osChunk, osChunk^.size);
      end else
    {$endif}
      begin
        { Add to freeOS. }
        freeOsNext := freeOS.first;
        osChunk^.prev := nil;
        osChunk^.next := freeOsNext;
        if Assigned(freeOsNext) then
          freeOsNext^.prev := osChunk
        else
          freeOS.last := pFreeOSChunk(osChunk);
        freeOS.first := pFreeOSChunk(osChunk);
      {$ifdef HAS_SYSOSFREE}
        inc(freeOS.n);
        if freeOS.n > MaxKeptOSChunks then
          dec(allocated, freeOS.FreeOne);
      {$endif}
      end;
    end;
    dec(result, VarHeaderSize);
  end;

  function HeapInc.ThreadState.TryResizeVar(p: pointer; size: SizeUint): pointer;
  var
    fp, p2: pointer;
    oldpsize, fSizeFlags, growby, statv: SizeUint;
  begin
    if (size <= MaxFixedHeaderAndPayload - CommonHeaderSize)
      or (size > GrowHeapSize2) { Not strictly necessary but rejects clearly wrong values early so adding headers to the size doesn’t overflow. }
    {$ifdef FPC_HAS_FEATURE_THREADING}
      or (pVarHeader(p - VarHeaderSize)^.threadState <> @self)
    {$endif}
    then
      exit(nil);
    size := (size + (VarHeaderSize + VarSizeQuant - 1)) and SizeUint(-VarSizeQuant);
  {$if (MaxFixedHeaderAndPayload - CommonHeaderSize + 1 + VarHeaderSize + VarSizeQuant - 1) div VarSizeQuant * VarSizeQuant < MinEmptyVarHeaderAndPayload}
    if size < MinEmptyVarHeaderAndPayload then
      size := MinEmptyVarHeaderAndPayload;
  {$endif}
    result := p; { From now on use result instead of p (saves a register). }

    oldpsize := pVarHeader(result - VarHeaderSize)^.ch.h and VarSizeMask;
    p2 := result + oldpsize;
    { (f)uture (f)ree chunk starting at p + size and having fSizeFlags will be created at the end, must exit before that if not required. }
    if size <= oldpsize then
    begin
      { Shrink. Maybe. }
      fSizeFlags := oldpsize - size;

      if (pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag <> 0) or (pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag <> 0) then
      begin
        { No empty chunk to the right: create free chunk if ≥ MinEmptyVarHeaderAndPayload, otherwise report success but change nothing. }
        if fSizeFlags < MinEmptyVarHeaderAndPayload then
          exit;
        dec(used, fSizeFlags);
        inc(fSizeFlags, pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag);
      end else
      begin
        if fSizeFlags = 0 then { Exit early if going to be a no-op. Branch above does the same with a broader check. }
          exit;
        dec(used, fSizeFlags);
        { Has empty chunk to the right: extend with freed space. }
        inc(fSizeFlags, pVarHeader(p2 - VarHeaderSize)^.ch.h); { Adds size and last flag, other bits are 0. }
        varFree.Remove(p2);
      end;

      { Update p size. }
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) + UsedFlag;
    end
    { Grow if there is free space. Note this can result in a chunk larger than e.g. SysGetMem allows (GrowHeapSize div 2 or so). That’s okay as it saves a Move. }
    else if (pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag = 0) and (pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag = 0) and
      (pVarHeader(p2)[-1].ch.h >= SizeUint(size - oldpsize)) { Can check without “and VarSizeMask”, will remain ≥ anyway. }
    then
    begin
      fSizeFlags := pVarHeader(p2)[-1].ch.h - (size - oldpsize); { Inherits LastFlag, other flags are 0. }
      if fSizeFlags < MinEmptyVarHeaderAndPayload then
        fSizeFlags := fSizeFlags and LastFlag;

      growby := pVarHeader(p2)[-1].ch.h - fSizeFlags;
      size := oldpsize + growby;
      statv := used + growby;
      used := statv;
      inc(statv, gs.hugeUsed);
      if statv > maxUsed then
        maxUsed := statv;

      varFree.Remove(p2);
      { Update p size. }
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) + UsedFlag;
      { No empty chunk? }
      if fSizeFlags <= LastFlag then
      begin
        inc(pVarHeader(result - VarHeaderSize)^.ch.h, fSizeFlags); { Either += LastFlag or a no-op. }
        if fSizeFlags = 0 then { logically “and LastFlag = 0” }
          pVarHeader(result + size - VarHeaderSize)^.prevSize := size;
        exit;
      end;
    end else
      { Possible another case to handle: on growth, if there is no space to the right but there is space to the LEFT, move the data there, avoiding the GetMem + FreeMem.
        Probably not common enough, but I didn’t even investigate. }
      exit(nil);

    { Format new free var chunk. }
    fp := result + size;
    pVarHeader(fp - VarHeaderSize)^.prevSize := size;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    pVarHeader(fp - VarHeaderSize)^.threadState := @self;
  {$endif}
    pVarHeader(fp - VarHeaderSize)^.ch.h := fSizeFlags;
    if fSizeFlags and LastFlag = 0 then
      pVarHeader(fp + fSizeFlags - VarHeaderSize)^.prevSize := fSizeFlags; { All flags are 0. }
    varFree.Add(fp, VarSizeToBinIndex(fSizeFlags and VarSizeMask, false));
  end;

  { If SysOSFree is available, huge chunks aren’t cached by any means.
    If SysOSFree is not available, there’s no choice but to cache them.
    Caching is done directly into gs.freeOS if FPC_HAS_FEATURE_THREADING, otherwise ThreadState.freeOS. }

  function HeapInc.ThreadState.AllocHuge(size: SizeUint): pointer;
  var
    userSize, hugeUsed: SizeUint;
  begin
    userSize := size;
    size := (size + (HugeChunkDataOffset + CommonHeaderSize + OSChunkVarSizeQuant - 1)) and SizeUint(-OSChunkVarSizeQuant);
    if size < userSize then { Overflow. }
      exit(AllocFailed);
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if Assigned(toFree) then
      FlushToFree;
  {$endif}
  {$ifdef HAS_SYSOSFREE}
    result := SysOSAlloc(size);
    if not Assigned(result) then
      exit(AllocFailed);
    pHugeChunk(result)^.size := size;
  {$else HAS_SYSOSFREE}
    result := GetOSChunk(size, High(SizeUint));
    if not Assigned(result) then
      exit; { GetOSChunk throws an error if required. }
    size := pOSChunk(result)^.size;
    dec(allocated, size); { After GetOSChunk chunk size is counted in “allocated”; don’t count. }
  {$endif HAS_SYSOSFREE}
    pCommonHeader(result + HugeChunkDataOffset)^.h := HugeHeader;
    inc(result, HugeChunkDataOffset + CommonHeaderSize);
  {$ifdef FPC_HAS_FEATURE_THREADING} EnterCriticalSection(gs.lock); {$endif}
    hugeUsed := gs.hugeUsed + size;
    gs.hugeUsed := hugeUsed;
  {$ifdef FPC_HAS_FEATURE_THREADING} LeaveCriticalSection(gs.lock); {$endif}
    UpdateMaxStats(hugeUsed);
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
  {$ifdef FPC_HAS_FEATURE_THREADING} EnterCriticalSection(gs.lock); {$endif}
    dec(gs.hugeUsed, result);
  {$ifndef HAS_SYSOSFREE} { But you’d better have SysOSFree... }
  {$ifdef FPC_HAS_FEATURE_THREADING}
    fOs := @gs.freeOS; { gs.freeOS aren’t counted anywhere (for now). }
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
  {$endif ndef HAS_SYSOSFREE}
  {$ifdef FPC_HAS_FEATURE_THREADING} LeaveCriticalSection(gs.lock); {$endif}
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
    {$ifdef FPC_HAS_FEATURE_THREADING} EnterCriticalSection(gs.lock); {$endif}
      gs.hugeUsed := gs.hugeUsed - oldSize + size;
    {$ifdef FPC_HAS_FEATURE_THREADING} LeaveCriticalSection(gs.lock); {$endif}
      if size > oldSize then
        UpdateMaxStats(gs.hugeUsed);
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
    next: pFreeChunk;
  begin
    repeat
      next := toFree;
      p^.next := next;
      WriteBarrier; { Write p after p^.next. }
    until InterlockedCompareExchange(toFree, p, next) = next;
  end;

  procedure HeapInc.ThreadState.FlushToFree;
  var
    tf, nx: pFreeChunk;
  begin
    tf := InterlockedExchange(toFree, nil);
    while Assigned(tf) do
    begin
      ReadDependencyBarrier; { Read toFree^.next after toFree. }
      nx := tf^.next;
      SysFreeMem(tf);
      tf := nx;
    end;
  end;

  procedure HeapInc.ThreadState.Orphan;
  var
    arena: pFixedArena;
    vOs, nextVOs, lastVOs: pVarOSChunk;
  {$ifndef HAS_SYSOSFREE}
    lastFree, nextFree: pFreeOSChunk;
  {$endif not HAS_SYSOSFREE}
  begin
    if gs.lockUse > 0 then
      EnterCriticalSection(HeapInc.gs.lock);
    FlushToFree; { Performing it under gs.lock guarantees there will be no new toFree requests. }

    { Has to free all empty arenas, otherwise the chunk that contains only empty arenas will leak (no one will ever adopt it, as it has nothing to free). }
    while nEmptyArenas > 0 do
    begin
      arena := emptyArenas;
      emptyArenas := arena^.next;
      dec(nEmptyArenas);
      FreeVar(arena);
    end;

{$ifndef HAS_SYSOSFREE}
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
    end;
{$endif not HAS_SYSOSFREE}
    { Prepend varOS to gs.varOS. }
    vOs := varOS;
    if Assigned(vOs) then
    begin
      nextVOs := gs.varOS;
      gs.varOS := vOs;
      repeat
        lastVOs := vOs;
        ChangeThreadState(vOs, nil);
        vOs := vOs^.next;
      until not Assigned(vOs);
      lastVOs^.next := nextVOs;
      if Assigned(nextVOs) then
        nextVOs^.prev := lastVOs;
    end;
    if gs.lockUse > 0 then
      LeaveCriticalSection(gs.lock);

{$ifdef HAS_SYSOSFREE}
    freeOS.FreeAll; { Does not require gs.lock. }
{$endif HAS_SYSOSFREE}

    { Zeroing is probably required, because Orphan is called from FinalizeHeap which is called from DoneThread which can be called twice, according to this comment from syswin.inc: }
    // DoneThread; { Assume everything is idempotent there }
    FillChar(self, sizeof(self), 0);
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
      inc(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h);
  end;

  procedure HeapInc.ThreadState.AdoptVarOwner(p: pointer);
  var
    prevSize, statv: SizeUint;
    h: uint32;
    vOs, osPrev, osNext: pVarOSChunk;
  begin
    repeat
      prevSize := pVarHeader(p - VarHeaderSize)^.prevSize;
      dec(p, prevSize);
    until prevSize = 0;

    { Move OS chunk from gs.varOS to varOS. }
    vOs := p - (VarOSChunkDataOffset + VarHeaderSize);
    osPrev := vOs^.prev;
    osNext := vOs^.next;
    if Assigned(osPrev) then
      osPrev^.next := osNext
    else
      gs.varOS := osNext;
    if Assigned(osNext) then
      osNext^.prev := osPrev;
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

    repeat
      pVarHeader(p - VarHeaderSize)^.threadState := @self;
      h := pVarHeader(p - VarHeaderSize)^.ch.h;
      if h and UsedFlag = 0 then
        varFree.Add(p, pFreeVarChunk(p)^.binIndex)
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

  class procedure HeapInc.ThreadState.ChangeThreadState(vOs: pVarOSChunk; ts: pThreadState);
  var
    h: uint32;
    p: pointer;
  begin
    p := pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize);
    repeat
      pVarHeader(p - VarHeaderSize)^.threadState := ts;
      h := pVarHeader(p - VarHeaderSize)^.ch.h;
      inc(p, h and VarSizeMask);
    until h and LastFlag <> 0;
  end;

{$ifndef FPC_SECTION_THREADVARS}
  procedure HeapInc.ThreadState.FixupSelfPtr;
  var
    vOs: pVarOSChunk;
  begin
    vOs := varOS;
    while Assigned(vOs) do
    begin
      ChangeThreadState(vOs, @self);
      vOs := vOs^.next;
    end;
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
    result := ts^.AllocFixed(size)
  else if (size < GrowHeapSize2 div 2) { Approximate idea on the max size of the variable chunk. Approximate because size does not include headers but GrowHeapSize2 does. }
    and (size < HeapInc.MaxVarHeaderAndPayload - HeapInc.VarHeaderSize) then
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

function SysTryResizeMem(var p: pointer; size: ptruint): boolean;
var
  ts: HeapInc.pThreadState;
  h: uint32;
  newp: pointer;
begin
  h := HeapInc.pCommonHeader(p - HeapInc.CommonHeaderSize)^.h;
  if h and HeapInc.FixedFlag <> 0 then
    { Don’t shrink fixed chunk. }
    result := size <= SizeUint(HeapInc.IndexToSize(h and HeapInc.SizeIndexMask) - HeapInc.CommonHeaderSize)
  else
  begin
    ts := @HeapInc.thisTs;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if Assigned(ts^.toFree) then
      ts^.FlushToFree;
  {$endif FPC_HAS_FEATURE_THREADING}
    if h <> HeapInc.HugeHeader then
      newp := ts^.TryResizeVar(p, size)
    else
      newp := ts^.TryResizeHuge(p, size);
    result := Assigned(newp);
    if result then
      p := newp;
  end;
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
  oldsize, newsize, tocopy: SizeUint;
begin
  if size = 0 then
    begin
      SysFreeMem(p);
      result := nil;
      p := nil;
    end
  else if not Assigned(p) then
    begin
      result := SysGetMem(size);
      p := result;
    end
  else if SysTryResizeMem(p, size) then
    result := p
  else
    begin
      oldsize := SysMemSize(p);
      newsize := size;
      result := SysGetMem(newsize);
      if not Assigned(result) then
        begin
          if size <= oldsize then
            { Don’t fail if shrinking. }
            result := p;
          exit; { If growing failed, return nil, but keep the old p. }
        end;
      tocopy := oldsize;
      if tocopy > newsize then
        tocopy := newsize;
      Move(p^, result^, tocopy);
      SysFreeMem(p);
      p := result;
    end;
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

{$ifndef FPC_NO_DEFAULT_HEAP}
{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
{$ifdef FPC_HAS_FEATURE_THREADING}
procedure InitHeapThread;
begin
  if HeapInc.gs.lockUse>0 then
    InterlockedIncrement(HeapInc.gs.lockUse);
end;
{$endif}

procedure InitHeap; public name '_FPC_InitHeap';
begin
  { we cannot initialize the locks here yet, thread support is
    not loaded yet }
end;

procedure RelocateHeap;
begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  if HeapInc.gs.lockUse > 0 then
    exit;
  HeapInc.gs.lockUse := 1;
  InitCriticalSection(HeapInc.gs.lock);
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
  if (HeapInc.gs.lockUse > 0) and (InterlockedDecrement(HeapInc.gs.lockUse) = 0) then
    DoneCriticalSection(HeapInc.gs.lock);
{$elseif defined(HAS_SYSOSFREE)}
  HeapInc.thisTs.freeOS.FreeAll;
{$endif FPC_HAS_FEATURE_THREADING | defined(HAS_SYSOSFREE)}
end;

{$endif ndef FPC_NO_DEFAULT_HEAP}

{$endif ndef HAS_MEMORYMANAGER and (defined(FPC_HAS_FEATURE_HEAP) or defined(FPC_IN_HEAPMGR))}
