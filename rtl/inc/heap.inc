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


{ Try to find the best matching block in general freelist }
{ define BESTMATCH}

{$ifndef FPC_NO_DEFAULT_MEMORYMANAGER}

{$endif HAS_MEMORYMANAGER}

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

{$if defined(FPC_HAS_FEATURE_HEAP) or defined(FPC_IN_HEAPMGR)}
{$ifndef HAS_MEMORYMANAGER}

{
  We use 'fixed' size chunks for small allocations,
  and os chunks with variable sized blocks for big
  allocations.

  * a block is an area allocated by user
  * a chunk is a block plus our bookkeeping
  * an os chunk is a collection of chunks

  Memory layout:
    fixed:                 < CommonHeader >   [ ... user data ... ]
    variable:  [ VarHeader < CommonHeader > ] [ ... user data ... ]

  When all chunks in an os chunk are free, we keep a few around
  but otherwise it will be freed to the OS.
}

type
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
    OSChunkFixedSizeQuant = 32 * 1024;
    OSChunkVarSizeQuant = 64 * 1024;
    MaxFixedChunkSize = 256 * 1024;

    { Variable freelist search strategy: -1 = exhaustive search for the best (smallest fitting) match, ≥0 = search this many after the first match. }
    MatchEffort = {$ifdef BESTMATCH} -1 {$else} 10 {$endif};

    { Limit on shrinking variable chunks and keeping the tail when splitting the chunk in AllocVar / TryResizeVar. }
    MinVarHeaderAndPayload = MaxFixedHeaderAndPayload * 3 div 4;

  { Adjustable part ends here~ }

  const
    SizeIndexBits = 1 + trunc(ln(FixedSizesCount - 1) /  ln(2));
    SizeIndexMask = 1 shl SizeIndexBits - 1;
    FixedBitPos = {$if SizeIndexBits >= 3} SizeIndexBits {$else} 3 {$endif}; { Variable chunks use 3 low bits for used / first / last. }
    FixedFlag = 1 shl FixedBitPos;
    ChunkOffsetShift = FixedBitPos + 1;

    { Not really used; MaxFixedChunkSize limit on fixed OS chunks assumed to be strictly enforced and (much!) more restricting than MaxChunkOffset.
      MaxFixedChunkSize = 256 Kb.
      MaxChunkOffset ~ 2^(32 - 6) ~ 67 Mb.
      Indices could be stored instead, but offsets avoid multiplications. }
    MaxChunkOffset = High(uint32) shr ChunkOffsetShift;

    UsedFlag = 1 shl 0;
    FirstFlag = 1 shl 1;
    LastFlag = 1 shl 2;
    VarSizeQuant = 1 shl ChunkOffsetShift; {$if VarSizeQuant < Alignment} {$error Assumed to be >= Alignment.} {$endif}
    VarSizeMask = SizeUint(-VarSizeQuant);

  type
    { Common header of any memory chunk, residing immediately to the left of the ~payload~ (block).

      Fixed chunk header, assuming SizeIndexBits = 4:
      h[0:3] = size index (= h and SizeIndexMask)
      h[4] = 1 (h and FixedFlag <> 0)
      h[5:31] — offset in the OS chunk (= h shr ChunkOffsetShift)

      Variable chunk header, assuming SizeIndexBits = 4:
      h[0] = used flag (h and UsedFlag <> 0)
      h[1] = first flag (h and FirstFlag <> 0)
      h[2] = last flag (h and LastFlag <> 0)
      h[3] = unused
      h[4] = 0 (h and FixedFlag = 0)
      h[5:31] = size, rounded up to 32 (VarSizeQuant), shr 5; in other words, size = h and VarSizeMask.

      If sizeof(SizeUint) > 4: “h and VarSizeMask” is low part of size, high part is stored in VarChunk.sizeHi. }

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

    pOSChunk = ^OSChunk;
    OSChunk = object { Common header for all OS chunks. }
      size: SizeUint; { Full size asked from SysOSAlloc. }
    {$ifdef FPC_HAS_FEATURE_THREADING}
      threadState: pThreadState; { Protected with gs.lock. Nil if orphaned. }
    {$endif}
      prev, next: pointer; { pOSChunk, but used for different subtypes. }

      { For what purpose this chunk was used the last time. -1 — var, N ≥ 0 — fixed size N. Allows for a small optimization when reusing fixed chunks. }
      sizeIndex: SizeInt;

      { Remove from list src and push to list dest. }
      procedure MoveTo(var src, dest: pOSChunk);
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

    pFixedOSChunk = ^FixedOSChunk;
    FixedOSChunk = object(OSChunk)
      { Data starts at FixedOSChunkDataOffset and spans for “maxSize” (virtual value, does not exist directly) bytes, of which:
        — first formattedSize are either allocated (“used”; counted in usedSize) or in the freelist (firstFreeChunk; size = formattedSize - usedSize),
        — the rest “maxSize” - formattedSize are yet unallocated space.

        This design, together with tracking free chunks per FixedOSChunk rather than per fixed size, trivializes reusing the chunk.
        Chopping all available space at once would get rid of the “unallocated space” entity, but is a lot of potentially wasted work:
        https://gitlab.com/freepascal.org/fpc/source/-/issues/40447.

        Values are multiples of the chunk size instead of counts (could be chunksUsed, chunksFormatted, chunksMax) to save on multiplications.
        Moreover, instead of “maxSize” from the explanation above, fullThreshold is used, which is such a value that the chunk is full if usedSize >= fullThreshold.
        maxSize = RoundUp(fullThreshold, chunk size).
        Reason is, calculating fullThreshold does not require division. }

      firstFreeChunk: pFreeChunk;
      usedSize, formattedSize, fullThreshold: uint32;
    end;

    pVarOSChunk = ^VarOSChunk;
    VarOSChunk = object(OSChunk)
    end;

    pVarHeader = ^VarHeader;
    VarHeader = record
      prevSize: SizeUint; { Always 0 for the first chunk. }
    {$ifdef FPC_HAS_FEATURE_THREADING}
      threadState: pThreadState;
    {$endif}
    {$if sizeof(SizeUint) > 4}
      sizeHi: uint32;
    {$endif}
      { Assumed to indeed match chunk’s CommonHeader, i.e. that there is no padding after this field.
        Otherwise must be accessed as pCommonHeader(pointer(varHdr) + (VarHeaderSize - CommonHeaderSize))^ :D. }
      ch: CommonHeader;
    end;

    { Reuses the payload of variable chunks whose ch.h and UsedFlag = 0, so variable chunk payload must always fit its size. }
    pFreeVarChunk = ^FreeVarChunk;
    FreeVarChunk = record
      prev, next: pFreeVarChunk;
      size: SizeUint; { Cached size for easier access when working with free chunks, always equals to header.sizeHi shl 32 or header.ch.h and VarSizeMask. }
    end;

    ThreadState = object
      fullOS: pFixedOSChunk; { Completely filled fixed OS chunks. }
      freeOS: FreeOSChunkList; { Completely empty OS chunks. }
    {$ifdef FPC_HAS_FEATURE_THREADING}
      toFree: pFreeChunk; { Free requests from other threads, atomic. }
    {$endif}

      used, maxUsed, allocated, maxAllocated: SizeUint; { Statistics. }

      varOS: pVarOSChunk;
      varFree: pFreeVarChunk;

      { OS chunks with at least 1 free chunk (including unformatted space), but not completely empty.
        OS chunks that become completely empty are moved to freeOS, completely full are moved to fullOS. }
      fixedPartialOS: array[0 .. FixedSizesCount - 1] of pFixedOSChunk;

      { Only to calculate preferable new fixed OS chunk sizes...
        (Updated infrequently, as opposed to possible “fixedUsed”. When a new fixed OS chunk is required, all existing chunks of its size are full.) }
      allocatedByFullFixed: array[0 .. FixedSizesCount - 1] of SizeUint;

    {$ifdef DEBUG_HEAP_INC}
      procedure Dump(var f: text);
    {$endif}

      function AllocFixed(size: SizeUint): pointer;
      function FreeFixed(p: pointer): SizeUint;

      function GetOSChunk(minSize, maxSize: SizeUint; sizeIndex: SizeInt): pOSChunk;
      function AllocateOSChunk(minSize: SizeUint; sizeIndex: SizeInt): pOSChunk;

      function AllocVar(size: SizeUint): pointer;
      function FreeVar(p: pointer): SizeUint;
      function TryResizeVar(p: pointer; size: SizeUint): pointer;
    {$ifdef HAS_SYSOSREALLOC}
      function TrySysOSRealloc(p: pointer; oldSize, newSize: SizeUint): pointer;
    {$endif}

    {$ifdef FPC_HAS_FEATURE_THREADING}
      procedure PushToFree(p: pFreeChunk);
      procedure FlushToFree;

      procedure Orphan; { Must be performed under gs.lock. }
      class procedure Orphan(list: pFixedOSChunk); static;
      procedure Adopt(osChunk: pFixedOSChunk); { Must be performed under gs.lock. }
      procedure AdoptVarOwner(p: pointer); { Adopts the OS chunk that contains p. Must be performed under gs.lock. }

      class function ChangeThreadStates(list: pOSChunk; ts: pThreadState): pOSChunk; static; { Returns the last item of the list. }
      class procedure ChangeThreadState(vOs: pVarOSChunk; ts: pThreadState); static;

    {$ifndef FPC_SECTION_THREADVARS}
      procedure FixupSelfPtr;
    {$endif ndef FPC_SECTION_THREADVARS}
    {$endif FPC_HAS_FEATURE_THREADING}
    end;

  {$ifdef FPC_HAS_FEATURE_THREADING}
    GlobalState = record
      lock: TRTLCriticalSection;
      lockUse: int32;

      { Data from dead threads (“orphaned”), protected by gs.lock. }
      fixedOS: pFixedOSChunk;
      freeOS: FreeOSChunkList;
      varOS: pVarOSChunk;
    end;
  {$endif FPC_HAS_FEATURE_THREADING}

{$ifdef FPC_HAS_FEATURE_THREADING}
  class var
    gs: GlobalState;
  class threadvar
    thisTs: ThreadState;
{$else FPC_HAS_FEATURE_THREADING}
  class var
    thisTs: ThreadState;
{$endif FPC_HAS_FEATURE_THREADING}

  const
    CommonHeaderSize = sizeof(CommonHeader);
  {$if MinFixedHeaderAndPayload < CommonHeaderSize + sizeof(FreeChunk)} {$error MinFixedHeaderAndPayload does not fit CommonHeader + FreeChunk.} {$endif}
    FixedOSChunkDataOffset = (sizeof(FixedOSChunk) + CommonHeaderSize + Alignment - 1) and -Alignment - CommonHeaderSize;
    VarHeaderSize = sizeof(VarHeader);
    VarOSChunkDataOffset = (sizeof(VarOSChunk) + VarHeaderSize + Alignment - 1) and -Alignment - VarHeaderSize;
    MaxVarPayload = High(SizeUint) - (VarOSChunkDataOffset + VarHeaderSize + OSChunkVarSizeQuant); { Absolute limit on chunk sizes. }
  end;

  class function HeapInc.SizeMinus1ToIndex(sizeMinus1: SizeUint): SizeUint;
  begin
    result := SizeMinus1Div16ToIndex[sizeMinus1 div 16];
  end;

  class function HeapInc.IndexToSize(sizeIndex: SizeUint): SizeUint;
  begin
    result := FixedSizes[sizeIndex];
  end;

  procedure HeapInc.OSChunk.MoveTo(var src, dest: pOSChunk);
  var
    osNext, osPrev: pOSChunk;
  begin
    osPrev := prev;
    osNext := next;
    if Assigned(osPrev) then
      osPrev^.next := osNext
    else
      src := osNext;
    if Assigned(osNext) then
      osNext^.prev := osPrev;
    prev := nil;
    osNext := dest;
    next := osNext;
    if Assigned(osNext) then
      osNext^.prev := @self;
    dest := @self;
  end;

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

{$ifdef DEBUG_HEAP_INC}
  procedure HeapInc.ThreadState.Dump(var f: text);
  var
    i: SizeInt;
    fix: pFixedOSChunk;
    fr: pFreeOSChunk;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    tf: pFreeChunk;
  {$endif}
    vf: pFreeVarChunk;
    vOs: pVarOSChunk;
    p: pointer;
  begin
    writeln(f, 'used = ', used, ', allocated = ', allocated, ', maxUsed = ', maxUsed, ', maxAllocated = ', maxAllocated);
    fix := fullOS;
    if Assigned(fix) then
    begin
      writeln(f);
      repeat
        writeln(f, 'Full fixed: size = ', fix^.size, ', usedSize = ', fix^.usedSize, ', formattedSize = ', fix^.formattedSize, ', fullThreshold = ', fix^.fullThreshold);
        fix := fix^.next;
      until not Assigned(fix);
    end;
    for i := 0 to FixedSizesCount - 1 do
      if Assigned(fixedPartialOS[i]) then
      begin
        write(f, 'Size #', i, ' (', IndexToSize(i), ')');
        if allocatedByFullFixed[i] <> 0 then
          write(f, ': allocatedByFullFixed = ', allocatedByFullFixed[i]);
        writeln(f);
        fix := fixedPartialOS[i];
        while Assigned(fix) do
        begin
          writeln(f, 'size = ', fix^.size, ', usedSize = ', fix^.usedSize, ', formattedSize = ', fix^.formattedSize, ', fullThreshold = ', fix^.fullThreshold);
          fix := fix^.next;
        end;
      end;
    vOs := varOS;
    while Assigned(vOs) do
    begin
      writeln(f, LineEnding, 'Var OS chunk, size ', vOs^.size);
      p := pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize);
      repeat
        write(f, HexStr(p), ': ',
          'prevSize = ', pVarHeader(p - VarHeaderSize)^.prevSize, ', size = ',
        {$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(p - VarHeaderSize)^.sizeHi) shl 32 or {$endif} pVarHeader(p - VarHeaderSize)^.ch.h and VarSizeMask);
        if pVarHeader(p - VarHeaderSize)^.ch.h and UsedFlag <> 0 then
          write(f, ', used')
        else
          write(f, ', f r e e');
        if pVarHeader(p - VarHeaderSize)^.ch.h and FirstFlag <> 0 then
          write(f, ', first');
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          write(f, ', last');
        writeln(f);
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          break;
        p := p + ({$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(p - VarHeaderSize)^.sizeHi) shl 32 or {$endif} pVarHeader(p - VarHeaderSize)^.ch.h and uint32(VarSizeMask));
      until false;
      vOs := vOs^.next;
    end;
    fr := freeOS.first;
    if Assigned(fr) then
    begin
      writeln(f);
      repeat
        writeln(f, 'Free OS: ', HexStr(fr), ', size = ', fr^.size);
        fr := fr^.next;
      until not Assigned(fr);
    end;
    vf := varFree;
    if Assigned(vf) then
    begin
      write(f, LineEnding, 'Var free:');
      repeat
        write(f, ' ', vf^.size);
        vf := vf^.next;
      until not Assigned(vf);
      writeln(f);
    end;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    tf := toFree;
    if Assigned(tf) then
    begin
      write(f, LineEnding, 'To-free:');
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

  function HeapInc.ThreadState.AllocFixed(size: SizeUint): pointer;
  var
    sizeIndex: SizeUint;
    osChunk, osNext: pFixedOSChunk;
  begin
    sizeIndex := SizeMinus1ToIndex(size + (CommonHeaderSize - 1));

    osChunk := fixedPartialOS[sizeIndex];
    if not Assigned(osChunk) then
    begin
    {$ifdef FPC_HAS_FEATURE_THREADING}
      if Assigned(toFree) then
      begin
        FlushToFree;
        osChunk := fixedPartialOS[sizeIndex];
      end;
      if not Assigned(osChunk) then
    {$endif FPC_HAS_FEATURE_THREADING}
      begin
        osChunk := pFixedOSChunk(GetOSChunk(FixedOSChunkDataOffset + MaxFixedHeaderAndPayload, MaxFixedChunkSize, sizeIndex));
        if not Assigned(osChunk) then
          exit(nil);
        if SizeUint(osChunk^.sizeIndex) = sizeIndex then
          { Lucky! Just don’t reset the chunk and use its old freelist. }
        else
        begin
          osChunk^.sizeIndex := sizeIndex;
          osChunk^.firstFreeChunk := nil;
          osChunk^.usedSize := 0;
          osChunk^.formattedSize := 0;
          osChunk^.fullThreshold := osChunk^.size - IndexToSize(sizeIndex) - (FixedOSChunkDataOffset - 1); { available OS chunk space - chunk size + 1. }
        end;

        { Add osChunk to fixedPartialOS[sizeIndex]. }
        osNext := fixedPartialOS[sizeIndex];
        osChunk^.prev := nil;
        osChunk^.next := osNext;
        if Assigned(osNext) then
          osNext^.prev := osChunk;
        fixedPartialOS[sizeIndex] := osChunk;
      end;
    end;

    size := IndexToSize(sizeIndex);
    inc(used, size);
    if used > maxUsed then
      maxUsed := used;

    { osChunk from the fixedPartialOS list has either free chunk or free unformatted space for a new chunk. }
    result := osChunk^.firstFreeChunk;
    if Assigned(result) then
      osChunk^.firstFreeChunk := pFreeChunk(result)^.next
    else
    begin
      result := pointer(osChunk) + (FixedOSChunkDataOffset + CommonHeaderSize) + osChunk^.formattedSize;
      pCommonHeader(result - CommonHeadersize)^.h := sizeIndex + osChunk^.formattedSize shl ChunkOffsetShift +
        (FixedFlag + (FixedOSChunkDataOffset + CommonHeaderSize) shl ChunkOffsetShift); { ← const }
      inc(osChunk^.formattedSize, size);
    end;
    inc(osChunk^.usedSize, size);
    if osChunk^.usedSize >= osChunk^.fullThreshold then
    begin
      inc(allocatedByFullFixed[sizeIndex], osChunk^.size);
      { Remove osChunk from fixedPartialOS[sizeIndex], add to fullOS. }
      osNext := osChunk^.next;
      fixedPartialOS[sizeIndex] := osNext;
      if Assigned(osNext) then
        osNext^.prev := nil;
      osNext := fullOS;
      osChunk^.next := osNext; { osChunk^.prev is already nil because osChunk was the first item of fixedPartialOS[sizeIndex]. }
      if Assigned(osNext) then
        osNext^.prev := osChunk;
      fullOS := osChunk;
    end;
  end;

  function HeapInc.ThreadState.FreeFixed(p: pointer): SizeUint;
  var
    sizeIndex, usedSize: SizeUint;
    osChunk, osPrev, osNext: pFixedOSChunk;
    freeOsNext: pFreeOSChunk;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    chunkTs: pThreadState;
  {$endif}
  begin
    osChunk := p - pCommonHeader(p - CommonHeaderSize)^.h shr ChunkOffsetShift;

  {$ifdef FPC_HAS_FEATURE_THREADING}
    { This can be checked without blocking; osChunk^.threadState can only change from one value not equal to @self to another value not equal to @self. }
    if osChunk^.threadState <> @self then
    begin
      EnterCriticalSection(gs.lock);
      chunkTs := osChunk^.threadState;
      if Assigned(chunkTs) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy chunkTs.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := IndexToSize(pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask) - CommonHeaderSize;
        chunkTs^.PushToFree(p);
        LeaveCriticalSection(gs.lock);
        exit;
      end;
      Adopt(osChunk); { ...And continue! }
      LeaveCriticalSection(gs.lock);
    end;
  {$endif FPC_HAS_FEATURE_THREADING}

    pFreeChunk(p)^.next := osChunk^.firstFreeChunk;
    osChunk^.firstFreeChunk := p;
    sizeIndex := pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask;
    result := IndexToSize(sizeIndex);
    dec(used, result);
    usedSize := osChunk^.usedSize;
    if usedSize >= osChunk^.fullThreshold then
    begin
      dec(allocatedByFullFixed[sizeIndex], osChunk^.size);

      { Remove osChunk from fullOS, add to fixedPartialOS[sizeIndex]. }
      osPrev := osChunk^.prev;
      osNext := osChunk^.next;
      if Assigned(osPrev) then
        osPrev^.next := osNext
      else
        fullOS := osNext;
      if Assigned(osNext) then
        osNext^.prev := osPrev;
      osChunk^.prev := nil;
      osNext := fixedPartialOS[sizeIndex];
      osChunk^.next := osNext;
      if Assigned(osNext) then
        osNext^.prev := osChunk;
      fixedPartialOS[sizeIndex] := osChunk;
    end;
    dec(usedSize, result);
    osChunk^.usedSize := usedSize;
    if usedSize = 0 then
    begin
      { Remove osChunk from fixedPartialOS[sizeIndex], add to freeOS. }
      osPrev := osChunk^.prev;
      osNext := osChunk^.next;
      if Assigned(osPrev) then
        osPrev^.next := osNext
      else
        fixedPartialOS[sizeIndex] := osNext;
      if Assigned(osNext) then
        osNext^.prev := osPrev;

      freeOsNext := freeOS.first;
      pFreeOSChunk(osChunk)^.prev := nil;
      pFreeOSChunk(osChunk)^.next := freeOsNext;
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
    dec(result, CommonHeaderSize);
  end;

  function HeapInc.ThreadState.GetOSChunk(minSize, maxSize: SizeUint; sizeIndex: SizeInt): pOSChunk;
  begin
    result := freeOS.Get(minSize, maxSize);
    if Assigned(result) then
      exit;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if Assigned(gs.freeOS.first) then { Racing precheck. }
    begin
      EnterCriticalSection(gs.lock);
      result := gs.freeOS.Get(minSize, maxSize);
      LeaveCriticalSection(gs.lock);
      if Assigned(result) then
      begin
        result^.threadState := @self;
        inc(allocated, result^.size);
        if allocated > maxAllocated then
          maxAllocated := allocated;
        exit;
      end;
    end;
  {$endif FPC_HAS_FEATURE_THREADING}
    result := AllocateOSChunk(minSize, sizeIndex);
  end;

  function HeapInc.ThreadState.AllocateOSChunk(minSize: SizeUint; sizeIndex: SizeInt): pOSChunk;
  var
    preferredSize: SizeUint;
  begin
    if sizeIndex < 0 then
    begin
      if minSize <= GrowHeapSize1 then { 256K by default. }
        preferredSize := GrowHeapSize1
      else if minSize <= GrowHeapSize2 then { 1M by default. }
        preferredSize := GrowHeapSize2
      else
        preferredSize := (minSize + (OSChunkVarSizeQuant - 1)) and SizeUint(-OSChunkVarSizeQuant);
    end else
    begin
      preferredSize := (allocatedByFullFixed[sizeIndex] div 8 + (OSChunkFixedSizeQuant - 1)) and SizeUint(-OSChunkFixedSizeQuant); { 12.5% of memory allocated by the size. }
      if preferredSize < growheapsizesmall then
        preferredSize := growheapsizesmall;
      if preferredSize > MaxFixedChunkSize then
        preferredSize := MaxFixedChunkSize;
    end;
    result := SysOSAlloc(preferredSize);
    if not Assigned(result) and (preferredSize > minSize) then
    begin
      preferredSize := minSize;
      result := SysOSAlloc(preferredSize);
    end;
    if not Assigned(result) then
      if ReturnNilIfGrowHeapFails then
        exit
      else
        HandleError(204);
    inc(allocated, preferredSize);
    if allocated > maxAllocated then
      maxAllocated := allocated;
    result^.size := preferredSize;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    result^.threadState := @self;
  {$endif}
    result^.sizeIndex := -2; { Neither −1 nor ≥0. }
  end;

  function HeapInc.ThreadState.AllocVar(size: SizeUint): pointer;
  var
    fv, fv2: pFreeVarChunk;
    osChunk, osNext: pVarOSChunk;
    varPrev, varNext: pFreeVarChunk;
    vSize, minSize, maxSize: SizeUint;
  {$if MatchEffort >= 0} fv2Size: SizeUint; {$endif}
  {$if MatchEffort > 1} triesLeft: uint32; {$endif}
  begin
    if size > MaxVarPayload then
      if ReturnNilIfGrowHeapFails then
        exit(nil)
      else
        HandleError(204);
    size := (size + (VarHeaderSize + VarSizeQuant - 1)) and SizeUint(-VarSizeQuant);

  {$ifdef FPC_HAS_FEATURE_THREADING}
      if Assigned(toFree) then
        FlushToFree;
  {$endif}

    { Seach varFree for a chunk that fits size, heuristically strive for smallest. }
    fv := varFree;
    while Assigned(fv) and (fv^.size < size) do
      fv := fv^.next;
  {$if MatchEffort <> 0}
    if Assigned(fv) and (fv^.size > size) then { Don’t search further if the size is already exact. }
    begin
    {$if MatchEffort > 1} triesLeft := MatchEffort + 1; {$endif}
      fv2 := fv;
      repeat
      {$if MatchEffort > 1}
        dec(triesLeft);
        if triesLeft = 0 then
          break;
      {$endif}
        fv2 := fv2^.next;
        if not Assigned(fv2) then
          break;
        fv2Size := fv2^.size;
        if (fv2Size < size) or (fv2Size >= fv^.size) then
          continue;
        fv := fv2;
      {$if MatchEffort > 1}
        if fv2Size = size then { Check here instead of the loop condition to prevent ‘continue’ from jumping to the check. }
          break;
      {$endif}
      until {$if MatchEffort = 1} true {$else} false {$endif};
    end;
  {$endif MatchEffort <> 0}

    if Assigned(fv) then
    begin
      { Remove fv from varFree. }
      varPrev := fv^.prev;
      varNext := fv^.next;
      if Assigned(varPrev) then
        varPrev^.next := varNext
      else
        varFree := varNext;
      if Assigned(varNext) then
        varNext^.prev := varPrev;
    end else
    begin
      minSize := VarOSChunkDataOffset + size;
      if minSize <= GrowHeapSize1 then
        maxSize := GrowHeapSize1
      else if minSize <= GrowHeapSize2 then
        maxSize := GrowHeapSize2
      else
        maxSize := High(SizeUint);
      osChunk := pVarOSChunk(GetOSChunk(minSize, maxSize, -1));
      if not Assigned(osChunk) then
        exit(nil);
      osChunk^.sizeIndex := -1;

      { Add osChunk to varOS. }
      osNext := varOS;
      osChunk^.prev := nil;
      osChunk^.next := osNext;
      if Assigned(osNext) then
        osNext^.prev := osChunk;
      varOS := osChunk;

      { Format new free var chunk spanning the entire osChunk (but don’t add to varFree, it is expected to be removed). }
      fv := pointer(osChunk) + (VarOSChunkDataOffset + VarHeaderSize);
      pVarHeader(pointer(fv) - VarHeaderSize)^.prevSize := 0;
    {$ifdef FPC_HAS_FEATURE_THREADING}
      pVarHeader(pointer(fv) - VarHeaderSize)^.threadState := @self;
    {$endif}
      vSize := SizeUint(osChunk^.size - VarOSChunkDataOffset) and SizeUint(-VarSizeQuant);
    {$if sizeof(SizeUint) > 4}
      pVarHeader(pointer(fv) - VarHeaderSize)^.sizeHi := vSize shr 32;
    {$endif}
      pVarHeader(pointer(fv) - VarHeaderSize)^.ch.h := uint32(vSize) or (FirstFlag or LastFlag);
      fv^.size := vSize;
    end;

    { Result will be allocated at the beginning of fv; maybe format the remainder and push it back to varFree. }
    result := fv;
    vSize := fv^.size - size;
    if (vSize > MaxFixedHeaderAndPayload) or
      { If fv is last in the OS chunk, remainder ≤ MaxFixedHeaderAndPayload is guaranteedly wasted.
        If fv is not last, there is a hope that occupied chunk to the right might get freed one day and merge with the remainder. }
      (vSize >= MinVarHeaderAndPayload) and (pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag = 0) then
    begin
      inc(pointer(fv), size); { result = allocated block, fv = remainder. }
      pVarHeader(pointer(fv) - VarHeaderSize)^.prevSize := size;
    {$ifdef FPC_HAS_FEATURE_THREADING}
      pVarHeader(pointer(fv) - VarHeaderSize)^.threadState := @self;
    {$endif}
    {$if sizeof(SizeUint) > 4}
      pVarHeader(pointer(fv) - VarHeaderSize)^.sizeHi := vSize shr 32;
    {$endif}
      { Remainder is still last in the OS chunk if the original chunk was last. }
      pVarHeader(pointer(fv) - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag or uint32(vSize);
      fv^.size := vSize;
      if pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag = 0 then
        pVarHeader(pointer(fv) + vSize - VarHeaderSize)^.prevSize := vSize;

      { Add fv to varFree. }
      varNext := varFree;
      fv^.prev := nil;
      fv^.next := varNext;
      if Assigned(varNext) then
        varNext^.prev := fv;
      varFree := fv;

      { Allocated chunk is still first in the OS chunk if the original chunk was first. }
      pVarHeader(result - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h and FirstFlag or UsedFlag or uint32(size);
    end else
    begin
      { Use the entire chunk. }
      size := fv^.size;
      pVarHeader(result - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h and (FirstFlag or LastFlag) or UsedFlag or uint32(size);
    end;
  {$if sizeof(SizeUint) > 4}
    pVarHeader(result - VarHeaderSize)^.sizeHi := size shr 32;
  {$endif}
    inc(used, size);
    if used > maxUsed then
      maxUsed := used;
  end;

  function HeapInc.ThreadState.FreeVar(p: pointer): SizeUint;
  var
  {$ifdef FPC_HAS_FEATURE_THREADING}
    chunkTs: pThreadState;
  {$endif}
    varPrev, varNext: pFreeVarChunk;
    p2: pointer;
    fSizeFlags: SizeUint;
    osChunk, osPrev, osNext: pVarOSChunk;
    freeOsNext: pFreeOSChunk;
  begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
    chunkTs := pVarHeader(p - VarHeaderSize)^.threadState;
    if chunkTs <> @self then
    begin
      EnterCriticalSection(gs.lock);
      chunkTs := pVarHeader(p - VarHeaderSize)^.threadState;
      if Assigned(chunkTs) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy chunkTs.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := {$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(p - VarHeaderSize)^.sizeHi) shl 32 or {$endif} pVarHeader(p - VarHeaderSize)^.ch.h and uint32(VarSizeMask) - VarHeaderSize;
        chunkTs^.PushToFree(p);
        LeaveCriticalSection(gs.lock);
        exit;
      end;
      AdoptVarOwner(p); { ...And continue! }
      LeaveCriticalSection(gs.lock);
    end;
  {$endif FPC_HAS_FEATURE_THREADING}

    fSizeFlags := {$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(p - VarHeaderSize)^.sizeHi) shl 32 or {$endif} pVarHeader(p - VarHeaderSize)^.ch.h;
    result := fSizeFlags and VarSizeMask;
    dec(used, result);

    { If next/prev are free, remove them from varFree and merge with f — (f)uture (f)ree chunk that starts at p, has fSizeFlags,
      and conveniently always inherits prevSize of its final location. }
    if fSizeFlags and LastFlag = 0 then
    begin
      p2 := p + result;
      if pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag = 0 then
      begin
        fSizeFlags := (fSizeFlags + pFreeVarChunk(p2)^.size) or pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag;

        { Remove p2 from varFree. }
        varPrev := pFreeVarChunk(p2)^.prev;
        varNext := pFreeVarChunk(p2)^.next;
        if Assigned(varPrev) then
          varPrev^.next := varNext
        else
          varFree := varNext;
        if Assigned(varNext) then
          varNext^.prev := varPrev;
      end;
    end;

    if fSizeFlags and FirstFlag = 0 then
    begin
      p2 := p - pVarHeader(p - VarHeaderSize)^.prevSize;
      if pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag = 0 then
      begin
        p := p2;
        fSizeFlags := (fSizeFlags + pFreeVarChunk(p2)^.size) or pVarHeader(p2 - VarHeaderSize)^.ch.h and FirstFlag;

        { Remove p2 from varFree. }
        varPrev := pFreeVarChunk(p2)^.prev;
        varNext := pFreeVarChunk(p2)^.next;
        if Assigned(varPrev) then
          varPrev^.next := varNext
        else
          varFree := varNext;
        if Assigned(varNext) then
          varNext^.prev := varPrev;
      end;
    end;

    { Turn p into a free chunk and add it back to varFree...
      unless it spans the entire OS chunk, in which case instead move the chunk from varOS to freeOS. }
    if fSizeFlags and (FirstFlag or LastFlag) <> FirstFlag or LastFlag then
    begin
      if fSizeFlags and LastFlag = 0 then
        pVarHeader(p + fSizeFlags and VarSizeMask - VarHeaderSize)^.prevSize := fSizeFlags and VarSizeMask;

    {$if sizeof(SizeUint) > 4}
      pVarHeader(p - VarHeaderSize)^.sizeHi := fSizeFlags shr 32;
    {$endif}
      pVarHeader(p - VarHeaderSize)^.ch.h := uint32(fSizeFlags) xor UsedFlag;
      pFreeVarChunk(p)^.size := fSizeFlags and VarSizeMask;

      { Add p to varFree. }
      varNext := varFree;
      pFreeVarChunk(p)^.prev := nil;
      pFreeVarChunk(p)^.next := varNext;
      if Assigned(varNext) then
        varNext^.prev := p;
      varFree := p;
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
    oldpsize, fSizeFlags, growby: SizeUint;
    varNext, varPrev: pFreeVarChunk;
  begin
    if (size < MinVarHeaderAndPayload - VarHeaderSize) or (size > MaxVarPayload)
    {$ifdef FPC_HAS_FEATURE_THREADING}
      or (pVarHeader(p - VarHeaderSize)^.threadState <> @self)
    {$endif}
    then
      exit(nil);
    size := (size + (VarHeaderSize + VarSizeQuant - 1)) and SizeUint(-VarSizeQuant);
    result := p; { From now on use result instead of p (saves a register). }

    oldpsize := {$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(result - VarHeaderSize)^.sizeHi) shl 32 or {$endif} pVarHeader(result - VarHeaderSize)^.ch.h and uint32(VarSizeMask);
    p2 := result + oldpsize;
    { (f)uture (f)ree chunk starting at p + size and having fSizeFlags will be created at the end, must exit before that if not required. }
    if size <= oldpsize then
    begin
      { Shrink. Maybe. }
      fSizeFlags := oldpsize - size;

      if pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag <> 0 then
      begin
        { Has nothing to the right: create free chunk if > MaxFixedHeaderAndPayload, otherwise report success but change nothing. }
        if fSizeFlags <= MaxFixedHeaderAndPayload then
          exit;
        dec(used, fSizeFlags);
        fSizeFlags := fSizeFlags or LastFlag;
      end
      else if pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag <> 0 then
      begin
        { Has used chunk to the right: create free chunk if ≥ MinVarHeaderAndPayload, following the same logic as in AllocVar. }
        if fSizeFlags < MinVarHeaderAndPayload then
          exit;
        dec(used, fSizeFlags);
      end else
      begin
        dec(used, fSizeFlags);
        { Has empty chunk to the right: extend with freed space. }
        fSizeFlags := (fSizeFlags + pFreeVarChunk(p2)^.size) or pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag;

        { Remove p2 from varFree. }
        varPrev := pFreeVarChunk(p2)^.prev;
        varNext := pFreeVarChunk(p2)^.next;
        if Assigned(varPrev) then
          varPrev^.next := varNext
        else
          varFree := varNext;
        if Assigned(varNext) then
          varNext^.prev := varPrev;
      end;

      { Update p size. }
    {$if sizeof(SizeUint) > 4}
      pVarHeader(result - VarHeaderSize)^.sizeHi := size shr 32;
    {$endif}
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) or pVarHeader(result - VarHeaderSize)^.ch.h and (FirstFlag or UsedFlag);
    end
    { Grow if there is free space. }
    else if (pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag = 0) and (pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag = 0) and
      (pFreeVarChunk(p2)^.size >= size - oldpsize) then
    begin
      fSizeFlags := pFreeVarChunk(p2)^.size - (size - oldpsize);
      if pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag = 0 then
      begin
        if fSizeFlags <= MaxFixedHeaderAndPayload then
          fSizeFlags := 0;
      end else
        if fSizeFlags < MinVarHeaderAndPayload then
          fSizeFlags := 0
        else
          fSizeFlags := fSizeFlags or LastFlag;

      growby := pFreeVarChunk(p2)^.size - fSizeFlags and VarSizeMask;
      size := oldpsize + growby;
      inc(used, growby);
      if used > maxUsed then
        maxUsed := used;

      { Remove p2 from varFree. }
      varPrev := pFreeVarChunk(p2)^.prev;
      varNext := pFreeVarChunk(p2)^.next;
      if Assigned(varPrev) then
        varPrev^.next := varNext
      else
        varFree := varNext;
      if Assigned(varNext) then
        varNext^.prev := varPrev;

      { Update p size. }
    {$if sizeof(SizeUint) > 4}
      pVarHeader(result - VarHeaderSize)^.sizeHi := size shr 32;
    {$endif}
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) or pVarHeader(result - VarHeaderSize)^.ch.h and (FirstFlag or UsedFlag);
      { No empty chunk? }
      if fSizeFlags = 0 then
      begin
        if pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          pVarHeader(result - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h or LastFlag
        else
          pVarHeader(result + size - VarHeaderSize)^.prevSize := size;
        exit;
      end;
    end
  {$ifdef HAS_SYSOSREALLOC}
    else if (oldpsize >= 64 * 1024) and { Don’t do SysOSRealloc if the source is under 64 Kb (arbitrary value). }
      (pVarHeader(result - VarHeaderSize)^.ch.h and FirstFlag <> 0) and
      ((pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag <> 0) or (pVarHeader(p2 - VarHeaderSize)^.ch.h and (LastFlag or UsedFlag) = LastFlag)) then
      exit(TrySysOSRealloc(result, oldpsize, size))
  {$endif}
    else
      exit(nil);

    { Format new free var chunk. }
    fp := result + size;
    pVarHeader(fp - VarHeaderSize)^.prevSize := size;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    pVarHeader(fp - VarHeaderSize)^.threadState := @self;
  {$endif}
  {$if sizeof(SizeUint) > 4}
    pVarHeader(fp - VarHeaderSize)^.sizeHi := fSizeFlags shr 32;
  {$endif}
    pVarHeader(fp - VarHeaderSize)^.ch.h := uint32(fSizeFlags);
    pFreeVarChunk(fp)^.size := fSizeFlags and VarSizeMask;
    if fSizeFlags and LastFlag = 0 then
      pVarHeader(fp + fSizeFlags and VarSizeMask - VarHeaderSize)^.prevSize := fSizeFlags and VarSizeMask;

    { Add fp to varFree. }
    varNext := varFree;
    pFreeVarChunk(fp)^.prev := nil;
    pFreeVarChunk(fp)^.next := varNext;
    if Assigned(varNext) then
      varNext^.prev := fp;
    varFree := fp;
  end;

{$ifdef HAS_SYSOSREALLOC}
  function HeapInc.ThreadState.TrySysOSRealloc(p: pointer; oldSize, newSize: SizeUint): pointer;
  var
    newOSSize: SizeUint;
    hasFreeChunkToTheRight: boolean;
    vf, varPrev, varNext: pFreeVarChunk;
  begin
    { Either p is the only chunk or has last empty chunk to the right. }
    hasFreeChunkToTheRight := pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag = 0;

    { Don’t do SysOSRealloc if the source chunk is <12.5% (arbitrary value) of the empty chunk to the right. }
    if hasFreeChunkToTheRight and (oldSize < pFreeVarChunk(p + oldSize)^.size div 8) then
      exit(nil);

    newOSSize := (newSize + (VarOSChunkDataOffset + OSChunkVarSizeQuant - 1)) and SizeUint(-OSChunkVarSizeQuant);
    p := SysOSRealloc(p - (VarOSChunkDataOffset + VarHeaderSize), pVarOSChunk(p - (VarOSChunkDataOffset + VarHeaderSize))^.size, newOSSize);
    if not Assigned(p) then
      exit(nil);

    inc(allocated, newOSSize - pVarOSChunk(p)^.size);
    if allocated > maxAllocated then
      maxAllocated := allocated;
    pVarOSChunk(p)^.size := newOSSize;
    { For simplicity, new chunk spans the entire OS chunk. }
    newOSSize := (newOSSize - VarOSChunkDataOffset) and SizeUint(-VarSizeQuant);
    inc(used, newOSSize - oldSize);
    if used > maxUsed then
      maxUsed := used;

    { Update p size. }
  {$if sizeof(SizeUint) > 4}
    pVarHeader(p + VarOSChunkDataOffset)^.sizeHi := newOSSize shr 32;
  {$endif}
    pVarHeader(p + VarOSChunkDataOffset)^.ch.h := uint32(newOSSize) or (FirstFlag or LastFlag or UsedFlag);

    { Careful! Old pointers into p are invalidated and must be fixed.
      There are up to 3 invalidated pointers: OS chunk in varOS, old p itself (p is reused for new OS chunk pointer), maybe empty chunk to the right in varFree. }
    if Assigned(pVarOSChunk(p)^.next) then
      pVarOSChunk(pVarOSChunk(p)^.next)^.prev := p;
    if Assigned(pVarOSChunk(p)^.prev) then
      pVarOSChunk(pVarOSChunk(p)^.prev)^.next := p
    else
      varOS := p;

    result := p + (VarOSChunkDataOffset + VarHeaderSize);
    if hasFreeChunkToTheRight then
    begin
      vf := result + oldSize;

      { Remove vf from varFree. }
      varPrev := vf^.prev;
      varNext := vf^.next;
      if Assigned(varPrev) then
        varPrev^.next := varNext
      else
        varFree := varNext;
      if Assigned(varNext) then
        varNext^.prev := varPrev;
    end;
  end;
{$endif HAS_SYSOSREALLOC}

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
      if pCommonHeader(pointer(tf) - CommonHeaderSize)^.h and FixedFlag <> 0 then
        FreeFixed(tf)
      else
        FreeVar(tf);
      tf := nx;
    end;
  end;

  procedure HeapInc.ThreadState.Orphan;
  var
    sizeIndex: SizeUint;
    lastFree, nextFree: pFreeOSChunk;
    vOs, nextVOs, lastVOs: pVarOSChunk;
  begin
    FlushToFree;
    Orphan(fullOS);
    for sizeIndex := 0 to High(fixedPartialOS) do
      Orphan(fixedPartialOS[sizeIndex]);
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
    {$ifdef HAS_SYSOSFREE}
      inc(gs.freeOS.n, freeOS.n);
      while gs.freeOS.n > MaxKeptOSChunks do
        gs.freeOS.FreeOne;
    {$endif}
    end;
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

    { Zeroing is probably required, because Orphan is called from FinalizeHeap which is called from DoneThread which can be called twice, according to this comment from syswin.inc: }
    // DoneThread; { Assume everything is idempotent there }
    FillChar(self, sizeof(self), 0);
  end;

  class procedure HeapInc.ThreadState.Orphan(list: pFixedOSChunk);
  var
    last, osNext: pFixedOSChunk;
  begin
    if not Assigned(list) then
      exit;
    last := pFixedOSChunk(ChangeThreadStates(list, nil));
    { Prepend list to gs.fixedOS. }
    osNext := gs.fixedOS;
    last^.next := osNext;
    if Assigned(osNext) then
      osNext^.prev := last;
    gs.fixedOS := list;
  end;

  procedure HeapInc.ThreadState.Adopt(osChunk: pFixedOSChunk);
  var
    sizeIndex: SizeUint;
    dest: ^pFixedOSChunk;
  begin
    sizeIndex := pCommonHeader(pointer(osChunk) + FixedOSChunkDataOffset)^.h and SizeIndexMask;
    inc(used, osChunk^.usedSize);
    if used > maxUsed then
      maxUsed := used;
    inc(allocated, osChunk^.size);
    if allocated > maxAllocated then
      maxAllocated := allocated;

    { Remove osChunk from gs.fixedOS, add to fullOS or fixedPartialOS[sizeIndex] as appropriate. }
    dest := @fixedPartialOS[sizeIndex];
    if osChunk^.usedSize >= osChunk^.fullThreshold then
    begin
      inc(allocatedByFullFixed[sizeIndex], osChunk^.size);
      dest := @fullOS;
    end;
    osChunk^.MoveTo(gs.fixedOS, dest^);

    osChunk^.threadState := @self;
  end;

  procedure HeapInc.ThreadState.AdoptVarOwner(p: pointer);
  var
    prevSize, size: SizeUint;
    h: uint32;
    varFreeHead: pFreeVarChunk;
  begin
    repeat
      prevSize := pVarHeader(p - VarHeaderSize)^.prevSize;
      dec(p, prevSize);
    until prevSize = 0;

    { Move OS chunk from gs.varOS to varOS. }
    pOSChunk(p - (VarOSChunkDataOffset + VarHeaderSize))^.MoveTo(gs.varOS, varOS);
    inc(allocated, pOSChunk(p - (VarOSChunkDataOffset + VarHeaderSize))^.size);
    if allocated > maxAllocated then
      maxAllocated := allocated;

    { Careful: even though VarHeaders have own threadState links, correct threadState in the OS chunk is required,
      as the chunk might be orphaned, then adopted with this function, then become free, then be reused as fixed chunk.
      GetOSChunk does not set threadState if it takes the chunk from local freeOS, assuming it is already set. }
    pOSChunk(p - (VarOSChunkDataOffset + VarHeaderSize))^.threadState := @self;

    varFreeHead := varFree;
    repeat
      pVarHeader(p - VarHeaderSize)^.threadState := @self;
      h := pVarHeader(p - VarHeaderSize)^.ch.h;
      size := {$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(p - VarHeaderSize)^.sizeHi) shl 32 or {$endif} h and uint32(VarSizeMask);
      if h and UsedFlag = 0 then
      begin
        { Add free chunk to varFree. }
        pFreeVarChunk(p)^.prev := nil;
        pFreeVarChunk(p)^.next := varFreeHead;
        if Assigned(varFreeHead) then
          varFreeHead^.prev := pFreeVarChunk(p);
        varFreeHead := p;
      end else
        inc(used, size); { maxUsed is updated after the loop. }
      inc(p, size);
    until h and LastFlag <> 0;
    varFree := varFreeHead;
    if used > maxUsed then
      maxUsed := used;
  end;

  class function HeapInc.ThreadState.ChangeThreadStates(list: pOSChunk; ts: pThreadState): pOSChunk; static; { Returns the last item of list. }
  begin
    if not Assigned(list) then
      exit(nil);
    repeat
      list^.threadState := ts;
      result := list;
      list := list^.next;
    until not Assigned(list);
  end;

  class procedure HeapInc.ThreadState.ChangeThreadState(vOs: pVarOSChunk; ts: pThreadState);
  var
    h: uint32;
    p: pointer;
  begin
    vOs^.threadState := ts; { Not really required (for now), but done for symmetry; also see the comment on setting OSChunk.threadState in AdoptVarOwner. }
    p := pointer(vOs) + (VarOSChunkDataOffset + VarHeaderSize);
    repeat
      pVarHeader(p - VarHeaderSize)^.threadState := ts;
      h := pVarHeader(p - VarHeaderSize)^.ch.h;
      inc(p, {$if sizeof(SizeUint) > 4} SizeUint(pVarHeader(p - VarHeaderSize)^.sizeHi) shl 32 or {$endif} h and uint32(VarSizeMask));
    until h and LastFlag <> 0;
  end;

{$ifndef FPC_SECTION_THREADVARS}
  procedure HeapInc.ThreadState.FixupSelfPtr;
  var
    sizeIndex: SizeUint;
    vOs: pVarOSChunk;
  begin
    ChangeThreadStates(fullOS, @self);
    for sizeIndex := 0 to High(fixedPartialOS) do
      ChangeThreadStates(fixedPartialOS[sizeIndex], @self);
    ChangeThreadStates(freeOS.first, @self);
    vOs := varOS;
    while Assigned(vOs) do
    begin
      ChangeThreadState(vOs, @self);
      vOs := vOs^.next;
    end;
  end;
{$endif ndef FPC_SECTION_THREADVARS}
{$endif FPC_HAS_FEATURE_THREADING}

function SysGetFPCHeapStatus:TFPCHeapStatus;
var
  ts: HeapInc.pThreadState;
begin
  ts := @HeapInc.thisTs;
  result.MaxHeapSize := ts^.maxAllocated;
  result.MaxHeapUsed := ts^.maxUsed;
  result.CurrHeapSize := ts^.allocated;
  result.CurrHeapUsed := ts^.used;
  result.CurrHeapFree := result.CurrHeapSize - result.CurrHeapUsed;
end;

function SysGetHeapStatus :THeapStatus;
var
  ts: HeapInc.pThreadState;
begin
  FillChar((@result)^, sizeof(result), 0);
  ts := @HeapInc.thisTs;
  result.TotalAllocated   :=ts^.used;
  result.TotalFree        :=ts^.allocated - ts^.used;
  result.TotalAddrSpace   :=ts^.allocated;
end;

function SysGetMem(size : ptruint):pointer;
var
  ts: HeapInc.pThreadState;
begin
  ts := @HeapInc.thisTs;
  if size <= HeapInc.MaxFixedHeaderAndPayload - HeapInc.CommonHeaderSize then
    result := ts^.AllocFixed(size)
  else
    result := ts^.AllocVar(size);
end;

function SysFreeMem(p: pointer): ptruint;
var
  ts: HeapInc.pThreadState;
begin
  result := 0;
  if Assigned(p) then
    begin
      ts := @HeapInc.thisTs;
      if HeapInc.pCommonHeader(p - HeapInc.CommonHeaderSize)^.h and HeapInc.FixedFlag <> 0 then
        result := ts^.FreeFixed(p)
      else
        result := ts^.FreeVar(p);
    end;
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
    newp := ts^.TryResizeVar(p, size);
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
  else
    result := {$if sizeof(SizeUint) > 4} SizeUint(HeapInc.pVarHeader(p - HeapInc.VarHeaderSize)^.sizeHi) shl 32 or {$endif}
      HeapInc.pVarHeader(p - HeapInc.VarHeaderSize)^.ch.h and uint32(HeapInc.VarSizeMask)
      - HeapInc.VarHeaderSize;
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

{$endif FPC_NO_DEFAULT_HEAP}

{$ifndef HAS_MEMORYMANAGER}

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
{$ifdef FPC_HAS_FEATURE_THREADING}
  if HeapInc.gs.lockUse > 0 then
    EnterCriticalSection(HeapInc.gs.lock);
  HeapInc.thisTs.Orphan;
  if HeapInc.gs.lockUse > 0 then
    begin
      LeaveCriticalSection(HeapInc.gs.lock);
      if InterlockedDecrement(HeapInc.gs.lockUse) = 0 then
        begin
          DoneCriticalSection(HeapInc.gs.lock);
        {$ifdef HAS_SYSOSFREE}
          HeapInc.gs.freeOS.FreeAll;
        {$endif}
        end;
    end;
{$else FPC_HAS_FEATURE_THREADING}
  HeapInc.thisTs.freeOS.FreeAll;
{$endif FPC_HAS_FEATURE_THREADING}
end;

{$endif ndef HAS_MEMORYMANAGER}

{$endif ndef FPC_NO_DEFAULT_MEMORYMANAGER}
{$endif defined(FPC_HAS_FEATURE_HEAP) or defined(FPC_IN_HEAPMGR)}
