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

{ Intrusive red-black tree, loosely translated from
  https://github.com/torvalds/linux/blob/master/tools/include/linux/rbtree.h
  https://github.com/torvalds/linux/blob/master/tools/include/linux/rbtree_augmented.h
  https://github.com/torvalds/linux/blob/master/lib/rbtree.c }

  RbTree = object
  type
    pNode = ^Node; ppNode = ^pNode;
    Node = record
      parentColor: PtrUint;
    case uint32 of
      0: (childs: array[0 .. 1] of pNode);
      1: (left, right: pNode);
    end;

    { For possible future templating. Right now, hardcoded for FreeVarChunk comparisons.
      Note that very few functions (Add and LowerBound) depend on Control, so turning the entire RbTree into a RbTree<Control> is unwise. }
    Control = object
      class function Less(a, b: pNode): boolean; static; inline;
      class function KeyCompare(key: pointer; b: pNode): PtrInt; static; inline;
    end;

    procedure Add(n: pNode);
    procedure Remove(n: pNode);
    function LowerBound(key: pointer): pNode; { Returns smallest node ≥ key. }
    function First: pNode;
    class function Next(n: pNode): pNode; static;
    class function Prev(n: pNode): pNode; static;

    { ChangeInPlace is the same optimization used in Boost rbtree_best_fit.hpp:
      shortcut Remove + Add if the node is already at correct position.

      Callers check this case manually: if the node value is decreased, ChangeInPlace is possible if the new value is still ≥ Prev(node), and vice versa.

      Beware of the overlaps... ChangeInPlace is potentially vulnerable to the RbTree.Node overlap in TryResizeVar,
      so sizeof(RbTree.Node) <= VarSizeQuant is ensured with a compile-time $if. }

    procedure ChangeInPlace(old, new: pNode);

  private const
    BlackTag = 1;
    ParentMask = PtrUint(not BlackTag);

  var
    root: pNode;

    procedure ChangeChild(old, new, parent: pNode); inline;
    procedure RotateSetParents(old, new: pNode; color: PtrUint); inline;
    procedure InsertColor(n: pNode);
    function EraseAugmented(n: pNode): pNode;
    procedure EraseColor(parent: pNode);
  end;

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

    { Limit on shrinking variable chunks and keeping the tail when splitting the chunk in AllocVar / TryResizeVar. }
    MinVarHeaderAndPayload = MaxFixedHeaderAndPayload * 3 div 4;

  { Adjustable part ends here~ }

  const
    SizeIndexBits = 1 + trunc(ln(FixedSizesCount - 1) /  ln(2));
    SizeIndexMask = 1 shl SizeIndexBits - 1;
    FixedBitPos = {$if SizeIndexBits >= 4} SizeIndexBits {$else} 4 {$endif}; { Variable chunks use 4 low bits for used / first / last / fixed arena. }
    FixedFlag = 1 shl FixedBitPos;
    FixedArenaOffsetShift = {$if FixedBitPos + 1 >= 5} FixedBitPos + 1 {$else} 5 {$endif}; { VarSizeQuant must be at least 2^5 to fit 3 64-bit pointers (RbTree.Node). }

    UsedFlag = 1 shl 0;
    FirstFlag = 1 shl 1;
    LastFlag = 1 shl 2;
    FixedArenaFlag = 1 shl 3;
    VarSizeQuant = 1 shl FixedArenaOffsetShift; {$if VarSizeQuant < Alignment} {$error Assumed to be >= Alignment.} {$endif} {$if VarSizeQuant < 3 * sizeof(pointer)} {$error Must fit RbTree.Node.} {$endif}
    VarSizeMask = SizeUint(-VarSizeQuant);
    HugeHeader = 0; { Special header value for huge chunks. FixedFlag must be 0, and the value must be impossible for a variable chunk. 0 turns out to be suitable. :) }

  type
    { Common header of any memory chunk, residing immediately to the left of the ~payload~ (block).

      Fixed chunk header, assuming SizeIndexBits = 4:
      h[0:3] = size index (= h and SizeIndexMask)
      h[4] = 1 (h and FixedFlag <> 0)
      h[5:31] — offset in the FixedArena (= h shr FixedArenaOffsetShift)

      Variable chunk header, assuming SizeIndexBits = 4:
      h[0] = used flag (h and UsedFlag <> 0)
      h[1] = first flag (h and FirstFlag <> 0)
      h[2] = last flag (h and LastFlag <> 0)
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
        — first formattedSize are either allocated (“used”; counted in usedSize) or in the freelist (firstFreeChunk; size = formattedSize - usedSize),
        — the rest “maxSize” - formattedSize are yet unallocated space.

        This design, together with tracking free chunks per FixedArena rather than per fixed size, trivializes reusing the fixed arenas.
        Chopping all available space at once would get rid of the “unallocated space” entity, but is a lot of potentially wasted work:
        https://gitlab.com/freepascal.org/fpc/source/-/issues/40447.

        Values are multiples of the chunk size instead of counts (could be chunksUsed, chunksFormatted, chunksMax) to save on multiplications.
        Moreover, instead of “maxSize” from the explanation above, fullThreshold is used, which is such a value that the chunk is full if usedSize >= fullThreshold.
        maxSize = RoundUp(fullThreshold, chunk size).
        Reason is, calculating fullThreshold does not require division. }

      firstFreeChunk: pFreeChunk;
      usedSize, formattedSize, fullThreshold: uint32;
      sizeIndex: uint32; { For what size it was used the last time. Allows for a small optimization when reusing fixed arenas. }
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
      size: SizeUint; { Cached size for easier access when working with free chunks, always equals to header.ch.h and VarSizeMask. }
      rbn: RbTree.Node;
    end;

    pHugeChunk = ^HugeChunk;
    HugeChunk = object(OSChunkBase)
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
      varFree: RbTree;

      { Fixed arenas with at least 1 free chunk (including unformatted space), but not completely empty.
        Fixed arenas that become completely empty are moved to emptyArenas, completely full are... not present in any list. }
      partialArenas: array[0 .. FixedSizesCount - 1] of pFixedArena;

      { Only to calculate preferable new fixed arena sizes...
        (Updated infrequently, as opposed to possible “usedPerArena”. When a new arena is required, all existing arenas of its size are full.) }
      allocatedByFullArenas: array[0 .. FixedSizesCount - 1] of SizeUint;

    {$ifdef DEBUG_HEAP_INC}
      procedure Dump(var f: text);
    {$endif}

      function ChooseFixedArenaSize(sizeIndex: SizeUint): SizeUint;
      function AllocFixed(size: SizeUint): pointer;
      function FreeFixed(p: pointer): SizeUint;

      function GetOSChunk(minSize, maxSize: SizeUint): pOSChunk;
      function AllocateOSChunk(minSize: SizeUint): pOSChunk;

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

      procedure Orphan; { Must be performed under gs.lock. }
      procedure AdoptArena(arena: pFixedArena);
      procedure AdoptVarOwner(p: pointer); { Adopts the OS chunk that contains p. Must be performed under gs.lock. }
      class procedure ChangeThreadState(vOs: pVarOSChunk; ts: pThreadState); static;
      procedure FreeEmptyArenas(untilCount: SizeUint);

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
      freeOS: FreeOSChunkList;
      varOS: pVarOSChunk;
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
    FreeVarChunkRbnOffset = PtrUint(@FreeVarChunk(nil^).rbn);
  end;

  class function RbTree.Control.Less(a, b: pNode): boolean;
  begin
    result := HeapInc.pFreeVarChunk(pointer(a) - HeapInc.FreeVarChunkRbnOffset)^.size < HeapInc.pFreeVarChunk(pointer(b) - HeapInc.FreeVarChunkRbnOffset)^.size;
  end;

  class function RbTree.Control.KeyCompare(key: pointer; b: pNode): PtrInt;
  begin
    result := PtrInt(PtrUint(key)) - PtrInt(HeapInc.pFreeVarChunk(pointer(b) - HeapInc.FreeVarChunkRbnOffset)^.size);
  end;

  procedure RbTree.Add(n: pNode);
  var
    link: ppNode;
    parent: pNode;
  begin
    link := @root;
    parent := nil;
    if Assigned(link^) then
      repeat
        parent := link^;
        if Control.Less(n, parent) then { For me, branching is much better here: Alloc/FreeVar are up to 20% faster (when compiling the compiler) than branchless. }
        begin
          link := @parent^.left;
          if Assigned(link^) then continue; { This way, “continue”s jump to the beginning of the loop. Important as this code is really hot. }
        end else
        begin
          link := @parent^.right;
          if Assigned(link^) then continue;
        end;
        break;
      until false;
    n^.parentColor := PtrUint(parent);
    n^.left := nil;
    n^.right := nil;
    link^ := n;
    InsertColor(n);
  end;

  procedure RbTree.Remove(n: pNode);
  begin
    n := EraseAugmented(n);
    if Assigned(n) then EraseColor(n);
  end;

  function RbTree.LowerBound(key: pointer): pNode;
  var
    n: pNode;
    cmp: PtrInt;
  begin
    result := nil;
    n := root;
    if Assigned(n) then
      repeat
        cmp := Control.KeyCompare(key, n);
        if cmp <= 0 then { Same as in Add: branch on the Control predicate, but avoid extra internal jumps. }
        begin
          result := n;
          if cmp = 0 then exit; { Not required (and not even correct for a method named LowerBound: if there are equal items, such a method should return first of them, not any of them, better name TBD), but exits early on many equal values, which tends to happen often with memory sizes. }
          n := n^.left;
          if Assigned(n) then continue;
        end else
        begin
          n := n^.right;
          if Assigned(n) then continue;
        end;
        break;
      until false;
  end;

  function RbTree.First: pNode;
  var
    child: pNode;
  begin
    result := root;
    if not Assigned(result) then exit;
    child := result;
    repeat
      result := child;
      child := child^.left;
    until not Assigned(child);
  end;

  class function RbTree.Next(n: pNode): pNode;
  var
    child: pNode;
  begin
    child := n^.right;
    if Assigned(child) then
    begin
      repeat
        result := child;
        child := result^.left;
      until not Assigned(child);
      exit;
    end;
    result := n;
    repeat
      child := result;
      result := pointer(result^.parentColor and ParentMask);
    until not Assigned(result) or (child = result^.left);
  end;

  class function RbTree.Prev(n: pNode): pNode;
  var
    child: pNode;
  begin
    child := n^.left;
    if Assigned(child) then
    begin
      repeat
        result := child;
        child := result^.right;
      until not Assigned(child);
      exit;
    end;
    result := n;
    repeat
      child := result;
      result := pointer(result^.parentColor and ParentMask);
    until not Assigned(result) or (child = result^.right);
  end;

  procedure RbTree.ChangeChild(old, new, parent: pNode);
  begin
    if Assigned(parent) then
      parent^.childs[ord(old = parent^.right)] := new
    else
      root := new;
  end;

  procedure RbTree.ChangeInPlace(old, new: pNode);
  var
    pc: PtrUint;
    adj: pNode;
  begin
    adj := old^.left;
    new^.left := adj;
    if Assigned(adj) then adj^.parentColor := PtrUint(new) + adj^.parentColor and BlackTag;
    adj := old^.right;
    new^.right := adj;
    if Assigned(adj) then adj^.parentColor := PtrUint(new) + adj^.parentColor and BlackTag;
    pc := old^.parentColor;
    new^.parentColor := pc;
    ChangeChild(old, new, pointer(pc and ParentMask));
  end;

  procedure RbTree.RotateSetParents(old, new: pNode; color: PtrUint);
  var
    oldParentColor: PtrUint;
  begin
    oldParentColor := old^.parentColor;
    new^.parentColor := oldParentColor;
    old^.parentColor := PtrUint(new) + color;
    ChangeChild(old, new, pointer(oldParentColor and ParentMask));
  end;

  procedure RbTree.InsertColor(n: pNode);
  var
    parent, gparent, tmp: pNode;
    L, R: SizeUint;
  begin
    parent := pNode(n^.parentColor);
    repeat
      if not Assigned(parent) then
      begin
        n^.parentColor := BlackTag;
        break;
      end;
      PtrUint(gparent) := parent^.parentColor;
      if PtrUint(gparent) and BlackTag <> 0 then
        break;
      { L and R are offsets of left and right child, or right and left for mirrored cases. }
      L := PtrUint(@pNode(nil)^.childs) + SizeUint(parent = gparent^.right) * sizeof(pointer);
      R := (2 * PtrUint(@pNode(nil)^.childs) + sizeof(pointer)) - L; { offsetof(childs, Node) + (sizeof(pointer) - (L - offsetof(childs, Node))), simplified. }
      tmp := pPointer(pointer(gparent) + R)^;
      if Assigned(tmp) and (tmp^.parentColor and BlackTag = 0) then
      begin
        tmp^.parentColor := PtrUint(gparent) + BlackTag;
        parent^.parentColor := PtrUint(gparent) + BlackTag;
        n := gparent;
        parent := pointer(n^.parentColor and ParentMask);
        n^.parentColor := PtrUint(parent);
        continue;
      end;
      tmp := pPointer(pointer(parent) + R)^;
      if n = tmp then
      begin
        tmp := pPointer(pointer(n) + L)^;
        pPointer(pointer(parent) + R)^ := tmp;
        pPointer(pointer(n) + L)^ := parent;
        if Assigned(tmp) then
          tmp^.parentColor := PtrUint(parent) + BlackTag;
        parent^.parentColor := PtrUint(n);
        parent := n;
        tmp := pPointer(pointer(n) + R)^;
      end;
      pPointer(pointer(gparent) + L)^ := tmp; { = parent^.R }
      pPointer(pointer(parent) + R)^ := gparent;
      if Assigned(tmp) then
        tmp^.parentColor := PtrUint(gparent) + BlackTag;
      RotateSetParents(gparent, parent, 0);
      break;
    until false;
  end;

  function RbTree.EraseAugmented(n: pNode): pNode;
  var
    child, tmp, parent, successor, child2: pNode;
    pc: PtrUint;
  begin
    result := nil;
    child := n^.right;
    tmp := n^.left;
    if not Assigned(tmp) then
    begin
      pc := n^.parentColor;
      parent := pointer(pc and ParentMask);
      ChangeChild(n, child, parent);
      if Assigned(child) then
        child^.parentColor := pc
      else if pc and BlackTag <> 0 then
        result := parent;
    end
    else if not Assigned(child) then
    begin
      pc := n^.parentColor;
      tmp^.parentColor := pc;
      parent := pointer(pc and ParentMask);
      ChangeChild(n, tmp, parent);
    end else
    begin
      successor := child;
      tmp := child^.left;
      if not Assigned(tmp) then
      begin
        parent := successor;
        child2 := successor^.right;
      end else
      begin
        repeat
          parent := successor;
          successor := tmp;
          tmp := tmp^.left;
        until not Assigned(tmp);
        child2 := successor^.right;
        parent^.left := child2;
        successor^.right := child;
        child^.parentColor := PtrUint(successor) + child^.parentColor and BlackTag;
      end;

      tmp := n^.left;
      successor^.left := tmp;
      tmp^.parentColor := PtrUint(successor) + tmp^.parentColor and BlackTag;

      pc := n^.parentColor;
      tmp := pointer(pc and ParentMask);
      ChangeChild(n, successor, tmp);

      if Assigned(child2) then
        child2^.parentColor := PtrUint(parent) + BlackTag
      else if successor^.parentColor and BlackTag <> 0 then
        result := parent;
      successor^.parentColor := pc;
    end;
  end;

  procedure RbTree.EraseColor(parent: pNode);
  var
    n, sibling, tmp1, tmp2: pNode;
    L, R: SizeUint;
  begin
    n := nil;
    repeat
      L := PtrUint(@pNode(nil)^.childs) + SizeUint(n = parent^.right) * sizeof(pointer);
      R := (2 * PtrUint(@pNode(nil)^.childs) + sizeof(pointer)) - L;
      sibling := pPointer(pointer(parent) + R)^;
      if sibling^.parentColor and BlackTag = 0 then
      begin
        tmp1 := pPointer(pointer(sibling) + L)^;
        pPointer(pointer(parent) + R)^ := tmp1;
        pPointer(pointer(sibling) + L)^ := parent;
        tmp1^.parentColor := PtrUint(parent) + BlackTag;
        RotateSetParents(parent, sibling, 0);
        sibling := tmp1;
      end;
      tmp1 := pPointer(pointer(sibling) + R)^;
      if not Assigned(tmp1) or (tmp1^.parentColor and BlackTag <> 0) then
      begin
        tmp2 := pPointer(pointer(sibling) + L)^;
        if not Assigned(tmp2) or (tmp2^.parentColor and BlackTag <> 0) then
        begin
          sibling^.parentColor := PtrUint(parent);
          if parent^.parentColor and BlackTag = 0 then
            inc(parent^.parentColor, BlackTag)
          else
          begin
            n := parent;
            parent := pointer(n^.parentColor and ParentMask);
            if Assigned(parent) then continue;
          end;
          break;
        end;
        tmp1 := pPointer(pointer(tmp2) + R)^;
        pPointer(pointer(sibling) + L)^ := tmp1;
        pPointer(pointer(tmp2) + R)^ := sibling;
        pPointer(pointer(parent) + R)^ := tmp2;
        if Assigned(tmp1) then
          tmp1^.parentColor := PtrUint(sibling) + BlackTag;
        tmp1 := sibling;
        sibling := tmp2;
      end;
      tmp2 := pPointer(pointer(sibling) + L)^;
      pPointer(pointer(parent) + R)^ := tmp2;
      pPointer(pointer(sibling) + L)^ := parent;
      tmp1^.parentColor := PtrUint(sibling) + BlackTag;
      if Assigned(tmp2) then
        tmp2^.parentColor := PtrUint(parent) + tmp2^.parentColor and BlackTag;
      RotateSetParents(parent, sibling, BlackTag);
      break;
    until false;
  end;

  class function HeapInc.SizeMinus1ToIndex(sizeMinus1: SizeUint): SizeUint;
  begin
    result := SizeMinus1Div16ToIndex[sizeMinus1 div 16];
  end;

  class function HeapInc.IndexToSize(sizeIndex: SizeUint): SizeUint;
  begin
    result := FixedSizes[sizeIndex];
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
    fix: pFixedArena;
    fr: pFreeOSChunk;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    tf: pFreeChunk;
  {$endif}
    vfrbn: RbTree.pNode;
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
          writeln(f, 'arena size = ', pVarHeader(fix)[-1].ch.h and VarSizeMask - VarHeaderSize - FixedArenaDataOffset, ', usedSize = ', fix^.usedSize, ', formattedSize = ', fix^.formattedSize, ', fullThreshold = ', fix^.fullThreshold);
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
        if pVarHeader(p - VarHeaderSize)^.ch.h and FirstFlag <> 0 then
          write(f, ', first');
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          write(f, ', last');
        if pVarHeader(p - VarHeaderSize)^.ch.h and FixedArenaFlag <> 0 then
          write(f, ', fixed arena');
        writeln(f);
        if pVarHeader(p - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          break;
        p := p + pVarHeader(p - VarHeaderSize)^.ch.h and uint32(VarSizeMask);
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
    vfrbn := varFree.First;
    if Assigned(vfrbn) then
    begin
      MaybeLE;
      write(f, 'Var free:');
      repeat
        write(f, ' ', pFreeVarChunk(pointer(vfrbn) - FreeVarChunkRbnOffset)^.size);
        vfrbn := varFree.Next(vfrbn);
      until not Assigned(vfrbn);
      writeln(f);
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
    dec(result, VarHeaderSize + FixedArenaDataOffset); { Prettier fit into OS chunks. }
  end;

  function HeapInc.ThreadState.AllocFixed(size: SizeUint): pointer;
  var
    sizeIndex, statv: SizeUint;
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
          arena^.sizeIndex := uint32(-1);
        end;
        if arena^.sizeIndex = sizeIndex then
          { Lucky! Just don’t reset the chunk and use its old freelist. }
        else
        begin
          arena^.sizeIndex := sizeIndex;
          arena^.firstFreeChunk := nil;
          arena^.usedSize := 0;
          arena^.formattedSize := 0;
          arena^.fullThreshold := pVarHeader(arena)[-1].ch.h and VarSizeMask - IndexToSize(sizeIndex) - (VarHeaderSize + FixedArenaDataOffset - 1); { available space - chunk size + 1. }
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

    size := IndexToSize(sizeIndex);
    statv := used + size;
    used := statv;
    inc(statv, gs.hugeUsed);
    if statv > maxUsed then
      maxUsed := statv;

    { arena from partialArenas has either free chunk or free unformatted space for a new chunk. }
    result := arena^.firstFreeChunk;
    if Assigned(result) then
      arena^.firstFreeChunk := pFreeChunk(result)^.next
    else
    begin
      result := pointer(arena) + (FixedArenaDataOffset + CommonHeaderSize) + arena^.formattedSize;
      pCommonHeader(result - CommonHeadersize)^.h := sizeIndex + arena^.formattedSize shl FixedArenaOffsetShift +
        (FixedFlag + (FixedArenaDataOffset + CommonHeaderSize) shl FixedArenaOffsetShift); { ← const }
      inc(arena^.formattedSize, size);
    end;
    inc(arena^.usedSize, size);
    if arena^.usedSize >= arena^.fullThreshold then
    begin
      inc(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h and VarSizeMask);
      { Remove arena from partialArenas[sizeIndex]. (It was first.) }
      nextArena := arena^.next;
      partialArenas[sizeIndex] := nextArena;
      if Assigned(nextArena) then
        nextArena^.prev := nil;
    end;
  end;

  function HeapInc.ThreadState.FreeFixed(p: pointer): SizeUint;
  var
    sizeIndex, usedSize: SizeUint;
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
    usedSize := arena^.usedSize;
    if usedSize >= arena^.fullThreshold then
    begin
      dec(allocatedByFullArenas[sizeIndex], pVarHeader(arena)[-1].ch.h and VarSizeMask);
      { Add arena to partialArenas[sizeIndex]. }
      nextArena := partialArenas[sizeIndex];
      arena^.next := nextArena;
      if Assigned(nextArena) then
        nextArena^.prev := arena;
      partialArenas[sizeIndex] := arena;
    end;
    dec(usedSize, result);
    arena^.usedSize := usedSize;
    if usedSize = 0 then
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
    dec(result, CommonHeaderSize);
  end;

  function HeapInc.ThreadState.GetOSChunk(minSize, maxSize: SizeUint): pOSChunk;
  var
    statv: SizeUint;
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
        statv := allocated + result^.size;
        allocated := statv;
        inc(statv, gs.hugeUsed);
        if statv > maxAllocated then
          maxAllocated := statv;
        exit;
      end;
    end;
  {$endif FPC_HAS_FEATURE_THREADING}
    result := AllocateOSChunk(minSize);
  end;

  function HeapInc.ThreadState.AllocateOSChunk(minSize: SizeUint): pOSChunk;
  var
    query, statv: SizeUint;
  begin
    query := used div 16 + minSize div 2; { Base: 6.25% of the memory used, so if GrowHeapSize2 = 1 Mb, 1 Mb OS allocations start at 16 Mb used. }
    if query > GrowHeapSize2 then { Limit by GrowHeapSize2. }
      query := GrowHeapSize2;
    if query < minSize then { But of course allocate at least the amount requested. }
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
    fv, toRemove: pFreeVarChunk;
    osChunk, osNext: pVarOSChunk;
    vSize, minSize, maxSize, statv: SizeUint;
    adj: RbTree.pNode;
  begin
    size := (size + (VarHeaderSize + VarSizeQuant - 1)) and SizeUint(-VarSizeQuant);
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if Assigned(toFree) then
      FlushToFree;
  {$endif}

    { Search varFree for smallest chunk ≥ size. }
    fv := pointer(varFree.LowerBound(pointer(PtrUint(size)))); { fv is temporarily RbTree.pNode. }

    if Assigned(fv) then
    begin
      dec(pointer(fv), FreeVarChunkRbnOffset); { Now fv is fair pFreeVarChunk. }
      toRemove := fv; { Postpone the removal to maybe ChangeInPlace. }
    end else
    begin
      toRemove := nil;
      minSize := VarOSChunkDataOffset + size;
      maxSize := GrowHeapSize2;
      if maxSize < minSize then
        maxSize := High(SizeUint); { Shouldn’t happen (unless GrowHeapSize2 is too small), so don’t optimize the case when it happens. }
      osChunk := pVarOSChunk(GetOSChunk(minSize, maxSize));
      if not Assigned(osChunk) then
        exit(nil);

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
      pVarHeader(pointer(fv) - VarHeaderSize)^.ch.h := uint32(vSize) + (FirstFlag or LastFlag);
      fv^.size := vSize;
    end;

    { Result will be allocated at the beginning of fv; maybe format the remainder and add it back to varFree. }
    result := fv;
    vSize := fv^.size - size;
    if (vSize > MaxFixedHeaderAndPayload) or
      { If fv is last in the OS chunk, remainder ≤ MaxFixedHeaderAndPayload is guaranteedly wasted.
        If fv is not last, there is a hope that occupied chunk to the right might get freed one day and merge with the remainder. }
      (vSize >= MinVarHeaderAndPayload) and (pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag = 0) then
    begin
      inc(pointer(fv), size); { result = allocated block, fv = remainder. }
      if Assigned(toRemove) then
      begin
        adj := varFree.Prev(@toRemove^.rbn);
        if not Assigned(adj) or (vSize >= pFreeVarChunk(pointer(adj) - FreeVarChunkRbnOffset)^.size) then { Tree position intact (free chunk size is always decreased). }
          varFree.ChangeInPlace(@toRemove^.rbn, @fv^.rbn)
        else
        begin
          varFree.Remove(@toRemove^.rbn);
          toRemove := nil;
        end;
      end;
      pVarHeader(pointer(fv) - VarHeaderSize)^.prevSize := size;
    {$ifdef FPC_HAS_FEATURE_THREADING}
      pVarHeader(pointer(fv) - VarHeaderSize)^.threadState := @self;
    {$endif}
      { Remainder is still last in the OS chunk if the original chunk was last. }
      pVarHeader(pointer(fv) - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag + uint32(vSize);
      fv^.size := vSize;
      if pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag = 0 then
        pVarHeader(pointer(fv) + vSize - VarHeaderSize)^.prevSize := vSize;

      if not Assigned(toRemove) then
        varFree.Add(@fv^.rbn);

      { Allocated chunk is still first in the OS chunk if the original chunk was first. }
      pVarHeader(result - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h and FirstFlag + UsedFlag + uint32(size);
    end else
    begin
      { Use the entire chunk. }
      if Assigned(toRemove) then
        varFree.Remove(@toRemove^.rbn);
      size := fv^.size;
      pVarHeader(result - VarHeaderSize)^.ch.h := pVarHeader(result - VarHeaderSize)^.ch.h and (FirstFlag or LastFlag) + UsedFlag + uint32(size);
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
    fSizeFlags: SizeUint;
    osChunk, osPrev, osNext: pVarOSChunk;
    freeOsNext: pFreeOSChunk;
    toRemove: pFreeVarChunk;
    adj: RbTree.pNode;
  begin
  {$ifdef FPC_HAS_FEATURE_THREADING}
    if pVarHeader(p - VarHeaderSize)^.threadState <> @self then
    begin
      EnterCriticalSection(gs.lock);
      if Assigned(pVarHeader(p - VarHeaderSize)^.threadState) then
      begin
        { Despite atomic Push lock must be held as otherwise target thread might end and destroy its threadState.
          However, target thread won’t block to free p, so PushToFree instantly invalidates p. }
        result := pVarHeader(p - VarHeaderSize)^.ch.h and uint32(VarSizeMask) - VarHeaderSize;
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
    toRemove := nil;

    { If next/prev are free, remove them from varFree and merge with f — (f)uture (f)ree chunk that starts at p, has fSizeFlags,
      and conveniently always inherits prevSize of its final location. }
    if fSizeFlags and LastFlag = 0 then
    begin
      p2 := p + result;
      if pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag = 0 then
      begin
        fSizeFlags := (fSizeFlags + pFreeVarChunk(p2)^.size) + pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag;
        toRemove := p2;
      end;
    end;

    if fSizeFlags and FirstFlag = 0 then
    begin
      p2 := p - pVarHeader(p - VarHeaderSize)^.prevSize;
      if pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag = 0 then
      begin
        p := p2;
        fSizeFlags := (fSizeFlags + pFreeVarChunk(p2)^.size) + pVarHeader(p2 - VarHeaderSize)^.ch.h and FirstFlag;
        { Keep the largest chunk in toRemove as it will have more chances for ChangeInPlace succeeding. }
        if Assigned(toRemove) then
          if pFreeVarChunk(p2)^.size >= toRemove^.size then
          begin
            varFree.Remove(@toRemove^.rbn);
            toRemove := p2;
          end else
            varFree.Remove(@pFreeVarChunk(p2)^.rbn)
        else
          toRemove := p2;
      end;
    end;

    { Turn p into a free chunk and add it back to varFree...
      unless it spans the entire OS chunk, in which case instead move the chunk from varOS to freeOS. }
    if fSizeFlags and (FirstFlag or LastFlag) <> FirstFlag or LastFlag then
    begin
      if Assigned(toRemove) then
      begin
        adj := varFree.Next(@toRemove^.rbn);
        if not Assigned(adj) or (fSizeFlags and VarSizeMask <= pFreeVarChunk(pointer(adj) - FreeVarChunkRbnOffset)^.size) then { Tree position intact (free chunk size is always increased). }
        begin
          if toRemove <> p then { Might happen. Skip a no-op ChangeInPlace. }
            varFree.ChangeInPlace(@toRemove^.rbn, @pFreeVarChunk(p)^.rbn);
        end else
        begin
          varFree.Remove(@toRemove^.rbn);
          toRemove := nil;
        end;
      end;

      if fSizeFlags and LastFlag = 0 then
        pVarHeader(p + fSizeFlags and VarSizeMask - VarHeaderSize)^.prevSize := fSizeFlags and VarSizeMask;

      pVarHeader(p - VarHeaderSize)^.ch.h := uint32(fSizeFlags) - UsedFlag;
      pFreeVarChunk(p)^.size := fSizeFlags and VarSizeMask;

      if not Assigned(toRemove) then
        varFree.Add(@pFreeVarChunk(p)^.rbn);
    end else
    begin
      if Assigned(toRemove) then
        varFree.Remove(@toRemove^.rbn);
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
    toRemove: pFreeVarChunk;
    adj: RbTree.pNode; { Assigned(adj) means ChangeInPlace is NOT possible. }
  begin
    if (size < MinVarHeaderAndPayload - VarHeaderSize)
      or (size > GrowHeapSize2) { Not strictly necessary but rejects clearly wrong values early so adding headers to the size doesn’t overflow. }
    {$ifdef FPC_HAS_FEATURE_THREADING}
      or (pVarHeader(p - VarHeaderSize)^.threadState <> @self)
    {$endif}
    then
      exit(nil);
    size := (size + (VarHeaderSize + VarSizeQuant - 1)) and SizeUint(-VarSizeQuant);
    result := p; { From now on use result instead of p (saves a register). }

    oldpsize := pVarHeader(result - VarHeaderSize)^.ch.h and uint32(VarSizeMask);
    p2 := result + oldpsize;
    { (f)uture (f)ree chunk starting at p + size and having fSizeFlags will be created at the end (replacing toRemove), must exit before that if not required. }
    if size <= oldpsize then
    begin
      { Shrink. Maybe. }
      fSizeFlags := oldpsize - size;
      toRemove := nil;

      if pVarHeader(result - VarHeaderSize)^.ch.h and LastFlag <> 0 then
      begin
        { Has nothing to the right: create free chunk if > MaxFixedHeaderAndPayload, otherwise report success but change nothing. }
        if fSizeFlags <= MaxFixedHeaderAndPayload then
          exit;
        dec(used, fSizeFlags);
        inc(fSizeFlags, LastFlag);
      end
      else if pVarHeader(p2 - VarHeaderSize)^.ch.h and UsedFlag <> 0 then
      begin
        { Has used chunk to the right: create free chunk if ≥ MinVarHeaderAndPayload, following the same logic as in AllocVar. }
        if fSizeFlags < MinVarHeaderAndPayload then
          exit;
        dec(used, fSizeFlags);
      end else
      begin
        if fSizeFlags = 0 then { Exit early if going to be a no-op. Two branches above do the same with different checks. }
          exit;
        dec(used, fSizeFlags);
        { Has empty chunk to the right: extend with freed space. }
        fSizeFlags := fSizeFlags + pFreeVarChunk(p2)^.size + pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag;
        toRemove := p2;
        { Free chunk size increased, so the tree position is intact if still ≤ than the next. }
        adj := varFree.Next(@toRemove^.rbn);
        if Assigned(adj) and (fSizeFlags and VarSizeMask <= pFreeVarChunk(pointer(adj) - FreeVarChunkRbnOffset)^.size) then
          adj := nil;
      end;

      { Update p size. }
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) + pVarHeader(result - VarHeaderSize)^.ch.h and (FirstFlag or UsedFlag);
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
          inc(fSizeFlags, LastFlag);

      growby := pFreeVarChunk(p2)^.size - fSizeFlags and VarSizeMask;
      size := oldpsize + growby;
      statv := used + growby;
      used := statv;
      inc(statv, gs.hugeUsed);
      if statv > maxUsed then
        maxUsed := statv;

      toRemove := p2;
      { Update p size. }
      pVarHeader(result - VarHeaderSize)^.ch.h := uint32(size) + pVarHeader(result - VarHeaderSize)^.ch.h and (FirstFlag or UsedFlag);
      { No empty chunk? }
      if fSizeFlags = 0 then
      begin
        varFree.Remove(@toRemove^.rbn);
        if pVarHeader(p2 - VarHeaderSize)^.ch.h and LastFlag <> 0 then
          inc(pVarHeader(result - VarHeaderSize)^.ch.h, LastFlag)
        else
          pVarHeader(result + size - VarHeaderSize)^.prevSize := size;
        exit;
      end;
      { Free chunk size decreased, so the tree position is intact if still ≥ than the previous. }
      adj := varFree.Prev(@toRemove^.rbn);
      if Assigned(adj) and (fSizeFlags and VarSizeMask >= pFreeVarChunk(pointer(adj) - FreeVarChunkRbnOffset)^.size) then
        adj := nil;
    end else
      exit(nil);

    { Format new free var chunk. }
    fp := result + size;
    if Assigned(toRemove) then
      if not Assigned(adj) then
        varFree.ChangeInPlace(@toRemove^.rbn, @pFreeVarChunk(fp)^.rbn)
      else
      begin
        varFree.Remove(@toRemove^.rbn);
        toRemove := nil;
      end;
    pVarHeader(fp - VarHeaderSize)^.prevSize := size;
  {$ifdef FPC_HAS_FEATURE_THREADING}
    pVarHeader(fp - VarHeaderSize)^.threadState := @self;
  {$endif}
    pVarHeader(fp - VarHeaderSize)^.ch.h := uint32(fSizeFlags);
    pFreeVarChunk(fp)^.size := fSizeFlags and VarSizeMask;
    if fSizeFlags and LastFlag = 0 then
      pVarHeader(fp + fSizeFlags and VarSizeMask - VarHeaderSize)^.prevSize := fSizeFlags and VarSizeMask;
    if not Assigned(toRemove) then
      varFree.Add(@pFreeVarChunk(fp)^.rbn);
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
      if pCommonHeader(pointer(tf) - CommonHeaderSize)^.h and FixedFlag <> 0 then
        FreeFixed(tf)
      else
        FreeVar(tf);
      tf := nx;
    end;
  end;

  procedure HeapInc.ThreadState.Orphan;
  var
    lastFree, nextFree: pFreeOSChunk;
    vOs, nextVOs, lastVOs: pVarOSChunk;
  begin
    FlushToFree;
    FreeEmptyArenas(0);
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

  procedure HeapInc.ThreadState.AdoptArena(arena: pFixedArena);
  var
    sizeIndex: SizeUint;
    nextArena: pFixedArena;
  begin
    sizeIndex := pCommonHeader(pointer(arena) + FixedArenaDataOffset)^.h and SizeIndexMask;
    inc(used, arena^.usedSize); { maxUsed is updated at the end of AdoptVarOwner. }

    if arena^.usedSize = 0 then
    begin
      { Add arena to emptyArenas. }
      arena^.next := emptyArenas;
      emptyArenas := arena;
      inc(nEmptyArenas); { May exceed MaxKeptFixedArenas, extra arenas are freed at the end of AdoptVarOwner. }
    end
    else if arena^.usedSize < arena^.fullThreshold then
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
        varFree.Add(@pFreeVarChunk(p)^.rbn)
      else if h and FixedArenaFlag <> 0 then
         AdoptArena(p)
      else
        inc(used, h and uint32(VarSizeMask)); { maxUsed is updated after the loop. }
      inc(p, h and uint32(VarSizeMask));
    until h and LastFlag <> 0;

    FreeEmptyArenas(MaxKeptFixedArenas);

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
      inc(p, h and uint32(VarSizeMask));
    until h and LastFlag <> 0;
  end;

  procedure HeapInc.ThreadState.FreeEmptyArenas(untilCount: SizeUint);
  var
    arena: pFixedArena;
  begin
    while nEmptyArenas > untilCount do
    begin
      arena := emptyArenas;
      emptyArenas := arena^.next;
      dec(nEmptyArenas);
      FreeVar(arena);
    end;
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
  else if size < GrowHeapSize2 div 2 then { Approximate idea on the max size of the variable chunk. Approximate because size does not include headers but GrowHeapSize2 does. }
    result := ts^.AllocVar(size, false)
  else
    result := ts^.AllocHuge(size);
end;

function SysFreeMem(p: pointer): ptruint;
var
  ts: HeapInc.pThreadState;
  h: uint32;
begin
  result := 0;
  if Assigned(p) then
    begin
      ts := @HeapInc.thisTs;
      h := HeapInc.pCommonHeader(p - HeapInc.CommonHeaderSize)^.h;
      if h and HeapInc.FixedFlag <> 0 then
        result := ts^.FreeFixed(p)
      else if h <> HeapInc.HugeHeader then
        result := ts^.FreeVar(p)
      else
        result := ts^.FreeHuge(p);
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
{$elseif defined(HAS_SYSOSFREE)}
  HeapInc.thisTs.freeOS.FreeAll;
{$endif FPC_HAS_FEATURE_THREADING | defined(HAS_SYSOSFREE)}
end;

{$endif ndef FPC_NO_DEFAULT_HEAP}

{$endif ndef HAS_MEMORYMANAGER and (defined(FPC_HAS_FEATURE_HEAP) or defined(FPC_IN_HEAPMGR))}
