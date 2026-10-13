{
  Tiny PS1 heap manager - PCSX-Redux PSYQo Heap Viewer compatible.

  Layout of EVERY block (allocated or free):

      offset 0: Next    (4 bytes; used only while free)
      offset 4: Size    (4 bytes; total size INCLUDING this header)
      offset 8: user payload

  Size must be 8-byte aligned and contain NO allocation/free flags.
  Free blocks are linked in ascending-address order.  Their list ends at
  @(_heapMetadata.MarkerNext), never at nil.

  The growing heap is [_heapStart, _heapEnd).  Above _heapEnd remains
  untouched space available up to _heapLimit.  The PCSX viewer shows the
  committed portion, not the entire maximum heap capacity.

  This file implements the FPC Sys* memory-manager functions, so normal
  GetMem/FreeMem/ReAllocMem and most RTL dynamic allocations are visible.
  This implementation is not interrupt-safe or thread-safe.
}

{$define HAS_MEMORYMANAGER}

const
  PS1_HEAP_ALIGN = SizeUInt(8);
  PS1_HEAP_HEADER = SizeUInt(8);


type
  PHeapBlock = ^THeapBlock;
  THeapBlock = packed record
    Next : PHeapBlock;      { +0: next FREE block (otherwise unused) }
    Size : SizeUInt;        { +4: TOTAL bytes, 8-aligned, no flags }
  end;

  THeapMetadata = packed record
    Head           : PHeapBlock; { +0: first free block or Marker }
    Bottom         : Pointer;    { +4: heap beginning }
    Top            : Pointer;    { +8: current committed heap end }
    MaximumHeapEnd : Pointer;    { +12: highest historical heap end }
    MarkerNext     : PHeapBlock; { +16: embedded end-of-list sentinel }
  end;

var
  _heapStart   : PtrUInt = 0;
  _heapEnd     : PtrUInt = 0;
  _heapLimit   : PtrUInt = 0;

  _heapMetadata: THeapMetadata;

  { Payload byte counters; allocation headers are excluded. }
  _heapUsed    : SizeUInt = 0;
  _heapMaxUsed : SizeUInt = 0;

{ --------------------------------------------------------------------------
  Internal helpers
  -------------------------------------------------------------------------- }

function _HeapMarker: PHeapBlock; inline;
begin
  Result := PHeapBlock(@_heapMetadata.MarkerNext);
end;

function _AlignHeapSize(Value: SizeUInt): SizeUInt; inline;
begin
  Result := (Value + PS1_HEAP_ALIGN - 1) and
            not SizeUInt(PS1_HEAP_ALIGN - 1);
end;

function _BlockSize(Block: PHeapBlock): SizeUInt; inline;
begin
  Result := Block^.Size - PS1_HEAP_HEADER;
end;

function _BlockData(Block: PHeapBlock): Pointer; inline;
begin
  Result := Pointer(PtrUInt(Block) + PS1_HEAP_HEADER);
end;

function _DataBlock(P: Pointer): PHeapBlock; inline;
begin
  Result := PHeapBlock(PtrUInt(P) - PS1_HEAP_HEADER);
end;

procedure _HeapAddUsed(Size: SizeUInt); inline;
begin
  _heapUsed := _heapUsed + Size;
  if _heapUsed > _heapMaxUsed then
    _heapMaxUsed := _heapUsed;
end;

procedure _HeapSubUsed(Size: SizeUInt); inline;
begin
  if Size <= _heapUsed then
    _heapUsed := _heapUsed - Size
  else
    _heapUsed := 0;
end;

{ Validate that Addr is an actual block boundary by scanning the heap.
  A corrupted block size makes the function return False. }
function _IsBlockStart(Addr: PtrUInt): Boolean;
var
  Cur, N: PtrUInt;
  Total: SizeUInt;
begin
  Result := False;
  if (Addr < _heapStart) or (Addr >= _heapEnd) then Exit;

  Cur := _heapStart;
  while Cur < _heapEnd do
  begin
    if Cur = Addr then
    begin
      Result := True;
      Exit;
    end;

    Total := PHeapBlock(Cur)^.Size;
    if (Total < PS1_HEAP_HEADER + PS1_HEAP_ALIGN) or
       ((Total and (PS1_HEAP_ALIGN - 1)) <> 0) or
       (Total > _heapEnd - Cur) then Exit;

    N := Cur + Total;
    if N <= Cur then Exit;
    Cur := N;
  end;
end;

{ Find a free block by exact address; optional result Prev is nil when
  the matching block is first in the free list. }
function _FindFree(Block: PHeapBlock; var Prev: PHeapBlock): Boolean;
var
  Cur: PHeapBlock;
begin
  Prev := nil;
  Cur := _heapMetadata.Head;
  while Cur <> _HeapMarker do
  begin
    if Cur = nil then Break;
    if PtrUInt(Cur) >= PtrUInt(Block) then Break;
    Prev := Cur;
    Cur := Cur^.Next;
  end;
  Result := Cur = Block;
end;

{ Unlink a known free block, with its preceding free block (or nil). }
procedure _UnlinkFree(Block, Prev: PHeapBlock); inline;
begin
  if Prev = nil then
    _heapMetadata.Head := Block^.Next
  else
    Prev^.Next := Block^.Next;
end;

{ Insert an existing free block into the sorted free list.  Merge adjacent
  blocks and discard the final free block by moving heap Top downward. }
procedure _InsertFree(Block: PHeapBlock);
var
  Prev, PrevPrev, Cur: PHeapBlock;
begin
  Prev := nil;
  PrevPrev := nil;
  Cur := _heapMetadata.Head;

  while (Cur <> nil) and (Cur <> _HeapMarker) and
        (PtrUInt(Cur) < PtrUInt(Block)) do
  begin
    PrevPrev := Prev;
    Prev := Cur;
    Cur := Cur^.Next;
  end;

  { Normally impossible for a valid non-free block. }
  if Cur = nil then Exit;
  if Cur = Block then Exit;
  if (Prev <> nil) and
     (PtrUInt(Prev) + Prev^.Size > PtrUInt(Block)) then Exit;
  if (Cur <> _HeapMarker) and
     (PtrUInt(Block) + Block^.Size > PtrUInt(Cur)) then Exit;

  Block^.Next := Cur;
  if Prev = nil then
    _heapMetadata.Head := Block
  else
    Prev^.Next := Block;

  { Coalesce forwards. }
  if (Cur <> _HeapMarker) and
     (PtrUInt(Block) + Block^.Size = PtrUInt(Cur)) then
  begin
    Block^.Size := Block^.Size + Cur^.Size;
    Block^.Next := Cur^.Next;
  end;

  { Coalesce backwards. }
  if (Prev <> nil) and
     (PtrUInt(Prev) + Prev^.Size = PtrUInt(Block)) then
  begin
    Prev^.Size := Prev^.Size + Block^.Size;
    Prev^.Next := Block^.Next;
    Block := Prev;
    { Remember the predecessor of the newly merged block. }
    Prev := PrevPrev;
  end;

  { Release a free block at the top of the committed heap.  The free
    list must not contain blocks beyond Top. }
  if PtrUInt(Block) + Block^.Size = _heapEnd then
  begin
    _UnlinkFree(Block, Prev);
    _heapEnd := PtrUInt(Block);
    _heapMetadata.Top := Pointer(_heapEnd);
  end;
end;

{ --------------------------------------------------------------------------
  Initialize the heap after BSS.  Called by the PS1 RTL startup code.
  -------------------------------------------------------------------------- }
procedure _InitHeap(TheBSSend, TheHeapLimit: PtrUInt);
begin
  _heapStart := (TheBSSend + PS1_HEAP_ALIGN - 1) and
                not PtrUInt(PS1_HEAP_ALIGN - 1);
  _heapLimit := TheHeapLimit and not PtrUInt(PS1_HEAP_ALIGN - 1);
  if _heapLimit < _heapStart then _heapLimit := _heapStart;
  _heapEnd := _heapStart;

  _heapUsed := 0;
  _heapMaxUsed := 0;

  _heapMetadata.Bottom := Pointer(_heapStart);
  _heapMetadata.Top := Pointer(_heapEnd);
  _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);
  _heapMetadata.MarkerNext := nil;
  _heapMetadata.Head := _HeapMarker;

  if pcsxPresent then 
    PLongWord(PtrUInt($1F8020A0))^ := LongWord(PtrUInt(@_heapMetadata));
  
end;

{ --------------------------------------------------------------------------
  Allocate: first-fit reuse of free blocks, or extend the committed heap.
  -------------------------------------------------------------------------- }
function _Malloc(Size: SizeUInt): Pointer;
var
  Prev, Block, Remain: PHeapBlock;
  Needed, Left, Payload: SizeUInt;
begin
  Result := nil;
  if Size = 0 then Exit;
  if (_heapStart = 0) or (_heapEnd > _heapLimit) then Exit;
  if Size > High(SizeUInt) - (PS1_HEAP_HEADER + PS1_HEAP_ALIGN - 1) then Exit;

  Payload := _AlignHeapSize(Size);
  Needed := Payload + PS1_HEAP_HEADER;

  Prev := nil;
  Block := _heapMetadata.Head;
  while Block <> _HeapMarker do
  begin
    if Block = nil then Exit; { corrupted free list }
    if Block^.Size >= Needed then
    begin
      Left := Block^.Size - Needed;
      if Left >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
      begin
        Remain := PHeapBlock(PtrUInt(Block) + Needed);
        Remain^.Next := Block^.Next;
        Remain^.Size := Left;
        if Prev = nil then
          _heapMetadata.Head := Remain
        else
          Prev^.Next := Remain;
        Block^.Size := Needed;
      end
      else
        _UnlinkFree(Block, Prev);

      Block^.Next := nil;
      _HeapAddUsed(_BlockSize(Block));
      Result := _BlockData(Block);
      Exit;
    end;
    Prev := Block;
    Block := Block^.Next;
  end;

  if Needed > _heapLimit - _heapEnd then Exit;
  Block := PHeapBlock(_heapEnd);
  Block^.Next := nil;
  Block^.Size := Needed;
  _heapEnd := _heapEnd + Needed;
  _heapMetadata.Top := Pointer(_heapEnd);
  if _heapEnd > PtrUInt(_heapMetadata.MaximumHeapEnd) then
    _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);

  _HeapAddUsed(Payload);
  Result := _BlockData(Block);
end;

{ --------------------------------------------------------------------------
  Free: insert in the free list, merge adjacent blocks, trim free tail.
  -------------------------------------------------------------------------- }
procedure _Free(P: Pointer);
var
  Block, Prev: PHeapBlock;
  Addr: PtrUInt;
  Size: SizeUInt;
begin
  if P = nil then Exit;
  if (PtrUInt(P) and (PS1_HEAP_ALIGN - 1)) <> 0 then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;
  Addr := PtrUInt(P) - PS1_HEAP_HEADER;
  if not _IsBlockStart(Addr) then Exit;
  Block := PHeapBlock(Addr);
  if _FindFree(Block, Prev) then Exit; { double free }

  Size := _BlockSize(Block);
  _HeapSubUsed(Size);
  _InsertFree(Block);
end;

{ --------------------------------------------------------------------------
  Copy bytes (avoids libc dependencies).
  -------------------------------------------------------------------------- }
procedure _CopyMem(Src, Dst: Pointer; Count: SizeUInt); inline;
var
  S, D: PByte;
begin
  S := PByte(Src);
  D := PByte(Dst);
  while Count <> 0 do
  begin
    D^ := S^;
    Inc(S);
    Inc(D);
    Dec(Count);
  end;
end;

{ --------------------------------------------------------------------------
  Reallocate, preferring in-place shrink/grow.
  -------------------------------------------------------------------------- }
function _Realloc(P: Pointer; Size: SizeUInt): Pointer;
var
  Block, Neighbor, FreePart, Prev: PHeapBlock;
  OldPayload, NewPayload, OldTotal, Needed, Capacity, Extra: SizeUInt;
  NewPtr: Pointer;
begin
  Result := nil;
  if P = nil then
  begin
    Result := _Malloc(Size);
    Exit;
  end;
  if Size = 0 then
  begin
    _Free(P);
    Exit;
  end;
  if Size > High(SizeUInt) - (PS1_HEAP_HEADER + PS1_HEAP_ALIGN - 1) then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;
  if not _IsBlockStart(PtrUInt(P) - PS1_HEAP_HEADER) then Exit;

  Block := _DataBlock(P);
  if _FindFree(Block, Prev) then Exit;
  OldTotal := Block^.Size;
  OldPayload := OldTotal - PS1_HEAP_HEADER;
  NewPayload := _AlignHeapSize(Size);
  Needed := NewPayload + PS1_HEAP_HEADER;

  if Needed = OldTotal then
  begin
    Result := P;
    Exit;
  end;

  if Needed < OldTotal then
  begin
    { A 0..8 byte remainder is left inside the allocation. }
    if OldTotal - Needed >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
    begin
      FreePart := PHeapBlock(PtrUInt(Block) + Needed);
      FreePart^.Size := OldTotal - Needed;
      FreePart^.Next := nil;
      Block^.Size := Needed;
      _HeapSubUsed(OldPayload - NewPayload);
      _InsertFree(FreePart);
    end;
    Result := P;
    Exit;
  end;

  { Expand into an immediately following free block if possible. }
  if PtrUInt(Block) + OldTotal < _heapEnd then
  begin
    Neighbor := PHeapBlock(PtrUInt(Block) + OldTotal);
    if _FindFree(Neighbor, Prev) then
    begin
      Capacity := OldTotal + Neighbor^.Size;
      if Capacity >= Needed then
      begin
        _UnlinkFree(Neighbor, Prev);
        Block^.Size := Capacity;
        Block^.Next := nil;
        if Capacity - Needed >= PS1_HEAP_HEADER + PS1_HEAP_ALIGN then
        begin
          FreePart := PHeapBlock(PtrUInt(Block) + Needed);
          FreePart^.Size := Capacity - Needed;
          FreePart^.Next := nil;
          Block^.Size := Needed;
          _InsertFree(FreePart);
        end;
        _HeapAddUsed(_BlockSize(Block) - OldPayload);
        Result := P;
        Exit;
      end;
    end;
  end;

  { Expand an allocation at the very top without copying. }
  if PtrUInt(Block) + OldTotal = _heapEnd then
  begin
    Extra := Needed - OldTotal;
    if Extra <= _heapLimit - _heapEnd then
    begin
      Block^.Size := Needed;
      _heapEnd := _heapEnd + Extra;
      _heapMetadata.Top := Pointer(_heapEnd);
      if _heapEnd > PtrUInt(_heapMetadata.MaximumHeapEnd) then
        _heapMetadata.MaximumHeapEnd := Pointer(_heapEnd);
      _HeapAddUsed(Extra);
      Result := P;
      Exit;
    end;
  end;

  { Fallback: allocate elsewhere, copy, then release the old block. }
  NewPtr := _Malloc(Size);
  if NewPtr = nil then Exit;
  if OldPayload < Size then
    _CopyMem(P, NewPtr, OldPayload)
  else
    _CopyMem(P, NewPtr, Size);
  _Free(P);
  Result := NewPtr;
end;

{ --------------------------------------------------------------------------
  Optional libc-style internal helper.
  -------------------------------------------------------------------------- }
function _Calloc(Num, Size: SizeUInt): Pointer;
var
  Total: SizeUInt;
begin
  Result := nil;
  if (Num <> 0) and (Size > High(SizeUInt) div Num) then Exit;
  Total := Num * Size;
  Result := _Malloc(Total);
  if Result <> nil then FillChar(Result^, Total, 0);
end;

{ --------------------------------------------------------------------------
  Functions expected by FPC's MemoryManager record.
  -------------------------------------------------------------------------- }
function SysGetMem(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
end;

function SysFreeMem(P: Pointer): PtrUInt;
var
  Size: PtrUInt;
begin
  if P = nil then
  begin
    Result := 0;
    Exit;
  end;
  if (PtrUInt(P) < _heapStart + PS1_HEAP_HEADER) or
     (not _IsBlockStart(PtrUInt(P) - PS1_HEAP_HEADER)) then
  begin
    Result := 0;
    Exit;
  end;
  Size := _BlockSize(_DataBlock(P));
  _Free(P);
  Result := Size;
end;

function SysFreeMemSize(P: Pointer; Size: PtrUInt): PtrUInt;
begin
  Result := SysFreeMem(P);
end;

function SysMemSize(P: Pointer): PtrUInt;
begin
  Result := 0;
  if P = nil then Exit;
  if PtrUInt(P) < _heapStart + PS1_HEAP_HEADER then Exit;
  if not _IsBlockStart(PtrUInt(P) - PS1_HEAP_HEADER) then Exit;
  Result := _BlockSize(_DataBlock(P));
end;

function SysAllocMem(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
  if Result <> nil then FillChar(Result^, SysMemSize(Result), 0);
end;

function SysReAllocMem(var P: Pointer; Size: PtrUInt): Pointer;
var
  NewPtr: Pointer;
begin
  NewPtr := _Realloc(P, Size);
  { On failure, keep the old pointer.  Size=0 releases it. }
  if (NewPtr <> nil) or (Size = 0) then P := NewPtr;
  Result := NewPtr;
end;

function SysGetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function SysGetHeapStatus: THeapStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ Kept for compatibility with the PS1 RTL OS allocator interface. }
function SysOSAlloc(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
end;

procedure SysOSFree(P: Pointer; Size: PtrUInt);
begin
  _Free(P);
end;

{ Generic heap.inc normally supplies these, disabled by HAS_MEMORYMANAGER. }
procedure InitHeap; public name '_FPC_InitHeap';
begin
end;

procedure RelocateHeap;
begin
end;

procedure FinalizeHeap;
begin
end;
