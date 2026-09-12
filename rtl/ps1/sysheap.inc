{
  Tiny PS1 heap manager.

  Layout of every allocation:

      +------------------+
      | SizeFlags        |  4 bytes
      | Next             |  4 bytes
      +------------------+
      | user data        |
      | ...              |
      +------------------+

  All blocks are 8-byte aligned.

  SizeFlags bit 0:
      0 = used
      1 = free

  The remaining bits contain the payload size.

  This allocator directly implements the FPC Sys* memory-manager
  functions.  The generic heap.inc HeapInc backend is therefore
  disabled.
}

{$define HAS_MEMORYMANAGER}


const
  PS1_HEAP_ALIGN = SizeUInt(8);
  PS1_BLOCK_FREE = SizeUInt(1);


type
  PHeapBlock = ^THeapBlock;

  THeapBlock = record
    SizeFlags : SizeUInt;
    Next      : PHeapBlock;
  end;


var
  _heapStart   : PtrUInt = 0;
  _heapEnd     : PtrUInt = 0;
  _heapLimit   : PtrUInt = 0;

  _heapHead    : PHeapBlock = nil;
  _heapTail    : PHeapBlock = nil;

  _heapUsed    : SizeUInt = 0;
  _heapMaxUsed : SizeUInt = 0;


{ --------------------------------------------------------------------------
  Small helpers
  -------------------------------------------------------------------------- }

function _AlignHeapSize(Value: SizeUInt): SizeUInt; inline;
begin
  Result :=
    (Value + PS1_HEAP_ALIGN - 1) and
    not SizeUInt(PS1_HEAP_ALIGN - 1);
end;


function _BlockSize(Block: PHeapBlock): SizeUInt; inline;
begin
  Result :=
    Block^.SizeFlags and
    not SizeUInt(PS1_HEAP_ALIGN - 1);
end;


function _BlockIsFree(Block: PHeapBlock): Boolean; inline;
begin
  Result := (Block^.SizeFlags and PS1_BLOCK_FREE) <> 0;
end;


procedure _SetBlock(Block: PHeapBlock;
                    Size: SizeUInt;
                    IsFree: Boolean); inline;
begin
  Block^.SizeFlags := Size;

  if IsFree then
    Block^.SizeFlags := Block^.SizeFlags or PS1_BLOCK_FREE;
end;


function _BlockData(Block: PHeapBlock): Pointer; inline;
begin
  Result :=
    Pointer(
      PtrUInt(Block) + SizeUInt(SizeOf(THeapBlock))
    );
end;


function _DataBlock(P: Pointer): PHeapBlock; inline;
begin
  Result :=
    PHeapBlock(
      PtrUInt(P) - SizeUInt(SizeOf(THeapBlock))
    );
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


{ --------------------------------------------------------------------------
  Split a block.

       old block

       [ header |                 payload                  ]

  becomes

       [ header | wanted ][ header | remaining free       ]
  -------------------------------------------------------------------------- }

function _SplitBlock(Block: PHeapBlock;
                     Wanted: SizeUInt): PHeapBlock;
var
  OldSize : SizeUInt;
  Remain  : SizeUInt;
  NewBlock: PHeapBlock;
  WasFree : Boolean;
begin
  Result := nil;

  OldSize := _BlockSize(Block);

  if OldSize <
     Wanted +
     SizeUInt(SizeOf(THeapBlock)) +
     PS1_HEAP_ALIGN then
    Exit;

  WasFree := _BlockIsFree(Block);

  NewBlock :=
    PHeapBlock(
      PtrUInt(Block) +
      SizeUInt(SizeOf(THeapBlock)) +
      Wanted
    );

  Remain :=
    OldSize -
    Wanted -
    SizeUInt(SizeOf(THeapBlock));

  NewBlock^.Next := Block^.Next;
  _SetBlock(NewBlock, Remain, True);

  Block^.Next := NewBlock;
  _SetBlock(Block, Wanted, WasFree);

  if _heapTail = Block then
    _heapTail := NewBlock;

  Result := NewBlock;
end;


{ --------------------------------------------------------------------------
  Merge Block with the free block immediately following it.
  -------------------------------------------------------------------------- }

procedure _MergeNext(Block: PHeapBlock);
var
  NextBlock: PHeapBlock;
  NewSize  : SizeUInt;
  WasFree  : Boolean;
begin
  if Block = nil then
    Exit;

  NextBlock := Block^.Next;

  if NextBlock = nil then
    Exit;

  if not _BlockIsFree(NextBlock) then
    Exit;

  WasFree := _BlockIsFree(Block);

  NewSize :=
    _BlockSize(Block) +
    SizeUInt(SizeOf(THeapBlock)) +
    _BlockSize(NextBlock);

  Block^.Next := NextBlock^.Next;

  _SetBlock(Block, NewSize, WasFree);

  if _heapTail = NextBlock then
    _heapTail := Block;
end;


function _FindPrevious(Block: PHeapBlock): PHeapBlock;
var
  Cur: PHeapBlock;
begin
  Result := nil;

  Cur := _heapHead;

  while Cur <> nil do
  begin
    if Cur^.Next = Block then
    begin
      Result := Cur;
      Exit;
    end;

    Cur := Cur^.Next;
  end;
end;


{ --------------------------------------------------------------------------
  Heap initialization

  Use BSS end as bottom and StackBottom as upper limit.
  -------------------------------------------------------------------------- }

procedure _InitHeap(TheBSSend, TheHeapLimit: PtrUInt);
begin
  _heapStart :=
    (TheBSSend + PS1_HEAP_ALIGN - 1) and
    not PtrUInt(PS1_HEAP_ALIGN - 1);

  _heapEnd := _heapStart;

  _heapLimit :=
    TheHeapLimit and
    not PtrUInt(PS1_HEAP_ALIGN - 1);

  _heapHead := nil;
  _heapTail := nil;

  _heapUsed := 0;
  _heapMaxUsed := 0;
end;


{ --------------------------------------------------------------------------
  Allocate
  -------------------------------------------------------------------------- }

function _Malloc(Size: SizeUInt): Pointer;
var
  Block    : PHeapBlock;
  NewBlock : PHeapBlock;
  NewSize  : SizeUInt;
  Total    : SizeUInt;
begin
  Result := nil;

  if Size = 0 then
    Exit;

  NewSize := _AlignHeapSize(Size);


  { First-fit search through previously freed blocks. }

  Block := _heapHead;

  while Block <> nil do
  begin
    if _BlockIsFree(Block) and
       (_BlockSize(Block) >= NewSize) then
    begin

      _SplitBlock(Block, NewSize);

      _SetBlock(
        Block,
        _BlockSize(Block),
        False
      );

      _HeapAddUsed(_BlockSize(Block));

      Result := _BlockData(Block);
      Exit;
    end;

    Block := Block^.Next;
  end;


  { Nothing reusable. Append a new block. }

  Total :=
    SizeUInt(SizeOf(THeapBlock)) +
    NewSize;

  if _heapEnd >= _heapLimit then
    Exit;

  if Total > (_heapLimit - _heapEnd) then
    Exit;


  NewBlock := PHeapBlock(_heapEnd);

  NewBlock^.Next := nil;
  _SetBlock(NewBlock, NewSize, False);


  if _heapTail <> nil then
    _heapTail^.Next := NewBlock
  else
    _heapHead := NewBlock;

  _heapTail := NewBlock;


  _heapEnd :=
    _heapEnd +
    Total;


  _HeapAddUsed(NewSize);

  Result := _BlockData(NewBlock);
end;


{ --------------------------------------------------------------------------
  Free
  -------------------------------------------------------------------------- }

procedure _Free(P: Pointer);
var
  Block: PHeapBlock;
  Prev : PHeapBlock;
  Size : SizeUInt;
begin
  if P = nil then
    Exit;

  Block := _DataBlock(P);

  if _BlockIsFree(Block) then
    Exit;


  Size := _BlockSize(Block);

  _HeapSubUsed(Size);

  _SetBlock(Block, Size, True);


  { Merge forward. }

  _MergeNext(Block);


  { Merge backward. }

  Prev := _FindPrevious(Block);

  if (Prev <> nil) and _BlockIsFree(Prev) then
  begin
    _MergeNext(Prev);
    Block := Prev;
  end;


  { If the final block is free we can move the heap end backwards. }

  if Block^.Next = nil then
  begin
    _heapEnd := PtrUInt(Block);

    Prev := _FindPrevious(Block);

    if Prev <> nil then
    begin
      Prev^.Next := nil;
      _heapTail := Prev;
    end
    else
    begin
      _heapHead := nil;
      _heapTail := nil;
    end;
  end;
end;


{ --------------------------------------------------------------------------
  Reallocate
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


function _Realloc(P: Pointer; Size: SizeUInt): Pointer;
var
  Block    : PHeapBlock;
  FreePart : PHeapBlock;

  OldSize  : SizeUInt;
  NewSize  : SizeUInt;
  Capacity : SizeUInt;
  Extra    : SizeUInt;
  CopySize : SizeUInt;

  NewPtr   : Pointer;
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


  Block := _DataBlock(P);

  OldSize := _BlockSize(Block);
  NewSize := _AlignHeapSize(Size);


  { --------------------------------------------------------
    Same size.
    -------------------------------------------------------- }

  if NewSize = OldSize then
  begin
    Result := P;
    Exit;
  end;


  { --------------------------------------------------------
    Shrink in place.
    -------------------------------------------------------- }

  if NewSize < OldSize then
  begin
    FreePart := _SplitBlock(Block, NewSize);

    if FreePart <> nil then
    begin
      _HeapSubUsed(
        OldSize - _BlockSize(Block)
      );

      { New free tail may touch an already free block. }

      _MergeNext(FreePart);
    end;

    Result := P;
    Exit;
  end;


  { --------------------------------------------------------
    Try growing into the following free block.
    -------------------------------------------------------- }

  if (Block^.Next <> nil) and
     _BlockIsFree(Block^.Next) then
  begin
    Capacity :=
      OldSize +
      SizeUInt(SizeOf(THeapBlock)) +
      _BlockSize(Block^.Next);

    if Capacity >= NewSize then
    begin
      _MergeNext(Block);

      _SplitBlock(Block, NewSize);

      _HeapAddUsed(
        _BlockSize(Block) - OldSize
      );

      Result := P;
      Exit;
    end;
  end;


  { --------------------------------------------------------
    Last block can grow directly upward.
    -------------------------------------------------------- }

  if Block = _heapTail then
  begin
    Extra := NewSize - OldSize;

    if (_heapEnd <= _heapLimit) and
       (Extra <= (_heapLimit - _heapEnd)) then
    begin
      _heapEnd := _heapEnd + Extra;

      _SetBlock(Block, NewSize, False);

      _HeapAddUsed(Extra);

      Result := P;
      Exit;
    end;
  end;


  { --------------------------------------------------------
    Generic fallback: allocate, copy, free.
    -------------------------------------------------------- }

  NewPtr := _Malloc(NewSize);

  if NewPtr = nil then
    Exit;


  CopySize := OldSize;

  if CopySize > NewSize then
    CopySize := NewSize;


  if CopySize <> 0 then
    _CopyMem(P, NewPtr, CopySize);


  _Free(P);

  Result := NewPtr;
end;


{ --------------------------------------------------------------------------
  Optional libc-style internal helpers
  -------------------------------------------------------------------------- }

function _Calloc(Num, Size: SizeUInt): Pointer;
var
  Total: SizeUInt;
begin
  Total := Num * Size;

  Result := _Malloc(Total);

  if Result <> nil then
    FillChar(Result^, Total, 0);
end;


{ --------------------------------------------------------------------------
  Functions expected by FPC's MemoryManager record
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

  Size := _BlockSize(_DataBlock(P));

  _Free(P);

  Result := Size;
end;


function SysFreeMemSize(P: Pointer;
                        Size: PtrUInt): PtrUInt;
begin
  { FPC cannot partially free an allocation. }

  Result := SysFreeMem(P);
end;


function SysMemSize(P: Pointer): PtrUInt;
begin
  if P = nil then
  begin
    Result := 0;
    Exit;
  end;

  Result := _BlockSize(_DataBlock(P));
end;


function SysAllocMem(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);

  if Result <> nil then
    FillChar(
      Result^,
      SysMemSize(Result),
      0
    );
end;


function SysReAllocMem(var P: Pointer;
                       Size: PtrUInt): Pointer;
var
  NewPtr: Pointer;
begin
  NewPtr := _Realloc(P, Size);

  {
    ReAlloc semantics:

      success      -> replace P
      Size = 0     -> P becomes nil
      grow failure -> preserve original P
  }

  if (NewPtr <> nil) or (Size = 0) then
    P := NewPtr;

  Result := NewPtr;
end;


{ --------------------------------------------------------------------------
  Tiny heap-status implementations.

  These are mainly here because TMemoryManager contains pointers to them.
  -------------------------------------------------------------------------- }

function SysGetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function SysGetHeapStatus: THeapStatus;
begin
  FillChar(Result, SizeOf(Result), 0);
end;


{ --------------------------------------------------------------------------
  Kept for compatibility with PS1 RTL code that may still reference the
  low-level OS allocator interface.
  -------------------------------------------------------------------------- }

function SysOSAlloc(Size: PtrUInt): Pointer;
begin
  Result := _Malloc(Size);
end;


procedure SysOSFree(P: Pointer;
                    Size: PtrUInt);
begin
  _Free(P);
end;


{ --------------------------------------------------------------------------
  Generic heap.inc normally supplies these, but HAS_MEMORYMANAGER removes
  that implementation, so provide tiny versions here.
  -------------------------------------------------------------------------- }

procedure InitHeap; public name '_FPC_InitHeap';
begin
end;


procedure RelocateHeap;
begin
end;


procedure FinalizeHeap;
begin
end;