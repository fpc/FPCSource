{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team.

    Tiny heap manager for the i8086 near heap, embedded targets, etc.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ The heap, implemented here is TP7-compatible in the i8086 far data memory
  models. It's basically a linked list of free blocks, which are kept ordered by
  start address. The FreeList variable points to the start of the list. Each
  free block, except the last one, contains a TTinyHeapBlock structure, which
  holds the block size and a pointer to the next free block. The HeapPtr
  variable points to the last free block, indicating the end of the list. The
  last block is special in that it doesn't contain a TTinyHeapBlock structure.
  Instead its size is determined by the pointer difference (HeapEnd-HeapPtr).
  It *can* become zero sized, when all the memory inside of it is allocated, in
  which case, HeapPtr will become equal to HeapEnd. }

{$ifdef cpui8086}
  {$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
    {$define FPC_TINYHEAP_HUGE}
  {$endif}
{$endif cpui8086}

{$ifdef FPC_TINYHEAP_HUGE}
  {$HugePointerArithmeticNormalization On}
  {$HugePointerComparisonNormalization On}
{$endif FPC_TINYHEAP_HUGE}

    type
      { TTinyHeapMemBlockSize holds the size of an *allocated* memory block,
        and is written at position:
          memblockstart-sizeof(TTinyHeapMemBlockSize) }
      PTinyHeapMemBlockSize = ^TTinyHeapMemBlockSize; {$ifdef FPC_TINYHEAP_HUGE}huge;{$endif}
      TTinyHeapMemBlockSize = PtrUInt;

      { TTinyHeapFreeBlockSize holds the size of a *free* memory block, as a
        part of the TTinyHeapBlock structure }
{$ifdef FPC_TINYHEAP_HUGE}
      TTinyHeapFreeBlockSize = record
        OfsSize: Word;
        SegSize: Word;
      end;
{$else FPC_TINYHEAP_HUGE}
      TTinyHeapFreeBlockSize = PtrUInt;
{$endif FPC_TINYHEAP_HUGE}

      TTinyHeapPointerArithmeticType = ^Byte; {$ifdef FPC_TINYHEAP_HUGE}huge;{$endif}

      PTinyHeapBlock = ^TTinyHeapBlock;
      TTinyHeapBlock = record
        Next: PTinyHeapBlock;
        Size: TTinyHeapFreeBlockSize;
      end;

    const
      TinyHeapMinBlock = sizeof(TTinyHeapBlock);

      TinyHeapAllocGranularity = sizeof(TTinyHeapBlock);

    function EncodeTinyHeapFreeBlockSize(Size: PtrUInt): TTinyHeapFreeBlockSize; inline;
      begin
{$ifdef FPC_TINYHEAP_HUGE}
        EncodeTinyHeapFreeBlockSize.OfsSize := Size and 15;
        EncodeTinyHeapFreeBlockSize.SegSize := Size shr 4;
{$else FPC_TINYHEAP_HUGE}
        EncodeTinyHeapFreeBlockSize := Size;
{$endif FPC_TINYHEAP_HUGE}
      end;

    function DecodeTinyHeapFreeBlockSize(Size: TTinyHeapFreeBlockSize): PtrUInt; inline;
      begin
{$ifdef FPC_TINYHEAP_HUGE}
        DecodeTinyHeapFreeBlockSize := (PtrUInt(Size.SegSize) shl 4) + Size.OfsSize;
{$else FPC_TINYHEAP_HUGE}
        DecodeTinyHeapFreeBlockSize := Size;
{$endif FPC_TINYHEAP_HUGE}
      end;

    procedure InternalTinyFreeMem(Addr: Pointer; Size: PtrUInt); forward;

    function FindSize(p: pointer): TTinyHeapMemBlockSize;
      begin
        FindSize := PTinyHeapMemBlockSize(p)[-1];
      end;

    function SysTinyGetMem(Size: ptruint): pointer;
      var
        p, prev, p2: PTinyHeapBlock;
        AllocSize, RestSize: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Write('SysTinyGetMem(', Size, ')=');
{$endif DEBUG_TINY_HEAP}
        AllocSize := align(size+sizeof(TTinyHeapMemBlockSize), TinyHeapAllocGranularity);

        p := FreeList;
        prev := nil;
        while (p<>HeapPtr) and (DecodeTinyHeapFreeBlockSize(p^.Size) < AllocSize) do
          begin
            prev := p;
            p := p^.Next;
          end;

        if p<>HeapPtr then
          begin
            result := @PTinyHeapMemBlockSize(p)[1];

            if DecodeTinyHeapFreeBlockSize(p^.Size)-AllocSize >= TinyHeapMinBlock then
              RestSize := DecodeTinyHeapFreeBlockSize(p^.Size)-AllocSize
            else
              begin
                AllocSize := DecodeTinyHeapFreeBlockSize(p^.Size);
                RestSize := 0;
              end;

            if RestSize > 0 then
              begin
                p2 := pointer(TTinyHeapPointerArithmeticType(p)+AllocSize);
                p2^.Next := p^.Next;
                p2^.Size := EncodeTinyHeapFreeBlockSize(RestSize);
                if prev = nil then
                  FreeList := p2
                else
                  prev^.next := p2;
              end
            else
              begin
                if prev = nil then
                  FreeList := p^.Next
                else
                  prev^.next := p^.next;
              end;

            PTinyHeapMemBlockSize(p)^ := size;
          end
        else
          begin
            { p=HeapPtr }
            if PtrUInt(TTinyHeapPointerArithmeticType(HeapEnd)-TTinyHeapPointerArithmeticType(HeapPtr))<AllocSize then
              if ReturnNilIfGrowHeapFails then
                Result := nil
              else
                 HandleError(203);

            result := @PTinyHeapMemBlockSize(HeapPtr)[1];
            PTinyHeapMemBlockSize(HeapPtr)^ := size;

            HeapPtr := pointer(TTinyHeapPointerArithmeticType(HeapPtr)+AllocSize);
            if prev = nil then
              FreeList := HeapPtr
            else
              prev^.next := HeapPtr;
          end;
{$ifdef DEBUG_TINY_HEAP}
        Writeln(HexStr(Result));
{$endif DEBUG_TINY_HEAP}
      end;

    function TinyGetAlignedMem(Size, Alignment: ptruint): pointer;
      var
        mem: Pointer;
        memp: ptruint;
      begin
        if alignment <= sizeof(pointer) then
          result := GetMem(size)
        else
          begin
            mem := GetMem(Size+Alignment-1);
            memp := align(ptruint(mem), Alignment);
            InternalTinyFreeMem(mem, TTinyHeapPointerArithmeticType(memp)-TTinyHeapPointerArithmeticType(mem));
            result := pointer(memp);
          end;
      end;

    procedure InternalTinyFreeMem(Addr: Pointer; Size: PtrUInt);
      var
        p, prev: PTinyHeapBlock;
      begin
        p := FreeList;
        prev := nil;

        while (p<>HeapPtr) and (TTinyHeapPointerArithmeticType(p) < TTinyHeapPointerArithmeticType(Addr)) do
          begin
            prev := p;
            p := p^.Next;
          end;

        { join with previous block? }
        if assigned(prev) and ((TTinyHeapPointerArithmeticType(prev)+DecodeTinyHeapFreeBlockSize(prev^.Size)) = TTinyHeapPointerArithmeticType(Addr)) then
          begin
            Addr:=prev;
            Size:=DecodeTinyHeapFreeBlockSize(prev^.size)+Size;
          end
        else
          if assigned(prev) then
            prev^.Next := Addr
          else
            FreeList := Addr;

        { join with next block? }
        if TTinyHeapPointerArithmeticType(p)=(TTinyHeapPointerArithmeticType(Addr)+Size) then
          begin
            if p=HeapPtr then
              HeapPtr:=Addr
            else
              begin
                PTinyHeapBlock(Addr)^.Next:=p^.Next;
                PTinyHeapBlock(Addr)^.Size:=EncodeTinyHeapFreeBlockSize(Size+DecodeTinyHeapFreeBlockSize(p^.Size));
              end;
          end
        else
          begin
            PTinyHeapBlock(Addr)^.Next:=p;
            PTinyHeapBlock(Addr)^.Size:=EncodeTinyHeapFreeBlockSize(Size);
          end;
      end;

    function SysTinyFreeMem(Addr: Pointer): ptruint;
      var
        sz: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('SysTinyFreeMem(', HexStr(Addr), ')');
{$endif DEBUG_TINY_HEAP}
        if addr=nil then
          begin
            result:=0;
            exit;
          end;
        if (TTinyHeapPointerArithmeticType(addr) < TTinyHeapPointerArithmeticType(HeapOrg)) or
           (TTinyHeapPointerArithmeticType(addr) >= TTinyHeapPointerArithmeticType(HeapPtr)) then
          HandleError(204);
        sz := Align(FindSize(addr)+SizeOf(TTinyHeapMemBlockSize), TinyHeapAllocGranularity);

        InternalTinyFreeMem(@PTinyHeapMemBlockSize(addr)[-1], sz);
        
        result := sz;
      end;

    function SysTinyFreeMemSize(Addr: Pointer; Size: Ptruint): ptruint;
      begin
        result := SysTinyFreeMem(addr);
      end;

    function SysTinyMemSize(p: pointer): ptruint;
      begin
        result := findsize(p);
      end;

    function SysTinyAllocMem(size: ptruint): pointer;
      begin
        result := SysTinyGetMem(size);
        if result<>nil then
          FillChar(result^,SysTinyMemSize(result),0);
      end;

    function SysTinyReAllocMem(var p: pointer; size: ptruint):pointer;
      var
        sz: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Write('SysTinyReAllocMem(', HexStr(p), ',', size, ')=');
{$endif DEBUG_TINY_HEAP}
        if size=0 then
          result := nil
        else
          result := AllocMem(size);
        if result <> nil then
          begin
            if p <> nil then
              begin
                sz := FindSize(p);
                if sz > size then
                  sz := size;
                move(pbyte(p)^, pbyte(result)^, sz);
              end;
          end;
        SysTinyFreeMem(p);
        p := result;
{$ifdef DEBUG_TINY_HEAP}
        Writeln(HexStr(result));
{$endif DEBUG_TINY_HEAP}
      end;

    function MemAvail: PtrUInt;
      var
        p: PTinyHeapBlock;
      begin
        MemAvail := PtrUInt(TTinyHeapPointerArithmeticType(HeapEnd)-TTinyHeapPointerArithmeticType(HeapPtr));
        if MemAvail > 0 then
          Dec(MemAvail, SizeOf(TTinyHeapMemBlockSize));

        p := FreeList;
        while p <> HeapPtr do
          begin
            Inc(MemAvail, DecodeTinyHeapFreeBlockSize(p^.Size)-SizeOf(TTinyHeapMemBlockSize));
            p := p^.Next;
          end;
      end;

    function MaxAvail: PtrUInt;
      var
        p: PTinyHeapBlock;
      begin
        MaxAvail := PtrUInt(TTinyHeapPointerArithmeticType(HeapEnd)-TTinyHeapPointerArithmeticType(HeapPtr));

        p := FreeList;
        while p <> HeapPtr do
          begin
            if DecodeTinyHeapFreeBlockSize(p^.Size) > MaxAvail then
              MaxAvail := DecodeTinyHeapFreeBlockSize(p^.Size);
            p := p^.Next;
          end;

        if MaxAvail > 0 then
          Dec(MaxAvail, SizeOf(TTinyHeapMemBlockSize));
      end;

    procedure InternalTinyAlign(var AAddress: Pointer; ASize: PtrUInt);
      var
        alignment_inc: smallint;
      begin
        alignment_inc := TTinyHeapPointerArithmeticType(align(AAddress,TinyHeapAllocGranularity))-TTinyHeapPointerArithmeticType(AAddress);
        Inc(AAddress,alignment_inc);
        Dec(ASize,alignment_inc);
        Dec(ASize,ASize mod TinyHeapAllocGranularity);
      end;

    { Strongly simplified version of RegisterTinyHeapBlock, which can be used when
      the heap is only a single contiguous memory block. If you want to add
      multiple blocks to the heap, you should use RegisterTinyHeapBlock instead. }
    procedure RegisterTinyHeapBlock_Simple(AAddress: Pointer; ASize: PtrUInt);
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock_Simple(', HexStr(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        InternalTinyAlign(AAddress, ASize);
        HeapOrg:=AAddress;
        HeapPtr:=AAddress;
        FreeList:=AAddress;
        HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
      end;

    { Strongly simplified version of RegisterTinyHeapBlock, which can be used when
      the heap is only a single contiguous memory block and the address and size
      are already aligned on a TinyHeapAllocGranularity boundary. }
    procedure RegisterTinyHeapBlock_Simple_Prealigned(AAddress: Pointer; ASize: PtrUInt);
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock_Simple_Prealigned(', HexStr(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        HeapOrg:=AAddress;
        HeapPtr:=AAddress;
        FreeList:=AAddress;
        HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
      end;

    procedure RegisterTinyHeapBlock(AAddress: pointer; ASize: ptruint);
      var
        alignment_inc: smallint;
        p: PTinyHeapBlock;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock(', HexStr(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        InternalTinyAlign(AAddress, ASize);
        if HeapOrg=nil then
          begin
            HeapOrg:=AAddress;
            HeapPtr:=AAddress;
            FreeList:=AAddress;
            HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
          end
        else
          begin
            if (TTinyHeapPointerArithmeticType(HeapOrg) > TTinyHeapPointerArithmeticType(AAddress)) then
              HeapOrg:=AAddress;
            if TTinyHeapPointerArithmeticType(AAddress) > TTinyHeapPointerArithmeticType(HeapEnd) then
              begin
                if TTinyHeapPointerArithmeticType(HeapPtr) = TTinyHeapPointerArithmeticType(HeapEnd) then
                  begin
                    if FreeList=HeapPtr then
                      FreeList:=AAddress
                    else
                      begin
                        p:=FreeList;
                        while p^.Next<>HeapPtr do
                          p:=p^.Next;
                        PTinyHeapBlock(HeapPtr)^.Next:=AAddress;
                      end;
                  end
                else
                  begin
                    PTinyHeapBlock(HeapPtr)^.Size:=EncodeTinyHeapFreeBlockSize(TTinyHeapPointerArithmeticType(HeapEnd)-TTinyHeapPointerArithmeticType(HeapPtr));
                    PTinyHeapBlock(HeapPtr)^.Next:=AAddress;
                  end;
                HeapPtr:=AAddress;
                HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
              end
            else if TTinyHeapPointerArithmeticType(AAddress) = TTinyHeapPointerArithmeticType(HeapEnd) then
              HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize)
            else
              InternalTinyFreeMem(AAddress, ASize);
          end;
      end;

    const
      TinyHeapMemoryManager: TMemoryManager = (
        NeedLock: false;  // Obsolete
        GetMem: @SysTinyGetMem;
        FreeMem: @SysTinyFreeMem;
        FreeMemSize: @SysTinyFreeMemSize;
        AllocMem: @SysTinyAllocMem;
        ReAllocMem: @SysTinyReAllocMem;
        MemSize: @SysTinyMemSize;
        InitThread: nil;
        DoneThread: nil;
        RelocateHeap: nil;
        GetHeapStatus: nil;
        GetFPCHeapStatus: nil;
      );

