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
      TinyHeapMaxBlock = High(ptruint) - sizeof(TTinyHeapBlock) - sizeof(TTinyHeapMemBlockSize);

      TinyHeapAllocGranularity = sizeof(TTinyHeapBlock);

    procedure RegisterTinyHeapBlock(AAddress: Pointer; ASize:{$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif}); forward;

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

    function SysGetMem(Size: ptruint): pointer;
      var
        p, prev, p2: PTinyHeapBlock;
        AllocSize, RestSize: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Write('SysGetMem(', Size, ')=');
{$endif DEBUG_TINY_HEAP}
        if size>TinyHeapMaxBlock then
          HandleError(203);
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
              begin
                 { align to 16 bytes }
                 AllocSize:= (AllocSize + $f) and (not $f);
                 p:=SysOSAlloc(AllocSize);
                 if assigned(p) then
                   begin
                     { This needs toi be fixed because
                       HeapEnd and HeapSize are not updated correctly
                     if p > HeapPtr then
                       begin
                         prev:=HeapPtr;
                         HeapPtr:=p;
                       end
                     else }
                       begin
{$ifdef DEBUG_TINY_HEAP}
                         Writeln('SysAlloc returned: ',HexStr(p));
{$endif DEBUG_TINY_HEAP}
                         RegisterTinyHeapBlock(p,AllocSize);
                         { Recursive call }
                         SysGetMem:=SysGetMem(Size);
                         exit;
                       end;
                   end
                 else
                   begin
                     if ReturnNilIfGrowHeapFails then
                       begin
                         Result := nil;
                         exit;
                       end
                     else
                       HandleError(203);
                   end;
              end;
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

    function SysFreeMem(p: Pointer): ptruint;
      var
        sz: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('SysFreeMem(', HexStr(p), ')');
{$endif DEBUG_TINY_HEAP}
        if p=nil then
          begin
            result:=0;
            exit;
          end;
        if (TTinyHeapPointerArithmeticType(p) < TTinyHeapPointerArithmeticType(HeapOrg)) or
           (TTinyHeapPointerArithmeticType(p) >= TTinyHeapPointerArithmeticType(HeapPtr)) then
          HandleError(204);
        sz := Align(FindSize(p)+SizeOf(TTinyHeapMemBlockSize), TinyHeapAllocGranularity);

        InternalTinyFreeMem(@PTinyHeapMemBlockSize(p)[-1], sz);

        result := sz;
      end;

    function SysFreeMemSize(p: Pointer; Size: Ptruint): ptruint;
      begin
        result := SysFreeMem(p);
      end;

    function SysMemSize(p: pointer): ptruint;
      begin
        result := findsize(p);
      end;

    function SysTryResizeMem(var p: pointer; size: ptruint) : boolean;
      begin
        result := false;
      end;

    function SysAllocMem(size: ptruint): pointer;
      begin
        result := SysGetMem(size);
        if result<>nil then
          FillChar(result^,SysMemSize(result),0);
      end;

    function SysReAllocMem(var p: pointer; size: ptruint):pointer;
      var
        oldsize, OldAllocSize, NewAllocSize: ptruint;
        after_block, before_block, before_before_block: PTinyHeapBlock;
        after_block_size, before_block_size: PtrUInt;
        new_after_block: PTinyHeapBlock;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Write('SysReAllocMem(', HexStr(p), ',', size, ')=');
{$endif DEBUG_TINY_HEAP}
        if size=0 then
          begin
            SysFreeMem(p);
            result := nil;
            p := nil;
          end
        else if p=nil then
          begin
            result := AllocMem(size);
            p := result;
          end
        else
          begin
            if (TTinyHeapPointerArithmeticType(p) < TTinyHeapPointerArithmeticType(HeapOrg)) or
               (TTinyHeapPointerArithmeticType(p) >= TTinyHeapPointerArithmeticType(HeapPtr)) then
              HandleError(204);
            if size>TinyHeapMaxBlock then
              HandleError(203);
            oldsize := FindSize(p);
            OldAllocSize := align(oldsize+sizeof(TTinyHeapMemBlockSize), TinyHeapAllocGranularity);
            NewAllocSize := align(size+sizeof(TTinyHeapMemBlockSize), TinyHeapAllocGranularity);
            if OldAllocSize = NewAllocSize then
              begin
                { old and new size are the same after alignment, so the memory block is already allocated }
                { we just need to update the size }
                PTinyHeapMemBlockSize(p)[-1] := size;
                if size > oldsize then
                  FillChar((TTinyHeapPointerArithmeticType(p)+oldsize)^, size-oldsize, 0);
              end
            else if OldAllocSize > NewAllocSize then
              begin
                { we're decreasing the memory block size, so we can just free the remaining memory at the end }
                PTinyHeapMemBlockSize(p)[-1] := size;
                InternalTinyFreeMem(Pointer(TTinyHeapPointerArithmeticType(p)+(NewAllocSize-PtrUInt(SizeOf(TTinyHeapMemBlockSize)))), OldAllocSize-NewAllocSize);
              end
            else
              begin
                { we're increasing the memory block size. First, find if there are free memory blocks immediately
                  before and after our memory block. }
                after_block := FreeList;
                before_block := nil;
                before_before_block := nil;
                while (after_block<>HeapPtr) and (TTinyHeapPointerArithmeticType(after_block) < TTinyHeapPointerArithmeticType(p)) do
                  begin
                    before_before_block := before_block;
                    before_block := after_block;
                    after_block := after_block^.Next;
                  end;
                { is after_block immediately after our block? }
                if after_block=Pointer(TTinyHeapPointerArithmeticType(p)+(OldAllocSize-PtrUInt(SizeOf(TTinyHeapMemBlockSize)))) then
                  begin
                    if after_block = HeapPtr then
                      after_block_size := PtrUInt(TTinyHeapPointerArithmeticType(HeapEnd)-TTinyHeapPointerArithmeticType(HeapPtr))
                    else
                      after_block_size := DecodeTinyHeapFreeBlockSize(after_block^.size);
                  end
                else
                  after_block_size := 0;
                { is there enough room after the block? }
                if (OldAllocSize+after_block_size)>=NewAllocSize then
                  begin
                    if after_block = HeapPtr then
                      begin
                        HeapPtr:=Pointer(TTinyHeapPointerArithmeticType(HeapPtr)+(NewAllocSize-OldAllocSize));
                        if assigned(before_block) then
                          before_block^.Next := HeapPtr
                        else
                          FreeList := HeapPtr;
                      end
                    else
                      begin
                        if (NewAllocSize-OldAllocSize)=after_block_size then
                          begin
                            if assigned(before_block) then
                              before_block^.Next := after_block^.Next
                            else
                              FreeList := after_block^.Next;
                          end
                        else
                          begin
                            new_after_block := PTinyHeapBlock(TTinyHeapPointerArithmeticType(after_block)+(NewAllocSize-OldAllocSize));
                            new_after_block^.Next:=after_block^.Next;
                            new_after_block^.Size:=EncodeTinyHeapFreeBlockSize(after_block_size-(NewAllocSize-OldAllocSize));
                            if assigned(before_block) then
                              before_block^.Next := new_after_block
                            else
                              FreeList := new_after_block;
                          end;
                      end;
                    PTinyHeapMemBlockSize(p)[-1] := size;
                    FillChar((TTinyHeapPointerArithmeticType(p)+oldsize)^, size-oldsize, 0);
                  end
                else
                  begin
                    { is before_block immediately before our block? }
                    if assigned(before_block) and (Pointer(TTinyHeapPointerArithmeticType(before_block)+DecodeTinyHeapFreeBlockSize(before_block^.Size))=Pointer(TTinyHeapPointerArithmeticType(p)-SizeOf(TTinyHeapMemBlockSize))) then
                      before_block_size := DecodeTinyHeapFreeBlockSize(before_block^.Size)
                    else
                      before_block_size := 0;

                    { if there's enough space, we can slide our current block back and reclaim before_block }
                    if (before_block_size<NewAllocSize) and ((before_block_size+OldAllocSize+after_block_size)>=NewAllocSize) and
                       { todo: implement this also for after_block_size>0 }
                       (after_block_size>0) then
                      begin
                        if (before_block_size+OldAllocSize+after_block_size)=NewAllocSize then
                          begin
                            if after_block=HeapPtr then
                              begin
                                HeapPtr := HeapEnd;
                                if assigned(before_before_block) then
                                  before_before_block^.Next := HeapPtr
                                else
                                  FreeList := HeapPtr;
                              end
                            else
                              if assigned(before_before_block) then
                                before_before_block^.Next := after_block^.Next
                              else
                                FreeList := after_block^.Next;
                          end;
                        Result := Pointer(TTinyHeapPointerArithmeticType(before_block)+SizeOf(TTinyHeapMemBlockSize));
                        Move(p^, Result^, oldsize);
                        PTinyHeapMemBlockSize(before_block)^ := size;
                        if (before_block_size+OldAllocSize+after_block_size)>NewAllocSize then
                          begin
                            new_after_block := PTinyHeapBlock(TTinyHeapPointerArithmeticType(before_block)+NewAllocSize);
                            new_after_block^.Next:=after_block^.Next;
                            new_after_block^.Size:=EncodeTinyHeapFreeBlockSize(before_block_size+after_block_size-(NewAllocSize-OldAllocSize));
                            if assigned(before_before_block) then
                              before_before_block^.Next := new_after_block
                            else
                              FreeList := new_after_block;
                          end;
                        FillChar((TTinyHeapPointerArithmeticType(Result)+oldsize)^, size-oldsize, 0);
                        p := Result;
                      end
                    else
                      begin
                        result := AllocMem(size);
                        if result <> nil then
                          begin
                            if oldsize > size then
                              oldsize := size;
                            move(pbyte(p)^, pbyte(result)^, oldsize);
                          end;
                        SysFreeMem(p);
                        p := result;
                      end;
                  end;
              end;
          end;
{$ifdef DEBUG_TINY_HEAP}
        Writeln(HexStr(result));
{$endif DEBUG_TINY_HEAP}
      end;

    function MemAvail: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif};
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

    function MaxAvail: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif};
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

    procedure Mark(var p: Pointer);
      begin
        p := HeapPtr;
      end;

    procedure Release(var p: Pointer);
      begin
        HeapPtr := p;
        FreeList := p;
      end;

    procedure InternalTinyAlign(var AAddress: Pointer; var ASize: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif});
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
    procedure RegisterTinyHeapBlock_Simple(AAddress: Pointer; ASize:{$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif});
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock_Simple(', HexStr(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        InternalTinyAlign(AAddress, ASize);
        HeapSize:=HeapSize + ASize;
        HeapOrg:=AAddress;
        HeapPtr:=AAddress;
        FreeList:=AAddress;
        HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
      end;

    { Strongly simplified version of RegisterTinyHeapBlock, which can be used when
      the heap is only a single contiguous memory block and the address and size
      are already aligned on a TinyHeapAllocGranularity boundary. }
    procedure RegisterTinyHeapBlock_Simple_Prealigned(AAddress: Pointer; ASize: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif});
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock_Simple_Prealigned(', HexStr(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        HeapOrg:=AAddress;
        HeapPtr:=AAddress;
        FreeList:=AAddress;
        HeapSize:=HeapSize + ASize;
        HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
      end;

    procedure RegisterTinyHeapBlock(AAddress: pointer; ASize: {$ifdef FPC_TINYHEAP_HUGE}LongInt{$else}PtrUInt{$endif});
      var
        alignment_inc: smallint;
        p: PTinyHeapBlock;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock(', HexStr(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        InternalTinyAlign(AAddress, ASize);
        HeapSize:=HeapSize + ASize;
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
                        PTinyHeapBlock(p)^.Next:=AAddress;
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


   function SysGetFPCHeapStatus : TFPCHeapStatus;
   {
     TFPCHeapStatus = record

      MaxHeapSize,
      MaxHeapUsed,
      CurrHeapSize,
      CurrHeapUsed,
      CurrHeapFree  : ptruint;
    end;
   }
     begin
       SysGetFPCHeapStatus.MaxHeapSize:=MaxAvail;
       { How can we compute this? }
       SysGetFPCHeapStatus.MaxHeapUsed:=0;
       SysGetFPCHeapStatus.CurrHeapFree:=MemAvail;
       SysGetFPCHeapStatus.CurrHeapUsed:=HeapSize-SysGetFPCHeapStatus.CurrHeapFree;
       SysGetFPCHeapStatus.CurrHeapSize:=HeapSize;
     end;

    function SysGetHeapStatus : THeapStatus;
      begin
        SysGetHeapStatus.TotalAddrSpace:= HeapSize;
        SysGetHeapStatus.TotalUncommitted:= 0;
        SysGetHeapStatus.TotalCommitted:= 0;
        SysGetHeapStatus.TotalAllocated:= HeapSize-MemAvail;
        SysGetHeapStatus.TotalFree:= MemAvail;
        SysGetHeapStatus.FreeSmall:= 0;
        SysGetHeapStatus.FreeBig:= 0;
        SysGetHeapStatus.Unused:= 0;
        SysGetHeapStatus.Overhead:= 0;
        SysGetHeapStatus.HeapErrorCode:= 0;
      end;

   procedure FinalizeHeap;
   begin
   end;


