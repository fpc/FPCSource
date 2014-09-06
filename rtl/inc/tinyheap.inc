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

{$ifdef cpui8086}
  {$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
    {$define FPC_HEAP_HUGE}
  {$endif}
{$endif cpui8086}

{$ifdef FPC_HEAP_HUGE}
  {$HugePointerArithmeticNormalization On}
  {$HugePointerComparisonNormalization On}
{$endif FPC_HEAP_HUGE}

    type
      { TTinyHeapMemBlockSize holds the size of an *allocated* memory block,
        and is written at position:
          memblockstart-sizeof(TTinyHeapMemBlockSize) }
      PTinyHeapMemBlockSize = ^TTinyHeapMemBlockSize; {$ifdef FPC_HEAP_HUGE}huge;{$endif}
      TTinyHeapMemBlockSize = PtrUInt;

      { TTinyHeapFreeBlockSize holds the size of a *free* memory block, as a
        part of the TTinyHeapBlock structure }
{$ifdef FPC_HEAP_HUGE}
      TTinyHeapFreeBlockSize = record
        OfsSize: Word;
        SegSize: Word;
      end;
{$else FPC_HEAP_HUGE}
      TTinyHeapFreeBlockSize = PtrUInt;
{$endif FPC_HEAP_HUGE}

      TTinyHeapPointerArithmeticType = ^Byte; {$ifdef FPC_HEAP_HUGE}huge;{$endif}

      PTinyHeapBlock = ^TTinyHeapBlock;
      TTinyHeapBlock = record
        Next: PTinyHeapBlock;
        Size: TTinyHeapFreeBlockSize;
      end;

    const
      TinyHeapMinBlock = sizeof(TTinyHeapBlock);

      TinyHeapAllocGranularity = sizeof(TTinyHeapBlock);

    var
      FreeList: Pointer = nil;

    function EncodeTinyHeapFreeBlockSize(Size: PtrUInt): TTinyHeapFreeBlockSize; inline;
      begin
{$ifdef FPC_HEAP_HUGE}
        EncodeTinyHeapFreeBlockSize.OfsSize := Size and 15;
        EncodeTinyHeapFreeBlockSize.SegSize := Size shr 4;
{$else FPC_HEAP_HUGE}
        EncodeTinyHeapFreeBlockSize := Size;
{$endif FPC_HEAP_HUGE}
      end;

    function DecodeTinyHeapFreeBlockSize(Size: TTinyHeapFreeBlockSize): PtrUInt; inline;
      begin
{$ifdef FPC_HEAP_HUGE}
        DecodeTinyHeapFreeBlockSize := (PtrUInt(Size.SegSize) shl 4) + Size.OfsSize;
{$else FPC_HEAP_HUGE}
        DecodeTinyHeapFreeBlockSize := Size;
{$endif FPC_HEAP_HUGE}
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
        while assigned(p) and (DecodeTinyHeapFreeBlockSize(p^.Size) < AllocSize) do
          begin
            prev := p;
            p := p^.Next;
          end;

        if assigned(p) then
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
          if ReturnNilIfGrowHeapFails then
            Result := nil
          else
            HandleError(203);
{$ifdef DEBUG_TINY_HEAP}
        Writeln(ptruint(Result));
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
        b, p, prev: PTinyHeapBlock;
        EndAddr: Pointer;
        concatenated: boolean;
      begin
        repeat
          concatenated := false;
          b := addr;

          b^.Next := FreeList;
          b^.Size := EncodeTinyHeapFreeBlockSize(Size);
          EndAddr := pointer(TTinyHeapPointerArithmeticType(addr)+size);

          if FreeList = nil then
            FreeList := b
          else
            begin
              p := FreeList;
              prev := nil;

              while assigned(p) do
                begin
                  if (TTinyHeapPointerArithmeticType(p)+DecodeTinyHeapFreeBlockSize(p^.Size)) = TTinyHeapPointerArithmeticType(Addr) then
                    begin
                      addr:=p;
                      size:=DecodeTinyHeapFreeBlockSize(p^.size)+size;
                      if prev = nil then
                        FreeList:=p^.next
                      else
                        prev^.next:=p^.next;
                      concatenated:=true;
                      break;
                    end
                  else if p = EndAddr then
                    begin
                      size:=DecodeTinyHeapFreeBlockSize(p^.size)+size;
                      if prev = nil then
                        FreeList:=p^.next
                      else
                        prev^.next:=p^.next;
                      concatenated:=true;
                      break;
                    end;

                  prev := p;
                  p := p^.next;
                end;

              if not concatenated then
                begin
                  p := FreeList;
                  prev := nil;

                  while assigned(p) and (TTinyHeapPointerArithmeticType(p) < TTinyHeapPointerArithmeticType(b)) do
                    begin
                      prev := p;
                      p := p^.Next;
                    end;

                  if assigned(prev) then
                    begin
                      b^.Next := p;
                      prev^.Next := b;
                    end
                  else
                    FreeList := b;
                end;
            end;
        until not concatenated;
      end;

    function SysTinyFreeMem(Addr: Pointer): ptruint;
      var
        sz: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('SysTinyFreeMem(', ptruint(Addr), ')');
{$endif DEBUG_TINY_HEAP}
        if addr=nil then
          begin
            result:=0;
            exit;
          end;
        if (TTinyHeapPointerArithmeticType(addr) < TTinyHeapPointerArithmeticType(HeapOrg)) or
           (TTinyHeapPointerArithmeticType(addr) >= TTinyHeapPointerArithmeticType(HeapEnd)) then
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
        Write('SysTinyReAllocMem(', ptruint(p), ',', size, ')=');
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
        Writeln(ptruint(result));
{$endif DEBUG_TINY_HEAP}
      end;

    procedure RegisterTinyHeapBlock(AAddress: pointer; ASize: ptruint);
      var
        alignment_inc: smallint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock(', ptruint(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        alignment_inc := TTinyHeapPointerArithmeticType(align(AAddress,TinyHeapAllocGranularity))-TTinyHeapPointerArithmeticType(AAddress);
        Inc(AAddress,alignment_inc);
        Dec(ASize,alignment_inc);
        Dec(ASize,ASize mod TinyHeapAllocGranularity);
        if (HeapOrg=nil) or (TTinyHeapPointerArithmeticType(HeapOrg) > TTinyHeapPointerArithmeticType(AAddress)) then
          HeapOrg:=AAddress;
        if (HeapEnd=nil) or (TTinyHeapPointerArithmeticType(HeapEnd) < (TTinyHeapPointerArithmeticType(AAddress)+ASize)) then
          HeapEnd:=Pointer(TTinyHeapPointerArithmeticType(AAddress)+ASize);
        InternalTinyFreeMem(AAddress, ASize);
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

