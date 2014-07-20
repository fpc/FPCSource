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

    type
      { TTinyHeapMemBlockSize holds the size of an *allocated* memory block,
        and is written at position:
          memblockstart-sizeof(TTinyHeapMemBlockSize) }
      PTinyHeapMemBlockSize = ^TTinyHeapMemBlockSize;
      TTinyHeapMemBlockSize = PtrUInt;

    const
      TinyHeapMinBlock = 4*sizeof(pointer);

    type
      PTinyHeapBlock = ^TTinyHeapBlock;
      TTinyHeapBlock = record
        Size: ptruint;
        Next: PTinyHeapBlock;
        EndAddr: pointer;
      end;

    var
      TinyHeapBlocks: PTinyHeapBlock = nil;

    procedure InternalTinyFreeMem(Addr: Pointer; Size: ptruint); forward;

    function FindSize(p: pointer): TTinyHeapMemBlockSize;
      begin
        FindSize := PTinyHeapMemBlockSize(p)[-1];
      end;

    function SysTinyGetMem(Size: ptruint): pointer;
      var
        p, prev: PTinyHeapBlock;
        AllocSize, RestSize: ptruint;
      begin
{$ifdef DEBUG_TINY_HEAP}
        Write('SysTinyGetMem(', Size, ')=');
{$endif DEBUG_TINY_HEAP}
        AllocSize := align(size+sizeof(TTinyHeapMemBlockSize), sizeof(pointer));

        p := TinyHeapBlocks;
        prev := nil;
        while assigned(p) and (p^.Size < AllocSize) do
          begin
            prev := p;
            p := p^.Next;
          end;

        if assigned(p) then
          begin
            result := @PTinyHeapMemBlockSize(p)[1];

            if p^.Size-AllocSize >= TinyHeapMinBlock then
              RestSize := p^.Size-AllocSize
            else
              begin
                AllocSize := p^.Size;
                RestSize := 0;
              end;

            if prev = nil then
              TinyHeapBlocks := p^.Next
            else
              prev^.next := p^.next;

            PTinyHeapMemBlockSize(p)^ := size;

            if RestSize > 0 then
              InternalTinyFreeMem(pointer(ptruint(p)+AllocSize), RestSize);
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
            InternalTinyFreeMem(mem, ptruint(memp)-ptruint(mem));
            result := pointer(memp);
          end;
      end;

    procedure InternalTinyFreeMem(Addr: Pointer; Size: ptruint);
      var 
        b, p, prev: PTinyHeapBlock;
        concatenated: boolean;
      begin
        repeat
          concatenated := false;
          b := addr;

          b^.Next := TinyHeapBlocks;
          b^.Size := Size;
          b^.EndAddr := pointer(ptruint(addr)+size);

          if TinyHeapBlocks = nil then
            TinyHeapBlocks := b
          else
            begin
              p := TinyHeapBlocks;
              prev := nil;

              while assigned(p) do
                begin
                  if p^.EndAddr = addr then
                    begin
                      addr:=p;
                      size:=p^.size+size;
                      if prev = nil then
                        TinyHeapBlocks:=p^.next
                      else
                        prev^.next:=p^.next;
                      concatenated:=true;
                      break;
                    end
                  else if p = b^.EndAddr then
                    begin
                      size:=p^.size+size;
                      if prev = nil then
                        TinyHeapBlocks:=p^.next
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
                  p := TinyHeapBlocks;
                  prev := nil;

                  while assigned(p) and (p^.Size < size) do
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
                    TinyHeapBlocks := b;
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
        sz := Align(FindSize(addr)+SizeOf(TTinyHeapMemBlockSize), sizeof(pointer));

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
      begin
{$ifdef DEBUG_TINY_HEAP}
        Writeln('RegisterTinyHeapBlock(', ptruint(AAddress), ',', ASize, ')');
{$endif DEBUG_TINY_HEAP}
        if (ptruint(AAddress) and 1) <> 0 then
          begin
            Inc(AAddress);
            Dec(ASize);
          end;
        if (ASize and 1) <> 0 then
          Dec(ASize);
        pptruint(AAddress)^ := ASize - SizeOf(ptruint);
        FreeMem(pptruint(AAddress) + 1, ASize - SizeOf(ptruint));
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

