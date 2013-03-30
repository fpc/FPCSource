{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team.

    Near heap manager for i8086, based on the FPC embedded target heap

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    const
      NearHeapMinBlock = 16;

    type
      PNearHelpBlock = ^TNearHeapBlock;
      TNearHeapBlock = record
        Size: ptruint;
        Next: PNearHelpBlock;
        EndAddr: pointer;
      end;

    var
      NearHeapBlocks: PNearHelpBlock = nil;

    procedure InternalFreeMem(Addr: Pointer; Size: ptruint); forward;

    function FindSize(p: pointer): ptruint;
      begin
        FindSize := PPtrUInt(p)[-1];
      end;

    function SysNearGetMem(Size: ptruint): pointer;
      var
        p, prev: PNearHelpBlock;
        AllocSize, RestSize: ptruint;
      begin
        AllocSize := align(size+sizeof(ptruint), sizeof(pointer));

        p := NearHeapBlocks;
        prev := nil;
        while assigned(p) and (p^.Size < AllocSize) do
          begin
            prev := p;
            p := p^.Next;
          end;

        if assigned(p) then
          begin
            result := @pptruint(p)[1];

            if p^.Size-AllocSize >= NearHeapMinBlock then
              RestSize := p^.Size-AllocSize
            else
              begin
                AllocSize := p^.Size;
                RestSize := 0;
              end;

            if prev = nil then
              NearHeapBlocks := p^.Next
            else
              prev^.next := p^.next;

            pptruint(p)^ := size;

            InternalFreemem(pointer(ptruint(p)+AllocSize), RestSize);
          end
        else
          Result := nil;
      end;

    function GetAlignedMem(Size, Alignment: ptruint): pointer;
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
            InternalFreemem(mem, ptruint(memp)-ptruint(mem));
            result := pointer(memp);
          end;
      end;

    procedure InternalFreeMem(Addr: Pointer; Size: ptruint);
      var 
        b, p, prev: PNearHelpBlock;
        concatenated: boolean;
      begin
        concatenated := true;
        while concatenated do
          begin
            concatenated := false;
            b := addr;

            b^.Next := NearHeapBlocks;
            b^.Size := Size;
            b^.EndAddr := pointer(ptruint(addr)+size);

            if NearHeapBlocks = nil then
              NearHeapBlocks := b
            else
              begin
                p := NearHeapBlocks;
                prev := nil;

                while assigned(p) do
                  begin
                    if p^.EndAddr = addr then
                      begin
                        addr:=p;
                        size:=p^.size+size;
                        if prev = nil then
                          NearHeapBlocks:=p^.next
                        else
                          prev^.next:=p^.next;
                        concatenated:=true;
                        break;
                      end
                    else if p = b^.EndAddr then
                      begin
                        size:=p^.size+size;
                        if prev = nil then
                          NearHeapBlocks:=p^.next
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
                    p := NearHeapBlocks;
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
                      NearHeapBlocks := b;
                  end;
              end;
          end;
      end;

    function SysNearFreeMem(Addr: Pointer): ptruint;
      var
        sz: ptruint;
      begin
        sz := Align(FindSize(addr)+SizeOf(ptruint), sizeof(pointer));

        InternalFreeMem(@pptruint(addr)[-1], sz);
        
        result := sz;
      end;

    function SysNearFreeMemSize(Addr: Pointer; Size: Ptruint): ptruint;
      begin
        result := SysNearFreeMem(addr);
      end;

    function SysNearMemSize(p: pointer): ptruint;
      begin
        result := findsize(p);
      end;

    function SysNearAllocMem(size: ptruint): pointer;
      begin
        result := SysNearGetMem(size);
        if result<>nil then
          FillChar(result^,SysNearMemSize(result),0);
      end;

    function SysNearReAllocMem(var p: pointer; size: ptruint):pointer;
      var
        sz: ptruint;
      begin
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
        SysNearFreeMem(p);
        p := result;
      end;

    procedure RegisterNearHeapBlock(AAddress: pointer; ASize: ptruint);
      begin
        if (ptruint(AAddress) and 1) = 0 then
          begin
            Inc(AAddress);
            Dec(ASize);
          end;
        pptruint(AAddress)^ := ASize - SizeOf(ptruint);
        FreeMem(pptruint(AAddress) + 1, ASize - SizeOf(ptruint));
      end;

    const
      NearHeapMemoryManager: TMemoryManager = (
        NeedLock: false;  // Obsolete
        GetMem: @SysNearGetMem;
        FreeMem: @SysNearFreeMem;
        FreeMemSize: @SysNearFreeMemSize;
        AllocMem: @SysNearAllocMem;
        ReAllocMem: @SysNearReAllocMem;
        MemSize: @SysNearMemSize;
        InitThread: nil;
        DoneThread: nil;
        RelocateHeap: nil;
        GetHeapStatus: nil;
        GetFPCHeapStatus: nil;
      );

