{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by the Free Pascal development team

    This file implements heap management for 16-bit Windows
    using the Windows local heap.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    function SysLocalGetMem(Size: ptruint): pointer;
      begin
        result:=NearPointer(LocalAlloc(LMEM_FIXED, Size));
        if not ReturnNilIfGrowHeapFails and (result=nil) then
          HandleError(203);
      end;

    function SysLocalFreeMem(Addr: Pointer): ptruint;
      begin
        if Addr<>nil then
          begin
            result:=LocalSize(THandle(Addr));
            if LocalFree(THandle(Addr))<>0 then
              HandleError(204);
          end
        else
          result:=0;
      end;

    function SysLocalFreeMemSize(Addr: Pointer; Size: Ptruint): ptruint;
      begin
        result:=SysLocalFreeMem(addr);
      end;

    function SysLocalAllocMem(size: ptruint): pointer;
      begin
        result:=NearPointer(LocalAlloc(LMEM_FIXED or LMEM_ZEROINIT, Size));
        if not ReturnNilIfGrowHeapFails and (result=nil) then
          HandleError(203);
      end;

    function SysLocalReAllocMem(var p: pointer; size: ptruint):pointer;
      begin
        if size=0 then
          begin
            SysLocalFreeMem(p);
            result := nil;
          end
        else if p=nil then
          result := SysLocalAllocMem(size)
        else
          begin
            result := NearPointer(LocalReAlloc(THandle(p), size, LMEM_MOVEABLE or LMEM_ZEROINIT));
            if not ReturnNilIfGrowHeapFails and (result=nil) then
              HandleError(203);
          end;
        p := result;
      end;

    function SysLocalMemSize(p: pointer): ptruint;
      begin
        result:=LocalSize(THandle(p));
      end;

    const
      LocalHeapMemoryManager: TMemoryManager = (
        NeedLock: false;  // Obsolete
        GetMem: @SysLocalGetMem;
        FreeMem: @SysLocalFreeMem;
        FreeMemSize: @SysLocalFreeMemSize;
        AllocMem: @SysLocalAllocMem;
        ReAllocMem: @SysLocalReAllocMem;
        MemSize: @SysLocalMemSize;
        InitThread: nil;
        DoneThread: nil;
        RelocateHeap: nil;
        GetHeapStatus: nil;
        GetFPCHeapStatus: nil;
      );
