{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by the Free Pascal development team

    This file implements heap management for 16-bit Windows
    using the Windows global heap.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    function SysGlobalGetMem(Size: ptruint): pointer;
      var
        hglob: HGLOBAL;
      begin
        hglob:=GlobalAlloc(HeapAllocFlags, Size);
        if hglob=0 then
          if ReturnNilIfGrowHeapFails then
            begin
              result:=nil;
              exit;
            end
          else
            HandleError(203);
        result:=GlobalLock(hglob);
        if result=nil then
          HandleError(204);
      end;

    function SysGlobalFreeMem(Addr: Pointer): ptruint;
      var
        hglob: HGLOBAL;
      begin
        if Addr<>nil then
          begin
            hglob:=HGLOBAL(GlobalHandle(Seg(Addr^)));
            if hglob=0 then
              HandleError(204);
            result:=GlobalSize(hglob);
            if GlobalUnlock(hglob) then
              HandleError(204);
            if GlobalFree(hglob)<>0 then
              HandleError(204);
          end
        else
          result:=0;
      end;

    function SysGlobalFreeMemSize(Addr: Pointer; Size: Ptruint): ptruint;
      begin
        result:=SysGlobalFreeMem(addr);
      end;

    function SysGlobalAllocMem(size: ptruint): pointer;
      var
        hglob: HGLOBAL;
      begin
        hglob:=GlobalAlloc(HeapAllocFlags or GMEM_ZEROINIT, Size);
        if hglob=0 then
          if ReturnNilIfGrowHeapFails then
            begin
              result:=nil;
              exit;
            end
          else
            HandleError(203);
        result:=GlobalLock(hglob);
        if result=nil then
          HandleError(204);
      end;

    function SysGlobalReAllocMem(var p: pointer; size: ptruint):pointer;
      var
        hglob: HGLOBAL;
      begin
        if size=0 then
          begin
            SysGlobalFreeMem(p);
            result := nil;
          end
        else if p=nil then
          result := SysGlobalAllocMem(size)
        else
          begin
            hglob:=HGLOBAL(GlobalHandle(Seg(p^)));
            if hglob=0 then
              HandleError(204);
            if GlobalUnlock(hglob) then
              HandleError(204);
            hglob:=GlobalReAlloc(hglob,size,HeapAllocFlags or GMEM_ZEROINIT);
            if hglob=0 then
              if ReturnNilIfGrowHeapFails then
                begin
                  result:=nil;
                  p:=nil;
                  exit;
                end
              else
                HandleError(203);
            result:=GlobalLock(hglob);
            if result=nil then
              HandleError(204);
          end;
        p := result;
      end;

    function SysGlobalMemSize(p: pointer): ptruint;
      var
        hglob: HGLOBAL;
      begin
        hglob:=HGLOBAL(GlobalHandle(Seg(p^)));
        if hglob=0 then
          HandleError(204);
        result:=GlobalSize(hglob);
      end;

    const
      GlobalHeapMemoryManager: TMemoryManager = (
        NeedLock: false;  // Obsolete
        GetMem: @SysGlobalGetMem;
        FreeMem: @SysGlobalFreeMem;
        FreeMemSize: @SysGlobalFreeMemSize;
        AllocMem: @SysGlobalAllocMem;
        ReAllocMem: @SysGlobalReAllocMem;
        MemSize: @SysGlobalMemSize;
        InitThread: nil;
        DoneThread: nil;
        RelocateHeap: nil;
        GetHeapStatus: nil;
        GetFPCHeapStatus: nil;
      );
