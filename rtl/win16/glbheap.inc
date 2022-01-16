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

{
The heap, implemented here is BP7-compatible for the Win16 and 286 protected
mode targets.
Large blocks (>=HeapLimit) are allocated as separate blocks on the global heap
via a separate call to GlobalAlloc(). Since this allocates a new segment
descriptor and there's a limit of how many of these are available to the system,
small blocks (<HeapLimit) are suballocated from blocks of size HeapBlock. Each
such heap block starts with a header of type TGlobalHeapBlockHeader, which is
always located at offset 0 of the heap block segment. These heap blocks form a
circular linked list.
}
    const
      GlobalHeapBlockID=20564;
    type
      PGlobalHeapBlockHeader=^TGlobalHeapBlockHeader;far;
      TGlobalHeapBlockHeader=record
        ID: LongWord;  { =GlobalHeapBlockID }
        FirstFreeOfs: Word;
        Unknown: Word;  { don't know what this is; seems to be 0 }
        TotalFreeSpaceInBlock: Word;
        NextBlockSeg: Word;  { the link to the next heap block }
      end;
      PFreeSubBlock=^TFreeSubBlock;far;
      TFreeSubBlock=record
        Next: Word;
        Size: Word;
      end;

    function NewHeapBlock(LastBlock: Word): Boolean;
      var
        hglob: HGLOBAL;
        pb: PGlobalHeapBlockHeader;
      begin
        hglob:=GlobalAlloc(HeapAllocFlags, HeapBlock);
        if hglob=0 then
          if ReturnNilIfGrowHeapFails then
            begin
              result:=false;
              exit;
            end
          else
            HandleError(203);
        pb:=GlobalLock(hglob);
        if (pb=nil) or (Ofs(pb^)<>0) then
          HandleError(204);
        with pb^ do
          begin
            ID:=GlobalHeapBlockID;
            FirstFreeOfs:=SizeOf(TGlobalHeapBlockHeader);
            Unknown:=0;
            TotalFreeSpaceInBlock:=HeapBlock-SizeOf(TGlobalHeapBlockHeader);
            if HeapList<>0 then
              NextBlockSeg:=HeapList
            else
              NextBlockSeg:=Seg(pb^);
            with PFreeSubBlock(Ptr(Seg(pb^),SizeOf(TGlobalHeapBlockHeader)))^ do
              begin
                Next:=0;
                Size:=HeapBlock-SizeOf(TGlobalHeapBlockHeader);
              end;
          end;
        HeapList:=Seg(pb^);
        if LastBlock<>0 then
          PGlobalHeapBlockHeader(Ptr(LastBlock,0))^.NextBlockSeg:=HeapList;
        result:=true;
      end;

    { tries to suballocate from the existing blocks. Returns nil if not enough
      free space is available. ASize must be aligned by 4. }
    function TryBlockGetMem(ASize: Word; out LastBlock: Word): FarPointer;
      var
        CurBlock: Word;
        CurBlockP: PGlobalHeapBlockHeader;
        CurSubBlock, PrevSubBlock: PFreeSubBlock;
      begin
        CurBlock:=HeapList;
        result:=nil;
        LastBlock:=0;
        if CurBlock=0 then
          exit;
        repeat
          CurBlockP:=Ptr(CurBlock,0);
          if CurBlockP^.TotalFreeSpaceInBlock>=ASize then
            begin
              PrevSubBlock:=nil;
              CurSubBlock:=Ptr(CurBlock,CurBlockP^.FirstFreeOfs);
              while Ofs(CurSubBlock^)<>0 do
                begin
                  if CurSubBlock^.Size>=ASize then
                    begin
                      result:=CurSubBlock;
                      if CurSubBlock^.Size=ASize then
                        begin
                          if PrevSubBlock<>nil then
                            PrevSubBlock^.Next:=CurSubBlock^.Next
                          else
                            CurBlockP^.FirstFreeOfs:=CurSubBlock^.Next;
                        end
                      else
                        begin
                          with PFreeSubBlock(Ptr(CurBlock,Ofs(CurSubBlock^)+ASize))^ do
                            begin
                              Next:=CurSubBlock^.Next;
                              Size:=CurSubBlock^.Size-ASize;
                            end;
                          if PrevSubBlock<>nil then
                            PrevSubBlock^.Next:=Ofs(CurSubBlock^)+ASize
                          else
                            CurBlockP^.FirstFreeOfs:=Ofs(CurSubBlock^)+ASize;
                        end;
                      Dec(CurBlockP^.TotalFreeSpaceInBlock,ASize);
                      { if TotalFreeSpaceInBlock becomes 0, then FirstFreeOfs
                      should also become 0, but that is already handled
                      correctly in the code above (in this case, by the
                      line 'CurBlockP^.FirstFreeOfs:=CurSubBlock^.Next',
                      so there's no need to set it explicitly here. }
                      exit;
                    end;
                  PrevSubBlock:=CurSubBlock;
                  CurSubBlock:=Ptr(CurBlock,CurSubBlock^.Next);
                end;
            end;
          LastBlock:=CurBlock;
          CurBlock:=CurBlockP^.NextBlockSeg;
        until CurBlock=HeapList;
      end;

    function SysGlobalBlockGetMem(Size: Word): FarPointer;
      var
        LastBlock: Word;
      begin
        Size:=(Size+3) and $fffc;
        result:=TryBlockGetMem(Size,LastBlock);
        if result<>nil then
          exit;
        if not NewHeapBlock(LastBlock) then
          begin
            { NewHeapBlock can only return false if ReturnNilIfGrowHeapFails=true }
            result:=nil;
            exit;
          end;
        result:=TryBlockGetMem(Size,LastBlock);
      end;

    function SysGlobalGetMem(Size: ptruint): FarPointer;
      type
        PFarWord=^Word;far;
      var
        hglob: HGLOBAL;
      begin
        if (size+2)>=HeapLimit then
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
          end
        else
          begin
            result:=SysGlobalBlockGetMem(Size+2);
            PFarWord(result)^:=Size;
            Inc(result,2);
          end;
      end;

    procedure TryBlockFreeMem(Addr: FarPointer; ASize: Word);
      var
        CurBlock: Word;
        CurBlockP: PGlobalHeapBlockHeader;
        CurSubBlock, PrevSubBlock: PFreeSubBlock;
      begin
        ASize:=(ASize+3) and $fffc;
        CurBlock:=Seg(Addr^);
        CurBlockP:=Ptr(CurBlock,0);
        if (Ofs(Addr^)<SizeOf(TGlobalHeapBlockHeader)) or ((Ofs(Addr^) and 3)<>0) or
           (CurBlockP^.ID<>GlobalHeapBlockID) then
          HandleError(204);
        if CurBlockP^.TotalFreeSpaceInBlock=0 then
          begin
            CurBlockP^.FirstFreeOfs:=Ofs(Addr^);
            with PFreeSubBlock(Addr)^ do
              begin
                Next:=0;
                Size:=ASize;
              end;
          end
        else if Ofs(Addr^)<CurBlockP^.FirstFreeOfs then
          begin
            if (Ofs(Addr^)+ASize)>CurBlockP^.FirstFreeOfs then
              HandleError(204)
            else if (Ofs(Addr^)+ASize)=CurBlockP^.FirstFreeOfs then
              begin
                PFreeSubBlock(Addr)^.Next:=PFreeSubBlock(Ptr(CurBlock,CurBlockP^.FirstFreeOfs))^.Next;
                PFreeSubBlock(Addr)^.Size:=ASize+PFreeSubBlock(Ptr(CurBlock,CurBlockP^.FirstFreeOfs))^.Size;
              end
            else
              begin
                PFreeSubBlock(Addr)^.Next:=CurBlockP^.FirstFreeOfs;
                PFreeSubBlock(Addr)^.Size:=ASize;
              end;
            CurBlockP^.FirstFreeOfs:=Ofs(Addr^);
          end
        else
          begin
            PrevSubBlock:=nil;
            CurSubBlock:=Ptr(CurBlock,CurBlockP^.FirstFreeOfs);
            while (Ofs(CurSubBlock^)<>0) and (Ofs(CurSubBlock^)<Ofs(Addr^)) do
              begin
                PrevSubBlock:=CurSubBlock;
                CurSubBlock:=Ptr(CurBlock,CurSubBlock^.Next);
              end;
            if PrevSubBlock=nil then
              HandleError(204);
            { merge with previous free block? }
            if Ofs(PrevSubBlock^)+PrevSubBlock^.Size=Ofs(Addr^) then
              begin
                Inc(PrevSubBlock^.Size,ASize);
                { merge with next as well? }
                if (Ofs(CurSubBlock^)<>0) and ((Ofs(PrevSubBlock^)+PrevSubBlock^.Size)=Ofs(CurSubBlock^)) then
                  begin
                    Inc(PrevSubBlock^.Size,CurSubBlock^.Size);
                    PrevSubBlock^.Next:=CurSubBlock^.Next;
                  end;
              end
            else
              begin
                PrevSubBlock^.Next:=Ofs(Addr^);
                if (Ofs(CurSubBlock^)<>0) and ((Ofs(Addr^)+ASize)=Ofs(CurSubBlock^)) then
                  with PFreeSubBlock(Addr)^ do
                    begin
                      Next:=CurSubBlock^.Next;
                      Size:=ASize+CurSubBlock^.Size;
                    end
                else
                  with PFreeSubBlock(Addr)^ do
                    begin
                      Next:=Ofs(CurSubBlock^);
                      Size:=ASize;
                    end;
              end;
          end;
        Inc(CurBlockP^.TotalFreeSpaceInBlock,ASize);
      end;

    function SysGlobalFreeMem(Addr: FarPointer): ptruint;
      type
        PFarWord=^Word;far;
      var
        hglob: HGLOBAL;
      begin
        if Addr<>nil then
          begin
            if Ofs(Addr^)=0 then
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
              begin
                Dec(Addr, 2);
                result:=PFarWord(Addr)^;
                TryBlockFreeMem(Addr, result+2);
              end;
          end
        else
          result:=0;
      end;

    function SysGlobalFreeMemSize(Addr: FarPointer; Size: Ptruint): ptruint;
      begin
        result:=SysGlobalFreeMem(addr);
      end;

    function SysGlobalAllocMem(size: ptruint): FarPointer;
      var
        hglob: HGLOBAL;
      begin
        if (size+2)>=HeapLimit then
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
          end
        else
          begin
            result:=SysGlobalGetMem(size);
            FillChar(result^,size,0);
          end;
      end;

    function SysGlobalMemSize(p: FarPointer): ptruint;
      type
        PFarWord=^Word;far;
      var
        hglob: HGLOBAL;
      begin
        if Ofs(p^)=0 then
          begin
            hglob:=HGLOBAL(GlobalHandle(Seg(p^)));
            if hglob=0 then
              HandleError(204);
            result:=GlobalSize(hglob);
          end
        else
          begin
            Dec(p,2);
            result:=PFarWord(p)^;
          end;
      end;

    function SysGlobalReAllocMem(var p: FarPointer; size: ptruint):FarPointer;
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
          if Ofs(p^)=0 then
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
            end
          else
            begin
              { todo: do it in a more optimal way? }
              result:=SysGlobalAllocMem(size);
              Move(p^,result^,SysGlobalMemSize(p));
              SysGlobalFreeMem(p);
            end;
        p := result;
      end;

    function MemAvail: LongInt;
      var
        CurBlock: Word;
        CurBlockP: PGlobalHeapBlockHeader;
        CurSubBlock: PFreeSubBlock;
      begin
        result:=GetFreeSpace(0);
        CurBlock:=HeapList;
        if CurBlock=0 then
          exit;
        repeat
          CurBlockP:=Ptr(CurBlock,0);
          CurSubBlock:=Ptr(CurBlock,CurBlockP^.FirstFreeOfs);
          while Ofs(CurSubBlock^)<>0 do
            begin
              if CurSubBlock^.Size>2 then
                Inc(result,CurSubBlock^.Size-2);
              CurSubBlock:=Ptr(CurBlock,CurSubBlock^.Next);
            end;
          CurBlock:=CurBlockP^.NextBlockSeg;
        until CurBlock=HeapList;
      end;

    function MaxAvail: LongInt;
      var
        CurBlock: Word;
        CurBlockP: PGlobalHeapBlockHeader;
        CurSubBlock: PFreeSubBlock;
      begin
        result:=GlobalCompact(0);
        if result>(65536-SizeOf(TGlobalHeapBlockHeader)-2) then
          exit;
        CurBlock:=HeapList;
        if CurBlock=0 then
          exit;
        repeat
          CurBlockP:=Ptr(CurBlock,0);
          if CurBlockP^.TotalFreeSpaceInBlock>(result+2) then
            begin
              CurSubBlock:=Ptr(CurBlock,CurBlockP^.FirstFreeOfs);
              while Ofs(CurSubBlock^)<>0 do
                begin
                  if CurSubBlock^.Size>(result+2) then
                    result:=CurSubBlock^.Size-2;
                  CurSubBlock:=Ptr(CurBlock,CurSubBlock^.Next);
                end;
            end;
          CurBlock:=CurBlockP^.NextBlockSeg;
        until CurBlock=HeapList;
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
