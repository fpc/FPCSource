{%MainUnit system.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Michael Van Canneyt,
    member of the Free Pascal development team.

    WASM minimal memory manager

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  WASM minimal TLS memory manager

  We can't use system unit memory manager, it uses threadvars.

  Wasm allocates new mem in pages of MemPageSize, but never frees blocks.
  So we must take care of freeing ourselves.

  We allocate 2 kind of blocks:

  - a pointer block TOSMemInfoBlock of MemBlockCount TOsMemBlock structure.
    linked. Each TOSMemBlock record has a pointer to TLS memory and a
    boolean to say whether it is used.

  - a TLS memory block, divided in blocks of (TLS size + SizeOf(Pointer))
    For each TLS block The first SizeOf(Pointer) bytes points back to the
    TOsMemBlock pointing to the TLS Block.
    This structure is represented by the TTLSMem structure

}

Type
  POSMemBlock = ^TOSMemBlock;
  PTLSMem = ^TTLSMem;
  TTLSMem = Record
    OSMemBlock : POSMemBlock;
    // Actually TTLSSize bytes, but we don't know in advance how much it is.
    TLSMemory : Array[0..0] of Byte;
  end;
  TOSMemBlock = record
    Data : PTLSMem;
    Used : Boolean;
  end;

Const
  MemPageSize = 65536;
  // Theoretical TOSMemBlock record count that fits in a page. (around 4000)
  MaxPageMemBlockCount = (MemPageSize - (2 * SizeOf(Pointer))) div SizeOf(TOSMemBlock);
  // Actual used record count. Should be less than MaxPageMemBlockCount.
  MemBlockCount = 1000;

Type
  TOSMemBlockArray = Array[0..MemBlockCount-1] of TOSMemBlock;
  POSMemInfoBlock = ^TOSMemInfoBlock;
  TOSMemInfoBlock = record
    Blocks : TOSMemBlockArray;
    Next : POSMemInfoBlock;
  end;


Var
  // Root block of linked list of TOSMemInfoBlock
  TLSInfoBlock : POSMemInfoBlock = nil;

Function TLSMemblockSize : PTrUint;

// Calculate the size of a TLS memory block.
// This is the TLS size + Size of a pointer (cannot use TTLSMem for this)

Var
  BlockSize : PTrUint;

begin
  BlockSize:=Align(fpc_wasm32_tls_size+SizeOf(Pointer),fpc_wasm32_tls_align);
  TLSMemblockSize:=BlockSize*MemBlockCount;
end;

Function AllocateOSInfoBlock : POSMemInfoBlock;

Var
  PMIB : POSMemInfoBlock;
  POMB : POSMemBlock;
  POSBlock,POSMem : PTLSMem;
  I : Integer;

begin
  // allocate block
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('AllocateOSInfoBlock');{$ENDIF}
  PMIB:=POSMemInfoBlock(SysOSAlloc(MemPageSize));
  if PMIB=Nil then
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('AllocateOSInfoBlock nil');{$ENDIF}
    Halt(203);
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('AllocateOSInfoBlock nil but halt returned');{$ENDIF}
    end;
  FillChar(PMIB^,SizeOf(TOSMemInfoBlock),#0);
  // Allocate corresponding TLS mem blocks
  POSBlock:=PTLSMem(SysOSAlloc(TLSMemblockSize));
  if POSBlock=Nil then
    Halt(203);
  POSMem:=POSBlock;
  For I:=0 to MemBlockCount-1 do
    begin
    PMIB^.Blocks[I].Data:=POSMem;
    POMB:=@(PMIB^.Blocks[I]);
    PosMem^.OSMemBlock:=POMB;
    Inc(Pointer(POSMem),BlockSize);
    end;
  AllocateOSInfoBlock:=PMIB;
end;

Function FindFreeOSBlock(aInfo: POSMemInfoBlock) : POSMemBlock;

Var
  I : integer;
  Res : POSMemBlock;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FindFreeOSBlock entry ('+IntToStr(PtrUint(aInfo))+')');{$ENDIF}
  Res:=Nil;
  I:=0;
  While (Res=Nil) and (I<MemBlockCount-1) do
    begin
    if Not aInfo^.Blocks[I].Used then
      begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FindFreeOSBlock: block '+IntToStr(i)+' is not used');{$ENDIF}
      aInfo^.Blocks[I].Used:=True;
      Res:=@(aInfo^.Blocks[I]);
      end;
    Inc(I);
    end;
  FindFreeOSBlock:=Res;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FindFreeOSBlock exit ('+IntToStr(PtrUint(aInfo))+')');{$ENDIF}
end;

Procedure LockOSMem;

begin
  // Todo
end;

Procedure UnLockOSMem;

begin
  // Todo
end;

Function GetFreeOSBlock : POSMemBlock;

Var
 aInfo : POSMemInfoBlock;
 Res : POSMemBlock;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetFreeOSBlock entry');{$ENDIF}
  LockOSMem;
  try
    Res:=nil;
    if TLSInfoBlock=Nil then
      begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetFreeOSBlock: Allocate OSInfoBlock');{$ENDIF}
      TLSInfoBlock:=AllocateOSInfoBlock;
      end
    else
      begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetFreeOSBlock: have OSInfoBlock ('+IntToStr(PtrUint(TLSInfoBlock)));{$ENDIF}
      end;
    aInfo:=TLSInfoBlock;
    While (Res=Nil) do
      begin
      Res:=FindFreeOSBlock(aInfo);
      if Res=Nil then
        begin
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetFreeOSBlock: did not find free block, allocating another OSInfoBlock');{$ENDIF}
        if aInfo^.Next=Nil then
          aInfo^.Next:=AllocateOSInfoBlock;
        aInfo:=aInfo^.next;
        end;
      end;
    GetFreeOSBlock:=Res;
  finally
    UnlockOSMem
  end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetFreeOSBlock exit, result='+IntToStr(PtrUint(Res)));{$ENDIF}
end;

Procedure FreeOSInfoBlock(aBlock : POSMemInfoBlock);

Var
  Next : POSMemInfoBlock;

begin
  While aBlock<>Nil do
    begin
    Next:=aBlock^.Next;
    SysOsFree(aBlock^.Blocks[0].Data,TLSMemblockSize);
    SysOsFree(aBlock,MemPageSize);
    aBlock:=Next;
    end;
end;


Procedure ReleaseOSBlock (aBlock : POSMemBlock);

begin
  aBlock^.Used:=False;
end;


