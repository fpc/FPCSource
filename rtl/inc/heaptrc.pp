{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by the Free Pascal development team.

    Heap tracer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit heaptrc;
interface

procedure dump_heap;
procedure mark_heap;


implementation

const
  tracesize = 4;
  quicktrace : boolean=true;

type
  pheap_mem_info = ^theap_mem_info;
  theap_mem_info = record
    next,
    previous : pheap_mem_info;
    size     : longint;
    sig      : longint;
    calls    : array [1..tracesize] of longint;
  end;

var
  heap_mem_root : pheap_mem_info;
  getmem_cnt,
  freemem_cnt   : longint;


{*****************************************************************************
                                Helpers
*****************************************************************************}

procedure call_stack(p : pointer);
var
  i  : longint;
  pp : pheap_mem_info;
begin
  pp:=pheap_mem_info(p-sizeof(theap_mem_info));
  writeln(stderr,'Call trace for block 0x',hexstr(longint(p),8),' size ',pp^.size);
  for i:=1 to tracesize do
   writeln(stderr,i,' 0x',hexstr(pp^.calls[i],8));
end;


procedure dump_free(p : pheap_mem_info);
begin
  Writeln(stderr,'Marked memory at ',HexStr(longint(p),8),' released');
  call_stack(p+sizeof(theap_mem_info));
  dump_stack(get_caller_frame(get_frame));
end;


function is_in_getmem_list (p : pointer) : boolean;
var
  i  : longint;
  pp : pheap_mem_info;
begin
  is_in_getmem_list:=false;
  pp:=heap_mem_root;
  i:=0;
  while pp<>nil do
   begin
     if (pp^.sig<>$DEADBEEF) and (pp^.sig <> $AAAAAAAA) then
      begin
        writeln(stderr,'error in linked list of heap_mem_info');
        RunError(204);
      end;
     if pp=p then
      is_in_getmem_list:=true;
     pp:=pp^.previous;
     inc(i);
     if i>getmem_cnt-freemem_cnt then
      writeln(stderr,'error in linked list of heap_mem_info');
   end;
end;


{*****************************************************************************
                               TraceGetMem
*****************************************************************************}

procedure TraceGetMem(var p:pointer;size:longint);
var
  i,bp : longint;
begin
{ Do the real GetMem, but alloc also for the info block }
  SysGetMem(p,size+sizeof(theap_mem_info));
{ Create the info block }
  pheap_mem_info(p)^.sig:=$DEADBEEF;
  pheap_mem_info(p)^.size:=size;
  bp:=get_caller_frame(get_frame);
  for i:=1 to tracesize do
   begin
     pheap_mem_info(p)^.calls[i]:=get_caller_addr(bp);
     bp:=get_caller_frame(bp);
   end;
  { insert in the linked list }
  if heap_mem_root<>nil then
   heap_mem_root^.next:=pheap_mem_info(p);
  pheap_mem_info(p)^.previous:=heap_mem_root;
  pheap_mem_info(p)^.next:=nil;
  heap_mem_root:=p;
{ update the pointer }
  inc(p,sizeof(theap_mem_info));
  inc(getmem_cnt);
end;


{*****************************************************************************
                               TraceFreeMem
*****************************************************************************}

procedure TraceFreeMem(var p:pointer;size:longint);
begin
  inc(size,sizeof(theap_mem_info));
  dec(p,sizeof(theap_mem_info));
  if not quicktrace and not(is_in_getmem_list(p)) then
    RunError(204);
  if pheap_mem_info(p)^.sig=$AAAAAAAA then
    dump_free(p);
  if pheap_mem_info(p)^.next<>nil then
    pheap_mem_info(p)^.next^.previous:=pheap_mem_info(p)^.previous;
  if pheap_mem_info(p)^.previous<>nil then
    pheap_mem_info(p)^.previous^.next:=pheap_mem_info(p)^.next;
  if pheap_mem_info(p)=heap_mem_root then
    heap_mem_root:=heap_mem_root^.previous;
  inc(freemem_cnt);
end;


{*****************************************************************************
                              Dump Heap
*****************************************************************************}

procedure dump_heap;
var
  pp : pheap_mem_info;
begin
  pp:=heap_mem_root;
  while pp<>nil do
   begin
     call_stack(pp+sizeof(theap_mem_info));
     pp:=pp^.previous;
   end;
end;


procedure mark_heap;
var
  pp : pheap_mem_info;
begin
  pp:=heap_mem_root;
  while pp<>nil do
   begin
     pp^.sig:=$AAAAAAAA;
     pp:=pp^.previous;
   end;
end;



{*****************************************************************************
                           Install MemoryManager
*****************************************************************************}

const
  TraceManager:TMemoryManager=(
    Getmem  : TraceGetMem;
    Freemem : TraceFreeMem
  );
var
  SaveExit : pointer;

procedure TraceExit;
begin
  ExitProc:=SaveExit;
  Dump_heap;
end;


begin
  SetMemoryManager(TraceManager);
  SaveExit:=ExitProc;
  ExitProc:=@TraceExit;
end.
{
  $Log$
  Revision 1.2  1998-10-02 10:35:38  peter
    + quicktrace

  Revision 1.1  1998/10/01 14:54:20  peter
    + first version

}
