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

const
  tracesize = 8;
  quicktrace : boolean=true;
  keepreleased : boolean=true;


implementation

type
  pheap_mem_info = ^theap_mem_info;
  { warning the size of theap_mem_info
    must be a multiple of 8
    because otherwise you will get
    problems when releasing the usual memory part !!
    sizeof(theap_mem_info = 16+tracesize*4 so
    tracesize must be even !! PM }
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
  getmem_size,
  freemem_size   : longint;


{*****************************************************************************
                                Helpers
*****************************************************************************}

procedure call_stack(pp : pheap_mem_info);
var
  i  : longint;
begin
  writeln(stderr,'Call trace for block 0x',hexstr(longint(pp+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(stderr,'  0x',hexstr(pp^.calls[i],8));
end;

procedure call_free_stack(pp : pheap_mem_info);
var
  i  : longint;

begin
  writeln(stderr,'Call trace for block 0x',hexstr(longint(pp+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize div 2 do
   if pp^.calls[i]<>0 then
     writeln(stderr,'  0x',hexstr(pp^.calls[i],8));
  writeln(stderr,' was released at ');
  for i:=(tracesize div 2)+1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(stderr,'  0x',hexstr(pp^.calls[i],8));
end;


procedure dump_already_free(p : pheap_mem_info);
begin
  Writeln(stderr,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' released');
  call_free_stack(p);
  Writeln(stderr,'freed again at');
  dump_stack(get_caller_frame(get_frame));
end;

procedure dump_error(p : pheap_mem_info);
begin
  Writeln(stderr,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' invalid');
  Writeln(stderr,'Wrong signature $',hexstr(p^.sig,8));
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
  inc(getmem_size,size);
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

  var i,bp : longint;
  pp : pheap_mem_info;
begin
  inc(freemem_size,size);
  inc(size,sizeof(theap_mem_info));
  dec(p,sizeof(theap_mem_info));
  pp:=pheap_mem_info(p);
  if not quicktrace and not(is_in_getmem_list(p)) then
    RunError(204);
  if pp^.sig=$AAAAAAAA then
    dump_already_free(pp)
  else if pp^.sig<>$DEADBEEF then
    begin
       dump_error(pp);
       { don't release anything in this case !! }
       exit;
    end;
  { now it is released !! }
  pp^.sig:=$AAAAAAAA;
  if not keepreleased then
    begin
       if pp^.next<>nil then
         pp^.next^.previous:=pp^.previous;
       if pp^.previous<>nil then
         pp^.previous^.next:=pp^.next;
       if pp=heap_mem_root then
         heap_mem_root:=heap_mem_root^.previous;
    end;
  bp:=get_caller_frame(get_frame);
  for i:=(tracesize div 2)+1 to tracesize do
   begin
     pp^.calls[i]:=get_caller_addr(bp);
     bp:=get_caller_frame(bp);
   end;
  inc(freemem_cnt);
  { release the normal memory at least !! }
  { this way we keep all info about all released memory !! }
  dec(size,sizeof(theap_mem_info));
  inc(p,sizeof(theap_mem_info));
  SysFreeMem(p,size);
end;


{*****************************************************************************
                              Dump Heap
*****************************************************************************}

procedure dump_heap;
var
  pp : pheap_mem_info;
  i : longint;
begin
  pp:=heap_mem_root;
  Writeln(stderr,'Heap dump by heaptrc unit');
  Writeln(stderr,getmem_cnt,' memory blocks allocated : ',getmem_size);
  Writeln(stderr,freemem_cnt,' memory blocks allocated : ',freemem_size);
  Writeln(stderr,'Unfreed memory size : ',getmem_size-freemem_size);
  i:=getmem_cnt-freemem_cnt;
  while pp<>nil do
   begin
     if i<0 then
       begin
          Writeln(stderr,'Error in heap memory list');
          Writeln(stderr,'More memory blocks than expected');
          exit;
       end;
     if pp^.sig=$DEADBEEF then
       begin
          { this one was not released !! }
          call_stack(pp);
          dec(i);
       end
     else if pp^.sig<>$AAAAAAAA then
       dump_error(pp);
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
  Revision 1.3  1998-10-06 17:09:13  pierre
   + added trace of first dispose for errors

  Revision 1.2  1998/10/02 10:35:38  peter
    + quicktrace

  Revision 1.1  1998/10/01 14:54:20  peter
    + first version

}
