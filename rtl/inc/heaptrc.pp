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

type
    fill_extra_info_type = procedure(p : pointer);

    { allows to add several longint value that can help
      to debug :
      see for instance ppheap.pas unit of the compiler source PM }
      
procedure set_extra_info( size : longint;func : fill_extra_info_type);

const
  { tracing level
    splitted in two if memory is released !! }
  tracesize = 8;
  quicktrace : boolean=true;
  { calls halt() on error by default !! }
  halt_on_error : boolean = true;
  { set this to true if you suspect that memory
    is freed several times }
  keepreleased : boolean=false;

implementation

const
  { allows to add custom info in heap_mem_info }
  extra_info_size : longint = 0;
  exact_info_size : longint = 0;
  { function to fill this info up }
  fill_extra_info : fill_extra_info_type = nil;

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
    extra_info : record
                 end;
  end;

var
  heap_mem_root : pheap_mem_info;
  getmem_cnt,
  freemem_cnt   : longint;
  getmem_size,
  freemem_size   : longint;
  getmem8_size,
  freemem8_size   : longint;


{*****************************************************************************
                                Helpers
*****************************************************************************}

type plongint = ^longint;

procedure call_stack(pp : pheap_mem_info);
var
  i  : longint;
begin
  writeln(stderr,'Call trace for block 0x',hexstr(longint(pp+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(stderr,'  0x',hexstr(pp^.calls[i],8));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(stderr,'info ',i,'=',plongint(@pp^.extra_info+4*i)^);
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
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(stderr,'info ',i,'=',plongint(@pp^.extra_info+4*i)^);
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

procedure dump_wrong_size(p : pheap_mem_info;size : longint);
begin
  Writeln(stderr,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' invalid');
  Writeln(stderr,'Wrong size : ',p^.size,' allocated ',size,' freed');
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
  inc(getmem8_size,((size+7) div 8)*8);
{ Do the real GetMem, but alloc also for the info block }
  SysGetMem(p,size+sizeof(theap_mem_info)+extra_info_size);
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
  if assigned(fill_extra_info) then
    fill_extra_info(@pheap_mem_info(p)^.extra_info);
{ update the pointer }
  inc(p,sizeof(theap_mem_info)+extra_info_size);
  inc(getmem_cnt);
end;


{*****************************************************************************
                               TraceFreeMem
*****************************************************************************}

procedure TraceFreeMem(var p:pointer;size:longint);

  var i,bp, ppsize : longint;
  pp : pheap_mem_info;
begin
  inc(freemem_size,size);
  inc(freemem8_size,((size+7) div 8)*8);
  ppsize:= size + sizeof(theap_mem_info)+extra_info_size;
  dec(p,sizeof(theap_mem_info)+extra_info_size);
  pp:=pheap_mem_info(p);
  if not quicktrace and not(is_in_getmem_list(p)) then
    RunError(204);
  if pp^.sig=$AAAAAAAA then
    begin
       dump_already_free(pp);
       if halt_on_error then halt(1);
    end
  else if pp^.sig<>$DEADBEEF then
    begin
       dump_error(pp);
       { don't release anything in this case !! }
       if halt_on_error then halt(1);
       exit;
    end
  else if pp^.size<>size then
    begin
       dump_wrong_size(pp,size);
       if halt_on_error then halt(1);
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
       bp:=get_caller_frame(get_frame);
       for i:=(tracesize div 2)+1 to tracesize do
        begin
          pp^.calls[i]:=get_caller_addr(bp);
          bp:=get_caller_frame(bp);
        end;
    end;
  inc(freemem_cnt);
  { release the normal memory at least !! }
  { this way we keep all info about all released memory !! }
  if keepreleased then
    begin
       dec(ppsize,sizeof(theap_mem_info)+extra_info_size);
       inc(p,sizeof(theap_mem_info)+extra_info_size);
    end;
  SysFreeMem(p,ppsize);
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
  Writeln(stderr,getmem_cnt, ' memory blocks allocated : ',getmem_size,'/',getmem8_size);
  Writeln(stderr,freemem_cnt,' memory blocks freed     : ',freemem_size,'/',freemem8_size);
  Writeln(stderr,getmem_cnt-freemem_cnt,' unfreed memory blocks : ',getmem_size-freemem_size);
  Writeln(stderr,'True heap size : ',system.HeapSize);
  Writeln(stderr,'True free heap : ',MemAvail);
  Writeln(stderr,'Should be : ',system.HeapSize-(getmem8_size-freemem8_size)-
    (getmem_cnt-freemem_cnt)*(sizeof(theap_mem_info)+extra_info_size));
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

procedure set_extra_info( size : longint;func : fill_extra_info_type);

  begin
     if getmem_cnt>0 then
       begin
         writeln(stderr,'settting extra info is only possible at start !! ');
         dump_heap;
       end
     else
       begin
          { the total size must stay multiple of 8 !! }
          exact_info_size:=size;
          extra_info_size:=((size+7) div 8)*8;
          fill_extra_info:=func;
       end;
  end;
  

begin
  SetMemoryManager(TraceManager);
  SaveExit:=ExitProc;
  ExitProc:=@TraceExit;
end.
{
  $Log$
  Revision 1.6  1998-11-06 08:46:01  pierre
    * size is now also checked
    + added halt_on_error variable (default true)
      to stop at first error in getmem/freemem

  Revision 1.5  1998/10/09 11:59:31  pierre
    * changed default to keepreleased=false
      (allows to compile pp in one call without reaching the
      64Mb limit of Windows 95 dos box)
    * corrected so typo errors

  Revision 1.4  1998/10/08 14:49:05  pierre
   + added possibility for more info

  Revision 1.3  1998/10/06 17:09:13  pierre
   + added trace of first dispose for errors

  Revision 1.2  1998/10/02 10:35:38  peter
    + quicktrace

  Revision 1.1  1998/10/01 14:54:20  peter
    + first version

}
