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

{$mode objfpc}

interface

Procedure DumpHeap;
Procedure MarkHeap;

{ define EXTRA to add more
  tests :
   - keep all memory after release and
   check by CRC value if not changed after release
   WARNING this needs extremely much memory (PM) }

type
    FillExtraInfoType = procedure(p : pointer);

    { allows to add several longint value that can help
      to debug :
      see for instance ppheap.pas unit of the compiler source PM }

Procedure SetExtraInfo( size : longint;func : FillExtraInfoType);
Procedure SetHeapTraceOutput(const name : string);

const
  { tracing level
    splitted in two if memory is released !! }
{$ifdef EXTRA}
  tracesize = 16;
{$else EXTRA}
  tracesize = 8;
{$endif EXTRA}
  quicktrace : boolean=true;
  { calls halt() on error by default !! }
  HaltOnError : boolean = true;
  { set this to true if you suspect that memory
    is freed several times }
{$ifdef EXTRA}
  keepreleased : boolean=true;
  add_tail : boolean = true;
{$else EXTRA}
  keepreleased : boolean=false;
  add_tail : boolean = false;
{$endif EXTRA}
  { put crc in sig
    this allows to test for writing into that part }
  usecrc : boolean = true;

implementation

type
   plongint = ^longint;

const
  { allows to add custom info in heap_mem_info }
  extra_info_size : longint = 0;
  exact_info_size : longint = 0;
  { function to fill this info up }
  fill_extra_info : FillExtraInfoType = nil;
  error_in_heap : boolean = false;
type
  pheap_mem_info = ^theap_mem_info;
  { warning the size of theap_mem_info
    must be a multiple of 8
    because otherwise you will get
    problems when releasing the usual memory part !!
    sizeof(theap_mem_info = 16+tracesize*4 so
    tracesize must be even !! PM }
  theap_mem_info = record
    previous,
    next     : pheap_mem_info;
    size     : longint;
    sig      : longint;
{$ifdef EXTRA}
    release_sig : longint;
    next_valid : pheap_mem_info;
{$endif EXTRA}
    calls    : array [1..tracesize] of longint;
    extra_info : record
                 end;
  end;

var
  ptext : ^text;
  ownfile : text;
{$ifdef EXTRA}
  error_file : text;
  heap_valid_first,
  heap_valid_last : pheap_mem_info;
{$endif EXTRA}
  heap_mem_root : pheap_mem_info;
  getmem_cnt,
  freemem_cnt   : longint;
  getmem_size,
  freemem_size   : longint;
  getmem8_size,
  freemem8_size   : longint;


{*****************************************************************************
                                   Crc 32
*****************************************************************************}

var
{$ifdef Delphi}
  Crc32Tbl : array[0..255] of longword;
{$else Delphi}
  Crc32Tbl : array[0..255] of longint;
{$endif Delphi}

procedure MakeCRC32Tbl;
var
{$ifdef Delphi}
  crc : longword;
{$else Delphi}
  crc : longint;
{$endif Delphi}
  i,n : byte;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if odd(crc) then
       crc:=(crc shr 1) xor $edb88320
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


{$ifopt R+}
{$define Range_check_on}
{$endif opt R+}

{$R- needed here }

Function UpdateCrc32(InitCrc:longint;var InBuf;InLen:Longint):longint;
var
  i : longint;
  p : pchar;
begin
  p:=@InBuf;
  for i:=1 to InLen do
   begin
     InitCrc:=Crc32Tbl[byte(InitCrc) xor byte(p^)] xor (InitCrc shr 8);
     inc(longint(p));
   end;
  UpdateCrc32:=InitCrc;
end;

Function calculate_sig(p : pheap_mem_info) : longint;
var
   crc : longint;
   pl : plongint;
begin
   crc:=$ffffffff;
   crc:=UpdateCrc32(crc,p^.size,sizeof(longint));
   crc:=UpdateCrc32(crc,p^.calls,tracesize*sizeof(longint));
   if extra_info_size>0 then
     crc:=UpdateCrc32(crc,p^.extra_info,exact_info_size);
   if add_tail then
     begin
        { Check also 4 bytes just after allocation !! }
        pl:=pointer(p)+extra_info_size+sizeof(theap_mem_info)+p^.size;
        crc:=UpdateCrc32(crc,pl^,sizeof(longint));
     end;
   calculate_sig:=crc;
end;

{$ifdef EXTRA}
Function calculate_release_sig(p : pheap_mem_info) : longint;
var
   crc : longint;
   pl : plongint;
begin
   crc:=$ffffffff;
   crc:=UpdateCrc32(crc,p^.size,sizeof(longint));
   crc:=UpdateCrc32(crc,p^.calls,tracesize*sizeof(longint));
   if extra_info_size>0 then
     crc:=UpdateCrc32(crc,p^.extra_info,exact_info_size);
   { Check the whole of the whole allocation }
   pl:=pointer(p)+extra_info_size+sizeof(theap_mem_info);
   crc:=UpdateCrc32(crc,pl^,p^.size);
   { Check also 4 bytes just after allocation !! }
   if add_tail then
     begin
        { Check also 4 bytes just after allocation !! }
        pl:=pointer(p)+extra_info_size+sizeof(theap_mem_info)+p^.size;
        crc:=UpdateCrc32(crc,pl^,sizeof(longint));
     end;
   calculate_release_sig:=crc;
end;
{$endif EXTRA}

{$ifdef Range_check_on}
{$R+}
{$undef Range_check_on}
{$endif Range_check_on}

{*****************************************************************************
                                Helpers
*****************************************************************************}

procedure call_stack(pp : pheap_mem_info;var ptext : text);
var
  i  : longint;
begin
  writeln(ptext,'Call trace for block 0x',hexstr(longint(pp+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(ptext,'  0x',hexstr(pp^.calls[i],8));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(@pp^.extra_info+4*i)^);
end;

procedure call_free_stack(pp : pheap_mem_info;var ptext : text);
var
  i  : longint;

begin
  writeln(ptext,'Call trace for block 0x',hexstr(longint(pp+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize div 2 do
   if pp^.calls[i]<>0 then
     writeln(ptext,'  0x',hexstr(pp^.calls[i],8));
  writeln(ptext,' was released at ');
  for i:=(tracesize div 2)+1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(ptext,'  0x',hexstr(pp^.calls[i],8));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(@pp^.extra_info+4*i)^);
end;


procedure dump_already_free(p : pheap_mem_info;var ptext : text);
begin
  Writeln(ptext,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' released');
  call_free_stack(p,ptext);
  Writeln(ptext,'freed again at');
  dump_stack(ptext,get_caller_frame(get_frame));
end;

procedure dump_error(p : pheap_mem_info;var ptext : text);
begin
  Writeln(ptext,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong signature $',hexstr(p^.sig,8)
    ,' instead of ',hexstr(calculate_sig(p),8));
  dump_stack(ptext,get_caller_frame(get_frame));
end;

{$ifdef EXTRA}
procedure dump_change_after(p : pheap_mem_info;var ptext : text);
 var pp : pchar;
     i : longint;
begin
  Writeln(ptext,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong release CRC $',hexstr(p^.release_sig,8)
    ,' instead of ',hexstr(calculate_release_sig(p),8));
  Writeln(ptext,'This memory was changed after call to freemem !');
  call_free_stack(p,ptext);
  pp:=pchar(p)+sizeof(theap_mem_info)+extra_info_size;
  for i:=0 to p^.size-1 do
    if byte(pp[i])<>$F0 then
      Writeln(ptext,'offset',i,':$',hexstr(i,8),'"',pp[i],'"');
end;
{$endif EXTRA}

procedure dump_wrong_size(p : pheap_mem_info;size : longint;var ptext : text);
var
  i : longint;
begin
  Writeln(ptext,'Marked memory at ',HexStr(longint(p+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong size : ',p^.size,' allocated ',size,' freed');
  dump_stack(ptext,get_caller_frame(get_frame));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(@p^.extra_info+4*i)^);
  call_stack(p,ptext);
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
     if ((pp^.sig<>$DEADBEEF) or usecrc) and
        ((pp^.sig<>calculate_sig(pp)) or not usecrc) and
        (pp^.sig <> $AAAAAAAA) then
      begin
        writeln(ptext^,'error in linked list of heap_mem_info');
        RunError(204);
      end;
     if pp=p then
      is_in_getmem_list:=true;
     pp:=pp^.previous;
     inc(i);
     if i>getmem_cnt-freemem_cnt then
      writeln(ptext^,'error in linked list of heap_mem_info');
   end;
end;


{*****************************************************************************
                               TraceGetMem
*****************************************************************************}

procedure TraceGetMem(var p:pointer;size:longint);
var
  i,bp : longint;
  pl : plongint;
begin
  inc(getmem_size,size);
  inc(getmem8_size,((size+7) div 8)*8);
{ Do the real GetMem, but alloc also for the info block }
  bp:=size+sizeof(theap_mem_info)+extra_info_size;
  if add_tail then
    bp:=bp+sizeof(longint);
  SysGetMem(p,bp);
{ Create the info block }
  pheap_mem_info(p)^.sig:=$DEADBEEF;
  pheap_mem_info(p)^.size:=size;
  if add_tail then
    begin
      pl:=pointer(p)+bp-sizeof(longint);
      pl^:=$DEADBEEF;
    end;
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
{$ifdef EXTRA}
  pheap_mem_info(p)^.next_valid:=nil;
  if assigned(heap_valid_last) then
    heap_valid_last^.next_valid:=pheap_mem_info(p);
  heap_valid_last:=pheap_mem_info(p);
  if not assigned(heap_valid_first) then
    heap_valid_first:=pheap_mem_info(p);
{$endif EXTRA}
  heap_mem_root:=p;
  if assigned(fill_extra_info) then
    fill_extra_info(@pheap_mem_info(p)^.extra_info);
{ update the pointer }
  if usecrc then
    pheap_mem_info(p)^.sig:=calculate_sig(pheap_mem_info(p));
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
  if add_tail then
    ppsize:=ppsize+sizeof(longint);
  dec(p,sizeof(theap_mem_info)+extra_info_size);
  pp:=pheap_mem_info(p);
  if not quicktrace and not(is_in_getmem_list(p)) then
    RunError(204);
  if pp^.sig=$AAAAAAAA then
    begin
       error_in_heap:=true;
       dump_already_free(pp,ptext^);
       if haltonerror then halt(1);
    end
  else if ((pp^.sig<>$DEADBEEF) or usecrc) and
        ((pp^.sig<>calculate_sig(pp)) or not usecrc) then
    begin
       error_in_heap:=true;
       dump_error(pp,ptext^);
{$ifdef EXTRA}
       dump_error(pp,error_file);
{$endif EXTRA}
       { don't release anything in this case !! }
       if haltonerror then halt(1);
       exit;
    end
  else if pp^.size<>size then
    begin
       error_in_heap:=true;
       dump_wrong_size(pp,size,ptext^);
{$ifdef EXTRA}
       dump_wrong_size(pp,size,error_file);
{$endif EXTRA}
       if haltonerror then halt(1);
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
    end
  else
    begin
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
{$ifndef EXTRA}
       dec(ppsize,sizeof(theap_mem_info)+extra_info_size);
       inc(p,sizeof(theap_mem_info)+extra_info_size);
{$else EXTRA}
      inc(p,sizeof(theap_mem_info)+extra_info_size);
      fillchar(p^,size,#240); { $F0 will lead to GFP if used as pointer ! }
      { We want to check if the memory was changed after release !! }
       pp^.release_sig:=calculate_release_sig(pp);
       if pp=heap_valid_first then
         begin
            heap_valid_first:=pp^.next_valid;
            if pp=heap_valid_last then
              heap_valid_last:=nil;
            exit;
         end;
       pp2:=heap_valid_first;
       while assigned(pp2) do
         begin
            if pp2^.next_valid=pp then
              begin
                 pp2^.next_valid:=pp^.next_valid;
                 if pp=heap_valid_last then
                   heap_valid_last:=pp2;
                 exit;
              end
            else
              pp2:=pp2^.next_valid;
         end;
       exit;
{$endif EXTRA}
    end;
  SysFreeMem(p,ppsize);
end;


{*****************************************************************************
                              Check pointer
*****************************************************************************}

{$ifdef go32v2}
var
   __stklen : cardinal;external name '__stklen';
   __stkbottom : cardinal;external name '__stkbottom';
   edata : cardinal; external name 'edata';
{$endif go32v2}

var
   heap_at_init : pointer;

procedure CheckPointer(p : pointer);[public, alias : 'FPC_CHECKPOINTER'];
var
  i  : longint;
  pp : pheap_mem_info;
  get_ebp,stack_top : cardinal;
  data_end : cardinal;
label
  _exit;
begin
  asm
     pushal
  end;
  if p=nil then
    goto _exit;

  i:=0;

{$ifdef go32v2}
  if cardinal(p)<$1000 then
    runerror(216);
  asm
     movl %ebp,get_ebp
     leal edata,%eax
     movl %eax,data_end
  end;
  stack_top:=__stkbottom+__stklen;
  { allow all between start of code and end of data }
  if cardinal(p)<=data_end then
    goto _exit;
  { .bss section }
  if cardinal(p)<=cardinal(heap_at_init) then
    goto _exit;
  { stack can be above heap !! }

  if (cardinal(p)>=get_ebp) and (cardinal(p)<=stack_top) then
    goto _exit;
{$endif go32v2}

  { I don't know where the stack is in other OS !! }

  if p>=heapptr then
    runerror(216);
  { first try valid list faster }

{$ifdef EXTRA}
  pp:=heap_valid_first;
  while pp<>nil do
   begin
     { inside this valid block ! }
     if (cardinal(p)>=cardinal(pp)+sizeof(theap_mem_info)+extra_info_size) and
        (cardinal(p)<=cardinal(pp)+sizeof(theap_mem_info)+extra_info_size+pp^.size) then
       begin
          { check allocated block }
          if ((pp^.sig=$DEADBEEF) and not usecrc) or
             ((pp^.sig=calculate_sig(pp)) and usecrc) then
            goto _exit;
       end
     else
       pp:=pp^.next_valid;
     inc(i);
     if i>getmem_cnt-freemem_cnt then
      begin
         writeln(ptext^,'error in linked list of heap_mem_info');
         halt(1);
      end;
   end;
  i:=0;
{$endif EXTRA}
  pp:=heap_mem_root;
  while pp<>nil do
   begin
     { inside this block ! }
     if (cardinal(p)>=cardinal(pp)+sizeof(theap_mem_info)+extra_info_size) and
        (cardinal(p)<=cardinal(pp)+sizeof(theap_mem_info)+extra_info_size+pp^.size) then
        { allocated block }
       if ((pp^.sig=$DEADBEEF) and not usecrc) or
          ((pp^.sig=calculate_sig(pp)) and usecrc) then
          goto _exit
       else
         begin
            writeln(ptext^,'pointer $',hexstr(longint(p),8),' points into invalid memory block');
            dump_error(pp,ptext^);
            runerror(204);
         end;
     pp:=pp^.previous;
     inc(i);
     if i>getmem_cnt then
      begin
         writeln(ptext^,'error in linked list of heap_mem_info');
         halt(1);
      end;
   end;
  writeln(ptext^,'pointer $',hexstr(longint(p),8),' does not point to valid memory block');
  runerror(204);
_exit:
  asm
     popal
  end;
end;

{*****************************************************************************
                              Dump Heap
*****************************************************************************}

procedure dumpheap;
var
  pp : pheap_mem_info;
  i : longint;
begin
  pp:=heap_mem_root;
  Writeln(ptext^,'Heap dump by heaptrc unit');
  Writeln(ptext^,getmem_cnt, ' memory blocks allocated : ',getmem_size,'/',getmem8_size);
  Writeln(ptext^,freemem_cnt,' memory blocks freed     : ',freemem_size,'/',freemem8_size);
  Writeln(ptext^,getmem_cnt-freemem_cnt,' unfreed memory blocks : ',getmem_size-freemem_size);
  Writeln(ptext^,'True heap size : ',system.HeapSize);
  Writeln(ptext^,'True free heap : ',MemAvail);
  Writeln(ptext^,'Should be : ',system.HeapSize-(getmem8_size-freemem8_size)-
    (getmem_cnt-freemem_cnt)*(sizeof(theap_mem_info)+extra_info_size));
  i:=getmem_cnt-freemem_cnt;
  while pp<>nil do
   begin
     if i<0 then
       begin
          Writeln(ptext^,'Error in heap memory list');
          Writeln(ptext^,'More memory blocks than expected');
          exit;
       end;
     if ((pp^.sig=$DEADBEEF) and not usecrc) or
        ((pp^.sig=calculate_sig(pp)) and usecrc) then
       begin
          { this one was not released !! }
          if exitcode<>203 then
            call_stack(pp,ptext^);
          dec(i);
       end
     else if pp^.sig<>$AAAAAAAA then
       begin
          dump_error(pp,ptext^);
{$ifdef EXTRA}
       dump_error(pp,error_file);
{$endif EXTRA}
          error_in_heap:=true;
       end
{$ifdef EXTRA}
     else if pp^.release_sig<>calculate_release_sig(pp) then
       begin
          dump_change_after(pp,ptext^);
          dump_change_after(pp,error_file);
          error_in_heap:=true;
       end
{$endif EXTRA}
       ;
     pp:=pp^.previous;
   end;
end;


procedure markheap;
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

procedure TraceExit;
begin
  { no dump if error
    because this gives long long listings }
  if (exitcode<>0) and (erroraddr<>nil) then
    begin
       Writeln(ptext^,'No heap dump by heaptrc unit');
       Writeln(ptext^,'Exitcode = ',exitcode);
       if ptext<>@stderr then
         begin
            ptext:=@stderr;
            close(ownfile);
         end;
       exit;
    end;
  if not error_in_heap then
    Dumpheap;
  if error_in_heap and (exitcode=0) then
    exitcode:=203;
{$ifdef EXTRA}
  Close(error_file);
{$endif EXTRA}
   if ptext<>@stderr then
     begin
        ptext:=@stderr;
        close(ownfile);
     end;
end;

Procedure SetHeapTraceOutput(const name : string);
begin
   if ptext<>@stderr then
     begin
        ptext:=@stderr;
        close(ownfile);
     end;
   assign(ownfile,name);
{$I-}
   append(ownfile);
   if IOResult<>0 then
     Rewrite(ownfile);
{$I+}
   ptext:=@ownfile;
end;

procedure SetExtraInfo( size : longint;func : fillextrainfotype);

  begin
     if getmem_cnt>0 then
       begin
         writeln(ptext^,'Setting extra info is only possible at start !! ');
         dumpheap;
       end
     else
       begin
          { the total size must stay multiple of 8 !! }
          exact_info_size:=size;
          extra_info_size:=((size+7) div 8)*8;
          fill_extra_info:=func;
       end;
  end;

Initialization
  MakeCRC32Tbl;
  SetMemoryManager(TraceManager);
  ptext:=@stderr;
{$ifdef EXTRA}
  Assign(error_file,'heap.err');
  Rewrite(error_file);
{$endif EXTRA}
  Heap_at_init:=HeapPtr;
finalization
  TraceExit;
end.
{
  $Log$
  Revision 1.16.2.3  1999-07-10 10:31:56  peter
    * removed unused var

  Revision 1.16.2.2  1999/07/09 10:44:23  michael
  + Merged finalize

  Revision 1.18  1999/07/09 10:38:10  michael
  + + heaptrc now uses finalize instead of exitproc

  Revision 1.17  1999/07/05 20:22:08  peter
    * merged

  Revision 1.16.2.1  1999/07/05 20:12:27  peter
    * removed warning

  Revision 1.16  1999/05/23 00:07:17  pierre
    * support for heap allocated before TraceGetMem is used in
      FPC_CHECKPOINTER
    * faster CHECKPOINTER routine (list of valid blocks only !)

  Revision 1.15  1999/05/18 22:15:55  pierre
   * allow for .bss section below heaporg in go32v2 code

  Revision 1.14  1999/05/16 23:56:09  pierre
   * allow nil pointer in FPC_CHECKPOINTER !!

  Revision 1.13  1999/05/12 16:49:29  pierre
   + with EXTRA memory is filled with $F0 and checked at end

  Revision 1.12  1999/05/11 12:52:42  pierre
   + extra's with -dEXTRA, uses a CRC check for released memory

  Revision 1.11  1999/03/26 19:10:34  peter
    * show also allocation stack for a wrong size

  Revision 1.10  1999/02/16 17:20:26  pierre
   * no heap dump if program has an heap error !

  Revision 1.9  1999/01/22 12:39:22  pierre
   + added text arg for dump_stack

  Revision 1.8  1998/12/15 23:49:51  michael
  + Removed underscores in heaptrc unit

  Revision 1.7  1998/11/16 12:20:13  peter
    * write extra info also for wrong size

  Revision 1.6  1998/11/06 08:46:01  pierre
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
