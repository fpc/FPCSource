{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by the Free Pascal development team.

    functions for heap management in the data segment


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
   Supported conditionnals:
   ------------------------
   TEMPHEAP     to allow to split the heap in two parts for easier release
                started for the compiler

   CHECKHEAP    if you want to test the heap integrity

}

{$ASMMODE DIRECT}

const
  max_size = 256;
  maxblock = max_size div 8;
  freerecord_list_length : longint = 0;

type
  pfreerecord = ^tfreerecord;
  tfreerecord = record
    next : pfreerecord;
    size : longint;
  end;

  tblocks   = array[1..maxblock] of pointer;
  pblocks   = ^tblocks;
  tnblocks  = array[1..maxblock] of longint;
  pnblocks  = ^tnblocks;


  ppointer = ^pointer;


var
  internal_memavail  : longint;
  internal_heapsize  : longint;
  baseblocks         : tblocks;
  basenblocks        : tnblocks;


const
  blocks  : pblocks  = @baseblocks;
  nblocks : pnblocks = @basenblocks;


{$IfDef CHECKHEAP}
    { 4 levels of tracing }
    const
       tracesize = 4;
    type
       pheap_mem_info = ^heap_mem_info;
       heap_mem_info = record
          next,
          previous : pheap_mem_info;
          size     : longint;
          sig      : longint; {dummy number for test }
          calls    : array [1..tracesize] of longint;
        end;
       { size 8*4 = 32 }
    const
       { help variables for debugging with GDB }
       check         : boolean = false;
       growheapstop  : boolean = false;
       free_nothing  : boolean = false;
       trace         : boolean = true;
    var

       last_assigned : pheap_mem_info;
       getmem_nb     : longint;
       freemem_nb    : longint;
{$EndIf CHECKHEAP}


{$ifdef TEMPHEAP}
    const
       heap_split : boolean = false;
    type
       pheapinfo = ^theapinfo;
       theapinfo = record
         heaporg,heapptr,
         heapend,freelist  : pointer;
         memavail,heapsize : longint;
         block  : pblocks;
         nblock : pnblocks;
  {$IfDef CHECKHEAP}
         last_mem : pheap_mem_info;
         nb_get,
         nb_free  : longint;
  {$EndIf CHECKHEAP}
       end;
    var
       baseheap  : theapinfo;
       curheap   : pheapinfo;
       tempheap  : theapinfo;
       otherheap : pheapinfo;
{$endif TEMPHEAP}


{$ifndef OS2}
{ OS2 function getheapstart is in sysos2.pas }
    function getheapstart : pointer;
      begin
         asm
            leal HEAP,%eax
            leave
            ret
         end ['EAX'];
      end;
{$endif}

    function getheapsize : longint;
      begin
         asm
            movl HEAPSIZE,%eax
            leave
            ret
         end ['EAX'];
      end;


{*****************************************************************************
                       Heapsize,Memavail,MaxAvail
*****************************************************************************}

function heapsize : longint;
begin
  heapsize:=internal_heapsize;
end;


function memavail : longint;
begin
  memavail:=internal_memavail;
end;


function maxavail : longint;
var
  hp : pfreerecord;
begin
  maxavail:=heapend-heapptr;
  hp:=freelist;
  while assigned(hp) do
   begin
     if hp^.size>maxavail then
       maxavail:=hp^.size;
     hp:=hp^.next;
   end;
end;


function calc_memavail : longint;
var
  hp : pfreerecord;
  ma : longint;
  i  : longint;
begin
  ma:=heapend-heapptr;
{ count blocks }

  if heapblocks then
   for i:=1 to maxblock do
    inc(ma,i*8*nblocks^[i]);
{ walk freelist }

  hp:=freelist;
  while assigned(hp) do
   begin
     inc(ma,hp^.size);
{$IfDef CHECKHEAP}
     if (longint(hp^.next)=0) then
      begin
        if ((longint(hp)+hp^.size)>longint(heapptr)) then
         writeln('freerecordlist bad at end ')
      end
     else
      if ((longint(hp^.next)<=(longint(hp)+hp^.size)) or
         ((hp^.size and 7) <> 0)) then
        writeln('error in freerecord list ');
{$EndIf CHECKHEAP}
     hp:=hp^.next;
   end;
  calc_memavail:=ma;
end;


{*****************************************************************************
                          Check Heap helpers
*****************************************************************************}

{$IfDef CHECKHEAP}

   procedure call_stack(p : pointer);
     var
       i  : longint;
       pp : pheap_mem_info;
     begin
       if trace then
        begin
          pp:=pheap_mem_info(p-sizeof(heap_mem_info));
          writeln('Call trace of 0x',hexstr(longint(p),8));
          writeln('of size ',pp^.size);
          for i:=1 to tracesize do
           writeln(i,' 0x',hexstr(pp^.calls[i],8));
        end
       else
        writeln('tracing not enabled, sorry !!');
     end;

   procedure dump_heap(mark : boolean);
     var
       pp : pheap_mem_info;
     begin
       pp:=last_assigned;
       while pp<>nil do
        begin
          call_stack(pp+sizeof(heap_mem_info));
          if mark then
            pp^.sig:=$AAAAAAAA;
          pp:=pp^.previous;
        end;
     end;

   procedure dump_free(p : pheap_mem_info);
     var
       ebp : longint;
     begin
       Writeln('Marked memory at ',HexStr(longint(p),8),' released');
       call_stack(p+sizeof(heap_mem_info));
       asm
           movl (%ebp),%eax
           movl (%eax),%eax
           movl %eax,ebp
       end;
       dump_stack(ebp);
     end;


   function is_in_getmem_list (p : pointer) : boolean;
     var
       i  : longint;
       pp : pheap_mem_info;
     begin
       is_in_getmem_list:=false;
       pp:=last_assigned;
       i:=0;
       while pp<>nil do
        begin
          if (pp^.sig<>$DEADBEEF) and (pp^.sig <> $AAAAAAAA) then
           begin
             writeln('error in linked list of heap_mem_info');
             HandleError(204);
           end
          if pp=p then
            is_in_getmem_list:=true;
          pp:=pp^.previous;
          inc(i);
          if i > getmem_nb - freemem_nb then
            writeln('error in linked list of heap_mem_info');
        end;
     end;

   function is_in_free(p : pointer) : boolean;
     var
       hp : pfreerecord;
     begin
       if p>heapptr then
        begin
          is_in_free:=true;
          exit;
        end
       else
        begin
          hp:=freelist;
          while assigned(hp) do
           begin
             if (p>=hp) and (p<hp+hp^.size) then
              begin
                is_in_free:=true;
                exit;
              end;
             hp:=hp^.next;
           end;
          is_in_free:=false;
        end;
     end;

   procedure test_memavail;
     begin
       if check and (internal_memavail<>calc_memavail) then
         writeln('Memavail error in getmem/freemem');
     end;

{$EndIf CHECKHEAP}


{*****************************************************************************
                             Temp Heap support
*****************************************************************************}

{$ifdef TEMPHEAP}
  procedure split_heap;
    begin
      if not heap_split then
       begin
         baseheap.heaporg:=heaporg;
         baseheap.heapptr:=heapptr;
         baseheap.freelist:=freelist;
         baseheap.block:=blocks;
         baseheap.nblock:=nblocks;
         longint(baseheap.heapend):=((longint(heapend)+longint(heapptr)) div 16)*8;
         tempheap.heaporg:=baseheap.heapend;
         tempheap.freelist:=nil;
         tempheap.heapptr:=tempheap.heaporg;
    {$IfDef CHECKHEAP}
         tempheap.last_mem:=nil;
         tempheap.nb_get:=0;
         tempheap.nb_free:=0;
    {$EndIf CHECKHEAP}
         tempheap.heapend:=heapend;
         tempheap.memavail:=longint(tempheap.heapend) - longint(tempheap.heaporg);
         tempheap.heapsize:=tempheap.memavail;
         getmem(tempheap.block,sizeof(tblocks));
         getmem(tempheap.nblock,sizeof(tnblocks));
	 fillchar(tempheap.block^,sizeof(tblocks),0);
	 fillchar(tempheap.nblock^,sizeof(tnblocks),0);
         heapend:=baseheap.heapend;
         internal_memavail:=calc_memavail;
         baseheap.memavail:=internal_memavail;
         baseheap.heapsize:=longint(baseheap.heapend)-longint(baseheap.heaporg);
         curheap:=@baseheap;
         otherheap:=@tempheap;
         heap_split:=true;
       end;
    end;

  procedure switch_to_temp_heap;
    begin
      if curheap = @baseheap then
       begin
         baseheap.heaporg:=heaporg;
         baseheap.heapend:=heapend;
         baseheap.heapptr:=heapptr;
         baseheap.freelist:=freelist;
         baseheap.memavail:=internal_memavail;
         baseheap.block:=blocks;
         baseheap.nblock:=nblocks;
     {$IfDef CHECKHEAP}
         baseheap.last_mem:=last_assigned;
         last_assigned:=tempheap.last_mem;
         baseheap.nb_get:=getmem_nb;
         baseheap.nb_free:=freemem_nb;
         getmem_nb:=tempheap.nb_get;
         freemem_nb:=tempheap.nb_free;
     {$EndIf CHECKHEAP}
         heaporg:=tempheap.heaporg;
         heapptr:=tempheap.heapptr;
         freelist:=tempheap.freelist;
         heapend:=tempheap.heapend;
         blocks:=tempheap.block;
         nblocks:=tempheap.nblock;
         internal_memavail:=calc_memavail;
         curheap:=@tempheap;
         otherheap:=@baseheap;
       end;
    end;

  procedure switch_to_base_heap;
    begin
      if curheap = @tempheap then
       begin
         tempheap.heaporg:=heaporg;
         tempheap.heapend:=heapend;
         tempheap.heapptr:=heapptr;
         tempheap.freelist:=freelist;
         tempheap.memavail:=internal_memavail;
     {$IfDef CHECKHEAP}
         tempheap.last_mem:=last_assigned;
         last_assigned:=baseheap.last_mem;
         tempheap.nb_get:=getmem_nb;
         tempheap.nb_free:=freemem_nb;
         getmem_nb:=baseheap.nb_get;
         freemem_nb:=baseheap.nb_free;
     {$EndIf CHECKHEAP}
         heaporg:=baseheap.heaporg;
         heapptr:=baseheap.heapptr;
         freelist:=baseheap.freelist;
         heapend:=baseheap.heapend;
         blocks:=baseheap.block;
         nblocks:=baseheap.nblock;
         internal_memavail:=calc_memavail;
         curheap:=@baseheap;
         otherheap:=@tempheap;
       end;
    end;

  procedure switch_heap;
    begin
      if not heap_split then
        split_heap;
      if curheap = @tempheap then
        switch_to_base_heap
      else
        switch_to_temp_heap;
    end;

  procedure gettempmem(var p : pointer;size : longint);
    begin
      split_heap;
      switch_to_temp_heap;
      allow_special:=true;
      getmem(p,size);
      allow_special:=false;
    end;

  procedure unsplit_heap;
    var
      hp,hp2,thp : pfreerecord;
    begin
    {heapend can be modified by HeapError }
      if not heap_split then
        exit;
      if baseheap.heapend = tempheap.heaporg then
       begin
         switch_to_base_heap;
         hp:=pfreerecord(freelist);
         if assigned(hp) then
          begin
            while assigned(hp^.next) do
             hp:=hp^.next;
          end;  
         if tempheap.heapptr<>tempheap.heaporg then
          begin
            if hp<>nil then
             hp^.next:=heapptr;
            hp:=pfreerecord(heapptr);
            hp^.size:=heapend-heapptr;
            hp^.next:=tempheap.freelist;
            heapptr:=tempheap.heapptr;
          end;
         heapend:=tempheap.heapend;
         internal_memavail:=calc_memavail;
         heap_split:=false;
       end
      else
       begin
         hp:=pfreerecord(baseheap.freelist);
         hp2:=pfreerecord(tempheap.freelist);
         while assigned(hp) and assigned(hp2) do
          begin
            if hp=hp2 then
             break;
            if hp>hp2 then
             begin
               thp:=hp2;
               hp2:=hp;
               hp:=thp;
             end;
            while assigned(hp^.next) and (hp^.next<hp2) do
             hp:=hp^.next;
            if assigned(hp^.next) then
             begin
               thp:=hp^.next;
               hp^.next:=hp2;
               hp:=thp;
             end
            else
             begin
               hp^.next:=hp2;
               hp:=nil;
             end;
          end;
         if heapend < tempheap.heapend then
           heapend:=tempheap.heapend;
         if heapptr < tempheap.heapptr then
           heapptr:=tempheap.heapptr;
         freemem(tempheap.block,sizeof(tblocks));
         freemem(tempheap.nblock,sizeof(tnblocks));
         internal_memavail:=calc_memavail;
         heap_split:=false;
       end;
    end;

    procedure releasetempheap;
    begin
      switch_to_temp_heap;
  {$ifdef CHECKHEAP}
      if heapptr<>heaporg then
        writeln('Warning in releasetempheap : ',longint(tempheap.heapsize)-longint(tempheap.memavail),' bytes used !');
      dump_heap(true);
    { release(heaporg);
      fillchar(heaporg^,longint(heapend)-longint(heaporg),#0); }
  {$endif CHECKHEAP }
      unsplit_heap;
      split_heap;
    end;
{$endif TEMPHEAP}


{*****************************************************************************
                                GetMem
*****************************************************************************}

procedure getmem(var p : pointer;size : longint);[public,alias: 'GETMEM'];

   { changed to removed the OS conditionnals }
   function call_heaperror(addr : pointer; size : longint) : integer;
   begin
     asm
              pushl size
              movl  addr,%eax
              { movl HEAPERROR,%eax doesn't work !!}
              call %eax
              movw %ax,__RESULT
      end;
   end;

var
  last,hp  : pfreerecord;
  again    : boolean;
  s,hpsize : longint;
{$IfDef CHECKHEAP}
  i,bp,orsize : longint;
label
  check_new;
{$endif CHECKHEAP}
begin
{$ifdef CHECKHEAP}
  if trace then
   begin
     orsize:=size;
     inc(size,sizeof(heap_mem_info));
   end;
{$endif CHECKHEAP}
  if size=0 then
   begin
     p:=heapend;
{$ifdef CHECKHEAP}
     goto check_new;
{$else CHECKHEAP}
     exit;
{$endif CHECKHEAP}
   end;
{ temp heap checking }
{$ifdef TEMPHEAP}
  if heap_split and not allow_special then
   begin
     if (@p < otherheap^.heapend) and (@p > otherheap^.heaporg) then
     { useful line for the debugger }
       writeln('warning : p and @p are in different heaps !');
   end;
{$endif TEMPHEAP}
{ calc to multiply of 8 }
  size:=(size+7) and (not 7);
  dec(internal_memavail,size);
{ first try heap blocks }
  if heapblocks then
   begin
     { search cache }
     if size<=max_size then
      begin
        s:=size shr 3;
        p:=blocks^[s];
        if assigned(p) then
         begin
           blocks^[s]:=pointer(p^);
           dec(nblocks^[s]);
{$ifdef CHECKHEAP}
           goto check_new;
{$else CHECKHEAP}
           exit;
{$endif CHECKHEAP}
         end;
      end;
   end;
{ walk free list }
  repeat
    again:=false;
    { search the freelist }
    if assigned(freelist) then
     begin
       last:=nil;
       hp:=freelist;
       while assigned(hp) do
        begin
          hpsize:=hp^.size;
          { take the first fitting block }
          if hpsize>=size then
           begin
             p:=hp;
             { need we the whole block ? }
             if (hpsize>size) and heapblocks then
              begin
                { we must check if we are still below the limit !! }
                if hpsize-size<=max_size then
                 begin
                   { adjust the list }
                   if assigned(last) then
                    last^.next:=hp^.next
                   else
                    freelist:=hp^.next;
                   { insert in chain }
                   s:=(hpsize-size) div 8;
                   ppointer(hp+size)^:=blocks^[s];
                   blocks^[s]:=hp+size;
                   inc(nblocks^[s]);
                 end
                else
                 begin
                   (hp+size)^.size:=hpsize-size;
                   (hp+size)^.next:=hp^.next;
                   if assigned(last) then
                    last^.next:=hp+size
                   else
                    freelist:=hp+size;
                 end;
              end
             else
              begin
{$IfDef CHECKHEAP}
                dec(freerecord_list_length);
{$endif CHECKHEAP}
               if assigned(last) then
                last^.next:=hp^.next
               else
                freelist:=hp^.next;
              end;
{$ifdef CHECKHEAP}
             goto check_new;
{$else CHECKHEAP}
             exit;
{$endif CHECKHEAP}
           end;
          last:=hp;
          hp:=hp^.next;
        end;
     end;
  { Latly, the top of the heap is checked, to see if there is }
  { still memory available.                                   }
    if heapend-heapptr<size then
     begin
       if assigned(heaperror) then
        begin
          case call_heaperror(heaperror,size) of
           0 : HandleError(203);
           1 : p:=nil;
           2 : again:=true;
          end;
        end
       else
        HandleError(203);
     end
    else
     begin
       p:=heapptr;
       inc(heapptr,size);
     end;
  until not again;
{$ifdef CHECKHEAP}
check_new:
  inc(getmem_nb);
  test_memavail;
  if trace then
   begin
     asm
         movl (%ebp),%eax
         movl %eax,bp
     end;
     pheap_mem_info(p)^.sig:=$DEADBEEF;
     pheap_mem_info(p)^.previous:=last_assigned;
     if last_assigned<>nil then
       last_assigned^.next:=pheap_mem_info(p);
     last_assigned:=p;
     pheap_mem_info(p)^.next:=nil;
     pheap_mem_info(p)^.size:=orsize;
     for i:=1 to tracesize do
      begin
        pheap_mem_info(p)^.calls[i]:=get_addr(bp);
        bp:=get_next_frame(bp);
      end;
     inc(p,sizeof(heap_mem_info));
   end;
{$endif CHECKHEAP}
end;


{*****************************************************************************
                                FreeMem
*****************************************************************************}

procedure freemem(var p : pointer;size : longint);[public,alias: 'FREEMEM'];
var
  hp : pfreerecord;
{$ifdef TEMPHEAP}
  heap_switched : boolean;
{$endif TEMPHEAP}
  s : longint;
label
  freemem_exit;
begin
  if size=0 then
   begin
     p:=nil;
     exit;
   end;
  if p=nil then
   HandleError(204);
{$ifdef CHECKHEAP}
  if free_nothing then
   begin
     p:=nil;
     exit;
   end;
  if trace then
   begin
     inc(size,sizeof(heap_mem_info));
     dec(p,sizeof(heap_mem_info));
   end;
{$endif CHECKHEAP}
{$ifdef TEMPHEAP}
  heap_switched:=false;
  if heap_split and not allow_special then
   begin
     if (p<=heapptr) and (p>=heaporg) and
        (@p<=otherheap^.heapend) and (@p>=otherheap^.heaporg) then
       writeln('warning : p and @p are in different heaps !');
   end;
  if (p<heaporg) or (p>heapptr) then
   begin
     if heap_split and (p<otherheap^.heapend) and (p>otherheap^.heaporg) then
      begin
        if (@p>=heaporg) and (@p<=heapptr) and not allow_special then
          writeln('warning : p and @p are in different heaps !');
        switch_heap;
        heap_switched:=true;
      end
     else
      begin
        writeln('pointer ',hexstr(longint(@p),8),' at ',hexstr(longint(p),8),' doesn''t points to the heap');
        HandleError(204);
      end;
   end;
{$endif TEMPHEAP}
{$ifdef CHECKHEAP}
  if trace then
   begin
     if not (is_in_getmem_list(p)) then
       HandleError(204);
     if pheap_mem_info(p)^.sig=$AAAAAAAA then
       dump_free(p);
     if pheap_mem_info(p)^.next<>nil then
       pheap_mem_info(p)^.next^.previous:=pheap_mem_info(p)^.previous;
     if pheap_mem_info(p)^.previous<>nil then
       pheap_mem_info(p)^.previous^.next:=pheap_mem_info(p)^.next;
     if pheap_mem_info(p)=last_assigned then
       last_assigned:=last_assigned^.previous;
   end;
{$endif CHECKHEAP}
{ calc to multiple of 8 }
  size:=(size+7) and (not 7);
  inc(internal_memavail,size);
{ end of the heap ? }
  if p+size>=heapptr then
   begin
     heapptr:=p;
     goto freemem_exit;
   end;
{ heap block? }
  if heapblocks and (size<=max_size) then
   begin
     s:=size shr 3;
     ppointer(p)^:=blocks^[s];
     blocks^[s]:=p;
     inc(nblocks^[s]);
   end
  else
   begin
     { size can be allways set }
     pfreerecord(p)^.size:=size;
     { if there is no free list }
     if not assigned(freelist) then
      begin
        { then generate one }
        freelist:=p;
        pfreerecord(p)^.next:=nil;
{$ifdef CHECKHEAP}
        inc(freerecord_list_length);
{$endif CHECKHEAP}
        goto freemem_exit;
      end;
     if p+size<freelist then
      begin
        pfreerecord(p)^.next:=freelist;
        freelist:=p;
{$ifdef CHECKHEAP}
        inc(freerecord_list_length);
{$endif CHECKHEAP}
        goto freemem_exit;
      end
     else
      if p+size=freelist then
       begin
         pfreerecord(p)^.size:=Pfreerecord(p)^.size+pfreerecord(freelist)^.size;
         pfreerecord(p)^.next:=pfreerecord(freelist)^.next;
         freelist:=p;
         { but now it can also connect the next block !!}
         if p+pfreerecord(p)^.size=pfreerecord(p)^.next then
          begin
            pfreerecord(p)^.size:=pfreerecord(p)^.size+pfreerecord(p)^.next^.size;
{$ifdef CHECKHEAP}
            dec(freerecord_list_length);
{$endif CHECKHEAP}
            pfreerecord(p)^.next:=pfreerecord(freelist)^.next^.next;
          end;
         goto freemem_exit;
       end;
   { insert block in freelist }
     hp:=freelist;
     while assigned(hp) do
      begin
        if p<hp+hp^.size then
         begin
{$ifdef CHECKHEAP}
           writeln('pointer to dispose at ',hexstr(longint(p),8),' has already been disposed');
{$endif CHECKHEAP}
           HandleError(204);
         end;
        { connecting two blocks ? }
        if hp+hp^.size=p then
         begin
           inc(hp^.size,size);
           { connecting also to next block ? }
           if hp+hp^.size=hp^.next then
            begin
              inc(hp^.size,hp^.next^.size);
{$ifdef CHECKHEAP}
              dec(freerecord_list_length);
{$endif CHECKHEAP}
              hp^.next:=hp^.next^.next;
            end
           else
            if (hp^.next<>nil) and (hp+hp^.size>hp^.next) then
             begin
{$ifdef CHECKHEAP}
               writeln('pointer to dispose at ',hexstr(longint(p),8),' is too big !!');
{$endif CHECKHEAP}
               HandleError(204);
             end;
           break;
         end
        { if the end is reached, then concat }
        else
         if hp^.next=nil then
          begin
            hp^.next:=p;
{$ifdef CHECKHEAP}
            inc(freerecord_list_length);
{$endif CHECKHEAP}
            pfreerecord(p)^.next:=nil;
            break;
          end
        { if next pointer is greater, then insert }
        else
         if hp^.next>p then
          begin
            { connect to blocks }
            if p+size=hp^.next then
             begin
               pfreerecord(p)^.next:=hp^.next^.next;
               pfreerecord(p)^.size:=pfreerecord(p)^.size+hp^.next^.size;
               { we have to reset the right position }
               hp^.next:=pfreerecord(p);
             end
            else
             begin
               pfreerecord(p)^.next:=hp^.next;
               hp^.next:=p;
{$ifdef CHECKHEAP}
               inc(freerecord_list_length);
{$endif CHECKHEAP}
             end;
            break;
          end;
        hp:=hp^.next;
      end;
   end;
freemem_exit:
{$ifdef CHECKHEAP}
  inc(freemem_nb);
  test_memavail;
{$endif CHECKHEAP}
{$ifdef TEMPHEAP}
  if heap_switched then
    switch_heap;
{$endif TEMPHEAP}
  p:=nil;
end;


{*****************************************************************************
                                Mark/Release
*****************************************************************************}

procedure release(var p : pointer);
begin
  heapptr:=p;
  freelist:=nil;
  internal_memavail:=calc_memavail;
end;


procedure mark(var p : pointer);
begin
  p:=heapptr;
end;


procedure markheap(var oldfreelist,oldheapptr : pointer);
begin
  oldheapptr:=heapptr;
  oldfreelist:=freelist;
  freelist:=nil;
  internal_memavail:=calc_memavail;
end;



procedure releaseheap(oldfreelist,oldheapptr : pointer);
begin
  heapptr:=oldheapptr;
  if longint(freelist) < longint(heapptr) then
   begin
     { here we should reget the freed blocks }
   end;
  freelist:=oldfreelist;
  internal_memavail:=calc_memavail;
end;


{*****************************************************************************
                                Grow Heap
*****************************************************************************}

function growheap(size :longint) : integer;
var
  Newlimit,
  NewPos,
  wantedsize : longint;
  hp         : pfreerecord;
begin
   wantedsize:=size;
   { Allocate by 64K size }
   size:=(size+$fffff) and $ffff0000;
   { first try 1Meg }
   if size<$100000 then
    begin
      NewPos:=Sbrk($100000);
      if NewPos>0 then
       size:=$100000;
    end
   else
    NewPos:=SBrk(size);
   { try again }
   if NewPos=-1 then
    begin
      NewPos:=Sbrk(size);
      if NewPos=-1 then
       begin
         GrowHeap:=0;
{$IfDef CHECKHEAP}
         writeln('Call to GrowHeap failed');
         readln;
{$EndIf CHECKHEAP}
         Exit;
       end;
    end;
  { make the room clean }
{$ifdef CHECKHEAP}
   Fillword(pointer(NewPos)^,size div 2,$ABCD);
   Newlimit:=(newpos+size) or $3fff;
{$endif CHECKHEAP}
   hp:=pfreerecord(freelist);
   if not assigned(hp) then
    begin
      if pointer(newpos) = heapend then
       heapend:=pointer(newpos+size)
      else
       begin
         if heapend - heapptr > 0 then
          begin
            freelist:=heapptr;
            hp:=pfreerecord(freelist);
            hp^.size:=heapend-heapptr;
            hp^.next:=nil;
          end;
         heapptr:=pointer(newpos);
         heapend:=pointer(newpos+size);
       end;
    end
   else
    begin
      if pointer(newpos) = heapend then
       heapend:=pointer(newpos+size)
      else
       begin
         while assigned(hp^.next) and (longint(hp^.next) < longint(NewPos)) do
          hp:=hp^.next;
         if hp^.next = nil then
          begin
            hp^.next:=pfreerecord(heapptr);
            hp:=pfreerecord(heapptr);
            hp^.size:=heapend-heapptr;
            hp^.next:=nil;
            heapptr:=pointer(NewPos);
            heapend:=pointer(NewPos+Size);
          end
         else
          begin
            pfreerecord(NewPos)^.Size:=Size;
            pfreerecord(NewPos)^.Next:=hp^.next;
            hp^.next:=pfreerecord(NewPos);
          end;
       end;
    end;
 { the wanted size has to be substracted
    why it will be substracted in the second try
    to get the memory PM }
   internal_memavail:=calc_memavail;
 { set the total new heap size }
   asm
           movl Size,%ebx
           movl HEAPSIZE,%eax
           addl %ebx,%eax
           movl %eax,HEAPSIZE
   end;
   inc(internal_heapsize,size);
  { try again }
   GrowHeap:=2;
{$IfDef CHECKHEAP}
    writeln('Call to GrowHeap succedeed : HeapSize = ',internal_heapsize,' MemAvail = ',memavail);
    writeln('New heap part begins at ',Newpos,' with size ',size);
    if growheapstop then
      readln;
{$EndIf CHECKHEAP}
end;


{*****************************************************************************
                                 InitHeap
*****************************************************************************}

{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeap;
begin
  FillChar(Blocks^,sizeof(Blocks^),0);
  FillChar(NBlocks^,sizeof(NBlocks^),0);
{$ifdef TEMPHEAP}
  Curheap:=@baseheap;
  Otherheap:=@tempheap;
{$endif TEMPHEAP}
  internal_memavail:=GetHeapSize;
  HeapOrg:=GetHeapStart;
  HeapPtr:=HeapOrg;
  HeapEnd:=HeapOrg+internal_memavail;
  HeapError:=@GrowHeap;
  internal_heapsize:=longint(heapend)-longint(heaporg);
  Freelist:=nil;
end;

{$ASMMODE ATT}

{
  $Log$
  Revision 1.18  1998-09-08 15:02:48  peter
    * much more readable :)

  Revision 1.17  1998/09/04 17:27:48  pierre
    * small corrections

  Revision 1.16  1998/08/25 14:15:51  pierre
    * corrected a bug introduced by my last change
      (allocating 1Mb but only using a small part !!)

  Revision 1.15  1998/08/24 14:44:04  pierre
    * bug allocation of more than 1 MB failed corrected

  Revision 1.14  1998/07/30 13:26:21  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.13  1998/07/02 14:24:09  michael
  Undid carls changes, but renamed _heapsize to internal_heapsize. Make cycle now works

  Revision 1.11  1998/06/25 09:26:10  daniel
  * Removed some more tabs

  Revision 1.10  1998/06/24 11:53:26  daniel
  * Removed some tabs.

  Revision 1.9  1998/06/16 14:55:49  daniel
  * Optimizations

  Revision 1.8  1998/06/15 15:15:13  daniel
  * Brought my policy into practive that the RTL should output only runtime
  errors and no other texts when things go wrong.

  Revision 1.7  1998/05/30 15:01:28  peter
    * this needs also direct mode :(

  Revision 1.6  1998/05/25 10:40:48  peter
    * remake3 works again on tflily

  Revision 1.4  1998/04/21 10:22:48  peter
    + heapblocks

  Revision 1.3  1998/04/09 08:32:14  daniel
  * Optimized some code.
}
