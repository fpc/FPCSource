{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
               functions for heap management in the data segment
 ****************************************************************************}
{**** 10/06/97 added checkings and corrected some bugs in getmem/freemem ****}
{**** Pierre Muller *********************************************************}

{ three conditionnals here }

{ TEMPHEAP to  allow to split the heap in two parts for easier release}
{ started for the compiler }
{ USEBLOCKS if you want special allocation for small blocks }
{ CHECKHEAP if you want to test the heap integrity }

{$IfDef CHECKHEAP}
    { 4 levels of tracing }
    const tracesize = 4;
    type   pheap_mem_info = ^heap_mem_info;
           heap_mem_info = record
           next,previous : pheap_mem_info;
           size : longint;
           sig : longint; {dummy number for test }
           calls : array [1..tracesize] of longint;
           end;
           { size 8*4 = 32 }
    { help variables for debugging with GDB }
    const check : boolean = false;
    const last_assigned : pheap_mem_info = nil;
    const growheapstop : boolean = false;

    const free_nothing : boolean = false;
    const trace : boolean = true;
    const getmem_nb : longint = 0;
    const freemem_nb : longint = 0;
{$EndIf CHECKHEAP}

    const
       heap_split : boolean = false;
       max_size = 256;
       maxblock = max_size div 8;
       freerecord_list_length : longint = 0;

    var
       _memavail : longint;
       _heapsize : longint;

    type
{$ifdef UseBlocks}
       tblocks   = array[1..maxblock] of pointer;
       pblocks   = ^tblocks;
       tnblocks  = array[1..maxblock] of longint;
       pnblocks  = ^tnblocks;
{$endif UseBlocks}
       pheapinfo = ^theapinfo;
       theapinfo = record
         heaporg,heapptr,heapend,freelist : pointer;
         memavail,heapsize : longint;
{$ifdef UseBlocks}
         block : pblocks;
         nblock : pnblocks;
{$endif UseBlocks}
{$IfDef CHECKHEAP}
        last_mem : pheap_mem_info;
        nb_get,nb_free : longint;
{$EndIf CHECKHEAP}
         end;
    type
       pfreerecord = ^tfreerecord;

       tfreerecord = record
          next : pfreerecord;
          size : longint;
       end;

    var
       baseheap : theapinfo;
       curheap : pheapinfo;
{$ifdef TEMPHEAP}
       tempheap : theapinfo;
       otherheap : pheapinfo;
{$endif TEMPHEAP}

{$ifdef UseBlocks}
       baseblocks : tblocks;
       basenblocks : tnblocks;
{$endif UseBlocks}

{ this is not supported by FPK <v093
    const
       blocks : pblocks = @baseblocks;
       nblocks : pnblocks = @basenblocks; }
      type
         ppointer = ^pointer;

{$ifdef UseBlocks}
    var blocks : pblocks;
        nblocks : pnblocks;
{$endif UseBlocks}


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

    function heapsize : longint;

      begin
         heapsize:=_heapsize;
      end;

{$IfDef CHECKHEAP}
    procedure call_stack(p : pointer);
      var i : longint;
          pp : pheap_mem_info;
      begin

        if trace then
          begin
             pp:=pheap_mem_info(p-sizeof(heap_mem_info));
             writeln('Call trace of 0x',hexstr(longint(p),8));
             writeln('of size ',pp^.size);
             for i:=1 to tracesize do
               begin
                 writeln(i,' 0x',hexstr(pp^.calls[i],8));
               end;
          end
        else
          writeln('tracing not enabled, sorry !!');
      end;

    procedure dump_heap(mark : boolean);
      var pp : pheap_mem_info;
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
      var ebp : longint;
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
        var pp : pheap_mem_info;
            i : longint;
      begin
        is_in_getmem_list:=false;
        pp:=last_assigned;
        i:=0;
        while pp<>nil do
          begin
             if (pp^.sig<>$DEADBEEF) and (pp^.sig <> $AAAAAAAA) then
               begin
                  writeln('error in linked list of heap_mem_info');
                  runerror(204);
               end;

             if pp=p then
               begin
                  is_in_getmem_list:=true;
               end;
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
{$EndIf CHECKHEAP}

    function cal_memavail : longint;

      var
         hp : pfreerecord;
         ma : longint;
{$ifdef UseBlocks}
         i  : longint;
{$endif UseBlocks}

      begin
         ma:=heapend-heapptr;
{$ifdef UseBlocks}
         for i:=1 to maxblock do
           ma:=ma+i*8*nblocks^[i];
{$endif UseBlocks}
         hp:=freelist;
         while assigned(hp) do
           begin
              ma:=ma+hp^.size;
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
         cal_memavail:=ma;
      end;

{$ifdef TEMPHEAP}
    procedure split_heap;
      var i :longint;
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
      for i:=1 to maxblock do
        begin
        tempheap.block^[i]:=nil;
        tempheap.nblock^[i]:=0;
        end;
      heapend:=baseheap.heapend;
      _memavail:=cal_memavail;
      baseheap.memavail:=_memavail;
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
      baseheap.memavail:=_memavail;
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
      _memavail:=cal_memavail;
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
      tempheap.memavail:=_memavail;
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
      _memavail:=cal_memavail;
      curheap:=@baseheap;
      otherheap:=@tempheap;
      end;
    end;

    procedure switch_heap;
    begin
    if not heap_split then split_heap;
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
{$endif TEMPHEAP}

    function memavail : longint;

      begin
         memavail:=_memavail;
      end;

{$ifdef TEMPHEAP}
    procedure unsplit_heap;
    var hp,hp2,thp : pfreerecord;
    begin
    {heapend can be modified by HeapError }
    if not heap_split then exit;
    if baseheap.heapend = tempheap.heaporg then
      begin
      switch_to_base_heap;
      hp:=pfreerecord(freelist);
      if assigned(hp) then
        while assigned(hp^.next) do hp:=hp^.next;
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
      _memavail:=cal_memavail;
      heap_split:=false;
      end else
      begin
      hp:=pfreerecord(baseheap.freelist);
      hp2:=pfreerecord(tempheap.freelist);
      while assigned(hp) and assigned(hp2) do
        begin
        if hp=hp2 then break;
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
            end else
            begin
            hp^.next:=hp2;
            hp:=nil;
            end;
          end ;
      if heapend < tempheap.heapend then
        heapend:=tempheap.heapend;
      if heapptr < tempheap.heapptr then
        heapptr:=tempheap.heapptr;
      freemem(tempheap.block,sizeof(tblocks));
      freemem(tempheap.nblock,sizeof(tnblocks));
      _memavail:=cal_memavail;
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
{    release(heaporg);
    fillchar(heaporg^,longint(heapend)-longint(heaporg),#0);}
{$endif CHECKHEAP }
    unsplit_heap;
    split_heap;
    end;
{$endif TEMPHEAP}

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

{$ifdef CHECKHEAP}
     procedure test_memavail;

       begin
          if check and (_memavail<>cal_memavail) then
            begin
               writeln('Memavail error in getmem/freemem');
            end;
       end;
{$endif CHECKHEAP}

    procedure getmem(var p : pointer;size : longint);[public,alias: 'GETMEM'];

{$IfDef CHECKHEAP}
      var i,bp,orsize : longint;
      label check_new;
{$endif CHECKHEAP}

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
         last,hp : pfreerecord;
         nochmal : boolean;
{$ifdef UseBlocks}
         s : longint;
{$endif}	

      begin
{$ifdef CHECKHEAP}
         if trace then
           begin
              orsize:=size;
              size:=size+sizeof(heap_mem_info);
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
{$ifdef TEMPHEAP}
         if heap_split and not allow_special then
           begin
           if (@p < otherheap^.heapend) and
              (@p > otherheap^.heaporg) then
              { useful line for the debugger }
             writeln('warning : p and @p are in different heaps !');
           end;
{$endif TEMPHEAP}
         { calc to multiply of 8 }
         size:=(size+7) and not 7;
         dec(_memavail,size);
{$ifdef UseBlocks}
         { search cache }
         if size<=max_size then
           begin
              s:=size div 8;
              if assigned(blocks^[s]) then
                begin
                   p:=blocks^[s];
{$ifdef VER0_6}
                   move(blocks^[s]^,blocks^[s],4);
{$else VER0_6}
                   blocks^[s]:=pointer(blocks^[s]^);
{$endif VER0_6}
                   dec(nblocks^[s]);
{$ifdef CHECKHEAP}
                   goto check_new;
{$else CHECKHEAP}
                   exit;
{$endif CHECKHEAP}
                end;
           end;
{$endif UseBlocks}
         repeat
           nochmal:=false;
           { search the freelist }
           if assigned(freelist) then
             begin
                last:=nil;
                hp:=freelist;
                while assigned(hp) do
                  begin
                     { take the first fitting block }
                     if hp^.size>=size then
                       begin
                          p:=hp;
                          { need we the whole block ? }
                          if hp^.size>size then
                            begin
{$ifdef UseBlocks}
                               { we must check if we are still below the limit !! }
                               if hp^.size-size<=max_size then
                                 begin
                                    { adjust the list }
                                    if assigned(last) then
                                      last^.next:=hp^.next
                                    else
                                      freelist:=hp^.next;
                                    { insert in chain }
                                    s:=(hp^.size-size) div 8;
                                    ppointer(hp+size)^:=blocks^[s];
                                    blocks^[s]:=hp+size;
                                    inc(nblocks^[s]);
                                 end
                               else
{$endif UseBlocks}
                               begin
                                  (hp+size)^.size:=hp^.size-size;
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
                                 {this was wrong !!}
                                 {freelist:=nil;}
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
                        0 : runerror(203);
                        1 : p:=nil;
                        2 : nochmal:=true;
                     end;
                  end
                else
                  runerror(203);
             end
           else
             begin
                p:=heapptr;
                heapptr:=heapptr+size;
             end;
         until not nochmal;
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
          p:=p+sizeof(heap_mem_info);
       end;
{$endif CHECKHEAP}
      end;

    procedure freemem(var p : pointer;size : longint);[public,alias: 'FREEMEM'];

      var
         hp : pfreerecord;
{$ifdef TEMPHEAP}
         heap_switched : boolean;
{$endif TEMPHEAP}
{$ifdef UseBlocks}
         s : longint;
{$endif UseBlocks}
      label freemem_exit;

      begin
{$ifdef CHECKHEAP}
         if free_nothing then
           begin
              p:=nil;
              exit;
           end;
     if trace then
       begin
          size:=size+sizeof(heap_mem_info);
          p:=p-sizeof(heap_mem_info);
          { made after heap_switch
          if not (is_in_getmem_list(p)) then
            runerror(204); }
       end;
{$endif CHECKHEAP}
         if size=0 then
           begin
              p:=nil;
              exit;
           end;
         if p=nil then RunError (204);
{$ifdef TEMPHEAP}
         heap_switched:=false;
         if heap_split and not allow_special then
           begin
              if (p <= heapptr) and
                 ( p >= heaporg) and
                 (@p <= otherheap^.heapend) and
                 (@p >= otherheap^.heaporg) then
                begin
                   writeln('warning : p and @p are in different heaps !');
                end;
           end;
         if (p<heaporg) or (p>heapptr) then
           begin
              if heap_split and (p<otherheap^.heapend) and
                 (p>otherheap^.heaporg) then
                begin
                   if (@p >= heaporg) and
                      (@p <= heapptr) and
                      not allow_special then
                      writeln('warning : p and @p are in different heaps !');
                   switch_heap;
                   heap_switched:=true;
                end
              else
                begin
                   writeln('pointer ',hexstr(longint(@p),8),' at ',
                     hexstr(longint(p),8),' doesn''t points to the heap');
                   runerror(204);
                end;
           end;
{$endif TEMPHEAP}
{$ifdef CHECKHEAP}
     if trace then
       begin
          if not (is_in_getmem_list(p)) then
            runerror(204);
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
         size:=(size+7) and not 7;
         inc(_memavail,size);
         if p+size>=heapptr then
           heapptr:=p
{$ifdef UseBlocks}
         { insert into cache }
         else if size<=max_size then
           begin
              s:=size div 8;
              ppointer(p)^:=blocks^[s];
              blocks^[s]:=p;
              inc(nblocks^[s]);
           end
{$endif UseBlocks}
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
                inc(pfreerecord(p)^.size,pfreerecord(freelist)^.size);
                pfreerecord(p)^.next:=pfreerecord(freelist)^.next;
                freelist:=p;
                { but now it can also connect the next block !!}
                if p+pfreerecord(p)^.size=pfreerecord(p)^.next then
                  begin
                     inc(pfreerecord(p)^.size,pfreerecord(p)^.next^.size);
{$ifdef CHECKHEAP}
                     dec(freerecord_list_length);
{$endif CHECKHEAP}
                     pfreerecord(p)^.next:=pfreerecord(freelist)^.next^.next;
                  end;
                goto freemem_exit;
                end;
              { search the insert position }
              hp:=freelist;
              while assigned(hp) do
                begin
                   if p<hp+hp^.size then
                      begin
                      writeln('pointer to dispose at ',hexstr(longint(p),8),
                        ' has already been disposed');
                      runerror(204);
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
                              writeln('pointer to dispose at ',hexstr(longint(p),8),
                                ' is too big !!');
                              runerror(204);
                            end;
                         break;
                      end
                   { if the end is reached, then concat }
                   else if hp^.next=nil then
                     begin
                        hp^.next:=p;
{$ifdef CHECKHEAP}
                        inc(freerecord_list_length);
{$endif CHECKHEAP}
                        pfreerecord(p)^.next:=nil;
                        break;
                     end
                   { falls der n„chste Zeiger gr”áer ist, dann }
                   { Einh„ngen                                 }
                   else if hp^.next>p then
                     begin
                        { connect to blocks }
                        if p+size=hp^.next then
                          begin
                             pfreerecord(p)^.next:=hp^.next^.next;
                             inc(pfreerecord(p)^.size,hp^.next^.size);
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
         p:=nil;
{$ifdef TEMPHEAP}
         if heap_switched then switch_heap;
{$endif TEMPHEAP}
      end;

    procedure release(var p : pointer);

      begin
         heapptr:=p;
         freelist:=nil;
         _memavail:=cal_memavail;
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
         _memavail:=cal_memavail;
      end;

    procedure releaseheap(oldfreelist,oldheapptr : pointer);

      begin
         heapptr:=oldheapptr;
         if longint(freelist) < longint(heapptr) then
           begin
           {here we should reget the freed blocks}
           end;
         freelist:=oldfreelist;
         _memavail:=cal_memavail;
      end;

{ the sbrk  function is moved to the system.pp }
{ as it is system dependent !!                 }
function growheap(size :longint) : integer;

  var NewPos,wantedsize : longint;
         hp : pfreerecord;
    Newlimit : longint;

begin
   wantedsize:=size;
   size:=size+$ffff;
   size:=size and $ffff0000;
   { Allocate by 64K size }
   { first try 1Meg }
   NewPos:=Sbrk($100000);
   if NewPos=-1 then
     NewPos:=Sbrk(size)
   else
     size:=$100000;
   if (NewPos = -1) then
     begin
        GrowHeap:=0;
        {$IfDef CHECKHEAP}
        writeln('Call to GrowHeap failed');
        readln;
        {$EndIf CHECKHEAP}
        Exit;
     end
   else
     begin
     { make the room clean }
{$ifdef CHECKHEAP}
        Fillword(pointer(NewPos)^,size div 2,$ABCD);
        Newlimit:= (newpos+size) or $3fff;
{$else }
        Fillchar(pointer(NewPos)^,size,#0);
{$endif }
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
        { the wanted size has to be substracted }
        _memavail:=cal_memavail-wantedsize;
        { set the total new heap size }
        asm
        movl Size,%ebx
        movl HEAPSIZE,%eax
        addl %ebx,%eax
        movl %eax,HEAPSIZE
        end;
        GrowHeap:=2;{ try again }
        _Heapsize:=size+_heapsize;
{$IfDef CHECKHEAP}
        writeln('Call to GrowHeap succedeed : HeapSize = ',_HeapSize,' MemAvail = ',memavail);
        writeln('New heap part begins at ',Newpos,' with size ',size);
        if growheapstop then
          readln;
{$EndIf CHECKHEAP}
        exit;
     end;
end;


{ This function will initialize the Heap manager and need to be called from
  the initialization of the system unit }
procedure InitHeap;
{$ifdef UseBlocks}
var
  i : longint;
{$endif UseBlocks}  
begin
{$ifdef UseBlocks}
  Blocks:=@baseblocks;
  Nblocks:=@basenblocks;
  for i:=1 to maxblock do
   begin
     Blocks^[i]:=nil;
     Nblocks^[i]:=0;
   end;
{$endif UseBlocks}
  Curheap := @baseheap;
{$ifdef TEMPHEAP}
  Otherheap := @tempheap;
{$endif TEMPHEAP}
  HeapOrg := GetHeapStart;
  HeapPtr := HeapOrg;
  _memavail := GetHeapSize;
  HeapEnd := HeapOrg + _memavail;
  HeapError := @GrowHeap;
  _heapsize:=longint(heapend)-longint(heaporg);
  Freelist := nil;
end;

{
  $Log$
  Revision 1.3  1998-04-09 08:32:14  daniel
  * Optimized some code.

  Revision 1.2  1998/03/31 19:01:41  daniel
  * Replaced 'mod 8' with 'and 7'. Generates more efficient code.

  Revision 1.1.1.1  1998/03/25 11:18:43  root
  * Restored version

  Revision 1.7  1998/01/26 11:59:20  michael
  + Added log at the end


  
  Working file: rtl/i386/heap.inc
  description:
  ----------------------------
  revision 1.6
  date: 1998/01/19 14:02:30;  author: pierre;  state: Exp;  lines: +2 -2
    * bug in initheap fixed
  ----------------------------
  revision 1.5
  date: 1998/01/05 16:51:21;  author: michael;  state: Exp;  lines: +31 -1
  + Moved init of heap to heap.inc: INITheap() (From Peter Vreman)
  ----------------------------
  revision 1.4
  date: 1998/01/03 00:46:08;  author: michael;  state: Exp;  lines: +9 -3
  * put ifdef useblocks around some local vars (From Peter Vreman)
  ----------------------------
  revision 1.3
  date: 1997/12/10 12:21:47;  author: michael;  state: Exp;  lines: +2 -1
  * Put test for nil pointer in freemem() function.
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:34:36;  author: michael;  state: Exp;  lines: +11 -3
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
