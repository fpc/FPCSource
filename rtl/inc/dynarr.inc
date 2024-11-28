{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Florian Klaempfl
    member of the Free Pascal development team.

    This file implements the helper routines for dyn. Arrays in FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

type
   { don't add new fields, the size is used }
   { to calculate memory requirements       }
   pdynarray = ^tdynarray;
   { removed packed here as
     1) both fields have typically the same size (2, 4 or 8 bytes), if this is not the case, packed
        should be used only for this architecture
     2) the memory blocks are sufficiently well aligned
     3) in particular 64 bit CPUs which require natural alignment suffer from
        the packed as it causes each field access being split in 8 single loads and appropriate shift operations
   }
   tdynarray = { packed } record
      refcount : ptrint;
      high : tdynarrayindex;
   end;

   pdynarraytypedata = ^tdynarraytypedata;
   tdynarraytypedata =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   record
     {$if declared(TRttiDataCommon)}
     common: TRttiDataCommon;
     {$endif declared TRttiDataCommon}
     case TTypeKind of
       tkArray: (
         elSize : SizeUInt;
         elType2 : PPointer;
         varType : Longint;
         elType : PPointer;
       );
       { include for proper alignment }
       tkInt64: (
         dummy : Int64
       );
   end;

procedure fpc_dynarray_rangecheck(p : pointer;i : tdynarrayindex);[Public,Alias:'FPC_DYNARRAY_RANGECHECK']; compilerproc;
  begin
     if not(assigned(p)) or (i<0) or (i>pdynarray(p-sizeof(tdynarray))^.high) then
       HandleErrorAddrFrameInd(201,get_pc_addr,get_frame);
  end;


function fpc_dynarray_length(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_LENGTH']; compilerproc;
  begin
     if assigned(p) then
       fpc_dynarray_length:=pdynarray(p-sizeof(tdynarray))^.high+1
     else
       fpc_dynarray_length:=0;
  end;


function fpc_dynarray_high(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_HIGH']; compilerproc;
  begin
     if assigned(p) then
       fpc_dynarray_high:=pdynarray(p-sizeof(tdynarray))^.high
     else
       fpc_dynarray_high:=-1;
  end;


procedure fpc_dynarray_clear(var p : pointer;ti : pointer); [Public,Alias:'FPC_DYNARRAY_CLEAR']; compilerproc;
  var
    pv : pdynarray;
  begin
    pv:=p;
    if not assigned(pv) then
      exit;
    p:=nil;
    if (pv[-1].refcount>0) and declocked(pv[-1].refcount) then
      begin
        ti:=pdynarraytypedata(aligntoqword(ti+2+PByte(ti)[1]))^.elType;
        if assigned(ti) then
          int_finalizearray(pv,PPointer(ti)^,pv[-1].high+1);
        freemem(pv-1);
      end;
  end;

{ alias for internal use }
Procedure fpc_dynarray_clear (var p : pointer;ti : pointer);[external name 'FPC_DYNARRAY_CLEAR'];


procedure fpc_dynarray_incr_ref(p : pointer);[Public,Alias:'FPC_DYNARRAY_INCR_REF']; compilerproc;
  begin
     if assigned(p) and (pdynarray(p)[-1].refcount>0) then
       inclocked(pdynarray(p)[-1].refcount);
  end;

{ provide local access to dynarr_decr_ref for dynarr_setlength }
procedure fpc_dynarray_incr_ref(p : pointer); [external name 'FPC_DYNARRAY_INCR_REF'];


procedure fpc_dynarray_assign(var dest: Pointer; src: Pointer; ti: pointer);[public,alias:'FPC_DYNARRAY_ASSIGN']; compilerproc;
  begin
    fpc_dynarray_incr_ref(src);
    fpc_dynarray_clear(dest,ti);
    Dest:=Src;
  end;

procedure fpc_dynarray_assign(var dest: Pointer; src: Pointer; ti: pointer);[external name 'FPC_DYNARRAY_ASSIGN'];

{ provide local access to dynarr_setlength }
procedure int_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : sizeint;dims : pdynarrayindex);[external name 'FPC_DYNARR_SETLENGTH'];

procedure fpc_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : sizeint;dims : pdynarrayindex);[Public,Alias:'FPC_DYNARR_SETLENGTH']; compilerproc;

  var
     i : tdynarrayindex;
     movelen,size,_size,elesize,oldlen,newlen : sizeint;
     { contains the "fixed" pointers where the refcount }
     { and high are at positive offsets                 }
     realp,newp : pdynarray;
     ti,eletypemngd : pointer;

  begin
     newlen:=dims[0];
     { negative or zero length? }
     if newlen<=0 then
       begin
         { negative length is not allowed }
         if newlen<0 then
           HandleErrorAddrFrameInd(201,get_pc_addr,get_frame);
         { if the new dimension is 0, we've to release all data }
         fpc_dynarray_clear(p,pti);
         exit;
       end;

     { skip kind and name }
     ti:=aligntoqword(Pointer(pti)+2+PByte(pti)[1]);

     elesize:=pdynarraytypedata(ti)^.elSize;
     { only set if type needs finalization }
     eletypemngd:=pdynarraytypedata(ti)^.elType;
     if assigned(eletypemngd) then
       eletypemngd:=PPointer(eletypemngd)^;

     { determine new memory size, throw a runtime error on overflow }
{$push} {$q+,r+}
     size:=elesize*newlen+sizeof(tdynarray);
{$pop}

     if assigned(p) then
       begin
          oldlen:=pdynarray(p-sizeof(tdynarray))^.high+1;
          if pdynarray(p-sizeof(tdynarray))^.refcount<>1 then
            begin
               newp:=getmem(size);
               { make an unique copy }
               movelen:=oldlen;
               if newlen<movelen then
                 movelen:=newlen;
               move(p^,(pointer(newp)+sizeof(tdynarray))^,elesize*movelen);
               { increment ref. count of managed members }
               if assigned(eletypemngd) then
                 int_AddRefArray(pointer(newp)+sizeof(tdynarray),eletypemngd,movelen);

               { a declock(ref. count) isn't enough here }
               { it could be that the in MT environments }
               { in the mean time the refcount was       }
               { decremented                             }

               { it is, because it doesn't really matter }
               { if the array is now removed             }
               fpc_dynarray_clear(p,pti);
            end
          else
            begin
               { Finalize if shrinking. }
               if assigned(eletypemngd) and (newlen<oldlen) then
                 int_finalizearray(p+elesize*newlen,eletypemngd,oldlen-newlen);
               realp:=p-sizeof(tdynarray);
               newp:=reallocmem(realp,size);
            end;
       end
     else
       begin
          oldlen:=0;
          newp:=AllocMem(size);
       end;
     if newlen>oldlen then
       begin
          { Initialize new items. }
          if oldlen<>0 then { Skip if AllocMem was used. CAREFUL: Assigned(p) won’t work because of fpc_dynarray_clear above. }
            fillchar((pointer(newp)+sizeof(tdynarray)+elesize*oldlen)^,(newlen-oldlen)*elesize,0);
          if assigned(eletypemngd) and (PTypeKind(eletypemngd)^ in [tkRecord, tkObject, tkArray]) and RTTIManagementAndSize(eletypemngd, rotInitialize, _size, true) then
            int_InitializeArray(pointer(newp)+sizeof(tdynarray)+elesize*oldlen,eletypemngd,newlen-oldlen);
       end;
     p:=pointer(newp)+sizeof(tdynarray);
     newp^.refcount:=1;
     newp^.high:=newlen-1;
     { handle nested arrays }
     if dimcount>1 then
       begin
          for i:=0 to newlen-1 do
            int_dynarray_setlength(pointer((pointer(newp)+sizeof(tdynarray)+i*elesize)^),
              pdynarraytypedata(ti)^.elType2^,dimcount-1,@dims[1]);
       end;
  end;


{ provide local access to array_to_dynarray_copy }
function int_array_to_dynarray_copy(psrc : pointer;ti : pointer;
    lowidx,count,maxcount:tdynarrayindex;
    elesize : sizeint;
    eletype : pointer
    ) : fpc_stub_dynarray;[external name 'FPC_ARR_TO_DYNARR_COPY'];


{$if defined(VER3_2)}
function fpc_dynarray_copy(psrc : pointer;ti : pointer;
    lowidx,count:tdynarrayindex) : fpc_stub_dynarray;[Public,Alias:'FPC_DYNARR_COPY'];compilerproc;
  var
    realpsrc : pdynarray;
    eletype,tti : pointer;
    elesize : sizeint;
  begin
     fpc_dynarray_clear(pointer(result),ti);
     if psrc=nil then
       exit;
     realpsrc:=pdynarray(psrc-sizeof(tdynarray));

     tti:=aligntoqword(ti+2+PByte(ti)[1]);
     elesize:=pdynarraytypedata(tti)^.elSize;
     { only set if type needs finalization }
     if assigned(pdynarraytypedata(tti)^.elType) then
       eletype:=pdynarraytypedata(tti)^.elType^
     else
       eletype:=nil;

     fpc_array_to_dynarray_copy(psrc,ti,lowidx,count,realpsrc^.high+1,elesize,eletype);
  end;
{$endif VER3_2}

{ copy a custom array (open/dynamic/static) to dynamic array }
function fpc_array_to_dynarray_copy(psrc : pointer;ti : pointer;
    lowidx,count,maxcount:tdynarrayindex;
    elesize : sizeint;
    eletype : pointer
    ) : fpc_stub_dynarray;[Public,Alias:'FPC_ARR_TO_DYNARR_COPY'];compilerproc;
  var
    size : sizeint;
  begin
     fpc_dynarray_clear(pointer(result),ti);
     if psrc=nil then
       exit;

{$ifndef FPC_DYNARRAYCOPY_FIXED}
     if (lowidx=-1) and (count=-1) then
       begin
         lowidx:=0;
         count:=high(tdynarrayindex);
       end;
{$endif FPC_DYNARRAYCOPY_FIXED}
     if (lowidx<0) then
       begin
       { Decrease count if index is negative, this is different from how copy()
         works on strings. Checked against D7. }
         if count<=0 then
           exit;              { may overflow when adding lowidx }
         count:=count+lowidx;
         lowidx:=0;
       end;
     if (count>maxcount-lowidx) then
       count:=maxcount-lowidx;
     if count<=0 then
       exit;

     { create new array }
     size:=elesize*count;
     getmem(pointer(result),size+sizeof(tdynarray));
     pdynarray(result)^.refcount:=1;
     pdynarray(result)^.high:=count-1;
     inc(pointer(result),sizeof(tdynarray));
     { copy data }
     move(pointer(psrc+elesize*lowidx)^,pointer(result)^,size);

     { increment ref. count of members? }
     if assigned(eletype) then
       int_AddRefArray(pointer(result),eletype,count);
  end;


procedure fpc_dynarray_delete(var p : pointer;source,count : SizeInt;pti : pointer);
   var
      newlen : tdynarrayindex;
      elesize : sizeint;
      { oldp is the same as p, actual header is accessed as oldp[-1].
        newp fairly points to the new header, array data starts at newp[1].
        realp takes the hit of being a var-parameter to ReallocMem not eligible for living in a register. }
      oldp,newp,realp : pdynarray;
      ti,eletypemngd : pointer;

   begin
     oldp:=p;
     if not assigned(oldp) or (count<=0) then
       exit;
     newlen:=oldp[-1].high+1;
     { Checks source < 0 or source >= len, using the fact that len is never negative. }
     if SizeUint(source)>=SizeUint(newlen) then
       exit;
     { cap count, and maybe delete whole array }
     if count>=newlen-source then
       begin
         if source=0 then
           begin
             fpc_dynarray_clear(p,pti);
             exit;
           end;
         count:=newlen-source;
       end;

     { skip kind and name }
     ti:=aligntoqword(Pointer(pti)+2+PByte(pti)[1]);

     elesize:=pdynarraytypedata(ti)^.elSize;
     { only set if type needs finalization }
     eletypemngd:=pdynarraytypedata(ti)^.elType;
     if assigned(eletypemngd) then
       eletypemngd:=PPointer(eletypemngd)^;

     newlen:=newlen-count;

     if oldp[-1].refcount<>1 then
       begin
          { make an unique copy }
          newp:=getmem(elesize*newlen+sizeof(tdynarray));
          newp^.refcount:=1;
          { copy the elements that we still need }
          move(oldp^,pointer(newp+1)^,source*elesize);
          move((pointer(oldp)+(source+count)*elesize)^,(pointer(newp+1)+source*elesize)^,(newlen-source)*elesize);

          { increment ref. count of managed members }
          if assigned(eletypemngd) then
            int_AddRefArray(newp+1,eletypemngd,newlen);

          { a declock(ref. count) isn't enough here }
          { it could be that the in MT environments  }
          { in the mean time the refcount was       }
          { decremented                             }

          { it is, because it doesn't really matter }
          { if the array is now removed             }
          fpc_dynarray_clear(p,pti);
        end
      else
        begin
          { finalize the elements that will be removed }
          if assigned(eletypemngd) then
            int_FinalizeArray(pointer(oldp)+source*elesize,eletypemngd,count);

          { close the gap by moving the trailing elements to the front }
          move((pointer(oldp)+(source+count)*elesize)^,(pointer(oldp)+source*elesize)^,(newlen-source)*elesize);

          { resize the array }
          realp:=oldp-1;
          newp:=reallocmem(realp,elesize*newlen+sizeof(tdynarray));
        end;
    newp^.high:=newlen-1;
    p:=newp+1;
  end;


procedure fpc_dynarray_insert(var p : pointer;source : SizeInt;data : pointer;count : SizeInt;pti : pointer);compilerproc;
  var
    newlen : tdynarrayindex;
    elesize,dataofs : sizeint;
    oldp,newp,realp : pdynarray;
    ti,eletypemngd : pointer;
  begin
    if count=0 then
      exit;

    oldp:=p;
    if assigned(oldp) then
      begin
        dec(oldp);
        { cap insert index }
        newlen:=oldp^.high+1;
        if SizeUint(source)>SizeUint(newlen) then { Checks for not (0 <= source <= len), using the fact than 'newlen' is never negative. }
          if source<0 then
            source:=0
          else
            source:=newlen;
        newlen:=newlen+count;
      end
    else
      begin
        source:=0;
        newlen:=count;
      end;

    { skip kind and name }
    ti:=aligntoqword(Pointer(pti)+2+PByte(pti)[1]);

    elesize:=pdynarraytypedata(ti)^.elSize;
    { only set if type needs initialization }
    eletypemngd:=pdynarraytypedata(ti)^.elType;
    if assigned(eletypemngd) then
      eletypemngd:=PPointer(eletypemngd)^;

    if not assigned(oldp) or (oldp^.refcount<>1) then
      begin
        newp:=getmem(elesize*newlen+sizeof(tdynarray));

        { copy leading elements. No-op when not Assigned(oldp) because in this case source = 0. }
        move(oldp[1],newp[1],source*elesize);
        { insert new elements }
        move(data^,(pointer(newp+1)+source*elesize)^,count*elesize);
        { copy trailing elements. This time must be careful with not Assigned(oldp). }
        if assigned(oldp) then
          move((pointer(oldp+1)+source*elesize)^,(pointer(newp+1)+(source+count)*elesize)^,(oldp^.high-source+1)*elesize);

        { increment ref. count of managed members }
        if assigned(eletypemngd) then
          int_AddRefArray(newp+1,eletypemngd,newlen);

        { a declock(ref. count) isn't enough here }
        { it could be that the in MT environments }
        { in the mean time the refcount was       }
        { decremented                             }

        { it is, because it doesn't really matter }
        { if the array is now removed             }
        fpc_dynarray_clear(p,pti);
      end
    else
      begin
        { dataofs >= 0 means that 'data' points into the source array with byte offset 'dataofs' from the header.
          dataofs < 0 means that 'data' does not point into the array. }
        dataofs:=-1;
        if (data>=oldp) and (data<=pointer(oldp+1)+oldp^.high*elesize) then
          dataofs:=data-pointer(oldp);

        { resize the array }
        realp:=oldp; { 'realp' as a 'var'-parameter avoids taking 'oldp' address. }
        newp:=reallocmem(realp,elesize*newlen+sizeof(tdynarray));

        { Fixup overlapping 'data'. }
        if dataofs>=0 then
          begin
            data:=pointer(newp)+dataofs;
            { If 'data' points into the trailing part, account for it being moved by 'count'. }
            if data>=pointer(newp+1)+source*elesize then
              data:=data+count*elesize;
          end;

        { move the trailing part after the inserted data }
        move((pointer(newp+1)+source*elesize)^,(pointer(newp+1)+(source+count)*elesize)^,(newp^.high-source+1)*elesize);

        { move the inserted data to the destination }
        move(data^,(pointer(newp+1)+source*elesize)^,count*elesize);

        { increase reference counts of inserted elements }
        if assigned(eletypemngd) then
          int_AddRefArray(pointer(newp+1)+source*elesize,eletypemngd,count);
      end;

    newp^.refcount:=1;
    newp^.high:=newlen-1;
    p:=newp+1;
  end;


procedure fpc_dynarray_concat_multi(var dest : pointer; pti: pointer; const sarr:array of pointer); compilerproc;
  var
    i,firstnonempty,elesize,totallen,copybytes,newdestdatapos : sizeint;
    newp,realp,copysrc,olddestp : pdynarray;
    ti,eletypemngd,copydest : pointer;
  begin
    totallen:=0;
    for i:=high(sarr) downto 0 do
      if assigned(sarr[i]) then
        begin
          inc(totallen,pdynarray(sarr[i])[-1].high+1);
          firstnonempty:=i; { 1) allows for append optimization to work even with some prepended []s, 2) required for the reuse optimization. }
        end;

    if totallen=0 then
      begin
        fpc_dynarray_clear(dest,pti);
        exit;
      end;

    { Reuse the only nonempty input? }
    if totallen=pdynarray(sarr[firstnonempty])[-1].high+1 then
      begin
        fpc_dynarray_assign(dest,sarr[firstnonempty],pti);
        exit;
      end;

    { skip kind and name }
    ti:=aligntoqword(Pointer(pti)+2+PByte(pti)[1]);

    elesize:=pdynarraytypedata(ti)^.elSize;
    { only set if type needs initialization }
    eletypemngd:=pdynarraytypedata(ti)^.elType;
    if Assigned(eletypemngd) then
      eletypemngd:=PPointer(eletypemngd)^;

    olddestp:=dest;
    { Reallocate when possible; in the hope this will reuse the chunk more often than do a redundant copy. }
    if assigned(olddestp) and (olddestp[-1].refcount=1) then
      begin
        if assigned(eletypemngd) then
          begin
            { Find dest occurence among inputs. If found, reuse: remember its position, don't finalize now and don't AddRef later. }
            newdestdatapos:=0;
            for i:=0 to high(sarr) do
              if assigned(sarr[i]) then
                if sarr[i]<>olddestp then
                  inc(newdestdatapos,pdynarray(sarr[i])[-1].high+1)
                else
                  break;
            if newdestdatapos=totallen then { Dest doesn't occur among inputs. }
              int_FinalizeArray(olddestp,eletypemngd,olddestp[-1].high+1);
          end;
        realp:=olddestp-1;
        newp:=reallocmem(realp,totallen*elesize+sizeof(tdynarray));
        { First array can be skipped if appending. }
        if olddestp=sarr[firstnonempty] then
          inc(firstnonempty);
      end
    else
      begin
        olddestp:=nil; { Realloc case is distinguished later as assigned(olddestp). }
        { allocate new array }
        newp:=getmem(totallen*elesize+sizeof(tdynarray));
        newp^.refcount:=1;
      end;

    { Copy arrays from last to the first, so that possible occurences of Dest could read from the beginning of the reallocated Dest. }
    copydest:=pointer(newp+1)+totallen*elesize;
    for i:=high(sarr) downto firstnonempty do
      begin
        copysrc:=sarr[i];
        if not assigned(copysrc) then
          continue;
        if copysrc=olddestp then
          { Dest used as one of the pieces! Use new pointer instead. Array header still conveniently contains original 'high'.
            Can trigger only in the ReallocMem case, as otherwise olddestp = nil. }
          copysrc:=newp+1;
        copybytes:=(copysrc[-1].high+1)*elesize;
        dec(copydest,copybytes);
        move(copysrc^,copydest^,copybytes);
      end;

    if assigned(eletypemngd) then
      begin
        { AddRef everything in GetMem case or if Dest data was not reused in the ReallocMem case. }
        if not assigned(olddestp) or (newdestdatapos=totallen) then
          int_AddRefArray(pointer(newp+1),eletypemngd,totallen)
        else
          begin
            { Dest := A + Dest + B, Dest data reused. }
            if newdestdatapos>0 then
              { AddRef A. Since Dest := Dest + B is a common case, shortcut if nothing to do. }
              int_AddRefArray(newp+1,eletypemngd,newdestdatapos);
            { AddRef B. }
            int_AddRefArray(pointer(newp+1)+(newdestdatapos+newp^.high+1)*elesize,eletypemngd,totallen-(newdestdatapos+newp^.high+1));
          end;
      end;

    if not assigned(olddestp) then
      { clear at the end, dest could be a reference to an array being used also as source }
      fpc_dynarray_clear(dest,pti);

    newp^.high:=totallen-1;
    dest:=newp+1;
  end;


procedure fpc_dynarray_concat(var dest : pointer; pti: pointer; const src1,src2 : pointer); compilerproc;
  var
    totallen,elesize,ofs2 : sizeint;
    newp,realp,olddestp,copysrc : pdynarray;
    ti,eletypemngd : pointer;
  begin
    if not assigned(src1) or not assigned(src2) then
      begin
        fpc_dynarray_assign(dest, pointer(ptruint(src1) or ptruint(src2)), pti);
        exit; { From now on, both src1 and src2 are non-nil. }
      end;
    totallen:=pdynarray(src1)[-1].high+pdynarray(src2)[-1].high+2;

    { skip kind and name }
    ti:=aligntoqword(Pointer(pti)+2+PByte(pti)[1]);

    elesize:=pdynarraytypedata(ti)^.elSize;
    { only set if type needs initialization }
    eletypemngd:=pdynarraytypedata(ti)^.elType;
    if assigned(eletypemngd) then
      eletypemngd:=PPointer(eletypemngd)^;
    ofs2:=(pdynarray(src1)[-1].high+1)*elesize; { Offset of src2 data in the resulting array. }

    olddestp:=dest;
    { Reallocate when possible; in the hope this will reuse the chunk more often than do a redundant copy. }
    if assigned(olddestp) and (olddestp[-1].refcount=1) then
      begin
        { Finalize old dest contents, if they aren't going to be reused. }
        if assigned(eletypemngd) and (olddestp<>src1) and (olddestp<>src2) then
          int_FinalizeArray(olddestp,eletypemngd,olddestp[-1].high+1);

        realp:=olddestp-1;
        newp:=reallocmem(realp,totallen*elesize+sizeof(tdynarray));
        { Copy src2 first, as in the case of olddestp = src2 it must be copied first and in other cases the order does not matter. }
        copysrc:=src2;
        if copysrc=olddestp then
          { Use new pointer instead. Array header still conveniently contains original 'high'. }
          copysrc:=newp+1;
        move(copysrc^,(pointer(newp+1)+ofs2)^,(copysrc[-1].high+1)*elesize);
        if olddestp<>src1 then { Not an append, need to copy src1? }
          move(src1^,newp[1],(pdynarray(src1)[-1].high+1)*elesize);

        { AddRef new data. }
        if assigned(eletypemngd) then
          if src1=olddestp then
            { Dest data stayed in the same position; AddRef only copied src2. }
            int_AddRefArray(pointer(newp+1)+ofs2,eletypemngd,copysrc[-1].high+1)
          else if src2=olddestp then
            { Dest data was moved as if it was src2; AddRef only copied src1. }
            int_AddRefArray(newp+1,eletypemngd,pdynarray(src1)[-1].high+1)
          else
            { Dest data was not used, AddRef everything. }
            int_AddRefArray(newp+1,eletypemngd,totallen);
      end
    else
      begin
        { allocate new array }
        newp:=getmem(totallen*elesize+sizeof(tdynarray));
        newp^.refcount:=1;
        move(src1^,newp[1],ofs2);
        move(src2^,(pointer(newp+1)+ofs2)^,(pdynarray(src2)[-1].high+1)*elesize);

        { increase reference counts of all the elements }
        if assigned(eletypemngd) then
          int_AddRefArray(newp+1,eletypemngd,totallen);

        { clear at the end, dest could be a reference to an array being also source }
        fpc_dynarray_clear(dest,pti);
      end;
    newp^.high:=totallen-1;
    dest:=newp+1;
  end;

procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: SizeInt; lengthVec: PSizeInt);
  external name 'FPC_DYNARR_SETLENGTH';

function DynArraySize(a : pointer): tdynarrayindex;
  external name 'FPC_DYNARRAY_LENGTH';

procedure DynArrayClear(var a: Pointer; typeInfo: Pointer);
  external name 'FPC_DYNARRAY_CLEAR';

procedure DynArrayAssign(var dest: Pointer; src: Pointer; typeInfo: pointer);
  external name 'FPC_DYNARRAY_ASSIGN';

function DynArrayDim(typeInfo: Pointer): Integer;
  begin
    result:=0;
    while (typeInfo <> nil) and (pdynarraytypeinfo(typeInfo)^.kind = tkDynArray) do
    begin
      { skip kind and name }
      typeInfo:=aligntoqword(typeInfo+2+PByte(typeInfo)[1]);

      { element type info}
      typeInfo:=pdynarraytypedata(typeInfo)^.elType2^;

      Inc(result);
    end;
  end;

function DynArrayBounds(a: Pointer; typeInfo: Pointer): TBoundArray;
  var
    i,dim: sizeint;
  begin
    dim:=DynArrayDim(typeInfo);
    SetLength(result, dim);

    for i:=0 to pred(dim) do
      if a = nil then
        exit
      else
      begin
        result[i]:=DynArraySize(a)-1;
        a:=PPointerArray(a)^[0];
      end;
  end;

function IsDynArrayRectangular(a: Pointer; typeInfo: Pointer): Boolean;
  var
    i,j: sizeint;
    dim,count: sizeint;
  begin
    dim:=DynArrayDim(typeInfo);
    for i:=1 to pred(dim) do
    begin
      count:=DynArraySize(PPointerArray(a)^[0]);

      for j:=1 to Pred(DynArraySize(a)) do
        if count<>DynArraySize(PPointerArray(a)^[j]) then
          exit(false);

      a:=PPointerArray(a)^[0];
    end;
    result:=true;
  end;

function DynArrayIndex(a: Pointer; const indices: array of SizeInt; typeInfo: Pointer): Pointer;
  var
    i,h: sizeint;
  begin
    h:=High(indices);
    for i:=0 to h do
    begin
      { skip kind and name }
      typeInfo:=aligntoqword(Pointer(typeInfo)+2+PByte(typeInfo)[1]);
      if i=h then
        break;
      a := PPointerArray(a)^[indices[i]];
      { element type info}
      typeInfo:=pdynarraytypedata(typeInfo)^.elType2^;
    end;
    result:=a+SizeUint(indices[h])*pdynarraytypedata(typeInfo)^.elSize;
  end;

{ obsolete but needed for bootstrapping }
procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer); [Public,Alias:'FPC_DYNARRAY_DECR_REF']; compilerproc;
  begin
    fpc_dynarray_clear(p,ti);
  end;

