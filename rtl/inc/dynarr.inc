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
   tdynarray = packed record
      refcount : ptrint;
      high : tdynarrayindex;
   end;


procedure fpc_dynarray_rangecheck(p : pointer;i : tdynarrayindex);[Public,Alias:'FPC_DYNARRAY_RANGECHECK']; compilerproc;
  begin
     if not(assigned(p)) or (i<0) or (i>pdynarray(p-sizeof(tdynarray))^.high) then
       HandleErrorFrame(201,get_frame);
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


{ releases and finalizes the data of a dyn. array and sets p to nil }
procedure fpc_dynarray_clear_internal(p : pointer;ti : pointer);
  var
    elesize : sizeint;
    eletype : pdynarraytypeinfo;
  begin
     if p=nil then
       exit;

     { skip kind and name }
     inc(pointer(ti),ord(pdynarraytypeinfo(ti)^.namelen)+2);

     ti:=aligntoptr(ti);

     elesize:=psizeint(ti)^;
     eletype:=pdynarraytypeinfo(pointer(pdynarraytypeinfo(pointer(ti)+sizeof(sizeint)))^);

     { finalize all data }
     int_finalizearray(p+sizeof(tdynarray),eletype,pdynarray(p)^.high+1,
                       elesize);

     { release the data }
     freemem(p);
  end;


procedure fpc_dynarray_clear(var p : pointer;ti : pointer); [Public,Alias:'FPC_DYNARRAY_CLEAR']; compilerproc;
  var
     realp : pdynarray;
  begin
    if (P=Nil) then
      exit;
    realp:=pdynarray(p-sizeof(tdynarray));
    if declocked(realp^.refcount) then
      fpc_dynarray_clear_internal(p-sizeof(tdynarray),ti);
    p:=nil;
  end;

{ alias for internal use }
Procedure fpc_dynarray_clear (var p : pointer;ti : pointer);[external name 'FPC_DYNARRAY_CLEAR'];


procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer); [Public,Alias:'FPC_DYNARRAY_DECR_REF']; compilerproc;
  var
     realp : pdynarray;
  begin
     if p=nil then
       exit;

     realp:=pdynarray(p-sizeof(tdynarray));
     if realp^.refcount=0 then
       HandleErrorFrame(204,get_frame);

     { decr. ref. count }
     { should we remove the array? }
     if declocked(realp^.refcount) then
       fpc_dynarray_clear_internal(realp,pdynarraytypeinfo(ti));
     p := nil;
  end;

{ provide local access to dynarr_decr_ref for dynarr_setlength }
procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer); [external name 'FPC_DYNARRAY_DECR_REF'];

procedure fpc_dynarray_incr_ref(p : pointer);[Public,Alias:'FPC_DYNARRAY_INCR_REF']; compilerproc;
  var
     realp : pdynarray;
  begin
     if p=nil then
       exit;

     realp:=pdynarray(p-sizeof(tdynarray));
     if realp^.refcount=0 then
       HandleErrorFrame(204,get_frame);

     inclocked(realp^.refcount);
  end;

{ provide local access to dynarr_decr_ref for dynarr_setlength }
procedure fpc_dynarray_incr_ref(p : pointer); [external name 'FPC_DYNARRAY_INCR_REF'];


{ provide local access to dynarr_setlength }
procedure int_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : dword;dims : pdynarrayindex);[external name 'FPC_DYNARR_SETLENGTH'];

procedure fpc_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : dword;dims : pdynarrayindex);[Public,Alias:'FPC_DYNARR_SETLENGTH']; compilerproc;

  var
     i : tdynarrayindex;
     movelen,
     size : sizeint;
     { contains the "fixed" pointers where the refcount }
     { and high are at positive offsets                 }
     realp,newp : pdynarray;
     ti : pdynarraytypeinfo;
     updatep: boolean;
     elesize : sizeint;
     eletype : pdynarraytypeinfo;

  begin
     ti:=pdynarraytypeinfo(pti);

     { skip kind and name }
     inc(pointer(ti),ord(pdynarraytypeinfo(ti)^.namelen)+2);

     ti:=aligntoptr(ti);

     elesize:=psizeint(ti)^;
     eletype:=pdynarraytypeinfo(pointer(pdynarraytypeinfo(pointer(ti)+sizeof(sizeint)))^);

     { determine new memory size }
     { dims[dimcount-1] because the dimensions are in reverse order! (JM) }
     size:=elesize*dims[dimcount-1]+sizeof(tdynarray);
     updatep := false;

     { not assigned yet? }
     if not(assigned(p)) then
       begin
          if dims[dimcount-1]<0 then
            HandleErrorFrame(201,get_frame);
          { do we have to allocate memory? }
          if dims[dimcount-1] = 0 then
            exit;
          getmem(newp,size);
          fillchar(newp^,size,0);
          updatep := true;
       end
     else
       begin
          realp:=pdynarray(p-sizeof(tdynarray));
          newp := realp;

          { if the new dimension is 0, we've to release all data }
          if dims[dimcount-1]<=0 then
            begin
               if dims[dimcount-1]<0 then
                 HandleErrorFrame(201,get_frame);
               if declocked(realp^.refcount) then
                 fpc_dynarray_clear_internal(realp,pdynarraytypeinfo(pti));
               p:=nil;
               exit;
            end;

          if realp^.refcount<>1 then
            begin
               updatep := true;
               { make an unique copy }
               getmem(newp,size);
               fillchar(newp^,size,0);
               if realp^.high < dims[dimcount-1] then
                 movelen := realp^.high+1
               else
                 movelen := dims[dimcount-1];
               move(p^,(pointer(newp)+sizeof(tdynarray))^,elesize*movelen);

               { increment ref. count of members }
               for i:= 0 to movelen-1 do
                 int_addref(pointer(newp)+sizeof(tdynarray)+elesize*i,eletype);

               { a declock(ref. count) isn't enough here }
               { it could be that the in MT enviroments  }
               { in the mean time the refcount was       }
               { decremented                             }

               { it is, because it doesn't really matter }
               { if the array is now removed             }
               { fpc_dynarray_decr_ref(p,ti); }
               if declocked(realp^.refcount) then
                 fpc_dynarray_clear_internal(realp,pdynarraytypeinfo(ti));
            end
          else if dims[dimcount-1]<>realp^.high+1 then
            begin
               { range checking is quite difficult ...  }
               { if size overflows then it is less than }
               { the values it was calculated from      }
               if (size<sizeof(tdynarray)) or
                 ((elesize>0) and (size<elesize)) then
                 HandleErrorFrame(201,get_frame);

               { resize? }
               { here, realp^.refcount has to be one, otherwise the previous }
               { if-statement would have been taken. Or is this also for MT  }
               { code? (JM)                                                  }
               if realp^.refcount=1 then
                 begin
                    { shrink the array? }
                    if dims[dimcount-1]<realp^.high+1 then
                      begin
                          int_finalizearray(pointer(realp)+sizeof(tdynarray)+
                            elesize*dims[dimcount-1],
                            eletype,realp^.high-dims[dimcount-1]+1,elesize);
                         reallocmem(realp,size);
                      end
                    else if dims[dimcount-1]>realp^.high+1 then
                      begin
                         reallocmem(realp,size);
                         fillchar((pointer(realp)+sizeof(tdynarray)+elesize*(realp^.high+1))^,
                           (dims[dimcount-1]-realp^.high-1)*elesize,0);
                      end;
                    newp := realp;
                    updatep := true;
                 end;
            end;
       end;
    { handle nested arrays }
    if dimcount>1 then
      begin
         for i:=0 to dims[dimcount-1]-1 do
           int_dynarray_setlength(pointer((pointer(newp)+sizeof(tdynarray)+i*elesize)^),
             eletype,dimcount-1,dims);
      end;
     if updatep then
       begin
         p:=pointer(newp)+sizeof(tdynarray);
         newp^.refcount:=1;
         newp^.high:=dims[dimcount-1]-1;
       end;
  end;


{ provide local access to dynarr_copy }
function int_dynarray_copy(psrc : pointer;ti : pointer;
    lowidx,count:tdynarrayindex) : pointer;[external name 'FPC_DYNARR_COPY'];

function fpc_dynarray_copy(psrc : pointer;ti : pointer;
    lowidx,count:tdynarrayindex) : pointer;[Public,Alias:'FPC_DYNARR_COPY'];compilerproc;
  var
    realpdest,
    realpsrc : pdynarray;
    cnt,
    i,size : longint;
    highidx : tdynarrayindex;
    elesize : sizeint;
    eletype : pdynarraytypeinfo;
    pdest : pointer;
  begin
     highidx:=lowidx+count-1;
     pdest:=nil;
     result:=pdest;
     if psrc=nil then
       exit;
     realpsrc:=pdynarray(psrc-sizeof(tdynarray));
     { skip kind and name }
     inc(pointer(ti),ord(pdynarraytypeinfo(ti)^.namelen)+2);

     ti:=aligntoptr(ti);

     elesize:=psizeint(ti)^;
     eletype:=pdynarraytypeinfo(pointer(pdynarraytypeinfo(pointer(ti)+sizeof(sizeint)))^);

     { -1, -1 (highidx=lowidx-1-1=-3) is used to copy the whole array like a:=copy(b);, so
       update the lowidx and highidx with the values from psrc }
     if (lowidx=-1) and (highidx=-3) then
      begin
        lowidx:=0;
        highidx:=realpsrc^.high;
      end;
     { get number of elements and check for invalid values }
     if (lowidx<0) or (highidx<0) or (lowidx > realpsrc^.high) then
       HandleErrorFrame(201,get_frame);
     cnt:=highidx-lowidx+1;
     if (cnt > realpsrc^.high - lowidx + 1) then
       cnt := realpsrc^.high - lowidx + 1;
     { create new array }
     size:=elesize*cnt;
     getmem(realpdest,size+sizeof(tdynarray));
     pdest:=pointer(realpdest)+sizeof(tdynarray);
     { copy data }
     move(pointer(psrc+elesize*lowidx)^,pdest^,size);
     { fill new refcount }
     realpdest^.refcount:=1;
     realpdest^.high:=cnt-1;
     { increment ref. count of members }
     for i:= 0 to cnt-1 do
       int_addref(pointer(pdest+elesize*i),eletype);
     result:=pdest;
  end;


procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: SizeInt; lengthVec: PSizeInt);
  var
    preallocated : array[0..10] of SizeInt;
    i : SizeInt;
    p : PSizeInt;
  begin
    if dimCnt<=high(preallocated)+1 then
      p:=@preallocated[0]
    else
      getmem(p,sizeof(SizeInt)*dimCnt);
    for i:=0 to dimCnt-1 do
      p[i]:=lengthVec[dimCnt-1-i];
    int_dynarray_setlength(a,typeInfo,dimCnt,p);
    if p<>@preallocated[0] then
      freemem(p);
  end;

