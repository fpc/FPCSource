{
    $Id$
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
      refcount : longint;
      high : tdynarrayindex;
   end;

   pdynarraytypeinfo = ^tdynarraytypeinfo;
   tdynarraytypeinfo = packed record
      kind : byte;
      namelen : byte;
      { here the chars follow, we've to skip them }
      elesize : t_size;
      eletype : pdynarraytypeinfo;
   end;


function fpc_dynarray_length(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}
  begin
     if assigned(p) then
       fpc_dynarray_length:=pdynarray(p-sizeof(tdynarray))^.high+1
     else
       fpc_dynarray_length:=0;
  end;


function fpc_dynarray_high(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_HIGH']; {$ifdef hascompilerproc} compilerproc; {$endif}
  begin
     if assigned(p) then
       fpc_dynarray_high:=pdynarray(p-sizeof(tdynarray))^.high
     else
       fpc_dynarray_high:=-1;
  end;

{ releases and finalizes the data of a dyn. array and sets p to nil }
procedure fpc_dynarray_clear(var p : pointer;ti : pointer); [Public,Alias:'FPC_DYNARRAY_CLEAR']; {$ifdef hascompilerproc} compilerproc; {$endif}
  begin
     if p=nil then
       exit;

     { skip kind and name }
     inc(pointer(ti),ord(pdynarraytypeinfo(ti)^.namelen));

     { finalize all data }
     int_finalizearray(p+sizeof(tdynarray),pdynarraytypeinfo(ti)^.eletype,pdynarray(p)^.high+1,
                       pdynarraytypeinfo(ti)^.elesize);

     { release the data }
     freemem(p,sizeof(tdynarray)+(pdynarray(p)^.high+1)*pdynarraytypeinfo(ti)^.elesize);
     p:=nil;
  end;

{$ifdef hascompilerproc}
{ alias for internal use }
Procedure fpc_dynarray_clear (var p : pointer;ti : pointer);[external name 'FPC_DYNARRAY_CLEAR'];
{$endif hascompilerproc}


procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer);saveregisters;[Public,Alias:'FPC_DYNARRAY_DECR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
       fpc_dynarray_clear(realp,pdynarraytypeinfo(ti));
     p := nil;
  end;

{$ifdef hascompilerproc}
{ provide local access to dynarr_decr_ref for dynarr_setlength }
procedure fpc_dynarray_decr_ref(var p : pointer;ti : pointer);saveregisters; [external name 'FPC_DYNARRAY_DECR_REF'];
{$endif}

procedure fpc_dynarray_incr_ref(p : pointer);saveregisters;[Public,Alias:'FPC_DYNARRAY_INCR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

{$ifdef hascompilerproc}
{ provide local access to dynarr_decr_ref for dynarr_setlength }
procedure fpc_dynarray_incr_ref(p : pointer);saveregisters; [external name 'FPC_DYNARRAY_INCR_REF'];
{$endif}

{ provide local access to dynarr_setlength }
procedure int_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : dword;dims : pdynarrayindex);[external name 'FPC_DYNARR_SETLENGTH'];

procedure fpc_dynarray_setlength(var p : pointer;pti : pointer;
  dimcount : dword;dims : pdynarrayindex);[Public,Alias:'FPC_DYNARR_SETLENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif}

  var
     movelen: cardinal;
     i : tdynarrayindex;
     size : t_size;
     { contains the "fixed" pointers where the refcount }
     { and high are at positive offsets                 }
     realp,newp : pdynarray;
     ti : pdynarraytypeinfo;
     updatep: boolean;

  begin
     ti:=pdynarraytypeinfo(pti);
     { skip kind and name }
     inc(pointer(ti),ord(ti^.namelen));

     { determine new memory size }
     { dims[dimcount-1] because the dimensions are in reverse order! (JM) }
     size:=ti^.elesize*dims[dimcount-1]+sizeof(tdynarray);
     updatep := false;

     { not assigned yet? }
     if not(assigned(p)) then
       begin
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

          if dims[dimcount-1]<0 then
            HandleErrorFrame(201,get_frame);

          { if the new dimension is 0, we've to release all data }
          if dims[dimcount-1]=0 then
            begin
               fpc_dynarray_clear(realp,pdynarraytypeinfo(pti));
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
               move(p^,(pointer(newp)+sizeof(tdynarray))^,ti^.elesize*movelen);

               { increment ref. count of members }
               for i:= 0 to movelen-1 do
                 int_addref(pointer(newp)+sizeof(tdynarray)+ti^.elesize*i,ti^.eletype);

               { a declock(ref. count) isn't enough here }
               { it could be that the in MT enviroments  }
               { in the mean time the refcount was       }
               { decremented                             }

               { it is, because it doesn't really matter }
               { if the array is now removed             }
               { fpc_dynarray_decr_ref(p,ti); }
               if declocked(realp^.refcount) then
                 fpc_dynarray_clear(realp,pdynarraytypeinfo(ti));
            end
          else if dims[dimcount-1]<>realp^.high+1 then
            begin

               { range checking is quite difficult ...  }
               { if size overflows then it is less than }
               { the values it was calculated from      }
               if (size<sizeof(tdynarray)) or
                 ((ti^.elesize>0) and (size<ti^.elesize)) then
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
                            ti^.elesize*dims[dimcount-1],
                            ti^.eletype,realp^.high-dims[dimcount-1]+1,ti^.elesize);
                         reallocmem(realp,size);
                      end
                    else if dims[dimcount-1]>realp^.high+1 then
                      begin
                         reallocmem(realp,size);
                         fillchar((pointer(realp)+sizeof(tdynarray)+ti^.elesize*(realp^.high+1))^,
                           (dims[dimcount-1]-realp^.high-1)*ti^.elesize,0);
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
           int_dynarray_setlength(pointer((pointer(newp)+sizeof(tdynarray)+i*ti^.elesize)^),
             ti^.eletype,dimcount-1,dims);
      end;
     if updatep then
       begin
         p:=pointer(newp)+sizeof(tdynarray);
         newp^.refcount:=1;
         newp^.high:=dims[dimcount-1]-1;
       end;
  end;


function fpc_dynarray_copy(var p : pointer;ti : pointer;
  dimcount : dword;dims : pdynarrayindex) : pointer;[Public,Alias:'FPC_DYNARRAY_COPY']; {$ifdef hascompilerproc} compilerproc; {$endif}

  begin
     { note: ti is of type pdynarrayinfo, but it can't be declared       }
     { that way because this procedure is also declared in the interface }
     { (as compilerproc) and the pdynarraytypeinfo isn't available there }
     {!!!!!!!!!!}
  end;


{
  $Log$
  Revision 1.18  2002-09-07 15:07:45  peter
    * old logs removed and tabs fixed

  Revision 1.17  2002/04/26 15:19:05  peter
    * use saveregisters for incr routines, saves also problems with
      the optimizer

  Revision 1.16  2002/04/25 20:14:56  peter
    * updated compilerprocs
    * incr ref count has now a value argument instead of var

  Revision 1.15  2002/01/21 20:16:08  peter
    * updated for dynarr:=nil

}
