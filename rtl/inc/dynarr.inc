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

procedure FinalizeArray(data,typeinfo : pointer;count,size : longint);forward;
Procedure Addref (Data,TypeInfo : Pointer);forward;
Procedure int_finalize (Data,TypeInfo: Pointer);forward;

type
   tdynarrayindex = longint;
   pdynarrayindex = ^tdynarrayindex;
   t_size = dword;

   { don't add new fields, the size is used }
   { to calculate memory requirements       }
   pdynarray = ^tdynarray;
   tdynarray = packed record
      refcount : dword;
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


function dynarray_length(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_LENGTH'];
  begin
     dynarray_length:=pdynarray(p-sizeof(tdynarray))^.high+1;
  end;


function dynarray_high(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_HIGH'];
  begin
     dynarray_high:=pdynarray(p-sizeof(tdynarray))^.high;
  end;


{ releases and finalizes the data of a dyn. array and sets p to nil }
procedure dynarray_clear(var p : pdynarray;ti : pdynarraytypeinfo);
  begin
     { skip kind and name }
     inc(pointer(ti),ord(ti^.namelen));

     { finalize all data }
     finalizearray(p,ti^.eletype,p^.high+1,ti^.elesize);

     { release the data }
     freemem(p,sizeof(tdynarray)+p^.high+1*ti^.elesize);
     p:=nil;
  end;


procedure dynarray_decr_ref(var p : pointer;ti : pdynarraytypeinfo);[Public,Alias:'FPC_DYNARRAY_DECR_REF'];
  var
     realp : pdynarray;
  begin
     if p=nil then
       exit;

     realp:=pdynarray(p-sizeof(tdynarray));
     if realp^.refcount=0 then
       HandleErrorFrame(204,get_frame);

     { this isn't MT safe! }
     { decr. ref. count }
     declocked(realp^.refcount);

     { should we remove the array? }
     if realp^.refcount=0 then
       dynarray_clear(realp,ti);
     p:=nil;
  end;


procedure dynarray_setlength(var p : pointer;pti : pdynarraytypeinfo;
  dimcount : dword;dims : pdynarrayindex);[Public,Alias:'FPC_DYNARR_SETLENGTH'];

  var
     i : tdynarrayindex;
     size : t_size;
     { contains the "fixed" pointers where the refcount }
     { and high are at positive offsets                 }
     realp,newp : pdynarray;
     ti : pdynarraytypeinfo;

  begin
     ti:=pti;
     { skip kind and name }
     inc(pointer(ti),ord(ti^.namelen));

     { determine new memory size }
     size:=ti^.elesize*dims[0]+sizeof(tdynarray);

     { not assigned yet? }
     if not(assigned(p)) then
       begin
          getmem(newp,size);
          fillchar(newp^,size,0);
       end
     else
       begin
          realp:=pdynarray(p-sizeof(tdynarray));

          if dims[0]<0 then
            HandleErrorFrame(201,get_frame);

          { if the new dimension is 0, we've to release all data }
          if dims[0]=0 then
            begin
               dynarray_clear(realp,pti);
               p:=nil;
               exit;
            end;

          if realp^.refcount<>1 then
            begin
               { make an unique copy }
               getmem(newp,size);
               move(p^,(newp+sizeof(tdynarray))^,ti^.elesize*dims[0]);

               { increment ref. count of members }
               for i:=0 to dims[0]-1 do
                 addref(newp+sizeof(tdynarray)+ti^.elesize*i,ti^.eletype);

               { a declock(ref. count) isn't enough here }
               { it could be that the in MT enviroments  }
               { in the mean time the refcount was       }
               { decremented                             }
               dynarray_decr_ref(p,ti);
            end
          else if dims[0]<>realp^.high+1 then
            begin

               { range checking is quite difficult ...  }
               { if size overflows then it is less than }
               { the values it was calculated from      }
               if (size<sizeof(tdynarray)) or
                 ((ti^.elesize>0) and (size<ti^.elesize)) then
                 HandleErrorFrame(201,get_frame);

               { resize? }
               if realp^.refcount=1 then
                 begin
                    { shrink the array? }
                    if dims[0]<realp^.high+1 then
                      begin
                          finalizearray(realp+sizeof(realp)+ti^.elesize*dims[0],
                            ti^.eletype,realp^.high-dims[0]+1,ti^.elesize);
                         reallocmem(realp,size);
                      end
                    else if dims[0]>realp^.high+1 then
                      begin
                         reallocmem(realp,size);
                         fillchar((realp+sizeof(realp)+ti^.elesize*(realp^.high+1))^,
                           (dims[0]-realp^.high-1)*ti^.elesize,0);
                      end;
                 end;
            end
          else
            newp:=realp;

          { handle nested arrays }
          if dimcount>1 then
            begin
               for i:=0 to dims[0]-1 do
                 dynarray_setlength(pointer(plongint(newp+sizeof(tdynarray))[i*ti^.elesize]),
                   ti^.eletype,dimcount-1,@dims[1]);
            end;
       end;
     p:=newp+sizeof(tdynarray);
     newp^.refcount:=1;
     newp^.high:=dims[0]-1;
  end;

function dynarray_copy(var p : pointer;ti : pdynarraytypeinfo;
  dimcount : dword;dims : pdynarrayindex) : pointer;[Public,Alias:'FPC_DYNARRAY_COPY'];

  begin
     {!!!!!!!!!!}
  end;


{
  $Log$
  Revision 1.4  2000-11-12 23:23:34  florian
    * interfaces basically running

  Revision 1.3  2000/11/07 23:42:21  florian
    + AfterConstruction and BeforeDestruction implemented
    + TInterfacedObject implemented

  Revision 1.2  2000/11/06 21:35:59  peter
    * removed some warnings

  Revision 1.1  2000/11/04 17:52:46  florian
    * fixed linker errors
}
