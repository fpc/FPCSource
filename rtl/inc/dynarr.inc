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
   tdynarrayindex = longint;
   pdynarrayindex = ^tdynarrayindex;
   t_size = dword;

   { don't add new fields, the size is used }
   { to calculate memory requirements       }
   tdynarray = record
      refcount : dword;
      high : tdynarrayindex;
   end;

   pdynarray = ^tdynarray;
   pdynarraytypeinfo = packed record
      kind : byte;
      namelen : byte;
      // here the chars follow, we've to skip them
      elesize : t_size;
      eletype : pdynarraytypeinfo;
   end;

function dynarray_length(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_LENGTH'];

  begin
     dynarray_length:=pdynarray(p-sizeof(tdynarray))^.high+1;
  end;

function dynarray_high(p : pointer) : tdynarrayindex;[Public,Alias:'FPC_DYNARRAY_HIGH'];

  begin
//!!!!!!!     dynarray_length:=pdynarray(p-sizeof(tdynarray))^.high;
  end;

procedure dynarray_decr_ref(var p : pointer);[Public,Alias:'FPC_DYNARRAY_DECR_REF'];

  begin
  end;

procedure dynarray_setlength(var p : pointer;ti : pdynarraytypeinfo;
  dimcount : dword;dims : pdynarrayindex);[Public,Alias:'FPC_DYNARRAY_SETLENGTH'];

  var
     i : tdynarrayindex;
     size : t_size;
     { contains the "fixed" pointers where the refcount }
     { and high are at positive offsets                 }
     realp,newp : pdynarray;

  begin
(* !!!!!!
     realp:=pdynarray(p-sizeof(tdynarray));
     if dims[0]<0 then
       HandleErrorFrame(201,get_frame);
     if dims[0]=0 then
       begin
          { release all data }
          !!!!!
          p:=nil;
          exit;
       end;
     if dims[0]<>realp^.high+1 then
       begin
          { determine new memory size }
          size:=ti.elesize*dims[0]+sizeof(tdynarray);

          { range checking is quite difficult ... }
          if (size<sizeof(tdynarray)) or
            ((ti.elesize>0) and (size<ti.elesize)) then
            HandleErrorFrame(201,get_frame);

          { skip kind and name }
          inc(pointer(ti),ord(ti.namelen));

          { resize? }
          if realp.refcount=1 then
            begin
               { shrink the array? }
               if dims[0]<realp^.high+1 then
                 begin
                    for i:=dims[0]-1 to realp^.high do
                       finalize(,ti^.eletype);
                    reallocmem(realp,size);
                 end
               else
                 begin
                    reallocmem(realp,size);
                    !!!!!! fillchar
                 end;
            end
          else
            begin
               { no, copy }
               !!!!!!!
            end;
       end;

     { handle nested arrays }
     if dimcount>1 then
       begin
          for i:=0 to dims[0]-1 do
            dynarray_setlength(newp+sizeof(tdynarray)+i*elesize,
              ti.eletype,dimcount-1,@dims[1]);
       end;
     p:=newp+sizeof(tdynarray);
!!!!!! *)
  end;

function dynarray_copy(var p : pointer;ti : pdynarraytypeinfo;
  dimcount : dword;dims : pdynarrayindex) : pointer;[Public,Alias:'FPC_DYNARRAY_COPY'];

  begin
  end;


{
  $Log$
  Revision 1.2  2000-11-06 21:35:59  peter
    * removed some warnings

  Revision 1.1  2000/11/04 17:52:46  florian
    * fixed linker errors

}
