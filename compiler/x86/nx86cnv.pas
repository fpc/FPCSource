{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate for x86-64 and i386 assembler for type converting nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit nx86cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcnv,defutil,defcmp;

    type
       tx86typeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
         { function first_int_to_real: tnode; override; }
         { procedure second_int_to_real;override; }
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
           procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override;  }
         { procedure second_char_to_char;override; }
       end;


implementation

   uses
      verbose,systems,globtype,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_2,
      ncon,ncal,ncnv,
      cpubase,
      cgobj,cga,tgobj,rgobj,ncgutil;


    procedure tx86typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
        pref      : treference;
        resflags  : tresflags;
        hlabel,oldtruelabel,oldfalselabel : tasmlabel;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);
         if codegenerror then
          exit;
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explicit in flags) and
            (left.resulttype.def.size=resulttype.def.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              truelabel:=oldtruelabel;
              falselabel:=oldfalselabel;
              exit;
           end;

         { Load left node into flag F_NE/F_E }
         resflags:=F_NE;
         location_release(exprasmlist,left.location);
         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE :
              begin
{$ifdef i386}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=rg.getregisterint(exprasmlist,OS_INT);
                   emit_ref_reg(A_MOV,S_L,left.location.reference,hregister);
                   pref:=left.location.reference;
                   inc(pref.offset,4);
                   emit_ref_reg(A_OR,S_L,pref,hregister);
                 end
                else
{$endif i386}
                 begin
                   location_force_reg(exprasmlist,left.location,left.location.size,true);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,left.location.size,left.location.register,left.location.register);
                 end;
              end;
            LOC_FLAGS :
              begin
                resflags:=left.location.resflags;
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
{$ifdef i386}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                 {$ifdef newra}
                   hregister:=rg.getregisterint(exprasmlist,OS_32);
                 {$else}
                   hregister:=cg.get_scratch_reg_int(exprasmlist,OS_32);
                 {$endif}
                   cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,left.location.registerlow,hregister);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,OS_32,left.location.registerhigh,hregister);
                 {$ifdef newra}
                   rg.ungetregisterint(exprasmlist,hregister);
                 {$else}
                   cg.free_scratch_reg(exprasmlist,hregister);
                 {$endif}
                 end
                else
{$endif i386}
                 cg.a_op_reg_reg(exprasmlist,OP_OR,left.location.size,left.location.register,left.location.register);
              end;
            LOC_JUMP :
              begin
                hregister:=rg.getregisterint(exprasmlist,OS_INT);
                objectlibrary.getlabel(hlabel);
                cg.a_label(exprasmlist,truelabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,1,hregister);
                cg.a_jmp_always(exprasmlist,hlabel);
                cg.a_label(exprasmlist,falselabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,0,hregister);
                cg.a_label(exprasmlist,hlabel);
                cg.a_op_reg_reg(exprasmlist,OP_OR,OS_INT,hregister,hregister);
              end;
            else
              internalerror(10062);
         end;
         { load flags to register }
         location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
         location.register:=def_getreg(resulttype.def);
         cg.g_flags2reg(exprasmlist,location.size,resflags,location.register);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
       end;

end.
{
  $Log$
  Revision 1.2  2003-05-22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.1  2003/05/01 08:02:42  florian
    * i386 and x86-64 share second_int_to_bool, moved to nx86cnv.pas
}
