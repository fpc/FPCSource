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
      verbose,systems,globals,
      aasmbase,aasmtai,
      cgbase,pass_2,
      ncon,ncal,ncnv,
      cpubase,
      cgobj,ncgutil;


    procedure tx86typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
        href      : treference;
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
         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE :
              begin
{$ifndef cpu64bit}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   location_release(exprasmlist,left.location);
                   hregister:=cg.getintregister(exprasmlist,OS_INT);
                   cg.a_load_ref_reg(exprasmlist,OS_32,OS_32,left.location.reference,hregister);
                   href:=left.location.reference;
                   inc(href.offset,4);
                   cg.ungetregister(exprasmlist,hregister);
                   cg.a_op_ref_reg(exprasmlist,OP_OR,OS_32,href,hregister);
                 end
                else
{$endif cpu64bit}
                 begin
                   location_force_reg(exprasmlist,left.location,left.location.size,true);
                   location_release(exprasmlist,left.location);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,left.location.size,left.location.register,left.location.register);
                 end;
              end;
            LOC_FLAGS :
              begin
                resflags:=left.location.resflags;
              end;
            LOC_REGISTER,LOC_CREGISTER :
              begin
{$ifndef cpu64bit}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=cg.getintregister(exprasmlist,OS_32);
                   cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,left.location.registerlow,hregister);
                   cg.ungetregister(exprasmlist,hregister);
                   location_release(exprasmlist,left.location);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,OS_32,left.location.registerhigh,hregister);
                 end
                else
{$endif cpu64bit}
                 begin
                   location_release(exprasmlist,left.location);
                   cg.a_op_reg_reg(exprasmlist,OP_OR,left.location.size,left.location.register,left.location.register);
                 end;
              end;
            LOC_JUMP :
              begin
                hregister:=cg.getintregister(exprasmlist,OS_INT);
                objectlibrary.getlabel(hlabel);
                cg.a_label(exprasmlist,truelabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,1,hregister);
                cg.a_jmp_always(exprasmlist,hlabel);
                cg.a_label(exprasmlist,falselabel);
                cg.a_load_const_reg(exprasmlist,OS_INT,0,hregister);
                cg.a_label(exprasmlist,hlabel);
                cg.ungetregister(exprasmlist,hregister);
                cg.a_op_reg_reg(exprasmlist,OP_OR,OS_INT,hregister,hregister);
              end;
            else
              internalerror(10062);
         end;
         { load flags to register }
         location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
         location.register:=cg.getintregister(exprasmlist,location.size);
         cg.g_flags2reg(exprasmlist,location.size,resflags,location.register);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
       end;

end.
{
  $Log$
  Revision 1.7  2003-10-10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.6  2003/10/09 21:31:38  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.5  2003/10/01 20:34:51  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.4  2003/09/28 21:48:57  peter
    * fix register leak

  Revision 1.3  2003/09/03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.2.2.1  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.2  2003/05/22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.1  2003/05/01 08:02:42  florian
    * i386 and x86-64 share second_int_to_bool, moved to nx86cnv.pas
}
