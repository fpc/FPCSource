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
         function first_real_to_real : tnode;override;
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
         function first_int_to_real: tnode; override;
         procedure second_int_to_real;override;
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
      aasmbase,aasmtai,aasmcpu,
      symconst,symdef,
      cgbase,cga,pass_2,
      ncon,ncal,ncnv,
      cpubase,
      cgutils,cgobj,cgx86,ncgutil,
      tgobj;


    function tx86typeconvnode.first_real_to_real : tnode;
      begin
         first_real_to_real:=nil;
        { comp isn't a floating type }
         if (tfloatdef(resulttype.def).typ=s64comp) and
            (tfloatdef(left.resulttype.def).typ<>s64comp) and
            not (nf_explicit in flags) then
           CGMessage(type_w_convert_real_2_comp);
         if use_sse(resulttype.def) then
           begin
             if registersmm<1 then
               registersmm:=1;
             expectloc:=LOC_MMREGISTER;
           end
         else
           begin
             if registersfpu<1 then
               registersfpu:=1;
             expectloc:=LOC_FPUREGISTER;
           end;
      end;


    procedure tx86typeconvnode.second_int_to_bool;
      var
        hregister : tregister;
{$ifndef cpu64bit}
        href      : treference;
{$endif cpu64bit}
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

    function tx86typeconvnode.first_int_to_real : tnode;

      begin
        first_int_to_real:=nil;
         if registersfpu<1 then
          registersfpu:=1;
        expectloc:=LOC_FPUREGISTER;
      end;


    procedure tx86typeconvnode.second_int_to_real;

      var
         href : treference;
         hregister : tregister;
         l1,l2 : tasmlabel;
         signtested : boolean;
      begin
      {
        if use_sse(resulttype.def) then
          begin

          end
        else
      }
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
            if (left.location.loc=LOC_REGISTER) and (torddef(left.resulttype.def).typ=u64bit) then
              begin
{$ifdef cpu64bit}
                emit_const_reg(A_BT,S_Q,63,left.location.register);
{$else cpu64bit}
                emit_const_reg(A_BT,S_L,31,left.location.register64.reghi);
{$endif cpu64bit}
                signtested:=true;
              end
            else
              signtested:=false;

            { We need to load from a reference }
            location_force_mem(exprasmlist,left.location);

            { For u32bit we need to load it as comp and need to
              make it 64bits }
            if (torddef(left.resulttype.def).typ=u32bit) then
              begin
                tg.GetTemp(exprasmlist,8,tt_normal,href);
                location_release(exprasmlist,left.location);
                location_freetemp(exprasmlist,left.location);
                cg.a_load_ref_ref(exprasmlist,left.location.size,OS_32,left.location.reference,href);
                inc(href.offset,4);
                cg.a_load_const_ref(exprasmlist,OS_32,0,href);
                dec(href.offset,4);
                left.location.reference:=href;
              end;

            { Load from reference to fpu reg }
            location_release(exprasmlist,left.location);
            case torddef(left.resulttype.def).typ of
              u32bit,
              scurrency,
              s64bit:
                exprasmlist.concat(taicpu.op_ref(A_FILD,S_IQ,left.location.reference));
              u64bit:
                begin
                   { unsigned 64 bit ints are harder to handle:
                     we load bits 0..62 and then check bit 63:
                     if it is 1 then we add $80000000 000000000
                     as double                                  }
                   objectlibrary.getdatalabel(l1);
                   objectlibrary.getlabel(l2);

                   if not(signtested) then
                     begin
                       inc(left.location.reference.offset,4);
                       emit_const_ref(A_BT,S_L,31,left.location.reference);
                       dec(left.location.reference.offset,4);
                     end;

                   exprasmlist.concat(taicpu.op_ref(A_FILD,S_IQ,left.location.reference));
                   cg.a_jmp_flags(exprasmlist,F_NC,l2);
                   Consts.concat(Tai_label.Create(l1));
                   { I got this constant from a test program (FK) }
                   Consts.concat(Tai_const.Create_32bit(0));
                   Consts.concat(Tai_const.Create_32bit(1138753536));
                   reference_reset_symbol(href,l1,0);
                   exprasmList.concat(Taicpu.Op_ref(A_FADD,S_FL,href));
                   cg.a_label(exprasmlist,l2);
                end
              else
                begin
                  if left.resulttype.def.size<4 then
                    begin
                      tg.GetTemp(exprasmlist,4,tt_normal,href);
                      location_freetemp(exprasmlist,left.location);
                      cg.a_load_ref_ref(exprasmlist,left.location.size,OS_32,left.location.reference,href);
                      left.location.reference:=href;
                    end;
                 exprasmlist.concat(taicpu.op_ref(A_FILD,S_IL,left.location.reference));
                end;
            end;
            location_freetemp(exprasmlist,left.location);
            tcgx86(cg).inc_fpu_stack;
            location.register:=NR_ST;
          end;
      end;

end.
{
  $Log$
  Revision 1.12  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.11  2004/05/10 20:57:45  florian
    * fixed qword -> <real> type cast

  Revision 1.10.2.4  2004/05/09 16:04:57  peter
    * compile fixes

  Revision 1.10.2.3  2004/05/05 05:34:24  florian
    * qword -> double type cast fixed

  Revision 1.10.2.2  2004/04/27 18:18:26  peter
    * aword -> aint

  Revision 1.10.2.1  2004/04/26 15:54:33  peter
    * small x86-64 fixes

  Revision 1.10  2004/02/27 10:21:06  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

  Revision 1.9  2004/02/05 01:24:08  florian
    * several fixes to compile x86-64 system

  Revision 1.8  2003/12/26 00:32:22  florian
    + fpu<->mm register conversion

  Revision 1.7  2003/10/10 17:48:14  peter
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