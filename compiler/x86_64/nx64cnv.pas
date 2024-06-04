{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86-64 assembler for type converting nodes

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
unit nx64cnv;

{$i fpcdefs.inc}

interface

    uses
      node,defutil,pass_1,
      nx86cnv;

    type
       tx8664typeconvnode = class(tx86typeconvnode)
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
         function first_int_to_real : tnode;override;
         procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
         { procedure second_int_to_bool;override; }
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override;  }
         { procedure second_char_to_char;override; }
       end;


implementation

    uses
      verbose,globals,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      cgbase,cga,
      ncnv,
      cpubase,
      cgutils,cgobj,hlcgobj,cgx86;


    function tx8664typeconvnode.first_int_to_real : tnode;
      begin
        result:=nil;
        if use_vectorfpu(resultdef) and
           (torddef(left.resultdef).ordtype=u32bit) then
          begin
            inserttypeconv(left,s64inttype);
            firstpass(left);
          end
        else
          result:=inherited first_int_to_real;
       if use_vectorfpu(resultdef) then
         expectloc:=LOC_MMREGISTER;
      end;


    procedure tx8664typeconvnode.second_int_to_real;
      var
         href : treference;
         l1,l2 : tasmlabel;
         op : tasmop;
      begin
        if use_vectorfpu(resultdef) then
          begin
            if is_double(resultdef) then
              op:=A_CVTSI2SD
            else if is_single(resultdef) then
              op:=A_CVTSI2SS
            else
              internalerror(200506061);

            location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
            location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);

            case torddef(left.resultdef).ordtype of
              u64bit:
                begin
                   { unsigned 64 bit ints are harder to handle:
                     we load bits 0..62 and then check bit 63:
                     if it is 1 then we add $80000000 000000000
                     as double                                  }
                   current_asmdata.getglobaldatalabel(l1);
                   current_asmdata.getjumplabel(l2);

                   { Get sign bit }
                   if not(left.location.loc in [LOC_REGISTER,LOC_REFERENCE]) then
                     hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
                   case left.location.loc of
                     LOC_REGISTER :
                       begin
                         cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                         emit_const_reg(A_BT,S_Q,63,left.location.register);
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,S_Q,left.location.register,location.register));
                       end;
                     LOC_REFERENCE :
                       begin
                         href:=left.location.reference;
                         tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                         inc(href.offset,4);
                         cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                         emit_const_ref(A_BT,S_L,31,href);
                         dec(href.offset,4);
                         current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(op,S_Q,href,location.register));
                       end;
                     else
                       internalerror(200710181);
                   end;

                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NC,l2);
                   cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                   new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,l1.name,const_align(sizeof(pint)));
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                   reference_reset_symbol(href,l1,0,4,[]);
                   { simplify for PIC }
                   tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);

                   { I got these constant from a test program (FK) }
                   if is_double(resultdef) then
                     begin
                       { double (2^64) }
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(0));
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($43f00000));
                       tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                       current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_ADDSD,S_NO,href,location.register));
                     end
                   else if is_single(resultdef) then
                     begin
                       { single(2^64) }
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($5f800000));
                       current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_ADDSS,S_NO,href,location.register));
                     end
                   else
                     internalerror(200506071);
                   cg.a_label(current_asmdata.CurrAsmList,l2);
                end
              else
                inherited second_int_to_real;
            end;
          end
        else
          inherited second_int_to_real;
      end;


begin
   ctypeconvnode:=tx8664typeconvnode;
end.
