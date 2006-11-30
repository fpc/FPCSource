{
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
      verbose,systems,globals,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      cgbase,cga,procinfo,pass_2,
      ncon,ncal,ncnv,
      cpubase,
      cgutils,cgobj,cgx86,ncgutil,
      tgobj;


    function tx86typeconvnode.first_real_to_real : tnode;
      begin
         first_real_to_real:=nil;
        { comp isn't a floating type }
         if (tfloatdef(resultdef).floattype=s64comp) and
            (tfloatdef(left.resultdef).floattype<>s64comp) and
            not (nf_explicit in flags) then
           CGMessage(type_w_convert_real_2_comp);
         if use_sse(resultdef) then
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
        hlabel,oldTrueLabel,oldFalseLabel : tasmlabel;
      begin
         oldTrueLabel:=current_procinfo.CurrTrueLabel;
         oldFalseLabel:=current_procinfo.CurrFalseLabel;
         current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
         current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
         secondpass(left);
         if codegenerror then
          exit;
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explicit in flags) and
            (left.resultdef.size=resultdef.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              location_copy(location,left.location);
              current_procinfo.CurrTrueLabel:=oldTrueLabel;
              current_procinfo.CurrFalseLabel:=oldFalseLabel;
              exit;
           end;

         { Load left node into flag F_NE/F_E }
         resflags:=F_NE;

         if (left.location.loc in [LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF]) then
           location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,true);

         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE :
              begin
{$ifndef cpu64bit}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                   cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.reference,hregister);
                   href:=left.location.reference;
                   inc(href.offset,4);
                   cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,href,hregister);
                 end
                else
{$endif cpu64bit}
                 begin
                   location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,true);
                   cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_OR,left.location.size,left.location.register,left.location.register);
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
                   hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                   cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.register64.reglo,hregister);
                   cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,left.location.register64.reghi,hregister);
                 end
                else
{$endif cpu64bit}
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_OR,left.location.size,left.location.register,left.location.register);
              end;
            LOC_JUMP :
              begin
                hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                current_asmdata.getjumplabel(hlabel);
                cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,1,hregister);
                cg.a_jmp_always(current_asmdata.CurrAsmList,hlabel);
                cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,0,hregister);
                cg.a_label(current_asmdata.CurrAsmList,hlabel);
                cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_INT,hregister,hregister);
              end;
            else
              internalerror(10062);
         end;
         { load flags to register }
         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
         cg.g_flags2reg(current_asmdata.CurrAsmList,location.size,resflags,location.register);
         current_procinfo.CurrTrueLabel:=oldTrueLabel;
         current_procinfo.CurrFalseLabel:=oldFalseLabel;
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
         hreg : tregister;
         op : tasmop;
      begin
        if (left.location.loc in [LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF]) then
          location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,true);

{$ifdef x86_64}
        if use_sse(resultdef) then
          begin
            { We can only directly convert s32bit and s64bit,u64bit values, for other
              values convert first to s64bit }
            if not(torddef(left.resultdef).ordtype in [s32bit,s64bit,u64bit]) then
              begin
                hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_S64);
                location_force_reg(current_asmdata.CurrAsmList,left.location,OS_S64,false);
              end;

            if is_double(resultdef) then
              op:=A_CVTSI2SD
            else if is_single(resultdef) then
              op:=A_CVTSI2SS
            else
              internalerror(200506061);

            location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
            location.register:=cg.getmmregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));

            case torddef(left.resultdef).ordtype of
              u64bit:
                begin
                   { unsigned 64 bit ints are harder to handle:
                     we load bits 0..62 and then check bit 63:
                     if it is 1 then we add $80000000 000000000
                     as double                                  }
                   current_asmdata.getdatalabel(l1);
                   current_asmdata.getjumplabel(l2);

                   { Get sign bit }
                   if (left.location.loc=LOC_REGISTER) then
                     emit_const_reg(A_BT,S_Q,63,left.location.register)
                   else
                     begin
                       inc(left.location.reference.offset,4);
                       emit_const_ref(A_BT,S_L,31,left.location.reference);
                       dec(left.location.reference.offset,4);
                     end;

                   current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(op,S_Q,left.location.reference,location.register));

                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NC,l2);
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                   reference_reset_symbol(href,l1,0);

                   { I got these constant from a test program (FK) }
                   if is_double(resultdef) then
                     begin
                       { double (2^64) }
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(0));
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($43f00000));
                       { simplify for PIC }
                       tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                       current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_ADDSD,S_NO,href,location.register));
                     end
                   else if is_single(resultdef) then
                     begin
                       { single(2^64) }
                       current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($5f800000));
                       { simplify for PIC }
                       tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                       current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_ADDSS,S_NO,href,location.register));
                     end
                   else
                     internalerror(200506071);
                   cg.a_label(current_asmdata.CurrAsmList,l2);
                end
              else
                begin
                  case left.location.loc of
                    LOC_CREFERENCE,
                    LOC_REFERENCE :
                      current_asmdata.CurrAsmList.concat(Taicpu.op_ref_reg(op,tcgsize2opsize[left.location.size],left.location.reference,location.register));
                    LOC_CREGISTER,
                    LOC_REGISTER :
                      current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg(op,tcgsize2opsize[left.location.size],left.location.register,location.register));
                    else
                      internalerror(200506072);
                  end;
                end;
            end;
          end
        else
{$endif x86_64}
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
            if (left.location.loc=LOC_REGISTER) and (torddef(left.resultdef).ordtype=u64bit) then
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
            location_force_mem(current_asmdata.CurrAsmList,left.location);

            { For u32bit we need to load it as comp and need to
              make it 64bits }
            if (torddef(left.resultdef).ordtype=u32bit) then
              begin
                tg.GetTemp(current_asmdata.CurrAsmList,8,tt_normal,href);
                location_freetemp(current_asmdata.CurrAsmList,left.location);
                cg.a_load_ref_ref(current_asmdata.CurrAsmList,left.location.size,OS_32,left.location.reference,href);
                inc(href.offset,4);
                cg.a_load_const_ref(current_asmdata.CurrAsmList,OS_32,0,href);
                dec(href.offset,4);
                left.location.reference:=href;
              end;

            { Load from reference to fpu reg }
            case torddef(left.resultdef).ordtype of
              u32bit,
              scurrency,
              s64bit:
                current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_FILD,S_IQ,left.location.reference));
              u64bit:
                begin
                   { unsigned 64 bit ints are harder to handle:
                     we load bits 0..62 and then check bit 63:
                     if it is 1 then we add $80000000 000000000
                     as double                                  }
                   current_asmdata.getdatalabel(l1);
                   current_asmdata.getjumplabel(l2);

                   if not(signtested) then
                     begin
                       inc(left.location.reference.offset,4);
                       emit_const_ref(A_BT,S_L,31,left.location.reference);
                       dec(left.location.reference.offset,4);
                     end;

                   current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_FILD,S_IQ,left.location.reference));
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NC,l2);
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                   { I got this constant from a test program (FK) }
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(0));
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(longint ($80000000)));
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($0000403f));
                   reference_reset_symbol(href,l1,0);
                   tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                   current_asmdata.CurrAsmList.concat(Taicpu.Op_ref(A_FLD,S_FX,href));
                   current_asmdata.CurrAsmList.concat(Taicpu.Op_reg_reg(A_FADDP,S_NO,NR_ST,NR_ST1));
                   cg.a_label(current_asmdata.CurrAsmList,l2);
                end
              else
                begin
                  if left.resultdef.size<4 then
                    begin
                      tg.GetTemp(current_asmdata.CurrAsmList,4,tt_normal,href);
                      location_freetemp(current_asmdata.CurrAsmList,left.location);
                      cg.a_load_ref_ref(current_asmdata.CurrAsmList,left.location.size,OS_32,left.location.reference,href);
                      left.location.reference:=href;
                    end;
                 current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_FILD,S_IL,left.location.reference));
                end;
            end;
            location_freetemp(current_asmdata.CurrAsmList,left.location);
            tcgx86(cg).inc_fpu_stack;
            location.register:=NR_ST;
          end;
      end;

begin
  ctypeconvnode:=tx86typeconvnode
end.
