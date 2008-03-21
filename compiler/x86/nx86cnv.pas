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
         { procedure second_set_to_set;override;  }
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
      cgbase,cga,procinfo,pass_1,pass_2,
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
           expectloc:=LOC_MMREGISTER
         else
           expectloc:=LOC_FPUREGISTER;
      end;


    procedure tx86typeconvnode.second_int_to_bool;
      var
{$ifndef cpu64bitalu}
        hreg2,
{$endif not cpu64bitalu}
        hregister : tregister;
{$ifndef cpu64bitalu}
        href      : treference;
{$endif not cpu64bitalu}
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
         { Explicit typecasts from any ordinal type to a boolean type }
         { must not change the ordinal value                          }
         if (nf_explicit in flags) and
            (left.resultdef.size=resultdef.size) and
            not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) then
           begin
              location_copy(location,left.location);
              location.size:=def_cgsize(resultdef);
              { change of sign? Then we have to sign/zero-extend in }
              { case of a loc_(c)register                           }
              if (location.size<>left.location.size) and
                 (location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                location_force_reg(current_asmdata.CurrAsmList,location,location.size,true);
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
{$ifndef cpu64bitalu}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                   cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.reference,hregister);
                   href:=left.location.reference;
                   inc(href.offset,4);
                   cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,href,hregister);
                 end
                else
{$endif not cpu64bitalu}
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
{$ifndef cpu64bitalu}
                if left.location.size in [OS_64,OS_S64] then
                 begin
                   hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                   cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.register64.reglo,hregister);
                   cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,left.location.register64.reghi,hregister);
                 end
                else
{$endif not cpu64bitalu}
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_OR,left.location.size,left.location.register,left.location.register);
              end;
            LOC_JUMP :
              begin
                location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
                location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
                current_asmdata.getjumplabel(hlabel);
                cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                if not(is_cbool(resultdef)) then
                  cg.a_load_const_reg(current_asmdata.CurrAsmList,location.size,1,location.register)
                else
                  cg.a_load_const_reg(current_asmdata.CurrAsmList,location.size,-1,location.register);
                cg.a_jmp_always(current_asmdata.CurrAsmList,hlabel);
                cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,location.size,0,location.register);
                cg.a_label(current_asmdata.CurrAsmList,hlabel);
              end;
            else
              internalerror(10062);
         end;
         if (left.location.loc<>LOC_JUMP) then
           begin
             { load flags to register }
             location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
{$ifndef cpu64bitalu}
              if (location.size in [OS_64,OS_S64]) then
                begin
                  hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                  cg.g_flags2reg(current_asmdata.CurrAsmList,OS_32,resflags,hreg2);
                  if (is_cbool(resultdef)) then
                    cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NEG,OS_32,hreg2,hreg2);
                  location.register64.reglo:=hreg2;
                  location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                  if (is_cbool(resultdef)) then
                    { reglo is either 0 or -1 -> reghi has to become the same }
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,location.register64.reglo,location.register64.reghi)
                  else
                    { unsigned }
                    cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reghi);
                end
             else
{$endif not cpu64bitalu}
               begin
                 location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
                 cg.g_flags2reg(current_asmdata.CurrAsmList,location.size,resflags,location.register);
                 if (is_cbool(resultdef)) then
                   cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NEG,location.size,location.register,location.register);
               end
           end;
         current_procinfo.CurrTrueLabel:=oldTrueLabel;
         current_procinfo.CurrFalseLabel:=oldFalseLabel;
       end;


    function tx86typeconvnode.first_int_to_real : tnode;

      begin
        first_int_to_real:=nil;
        if (left.resultdef.size<4) then
          begin
            inserttypeconv(left,s32inttype);
            firstpass(left)
          end;

        if use_sse(resultdef) and
           (torddef(left.resultdef).ordtype = s32bit) then
          expectloc:=LOC_MMREGISTER
        else
          expectloc:=LOC_FPUREGISTER;
      end;


    procedure tx86typeconvnode.second_int_to_real;

      var
         href : treference;
         l1,l2 : tasmlabel;
         op: tasmop;
         opsize: topsize;
         signtested : boolean;
      begin
        if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE]) then
          location_force_reg(current_asmdata.CurrAsmList,left.location,left.location.size,false);
        if use_sse(resultdef) and
{$ifdef cpu64bitalu}
           (torddef(left.resultdef).ordtype in [s32bit,s64bit]) then
{$else cpu64bitalu}
           (torddef(left.resultdef).ordtype=s32bit) then
{$endif cpu64bitalu}
          begin
            location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
            location.register:=cg.getmmregister(current_asmdata.CurrAsmList,location.size);
            case location.size of
              OS_F32:
                op:=A_CVTSI2SS;
              OS_F64:
                op:=A_CVTSI2SD;
              else
                internalerror(2007120902);
            end;
            case left.location.size of
              OS_S32:
                opsize:=S_L;
              OS_S64:
                opsize:=S_Q;
              else
                internalerror(2007120903);
            end;
            case left.location.loc of
              LOC_REFERENCE,
              LOC_CREFERENCE:
                begin
                  href:=left.location.reference;
                  tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                  current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(op,opsize,href,location.register));
                end;
              LOC_REGISTER,
              LOC_CREGISTER:
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,opsize,left.location.register,location.register));
            end;
          end
        else
          begin
            location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
            if (left.location.loc=LOC_REGISTER) and (torddef(left.resultdef).ordtype=u64bit) then
              begin
    {$ifdef cpu64bitalu}
                emit_const_reg(A_BT,S_Q,63,left.location.register);
    {$else cpu64bitalu}
                emit_const_reg(A_BT,S_L,31,left.location.register64.reghi);
    {$endif cpu64bitalu}
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
                begin
                  href:=left.location.reference;
                  tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                  current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_FILD,S_IQ,href));
                end;
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
                    internalerror(2007120901);
                 href:=left.location.reference;
                 tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,href);
                 current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_FILD,S_IL,href));
                end;
            end;
            tcgx86(cg).inc_fpu_stack;
            location.register:=NR_ST;
          end;
        location_freetemp(current_asmdata.CurrAsmList,left.location);
      end;

begin
  ctypeconvnode:=tx86typeconvnode
end.
