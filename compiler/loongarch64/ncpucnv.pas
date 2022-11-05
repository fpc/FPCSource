{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate LoongArch64 assembler for type converting nodes

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
unit ncpucnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv;

    type
       tloongarch64typeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
           function first_int_to_real: tnode; override;
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
           procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
           procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
       end;


implementation

   uses
      verbose,globtype,globals,systems,
      symconst,symdef,aasmbase,aasmtai,aasmdata,
      defutil, symcpu,
      cgbase,cgutils,pass_1,pass_2,
      ncon, ncal,procinfo,
      ncgutil,
      cpubase,aasmcpu,
      rgobj,tgobj,cgobj,hlcgobj;


    {*****************************************************************************
                                 FirstTypeConv
    *****************************************************************************}

    function tloongarch64typeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
      begin
        { converting a 64bit integer to a float requires a helper }
        if is_64bitint(left.resultdef) or
          is_currency(left.resultdef) then
          begin
            { hack to avoid double division by 10000, as it's
              already done by typecheckpass.resultdef_int_to_real }
            if is_currency(left.resultdef) then
              left.resultdef := s64inttype
            else if not is_signed(left.resultdef) then
              begin
                fname := 'fpc_qword_to_double';
                result := ccallnode.createintern(fname,ccallparanode.create(left,nil));
                left:=nil;
                if (tfloatdef(resultdef).floattype=s32real) then
                  inserttypeconv(result,s32floattype);
                firstpass(result);
                exit;
              end;
          end
        else
          begin
            { Else signed supposed to be 32 bit, or unsigned supposed to be 64 bit }
            if is_signed(left.resultdef) then
              inserttypeconv(left,s32inttype)
            else
              inserttypeconv(left,s64inttype);
            firstpass(left);
          end;
        result := nil;
        expectloc:=LOC_FPUREGISTER;
      end;


    {*****************************************************************************
                                 SecondTypeConv
    *****************************************************************************}


    procedure tloongarch64typeconvnode.second_int_to_real;
      var
        op, movop: TAsmOp;
        restype: tfloattype;
        hreg: tregister;
      begin
        location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));
        restype:=tfloatdef(resultdef).floattype;
        location.Register := cg.getfpuregister(current_asmdata.CurrAsmList, tfloat2tcgsize[restype]);

        if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList, left.location, left.resultdef, left.resultdef, true);
        case left.location.size of
          OS_32,OS_64: internalerror(2022111928);
          OS_S32:
            begin
              if restype=s32real then
                op:=A_FFINT_S_W
              else if restype=s64real then
                op:=A_FFINT_D_W
              else
                internalerror(2022111929);
              hreg:=cg.getfpuregister(current_asmdata.CurrAsmList, OS_F32);
              movop:=A_MOVGR2FR_W;
            end;
          OS_S64:
            begin
              if restype=s32real then
                op:=A_FFINT_S_L
              else if restype=s64real then
                op:=A_FFINT_D_L
              else
                internalerror(2022111930);
              hreg:= cg.getfpuregister(current_asmdata.CurrAsmList, OS_F64);
              movop:=A_MOVGR2FR_D;
            end;
        else
          internalerror(2022111931);
        end;
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(movop, hreg, left.location.register));
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op, location.register, hreg));
      end;


    procedure tloongarch64typeconvnode.second_int_to_bool;
      var
        hreg1, hreg2: tregister;
        opsize: tcgsize;
        hlabel: tasmlabel;
        newsize  : tcgsize;
        href: treference;
      begin
        secondpass(left);
        if codegenerror then
          exit;

        { Explicit typecasts from any ordinal type to a boolean type }
        { must not change the ordinal value                          }
        if (nf_explicit in flags) and
           not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) then
          begin
             location_copy(location,left.location);
             newsize:=def_cgsize(resultdef);
             { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
             if (tcgsize2size[newsize]<>tcgsize2size[left.location.size]) or
                ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
               hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,resultdef,true)
             else
               location.size:=newsize;
             exit;
          end;

        location_reset(location, LOC_REGISTER, def_cgsize(resultdef));
        opsize := def_cgsize(left.resultdef);

        if (left.location.loc in [LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF]) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

        case left.location.loc of
          LOC_CREFERENCE, LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER:
            begin
              if left.location.loc in [LOC_CREFERENCE, LOC_REFERENCE] then
                begin
                  hreg2 := cg.getintregister(current_asmdata.CurrAsmList, opsize);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList, opsize, opsize, left.location.reference, hreg2);
                end
              else
                begin
                  hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.register,hreg2);
                end;
              hreg1 := cg.getintregister(current_asmdata.CurrAsmList, opsize);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SLTU, hreg1, NR_R0, hreg2));
            end;
          LOC_JUMP:
            begin
              hreg1 := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
              current_asmdata.getjumplabel(hlabel);
              cg.a_label(current_asmdata.CurrAsmList, left.location.truelabel);
              cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 1, hreg1);
              cg.a_jmp_always(current_asmdata.CurrAsmList, hlabel);
              cg.a_label(current_asmdata.CurrAsmList, left.location.falselabel);
              cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 0, hreg1);
              cg.a_label(current_asmdata.CurrAsmList, hlabel);
            end;
          LOC_FLAGS:
            Internalerror(2022111932);
          else
            internalerror(2022111933);
        end;
        { Now hreg1 is either 0 or 1. For C booleans it must be 0 or -1. }
        if is_cbool(resultdef) then
          cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NEG,OS_SINT,hreg1,hreg1);

        location.Register := hreg1;
      end;


begin
  ctypeconvnode := tloongarch64typeconvnode;
end.
