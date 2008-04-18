{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SPARC assembler for type converting nodes

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

 ****************************************************************************}
unit ncpucnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,defcmp;

    type
       tsparctypeconvnode = class(TCgTypeConvNode)
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
      verbose,globals,systems,globtype,
      symconst,symdef,aasmbase,aasmtai,aasmdata,
      defutil,
      cgbase,cgutils,pass_1,pass_2,
      ncon,ncal,procinfo,
      ncgutil,
      cpubase,aasmcpu,
      tgobj,cgobj;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function tsparctypeconvnode.first_int_to_real: tnode;
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
              left.resultdef := s64inttype;
            if is_signed(left.resultdef) then
              fname := 'fpc_int64_to_double'
            else
              fname := 'fpc_qword_to_double';
            result := ccallnode.createintern(fname,ccallparanode.create(
              left,nil));
            left:=nil;
            firstpass(result);
            exit;
          end
        else
          { other integers are supposed to be 32 bit }
          begin
            if is_signed(left.resultdef) then
              inserttypeconv(left,s32inttype)
            else
              inserttypeconv(left,u32inttype);
            firstpass(left);
          end;
        result := nil;
        expectloc:=LOC_FPUREGISTER;
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure tsparctypeconvnode.second_int_to_real;

      procedure loadsigned;
        begin
          location_force_mem(current_asmdata.CurrAsmList,left.location);
          { Load memory in fpu register }
          cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F32,OS_F32,left.location.reference,location.register);
          tg.ungetiftemp(current_asmdata.CurrAsmList,left.location.reference);
          { Convert value in fpu register from integer to float }
          case tfloatdef(resultdef).floattype of
            s32real:
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FiTOs,location.register,location.register));
            s64real:
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FiTOd,location.register,location.register));
            s128real:
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FiTOq,location.register,location.register));
            else
              internalerror(200408011);
          end;
        end;

      var
        href : treference;
        hregister : tregister;
        l1,l2 : tasmlabel;

      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        if is_signed(left.resultdef) then
          begin
            location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
            loadsigned;
          end
        else
          begin
            current_asmdata.getdatalabel(l1);
            current_asmdata.getjumplabel(l2);
            reference_reset_symbol(href,l1,0);
            hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_32,left.location,hregister);

            { here we need always an 64 bit register }
            location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
            location_force_mem(current_asmdata.CurrAsmList,left.location);
            { Load memory in fpu register }
            cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F32,OS_F32,left.location.reference,location.register);
            tg.ungetiftemp(current_asmdata.CurrAsmList,left.location.reference);
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FiTOd,location.register,location.register));

            current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg(A_CMP,hregister,NR_G0));
            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_GE,l2);

            case tfloatdef(resultdef).floattype of
               { converting dword to s64real first and cut off at the end avoids precision loss }
               s32real,
               s64real:
                 begin
                   hregister:=cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
                   current_asmdata.asmlists[al_typedconsts].concat(tai_align.create(const_align(8)));
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                   { I got this constant from a test program (FK) }
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($41f00000));
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(0));

                   cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64,OS_F64,href,hregister);
                   current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FADDD,location.register,hregister,location.register));
                   cg.a_label(current_asmdata.CurrAsmList,l2);

                   { cut off if we should convert to single }
                   if tfloatdef(resultdef).floattype=s32real then
                     begin
                       hregister:=location.register;
                       location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                       current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FDTOS,hregister,location.register));
                     end;
                 end;
               else
                 internalerror(200410031);
            end;
          end;
       end;


(*
    procedure tsparctypeconvnode.second_real_to_real;
      const
        conv_op : array[tfloattype,tfloattype] of tasmop = (
          {    from:   s32     s64     s80     c64     cur    f128 }
          { s32 }  ( A_FMOVS,A_FDTOS,A_NONE, A_NONE, A_NONE, A_NONE ),
          { s64 }  ( A_FSTOD,A_FMOVD,A_NONE, A_NONE, A_NONE, A_NONE ),
          { s80 }  ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE ),
          { c64 }  ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE ),
          { cur }  ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE ),
          { f128 } ( A_NONE, A_NONE, A_NONE, A_NONE, A_NONE, A_NONE )
        );
      var
        op : tasmop;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
        { Convert value in fpu register from integer to float }
        op:=conv_op[tfloatdef(resultdef).floattype,tfloatdef(left.resultdef).floattype];
        if op=A_NONE then
          internalerror(200401121);
        location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,left.location.register,location.register));
      end;
*)

    procedure tsparctypeconvnode.second_int_to_bool;
      var
        hreg1,hreg2 : tregister;
        resflags : tresflags;
        opsize   : tcgsize;
        hlabel,oldTrueLabel,oldFalseLabel : tasmlabel;
        newsize  : tcgsize;
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
            not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) then
           begin
              location_copy(location,left.location);
              newsize:=def_cgsize(resultdef);
              { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
              if (tcgsize2size[newsize]<>tcgsize2size[left.location.size]) or
                 ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
                location_force_reg(current_asmdata.CurrAsmList,location,newsize,true)
              else
                location.size:=newsize;
              current_procinfo.CurrTrueLabel:=oldTrueLabel;
              current_procinfo.CurrFalseLabel:=oldFalseLabel;
              exit;
           end;

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        opsize:=def_cgsize(left.resultdef);
        case left.location.loc of
          LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER:
            begin
              if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                begin
                  hreg2:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.reference,hreg2);
                end
              else
                hreg2:=left.location.register;
{$ifndef cpu64bitalu}
              if left.location.size in [OS_64,OS_S64] then
                begin
                  hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                  cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hreg2,left.location.register64.reghi,hreg1);
                  hreg2:=hreg1;
                  opsize:=OS_32;
                end;
{$endif not cpu64bitalu}
              hreg1:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBCC,NR_G0,hreg2,NR_G0));
              if is_pasbool(resultdef) then
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADDX,NR_G0,NR_G0,hreg1))
              else
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBX,NR_G0,NR_G0,hreg1));
            end;
          LOC_FLAGS :
            begin
              hreg1:=cg.GetIntRegister(current_asmdata.CurrAsmList,location.size);
              resflags:=left.location.resflags;
              cg.g_flags2reg(current_asmdata.CurrAsmList,location.size,resflags,hreg1);
              if (is_cbool(resultdef)) then
                cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NEG,location.size,hreg1,hreg1);
            end;
          LOC_JUMP :
            begin
              hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              current_asmdata.getjumplabel(hlabel);
              cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
              if not(is_cbool(resultdef)) then
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,1,hreg1)
              else
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,-1,hreg1);
              cg.a_jmp_always(current_asmdata.CurrAsmList,hlabel);
              cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
              cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,0,hreg1);
              cg.a_label(current_asmdata.CurrAsmList,hlabel);
            end;
          else
            internalerror(10062);
        end;
{$ifndef cpu64bitalu}
         if (location.size in [OS_64,OS_S64]) then
           begin
             location.register64.reglo:=hreg1;
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
           location.register:=hreg1;

        current_procinfo.CurrTrueLabel:=oldTrueLabel;
        current_procinfo.CurrFalseLabel:=oldFalseLabel;
      end;


begin
   ctypeconvnode:=tsparctypeconvnode;
end.
