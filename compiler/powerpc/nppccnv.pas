{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for type converting nodes

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
unit nppccnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,ngppccnv;

    type
       tppctypeconvnode = class(tgenppctypeconvnode)
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
         { procedure second_int_to_bool;override; }
         { procedure second_set_to_set;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
       end;

implementation

   uses
      verbose,globtype,globals,systems,
      symconst,symdef,aasmbase,aasmtai,aasmdata,
      defutil,symcpu,
      cgbase,cgutils,pass_1,pass_2,
      ncon,ncal,
      ncgutil,procinfo,
      cpubase,aasmcpu,
      rgobj,tgobj,cgobj,hlcgobj;


{*****************************************************************************
                             FirstTypeConv
*****************************************************************************}

    function tppctypeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
      begin
        { converting a 64bit integer to a float requires a helper }
        if is_64bitint(left.resultdef) or
                is_currency(left.resultdef) then
          begin
            { hack to avoid double division by 10000, as it's       }
            { already done by typecheckpass.resultdef_int_to_real }
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

    procedure tppctypeconvnode.second_int_to_real;

      type
        tdummyarray = packed array[0..7] of byte;

      const
         dummy1: int64 = $4330000080000000;
         dummy2: int64 = $4330000000000000;

      var
        tempconst: tnode;
        ref: treference;
        valuereg, tempreg, leftreg, tmpfpureg: tregister;
        size: tcgsize;
        signed : boolean;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));

        { the code here comes from the PowerPC Compiler Writer's Guide }

        { * longint to double                               }
        { addis R0,R0,0x4330  # R0 = 0x43300000             }
        { stw R0,disp(R1)     # store upper half            }
        { xoris R3,R3,0x8000  # flip sign bit               }
        { stw R3,disp+4(R1)   # store lower half            }
        { lfd FR1,disp(R1)    # float load double of value  }
        { fsub FR1,FR1,FR2    # subtract 0x4330000080000000 }

        { * cardinal to double                              }
        { addis R0,R0,0x4330  # R0 = 0x43300000             }
        { stw R0,disp(R1)     # store upper half            }
        { stw R3,disp+4(R1)   # store lower half            }
        { lfd FR1,disp(R1)    # float load double of value  }
        { fsub FR1,FR1,FR2    # subtract 0x4330000000000000 }
        tg.Gettemp(current_asmdata.CurrAsmList,8,8,tt_normal,ref);

        signed := is_signed(left.resultdef);

        { we need a certain constant for the conversion, so create it here }
        if signed then
          tempconst :=
            crealconstnode.create(double(tdummyarray(dummy1)),
            pbestrealtype^)
        else
          tempconst :=
            crealconstnode.create(double(tdummyarray(dummy2)),
            pbestrealtype^);

        typecheckpass(tempconst);
        firstpass(tempconst);
        secondpass(tempconst);
        if (tempconst.location.loc <> LOC_CREFERENCE) or
           { has to be handled by a helper }
           is_64bitint(left.resultdef) then
          internalerror(200110011);

        if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_REFERENCE,LOC_CREFERENCE]) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
        case left.location.loc of
          LOC_REGISTER:
            begin
              leftreg := left.location.register;
              valuereg := leftreg;
            end;
          LOC_CREGISTER:
            begin
              leftreg := left.location.register;
              if signed then
                valuereg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT)
              else
                valuereg := leftreg;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              leftreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
              valuereg := leftreg;
              if signed then
                size := OS_S32
              else
                size := OS_32;
              cg.a_load_ref_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),
                size,left.location.reference,leftreg);
            end
          else
            internalerror(200110012);
         end;
         tempreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_LIS,tempreg,$4330));
         cg.a_load_reg_ref(current_asmdata.CurrAsmList,OS_32,OS_32,tempreg,ref);
         if signed then
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_XORIS,valuereg,
             { xoris expects a unsigned 16 bit int (FK) }
             leftreg,$8000));
         inc(ref.offset,4);
         cg.a_load_reg_ref(current_asmdata.CurrAsmList,OS_32,OS_32,valuereg,ref);
         dec(ref.offset,4);

         tmpfpureg := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
         cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64,OS_F64,tempconst.location.reference,
           tmpfpureg);
         tempconst.free;

         location.register := cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
         cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,OS_F64,OS_F64,ref,location.register);

         tg.ungetiftemp(current_asmdata.CurrAsmList,ref);

         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_FSUB,location.register,
           location.register,tmpfpureg));

        { make sure the precision is correct }
        if (tfloatdef(resultdef).floattype = s32real) then
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FRSP,location.register,
             location.register));
       end;

begin
   ctypeconvnode:=tppctypeconvnode;
end.
