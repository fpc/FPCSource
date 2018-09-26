{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Risc-V32 assembler for type converting nodes

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
unit nrv32cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv,nrvcnv;

    type
       trv32typeconvnode = class(trvtypeconvnode)
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

    function trv32typeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
      begin                                                        
        if (cs_fp_emulation in current_settings.moduleswitches) then
          result:=inherited first_int_to_real                      
        { converting a 64bit integer to a float requires a helper }
        else
          begin
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
      end;


{*****************************************************************************
                             SecondTypeConv
*****************************************************************************}

    procedure trv32typeconvnode.second_int_to_real;
      const
        ops: array[boolean,s32real..s64real] of TAsmOp =
          ((A_FCVT_S_WU,A_FCVT_D_WU),
           (A_FCVT_S_W,A_FCVT_D_W));
      var
        restype: tfloattype;
      begin
        location_reset(location, LOC_FPUREGISTER, def_cgsize(resultdef));

        restype:=tfloatdef(resultdef).floattype;

        location.Register := cg.getfpuregister(current_asmdata.CurrAsmList, tfloat2tcgsize[restype]);
        if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(ops[is_signed(left.resultdef),restype], location.register, left.location.register));
          end
        else
          begin
            { Load memory in fpu register }
            hlcg.location_force_mem(current_asmdata.CurrAsmList, left.location, left.resultdef);
            cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList, OS_F32, OS_F32, left.location.reference, location.Register);
            tg.ungetiftemp(current_asmdata.CurrAsmList, left.location.reference);

            case restype of
              s64real: cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList, OS_F32, OS_F64, location.register, location.Register);
            end;
          end;
      end;

begin
   ctypeconvnode:=trv32typeconvnode;
end.

