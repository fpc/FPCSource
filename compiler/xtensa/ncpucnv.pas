{
    Copyright (c) 1998-2019 by Florian Klaempfl

    Generate Xtensa assembler for type converting nodes

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
      node,ncnv,ncgcnv,compilerbase;

    type
       tcputypeconvnode = class(tcgtypeconvnode)
       protected
         function first_real_to_real: tnode;override;
         procedure second_int_to_bool(ctx:tpassgeneratecodecontext);override;
         procedure second_int_to_real(ctx:tpassgeneratecodecontext);override;
         function first_int_to_real: tnode;override;
       end;

implementation

   uses
      verbose,globtype,globals,symdef,aasmbase,aasmtai,aasmdata,symtable,
      defutil,
      cgbase,cgutils,
      pass_1,pass_2,pass_2_context,procinfo,ncal,
      ncgutil,
      cpubase,cpuinfo,aasmcpu,cgobj,nodehelper,cgcpu,compiler;


{*****************************************************************************
                             tcputypeconvnode
*****************************************************************************}

    function tcputypeconvnode.first_real_to_real: tnode;
      begin
        if (FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]) and
          not(FPUXTENSA_DOUBLE in fpu_capabilities[compiler.globals.current_settings.fputype]) then
          begin
            case tfloatdef(left.resultdef).floattype of
              s32real:
                case tfloatdef(resultdef).floattype of
                  s64real:
                    result:=compiler.ctypeconvnode_explicit(compiler.ccallnode_intern('float32_to_float64',compiler.ccallparanode(
                      compiler.ctypeconvnode_internal(left,search_system_type('FLOAT32REC').typedef),nil)),resultdef);
                  s32real:
                    begin
                      result:=left;
                      left:=nil;
                    end;
                  else
                    internalerror(2020092603);
                end;
              s64real:
                case tfloatdef(resultdef).floattype of
                  s32real:
                    result:=compiler.ctypeconvnode_explicit(compiler.ccallnode_intern('float64_to_float32',compiler.ccallparanode(
                      compiler.ctypeconvnode_internal(left,search_system_type('FLOAT64').typedef),nil)),resultdef);
                  s64real:
                    begin
                      result:=left;
                      left:=nil;
                    end;
                  else
                    internalerror(2020092602);
                end;
              else
                internalerror(2020092601);
            end;
            left:=nil;
            firstpass(result);
            exit;
          end
        else
          Result := inherited first_real_to_real;
      end;


    procedure tcputypeconvnode.second_int_to_bool(ctx:tpassgeneratecodecontext);
      var
        hreg1, onereg: tregister;
        href      : treference;
        hlabel    : tasmlabel;
        newsize   : tcgsize;
      begin
        secondpass(left,ctx);
        if compiler.verbose.codegenerror then
          exit;

        { Explicit typecasts from any ordinal type to a boolean type
          must not change the ordinal value                          }
        if (nf_explicit in flags) and
           not(left.location.loc in [LOC_JUMP]) then
          begin
             location_copy(location,left.location);
             newsize:=def_cgsize(resultdef);
             { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
             if (tcgsize2size[newsize]<>tcgsize2size[left.location.size]) or
                ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
               ctx.hlcg.location_force_reg(ctx.CurrAsmList,location,left.resultdef,resultdef,true)
             else
               location.size:=newsize;
             exit;
          end;

        if (left.location.loc in [LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF]) then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));

        onereg:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
        ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,1,onereg);
        hreg1:=ctx.cg.getintregister(ctx.CurrAsmList,location.size);
        case left.location.loc of
          LOC_CREFERENCE,
          LOC_REFERENCE :
            begin
              if left.location.size in [OS_64,OS_S64] then
                begin
                  ctx.cg.a_load_ref_reg(ctx.CurrAsmList,OS_32,OS_32,left.location.reference,hreg1);
                  href:=left.location.reference;
                  inc(href.offset,4);
                  ctx.cg.a_op_ref_reg(ctx.CurrAsmList,OP_OR,OS_32,href,hreg1);
                end
              else
                ctx.cg.a_load_ref_reg(ctx.CurrAsmList,left.location.size,OS_32,left.location.reference,hreg1);
              ctx.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_MOVNEZ,hreg1,onereg,hreg1));
            end;
          LOC_REGISTER,LOC_CREGISTER :
            begin
              if left.location.size in [OS_64,OS_S64] then
                ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,OS_32,left.location.register64.reglo,left.location.register64.reghi,hreg1)
              else
                ctx.cg.a_load_reg_reg(ctx.CurrAsmList,left.location.size,OS_32,left.location.register,hreg1);
              ctx.CurrAsmList.Concat(taicpu.op_reg_reg_reg(A_MOVNEZ,hreg1,onereg,hreg1));
            end;
          LOC_JUMP :
            begin
              ctx.CurrAsmList.AsmData.getjumplabel(hlabel);
              ctx.cg.a_label(ctx.CurrAsmList,left.location.truelabel);
              ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,1,hreg1);
              ctx.cg.a_jmp_always(ctx.CurrAsmList,hlabel);
              ctx.cg.a_label(ctx.CurrAsmList,left.location.falselabel);
              ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,0,hreg1);
              ctx.cg.a_label(ctx.CurrAsmList,hlabel);
            end;
          else
            internalerror(2020031504);
        end;
        if (is_cbool(resultdef)) then
          ctx.cg.a_op_reg_reg(ctx.CurrAsmList,OP_NEG,location.size,hreg1,hreg1);

{$ifndef cpu64bitalu}
        if (location.size in [OS_64,OS_S64]) then
          begin
            location.register64.reglo:=hreg1;
            location.register64.reghi:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
            if (is_cbool(resultdef)) then
              { reglo is either 0 or -1 -> reghi has to become the same }
              ctx.cg.a_load_reg_reg(ctx.CurrAsmList,OS_32,OS_32,location.register64.reglo,location.register64.reghi)
            else
              { unsigned }
              ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,location.register64.reghi);
          end
        else
{$endif cpu64bitalu}
          location.register:=hreg1;
      end;


    function tcputypeconvnode.first_int_to_real: tnode;
      var
        fname: string[19];
      begin
        if (cs_fp_emulation in compiler.globals.current_settings.moduleswitches) or
          (compiler.globals.current_settings.fputype=fpu_soft) or
          not(FPUXTENSA_SINGLE in fpu_capabilities[compiler.globals.current_settings.fputype]) or
          ((is_double(resultdef)) and not(FPUXTENSA_DOUBLE in fpu_capabilities[compiler.globals.current_settings.fputype])) or
          is_64bitint(left.resultdef) or
          is_currency(left.resultdef) or
          (is_32bit(left.resultdef) and not(is_signed(left.resultdef))) then
          result:=inherited first_int_to_real
        else
          begin
            { other integers are supposed to be 32 bit }
            inserttypeconv(left,compiler.deftypes.s32inttype,compiler);
            firstpass(left);
            result:=nil;
            expectloc:=LOC_FPUREGISTER;
          end;
      end;


    procedure tcputypeconvnode.second_int_to_real(ctx:tpassgeneratecodecontext);
      var
        ai: taicpu;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);
        ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,compiler.deftypes.s32inttype,true);
        ai:=taicpu.op_reg_reg_const(A_FLOAT,location.register,left.location.register,0);
        ai.oppostfix:=PF_S;
        ctx.CurrAsmList.concat(ai);
      end;

begin
  ctypeconvnode:=tcputypeconvnode;
end.
