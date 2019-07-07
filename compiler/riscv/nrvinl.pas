{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate Risc-V32/64 inline nodes

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
unit nrvinl;

{$i fpcdefs.inc}

interface

    uses
       cpubase,
       node,ninl,ncginl;

    type

       { trvinlinenode }

       trvinlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
          function first_sqrt_real: tnode; override;
          function first_abs_real: tnode; override;
          function first_sqr_real: tnode; override;
          function first_round_real: tnode; override;
          function first_trunc_real: tnode; override;

          function first_fma: tnode; override;

          procedure second_sqrt_real; override;
          procedure second_abs_real; override;
          procedure second_sqr_real; override;
          procedure second_round_real; override;
          procedure second_trunc_real; override;

          procedure second_fma; override;
       protected
          procedure load_fpu_location;
       end;

implementation

    uses
      ncal,
      cutils,globals,verbose,globtype,
      aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      defutil,
      cgbase,pass_2,
      cpuinfo,ncgutil,
      hlcgobj,cgutils,cgobj,rgobj,tgobj;


{*****************************************************************************
                              trvinlinenode
*****************************************************************************}

     function trvinlinenode.first_sqrt_real : tnode;
       begin
         if (current_settings.fputype >= fpu_fd) then
           begin
             expectloc:=LOC_FPUREGISTER;
             first_sqrt_real := nil;
           end
         else
           result:=inherited first_sqrt_real;
       end;


     function trvinlinenode.first_abs_real : tnode;
       begin
         if (current_settings.fputype >= fpu_fd) then
           begin
             expectloc:=LOC_FPUREGISTER;
             first_abs_real := nil;
           end
         else
           result:=inherited first_abs_real;
       end;


     function trvinlinenode.first_sqr_real : tnode;
       begin
         if (current_settings.fputype >= fpu_fd) then
           begin
             expectloc:=LOC_FPUREGISTER;
             first_sqr_real := nil;
           end
         else
           result:=inherited first_sqr_real;
       end;


     function trvinlinenode.first_round_real: tnode;
       begin
         if (current_settings.fputype >= fpu_fd) then
           begin
             expectloc:=LOC_FPUREGISTER;
             first_round_real := nil;
           end
         else
           result:=inherited first_round_real;
       end;


     function trvinlinenode.first_trunc_real: tnode;
       begin
         if (current_settings.fputype >= fpu_fd) then
           begin
             expectloc:=LOC_FPUREGISTER;
             first_trunc_real := nil;
           end
         else
           result:=inherited first_trunc_real;
       end;


     function trvinlinenode.first_fma: tnode;
       begin
         Result:=nil;
       end;


     { load the FPU into the an fpu register }
     procedure trvinlinenode.load_fpu_location;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location.loc := LOC_FPUREGISTER;
         location.register := cg.getfpuregister(current_asmdata.CurrAsmList,def_cgsize(resultdef));
       end;


    procedure trvinlinenode.second_sqrt_real;
      begin
        location.loc:=LOC_FPUREGISTER;
        load_fpu_location;
        case left.location.size of
          OS_F32:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRT_S,location.register,
                left.location.register));
              cg.g_check_for_fpu_exception(current_asmdata.CurrAsmList);
            end;
          OS_F64:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FSQRT_D,location.register,
                left.location.register));
              cg.g_check_for_fpu_exception(current_asmdata.CurrAsmList);
             end
          else
            inherited;
        end;
      end;


     procedure trvinlinenode.second_abs_real;
       var
         op: TAsmOp;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         if (left.location.size = OS_F32) then
           op := A_FSGNJX_S
         else
           op := A_FSGNJX_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,left.location.register));
       end;


     procedure trvinlinenode.second_sqr_real;
       var
         op: tasmop;
       begin
         location.loc:=LOC_FPUREGISTER;
         load_fpu_location;
         if (left.location.size = OS_F32) then
           op := A_FMUL_S
         else
           op := A_FMUL_D;
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,location.register,left.location.register,left.location.register));
         cg.g_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


     procedure trvinlinenode.second_round_real;
       var
         op: TAsmOp;
       begin
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
{$ifdef RISCV32}
         if (location.size in [OS_S64,OS_64]) then
           begin
             location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
             location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
           end
         else
           location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
{$else}
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
{$endif}
         { convert to signed integer rounding towards zero (there's no "round to
           integer using current rounding mode") }

{$ifdef RISCV32}
         if (left.location.size = OS_F32) then
           op := A_FCVT_W_S
         else
           op := A_FCVT_W_D;
{$else}
         if (left.location.size = OS_F32) then
           op := A_FCVT_L_S
         else
           op := A_FCVT_L_D;
{$endif}

         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,location.register,left.location.register));
         cg.g_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


     procedure trvinlinenode.second_trunc_real;
       var
         op: TAsmOp;
       begin
         secondpass(left);
         hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
{$ifdef RISCV32}
         if (location.size in [OS_S64,OS_64]) then
           begin
             location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
             location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
           end
         else
           location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
{$else}
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
{$endif}
         { convert to signed integer rounding towards zero (there's no "round to
           integer using current rounding mode") }

{$ifdef RISCV32}
         if (left.location.size = OS_F32) then
           op := A_FCVT_W_S
         else
           op := A_FCVT_W_D;
{$else}
         if (left.location.size = OS_F32) then
           op := A_FCVT_L_S
         else
           op := A_FCVT_L_D;
{$endif}

         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_roundingmode(op,location.register,left.location.register,RM_RTZ));
         cg.g_check_for_fpu_exception(current_asmdata.CurrAsmList);
       end;


     procedure trvinlinenode.second_fma;
       const
         op : array[os_f32..os_f64,false..true,false..true] of TAsmOp =
           (
            (
             (A_FMADD_S,A_FMSUB_S),
             (A_FNMADD_S,A_FNMSUB_S)
            ),
            (
             (A_FMADD_D,A_FMSUB_D),
             (A_FNMADD_D,A_FNMSUB_D)
            )
           );
       var
         paraarray : array[1..3] of tnode;
         i : integer;
         negop3,
         negproduct : boolean;
       begin
         if current_settings.fputype in [fpu_fd] then
           begin
             negop3:=false;
             negproduct:=false;
             paraarray[1]:=tcallparanode(tcallparanode(tcallparanode(parameters).nextpara).nextpara).paravalue;
             paraarray[2]:=tcallparanode(tcallparanode(parameters).nextpara).paravalue;
             paraarray[3]:=tcallparanode(parameters).paravalue;

             { check if a neg. node can be removed
               this is possible because changing the sign of
               a floating point number does not affect its absolute
               value in any way
             }
             if paraarray[1].nodetype=unaryminusn then
               begin
                 paraarray[1]:=tunarynode(paraarray[1]).left;
                 { do not release the unused unary minus node, it is kept and release together with the other nodes,
                   only no code is generated for it }
                 negproduct:=not(negproduct);
               end;

             if paraarray[2].nodetype=unaryminusn then
               begin
                 paraarray[2]:=tunarynode(paraarray[2]).left;
                 { do not release the unused unary minus node, it is kept and release together with the other nodes,
                   only no code is generated for it }
                 negproduct:=not(negproduct);
               end;

             if paraarray[3].nodetype=unaryminusn then
               begin
                 paraarray[3]:=tunarynode(paraarray[3]).left;
                 { do not release the unused unary minus node, it is kept and release together with the other nodes,
                   only no code is generated for it }
                 negop3:=true;
               end;

              for i:=1 to 3 do
               secondpass(paraarray[i]);

             { no memory operand is allowed }
             for i:=1 to 3 do
               begin
                 if not(paraarray[i].location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
                   hlcg.location_force_fpureg(current_asmdata.CurrAsmList,paraarray[i].location,paraarray[i].resultdef,true);
               end;

             location_reset(location,LOC_FPUREGISTER,paraarray[1].location.size);
             location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);

             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_reg(op[def_cgsize(resultdef), negproduct,negop3],location.register,paraarray[1].location.register,paraarray[2].location.register,paraarray[2].location.register));
             cg.g_check_for_fpu_exception(current_asmdata.CurrAsmList);
           end
         else
           internalerror(2014032301);
       end;


begin
   cinlinenode:=trvinlinenode;
end.
