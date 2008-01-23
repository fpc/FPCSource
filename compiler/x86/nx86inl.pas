{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86 inline nodes

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
unit nx86inl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl,ncginl;

    type
       tx86inlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
          function first_pi: tnode ; override;
          function first_arctan_real: tnode; override;
          function first_abs_real: tnode; override;
          function first_sqr_real: tnode; override;
          function first_sqrt_real: tnode; override;
          function first_ln_real: tnode; override;
          function first_cos_real: tnode; override;
          function first_sin_real: tnode; override;
          function first_round_real: tnode; override;
          function first_trunc_real: tnode; override;
          { second pass override to generate these nodes }
          procedure second_IncludeExclude;override;
          procedure second_pi; override;
          procedure second_arctan_real; override;
          procedure second_abs_real; override;
          procedure second_round_real; override;
          procedure second_sqr_real; override;
          procedure second_sqrt_real; override;
          procedure second_ln_real; override;
          procedure second_cos_real; override;
          procedure second_sin_real; override;
          procedure second_trunc_real; override;

          procedure second_prefetch;override;

          procedure second_abs_long;override;
       private
          procedure load_fpu_location;
       end;

implementation

    uses
      systems,
      globtype,globals,
      cutils,verbose,
      symconst,
      defutil,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symdef,
      cgbase,pass_2,
      cpuinfo,cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,ncgutil,
      tgobj,
      cga,cgutils,cgx86,cgobj;


{*****************************************************************************
                              TX86INLINENODE
*****************************************************************************}

     function tx86inlinenode.first_pi : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_pi := nil;
      end;


     function tx86inlinenode.first_arctan_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_arctan_real := nil;
      end;

     function tx86inlinenode.first_abs_real : tnode;
       begin
         if use_sse(resultdef) then
           expectloc:=LOC_MMREGISTER
         else
           expectloc:=LOC_FPUREGISTER;
        first_abs_real := nil;
      end;

     function tx86inlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqr_real := nil;
      end;

     function tx86inlinenode.first_sqrt_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sqrt_real := nil;
      end;

     function tx86inlinenode.first_ln_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_ln_real := nil;
      end;

     function tx86inlinenode.first_cos_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_cos_real := nil;
      end;

     function tx86inlinenode.first_sin_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        first_sin_real := nil;
      end;


     function tx86inlinenode.first_round_real : tnode;
      begin
{$ifdef x86_64}
        if use_sse(left.resultdef) then
          expectloc:=LOC_REGISTER
        else
{$endif x86_64}
          expectloc:=LOC_REFERENCE;
        result:=nil;
      end;


     function tx86inlinenode.first_trunc_real: tnode;
       begin
         if (cs_opt_size in current_settings.optimizerswitches)
{$ifdef x86_64}
           and not(use_sse(left.resultdef))
{$endif x86_64}
           then
           result:=inherited
         else
           begin
{$ifdef x86_64}
             if use_sse(left.resultdef) then
               expectloc:=LOC_REGISTER
             else
{$endif x86_64}
               expectloc:=LOC_REFERENCE;
             result:=nil;
           end;
       end;


     procedure tx86inlinenode.second_Pi;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         emit_none(A_FLDPI,S_NO);
         tcgx86(cg).inc_fpu_stack;
         location.register:=NR_FPU_RESULT_REG;
       end;

     { load the FPU into the an fpu register }
     procedure tx86inlinenode.load_fpu_location;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         location.register:=NR_FPU_RESULT_REG;
         secondpass(left);
         case left.location.loc of
           LOC_FPUREGISTER:
             ;
           LOC_CFPUREGISTER:
             begin
               cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,left.location.size,
                 left.location.size,left.location.register,location.register);
             end;
           LOC_REFERENCE,LOC_CREFERENCE:
             begin
               cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,
                  left.location.size,left.location.size,
                  left.location.reference,location.register);
             end;
           LOC_MMREGISTER,LOC_CMMREGISTER:
             begin
               location:=left.location;
               location_force_fpureg(current_asmdata.CurrAsmList,location,false);
             end;
           else
             internalerror(309991);
         end;
       end;


     procedure tx86inlinenode.second_arctan_real;
       begin
         load_fpu_location;
         emit_none(A_FLD1,S_NO);
         emit_none(A_FPATAN,S_NO);
       end;


     procedure tx86inlinenode.second_abs_real;
       var
         href : treference;
       begin
         if use_sse(resultdef) then
           begin
             secondpass(left);
             location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
             location:=left.location;
             case tfloatdef(resultdef).floattype of
               s32real:
                 reference_reset_symbol(href,current_asmdata.RefAsmSymbol('FPC_ABSMASK_SINGLE'),0);
               s64real:
                 reference_reset_symbol(href,current_asmdata.RefAsmSymbol('FPC_ABSMASK_DOUBLE'),0);
               else
                 internalerror(200506081);
             end;
             current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_ANDPS,S_XMM,href,location.register))
           end
         else
           begin
             load_fpu_location;
             emit_none(A_FABS,S_NO);
           end;
       end;


     procedure tx86inlinenode.second_round_real;
       begin
{$ifdef x86_64}
         if use_sse(left.resultdef) then
           begin
             secondpass(left);
             location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
             location_reset(location,LOC_REGISTER,OS_S64);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_S64);
             case left.location.size of
               OS_F32:
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVTSS2SI,S_Q,left.location.register,location.register));
               OS_F64:
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVTSD2SI,S_Q,left.location.register,location.register));
               else
                 internalerror(2007031402);
             end;
           end
         else
{$endif x86_64}
          begin
            load_fpu_location;
            location_reset(location,LOC_REFERENCE,OS_S64);
            tg.GetTempTyped(current_asmdata.CurrAsmList,resultdef,tt_normal,location.reference);
            emit_ref(A_FISTP,S_IQ,location.reference);
            emit_none(A_FWAIT,S_NO);
           end;
       end;


     procedure tx86inlinenode.second_trunc_real;
       var
         oldcw,newcw : treference;
       begin
{$ifdef x86_64}
         if use_sse(left.resultdef) and
           not((left.location.loc=LOC_FPUREGISTER) and (current_settings.fputype>=fpu_sse3)) then
           begin
             secondpass(left);
             location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
             location_reset(location,LOC_REGISTER,OS_S64);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_S64);
             case left.location.size of
               OS_F32:
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVTTSS2SI,S_Q,left.location.register,location.register));
               OS_F64:
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CVTTSD2SI,S_Q,left.location.register,location.register));
               else
                 internalerror(2007031401);
             end;
           end
         else
{$endif x86_64}
          begin
            if (current_settings.fputype>=fpu_sse3) then
              begin
                load_fpu_location;
                location_reset(location,LOC_REFERENCE,OS_S64);
                tg.GetTempTyped(current_asmdata.CurrAsmList,resultdef,tt_normal,location.reference);
                emit_ref(A_FISTTP,S_IQ,location.reference);
              end
            else
              begin
                tg.GetTemp(current_asmdata.CurrAsmList,2,tt_normal,oldcw);
                tg.GetTemp(current_asmdata.CurrAsmList,2,tt_normal,newcw);
                emit_ref(A_FNSTCW,S_NO,newcw);
                emit_ref(A_FNSTCW,S_NO,oldcw);
                emit_const_ref(A_OR,S_W,$0f00,newcw);
                load_fpu_location;
                emit_ref(A_FLDCW,S_NO,newcw);
                location_reset(location,LOC_REFERENCE,OS_S64);
                tg.GetTempTyped(current_asmdata.CurrAsmList,resultdef,tt_normal,location.reference);
                emit_ref(A_FISTP,S_IQ,location.reference);
                emit_ref(A_FLDCW,S_NO,oldcw);
                emit_none(A_FWAIT,S_NO);
                tg.UnGetTemp(current_asmdata.CurrAsmList,oldcw);
                tg.UnGetTemp(current_asmdata.CurrAsmList,newcw);
              end;
           end;
       end;


     procedure tx86inlinenode.second_sqr_real;

       begin
         if use_sse(resultdef) then
           begin
             secondpass(left);
             location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
             location:=left.location;
             cg.a_opmm_loc_reg(current_asmdata.CurrAsmList,OP_MUL,left.location.size,left.location,left.location.register,mms_movescalar);
           end
         else
           begin
             load_fpu_location;
             emit_reg_reg(A_FMUL,S_NO,NR_ST0,NR_ST0);
           end;
       end;


     procedure tx86inlinenode.second_sqrt_real;
       begin
         if use_sse(resultdef) then
           begin
             secondpass(left);
             location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
             location:=left.location;
             case tfloatdef(resultdef).floattype of
               s32real:
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_SQRTSS,S_XMM,location.register,location.register));
               s64real:
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_SQRTSD,S_XMM,location.register,location.register));
               else
                 internalerror(200510031);
             end;
           end
         else
           begin
             load_fpu_location;
             emit_none(A_FSQRT,S_NO);
           end;
       end;

     procedure tx86inlinenode.second_ln_real;
       begin
         load_fpu_location;
         emit_none(A_FLDLN2,S_NO);
         emit_none(A_FXCH,S_NO);
         emit_none(A_FYL2X,S_NO);
       end;

     procedure tx86inlinenode.second_cos_real;
       begin
         load_fpu_location;
         emit_none(A_FCOS,S_NO);
       end;

     procedure tx86inlinenode.second_sin_real;
       begin
         load_fpu_location;
         emit_none(A_FSIN,S_NO)
       end;

     procedure tx86inlinenode.second_prefetch;
       var
         ref : treference;
         r : tregister;
       begin
{$ifdef i386}
         if current_settings.cputype>=cpu_Pentium3 then
{$endif i386}
           begin
             secondpass(left);
             case left.location.loc of
               LOC_CREFERENCE,
               LOC_REFERENCE:
                 begin
                   r:=cg.getintregister(current_asmdata.CurrAsmList,OS_ADDR);
                   cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,r);
                   reference_reset_base(ref,r,0);
                   current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_PREFETCHNTA,S_NO,ref));
                 end;
               else
                 internalerror(200402021);
             end;
           end;
       end;


    procedure tx86inlinenode.second_abs_long;
      var
        hregister : tregister;
        opsize : tcgsize;
        hp : taicpu;
      begin
{$ifdef i386}
        if current_settings.cputype<cpu_Pentium2 then
          begin
            opsize:=int_cgsize(left.resultdef.size);
            secondpass(left);
            location_force_reg(current_asmdata.CurrAsmList,left.location,opsize,false);
            location:=left.location;
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
            emit_reg_reg(A_MOV,S_L,left.location.register,location.register);
            emit_const_reg(A_SAR,tcgsize2opsize[opsize],31,left.location.register);
            emit_reg_reg(A_XOR,S_L,left.location.register,location.register);
            emit_reg_reg(A_SUB,S_L,left.location.register,location.register);
          end
        else
{$endif i386}
          begin
            opsize:=int_cgsize(left.resultdef.size);
            secondpass(left);
            location_force_reg(current_asmdata.CurrAsmList,left.location,opsize,true);
            hregister:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
            location:=left.location;
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.register,hregister);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.register,location.register);
            emit_reg(A_NEG,tcgsize2opsize[opsize],hregister);
            hp:=taicpu.op_reg_reg(A_CMOVcc,tcgsize2opsize[opsize],hregister,location.register);
            hp.condition:=C_NS;
            current_asmdata.CurrAsmList.concat(hp);
          end;
      end;

{*****************************************************************************
                     INCLUDE/EXCLUDE GENERIC HANDLING
*****************************************************************************}

      procedure tx86inlinenode.second_IncludeExclude;
        var
         hregister,
         hregister2: tregister;
         setbase   : aint;
         bitsperop,l : longint;
         cgop : topcg;
         asmop : tasmop;
         opsize,
         orgsize: tcgsize;
        begin
          if not(is_varset(tcallparanode(left).resultdef)) then
            opsize:=int_cgsize(tcallparanode(left).resultdef.size)
          else
            opsize:=OS_32;
          bitsperop:=(8*tcgsize2size[opsize]);
          secondpass(tcallparanode(left).left);
          secondpass(tcallparanode(tcallparanode(left).right).left);
          setbase:=tsetdef(tcallparanode(left).left.resultdef).setbase;
          if tcallparanode(tcallparanode(left).right).left.location.loc=LOC_CONSTANT then
            begin
              { calculate bit position }
              l:=1 shl ((tcallparanode(tcallparanode(left).right).left.location.value-setbase) mod bitsperop);

              { determine operator }
              if inlinenumber=in_include_x_y then
                cgop:=OP_OR
              else
                begin
                  cgop:=OP_AND;
                  l:=not(l);
                end;
              case tcallparanode(left).left.location.loc of
                LOC_REFERENCE :
                  begin
                    inc(tcallparanode(left).left.location.reference.offset,
                      ((tcallparanode(tcallparanode(left).right).left.location.value-setbase) div bitsperop)*tcgsize2size[opsize]);
                    cg.a_op_const_ref(current_asmdata.CurrAsmList,cgop,opsize,l,tcallparanode(left).left.location.reference);
                  end;
                LOC_CREGISTER :
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,cgop,tcallparanode(left).left.location.size,l,tcallparanode(left).left.location.register);
                else
                  internalerror(200405022);
              end;
            end
          else
            begin
              orgsize:=opsize;
              if opsize in [OS_8,OS_S8] then
                begin
                  opsize:=OS_32;
                end;
              { determine asm operator }
              if inlinenumber=in_include_x_y then
                 asmop:=A_BTS
              else
                 asmop:=A_BTR;

              location_force_reg(current_asmdata.CurrAsmList,tcallparanode(tcallparanode(left).right).left.location,opsize,true);
              register_maybe_adjust_setbase(current_asmdata.CurrAsmList,tcallparanode(tcallparanode(left).right).left.location,setbase);
              hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
              if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                emit_reg_ref(asmop,tcgsize2opsize[opsize],hregister,tcallparanode(left).left.location.reference)
              else
                begin
                  { second argument can't be an 8 bit register either }
                  hregister2:=tcallparanode(left).left.location.register;
                  if (orgsize in [OS_8,OS_S8]) then
                    hregister2:=cg.makeregsize(current_asmdata.CurrAsmList,hregister2,opsize);
                  emit_reg_reg(asmop,tcgsize2opsize[opsize],hregister,hregister2);
                end;
            end;
        end;


end.
