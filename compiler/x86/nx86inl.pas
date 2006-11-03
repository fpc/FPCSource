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
          { second pass override to generate these nodes }
          procedure second_IncludeExclude;override;
          procedure second_pi; override;
          procedure second_arctan_real; override;
          procedure second_abs_real; override;
          procedure second_sqr_real; override;
          procedure second_sqrt_real; override;
          procedure second_ln_real; override;
          procedure second_cos_real; override;
          procedure second_sin_real; override;

          procedure second_prefetch;override;
       private
          procedure load_fpu_location;
       end;

implementation

    uses
      systems,
      globals,
      cutils,verbose,
      symconst,
      defutil,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symdef,
      cgbase,pass_2,
      cpuinfo,cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,ncgutil,
      cga,cgutils,cgx86,cgobj;


{*****************************************************************************
                              TX86INLINENODE
*****************************************************************************}

     function tx86inlinenode.first_pi : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersfpu:=1;
        first_pi := nil;
      end;


     function tx86inlinenode.first_arctan_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,2);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_arctan_real := nil;
      end;

     function tx86inlinenode.first_abs_real : tnode;
       begin
         if use_sse(resultdef) then
           begin
             expectloc:=LOC_MMREGISTER;
             registersmm:=max(left.registersmm,1);
           end
         else
           begin
             expectloc:=LOC_FPUREGISTER;
             registersfpu:=max(left.registersfpu,1);
          end;
        registersint:=left.registersint;
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_abs_real := nil;
      end;

     function tx86inlinenode.first_sqr_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sqr_real := nil;
      end;

     function tx86inlinenode.first_sqrt_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sqrt_real := nil;
      end;

     function tx86inlinenode.first_ln_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,2);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_ln_real := nil;
      end;

     function tx86inlinenode.first_cos_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_cos_real := nil;
      end;

     function tx86inlinenode.first_sin_real : tnode;
      begin
        expectloc:=LOC_FPUREGISTER;
        registersint:=left.registersint;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sin_real := nil;
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
                 left.location.register,location.register);
             end;
           LOC_REFERENCE,LOC_CREFERENCE:
             begin
               cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,
                  def_cgsize(left.resultdef),
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

{*****************************************************************************
                     INCLUDE/EXCLUDE GENERIC HANDLING
*****************************************************************************}

      procedure tx86inlinenode.second_IncludeExclude;
        var
         hregister : tregister;
         asmop : tasmop;
         bitsperop,l : longint;
         cgop : topcg;
         opsize : tcgsize;
        begin
          opsize:=OS_32;
          bitsperop:=(8*tcgsize2size[opsize]);
          secondpass(tcallparanode(left).left);
          if tcallparanode(tcallparanode(left).right).left.nodetype=ordconstn then
            begin
              { calculate bit position }
              l:=1 shl (tordconstnode(tcallparanode(tcallparanode(left).right).left).value mod bitsperop);

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
                      (tordconstnode(tcallparanode(tcallparanode(left).right).left).value div bitsperop)*tcgsize2size[opsize]);
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
              { generate code for the element to set }
              secondpass(tcallparanode(tcallparanode(left).right).left);
              { determine asm operator }
              if inlinenumber=in_include_x_y then
                 asmop:=A_BTS
              else
                 asmop:=A_BTR;

              if tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                { we don't need a mod 32 because this is done automatically  }
                { by the bts instruction. For proper checking we would       }

                { note: bts doesn't do any mod'ing, that's why we can also use }
                { it for normalsets! (JM)                                      }

                { need a cmp and jmp, but this should be done by the         }
                { type cast code which does range checking if necessary (FK) }
                hregister:=cg.makeregsize(current_asmdata.CurrAsmList,Tcallparanode(Tcallparanode(left).right).left.location.register,opsize)
              else
                hregister:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
              cg.a_load_loc_reg(current_asmdata.CurrAsmList,opsize,tcallparanode(tcallparanode(left).right).left.location,hregister);
              if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                emit_reg_ref(asmop,tcgsize2opsize[opsize],hregister,tcallparanode(left).left.location.reference)
              else
                emit_reg_reg(asmop,tcgsize2opsize[opsize],hregister,tcallparanode(left).left.location.register);
            end;
        end;


end.
