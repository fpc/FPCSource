{
    Copyright (c) 2015 by the Free Pascal Development team

    Generates Motorola 68k inline nodes

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
unit n68kinl;

{$i fpcdefs.inc}

interface

    uses
      node,ninl,ncginl,cpubase;

    type
      t68kinlinenode = class(tcgInlineNode)
        function first_abs_real: tnode; override;
        function first_sqr_real: tnode; override;
        function first_sqrt_real: tnode; override;
        {function first_arctan_real: tnode; override;
        function first_ln_real: tnode; override;
        function first_cos_real: tnode; override;
        function first_sin_real: tnode; override;}

        procedure second_abs_real; override;
        procedure second_sqr_real; override;
        procedure second_sqrt_real; override;
        {procedure second_arctan_real; override;
        procedure second_ln_real; override;
        procedure second_cos_real; override;
        procedure second_sin_real; override;
        procedure second_prefetch; override;
        procedure second_abs_long; override;}
      private
        procedure second_do_operation(op: TAsmOp);
      end;


implementation

    uses
      globtype,verbose,globals,cutils,
      cpuinfo,defutil,symdef,aasmdata,aasmcpu,aasmtai,
      cgbase,cgutils,pass_1,pass_2,
      ncgutil,cgobj,cgcpu,hlcgobj;

{*****************************************************************************
                              t68kinlinenode
*****************************************************************************}

    function t68kinlinenode.first_abs_real : tnode;
      begin
        if (cs_fp_emulation in current_settings.moduleswitches) then
          result:=inherited first_abs_real
        else
          begin
            case current_settings.fputype of
              fpu_68881:
                expectloc:=LOC_FPUREGISTER;
              else
                internalerror(2015022206);
            end;
            first_abs_real:=nil;
          end;
      end;

    function t68kinlinenode.first_sqr_real : tnode;
      begin
        if (cs_fp_emulation in current_settings.moduleswitches) then
          result:=inherited first_sqr_real
        else
          begin
            case current_settings.fputype of
              fpu_68881:
                expectloc:=LOC_FPUREGISTER;
              else
                internalerror(2015022201);
            end;
            first_sqr_real:=nil;
          end;
      end;

    function t68kinlinenode.first_sqrt_real : tnode;
      begin
        if (cs_fp_emulation in current_settings.moduleswitches) then
          result:=inherited first_sqrt_real
        else
          begin
            case current_settings.fputype of
              fpu_68881:
                expectloc:=LOC_FPUREGISTER;
              else
                internalerror(2015022203);
            end;
            first_sqrt_real:=nil;
          end;
      end;

    procedure t68kinlinenode.second_abs_real;
      begin
        //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('second_abs_real called!')));
        second_do_operation(A_FABS);
      end;

    procedure t68kinlinenode.second_sqr_real;
      begin
        secondpass(left);
        case current_settings.fputype of
          fpu_68881:
            begin
              //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('second_sqr_real called!')));
              hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);
              location_copy(location,left.location);
              if left.location.loc=LOC_CFPUREGISTER then
                begin
                  //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('second_srq_real called!: left was cfpuregister!')));
                  location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                  location.loc := LOC_FPUREGISTER;
                  cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmlist,OS_NO,OS_NO,left.location.register,location.register);
                end;
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FMUL,S_FX,left.location.register,location.register));
            end;
        else
          internalerror(2015022202);
        end;
      end;

    procedure t68kinlinenode.second_sqrt_real;
      begin
        //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('second_sqrt_real called!')));
        second_do_operation(A_FSQRT);
      end;

    procedure t68kinlinenode.second_do_operation(op: TAsmOp);
      var
        href: TReference;
      begin
        secondpass(left);
        case current_settings.fputype of
          fpu_68881:
            begin
              location_reset(location,LOC_FPUREGISTER,left.location.size);

              case left.location.loc of
                LOC_FPUREGISTER:
                  begin
                    location.register:=left.location.register;
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg(op,S_FX,location.register))
                  end;
                LOC_CFPUREGISTER:
                  begin
                    location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,S_FX,left.location.register,location.register));
                  end;
                LOC_REFERENCE,LOC_CREFERENCE:
                  begin
                    location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                    href:=left.location.reference;
                    tcg68k(cg).fixref(current_asmdata.CurrAsmList,href);
                    current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(op,tcgsize2opsize[left.location.size],href,location.register));
                  end;
                else
                  internalerror(2015022205);
              end;
            end;
        else
          internalerror(2015022204);
        end;
      end;

      { ideas for second_abs_long (KB) }

      { This is probably faster on 68000 than the generic implementation,
        because shifting is slow on the original 68000, maybe also on the 68020?
        Also needs to be tested on 040/060. This can also work on a CF.
        input - d0, output - d2
        move.l d0,d2
        btst   #31,d2
        sne    d1
        extb.l d1 (or ext.w + ext.l on 68000)
        eor.l  d1,d2
        sub.l  d1,d2
      }

      { Solution using bitfield extraction, we don't support the necessary asm
        construct for this yet, probably this is the fastest on 020, slower on
        040/060 than the one above, doesn't work on '000 or CF.
        input - d0, output - d2
        move.l  d0,d2
        bfexts  d0[0:1],d1
        eor.l   d1,d2
        sub.l   d1,d2
      }
begin
  cinlinenode:=t68kinlinenode;
end.
