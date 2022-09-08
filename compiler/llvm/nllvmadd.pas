{
    Copyright (c) 2013 by Jonas Maebe

    Generate LLVM bytecode for add nodes

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
unit nllvmadd;

{$i fpcdefs.inc}

interface

    uses
      node,
      ncgadd;

    type
      tllvmaddnode = class(tcgaddnode)
       public
        function pass_1: tnode; override;
        procedure force_reg_left_right(allow_swap, allow_constant: boolean); override;
       protected
        procedure second_cmpsmallset; override;
        procedure second_cmpordinal; override;
        procedure second_add64bit; override;
        procedure second_cmp64bit; override;
        procedure second_addfloat; override;
        procedure second_cmpfloat; override;
        procedure second_opvector; override;
      end;


implementation

     uses
       verbose,globtype,globals,cutils,
       aasmdata,
       symconst,symtype,symdef,defutil,
       llvmbase,aasmllvm,aasmllvmmetadata,
       cgbase,cgutils,pass_1,
       hlcgobj,
       nadd,ncal,ncnv,ncon
       ;

{ tllvmaddnode }

  function tllvmaddnode.pass_1: tnode;
    var
      exceptmode: ansistring;
      intrname: string;
      iscompcurrency: boolean;
    begin
      result:=inherited pass_1;
      if not assigned(result) and
         is_fpu(left.resultdef) and
         not(cs_opt_fastmath in current_settings.optimizerswitches) then
        begin
          case nodetype of
            addn:
              begin
                intrname:='LLVM_EXPERIMENTAL_CONSTRAINED_FADD';
              end;
            subn:
              begin
                intrname:='LLVM_EXPERIMENTAL_CONSTRAINED_FSUB';
              end;
            muln:
              begin
                intrname:='LLVM_EXPERIMENTAL_CONSTRAINED_FMUL';
              end;
            slashn:
              begin
                intrname:='LLVM_EXPERIMENTAL_CONSTRAINED_FDIV';
              end;
            else
              begin
                intrname:='';
              end;
          end;
          if intrname<>'' then
            begin
              iscompcurrency:=tfloatdef(left.resultdef).floattype in [s64currency,s64comp];
              if iscompcurrency then
                begin
                  inserttypeconv_internal(left,s80floattype);
                  inserttypeconv_internal(right,s80floattype);
                end;
              exceptmode:=llvm_constrainedexceptmodestring;
              result:=ccallnode.createintern(intrname,
                ccallparanode.create(cstringconstnode.createpchar(ansistring2pchar(exceptmode),length(exceptmode),llvm_metadatatype),
                  ccallparanode.create(cstringconstnode.createpchar(ansistring2pchar('round.dynamic'),length('round.dynamic'),llvm_metadatatype),
                    ccallparanode.create(right,
                      ccallparanode.create(left,nil)
                    )
                  )
                )
              );
              if iscompcurrency then
                begin
                  result:=ctypeconvnode.create_internal(result,resultdef);
                end;
              left:=nil;
              right:=nil;
              exit;
            end;
        end;
      { there are no flags in LLVM }
      if expectloc=LOC_FLAGS then
        expectloc:=LOC_REGISTER;
    end;


  procedure tllvmaddnode.force_reg_left_right(allow_swap, allow_constant: boolean);
    begin
      { comparison with pointer -> no immediate, as icmp can't handle pointer
        immediates (except for nil as "null", but we don't generate that) }
      if (nodetype in [equaln,unequaln,gtn,gten,ltn,lten]) and
         (is_address(left.resultdef) or
          is_address(right.resultdef)) then
        allow_constant:=false;
      inherited;
      { pointer - pointer = integer -> make all defs integer since we can't
        subtract pointers }
      if (nodetype=subn) and
         is_address(left.resultdef) and
         is_address(right.resultdef) then
        begin
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
          hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,resultdef,true);
        end
      { pointer +/- integer -> make defs the same since a_op_* only gets a
        single type as argument }
      else if (nodetype in [addn,subn]) and
              (is_address(left.resultdef)<>is_address(right.resultdef)) then
        begin
          { the result is a pointerdef -> typecast both arguments to pointer;
            a_op_*_reg will convert them back to integer as needed }
          if not is_address(left.resultdef) then
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
          if not is_address(right.resultdef) then
            hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,resultdef,true);
        end;
    end;


  procedure tllvmaddnode.second_cmpsmallset;
    var
      tmpreg,
      tmpreg2: tregister;
      cmpop : topcmp;
    begin
      pass_left_right;

      location_reset(location,LOC_REGISTER,OS_8);
      location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,llvmbool1type);

      force_reg_left_right(false,false);

      case nodetype of
        equaln,
        unequaln:
          begin
            if nodetype=equaln then
              cmpop:=OC_EQ
            else
              cmpop:=OC_NE;
            current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_reg(la_icmp,
              location.register,cmpop,left.resultdef,left.location.register,right.location.register));
          end;
        lten,
        gten:
          begin
            if (not(nf_swapped in flags) and
                (nodetype = lten)) or
               ((nf_swapped in flags) and
                (nodetype = gten)) then
              swapleftright;
            { set1<=set2 <-> set2 and not(set1) = 0 }
            tmpreg:=hlcg.getintregister(current_asmdata.CurrAsmList,left.resultdef);
            hlcg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NOT,left.resultdef,left.location.register,tmpreg);
            tmpreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,left.resultdef);
            hlcg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_AND,left.resultdef,right.location.register,tmpreg,tmpreg2);
            current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_const(la_icmp,
              location.register,OC_EQ,left.resultdef,tmpreg2,0));
          end;
        else
          internalerror(2012042701);
      end;
      tmpreg:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
      hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,llvmbool1type,resultdef,location.register,tmpreg);
      location.register:=tmpreg;
    end;


  procedure tllvmaddnode.second_cmpordinal;
    var
      tmpreg: tregister;
      cmpop: topcmp;
      unsigned : boolean;
    begin
      pass_left_right;
      force_reg_left_right(true,true);

      unsigned:=not(is_signed(left.resultdef)) or
                not(is_signed(right.resultdef));

      case nodetype of
        ltn:
          if unsigned then
            cmpop:=OC_B
          else
            cmpop:=OC_LT;
        lten:
          if unsigned then
            cmpop:=OC_BE
          else
            cmpop:=OC_LTE;
        gtn:
          if unsigned then
            cmpop:=OC_A
          else
            cmpop:=OC_GT;
        gten:
          if unsigned then
            cmpop:=OC_AE
          else
            cmpop:=OC_GTE;
        equaln:
          cmpop:=OC_EQ;
        unequaln:
          cmpop:=OC_NE;
        else
          internalerror(2015031505);
      end;
      if nf_swapped in flags then
        cmpop:=swap_opcmp(cmpop);

      location_reset(location,LOC_REGISTER,OS_8);
      location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,llvmbool1type);

      if right.location.loc=LOC_CONSTANT then
        current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_const(la_icmp,
          location.register,cmpop,left.resultdef,left.location.register,right.location.value64))
      else
        current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_reg(la_icmp,
          location.register,cmpop,left.resultdef,left.location.register,right.location.register));

      tmpreg:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
      hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,llvmbool1type,resultdef,location.register,tmpreg);
      location.register:=tmpreg;
    end;


  procedure tllvmaddnode.second_add64bit;
    begin
      second_addordinal;
    end;


  procedure tllvmaddnode.second_cmp64bit;
    begin
      second_cmpordinal;
    end;


  procedure tllvmaddnode.second_addfloat;
    var
      tmpreg: tregister;
      op    : tllvmop;
      llvmfpcmp : tllvmfpcmp;
      size  : tdef;
    begin
      pass_left_right;

      { get the operands in the correct order; there are no special cases here,
        everything is register-based }
      if nf_swapped in flags then
        swapleftright;

      { put both operands in a register }
      hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,true);
      hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

      { see comment in thlcgllvm.a_loadfpu_ref_reg }
      if tfloatdef(left.resultdef).floattype in [s64comp,s64currency] then
        size:=sc80floattype
      else
        size:=left.resultdef;

      if nodetype in [ltn,lten,gtn,gten,equaln,unequaln] then
        begin
          case nodetype of
            ltn:
              llvmfpcmp:=lfc_olt;
            lten:
              llvmfpcmp:=lfc_ole;
            gtn:
              llvmfpcmp:=lfc_ogt;
            gten:
              llvmfpcmp:=lfc_oge;
            equaln:
              llvmfpcmp:=lfc_oeq;
            unequaln:
              llvmfpcmp:=lfc_une;
            else
              internalerror(2015031506);
          end;
          location_reset(location,LOC_REGISTER,OS_8);
          location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,llvmbool1type);

          current_asmdata.CurrAsmList.concat(taillvm.op_reg_fpcond_size_reg_reg(la_fcmp ,
            location.register,llvmfpcmp,size,left.location.register,right.location.register));
          tmpreg:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
          hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,llvmbool1type,resultdef,location.register,tmpreg);
          location.register:=tmpreg;
        end
      else
        begin
          case nodetype of
            addn :
              op:=la_fadd;
            muln :
              op:=la_fmul;
            subn :
              op:=la_fsub;
            slashn :
              op:=la_fdiv;
            else
              internalerror(2013102401);
          end;
          location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
          location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);

          current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_reg_reg(op,location.register,size,
            left.location.register,right.location.register))
        end;
    end;


  procedure tllvmaddnode.second_cmpfloat;
    begin
      second_addfloat;
    end;

  procedure tllvmaddnode.second_opvector;

    var
      lv, rv: tdef;
      hreg: tregister;
      tempref: treference;
      op: tllvmop;
      isfloat: boolean;
    begin
      if not is_vector(left.resultdef) or
         not is_vector(right.resultdef) or
         not is_vector(resultdef) or
         not tarraydef(resultdef).is_hwvector then
        internalerror(2022090710);

      pass_left_right;
      if (nf_swapped in flags) then
        swapleftright;

      isfloat:=tarraydef(left.resultdef).elementdef.typ=floatdef;
      case nodetype of
        addn :
          if isfloat then
            op:=la_fadd
          else
            op:=la_add;
        muln :
          if isfloat then
            op:=la_fmul
          else
            op:=la_mul;
        subn :
          if isfloat then
            op:=la_fsub
          else
            op:=la_sub;
        slashn :
          if isfloat then
            op:=la_fdiv
          else if is_signed(tarraydef(left.resultdef).elementdef) then
            op:=la_sdiv
          else
            op:=la_udiv;
        xorn:
          if not isfloat then
            op:=la_xor
          else
            internalerror(2022090711);
        orn:
          if not isfloat then
            op:=la_or
          else
            internalerror(2022090712);
        andn:
          if not isfloat then
            op:=la_and
          else
            internalerror(2022090712);
        else
          internalerror(200610073);
      end;

      location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
      lv:=to_hwvectordef(tarraydef(left.resultdef),false);
      rv:=to_hwvectordef(tarraydef(right.resultdef),false);
      hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,lv,false);
      hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,rv,false);
      location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
      current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_reg_reg(op,location.register,lv,left.location.register,right.location.register));
    end;


begin
  caddnode:=tllvmaddnode;
end.

