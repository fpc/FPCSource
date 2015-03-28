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
      end;


implementation

     uses
       verbose,globtype,
       aasmdata,
       symconst,symtype,symdef,defutil,
       llvmbase,aasmllvm,
       cgbase,cgutils,
       hlcgobj,
       nadd
       ;

{ tllvmaddnode }

  function tllvmaddnode.pass_1: tnode;
    begin
      result:=inherited pass_1;
      { there are no flags in LLVM }
      if expectloc=LOC_FLAGS then
        expectloc:=LOC_REGISTER;
    end;


  procedure tllvmaddnode.force_reg_left_right(allow_swap, allow_constant: boolean);
    begin
      inherited;
      { pointer +/- integer -> make defs the same since a_op_* only gets a
        single type as argument }
      if (left.resultdef.typ=pointerdef)<>(right.resultdef.typ=pointerdef) then
        begin
          { the result is a pointerdef -> typecast both arguments to pointer;
            a_op_*_reg will convert them back to integer as needed }
          if left.resultdef.typ<>pointerdef then
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
          if right.resultdef.typ<>pointerdef then
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
      location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,pasbool8type);

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
    end;


  procedure tllvmaddnode.second_cmpordinal;
    var
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
      location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);

      if right.location.loc=LOC_CONSTANT then
        current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_const(la_icmp,
          location.register,cmpop,left.resultdef,left.location.register,right.location.value64))
      else
        current_asmdata.CurrAsmList.concat(taillvm.op_reg_cond_size_reg_reg(la_icmp,
          location.register,cmpop,left.resultdef,left.location.register,right.location.register));
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
      op    : tllvmop;
      llvmfpcmp : tllvmfpcmp;
      size : tdef;
      cmpop,
      singleprec : boolean;
    begin
      pass_left_right;

      cmpop:=false;
      singleprec:=tfloatdef(left.resultdef).floattype=s32real;
      { avoid uninitialised warning }
      llvmfpcmp:=lfc_invalid;
      case nodetype of
        addn :
          op:=la_fadd;
        muln :
          op:=la_fmul;
        subn :
          op:=la_fsub;
        slashn :
          op:=la_fdiv;
        ltn,lten,gtn,gten,
        equaln,unequaln :
          begin
            op:=la_fcmp;
            cmpop:=true;
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
                llvmfpcmp:=lfc_one;
              else
                internalerror(2015031506);
            end;
          end;
        else
          internalerror(2013102401);
      end;

      { get the operands in the correct order; there are no special cases here,
        everything is register-based }
      if nf_swapped in flags then
        swapleftright;

      { put both operands in a register }
      hlcg.location_force_fpureg(current_asmdata.CurrAsmList,right.location,right.resultdef,true);
      hlcg.location_force_fpureg(current_asmdata.CurrAsmList,left.location,left.resultdef,true);

      { initialize the result location }
      if not cmpop then
        begin
          location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
          location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);
        end
      else
        begin
          location_reset(location,LOC_REGISTER,OS_8);
          location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
        end;

      { see comment in thlcgllvm.a_loadfpu_ref_reg }
      if tfloatdef(left.resultdef).floattype in [s64comp,s64currency] then
        size:=sc80floattype
      else
        size:=left.resultdef;

      { emit the actual operation }
      if not cmpop then
        begin
          current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_reg_reg(op,location.register,size,
            left.location.register,right.location.register))
        end
      else
        begin
          current_asmdata.CurrAsmList.concat(taillvm.op_reg_fpcond_size_reg_reg(op,
            location.register,llvmfpcmp,size,left.location.register,right.location.register))
        end;
    end;


  procedure tllvmaddnode.second_cmpfloat;
    begin
      second_addfloat;
    end;


begin
  caddnode:=tllvmaddnode;
end.

