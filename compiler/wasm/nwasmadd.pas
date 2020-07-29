{
    Copyright (c) 2019 by Dmitry Boyarintsev based on JVM by Jonas Maebe

    Code generation for add nodes on the WebAssembly

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
unit nwasmadd;

{$i fpcdefs.inc}

interface

    uses
       cgbase,
       node,ncgadd,cpubase, globals, pass_2;

    type

       { twasmaddnode }

       twasmaddnode = class(tcgaddnode)
       protected
          procedure second_generic_compare(unsigned: boolean);

          procedure pass_left_right;override;
          //procedure second_addfloat;override;
          procedure second_cmpfloat;override;
          procedure second_cmpboolean;override;
          procedure second_cmp64bit;override;
          procedure second_add64bit; override;
          procedure second_cmpordinal;override;

          // special treatement for short-boolean expressions
          // using IF block, instead of direct labels
          procedure second_addboolean; override;
       end;

  implementation

    uses
      systems,
      cutils,verbose,constexp,globtype,compinnr,
      symconst,symtable,symdef,symcpu,
      paramgr,procinfo,pass_1,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,
      hlcgobj,hlcgcpu,cgutils,
      cpupara,
      nbas,ncon,nset,nadd,ncal,ncnv,ninl,nld,nmat,nmem,
      //njvmcon,
      cgobj;

{*****************************************************************************
                               tjvmaddnode
*****************************************************************************}

    procedure twasmaddnode.pass_left_right;
      begin
        //if not((nodetype in [orn,andn]) and
        //       is_boolean(left.resultdef)) then
        //  swapleftright;
        inherited pass_left_right;
      end;


    procedure twasmaddnode.second_cmpfloat;
      //var
      //  truelabel,
      //  falselabel: tasmlabel;
      //  op: tasmop;
      //  cmpop: TOpCmp;
      begin
        //truelabel:=nil;
        //falselabel:=nil;
        //pass_left_right;
        //{ swap the operands to make it easier for the optimizer to optimize
        //  the operand stack slot reloading in case both are in a register }
        //if (left.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) and
        //   (right.location.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
        //  swapleftright;
        //cmpop:=cmpnode2topcmp(false);
        //if (nf_swapped in flags) then
        //  cmpop:=swap_opcmp(cmpop);
        //
        //current_asmdata.getjumplabel(truelabel);
        //current_asmdata.getjumplabel(falselabel);
        //location_reset_jump(location,truelabel,falselabel);
        //
        //thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        //thlcgjvm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,right.resultdef,right.location);
        //
        //{ compares two floating point values and puts 1/0/-1 on stack depending
        //  on whether value1 >/=/< value2 }
        //if left.location.size=OS_F64 then
        //  { make sure that comparisons with NaNs always return false for </> }
        //  if nodetype in [ltn,lten] then
        //    op:=a_dcmpg
        //  else
        //    op:=a_dcmpl
        //else if nodetype in [ltn,lten] then
        //  op:=a_fcmpg
        //else
        //  op:=a_fcmpl;
        //current_asmdata.CurrAsmList.concat(taicpu.op_none(op));
        //thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,(1+ord(left.location.size=OS_F64))*2-1);
        //
        //current_asmdata.CurrAsmList.concat(taicpu.op_sym(opcmp2if[cmpop],location.truelabel));
        //thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        //hlcg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
      end;


    procedure twasmaddnode.second_cmpboolean;
      begin
        //second_generic_compare(true);
      end;


    procedure twasmaddnode.second_cmp64bit;
      begin
        //second_generic_compare(not is_signed(left.resultdef));
      end;


    procedure twasmaddnode.second_add64bit;
      begin
        //second_opordinal;
      end;


    procedure twasmaddnode.second_cmpordinal;
      begin
        second_generic_compare(not is_signed(left.resultdef));
      end;

    procedure twasmaddnode.second_addboolean;
      begin
        if (nodetype in [orn,andn]) and
           (not(cs_full_boolean_eval in current_settings.localswitches) or
            (nf_short_bool in flags)) then
        begin
          secondpass(left);
          current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_if) );

          case nodetype of
              andn :
                begin
                   // inside of IF (the condition evaluated as true)
                   // for "and" must evaluate the right section
                   secondpass(right);

                   current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_else) );

                   // inside of ELSE (the condition evaluated as false)
                   // for "and" must end evaluation immediately
                   current_asmdata.CurrAsmList.Concat( taicpu.op_const(a_i32_const, 0) );
                   current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_end) );
                end;
              orn :
                begin
                   // inside of IF (the condition evaluated as true)
                   // for "or" must end evalaution immediately - satified!
                   current_asmdata.CurrAsmList.Concat( taicpu.op_const(a_i32_const, 1) );

                   current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_else) );
                   // inside of ELSE (the condition evaluated as false)
                   // for "or" must evaluate the right part
                   secondpass(right);
                   // inside of ELSE (the condition evaluated as false)
                   // for "and" must end evaluation immediately
                   current_asmdata.CurrAsmList.Concat( taicpu.op_none(a_end) );
                end;
              else
                Internalerror(2019091902);
              end;
          // todo: need to reset location to the stack?

          //Internalerror(2019091901);
        end else
          inherited;
      end;

    procedure twasmaddnode.second_generic_compare(unsigned: boolean);
      var
        truelabel,
        falselabel: tasmlabel;
        cmpop: TOpCmp;
      begin
        truelabel:=nil;
        falselabel:=nil;
        pass_left_right;
        { swap the operands to make it easier for the optimizer to optimize
          the operand stack slot reloading in case both are in a register }
        if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) and
           (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          swapleftright;
        cmpop:=cmpnode2topcmp(unsigned);
        if (nf_swapped in flags) then
          cmpop:=swap_opcmp(cmpop);

        // must generate those labels...
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
          hlcg.a_cmp_loc_reg_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location,left.location.register,location.truelabel)
        else case right.location.loc of
          LOC_REGISTER,LOC_CREGISTER:
            hlcg.a_cmp_reg_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location.register,left.location,location.truelabel);
          LOC_REFERENCE,LOC_CREFERENCE:
            hlcg.a_cmp_ref_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location.reference,left.location,location.truelabel);
          LOC_CONSTANT:
            hlcg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,left.resultdef,cmpop,right.location.value,left.location,location.truelabel);
          else
            internalerror(2011010413);
        end;
      end;

begin
  caddnode:=twasmaddnode;
end.
