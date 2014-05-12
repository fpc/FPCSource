{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate 680x0 assembler for math nodes

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
unit n68kmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,cpubase,cgbase;

    type


      tm68knotnode = class(tnotnode)
         procedure pass_generate_code;override;
      end;

      tm68kmoddivnode = class(tcgmoddivnode)
      private
        procedure call_rtl_divmod_reg_reg(denum,num:tregister;const name:string);
      public
         procedure emit_div_reg_reg(signed: boolean;denum,num : tregister);override;
         procedure emit_mod_reg_reg(signed: boolean;denum,num : tregister);override;
      end;

      tm68kshlshrnode = class(tshlshrnode)
         procedure pass_generate_code;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,symtable,aasmbase,aasmtai,aasmdata,aasmcpu,
      pass_1,pass_2,procinfo,
      ncon,
      cpuinfo,paramgr,defutil,parabase,
      tgobj,ncgutil,cgobj,hlcgobj,cgutils,rgobj,rgcpu,cgcpu,cg64f32;




{*****************************************************************************
                               TM68KNOTNODE
*****************************************************************************}

    procedure tm68knotnode.pass_generate_code;
      var
         hl : tasmlabel;
         opsize : tcgsize;
         loc : tcgloc;
      begin
         opsize:=def_cgsize(resultdef);
         if is_boolean(resultdef) then
          begin
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if left.expectloc<>LOC_JUMP then
              begin
                secondpass(left);
                loc:=left.location.loc;
              end
            else
              loc:=LOC_JUMP;

            case loc of
              LOC_JUMP :
                begin
                  location_reset(location,LOC_JUMP,OS_NO);
                  hl:=current_procinfo.CurrTrueLabel;
                  current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
                  current_procinfo.CurrFalseLabel:=hl;
                  secondpass(left);
                  maketojumpbool(current_asmdata.CurrAsmList,left,lr_load_regvars);
                  hl:=current_procinfo.CurrTrueLabel;
                  current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
                  current_procinfo.CurrFalseLabel:=hl;
                end;
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
//                  location_release(current_asmdata.CurrAsmList,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_CONSTANT,
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_REFERENCE,
              LOC_CREFERENCE,
              LOC_SUBSETREG,
              LOC_CSUBSETREG,
              LOC_SUBSETREF,
              LOC_CSUBSETREF:
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
                  current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_TST,tcgsize2opsize[opsize],left.location.register));
//                  location_release(current_asmdata.CurrAsmList,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
             else
                internalerror(200203223);
            end;
          end
         else if is_64bitint(left.resultdef) then
           begin
              secondpass(left);
              location_copy(location,left.location);
              hlcg.location_force_reg(current_asmdata.CurrAsmList,location,left.resultdef,u64inttype,false);
              cg64.a_op64_loc_reg(current_asmdata.CurrAsmList,OP_NOT,OS_64,location,
                joinreg64(location.register64.reglo,location.register64.reghi));
           end
         else
          begin
             secondpass(left);
             hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
             location_copy(location,left.location);
             if location.loc=LOC_CREGISTER then
              location.register := cg.getintregister(current_asmdata.CurrAsmList,opsize);
             { perform the NOT operation }
             cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NOT,opsize,location.register,left.location.register);
          end;
      end;

  procedure tm68kmoddivnode.call_rtl_divmod_reg_reg(denum,num:tregister;const name:string);
    var
      paraloc1,paraloc2 : tcgpara;
      pd : tprocdef;
    begin
      pd:=search_system_proc(name);
      paraloc1.init;
      paraloc2.init;
      paramanager.getintparaloc(pd,1,paraloc1);
      paramanager.getintparaloc(pd,2,paraloc2);
      cg.a_load_reg_cgpara(current_asmdata.CurrAsmList,OS_32,num,paraloc2);
      cg.a_load_reg_cgpara(current_asmdata.CurrAsmList,OS_32,denum,paraloc1);
      paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc2);
      paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
      cg.alloccpuregisters(current_asmdata.CurrAsmList,R_ADDRESSREGISTER,paramanager.get_volatile_registers_address(pd.proccalloption));
      cg.alloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,paramanager.get_volatile_registers_int(pd.proccalloption));
      cg.a_call_name(current_asmdata.CurrAsmList,name,false);
      cg.dealloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,paramanager.get_volatile_registers_int(pd.proccalloption));
      cg.dealloccpuregisters(current_asmdata.CurrAsmList,R_ADDRESSREGISTER,paramanager.get_volatile_registers_address(pd.proccalloption));
      cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_FUNCTION_RESULT_REG);
      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,NR_FUNCTION_RESULT_REG,num);
      paraloc2.done;
      paraloc1.done;
    end;

{*****************************************************************************
                               TM68KMODDIVNODE
*****************************************************************************}
  procedure tm68kmoddivnode.emit_div_reg_reg(signed: boolean;denum,num : tregister);
   begin
     if current_settings.cputype=cpu_MC68020 then
       begin
         if signed then
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_DIVS,S_L,denum,num))
         else
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_DIVU,S_L,denum,num));
       end
     else
       begin
         { On MC68000/68010/Coldfire we must pass through RTL routines }
         if signed then
           call_rtl_divmod_reg_reg(denum,num,'fpc_div_longint')
         else
           call_rtl_divmod_reg_reg(denum,num,'fpc_div_dword');
       end;
   end;


  procedure tm68kmoddivnode.emit_mod_reg_reg(signed: boolean;denum,num : tregister);
      var tmpreg : tregister;
          continuelabel : tasmlabel;
          signlabel : tasmlabel;
          reg_d0,reg_d1 : tregister;
    begin
     if current_settings.cputype=cpu_MC68020 then
       begin
         tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
         { copy the numerator to the tmpreg, so we can use it as quotient, which
           means we'll get the remainder immediately in the numerator }
         cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,num,tmpreg);
         if signed then
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_DIVSL,S_L,denum,num,tmpreg))
         else
           current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_DIVUL,S_L,denum,num,tmpreg));
       end
     else
       begin
         { On MC68000/68010/coldfire we must pass through RTL routines }
         if signed then
           call_rtl_divmod_reg_reg(denum,num,'fpc_mod_longint')
         else
           call_rtl_divmod_reg_reg(denum,num,'fpc_mod_dword');
       end;
    end;


{*****************************************************************************
                             TM68KSHLRSHRNODE
*****************************************************************************}

    function tm68kShlShrNode.first_shlshr64bitint:TNode;
      begin
        if is_64bit(left.resultdef) and not (right.nodetype=ordconstn) then
          { for 64bit shifts with anything but constants we use rtl helpers }
          result:=inherited
        else
          { 2nd pass is our friend }
          result := nil;
      end;


{ TODO: FIX ME!!! shlshrnode needs review}
    procedure tm68kshlshrnode.pass_generate_code;
      var
        hregister,resultreg,hregister1,
        hreg64hi,hreg64lo : tregister;
        op : topcg;
        shiftval: aint;
      begin
        secondpass(left);
        secondpass(right);
        if is_64bit(left.resultdef) then
          begin
            location_reset(location,LOC_REGISTER,OS_64);

            { load left operator in a register }
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,u64inttype,false);
            hreg64hi:=left.location.register64.reghi;
            hreg64lo:=left.location.register64.reglo;

            shiftval := tordconstnode(right).value.svalue;
	    shiftval := shiftval and 63;
            if shiftval > 31 then
              begin
                if nodetype = shln then
                  begin
                    cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,hreg64hi);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,shiftval and 31,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,hreg64lo);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,shiftval and 31,hreg64hi,hreg64hi);
                  end;
                location.register64.reglo:=hreg64hi;
                location.register64.reghi:=hreg64lo;
              end
            else
              begin
                hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                if nodetype = shln then
                  begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,32-shiftval,hreg64lo,hregister);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,shiftval,hreg64hi,hreg64hi);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hregister,hreg64hi,hreg64hi);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,shiftval,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,32-shiftval,hreg64hi,hregister);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,shiftval,hreg64lo,hreg64lo);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hregister,hreg64lo,hreg64lo);
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,shiftval,hreg64hi,hreg64hi);
                  end;
                location.register64.reghi:=hreg64hi;
                location.register64.reglo:=hreg64lo;
              end;
          end
        else
          begin
            { load left operators in a register }
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
            location_copy(location,left.location);
            resultreg := location.register;
            hregister1 := location.register;
            if (location.loc = LOC_CREGISTER) then
              begin
                location.loc := LOC_REGISTER;
                resultreg := cg.GetIntRegister(current_asmdata.CurrAsmList,OS_INT);
                location.register := resultreg;
              end;
            { determine operator }
            if nodetype=shln then
              op:=OP_SHL
            else
              op:=OP_SHR;
            { shifting by a constant directly coded: }
            if (right.nodetype=ordconstn) then
              begin
                if tordconstnode(right).value.svalue and 31<>0 then
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,op,OS_32,tordconstnode(right).value.svalue and 31,hregister1,resultreg)
              end
            else
              begin
                { load shift count in a register if necessary }
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
                cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,op,OS_32,right.location.register,hregister1,resultreg);
              end;
          end;
      end;



begin
   cnotnode:=tm68knotnode;
   cmoddivnode:=tm68kmoddivnode;
   cshlshrnode:=tm68kshlshrnode;
end.
