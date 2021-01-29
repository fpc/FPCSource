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


      tm68knotnode = class(tcgnotnode)
         procedure second_boolean;override;
      end;

      tm68kmoddivnode = class(tcgmoddivnode)
      public
        function first_moddivint: tnode;override;
        procedure emit_div_reg_reg(signed: boolean;denum,num : tregister);override;
        procedure emit_mod_reg_reg(signed: boolean;denum,num : tregister);override;
      end;

      tm68kunaryminusnode = class(tcgunaryminusnode)
        procedure second_float;override;
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

    procedure tm68knotnode.second_boolean;
      var
        hreg: tregister;
        opsize : tcgsize;
      begin
        secondpass(left);
        if not handle_locjump then
          begin
            opsize:=def_cgsize(resultdef);

            if ((left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and needs_unaligned(left.location.reference.alignment,opsize)) then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);

            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE:
                begin
                  tcg68k(cg).fixref(current_asmdata.CurrAsmList,left.location.reference,false);
                  if is_64bit(resultdef) then
                   begin
                     hreg:=cg.GetIntRegister(current_asmdata.CurrAsmList,OS_32);
                     cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_32,left.location.reference,hreg);
                     inc(left.location.reference.offset,4);
                     cg.a_op_ref_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,left.location.reference,hreg);
                   end
                 else
                   current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_TST,tcgsize2opsize[opsize],left.location.reference));
                   location_reset(location,LOC_FLAGS,OS_NO);
                   location.resflags:=F_E;
                end;
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_SUBSETREG,
              LOC_CSUBSETREG,
              LOC_SUBSETREF,
              LOC_CSUBSETREF:
                begin
                  if is_64bit(resultdef) then
                    begin
                      hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
                      current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_OR,S_L,left.location.register64.reghi,left.location.register64.reglo));
                    end
                  else
                    begin
                      hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,true);
                      if (current_settings.cputype = cpu_mc68000) and isaddressregister(left.location.register) then
                        begin
                          hreg:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
                          cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,opsize,left.location.register,hreg);
                        end
                      else
                        hreg:=left.location.register;
                      current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_TST,tcgsize2opsize[opsize],hreg));
                    end;
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
            else
              internalerror(200203223);
            end;
          end;
      end;


{*****************************************************************************
                               TM68KMODDIVNODE
*****************************************************************************}

  function tm68kmoddivnode.first_moddivint: tnode;
    begin
      if CPUM68K_HAS_32BITDIV in cpu_capabilities[current_settings.cputype] then
        result:=nil
      else
        result:=inherited first_moddivint;
    end;


  procedure tm68kmoddivnode.emit_div_reg_reg(signed: boolean;denum,num : tregister);
   const
     divudivs: array[boolean] of tasmop = (A_DIVU,A_DIVS);
   begin
     if CPUM68K_HAS_32BITDIV in cpu_capabilities[current_settings.cputype] then
       begin
         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(divudivs[signed],S_L,denum,num));
       end
     else
       InternalError(2014062801);
   end;


  procedure tm68kmoddivnode.emit_mod_reg_reg(signed: boolean;denum,num : tregister);
    const
      remop: array[boolean,boolean] of tasmop = ((A_DIVUL,A_DIVSL),(A_REMU,A_REMS));
    var
      tmpreg : tregister;
    begin
      if CPUM68K_HAS_32BITDIV in cpu_capabilities[current_settings.cputype] then
        begin
          tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
          { copy the numerator to the tmpreg, so we can use it as quotient, which
            means we'll get the remainder immediately in the numerator }
          cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,num,tmpreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(
            remop[CPUM68K_HAS_REMSREMU in cpu_capabilities[current_settings.cputype],signed],S_L,denum,num,tmpreg));
        end
      else
        InternalError(2014062802);
    end;


{*****************************************************************************
                          TM68KUNARYMINUSNODE
*****************************************************************************}

    procedure tm68kunaryminusnode.second_float;
      var
        href: treference;
      begin
        secondpass(left);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        //current_asmdata.CurrAsmList.concat(tai_comment.create(strpnew('unaryminus second_float called!')));

        case left.location.loc of
          LOC_REFERENCE,
          LOC_CREFERENCE :
            begin
              location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
              href:=left.location.reference;
              tcg68k(cg).fixref(current_asmdata.CurrAsmList,href,current_settings.fputype = fpu_coldfire);
              current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_FNEG,tcgsize2opsize[left.location.size],href,location.register));
            end;
          LOC_FPUREGISTER:
            begin
              location.register:=left.location.register;
              current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_FNEG,fpuregopsize,location.register));
            end;
          LOC_CFPUREGISTER:
            begin
               location.register:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEG,fpuregopsize,left.location.register,location.register));
            end;
          else
            internalerror(2003060202);
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

    procedure tm68kshlshrnode.pass_generate_code;
      var
        hregister, hreg64hi, hreg64lo : tregister;
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
              if (shiftval = 1) and (CPUM68K_HAS_ROLROR in cpu_capabilities[current_settings.cputype]) then
                begin
                  if nodetype = shln then
                    begin
                      current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_LSL,S_L,1,hreg64lo));
                      current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_ROXL,S_L,1,hreg64hi));
                    end
                  else
                    begin
                      current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_LSR,S_L,1,hreg64hi));
                      current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_ROXR,S_L,1,hreg64lo));
                    end;
                  location.register64.reghi:=hreg64hi;
                  location.register64.reglo:=hreg64lo;
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
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
            location_copy(location,left.location);

            { determine operator }
            if nodetype=shln then
              op:=OP_SHL
            else
              op:=OP_SHR;
            { shifting by a constant directly coded: }
            if (right.nodetype=ordconstn) then
              begin
                if tordconstnode(right).value.svalue and 31<>0 then
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,op,OS_32,tordconstnode(right).value.svalue and 31,location.register)
              end
            else
              begin
                { load shift count in a register if necessary }
                hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
                cg.a_op_reg_reg(current_asmdata.CurrAsmList,op,OS_32,right.location.register,location.register);
              end;
          end;
      end;



begin
   cnotnode:=tm68knotnode;
   cmoddivnode:=tm68kmoddivnode;
   cunaryminusnode:=tm68kunaryminusnode;
   cshlshrnode:=tm68kshlshrnode;
end.
