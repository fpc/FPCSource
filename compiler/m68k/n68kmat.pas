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
         procedure pass_2;override;
      end;

      tm68kmoddivnode = class(tcgmoddivnode)
         procedure emit_div_reg_reg(signed: boolean;denum,num : tregister);override;
         procedure emit_mod_reg_reg(signed: boolean;denum,num : tregister);override;
      end;

      tm68kshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,
      pass_1,pass_2,
      ncon,
      cpuinfo,paramgr,defutil,parabase,
      tgobj,ncgutil,cgobj,cgutils,rgobj,rgcpu,cgcpu,cg64f32;




{*****************************************************************************
                               TM68KNOTNODE
*****************************************************************************}

    procedure tm68knotnode.pass_2;
      var
         hl : tasmlabel;
         opsize : tcgsize;
      begin
         opsize:=def_cgsize(resulttype.def);
         if is_boolean(resulttype.def) then
          begin
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if left.location.loc<>LOC_JUMP then
             secondpass(left);

            case left.location.loc of
              LOC_JUMP :
                begin
                  location_reset(location,LOC_JUMP,OS_NO);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(left);
                  maketojumpbool(exprasmlist,left,lr_load_regvars);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
//                  location_release(exprasmlist,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_CONSTANT,
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  location_force_reg(exprasmlist,left.location,def_cgsize(resulttype.def),true);
                  exprasmlist.concat(taicpu.op_reg(A_TST,tcgsize2opsize[opsize],left.location.register));
//                  location_release(exprasmlist,left.location);
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
             else
                internalerror(200203224);
            end;
          end
         else if is_64bitint(left.resulttype.def) then
           begin
              secondpass(left);
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_64,false);
              cg64.a_op64_loc_reg(exprasmlist,OP_NOT,OS_64,location,
                joinreg64(location.register64.reglo,location.register64.reghi));
           end
         else
          begin
             secondpass(left);
             location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);
             location_copy(location,left.location);
             if location.loc=LOC_CREGISTER then
              location.register := cg.getintregister(exprasmlist,opsize);
             { perform the NOT operation }
             cg.a_op_reg_reg(exprasmlist,OP_NOT,opsize,location.register,left.location.register);
          end;
      end;


{*****************************************************************************
                               TM68KMODDIVNODE
*****************************************************************************}
  procedure tm68kmoddivnode.emit_div_reg_reg(signed: boolean;denum,num : tregister);
   var
     continuelabel : tasmlabel;
     reg_d0,reg_d1 : tregister;
     paraloc1 : tcgpara;
   begin
     { no RTL call, so inline a zero denominator verification }
     if aktoptprocessor <> MC68000 then
       begin
         { verify if denominator is zero }
         objectlibrary.getjumplabel(continuelabel);
         { compare against zero, if not zero continue }
         cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_NE,0,denum,continuelabel);
//       paraloc1.init;
//         cg.a_param_const(exprasmlist,OS_S32,200,paramanager.getintparaloc(pocall_default,1,paraloc1));

         cg.a_call_name(exprasmlist,'FPC_HANDLEERROR');
         cg.a_label(exprasmlist, continuelabel);
         if signed then
            exprasmlist.concat(taicpu.op_reg_reg(A_DIVS,S_L,denum,num))
         else
            exprasmlist.concat(taicpu.op_reg_reg(A_DIVU,S_L,denum,num));
         { result should be in denuminator }
         cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,num,denum);
       end
     else
       begin
         { On MC68000/68010 mw must pass through RTL routines }
         reg_d0:=NR_D0;
         cg.getcpuregister(exprasmlist,NR_D0);
         reg_d1:=NR_D1;
         cg.getcpuregister(exprasmlist,NR_D1);
         { put numerator in d0 }
         cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,num,reg_d0);
         { put denum in D1 }
         cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,denum,reg_d1);
         if signed then
             cg.a_call_name(exprasmlist,'FPC_DIV_LONGINT')
         else
             cg.a_call_name(exprasmlist,'FPC_DIV_CARDINAL');
        cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,reg_d0,denum);
        cg.ungetcpuregister(exprasmlist,reg_d0);
        cg.ungetcpuregister(exprasmlist,reg_d1);
       end;
   end;


  procedure tm68kmoddivnode.emit_mod_reg_reg(signed: boolean;denum,num : tregister);
      var tmpreg : tregister;
          continuelabel : tasmlabel;
          signlabel : tasmlabel;
          reg_d0,reg_d1 : tregister;
    begin
//     writeln('emit mod reg reg');
     { no RTL call, so inline a zero denominator verification }
     if aktoptprocessor <> MC68000 then
       begin
         { verify if denominator is zero }
         objectlibrary.getjumplabel(continuelabel);
         { compare against zero, if not zero continue }
         cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_NE,0,denum,continuelabel);
//         cg.a_param_const(exprasmlist, OS_S32,200,paramanager.getintparaloc(pocall_default,1));
         cg.a_call_name(exprasmlist,'FPC_HANDLEERROR');
         cg.a_label(exprasmlist, continuelabel);

         tmpreg:=cg.getintregister(exprasmlist,OS_INT);

         { we have to prepare the high register with the  }
         { correct sign. i.e we clear it, check if the low dword reg }
         { which will participate in the division is signed, if so we}
         { we extend the sign to the high doword register by inverting }
         { all the bits.                                             }
         exprasmlist.concat(taicpu.op_reg(A_CLR,S_L,tmpreg));
         objectlibrary.getjumplabel(signlabel);
         exprasmlist.concat(taicpu.op_reg(A_TST,S_L,tmpreg));
         cg.a_cmp_const_reg_label(exprasmlist,OS_S32,OC_A,0,tmpreg,signlabel);
         { its a negative value, therefore change sign }
         cg.a_label(exprasmlist,signlabel);
         { tmpreg:num / denum }

         if signed then
           exprasmlist.concat(taicpu.op_reg_reg_reg(A_DIVSL,S_L,denum,tmpreg,num))
         else
           exprasmlist.concat(taicpu.op_reg_reg_reg(A_DIVUL,S_L,denum,tmpreg,num));
         { remainder in tmpreg }
         cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,tmpreg,denum);
//         cg.ungetcpuregister(exprasmlist,tmpreg);
       end
     else
       begin
         { On MC68000/68010 mw must pass through RTL routines }
         Reg_d0:=NR_D0;
         cg.getcpuregister(exprasmlist,NR_D0);
         Reg_d1:=NR_D1;
         cg.getcpuregister(exprasmlist,NR_D1);
         { put numerator in d0 }
         cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,num,Reg_D0);
         { put denum in D1 }
         cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,denum,Reg_D1);
         if signed then
             cg.a_call_name(exprasmlist,'FPC_MOD_LONGINT')
         else
             cg.a_call_name(exprasmlist,'FPC_MOD_CARDINAL');
        cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,Reg_D0,denum);
        cg.ungetcpuregister(exprasmlist,Reg_D0);
        cg.ungetcpuregister(exprasmlist,Reg_D1);
       end;
//      writeln('exits'); 
    end;


{*****************************************************************************
                             TM68KSHLRSHRNODE
*****************************************************************************}

    function tm68kShlShrNode.first_shlshr64bitint:TNode;
      begin
        { 2nd pass is our friend }
        result := nil;
      end;


{$WARNING FIX ME!!! shlshrnode needs review}
    procedure tm68kshlshrnode.pass_2;
      var
        hregister,resultreg,hregister1,
        hreg64hi,hreg64lo : tregister;
        op : topcg;
        shiftval: aword;
      begin
        secondpass(left);
        secondpass(right);
        if is_64bit(left.resulttype.def) then
          begin
            location_reset(location,LOC_REGISTER,OS_64);

            { load left operator in a register }
            location_force_reg(exprasmlist,left.location,OS_64,false);
            hreg64hi:=left.location.register64.reghi;
            hreg64lo:=left.location.register64.reglo;

            shiftval := tordconstnode(right).value and 63;
            if shiftval > 31 then
              begin
                if nodetype = shln then
                  begin
                    cg.a_load_const_reg(exprasmlist,OS_32,0,hreg64hi);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval and 31,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_load_const_reg(exprasmlist,OS_32,0,hreg64lo);
                    if (shiftval and 31) <> 0 then
                      cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval and 31,hreg64hi,hreg64hi);
                  end;
                location.register64.reglo:=hreg64hi;
                location.register64.reghi:=hreg64lo;
              end
            else
              begin
                hregister:=cg.getintregister(exprasmlist,OS_32);
                if nodetype = shln then
                  begin
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,32-shiftval,hreg64lo,hregister);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval,hreg64hi,hreg64hi);
                    cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,hregister,hreg64hi,hreg64hi);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,shiftval,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_32,32-shiftval,hreg64hi,hregister);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval,hreg64lo,hreg64lo);
                    cg.a_op_reg_reg_reg(exprasmlist,OP_OR,OS_32,hregister,hreg64lo,hreg64lo);
                    cg.a_op_const_reg_reg(exprasmlist,OP_SHR,OS_32,shiftval,hreg64hi,hreg64hi);
                  end;
                location.register64.reghi:=hreg64hi;
                location.register64.reglo:=hreg64lo;
              end;
          end
        else
          begin
            { load left operators in a register }
            location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),true);
            location_copy(location,left.location);
            resultreg := location.register;
            hregister1 := location.register;
            if (location.loc = LOC_CREGISTER) then
              begin
                location.loc := LOC_REGISTER;
                resultreg := cg.GetIntRegister(exprasmlist,OS_INT);
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
                if tordconstnode(right).value and 31<>0 then
                  cg.a_op_const_reg_reg(exprasmlist,op,OS_32,tordconstnode(right).value and 31,hregister1,resultreg)
              end
            else
              begin
                { load shift count in a register if necessary }
                location_force_reg(exprasmlist,right.location,def_cgsize(right.resulttype.def),true);
                cg.a_op_reg_reg_reg(exprasmlist,op,OS_32,right.location.register,hregister1,resultreg);
              end;
          end;
      end;



begin
   cnotnode:=tm68knotnode;
   cmoddivnode:=tm68kmoddivnode;
   cshlshrnode:=tm68kshlshrnode;
end.
