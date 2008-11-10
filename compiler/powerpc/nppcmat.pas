{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for math nodes

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
unit nppcmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat;

    type
      tppcmoddivnode = class(tmoddivnode)
         function pass_1: tnode;override;
         procedure pass_generate_code;override;
      end;

      tppcshlshrnode = class(tshlshrnode)
         procedure pass_generate_code;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      tppcunaryminusnode = class(tunaryminusnode)
         procedure pass_generate_code;override;
      end;

      tppcnotnode = class(tnotnode)
         procedure pass_generate_code;override;
      end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,
      aasmbase,aasmcpu,aasmtai,aasmdata,
      defutil,
      cgbase,cgutils,cgobj,pass_2,
      ncon,procinfo,
      cpubase,
      ncgutil,cgcpu;

{$ifopt r+}
{$r-}
{$define rangeon}
{$endif}

{$ifopt q+}
{$q-}
{$define overflowon}
{$endif}
{ helper functions }
procedure getmagic_unsigned32(d : dword; out magic_m : dword; out magic_add : boolean; out magic_shift : dword);
var
    p : longint;
    nc, delta, q1, r1, q2, r2 : dword;
    
begin
    assert(d > 0);
    
    magic_add := false;
    nc := dword(- 1) - dword(-d) mod d;
    p := 31; { initialize p }
    q1 := $80000000 div nc; { initialize q1 = 2p/nc }
    r1 := $80000000 - q1*nc; { initialize r1 = rem(2p,nc) }
    q2 := $7FFFFFFF div d; { initialize q2 = (2p-1)/d }
    r2 := $7FFFFFFF - q2*d; { initialize r2 = rem((2p-1),d) }
    repeat
        inc(p);
        if (r1 >= (nc - r1)) then begin
            q1 := 2 * q1 + 1; { update q1 }
            r1 := 2*r1 - nc; { update r1 }
        end else begin
            q1 := 2*q1; { update q1 }
            r1 := 2*r1; { update r1 }
        end;
        if ((r2 + 1) >= (d - r2)) then begin
            if (q2 >= $7FFFFFFF) then
                magic_add := true;
            q2 := 2*q2 + 1; { update q2 }
            r2 := 2*r2 + 1 - d; { update r2 }
        end else begin
            if (q2 >= $80000000) then 
                magic_add := true;
            q2 := 2*q2; { update q2 }
            r2 := 2*r2 + 1; { update r2 }
        end;
        delta := d - 1 - r2;
    until not ((p < 64) and ((q1 < delta) or ((q1 = delta) and (r1 = 0))));
    magic_m := q2 + 1; { resulting magic number }
    magic_shift := p - 32; { resulting shift }
end;

procedure getmagic_signed32(d : longint; out magic_m : longint; out magic_s : longint);
const
    two_31 : DWord = high(longint)+1;
var
    p : Longint;
    ad, anc, delta, q1, r1, q2, r2, t : DWord;
    
begin
    assert((d < -1) or (d > 1));

    ad := abs(d);
    t := two_31 + (DWord(d) shr 31);
    anc := t - 1 - t mod ad; { absolute value of nc }
    p := 31; { initialize p }
    q1 := two_31 div anc; { initialize q1 = 2p/abs(nc) }
    r1 := two_31 - q1*anc; { initialize r1 = rem(2p,abs(nc)) }
    q2 := two_31 div ad; { initialize q2 = 2p/abs(d) }
    r2 := two_31 - q2*ad; { initialize r2 = rem(2p,abs(d)) }
    repeat 
        inc(p);
        q1 := 2*q1; { update q1 = 2p/abs(nc) }
        r1 := 2*r1; { update r1 = rem(2p/abs(nc)) }
        if (r1 >= anc) then begin { must be unsigned comparison }
            inc(q1);
            dec(r1, anc);
        end;
        q2 := 2*q2; { update q2 = 2p/abs(d) }
        r2 := 2*r2; { update r2 = rem(2p/abs(d)) }
        if (r2 >= ad) then begin { must be unsigned comparison }
            inc(q2);
            dec(r2, ad);
        end;
        delta := ad - r2;
    until not ((q1 < delta) or ((q1 = delta) and (r1 = 0)));
    magic_m := q2 + 1;
    if (d < 0) then begin
        magic_m := -magic_m; { resulting magic number }
    end;
    magic_s := p - 32; { resulting shift }
end;

{$ifdef rangeon}
{$r+}
{$undef rangeon}
{$endif}

{$ifdef overflowon}
{$q+}
{$undef overflowon}
{$endif}

{*****************************************************************************
                             TPPCMODDIVNODE
*****************************************************************************}

    function tppcmoddivnode.pass_1: tnode;
      begin
        result := inherited pass_1;
        if not assigned(result) then
          include(current_procinfo.flags,pi_do_call);
      end;


    procedure tppcmoddivnode.pass_generate_code;
      const
                    { signed   overflow }
        divops: array[boolean, boolean] of tasmop =
          ((A_DIVWU,A_DIVWU_),(A_DIVW,A_DIVWO_));
        zerocond: tasmcond = (dirhint: DH_Plus; simple: true; cond:C_NE; cr: RS_CR1);
      var
         power  : longint;
         op         : tasmop;
         numerator,
         divider,
         resultreg  : tregister;
         size       : Tcgsize;
         hl : tasmlabel;
         done: boolean;
         
         procedure genOrdConstNodeDiv;
         const
             negops : array[boolean] of tasmop = (A_NEG, A_NEGO);
         var
             magic, shift : longint;
             u_magic, u_shift : dword;
             u_add : boolean;
             
             divreg : tregister;
         begin
             if (tordconstnode(right).value = 0) then begin
                 internalerror(2005061701);
             end else if (tordconstnode(right).value = 1) then begin
                cg.a_load_reg_reg(current_asmdata.CurrAsmList, OS_INT, OS_INT, numerator, resultreg);
             end else if (tordconstnode(right).value = int64(-1)) then begin
                // note: only in the signed case possible..., may overflow
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(negops[cs_check_overflow in current_settings.localswitches], resultreg, numerator));
             end else if (ispowerof2(tordconstnode(right).value, power)) then begin
                if (is_signed(right.resultdef)) then begin
                    { From "The PowerPC Compiler Writer's Guide", pg. 52ff          }
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SAR, OS_INT, power,
                        numerator, resultreg);
                    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_ADDZE, resultreg, resultreg));
                end else begin
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_INT, power, numerator, resultreg)
                end;
             end else begin
                 { replace division by multiplication, both implementations }
                 { from "The PowerPC Compiler Writer's Guide" pg. 53ff      }
                 divreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
                 if (is_signed(right.resultdef)) then begin
                     getmagic_signed32(tordconstnode(right).value.svalue, magic, shift);
                     // load magic value
                     cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, magic, divreg);
                     // multiply
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULHW, resultreg, numerator, divreg));
                     // add/subtract numerator
                     if (tordconstnode(right).value > 0) and (magic < 0) then begin
                         cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_ADD, OS_INT, numerator, resultreg, resultreg);
                     end else if (tordconstnode(right).value < 0) and (magic > 0) then begin
                         cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT, numerator, resultreg, resultreg);
                     end;
                     // shift shift places to the right (arithmetic)
                     cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SAR, OS_INT, shift, resultreg, resultreg);                     
                     // extract and add sign bit
                     if (tordconstnode(right).value >= 0) then begin
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_INT, 31, numerator, divreg);
                     end else begin
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_INT, 31, resultreg, divreg);
                     end;                     
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_ADD, OS_INT, resultreg, divreg, resultreg);
                 end else begin
                     getmagic_unsigned32(tordconstnode(right).value.uvalue, u_magic, u_add, u_shift);
                     // load magic in divreg
                     cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, aint(u_magic), divreg);
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULHWU, resultreg, numerator, divreg));
                     if (u_add) then begin
                         cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT, resultreg, numerator, divreg);
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_INT,  1, divreg, divreg);
                         cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_ADD, OS_INT, divreg, resultreg, divreg);
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_INT, u_shift-1, divreg, resultreg);
                     end else begin
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, OS_INT, u_shift, resultreg, resultreg);
                     end;
                 end;
             end;
             done := true;
         end;

         procedure genOrdConstNodeMod;
         var
             modreg, maskreg, tempreg : tregister;
         begin
             if (tordconstnode(right).value = 0) then begin
                 internalerror(2005061702);
             end else if (abs(tordconstnode(right).value.svalue) = 1) then begin
                // x mod +/-1 is always zero
                cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, 0, resultreg);
             end else if (ispowerof2(tordconstnode(right).value, power)) then begin
                 if (is_signed(right.resultdef)) then begin
                     
                     tempreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
                     maskreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);
                     modreg := cg.getintregister(current_asmdata.CurrAsmList, OS_INT);

                     cg.a_load_const_reg(current_asmdata.CurrAsmList, OS_INT, abs(tordconstnode(right).value.svalue)-1, modreg);
                     cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SAR, OS_INT, 31, numerator, maskreg);
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, numerator, modreg, tempreg);

                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ANDC, maskreg, maskreg, modreg));
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBFIC, modreg, tempreg, 0));
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBFE, modreg, modreg, modreg));
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, modreg, maskreg, maskreg);
                     cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_OR, OS_INT, maskreg, tempreg, resultreg);
                 end else begin
                     cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_AND, OS_INT, tordconstnode(right).value.svalue-1, numerator, resultreg);
                 end;
             end else begin
                 genOrdConstNodeDiv();
                 cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_MUL, OS_INT, tordconstnode(right).value.svalue, resultreg, resultreg);
                 cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList, OP_SUB, OS_INT, resultreg, numerator, resultreg);
             end;
         end;

         
      begin
         secondpass(left);
         secondpass(right);
         location_copy(location,left.location);

         { put numerator in register }
         size:=def_cgsize(left.resultdef);
         location_force_reg(current_asmdata.CurrAsmList,left.location,
           size,true);
         location_copy(location,left.location);
         numerator := location.register;
         resultreg := location.register;
         if (location.loc = LOC_CREGISTER) then begin
           location.loc := LOC_REGISTER;
           location.register := cg.getintregister(current_asmdata.CurrAsmList,size);
           resultreg := location.register;
         end else if (nodetype = modn) or (right.nodetype = ordconstn) then begin
           // for a modulus op, and for const nodes we need the result register
           // to be an extra register
           resultreg := cg.getintregister(current_asmdata.CurrAsmList,size);
         end;

         done := false;
         if (right.nodetype = ordconstn) then begin
           if (nodetype = divn) then
             genOrdConstNodeDiv
           else
             genOrdConstNodeMod;
           done := true;
         end;

         if (not done) then begin
             { load divider in a register if necessary }
             location_force_reg(current_asmdata.CurrAsmList,right.location,
               def_cgsize(right.resultdef),true);
             if (right.nodetype <> ordconstn) then
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_CMPWI,NR_CR1,
                 right.location.register,0));
             divider := right.location.register;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             op := divops[is_signed(right.resultdef),
                          cs_check_overflow in current_settings.localswitches];
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(op,resultreg,numerator,
               divider));

           if (nodetype = modn) then
             begin
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_MULLW,resultreg,
                 divider,resultreg));
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUB,location.register,
                 numerator,resultreg));
               resultreg := location.register;
             end;
           end;
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        if right.nodetype <> ordconstn then
          begin
            current_asmdata.getjumplabel(hl);
            current_asmdata.CurrAsmList.concat(taicpu.op_cond_sym(A_BC,zerocond,hl));
            cg.a_call_name(current_asmdata.CurrAsmList,'FPC_DIVBYZERO',false);
            cg.a_label(current_asmdata.CurrAsmList,hl);
          end;
        { unsigned division/module can only overflow in case of division by zero }
        { (but checking this overflow flag is more convoluted than performing a  }
        {  simple comparison with 0)                                             }
        if is_signed(right.resultdef) then
          cg.g_overflowcheck(current_asmdata.CurrAsmList,location,resultdef);
      end;


{*****************************************************************************
                             TPPCSHLRSHRNODE
*****************************************************************************}

    function tppcshlshrnode.first_shlshr64bitint: tnode;
      begin
        result := nil;
      end;

    procedure tppcshlshrnode.pass_generate_code;

      var
         resultreg, hregister1,hregister2,
         hreg64hi,hreg64lo : tregister;
         op : topcg;
         asmop1, asmop2: tasmop;
         shiftval: aint;

      begin
         secondpass(left);
         secondpass(right);

         if is_64bitint(left.resultdef) then
           begin
             location_force_reg(current_asmdata.CurrAsmList,left.location,
               def_cgsize(left.resultdef),true);
             location_copy(location,left.location);
             hreg64hi := location.register64.reghi;
             hreg64lo := location.register64.reglo;
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.loc := LOC_REGISTER;
                 location.register64.reghi := cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                 location.register64.reglo := cg.getintregister(current_asmdata.CurrAsmList,OS_32);
               end;
             if (right.nodetype = ordconstn) then
               begin
                 shiftval := tordconstnode(right).value.svalue;
                 shiftval := shiftval and 63;
{
              I think the statements below is much more correct instead of the hack above,
              but then we fail tshlshr.pp :/

                 if shiftval > 63 then
                   begin
                     cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reglo);
                     cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reglo);
                   end
                 else } if shiftval > 31 then
                   begin
                     if nodetype = shln then
                       begin
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_32,
                           shiftval and 31,hreg64lo,location.register64.reghi);
                         cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reglo);
                       end
                     else
                       begin
                         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_32,
                           shiftval and 31,hreg64hi,location.register64.reglo);
                         cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_32,0,location.register64.reghi);
                       end;
                   end
                 else
                   begin
                     if nodetype = shln then
                       begin
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.register64.reghi,hreg64hi,shiftval,
                           0,31-shiftval));
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWIMI,location.register64.reghi,hreg64lo,shiftval,
                           32-shiftval,31));
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.register64.reglo,hreg64lo,shiftval,
                           0,31-shiftval));
                       end
                     else
                       begin
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.register64.reglo,hreg64lo,32-shiftval,
                           shiftval,31));
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWIMI,location.register64.reglo,hreg64hi,32-shiftval,
                           0,shiftval-1));
                         current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const_const_const(
                           A_RLWINM,location.register64.reghi,hreg64hi,32-shiftval,
                           shiftval,31));
                       end;
                   end;
               end
             else
               { no constant shiftcount }
               begin
                 location_force_reg(current_asmdata.CurrAsmList,right.location,OS_S32,true);
                 hregister1 := right.location.register;
                 if nodetype = shln then
                   begin
                     asmop1 := A_SLW;
                     asmop2 := A_SRW;
                   end
                 else
                   begin
                     asmop1 := A_SRW;
                     asmop2 := A_SLW;
                     resultreg := hreg64hi;
                     hreg64hi := hreg64lo;
                     hreg64lo := resultreg;
                     resultreg := location.register64.reghi;
                     location.register64.reghi := location.register64.reglo;
                     location.register64.reglo := resultreg;
                   end;

                 cg.getcpuregister(current_asmdata.CurrAsmList,NR_R0);
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBFIC,
                   NR_R0,hregister1,32));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(asmop1,
                   location.register64.reghi,hreg64hi,hregister1));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(asmop2,
                   NR_R0,hreg64lo,NR_R0));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR,
                   location.register64.reghi,location.register64.reghi,NR_R0));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBI,
                   NR_R0,hregister1,32));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(asmop1,
                   NR_R0,hreg64lo,NR_R0));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_OR,
                   location.register64.reghi,location.register64.reghi,NR_R0));
                 current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(asmop1,
                   location.register64.reglo,hreg64lo,hregister1));
                 cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_R0);

                 if nodetype = shrn then
                   begin
                     resultreg := location.register64.reghi;
                     location.register64.reghi := location.register64.reglo;
                     location.register64.reglo := resultreg;
                   end;
               end
           end
         else
           begin
             { load left operators in a register }
             location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
             location_copy(location,left.location);
             resultreg := location.register;
             hregister1 := location.register;
             location.loc := LOC_REGISTER;
             resultreg := cg.getintregister(current_asmdata.CurrAsmList,location.size);
             location.register := resultreg;

             { determine operator }
             if nodetype=shln then
               op:=OP_SHL
             else
               op:=OP_SHR;

             { shifting by a constant directly coded: }
             if (right.nodetype=ordconstn) then
               cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,op,location.size,
                 tordconstnode(right).value.svalue and 31,hregister1,resultreg)
             else
               begin
                 { load shift count in a register if necessary }
                 location_force_reg(current_asmdata.CurrAsmList,right.location,def_cgsize(right.resultdef),true);
                 hregister2 := right.location.register;

                 cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,op,location.size,hregister2,
                  hregister1,resultreg);
               end;
           end;
      end;


{*****************************************************************************
                          TPPCUNARYMINUSNODE
*****************************************************************************}

    procedure tppcunaryminusnode.pass_generate_code;

      var
        src1: tregister;
        op: tasmop;

      begin
         secondpass(left);
         if is_64bit(left.resultdef) then
           begin
             location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
             location_copy(location,left.location);
             if (location.loc = LOC_CREGISTER) then
               begin
                 location.register64.reglo := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                 location.register64.reghi := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                 location.loc := LOC_REGISTER;
               end;
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBFIC,
               location.register64.reglo,left.location.register64.reglo,0));
             if not(cs_check_overflow in current_settings.localswitches) then
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_SUBFZE,
                 location.register64.reghi,left.location.register64.reghi))
             else
               current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_SUBFZEO_,
                 location.register64.reghi,left.location.register64.reghi));
           end
         else
           begin
              location_copy(location,left.location);
              location.loc:=LOC_REGISTER;
              case left.location.loc of
                LOC_FPUREGISTER, LOC_REGISTER:
                  begin
                    src1 := left.location.register;
                    location.register := src1;
                  end;
                LOC_CFPUREGISTER, LOC_CREGISTER:
                  begin
                     src1 := left.location.register;
                     if left.location.loc = LOC_CREGISTER then
                       location.register := cg.getintregister(current_asmdata.CurrAsmList,OS_INT)
                     else
                       location.register := cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
                  end;
                LOC_REFERENCE,LOC_CREFERENCE:
                  begin
                     if (left.resultdef.typ=floatdef) then
                       begin
                          src1 := cg.getfpuregister(current_asmdata.CurrAsmList,def_cgsize(left.resultdef));
                          location.register := src1;
                          cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,
                            left.location.size,left.location.size,
                            left.location.reference,src1);
                       end
                     else
                       begin
                          src1 := cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                          location.register:= src1;
                          cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_32,
                            left.location.reference,src1);
                       end;
                  end;
              end;
              { choose appropriate operand }
              if left.resultdef.typ <> floatdef then
                begin
                  if not(cs_check_overflow in current_settings.localswitches) then
                    op := A_NEG
                  else
                    op := A_NEGO_;
                  location.loc := LOC_REGISTER;
                end
              else
                begin
                  op := A_FNEG;
                  location.loc := LOC_FPUREGISTER;
                end;
              { emit operation }
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(op,location.register,src1));
           end;
{ Here was a problem...     }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!     }
{ So this is useless...     }
{ that's not true: -2^31 gives an overflow error if it is negated (FK) }
        cg.g_overflowcheck(current_asmdata.CurrAsmList,location,resultdef);
      end;


{*****************************************************************************
                               TPPCNOTNODE
*****************************************************************************}

    procedure tppcnotnode.pass_generate_code;

      var
         hl : tasmlabel;

      begin
         if is_boolean(resultdef) then
          begin
            { if the location is LOC_JUMP, we do the secondpass after the
              labels are allocated
            }
            if left.expectloc=LOC_JUMP then
              begin
                hl:=current_procinfo.CurrTrueLabel;
                current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
                current_procinfo.CurrFalseLabel:=hl;
                secondpass(left);
                maketojumpbool(current_asmdata.CurrAsmList,left,lr_load_regvars);
                hl:=current_procinfo.CurrTrueLabel;
                current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
                current_procinfo.CurrFalseLabel:=hl;
                location.loc:=LOC_JUMP;
              end
            else
              begin
                secondpass(left);
                case left.location.loc of
                  LOC_FLAGS :
                    begin
                      location_copy(location,left.location);
                      inverse_flags(location.resflags);
                    end;
                  LOC_REGISTER, LOC_CREGISTER,
                  LOC_REFERENCE, LOC_CREFERENCE,
                  LOC_SUBSETREG, LOC_CSUBSETREG, 
                  LOC_SUBSETREF, LOC_CSUBSETREF:
                    begin
                      location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
                      current_asmdata.CurrAsmList.concat(taicpu.op_reg_const(A_CMPWI,left.location.register,0));
                      location_reset(location,LOC_FLAGS,OS_NO);
                      location.resflags.cr:=RS_CR0;
                      location.resflags.flag:=F_EQ;
                   end;
                  else
                    internalerror(2003042401);
                end;
              end;
          end
         else if is_64bitint(left.resultdef) then
           begin
             secondpass(left);
             location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),false);
             location_copy(location,left.location);
             { perform the NOT operation }
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_NOT,location.register64.reghi,
               location.register64.reghi));
             current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_NOT,location.register64.reglo,
               location.register64.reglo));
           end
         else
           begin
             secondpass(left);
             location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),true);
             location_copy(location,left.location);
             location.loc := LOC_REGISTER;
             location.register := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
             { perform the NOT operation }
             cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_NOT,def_cgsize(resultdef),left.location.register,
               location.register);
          end;
      end;

begin
   cmoddivnode:=tppcmoddivnode;
   cshlshrnode:=tppcshlshrnode;
   cunaryminusnode:=tppcunaryminusnode;
   cnotnode:=tppcnotnode;
end.
