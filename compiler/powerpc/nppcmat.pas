{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
      node,nmat;

    type
      tppcmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tppcshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

      tppcunaryminusnode = class(tunaryminusnode)
         procedure pass_2;override;
      end;

      tppcnotnode = class(tnotnode)
         procedure pass_2;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasm,types,
      cgbase,cgobj,temp_gen,pass_1,pass_2,
      ncon,
      cpubase,
      cga,tgcpu,nppcutil,cgcpu;

{*****************************************************************************
                             TPPCMODDIVNODE
*****************************************************************************}

    procedure tppcmoddivnode.pass_2;
      const
                    { signed   overflow }
        divops: array[boolean, boolean] of tasmop =
          ((A_DIVWU,A_DIVWUO),(A_DIVW,A_DIVWO));
      var
         power,
         l1, l2     : longint;
         op         : tasmop;
         numerator,
         divider,
         resultreg  : tregister;
         saved      : boolean;

      begin
         secondpass(left);
         saved:=maybe_savetotemp(right.registers32,left,is_64bitint(left.resulttype.def));
         secondpass(right);
         if saved then
           restorefromtemp(left,is_64bitint(left.resulttype.def));
         set_location(location,left.location);

         resultreg := R_NO;
         { put numerator in register }
         if (left.location.loc in [LOC_REFERENCE,LOC_MEM]) then
           begin
             del_reference(left.location.reference);
             numerator := getregisterint;
             { OS_32 because everything is always converted to longint/ }
             { cardinal in the resulttype pass (JM)                     }
             cg.a_load_ref_reg(expraslist,OS_32,left.location.reference,
               numerator);
             resultreg := numerator;
           end
         else
           begin
             numerator := left.location.register;
             if left.location.loc = LOC_CREGISTER then
               resultreg := getregisterint
             else
               resultreg := numerator;
           end;

         if (nodetype = divn) and
            (right.nodetype = ordconstn) and
            ispowerof2(tordconstnode(right).value,power) then
           begin
             { From 'The PowerPC Compiler Writer's Guide":                   }
             { This code uses the fact that, in the PowerPC architecture,    }
             { the shift right algebraic instructions set the Carry bit if   }
             { the source register contains a negative number and one or     }
             { more 1-bits are shifted out. Otherwise, the carry bit is      }
             { cleared. The addze instruction corrects the quotient, if      }
             { necessary, when the dividend is negative. For example, if     }
             { n = -13, (0xFFFF_FFF3), and k = 2, after executing the srawi  }
             { instruction, q = -4 (0xFFFF_FFFC) and CA = 1. After executing }
             { the addze instruction, q = -3, the correct quotient.          }
             cg.a_op_reg_reg_reg(OP_SAR,power,numerator,resultreg);
             exprasmlist.concat(taicpu.op_reg_reg(A_ADDZE,resultreg,resultreg));
           end
         else
           begin
             { load divider in a register if necessary }
             case right.location.loc of
               LOC_CREGISTER, LOC_REGISTER:
                 divider := right.location.register;
               LOC_REFERENCE, LOC_MEM:
                 begin
                   divider := cg.get_scratch_reg(exprasmlist);
                   cg.a_load_ref_reg(exprasmlist,OS_32,
                     right.location.reference,divider);
                   del_reference(right.location.reference);
                 end;
             end;

             { needs overflow checking, (-maxlongint-1) div (-1) overflows! }
             { (JM)                                                         }
             op := divops[is_signed(right.resulttype.def),
                          cs_check_overflow in aktlocalswitches];
             exprasmlist(taicpu.op_reg_reg_reg(op,resultreg,numerator,
               divider))
           end;
         { free used registers }
         if right.location.loc in [LOC_REFERENCE,LOC_MEM] then
           cg.free_scratch_reg(exprasmlist,divider)
         else
           ungetregister(divider);
        if numerator <> resultreg then
          ungetregisterint(numerator);
        { set result location }
        location.loc:=LOC_REGISTER;
        location.register:=resultreg;
        emitoverflowcheck(self);
      end;


{*****************************************************************************
                             TPPCSHLRSHRNODE
*****************************************************************************}

    procedure tppcshlshrnode.pass_2;
      var
         resultreg, hregister1,hregister2,hregister3,
         hregisterhigh,hregisterlow : tregister;
         op : topcg;
         saved : boolean;

      begin
         secondpass(left);
         saved:=maybe_savetotemp(right.registers32,left,is_64bitint(left.resulttype.def));
         secondpass(right);
         if saved then
           restorefromtemp(left,is_64bitint(left.resulttype.def));

         if is_64bitint(left.resulttype.def) then
           begin
             { see green book appendix E, still needs to be implemented }
           end
         else
           begin
             { load left operators in a register }
             if (left.location.loc in [LOC_REFERENCE,LOC_MEM]) then
               begin
                 del_reference(left.location.reference);
                 hregister1 := getregisterint;
                 { OS_32 because everything is always converted to longint/ }
                 { cardinal in the resulttype pass (JM)                     }
                 cg.a_load_ref_reg(expraslist,OS_32,left.location.reference,
                   hregister1);
                 resultreg := hregister1;
               end
             else
               begin
                 hregister1 := left.location.register;
                 if left.location.loc = LOC_CREGISTER then
                   resultreg := getregisterint
                 else
                   resultreg := hregister1;
               end;

              { determine operator }
              if nodetype=shln then
                op:=OP_SHL
              else
                op:=OP_SHR;

              { shifting by a constant directly coded: }
              if (right.nodetype=ordconstn) then
                cg.a_op_reg_reg_const(exprasmlist,op,OS_32,resultreg,
                  hregister1,tordconstnode(right).value and 31)
              else
                begin
                  { load shift count in a register if necessary }
                  case right.location.loc of
                    LOC_CREGISTER, LOC_REGISTER:
                      hregister2 := right.location.register;
                    LOC_REFERENCE, LOC_MEM:
                      begin
                        hregister2 := cg.get_scratch_reg(exprasmlist);
                        cg.a_load_ref_reg(exprasmlist,OS_32,
                          right.location.reference,hregister2);
                        del_reference(right.location.reference);
                      end;
                  end;

                  tcgppc(cg).a_op_reg_reg_reg(exprasmlist,op,hregister1,
                    hregister2,resultreg);

                  if right.location.loc in [LOC_REFERENCE,LOC_MEM] then
                    cg.free_scratch_reg(exprasmlist,hregister2)
                  else
                    ungetregister(hregister2);
                end;
              { set result location }
              location.loc:=LOC_REGISTER;
              location.register:=resultreg;
           end;
      end;


{*****************************************************************************
                          TPPCUNARYMINUSNODE
*****************************************************************************}

    procedure tppcunaryminusnode.pass_2;

      var
        src1, src2, tmp: tregister;
        op: tasmop;
      begin
         secondpass(left);
         if is_64bitint(left.resulttype.def) then
           begin
              clear_location(location);
              location.loc:=LOC_REGISTER;
              case left.location.loc of
                LOC_REGISTER, LOC_CREGISTER :
                  begin
                    src1 := left.location.registerlow;
                    src2 := left.location.registerhigh;
                    if left.location.loc = LOC_REGISTER then
                      begin
                        location.registerlow:=src1;
                        location.registerhigh:=src2;
                      end
                    else
                      begin
                        location.registerlow := getregisterint;
                        location.registerhigh := getregisterint;
                      end;
                  end;
                LOC_REFERENCE,LOC_MEM :
                  begin
                    del_reference(left.location.reference);
                    location.registerlow:=getregisterint;
                    src1 := location.registerlow;
                    location.registerhigh:=getregisterint;
                    src2 := location.registerhigh;
                    tcg64f32(cg).a_load64_ref_reg(exprasmlist,left.location.reference,
                      location.registerlow,
                      location.registerhigh);
                  end;
              end;
             exprasmlist.concat(taicpu.op_reg_reg(A_NEG,location.registerlow,
               src1));
             cg.a_op_reg_reg(OP_NOT,OS_32,src2,location.registerhigh);
             tmp := cg.get_scratch_reg(exprasmlist);
             tcgppc(cg).a_op_const_reg_reg(OP_SAR,31,location.registerlow,tmp);
             if not(cs_check_overflow in aktlocalswitches) then
               cg.a_op_reg_reg(OP_ADD,OS_32,location.registerhigh,tmp)
             else
               exprasmlist.concat(taicpu.op_reg_reg_reg(A_ADDO,tmp,
                 location.registerhigh,tmp));
             cg.free_scratch_reg(exprasmlist,tmp);
           end
         else
           begin
              location.loc:=LOC_REGISTER;
              case left.location.loc of
                LOC_FPU, LOC_REGISTER:
                  begin
                    src1 := left.location.register;
                    location.register := src1;
                  end;
                LOC_CFPUREGISTER, LOC_CREGISTER:
                  begin
                     src1 := left.location.register;
                     if left.location.loc = LOC_CREGISTER then
                       location.register := getregisterint
                     else
                       location.register := getregisterfpu;
                  end;
                LOC_REFERENCE,LOC_MEM:
                  begin
                     del_reference(left.location.reference);
                     if (left.resulttype.def.deftype=floatdef) then
                       begin
                          src1 := getregisterfpu;
                          location.register := src1;
                          floatload(tfloatdef(left.resulttype.def).typ,
                            left.location.reference,src1);
                       end
                     else
                       begin
                          src1 := getregisterint;
                          location.register:= src1;
                          cg.a_load_ref_reg(exprasmlist,OS_32,
                            left.location.reference,src1);
                       end;
                  end;
              end;
              { choose appropriate operand }
              if left.resulttype.def <> floatdef then
                if not(cs_check_overflow in aktlocalswitches) then
                  op := A_NEG
                else
                  op := A_NEGO
              else
                op := A_FNEG;
              { emit operation }
              eprasmlist.concat(taicpu.op_reg_reg(op,location.register,src1));
           end;
{ Here was a problem...     }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!     }
{ So this is useless...     }
{ that's not true: -2^31 gives an overflow error if it is negated (FK) }
        emitoverflowcheck(self);
      end;


{*****************************************************************************
                               TPPCNOTNODE
*****************************************************************************}

    procedure tppcnotnode.pass_2;
      var
         hl : tasmlabel;
         regl, regh: tregister;
      begin
         if is_boolean(resulttype.def) then
          begin
            { the second pass could change the location of left }
            { if it is a register variable, so we've to do      }
            { this before the case statement                    }
            if left.location.loc in [LOC_REFERENCE,LOC_MEM,
              LOC_FLAGS,LOC_REGISTER,LOC_CREGISTER] then
              secondpass(left);
            case left.location.loc of
              LOC_JUMP :
                begin
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                  secondpass(left);
                  maketojumpbool(left,lr_load_regvars);
                  hl:=truelabel;
                  truelabel:=falselabel;
                  falselabel:=hl;
                end;
              LOC_FLAGS :
                location.resflags:=inverse_flags(left.location.resflags);
              LOC_REGISTER, LOC_CREGISTER, LOC_REFERENCE, LOC_MEM :
                begin
                  if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                    regl := left.location.register
                  else
                    begin
                      regl := getregisterint;
                      cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),
                        left.location.reference,regl);
                    end;
                  location.loc:=LOC_FLAGS;
                  location.resflags.cr:=0;
                  location.resflags.flag:=F_EQ;
                  exprasmlist.concat(taicpu.op_reg_const(A_CMPWI,regl,0));
                  ungetregister(regl);
                end;
            end;
          end
         else if is_64bitint(left.resulttype.def) then
           begin
             secondpass(left);
             clear_location(location);
             location.loc:=LOC_REGISTER;
             { make sure left is in a register and set the dest register }
             case left.location.loc of
               LOC_REFERENCE, LOC_MEM, LOC_CREGISTER:
                 begin
                   location.registerlow := getregisterint;
                   location.registerhigh := getregisterint;
                   if left.location.loc <> LOC_CREGISTER then
                     begin
                       tcg64f32(cg).a_load64_ref_reg(exprasmlist,
                         left.location.reference,location.registerlow,
                         location.registerhigh);
                       regl := location.registerlow;
                       regh := location.registerhigh;
                     end
                   else
                     begin
                       regl := left.location.registerlow;
                       regh := left.location.registerhigh;
                     end;
                 end;
               LOC_REGISTER:
                 begin
                   regl := left.location.registerlow;
                   location.registerlow := regl;
                   regh := left.location.registerhigh;
                   location.registerhigh := regh;
                 end;
             end;
             { perform the NOT operation }
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.registerhigh,
               regh);
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.registerlow,
               regl);
           end
         else
           begin
             secondpass(left);
             clear_location(location);
             location.loc:=LOC_REGISTER;
             { make sure left is in a register and set the dest register }
             case left.location.loc of
               LOC_REFERENCE, LOC_MEM, LOC_CREGISTER:
                 begin
                   location.register := getregisterint;
                   if left.location.loc <> LOC_CREGISTER then
                     begin
                       cg.a_load_ref_reg(exprasmlist,left.location.reference,
                         location.register);
                       regl := location.register;
                     end
                   else
                     regl := left.location.register;
                 end;
               LOC_REGISTER:
                 regl := left.location.register;
             end;
             { perform the NOT operation }
             exprasmlist.concat(taicpu.op_reg_reg(A_NOT,location.register,
               regl);
             { release the source reg if it wasn't reused }
             if regl <> location.register then
               ungetregisterint(regl);
          end;
      end;

begin
   cmoddivnode:=tppcmoddivnode;
   cshlshrnode:=tppcshlshrnode;
   cunaryminusnode:=tppcunaryminusnode;
   cnotnode:=tppcnotnode;
end.
{
  $Log$
  Revision 1.1  2001-12-29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)


}
