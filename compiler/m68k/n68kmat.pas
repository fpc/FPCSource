{
    $Id$
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
      node,nmat;

    type
      tm68kmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tm68kshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

      tm68knotnode = class(tnotnode)
         procedure pass_2;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,defbase,
      cginfo,cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,paramgr,
      tgobj,ncgutil,cgobj,rgobj,rgcpu,cgcpu,cg64f32;

{*****************************************************************************
                             TM68kMODDIVNODE
*****************************************************************************}

    procedure tm68kmoddivnode.pass_2;
      var
         hreg1 : tregister;
         hdenom,hnumerator : tregister;
         shrdiv,popeax,popedx : boolean;
         power : longint;
         hl : tasmlabel;
         pushedregs : tmaybesave;
      begin
         shrdiv := false;
         secondpass(left);
         if codegenerror then
          exit;
         maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
         secondpass(right);
         maybe_restore(exprasmlist,left.location,pushedregs);
         if codegenerror then
          exit;
         location_copy(location,left.location);

         if is_64bitint(resulttype.def) then
           begin
             { should be handled in pass_1 (JM) }
             internalerror(200109052);
           end
         else
           begin
              { put numerator in register }
              location_force_reg(exprasmlist,left.location,OS_INT,false);
              hreg1:=left.location.register;

              if (nodetype=divn) and
                 (right.nodetype=ordconstn) and
                 ispowerof2(tordconstnode(right).value,power) then
                Begin
                  shrdiv := true;
                  { for signed numbers, the numerator must be adjusted before the
                    shift instruction, but not wih unsigned numbers! Otherwise,
                    "Cardinal($ffffffff) div 16" overflows! (JM) }
                  If is_signed(left.resulttype.def) Then
                    Begin
                      objectlibrary.getlabel(hl);
                      cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_GT,0,hreg1,hl);
                      if power=1 then
                          cg.a_op_const_reg(exprasmlist,OP_ADD,1,hreg1)
                      else
                          cg.a_op_const_reg(exprasmlist,OP_ADD,
                             tordconstnode(right).value-1,hreg1);
                      cg.a_label(exprasmlist,hl);    
                      cg.a_op_const_reg(exprasmlist,OP_SAR,power,hreg1);
                      End
                    Else { not signed }
                     Begin
                      cg.a_op_const_reg(exprasmlist,OP_SHR,power,hreg1);
                     end;
                End
              else
                begin
                  { bring denominator to hdenom }
                  { hdenom is always free, it's }
                  { only used for temporary }
                  { purposes                }
                  hdenom := rg.getregisterint(exprasmlist);
                  if right.location.loc<>LOC_CREGISTER then
                   location_release(exprasmlist,right.location);
                  cg.a_load_loc_reg(exprasmlist,right.location,hdenom);
                  if nodetype = modn then
                    begin
                      hnumerator := rg.getregisterint(exprasmlist);
                      cg.a_load_reg_reg(exprasmlist,OS_INT,hreg1,hnumerator);
                    end;
                  
                  { verify if the divisor is zero, if so return an error
                    immediately
                  }
                  objectlibrary.getlabel(hl);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_NE,0,hdenom,hl);
                  cg.a_param_const(exprasmlist,OS_S32,200,paramanager.getintparaloc(1));
                  cg.a_call_name(exprasmlist,'FPC_HANDLERROR');
                  cg.a_label(exprasmlist,hl);
                  if is_signed(left.resulttype.def) then
                     cg.a_op_reg_reg(exprasmlist,OP_IDIV,OS_INT,hdenom,hreg1)
                  else
                     cg.a_op_reg_reg(exprasmlist,OP_DIV,OS_INT,hdenom,hreg1);
                     
                  if nodetype = modn then
                    begin
{$warning modnode should be tested}                    
                     {  I mod J = I - (I div J) * J }
                      cg.a_op_reg_reg(exprasmlist,OP_IMUL,OS_INT,hdenom,hreg1);
                      cg.a_op_reg_reg(exprasmlist,OP_SUB,OS_INT,hnumerator,hreg1);
                      rg.ungetregister(exprasmlist,hnumerator);
                    end;
                end;
              location_reset(location,LOC_REGISTER,OS_INT);
              location.register:=hreg1;
           end;
        cg.g_overflowcheck(exprasmlist,self);
      end;


{*****************************************************************************
                             TI386SHLRSHRNODE
*****************************************************************************}

    procedure tm68kshlshrnode.pass_2;
      var
         hcountreg : tregister;
         op : topcg;
         l1,l2,l3 : tasmlabel;
         pushedregs : tmaybesave;
         freescratch : boolean;
      begin
         freescratch:=false;
         secondpass(left);
         maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
         secondpass(right);
         maybe_restore(exprasmlist,left.location,pushedregs);
         { determine operator }
         case nodetype of
           shln: op:=OP_SHL;
           shrn: op:=OP_SHR;
         end;
         
         if is_64bitint(left.resulttype.def) then
           begin
              location_reset(location,LOC_REGISTER,OS_64);

              { load left operator in a register }
              location_force_reg(exprasmlist,left.location,OS_64,false);
              location_copy(location,left.location);

              if (right.nodetype=ordconstn) then
                begin
                   cg64.a_op64_const_reg(exprasmlist,op,tordconstnode(right).value,
                     joinreg64(location.registerlow,location.registerhigh));
                end
              else
                begin
                   { load right operators in a register - this  
                     is done since most target cpu which will use this
                     node do not support a shift count in a mem. location (cec)
                   }
                   
                   if right.location.loc<>LOC_REGISTER then
                     begin
                       if right.location.loc<>LOC_CREGISTER then
                        location_release(exprasmlist,right.location);
                       hcountreg:=cg.get_scratch_reg_int(exprasmlist);
                       cg.a_load_loc_reg(exprasmlist,right.location,hcountreg);
                       freescratch := true;
                     end
                   else
                      hcountreg:=right.location.register;
                   cg64.a_op64_reg_reg(exprasmlist,op,hcountreg,
                     joinreg64(location.registerlow,location.registerhigh));
                   if freescratch then
                      cg.free_scratch_reg(exprasmlist,hcountreg);
                end;
           end
         else
           begin
              { load left operators in a register }
              location_copy(location,left.location);
              location_force_reg(exprasmlist,location,OS_INT,false);

              { shifting by a constant directly coded: }
              if (right.nodetype=ordconstn) then
                begin
                   { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)
                   if right.value<=31 then
                   }
                   cg.a_op_const_reg(exprasmlist,op,tordconstnode(right).value and 31,
                     location.register);
                   {
                   else
                     emit_reg_reg(A_XOR,S_L,hregister1,
                       hregister1);
                   }
                end
              else
                begin
                   { load right operators in a register - this  
                     is done since most target cpu which will use this
                     node do not support a shift count in a mem. location (cec)
                   }
                   if right.location.loc<>LOC_REGISTER then
                     begin
                       if right.location.loc<>LOC_CREGISTER then
                        location_release(exprasmlist,right.location);
                       hcountreg:=cg.get_scratch_reg_int(exprasmlist);
                       freescratch := true;
                       cg.a_load_loc_reg(exprasmlist,right.location,hcountreg);
                     end
                   else
                     hcountreg:=right.location.register;
                   cg.a_op_reg_reg(exprasmlist,op,OS_INT,hcountreg,location.register);
                   if freescratch then
                      cg.free_scratch_reg(exprasmlist,hcountreg);
                end;
           end;
      end;



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
                  location_release(exprasmlist,left.location);
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
                  location_release(exprasmlist,left.location);
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
              cg64.a_op64_loc_reg(exprasmlist,OP_NOT,location,
                joinreg64(location.registerlow,location.registerhigh));
           end
         else
          begin
            secondpass(left);
            location_copy(location,left.location);
            location_force_reg(exprasmlist,location,opsize,false);
            cg.a_op_reg_reg(exprasmlist,OP_NOT,opsize,location.register,location.register);
          end;
      end;

begin
   cmoddivnode:=tm68kmoddivnode;
   cshlshrnode:=tm68kshlshrnode;
   cnotnode:=tm68knotnode;
end.
{
  $Log$
  Revision 1.2  2002-08-15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.1  2002/08/14 19:16:34  carl
    + m68k type conversion nodes
    + started some mathematical nodes
    * out of bound references should now be handled correctly

}
