{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86-64 assembler for math nodes

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
unit nx64mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,nx86mat;

    type
      tx8664moddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tx8664shlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

      tx8664unaryminusnode = class(tx86unaryminusnode)
      end;

      tx8664notnode = class(tx86notnode)
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,defutil,
      cgbase,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,ncgutil,cgobj;

{*****************************************************************************
                             TX8664MODDIVNODE
*****************************************************************************}

    procedure tx8664moddivnode.pass_2;
      var
        hreg1,hreg2:Tregister;
        power:longint;
        op:Tasmop;
      begin
        secondpass(left);
        if codegenerror then
          exit;
        secondpass(right);
        if codegenerror then
          exit;

        { put numerator in register }
        location_reset(location,LOC_REGISTER,OS_INT);
        location_force_reg(exprasmlist,left.location,OS_INT,false);
        hreg1:=left.location.register;

        if (nodetype=divn) and (right.nodetype=ordconstn) and
           ispowerof2(tordconstnode(right).value,power) then
          begin
            { for signed numbers, the numerator must be adjusted before the
              shift instruction, but not wih unsigned numbers! Otherwise,
              "Cardinal($ffffffff) div 16" overflows! (JM) }
            if is_signed(left.resulttype.def) Then
              begin
                  { use a sequence without jumps, saw this in
                    comp.compilers (JM) }
                  { no jumps, but more operations }
                  hreg2:=cg.getintregister(exprasmlist,OS_INT);
                  emit_reg_reg(A_MOV,S_Q,hreg1,hreg2);
                  {If the left value is signed, hreg2=$ffffffff, otherwise 0.}
                  emit_const_reg(A_SAR,S_Q,63,hreg2);
                  {If signed, hreg2=right value-1, otherwise 0.}
                  emit_const_reg(A_AND,S_Q,tordconstnode(right).value-1,hreg2);
                  { add to the left value }
                  emit_reg_reg(A_ADD,S_Q,hreg2,hreg1);
                  { release EDX if we used it }
                  cg.ungetregister(exprasmlist,hreg2);
                  { do the shift }
                  emit_const_reg(A_SAR,S_Q,power,hreg1);
              end
            else
              emit_const_reg(A_SHR,S_Q,power,hreg1);
            location.register:=hreg1;
          end
        else
          begin
            {Bring denominator to a register.}
            cg.ungetregister(exprasmlist,hreg1);
            cg.getexplicitregister(exprasmlist,NR_RAX);
            emit_reg_reg(A_MOV,S_Q,hreg1,NR_RAX);
            cg.getexplicitregister(exprasmlist,NR_RDX);
            {Sign extension depends on the left type.}
            if torddef(left.resulttype.def).typ=u64bit then
              emit_reg_reg(A_XOR,S_Q,NR_RDX,NR_RDX)
            else
              emit_none(A_CDQ,S_NO);

            {Division depends on the right type.}
            if Torddef(right.resulttype.def).typ=u64bit then
              op:=A_DIV
            else
              op:=A_IDIV;

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              emit_ref(op,S_Q,right.location.reference)
            else if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              emit_reg(op,S_Q,right.location.register)
            else
              begin
                hreg1:=cg.getintregister(exprasmlist,right.location.size);
                cg.a_load_loc_reg(exprasmlist,OS_64,right.location,hreg1);
                cg.ungetregister(exprasmlist,hreg1);
                emit_reg(op,S_Q,hreg1);
              end;
            location_release(exprasmlist,right.location);

            { Copy the result into a new register. Release RAX & RDX.}
            if nodetype=divn then
              begin
                cg.ungetregister(exprasmlist,NR_RDX);
                cg.ungetregister(exprasmlist,NR_RAX);
                location.register:=cg.getintregister(exprasmlist,OS_INT);
                emit_reg_reg(A_MOV,S_Q,NR_RAX,location.register);
              end
            else
              begin
                cg.ungetregister(exprasmlist,NR_RAX);
                cg.ungetregister(exprasmlist,NR_RDX);
                location.register:=cg.getintregister(exprasmlist,OS_INT);
                emit_reg_reg(A_MOV,S_Q,NR_RDX,location.register);
              end;
          end;
      end;


{*****************************************************************************
                             TX8664SHLRSHRNODE
*****************************************************************************}


    procedure tx8664shlshrnode.pass_2;
      var
        op : Tasmop;
      begin
        secondpass(left);
        secondpass(right);

        { determine operator }
        if nodetype=shln then
          op:=A_SHL
        else
          op:=A_SHR;

        { load left operators in a register }
        location_copy(location,left.location);
        location_force_reg(exprasmlist,location,OS_INT,false);

        { shifting by a constant directly coded: }
        if (right.nodetype=ordconstn) then
          { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)}
          emit_const_reg(op,S_Q,tordconstnode(right).value and 63,location.register)
        else
          begin
            { load right operators in a RCX }
            if right.location.loc<>LOC_CREGISTER then
              location_release(exprasmlist,right.location);
            cg.getexplicitregister(exprasmlist,NR_RCX);
            cg.a_load_loc_reg(exprasmlist,OS_64,right.location,NR_RCX);

            { right operand is in ECX }
            cg.ungetregister(exprasmlist,NR_RCX);
            emit_reg_reg(op,S_Q,NR_CL,location.register);
          end;
      end;


begin
   cunaryminusnode:=tx8664unaryminusnode;
   cmoddivnode:=tx8664moddivnode;
   cshlshrnode:=tx8664shlshrnode;
   cnotnode:=tx8664notnode;
end.
{
  $Log$
  Revision 1.4  2004-02-05 18:28:37  peter
    * x86_64 fixes for opsize

  Revision 1.3  2004/02/05 01:24:08  florian
    * several fixes to compile x86-64 system

  Revision 1.2  2004/02/04 19:22:27  peter
  *** empty log message ***

  Revision 1.1  2004/01/20 12:59:37  florian
    * common addnode code for x86-64 and i386
}
