{
    $Id$
    Copyright (c) 1999-2002 by Mazen Neifer

    Contains the assembler object for the SPARC

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
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  globtype,globals,verbose,
  aasmbase,aasmtai,
  cgbase,cpubase,cpuinfo;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 1;

    type
      taicpu = class(tai_cpu_abstract)
         delayslot_annulled : boolean;   { conditinal opcode with ,a }
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : LongInt);
         constructor op_ref(op : tasmop;const _op1 : treference);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: LongInt);
         constructor op_const_reg(op:tasmop; _op1: LongInt; _op2: tregister);
         constructor op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_ref_reg(op:tasmop;_op1:TRegister;_op2:TReference;_op3:tregister);
         constructor op_reg_const_reg(op:tasmop;_op1:TRegister;_op2:aint;_op3:tregister);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         procedure loadbool(opidx:longint;_b:boolean);
         { register allocation }
         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister): tai;
    function spilling_create_store(r:tregister; const ref:treference): tai;

implementation

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    procedure taicpu.loadbool(opidx:longint;_b:boolean);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx]^ do
         begin
           if typ=top_ref then
            dispose(ref);
           b:=_b;
           typ:=top_bool;
         end;
      end;


    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited create(op);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
         inherited create(op);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : LongInt);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: LongInt);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: LongInt; _op2: tregister);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_ref_reg(op:tasmop;_op1:TRegister;_op2:TReference;_op3:tregister);
      begin
         inherited create(op);
         { only allowed to load the address }
         if not(_op2.refaddr in [addr_lo,addr_hi]) then
           internalerror(200305311);
         ops:=3;
         loadreg(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_const_reg(op:tasmop;_op1:TRegister;_op2:aint;_op3:tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=(
                 ((opcode=A_MOV) and (regtype = R_INTREGISTER)) or
                 ((regtype = R_FPUREGISTER) and (opcode in [A_FMOVS,A_FMOVD]))
                ) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        if opnr=ops-1 then
          result := operand_write
        else
          result := operand_read;
      end;


    function spilling_create_load(const ref:treference;r:tregister): tai;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_ref_reg(A_LD,ref,r);
          R_FPUREGISTER :
            begin
              case getsubreg(r) of
                R_SUBFS :
                  result:=taicpu.op_ref_reg(A_LDF,ref,r);
                R_SUBFD :
                  result:=taicpu.op_ref_reg(A_LDD,ref,r);
                else
                  internalerror(200401042);
              end;
            end
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference): tai;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_ST,r,ref);
          R_FPUREGISTER :
            begin
              case getsubreg(r) of
                R_SUBFS :
                  result:=taicpu.op_reg_ref(A_STF,r,ref);
                R_SUBFD :
                  result:=taicpu.op_reg_ref(A_STD,r,ref);
                else
                  internalerror(200401042);
              end;
            end
          else
            internalerror(200401041);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

begin
  cai_cpu:=taicpu;
  cai_align:=tai_align;
end.
{
  $Log$
  Revision 1.49  2004-06-20 08:47:33  florian
    * spilling of doubles on sparc fixed

  Revision 1.48  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.47.2.5  2004/06/03 19:23:41  florian
    * fixed some spilling issues

  Revision 1.47.2.4  2004/05/30 17:54:14  florian
    + implemented cmp64bit
    * started to fix spilling
    * fixed int64 sub partially

  Revision 1.47.2.3  2004/05/28 20:29:49  florian
    * fixed currency trouble on x86-64

  Revision 1.47.2.2  2004/05/25 21:38:53  peter
    * assembler reader/writer updates

  Revision 1.47.2.1  2004/05/11 21:06:51  peter
    * sparc compiler fixed

  Revision 1.47  2004/03/12 08:18:11  mazen
  - revert '../' from include path

  Revision 1.46  2004/03/11 16:21:27  mazen
  + help lazarus analyze the file

  Revision 1.45  2004/03/08 16:28:39  mazen
  * make it as similar to PPC one ase possible

  Revision 1.44  2004/02/27 11:47:32  mazen
  * symaddr ==> refaddr to follow the rest of compiler changes

  Revision 1.43  2004/02/08 23:10:21  jonas
    * taicpu.is_same_reg_move() now gets a regtype parameter so it only
      removes moves of that particular register type. This is necessary so
      we don't remove the live_start instruction of a register before it
      has been processed

  Revision 1.42  2004/02/08 20:15:43  jonas
    - removed taicpu.is_reg_move because it's not used anymore
    + support tracking fpu register moves by rgobj for the ppc

  Revision 1.41  2004/01/12 16:39:40  peter
    * sparc updates, mostly float related

  Revision 1.40  2003/12/28 16:20:09  jonas
    - removed unused methods from old generic spilling code

  Revision 1.39  2003/12/26 14:02:30  peter
    * sparc updates
    * use registertype in spill_register

  Revision 1.38  2003/12/19 14:38:03  mazen
  * new TRegister definition applied

  Revision 1.37  2003/12/10 13:16:35  mazen
  * improve hadlign %hi and %lo operators

  Revision 1.36  2003/10/30 15:03:18  mazen
  * now uses standard routines in rgHelper unit to search registers by number and by name

  Revision 1.35  2003/10/24 07:00:17  mazen
  * fixed compil problem when using ObjFpc mode (^ required).

  Revision 1.34  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.33  2003/09/14 19:19:04  peter
    * updates for new ra

  Revision 1.32  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.31.2.1  2003/08/31 21:08:16  peter
    * first batch of sparc fixes

  Revision 1.31  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.30  2003/06/14 14:53:50  jonas
    * fixed newra cycle for x86
    * added constants for indicating source and destination operands of the
      "move reg,reg" instruction to aasmcpu (and use those in rgobj)

  Revision 1.29  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.28  2003/06/01 01:03:41  peter
    * remove unsupported combinations
    * reg_ref_reg only allowed for refs_lo,refs_hi

  Revision 1.27  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

}
