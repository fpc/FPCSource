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
  cclasses,aasmtai,
  aasmbase,globals,verbose,
  cgbase,cpubase,cpuinfo;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 1;

    type
      taicpu = class(taicpu_abstract)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_const(op : tasmop;_op1 : aword);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aword);
         constructor op_const_reg(op:tasmop; _op1: aword; _op2: tregister);
         constructor op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_ref_reg(op:tasmop;_op1:TRegister;_op2:TReference;_op3:tregister);
         constructor op_reg_const_reg(op:tasmop;_op1:TRegister;_op2:aword;_op3:tregister);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);

         { register allocation }
         function is_reg_move:boolean;override;
         function is_same_reg_move:boolean;override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;
         function spilling_create_load(const ref:treference;r:tregister): tai;override;
         function spilling_create_store(r:tregister; const ref:treference): tai;override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;


implementation

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

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


    constructor taicpu.op_const(op : tasmop;_op1 : aword);
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

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aword);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: aword; _op2: tregister);
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
         if not(_op2.symaddr in [refs_lo,refs_hi]) then
           internalerror(200305311);
         ops:=3;
         loadreg(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_const_reg(op:tasmop;_op1:TRegister;_op2:aword;_op3:tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_cond_sym(op:tasmop;cond:TAsmCond;_op1:tasmsymbol);
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


    function taicpu.is_reg_move:boolean;
      begin
        result:=((opcode=A_MOV) or (opcode=A_FMOVS)) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg);
      end;


    function taicpu.is_same_reg_move:boolean;
      begin
        result:=((opcode=A_MOV) or (opcode=A_FMOVS)) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        result := operand_read;
        case opcode of
          A_STB..A_STDA:
            begin
              if opnr = 1 then
                result := operand_write;
            end;
          else
            if opnr = 0 then
              result := operand_write;
          end;
      end;


    function taicpu.spilling_create_load(const ref:treference;r:tregister): tai;
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


    function taicpu.spilling_create_store(r:tregister; const ref:treference): tai;
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

end.
{
  $Log$
  Revision 1.41  2004-01-12 16:39:40  peter
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
