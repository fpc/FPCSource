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
  cpubase,cpuinfo;

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
         function is_nop:boolean;override;
         function is_move:boolean;override;
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
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031207);
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
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031205);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031206);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aword);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031208);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: aword; _op2: tregister);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031209);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031210);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031210);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031211);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031212);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031213);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_ref_reg(op:tasmop;_op1:TRegister;_op2:TReference;_op3:tregister);
      begin
         inherited create(op);
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031214);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031215);
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
         if (_op1.enum = R_INTREGISTER) and (_op1.number = NR_NO) then
           internalerror(2003031216);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031217);
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


    function taicpu.is_nop:boolean;
      begin
        result:=(opcode=A_NOP);
      end;


    function taicpu.is_move:boolean;
      begin
        result:=(opcode=A_MOV) and
                (oper[0].typ=top_reg) and
                (oper[1].typ=top_reg);
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
  Revision 1.30  2003-06-14 14:53:50  jonas
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
