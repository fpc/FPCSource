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
         constructor op_const_ref_reg(op:tasmop;_op1:aword;const _op2:treference;_op3:tregister);
         constructor op_const_reg_ref(op:tasmop;_op1:aword;_op2:tregister;const _op3:treference);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
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


    constructor taicpu.op_const_ref_reg(op:tasmop;_op1:aword;const _op2:treference;_op3:tregister);
      begin
         inherited create(op);
         if (_op3.enum = R_INTREGISTER) and (_op3.number = NR_NO) then
           internalerror(2003031218);
         ops:=3;
         loadconst(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_const_reg_ref(op:tasmop;_op1:aword;_op2:tregister;const _op3:treference);
      begin
         inherited create(op);
         if (_op2.enum = R_INTREGISTER) and (_op2.number = NR_NO) then
           internalerror(2003031219);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
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


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

end.
{
  $Log$
  Revision 1.27  2003-05-30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

}
