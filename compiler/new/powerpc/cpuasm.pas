{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Contains the assembler object for the PowerPC

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
unit cpuasm;

interface

uses
  cobjects,
  aasm,globals,verbose,
  cpubase, tainst;

type

  paicpu = ^taicpu;
  taicpu = object(tainstruction)

     constructor op_none(op : tasmop);

     constructor op_reg(op : tasmop;_op1 : tregister);
     constructor op_const(op : tasmop;_op1 : longint);

     constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
     constructor op_reg_ref(op : tasmop;_op1 : tregister;_op2 : preference);
     constructor op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
     constructor op_const_reg(op:tasmop; _op1: longint; _op2: tregister);

     constructor op_const_const(op : tasmop;_op1,_op2 : longint);

     constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
     constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: Longint);
     constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: pasmsymbol;_op3ofs: longint);
     constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; _op3: preference);
     constructor op_const_reg_reg(op : tasmop;_op1 : longint;_op2, _op3 : tregister);
     constructor op_const_reg_const(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : longint);

     constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
     constructor op_reg_bool_reg_reg(op : tasmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
     constructor op_reg_bool_reg_const(op : tasmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: longint);

     constructor op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : Longint);


     { this is for Jmp instructions }
     constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : pasmsymbol);
     constructor op_const_const_sym(op : tasmop;_op1,_op2 : longint;_op3: pasmsymbol);


     constructor op_sym(op : tasmop;_op1 : pasmsymbol);
     constructor op_sym_ofs(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint);
     constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:pasmsymbol;_op2ofs : longint);
     constructor op_sym_ofs_ref(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);

     procedure loadbool(opidx:longint;_b:boolean);

     destructor done;virtual;
  private
  end;


implementation

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    procedure taicpu.loadbool(opidx:longint;_b:boolean);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           b:=_b;
           typ:=top_bool;
         end;
      end;


    constructor taicpu.op_none(op : tasmop);
      begin
         inherited init(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited init(op);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited init(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited init(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
      begin
         inherited init(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: longint; _op2: tregister);
      begin
         inherited init(op);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;_op2 : preference);
      begin
         inherited init(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_op1,_op2 : longint);
      begin
         inherited init(op);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited init(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: Longint);
       begin
         inherited init(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;

     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: pasmsymbol;_op3ofs: longint);
       begin
         inherited init(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;

     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister;  _op3: preference);
       begin
         inherited init(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;

    constructor taicpu.op_const_reg_reg(op : tasmop;_op1 : longint;_op2, _op3 : tregister);
      begin
         inherited init(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_const_reg_const(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : longint);
      begin
         inherited init(op);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited init(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_reg(op : tasmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
      begin
         inherited init(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_const(op : tasmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: longint);
      begin
         inherited init(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(0,_op2);
         loadreg(0,_op3);
         loadconst(0,_op4);
      end;

     constructor taicpu.op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : Longint);
      begin
         inherited init(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
         loadconst(3,_op4);
         loadconst(4,_op5);
      end;

    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : pasmsymbol);
      begin
         inherited init(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;

     constructor taicpu.op_const_const_sym(op : tasmop;_op1,_op2 : longint; _op3: pasmsymbol);
      begin
         inherited init(op);
         ops:=3;
         loadconst(0,_op1);
         loadconst(1,_op2);
         loadsymbol(2,_op3,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : pasmsymbol);
      begin
         inherited init(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint);
      begin
         inherited init(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:pasmsymbol;_op2ofs : longint);
      begin
         inherited init(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);
      begin
         inherited init(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;

    destructor taicpu.done;
      var
        i : longint;
      begin
          for i:=ops-1 downto 0 do
            if (oper[i].typ=top_ref) then
              dispose(oper[i].ref);
        inherited done;
      end;

end.
{
  $Log$
  Revision 1.5  2000-01-07 01:14:58  peter
    * updated copyright to 2000

  Revision 1.4  1999/08/25 12:00:24  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.3  1999/08/06 16:41:11  jonas
    * PowerPC compiles again, several routines implemented in cgcpu.pas
    * added constant to cpubase of alpha and powerpc for maximum
      number of operands

  Revision 1.2  1999/08/04 12:59:24  jonas
    * all tokes now start with an underscore
    * PowerPC compiles!!

  Revision 1.1  1999/08/03 23:37:53  jonas
    + initial implementation for PowerPC based on the Alpha stuff

}