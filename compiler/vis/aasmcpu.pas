{
    Copyright (c) 1998-2001 by Florian Klaempfl and Pierre Muller

    virtual instruction set family assembler instructions

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
  cclasses,aasmtai,aasmdata,
  aasmbase,globals,verbose,
  cpubase,cpuinfo;


type

  taicpu = class(taicpu_abstract)
     opsize : topsize;
     constructor op_none(op : tasmop;_size : topsize);

     constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
     constructor op_const(op : tasmop;_size : topsize;_op1 : longint);
     constructor op_ref(op : tasmop;_size : topsize;_op1 : treference);

     constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
     constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : treference);
     constructor op_ref_reg(op : tasmop;_size : topsize;_op1 : treference;_op2 : tregister);

     constructor op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
     constructor op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : treference);


     { this is for Jmp instructions }
     constructor op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);

     constructor op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
     { for DBxx opcodes }
     constructor op_reg_sym(op: tasmop; _size : topsize; _op1: tregister; _op2 :tasmsymbol);
     constructor op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);

     constructor op_sym_ofs(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint);
     constructor op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

  private
     procedure init(_size : topsize); { this need to be called by all constructor }
  end;


  tai_align = class(tai_align_abstract)
        { nothing to add }
  end;

  procedure InitAsm;
  procedure DoneAsm;


implementation


{*****************************************************************************
                                 Taicpu Constructors
*****************************************************************************}




    procedure taicpu.init(_size : topsize);
      begin
         typ:=ait_instruction;
         is_jmp:=false;
         opsize:=_size;
         ops:=0;
      end;


    constructor taicpu.op_none(op : tasmop;_size : topsize);
      begin
         inherited create(op);;
         init(_size);
      end;


    constructor taicpu.op_reg(op : tasmop;_size : topsize;_op1 : tregister);
      begin
         inherited create(op);;
         init(_size);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : longint);
      begin
         inherited create(op);;
         init(_size);
         ops:=1;
         loadconst(0,aword(_op1));
      end;


    constructor taicpu.op_ref(op : tasmop;_size : topsize;_op1 : treference);
      begin
         inherited create(op);;
         init(_size);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
      begin
         inherited create(op);;
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;



    constructor taicpu.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : treference);
      begin
         inherited create(op);;
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
      begin
         inherited create(op);;
         init(_size);
         ops:=2;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
      end;



    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : treference);
      begin
         inherited create(op);;
         init(_size);
         ops:=2;
         loadconst(0,aword(_op1));
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;_size : topsize;_op1 : treference;_op2 : tregister);
      begin
         inherited create(op);;
         init(_size);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);;
         init(_size);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


     constructor taicpu.op_reg_sym(op: tasmop; _size : topsize; _op1: tregister; _op2 :tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,0);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;

    constructor taicpu.op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);
      begin
         inherited create(op);;
         init(_size);
         ops:=2;
         loadreg(0,_op2);
         loadsymbol(1,_op1,_op1ofs);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;



    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

end.
