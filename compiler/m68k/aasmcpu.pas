{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl and Pierre Muller

    m68k family assembler instructions

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
  cpubase;


type

  taicpu = class(taicpu_abstract)
     opsize : topsize;
     constructor op_none(op : tasmop;_size : topsize);

     constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
     constructor op_const(op : tasmop;_size : topsize;_op1 : longint);
     constructor op_ref(op : tasmop;_size : topsize;_op1 : preference);

     constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
     constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);
     constructor op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);

     constructor op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
     constructor op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
     constructor op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);

     constructor op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);
     { this is only allowed if _op1 is an int value (_op1^.isintvalue=true) }
     constructor op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);

     constructor op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
     constructor op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);
     constructor op_const_ref_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference;_op3 : tregister);
     constructor op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister; _op3 : preference);
     constructor op_const_reg_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : preference);

     constructor op_reg_reglist(op: tasmop; _size : topsize; _op1: tregister;_op2: tregisterlist);
     constructor op_reglist_reg(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: tregister);

     constructor op_ref_reglist(op: tasmop; _size : topsize; _op1: preference;_op2: tregisterlist);
     constructor op_reglist_ref(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: preference);

     { this is for Jmp instructions }
     constructor op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);

     constructor op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
     constructor op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);

     procedure loadreglist(opidx:longint;r:pregisterlist);

     destructor destroy;
  private
     procedure init(op : tasmop;_size : topsize); { this need to be called by all constructor }
  end;


{*****************************************************************************
                                Labeled instruction
*****************************************************************************}

    pai_labeled = ^tai_labeled;
    tai_labeled = object(tai)
      opcode : tasmop;
      register : tregister;
      lab : pasmlabel;
      sym : tasmsymbol;
      constructor init(op : tasmop; l : pasmlabel);
      constructor init_sym(op : tasmop; asym : tasmsymbol);
      constructor init_reg(op: tasmop; l : pasmlabel; reg: tregister);
      constructor init_reg_sym(op : tasmop; asym: tasmsymbol; reg :tregister);
      destructor done;virtual;
    end;


implementation


{*****************************************************************************
                                 Taicpu Constructors
*****************************************************************************}



   procedure taicpu.loadreglist(opidx:longint;r:pregisterlist);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            disposereference(ref);
           registerlist:=r;
           typ:=top_reglist;
         end;
      end;


    procedure taicpu.init(_size : topsize);
      begin
         typ:=ait_instruction;
         is_jmp:=false;
         opcode:=op;
         opsize:=_size;
         ops:=0;
      end;


    constructor taicpu.op_none(op : tasmop;_size : topsize);
      begin
         inherited init;
         init(op,_size);
      end;


    constructor taicpu.op_reg(op : tasmop;_size : topsize;_op1 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : longint);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;_size : topsize;_op1 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadconst(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;_size : topsize;_op1 : preference;_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         loadref(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister;_op3 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_const_ref_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : preference;_op3 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadconst(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_const_reg_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister;_op3 : preference);
      begin
         inherited init;
         init(op,_size);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


   constructor taicpu.op_ref_reglist(op: tasmop; _size : topsize; _op1: preference;_op2: tregisterlist);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadref(0,_op1);
        loadreglist(1,newreglist(_op2));
     end;

   constructor taicpu.op_reglist_ref(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: preference);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadreglist(0,newreglist(_op1));
        loadref(1,_op2);
     End;



   constructor taicpu.op_reg_reglist(op: tasmop; _size : topsize; _op1: tregister;_op2: tregisterlist);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadreg(0,_op1);
        loadreglist(1,newreglist(_op2));
     end;


   constructor taicpu.op_reglist_reg(op: tasmop; _size : topsize; _op1: tregisterlist; _op2: tregister);
     Begin
        inherited init;
        init(op,_size);
        ops:=2;
        loadreglist(0,newreglist(_op1));
        loadreg(1,_op2);
     End;




    constructor taicpu.op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited init;
         init(op,_size);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;




    constructor taicpu.op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);
      begin
         inherited init;
         init(op,_size);
         ops:=2;
         if ((op >= A_DBCC) and (op <= A_DBF))
          or ((op >= A_FDBEQ) and (op <= A_FBDNGLE)) then
           begin
             loadreg(0,_op2);
             loadsymbol(1,_op1,_op1ofs);
           end
          else
           begin
             loadsymbol(0,_op1,_op1ofs);
             loadreg(1,_op2);
           end;
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    destructor taicpu.destroy;
      var
        i : longint;
      begin
          for i:=ops-1 downto 0 do
            if (oper[i].typ=top_ref) then
              dispose(oper[i].ref);
        inherited destroy;
      end;



{****************************************************************************
                              TAI_LABELED
 ****************************************************************************}

    constructor tai_labeled.init(op : tasmop; l : pasmlabel);

      begin
         inherited init;
         sym := nil;
         opcode := op;
         lab := l;
         register := R_NO;
         typ:=ait_labeled_instruction;
         inc(lab^.refs);
      end;


    constructor tai_labeled.init_sym(op : tasmop; asym: tasmsymbol);
      begin
         inherited init;
         sym:= asym;
         lab := nil;
         opcode := op;
         register := R_NO;
         typ:=ait_labeled_instruction;
{         inc(lab^.refs);}
      end;

    constructor tai_labeled.init_reg_sym(op : tasmop; asym: tasmsymbol; reg :tregister);
      begin
         inherited init;
         sym:= asym;
         lab := nil;
         opcode := op;
         register := reg;
         typ:=ait_labeled_instruction;
{         inc(lab^.refs);}
      end;

    constructor tai_labeled.init_reg(op : tasmop; l : pasmlabel; reg: tregister);

      begin
         inherited init;
         sym := nil;
         lab := l;
         opcode := op;
         register := reg;
         typ:=ait_labeled_instruction;
         inc(lab^.refs);
      end;

    destructor tai_labeled.done;

      begin
         if assigned(lab) then
           dec(lab^.refs);
         inherited done;
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
  Revision 1.1  2002-07-29 17:51:32  carl
    + restart m68k support


}
