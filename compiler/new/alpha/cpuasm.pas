{
    $Id$
    Copyright (c) 1999 by Florian Klaempfl

    Contains the assembler object for the Alpha

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
  cpubase;

type
  pairegalloc = ^tairegalloc;
  tairegalloc = object(tai)
     allocation : boolean;
     reg        : tregister;
     constructor alloc(r : tregister);
     constructor dealloc(r : tregister);
  end;

  paalpha = ^taalpha;
  taalpha = object(tai)
     is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
     opcode    : tasmop;
     ops       : longint;
     condition : TasmCond;
     oper      : array[0..2] of toper;
     constructor op_none(op : tasmop);

     constructor op_reg(op : tasmop;_op1 : tregister);
     constructor op_const(op : tasmop;_op1 : longint);
     constructor op_ref(op : tasmop;_op1 : preference);

     constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
     constructor op_reg_ref(op : tasmop;_op1 : tregister;_op2 : preference);
     constructor op_reg_const(op:tasmop; _op1: tregister; _op2: longint);

     constructor op_const_reg(op : tasmop;_op1 : longint;_op2 : tregister);
     constructor op_const_const(op : tasmop;_op1,_op2 : longint);
     constructor op_const_ref(op : tasmop;_op1 : longint;_op2 : preference);

     constructor op_ref_reg(op : tasmop;_op1 : preference;_op2 : tregister);
     { this is only allowed if _op1 is an int value (_op1^.isintvalue=true) }
     constructor op_ref_ref(op : tasmop;_op1,_op2 : preference);

     constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
     constructor op_const_reg_reg(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : tregister);
     constructor op_const_ref_reg(op : tasmop;_op1 : longint;_op2 : preference;_op3 : tregister);
     constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; _op3 : preference);
     constructor op_const_reg_ref(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : preference);

     { this is for Jmp instructions }
     constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : pasmsymbol);

     constructor op_sym(op : tasmop;_op1 : pasmsymbol);
     constructor op_sym_ofs(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint);
     constructor op_sym_ofs_reg(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : tregister);
     constructor op_sym_ofs_ref(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);

     destructor done;virtual;
     function  getcopy:plinkedlist_item;virtual;
  private
     segprefix : tregister;
     procedure init(op : tasmop); { this need to be called by all constructor }
  end;


implementation
uses
  og386;

{*****************************************************************************
                                 TaiRegAlloc
*****************************************************************************}

    constructor tairegalloc.alloc(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=true;
        reg:=r;
      end;


    constructor tairegalloc.dealloc(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=false;
        reg:=r;
      end;


{*****************************************************************************
                                 Taalpha Constructors
*****************************************************************************}

    procedure taalpha.init(op : tasmop);
      begin
         typ:=ait_instruction;
         is_jmp:=false;
         segprefix:=R_NO;
         opcode:=op;
         ops:=0;
         condition:=c_none;
         fillchar(oper,sizeof(oper),0);
      end;

    constructor taalpha.op_none(op : tasmop);
      begin
         inherited init;
         init(op);
      end;


    constructor taalpha.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited init;
         init(op);
         ops:=1;
      end;


    constructor taalpha.op_const(op : tasmop;_op1 : longint);
      begin
         inherited init;
         init(op);
         ops:=1;
      end;


    constructor taalpha.op_ref(op : tasmop;_op1 : preference);
      begin
         inherited init;
         init(op);
         ops:=1;
      end;


    constructor taalpha.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_reg_ref(op : tasmop;_op1 : tregister;_op2 : preference);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_const_reg(op : tasmop;_op1 : longint;_op2 : tregister);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_const_const(op : tasmop;_op1,_op2 : longint);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_const_ref(op : tasmop;_op1 : longint;_op2 : preference);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;

    constructor taalpha.op_ref_reg(op : tasmop;_op1 : preference;_op2 : tregister);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_ref_ref(op : tasmop;_op1,_op2 : preference);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited init;
         init(op);
         ops:=3;
      end;

    constructor taalpha.op_const_reg_reg(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : tregister);
      begin
         inherited init;
         init(op);
         ops:=3;
      end;

     constructor taalpha.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister;_op3 : preference);
      begin
         inherited init;
         init(op);
         ops:=3;
      end;

     constructor taalpha.op_const_ref_reg(op : tasmop;_op1 : longint;_op2 : preference;_op3 : tregister);
      begin
         inherited init;
         init(op);
         ops:=3;
      end;

     constructor taalpha.op_const_reg_ref(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : preference);
      begin
         inherited init;
         init(op);
         ops:=3;
      end;


    constructor taalpha.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : pasmsymbol);
      begin
         inherited init;
         init(op);
         condition:=cond;
         ops:=1;
      end;


    constructor taalpha.op_sym(op : tasmop;_op1 : pasmsymbol);
      begin
         inherited init;
         init(op);
         ops:=1;
      end;


    constructor taalpha.op_sym_ofs(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint);
      begin
         inherited init;
         init(op);
         ops:=1;
      end;


    constructor taalpha.op_sym_ofs_reg(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : tregister);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;


    constructor taalpha.op_sym_ofs_ref(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);
      begin
         inherited init;
         init(op);
         ops:=2;
      end;

    destructor taalpha.done;
      var
        i : longint;
      begin
          for i:=1 to ops do
            if (oper[i-1].typ=top_ref) then
              dispose(oper[i-1].ref);
        inherited done;
      end;

    function taalpha.getcopy:plinkedlist_item;
      var
        i : longint;
        p : plinkedlist_item;
      begin
        p:=inherited getcopy;
        { make a copy of the references }
        for i:=1 to ops do
         if (paalpha(p)^.oper[i-1].typ=top_ref) then
          begin
            new(paalpha(p)^.oper[i-1].ref);
            paalpha(p)^.oper[i-1].ref^:=oper[i-1].ref^;
          end;
        getcopy:=p;
      end;

end.
{
  $Log$
  Revision 1.1  1999-08-03 00:24:01  michael
  + Initial implementation

}