{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
  cpubase,tainst;

type
  paiframe = ^taiframe;
  taiframe = object(tai)
     G,R : TRegister;
     LS,LU : longint;
    Constructor init (GP : Tregister; Localsize : Longint; RA : TRegister; L : longint);
    end;

  paient = ^taient;
  taient = object(tai)
    Name : string;
    Constructor Init (ProcName : String);
    end;


  paicpu = ^taicpu;
  taicpu = object(tainstruction)
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
     constructor op_reg_const_reg(op : tasmop;_op1 : tregister;_op2 : longint;_op3 : tregister);
     constructor op_const_ref_reg(op : tasmop;_op1 : longint;_op2 : preference;_op3 : tregister);
     constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; _op3 : preference);
     constructor op_const_reg_ref(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : preference);
     constructor op_reg_ref_const(op : tasmop;_op1 : tregister;_op2 : preference;_op3 : longint);

     { this is for Jmp instructions }
     constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : pasmsymbol);

     constructor op_sym(op : tasmop;_op1 : pasmsymbol);
     constructor op_sym_ofs(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint);
     constructor op_sym_ofs_reg(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : tregister);
     constructor op_sym_ofs_ref(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);

     function  getcopy:plinkedlist_item;virtual;
  private
     segprefix : tregister;
  end;


implementation


{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}


    constructor taicpu.op_none(op : tasmop);
      begin
         inherited init(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited init(op);
         ops:=1;
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited init(op);
         ops:=1;
      end;


    constructor taicpu.op_ref(op : tasmop;_op1 : preference);
      begin
         inherited init(op);
         ops:=1;
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;_op2 : preference);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_const_reg(op : tasmop;_op1 : longint;_op2 : tregister);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_const_const(op : tasmop;_op1,_op2 : longint);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_const_ref(op : tasmop;_op1 : longint;_op2 : preference);
      begin
         inherited init(op);
         ops:=2;
      end;

    constructor taicpu.op_ref_reg(op : tasmop;_op1 : preference;_op2 : tregister);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_ref_ref(op : tasmop;_op1,_op2 : preference);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited init(op);
         ops:=3;
      end;

    constructor taicpu.op_reg_const_reg(op : tasmop;_op1 : tregister;_op2 : longint;_op3 : tregister);
      begin
         inherited init(op);
         ops:=3;
      end;

     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister;_op3 : preference);
      begin
         inherited init(op);
         ops:=3;
      end;

     constructor taicpu.op_const_ref_reg(op : tasmop;_op1 : longint;_op2 : preference;_op3 : tregister);
      begin
         inherited init(op);
         ops:=3;
      end;

     constructor taicpu.op_const_reg_ref(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : preference);
      begin
         inherited init(op);
         ops:=3;
      end;

     constructor taicpu.op_reg_ref_const(op : tasmop;_op1 : tregister;_op2 : preference;_op3 : longint);
      begin
         inherited init(op);
         ops:=3;
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : pasmsymbol);
      begin
         inherited init(op);
         condition:=cond;
         ops:=1;
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : pasmsymbol);
      begin
         inherited init(op);
         ops:=1;
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint);
      begin
         inherited init(op);
         ops:=1;
      end;


    constructor taicpu.op_sym_ofs_reg(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : tregister);
      begin
         inherited init(op);
         ops:=2;
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : pasmsymbol;_op1ofs:longint;_op2 : preference);
      begin
         inherited init(op);
         ops:=2;
      end;

    function taicpu.getcopy:plinkedlist_item;
      var
        i : longint;
        p : plinkedlist_item;
      begin
        p:=inherited getcopy;
        { make a copy of the references }
        for i:=1 to ops do
         if (paicpu(p)^.oper[i-1].typ=top_ref) then
          begin
            new(paicpu(p)^.oper[i-1].ref);
            paicpu(p)^.oper[i-1].ref^:=oper[i-1].ref^;
          end;
        getcopy:=p;
      end;

    Constructor taiframe.init (GP : Tregister; Localsize : Longint; RA : TRegister; L : longint);

    begin
      Inherited Init;
      typ:=ait_frame;
      G:=GP;
      R:=RA;
      LS:=LocalSize;
      LU:=L;
    end;

    Constructor taient.Init (ProcName : String);

    begin
      Inherited init;
      typ:=ait_ent;
      Name:=ProcName;
    end;

end.
{
  $Log$
  Revision 1.1  2002-09-29 22:34:17  florian
    * cpuasm renamed to aasmcpu

  Revision 1.2  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/18 09:06:54  florian
    * alpha files moved compiler/alpha

}
