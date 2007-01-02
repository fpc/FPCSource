{
    Copyright (c) 1998-2000 by Florian Klaempfl

    Implements the assembler classes specific for the Alpha

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
{
  Implements the assembler classes specific for the Alpha.
}
unit aasmcpu;

{$i fpcdefs.inc}

  interface

    uses
       aasmbase,globals,verbose,
       cpubase,aasmtai,aasmdata,aasmsym;

    type
      tai_frame = class(tai)
         G,R : TRegister;
         LS,LU : longint;
        Constructor Create (GP : Tregister; Localsize : Longint; RA : TRegister; L : longint);
        end;

      tai_ent = class(tai)
        Name : string;
        Constructor Create (const ProcName : String);
        end;


      taicpu = class(tai_cpu_abstract_sym)
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
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);

         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_sym_ofs_reg(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;_op2 : preference);
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
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited create(op);
         ops:=1;
      end;


    constructor taicpu.op_ref(op : tasmop;_op1 : preference);
      begin
         inherited create(op);
         ops:=1;
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;_op2 : preference);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_const_reg(op : tasmop;_op1 : longint;_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_const_const(op : tasmop;_op1,_op2 : longint);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_const_ref(op : tasmop;_op1 : longint;_op2 : preference);
      begin
         inherited create(op);
         ops:=2;
      end;

    constructor taicpu.op_ref_reg(op : tasmop;_op1 : preference;_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_ref_ref(op : tasmop;_op1,_op2 : preference);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
      end;

    constructor taicpu.op_reg_const_reg(op : tasmop;_op1 : tregister;_op2 : longint;_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
      end;

     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister;_op3 : preference);
      begin
         inherited create(op);
         ops:=3;
      end;

     constructor taicpu.op_const_ref_reg(op : tasmop;_op1 : longint;_op2 : preference;_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
      end;

     constructor taicpu.op_const_reg_ref(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : preference);
      begin
         inherited create(op);
         ops:=3;
      end;

     constructor taicpu.op_reg_ref_const(op : tasmop;_op1 : tregister;_op2 : preference;_op3 : longint);
      begin
         inherited create(op);
         ops:=3;
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         ops:=1;
      end;


    constructor taicpu.op_sym_ofs_reg(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;_op2 : preference);
      begin
         inherited create(op);
         ops:=2;
      end;

    Constructor tai_frame.create (GP : Tregister; Localsize : Longint; RA : TRegister; L : longint);

    begin
      Inherited Create;
      typ:=ait_frame;
      G:=GP;
      R:=RA;
      LS:=LocalSize;
      LU:=L;
    end;

    Constructor tai_ent.Create (const ProcName : String);

    begin
      Inherited Create;
      typ:=ait_ent;
      Name:=ProcName;
    end;

    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


    end.
