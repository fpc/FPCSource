{
    $Id$
    Copyright (c) 1999-2002 by Jonas Maebe

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
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,aasmtai,
  aasmbase,globals,verbose,
  cginfo,cpubase,cpuinfo;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;


    type
      taicpu = class(taicpu_abstract)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : longint);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
         constructor op_const_reg(op:tasmop; _op1: longint; _op2: tregister);

         constructor op_const_const(op : tasmop;_op1,_op2 : longint);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: Longint);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         constructor op_const_reg_reg(op : tasmop;_op1 : longint;_op2, _op3 : tregister);
         constructor op_const_reg_const(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : longint);

         constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
         constructor op_reg_bool_reg_reg(op : tasmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
         constructor op_reg_bool_reg_const(op : tasmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: longint);

         constructor op_reg_reg_reg_const_const(op : tasmop;_op1,_op2,_op3 : tregister;_op4,_op5 : Longint);
         constructor op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : Longint);


         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_const_const_sym(op : tasmop;_op1,_op2 : longint;_op3: tasmsymbol);


         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

         procedure loadbool(opidx:longint;_b:boolean);


         function is_nop: boolean; override;
         function is_move:boolean; override;

         { register spilling code }
         function spilling_decode_loadstore(op: tasmop; var counterpart: tasmop; var wasload: boolean): boolean;override;
         function spilling_create_loadstore(op: tasmop; r:tregister; const ref:treference): tai;override;
         function spilling_create_load(const ref:treference;r:tregister): tai;override;
         function spilling_create_store(r:tregister; const ref:treference): tai;override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;


implementation

uses cutils,rgobj;

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


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,aword(_op1));
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: longint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aword(_op2));
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: longint; _op2: tregister);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_op1,_op2 : longint);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,aword(_op1));
         loadconst(1,aword(_op2));
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: Longint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
      end;

     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;

     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;

    constructor taicpu.op_const_reg_reg(op : tasmop;_op1 : longint;_op2, _op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

     constructor taicpu.op_const_reg_const(op : tasmop;_op1 : longint;_op2 : tregister;_op3 : longint);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
      end;


     constructor taicpu.op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_reg(op : tasmop;_op1: tregister;_op2:boolean;_op3,_op4:tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;

     constructor taicpu.op_reg_bool_reg_const(op : tasmop;_op1: tregister;_op2:boolean;_op3:tregister;_op4: longint);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadbool(0,_op2);
         loadreg(0,_op3);
         loadconst(0,cardinal(_op4));
      end;


     constructor taicpu.op_reg_reg_reg_const_const(op : tasmop;_op1,_op2,_op3 : tregister;_op4,_op5 : Longint);
      begin
         inherited create(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadconst(3,cardinal(_op4));
         loadconst(4,cardinal(_op5));
      end;

     constructor taicpu.op_reg_reg_const_const_const(op : tasmop;_op1,_op2 : tregister;_op3,_op4,_op5 : Longint);
      begin
         inherited create(op);
         ops:=5;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aword(_op3));
         loadconst(3,cardinal(_op4));
         loadconst(4,cardinal(_op5));
      end;

    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;

     constructor taicpu.op_const_const_sym(op : tasmop;_op1,_op2 : longint; _op3: tasmsymbol);
      begin
         inherited create(op);
         ops:=3;
         loadconst(0,aword(_op1));
         loadconst(1,aword(_op2));
         loadsymbol(2,_op3,0);
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


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


{ ****************************** newra stuff *************************** }

    function taicpu.is_nop: boolean;
      begin
        { we don't insert any more nops than necessary }
        is_nop :=
          ((opcode=A_MR) and (oper[0].typ=top_reg) and (oper[1].typ=top_reg) and (oper[0].reg=oper[1].reg));
      end;


    function taicpu.is_move:boolean;
      begin
        is_move := (opcode = A_MR) or
                   (opcode = A_EXTSB) or
                   (opcode = A_EXTSH) or
                   ((opcode = A_RLWINM) and
                    (oper[2].val = 0) and
                    (oper[4].val = 31) and
                    (oper[3].val in [31-8+1,31-16+1]));
      end;


    function taicpu.spilling_decode_loadstore(op: tasmop; var counterpart: tasmop; var wasload: boolean): boolean;
      begin
        result := true;
        wasload := true;
        case op of
          A_LBZ:
            begin
              counterpart := A_STB;
            end;
          A_LBZX:
            begin
              counterpart := A_STBX;
            end;
          A_LHZ,A_LHA:
            begin
              counterpart := A_STH;
            end;
          A_LHZX,A_LHAX:
            begin
              counterpart := A_STHX;
            end;
          A_LWZ:
            begin
              counterpart := A_STW;
            end;
          A_LWZX:
            begin
              counterpart := A_STWX;
            end;
          A_STB:
            begin
              counterpart := A_LBZ;
              wasload := false;
            end;
          A_STBX:
            begin
              counterpart := A_LBZX;
              wasload := false;
            end;
          A_STH:
            begin
              counterpart := A_LHZ;
              wasload := false;
            end;
          A_STHX:
            begin
              counterpart := A_LHZX;
              wasload := false;
            end;
          A_STW:
            begin
              counterpart := A_LWZ;
              wasload := false;
            end;
          A_STWX:
            begin
              counterpart := A_LWZX;
              wasload := false;
            end;
          A_LBZU,A_LBZUX,A_LHZU,A_LHZUX,A_LHAU,A_LHAUX,
          A_LWZU,A_LWZUX,A_STBU,A_STBUX,A_STHU,A_STHUX,
          A_STWU,A_STWUX:
            internalerror(2003070602);
          else
            result := false;
        end;
      end;


    function taicpu.spilling_create_loadstore(op: tasmop; r:tregister; const ref:treference): tai;
      begin
        result:=taicpu.op_reg_ref(opcode,r,ref);
      end;


    function taicpu.spilling_create_load(const ref:treference;r:tregister): tai;
      begin
        result:=taicpu.op_reg_ref(A_LWZ,r,ref);
      end;


    function taicpu.spilling_create_store(r:tregister; const ref:treference): tai;
      begin
        result:=taicpu.op_reg_ref(A_STW,r,ref);
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
  Revision 1.17  2003-09-03 19:35:24  peter
    * powerpc compiles again

  Revision 1.16  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.15.2.1  2003/08/31 21:08:16  peter
    * first batch of sparc fixes

  Revision 1.15  2003/08/18 21:27:00  jonas
    * some newra optimizations (eliminate lots of moves between registers)

  Revision 1.14  2003/08/17 16:53:19  jonas
    * fixed compilation of ppc compiler with -dnewra

  Revision 1.13  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.12  2002/09/30 23:16:49  jonas
    * is_nop() now identifies "mr rA,rA" instructions for removal

  Revision 1.11  2003/07/23 10:58:06  jonas
    - disabled some debugging code

  Revision 1.10  2003/07/06 21:26:06  jonas
    * committed wrong file previously :(

  Revision 1.8  2003/06/14 22:32:43  jonas
    * ppc compiles with -dnewra, haven't tried to compile anything with it
      yet though

  Revision 1.7  2003/06/14 14:53:50  jonas
    * fixed newra cycle for x86
    * added constants for indicating source and destination operands of the
      "move reg,reg" instruction to aasmcpu (and use those in rgobj)

  Revision 1.6  2003/05/11 11:08:25  jonas
    + op_reg_reg_reg_const_const (for rlwnm)

  Revision 1.5  2003/03/12 22:43:38  jonas
    * more powerpc and generic fixes related to the new register allocator

  Revision 1.4  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.3  2002/09/17 18:26:02  jonas
    - removed taicpu.destroy, its job is already handled by
      taicpu_abstract.destroy() and this caused heap corruption

  Revision 1.2  2002/07/26 11:19:57  jonas
    * fixed range errors

  Revision 1.1  2002/07/07 09:44:31  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.8  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.7  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.4  2002/05/13 19:52:46  peter
    * a ppcppc can be build again

}
