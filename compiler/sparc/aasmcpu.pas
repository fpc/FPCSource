{*****************************************************************************
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman
    
    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
 ****************************************************************************}
unit aasmcpu;
{$INCLUDE fpcdefs.inc}
interface
uses
  cclasses,globals,verbose,
  cpuinfo,cpubase,
  symppu,
  aasmbase,aasmtai;
const
  MaxPrefixes=4;
type
  { alignment for operator }
  tai_align=class(tai_align_abstract)
 reg:tregister;
 constructor create(b:byte);
 constructor create_op(b:byte; _op:byte);
  end;
  taicpu = class(taicpu_abstract)
    opsize:topsize;
    constructor op_none(op:tasmop);
    constructor op_reg(op:tasmop;reg:tregister);
    constructor op_const(op:tasmop;_op1:aword);
    constructor op_ref(op:tasmop;const _op1:treference);
    constructor op_reg_reg(op:tasmop;_op1,_op2:tregister);
    constructor op_reg_ref(Op:TAsmOp;Reg:TRegister;const Ref:TReference);
    constructor op_reg_const(op:tasmop;_op1:tregister;_op2:aword);
    constructor op_const_reg(op:tasmop;_op1:aword;_op2:tregister);
    constructor op_ref_reg(Op:TAsmOp;const Ref:TReference;Reg:TRegister);
    constructor op_ref_ref(op:tasmop;_size:topsize;const _op1,_op2:treference);
    constructor op_reg_reg_reg(op:tasmop;_op1,_op2,_op3:tregister);
    constructor op_reg_const_reg(Op:TAsmOp;SrcReg:TRegister;value:aWord;DstReg:TRegister);
    constructor op_const_ref_reg(op:tasmop;_size:topsize;_op1:aword;const _op2:treference;_op3:tregister);
    constructor op_const_reg_ref(op:tasmop;_size:topsize;_op1:aword;_op2:tregister;const _op3:treference);

 { this is for Jmp instructions }
    constructor op_cond_sym(op:tasmop;cond:TAsmCond;_size:topsize;_op1:tasmsymbol);
    constructor op_sym(op:tasmop;_size:topsize;_op1:tasmsymbol);
    constructor op_sym_ofs(op:tasmop;_size:topsize;_op1:tasmsymbol;_op1ofs:longint);
    constructor op_sym_ofs_reg(op:tasmop;_size:topsize;_op1:tasmsymbol;_op1ofs:longint;_op2:tregister);
    constructor op_sym_ofs_ref(op:tasmop;_size:topsize;_op1:tasmsymbol;_op1ofs:longint;const _op2:treference);
    constructor op_caddr_reg(op:TAsmOp;rgb:TRegister;cnst:Integer;reg:TRegister);
    constructor op_raddr_reg(op:TAsmOp;rg1,rg2:TRegister;reg:TRegister);
    procedure changeopsize(siz:topsize);
    procedure loadcaddr(opidx:longint;aReg:TRegister;cnst:Integer);
    procedure loadraddr(opidx:longint;rg1,rg2:TRegister);
  private
    procedure init(_size:topsize);{this need to be called by all constructor}
  public
    { the next will reset all instructions that can change in pass 2 }
    procedure SetCondition(const c:TAsmCond);
  private
    { next fields are filled in pass1, so pass2 is faster }
    procedure Swatoperands;
  end;
PROCEDURE DoneAsm;
PROCEDURE InitAsm;
implementation
uses
  cutils,
  CpuGas;
const
  _size=S_SW;{To be removed soon}
{****************************************************************************
                          TAI_ALIGN
 ****************************************************************************}
constructor tai_align.create(b:byte);
  begin
    inherited create(b);
    reg.enum:= R_NONE;
  end;
constructor tai_align.create_op(b:byte; _op:byte);
  begin
    inherited create_op(b,_op);
    reg.enum:= R_NONE;
  end;
{*****************************************************************************
                             Taicpu Constructors
*****************************************************************************}
procedure taicpu.changeopsize(siz:topsize);
  begin
    opsize:=siz;
  end;
procedure taicpu.init(_size:topsize);
  begin
    opsize:=_size;
  end;
constructor taicpu.op_none(op:tasmop);
  begin
     inherited create(op);
     init(_size);
  end;
constructor taicpu.op_reg(op:tasmop;reg:tregister);
  begin
     inherited create(op);
     init(_size);
     ops:=1;
     loadreg(0,reg);
  end;
constructor taicpu.op_const(op:tasmop;_op1:aword);
  begin
     inherited create(op);
     init(_size);
     ops:=1;
     loadconst(0,aword(_op1));
  end;
constructor taicpu.op_ref(op:tasmop;const _op1:treference);
  begin
     inherited create(op);
     init(_size);
     ops:=1;
     loadref(0,_op1);
  end;
constructor taicpu.op_reg_reg(op:tasmop;_op1,_op2:tregister);
  begin
     inherited create(op);
     init(_size);
     ops:=2;
     loadreg(0,_op1);
     loadreg(1,_op2);
  end;
constructor taicpu.op_reg_const(op:tasmop;_op1:tregister; _op2:aword);
  begin
     inherited create(op);
     init(_size);
     ops:=2;
     loadreg(0,_op1);
     loadconst(1,aword(_op2));
  end;
constructor taicpu.op_reg_ref(Op:TAsmOp;Reg:TRegister;const Ref:TReference);
  begin
    if not(Op in [A_STB..A_STDFQ])
    then
      fail;
    inherited Create(Op);
    init(_size);
    ops:=2;
    LoadReg(0,Reg);
    LoadRef(1,Ref);
  end;
constructor taicpu.op_const_reg(op:tasmop;_op1:aword;_op2:tregister);
  begin
     inherited create(op);
     init(_size);
     ops:=2;
     loadconst(0,aword(_op1));
     loadreg(1,_op2);
  end;
constructor TAiCpu.op_ref_reg(Op:TAsmOp;const Ref:TReference;Reg:TRegister);
  begin
    if not(Op in [A_JMPL,A_FLUSH,A_LDSB..A_LDDC,A_RETT,A_SWAP])
    then
      InternalError(2003042900);
    inherited Create(Op);
    Init(S_SW);
    Ops:=2;
    LoadRef(0,Ref);
    LoadReg(1,Reg);
  end;
constructor taicpu.op_ref_ref(op:tasmop;_size:topsize;const _op1,_op2:treference);
  begin
     inherited create(op);
     init(_size);
     ops:=2;
     loadref(0,_op1);
     loadref(1,_op2);
  end;
constructor taicpu.op_reg_reg_reg(op:tasmop;_op1,_op2,_op3:tregister);
  begin
     inherited create(op);
     init(_size);
     ops:=3;
     loadreg(0,_op1);
     loadreg(1,_op2);
     loadreg(2,_op3);
  end;
constructor taicpu.op_reg_const_reg(op:TAsmOp;SrcReg:TRegister;Value:aWord;DstReg:TRegister);
  begin
    inherited Create(Op);
    Init(S_W);
    ops:=3;
    LoadReg(0,SrcReg);
    LoadConst(1,Value);
    LoadReg(2,DstReg);
  end;
constructor taicpu.op_const_ref_reg(op:tasmop;_size:topsize;_op1:aword;const _op2:treference;_op3:tregister);
  begin
     inherited create(op);
     init(_size);
     ops:=3;
     loadconst(0,aword(_op1));
     loadref(1,_op2);
     loadreg(2,_op3);
  end;
constructor taicpu.op_const_reg_ref(op:tasmop;_size:topsize;_op1:aword;_op2:tregister;const _op3:treference);
  begin
     inherited create(op);
     init(_size);
     ops:=3;
     loadconst(0,aword(_op1));
     loadreg(1,_op2);
     loadref(2,_op3);
  end;
constructor taicpu.op_cond_sym(op:tasmop;cond:TAsmCond;_size:topsize;_op1:tasmsymbol);
  begin
     inherited create(op);
     init(_size);
     condition:=cond;
     ops:=1;
     loadsymbol(0,_op1,0);
  end;
constructor taicpu.op_sym(op:tasmop;_size:topsize;_op1:tasmsymbol);
  begin
     inherited create(op);
     init(_size);
     ops:=1;
     loadsymbol(0,_op1,0);
  end;
constructor taicpu.op_sym_ofs(op:tasmop;_size:topsize;_op1:tasmsymbol;_op1ofs:longint);
  begin
     inherited create(op);
     init(_size);
     ops:=1;
     loadsymbol(0,_op1,_op1ofs);
  end;
constructor taicpu.op_sym_ofs_reg(op:tasmop;_size:topsize;_op1:tasmsymbol;_op1ofs:longint;_op2:tregister);
  begin
     inherited create(op);
     init(_size);
     ops:=2;
     loadsymbol(0,_op1,_op1ofs);
     loadreg(1,_op2);
  end;


constructor taicpu.op_sym_ofs_ref(op:tasmop;_size:topsize;_op1:tasmsymbol;_op1ofs:longint;const _op2:treference);
  begin
     inherited create(op);
     init(_size);
     ops:=2;
     loadsymbol(0,_op1,_op1ofs);
     loadref(1,_op2);
  end;

constructor taicpu.op_caddr_reg(op:TAsmOp;rgb:TRegister;cnst:Integer;reg:TRegister);
  begin
    inherited create(op);
    init(S_SW);
    ops:=2;
    loadcaddr(0,rgb,cnst);
    loadreg(1,reg);
  end;
constructor taicpu.op_raddr_reg(op:TAsmOp;rg1,rg2,reg:TRegister);
  begin
    inherited create(op);
    init(S_SW);
    ops:=2;
    loadraddr(0,rg1,rg2);
    loadreg(1,reg);
  end;
procedure taicpu.Swatoperands;
  var
    p:TOper;
  begin
    { Fix the operands which are in AT&T style and we need them in Intel style }
    case ops of
      2:begin
            { 0,1 -> 1,0 }
            p:=oper[0];
            oper[0]:=oper[1];
            oper[1]:=p;
          end;
      3:begin
            { 0,1,2 -> 2,1,0 }
            p:=oper[0];
            oper[0]:=oper[2];
            oper[2]:=p;
          end;
    end;
  end;
{ This check must be done with the operand in ATT order
  i.e.after swapping in the intel reader
  but before swapping in the NASM and TASM writers PM }
{*****************************************************************************
                            Assembler
*****************************************************************************}
procedure TAiCpu.SetCondition(const c:TAsmCond);
  const
    AsmCond2OpCode:array[TAsmCond]of TAsmOp=
      (A_BN,A_BNE,A_BE,A_BG,A_BLE,A_BGE,A_BL,A_BGU,A_BLEU,A_BCC,
A_BCS,A_BPOS,A_NEG,A_BVC,A_BVS,A_BA,A_BNE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE);
  begin
    inherited SetCondition(c);
    if Opcode=A_BA
    then
      begin
        is_jmp:=true;
        Opcode:=AsmCond2OpCode[c];
      {$IFDEF EXTDEBUG}
        WriteLn('In TAiCpu.SetCondition TAsmCond=',cond2str[c],'==>',std_op2str[OpCode]);
      {$ENDIF EXTDEBUG}
      end;
  end;
procedure taicpu.loadcaddr(opidx:longint;aReg:TRegister;cnst:Integer);
  begin
    if opidx>=ops
    then
      ops:=opidx+1;
    with oper[opidx] do
      begin
        typ:=top_caddr;
        regb:=aReg;
        const13:=cnst;
      end;
  end;
procedure taicpu.loadraddr(opidx:longint;rg1,rg2:TRegister);
  begin
    if opidx>=ops
    then
      ops:=opidx+1;
    with oper[opidx] do
      begin
        typ:=top_caddr;
        reg1:=rg1;
        reg2:=rg2;
      end;
  end;
procedure DoneAsm;
  begin
  end;
procedure InitAsm;
  begin
  end;
end.
{
    $Log$
    Revision 1.25  2003-05-07 11:45:02  mazen
    - removed unused code

    Revision 1.24  2003/05/07 11:28:26  mazen
    - method CheckNonCommutativeOpcode removed as not used

    Revision 1.23  2003/05/06 20:27:43  mazen
    * A_BI changed to A_BL

    Revision 1.22  2003/05/06 15:00:36  mazen
    - non used code removed to bring up with powerpc changes

    Revision 1.21  2003/04/29 11:06:15  mazen
    * test of invalid opcode/operand combination gives internal error

    Revision 1.20  2003/04/28 09:40:47  mazen
    * Debug message in SetCondition more explicit.

    Revision 1.19  2003/03/15 22:51:58  mazen
    * remaking sparc rtl compile

    Revision 1.18  2003/03/10 21:59:54  mazen
    * fixing index overflow in handling new registers arrays.

    Revision 1.17  2003/02/18 22:00:20  mazen
    * asm condition generation modified by TAiCpu.SetCondition

    Revision 1.16  2003/01/08 18:43:58  daniel
     * Tregister changed into a record

    Revision 1.15  2003/01/05 21:32:35  mazen
    * fixing several bugs compiling the RTL

    Revision 1.14  2002/12/14 15:02:03  carl
      * maxoperands -> max_operands (for portability in rautils.pas)
      * fix some range-check errors with loadconst
      + add ncgadd unit to m68k
      * some bugfix of a_param_reg with LOC_CREFERENCE

    Revision 1.13  2002/11/17 16:32:04  carl
      * memory optimization (3-4%) : cleanup of tai fields,
         cleanup of tdef and tsym fields.
      * make it work for m68k

    Revision 1.12  2002/11/10 19:07:46  mazen
    * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

    Revision 1.11  2002/11/06 11:31:24  mazen
    * op_reg_reg_reg don't need any more a TOpSize parameter

    Revision 1.10  2002/11/05 16:15:00  mazen
    *** empty log message ***

    Revision 1.9  2002/10/28 20:59:17  mazen
    * TOpSize values changed S_L --> S_SW

    Revision 1.8  2002/10/22 13:43:01  mazen
    - cga.pas redueced to an empty unit

    Revision 1.7  2002/10/20 19:01:38  mazen
    + op_raddr_reg and op_caddr_reg added to fix functions prologue

    Revision 1.6  2002/10/19 20:35:07  mazen
    * carl's patch applied

    Revision 1.5  2002/10/15 09:00:28  mazen
    * sone coding style modified

    Revision 1.4  2002/10/13 21:46:07  mazen
    * assembler output format fixed
}
