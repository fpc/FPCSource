{******************************************************************************
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
  procedure CheckNonCommutativeOpcodes;
  procedure loadcaddr(opidx:longint;aReg:TRegister;cnst:Integer);
  procedure loadraddr(opidx:longint;rg1,rg2:TRegister);
  private
 procedure init(_size:topsize);{this need to be called by all constructor}
 public
     { the next will reset all instructions that can change in pass 2 }
     procedure SetCondition(const c:TAsmCond);
  private
     { next fields are filled in pass1, so pass2 is faster }
     insentry  : PInsEntry;
     insoffset,
     inssize   : longint;
     LastInsOffset : longint; { need to be public to be reset }
     function  InsEnd:longint;
     function  calcsize(p:PInsEntry):longint;
     function  NeedAddrPrefix(opidx:byte):boolean;
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
procedure taicpu.CheckNonCommutativeOpcodes;
begin
{  if ((ops=2) and
 (oper[0].typ=top_reg) and
 (oper[1].typ=top_reg) and
 (oper[0].reg IN [R_F0..RF31])) or
 (ops=0) then
  if opcode=A_FSUBR then
    opcode:=A_FSUB
  else if opcode=A_FSUB then
    opcode:=A_FSUBR
  else if opcode=A_FDIVR then
    opcode:=A_FDIV
  else if opcode=A_FDIV then
    opcode:=A_FDIVR
  else if opcode=A_FSUBRP then
    opcode:=A_FSUBP
  else if opcode=A_FSUBP then
    opcode:=A_FSUBRP
  else if opcode=A_FDIVRP then
    opcode:=A_FDIVP
  else if opcode=A_FDIVP then
    opcode:=A_FDIVRP;
   if  ((ops=1) and
  (oper[0].typ=top_reg) and
  (oper[0].reg in [R_ST1..R_ST7])) then
  if opcode=A_FSUBRP then
    opcode:=A_FSUBP
  else if opcode=A_FSUBP then
    opcode:=A_FSUBRP
  else if opcode=A_FDIVRP then
    opcode:=A_FDIVP
  else if opcode=A_FDIVP then
    opcode:=A_FDIVRP;}
end;


{*****************************************************************************
                            Assembler
*****************************************************************************}
type
  ea=packed record
sib_present:boolean;
bytes:byte;
size:byte;
modrm:byte;
sib:byte;
  end;
function taicpu.InsEnd:longint;
begin
  InsEnd:=InsOffset+InsSize;
end;
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
function taicpu.NeedAddrPrefix(opidx:byte):boolean;
var
  i,b:tregister;
begin
{  if (OT_MEMORY and (not oper[opidx].ot))=0 then
   begin
 i:=oper[opidx].ref^.index;
 b:=oper[opidx].ref^.base;
 if not(i in [R_NONE,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI]) or
    not(b in [R_NONE,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI]) then
  begin
    NeedAddrPrefix:=true;
    exit;
  end;
   end;}
  NeedAddrPrefix:=false;
end;


function regval(r:tregister):byte;
begin
  {case r of
R_EAX,R_AX,R_AL,R_ES,R_CR0,R_DR0,R_ST,R_ST0,R_MM0,R_XMM0:
  regval:=0;
R_ECX,R_CX,R_CL,R_CS,R_DR1,R_ST1,R_MM1,R_XMM1:
  regval:=1;
R_EDX,R_DX,R_DL,R_SS,R_CR2,R_DR2,R_ST2,R_MM2,R_XMM2:
  regval:=2;
R_EBX,R_BX,R_BL,R_DS,R_CR3,R_DR3,R_TR3,R_ST3,R_MM3,R_XMM3:
  regval:=3;
R_ESP,R_SP,R_AH,R_FS,R_CR4,R_TR4,R_ST4,R_MM4,R_XMM4:
  regval:=4;
R_EBP,R_BP,R_CH,R_GS,R_TR5,R_ST5,R_MM5,R_XMM5:
  regval:=5;
R_ESI,R_SI,R_DH,R_DR6,R_TR6,R_ST6,R_MM6,R_XMM6:
  regval:=6;
R_EDI,R_DI,R_BH,R_DR7,R_TR7,R_ST7,R_MM7,R_XMM7:
  regval:=7;
else}
  begin
    internalerror(777001);
    regval:=0;
  end;
{  end;}
end;


function process_ea(const input:toper;var output:ea;rfield:longint):boolean;
{const
  regs:array[0..63] of tregister=(
R_MM0, R_EAX, R_AX, R_AL, R_XMM0, R_NONE, R_NONE, R_NONE,
R_MM1, R_ECX, R_CX, R_CL, R_XMM1, R_NONE, R_NONE, R_NONE,
R_MM2, R_EDX, R_DX, R_DL, R_XMM2, R_NONE, R_NONE, R_NONE,
R_MM3, R_EBX, R_BX, R_BL, R_XMM3, R_NONE, R_NONE, R_NONE,
R_MM4, R_ESP, R_SP, R_AH, R_XMM4, R_NONE, R_NONE, R_NONE,
R_MM5, R_EBP, R_BP, R_CH, R_XMM5, R_NONE, R_NONE, R_NONE,
R_MM6, R_ESI, R_SI, R_DH, R_XMM6, R_NONE, R_NONE, R_NONE,
R_MM7, R_EDI, R_DI, R_BH, R_XMM7, R_NONE, R_NONE, R_NONE
  );}
var
  j:longint;
  i,b:tregister;
  sym:tasmsymbol;
  md,s:byte;
  base,index,scalefactor,
  o:longint;
begin
  process_ea:=false;
{ register ? }
{  if (input.typ=top_reg) then
   begin
 j:=0;
 while (j<=high(regs)) do
  begin
    if input.reg=regs[j] then
     break;
    inc(j);
  end;
 if j<=high(regs) then
  begin
    output.sib_present:=false;
    output.bytes:=0;
    output.modrm:=$c0 or (rfield shl 3) or (j shr 3);
    output.size:=1;
    process_ea:=true;
  end;
 exit;
   end;}
{ memory reference }
  i:=input.ref^.index;
  b:=input.ref^.base;
  s:=input.ref^.scalefactor;
  o:=input.ref^.offset+input.ref^.offsetfixup;
  sym:=input.ref^.symbol;
{ it's direct address }
  if (b.enum=R_NONE) and (i.enum=R_NONE) then
   begin
 { it's a pure offset }
 output.sib_present:=false;
 output.bytes:=4;
 output.modrm:=5 or (rfield shl 3);
   end
  else
  { it's an indirection }
   begin
 { 16 bit address? }
{     if not((i in [R_NONE,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI]) and
        (b in [R_NONE,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI])) then
  Message(asmw_e_16bit_not_supported);}
{$ifdef OPTEA}
 { make single reg base }
 if (b=R_NONE) and (s=1) then
  begin
    b:=i;
    i:=R_NONE;
  end;
 { convert [3,5,9]*EAX to EAX+[2,4,8]*EAX }
{     if (b=R_NONE) and
    (((s=2) and (i<>R_ESP)) or
      (s=3) or (s=5) or (s=9)) then
  begin
    b:=i;
    dec(s);
  end;}
 { swap ESP into base if scalefactor is 1 }
{     if (s=1) and (i=R_ESP) then
  begin
    i:=b;
    b:=R_ESP;
  end;}
{$endif}
 { wrong, for various reasons }
{     if (i=R_ESP) or ((s<>1) and (s<>2) and (s<>4) and (s<>8) and (i<>R_NONE)) then
  exit;}
 { base }
{     case b of
   R_EAX:base:=0;
   R_ECX:base:=1;
   R_EDX:base:=2;
   R_EBX:base:=3;
   R_ESP:base:=4;
   R_NONE,
   R_EBP:base:=5;
   R_ESI:base:=6;
   R_EDI:base:=7;
 else
   exit;
 end;}
 { index }
{     case i of
   R_EAX:index:=0;
   R_ECX:index:=1;
   R_EDX:index:=2;
   R_EBX:index:=3;
   R_NONE:index:=4;
   R_EBP:index:=5;
   R_ESI:index:=6;
   R_EDI:index:=7;
 else
   exit;
 end;
 case s of
  0,
  1:scalefactor:=0;
  2:scalefactor:=1;
  4:scalefactor:=2;
  8:scalefactor:=3;
 else
  exit;
 end;
 if (b=R_NONE) or
    ((b<>R_EBP) and (o=0) and (sym=nil)) then
  md:=0
 else
  if ((o>=-128) and (o<=127) and (sym=nil)) then
   md:=1
  else
   md:=2;
 if (b=R_NONE) or (md=2) then
  output.bytes:=4
 else
  output.bytes:=md;}
 { SIB needed ? }
{     if (i=R_NONE) and (b<>R_ESP) then
  begin
    output.sib_present:=false;
    output.modrm:=(md shl 6) or (rfield shl 3) or base;
  end
 else
  begin
    output.sib_present:=true;
    output.modrm:=(md shl 6) or (rfield shl 3) or 4;
    output.sib:=(scalefactor shl 6) or (index shl 3) or base;
  end;}
   end;
  if output.sib_present then
   output.size:=2+output.bytes
  else
   output.size:=1+output.bytes;
  process_ea:=true;
end;


function taicpu.calcsize(p:PInsEntry):longint;
var
  codes:pchar;
  c:byte;
  len:longint;
  ea_data:ea;
begin
  len:=0;
  codes:=@p^.code;
  repeat
c:=ord(codes^);
inc(codes);
case c of
  0:
    break;
  1,2,3:
    begin
      inc(codes,c);
      inc(len,c);
    end;
  8,9,10:
    begin
      inc(codes);
      inc(len);
    end;
  4,5,6,7:
    begin
      if opsize=S_W then
        inc(len,2)
      else
        inc(len);
    end;
  15,
  12,13,14,
  16,17,18,
  20,21,22,
  40,41,42:
    inc(len);
  24,25,26,
  31,
  48,49,50:
    inc(len,2);
  28,29,30, { we don't have 16 bit immediates code }
  32,33,34,
  52,53,54,
  56,57,58:
    inc(len,4);
  192,193,194:
    if NeedAddrPrefix(c-192) then
     inc(len);
  208:
    inc(len);
  200,
  201,
  202,
  209,
  210,
  217,218,219:;
  216:
    begin
      inc(codes);
      inc(len);
    end;
  224,225,226:
    begin
      InternalError(777002);
    end;
  else
    begin
      if (c>=64) and (c<=191) then
       begin
         if not process_ea(oper[(c shr 3) and 7], ea_data, 0) then
          Message(asmw_e_invalid_effective_address)
         else
          inc(len,ea_data.size);
       end
      else
       InternalError(777003);
    end;
end;
  until false;
  calcsize:=len;
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
    Revision 1.23  2003-05-06 20:27:43  mazen
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
