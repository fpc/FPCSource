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
 function  GetString:string;
 procedure CheckNonCommutativeOpcodes;
  procedure loadcaddr(opidx:longint;aReg:TRegister;cnst:Integer);
  procedure loadraddr(opidx:longint;rg1,rg2:TRegister);
  private
 procedure init(_size:topsize);{this need to be called by all constructor}
 public
     { the next will reset all instructions that can change in pass 2 }
     procedure ResetPass1;
     procedure ResetPass2;
     function  CheckIfValid:boolean;
     function  Pass1(offset:longint):longint;virtual;
     procedure SetCondition(const c:TAsmCond);
  private
     { next fields are filled in pass1, so pass2 is faster }
     insentry  : PInsEntry;
     insoffset,
     inssize   : longint;
     LastInsOffset : longint; { need to be public to be reset }
     function  InsEnd:longint;
     procedure create_ot;
     function  Matches(p:PInsEntry):longint;
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
      fail;
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
function taicpu.GetString:string;
  var
    i:longint;
    s:string;
    addsize:boolean;
  begin
    s:='['+std_op2str[opcode];
    for i:=1to ops do
     begin
       if i=1 then
        s:=s+' '
       else
        s:=s+',';
       { type }
       addsize:=false;
       if (oper[i-1].ot and OT_XMMREG)=OT_XMMREG then
        s:=s+'xmmreg'
       else
         if (oper[i-1].ot and OT_MMXREG)=OT_MMXREG then
          s:=s+'mmxreg'
       else
         if (oper[i-1].ot and OT_FPUREG)=OT_FPUREG then
          s:=s+'fpureg'
       else
        if (oper[i-1].ot and OT_REGISTER)=OT_REGISTER then
         begin
           s:=s+'reg';
           addsize:=true;
         end
       else
        if (oper[i-1].ot and OT_IMMEDIATE)=OT_IMMEDIATE then
         begin
           s:=s+'imm';
           addsize:=true;
         end
       else
        if (oper[i-1].ot and OT_MEMORY)=OT_MEMORY then
         begin
           s:=s+'mem';
           addsize:=true;
         end
       else
         s:=s+'???';
       { size }
       if addsize then
        begin
          if (oper[i-1].ot and OT_BITS8)<>0 then
            s:=s+'8'
          else
           if (oper[i-1].ot and OT_BITS16)<>0 then
            s:=s+'16'
          else
           if (oper[i-1].ot and OT_BITS32)<>0 then
            s:=s+'32'
          else
            s:=s+'??';
          { signed }
          if (oper[i-1].ot and OT_SIGNED)<>0 then
           s:=s+'s';
        end;
     end;
    GetString:=s+']';
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

procedure taicpu.create_ot;
{
  this function will also fix some other fields which only needs to be once
}
var
  i,l,relsize:longint;
begin
  if ops=0 then
   exit;
  { update oper[].ot field }
  for i:=0 to ops-1 do
   with oper[i] do
begin
  case typ of
    top_reg:
      {ot:=reg2type[reg]};
    top_ref:
      begin
      { create ot field }
        {if (ot and OT_SIZE_MASK)=0 then
          ot:=OT_MEMORY or opsize_2_type[i,opsize]
        else
          ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
        if (ref^.base=R_NONE) and (ref^.index=R_NONE) then
          ot:=ot or OT_MEM_OFFS;}
      { fix scalefactor }
        if (ref^.index.enum=R_NONE) then
         ref^.scalefactor:=0
        else
         if (ref^.scalefactor=0) then
          ref^.scalefactor:=1;
      end;
    top_const:
      begin
        if (opsize<>S_W) and (longint(val)>=-128) and (val<=127) then
          ot:=OT_IMM8 or OT_SIGNED
        else
          ot:=OT_IMMEDIATE {or opsize_2_type[i,opsize];}
      end;
    top_symbol:
      begin
        if LastInsOffset=-1 then
         l:=0
        else
         l:=InsOffset-LastInsOffset;
        inc(l,symofs);
        if assigned(sym) then
         inc(l,sym.address);
        { instruction size will then always become 2 (PFV) }
        relsize:=(InsOffset+2)-l;
        if (not assigned(sym) or
            ((sym.currbind<>AB_EXTERNAL) and (sym.address<>0))) and
           (relsize>=-128) and (relsize<=127) then
         ot:=OT_IMM32 or OT_SHORT
        else
         ot:=OT_IMM32 or OT_NEAR;
      end;
  end;
end;
end;


function taicpu.InsEnd:longint;
begin
  InsEnd:=InsOffset+InsSize;
end;


function taicpu.Matches(p:PInsEntry):longint;
{ * IF_SM stands for Size Match:any operand whose size is not
 * explicitly specified by the template is `really' intended to be
 * the same size as the first size-specified operand.
 * Non-specification is tolerated in the input instruction, but
 * _wrong_ specification is not.
 *
 * IF_SM2 invokes Size Match on only the first _two_ operands, for
 * three-operand instructions such as SHLD:it implies that the
 * first two operands must match in size, but that the third is
 * required to be _unspecified_.
 *
 * IF_SB invokes Size Byte:operands with unspecified size in the
 * template are really bytes, and so no non-byte specification in
 * the input instruction will be tolerated. IF_SW similarly invokes
 * Size Word, and IF_SD invokes Size Doubleword.
 *
 * (The default state if neither IF_SM nor IF_SM2 is specified is
 * that any operand with unspecified size in the template is
 * required to have unspecified size in the instruction too...)
}
var
  i,j,asize,oprs:longint;
  siz:array[0..2] of longint;
begin
  Matches:=100;

  { Check the opcode and operands }
  if (p^.opcode<>opcode) or (p^.ops<>ops) then
   begin
 Matches:=0;
 exit;
   end;

  { Check that no spurious colons or TOs are present }
  for i:=0 to p^.ops-1 do
   if (oper[i].ot and (not p^.optypes[i]) and (OT_COLON or OT_TO))<>0 then
begin
  Matches:=0;
  exit;
end;

  { Check that the operand flags all match up }
  for i:=0 to p^.ops-1 do
   begin
 if ((p^.optypes[i] and (not oper[i].ot)) or
     ((p^.optypes[i] and OT_SIZE_MASK) and
      ((p^.optypes[i] xor oper[i].ot) and OT_SIZE_MASK)))<>0 then
  begin
    if ((p^.optypes[i] and (not oper[i].ot) and OT_NON_SIZE) or
        (oper[i].ot and OT_SIZE_MASK))<>0 then
     begin
       Matches:=0;
       exit;
     end
    else
     Matches:=1;
  end;
   end;

{ Check operand sizes }
  { as default an untyped size can get all the sizes, this is different
from nasm, but else we need to do a lot checking which opcodes want
size or not with the automatic size generation }
  asize:=longint($ffffffff);
  if (p^.flags and IF_SB)<>0 then
asize:=OT_BITS8
  else if (p^.flags and IF_SW)<>0 then
asize:=OT_BITS16
  else if (p^.flags and IF_SD)<>0 then
asize:=OT_BITS32;
  if (p^.flags and IF_ARMASK)<>0 then
   begin
 siz[0]:=0;
 siz[1]:=0;
 siz[2]:=0;
 if (p^.flags and IF_AR0)<>0 then
  siz[0]:=asize
 else if (p^.flags and IF_AR1)<>0 then
  siz[1]:=asize
 else if (p^.flags and IF_AR2)<>0 then
  siz[2]:=asize;
   end
  else
   begin
   { we can leave because the size for all operands is forced to be
 the same
 but not if IF_SB IF_SW or IF_SD is set PM }
 if asize=-1 then
   exit;
 siz[0]:=asize;
 siz[1]:=asize;
 siz[2]:=asize;
   end;

  if (p^.flags and (IF_SM or IF_SM2))<>0 then
   begin
 if (p^.flags and IF_SM2)<>0 then
  oprs:=2
 else
  oprs:=p^.ops;
 for i:=0 to oprs-1 do
  if ((p^.optypes[i] and OT_SIZE_MASK) <> 0) then
   begin
     for j:=0 to oprs-1 do
      siz[j]:=p^.optypes[i] and OT_SIZE_MASK;
     break;
   end;
end
   else
oprs:=2;

  { Check operand sizes }
  for i:=0 to p^.ops-1 do
   begin
 if ((p^.optypes[i] and OT_SIZE_MASK)=0) and
    ((oper[i].ot and OT_SIZE_MASK and (not siz[i]))<>0) and
    { Immediates can always include smaller size }
    ((oper[i].ot and OT_IMMEDIATE)=0) and
     (((p^.optypes[i] and OT_SIZE_MASK) or siz[i])<(oper[i].ot and OT_SIZE_MASK)) then
  Matches:=2;
   end;
end;


procedure taicpu.ResetPass1;
begin
  { we need to reset everything here, because the choosen insentry
can be invalid for a new situation where the previously optimized
insentry is not correct }
  InsEntry:=nil;
  InsSize:=0;
  LastInsOffset:=-1;
end;


procedure taicpu.ResetPass2;
begin
  { we are here in a second pass, check if the instruction can be optimized }
  if assigned(InsEntry) and
 ((InsEntry^.flags and IF_PASS2)<>0) then
   begin
 InsEntry:=nil;
 InsSize:=0;
   end;
  LastInsOffset:=-1;
end;


function taicpu.CheckIfValid:boolean;
var
  m,i:longint;
begin
  CheckIfValid:=false;
{ Things which may only be done once, not when a second pass is done to
  optimize }
  if (Insentry=nil) or ((InsEntry^.flags and IF_PASS2)<>0) then
   begin
 { create the .ot fields }
 create_ot;
 { set the file postion }
 aktfilepos:=fileinfo;
   end
  else
   begin
 { we've already an insentry so it's valid }
 CheckIfValid:=true;
 exit;
   end;
{ Lookup opcode in the table }
  InsSize:=-1;
  i:=instabcache^[opcode];
  if i=-1 then
   begin
{$ifdef TP}
 Message1(asmw_e_opcode_not_in_table,'');
{$else}
 Message1(asmw_e_opcode_not_in_table,std_op2str[opcode]);
{$endif}
 exit;
   end;
//  insentry:=@instab[i];
  while (insentry^.opcode=opcode) do
   begin
 m:=matches(insentry);
 if m=100 then
  begin
    InsSize:=calcsize(insentry);
    {if (segprefix<>R_NONE) then
     inc(InsSize);}{No segprefix!}
    { For opsize if size if forced }
    if (insentry^.flags and (IF_SB or IF_SW or IF_SD))<>0 then
       begin
         if (insentry^.flags and IF_ARMASK)=0 then
           begin
             if (insentry^.flags and IF_SB)<>0 then
               begin
                 if opsize=S_NO then
                   opsize:=S_B;
               end
             else if (insentry^.flags and IF_SW)<>0 then
               begin
                 if opsize=S_NO then
                   opsize:=S_W;
               end
             else if (insentry^.flags and IF_SD)<>0 then
               begin
                 if opsize=S_NO then
                   opsize:=S_SW;
               end;
           end;
       end;
    CheckIfValid:=true;
    exit;
  end;
 inc(i);
//     insentry:=@instab[i];
   end;
  if insentry^.opcode<>opcode then
   Message1(asmw_e_invalid_opcode_and_operands,GetString);
{ No instruction found, set insentry to nil and inssize to -1 }
  insentry:=nil;
  inssize:=-1;
end;
function taicpu.Pass1(offset:longint):longint;
begin
  Pass1:=0;
{ Save the old offset and set the new offset }
  InsOffset:=Offset;
{ Things which may only be done once, not when a second pass is done to
  optimize }
  if Insentry=nil then
   begin
 { Check if error last time then InsSize=-1 }
 if InsSize=-1 then
  exit;
 { set the file postion }
 aktfilepos:=fileinfo;
   end
  else
   begin
{$ifdef PASS2FLAG}
 { we are here in a second pass, check if the instruction can be optimized }
 if (InsEntry^.flags and IF_PASS2)=0 then
  begin
    Pass1:=InsSize;
    exit;
  end;
 { update the .ot fields, some top_const can be updated }
 create_ot;
{$endif}
   end;
{ Check if it's a valid instruction }
  if CheckIfValid then
   begin
 LastInsOffset:=InsOffset;
 Pass1:=InsSize;
 exit;
   end;
  LastInsOffset:=-1;
end;
procedure TAiCpu.SetCondition(const c:TAsmCond);
  const
    AsmCond2OpCode:array[TAsmCond]of TAsmOp=
      (A_BN,A_BNE,A_BE,A_BG,A_BLE,A_BGE,A_BI,A_BGU,A_BLEU,A_BCC,
A_BCS,A_BPOS,A_NEG,A_BVC,A_BVS,A_BA,A_BNE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE,A_NONE);
  begin
    inherited SetCondition(c);
    if Opcode=A_BA
    then
      begin
        is_jmp:=true;
        Opcode:=AsmCond2OpCode[c];
      {$IFDEF EXTDEBUG}
        WriteLn('In TAiCpu.SetCondition TAsmCond=',Byte(Opcode),'==>',std_op2str[AsmCond2OpCode[c]]);
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
    Revision 1.19  2003-03-15 22:51:58  mazen
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
