{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Contains the assembler object for the i386

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

 ****************************************************************************
}
unit cpuasm;

{$i fpcdefs.inc}

{ Optimize addressing and skip already passed nodes }
{$ifndef NASMDEBUG}
  {$define OPTEA}
  {$define PASS2FLAG}
{$endif ndef NASMDEBUG}

{ Give warnings when an immediate is found in the reference struct }
{.$define REF_IMMEDIATE_WARN}

interface

uses
  cclasses,tainst,
  aasm,globals,verbose,
  cpuinfo,cpubase;

const
  MaxPrefixes=4;

{*****************************************************************************
                              Instruction table
*****************************************************************************}

type
  tinsentry=packed record
    opcode  : tasmop;
    ops     : byte;
    optypes : array[0..2] of longint;
    code    : array[0..maxinfolen] of char;
    flags   : longint;
  end;
  pinsentry=^tinsentry;

  TInsTabCache=array[TasmOp] of longint;
  PInsTabCache=^TInsTabCache;

const
  InsTab:array[0..instabentries-1] of TInsEntry={$i i386tab.inc}

var
  InsTabCache : PInsTabCache;
{*****************************************************************************}

type
  TOperandOrder = (op_intel,op_att);

  { alignment for operator }
  tai_align = class(tai_align_abstract)
     reg       : tregister;
     constructor create(b:byte);
     constructor create_op(b: byte; _op: byte);
     function getfillbuf:pchar;
  end;

  taicpu = class(tainstruction)
     opsize    : topsize;
     constructor op_none(op : tasmop;_size : topsize);

     constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
     constructor op_const(op : tasmop;_size : topsize;_op1 : aword);
     constructor op_ref(op : tasmop;_size : topsize;const _op1 : treference);

     constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
     constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;const _op2 : treference);
     constructor op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: aword);

     constructor op_const_reg(op : tasmop;_size : topsize;_op1 : aword;_op2 : tregister);
     constructor op_const_const(op : tasmop;_size : topsize;_op1,_op2 : aword);
     constructor op_const_ref(op : tasmop;_size : topsize;_op1 : aword;const _op2 : treference);

     constructor op_ref_reg(op : tasmop;_size : topsize;const _op1 : treference;_op2 : tregister);
     { this is only allowed if _op1 is an int value (_op1^.isintvalue=true) }
     constructor op_ref_ref(op : tasmop;_size : topsize;const _op1,_op2 : treference);

     constructor op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
     constructor op_const_reg_reg(op : tasmop;_size : topsize;_op1 : aword;_op2 : tregister;_op3 : tregister);
     constructor op_const_ref_reg(op : tasmop;_size : topsize;_op1 : aword;const _op2 : treference;_op3 : tregister);
     constructor op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister; const _op3 : treference);
     constructor op_const_reg_ref(op : tasmop;_size : topsize;_op1 : aword;_op2 : tregister;const _op3 : treference);

     { this is for Jmp instructions }
     constructor op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);

     constructor op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
     constructor op_sym_ofs(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint);
     constructor op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);
     constructor op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

     procedure changeopsize(siz:topsize);

     function  GetString:string;
     procedure CheckNonCommutativeOpcodes;
  private
     FOperandOrder : TOperandOrder;
     procedure init(_size : topsize); { this need to be called by all constructor }
{$ifndef NOAG386BIN}
  public
     { the next will reset all instructions that can change in pass 2 }
     procedure ResetPass1;
     procedure ResetPass2;
     function  CheckIfValid:boolean;
     function  Pass1(offset:longint):longint;virtual;
     procedure Pass2;virtual;
     procedure SetOperandOrder(order:TOperandOrder);
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
     procedure gencode;
     function  NeedAddrPrefix(opidx:byte):boolean;
     procedure Swatoperands;
{$endif NOAG386BIN}
  end;

  procedure InitAsm;
  procedure DoneAsm;


implementation

uses
  cutils,
  ogbase,
  ag386att;

{****************************************************************************
                              TAI_ALIGN
 ****************************************************************************}

    constructor tai_align.create(b: byte);
      begin
        inherited create(b);
        reg := R_ECX;
      end;


    constructor tai_align.create_op(b: byte; _op: byte);
      begin
        inherited create_op(b,_op);
        reg := R_NO;
      end;


    function tai_align.getfillbuf:pchar;
      const
        alignarray:array[0..5] of string[8]=(
          #$8D#$B4#$26#$00#$00#$00#$00,
          #$8D#$B6#$00#$00#$00#$00,
          #$8D#$74#$26#$00,
          #$8D#$76#$00,
          #$89#$F6,
          #$90
        );
      var
        bufptr : pchar;
        j : longint;
      begin
        if not use_op then
         begin
           bufptr:=@buf;
           while (fillsize>0) do
            begin
              for j:=0 to 5 do
               if (fillsize>=length(alignarray[j])) then
                break;
              move(alignarray[j][1],bufptr^,length(alignarray[j]));
              inc(bufptr,length(alignarray[j]));
              dec(fillsize,length(alignarray[j]));
            end;
         end;
        getfillbuf:=pchar(@buf);
      end;


{*****************************************************************************
                                 Taicpu Constructors
*****************************************************************************}

    procedure taicpu.changeopsize(siz:topsize);
      begin
        opsize:=siz;
      end;


    procedure taicpu.init(_size : topsize);
      begin
         { default order is att }
         FOperandOrder:=op_att;
         segprefix:=R_NO;
         opsize:=_size;
{$ifndef NOAG386BIN}
         insentry:=nil;
         LastInsOffset:=-1;
         InsOffset:=0;
         InsSize:=0;
{$endif}
      end;


    constructor taicpu.op_none(op : tasmop;_size : topsize);
      begin
         inherited create(op);
         init(_size);
      end;


    constructor taicpu.op_reg(op : tasmop;_size : topsize;_op1 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : aword);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;_size : topsize;const _op1 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: aword);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : aword;_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : aword);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : aword;const _op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;_size : topsize;const _op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_ref_ref(op : tasmop;_size : topsize;const _op1,_op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadref(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : aword;_op2 : tregister;_op3 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister;const _op3 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_const_ref_reg(op : tasmop;_size : topsize;_op1 : aword;const _op2 : treference;_op3 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadconst(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_const_reg_ref(op : tasmop;_size : topsize;_op1 : aword;_op2 : tregister;const _op3 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadconst(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadsymbol(0,_op1,0);
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
         inherited create(op);
         init(_size);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;

    function taicpu.GetString:string;
      var
        i : longint;
        s : string;
        addsize : boolean;
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
        p : TOper;
      begin
        { Fix the operands which are in AT&T style and we need them in Intel style }
        case ops of
          2 : begin
                { 0,1 -> 1,0 }
                p:=oper[0];
                oper[0]:=oper[1];
                oper[1]:=p;
              end;
          3 : begin
                { 0,1,2 -> 2,1,0 }
                p:=oper[0];
                oper[0]:=oper[2];
                oper[2]:=p;
              end;
        end;
      end;


    procedure taicpu.SetOperandOrder(order:TOperandOrder);
      begin
        if FOperandOrder<>order then
         begin
           Swatoperands;
           FOperandOrder:=order;
         end;
      end;


{ This check must be done with the operand in ATT order
  i.e.after swapping in the intel reader
  but before swapping in the NASM and TASM writers PM }
procedure taicpu.CheckNonCommutativeOpcodes;
begin
  if ((ops=2) and
     (oper[0].typ=top_reg) and
     (oper[1].typ=top_reg) and
     { if the first is ST and the second is also a register
       it is necessarily ST1 .. ST7 }
     (oper[0].reg=R_ST)) or
     { ((ops=1) and
      (oper[0].typ=top_reg) and
      (oper[0].reg in [R_ST1..R_ST7]))  or}
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
        opcode:=A_FDIVRP;
end;


{*****************************************************************************
                                Assembler
*****************************************************************************}

{$ifndef NOAG386BIN}

type
  ea=packed record
    sib_present : boolean;
    bytes : byte;
    size  : byte;
    modrm : byte;
    sib   : byte;
  end;

procedure taicpu.create_ot;
{
  this function will also fix some other fields which only needs to be once
}
var
  i,l,relsize : longint;
begin
  if ops=0 then
   exit;
  { update oper[].ot field }
  for i:=0 to ops-1 do
   with oper[i] do
    begin
      case typ of
        top_reg :
          ot:=reg2type[reg];
        top_ref :
          begin
          { create ot field }
            if (ot and OT_SIZE_MASK)=0 then
              ot:=OT_MEMORY or opsize_2_type[i,opsize]
            else
              ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
            if (ref^.base=R_NO) and (ref^.index=R_NO) then
              ot:=ot or OT_MEM_OFFS;
          { fix scalefactor }
            if (ref^.index=R_NO) then
             ref^.scalefactor:=0
            else
             if (ref^.scalefactor=0) then
              ref^.scalefactor:=1;
          end;
        top_const :
          begin
            if (opsize<>S_W) and (longint(val)>=-128) and (val<=127) then
              ot:=OT_IMM8 or OT_SIGNED
            else
              ot:=OT_IMMEDIATE or opsize_2_type[i,opsize];
          end;
        top_symbol :
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
                ((sym.bind<>AB_EXTERNAL) and (sym.address<>0))) and
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
{ * IF_SM stands for Size Match: any operand whose size is not
 * explicitly specified by the template is `really' intended to be
 * the same size as the first size-specified operand.
 * Non-specification is tolerated in the input instruction, but
 * _wrong_ specification is not.
 *
 * IF_SM2 invokes Size Match on only the first _two_ operands, for
 * three-operand instructions such as SHLD: it implies that the
 * first two operands must match in size, but that the third is
 * required to be _unspecified_.
 *
 * IF_SB invokes Size Byte: operands with unspecified size in the
 * template are really bytes, and so no non-byte specification in
 * the input instruction will be tolerated. IF_SW similarly invokes
 * Size Word, and IF_SD invokes Size Doubleword.
 *
 * (The default state if neither IF_SM nor IF_SM2 is specified is
 * that any operand with unspecified size in the template is
 * required to have unspecified size in the instruction too...)
}
var
  i,j,asize,oprs : longint;
  siz : array[0..2] of longint;
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
  m,i : longint;
begin
  CheckIfValid:=false;
{ Things which may only be done once, not when a second pass is done to
  optimize }
  if (Insentry=nil) or ((InsEntry^.flags and IF_PASS2)<>0) then
   begin
     { We need intel style operands }
     SetOperandOrder(op_intel);
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
     Message1(asmw_e_opcode_not_in_table,gas_op2str[opcode]);
{$endif}
     exit;
   end;
  insentry:=@instab[i];
  while (insentry^.opcode=opcode) do
   begin
     m:=matches(insentry);
     if m=100 then
      begin
        InsSize:=calcsize(insentry);
        if (segprefix<>R_NO) then
         inc(InsSize);
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
                       opsize:=S_L;
                   end;
               end;
           end;
        CheckIfValid:=true;
        exit;
      end;
     inc(i);
     insentry:=@instab[i];
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


procedure taicpu.Pass2;
var
  c : longint;
begin
  { error in pass1 ? }
  if insentry=nil then
   exit;
  aktfilepos:=fileinfo;
  { Segment override }
  if (segprefix<>R_NO) then
   begin
     case segprefix of
       R_CS : c:=$2e;
       R_DS : c:=$3e;
       R_ES : c:=$26;
       R_FS : c:=$64;
       R_GS : c:=$65;
       R_SS : c:=$36;
     end;
     objectdata.writebytes(c,1);
     { fix the offset for GenNode }
     inc(InsOffset);
   end;
  { Generate the instruction }
  GenCode;
end;


function taicpu.NeedAddrPrefix(opidx:byte):boolean;
var
  i,b : tregister;
begin
  if (OT_MEMORY and (not oper[opidx].ot))=0 then
   begin
     i:=oper[opidx].ref^.index;
     b:=oper[opidx].ref^.base;
     if not(i in [R_NO,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI]) or
        not(b in [R_NO,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI]) then
      begin
        NeedAddrPrefix:=true;
        exit;
      end;
   end;
  NeedAddrPrefix:=false;
end;


function regval(r:tregister):byte;
begin
  case r of
    R_EAX,R_AX,R_AL,R_ES,R_CR0,R_DR0,R_ST,R_ST0,R_MM0,R_XMM0 :
      regval:=0;
    R_ECX,R_CX,R_CL,R_CS,R_DR1,R_ST1,R_MM1,R_XMM1 :
      regval:=1;
    R_EDX,R_DX,R_DL,R_SS,R_CR2,R_DR2,R_ST2,R_MM2,R_XMM2 :
      regval:=2;
    R_EBX,R_BX,R_BL,R_DS,R_CR3,R_DR3,R_TR3,R_ST3,R_MM3,R_XMM3 :
      regval:=3;
    R_ESP,R_SP,R_AH,R_FS,R_CR4,R_TR4,R_ST4,R_MM4,R_XMM4 :
      regval:=4;
    R_EBP,R_BP,R_CH,R_GS,R_TR5,R_ST5,R_MM5,R_XMM5 :
      regval:=5;
    R_ESI,R_SI,R_DH,R_DR6,R_TR6,R_ST6,R_MM6,R_XMM6 :
      regval:=6;
    R_EDI,R_DI,R_BH,R_DR7,R_TR7,R_ST7,R_MM7,R_XMM7 :
      regval:=7;
    else
      begin
        internalerror(777001);
        regval:=0;
      end;
  end;
end;


function process_ea(const input:toper;var output:ea;rfield:longint):boolean;
const
  regs : array[0..63] of tregister=(
    R_MM0, R_EAX, R_AX, R_AL, R_XMM0, R_NO, R_NO, R_NO,
    R_MM1, R_ECX, R_CX, R_CL, R_XMM1, R_NO, R_NO, R_NO,
    R_MM2, R_EDX, R_DX, R_DL, R_XMM2, R_NO, R_NO, R_NO,
    R_MM3, R_EBX, R_BX, R_BL, R_XMM3, R_NO, R_NO, R_NO,
    R_MM4, R_ESP, R_SP, R_AH, R_XMM4, R_NO, R_NO, R_NO,
    R_MM5, R_EBP, R_BP, R_CH, R_XMM5, R_NO, R_NO, R_NO,
    R_MM6, R_ESI, R_SI, R_DH, R_XMM6, R_NO, R_NO, R_NO,
    R_MM7, R_EDI, R_DI, R_BH, R_XMM7, R_NO, R_NO, R_NO
  );
var
  j     : longint;
  i,b   : tregister;
  sym   : tasmsymbol;
  md,s  : byte;
  base,index,scalefactor,
  o     : longint;
begin
  process_ea:=false;
{ register ? }
  if (input.typ=top_reg) then
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
   end;
{ memory reference }
  i:=input.ref^.index;
  b:=input.ref^.base;
  s:=input.ref^.scalefactor;
  o:=input.ref^.offset+input.ref^.offsetfixup;
  sym:=input.ref^.symbol;
{ it's direct address }
  if (b=R_NO) and (i=R_NO) then
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
     if not((i in [R_NO,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI]) and
            (b in [R_NO,R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESP,R_ESI,R_EDI])) then
      Message(asmw_e_16bit_not_supported);
{$ifdef OPTEA}
     { make single reg base }
     if (b=R_NO) and (s=1) then
      begin
        b:=i;
        i:=R_NO;
      end;
     { convert [3,5,9]*EAX to EAX+[2,4,8]*EAX }
     if (b=R_NO) and
        (((s=2) and (i<>R_ESP)) or
          (s=3) or (s=5) or (s=9)) then
      begin
        b:=i;
        dec(s);
      end;
     { swap ESP into base if scalefactor is 1 }
     if (s=1) and (i=R_ESP) then
      begin
        i:=b;
        b:=R_ESP;
      end;
{$endif}
     { wrong, for various reasons }
     if (i=R_ESP) or ((s<>1) and (s<>2) and (s<>4) and (s<>8) and (i<>R_NO)) then
      exit;
     { base }
     case b of
       R_EAX : base:=0;
       R_ECX : base:=1;
       R_EDX : base:=2;
       R_EBX : base:=3;
       R_ESP : base:=4;
       R_NO,
       R_EBP : base:=5;
       R_ESI : base:=6;
       R_EDI : base:=7;
     else
       exit;
     end;
     { index }
     case i of
       R_EAX : index:=0;
       R_ECX : index:=1;
       R_EDX : index:=2;
       R_EBX : index:=3;
       R_NO  : index:=4;
       R_EBP : index:=5;
       R_ESI : index:=6;
       R_EDI : index:=7;
     else
       exit;
     end;
     case s of
      0,
      1 : scalefactor:=0;
      2 : scalefactor:=1;
      4 : scalefactor:=2;
      8 : scalefactor:=3;
     else
      exit;
     end;
     if (b=R_NO) or
        ((b<>R_EBP) and (o=0) and (sym=nil)) then
      md:=0
     else
      if ((o>=-128) and (o<=127) and (sym=nil)) then
       md:=1
      else
       md:=2;
     if (b=R_NO) or (md=2) then
      output.bytes:=4
     else
      output.bytes:=md;
     { SIB needed ? }
     if (i=R_NO) and (b<>R_ESP) then
      begin
        output.sib_present:=false;
        output.modrm:=(md shl 6) or (rfield shl 3) or base;
      end
     else
      begin
        output.sib_present:=true;
        output.modrm:=(md shl 6) or (rfield shl 3) or 4;
        output.sib:=(scalefactor shl 6) or (index shl 3) or base;
      end;
   end;
  if output.sib_present then
   output.size:=2+output.bytes
  else
   output.size:=1+output.bytes;
  process_ea:=true;
end;


function taicpu.calcsize(p:PInsEntry):longint;
var
  codes : pchar;
  c     : byte;
  len     : longint;
  ea_data : ea;
begin
  len:=0;
  codes:=@p^.code;
  repeat
    c:=ord(codes^);
    inc(codes);
    case c of
      0 :
        break;
      1,2,3 :
        begin
          inc(codes,c);
          inc(len,c);
        end;
      8,9,10 :
        begin
          inc(codes);
          inc(len);
        end;
      4,5,6,7 :
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
      40,41,42 :
        inc(len);
      24,25,26,
      31,
      48,49,50 :
        inc(len,2);
      28,29,30, { we don't have 16 bit immediates code }
      32,33,34,
      52,53,54,
      56,57,58 :
        inc(len,4);
      192,193,194 :
        if NeedAddrPrefix(c-192) then
         inc(len);
      208 :
        inc(len);
      200,
      201,
      202,
      209,
      210,
      217,218,219 : ;
      216 :
        begin
          inc(codes);
          inc(len);
        end;
      224,225,226 :
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


procedure taicpu.GenCode;
{
 * the actual codes (C syntax, i.e. octal):
 * \0            - terminates the code. (Unless it's a literal of course.)
 * \1, \2, \3    - that many literal bytes follow in the code stream
 * \4, \6        - the POP/PUSH (respectively) codes for CS, DS, ES, SS
 *                 (POP is never used for CS) depending on operand 0
 * \5, \7        - the second byte of POP/PUSH codes for FS, GS, depending
 *                 on operand 0
 * \10, \11, \12 - a literal byte follows in the code stream, to be added
 *                 to the register value of operand 0, 1 or 2
 * \17           - encodes the literal byte 0. (Some compilers don't take
 *                 kindly to a zero byte in the _middle_ of a compile time
 *                 string constant, so I had to put this hack in.)
 * \14, \15, \16 - a signed byte immediate operand, from operand 0, 1 or 2
 * \20, \21, \22 - a byte immediate operand, from operand 0, 1 or 2
 * \24, \25, \26 - an unsigned byte immediate operand, from operand 0, 1 or 2
 * \30, \31, \32 - a word immediate operand, from operand 0, 1 or 2
 * \34, \35, \36 - select between \3[012] and \4[012] depending on 16/32 bit
 *                 assembly mode or the address-size override on the operand
 * \37           - a word constant, from the _segment_ part of operand 0
 * \40, \41, \42 - a long immediate operand, from operand 0, 1 or 2
 * \50, \51, \52 - a byte relative operand, from operand 0, 1 or 2
 * \60, \61, \62 - a word relative operand, from operand 0, 1 or 2
 * \64, \65, \66 - select between \6[012] and \7[012] depending on 16/32 bit
 *                 assembly mode or the address-size override on the operand
 * \70, \71, \72 - a long relative operand, from operand 0, 1 or 2
 * \1ab          - a ModRM, calculated on EA in operand a, with the spare
 *                 field the register value of operand b.
 * \2ab          - a ModRM, calculated on EA in operand a, with the spare
 *                 field equal to digit b.
 * \30x          - might be an 0x67 byte, depending on the address size of
 *                 the memory reference in operand x.
 * \310          - indicates fixed 16-bit address size, i.e. optional 0x67.
 * \311          - indicates fixed 32-bit address size, i.e. optional 0x67.
 * \320          - indicates fixed 16-bit operand size, i.e. optional 0x66.
 * \321          - indicates fixed 32-bit operand size, i.e. optional 0x66.
 * \322          - indicates that this instruction is only valid when the
 *                 operand size is the default (instruction to disassembler,
 *                 generates no code in the assembler)
 * \330          - a literal byte follows in the code stream, to be added
 *                 to the condition code value of the instruction.
 * \340          - reserve <operand 0> bytes of uninitialised storage.
 *                 Operand 0 had better be a segmentless constant.
}

var
  currval : longint;
  currsym : tasmsymbol;

  procedure getvalsym(opidx:longint);
  begin
    case oper[opidx].typ of
      top_ref :
        begin
          currval:=oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup;
          currsym:=oper[opidx].ref^.symbol;
        end;
      top_const :
        begin
          currval:=longint(oper[opidx].val);
          currsym:=nil;
        end;
      top_symbol :
        begin
          currval:=oper[opidx].symofs;
          currsym:=oper[opidx].sym;
        end;
      else
        Message(asmw_e_immediate_or_reference_expected);
    end;
  end;

const
  CondVal:array[TAsmCond] of byte=($0,
   $7, $3, $2, $6, $2, $4, $F, $D, $C, $E, $6, $2,
   $3, $7, $3, $5, $E, $C, $D, $F, $1, $B, $9, $5,
   $0, $A, $A, $B, $8, $4);
var
  c : byte;
  pb,
  codes : pchar;
  bytes : array[0..3] of byte;
  rfield,
  data,s,opidx : longint;
  ea_data : ea;
begin
{$ifdef EXTDEBUG}
  { safety check }
  if objectdata.currsectionsize<>insoffset then
   internalerror(200130121);
{$endif EXTDEBUG}
  { load data to write }
  codes:=insentry^.code;
  { Force word push/pop for registers }
  if (opsize=S_W) and ((codes[0]=#4) or (codes[0]=#6) or
      ((codes[0]=#1) and ((codes[2]=#5) or (codes[2]=#7)))) then
    begin
      bytes[0]:=$66;
      objectdata.writebytes(bytes,1);
    end;
  repeat
    c:=ord(codes^);
    inc(codes);
    case c of
      0 :
        break;
      1,2,3 :
        begin
          objectdata.writebytes(codes^,c);
          inc(codes,c);
        end;
      4,6 :
        begin
          case oper[0].reg of
            R_CS :
              begin
                if c=4 then
                 bytes[0]:=$f
                else
                 bytes[0]:=$e;
              end;
            R_NO,
            R_DS :
              begin
                if c=4 then
                 bytes[0]:=$1f
                else
                 bytes[0]:=$1e;
              end;
            R_ES :
              begin
                if c=4 then
                 bytes[0]:=$7
                else
                 bytes[0]:=$6;
              end;
            R_SS :
              begin
                if c=4 then
                 bytes[0]:=$17
                else
                 bytes[0]:=$16;
              end;
            else
              InternalError(777004);
          end;
          objectdata.writebytes(bytes,1);
        end;
      5,7 :
        begin
          case oper[0].reg of
            R_FS :
              begin
                if c=5 then
                 bytes[0]:=$a1
                else
                 bytes[0]:=$a0;
              end;
            R_GS :
              begin
                if c=5 then
                 bytes[0]:=$a9
                else
                 bytes[0]:=$a8;
              end;
            else
              InternalError(777005);
          end;
          objectdata.writebytes(bytes,1);
        end;
      8,9,10 :
        begin
          bytes[0]:=ord(codes^)+regval(oper[c-8].reg);
          inc(codes);
          objectdata.writebytes(bytes,1);
        end;
      15 :
        begin
          bytes[0]:=0;
          objectdata.writebytes(bytes,1);
        end;
      12,13,14 :
        begin
          getvalsym(c-12);
          if (currval<-128) or (currval>127) then
           Message2(asmw_e_value_exceeds_bounds,'signed byte',tostr(currval));
          if assigned(currsym) then
            objectdata.writereloc(currval,1,currsym,relative_false)
          else
            objectdata.writebytes(currval,1);
        end;
      16,17,18 :
        begin
          getvalsym(c-16);
          if (currval<-256) or (currval>255) then
           Message2(asmw_e_value_exceeds_bounds,'byte',tostr(currval));
          if assigned(currsym) then
           objectdata.writereloc(currval,1,currsym,relative_false)
          else
           objectdata.writebytes(currval,1);
        end;
      20,21,22 :
        begin
          getvalsym(c-20);
          if (currval<0) or (currval>255) then
           Message2(asmw_e_value_exceeds_bounds,'unsigned byte',tostr(currval));
          if assigned(currsym) then
           objectdata.writereloc(currval,1,currsym,relative_false)
          else
           objectdata.writebytes(currval,1);
        end;
      24,25,26 :
        begin
          getvalsym(c-24);
          if (currval<-65536) or (currval>65535) then
           Message2(asmw_e_value_exceeds_bounds,'word',tostr(currval));
          if assigned(currsym) then
           objectdata.writereloc(currval,2,currsym,relative_false)
          else
           objectdata.writebytes(currval,2);
        end;
      28,29,30 :
        begin
          getvalsym(c-28);
          if assigned(currsym) then
           objectdata.writereloc(currval,4,currsym,relative_false)
          else
           objectdata.writebytes(currval,4);
        end;
      32,33,34 :
        begin
          getvalsym(c-32);
          if assigned(currsym) then
           objectdata.writereloc(currval,4,currsym,relative_false)
          else
           objectdata.writebytes(currval,4);
        end;
      40,41,42 :
        begin
          getvalsym(c-40);
          data:=currval-insend;
          if assigned(currsym) then
           inc(data,currsym.address);
          if (data>127) or (data<-128) then
           Message1(asmw_e_short_jmp_out_of_range,tostr(data));
          objectdata.writebytes(data,1);
        end;
      52,53,54 :
        begin
          getvalsym(c-52);
          if assigned(currsym) then
           objectdata.writereloc(currval,4,currsym,relative_true)
          else
           objectdata.writereloc(currval-insend,4,nil,relative_false)
        end;
      56,57,58 :
        begin
          getvalsym(c-56);
          if assigned(currsym) then
           objectdata.writereloc(currval,4,currsym,relative_true)
          else
           objectdata.writereloc(currval-insend,4,nil,relative_false)
        end;
      192,193,194 :
        begin
          if NeedAddrPrefix(c-192) then
           begin
             bytes[0]:=$67;
             objectdata.writebytes(bytes,1);
           end;
        end;
      200 :
        begin
          bytes[0]:=$67;
          objectdata.writebytes(bytes,1);
        end;
      208 :
        begin
          bytes[0]:=$66;
          objectdata.writebytes(bytes,1);
        end;
      216 :
        begin
          bytes[0]:=ord(codes^)+condval[condition];
          inc(codes);
          objectdata.writebytes(bytes,1);
        end;
      201,
      202,
      209,
      210,
      217,218,219 :
        begin
          { these are dissambler hints or 32 bit prefixes which
            are not needed }
        end;
      31,
      48,49,50,
      224,225,226 :
        begin
          InternalError(777006);
        end
      else
        begin
          if (c>=64) and (c<=191) then
           begin
             if (c<127) then
              begin
                if (oper[c and 7].typ=top_reg) then
                  rfield:=regval(oper[c and 7].reg)
                else
                  rfield:=regval(oper[c and 7].ref^.base);
              end
             else
              rfield:=c and 7;
             opidx:=(c shr 3) and 7;
             if not process_ea(oper[opidx], ea_data, rfield) then
              Message(asmw_e_invalid_effective_address);

             pb:=@bytes;
             pb^:=chr(ea_data.modrm);
             inc(pb);
             if ea_data.sib_present then
              begin
                pb^:=chr(ea_data.sib);
                inc(pb);
              end;

             s:=pb-pchar(@bytes);
             objectdata.writebytes(bytes,s);

             case ea_data.bytes of
               0 : ;
               1 :
                 begin
                   if (oper[opidx].ot and OT_MEMORY)=OT_MEMORY then
                    objectdata.writereloc(oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup,1,oper[opidx].ref^.symbol,relative_false)
                   else
                    begin
                      bytes[0]:=oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup;
                      objectdata.writebytes(bytes,1);
                    end;
                   inc(s);
                 end;
               2,4 :
                 begin
                   objectdata.writereloc(oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup,ea_data.bytes,
                     oper[opidx].ref^.symbol,relative_false);
                   inc(s,ea_data.bytes);
                 end;
             end;
           end
          else
           InternalError(777007);
        end;
    end;
  until false;
end;
{$endif NOAG386BIN}

{*****************************************************************************
                              Instruction table
*****************************************************************************}

    procedure BuildInsTabCache;
{$ifndef NOAG386BIN}
      var
        i : longint;
{$endif}
      begin
{$ifndef NOAG386BIN}
        new(instabcache);
        FillChar(instabcache^,sizeof(tinstabcache),$ff);
        i:=0;
        while (i<InsTabEntries) do
         begin
           if InsTabCache^[InsTab[i].OPcode]=-1 then
            InsTabCache^[InsTab[i].OPcode]:=i;
           inc(i);
         end;
{$endif NOAG386BIN}
      end;


    procedure InitAsm;
      begin
{$ifndef NOAG386BIN}
        if not assigned(instabcache) then
          BuildInsTabCache;
{$endif NOAG386BIN}
      end;


    procedure DoneAsm;
      begin
{$ifndef NOAG386BIN}
        if assigned(instabcache) then
         dispose(instabcache);
{$endif NOAG386BIN}
      end;



end.
{
  $Log$
  Revision 1.24  2002-05-16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.21  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.20  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.19  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.18  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.17  2001/12/31 16:59:43  peter
    * protected/private symbols parsing fixed

  Revision 1.16  2001/12/29 15:29:59  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.15  2001/04/21 12:13:15  peter
    * restore correct pass2 handling bug 1425 (merged)

  Revision 1.14  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2001/04/05 21:33:45  peter
    * movd and opsize fix merged

  Revision 1.12  2001/03/25 12:29:45  peter
    * offset_fixup fixes (merged)

  Revision 1.11  2001/02/20 21:51:36  peter
    * fpu fixes (merged)

  Revision 1.10  2001/01/13 20:24:24  peter
    * fixed operand order that got mixed up for external writers after
      my previous assembler block valid instruction check

  Revision 1.9  2001/01/12 19:18:42  peter
    * check for valid asm instructions

  Revision 1.8  2001/01/07 15:48:56  jonas
    * references to symbols were only decreased in taicpu.done for jmps, fixed

  Revision 1.7  2000/12/26 15:56:17  peter
    * unrolled loops in taicpu.destroy

  Revision 1.6  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/12/23 19:59:35  peter
    * object to class for ow/og objects
    * split objectdata from objectoutput

  Revision 1.4  2000/12/07 17:19:45  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.3  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.2  2000/10/15 10:50:46  florian
   * fixed xmm register access

  Revision 1.1  2000/10/15 09:39:37  peter
    * moved cpu*.pas to i386/
    * renamed n386 to common cpunode

  Revision 1.5  2000/09/24 15:06:14  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/07/13 12:08:25  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:38  michael
  + removed logs

}
