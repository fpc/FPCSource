{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    Contains the abstract assembler implementation for the i386

    * Portions of this code was inspired by the NASM sources
      The Netwide Assembler is Copyright (c) 1996 Simon Tatham and
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
unit aasmcpu;

{$i fpcdefs.inc}

interface

    uses
      cclasses,globals,verbose,
      cpuinfo,cpubase,
      symppu,
      aasmbase,aasmtai;

    const
    { Operand types }
      OT_NONE      = $00000000;

      OT_BITS8     = $00000001;  { size, and other attributes, of the operand  }
      OT_BITS16    = $00000002;
      OT_BITS32    = $00000004;
      OT_BITS64    = $00000008;  { FPU only  }
      OT_BITS80    = $00000010;
      OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
      OT_NEAR      = $00000040;
      OT_SHORT     = $00000080;

      OT_SIZE_MASK = $000000FF;  { all the size attributes  }
      OT_NON_SIZE  = longint(not OT_SIZE_MASK);

      OT_SIGNED    = $00000100;  { the operand need to be signed -128-127 }

      OT_TO        = $00000200;  { operand is followed by a colon  }
                                 { reverse effect in FADD, FSUB &c  }
      OT_COLON     = $00000400;

      OT_REGISTER  = $00001000;
      OT_IMMEDIATE = $00002000;
      OT_IMM8      = $00002001;
      OT_IMM16     = $00002002;
      OT_IMM32     = $00002004;
      OT_IMM64     = $00002008;
      OT_IMM80     = $00002010;
      OT_REGMEM    = $00200000;  { for r/m, ie EA, operands  }
      OT_REGNORM   = $00201000;  { 'normal' reg, qualifies as EA  }
      OT_REG8      = $00201001;
      OT_REG16     = $00201002;
      OT_REG32     = $00201004;
      OT_MMXREG    = $00201008;  { MMX registers  }
      OT_XMMREG    = $00201010;  { Katmai registers  }
      OT_MEMORY    = $00204000;  { register number in 'basereg'  }
      OT_MEM8      = $00204001;
      OT_MEM16     = $00204002;
      OT_MEM32     = $00204004;
      OT_MEM64     = $00204008;
      OT_MEM80     = $00204010;
      OT_FPUREG    = $01000000;  { floating point stack registers  }
      OT_FPU0      = $01000800;  { FPU stack register zero  }
      OT_REG_SMASK = $00070000;  { special register operands: these may be treated differently  }
                                 { a mask for the following  }
      OT_REG_ACCUM = $00211000;  { accumulator: AL, AX or EAX  }
      OT_REG_AL    = $00211001;    { REG_ACCUM | BITSxx  }
      OT_REG_AX    = $00211002;    { ditto  }
      OT_REG_EAX   = $00211004;    { and again  }
      OT_REG_COUNT = $00221000;  { counter: CL, CX or ECX  }
      OT_REG_CL    = $00221001;    { REG_COUNT | BITSxx  }
      OT_REG_CX    = $00221002;    { ditto  }
      OT_REG_ECX   = $00221004;    { another one  }
      OT_REG_DX    = $00241002;

      OT_REG_SREG  = $00081002;  { any segment register  }
      OT_REG_CS    = $01081002;  { CS  }
      OT_REG_DESS  = $02081002;  { DS, ES, SS (non-CS 86 registers)  }
      OT_REG_FSGS  = $04081002;  { FS, GS (386 extended registers)  }

      OT_REG_CDT   = $00101004;  { CRn, DRn and TRn  }
      OT_REG_CREG  = $08101004;  { CRn  }
      OT_REG_CR4   = $08101404;  { CR4 (Pentium only)  }
      OT_REG_DREG  = $10101004;  { DRn  }
      OT_REG_TREG  = $20101004;  { TRn  }

      OT_MEM_OFFS  = $00604000;  { special type of EA  }
                                 { simple [address] offset  }
      OT_ONENESS   = $00800000;  { special type of immediate operand  }
                                 { so UNITY == IMMEDIATE | ONENESS  }
      OT_UNITY     = $00802000;  { for shift/rotate instructions  }

      { Size of the instruction table converted by nasmconv.pas }
      instabentries = {$i i386nop.inc}
      maxinfolen    = 8;

    type
      TOperandOrder = (op_intel,op_att);

      tinsentry=packed record
        opcode  : tasmop;
        ops     : byte;
        optypes : array[0..2] of longint;
        code    : array[0..maxinfolen] of char;
        flags   : longint;
      end;
      pinsentry=^tinsentry;

      { alignment for operator }
      tai_align = class(tai_align_abstract)
         reg       : tregister;
         constructor create(b:byte);
         constructor create_op(b: byte; _op: byte);
         function getfillbuf:pchar;override;
      end;

      taicpu = class(taicpu_abstract)
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
         procedure Pass2(sec:TAsmObjectdata);virtual;
         procedure SetOperandOrder(order:TOperandOrder);
      protected
         procedure ppuloadoper(ppufile:tcompilerppufile;var o:toper);override;
         procedure ppuwriteoper(ppufile:tcompilerppufile;const o:toper);override;
         procedure ppuderefoper(var o:toper);override;
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
         procedure gencode(sec:TAsmObjectData);
         function  NeedAddrPrefix(opidx:byte):boolean;
         procedure Swapoperands;
    {$endif NOAG386BIN}
      end;


    procedure InitAsm;
    procedure DoneAsm;


implementation

     uses
       cutils,
       ag386att;

{*****************************************************************************
                              Instruction table
*****************************************************************************}

    const
     {Instruction flags }
       IF_NONE   = $00000000;
       IF_SM     = $00000001;        { size match first two operands  }
       IF_SM2    = $00000002;
       IF_SB     = $00000004;  { unsized operands can't be non-byte  }
       IF_SW     = $00000008;  { unsized operands can't be non-word  }
       IF_SD     = $00000010;  { unsized operands can't be nondword  }
       IF_AR0    = $00000020;  { SB, SW, SD applies to argument 0  }
       IF_AR1    = $00000040;  { SB, SW, SD applies to argument 1  }
       IF_AR2    = $00000060;  { SB, SW, SD applies to argument 2  }
       IF_ARMASK = $00000060;  { mask for unsized argument spec  }
       IF_PRIV   = $00000100;  { it's a privileged instruction  }
       IF_SMM    = $00000200;  { it's only valid in SMM  }
       IF_PROT   = $00000400;  { it's protected mode only  }
       IF_UNDOC  = $00001000;  { it's an undocumented instruction  }
       IF_FPU    = $00002000;  { it's an FPU instruction  }
       IF_MMX    = $00004000;  { it's an MMX instruction  }
       { it's a 3DNow! instruction  }
       IF_3DNOW  = $00008000;
       { it's a SSE (KNI, MMX2) instruction  }
       IF_SSE    = $00010000;
       { SSE2 instructions  }
       IF_SSE2   = $00020000;
       { the mask for processor types  }
       IF_PMASK  = longint($FF000000);
       { the mask for disassembly "prefer"  }
       IF_PFMASK = longint($F001FF00);
       IF_8086   = $00000000;  { 8086 instruction  }
       IF_186    = $01000000;  { 186+ instruction  }
       IF_286    = $02000000;  { 286+ instruction  }
       IF_386    = $03000000;  { 386+ instruction  }
       IF_486    = $04000000;  { 486+ instruction  }
       IF_PENT   = $05000000;  { Pentium instruction  }
       IF_P6     = $06000000;  { P6 instruction  }
       IF_KATMAI = $07000000;  { Katmai instructions  }
       { Willamette instructions }
       IF_WILLAMETTE = $08000000;
       IF_CYRIX  = $10000000;  { Cyrix-specific instruction  }
       IF_AMD    = $20000000;  { AMD-specific instruction  }
       { added flags }
       IF_PRE    = $40000000;  { it's a prefix instruction }
       IF_PASS2  = longint($80000000);  { if the instruction can change in a second pass }

     type
       TInsTabCache=array[TasmOp] of longint;
       PInsTabCache=^TInsTabCache;

     const
       InsTab:array[0..instabentries-1] of TInsEntry={$i i386tab.inc}

     var
       InsTabCache : PInsTabCache;

     const
       { Intel style operands ! }
       opsize_2_type:array[0..2,topsize] of longint=(
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS16,OT_BITS32,OT_BITS32,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT
         ),
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS8,OT_BITS8,OT_BITS16,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT
         ),
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_NONE,OT_NONE,OT_NONE,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_BITS64,OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT
         )
       );

       { Convert reg to operand type }
       reg2type : array[firstreg..lastreg] of longint = (OT_NONE,
         OT_REG_EAX,OT_REG_ECX,OT_REG32,OT_REG32,OT_REG32,OT_REG32,OT_REG32,OT_REG32,
         OT_REG_AX,OT_REG_CX,OT_REG_DX,OT_REG16,OT_REG16,OT_REG16,OT_REG16,OT_REG16,
         OT_REG_AL,OT_REG_CL,OT_REG8,OT_REG8,OT_REG8,OT_REG8,OT_REG8,OT_REG8,
         OT_REG_CS,OT_REG_DESS,OT_REG_DESS,OT_REG_DESS,OT_REG_FSGS,OT_REG_FSGS,
         OT_FPU0,OT_FPU0,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,OT_FPUREG,
         OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,OT_REG_DREG,
         OT_REG_CREG,OT_REG_CREG,OT_REG_CREG,OT_REG_CR4,
         OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,OT_REG_TREG,
         OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,OT_MMXREG,
         OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG,OT_XMMREG
       );


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


    procedure taicpu.Swapoperands;
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
           Swapoperands;
           FOperandOrder:=order;
         end;
      end;


    procedure taicpu.ppuloadoper(ppufile:tcompilerppufile;var o:toper);
      begin
        o.typ:=toptype(ppufile.getbyte);
        o.ot:=ppufile.getlongint;
        case o.typ of
          top_reg :
            o.reg:=tregister(ppufile.getbyte);
          top_ref :
            begin
              new(o.ref);
              o.ref^.segment:=tregister(ppufile.getbyte);
              o.ref^.base:=tregister(ppufile.getbyte);
              o.ref^.index:=tregister(ppufile.getbyte);
              o.ref^.scalefactor:=ppufile.getbyte;
              o.ref^.offset:=ppufile.getlongint;
              o.ref^.symbol:=ppufile.getasmsymbol;
              o.ref^.offsetfixup:=ppufile.getlongint;
              o.ref^.options:=trefoptions(ppufile.getbyte);
            end;
          top_const :
            o.val:=aword(ppufile.getlongint);
          top_symbol :
            begin
              o.sym:=ppufile.getasmsymbol;
              o.symofs:=ppufile.getlongint;
            end;
        end;
      end;


    procedure taicpu.ppuwriteoper(ppufile:tcompilerppufile;const o:toper);
      begin
        ppufile.putbyte(byte(o.typ));
        ppufile.putlongint(o.ot);
        case o.typ of
          top_reg :
            ppufile.putbyte(byte(o.reg));
          top_ref :
            begin
              ppufile.putbyte(byte(o.ref^.segment));
              ppufile.putbyte(byte(o.ref^.base));
              ppufile.putbyte(byte(o.ref^.index));
              ppufile.putbyte(o.ref^.scalefactor);
              ppufile.putlongint(o.ref^.offset);
              ppufile.putasmsymbol(o.ref^.symbol);
              ppufile.putlongint(o.ref^.offsetfixup);
              ppufile.putbyte(byte(o.ref^.options));
            end;
          top_const :
            ppufile.putlongint(longint(o.val));
          top_symbol :
            begin
              ppufile.putasmsymbol(o.sym);
              ppufile.putlongint(longint(o.symofs));
            end;
        end;
      end;


    procedure taicpu.ppuderefoper(var o:toper);
      begin
        case o.typ of
          top_ref :
            begin
              if assigned(o.ref^.symbol) then
               objectlibrary.derefasmsymbol(o.ref^.symbol);
            end;
          top_symbol :
            objectlibrary.derefasmsymbol(o.sym);
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
           (oper[0].reg in [R_ST..R_ST0])) or
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
           Message1(asmw_e_opcode_not_in_table,gas_op2str[opcode]);
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
{$endif PASS2FLAG}
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


    procedure taicpu.Pass2(sec:TAsmObjectData);
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
           sec.writebytes(c,1);
           { fix the offset for GenNode }
           inc(InsOffset);
         end;
        { Generate the instruction }
        GenCode(sec);
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
{$endif OPTEA}
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


    procedure taicpu.GenCode(sec:TAsmObjectData);
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
        if sec.sects[sec.currsec].datasize<>insoffset then
         internalerror(200130121);
{$endif EXTDEBUG}
        { load data to write }
        codes:=insentry^.code;
        { Force word push/pop for registers }
        if (opsize=S_W) and ((codes[0]=#4) or (codes[0]=#6) or
            ((codes[0]=#1) and ((codes[2]=#5) or (codes[2]=#7)))) then
          begin
            bytes[0]:=$66;
            sec.writebytes(bytes,1);
          end;
        repeat
          c:=ord(codes^);
          inc(codes);
          case c of
            0 :
              break;
            1,2,3 :
              begin
                sec.writebytes(codes^,c);
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
                sec.writebytes(bytes,1);
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
                sec.writebytes(bytes,1);
              end;
            8,9,10 :
              begin
                bytes[0]:=ord(codes^)+regval(oper[c-8].reg);
                inc(codes);
                sec.writebytes(bytes,1);
              end;
            15 :
              begin
                bytes[0]:=0;
                sec.writebytes(bytes,1);
              end;
            12,13,14 :
              begin
                getvalsym(c-12);
                if (currval<-128) or (currval>127) then
                 Message2(asmw_e_value_exceeds_bounds,'signed byte',tostr(currval));
                if assigned(currsym) then
                  sec.writereloc(currval,1,currsym,RELOC_ABSOLUTE)
                else
                  sec.writebytes(currval,1);
              end;
            16,17,18 :
              begin
                getvalsym(c-16);
                if (currval<-256) or (currval>255) then
                 Message2(asmw_e_value_exceeds_bounds,'byte',tostr(currval));
                if assigned(currsym) then
                 sec.writereloc(currval,1,currsym,RELOC_ABSOLUTE)
                else
                 sec.writebytes(currval,1);
              end;
            20,21,22 :
              begin
                getvalsym(c-20);
                if (currval<0) or (currval>255) then
                 Message2(asmw_e_value_exceeds_bounds,'unsigned byte',tostr(currval));
                if assigned(currsym) then
                 sec.writereloc(currval,1,currsym,RELOC_ABSOLUTE)
                else
                 sec.writebytes(currval,1);
              end;
            24,25,26 :
              begin
                getvalsym(c-24);
                if (currval<-65536) or (currval>65535) then
                 Message2(asmw_e_value_exceeds_bounds,'word',tostr(currval));
                if assigned(currsym) then
                 sec.writereloc(currval,2,currsym,RELOC_ABSOLUTE)
                else
                 sec.writebytes(currval,2);
              end;
            28,29,30 :
              begin
                getvalsym(c-28);
                if assigned(currsym) then
                 sec.writereloc(currval,4,currsym,RELOC_ABSOLUTE)
                else
                 sec.writebytes(currval,4);
              end;
            32,33,34 :
              begin
                getvalsym(c-32);
                if assigned(currsym) then
                 sec.writereloc(currval,4,currsym,RELOC_ABSOLUTE)
                else
                 sec.writebytes(currval,4);
              end;
            40,41,42 :
              begin
                getvalsym(c-40);
                data:=currval-insend;
                if assigned(currsym) then
                 inc(data,currsym.address);
                if (data>127) or (data<-128) then
                 Message1(asmw_e_short_jmp_out_of_range,tostr(data));
                sec.writebytes(data,1);
              end;
            52,53,54 :
              begin
                getvalsym(c-52);
                if assigned(currsym) then
                 sec.writereloc(currval,4,currsym,RELOC_RELATIVE)
                else
                 sec.writereloc(currval-insend,4,nil,RELOC_ABSOLUTE)
              end;
            56,57,58 :
              begin
                getvalsym(c-56);
                if assigned(currsym) then
                 sec.writereloc(currval,4,currsym,RELOC_RELATIVE)
                else
                 sec.writereloc(currval-insend,4,nil,RELOC_ABSOLUTE)
              end;
            192,193,194 :
              begin
                if NeedAddrPrefix(c-192) then
                 begin
                   bytes[0]:=$67;
                   sec.writebytes(bytes,1);
                 end;
              end;
            200 :
              begin
                bytes[0]:=$67;
                sec.writebytes(bytes,1);
              end;
            208 :
              begin
                bytes[0]:=$66;
                sec.writebytes(bytes,1);
              end;
            216 :
              begin
                bytes[0]:=ord(codes^)+condval[condition];
                inc(codes);
                sec.writebytes(bytes,1);
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
                   sec.writebytes(bytes,s);

                   case ea_data.bytes of
                     0 : ;
                     1 :
                       begin
                         if (oper[opidx].ot and OT_MEMORY)=OT_MEMORY then
                          sec.writereloc(oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup,1,oper[opidx].ref^.symbol,RELOC_ABSOLUTE)
                         else
                          begin
                            bytes[0]:=oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup;
                            sec.writebytes(bytes,1);
                          end;
                         inc(s);
                       end;
                     2,4 :
                       begin
                         sec.writereloc(oper[opidx].ref^.offset+oper[opidx].ref^.offsetfixup,ea_data.bytes,
                           oper[opidx].ref^.symbol,RELOC_ABSOLUTE);
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
  Revision 1.6  2002-10-31 13:28:32  pierre
   * correct last wrong fix for tw2158

  Revision 1.5  2002/10/30 17:10:00  pierre
   * merge of fix for tw2158 bug

  Revision 1.4  2002/08/15 19:10:36  peter
    * first things tai,tnode storing in ppu

  Revision 1.3  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.2  2002/07/20 11:57:59  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.1  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

}
