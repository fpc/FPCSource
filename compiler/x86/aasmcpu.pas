{
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
      globtype,verbose,
      cpubase,
      cgbase,cgutils,
      symtype,
      aasmbase,aasmtai,aasmdata,aasmsym,
      ogbase;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" destination operand number }
      O_MOV_DEST = 1;

    { Operand types }
      OT_NONE      = $00000000;

      OT_BITS8     = $00000001;  { size, and other attributes, of the operand  }
      OT_BITS16    = $00000002;
      OT_BITS32    = $00000004;
      OT_BITS64    = $00000008;  { FPU only  }
      OT_BITS80    = $00000010;

      OT_SIZE_MASK = $0000001F;  { all the size attributes  }
      OT_NON_SIZE  = longint(not OT_SIZE_MASK);

      OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
      OT_NEAR      = $00000040;
      OT_SHORT     = $00000080;
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
      OT_REG64     = $00201008;
      OT_XMMREG    = $00201010;  { Katmai registers  }
      OT_MMXREG    = $00201020;  { MMX registers  }
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
      OT_REG_ACCUM = $00211000;  { FUNCTION_RETURN_REG: AL, AX or EAX  }
      OT_REG_AL    = $00211001;    { REG_ACCUM | BITSxx  }
      OT_REG_AX    = $00211002;    { ditto  }
      OT_REG_EAX   = $00211004;    { and again  }
{$ifdef x86_64}
      OT_REG_RAX   = $00211008;
{$endif x86_64}
      OT_REG_COUNT = $00221000;  { counter: CL, CX or ECX  }
      OT_REG_CL    = $00221001;    { REG_COUNT | BITSxx  }
      OT_REG_CX    = $00221002;    { ditto  }
      OT_REG_ECX   = $00221004;    { another one  }
{$ifdef x86_64}
      OT_REG_RCX   = $00221008;
{$endif x86_64}
      OT_REG_DX    = $00241002;
      OT_REG_EDX   = $00241004;

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
{$ifdef x86_64}
      instabentries = {$i x8664nop.inc}
{$else x86_64}
      instabentries = {$i i386nop.inc}
{$endif x86_64}
      maxinfolen    = 11;
      MaxInsChanges = 3; { Max things a instruction can change }

    type
      { What an instruction can change. Needed for optimizer and spilling code.

        Note: The order of this enumeration is should not be changed! }
      TInsChange = (Ch_None,
        {Read from a register}
        Ch_REAX, Ch_RECX, Ch_REDX, Ch_REBX, Ch_RESP, Ch_REBP, Ch_RESI, Ch_REDI,
        {write from a register}
        Ch_WEAX, Ch_WECX, Ch_WEDX, Ch_WEBX, Ch_WESP, Ch_WEBP, Ch_WESI, Ch_WEDI,
        {read and write from/to a register}
        Ch_RWEAX, Ch_RWECX, Ch_RWEDX, Ch_RWEBX, Ch_RWESP, Ch_RWEBP, Ch_RWESI, Ch_RWEDI,
        {modify the contents of a register with the purpose of using
         this changed content afterwards (add/sub/..., but e.g. not rep
         or movsd)}
        Ch_MEAX, Ch_MECX, Ch_MEDX, Ch_MEBX, Ch_MESP, Ch_MEBP, Ch_MESI, Ch_MEDI,
        Ch_CDirFlag {clear direction flag}, Ch_SDirFlag {set dir flag},
        Ch_RFlags, Ch_WFlags, Ch_RWFlags, Ch_FPU,
        Ch_Rop1, Ch_Wop1, Ch_RWop1,Ch_Mop1,
        Ch_Rop2, Ch_Wop2, Ch_RWop2,Ch_Mop2,
        Ch_Rop3, Ch_WOp3, Ch_RWOp3,Ch_Mop3,
        Ch_WMemEDI,
        Ch_All,
        { x86_64 registers }
        Ch_RRAX, Ch_RRCX, Ch_RRDX, Ch_RRBX, Ch_RRSP, Ch_RRBP, Ch_RRSI, Ch_RRDI,
        Ch_WRAX, Ch_WRCX, Ch_WRDX, Ch_WRBX, Ch_WRSP, Ch_WRBP, Ch_WRSI, Ch_WRDI,
        Ch_RWRAX, Ch_RWRCX, Ch_RWRDX, Ch_RWRBX, Ch_RWRSP, Ch_RWRBP, Ch_RWRSI, Ch_RWRDI,
        Ch_MRAX, Ch_MRCX, Ch_MRDX, Ch_MRBX, Ch_MRSP, Ch_MRBP, Ch_MRSI, Ch_MRDI
      );

      TInsProp = packed record
        Ch : Array[1..MaxInsChanges] of TInsChange;
      end;

    const
      InsProp : array[tasmop] of TInsProp =
{$ifdef x86_64}
        {$i x8664pro.inc}
{$else x86_64}
        {$i i386prop.inc}
{$endif x86_64}

    type
      TOperandOrder = (op_intel,op_att);

      tinsentry=packed record
        opcode  : tasmop;
        ops     : byte;
        optypes : array[0..2] of longint;
        code    : array[0..maxinfolen] of char;
        flags   : cardinal;
      end;
      pinsentry=^tinsentry;

      { alignment for operator }
      tai_align = class(tai_align_abstract)
         reg       : tregister;
         constructor create(b:byte);override;
         constructor create_op(b: byte; _op: byte);override;
         function calculatefillbuf(var buf : tfillbuffer):pchar;override;
      end;

      taicpu = class(tai_cpu_abstract_sym)
         opsize    : topsize;
         constructor op_none(op : tasmop);
         constructor op_none(op : tasmop;_size : topsize);

         constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
         constructor op_const(op : tasmop;_size : topsize;_op1 : aint);
         constructor op_ref(op : tasmop;_size : topsize;const _op1 : treference);

         constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: aint);

         constructor op_const_reg(op : tasmop;_size : topsize;_op1 : aint;_op2 : tregister);
         constructor op_const_const(op : tasmop;_size : topsize;_op1,_op2 : aint);
         constructor op_const_ref(op : tasmop;_size : topsize;_op1 : aint;const _op2 : treference);

         constructor op_ref_reg(op : tasmop;_size : topsize;const _op1 : treference;_op2 : tregister);

         constructor op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
         constructor op_const_reg_reg(op : tasmop;_size : topsize;_op1 : aint;_op2 : tregister;_op3 : tregister);
         constructor op_const_ref_reg(op : tasmop;_size : topsize;_op1 : aint;const _op2 : treference;_op3 : tregister);
         constructor op_reg_reg_ref(op : tasmop;_size : topsize;_op1,_op2 : tregister; const _op3 : treference);
         constructor op_const_reg_ref(op : tasmop;_size : topsize;_op1 : aint;_op2 : tregister;const _op3 : treference);

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
      public
         { the next will reset all instructions that can change in pass 2 }
         procedure ResetPass1;override;
         procedure ResetPass2;override;
         function  CheckIfValid:boolean;
         function  Pass1(objdata:TObjData):longint;override;
         procedure Pass2(objdata:TObjData);override;
         procedure SetOperandOrder(order:TOperandOrder);
         function is_same_reg_move(regtype: Tregistertype):boolean;override;
         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;
      private
         { next fields are filled in pass1, so pass2 is faster }
         insentry  : PInsEntry;
         insoffset : longint;
         LastInsOffset : longint; { need to be public to be reset }
         inssize   : shortint;
{$ifdef x86_64}
         rex       : byte;
{$endif x86_64}
         function  InsEnd:longint;
         procedure create_ot(objdata:TObjData);
         function  Matches(p:PInsEntry):boolean;
         function  calcsize(p:PInsEntry):shortint;
         procedure gencode(objdata:TObjData);
         function  NeedAddrPrefix(opidx:byte):boolean;
         procedure Swapoperands;
         function  FindInsentry(objdata:TObjData):boolean;
      end;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    procedure InitAsm;
    procedure DoneAsm;


implementation

     uses
       cutils,
       globals,
       itcpugas,
       symsym;

{*****************************************************************************
                              Instruction table
*****************************************************************************}

    const
     {Instruction flags }
       IF_NONE   = $00000000;
       IF_SM     = $00000001;  { size match first two operands  }
       IF_SM2    = $00000002;
       IF_SB     = $00000004;  { unsized operands can't be non-byte  }
       IF_SW     = $00000008;  { unsized operands can't be non-word  }
       IF_SD     = $00000010;  { unsized operands can't be nondword  }
       IF_SMASK  = $0000001f;
       IF_AR0    = $00000020;  { SB, SW, SD applies to argument 0  }
       IF_AR1    = $00000040;  { SB, SW, SD applies to argument 1  }
       IF_AR2    = $00000060;  { SB, SW, SD applies to argument 2  }
       IF_ARMASK = $00000060;  { mask for unsized argument spec  }
       IF_PRIV   = $00000100;  { it's a privileged instruction  }
       IF_SMM    = $00000200;  { it's only valid in SMM  }
       IF_PROT   = $00000400;  { it's protected mode only  }
       IF_NOX86_64 = $00000800;  { removed instruction in x86_64  }
       IF_UNDOC  = $00001000;  { it's an undocumented instruction  }
       IF_FPU    = $00002000;  { it's an FPU instruction  }
       IF_MMX    = $00004000;  { it's an MMX instruction  }
       { it's a 3DNow! instruction  }
       IF_3DNOW  = $00008000;
       { it's a SSE (KNI, MMX2) instruction  }
       IF_SSE    = $00010000;
       { SSE2 instructions  }
       IF_SSE2   = $00020000;
       { SSE3 instructions  }
       IF_SSE3   = $00040000;
       { SSE64 instructions  }
       IF_SSE64  = $00080000;
       { the mask for processor types  }
       {IF_PMASK  = longint($FF000000);}
       { the mask for disassembly "prefer"  }
       {IF_PFMASK = longint($F001FF00);}
       { SVM instructions  }
       IF_SVM    = $00100000;
       { SSE4 instructions  }
       IF_SSE4   = $00200000;

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
       { Prescott instructions }
       IF_PRESCOTT = $09000000;
       IF_X86_64 = $0a000000;
       IF_CYRIX  = $0b000000;  { Cyrix-specific instruction  }
       IF_AMD    = $0c000000;  { AMD-specific instruction  }
       IF_CENTAUR = $0d000000;  { centaur-specific instruction  }
       { added flags }
       IF_PRE    = $40000000;  { it's a prefix instruction }
       IF_PASS2  = $80000000;  { if the instruction can change in a second pass }

     type
       TInsTabCache=array[TasmOp] of longint;
       PInsTabCache=^TInsTabCache;

     const
{$ifdef x86_64}
       InsTab:array[0..instabentries-1] of TInsEntry={$i x8664tab.inc}
{$else x86_64}
       InsTab:array[0..instabentries-1] of TInsEntry={$i i386tab.inc}
{$endif x86_64}
     var
       InsTabCache : PInsTabCache;

     const
{$ifdef x86_64}
       { Intel style operands ! }
       opsize_2_type:array[0..2,topsize] of longint=(
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS64,OT_BITS16,OT_BITS32,OT_BITS32,OT_BITS64,OT_BITS64,OT_BITS64,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_NONE,
          OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT,
          OT_NONE,
          OT_NONE
         ),
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS64,OT_BITS8,OT_BITS8,OT_BITS16,OT_BITS8,OT_BITS16,OT_BITS32,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_NONE,
          OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT,
          OT_NONE,
          OT_NONE
         ),
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS64,OT_NONE,OT_NONE,OT_NONE,OT_NONE,OT_NONE,OT_NONE,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_NONE,
          OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT,
          OT_NONE,
          OT_NONE
         )
       );

      reg_ot_table : array[tregisterindex] of longint = (
        {$i r8664ot.inc}
      );
{$else x86_64}
       { Intel style operands ! }
       opsize_2_type:array[0..2,topsize] of longint=(
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS64,OT_BITS16,OT_BITS32,OT_BITS32,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_NONE,
          OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT,
          OT_NONE,
          OT_NONE
         ),
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS64,OT_BITS8,OT_BITS8,OT_BITS16,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_NONE,
          OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT,
          OT_NONE,
          OT_NONE
         ),
         (OT_NONE,
          OT_BITS8,OT_BITS16,OT_BITS32,OT_BITS64,OT_NONE,OT_NONE,OT_NONE,
          OT_BITS16,OT_BITS32,OT_BITS64,
          OT_BITS32,OT_BITS64,OT_BITS80,OT_BITS64,OT_NONE,
          OT_BITS64,
          OT_NEAR,OT_FAR,OT_SHORT,
          OT_NONE,
          OT_NONE
         )
      );

      reg_ot_table : array[tregisterindex] of longint = (
        {$i r386ot.inc}
      );
{$endif x86_64}

    { Operation type for spilling code }
    type
      toperation_type_table=array[tasmop,0..Max_Operands] of topertype;
    var
      operation_type_table : ^toperation_type_table;


{****************************************************************************
                              TAI_ALIGN
 ****************************************************************************}

    constructor tai_align.create(b: byte);
      begin
        inherited create(b);
        reg:=NR_ECX;
      end;


    constructor tai_align.create_op(b: byte; _op: byte);
      begin
        inherited create_op(b,_op);
        reg:=NR_NO;
      end;


    function tai_align.calculatefillbuf(var buf : tfillbuffer):pchar;
      const
{$ifdef x86_64}
        alignarray:array[0..3] of string[4]=(
          #$66#$66#$66#$90,
          #$66#$66#$90,
          #$66#$90,
          #$90
        );
{$else x86_64}
        alignarray:array[0..5] of string[8]=(
          #$8D#$B4#$26#$00#$00#$00#$00,
          #$8D#$B6#$00#$00#$00#$00,
          #$8D#$74#$26#$00,
          #$8D#$76#$00,
          #$89#$F6,
          #$90);
{$endif x86_64}
      var
        bufptr : pchar;
        j : longint;
        localsize: byte;
      begin
        inherited calculatefillbuf(buf);
        if not use_op then
         begin
           bufptr:=pchar(@buf);
           { fillsize may still be used afterwards, so don't modify }
           { e.g. writebytes(hp.calculatefillbuf(buf)^,hp.fillsize) }
           localsize:=fillsize;
           while (localsize>0) do
            begin
              for j:=low(alignarray) to high(alignarray) do
               if (localsize>=length(alignarray[j])) then
                break;
              move(alignarray[j][1],bufptr^,length(alignarray[j]));
              inc(bufptr,length(alignarray[j]));
              dec(localsize,length(alignarray[j]));
            end;
         end;
        calculatefillbuf:=pchar(@buf);
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
         segprefix:=NR_NO;
         opsize:=_size;
         insentry:=nil;
         LastInsOffset:=-1;
         InsOffset:=0;
         InsSize:=0;
      end;


    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
         init(S_NO);
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


    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : aint);
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


    constructor taicpu.op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: aint);
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


    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : aint;_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : aint);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,_op1);
         loadconst(1,_op2);
      end;


    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : aint;const _op2 : treference);
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


    constructor taicpu.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : aint;_op2 : tregister;_op3 : tregister);
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


    constructor taicpu.op_const_ref_reg(op : tasmop;_size : topsize;_op1 : aint;const _op2 : treference;_op3 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadconst(0,_op1);
         loadref(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_const_reg_ref(op : tasmop;_size : topsize;_op1 : aint;_op2 : tregister;const _op3 : treference);
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
        for i:=0 to ops-1 do
         begin
           with oper[i]^ do
             begin
               if i=0 then
                s:=s+' '
               else
                s:=s+',';
               { type }
               addsize:=false;
               if (ot and OT_XMMREG)=OT_XMMREG then
                s:=s+'xmmreg'
               else
                 if (ot and OT_MMXREG)=OT_MMXREG then
                  s:=s+'mmxreg'
               else
                 if (ot and OT_FPUREG)=OT_FPUREG then
                  s:=s+'fpureg'
               else
                if (ot and OT_REGISTER)=OT_REGISTER then
                 begin
                   s:=s+'reg';
                   addsize:=true;
                 end
               else
                if (ot and OT_IMMEDIATE)=OT_IMMEDIATE then
                 begin
                   s:=s+'imm';
                   addsize:=true;
                 end
               else
                if (ot and OT_MEMORY)=OT_MEMORY then
                 begin
                   s:=s+'mem';
                   addsize:=true;
                 end
               else
                 s:=s+'???';
               { size }
               if addsize then
                begin
                  if (ot and OT_BITS8)<>0 then
                    s:=s+'8'
                  else
                   if (ot and OT_BITS16)<>0 then
                    s:=s+'16'
                  else
                   if (ot and OT_BITS32)<>0 then
                    s:=s+'32'
                  else
                   if (ot and OT_BITS64)<>0 then
                    s:=s+'64'
                  else
                    s:=s+'??';
                  { signed }
                  if (ot and OT_SIGNED)<>0 then
                   s:=s+'s';
                end;
             end;
         end;
        GetString:=s+']';
      end;


    procedure taicpu.Swapoperands;
      var
        p : POper;
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


    procedure taicpu.CheckNonCommutativeOpcodes;
      begin
        { we need ATT order }
        SetOperandOrder(op_att);

        if (
            (ops=2) and
            (oper[0]^.typ=top_reg) and
            (oper[1]^.typ=top_reg) and
           { if the first is ST and the second is also a register
             it is necessarily ST1 .. ST7 }
            ((oper[0]^.reg=NR_ST) or
             (oper[0]^.reg=NR_ST0))
           ) or
           { ((ops=1) and
            (oper[0]^.typ=top_reg) and
            (oper[0]^.reg in [R_ST1..R_ST7]))  or}
           (ops=0) then
          begin
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
          end;
        if (
            (ops=1) and
            (oper[0]^.typ=top_reg) and
            (getregtype(oper[0]^.reg)=R_FPUREGISTER) and
            (oper[0]^.reg<>NR_ST)
           ) then
         begin
           if opcode=A_FSUBRP then
             opcode:=A_FSUBP
           else if opcode=A_FSUBP then
             opcode:=A_FSUBRP
           else if opcode=A_FDIVRP then
             opcode:=A_FDIVP
           else if opcode=A_FDIVP then
             opcode:=A_FDIVRP;
         end;
      end;


{*****************************************************************************
                                Assembler
*****************************************************************************}

    type
      ea = packed record
        sib_present : boolean;
        bytes : byte;
        size  : byte;
        modrm : byte;
        sib   : byte;
{$ifdef x86_64}
        rex_present : boolean;
        rex   : byte;
{$endif x86_64}
      end;

    procedure taicpu.create_ot(objdata:TObjData);
      {
        this function will also fix some other fields which only needs to be once
      }
      var
        i,l,relsize : longint;
        currsym : TObjSymbol;
      begin
        if ops=0 then
         exit;
        { update oper[].ot field }
        for i:=0 to ops-1 do
         with oper[i]^ do
          begin
            case typ of
              top_reg :
                begin
                  ot:=reg_ot_table[findreg_by_number(reg)];
                end;
              top_ref :
                begin
                  if (ref^.refaddr=addr_no)
{$ifdef x86_64}
                     or (
                         (ref^.refaddr=addr_pic) and
                         (ref^.base<>NR_NO)
                        )
{$endif x86_64}
                     then
                    begin
                      { create ot field }
                      if (ot and OT_SIZE_MASK)=0 then
                        ot:=OT_MEMORY or opsize_2_type[i,opsize]
                      else
                        ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
                      if (ref^.base=NR_NO) and (ref^.index=NR_NO) then
                        ot:=ot or OT_MEM_OFFS;
                      { fix scalefactor }
                      if (ref^.index=NR_NO) then
                       ref^.scalefactor:=0
                      else
                       if (ref^.scalefactor=0) then
                        ref^.scalefactor:=1;
                    end
                  else
                    begin
                      { Jumps use a relative offset which can be 8bit,
                        for other opcodes we always need to generate the full
                        32bit address }
                      if assigned(objdata) and
                         is_jmp then
                        begin
                          currsym:=objdata.symbolref(ref^.symbol);
                          l:=ref^.offset;
                          if assigned(currsym) then
                            inc(l,currsym.address);
                          { when it is a forward jump we need to compensate the
                            offset of the instruction since the previous time,
                            because the symbol address is then still using the
                            'old-style' addressing.
                            For backwards jumps this is not required because the
                            address of the symbol is already adjusted to the
                            new offset }
                          if (l>InsOffset) and (LastInsOffset<>-1) then
                            inc(l,InsOffset-LastInsOffset);
                          { instruction size will then always become 2 (PFV) }
                          relsize:=(InsOffset+2)-l;
                          if (relsize>=-128) and (relsize<=127) and
                             (
                              not assigned(currsym) or
                              (currsym.objsection=objdata.currobjsec)
                             ) then
                            ot:=OT_IMM8 or OT_SHORT
                          else
                            ot:=OT_IMM32 or OT_NEAR;
                        end
                      else
                        ot:=OT_IMM32 or OT_NEAR;
                    end;
                end;
              top_local :
                begin
                  if (ot and OT_SIZE_MASK)=0 then
                    ot:=OT_MEMORY or opsize_2_type[i,opsize]
                  else
                    ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
                end;
              top_const :
                begin
                  { allow 2nd or 3rd operand being a constant and expect no size for shuf* etc. }
                  if (opsize=S_NO) and not(i in [1,2]) then
                    message(asmr_e_invalid_opcode_and_operand);
                  if (opsize<>S_W) and (aint(val)>=-128) and (val<=127) then
                    ot:=OT_IMM8 or OT_SIGNED
                  else
                    ot:=OT_IMMEDIATE or opsize_2_type[i,opsize];
                end;
              top_none :
                begin
                  { generated when there was an error in the
                    assembler reader. It never happends when generating
                    assembler }
                end;
              else
                internalerror(200402261);
            end;
          end;
      end;


    function taicpu.InsEnd:longint;
      begin
        InsEnd:=InsOffset+InsSize;
      end;


      function taicpu.Matches(p:PInsEntry):boolean;
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
        insot,
        currot,
        i,j,asize,oprs : longint;
        insflags:cardinal;
        siz : array[0..2] of longint;
      begin
        result:=false;

        { Check the opcode and operands }
        if (p^.opcode<>opcode) or (p^.ops<>ops) then
          exit;

        for i:=0 to p^.ops-1 do
         begin
           insot:=p^.optypes[i];
           currot:=oper[i]^.ot;
           { Check the operand flags }
           if (insot and (not currot) and OT_NON_SIZE)<>0 then
             exit;
           { Check if the passed operand size matches with one of
             the supported operand sizes }
           if ((insot and OT_SIZE_MASK)<>0) and
              ((insot and currot and OT_SIZE_MASK)<>(currot and OT_SIZE_MASK)) then
             exit;
         end;

        { Check operand sizes }
        insflags:=p^.flags;
        if insflags and IF_SMASK<>0 then
          begin
            { as default an untyped size can get all the sizes, this is different
              from nasm, but else we need to do a lot checking which opcodes want
              size or not with the automatic size generation }
            asize:=-1;
            if (insflags and IF_SB)<>0 then
              asize:=OT_BITS8
            else if (insflags and IF_SW)<>0 then
              asize:=OT_BITS16
            else if (insflags and IF_SD)<>0 then
              asize:=OT_BITS32;
            if (insflags and IF_ARMASK)<>0 then
             begin
               siz[0]:=0;
               siz[1]:=0;
               siz[2]:=0;
               if (insflags and IF_AR0)<>0 then
                siz[0]:=asize
               else if (insflags and IF_AR1)<>0 then
                siz[1]:=asize
               else if (insflags and IF_AR2)<>0 then
                siz[2]:=asize;
             end
            else
             begin
               siz[0]:=asize;
               siz[1]:=asize;
               siz[2]:=asize;
             end;

            if (insflags and (IF_SM or IF_SM2))<>0 then
             begin
               if (insflags and IF_SM2)<>0 then
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
                insot:=p^.optypes[i];
                currot:=oper[i]^.ot;
                if ((insot and OT_SIZE_MASK)=0) and
                   ((currot and OT_SIZE_MASK and (not siz[i]))<>0) and
                   { Immediates can always include smaller size }
                   ((currot and OT_IMMEDIATE)=0) and
                    (((insot and OT_SIZE_MASK) or siz[i])<(currot and OT_SIZE_MASK)) then
                  exit;
              end;
          end;

        result:=true;
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
      begin
        result:=FindInsEntry(nil);
      end;


    function taicpu.FindInsentry(objdata:TObjData):boolean;
      var
        i : longint;
      begin
        result:=false;
      { Things which may only be done once, not when a second pass is done to
        optimize }
        if (Insentry=nil) or ((InsEntry^.flags and IF_PASS2)<>0) then
         begin
           { We need intel style operands }
           SetOperandOrder(op_intel);
           { create the .ot fields }
           create_ot(objdata);
           { set the file postion }
           current_filepos:=fileinfo;
         end
        else
         begin
           { we've already an insentry so it's valid }
           result:=true;
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
           if matches(insentry) then
             begin
               result:=true;
               exit;
             end;
           inc(insentry);
         end;
        Message1(asmw_e_invalid_opcode_and_operands,GetString);
        { No instruction found, set insentry to nil and inssize to -1 }
        insentry:=nil;
        inssize:=-1;
      end;


    function taicpu.Pass1(objdata:TObjData):longint;
      begin
        Pass1:=0;
        { Save the old offset and set the new offset }
        InsOffset:=ObjData.CurrObjSec.Size;
        { Error? }
        if (Insentry=nil) and (InsSize=-1) then
          exit;
        { set the file postion }
        current_filepos:=fileinfo;
        { Get InsEntry }
        if FindInsEntry(ObjData) then
         begin
           { Calculate instruction size }
           InsSize:=calcsize(insentry);
           if segprefix<>NR_NO then
            inc(InsSize);
           { Fix opsize if size if forced }
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
           LastInsOffset:=InsOffset;
           Pass1:=InsSize;
           exit;
         end;
        LastInsOffset:=-1;
      end;


    procedure taicpu.Pass2(objdata:TObjData);
      var
        c : longint;
      begin
        { error in pass1 ? }
        if insentry=nil then
         exit;
        current_filepos:=fileinfo;
        { Segment override }
        if (segprefix<>NR_NO) then
         begin
           case segprefix of
             NR_CS : c:=$2e;
             NR_DS : c:=$3e;
             NR_ES : c:=$26;
             NR_FS : c:=$64;
             NR_GS : c:=$65;
             NR_SS : c:=$36;
           end;
           objdata.writebytes(c,1);
           { fix the offset for GenNode }
           inc(InsOffset);
         end;
        { Generate the instruction }
        GenCode(objdata);
      end;


    function taicpu.needaddrprefix(opidx:byte):boolean;
      begin
        result:=(oper[opidx]^.typ=top_ref) and
                (oper[opidx]^.ref^.refaddr=addr_no) and
                (
                 (
                  (oper[opidx]^.ref^.index<>NR_NO) and
                  (getsubreg(oper[opidx]^.ref^.index)<>R_SUBADDR)
                 ) or
                 (
                  (oper[opidx]^.ref^.base<>NR_NO) and
                  (getsubreg(oper[opidx]^.ref^.base)<>R_SUBADDR)
                 )
                );
      end;


    function regval(r:Tregister):byte;
      const
    {$ifdef x86_64}
        opcode_table:array[tregisterindex] of tregisterindex = (
          {$i r8664op.inc}
        );
    {$else x86_64}
        opcode_table:array[tregisterindex] of tregisterindex = (
          {$i r386op.inc}
        );
    {$endif x86_64}
      var
        regidx : tregisterindex;
      begin
        regidx:=findreg_by_number(r);
        if regidx<>0 then
          result:=opcode_table[regidx]
        else
          begin
            Message1(asmw_e_invalid_register,generic_regname(r));
            result:=0;
          end;
      end;


{$ifdef x86_64}
    function process_ea(const input:toper;out output:ea;rfield:longint):boolean;
      var
        sym   : tasmsymbol;
        md,s,rv  : byte;
        base,index,scalefactor,
        o     : longint;
        ir,br : Tregister;
        isub,bsub : tsubregister;
      begin
        process_ea:=false;
        fillchar(output,sizeof(output),0);
        {Register ?}
        if (input.typ=top_reg) then
          begin
            rv:=regval(input.reg);
            output.modrm:=$c0 or (rfield shl 3) or rv;
            output.size:=1;

            if ((getregtype(input.reg)=R_INTREGISTER) and
              (getsupreg(input.reg)>=RS_R8)) or
              ((getregtype(input.reg)=R_MMREGISTER) and
              (getsupreg(input.reg)>=RS_XMM8)) then
              begin
                output.rex_present:=true;
                output.rex:=output.rex or $41;
                inc(output.size,1);
              end
            else if (getregtype(input.reg)=R_INTREGISTER) and
              (getsubreg(input.reg)=R_SUBL) and
              (getsupreg(input.reg) in [RS_RDI,RS_RSI,RS_RBP,RS_RSP]) then
              begin
                output.rex_present:=true;
                output.rex:=output.rex or $40;
                inc(output.size,1);
              end;

            process_ea:=true;

            exit;
         end;
        {No register, so memory reference.}
        if input.typ<>top_ref then
          internalerror(200409263);
        ir:=input.ref^.index;
        br:=input.ref^.base;
        isub:=getsubreg(ir);
        bsub:=getsubreg(br);
        s:=input.ref^.scalefactor;
        o:=input.ref^.offset;
        sym:=input.ref^.symbol;
        if ((ir<>NR_NO) and (getregtype(ir)<>R_INTREGISTER)) or
           ((br<>NR_NO) and (br<>NR_RIP) and (getregtype(br)<>R_INTREGISTER)) then
          internalerror(200301081);
        { it's direct address }
        if (br=NR_NO) and (ir=NR_NO) then
         begin
           output.sib_present:=true;
           output.bytes:=4;
           output.modrm:=4 or (rfield shl 3);
           output.sib:=$25;
         end
        else if (br=NR_RIP) and (ir=NR_NO) then
          begin
            { rip based }
            output.sib_present:=false;
            output.bytes:=4;
            output.modrm:=5 or (rfield shl 3);
          end
        else
        { it's an indirection }
         begin
           { 16 bit or 32 bit address? }
           if ((ir<>NR_NO) and (isub<>R_SUBADDR)) or
              ((br<>NR_NO) and (bsub<>R_SUBADDR)) then
             message(asmw_e_16bit_32bit_not_supported);
           { wrong, for various reasons }
           if (ir=NR_ESP) or ((s<>1) and (s<>2) and (s<>4) and (s<>8) and (ir<>NR_NO)) then
            exit;

           if ((getregtype(br)=R_INTREGISTER) and
             (getsupreg(br)>=RS_R8)) or
             ((getregtype(br)=R_MMREGISTER) and
             (getsupreg(br)>=RS_XMM8)) then
             begin
               output.rex_present:=true;
               output.rex:=output.rex or $41;
             end;

           if ((getregtype(ir)=R_INTREGISTER) and
             (getsupreg(ir)>=RS_R8)) or
             ((getregtype(ir)=R_MMREGISTER) and
             (getsupreg(ir)>=RS_XMM8)) then
             begin
               output.rex_present:=true;
               output.rex:=output.rex or $42;
             end;

           process_ea:=true;


           { base }
           case br of
             NR_R8,
             NR_RAX : base:=0;
             NR_R9,
             NR_RCX : base:=1;
             NR_R10,
             NR_RDX : base:=2;
             NR_R11,
             NR_RBX : base:=3;
             NR_R12,
             NR_RSP : base:=4;
             NR_R13,
             NR_NO,
             NR_RBP : base:=5;
             NR_R14,
             NR_RSI : base:=6;
             NR_R15,
             NR_RDI : base:=7;
           else
             exit;
           end;
           { index }
           case ir of
             NR_R8,
             NR_RAX : index:=0;
             NR_R9,
             NR_RCX : index:=1;
             NR_R10,
             NR_RDX : index:=2;
             NR_R11,
             NR_RBX : index:=3;
             NR_R12,
             NR_NO  : index:=4;
             NR_R13,
             NR_RBP : index:=5;
             NR_R14,
             NR_RSI : index:=6;
             NR_R15,
             NR_RDI : index:=7;
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
           { If rbp or r13 is used we must always include an offset }
           if (br=NR_NO) or
              ((br<>NR_RBP) and (br<>NR_R13) and (o=0) and (sym=nil)) then
            md:=0
           else
            if ((o>=-128) and (o<=127) and (sym=nil)) then
             md:=1
            else
             md:=2;
           if (br=NR_NO) or (md=2) then
            output.bytes:=4
           else
            output.bytes:=md;
           { SIB needed ? }
           if (ir=NR_NO) and (br<>NR_RSP) and (br<>NR_R12) then
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
        output.size:=1+ord(output.sib_present)+ord(output.rex_present)+output.bytes;
        process_ea:=true;
      end;


{$else x86_64}

    function process_ea(const input:toper;out output:ea;rfield:longint):boolean;
      var
        sym   : tasmsymbol;
        md,s,rv  : byte;
        base,index,scalefactor,
        o     : longint;
        ir,br : Tregister;
        isub,bsub : tsubregister;
      begin
        process_ea:=false;
        fillchar(output,sizeof(output),0);
        {Register ?}
        if (input.typ=top_reg) then
          begin
            rv:=regval(input.reg);
            output.modrm:=$c0 or (rfield shl 3) or rv;
            output.size:=1;
            process_ea:=true;
            exit;
         end;
        {No register, so memory reference.}
        if (input.typ<>top_ref) then
          internalerror(200409262);
        if ((input.ref^.index<>NR_NO) and (getregtype(input.ref^.index)<>R_INTREGISTER)) or
           ((input.ref^.base<>NR_NO) and (getregtype(input.ref^.base)<>R_INTREGISTER)) then
          internalerror(200301081);
        ir:=input.ref^.index;
        br:=input.ref^.base;
        isub:=getsubreg(ir);
        bsub:=getsubreg(br);
        s:=input.ref^.scalefactor;
        o:=input.ref^.offset;
        sym:=input.ref^.symbol;
      { it's direct address }
        if (br=NR_NO) and (ir=NR_NO) then
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
           if ((ir<>NR_NO) and (isub<>R_SUBADDR)) or
              ((br<>NR_NO) and (bsub<>R_SUBADDR)) then
             message(asmw_e_16bit_not_supported);
{$ifdef OPTEA}
           { make single reg base }
           if (br=NR_NO) and (s=1) then
            begin
              br:=ir;
              ir:=NR_NO;
            end;
           { convert [3,5,9]*EAX to EAX+[2,4,8]*EAX }
           if (br=NR_NO) and
              (((s=2) and (ir<>NR_ESP)) or
                (s=3) or (s=5) or (s=9)) then
            begin
              br:=ir;
              dec(s);
            end;
           { swap ESP into base if scalefactor is 1 }
           if (s=1) and (ir=NR_ESP) then
            begin
              ir:=br;
              br:=NR_ESP;
            end;
{$endif OPTEA}
           { wrong, for various reasons }
           if (ir=NR_ESP) or ((s<>1) and (s<>2) and (s<>4) and (s<>8) and (ir<>NR_NO)) then
            exit;
           { base }
           case br of
             NR_EAX : base:=0;
             NR_ECX : base:=1;
             NR_EDX : base:=2;
             NR_EBX : base:=3;
             NR_ESP : base:=4;
             NR_NO,
             NR_EBP : base:=5;
             NR_ESI : base:=6;
             NR_EDI : base:=7;
           else
             exit;
           end;
           { index }
           case ir of
             NR_EAX : index:=0;
             NR_ECX : index:=1;
             NR_EDX : index:=2;
             NR_EBX : index:=3;
             NR_NO  : index:=4;
             NR_EBP : index:=5;
             NR_ESI : index:=6;
             NR_EDI : index:=7;
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
           if (br=NR_NO) or
              ((br<>NR_EBP) and (o=0) and (sym=nil)) then
            md:=0
           else
            if ((o>=-128) and (o<=127) and (sym=nil)) then
             md:=1
            else
             md:=2;
           if (br=NR_NO) or (md=2) then
            output.bytes:=4
           else
            output.bytes:=md;
           { SIB needed ? }
           if (ir=NR_NO) and (br<>NR_ESP) then
            begin
              output.sib_present:=false;
              output.modrm:=(longint(md) shl 6) or (rfield shl 3) or base;
            end
           else
            begin
              output.sib_present:=true;
              output.modrm:=(longint(md) shl 6) or (rfield shl 3) or 4;
              output.sib:=(scalefactor shl 6) or (index shl 3) or base;
            end;
         end;
        if output.sib_present then
         output.size:=2+output.bytes
        else
         output.size:=1+output.bytes;
        process_ea:=true;
      end;
{$endif x86_64}

    function taicpu.calcsize(p:PInsEntry):shortint;
      var
        codes : pchar;
        c     : byte;
        len     : shortint;
        ea_data : ea;
      begin
        len:=0;
        codes:=@p^.code[0];
{$ifdef x86_64}
        rex:=0;
{$endif x86_64}
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
{$ifdef x86_64}
                if ((getregtype(oper[c-8]^.reg)=R_INTREGISTER) and
                  (getsupreg(oper[c-8]^.reg)>=RS_R8)) or
                  ((getregtype(oper[c-8]^.reg)=R_MMREGISTER) and
                  (getsupreg(oper[c-8]^.reg)>=RS_XMM8)) then
                  begin
                    if rex=0 then
                      inc(len);
                    rex:=rex or $41;
                  end
                else if (getregtype(oper[c-8]^.reg)=R_INTREGISTER) and
                  (getsubreg(oper[c-8]^.reg)=R_SUBL) and
                  (getsupreg(oper[c-8]^.reg) in [RS_RDI,RS_RSI,RS_RBP,RS_RSP]) then
                  begin
                    if rex=0 then
                      inc(len);

                    rex:=rex or $40;
                  end;
{$endif x86_64}
                inc(codes);
                inc(len);
              end;
            11 :
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
            28,29,30:
              begin
                if opsize=S_Q then
                  inc(len,8)
                else
                  inc(len,4);
              end;
            32,33,34,
            52,53,54,
            56,57,58 :
              inc(len,4);
            192,193,194 :
              if NeedAddrPrefix(c-192) then
               inc(len);
            208,209,210 :
              begin
                case (oper[c-208]^.ot and OT_SIZE_MASK) of
                  OT_BITS16:
                    inc(len);
{$ifdef x86_64}
                  OT_BITS64:
                    begin
                      if rex=0 then
                        inc(len);
                      rex:=rex or $48;
                    end;
{$endif x86_64}
                end;
              end;
            200,
            212 :
              inc(len);
            214 :
              begin
{$ifdef x86_64}
                if rex=0 then
                  inc(len);
                rex:=rex or $48;
{$endif x86_64}
              end;
            201,
            202,
            211,
            213,
            215,
            217,218: ;
            219,220 :
              inc(len);
            221:
{$ifdef x86_64}
              { remove rex competely? }
              if rex=$48 then
                begin
                  rex:=0;
                  dec(len);
                end
              else
                rex:=rex and $f7
{$endif x86_64}
              ;
            64..191 :
              begin
{$ifdef x86_64}
                 if (c<127) then
                  begin
                    if (oper[c and 7]^.typ=top_reg) then
                      begin
                        if ((getregtype(oper[c and 7]^.reg)=R_INTREGISTER) and
                          (getsupreg(oper[c and 7]^.reg)>=RS_R8)) or
                          ((getregtype(oper[c and 7]^.reg)=R_MMREGISTER) and
                          (getsupreg(oper[c and 7]^.reg)>=RS_XMM8)) then
                          begin
                            if rex=0 then
                              inc(len);

                            rex:=rex or $44;
                          end
                        else if (getregtype(oper[c and 7]^.reg)=R_INTREGISTER) and
                          (getsubreg(oper[c and 7]^.reg)=R_SUBL) and
                          (getsupreg(oper[c and 7]^.reg) in [RS_RDI,RS_RSI,RS_RBP,RS_RSP]) then
                          begin
                            if rex=0 then
                              inc(len);

                            rex:=rex or $40;
                          end;
                      end;
                  end;

{$endif x86_64}
                if not process_ea(oper[(c shr 3) and 7]^, ea_data, 0) then
                  Message(asmw_e_invalid_effective_address)
                else
                  inc(len,ea_data.size);
{$ifdef x86_64}
                { did we already create include a rex into the length calculation? }
                if (rex<>0) and (ea_data.rex<>0) then
                  dec(len);
                rex:=rex or ea_data.rex;
{$endif x86_64}

              end;
            else
             InternalError(200603141);
          end;
        until false;
        calcsize:=len;
      end;


    procedure taicpu.GenCode(objdata:TObjData);
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
       * \13           - a literal byte follows in the code stream, to be added
       *                 to the condition code value of the instruction.
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
       * \300,\301,\302 - might be an 0x67, depending on the address size of
       *                 the memory reference in operand x.
       * \310          - indicates fixed 16-bit address size, i.e. optional 0x67.
       * \311          - indicates fixed 32-bit address size, i.e. optional 0x67.
       * \312          - indicates fixed 64-bit address size, i.e. optional 0x48.
       * \320,\321,\322 - might be an 0x66 or 0x48 byte, depending on the operand
       *                 size of operand x.
       * \323          - insert x86_64 REX at this position.
       * \324          - indicates fixed 16-bit operand size, i.e. optional 0x66.
       * \325          - indicates fixed 32-bit operand size, i.e. optional 0x66.
       * \326          - indicates fixed 64-bit operand size, i.e. optional 0x48.
       * \327          - indicates that this instruction is only valid when the
       *                 operand size is the default (instruction to disassembler,
       *                 generates no code in the assembler)
       * \331          - instruction not valid with REP prefix.  Hint for
       *                 disassembler only; for SSE instructions.
       * \332	       - disassemble a rep (0xF3 byte) prefix as repe not rep.
       * \333          - REP prefix (0xF3 byte); for SSE instructions.  Not encoded
       * \335          - removes rex size prefix, i.e. rex.w must be the last opcode
      }

      var
        currval : aint;
        currsym : tobjsymbol;
        currrelreloc,
        currabsreloc,
        currabsreloc32 : TObjRelocationType;
{$ifdef x86_64}
        rexwritten : boolean;
{$endif x86_64}

        procedure getvalsym(opidx:longint);
          begin
            case oper[opidx]^.typ of
              top_ref :
                begin
                  currval:=oper[opidx]^.ref^.offset;
                  currsym:=ObjData.symbolref(oper[opidx]^.ref^.symbol);
{$ifdef x86_64}
                  if oper[opidx]^.ref^.refaddr=addr_pic then
                    begin
                      currrelreloc:=RELOC_PLT32;
                      currabsreloc:=RELOC_GOTPCREL;
                      currabsreloc32:=RELOC_GOTPCREL;
                    end
                  else
{$endif x86_64}
                    begin
                      currrelreloc:=RELOC_RELATIVE;
                      currabsreloc:=RELOC_ABSOLUTE;
                      currabsreloc32:=RELOC_ABSOLUTE32;
                    end;
                end;
              top_const :
                begin
                  currval:=aint(oper[opidx]^.val);
                  currsym:=nil;
                  currabsreloc:=RELOC_ABSOLUTE;
                  currabsreloc32:=RELOC_ABSOLUTE32;
                end;
              else
                Message(asmw_e_immediate_or_reference_expected);
            end;
          end;

{$ifdef x86_64}
       procedure maybewriterex;
       begin
          if (rex<>0) and not(rexwritten) then
            begin
              rexwritten:=true;
              objdata.writebytes(rex,1);
            end;
        end;
{$endif x86_64}

      const
        CondVal:array[TAsmCond] of byte=($0,
         $7, $3, $2, $6, $2, $4, $F, $D, $C, $E, $6, $2,
         $3, $7, $3, $5, $E, $C, $D, $F, $1, $B, $9, $5,
         $0, $A, $A, $B, $8, $4);
      var
        c : byte;
        pb : pbyte;
        codes : pchar;
        bytes : array[0..3] of byte;
        rfield,
        data,s,opidx : longint;
        ea_data : ea;
      begin
        { safety check }
        if objdata.currobjsec.size<>longword(insoffset) then
           internalerror(200130121);
        { load data to write }
        codes:=insentry^.code;
{$ifdef x86_64}
        rexwritten:=false;
{$endif x86_64}
        { Force word push/pop for registers }
        if (opsize=S_W) and ((codes[0]=#4) or (codes[0]=#6) or
            ((codes[0]=#1) and ((codes[2]=#5) or (codes[2]=#7)))) then
          begin
            bytes[0]:=$66;
            objdata.writebytes(bytes,1);
          end;
        repeat
          c:=ord(codes^);
          inc(codes);
          case c of
            0 :
              break;
            1,2,3 :
              begin
                objdata.writebytes(codes^,c);
                inc(codes,c);
              end;
            4,6 :
              begin
                case oper[0]^.reg of
                  NR_CS:
                    bytes[0]:=$e;
                  NR_NO,
                  NR_DS:
                    bytes[0]:=$1e;
                  NR_ES:
                    bytes[0]:=$6;
                  NR_SS:
                    bytes[0]:=$16;
                  else
                    internalerror(777004);
                end;
                if c=4 then
                  inc(bytes[0]);
                objdata.writebytes(bytes,1);
              end;
            5,7 :
              begin
                case oper[0]^.reg of
                  NR_FS:
                    bytes[0]:=$a0;
                  NR_GS:
                    bytes[0]:=$a8;
                  else
                    internalerror(777005);
                end;
                if c=5 then
                  inc(bytes[0]);
                objdata.writebytes(bytes,1);
              end;
            8,9,10 :
              begin
                bytes[0]:=ord(codes^)+regval(oper[c-8]^.reg);
                inc(codes);
                objdata.writebytes(bytes,1);
              end;
            11 :
              begin
                bytes[0]:=ord(codes^)+condval[condition];
                inc(codes);
                objdata.writebytes(bytes,1);
              end;
            15 :
              begin
                bytes[0]:=0;
                objdata.writebytes(bytes,1);
              end;
            12,13,14 :
              begin
                getvalsym(c-12);
                if (currval<-128) or (currval>127) then
                 Message2(asmw_e_value_exceeds_bounds,'signed byte',tostr(currval));
                if assigned(currsym) then
                  objdata.writereloc(currval,1,currsym,currabsreloc)
                else
                  objdata.writebytes(currval,1);
              end;
            16,17,18 :
              begin
                getvalsym(c-16);
                if (currval<-256) or (currval>255) then
                 Message2(asmw_e_value_exceeds_bounds,'byte',tostr(currval));
                if assigned(currsym) then
                 objdata.writereloc(currval,1,currsym,currabsreloc)
                else
                 objdata.writebytes(currval,1);
              end;
            20,21,22 :
              begin
                getvalsym(c-20);
                if (currval<0) or (currval>255) then
                 Message2(asmw_e_value_exceeds_bounds,'unsigned byte',tostr(currval));
                if assigned(currsym) then
                 objdata.writereloc(currval,1,currsym,currabsreloc)
                else
                 objdata.writebytes(currval,1);
              end;
            24,25,26 :
              begin
                getvalsym(c-24);
                if (currval<-65536) or (currval>65535) then
                 Message2(asmw_e_value_exceeds_bounds,'word',tostr(currval));
                if assigned(currsym) then
                 objdata.writereloc(currval,2,currsym,currabsreloc)
                else
                 objdata.writebytes(currval,2);
              end;
            28,29,30 :
              begin
                getvalsym(c-28);
                if opsize=S_Q then
                  begin
                    if assigned(currsym) then
                     objdata.writereloc(currval,8,currsym,currabsreloc)
                    else
                     objdata.writebytes(currval,8);
                  end
                else
                  begin
                    if assigned(currsym) then
                      objdata.writereloc(currval,4,currsym,currabsreloc32)
                    else
                      objdata.writebytes(currval,4);
                  end
              end;
            32,33,34 :
              begin
                getvalsym(c-32);
                if assigned(currsym) then
                 objdata.writereloc(currval,4,currsym,currabsreloc32)
                else
                 objdata.writebytes(currval,4);
              end;
            40,41,42 :
              begin
                getvalsym(c-40);
                data:=currval-insend;
                if assigned(currsym) then
                 inc(data,currsym.address);
                if (data>127) or (data<-128) then
                 Message1(asmw_e_short_jmp_out_of_range,tostr(data));
                objdata.writebytes(data,1);
              end;
            52,53,54 :
              begin
                getvalsym(c-52);
                if assigned(currsym) then
                 objdata.writereloc(currval,4,currsym,currrelreloc)
                else
                 objdata.writereloc(currval-insend,4,nil,currabsreloc32)
              end;
            56,57,58 :
              begin
                getvalsym(c-56);
                if assigned(currsym) then
                 objdata.writereloc(currval,4,currsym,currrelreloc)
                else
                 objdata.writereloc(currval-insend,4,nil,currabsreloc32)
              end;
            192,193,194 :
              begin
                if NeedAddrPrefix(c-192) then
                 begin
                   bytes[0]:=$67;
                   objdata.writebytes(bytes,1);
                 end;
              end;
            200 :
              begin
                bytes[0]:=$67;
                objdata.writebytes(bytes,1);
              end;
            208,209,210 :
              begin
                case oper[c-208]^.ot and OT_SIZE_MASK of
                  OT_BITS16 :
                    begin
                      bytes[0]:=$66;
                      objdata.writebytes(bytes,1);
                    end;
{$ifndef x86_64}
                  OT_BITS64 :
                      Message(asmw_e_64bit_not_supported);
{$endif x86_64}
                end;
{$ifdef x86_64}
                maybewriterex;
{$endif x86_64}
              end;
            211,
            213 :
              begin
{$ifdef x86_64}
                maybewriterex;
{$endif x86_64}
              end;
            212 :
              begin
                bytes[0]:=$66;
                objdata.writebytes(bytes,1);
{$ifdef x86_64}
                maybewriterex;
{$endif x86_64}
              end;
            214 :
              begin
{$ifdef x86_64}
                maybewriterex;
{$else x86_64}
                Message(asmw_e_64bit_not_supported);
{$endif x86_64}
              end;
            219 :
              begin
                bytes[0]:=$f3;
                objdata.writebytes(bytes,1);
{$ifdef x86_64}
                maybewriterex;
{$endif x86_64}
              end;
            220 :
              begin
                bytes[0]:=$f2;
                objdata.writebytes(bytes,1);
              end;
            221:
              ;
            201,
            202,
            215,
            217,218 :
              begin
                { these are dissambler hints or 32 bit prefixes which
                  are not needed
                  It's usefull to write rex :) (FK) }
{$ifdef x86_64}
                maybewriterex;
{$endif x86_64}
              end;
            31,
            48,49,50 :
              begin
                InternalError(777006);
              end
            else
              begin
                { rex should be written at this point }
{$ifdef x86_64}
                if (rex<>0) and not(rexwritten) then
                  internalerror(200603191);
{$endif x86_64}
                if (c>=64) and (c<=191) then
                 begin
                   if (c<127) then
                    begin
                      if (oper[c and 7]^.typ=top_reg) then
                        rfield:=regval(oper[c and 7]^.reg)
                      else
                        rfield:=regval(oper[c and 7]^.ref^.base);
                    end
                   else
                    rfield:=c and 7;
                   opidx:=(c shr 3) and 7;
                   if not process_ea(oper[opidx]^,ea_data,rfield) then
                     Message(asmw_e_invalid_effective_address);

                   pb:=@bytes[0];
                   pb^:=ea_data.modrm;
                   inc(pb);
                   if ea_data.sib_present then
                    begin
                      pb^:=ea_data.sib;
                      inc(pb);
                    end;

                   s:=pb-@bytes[0];
                   objdata.writebytes(bytes,s);

                   case ea_data.bytes of
                     0 : ;
                     1 :
                       begin
                         if (oper[opidx]^.ot and OT_MEMORY)=OT_MEMORY then
                           begin
                             currsym:=objdata.symbolref(oper[opidx]^.ref^.symbol);
{$ifdef x86_64}
                             if oper[opidx]^.ref^.refaddr=addr_pic then
                               currabsreloc:=RELOC_GOTPCREL
                             else
{$endif x86_64}
                               currabsreloc:=RELOC_ABSOLUTE;
                             objdata.writereloc(oper[opidx]^.ref^.offset,1,currsym,currabsreloc);
                           end
                         else
                          begin
                            bytes[0]:=oper[opidx]^.ref^.offset;
                            objdata.writebytes(bytes,1);
                          end;
                         inc(s);
                       end;
                     2,4 :
                       begin
                         currsym:=objdata.symbolref(oper[opidx]^.ref^.symbol);
{$ifdef x86_64}
                         if oper[opidx]^.ref^.refaddr=addr_pic then
                           currabsreloc:=RELOC_GOTPCREL
                         else
{$endif x86_64}
                           currabsreloc:=RELOC_ABSOLUTE32;
                         objdata.writereloc(oper[opidx]^.ref^.offset,ea_data.bytes,currsym,currabsreloc);
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


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=(((opcode=A_MOV) or (opcode=A_XCHG)) and
                 (regtype = R_INTREGISTER) and
                 (ops=2) and
                 (oper[0]^.typ=top_reg) and
                 (oper[1]^.typ=top_reg) and
                 (oper[0]^.reg=oper[1]^.reg)
                ) or
                (((opcode=A_MOVSS) or (opcode=A_MOVSD) or (opcode=A_MOVQ) or
                  (opcode=A_MOVAPS) or (OPCODE=A_MOVAPD)) and
                 (regtype = R_MMREGISTER) and
                 (ops=2) and
                 (oper[0]^.typ=top_reg) and
                 (oper[1]^.typ=top_reg) and
                 (oper[0]^.reg=oper[1]^.reg)
                );
      end;


    procedure build_spilling_operation_type_table;
      var
        opcode : tasmop;
        i      : integer;
      begin
        new(operation_type_table);
        fillchar(operation_type_table^,sizeof(toperation_type_table),byte(operand_read));
        for opcode:=low(tasmop) to high(tasmop) do
          begin
            for i:=1 to MaxInsChanges do
              begin
                case InsProp[opcode].Ch[i] of
                  Ch_Rop1 :
                    operation_type_table^[opcode,0]:=operand_read;
                  Ch_Wop1 :
                    operation_type_table^[opcode,0]:=operand_write;
                  Ch_RWop1,
                  Ch_Mop1 :
                    operation_type_table^[opcode,0]:=operand_readwrite;
                  Ch_Rop2 :
                    operation_type_table^[opcode,1]:=operand_read;
                  Ch_Wop2 :
                    operation_type_table^[opcode,1]:=operand_write;
                  Ch_RWop2,
                  Ch_Mop2 :
                    operation_type_table^[opcode,1]:=operand_readwrite;
                  Ch_Rop3 :
                    operation_type_table^[opcode,2]:=operand_read;
                  Ch_Wop3 :
                    operation_type_table^[opcode,2]:=operand_write;
                  Ch_RWop3,
                  Ch_Mop3 :
                    operation_type_table^[opcode,2]:=operand_readwrite;
                end;
              end;
          end;
        { Special cases that can't be decoded from the InsChanges flags }
        operation_type_table^[A_IMUL,1]:=operand_readwrite;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        { the information in the instruction table is made for the string copy
          operation MOVSD so hack here (FK)
        }
        if (opcode=A_MOVSD) and (ops=2) then
          begin
            case opnr of
              0:
                result:=operand_read;
              1:
                result:=operand_write;
              else
                internalerror(200506055);
            end
          end
        else
          result:=operation_type_table^[opcode,opnr];
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_ref_reg(A_MOV,reg2opsize(r),ref,r);
          R_MMREGISTER :
            case getsubreg(r) of
              R_SUBMMD:
                result:=taicpu.op_ref_reg(A_MOVSD,reg2opsize(r),ref,r);
              R_SUBMMS:
                result:=taicpu.op_ref_reg(A_MOVSS,reg2opsize(r),ref,r);
              R_SUBMMWHOLE:
                result:=taicpu.op_ref_reg(A_MOVQ,S_NO,ref,r);
              else
                internalerror(200506043);
            end;
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_MOV,reg2opsize(r),r,ref);
          R_MMREGISTER :
            case getsubreg(r) of
              R_SUBMMD:
                result:=taicpu.op_reg_ref(A_MOVSD,reg2opsize(r),r,ref);
              R_SUBMMS:
                result:=taicpu.op_reg_ref(A_MOVSS,reg2opsize(r),r,ref);
              R_SUBMMWHOLE:
                result:=taicpu.op_reg_ref(A_MOVQ,S_NO,r,ref);
              else
                internalerror(200506042);
            end;
          else
            internalerror(200401041);
        end;
      end;


{*****************************************************************************
                              Instruction table
*****************************************************************************}

    procedure BuildInsTabCache;
      var
        i : longint;
      begin
        new(instabcache);
        FillChar(instabcache^,sizeof(tinstabcache),$ff);
        i:=0;
        while (i<InsTabEntries) do
         begin
           if InsTabCache^[InsTab[i].OPcode]=-1 then
            InsTabCache^[InsTab[i].OPcode]:=i;
           inc(i);
         end;
      end;


    procedure InitAsm;
      begin
        build_spilling_operation_type_table;
        if not assigned(instabcache) then
          BuildInsTabCache;
      end;


    procedure DoneAsm;
      begin
        if assigned(operation_type_table) then
          begin
            dispose(operation_type_table);
            operation_type_table:=nil;
          end;
        if assigned(instabcache) then
          begin
            dispose(instabcache);
            instabcache:=nil;
          end;
      end;


begin
  cai_align:=tai_align;
  cai_cpu:=taicpu;
end.
