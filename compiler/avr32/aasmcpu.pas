{
    Copyright (c) 2003 by Florian Klaempfl

    Contains the assembler object for the AVR32

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
  cclasses,globtype,globals,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  ogbase,
  symtype,
  cpubase,cpuinfo,cgbase,cgutils;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

      { Operand types }
      OT_NONE      = $00000000;

      OT_BITS5     = $00000001;  { size, and other attributes, of the operand  }
      OT_BITS8     = $00000002;
      OT_BITS16    = $00000002;
      OT_BITS21    = $00000002;
      OT_BITS32    = $00000004;
      OT_BITS64    = $00000008;  { FPU only  }
      OT_BITS80    = $00000010;
      OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
      OT_NEAR      = $00000040;
      OT_SHORT     = $00000080;
      OT_BITSTINY  = $00000100;  { fpu constant }
      OT_BITSSHIFTER =
                     $00000200;
      OT_COH =
                     $00000200;

      OT_SIZE_MASK = $000003FF;  { all the size attributes  }
      OT_NON_SIZE  = longint(not OT_SIZE_MASK);

      OT_SIGNED    = $00000100;  { the operand need to be signed -128-127 }

      OT_TO        = $00000200;  { operand is followed by a colon  }
                                 { reverse effect in FADD, FSUB &c  }
      OT_COLON     = $00000400;

      OT_SHIFTEROP = $00000800;
      OT_REGISTER  = $00001000;
      OT_IMM       = $00002000;
      OT_REGLIST   = $00008000;
      OT_IMM5      = $00002001;
      OT_IMM8      = $00002001;
      OT_IMM16     = $00002001;
      OT_IMM24     = $00002002;
      OT_IMM32     = $00002004;
      OT_IMM64     = $00002008;
      OT_IMM80     = $00002010;
      OT_IMMTINY   = $00002100;
      OT_IMMSHIFTER= $00002200;
      OT_IMMEDIATE24 = OT_IMM24;
      OT_SHIFTIMM  = OT_SHIFTEROP or OT_IMMSHIFTER;
      OT_SHIFTIMMEDIATE = OT_SHIFTIMM;
      OT_IMMEDIATESHIFTER = OT_IMMSHIFTER;

      OT_IMMEDIATEFPU = OT_IMMTINY;

      OT_REGMEM    = $00200000;  { for r/m, ie EA, operands  }
      OT_REGNORM   = $00201000;  { 'normal' reg, qualifies as EA  }
      OT_REG8      = $00201001;
      OT_REG16     = $00201002;
      OT_REG32     = $00201004;
      OT_REG64     = $00201008;
      OT_VREG      = $00201010;  { vector register }
      OT_REGF      = $00201020;  { coproc register }
      OT_MEMORY    = $00204000;  { register number in 'basereg'  }
      OT_MEM8      = $00204001;
      OT_MEM16     = $00204002;
      OT_MEM32     = $00204004;
      OT_MEM64     = $00204008;
      OT_MEM80     = $00204010;
      { word/byte load/store }
      OT_AM2       = $00010000;
      { misc ld/st operations }
      OT_AM3       = $00020000;
      { multiple ld/st operations }
      OT_AM4       = $00040000;
      { co proc. ld/st operations }
      OT_AM5       = $00080000;
      OT_AMMASK    = $000f0000;
      { IT instruction }
      OT_CONDITION = $00100000;

      OT_MEMORYAM2 = OT_MEMORY or OT_AM2;
      OT_MEMORYAM3 = OT_MEMORY or OT_AM3;
      OT_MEMORYAM4 = OT_MEMORY or OT_AM4;
      OT_MEMORYAM5 = OT_MEMORY or OT_AM5;

      OT_FPUREG    = $01000000;  { floating point stack registers  }
      OT_REG_SMASK = $00070000;  { special register operands: these may be treated differently  }
                                 { a mask for the following  }

      OT_MEM_OFFS  = $00604000;  { special type of EA  }
                                 { simple [address] offset  }
      OT_ONENESS   = $00800000;  { special type of immediate operand  }
                                 { so UNITY == IMMEDIATE | ONENESS  }
      OT_UNITY     = $00802000;  { for shift/rotate instructions  }

      instabentries = {$i avr32nop.inc}

      maxinfolen = 5;

      IF_NONE   = $00000000;

      IF_ARMMASK    = $000F0000;
      IF_1          = $00070000;
      IF_FPMASK     = $00F00000;
      IF_FPA        = $00100000;

      { if the instruction can change in a second pass }
      IF_PASS2  = longint($80000000);

    type
      TInsTabCache=array[TasmOp] of longint;
      PInsTabCache=^TInsTabCache;

      tinsentry = record
        opcode  : tasmop;
        ops     : byte;
        optypes : array[0..3] of longint;
        code    : array[0..maxinfolen] of char;
        flags   : longint;
      end;

      pinsentry=^tinsentry;

    const
      InsTab : array[0..instabentries-1] of TInsEntry={$i avr32tab.inc}

    var
      InsTabCache : PInsTabCache;

    type

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
         oppostfix : TOpPostfix;
         wideformat : boolean;
         roundingmode : troundingmode;
         procedure loadregisterselector(opidx:longint;_op1:tregister;aselector:tregisterselector);
         procedure loadCOH(opidx:longint);
         procedure loadshifterop(opidx:longint;const so:tshifterop);
         procedure loadregset(opidx:longint; regsetregtype: tregistertype; regsetsubregtype: tsubregister; const s:tcpuregisterset);
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_const(op : tasmop;_op1 : longint);
         constructor op_regset(op:tasmop; regtype: tregistertype; subreg: tsubregister; _op1: tcpuregisterset);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
         constructor op_reg_const_coh(op:tasmop; _op1: tregister; _op2: aint);

         constructor op_ref_regset(op:tasmop; _op1: treference; regtype: tregistertype; subreg: tsubregister; _op2: tcpuregisterset);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         constructor op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);
         constructor op_reg_reg_reg_shifterop(op : tasmop;_op1,_op2,_op3 : tregister;_op4 : tshifterop);
         constructor op_reg_reg_const_const(op : tasmop;_op1,_op2 : tregister; _op3,_op4: aint);
         { SFM/LFM }
         constructor op_reg_const_ref(op : tasmop;_op1 : tregister;_op2 : aint;_op3 : treference);

         { *M*LL }
         constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);

         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         function spilling_get_operation_type(opnr: longint): topertype;override;

         { assembler }
      public
         { the next will reset all instructions that can change in pass 2 }
         procedure ResetPass1;override;
         procedure ResetPass2;override;
         function  CheckIfValid:boolean;
         function GetString:string;
         function  Pass1(objdata:TObjData):longint;override;
         procedure Pass2(objdata:TObjData);override;
      protected
         procedure ppuloadoper(ppufile:tcompilerppufile;var o:toper);override;
         procedure ppuwriteoper(ppufile:tcompilerppufile;const o:toper);override;
         procedure ppubuildderefimploper(var o:toper);override;
         procedure ppuderefoper(var o:toper);override;
      private
         { next fields are filled in pass1, so pass2 is faster }
         inssize   : shortint;
         insoffset : longint;
         LastInsOffset : longint; { need to be public to be reset }
         insentry  : PInsEntry;
         function  InsEnd:longint;
         procedure create_ot(objdata:TObjData);
         function  Matches(p:PInsEntry):longint;
         function  calcsize(p:PInsEntry):shortint;
         procedure gencode(objdata:TObjData);
         function  NeedAddrPrefix(opidx:byte):boolean;
         procedure Swapoperands;
         function  FindInsentry(objdata:TObjData):boolean;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    function setoppostfix(i : taicpu;pf : toppostfix) : taicpu;
    function setroundingmode(i : taicpu;rm : troundingmode) : taicpu;
    function setcondition(i : taicpu;c : tasmcond) : taicpu;

    { inserts pc relative symbols at places where they are reachable
      and transforms special instructions to valid instruction encodings }
    procedure finalizeavr32code(list,listtoinsert : TAsmList);

    procedure InitAsm;
    procedure DoneAsm;


implementation

  uses
    cutils,rgobj,itcpugas;

    procedure taicpu.loadregisterselector(opidx: longint; _op1: tregister; aselector: tregisterselector);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_selector then
              clearop(opidx);
            topreg:=_op1;
            selector:=aselector;
            typ:=top_selector;
          end;
      end;

    procedure taicpu.loadCOH(opidx: longint);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_coh then
              clearop(opidx);
            typ:=top_coh;
          end;
      end;

    procedure taicpu.loadshifterop(opidx:longint;const so:tshifterop);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_shifterop then
              begin
                clearop(opidx);
                new(shifterop);
              end;
            shifterop^:=so;
            typ:=top_shifterop;
          end;
      end;


    procedure taicpu.loadregset(opidx:longint; regsetregtype: tregistertype; regsetsubregtype: tsubregister; const s:tcpuregisterset);
      var
        i : byte;
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_regset then
             begin
               clearop(opidx);
               new(regset);
             end;
           regset^:=s;
           regtyp:=regsetregtype;
           subreg:=regsetsubregtype;
           typ:=top_regset;
           case regsetregtype of
             R_INTREGISTER:
               for i:=RS_R0 to RS_R15 do
                 begin
                   if assigned(add_reg_instruction_hook) and (i in regset^) then
                     add_reg_instruction_hook(self,newreg(R_INTREGISTER,i,regsetsubregtype));
                 end;
           end;
         end;
      end;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
      end;


    { for pld }
    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
         inherited create(op);
         ops:=1;
         loadref(0,_op1);
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
         loadconst(0,aint(_op1));
      end;

    constructor taicpu.op_regset(op: tasmop; regtype: tregistertype; subreg: tsubregister; _op1: tcpuregisterset);
      begin
         inherited create(op);
         ops:=1;
         loadregset(0,regtype,subreg,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_ref_reg(op: tasmop; const _op1: treference; _op2: tregister);
      begin
         inherited create(op);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aint(_op2));
      end;

    constructor taicpu.op_reg_const_coh(op: tasmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,aint(_op2));
         loadCOH(2);
      end;


    constructor taicpu.op_ref_regset(op:tasmop; _op1: treference; regtype: tregistertype; subreg: tsubregister; _op2: tcpuregisterset);
      begin
         inherited create(op);
         ops:=2;
         loadref(0,_op1);
         loadregset(1,regtype,subreg,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
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


     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aint(_op3));
      end;


    constructor taicpu.op_reg_const_ref(op : tasmop;_op1 : tregister;_op2 : aint;_op3 : treference);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,_op2);
         loadref(2,_op3);
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


     constructor taicpu.op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadshifterop(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_shifterop(op : tasmop;_op1,_op2,_op3 : tregister;_op4 : tshifterop);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadshifterop(3,_op4);
      end;

    constructor taicpu.op_reg_reg_const_const(op: tasmop; _op1, _op2: tregister; _op3, _op4: aint);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,_op3);
         loadconst(3,_op4);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
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


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        { allow the register allocator to remove unnecessary moves }
        result:=(((opcode=A_MOV) and (regtype = R_INTREGISTER))
                ) and
                (condition=C_None) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      var
        op: tasmop;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=setoppostfix(taicpu.op_reg_ref(A_LDDSP,r,ref),PF_W);
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      var
        op: tasmop;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=setoppostfix(taicpu.op_ref_reg(A_STDSP,ref,r),PF_W);
          else
            internalerror(200401041);
        end;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        result := operand_read;
        case opcode of
          A_ABS,
          A_ACR,
          A_NEG,
          A_SCR,
          A_SATRNDS,A_SATRNDU,
          A_SATS,A_SATU,
          A_MACHH,A_MACWH,A_MACSATHH,
          A_ANDH,A_ANDL,
          A_COM,
          A_EORH,A_EORL,
          A_ORH,A_ORL,
          A_TST,
          A_MAC,A_MACS,A_MACU,
          A_CASTS,A_CASTU,
          A_SBR,A_CBR,A_SWAP,
          A_ROL,A_ROR:
            if opnr = 0 then
              result:=operand_readwrite;
          A_CPC,A_CP,
          A_TBNZ,
          A_BLD,A_BST,
          A_BR,
          A_RJMP,A_ACALL,A_ICALL,A_MCALL,A_RCALL,A_SCALL,
          A_RET,A_RETD,A_RETE,A_RETS,
          A_MEMC,A_MEMS,A_MEMT,A_POPM,
          A_PUSHM:;

          A_ADD,
          A_RSUB,A_SUB,
          A_MUL,
          A_AND,A_ANDN,
          A_EOR,A_OR,
          A_ASR,
          A_LSL,
          A_LSR:
            if (ops=2) and (opnr=0) then
              result:=operand_readwrite
            else if opnr=0 then
              result:=operand_write;

          A_ADC,A_ADDABS,
          A_MAX,A_MIN,A_SBC,
          A_DIVS,A_DIVU,
          A_ADDHH,
          A_MULS,A_MULU,
          A_MULHH,A_MULWH,A_MULNHH,A_MULNWH,
          A_SATADD,A_SATSUB,
          A_SUBHH,
          A_MULSATHH,A_MULSATRNDHH,A_MULSATRNDWH,A_MULSATWH,
          A_BFEXTS,A_BFEXTU,
          A_BFINS,
          A_BREV,
          A_CLZ,
          A_MOV,
          A_MOVH,
          A_LD,A_LDINS,A_LDSWP,A_LDDPC,A_LDDSP,
          A_ST,A_STCOND,A_STDSP,A_STHH,A_STSWP,
          A_XCHG,
          A_LDM,A_LDMTS,A_STM,A_STMTS,
          A_BREAKPOINT,
          A_CACHE,
          A_CSRF,
          A_CSRFCZ,
          A_FRS,
          A_MFDR,
          A_MFSR,
          A_MTDR,
          A_MTSR,
          A_MUSFR,
          A_MUSTR,
          A_NOP,
          A_PREF,
          A_SLEEP,
          A_SR,
          A_SSRF,
          A_SYNC,
          A_TLBR,
          A_TLBS,
          A_TLBW,
          A_COP,
          A_LDC,
          A_LDC0,
          A_LDCM,
          A_MVCR,
          A_MVRC,
          A_STC,
          A_STC0,
          A_STCM,
          A_PABS,
          A_PACKSH,
          A_PACKW,
          A_PADD,
          A_PADDH,
          A_PADDS,
          A_PADDSUB,
          A_PADDSUBH,
          A_PADDSUBS,
          A_PADDX,
          A_PADDXH,
          A_PADDXS,
          A_PASR,
          A_PAVG,
          A_PLSL,
          A_PLSR,
          A_PMAX,
          A_PMIN,
          A_PSAD,
          A_PSUB,
          A_PSUBADD,
          A_PSUBADDH,
          A_PSUBADDS,
          A_PSUBH,
          A_PSUBS,
          A_PSUBX,
          A_PSUBXH,
          A_PSUBXS,
          A_PUNPCK:
            if opnr = 0 then
              result := operand_write
            else
              result := operand_read;
          else
            internalerror(200403151);
        end;
      end;


    procedure BuildInsTabCache;
      var
        i : longint;
      begin
        new(instabcache);
        FillChar(instabcache^,sizeof(tinstabcache),$ff);
        i:=0;
        while (i<InsTabEntries) do
          begin
            if InsTabCache^[InsTab[i].Opcode]=-1 then
              InsTabCache^[InsTab[i].Opcode]:=i;
            inc(i);
          end;
      end;


    procedure InitAsm;
      begin
        if not assigned(instabcache) then
          BuildInsTabCache;
      end;


    procedure DoneAsm;
      begin
        if assigned(instabcache) then
          begin
            dispose(instabcache);
            instabcache:=nil;
          end;
      end;


    function setoppostfix(i : taicpu;pf : toppostfix) : taicpu;
      begin
        i.oppostfix:=pf;
        result:=i;
      end;


    function setroundingmode(i : taicpu;rm : troundingmode) : taicpu;
      begin
        i.roundingmode:=rm;
        result:=i;
      end;


    function setcondition(i : taicpu;c : tasmcond) : taicpu;
      begin
        i.condition:=c;
        result:=i;
      end;


    Function SimpleGetNextInstruction(Current: tai; Var Next: tai): Boolean;
      Begin
        Current:=tai(Current.Next);
        While Assigned(Current) And (Current.typ In SkipInstr) Do
          Current:=tai(Current.Next);
        Next:=Current;
        If Assigned(Next) And Not(Next.typ In SkipInstr) Then
           Result:=True
          Else
            Begin
              Next:=Nil;
              Result:=False;
            End;
      End;


    procedure insertpcrelativedata(list,listtoinsert : TAsmList);
      var
        curinspos,
        penalty,
        lastinspos,
        { increased for every data element > 4 bytes inserted }
        extradataoffset,
        limit: longint;
        curop : longint;
        curtai : tai;
        curdatatai,hp,hp2 : tai;
        curdata : TAsmList;
        l : tasmlabel;
        doinsert,
        removeref : boolean;
      begin
        curdata:=TAsmList.create;
        lastinspos:=-1;
        curinspos:=0;
        extradataoffset:=0;
        limit:=1016;
        curtai:=tai(list.first);
        doinsert:=false;
        while assigned(curtai) do
          begin
            { instruction? }
            case curtai.typ of
              ait_instruction:
                begin
                  { walk through all operand of the instruction }
                  for curop:=0 to taicpu(curtai).ops-1 do
                    begin
                      { reference? }
                      if (taicpu(curtai).oper[curop]^.typ=top_ref) then
                        begin
                          { pc relative symbol? }
                          curdatatai:=tai(taicpu(curtai).oper[curop]^.ref^.symboldata);
                          if assigned(curdatatai) and
                            { move only if we're at the first reference of a label }
                            (taicpu(curtai).oper[curop]^.ref^.offset=0) then
                            begin
                              { check if symbol already used. }
                              { if yes, reuse the symbol }
                              hp:=tai(curdatatai.next);
                              removeref:=false;
                              if assigned(hp) then
                                begin
                                  case hp.typ of
                                    ait_const:
                                      begin
                                        if (tai_const(hp).consttype=aitconst_64bit) then
                                          inc(extradataoffset);
                                      end;
                                    ait_comp_64bit,
                                    ait_real_64bit:
                                      begin
                                        inc(extradataoffset);
                                      end;
                                    ait_real_80bit:
                                      begin
                                        inc(extradataoffset,2);
                                      end;
                                  end;
                                  if (hp.typ=ait_const) then
                                    begin
                                      hp2:=tai(curdata.first);
                                      while assigned(hp2) do
                                        begin
    {                                      if armconstequal(hp2,hp) then }
                                          if (hp2.typ=ait_const) and (tai_const(hp2).sym=tai_const(hp).sym)
                                            and (tai_const(hp2).value=tai_const(hp).value) and (tai(hp2.previous).typ=ait_label)
                                          then
                                            begin
                                              with taicpu(curtai).oper[curop]^.ref^ do
                                                begin
                                                  symboldata:=hp2.previous;
                                                  symbol:=tai_label(hp2.previous).labsym;
                                                end;
                                              removeref:=true;
                                              break;
                                            end;
                                          hp2:=tai(hp2.next);
                                        end;
                                    end;
                                end;
                              { move or remove symbol reference }
                              repeat
                                hp:=tai(curdatatai.next);
                                listtoinsert.remove(curdatatai);
                                if removeref then
                                  curdatatai.free
                                else
                                  curdata.concat(curdatatai);
                                curdatatai:=hp;
                              until (curdatatai=nil) or (curdatatai.typ=ait_label);
                              if lastinspos=-1 then
                                lastinspos:=curinspos;
                            end;
                        end;
                    end;
                  inc(curinspos);
                end;
              ait_const:
                begin
                  inc(curinspos);
                  if (tai_const(curtai).consttype=aitconst_64bit) then
                    inc(curinspos);
                end;
              ait_real_32bit:
                begin
                  inc(curinspos);
                end;
              ait_comp_64bit,
              ait_real_64bit:
                begin
                  inc(curinspos,2);
                end;
              ait_real_80bit:
                begin
                  inc(curinspos,3);
                end;
            end;
            { special case for case jump tables }
            if SimpleGetNextInstruction(curtai,hp) and
              (tai(hp).typ=ait_instruction) and
              (taicpu(hp).opcode=A_LD) and
              (taicpu(hp).oper[0]^.typ=top_reg) and
              (taicpu(hp).oper[0]^.reg=NR_PC) then
              begin
                penalty:=1;
                hp:=tai(hp.next);
                while assigned(hp) and (hp.typ=ait_const) do
                  begin
                    inc(penalty);
                    hp:=tai(hp.next);
                  end;
              end
            else
              penalty:=0;

            { don't miss an insert }
            doinsert:=doinsert or
              (not(curdata.empty) and
               (curinspos-lastinspos+penalty+extradataoffset>limit));

            { split only at real instructions else the test below fails }
            if doinsert and (curtai.typ=ait_instruction) and
              (
                { don't split loads of pc to lr and the following move }
                not(
                    (taicpu(curtai).opcode=A_MOV) and
                    (taicpu(curtai).oper[0]^.typ=top_reg) and
                    (taicpu(curtai).oper[0]^.reg=NR_R14) and
                    (taicpu(curtai).oper[1]^.typ=top_reg) and
                    (taicpu(curtai).oper[1]^.reg=NR_PC)
                   )
              ) then
              begin
                lastinspos:=-1;
                extradataoffset:=0;
                limit:=1016;
                doinsert:=false;
                hp:=tai(curtai.next);
                current_asmdata.getjumplabel(l);
                curdata.insert(taicpu.op_sym(A_BR,l));
                curdata.concat(tai_label.create(l));
                curdata.Insert(tai_align.Create_zeros(4));
                list.insertlistafter(curtai,curdata);
                curtai:=hp;
              end
            else
              curtai:=tai(curtai.next);
          end;
        list.concat(tai_align.Create_zeros(4));
        list.concatlist(curdata);
        curdata.free;
      end;

    procedure finalizeavr32code(list, listtoinsert: TAsmList);
      begin
        insertpcrelativedata(list, listtoinsert);
      end;

(*
      Floating point instruction format information, taken from the linux kernel
      ARM Floating Point Instruction Classes
      | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
      |c o n d|1 1 0 P|U|u|W|L|   Rn  |v|  Fd |0|0|0|1|  o f f s e t  | CPDT
      |c o n d|1 1 0 P|U|w|W|L|   Rn  |x|  Fd |0|0|1|0|  o f f s e t  | CPDT (copro 2)
      | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
      |c o n d|1 1 1 0|a|b|c|d|e|  Fn |j|  Fd |0|0|0|1|f|g|h|0|i|  Fm | CPDO
      |c o n d|1 1 1 0|a|b|c|L|e|  Fn |   Rd  |0|0|0|1|f|g|h|1|i|  Fm | CPRT
      |c o n d|1 1 1 0|a|b|c|1|e|  Fn |1|1|1|1|0|0|0|1|f|g|h|1|i|  Fm | comparisons
      | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |

      CPDT            data transfer instructions
                      LDF, STF, LFM (copro 2), SFM (copro 2)

      CPDO            dyadic arithmetic instructions
                      ADF, MUF, SUF, RSF, DVF, RDF,
                      POW, RPW, RMF, FML, FDV, FRD, POL

      CPDO            monadic arithmetic instructions
                      MVF, MNF, ABS, RND, SQT, LOG, LGN, EXP,
                      SIN, COS, TAN, ASN, ACS, ATN, URD, NRM

      CPRT            joint arithmetic/data transfer instructions
                      FIX (arithmetic followed by load/store)
                      FLT (load/store followed by arithmetic)
                      CMF, CNF CMFE, CNFE (comparisons)
                      WFS, RFS (write/read floating point status register)
                      WFC, RFC (write/read floating point control register)

      cond            condition codes
      P               pre/post index bit: 0 = postindex, 1 = preindex
      U               up/down bit: 0 = stack grows down, 1 = stack grows up
      W               write back bit: 1 = update base register (Rn)
      L               load/store bit: 0 = store, 1 = load
      Rn              base register
      Rd              destination/source register
      Fd              floating point destination register
      Fn              floating point source register
      Fm              floating point source register or floating point constant

      uv              transfer length (TABLE 1)
      wx              register count (TABLE 2)
      abcd            arithmetic opcode (TABLES 3 & 4)
      ef              destination size (rounding precision) (TABLE 5)
      gh              rounding mode (TABLE 6)
      j               dyadic/monadic bit: 0 = dyadic, 1 = monadic
      i               constant bit: 1 = constant (TABLE 6)
      */

      /*
      TABLE 1
      +-------------------------+---+---+---------+---------+
      |  Precision              | u | v | FPSR.EP | length  |
      +-------------------------+---+---+---------+---------+
      | Single                  | 0 | 0 |    x    | 1 words |
      | Double                  | 1 | 1 |    x    | 2 words |
      | Extended                | 1 | 1 |    x    | 3 words |
      | Packed decimal          | 1 | 1 |    0    | 3 words |
      | Expanded packed decimal | 1 | 1 |    1    | 4 words |
      +-------------------------+---+---+---------+---------+
      Note: x = don't care
      */

      /*
      TABLE 2
      +---+---+---------------------------------+
      | w | x | Number of registers to transfer |
      +---+---+---------------------------------+
      | 0 | 1 |  1                              |
      | 1 | 0 |  2                              |
      | 1 | 1 |  3                              |
      | 0 | 0 |  4                              |
      +---+---+---------------------------------+
      */

      /*
      TABLE 3: Dyadic Floating Point Opcodes
      +---+---+---+---+----------+-----------------------+-----------------------+
      | a | b | c | d | Mnemonic | Description           | Operation             |
      +---+---+---+---+----------+-----------------------+-----------------------+
      | 0 | 0 | 0 | 0 | ADF      | Add                   | Fd := Fn + Fm         |
      | 0 | 0 | 0 | 1 | MUF      | Multiply              | Fd := Fn * Fm         |
      | 0 | 0 | 1 | 0 | SUF      | Subtract              | Fd := Fn - Fm         |
      | 0 | 0 | 1 | 1 | RSF      | Reverse subtract      | Fd := Fm - Fn         |
      | 0 | 1 | 0 | 0 | DVF      | Divide                | Fd := Fn / Fm         |
      | 0 | 1 | 0 | 1 | RDF      | Reverse divide        | Fd := Fm / Fn         |
      | 0 | 1 | 1 | 0 | POW      | Power                 | Fd := Fn ^ Fm         |
      | 0 | 1 | 1 | 1 | RPW      | Reverse power         | Fd := Fm ^ Fn         |
      | 1 | 0 | 0 | 0 | RMF      | Remainder             | Fd := IEEE rem(Fn/Fm) |
      | 1 | 0 | 0 | 1 | FML      | Fast Multiply         | Fd := Fn * Fm         |
      | 1 | 0 | 1 | 0 | FDV      | Fast Divide           | Fd := Fn / Fm         |
      | 1 | 0 | 1 | 1 | FRD      | Fast reverse divide   | Fd := Fm / Fn         |
      | 1 | 1 | 0 | 0 | POL      | Polar angle (ArcTan2) | Fd := arctan2(Fn,Fm)  |
      | 1 | 1 | 0 | 1 |          | undefined instruction | trap                  |
      | 1 | 1 | 1 | 0 |          | undefined instruction | trap                  |
      | 1 | 1 | 1 | 1 |          | undefined instruction | trap                  |
      +---+---+---+---+----------+-----------------------+-----------------------+
      Note: POW, RPW, POL are deprecated, and are available for backwards
            compatibility only.
      */

      /*
      TABLE 4: Monadic Floating Point Opcodes
      +---+---+---+---+----------+-----------------------+-----------------------+
      | a | b | c | d | Mnemonic | Description           | Operation             |
      +---+---+---+---+----------+-----------------------+-----------------------+
      | 0 | 0 | 0 | 0 | MVF      | Move                  | Fd := Fm              |
      | 0 | 0 | 0 | 1 | MNF      | Move negated          | Fd := - Fm            |
      | 0 | 0 | 1 | 0 | ABS      | Absolute value        | Fd := abs(Fm)         |
      | 0 | 0 | 1 | 1 | RND      | Round to integer      | Fd := int(Fm)         |
      | 0 | 1 | 0 | 0 | SQT      | Square root           | Fd := sqrt(Fm)        |
      | 0 | 1 | 0 | 1 | LOG      | Log base 10           | Fd := log10(Fm)       |
      | 0 | 1 | 1 | 0 | LGN      | Log base e            | Fd := ln(Fm)          |
      | 0 | 1 | 1 | 1 | EXP      | Exponent              | Fd := e ^ Fm          |
      | 1 | 0 | 0 | 0 | SIN      | Sine                  | Fd := sin(Fm)         |
      | 1 | 0 | 0 | 1 | COS      | Cosine                | Fd := cos(Fm)         |
      | 1 | 0 | 1 | 0 | TAN      | Tangent               | Fd := tan(Fm)         |
      | 1 | 0 | 1 | 1 | ASN      | Arc Sine              | Fd := arcsin(Fm)      |
      | 1 | 1 | 0 | 0 | ACS      | Arc Cosine            | Fd := arccos(Fm)      |
      | 1 | 1 | 0 | 1 | ATN      | Arc Tangent           | Fd := arctan(Fm)      |
      | 1 | 1 | 1 | 0 | URD      | Unnormalized round    | Fd := int(Fm)         |
      | 1 | 1 | 1 | 1 | NRM      | Normalize             | Fd := norm(Fm)        |
      +---+---+---+---+----------+-----------------------+-----------------------+
      Note: LOG, LGN, EXP, SIN, COS, TAN, ASN, ACS, ATN are deprecated, and are
            available for backwards compatibility only.
      */

      /*
      TABLE 5
      +-------------------------+---+---+
      |  Rounding Precision     | e | f |
      +-------------------------+---+---+
      | IEEE Single precision   | 0 | 0 |
      | IEEE Double precision   | 0 | 1 |
      | IEEE Extended precision | 1 | 0 |
      | undefined (trap)        | 1 | 1 |
      +-------------------------+---+---+
      */

      /*
      TABLE 5
      +---------------------------------+---+---+
      |  Rounding Mode                  | g | h |
      +---------------------------------+---+---+
      | Round to nearest (default)      | 0 | 0 |
      | Round toward plus infinity      | 0 | 1 |
      | Round toward negative infinity  | 1 | 0 |
      | Round toward zero               | 1 | 1 |
      +---------------------------------+---+---+
*)
    function taicpu.GetString:string;
      var
        i : longint;
        s : string;
        addsize : boolean;
      begin
        s:='['+gas_op2str[opcode];
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
               if (ot and OT_VREG)=OT_VREG then
                s:=s+'vreg'
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
                if (ot and OT_REGLIST)=OT_REGLIST then
                 begin
                   s:=s+'reglist';
                   addsize:=false;
                 end
               else
                if (ot and OT_IMM)=OT_IMM then
                 begin
                   s:=s+'imm';
                   addsize:=true;
                 end
               else
                if (ot and OT_MEMORY)=OT_MEMORY then
                 begin
                   s:=s+'mem';
                   addsize:=true;
                   if (ot and OT_AM2)<>0 then
                     s:=s+' am2 ';
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
                    s:=s+'24'
                  else
                   if (ot and OT_BITS32)<>0 then
                    s:=s+'32'
                  else
                   if (ot and OT_BITSSHIFTER)<>0 then
                    s:=s+'shifter'
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
        Result:=False; { unimplemented }
      end;


    function taicpu.Pass1(objdata:TObjData):longint;
      {var
        ldr2op : array[PF_B..PF_T] of tasmop = (
          A_LDRB,A_LDRSB,A_LDRBT,A_LDRH,A_LDRSH,A_LDRT);
        str2op : array[PF_B..PF_T] of tasmop = (
          A_STRB,A_None,A_STRBT,A_STRH,A_None,A_STRT);}
      begin
        Pass1:=0;
        { Save the old offset and set the new offset }
        InsOffset:=ObjData.CurrObjSec.Size;
        { Error? }
        if (Insentry=nil) and (InsSize=-1) then
          exit;
        { set the file postion }
        current_filepos:=fileinfo;

        { tranlate LDR+postfix to complete opcode }
        {if (opcode=A_LDR) and (oppostfix<>PF_None) then
          begin
            if (oppostfix in [low(ldr2op)..high(ldr2op)]) then
              opcode:=ldr2op[oppostfix]
            else
              internalerror(2005091001);
            if opcode=A_None then
              internalerror(2005091004);
            { postfix has been added to opcode }
            oppostfix:=PF_None;
          end
        else if (opcode=A_STR) and (oppostfix<>PF_None) then
          begin
            if (oppostfix in [low(str2op)..high(str2op)]) then
              opcode:=str2op[oppostfix]
            else
              internalerror(2005091002);
            if opcode=A_None then
              internalerror(2005091003);
            { postfix has been added to opcode }
            oppostfix:=PF_None;
          end;  }

        { Get InsEntry }
        if FindInsEntry(objdata) then
         begin
           InsSize:=4;
           LastInsOffset:=InsOffset;
           Pass1:=InsSize;
           exit;
         end;
        LastInsOffset:=-1;
      end;


    procedure taicpu.Pass2(objdata:TObjData);
      begin
        { error in pass1 ? }
        if insentry=nil then
         exit;
        current_filepos:=fileinfo;
        { Generate the instruction }
        GenCode(objdata);
      end;


    procedure taicpu.ppuloadoper(ppufile:tcompilerppufile;var o:toper);
      begin
      end;


    procedure taicpu.ppuwriteoper(ppufile:tcompilerppufile;const o:toper);
      begin
      end;


    procedure taicpu.ppubuildderefimploper(var o:toper);
      begin
      end;


    procedure taicpu.ppuderefoper(var o:toper);
      begin
      end;


    function  taicpu.InsEnd:longint;
      begin
        Result:=0; { unimplemented }
      end;


    procedure taicpu.create_ot(objdata:TObjData);
      var
        i,l,relsize : longint;
        dummy : byte;
        currsym : TObjSymbol;
      begin
        if ops=0 then
         exit;
        { update oper[].ot field }
        for i:=0 to ops-1 do
         with oper[i]^ do
          begin
            case typ of
              top_regset:
                begin
                  ot:=OT_REGLIST;
                end;
              top_reg :
                begin
                  case getregtype(reg) of
                    R_INTREGISTER:
                      ot:=OT_REG32 or OT_SHIFTEROP;
                    R_FPUREGISTER:
                      ot:=OT_FPUREG;
                    else
                      internalerror(2005090901);
                  end;
                end;
              top_ref :
                begin
                  if ref^.refaddr=addr_no then
                    begin
                      { create ot field }
                      { we should get the size here dependend on the
                        instruction }
                      if (ot and OT_SIZE_MASK)=0 then
                        ot:=OT_MEMORY or OT_BITS32
                      else
                        ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
                      if (ref^.base=NR_NO) and (ref^.index=NR_NO) then
                        ot:=ot or OT_MEM_OFFS;
                      { if we need to fix a reference, we do it here }

                      { pc relative addressing }
                      if (ref^.base=NR_NO) and
                        (ref^.index=NR_NO)
                        { at least we should check if the destination symbol
                          is in a text section }
                        { and
                        (ref^.symbol^.owner="text") } then
                        ref^.base:=NR_PC;

                      { determine possible address modes }
                      if (ref^.base<>NR_NO) and
                        (
                          (
                            (ref^.index=NR_NO) and
                            (ref^.offset>=-4097) and
                            (ref^.offset<=4097)
                          ) or
                          (
                            (ref^.offset=0)
                          ) or
                          (
                            (ref^.index<>NR_NO) and
                            (ref^.offset=0)
                          )
                        ) then
                        ot:=ot or OT_AM2;

                    end
                  else
                    begin
                      l:=ref^.offset;
                      currsym:=ObjData.symbolref(ref^.symbol);
                      if assigned(currsym) then
                        inc(l,currsym.address);
                      relsize:=(InsOffset+2)-l;
                      if (relsize<-33554428) or (relsize>33554428) then
                       ot:=OT_IMM32
                      else
                       ot:=OT_IMM24;
                    end;
                end;
              top_local :
                begin
                  { we should get the size here dependend on the
                    instruction }
                  if (ot and OT_SIZE_MASK)=0 then
                    ot:=OT_MEMORY or OT_BITS32
                  else
                    ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
                end;
              top_const :
                begin
                  ot:=OT_IMM;
                end;
              top_none :
                begin
                  { generated when there was an error in the
                    assembler reader. It never happends when generating
                    assembler }
                end;
              top_shifterop:
                begin
                  ot:=OT_SHIFTEROP;
                end;
              top_coh:
                begin
                  ot:=OT_COH;
                end;
              else
                internalerror(200402261);
            end;
          end;
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
        i{,j,asize,oprs} : longint;
        {siz : array[0..3] of longint;}
      begin
        Matches:=100;
        writeln(getstring,'---');

        { Check the opcode and operands }
        if (p^.opcode<>opcode) or (p^.ops<>ops) then
         begin
           Matches:=0;
           exit;
         end;

        { Check that no spurious colons or TOs are present }
        for i:=0 to p^.ops-1 do
         if (oper[i]^.ot and (not p^.optypes[i]) and (OT_COLON or OT_TO))<>0 then
          begin
            Matches:=0;
            exit;
          end;

        { Check that the operand flags all match up }
        for i:=0 to p^.ops-1 do
         begin
           if ((p^.optypes[i] and (not oper[i]^.ot)) or
               ((p^.optypes[i] and OT_SIZE_MASK) and
                ((p^.optypes[i] xor oper[i]^.ot) and OT_SIZE_MASK)))<>0 then
            begin
              if ((p^.optypes[i] and (not oper[i]^.ot) and OT_NON_SIZE) or
                  (oper[i]^.ot and OT_SIZE_MASK))<>0 then
               begin
                 Matches:=0;
                 exit;
               end
              else
               Matches:=1;
            end;
         end;
      end;


    function  taicpu.calcsize(p:PInsEntry):shortint;
      begin
        result:=4;
      end;


    function  taicpu.NeedAddrPrefix(opidx:byte):boolean;
      begin
        Result:=False; { unimplemented }
      end;


    procedure taicpu.Swapoperands;
      begin
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
           if matches(insentry)=100 then
             begin
               result:=true;
               exit;
             end;
           inc(i);
           insentry:=@instab[i];
         end;
        Message1(asmw_e_invalid_opcode_and_operands,GetString);
        { No instruction found, set insentry to nil and inssize to -1 }
        insentry:=nil;
        inssize:=-1;
      end;


    procedure taicpu.gencode(objdata:TObjData);
      var
        bytes : dword;
        i_field : byte;

      procedure setshifterop(op : byte);
        begin
          case oper[op]^.typ of
            top_const:
              begin
                i_field:=1;
                bytes:=bytes or dword(oper[op]^.val and $fff);
              end;
            top_reg:
              begin
                i_field:=0;
                bytes:=bytes or (getsupreg(oper[op]^.reg) shl 16);

                { does a real shifter op follow? }
                if (op+1<=op) and (oper[op+1]^.typ=top_shifterop) then
                  begin
                  end;
              end;
          else
            internalerror(2005091103);
          end;
        end;

      begin
        bytes:=$0;
        { evaluate and set condition code }

        { condition code allowed? }

        { setup rest of the instruction }
        case insentry^.code[0] of
          #$08:
            begin
              { set instruction code }
              bytes:=bytes or (ord(insentry^.code[1]) shl 26);
              bytes:=bytes or (ord(insentry^.code[2]) shl 21);

              { set destination }
              bytes:=bytes or (getsupreg(oper[0]^.reg) shl 12);

              { create shifter op }
              setshifterop(1);

              { set i field }
              bytes:=bytes or (i_field shl 25);
            end;
          #$ff:
            internalerror(2005091101);
          else
            internalerror(2005091102);
        end;
        { we're finished, write code }
        objdata.writebytes(bytes,sizeof(bytes));
      end;


begin
  cai_align:=tai_align;
end.

