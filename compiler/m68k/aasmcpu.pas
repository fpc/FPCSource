{
    Copyright (c) 1998-2001 by Florian Klaempfl and Pierre Muller

    m68k family assembler instructions

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
  cclasses,aasmtai,aasmdata,aasmsym,
  aasmbase,globals,verbose,symtype,
  cpubase,cpuinfo,cgbase,cgutils,
  ogbase;


const
  { "mov reg,reg" source operand number }
  O_MOV_SOURCE = 0;
  { "mov reg,reg" source operand number }
  O_MOV_DEST = 1;

  instabentries = {$i m68knop.inc}

type
  TOperandType = (
    OT_DATA,
    OT_ADDR,
    OT_ADDR_INDIR,
    OT_ADDR_INDIR_POSTINC,
    OT_ADDR_INDIR_PREDEC,
    OT_ADDR_DISP16,
    OT_ADDR_IDX_DISP8,
    OT_ABS_SHORT,
    OT_ABS_LONG,
    OT_PC_DISP16,
    OT_PC_IDX_DISP8,
    OT_IMMEDIATE,
    OT_REG_LIST,
    OT_FPUREG_LIST,
    OT_FPUREG,
    OT_SPECIALREG
  );

  TOperandFlags = (
    OF_IMM_QUICK,
    OF_IMM_FLOAT,
    OF_IMM_64BIT,
    OF_SPECREG,
    OF_SPECREG_CCR,
    OF_SPECREG_SR,
    OF_SPECREG_USP,
    OF_SPECREG_FPIAR,
    OF_SPECREG_FPU,
    OF_BITFIELD,
    OF_BRANCH,
    OF_DOUBLE_REG,
    OF_KFACTOR,
    OF_NOSIZE
  );

  TOpSizeFlag = (
    OPS_UNSIZED,
    OPS_SHORT,
    OPS_BYTE,
    OPS_WORD,
    OPS_LONG,
    OPS_QUAD,
    OPS_SINGLE,
    OPS_DOUBLE,
    OPS_EXTENDED,
    OPS_PACKED,
    OPS_COLDFIRE
  );

  TOpSupported = (
    OS_M68000,
    OS_M68000UP,
    OS_M68010UP,
    OS_M68020,
    OS_M68020UP,
    OS_M68030,
    OS_M68040,
    OS_M68040UP,
    OS_M68060,
    OS_M68881,
    OS_M68851,
    OS_CPU32,
    OS_CF,
    OS_CF_ISA_A,
    OS_CF_ISA_APL,
    OS_CF_ISA_B,
    OS_CF_ISA_C,
    OS_CF_HWDIV,
    OS_CF_FPU,
    OS_CF_USP,
    OS_GNU_AS
  );

const
  AM_Dn          = 0;
  AM_An          = 1;
  AM_An_Indir    = 2;
  AM_An_PostInc  = 3;
  AM_An_PreDec   = 4;
  AM_An_Disp16   = 5;
  AM_An_Format8  = 6;
  AM_Extended    = 7;
  AM_FPn         = 8;
  AM_SpecReg     = 9;

  REG_AbsShort   = 0;
  REG_AbsLong    = 1;
  REG_PC_Disp16  = 2;
  REG_PC_Format8 = 3;
  REG_Immediate  = 4;
  REG_RegList    = 5;
  REG_FPURegList = 6;

type
  toperandtypeset = set of toperandtype;
  toperandflagset = set of toperandflags;
  topsupportedset = set of topsupported;
  topsizeflagset  = set of topsizeflag;

type
  tinsentry = record
    opcode   : tasmop;
    ops      : byte;
    optypes  : array[0..max_operands-1] of toperandtypeset;
    opflags  : array[0..max_operands-1] of toperandflagset;
    codelen  : byte;
    code     : array[0..1] of word;
    support  : topsupportedset;
    sizes    : topsizeflagset;
  end;
  pinsentry = ^tinsentry;


type
  TInsTabCache=array[TasmOp] of longint;
  PInsTabCache=^TInsTabCache;

var
  InsTabCache: PInsTabCache;

const
  InsTab:array[0..instabentries-1] of TInsEntry = {$i m68ktab.inc}

type

  taicpu = class(tai_cpu_abstract_sym)
    private
      { next fields are filled in pass1, so pass2 is faster }
      insentry  : PInsEntry;
      inssize   : shortint;
      insoffset : longint;
      LastInsOffset : longint;

      function CalcSize(p: PInsEntry):shortint;
      function Matches(p: PInsEntry; objdata: TObjData):boolean;
      function FindInsEntry(objdata: TObjData):boolean;
      procedure GenCode(objdata: TObjData);

      procedure init(_size : topsize); { this need to be called by all constructor }
    public
      opsize : topsize;

      procedure loadregset(opidx:longint; const dataregs,addrregs,fpuregs:tcpuregisterset);
      procedure loadregpair(opidx:longint; const _reghi,_reglo: tregister);
      procedure loadrealconst(opidx:longint; const value_real: bestreal);

      constructor op_none(op : tasmop);
      constructor op_none(op : tasmop;_size : topsize);

      constructor op_reg(op : tasmop;_size : topsize;_op1 : tregister);
      constructor op_const(op : tasmop;_size : topsize;_op1 : longint);
      constructor op_ref(op : tasmop;_size : topsize;_op1 : treference);

      constructor op_reg_reg(op : tasmop;_size : topsize;_op1,_op2 : tregister);
      constructor op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : treference);
      constructor op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);

      constructor op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
      constructor op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
      constructor op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : treference);
      constructor op_realconst_reg(op : tasmop;_size : topsize;_op1: bestreal;_op2: tregister);

      constructor op_ref_reg(op : tasmop;_size : topsize;_op1 : treference;_op2 : tregister);
      { this is only allowed if _op1 is an int value (_op1^.isintvalue=true) }
      constructor op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : treference);

      { this is used for mulx/divx/remx regpair generation }
      constructor op_reg_reg_reg(op : tasmop;_size : topsize;_op1,_op2,_op3 : tregister);
      constructor op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint; _op2,_op3 : tregister);
      constructor op_ref_reg_reg(op : tasmop;_size : topsize;_op1 : treference; _op2,_op3 : tregister);

      constructor op_reg_regset(op: tasmop; _size : topsize; _op1: tregister;const _op2data,_op2addr,_op2fpu: tcpuregisterset);
      constructor op_regset_reg(op: tasmop; _size : topsize;const _op1data,_op1addr,_op1fpu: tcpuregisterset; _op2: tregister);

      constructor op_ref_regset(op: tasmop; _size : topsize; _op1: treference;const _op2data,_op2addr,_op2fpu: tcpuregisterset);
      constructor op_regset_ref(op: tasmop; _size : topsize;const _op1data,_op1addr,_op1fpu: tcpuregisterset; _op2: treference);

      { this is for Jmp instructions }
      constructor op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);

      constructor op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
      { for DBxx opcodes }
      constructor op_reg_sym(op: tasmop; _size : topsize; _op1: tregister; _op2 :tasmsymbol);
      constructor op_sym_ofs_reg(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;_op2 : tregister);

      constructor op_sym_ofs(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint);
      constructor op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

      function is_same_reg_move(regtype: Tregistertype):boolean;override;
      function spilling_get_operation_type(opnr: longint): topertype;override;
      function spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;override;

      procedure ResetPass1;override;
      procedure ResetPass2;override;
      function  Pass1(objdata:TObjData):longint;override;
      procedure Pass2(objdata:TObjData);override;

  end;


  tai_align = class(tai_align_abstract)
        { nothing to add }
  end;

  procedure InitAsm;
  procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

  implementation

    uses
      globtype, itcpugas;


{*****************************************************************************
                                 Taicpu Constructors
*****************************************************************************}



    procedure taicpu.loadregset(opidx:longint; const dataregs,addrregs,fpuregs:tcpuregisterset);
      var
        i : byte;
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_regset then
             clearop(opidx);
           dataregset:=dataregs;
           addrregset:=addrregs;
           fpuregset:=fpuregs;
           typ:=top_regset;
           for i:=RS_D0 to RS_D7 do
             begin
               if assigned(add_reg_instruction_hook) and (i in dataregset) then
                 add_reg_instruction_hook(self,newreg(R_INTREGISTER,i,R_SUBWHOLE));
             end;
           for i:=RS_A0 to RS_SP do
             begin
               if assigned(add_reg_instruction_hook) and (i in addrregset) then
                 add_reg_instruction_hook(self,newreg(R_ADDRESSREGISTER,i,R_SUBWHOLE));
             end;
           for i:=RS_FP0 to RS_FP7 do
             begin
               if assigned(add_reg_instruction_hook) and (i in fpuregset) then
                 add_reg_instruction_hook(self,newreg(R_FPUREGISTER,i,R_SUBWHOLE));
             end;
         end;
      end;

    procedure taicpu.loadregpair(opidx:longint; const _reghi,_reglo: tregister);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_regpair then
              clearop(opidx);
            typ:=top_regpair;
            reghi:=_reghi;
            reglo:=_reglo;
          end;
      end;

    procedure taicpu.loadrealconst(opidx:longint; const value_real: bestreal);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_realconst then
              clearop(opidx);
            val_real:=value_real;
            typ:=top_realconst;
          end;
      end;


    procedure taicpu.init(_size : topsize);
      begin
         typ:=ait_instruction;
         is_jmp:=false;
         opsize:=_size;
         ops:=0;
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


    constructor taicpu.op_const(op : tasmop;_size : topsize;_op1 : longint);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadconst(0,aword(_op1));
      end;


    constructor taicpu.op_ref(op : tasmop;_size : topsize;_op1 : treference);
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


    constructor taicpu.op_reg_const(op:tasmop; _size: topsize; _op1: tregister; _op2: longint);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aword(_op2));
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_size : topsize;_op1 : tregister;_op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_const_reg(op : tasmop;_size : topsize;_op1 : longint;_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
      end;


    constructor taicpu.op_const_const(op : tasmop;_size : topsize;_op1,_op2 : longint);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,aword(_op1));
         loadconst(1,aword(_op2));
      end;


    constructor taicpu.op_const_ref(op : tasmop;_size : topsize;_op1 : longint;_op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadconst(0,aword(_op1));
         loadref(1,_op2);
      end;

    constructor taicpu.op_realconst_reg(op : tasmop;_size : topsize;_op1 : bestreal;_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadrealconst(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_ref_reg(op : tasmop;_size : topsize;_op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_ref_ref(op : tasmop;_size : topsize;_op1,_op2 : treference);
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

    constructor taicpu.op_const_reg_reg(op : tasmop;_size : topsize;_op1 : longint; _op2,_op3 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadconst(0,aword(_op1));
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

    constructor taicpu.op_ref_reg_reg(op : tasmop;_size : topsize;_op1 : treference; _op2,_op3 : tregister);
      begin
         inherited create(op);
         init(_size);
         ops:=3;
         loadref(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;

   constructor taicpu.op_ref_regset(op: tasmop; _size : topsize; _op1: treference;const _op2data,_op2addr,_op2fpu: tcpuregisterset);
     Begin
        inherited create(op);
        init(_size);
        ops:=2;
        loadref(0,_op1);
        loadregset(1,_op2data,_op2addr,_op2fpu);
     end;


   constructor taicpu.op_regset_ref(op: tasmop; _size : topsize;const _op1data,_op1addr,_op1fpu: tcpuregisterset; _op2: treference);
     Begin
        inherited create(op);
        init(_size);
        ops:=2;
        loadregset(0,_op1data,_op1addr,_op1fpu);
        loadref(1,_op2);
     End;



   constructor taicpu.op_reg_regset(op: tasmop; _size : topsize; _op1: tregister;const _op2data,_op2addr,_op2fpu: tcpuregisterset);
     Begin
        inherited create(op);
        init(_size);
        ops:=2;
        loadreg(0,_op1);
        loadregset(1,_op2data,_op2addr,_op2fpu);
     end;


   constructor taicpu.op_regset_reg(op: tasmop; _size : topsize;const _op1data,_op1addr,_op1fpu: tcpuregisterset; _op2: tregister);
     Begin
        inherited create(op);
        init(_size);
        ops:=2;
        loadregset(0,_op1data,_op1addr,_op1fpu);
        loadreg(1,_op2);
     End;


    constructor taicpu.op_sym(op : tasmop;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


     constructor taicpu.op_reg_sym(op: tasmop; _size : topsize; _op1: tregister; _op2 :tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,0);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_size : topsize;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         init(_size);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
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
         if ((op >= A_DBCC) and (op <= A_DBF))
          or ((op >= A_FDBEQ) and (op <= A_FDBNGLE)) then
           begin
             loadreg(0,_op2);
             loadsymbol(1,_op1,_op1ofs);
           end
          else
           begin
             loadsymbol(0,_op1,_op1ofs);
             loadreg(1,_op2);
           end;
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_size : topsize;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init(_size);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=(((opcode=A_MOVE) or (opcode=A_EXG)) and
                 (regtype = R_INTREGISTER) and
                 (ops=2) and
                 (oper[0]^.typ=top_reg) and
                 (oper[1]^.typ=top_reg) and
                 (isregoverlap(oper[0]^.reg,oper[1]^.reg))
                ) or
                (((opcode=A_MOVE) or (opcode=A_EXG) or (opcode=A_MOVEA)) and
                 (regtype = R_ADDRESSREGISTER) and
                 (ops=2) and
                 (oper[0]^.typ=top_reg) and
                 (oper[1]^.typ=top_reg) and
                 (isregoverlap(oper[0]^.reg,oper[1]^.reg))
                ) or
                ((opcode=A_FMOVE) and
                 (regtype = R_FPUREGISTER) and
                 (ops=2) and
                 (oper[0]^.typ=top_reg) and
                 (oper[1]^.typ=top_reg) and
                 (oper[0]^.reg=oper[1]^.reg)
                );
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        result:=operand_read;

        case opcode of
          // CPU opcodes
          A_MOVE, A_MOVEQ, A_MOVEA, A_MVZ, A_MVS, A_MOV3Q, A_LEA:
            if opnr=1 then
              result:=operand_write;
          A_ADD, A_ADDQ, A_ADDX, A_SUB, A_SUBQ, A_SUBX,
          A_AND, A_LSR, A_LSL, A_ASR, A_ASL, A_EOR, A_EORI, A_OR,
          A_ROL, A_ROR, A_ROXL, A_ROXR,
          A_BSET, A_BCLR:
            if opnr=1 then
              result:=operand_readwrite;
          A_MULS, A_MULU, A_DIVS, A_DIVU, A_DIVSL, A_DIVUL, A_REMS, A_REMU:
            { FIXME: actually, one of the operand of the 3 op DIV/MUL is write only,
                     but we can't handle it easily... }
            if opnr>0 then
              result:=operand_readwrite;
          A_DBRA:
            if opnr=0 then
              result:=operand_readwrite;
          A_CLR, A_SXX, A_SEQ, A_SNE, A_SLT, A_SLE, A_SGT, A_SGE, A_SCS, A_SCC,
          A_SMI, A_SPL, A_SF, A_ST, A_SVS, A_SVC, A_SHI, A_SLS:
            result:=operand_write;
          A_NEG, A_NEGX, A_EXT, A_EXTB, A_NOT, A_SWAP:
            result:=operand_readwrite;
          A_TST, A_CMP, A_CMPI, A_BTST:
            begin end; { Do nothing, default operand_read is fine here. }

          // FPU opcodes
          A_FSXX, A_FSEQ, A_FSNE, A_FSLT, A_FSLE, A_FSGT, A_FSGE:
             result:=operand_write;
          A_FABS, A_FSABS, A_FDABS,
          A_FSQRT, A_FSSQRT, A_FDSQRT,
          A_FNEG, A_FSNEG, A_FDNEG,
          A_FSIN, A_FCOS,
          A_FINT, A_FINTRZ:
             if ops = 1 then
               begin
                 if opnr = 0 then
                   result:=operand_readwrite;
               end
             else
               if opnr = 1 then
                 result:=operand_write;
          A_FMOVE, A_FSMOVE, A_FDMOVE:
             if opnr=1 then
               result:=operand_write;
          A_FADD, A_FSADD, A_FDADD,
          A_FSUB, A_FSSUB, A_FDSUB,
          A_FMUL, A_FSMUL, A_FDMUL, A_FSGLMUL,
          A_FDIV, A_FSDIV, A_FDDIV, A_FSGLDIV:
             if opnr=1 then
               result:=operand_readwrite;
          A_FCMP, A_FTST:
             begin end; { operand_read }

          else begin
            internalerror(2004040903);
          end;
        end;
      end;

    function taicpu.spilling_get_operation_type_ref(opnr: longint; reg: tregister): topertype;
      begin
        result := operand_read;
        if (oper[opnr]^.ref^.base = reg) and
          (oper[opnr]^.ref^.direction <> dir_none) then
           result := operand_readwrite;
      end;


    function taicpu.CalcSize(p: PInsEntry): shortint;
      begin
        result:=p^.codelen * 2;
      end;

    function taicpu.Matches(p: PInsEntry; objdata:TObjData): boolean;

      function TargetMatch: boolean;
        const
          CPUTypeToOpSupported: array[TCPUtype] of topsupportedset = (
              {* cpu_none *}     [],
              {* cpu_MC68000 *}  [OS_M68000,OS_M68000UP],
              {* cpu_MC68020 *}  [OS_M68020,OS_M68000UP,OS_M68010UP,OS_M68020UP,OS_M68851],
              {* cpu_MC68040 *}  [OS_M68040,OS_M68000UP,OS_M68010UP,OS_M68020UP,OS_M68040UP],
              {* cpu_MC68060 *}  [OS_M68060,OS_M68000UP,OS_M68010UP,OS_M68020UP,OS_M68040UP],
              {* cpu_isa_a *}    [OS_CF,OS_CF_ISA_A],
              {* cpu_isa_a_p *}  [OS_CF,OS_CF_ISA_APL],
              {* cpu_isa_b *}    [OS_CF,OS_CF_ISA_B],
              {* cpu_isa_c *}    [OS_CF,OS_CF_ISA_C],
              {* cpu_cfv4e *}    [OS_CF,OS_CF_ISA_B]
          );
          FPUTypeToOpSupported: array[TFPUtype] of topsupportedset = (
              {* fpu_none *}     [],
              {* fpu_soft *}     [],
              {* fpu_libgcc *}   [],
              {* fpu_68881 *}    [OS_M68881],
              {* fpu_68040 *}    [OS_M68881,OS_M68040,OS_M68040UP],
              {* fpu_68060 *}    [OS_M68881,OS_M68040,OS_M68040UP,OS_M68060],
              {* fpu_coldfire *} [OS_CF_FPU]
          );
        begin
          result:=((CPUTypeToOpSupported[current_settings.cputype] * p^.support) <> []) or
                  ((FPUTypeToOpSupported[current_settings.fputype] * p^.support) <> []);
        end;

      function OpsizeMatch: boolean;
        const
          TOpSizeToOpSizeFlag: array[TOpSize] of TOpSizeFlagSet = (
              { S_NO } [ OPS_UNSIZED],
              { S_B  } [ OPS_SHORT, OPS_BYTE ],
              { S_W  } [ OPS_WORD ],
              { S_L  } [ OPS_LONG ],
              { S_FS } [ OPS_SINGLE ],
              { S_FD } [ OPS_DOUBLE ],
              { S_FX } [ OPS_EXTENDED ]
          );
        begin
          result:=(TOpSizeToOpSizeFlag[opsize] * p^.sizes) <> [];

          { Special handling for instructions where the size can be
            implicitly determined, because only one size is possible. }
          if not result and (opsize in [S_NO]) then
            begin
              result:=(p^.sizes <> []) and (
                 { if OPS_SHORT is in sizes, it means we have a branch
                   instruction, so let unsized pass. }
                 (OPS_SHORT in p^.sizes) or
                 { Or only one size is possible. }
                 ((p^.sizes - [ OPS_BYTE ]) = []) or
                 ((p^.sizes - [ OPS_WORD ]) = []) or
                 ((p^.sizes - [ OPS_LONG ]) = []));
            end;
        end;

      function OperandsMatch(const oper: toper; const ots: toperandtypeset): boolean;
        var
          ot: toperandtype;
        begin
          // fix me: this function could use some improvements, in particular checking
          // agains for example CF or 68000 limitations, etc
          result:=false;

          for ot in ots do
            begin
              case ot of
                OT_DATA:
                  result:=(oper.typ=top_reg) and isintregister(oper.reg);
                OT_ADDR:
                  result:=(oper.typ=top_reg) and isaddressregister(oper.reg);
                OT_ADDR_INDIR:
                  result:=(oper.typ=top_ref) and isaddressregister(oper.ref^.base)
                    and (oper.ref^.direction=dir_none) and (oper.ref^.index=NR_NO)
                    and (oper.ref^.offset=0) and (oper.ref^.symbol=nil);
                OT_ADDR_INDIR_POSTINC:
                  result:=(oper.typ=top_ref) and isaddressregister(oper.ref^.base)
                    and (oper.ref^.direction=dir_inc) and (oper.ref^.index=NR_NO)
                    and (oper.ref^.offset=0) and (oper.ref^.symbol=nil);
                OT_ADDR_INDIR_PREDEC:
                  result:=(oper.typ=top_ref) and isaddressregister(oper.ref^.base)
                    and (oper.ref^.direction=dir_dec) and (oper.ref^.index=NR_NO)
                    and (oper.ref^.offset=0) and (oper.ref^.symbol=nil);
                OT_ADDR_DISP16:
                  // fix me: also needs checking offset sizes, incl. 020+ base displacements!
                  result:=(oper.typ=top_ref) and isaddressregister(oper.ref^.base)
                    and (oper.ref^.direction=dir_none) and (oper.ref^.index=NR_NO)
                    and (oper.ref^.symbol=nil);
                OT_ADDR_IDX_DISP8:
                  // fix me: also needs checking offset sizes, incl. 020+ base displacements!
                  result:=(oper.typ=top_ref) and isaddressregister(oper.ref^.base)
                    and (isaddressregister(oper.ref^.index) or isintregister(oper.ref^.index))
                    and (oper.ref^.direction=dir_none)
                    and (oper.ref^.symbol=nil);
                OT_ABS_SHORT,
                  // fix me: also needs checking sizes!
                OT_ABS_LONG:
                  result:=((oper.typ=top_ref) and assigned(oper.ref^.symbol)
                    and (oper.ref^.base=NR_NO) and (oper.ref^.index=NR_NO)
                    and (oper.ref^.direction=dir_none)) or
                    (oper.typ=top_const);
                OT_PC_DISP16:
                  // fix me: also needs checking offset sizes, incl. 020+ base displacements!
                  result:=(oper.typ=top_ref) and (oper.ref^.base=NR_PC)
                    and (oper.ref^.direction=dir_none) and (oper.ref^.index=NR_NO)
                    and (oper.ref^.symbol=nil);
                OT_PC_IDX_DISP8:
                  // fix me: also needs checking offset sizes, incl. 020+ base displacements!
                  result:=(oper.typ=top_ref) and (oper.ref^.base=NR_PC)
                    and (isaddressregister(oper.ref^.index) or isintregister(oper.ref^.index))
                    and (oper.ref^.direction=dir_none)
                    and (oper.ref^.symbol=nil);
                OT_IMMEDIATE:
                  // fix me: needs checking against OF_IMM_QUICK and others
                  result:=(oper.typ=top_const);
                OT_REG_LIST:
                  result:=(oper.typ=top_regset) and (oper.fpuregset=[]) and
                    ((oper.dataregset<>[]) or (oper.addrregset<>[]));
                OT_FPUREG_LIST:
                  result:=(oper.typ=top_regset) and (oper.fpuregset<>[]) and
                    ((oper.dataregset=[]) or (oper.addrregset=[]));
                OT_FPUREG:
                  result:=(oper.typ=top_reg) and isfpuregister(oper.reg);
                {OT_SPECIALREG}
              else
                internalerror(2023010101);
            end;
            if result then
              break;
          end;
        end;

      var
        i: Integer;
      begin
        result:=false;

        { Check the opcode and number of operands }
        if (p^.opcode<>opcode) or (p^.ops<>ops) then
          exit;

        { Check if opcode is valid for this target }
        if not TargetMatch then
          exit;

        { Check if opcode size is valid }
        if not OpsizeMatch then
          exit;

        { Check the operands }
        for i:=0 to p^.ops-1 do
          if not OperandsMatch(oper[i]^,p^.optypes[i]) then
            exit;

        result:=true;
      end;

    function taicpu.FindInsEntry(objdata: TObjData): boolean;
      var
        i : longint;
      begin
        result:=false;
        { Things which may only be done once, not when a second pass is done to
          optimize }

        if (InsEntry=nil) then
          begin
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
        i:=InsTabCache^[opcode];
        if i=-1 then
          begin
            Message1(asmw_e_opcode_not_in_table,gas_op2str[opcode]);
            exit;
          end;
        InsEntry:=@instab[i];
        while (InsEntry^.opcode=opcode) do
          begin
            if Matches(insentry,objdata) then
              begin
                result:=true;
                exit;
              end;
            inc(insentry);
          end;
        Message1(asmw_e_invalid_opcode_and_operands,gas_op2str[opcode]{,GetString});
        { No instruction found, set insentry to nil and inssize to -1 }
        InsEntry:=nil;
        InsSize:=-1;
      end;

    procedure taicpu.GenCode(objdata: TObjData);

      procedure WriteWord(w: word);
        var
          bytes: array [0..1] of Byte;
        begin
          Word(bytes):=NToBE(w);
          objdata.writebytes(bytes,2);
        end;

      procedure OpcodeSetReg(opcode: word; regnum: byte);
        begin
          opcode:=(opcode and $fff8) or (regnum and $7);
        end;

      procedure OpcodeSetMode(opcode: word; mode: byte);
        begin
          opcode:=(opcode and $ffc7) or ((mode and $7) shl 3);
        end;

      procedure OpcodeSetEA(opcode: word; mode: byte; regnum: byte);
        begin
          opcode:=(opcode and $ffc0) or ((mode and $7) shl 3) or (regnum and $7);
        end;


      var
        i: longint;
      begin
//        writeln('GenCode: ',insentry^.opcode);
        for i:=0 to insentry^.codelen do
          WriteWord(insentry^.code[i]);
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
        if assigned(InsEntry) then
          begin
            InsEntry:=nil;
            InsSize:=0;
          end;
        LastInsOffset:=-1;
      end;


   function taicpu.Pass1(objdata:TObjData):longint;
      begin
        Pass1:=0;
        { Save the old offset and set the new offset }
        InsOffset:=ObjData.CurrObjSec.Size;
        { Error? }
        if (InsEntry=nil) and (InsSize=-1) then
          exit;
        { set the file postion }
        current_filepos:=fileinfo;
        { Get InsEntry }
        if FindInsEntry(ObjData) then
          begin
            { Calculate instruction size }
            InsSize:=CalcSize(InsEntry);
            LastInsOffset:=InsOffset;
            Pass1:=InsSize;
            exit;
          end;
        LastInsOffset:=-1;
      end;

    procedure taicpu.Pass2(objdata: TObjData);
      begin
        { error in pass1 ? }
        if InsEntry=nil then
          exit;
        current_filepos:=fileinfo;

        { Generate the instruction }
        GenCode(ObjData);
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_ref_reg(A_MOVE,S_L,ref,r);
          R_ADDRESSREGISTER :
            result:=taicpu.op_ref_reg(A_MOVE,S_L,ref,r);
          R_FPUREGISTER :
            result:=taicpu.op_ref_reg(A_FMOVE,fpuregopsize,ref,r);
          else
            internalerror(200602011);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_MOVE,S_L,r,ref);
          R_ADDRESSREGISTER :
            result:=taicpu.op_reg_ref(A_MOVE,S_L,r,ref);
          R_FPUREGISTER :
            result:=taicpu.op_reg_ref(A_FMOVE,fpuregopsize,r,ref);
          else
            internalerror(200602012);
        end;
      end;


{****************************************************************************
                                Instruction table
*****************************************************************************}

    procedure BuildInsTabCache;
      var
        i : longint;
      begin
        new(InsTabCache);
        FillChar(InsTabCache^,sizeof(TInsTabCache),$ff);
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
        if not assigned(InsTabCache) then
          BuildInsTabCache;
      end;


    procedure DoneAsm;
      begin
        if assigned(InsTabCache) then
          begin
            dispose(InsTabCache);
            InsTabCache:=nil;
          end;
      end;

begin
  cai_align:=tai_align;
  cai_cpu:=taicpu;
end.
