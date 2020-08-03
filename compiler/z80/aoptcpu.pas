{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the Z80 optimizer object

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


Unit aoptcpu;

{$i fpcdefs.inc}

{$define DEBUG_AOPTCPU}

Interface

uses cpubase, cgbase, aasmtai, aopt,AoptObj, aoptcpub;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    { outputs a debug message into the assembler file }
    procedure DebugMsg(const s: string; p: tai);

    { checks whether loading a new value in reg1 overwrites the entirety of reg2 }
    function Reg1WriteOverwritesReg2Entirely(reg1, reg2: tregister): boolean;

    Function GetNextInstructionUsingReg(Current: tai; Var Next: tai;reg : TRegister): Boolean;
    function RegLoadedWithNewValue(reg : tregister; hp : tai) : boolean; override;
    function InstructionLoadsFromReg(const reg : TRegister; const hp : tai) : boolean; override;

    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;
  End;

Implementation

  uses
    cutils,
    verbose,
    cpuinfo,
    aasmbase,aasmcpu,aasmdata,
    globals,globtype,
    cgutils;

  type
    TAsmOpSet = set of TAsmOp;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function RefsEqual(const r1, r2: treference): boolean;
    begin
      refsequal :=
        (r1.offset = r2.offset) and
        (r1.base = r2.base) and
        (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
        (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
        (r1.relsymbol = r2.relsymbol);
    end;


  function MatchOperand(const oper1: TOper; const oper2: TOper): boolean; inline;
    begin
      result:=oper1.typ=oper2.typ;

      if result then
        case oper1.typ of
          top_const:
            Result:=oper1.val = oper2.val;
          top_reg:
            Result:=oper1.reg = oper2.reg;
          top_ref:
            Result:=RefsEqual(oper1.ref^, oper2.ref^);
          else Result:=false;
        end
    end;


  function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
    begin
      result := (oper.typ = top_reg) and (oper.reg = reg);
    end;


  function MatchInstruction(const instr: tai; const op: TAsmOp): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode = op);
    end;


  function MatchInstruction(const instr: tai; const ops: TAsmOpSet): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode in ops);
    end;


  function MatchInstruction(const instr: tai; const ops: TAsmOpSet;opcount : byte): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode in ops) and
        (taicpu(instr).ops=opcount);
    end;


  function MatchOpType(const instr : tai;ot0,ot1 : toptype) : Boolean;
    begin
      Result:=(taicpu(instr).ops=2) and
        (taicpu(instr).oper[0]^.typ=ot0) and
        (taicpu(instr).oper[1]^.typ=ot1);
    end;

{$ifdef DEBUG_AOPTCPU}
  procedure TCpuAsmOptimizer.DebugMsg(const s: string;p : tai);
    begin
      asml.insertbefore(tai_comment.Create(strpnew(s)), p);
    end;
{$else DEBUG_AOPTCPU}
  procedure TCpuAsmOptimizer.DebugMsg(const s: string;p : tai);inline;
    begin
    end;
{$endif DEBUG_AOPTCPU}


  function TCpuAsmOptimizer.Reg1WriteOverwritesReg2Entirely(reg1, reg2: tregister): boolean;
    begin
      case reg1 of
        NR_F:
          result:=SuperRegistersEqual(reg2,NR_DEFAULTFLAGS);
        NR_AF:
          result:=(reg2=NR_A) or (reg2=NR_AF) or SuperRegistersEqual(reg2,NR_DEFAULTFLAGS);
        NR_BC:
          result:=(reg2=NR_B) or (reg2=NR_C) or (reg2=NR_BC);
        NR_DE:
          result:=(reg2=NR_D) or (reg2=NR_E) or (reg2=NR_DE);
        NR_HL:
          result:=(reg2=NR_H) or (reg2=NR_L) or (reg2=NR_HL);
        NR_F_:
          result:=SuperRegistersEqual(reg2,NR_F_);
        NR_AF_:
          result:=(reg2=NR_A_) or (reg2=NR_AF_) or SuperRegistersEqual(reg2,NR_F_);
        NR_BC_:
          result:=(reg2=NR_B_) or (reg2=NR_C_) or (reg2=NR_BC_);
        NR_DE_:
          result:=(reg2=NR_D_) or (reg2=NR_E_) or (reg2=NR_DE_);
        NR_HL_:
          result:=(reg2=NR_H_) or (reg2=NR_L_) or (reg2=NR_HL_);
        else
          result:=reg1=reg2;
      end;
    end;


  function TCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai;
    var Next: tai; reg: TRegister): Boolean;
    begin
      Next:=Current;
      repeat
        Result:=GetNextInstruction(Next,Next);
      until not(cs_opt_level3 in current_settings.optimizerswitches) or not(Result) or (Next.typ<>ait_instruction) or (RegInInstruction(reg,Next)) or
        (is_calljmp(taicpu(Next).opcode));
    end;


  function TCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    var
      p: taicpu;
    begin
      if not assigned(hp) or
         (hp.typ <> ait_instruction) then
       begin
         Result := false;
         exit;
       end;
      p := taicpu(hp);
      if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) and (reg<>NR_AF) then
        begin
          case p.opcode of
            A_PUSH,A_POP,A_EX,A_EXX,A_NOP,A_HALT,A_DI,A_EI,A_IM,A_SET,A_RES,A_JP,A_JR,A_JRJP,A_DJNZ,A_CALL,A_RET,A_RETI,A_RETN,A_RST,A_OUT:
              result:=false;
            A_LD:
              begin
                if p.ops<>2 then
                  internalerror(2020051112);
                { LD A,I or LD A,R ? }
                if (p.oper[0]^.typ=top_reg) and (p.oper[0]^.reg=NR_A) and
                   (p.oper[1]^.typ=top_reg) and ((p.oper[1]^.reg=NR_I) or (p.oper[1]^.reg=NR_R)) then
                  result:=(reg=NR_ADDSUBTRACTFLAG) or
                          (reg=NR_PARITYOVERFLOWFLAG) or
                          (reg=NR_HALFCARRYFLAG) or
                          (reg=NR_ZEROFLAG) or
                          (reg=NR_SIGNFLAG)
                else
                  result:=false;
              end;
            A_LDI,A_LDIR,A_LDD,A_LDDR:
              result:=(reg=NR_ADDSUBTRACTFLAG) or
                      (reg=NR_PARITYOVERFLOWFLAG) or
                      (reg=NR_HALFCARRYFLAG);
            A_INC,A_DEC:
              begin
                if p.ops<>1 then
                  internalerror(2020051602);
                if (p.oper[0]^.typ=top_reg) and ((p.oper[0]^.reg=NR_BC) or
                                                 (p.oper[0]^.reg=NR_DE) or
                                                 (p.oper[0]^.reg=NR_HL) or
                                                 (p.oper[0]^.reg=NR_SP) or
                                                 (p.oper[0]^.reg=NR_IX) or
                                                 (p.oper[0]^.reg=NR_IY)) then
                  result:=false
                else
                  result:=(reg=NR_ADDSUBTRACTFLAG) or
                          (reg=NR_PARITYOVERFLOWFLAG) or
                          (reg=NR_HALFCARRYFLAG) or
                          (reg=NR_ZEROFLAG) or
                          (reg=NR_SIGNFLAG);
              end;
            A_CPI,A_CPIR,A_CPD,A_CPDR,A_RLD,A_RRD,A_BIT,A_INI,A_INIR,A_IND,A_INDR,A_OUTI,A_OTIR,A_OUTD,A_OTDR:
              result:=(reg=NR_ADDSUBTRACTFLAG) or
                      (reg=NR_PARITYOVERFLOWFLAG) or
                      (reg=NR_HALFCARRYFLAG) or
                      (reg=NR_ZEROFLAG) or
                      (reg=NR_SIGNFLAG);
            A_ADD:
              begin
                if p.ops<>2 then
                  internalerror(2020051601);
                if (p.oper[0]^.typ=top_reg) and ((p.oper[0]^.reg=NR_HL) or (p.oper[0]^.reg=NR_IX) or (p.oper[0]^.reg=NR_IY)) then
                  result:=(reg=NR_HALFCARRYFLAG) or
                          (reg=NR_ADDSUBTRACTFLAG) or
                          (reg=NR_CARRYFLAG)
                else
                  result:=true;
              end;
            A_ADC,A_SUB,A_SBC,A_AND,A_OR,A_XOR,A_CP,A_NEG,A_RLC,A_RL,A_RRC,A_RR,A_SLA,A_SRA,A_SRL:
              result:=true;
            A_DAA:
              result:=(reg=NR_PARITYOVERFLOWFLAG) or
                      (reg=NR_HALFCARRYFLAG) or
                      (reg=NR_ZEROFLAG) or
                      (reg=NR_SIGNFLAG) or
                      (reg=NR_CARRYFLAG);
            A_CPL:
              result:=(reg=NR_HALFCARRYFLAG) or
                      (reg=NR_ADDSUBTRACTFLAG);
            A_CCF,A_SCF,A_RLCA,A_RLA,A_RRCA,A_RRA:
              result:=(reg=NR_HALFCARRYFLAG) or
                      (reg=NR_ADDSUBTRACTFLAG) or
                      (reg=NR_CARRYFLAG);
            A_IN:
              begin
                if p.ops<>2 then
                  internalerror(2020051602);
                if (p.oper[1]^.typ=top_ref) and ((p.oper[1]^.ref^.base=NR_C) or (p.oper[1]^.ref^.index=NR_C)) then
                  result:=(reg=NR_ADDSUBTRACTFLAG) or
                          (reg=NR_PARITYOVERFLOWFLAG) or
                          (reg=NR_HALFCARRYFLAG) or
                          (reg=NR_ZEROFLAG) or
                          (reg=NR_SIGNFLAG)
                else
                  result:=false;
              end;
            else
              internalerror(2020051111);
          end;
        end
      else
        case p.opcode of
          A_LD:
            begin
              if p.ops<>2 then
                internalerror(2020051112);
              result:=(p.oper[0]^.typ = top_reg) and
                      (Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg)) and
                      ((p.oper[1]^.typ = top_const) or
                       ((p.oper[1]^.typ = top_reg) and not(Reg1ReadDependsOnReg2(p.oper[1]^.reg,reg))) or
                       ((p.oper[1]^.typ = top_ref) and not RegInRef(reg,p.oper[1]^.ref^)));
            end;
          A_PUSH,A_EX,A_EXX,A_LDI,A_LDIR,A_LDD,A_LDDR,A_CPI,A_CPIR,A_CPD,A_CPDR,
          A_ADD,A_ADC,A_SBC,A_CP,A_INC,A_DEC,A_DAA,A_CPL,A_NEG,A_CCF,A_SCF,
          A_NOP,A_HALT,A_DI,A_EI,A_IM,A_RLCA,A_RLA,A_RRCA,A_RRA,A_RLC,A_RL,
          A_RRC,A_RR,A_SLA,A_SRA,A_SRL,A_RLD,A_RRD,A_BIT,A_SET,A_RES,A_JP,A_JR,A_JRJP,
          A_DJNZ,A_CALL,A_RET,A_RETI,A_RETN,A_RST,A_INI,A_INIR,A_IND,A_INDR,
          A_OUT,A_OUTI,A_OTIR,A_OUTD,A_OTDR:
            result:=false;
          A_POP:
            begin
              if p.ops<>1 then
                internalerror(2020051603);
              if p.oper[0]^.typ<>top_reg then
                internalerror(2020051604);
              result:=Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg);
            end;
          A_SUB,A_XOR:
            begin
              if p.ops<>2 then
                internalerror(2020051605);
              result:=(p.oper[0]^.typ=top_reg) and (p.oper[0]^.reg=NR_A) and
                      (p.oper[1]^.typ=top_reg) and (p.oper[1]^.reg=NR_A) and
                      Reg1WriteOverwritesReg2Entirely(NR_A,reg);
            end;
          A_AND:
            begin
              if p.ops<>2 then
                internalerror(2020051606);
              result:=(p.oper[0]^.typ=top_reg) and (p.oper[0]^.reg=NR_A) and
                      (p.oper[1]^.typ=top_const) and (p.oper[1]^.val=0) and
                      Reg1WriteOverwritesReg2Entirely(NR_A,reg);
            end;
          A_OR:
            begin
              if p.ops<>2 then
                internalerror(2020051607);
              result:=(p.oper[0]^.typ=top_reg) and (p.oper[0]^.reg=NR_A) and
                      (p.oper[1]^.typ=top_const) and (byte(p.oper[1]^.val)=255) and
                      Reg1WriteOverwritesReg2Entirely(NR_A,reg);
            end;
          A_IN:
            begin
              if p.ops<>2 then
                internalerror(2020051608);
              if p.oper[0]^.typ<>top_reg then
                internalerror(2020051609);
              if p.oper[1]^.typ<>top_ref then
                internalerror(2020051610);
              result:=Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg) and
                      (((p.oper[1]^.ref^.base<>NR_C) and (p.oper[1]^.ref^.index<>NR_C)) or
                       not(Reg1ReadDependsOnReg2(NR_BC,reg)));
            end;
          else
            internalerror(2020051111);
        end;
    end;


  function TCpuAsmOptimizer.InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
    var
      p: taicpu;
    begin
      Result := false;
      if not (assigned(hp) and (hp.typ = ait_instruction)) then
        exit;
      p:=taicpu(hp);

      case p.opcode of
        A_LD,A_BIT,A_SET,A_RES:
          begin
            if p.ops<>2 then
              internalerror(2020051102);
            result:=((p.oper[0]^.typ=top_ref) and RegInRef(reg,p.oper[0]^.ref^)) or
                    RegInOp(reg,p.oper[1]^);
          end;
        A_PUSH,A_INC,A_DEC,A_RLC,A_RRC,A_SLA,A_SRA,A_SRL:
          begin
            if p.ops<>1 then
              internalerror(2020051103);
            result:=RegInOp(reg,p.oper[0]^);
          end;
        A_POP:
          result:=(reg=NR_SP);
        A_EX,A_ADD,A_SUB,A_AND,A_OR,A_XOR,A_CP:
          begin
            if p.ops<>2 then
              internalerror(2020051104);
            result:=RegInOp(reg,p.oper[0]^) or
                    RegInOp(reg,p.oper[1]^);
          end;
        A_EXX:
          result:=SuperRegistersEqual(reg,NR_BC)  or SuperRegistersEqual(reg,NR_DE)  or SuperRegistersEqual(reg,NR_HL) or
                  SuperRegistersEqual(reg,NR_BC_) or SuperRegistersEqual(reg,NR_DE_) or SuperRegistersEqual(reg,NR_HL_);
        A_LDI,A_LDIR,A_LDD,A_LDDR:
          result:=SuperRegistersEqual(reg,NR_BC) or SuperRegistersEqual(reg,NR_DE) or SuperRegistersEqual(reg,NR_HL);
        A_CPI,A_CPIR,A_CPD,A_CPDR:
          result:=SuperRegistersEqual(reg,NR_BC) or SuperRegistersEqual(reg,NR_HL) or RegistersInterfere(reg,NR_A);
        A_ADC,A_SBC:
          begin
            if p.ops<>2 then
              internalerror(2020051105);
            result:=RegInOp(reg,p.oper[0]^) or
                    RegInOp(reg,p.oper[1]^) or (reg=NR_CARRYFLAG) or (reg=NR_DEFAULTFLAGS);
          end;
        A_DAA:
          result:=RegistersInterfere(reg,NR_A) or (reg=NR_CARRYFLAG) or (reg=NR_HALFCARRYFLAG) or (reg=NR_ADDSUBTRACTFLAG) or (reg=NR_DEFAULTFLAGS);
        A_CPL,A_NEG,A_RLCA,A_RRCA:
          result:=RegistersInterfere(reg,NR_A);
        A_CCF:
          result:=(reg=NR_CARRYFLAG) or (reg=NR_DEFAULTFLAGS);
        A_SCF,A_NOP,A_HALT,A_DI,A_EI,A_IM:
          result:=false;
        A_RLA,A_RRA:
          result:=RegistersInterfere(reg,NR_A) or (reg=NR_CARRYFLAG) or (reg=NR_DEFAULTFLAGS);
        A_RL,A_RR:
          begin
            if p.ops<>1 then
              internalerror(2020051106);
            result:=RegInOp(reg,p.oper[0]^) or (reg=NR_CARRYFLAG) or (reg=NR_DEFAULTFLAGS);
          end;
        A_RLD,A_RRD:
          result:=RegistersInterfere(reg,NR_A) or RegistersInterfere(reg,NR_HL);
        A_JP,A_JR,A_JRJP:
          begin
            if p.ops<>1 then
              internalerror(2020051107);
            if RegInOp(reg,p.oper[0]^) then
              result:=true
            else
              case p.condition of
                C_None:
                  result:=false;
                C_NZ,C_Z:
                  result:=(reg=NR_ZEROFLAG) or (reg=NR_DEFAULTFLAGS);
                C_NC,C_C:
                  result:=(reg=NR_CARRYFLAG) or (reg=NR_DEFAULTFLAGS);
                C_PO,C_PE:
                  result:=(reg=NR_PARITYOVERFLOWFLAG) or (reg=NR_DEFAULTFLAGS);
                C_P,C_M:
                  result:=(reg=NR_SIGNFLAG) or (reg=NR_DEFAULTFLAGS);
              end;
          end;
        A_DJNZ:
          result:=RegistersInterfere(reg,NR_B);
        A_CALL,A_RET,A_RETI,A_RETN,A_RST:
          result:=true;
        A_IN:
          begin
            if p.ops<>2 then
              internalerror(2020051109);
            result:=(p.oper[1]^.typ=top_ref) and (p.oper[1]^.ref^.base=NR_C) and RegistersInterfere(reg,NR_BC);
          end;
        A_OUT:
          begin
            if p.ops<>2 then
              internalerror(2020051110);
            result:=RegInOp(reg,p.oper[1]^) or (p.oper[0]^.typ=top_ref) and (p.oper[0]^.ref^.base=NR_C) and RegistersInterfere(reg,NR_BC);
          end;
        A_INI,A_INIR,A_IND,A_INDR,A_OUTI,A_OTIR,A_OUTD,A_OTDR:
          result:=SuperRegistersEqual(reg,NR_BC) or SuperRegistersEqual(reg,NR_HL);
        else
          internalerror(2020051101);
      end;
    end;

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1,hp2,hp3,hp4,hp5: tai;
      alloc, dealloc: tai_regalloc;
      i: integer;
      l: TAsmLabel;
      //TmpUsedRegs : TAllUsedRegs;
    begin
      result := false;
      //case p.typ of
      //  ait_instruction:
      //    begin
      //    end;
      //end;
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    begin
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
