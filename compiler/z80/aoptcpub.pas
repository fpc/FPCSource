 {
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains several types and constants necessary for the
    optimizer to work on the Z80 architecture

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
Unit aoptcpub; { Assembler OPTimizer CPU specific Base }

{$i fpcdefs.inc}

{ enable the following define if memory references can have a scaled index }
{ define RefsHaveScale}

{ enable the following define if memory references can have a segment }
{ override                                                            }

{ define RefsHaveSegment}

Interface

Uses
  cpubase,
  cgbase,
  aasmcpu,aasmtai,
  AOptBase;

Type

{ type of a normal instruction }
  TInstr = Taicpu;
  PInstr = ^TInstr;

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
{ Info about the conditional registers                                      }
  TCondRegs = Object
    Constructor Init;
    Destructor Done;
  End;

{ ************************************************************************* }
{ **************************** TAoptBaseCpu ******************************* }
{ ************************************************************************* }

  TAoptBaseCpu = class(TAoptBase)
    { checks whether reading the value in reg1 depends on the value of reg2. This
      is very similar to SuperRegisterEquals, except it takes into account that
      R_SUBH and R_SUBL are independendent (e.g. reading from AL does not
      depend on the value in AH). }
    function Reg1ReadDependsOnReg2(reg1, reg2: tregister): boolean;
    function RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean; override;
  End;


{ ************************************************************************* }
{ ******************************* Constants ******************************* }
{ ************************************************************************* }
Const

{ the maximum number of things (registers, memory, ...) a single instruction }
{ changes                                                                    }

  MaxCh = 2;

{ the maximum number of operands an instruction has }

  MaxOps = 2;

{Oper index of operand that contains the source (reference) with a load }
{instruction                                                            }

  LoadSrc = 1;

{Oper index of operand that contains the destination (register) with a load }
{instruction                                                                }

  LoadDst = 0;

{Oper index of operand that contains the source (register) with a store }
{instruction                                                            }

  StoreSrc = 1;

{Oper index of operand that contains the destination (reference) with a load }
{instruction                                                                 }

  StoreDst = 0;

  aopt_uncondjmp = [A_JP,A_JR];
  aopt_condjmp = [A_JP,A_JR];

Implementation

Uses
  verbose;

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
  Constructor TCondRegs.init;
    Begin
    End;

  Destructor TCondRegs.Done; {$ifdef inl} inline; {$endif inl}
    Begin
    End;


  function TAoptBaseCpu.Reg1ReadDependsOnReg2(reg1, reg2: tregister): boolean;
    begin
      case reg1 of
        NR_AF:
          result:=(reg2=NR_A) or (reg2=NR_AF) or SuperRegistersEqual(reg2,NR_DEFAULTFLAGS);
        NR_A:
          result:=(reg2=NR_A) or (reg2=NR_AF);
        NR_F:
          result:=SuperRegistersEqual(reg2,NR_DEFAULTFLAGS);
        NR_BC:
          result:=(reg2=NR_B) or (reg2=NR_C) or (reg2=NR_BC);
        NR_B:
          result:=(reg2=NR_B) or (reg2=NR_BC);
        NR_C:
          result:=(reg2=NR_C) or (reg2=NR_BC);
        NR_DE:
          result:=(reg2=NR_D) or (reg2=NR_E) or (reg2=NR_DE);
        NR_D:
          result:=(reg2=NR_D) or (reg2=NR_DE);
        NR_E:
          result:=(reg2=NR_E) or (reg2=NR_DE);
        NR_HL:
          result:=(reg2=NR_H) or (reg2=NR_L) or (reg2=NR_HL);
        NR_H:
          result:=(reg2=NR_H) or (reg2=NR_HL);
        NR_L:
          result:=(reg2=NR_L) or (reg2=NR_HL);
        NR_AF_:
          result:=(reg2=NR_A_) or (reg2=NR_AF_) or SuperRegistersEqual(reg2,NR_F_);
        NR_A_:
          result:=(reg2=NR_A_) or (reg2=NR_AF_);
        NR_F_:
          result:=SuperRegistersEqual(reg2,NR_F_);
        NR_BC_:
          result:=(reg2=NR_B_) or (reg2=NR_C_) or (reg2=NR_BC_);
        NR_B_:
          result:=(reg2=NR_B_) or (reg2=NR_BC_);
        NR_C_:
          result:=(reg2=NR_C_) or (reg2=NR_BC_);
        NR_DE_:
          result:=(reg2=NR_D_) or (reg2=NR_E_) or (reg2=NR_DE_);
        NR_D_:
          result:=(reg2=NR_D_) or (reg2=NR_DE_);
        NR_E_:
          result:=(reg2=NR_E_) or (reg2=NR_DE_);
        NR_HL_:
          result:=(reg2=NR_H_) or (reg2=NR_L_) or (reg2=NR_HL_);
        NR_H_:
          result:=(reg2=NR_H_) or (reg2=NR_HL_);
        NR_L_:
          result:=(reg2=NR_L_) or (reg2=NR_HL_);
        else
          result:=reg1=reg2;
      end;
    end;


  function TAoptBaseCpu.RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean;
    var
      i : Longint;
      p: taicpu;
    begin
      p:=taicpu(p1);
      result:=false;
      for i:=0 to p.ops-1 do
        if (p.oper[i]^.typ=top_reg) and Reg1ReadDependsOnReg2(Reg,p.oper[i]^.reg) and (p.spilling_get_operation_type(i) in [operand_write,operand_readwrite]) then
          begin
            result:=true;
            exit;
          end;
      case p.opcode of
        A_LD,A_EX,A_ADD,A_ADC,A_SUB,A_SBC,A_AND,A_OR,A_XOR,A_CP,A_INC,A_DEC,
        A_CCF,A_SCF,A_NOP,A_HALT,A_DI,A_EI,A_IM,A_RLC,A_RL,A_RRC,A_RR,A_SLA,
        A_SRA,A_SRL,A_BIT,A_SET,A_RES,A_JP,A_JR,A_CALL,A_RET,A_RETI,A_RETN,
        A_RST,A_IN,A_OUT:
          ;
        A_PUSH,A_POP:
          result:=Reg1ReadDependsOnReg2(Reg,NR_SP);
        A_EXX:
          result:=Reg1ReadDependsOnReg2(Reg,NR_BC) or Reg1ReadDependsOnReg2(Reg,NR_BC_) or
                  Reg1ReadDependsOnReg2(Reg,NR_DE) or Reg1ReadDependsOnReg2(Reg,NR_DE_) or
                  Reg1ReadDependsOnReg2(Reg,NR_HL) or Reg1ReadDependsOnReg2(Reg,NR_HL_);
        A_LDI,A_LDIR,A_LDD,A_LDDR:
          result:=Reg1ReadDependsOnReg2(Reg,NR_BC) or
                  Reg1ReadDependsOnReg2(Reg,NR_DE) or
                  Reg1ReadDependsOnReg2(Reg,NR_HL);
        A_CPI,A_CPIR,A_CPD,A_CPDR:
          result:=Reg1ReadDependsOnReg2(Reg,NR_BC) or
                  Reg1ReadDependsOnReg2(Reg,NR_HL);
        A_DAA,A_CPL,A_NEG,A_RLCA,A_RLA,A_RRCA,A_RRA,A_RLD,A_RRD:
          result:=Reg1ReadDependsOnReg2(Reg,NR_A);
        A_DJNZ:
          result:=Reg1ReadDependsOnReg2(Reg,NR_B);
        A_INI,A_INIR,A_IND,A_INDR,A_OUTI,A_OTIR,A_OUTD,A_OTDR:
          result:=Reg1ReadDependsOnReg2(Reg,NR_B) or
                  Reg1ReadDependsOnReg2(Reg,NR_HL);
        else
          internalerror(2020052001);
      end;
      if not result and SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
        begin
          case p.opcode of
            A_PUSH,A_POP,A_EX,A_EXX,A_NOP,A_HALT,A_DI,A_EI,A_IM,A_SET,A_RES,A_JP,A_JR,A_DJNZ,A_CALL,A_RET,A_RETI,A_RETN,A_RST,A_OUT:
              result:=false;
            A_LD:
              begin
                if p.ops<>2 then
                  internalerror(2020051112);
                { LD A,I or LD A,R ? }
                if (p.oper[0]^.typ=top_reg) and (p.oper[0]^.reg=NR_A) and
                   (p.oper[1]^.typ=top_reg) and ((p.oper[1]^.reg=NR_I) or (p.oper[1]^.reg=NR_R)) then
                  result:=Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_PARITYOVERFLOWFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_ZEROFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_SIGNFLAG)
                else
                  result:=false;
              end;
            A_LDI,A_LDIR,A_LDD,A_LDDR:
              result:=Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_PARITYOVERFLOWFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG);
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
                  result:=Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_PARITYOVERFLOWFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_ZEROFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_SIGNFLAG);
              end;
            A_CPI,A_CPIR,A_CPD,A_CPDR,A_RLD,A_RRD,A_BIT,A_INI,A_INIR,A_IND,A_INDR,A_OUTI,A_OTIR,A_OUTD,A_OTDR:
              result:=Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_PARITYOVERFLOWFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_ZEROFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_SIGNFLAG);
            A_ADD:
              begin
                if p.ops<>2 then
                  internalerror(2020051601);
                if (p.oper[0]^.typ=top_reg) and ((p.oper[0]^.reg=NR_HL) or (p.oper[0]^.reg=NR_IX) or (p.oper[0]^.reg=NR_IY)) then
                  result:=Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_CARRYFLAG)
                else
                  result:=true;
              end;
            A_ADC,A_SUB,A_SBC,A_AND,A_OR,A_XOR,A_CP,A_NEG,A_RLC,A_RL,A_RRC,A_RR,A_SLA,A_SRA,A_SRL:
              result:=true;
            A_DAA:
              result:=Reg1ReadDependsOnReg2(Reg,NR_PARITYOVERFLOWFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_ZEROFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_SIGNFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_CARRYFLAG);
            A_CPL:
              result:=Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG);
            A_CCF,A_SCF,A_RLCA,A_RLA,A_RRCA,A_RRA:
              result:=Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                      Reg1ReadDependsOnReg2(Reg,NR_CARRYFLAG);
            A_IN:
              begin
                if p.ops<>2 then
                  internalerror(2020051602);
                if (p.oper[1]^.typ=top_ref) and ((p.oper[1]^.ref^.base=NR_C) or (p.oper[1]^.ref^.index=NR_C)) then
                  result:=Reg1ReadDependsOnReg2(Reg,NR_ADDSUBTRACTFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_PARITYOVERFLOWFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_HALFCARRYFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_ZEROFLAG) or
                          Reg1ReadDependsOnReg2(Reg,NR_SIGNFLAG)
                else
                  result:=false;
              end;
            else
              internalerror(2020051111);
          end;
        end;
    end;

End.
