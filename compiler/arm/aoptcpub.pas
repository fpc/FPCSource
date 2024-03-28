 {
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains several types and constants necessary for the
    optimizer to work on the ARM architecture

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

{ enable the following define if memory references can have both a base and }
{ index register in 1 operand                                               }

{ enable the following define if memory references can have a scaled index }
{ define RefsHaveScale}

{ enable the following define if memory references can have a segment }
{ override                                                            }

{ define RefsHaveSegment}

Interface

Uses
  cgbase,aasmtai,
  cpubase,aasmcpu,AOptBase;

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
    function RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean; override;
  End;


{ ************************************************************************* }
{ ******************************* Constants ******************************* }
{ ************************************************************************* }
Const

{ the maximum number of things (registers, memory, ...) a single instruction }
{ changes                                                                    }

  MaxCh = 3;

{Oper index of operand that contains the source (reference) with a load }
{instruction                                                            }

  LoadSrc = 0;

{Oper index of operand that contains the destination (register) with a load }
{instruction                                                                }

  LoadDst = 1;

{Oper index of operand that contains the source (register) with a store }
{instruction                                                            }

  StoreSrc = 0;

{Oper index of operand that contains the destination (reference) with a load }
{instruction                                                                 }

  StoreDst = 1;

  aopt_uncondjmp = A_B;
  aopt_condjmp = A_B;

Implementation

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
  Constructor TCondRegs.init;
    Begin
    End;


  Destructor TCondRegs.Done; {$ifdef inl} inline; {$endif inl}
    Begin
    End;


  function TAoptBaseCpu.RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean;
    var
      i : Longint;
    begin
      result:=false;
      if (p1.typ <> ait_instruction) then
        Exit;

      if SuperRegistersEqual(Reg, NR_DEFAULTFLAGS) then
        begin
          { Comparison instructions (and procedural jump) }
          if (taicpu(p1).opcode in [A_BL, A_CMP, A_CMN, A_TST, A_TEQ]) then
            Exit(True);

          { Instruction sets CPSR register due to S suffix (floating-point
            instructios won't raise false positives) }
          if (taicpu(p1).oppostfix = PF_S) then
            Exit(True);

          { Everything else (conditional instructions only read CPSR) }
          Exit;
        end;

      case taicpu(p1).opcode of
        A_LDR:
          begin
            { special handling for LDRD }
            if (taicpu(p1).oppostfix=PF_D) and (getsupreg(taicpu(p1).oper[0]^.reg)+1=getsupreg(Reg)) then
              begin
                result:=true;
                exit;
              end;
          end;
        else
          ;
      end;
      for i:=0 to taicpu(p1).ops-1 do
        case taicpu(p1).oper[i]^.typ of
          top_reg:
            if (taicpu(p1).oper[i]^.reg=Reg) and (taicpu(p1).spilling_get_operation_type(i) in [operand_write,operand_readwrite]) then
              exit(true);
          top_ref:
            begin
              if (taicpu(p1).spilling_get_operation_type_ref(i,Reg)<>operand_read) then
                exit(true);
            end
          else
            ;
        end;
    end;

End.
