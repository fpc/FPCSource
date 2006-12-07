{
    Copyright (c) 1998-2003 by Carl Eric Codere and Peter Vreman

    Handles the common 68k assembler reader routines

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
unit ra68k;

{$i fpcdefs.inc}

  interface

    uses
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cpubase,rautils,cclasses;

    type
      Tm68kOperand=class(TOperand)
      end;

      Tm68kInstruction=class(TInstruction)
        opsize : topsize;
        function ConcatInstruction(p : TAsmList):tai;override;
        function ConcatLabeledInstr(p : TAsmList):tai;
      end;

  implementation

     uses
       verbose,cgbase;

{*****************************************************************************
                                TM68kInstruction
*****************************************************************************}

 function TM68kInstruction.ConcatInstruction(p : TAsmList):tai;
   var
     ai : taicpu;
   begin
     ai:=taicpu(inherited ConcatInstruction(p));
     ai.opsize:=opsize;
   end;
{
 function TM68kInstruction.ConcatInstruction(p : TAsmList):tai;
  var
    fits : boolean;
  begin
     writeln('jaj mami');
     result:=nil;
     fits := FALSE;
    { setup specific opcodetions for first pass }

    { Setup special operands }
    { Convert to general form as to conform to the m68k opcode table }
    if (opcode = A_ADDA) or (opcode = A_ADDI)
       then opcode := A_ADD
    else
    { CMPM excluded because of GAS v1.34 BUG }
    if (opcode = A_CMPA) or
       (opcode = A_CMPI) then
       opcode := A_CMP
    else
    if opcode = A_EORI then
      opcode := A_EOR
    else
    if opcode = A_MOVEA then
     opcode := A_MOVE
    else
    if opcode = A_ORI then
      opcode := A_OR
    else
    if (opcode = A_SUBA) or (opcode = A_SUBI) then
      opcode :=  A_SUB;

    { Setup operand types }

(*
    in opcode <> A_MOVEM then
    begin

      while not(fits) do
        begin
         { set the opcodetion cache, if the opcodetion }
         { occurs the first time                         }
         if (it[i].i=opcode) and (ins_cache[opcode]=-1) then
             ins_cache[opcode]:=i;

         if (it[i].i=opcode) and (instr.ops=it[i].ops) then
         begin
            { first fit }
           case instr.ops of
             0 : begin
                   fits:=true;
                   break;
                end;
            1 :
                begin
                  if (optyp1 and it[i].o1)<>0 then
                  begin
                    fits:=true;
                     break;
                  end;
                end;
            2 : if ((optyp1 and it[i].o1)<>0) and
                 ((optyp2 and it[i].o2)<>0) then
                 begin
                       fits:=true;
                       break;
                 end
            3 : if ((optyp1 and it[i].o1)<>0) and
                 ((optyp2 and it[i].o2)<>0) and
                 ((optyp3 and it[i].o3)<>0) then
                 begin
                   fits:=true;
                   break;
                 end;
           end; { end case }
        end; { endif }
        if it[i].i=A_NONE then
        begin
          { NO MATCH! }
          Message(asmr_e_invalid_combination_opcode_and_operand);
          exit;
        end;
        inc(i);
       end; { end while }
             *)
  fits:=TRUE;

  { We add the opcode to the opcode linked list }
  if fits then
  begin
    case ops of
     0:
        if opsize <> S_NO then
          result:=(taicpu.op_none(opcode,opsize))
        else
          result:=(taicpu.op_none(opcode,S_NO));
     1: begin
          case operands[1].opr.typ of
           OPR_SYMBOL:
              begin
                result:=(taicpu.op_sym_ofs(opcode,
                  opsize, operands[1].opr.symbol,operands[1].opr.symofs));
              end;
           OPR_CONSTANT:
              begin
                result:=(taicpu.op_const(opcode,
                  opsize, operands[1].opr.val));
              end;
           OPR_REGISTER:
              result:=(taicpu.op_reg(opcode,opsize,operands[1].opr.reg));
           OPR_REFERENCE:
              if opsize <> S_NO then
                begin
                  result:=(taicpu.op_ref(opcode,
                    opsize,operands[1].opr.ref));
                end
               else
                begin
                  { special jmp and call case with }
                  { symbolic references.           }
                  if opcode in [A_BSR,A_JMP,A_JSR,A_BRA,A_PEA] then
                    begin
                      result:=(taicpu.op_ref(opcode,
                        S_NO,operands[1].opr.ref));
                    end
                  else
                    Message(asmr_e_invalid_opcode_and_operand);
                end;
           OPR_NONE:
                Message(asmr_e_invalid_opcode_and_operand);
          else
           begin
             Message(asmr_e_invalid_opcode_and_operand);
           end;
          end;
        end;
     2: begin
                { source }
                  case operands[1].opr.typ of
                  { reg,reg     }
                  { reg,ref     }
                   OPR_REGISTER:
                     begin
                       case operands[2].opr.typ of
                         OPR_REGISTER:
                            begin
                               result:=(taicpu.op_reg_reg(opcode,
                               opsize,operands[1].opr.reg,operands[2].opr.reg));
                            end;
                         OPR_REFERENCE:
                                  result:=(taicpu.op_reg_ref(opcode,
                                  opsize,operands[1].opr.reg,operands[2].opr.ref));
                       else { else case }
                         begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                       end; { end second operand case for OPR_REGISTER }
                     end;
                  { regset, ref }
                   OPR_regset:
                          begin
                            case operands[2].opr.typ of
                              OPR_REFERENCE :
                                  result:=(taicpu.op_regset_ref(opcode,
                                  opsize,operands[1].opr.regset,operands[2].opr.ref));
                            else
                             begin
                               Message(asmr_e_invalid_opcode_and_operand);
                             end;
                            end; { end second operand case for OPR_regset }
                          end;

                  { const,reg   }
                  { const,const }
                  { const,ref   }
                   OPR_CONSTANT:
                      case operands[2].opr.typ of
                      { constant, constant does not have a specific size. }
                        OPR_CONSTANT:
                           result:=(taicpu.op_const_const(opcode,
                           S_NO,operands[1].opr.val,operands[2].opr.val));
                        OPR_REFERENCE:
                           begin
                                 result:=(taicpu.op_const_ref(opcode,
                                 opsize,operands[1].opr.val,
                                 operands[2].opr.ref))
                           end;
                        OPR_REGISTER:
                           begin
                                 result:=(taicpu.op_const_reg(opcode,
                                 opsize,operands[1].opr.val,
                                 operands[2].opr.reg))
                           end;
                      else
                         begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_CONSTANT }
                   { ref,reg     }
                   { ref,ref     }
                   OPR_REFERENCE:
                      case operands[2].opr.typ of
                         OPR_REGISTER:
                            begin
                              result:=(taicpu.op_ref_reg(opcode,
                               opsize,operands[1].opr.ref,
                               operands[2].opr.reg));
                            end;
                         OPR_regset:
                            begin
                              result:=(taicpu.op_ref_regset(opcode,
                               opsize,operands[1].opr.ref,
                               operands[2].opr.regset));
                            end;
                         OPR_REFERENCE: { special opcodes }
                            result:=(taicpu.op_ref_ref(opcode,
                            opsize,operands[1].opr.ref,
                            operands[2].opr.ref));
                      else
                         begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_REFERENCE }
           OPR_SYMBOL: case operands[2].opr.typ of
                        OPR_REFERENCE:
                           begin
                                 result:=(taicpu.op_sym_ofs_ref(opcode,
                                   opsize,operands[1].opr.symbol,operands[1].opr.symofs,
                                   operands[2].opr.ref))
                           end;
                        OPR_REGISTER:
                           begin
                                 result:=(taicpu.op_sym_ofs_reg(opcode,
                                   opsize,operands[1].opr.symbol,operands[1].opr.symofs,
                                   operands[2].opr.reg))
                           end;
                      else
                         begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_SYMBOL }
                  else
                     begin
                       Message(asmr_e_invalid_opcode_and_operand);
                     end;
                  end; { end first operand case }
        end;
     3: begin
           if (opcode = A_DIVSL) or (opcode = A_DIVUL) or (opcode = A_MULU)
           or (opcode = A_MULS) or (opcode = A_DIVS) or (opcode = A_DIVU) then
           begin
             if (operands[1].opr.typ <> OPR_REGISTER)
             or (operands[2].opr.typ <> OPR_REGISTER)
             or (operands[3].opr.typ <> OPR_REGISTER) then
             begin
               Message(asmr_e_invalid_opcode_and_operand);
             end
             else
             begin
               result:=(taicpu. op_reg_reg_reg(opcode,opsize,
                 operands[1].opr.reg,operands[2].opr.reg,operands[3].opr.reg));
             end;
           end
           else
            Message(asmr_e_invalid_opcode_and_operand);
        end;
  end; { end case }
 end;
   if assigned(result) then
     p.concat(result);
 end;
}

    function TM68kInstruction.ConcatLabeledInstr(p : TAsmList):tai;
      begin
        if ((opcode >= A_BCC) and (opcode <= A_BVS)) or
           (opcode = A_BRA) or (opcode = A_BSR) or
           (opcode = A_JMP) or (opcode = A_JSR) or
           ((opcode >= A_FBEQ) and (opcode <= A_FBNGLE)) then
          begin
           if ops > 2 then
             Message(asmr_e_invalid_opcode_and_operand)
           else if operands[1].opr.typ <> OPR_SYMBOL then
             Message(asmr_e_invalid_opcode_and_operand)
           else if (operands[1].opr.typ = OPR_SYMBOL) and
            (ops = 1) then
              if assigned(operands[1].opr.symbol) and
                 (operands[1].opr.symofs=0) then
                result:=taicpu.op_sym(opcode,S_NO,
                  operands[1].opr.symbol)
              else
                Message(asmr_e_invalid_opcode_and_operand);
          end
        else if ((opcode >= A_DBCC) and (opcode <= A_DBF))
          or ((opcode >= A_FDBEQ) and (opcode <= A_FDBNGLE)) then
          begin
            if (ops<>2) or
               (operands[1].opr.typ <> OPR_REGISTER) or
               (operands[2].opr.typ <> OPR_SYMBOL) or
               (operands[2].opr.symofs <> 0) then
              Message(asmr_e_invalid_opcode_and_operand)
            else
             result:=taicpu.op_reg_sym(opcode,opsize,operands[1].opr.reg,
              operands[2].opr.symbol);
          end
        else
          Message(asmr_e_invalid_opcode_and_operand);
       if assigned(result) then
          p.concat(result);
      end;




end.
