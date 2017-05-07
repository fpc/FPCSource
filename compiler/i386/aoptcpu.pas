{
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    This unit contains the peephole optimizer for i386

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

unit aoptcpu;

{$i fpcdefs.inc}

{ $define DEBUG_AOPTCPU}

  Interface

    uses
      cgbase,
      cpubase, aoptobj, aoptcpub, aopt, aoptx86,
      Aasmbase,aasmtai,aasmdata;

    Type
      TCpuAsmOptimizer = class(TX86AsmOptimizer)
        procedure Optimize; override;
        procedure PrePeepHoleOpts; override;
        procedure PeepHoleOptPass1; override;
        procedure PeepHoleOptPass2; override;
        procedure PostPeepHoleOpts; override;
        function DoFpuLoadStoreOpt(var p : tai) : boolean;
        function RegReadByInstruction(reg : TRegister; hp : tai) : boolean;
        function InstructionLoadsFromReg(const reg : TRegister;const hp : tai) : boolean;override;
      end;

    Var
      AsmOptimizer : TCpuAsmOptimizer;

  Implementation

    uses
      verbose,globtype,globals,
      cutils,
      aoptbase,
      cpuinfo,
      aasmcpu,
      aoptutils,
      procinfo,
      cgutils,cgx86,
      { units we should get rid off: }
      symsym,symconst;

    function TCPUAsmoptimizer.DoFpuLoadStoreOpt(var p: tai): boolean;
    { returns true if a "continue" should be done after this optimization }
    var hp1, hp2: tai;
    begin
      DoFpuLoadStoreOpt := false;
      if (taicpu(p).oper[0]^.typ = top_ref) and
         getNextInstruction(p, hp1) and
         (hp1.typ = ait_instruction) and
         (((taicpu(hp1).opcode = A_FLD) and
           (taicpu(p).opcode = A_FSTP)) or
          ((taicpu(p).opcode = A_FISTP) and
           (taicpu(hp1).opcode = A_FILD))) and
         (taicpu(hp1).oper[0]^.typ = top_ref) and
         (taicpu(hp1).opsize = taicpu(p).opsize) and
         RefsEqual(taicpu(p).oper[0]^.ref^, taicpu(hp1).oper[0]^.ref^) then
        begin
          { replacing fstp f;fld f by fst f is only valid for extended because of rounding }
          if (taicpu(p).opsize=S_FX) and
             getNextInstruction(hp1, hp2) and
             (hp2.typ = ait_instruction) and
             IsExitCode(hp2) and
             (taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
             not(assigned(current_procinfo.procdef.funcretsym) and
                 (taicpu(p).oper[0]^.ref^.offset < tabstractnormalvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset)) and
             (taicpu(p).oper[0]^.ref^.index = NR_NO) then
            begin
              asml.remove(p);
              asml.remove(hp1);
              p.free;
              hp1.free;
              p := hp2;
              removeLastDeallocForFuncRes(p);
              doFPULoadStoreOpt := true;
            end
          (* can't be done because the store operation rounds
          else
            { fst can't store an extended value! }
            if (taicpu(p).opsize <> S_FX) and
               (taicpu(p).opsize <> S_IQ) then
              begin
                if (taicpu(p).opcode = A_FSTP) then
                  taicpu(p).opcode := A_FST
                else taicpu(p).opcode := A_FIST;
                asml.remove(hp1);
                hp1.free;
              end
          *)
        end;
    end;


  { converts a TChange variable to a TRegister }
  function tch2reg(ch: tinschange): tsuperregister;
    const
      ch2reg: array[CH_REAX..CH_REDI] of tsuperregister = (RS_EAX,RS_ECX,RS_EDX,RS_EBX,RS_ESP,RS_EBP,RS_ESI,RS_EDI);
    begin
      if (ch <= CH_REDI) then
        tch2reg := ch2reg[ch]
      else if (ch <= CH_WEDI) then
        tch2reg := ch2reg[tinschange(ord(ch) - ord(CH_REDI))]
      else if (ch <= CH_RWEDI) then
        tch2reg := ch2reg[tinschange(ord(ch) - ord(CH_WEDI))]
      else if (ch <= CH_MEDI) then
        tch2reg := ch2reg[tinschange(ord(ch) - ord(CH_RWEDI))]
      else
        InternalError(2016041901)
    end;


  { Checks if the register is a 32 bit general purpose register }
  function isgp32reg(reg: TRegister): boolean;
    begin
      {$push}{$warnings off}
      isgp32reg:=(getregtype(reg)=R_INTREGISTER) and (getsupreg(reg)>=RS_EAX) and (getsupreg(reg)<=RS_EBX);
      {$pop}
    end;


  function TCpuAsmOptimizer.InstructionLoadsFromReg(const reg: TRegister;const hp: tai): boolean;
    begin
      Result:=RegReadByInstruction(reg,hp);
    end;


  function TCpuAsmOptimizer.RegReadByInstruction(reg: TRegister; hp: tai): boolean;
    var
      p: taicpu;
      opcount: longint;
    begin
      RegReadByInstruction := false;
      if hp.typ <> ait_instruction then
        exit;
      p := taicpu(hp);
      case p.opcode of
        A_CALL:
          regreadbyinstruction := true;
        A_IMUL:
          case p.ops of
            1:
              regReadByInstruction := RegInOp(reg,p.oper[0]^) or
                 (
                  ((getregtype(reg)=R_INTREGISTER) and (getsupreg(reg)=RS_EAX)) and
                  ((getsubreg(reg)<>R_SUBH) or (p.opsize<>S_B))
                 );
            2,3:
              regReadByInstruction :=
                reginop(reg,p.oper[0]^) or
                reginop(reg,p.oper[1]^);
          end;
        A_MUL:
          begin
            regReadByInstruction := RegInOp(reg,p.oper[0]^) or
               (
                ((getregtype(reg)=R_INTREGISTER) and (getsupreg(reg)=RS_EAX)) and
                ((getsubreg(reg)<>R_SUBH) or (p.opsize<>S_B))
               );
          end;
        A_IDIV,A_DIV:
          begin
            regReadByInstruction := RegInOp(reg,p.oper[0]^) or
               (
                 (getregtype(reg)=R_INTREGISTER) and
                 (
                   (getsupreg(reg)=RS_EAX) or ((getsupreg(reg)=RS_EDX) and (p.opsize<>S_B))
                 )
               );
          end;
        else
          begin
            if (p.opcode=A_LEA) and is_segment_reg(reg) then
              begin
                RegReadByInstruction := false;
                exit;
              end;
            for opcount := 0 to p.ops-1 do
              if (p.oper[opCount]^.typ = top_ref) and
                 RegInRef(reg,p.oper[opcount]^.ref^) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            { special handling for SSE MOVSD }
            if (p.opcode=A_MOVSD) and (p.ops>0) then
              begin
                if p.ops<>2 then
                  internalerror(2017042702);
                regReadByInstruction := reginop(reg,p.oper[0]^) or
                  (
                   (p.oper[1]^.typ=top_reg) and (p.oper[0]^.typ=top_reg) and reginop(reg, p.oper[1]^)
                  );
                exit;
              end;
            with insprop[p.opcode] do
              begin
                if getregtype(reg)=R_INTREGISTER then
                  begin
                    case getsupreg(reg) of
                      RS_EAX:
                        if [Ch_REAX,Ch_RWEAX,Ch_MEAX]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_ECX:
                        if [Ch_RECX,Ch_RWECX,Ch_MECX]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_EDX:
                        if [Ch_REDX,Ch_RWEDX,Ch_MEDX]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_EBX:
                        if [Ch_REBX,Ch_RWEBX,Ch_MEBX]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_ESP:
                        if [Ch_RESP,Ch_RWESP,Ch_MESP]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_EBP:
                        if [Ch_REBP,Ch_RWEBP,Ch_MEBP]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_ESI:
                        if [Ch_RESI,Ch_RWESI,Ch_MESI]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                      RS_EDI:
                        if [Ch_REDI,Ch_RWEDI,Ch_MEDI]*Ch<>[] then
                          begin
                            RegReadByInstruction := true;
                            exit
                          end;
                    end;
                  end;
                if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
                  begin
                    if (Ch_RFLAGScc in Ch) and not(getsubreg(reg) in [R_SUBW,R_SUBD,R_SUBQ]) then
                      begin
                        case p.condition of
                          C_A,C_NBE,       { CF=0 and ZF=0  }
                          C_BE,C_NA:       { CF=1 or ZF=1   }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGCARRY,R_SUBFLAGZERO];
                          C_AE,C_NB,C_NC,  { CF=0           }
                          C_B,C_NAE,C_C:   { CF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGCARRY];
                          C_NE,C_NZ,       { ZF=0           }
                          C_E,C_Z:         { ZF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGZERO];
                          C_G,C_NLE,       { ZF=0 and SF=OF }
                          C_LE,C_NG:       { ZF=1 or SF<>OF }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGZERO,R_SUBFLAGSIGN,R_SUBFLAGOVERFLOW];
                          C_GE,C_NL,       { SF=OF          }
                          C_L,C_NGE:       { SF<>OF         }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGSIGN,R_SUBFLAGOVERFLOW];
                          C_NO,            { OF=0           }
                          C_O:             { OF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGOVERFLOW];
                          C_NP,C_PO,       { PF=0           }
                          C_P,C_PE:        { PF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGPARITY];
                          C_NS,            { SF=0           }
                          C_S:             { SF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGSIGN];
                          else
                            internalerror(2017042701);
                        end;
                        if RegReadByInstruction then
                          exit;
                      end;
                    case getsubreg(reg) of
                      R_SUBW,R_SUBD,R_SUBQ:
                        RegReadByInstruction :=
                          [Ch_RCarryFlag,Ch_RParityFlag,Ch_RAuxiliaryFlag,Ch_RZeroFlag,Ch_RSignFlag,Ch_ROverflowFlag,
                           Ch_RWCarryFlag,Ch_RWParityFlag,Ch_RWAuxiliaryFlag,Ch_RWZeroFlag,Ch_RWSignFlag,Ch_RWOverflowFlag,
                           Ch_RDirFlag,Ch_RFlags,Ch_RWFlags,Ch_RFLAGScc]*Ch<>[];
                      R_SUBFLAGCARRY:
                        RegReadByInstruction:=[Ch_RCarryFlag,Ch_RWCarryFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGPARITY:
                        RegReadByInstruction:=[Ch_RParityFlag,Ch_RWParityFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGAUXILIARY:
                        RegReadByInstruction:=[Ch_RAuxiliaryFlag,Ch_RWAuxiliaryFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGZERO:
                        RegReadByInstruction:=[Ch_RZeroFlag,Ch_RWZeroFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGSIGN:
                        RegReadByInstruction:=[Ch_RSignFlag,Ch_RWSignFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGOVERFLOW:
                        RegReadByInstruction:=[Ch_ROverflowFlag,Ch_RWOverflowFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGINTERRUPT:
                        RegReadByInstruction:=[Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGDIRECTION:
                        RegReadByInstruction:=[Ch_RDirFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      else
                        internalerror(2017042601);
                    end;
                    exit;
                  end;
                if (Ch_NoReadIfEqualRegs in Ch) and (p.ops=2) and
                   (p.oper[0]^.typ=top_reg) and (p.oper[1]^.typ=top_reg) and
                   (p.oper[0]^.reg=p.oper[1]^.reg) then
                  exit;
                if ([CH_RWOP1,CH_ROP1,CH_MOP1]*Ch<>[]) and reginop(reg,p.oper[0]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
                if ([Ch_RWOP2,Ch_ROP2,Ch_MOP2]*Ch<>[]) and reginop(reg,p.oper[1]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
                if ([Ch_RWOP3,Ch_ROP3,Ch_MOP3]*Ch<>[]) and reginop(reg,p.oper[2]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
              end;
          end;
      end;
    end;


{ returns true if p contains a memory operand with a segment set }
function InsContainsSegRef(p: taicpu): boolean;
var
  i: longint;
begin
  result:=true;
  for i:=0 to p.opercnt-1 do
    if (p.oper[i]^.typ=top_ref) and
       (p.oper[i]^.ref^.segment<>NR_NO) then
      exit;
  result:=false;
end;


function InstrReadsFlags(p: tai): boolean;
  var
    l: longint;
  begin
    InstrReadsFlags := true;
    case p.typ of
      ait_instruction:
        if InsProp[taicpu(p).opcode].Ch*
           [Ch_RCarryFlag,Ch_RParityFlag,Ch_RAuxiliaryFlag,Ch_RZeroFlag,Ch_RSignFlag,Ch_ROverflowFlag,
            Ch_RWCarryFlag,Ch_RWParityFlag,Ch_RWAuxiliaryFlag,Ch_RWZeroFlag,Ch_RWSignFlag,Ch_RWOverflowFlag,
            Ch_RFlags,Ch_RWFlags,Ch_RFLAGScc,Ch_All]<>[] then
          exit;
      ait_label:
        exit;
    end;
    InstrReadsFlags := false;
  end;


procedure TCPUAsmOptimizer.PrePeepHoleOpts;
var
  p,hp1: tai;
  l: aint;
  tmpRef: treference;
begin
  p := BlockStart;
  while (p <> BlockEnd) Do
    begin
      case p.Typ Of
        Ait_Instruction:
          begin
            if InsContainsSegRef(taicpu(p)) then
              begin
                p := tai(p.next);
                continue;
              end;
            case taicpu(p).opcode Of
              A_IMUL:
                {changes certain "imul const, %reg"'s to lea sequences}
                begin
                  if (taicpu(p).oper[0]^.typ = Top_Const) and
                     (taicpu(p).oper[1]^.typ = Top_Reg) and
                     (taicpu(p).opsize = S_L) then
                    if (taicpu(p).oper[0]^.val = 1) then
                      if (taicpu(p).ops = 2) then
                       {remove "imul $1, reg"}
                        begin
                          hp1 := tai(p.Next);
                          asml.remove(p);
                          p.free;
                          p := hp1;
                          continue;
                        end
                      else
                       {change "imul $1, reg1, reg2" to "mov reg1, reg2"}
                        begin
                          hp1 := taicpu.Op_Reg_Reg(A_MOV, S_L, taicpu(p).oper[1]^.reg,taicpu(p).oper[2]^.reg);
                          InsertLLItem(p.previous, p.next, hp1);
                          p.free;
                          p := hp1;
                        end
                    else if
                     ((taicpu(p).ops <= 2) or
                      (taicpu(p).oper[2]^.typ = Top_Reg)) and
                     (taicpu(p).oper[0]^.val <= 12) and
                     not(cs_opt_size in current_settings.optimizerswitches) and
                     (not(GetNextInstruction(p, hp1)) or
                       {GetNextInstruction(p, hp1) and}
                       not((tai(hp1).typ = ait_instruction) and
                           ((taicpu(hp1).opcode=A_Jcc) and
                            (taicpu(hp1).condition in [C_O,C_NO])))) then
                      begin
                        reference_reset(tmpref,1,[]);
                        case taicpu(p).oper[0]^.val Of
                          3: begin
                             {imul 3, reg1, reg2 to
                                lea (reg1,reg1,2), reg2
                              imul 3, reg1 to
                                lea (reg1,reg1,2), reg1}
                               TmpRef.base := taicpu(p).oper[1]^.reg;
                               TmpRef.index := taicpu(p).oper[1]^.reg;
                               TmpRef.ScaleFactor := 2;
                               if (taicpu(p).ops = 2) then
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg)
                               else
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                               InsertLLItem(p.previous, p.next, hp1);
                               p.free;
                               p := hp1;
                            end;
                         5: begin
                            {imul 5, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                             imul 5, reg1 to
                               lea (reg1,reg1,4), reg1}
                              TmpRef.base := taicpu(p).oper[1]^.reg;
                              TmpRef.index := taicpu(p).oper[1]^.reg;
                              TmpRef.ScaleFactor := 4;
                              if (taicpu(p).ops = 2) then
                                hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg)
                              else
                                hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                              InsertLLItem(p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            end;
                         6: begin
                            {imul 6, reg1, reg2 to
                               lea (,reg1,2), reg2
                               lea (reg2,reg1,4), reg2
                             imul 6, reg1 to
                               lea (reg1,reg1,2), reg1
                               add reg1, reg1}
                              if (current_settings.optimizecputype <= cpu_386) then
                                begin
                                  TmpRef.index := taicpu(p).oper[1]^.reg;
                                  if (taicpu(p).ops = 3) then
                                    begin
                                      TmpRef.base := taicpu(p).oper[2]^.reg;
                                      TmpRef.ScaleFactor := 4;
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                    end
                                  else
                                    begin
                                      hp1 :=  taicpu.op_reg_reg(A_ADD, S_L,
                                        taicpu(p).oper[1]^.reg,taicpu(p).oper[1]^.reg);
                                    end;
                                  InsertLLItem(p, p.next, hp1);
                                  reference_reset(tmpref,2,[]);
                                  TmpRef.index := taicpu(p).oper[1]^.reg;
                                  TmpRef.ScaleFactor := 2;
                                  if (taicpu(p).ops = 3) then
                                    begin
                                      TmpRef.base := NR_NO;
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef,
                                        taicpu(p).oper[2]^.reg);
                                    end
                                  else
                                    begin
                                      TmpRef.base := taicpu(p).oper[1]^.reg;
                                      hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                    end;
                                  InsertLLItem(p.previous, p.next, hp1);
                                  p.free;
                                  p := tai(hp1.next);
                                end
                            end;
                          9: begin
                             {imul 9, reg1, reg2 to
                                lea (reg1,reg1,8), reg2
                              imul 9, reg1 to
                                lea (reg1,reg1,8), reg1}
                               TmpRef.base := taicpu(p).oper[1]^.reg;
                               TmpRef.index := taicpu(p).oper[1]^.reg;
                               TmpRef.ScaleFactor := 8;
                               if (taicpu(p).ops = 2) then
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg)
                               else
                                 hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                               InsertLLItem(p.previous, p.next, hp1);
                               p.free;
                               p := hp1;
                             end;
                         10: begin
                            {imul 10, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                               add reg2, reg2
                             imul 10, reg1 to
                               lea (reg1,reg1,4), reg1
                               add reg1, reg1}
                               if (current_settings.optimizecputype <= cpu_386) then
                                 begin
                                   if (taicpu(p).ops = 3) then
                                     hp1 :=  taicpu.op_reg_reg(A_ADD, S_L,
                                       taicpu(p).oper[2]^.reg,taicpu(p).oper[2]^.reg)
                                   else
                                     hp1 := taicpu.op_reg_reg(A_ADD, S_L,
                                       taicpu(p).oper[1]^.reg,taicpu(p).oper[1]^.reg);
                                   InsertLLItem(p, p.next, hp1);
                                   TmpRef.base := taicpu(p).oper[1]^.reg;
                                   TmpRef.index := taicpu(p).oper[1]^.reg;
                                   TmpRef.ScaleFactor := 4;
                                   if (taicpu(p).ops = 3) then
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg)
                                    else
                                      hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                   InsertLLItem(p.previous, p.next, hp1);
                                   p.free;
                                   p := tai(hp1.next);
                                 end
                             end;
                         12: begin
                            {imul 12, reg1, reg2 to
                               lea (,reg1,4), reg2
                               lea (reg2,reg1,8), reg2
                             imul 12, reg1 to
                               lea (reg1,reg1,2), reg1
                               lea (,reg1,4), reg1}
                               if (current_settings.optimizecputype <= cpu_386)
                                 then
                                   begin
                                     TmpRef.index := taicpu(p).oper[1]^.reg;
                                     if (taicpu(p).ops = 3) then
                                       begin
                                         TmpRef.base := taicpu(p).oper[2]^.reg;
                                         TmpRef.ScaleFactor := 8;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                                       end
                                     else
                                       begin
                                         TmpRef.base := NR_NO;
                                         TmpRef.ScaleFactor := 4;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                       end;
                                     InsertLLItem(p, p.next, hp1);
                                     reference_reset(tmpref,2,[]);
                                     TmpRef.index := taicpu(p).oper[1]^.reg;
                                     if (taicpu(p).ops = 3) then
                                       begin
                                         TmpRef.base := NR_NO;
                                         TmpRef.ScaleFactor := 4;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[2]^.reg);
                                       end
                                     else
                                       begin
                                         TmpRef.base := taicpu(p).oper[1]^.reg;
                                         TmpRef.ScaleFactor := 2;
                                         hp1 :=  taicpu.op_ref_reg(A_LEA, S_L, TmpRef, taicpu(p).oper[1]^.reg);
                                       end;
                                     InsertLLItem(p.previous, p.next, hp1);
                                     p.free;
                                     p := tai(hp1.next);
                                   end
                             end
                        end;
                      end;
                end;
              A_SAR, A_SHR:
                  {changes the code sequence
                   shr/sar const1, x
                   shl     const2, x
                   to either "sar/and", "shl/and" or just "and" depending on const1 and const2}
                begin
                  if GetNextInstruction(p, hp1) and
                     (tai(hp1).typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_SHL) and
                     (taicpu(p).oper[0]^.typ = top_const) and
                     (taicpu(hp1).oper[0]^.typ = top_const) and
                     (taicpu(hp1).opsize = taicpu(p).opsize) and
                     (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[1]^.typ) and
                     OpsEqual(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^) then
                    if (taicpu(p).oper[0]^.val > taicpu(hp1).oper[0]^.val) and
                       not(cs_opt_size in current_settings.optimizerswitches) then
                  { shr/sar const1, %reg
                    shl     const2, %reg
                    with const1 > const2 }
                      begin
                        taicpu(p).loadConst(0,taicpu(p).oper[0]^.val-taicpu(hp1).oper[0]^.val);
                        taicpu(hp1).opcode := A_AND;
                        l := (1 shl (taicpu(hp1).oper[0]^.val)) - 1;
                        case taicpu(p).opsize Of
                          S_L: taicpu(hp1).loadConst(0,l Xor aint($ffffffff));
                          S_B: taicpu(hp1).loadConst(0,l Xor $ff);
                          S_W: taicpu(hp1).loadConst(0,l Xor $ffff);
                        end;
                      end
                    else if (taicpu(p).oper[0]^.val<taicpu(hp1).oper[0]^.val) and
                            not(cs_opt_size in current_settings.optimizerswitches) then
                  { shr/sar const1, %reg
                    shl     const2, %reg
                    with const1 < const2 }
                      begin
                        taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val-taicpu(p).oper[0]^.val);
                        taicpu(p).opcode := A_AND;
                        l := (1 shl (taicpu(p).oper[0]^.val))-1;
                        case taicpu(p).opsize Of
                          S_L: taicpu(p).loadConst(0,l Xor aint($ffffffff));
                          S_B: taicpu(p).loadConst(0,l Xor $ff);
                          S_W: taicpu(p).loadConst(0,l Xor $ffff);
                        end;
                      end
                    else
                  { shr/sar const1, %reg
                    shl     const2, %reg
                    with const1 = const2 }
                      if (taicpu(p).oper[0]^.val = taicpu(hp1).oper[0]^.val) then
                        begin
                          taicpu(p).opcode := A_AND;
                          l := (1 shl (taicpu(p).oper[0]^.val))-1;
                          case taicpu(p).opsize Of
                            S_B: taicpu(p).loadConst(0,l Xor $ff);
                            S_W: taicpu(p).loadConst(0,l Xor $ffff);
                            S_L: taicpu(p).loadConst(0,l Xor aint($ffffffff));
                          end;
                          asml.remove(hp1);
                          hp1.free;
                        end;
                end;
              A_XOR:
                if (taicpu(p).oper[0]^.typ = top_reg) and
                   (taicpu(p).oper[1]^.typ = top_reg) and
                   (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                 { temporarily change this to 'mov reg,0' to make it easier }
                 { for the CSE. Will be changed back in pass 2              }
                  begin
                    taicpu(p).opcode := A_MOV;
                    taicpu(p).loadConst(0,0);
                  end;
            end;
          end;
      end;
      p := tai(p.next)
    end;
end;


{ First pass of peephole optimizations }
procedure TCPUAsmOPtimizer.PeepHoleOptPass1;

function WriteOk : Boolean;
  begin
    writeln('Ok');
    Result:=True;
  end;

var
  l : longint;
  p,hp1,hp2 : tai;
  hp3,hp4: tai;
  v:aint;

  TmpRef: TReference;

  TmpBool1, TmpBool2: Boolean;

  function GetFinalDestination(asml: TAsmList; hp: taicpu; level: longint): boolean;
  {traces sucessive jumps to their final destination and sets it, e.g.
   je l1                je l3
   <code>               <code>
   l1:       becomes    l1:
   je l2                je l3
   <code>               <code>
   l2:                  l2:
   jmp l3               jmp l3

   the level parameter denotes how deeep we have already followed the jump,
   to avoid endless loops with constructs such as "l5: ; jmp l5"           }

  var p1, p2: tai;
      l: tasmlabel;

    function FindAnyLabel(hp: tai; var l: tasmlabel): Boolean;
    begin
      FindAnyLabel := false;
      while assigned(hp.next) and
            (tai(hp.next).typ in (SkipInstr+[ait_align])) Do
        hp := tai(hp.next);
      if assigned(hp.next) and
         (tai(hp.next).typ = ait_label) then
        begin
          FindAnyLabel := true;
          l := tai_label(hp.next).labsym;
        end
    end;

  begin
    GetfinalDestination := false;
    if level > 20 then
      exit;
    p1 := getlabelwithsym(tasmlabel(hp.oper[0]^.ref^.symbol));
    if assigned(p1) then
      begin
        SkipLabels(p1,p1);
        if (tai(p1).typ = ait_instruction) and
           (taicpu(p1).is_jmp) then
          if { the next instruction after the label where the jump hp arrives}
             { is unconditional or of the same type as hp, so continue       }
             (taicpu(p1).condition in [C_None,hp.condition]) or
             { the next instruction after the label where the jump hp arrives}
             { is the opposite of hp (so this one is never taken), but after }
             { that one there is a branch that will be taken, so perform a   }
             { little hack: set p1 equal to this instruction (that's what the}
             { last SkipLabels is for, only works with short bool evaluation)}
             ((taicpu(p1).condition = inverse_cond(hp.condition)) and
              SkipLabels(p1,p2) and
              (p2.typ = ait_instruction) and
              (taicpu(p2).is_jmp) and
              (taicpu(p2).condition in [C_None,hp.condition]) and
              SkipLabels(p1,p1)) then
            begin
              { quick check for loops of the form "l5: ; jmp l5 }
              if (tasmlabel(taicpu(p1).oper[0]^.ref^.symbol).labelnr =
                   tasmlabel(hp.oper[0]^.ref^.symbol).labelnr) then
                exit;
              if not GetFinalDestination(asml, taicpu(p1),succ(level)) then
                exit;
              tasmlabel(hp.oper[0]^.ref^.symbol).decrefs;
              hp.oper[0]^.ref^.symbol:=taicpu(p1).oper[0]^.ref^.symbol;
              tasmlabel(hp.oper[0]^.ref^.symbol).increfs;
            end
          else
            if (taicpu(p1).condition = inverse_cond(hp.condition)) then
              if not FindAnyLabel(p1,l) then
                begin
  {$ifdef finaldestdebug}
                  insertllitem(asml,p1,p1.next,tai_comment.Create(
                    strpnew('previous label inserted'))));
  {$endif finaldestdebug}
                  current_asmdata.getjumplabel(l);
                  insertllitem(p1,p1.next,tai_label.Create(l));
                  tasmlabel(taicpu(hp).oper[0]^.ref^.symbol).decrefs;
                  hp.oper[0]^.ref^.symbol := l;
                  l.increfs;
  {               this won't work, since the new label isn't in the labeltable }
  {               so it will fail the rangecheck. Labeltable should become a   }
  {               hashtable to support this:                                   }
  {               GetFinalDestination(asml, hp);                               }
                end
              else
                begin
  {$ifdef finaldestdebug}
                  insertllitem(asml,p1,p1.next,tai_comment.Create(
                    strpnew('next label reused'))));
  {$endif finaldestdebug}
                  l.increfs;
                  hp.oper[0]^.ref^.symbol := l;
                  if not GetFinalDestination(asml, hp,succ(level)) then
                    exit;
                end;
      end;
    GetFinalDestination := true;
  end;

  function DoSubAddOpt(var p: tai): Boolean;
  begin
    DoSubAddOpt := False;
    if GetLastInstruction(p, hp1) and
       (hp1.typ = ait_instruction) and
       (taicpu(hp1).opsize = taicpu(p).opsize) then
      case taicpu(hp1).opcode Of
        A_DEC:
          if (taicpu(hp1).oper[0]^.typ = top_reg) and
             (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
            begin
              taicpu(p).loadConst(0,taicpu(p).oper[0]^.val+1);
              asml.remove(hp1);
              hp1.free;
            end;
         A_SUB:
           if (taicpu(hp1).oper[0]^.typ = top_const) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
             begin
               taicpu(p).loadConst(0,taicpu(p).oper[0]^.val+taicpu(hp1).oper[0]^.val);
               asml.remove(hp1);
               hp1.free;
             end;
         A_ADD:
           if (taicpu(hp1).oper[0]^.typ = top_const) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
             begin
               taicpu(p).loadConst(0,taicpu(p).oper[0]^.val-taicpu(hp1).oper[0]^.val);
               asml.remove(hp1);
               hp1.free;
               if (taicpu(p).oper[0]^.val = 0) then
                 begin
                   hp1 := tai(p.next);
                   asml.remove(p);
                   p.free;
                   if not GetLastInstruction(hp1, p) then
                     p := hp1;
                   DoSubAddOpt := True;
                 end
             end;
       end;
  end;

begin
  p := BlockStart;
  ClearUsedRegs;
  while (p <> BlockEnd) Do
    begin
      UpDateUsedRegs(UsedRegs, tai(p.next));
      case p.Typ Of
        ait_instruction:
          begin
            current_filepos:=taicpu(p).fileinfo;
            if InsContainsSegRef(taicpu(p)) then
              begin
                p := tai(p.next);
                continue;
              end;
            { Handle Jmp Optimizations }
            if taicpu(p).is_jmp then
              begin
      {the following if-block removes all code between a jmp and the next label,
        because it can never be executed}
                if (taicpu(p).opcode = A_JMP) then
                  begin
                    hp2:=p;
                    while GetNextInstruction(hp2, hp1) and
                          (hp1.typ <> ait_label) do
                      if not(hp1.typ in ([ait_label,ait_align]+skipinstr)) then
                        begin
                          { don't kill start/end of assembler block,
                            no-line-info-start/end etc }
                          if hp1.typ<>ait_marker then
                            begin
                              asml.remove(hp1);
                              hp1.free;
                            end
                          else
                            hp2:=hp1;
                        end
                      else break;
                    end;
                { remove jumps to a label coming right after them }
                if GetNextInstruction(p, hp1) then
                  begin
                    if FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol), hp1) and
  { TODO: FIXME removing the first instruction fails}
                        (p<>blockstart) then
                      begin
                        hp2:=tai(hp1.next);
                        asml.remove(p);
                        p.free;
                        p:=hp2;
                        continue;
                      end
                    else
                      begin
                        if hp1.typ = ait_label then
                          SkipLabels(hp1,hp1);
                        if (tai(hp1).typ=ait_instruction) and
                            (taicpu(hp1).opcode=A_JMP) and
                            GetNextInstruction(hp1, hp2) and
                            FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol), hp2) then
                          begin
                            if taicpu(p).opcode=A_Jcc then
                              begin
                                taicpu(p).condition:=inverse_cond(taicpu(p).condition);
                                tai_label(hp2).labsym.decrefs;
                                taicpu(p).oper[0]^.ref^.symbol:=taicpu(hp1).oper[0]^.ref^.symbol;
                                { when free'ing hp1, the ref. isn't decresed, so we don't
                                  increase it (FK)

                                  taicpu(p).oper[0]^.ref^.symbol.increfs;
                                }
                                asml.remove(hp1);
                                hp1.free;
                                GetFinalDestination(asml, taicpu(p),0);
                              end
                            else
                              begin
                                GetFinalDestination(asml, taicpu(p),0);
                                p:=tai(p.next);
                                continue;
                              end;
                          end
                        else
                          GetFinalDestination(asml, taicpu(p),0);
                      end;
                  end;
              end
            else
            { All other optimizes }
              begin
                for l := 0 to taicpu(p).ops-1 Do
                  if (taicpu(p).oper[l]^.typ = top_ref) then
                    With taicpu(p).oper[l]^.ref^ Do
                      begin
                        if (base = NR_NO) and
                           (index <> NR_NO) and
                           (scalefactor in [0,1]) then
                          begin
                            base := index;
                            index := NR_NO
                          end
                      end;
                case taicpu(p).opcode Of
                  A_AND:
                    if OptPass1And(p) then
                      continue;
                  A_CMP:
                    begin
                      { cmp register,$8000                neg register
                        je target                 -->     jo target

                        .... only if register is deallocated before jump.}
                      case Taicpu(p).opsize of
                        S_B: v:=$80;
                        S_W: v:=$8000;
                        S_L: v:=aint($80000000);
                        else
                          internalerror(2013112905);
                      end;
                      if (taicpu(p).oper[0]^.typ=Top_const) and
                         (taicpu(p).oper[0]^.val=v) and
                         (Taicpu(p).oper[1]^.typ=top_reg) and
                         GetNextInstruction(p, hp1) and
                         (hp1.typ=ait_instruction) and
                         (taicpu(hp1).opcode=A_Jcc) and
                         (Taicpu(hp1).condition in [C_E,C_NE]) and
                         not(RegInUsedRegs(Taicpu(p).oper[1]^.reg, UsedRegs)) then
                        begin
                          Taicpu(p).opcode:=A_NEG;
                          Taicpu(p).loadoper(0,Taicpu(p).oper[1]^);
                          Taicpu(p).clearop(1);
                          Taicpu(p).ops:=1;
                          if Taicpu(hp1).condition=C_E then
                            Taicpu(hp1).condition:=C_O
                          else
                            Taicpu(hp1).condition:=C_NO;
                          continue;
                        end;
                      {
                      @@2:                              @@2:
                        ....                              ....
                        cmp operand1,0
                        jle/jbe @@1
                        dec operand1             -->      sub operand1,1
                        jmp @@2                           jge/jae @@2
                      @@1:                              @@1:
                        ...                               ....}
                      if (taicpu(p).oper[0]^.typ = top_const) and
                         (taicpu(p).oper[1]^.typ in [top_reg,top_ref]) and
                         (taicpu(p).oper[0]^.val = 0) and
                         GetNextInstruction(p, hp1) and
                         (hp1.typ = ait_instruction) and
                         (taicpu(hp1).is_jmp) and
                         (taicpu(hp1).opcode=A_Jcc) and
                         (taicpu(hp1).condition in [C_LE,C_BE]) and
                         GetNextInstruction(hp1,hp2) and
                         (hp2.typ = ait_instruction) and
                         (taicpu(hp2).opcode = A_DEC) and
                         OpsEqual(taicpu(hp2).oper[0]^,taicpu(p).oper[1]^) and
                         GetNextInstruction(hp2, hp3) and
                         (hp3.typ = ait_instruction) and
                         (taicpu(hp3).is_jmp) and
                         (taicpu(hp3).opcode = A_JMP) and
                         GetNextInstruction(hp3, hp4) and
                         FindLabel(tasmlabel(taicpu(hp1).oper[0]^.ref^.symbol),hp4) then
                        begin
                          taicpu(hp2).Opcode := A_SUB;
                          taicpu(hp2).loadoper(1,taicpu(hp2).oper[0]^);
                          taicpu(hp2).loadConst(0,1);
                          taicpu(hp2).ops:=2;
                          taicpu(hp3).Opcode := A_Jcc;
                          case taicpu(hp1).condition of
                            C_LE: taicpu(hp3).condition := C_GE;
                            C_BE: taicpu(hp3).condition := C_AE;
                          end;
                          asml.remove(p);
                          asml.remove(hp1);
                          p.free;
                          hp1.free;
                          p := hp2;
                          continue;
                        end
                    end;
                  A_FLD:
                    begin
                      if (taicpu(p).oper[0]^.typ = top_reg) and
                         GetNextInstruction(p, hp1) and
                         (hp1.typ = Ait_Instruction) and
                          (taicpu(hp1).oper[0]^.typ = top_reg) and
                         (taicpu(hp1).oper[1]^.typ = top_reg) and
                         (taicpu(hp1).oper[0]^.reg = NR_ST) and
                         (taicpu(hp1).oper[1]^.reg = NR_ST1) then
                         { change                        to
                             fld      reg               fxxx reg,st
                             fxxxp    st, st1 (hp1)
                           Remark: non commutative operations must be reversed!
                         }
                        begin
                            case taicpu(hp1).opcode Of
                              A_FMULP,A_FADDP,
                              A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                                begin
                                  case taicpu(hp1).opcode Of
                                    A_FADDP: taicpu(hp1).opcode := A_FADD;
                                    A_FMULP: taicpu(hp1).opcode := A_FMUL;
                                    A_FSUBP: taicpu(hp1).opcode := A_FSUBR;
                                    A_FSUBRP: taicpu(hp1).opcode := A_FSUB;
                                    A_FDIVP: taicpu(hp1).opcode := A_FDIVR;
                                    A_FDIVRP: taicpu(hp1).opcode := A_FDIV;
                                  end;
                                  taicpu(hp1).oper[0]^.reg := taicpu(p).oper[0]^.reg;
                                  taicpu(hp1).oper[1]^.reg := NR_ST;
                                  asml.remove(p);
                                  p.free;
                                  p := hp1;
                                  continue;
                                end;
                            end;
                        end
                      else
                        if (taicpu(p).oper[0]^.typ = top_ref) and
                           GetNextInstruction(p, hp2) and
                           (hp2.typ = Ait_Instruction) and
                           (taicpu(hp2).ops = 2) and
                           (taicpu(hp2).oper[0]^.typ = top_reg) and
                           (taicpu(hp2).oper[1]^.typ = top_reg) and
                           (taicpu(p).opsize in [S_FS, S_FL]) and
                           (taicpu(hp2).oper[0]^.reg = NR_ST) and
                           (taicpu(hp2).oper[1]^.reg = NR_ST1) then
                          if GetLastInstruction(p, hp1) and
                             (hp1.typ = Ait_Instruction) and
                             ((taicpu(hp1).opcode = A_FLD) or
                              (taicpu(hp1).opcode = A_FST)) and
                             (taicpu(hp1).opsize = taicpu(p).opsize) and
                             (taicpu(hp1).oper[0]^.typ = top_ref) and
                             RefsEqual(taicpu(p).oper[0]^.ref^, taicpu(hp1).oper[0]^.ref^) then
                            if ((taicpu(hp2).opcode = A_FMULP) or
                                (taicpu(hp2).opcode = A_FADDP)) then
                            { change                      to
                                fld/fst   mem1  (hp1)       fld/fst   mem1
                                fld       mem1  (p)         fadd/
                                faddp/                       fmul     st, st
                                fmulp  st, st1 (hp2) }
                              begin
                                asml.remove(p);
                                p.free;
                                p := hp1;
                                if (taicpu(hp2).opcode = A_FADDP) then
                                  taicpu(hp2).opcode := A_FADD
                                else
                                  taicpu(hp2).opcode := A_FMUL;
                                taicpu(hp2).oper[1]^.reg := NR_ST;
                              end
                            else
                            { change              to
                                fld/fst mem1 (hp1)   fld/fst mem1
                                fld     mem1 (p)     fld      st}
                              begin
                                taicpu(p).changeopsize(S_FL);
                                taicpu(p).loadreg(0,NR_ST);
                              end
                          else
                            begin
                              case taicpu(hp2).opcode Of
                                A_FMULP,A_FADDP,A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                          { change                        to
                              fld/fst  mem1    (hp1)      fld/fst    mem1
                              fld      mem2    (p)        fxxx       mem2
                              fxxxp    st, st1 (hp2)                      }

                                  begin
                                    case taicpu(hp2).opcode Of
                                      A_FADDP: taicpu(p).opcode := A_FADD;
                                      A_FMULP: taicpu(p).opcode := A_FMUL;
                                      A_FSUBP: taicpu(p).opcode := A_FSUBR;
                                      A_FSUBRP: taicpu(p).opcode := A_FSUB;
                                      A_FDIVP: taicpu(p).opcode := A_FDIVR;
                                      A_FDIVRP: taicpu(p).opcode := A_FDIV;
                                    end;
                                    asml.remove(hp2);
                                    hp2.free;
                                  end
                              end
                            end
                    end;
                  A_FSTP,A_FISTP:
                    if doFpuLoadStoreOpt(p) then
                      continue;
                  A_LEA:
                    begin
                      {removes seg register prefixes from LEA operations, as they
                      don't do anything}
                      taicpu(p).oper[0]^.ref^.Segment := NR_NO;
                      {changes "lea (%reg1), %reg2" into "mov %reg1, %reg2"}
                      if (taicpu(p).oper[0]^.ref^.base <> NR_NO) and
                         (getsupreg(taicpu(p).oper[0]^.ref^.base) in [RS_EAX..RS_ESP]) and
                         (taicpu(p).oper[0]^.ref^.index = NR_NO) and
                         (not(Assigned(taicpu(p).oper[0]^.ref^.Symbol))) then
                        begin
                          if (taicpu(p).oper[0]^.ref^.base <> taicpu(p).oper[1]^.reg) and
                             (taicpu(p).oper[0]^.ref^.offset = 0) then
                            begin
                              hp1 := taicpu.op_reg_reg(A_MOV, S_L,taicpu(p).oper[0]^.ref^.base,
                                taicpu(p).oper[1]^.reg);
                              InsertLLItem(p.previous,p.next, hp1);
                              p.free;
                              p := hp1;
                              continue;
                            end
                          else if (taicpu(p).oper[0]^.ref^.offset = 0) then
                            begin
                              hp1 := tai(p.Next);
                              asml.remove(p);
                              p.free;
                              p := hp1;
                              continue;
                            end
                          { continue to use lea to adjust the stack pointer,
                            it is the recommended way, but only if not optimizing for size }
                          else if (taicpu(p).oper[1]^.reg<>NR_STACK_POINTER_REG) or
                            (cs_opt_size in current_settings.optimizerswitches) then
                            with taicpu(p).oper[0]^.ref^ do
                              if (base = taicpu(p).oper[1]^.reg) then
                                begin
                                  l := offset;
                                  if (l=1) and UseIncDec then
                                    begin
                                      taicpu(p).opcode := A_INC;
                                      taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                                      taicpu(p).ops := 1
                                    end
                                  else if (l=-1) and UseIncDec then
                                    begin
                                      taicpu(p).opcode := A_DEC;
                                      taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                                      taicpu(p).ops := 1;
                                    end
                                  else
                                    begin
                                      if (l<0) and (l<>-2147483648) then
                                        begin
                                          taicpu(p).opcode := A_SUB;
                                          taicpu(p).loadConst(0,-l);
                                        end
                                      else
                                        begin
                                          taicpu(p).opcode := A_ADD;
                                          taicpu(p).loadConst(0,l);
                                        end;
                                    end;
                                end;
                        end
(*
                      This is unsafe, lea doesn't modify the flags but "add"
                      does. This breaks webtbs/tw15694.pp. The above
                      transformations are also unsafe, but they don't seem to
                      be triggered by code that FPC generators (or that at
                      least does not occur in the tests...). This needs to be
                      fixed by checking for the liveness of the flags register.

                      else if MatchReference(taicpu(p).oper[0]^.ref^,taicpu(p).oper[1]^.reg,NR_INVALID) then
                        begin
                          hp1:=taicpu.op_reg_reg(A_ADD,S_L,taicpu(p).oper[0]^.ref^.index,
                            taicpu(p).oper[0]^.ref^.base);
                          InsertLLItem(asml,p.previous,p.next, hp1);
                          DebugMsg('Peephole Lea2AddBase done',hp1);
                          p.free;
                          p:=hp1;
                          continue;
                        end
                      else if MatchReference(taicpu(p).oper[0]^.ref^,NR_INVALID,taicpu(p).oper[1]^.reg) then
                        begin
                          hp1:=taicpu.op_reg_reg(A_ADD,S_L,taicpu(p).oper[0]^.ref^.base,
                            taicpu(p).oper[0]^.ref^.index);
                          InsertLLItem(asml,p.previous,p.next,hp1);
                          DebugMsg('Peephole Lea2AddIndex done',hp1);
                          p.free;
                          p:=hp1;
                          continue;
                        end
*)
                    end;

                  A_MOV:
                    begin
                      If OptPass1MOV(p) then
                        Continue;
                    end;

                  A_MOVSX,
                  A_MOVZX :
                    begin
                      if (taicpu(p).oper[1]^.typ = top_reg) and
                         GetNextInstruction(p,hp1) and
                         (hp1.typ = ait_instruction) and
                         IsFoldableArithOp(taicpu(hp1),taicpu(p).oper[1]^.reg) and
                         (getsupreg(taicpu(hp1).oper[0]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX]) and
                         GetNextInstruction(hp1,hp2) and
                         MatchInstruction(hp2,A_MOV,[]) and
                         (taicpu(hp2).oper[0]^.typ = top_reg) and
                         OpsEqual(taicpu(hp2).oper[1]^,taicpu(p).oper[0]^) and
                         (((taicpu(hp1).ops=2) and
                           (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg))) or
                           ((taicpu(hp1).ops=1) and
                           (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[0]^.reg)))) and
                         not(RegUsedAfterInstruction(taicpu(hp2).oper[0]^.reg,hp2,UsedRegs)) then
                      { change   movsX/movzX    reg/ref, reg2             }
                      {          add/sub/or/... reg3/$const, reg2         }
                      {          mov            reg2 reg/ref              }
                      { to       add/sub/or/... reg3/$const, reg/ref      }
                        begin
                          { by example:
                              movswl  %si,%eax        movswl  %si,%eax      p
                              decl    %eax            addl    %edx,%eax     hp1
                              movw    %ax,%si         movw    %ax,%si       hp2
                            ->
                              movswl  %si,%eax        movswl  %si,%eax      p
                              decw    %eax            addw    %edx,%eax     hp1
                              movw    %ax,%si         movw    %ax,%si       hp2
                          }
                          taicpu(hp1).changeopsize(taicpu(hp2).opsize);
                          {
                            ->
                              movswl  %si,%eax        movswl  %si,%eax      p
                              decw    %si             addw    %dx,%si       hp1
                              movw    %ax,%si         movw    %ax,%si       hp2
                          }
                          case taicpu(hp1).ops of
                            1:
                             taicpu(hp1).loadoper(0,taicpu(hp2).oper[1]^);
                            2:
                              begin
                                taicpu(hp1).loadoper(1,taicpu(hp2).oper[1]^);
                                if (taicpu(hp1).oper[0]^.typ = top_reg) then
                                  setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                              end;
                            else
                              internalerror(2008042701);
                          end;
                          {
                            ->
                              decw    %si             addw    %dx,%si       p
                          }
                          asml.remove(p);
                          asml.remove(hp2);
                          p.free;
                          hp2.free;
                          p := hp1
                        end
                      { removes superfluous And's after movzx's }
                      else if taicpu(p).opcode=A_MOVZX then
                        begin
                          if (taicpu(p).oper[1]^.typ = top_reg) and
                             GetNextInstruction(p, hp1) and
                             (tai(hp1).typ = ait_instruction) and
                             (taicpu(hp1).opcode = A_AND) and
                             (taicpu(hp1).oper[0]^.typ = top_const) and
                             (taicpu(hp1).oper[1]^.typ = top_reg) and
                             (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                            case taicpu(p).opsize Of
                              S_BL, S_BW:
                                if (taicpu(hp1).oper[0]^.val = $ff) then
                                  begin
                                    asml.remove(hp1);
                                    hp1.free;
                                  end;
                              S_WL:
                                if (taicpu(hp1).oper[0]^.val = $ffff) then
                                  begin
                                    asml.remove(hp1);
                                    hp1.free;
                                  end;
                            end;
                        {changes some movzx constructs to faster synonims (all examples
                        are given with eax/ax, but are also valid for other registers)}
                          if (taicpu(p).oper[1]^.typ = top_reg) then
                            if (taicpu(p).oper[0]^.typ = top_reg) then
                              case taicpu(p).opsize of
                                S_BW:
                                  begin
                                    if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                                       not(cs_opt_size in current_settings.optimizerswitches) then
                                      {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                                      begin
                                        taicpu(p).opcode := A_AND;
                                        taicpu(p).changeopsize(S_W);
                                        taicpu(p).loadConst(0,$ff);
                                      end
                                    else if GetNextInstruction(p, hp1) and
                                         (tai(hp1).typ = ait_instruction) and
                                         (taicpu(hp1).opcode = A_AND) and
                                         (taicpu(hp1).oper[0]^.typ = top_const) and
                                         (taicpu(hp1).oper[1]^.typ = top_reg) and
                                         (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                     {Change "movzbw %reg1, %reg2; andw $const, %reg2"
                                      to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                                      begin
                                        taicpu(p).opcode := A_MOV;
                                        taicpu(p).changeopsize(S_W);
                                        setsubreg(taicpu(p).oper[0]^.reg,R_SUBW);
                                        taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                      end;
                                  end;
                                S_BL:
                                  begin
                                    if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                                       not(cs_opt_size in current_settings.optimizerswitches) then
                                      {Change "movzbl %al, %eax" to "andl $0x0ffh, %eax"}
                                      begin
                                        taicpu(p).opcode := A_AND;
                                        taicpu(p).changeopsize(S_L);
                                        taicpu(p).loadConst(0,$ff)
                                      end
                                    else if GetNextInstruction(p, hp1) and
                                        (tai(hp1).typ = ait_instruction) and
                                        (taicpu(hp1).opcode = A_AND) and
                                        (taicpu(hp1).oper[0]^.typ = top_const) and
                                        (taicpu(hp1).oper[1]^.typ = top_reg) and
                                        (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                    {Change "movzbl %reg1, %reg2; andl $const, %reg2"
                                      to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                                      begin
                                        taicpu(p).opcode := A_MOV;
                                        taicpu(p).changeopsize(S_L);
                                        setsubreg(taicpu(p).oper[0]^.reg,R_SUBWHOLE);
                                        taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                      end
                                  end;
                                S_WL:
                                  begin
                                    if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                                       not(cs_opt_size in current_settings.optimizerswitches) then
                                    {Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax"}
                                      begin
                                        taicpu(p).opcode := A_AND;
                                        taicpu(p).changeopsize(S_L);
                                        taicpu(p).loadConst(0,$ffff);
                                      end
                                    else if GetNextInstruction(p, hp1) and
                                        (tai(hp1).typ = ait_instruction) and
                                        (taicpu(hp1).opcode = A_AND) and
                                        (taicpu(hp1).oper[0]^.typ = top_const) and
                                        (taicpu(hp1).oper[1]^.typ = top_reg) and
                                        (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                      {Change "movzwl %reg1, %reg2; andl $const, %reg2"
                                      to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                                      begin
                                        taicpu(p).opcode := A_MOV;
                                        taicpu(p).changeopsize(S_L);
                                        setsubreg(taicpu(p).oper[0]^.reg,R_SUBWHOLE);
                                        taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                                      end;
                                  end;
                                end
                              else if (taicpu(p).oper[0]^.typ = top_ref) then
                                begin
                                  if GetNextInstruction(p, hp1) and
                                     (tai(hp1).typ = ait_instruction) and
                                     (taicpu(hp1).opcode = A_AND) and
                                     (taicpu(hp1).oper[0]^.typ = Top_Const) and
                                     (taicpu(hp1).oper[1]^.typ = Top_Reg) and
                                     (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                                    begin
                                      taicpu(p).opcode := A_MOV;
                                      case taicpu(p).opsize Of
                                        S_BL:
                                          begin
                                            taicpu(p).changeopsize(S_L);
                                            taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                          end;
                                        S_WL:
                                          begin
                                            taicpu(p).changeopsize(S_L);
                                            taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                                          end;
                                        S_BW:
                                          begin
                                            taicpu(p).changeopsize(S_W);
                                            taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                                          end;
                                      end;
                                    end;
                                end;
                        end;
                    end;

(* should not be generated anymore by the current code generator
                  A_POP:
                    begin
                      if target_info.system=system_i386_go32v2 then
                      begin
                        { Transform a series of pop/pop/pop/push/push/push to }
                        { 'movl x(%esp),%reg' for go32v2 (not for the rest,   }
                        { because I'm not sure whether they can cope with     }
                        { 'movl x(%esp),%reg' with x > 0, I believe we had    }
                        { such a problem when using esp as frame pointer (JM) }
                        if (taicpu(p).oper[0]^.typ = top_reg) then
                          begin
                            hp1 := p;
                            hp2 := p;
                            l := 0;
                            while getNextInstruction(hp1,hp1) and
                                  (hp1.typ = ait_instruction) and
                                  (taicpu(hp1).opcode = A_POP) and
                                  (taicpu(hp1).oper[0]^.typ = top_reg) do
                              begin
                                hp2 := hp1;
                                inc(l,4);
                              end;
                            getLastInstruction(p,hp3);
                            l1 := 0;
                            while (hp2 <> hp3) and
                                  assigned(hp1) and
                                  (hp1.typ = ait_instruction) and
                                  (taicpu(hp1).opcode = A_PUSH) and
                                  (taicpu(hp1).oper[0]^.typ = top_reg) and
                                  (taicpu(hp1).oper[0]^.reg.enum = taicpu(hp2).oper[0]^.reg.enum) do
                              begin
                                { change it to a two op operation }
                                taicpu(hp2).oper[1]^.typ:=top_none;
                                taicpu(hp2).ops:=2;
                                taicpu(hp2).opcode := A_MOV;
                                taicpu(hp2).loadoper(1,taicpu(hp1).oper[0]^);
                                reference_reset(tmpref);
                                tmpRef.base.enum:=R_INTREGISTER;
                                tmpRef.base.number:=NR_STACK_POINTER_REG;
                                convert_register_to_enum(tmpref.base);
                                tmpRef.offset := l;
                                taicpu(hp2).loadRef(0,tmpRef);
                                hp4 := hp1;
                                getNextInstruction(hp1,hp1);
                                asml.remove(hp4);
                                hp4.free;
                                getLastInstruction(hp2,hp2);
                                dec(l,4);
                                inc(l1);
                              end;
                            if l <> -4 then
                              begin
                                inc(l,4);
                                for l1 := l1 downto 1 do
                                  begin
                                    getNextInstruction(hp2,hp2);
                                    dec(taicpu(hp2).oper[0]^.ref^.offset,l);
                                  end
                              end
                          end
                        end
                      else
                        begin
                          if (taicpu(p).oper[0]^.typ = top_reg) and
                            GetNextInstruction(p, hp1) and
                            (tai(hp1).typ=ait_instruction) and
                            (taicpu(hp1).opcode=A_PUSH) and
                            (taicpu(hp1).oper[0]^.typ = top_reg) and
                            (taicpu(hp1).oper[0]^.reg.enum=taicpu(p).oper[0]^.reg.enum) then
                            begin
                              { change it to a two op operation }
                              taicpu(p).oper[1]^.typ:=top_none;
                              taicpu(p).ops:=2;
                              taicpu(p).opcode := A_MOV;
                              taicpu(p).loadoper(1,taicpu(p).oper[0]^);
                              reference_reset(tmpref);
                              TmpRef.base.enum := R_ESP;
                              taicpu(p).loadRef(0,TmpRef);
                              asml.remove(hp1);
                              hp1.free;
                            end;
                        end;
                    end;
*)
                  A_PUSH:
                    begin
                      if (taicpu(p).opsize = S_W) and
                         (taicpu(p).oper[0]^.typ = Top_Const) and
                         GetNextInstruction(p, hp1) and
                         (tai(hp1).typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_PUSH) and
                         (taicpu(hp1).oper[0]^.typ = Top_Const) and
                         (taicpu(hp1).opsize = S_W) then
                        begin
                          taicpu(p).changeopsize(S_L);
                          taicpu(p).loadConst(0,taicpu(p).oper[0]^.val shl 16 + word(taicpu(hp1).oper[0]^.val));
                          asml.remove(hp1);
                          hp1.free;
                        end;
                    end;
                  A_SHL, A_SAL:
                    begin
                      if (taicpu(p).oper[0]^.typ = Top_Const) and
                         (taicpu(p).oper[1]^.typ = Top_Reg) and
                         (taicpu(p).opsize = S_L) and
                         (taicpu(p).oper[0]^.val <= 3) then
                    {Changes "shl const, %reg32; add const/reg, %reg32" to one lea statement}
                        begin
                          TmpBool1 := True; {should we check the next instruction?}
                          TmpBool2 := False; {have we found an add/sub which could be
                                              integrated in the lea?}
                          reference_reset(tmpref,2,[]);
                          TmpRef.index := taicpu(p).oper[1]^.reg;
                          TmpRef.scalefactor := 1 shl taicpu(p).oper[0]^.val;
                          while TmpBool1 and
                                GetNextInstruction(p, hp1) and
                                (tai(hp1).typ = ait_instruction) and
                                ((((taicpu(hp1).opcode = A_ADD) or
                                   (taicpu(hp1).opcode = A_SUB)) and
                                  (taicpu(hp1).oper[1]^.typ = Top_Reg) and
                                  (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg)) or
                                 (((taicpu(hp1).opcode = A_INC) or
                                   (taicpu(hp1).opcode = A_DEC)) and
                                  (taicpu(hp1).oper[0]^.typ = Top_Reg) and
                                  (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg))) and
                                (not GetNextInstruction(hp1,hp2) or
                                 not instrReadsFlags(hp2)) Do
                            begin
                              TmpBool1 := False;
                              if (taicpu(hp1).oper[0]^.typ = Top_Const) then
                                begin
                                  TmpBool1 := True;
                                  TmpBool2 := True;
                                  case taicpu(hp1).opcode of
                                    A_ADD:
                                      inc(TmpRef.offset, longint(taicpu(hp1).oper[0]^.val));
                                    A_SUB:
                                      dec(TmpRef.offset, longint(taicpu(hp1).oper[0]^.val));
                                  end;
                                  asml.remove(hp1);
                                  hp1.free;
                                end
                              else
                                if (taicpu(hp1).oper[0]^.typ = Top_Reg) and
                                   (((taicpu(hp1).opcode = A_ADD) and
                                     (TmpRef.base = NR_NO)) or
                                    (taicpu(hp1).opcode = A_INC) or
                                    (taicpu(hp1).opcode = A_DEC)) then
                                  begin
                                    TmpBool1 := True;
                                    TmpBool2 := True;
                                    case taicpu(hp1).opcode of
                                      A_ADD:
                                        TmpRef.base := taicpu(hp1).oper[0]^.reg;
                                      A_INC:
                                        inc(TmpRef.offset);
                                      A_DEC:
                                        dec(TmpRef.offset);
                                    end;
                                    asml.remove(hp1);
                                    hp1.free;
                                  end;
                            end;
                          if TmpBool2 or
                             ((current_settings.optimizecputype < cpu_Pentium2) and
                             (taicpu(p).oper[0]^.val <= 3) and
                             not(cs_opt_size in current_settings.optimizerswitches)) then
                            begin
                              if not(TmpBool2) and
                                  (taicpu(p).oper[0]^.val = 1) then
                                begin
                                  hp1 := taicpu.Op_reg_reg(A_ADD,taicpu(p).opsize,
                                            taicpu(p).oper[1]^.reg, taicpu(p).oper[1]^.reg)
                                end
                              else
                                hp1 := taicpu.op_ref_reg(A_LEA, S_L, TmpRef,
                                            taicpu(p).oper[1]^.reg);
                              InsertLLItem(p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            end;
                        end
                      else
                        if (current_settings.optimizecputype < cpu_Pentium2) and
                           (taicpu(p).oper[0]^.typ = top_const) and
                           (taicpu(p).oper[1]^.typ = top_reg) then
                          if (taicpu(p).oper[0]^.val = 1) then
    {changes "shl $1, %reg" to "add %reg, %reg", which is the same on a 386,
    but faster on a 486, and Tairable in both U and V pipes on the Pentium
    (unlike shl, which is only Tairable in the U pipe)}
                            begin
                              hp1 := taicpu.Op_reg_reg(A_ADD,taicpu(p).opsize,
                                        taicpu(p).oper[1]^.reg, taicpu(p).oper[1]^.reg);
                              InsertLLItem(p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            end
                          else if (taicpu(p).opsize = S_L) and
                                  (taicpu(p).oper[0]^.val<= 3) then
                    {changes "shl $2, %reg" to "lea (,%reg,4), %reg"
                            "shl $3, %reg" to "lea (,%reg,8), %reg}
                              begin
                                reference_reset(tmpref,2,[]);
                                TmpRef.index := taicpu(p).oper[1]^.reg;
                                TmpRef.scalefactor := 1 shl taicpu(p).oper[0]^.val;
                                hp1 := taicpu.Op_ref_reg(A_LEA,S_L,TmpRef, taicpu(p).oper[1]^.reg);
                                InsertLLItem(p.previous, p.next, hp1);
                                p.free;
                                p := hp1;
                              end
                    end;
                  A_SETcc :
                    { changes
                        setcc (funcres)             setcc reg
                        movb (funcres), reg      to leave/ret
                        leave/ret                               }
                    begin
                      if (taicpu(p).oper[0]^.typ = top_ref) and
                         GetNextInstruction(p, hp1) and
                         GetNextInstruction(hp1, hp2) and
                         IsExitCode(hp2) and
                         (taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
                         (taicpu(p).oper[0]^.ref^.index = NR_NO) and
                         not(assigned(current_procinfo.procdef.funcretsym) and
                             (taicpu(p).oper[0]^.ref^.offset < tabstractnormalvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset)) and
                         (hp1.typ = ait_instruction) and
                         (taicpu(hp1).opcode = A_MOV) and
                         (taicpu(hp1).opsize = S_B) and
                         (taicpu(hp1).oper[0]^.typ = top_ref) and
                         RefsEqual(taicpu(hp1).oper[0]^.ref^, taicpu(p).oper[0]^.ref^) then
                        begin
                          taicpu(p).loadReg(0,taicpu(hp1).oper[1]^.reg);
                          DebugMsg('Peephole optimizer SetccMovbLeaveRet2SetccLeaveRet',p);
                          asml.remove(hp1);
                          hp1.free;
                        end
                    end;
                  A_SUB:
                    { * change "subl $2, %esp; pushw x" to "pushl x"}
                    { * change "sub/add const1, reg" or "dec reg" followed by
                        "sub const2, reg" to one "sub ..., reg" }
                    begin
                      if (taicpu(p).oper[0]^.typ = top_const) and
                         (taicpu(p).oper[1]^.typ = top_reg) then
                        if (taicpu(p).oper[0]^.val = 2) and
                           (taicpu(p).oper[1]^.reg = NR_ESP) and
                           { Don't do the sub/push optimization if the sub }
                           { comes from setting up the stack frame (JM)    }
                           (not getLastInstruction(p,hp1) or
                           (hp1.typ <> ait_instruction) or
                           (taicpu(hp1).opcode <> A_MOV) or
                           (taicpu(hp1).oper[0]^.typ <> top_reg) or
                           (taicpu(hp1).oper[0]^.reg <> NR_ESP) or
                           (taicpu(hp1).oper[1]^.typ <> top_reg) or
                           (taicpu(hp1).oper[1]^.reg <> NR_EBP)) then
                          begin
                            hp1 := tai(p.next);
                            while Assigned(hp1) and
                                  (tai(hp1).typ in [ait_instruction]+SkipInstr) and
                                  not RegReadByInstruction(NR_ESP,hp1) and
                                  not RegModifiedByInstruction(NR_ESP,hp1) do
                              hp1 := tai(hp1.next);
                            if Assigned(hp1) and
                               (tai(hp1).typ = ait_instruction) and
                               (taicpu(hp1).opcode = A_PUSH) and
                               (taicpu(hp1).opsize = S_W) then
                              begin
                                taicpu(hp1).changeopsize(S_L);
                                if taicpu(hp1).oper[0]^.typ=top_reg then
                                  setsubreg(taicpu(hp1).oper[0]^.reg,R_SUBWHOLE);
                                hp1 := tai(p.next);
                                asml.remove(p);
                                p.free;
                                p := hp1;
                                continue
                              end;
                            if DoSubAddOpt(p) then
                              continue;
                          end
                        else if DoSubAddOpt(p) then
                          continue
                    end;
                  A_VMOVAPS,
                  A_VMOVAPD:
                    if OptPass1VMOVAP(p) then
                      continue;
                  A_VDIVSD,
                  A_VDIVSS,
                  A_VSUBSD,
                  A_VSUBSS,
                  A_VMULSD,
                  A_VMULSS,
                  A_VADDSD,
                  A_VADDSS:
                    if OptPass1VOP(p) then
                      continue;
                end;
            end; { if is_jmp }
          end;
      end;
      updateUsedRegs(UsedRegs,p);
      p:=tai(p.next);
    end;
end;


procedure TCPUAsmOptimizer.PeepHoleOptPass2;

{$ifdef DEBUG_AOPTCPU}
  procedure DebugMsg(const s: string;p : tai);
    begin
      asml.insertbefore(tai_comment.Create(strpnew(s)), p);
    end;
{$else DEBUG_AOPTCPU}
  procedure DebugMsg(const s: string;p : tai);inline;
    begin
    end;
{$endif DEBUG_AOPTCPU}

var
  p,hp1,hp2,hp3: tai;
  l : longint;
  condition : tasmcond;
  carryadd_opcode: Tasmop;

begin
  p := BlockStart;
  ClearUsedRegs;
  while (p <> BlockEnd) Do
    begin
      UpdateUsedRegs(UsedRegs, tai(p.next));
      case p.Typ Of
        Ait_Instruction:
          begin
            if InsContainsSegRef(taicpu(p)) then
              begin
                p := tai(p.next);
                continue;
              end;
            case taicpu(p).opcode Of
              A_Jcc:
                if OptPass2Jcc(p) then
                  continue;
              A_FSTP,A_FISTP:
                if DoFpuLoadStoreOpt(p) then
                  continue;
              A_IMUL:
                if OptPass2Imul(p) then
                  continue;
              A_JMP:
                if OptPass2Jmp(p) then
                  continue;
              A_MOV:
                if OptPass2MOV(p) then
                  continue;
            end;
          end;
      end;
      p := tai(p.next)
    end;
end;


procedure TCPUAsmOptimizer.PostPeepHoleOpts;
var
  p,hp1,hp2: tai;
  IsTestConstX: boolean;
begin
  p := BlockStart;
  ClearUsedRegs;
  while (p <> BlockEnd) Do
    begin
      UpdateUsedRegs(UsedRegs, tai(p.next));
      case p.Typ Of
        Ait_Instruction:
          begin
            if InsContainsSegRef(taicpu(p)) then
              begin
                p := tai(p.next);
                continue;
              end;
            case taicpu(p).opcode Of
              A_CALL:
                begin
                  { don't do this on modern CPUs, this really hurts them due to
                    broken call/ret pairing }
                  if (current_settings.optimizecputype < cpu_Pentium2) and
                     not(cs_create_pic in current_settings.moduleswitches) and
                     GetNextInstruction(p, hp1) and
                     (hp1.typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_JMP) and
                     ((taicpu(hp1).oper[0]^.typ=top_ref) and (taicpu(hp1).oper[0]^.ref^.refaddr=addr_full)) then
                    begin
                      hp2 := taicpu.Op_sym(A_PUSH,S_L,taicpu(hp1).oper[0]^.ref^.symbol);
                      InsertLLItem(p.previous, p, hp2);
                      taicpu(p).opcode := A_JMP;
                      taicpu(p).is_jmp := true;
                      asml.remove(hp1);
                      hp1.free;
                    end
                  { replace
                      call   procname
                      ret
                    by
                      jmp    procname

                    this should never hurt except when pic is used, not sure
                    how to handle it then

                    but do it only on level 4 because it destroys stack back traces
                  }
                  else if (cs_opt_level4 in current_settings.optimizerswitches) and
                     not(cs_create_pic in current_settings.moduleswitches) and
                     GetNextInstruction(p, hp1) and
                     (hp1.typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_RET) and
                     (taicpu(hp1).ops=0) then
                    begin
                      taicpu(p).opcode := A_JMP;
                      taicpu(p).is_jmp := true;
                      asml.remove(hp1);
                      hp1.free;
                    end;
                end;
              A_CMP:
                begin
                  if (taicpu(p).oper[0]^.typ = top_const) and
                     (taicpu(p).oper[0]^.val = 0) and
                     (taicpu(p).oper[1]^.typ = top_reg) then
                   {change "cmp $0, %reg" to "test %reg, %reg"}
                    begin
                      taicpu(p).opcode := A_TEST;
                      taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                      continue;
                    end;
                end;
              A_MOV:
                PostPeepholeOptMov(p);
              A_MOVZX:
                { if register vars are on, it's possible there is code like }
                {   "cmpl $3,%eax; movzbl 8(%ebp),%ebx; je .Lxxx"           }
                { so we can't safely replace the movzx then with xor/mov,   }
                { since that would change the flags (JM)                    }
                if not(cs_opt_regvar in current_settings.optimizerswitches) then
                 begin
                  if (taicpu(p).oper[1]^.typ = top_reg) then
                    if (taicpu(p).oper[0]^.typ = top_reg)
                      then
                        case taicpu(p).opsize of
                          S_BL:
                            begin
                              if IsGP32Reg(taicpu(p).oper[1]^.reg) and
                                 not(cs_opt_size in current_settings.optimizerswitches) and
                                 (current_settings.optimizecputype = cpu_Pentium) then
                                  {Change "movzbl %reg1, %reg2" to
                                   "xorl %reg2, %reg2; movb %reg1, %reg2" for Pentium and
                                   PentiumMMX}
                                begin
                                  hp1 := taicpu.op_reg_reg(A_XOR, S_L,
                                              taicpu(p).oper[1]^.reg, taicpu(p).oper[1]^.reg);
                                  InsertLLItem(p.previous, p, hp1);
                                  taicpu(p).opcode := A_MOV;
                                  taicpu(p).changeopsize(S_B);
                                  setsubreg(taicpu(p).oper[1]^.reg,R_SUBL);
                                end;
                            end;
                        end
                      else if (taicpu(p).oper[0]^.typ = top_ref) and
                          (taicpu(p).oper[0]^.ref^.base <> taicpu(p).oper[1]^.reg) and
                          (taicpu(p).oper[0]^.ref^.index <> taicpu(p).oper[1]^.reg) and
                          not(cs_opt_size in current_settings.optimizerswitches) and
                          IsGP32Reg(taicpu(p).oper[1]^.reg) and
                          (current_settings.optimizecputype = cpu_Pentium) and
                          (taicpu(p).opsize = S_BL) then
                        {changes "movzbl mem, %reg" to "xorl %reg, %reg; movb mem, %reg8" for
                          Pentium and PentiumMMX}
                        begin
                          hp1 := taicpu.Op_reg_reg(A_XOR, S_L, taicpu(p).oper[1]^.reg,
                                      taicpu(p).oper[1]^.reg);
                          taicpu(p).opcode := A_MOV;
                          taicpu(p).changeopsize(S_B);
                          setsubreg(taicpu(p).oper[1]^.reg,R_SUBL);
                          InsertLLItem(p.previous, p, hp1);
                        end;
                 end;
              A_TEST, A_OR:
                {removes the line marked with (x) from the sequence
                 and/or/xor/add/sub/... $x, %y
                 test/or %y, %y  | test $-1, %y    (x)
                 j(n)z _Label
                    as the first instruction already adjusts the ZF
                    %y operand may also be a reference }
                 begin
                   IsTestConstX:=(taicpu(p).opcode=A_TEST) and
                     MatchOperand(taicpu(p).oper[0]^,-1);
                   if (OpsEqual(taicpu(p).oper[0]^,taicpu(p).oper[1]^) or IsTestConstX) and
                      GetLastInstruction(p, hp1) and
                      (tai(hp1).typ = ait_instruction) and
                      GetNextInstruction(p,hp2) and
                      MatchInstruction(hp2,A_SETcc,A_Jcc,A_CMOVcc,[]) then
                     case taicpu(hp1).opcode Of
                       A_ADD, A_SUB, A_OR, A_XOR, A_AND:
                         begin
                           if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[1]^) and
                             { does not work in case of overflow for G(E)/L(E)/C_O/C_NO }
                             { and in case of carry for A(E)/B(E)/C/NC                  }
                              ((taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) or
                               ((taicpu(hp1).opcode <> A_ADD) and
                                (taicpu(hp1).opcode <> A_SUB))) then
                             begin
                               hp1 := tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := tai(hp1);
                               continue
                             end;
                         end;
                       A_SHL, A_SAL, A_SHR, A_SAR:
                         begin
                           if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[1]^) and
                             { SHL/SAL/SHR/SAR with a value of 0 do not change the flags }
                             { therefore, it's only safe to do this optimization for     }
                             { shifts by a (nonzero) constant                            }
                              (taicpu(hp1).oper[0]^.typ = top_const) and
                              (taicpu(hp1).oper[0]^.val <> 0) and
                             { does not work in case of overflow for G(E)/L(E)/C_O/C_NO }
                             { and in case of carry for A(E)/B(E)/C/NC                  }
                              (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) then
                             begin
                               hp1 := tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := tai(hp1);
                               continue
                             end;
                         end;
                       A_DEC, A_INC, A_NEG:
                         begin
                           if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) and
                             { does not work in case of overflow for G(E)/L(E)/C_O/C_NO }
                             { and in case of carry for A(E)/B(E)/C/NC                  }
                             (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) then
                             begin
                               case taicpu(hp1).opcode Of
                                 A_DEC, A_INC:
 {replace inc/dec with add/sub 1, because inc/dec doesn't set the carry flag}
                                   begin
                                     case taicpu(hp1).opcode Of
                                       A_DEC: taicpu(hp1).opcode := A_SUB;
                                       A_INC: taicpu(hp1).opcode := A_ADD;
                                     end;
                                     taicpu(hp1).loadoper(1,taicpu(hp1).oper[0]^);
                                     taicpu(hp1).loadConst(0,1);
                                     taicpu(hp1).ops:=2;
                                   end
                                 end;
                               hp1 := tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := tai(hp1);
                               continue
                             end;
                         end
                     else
                       { change "test  $-1,%reg" into "test %reg,%reg" }
                       if IsTestConstX and (taicpu(p).oper[1]^.typ=top_reg) then
                         taicpu(p).loadoper(0,taicpu(p).oper[1]^);
                     end { case }
                   else
                     { change "test  $-1,%reg" into "test %reg,%reg" }
                     if IsTestConstX and (taicpu(p).oper[1]^.typ=top_reg) then
                       taicpu(p).loadoper(0,taicpu(p).oper[1]^);
                 end;
            end;
          end;
      end;
      p := tai(p.next)
    end;
end;


Procedure TCpuAsmOptimizer.Optimize;
Var
  HP: Tai;
  pass: longint;
  slowopt, changed, lastLoop: boolean;
Begin
  slowopt := (cs_opt_level3 in current_settings.optimizerswitches);
  pass := 0;
  changed := false;
  repeat
     lastLoop :=
       not(slowopt) or
       (not changed and (pass > 2)) or
      { prevent endless loops }
       (pass = 4);
     changed := false;
   { Setup labeltable, always necessary }
     blockstart := tai(asml.first);
     pass_1;
   { Blockend now either contains an ait_marker with Kind = mark_AsmBlockStart, }
   { or nil                                                                }
     While Assigned(BlockStart) Do
       Begin
         if (cs_opt_peephole in current_settings.optimizerswitches) then
           begin
            if (pass = 0) then
              PrePeepHoleOpts;
              { Peephole optimizations }
               PeepHoleOptPass1;
              { Only perform them twice in the first pass }
               if pass = 0 then
                 PeepHoleOptPass1;
           end;
        { More peephole optimizations }
         if (cs_opt_peephole in current_settings.optimizerswitches) then
           begin
             PeepHoleOptPass2;
             if lastLoop then
               PostPeepHoleOpts;
           end;

        { Continue where we left off, BlockEnd is either the start of an }
        { assembler block or nil                                         }
         BlockStart := BlockEnd;
         While Assigned(BlockStart) And
               (BlockStart.typ = ait_Marker) And
               (Tai_Marker(BlockStart).Kind = mark_AsmBlockStart) Do
           Begin
           { We stopped at an assembler block, so skip it }
            Repeat
              BlockStart := Tai(BlockStart.Next);
            Until (BlockStart.Typ = Ait_Marker) And
                  (Tai_Marker(Blockstart).Kind = mark_AsmBlockEnd);
           { Blockstart now contains a Tai_marker(mark_AsmBlockEnd) }
             If GetNextInstruction(BlockStart, HP) And
                ((HP.typ <> ait_Marker) Or
                 (Tai_Marker(HP).Kind <> mark_AsmBlockStart)) Then
             { There is no assembler block anymore after the current one, so }
             { optimize the next block of "normal" instructions              }
               pass_1
             { Otherwise, skip the next assembler block }
             else
               blockStart := hp;
           End;
       End;
     inc(pass);
  until lastLoop;
  dfa.free;

End;


begin
  casmoptimizer:=TCpuAsmOptimizer;
end.

