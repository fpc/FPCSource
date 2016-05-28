{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the SPC32 optimizer object

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

uses cpubase, cgbase, aasmtai, aopt, aoptcpub;

Type

  { TCpuAsmOptimizer }

  TCpuAsmOptimizer = class(TAsmOptimizer)
  private
    function SkipMarkers(var p: tai): boolean;
  public
    Function GetNextInstructionUsingReg(Current: tai; Var Next: tai;reg : TRegister): Boolean;

    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;

    procedure DebugMsg(const s: string; p: tai);
  End;

Implementation

  uses
    cpuinfo,
    aasmbase,aasmcpu,
    globals,globtype,
    cutils;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function TCpuAsmOptimizer.SkipMarkers(var p: tai): boolean;
    begin
      result:=assigned(p);
      while result and
            assigned(p) and
            (p.typ in [ait_marker,ait_force_line]) do
        begin
          p:=tai(p.next);
          result:=assigned(p);
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


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1,hp2,hp3: tai;
      alloc, dealloc: tai_regalloc;
      i: integer;
    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_LD:
                begin
                  {
                    turn
                      ld rx
                      st rx
                    into
                      ld rx
                  }
                  if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_ST) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) then
                    begin
                      DebugMsg('LdSt2Ld', p);

                      AsmL.Remove(hp1);
                      hp1.free;

                      result:=true;
                    end
                  {
                    turn
                      ld *
                      ld rx
                    into
                      ld rx
                  }
                  else if (taicpu(p).ops=1) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_LD,A_NUL,A_GS]) then
                    begin
                      DebugMsg('LdLd2Ld', p);

                      AsmL.Remove(p);
                      p.free;
                      p:=hp1;

                      result:=true;
                    end
                  {
                    turn
                      ld rx
                      ldX #0
                    into
                      nul
                      ldX rx
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_LDB,A_LDH,A_LDW]) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_const) and
                     (taicpu(hp1).oper[0]^.val=0) then
                    begin
                      DebugMsg('LdLdx2Ldx', p);

                      taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);

                      taicpu(p).ops:=0;
                      taicpu(p).opcode:=A_NUL;

                      result:=true;
                    end
                  {
                    turn
                      ld rx
                      alloc ry
                      st ry
                      ...
                      op ry
                      dealloc ry
                    into
                      ld rx
                      ...
                      op rx
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_ST) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     GetNextInstructionUsingReg(hp1,hp2,taicpu(hp1).oper[0]^.reg) and
                     (hp2.typ=ait_instruction) and
                     (taicpu(hp2).ops=1) and
                     (taicpu(hp2).opcode<>A_ST) and
                     (taicpu(hp2).oper[0]^.typ=top_reg) and
                     (taicpu(hp2).oper[0]^.reg=taicpu(hp1).oper[0]^.reg) and
                     assigned(FindRegDeAlloc(taicpu(hp1).oper[0]^.reg,tai(hp2.Next))) and
                     (not RegModifiedBetween(taicpu(p).oper[0]^.reg,p,hp2)) then
                    begin
                      DebugMsg('LdStOp2LdOp', p);

                      alloc:=FindRegAllocBackward(taicpu(hp1).oper[0]^.reg, tai(hp1.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(hp1).oper[0]^.reg,tai(hp2.next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.free;

                          asml.Remove(dealloc);
                          dealloc.free;
                        end;

                      AsmL.Remove(hp1);
                      hp1.free;

                      taicpu(hp2).oper[0]^.reg:=taicpu(p).oper[0]^.reg;

                      result:=true;
                    end
                  {
                    turn
                      ld rx
                      stX *
                      ld rx
                    into
                      ld rx
                      stX *
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_STB,A_STH,A_STW]) and
                     (taicpu(hp1).ops=1) and
                     GetNextInstruction(hp1,hp2) and
                     (hp2.typ=ait_instruction) and
                     (taicpu(hp2).opcode=A_LD) and
                     (taicpu(hp2).ops=1) and
                     (taicpu(hp2).oper[0]^.typ=top_reg) and
                     (taicpu(hp2).oper[0]^.reg=taicpu(p).oper[0]^.reg) then
                    begin
                      DebugMsg('LdStxLd2LdStx', p);

                      asml.remove(hp2);
                      hp2.free;

                      result:=true;
                    end
                  {
                    turn
                      ld #x
                      ldu #y
                      ldX #0
                    into
                      nul
                      ldu #y+$8000
                      ldX #x
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_ref) and
                     (taicpu(p).oper[0]^.ref^.refaddr=addr_lo16) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_LDU]) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_ref) and
                     (taicpu(hp1).oper[0]^.ref^.refaddr=addr_hi16) and
                     GetNextInstruction(hp1,hp2) and
                     (hp2.typ=ait_instruction) and
                     (taicpu(hp2).opcode in [A_LDB,A_LDH,A_LDW]) and
                     (taicpu(hp2).ops=1) and
                     (taicpu(hp2).oper[0]^.typ=top_const) and
                     (taicpu(hp2).oper[0]^.val=0) then
                    begin
                      DebugMsg('LdLduLdx2NulLduLdx', p);

                      inc(taicpu(hp1).oper[0]^.ref^.offset, $8000);

                      taicpu(hp2).loadref(0, taicpu(p).oper[0]^.ref^);

                      taicpu(p).ops:=0;
                      taicpu(p).opcode:=A_NUL;

                      result:=true;
                    end
                  {
                    turn
                      ld 0
                    into
                      nul
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_const) and
                     (taicpu(p).oper[0]^.val=0) then
                    begin
                      DebugMsg('Ld2Nul', p);

                      taicpu(p).ops:=0;
                      taicpu(p).opcode:=A_NUL;

                      result:=true;
                    end
                  {
                    turn
                      ld rX
                      ss 6
                      ...
                      gs 6
                    into
                      ...
                      ld rX
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_SS) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_const) and
                     (taicpu(hp1).oper[0]^.val=6) then
                    begin
                      hp2:=hp1;

                      for i:=0 to 4 do
                        begin
                          if not GetNextInstruction(hp2,hp2) then
                            exit;

                          if (hp2.typ=ait_instruction) and
                             taicpu(hp2).is_jmp then
                            exit;

                          if (hp2.typ=ait_instruction) and
                             (taicpu(hp2).opcode=A_GS) and
                             (taicpu(hp2).ops=1) and
                             (taicpu(hp2).oper[0]^.typ=top_const) and
                             (taicpu(hp2).oper[0]^.val=6) then
                            begin
                              DebugMsg('LdSS6_2_...Ld', p);

                              if RegModifiedBetween(taicpu(p).oper[0]^.reg,hp1,hp2) then
                                exit;

                              asml.Remove(p);
                              asml.InsertAfter(p,hp2);

                              asml.Remove(hp2);
                              hp2.Free;

                              GetNextInstruction(hp1,p);

                              asml.Remove(hp1);
                              hp1.Free;

                              result:=true;

                              break;
                            end;
                        end;
                    end;
                end;
              A_ST:
                begin
                  {
                    turn
                      st rx
                      ld rx
                      dealloc rx
                    into
                      ...
                  }
                  if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     SkipMarkers(hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_LD) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) and
                     assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.next))) then
                    begin
                      DebugMsg('StLd2*', p);

                      alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg, tai(p.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.free;

                          asml.Remove(dealloc);
                          dealloc.free;
                        end;

                      AsmL.Remove(hp1);
                      hp1.free;

                      GetNextInstruction(p,hp1);
                      AsmL.Remove(p);
                      p.free;
                      p:=hp1;

                      result:=true;
                    end
                  {
                    turn
                      st rx
                      ld rx
                    into
                      st rx
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     SkipMarkers(hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_LD) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) then
                    begin
                      DebugMsg('StLd2St', p);

                      AsmL.Remove(hp1);
                      hp1.free;

                      result:=true;
                    end
                  {
                    turn
                      st rx
                      st rx
                    into
                      st rx
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     SkipMarkers(hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_ST) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) then
                    begin
                      DebugMsg('StSt2St', p);

                      AsmL.Remove(hp1);
                      hp1.free;

                      result:=true;
                    end
                  {
                    turn
                      st rx
                      ...
                      st rx
                    into
                      st rx
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
                     SkipMarkers(hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_ST) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) and
                     (not RegModifiedBetween(taicpu(p).oper[0]^.reg,p,hp1)) and
                     (not RegUsedBetween(taicpu(p).oper[0]^.reg,p,hp1)) then
                    begin
                      DebugMsg('St...St2...St', p);

                      GetNextInstruction(p,hp1);
                      AsmL.Remove(p);
                      p.free;
                      p:=hp1;

                      result:=true;
                    end
                  {
                    turn
                      st rx
                      nul
                      ldw rx
                      dealloc rx
                    into
                      ldw 0
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_NUL) and
                     GetNextInstruction(hp1,hp2) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp2).opcode in [A_LDW,A_LDH,A_LDB]) and
                     (taicpu(hp2).ops=1) and
                     (taicpu(hp2).oper[0]^.typ=top_reg) and
                     (taicpu(hp2).oper[0]^.reg=taicpu(p).oper[0]^.reg) and
                     Assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp2.next))) then
                    begin
                      DebugMsg('StNulLdx2Ldx', p);

                      alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg, tai(p.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp2.next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.free;

                          asml.Remove(dealloc);
                          dealloc.free;
                        end;

                      AsmL.Remove(p);
                      p.free;

                      AsmL.Remove(hp1);
                      hp1.free;

                      taicpu(hp2).loadconst(0,0);
                      p:=taicpu(hp2);

                      if p.Previous<>nil then
                        p:=tai(p.Previous); // try to enable some extra optimizations

                      result:=true;
                    end
                  {
                    turn
                      st rx
                      ld ry
                      [dealloc ry]
                      op rx
                      dealloc rx
                    into
                      op ry
                      [dealloc ry]
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     SkipMarkers(hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_LD) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     GetNextInstruction(hp1,hp2) and
                     SkipMarkers(hp2) and
                     (hp2.typ=ait_instruction) and
                     (taicpu(hp2).opcode in [A_ADD,A_ADC,A_MUL,
                                             A_AND,A_ORR,A_XOR]) and
                     (taicpu(hp2).ops=1) and
                     (taicpu(hp2).oper[0]^.typ=top_reg) and
                     (taicpu(hp2).oper[0]^.reg=taicpu(p).oper[0]^.reg) and
                     assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp2.next))) then
                    begin
                      DebugMsg('StLdOp2Op', p);

                      dealloc:=FindRegDeAlloc(taicpu(hp1).oper[0]^.reg,tai(hp1.next));
                      if assigned(dealloc) then
                        begin
                          asml.Remove(dealloc);
                          asml.InsertAfter(dealloc,hp2);
                        end;

                      alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg, tai(p.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp2.next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.free;

                          asml.Remove(dealloc);
                          dealloc.free;
                        end;

                      taicpu(hp2).oper[0]^.reg:=taicpu(hp1).oper[0]^.reg;

                      AsmL.Remove(hp1);
                      hp1.free;

                      AsmL.Remove(p);
                      p.free;

                      p:=hp2;

                      result:=true;
                    end;
                end;
              A_GS:
                begin
                  {
                    turn
                      gs pc
                      j...
                      gs pc
                      ...
                    into
                      gs pc
                      j...
                      ...
                  }
                  if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_const) and
                     (taicpu(p).oper[0]^.val=SS_PC) then
                    begin
                      hp1:=p;

                      {while GetNextInstruction(hp1,hp2) and
                            (hp2.typ=ait_instruction) and
                            (taicpu(hp2).is_jmp) do
                        begin
                          if GetNextInstruction(hp2,hp3) and
                             (hp3.typ=ait_instruction) and
                             (taicpu(hp3).opcode=A_GS) and
                             (taicpu(hp3).ops=1) and
                             (taicpu(hp3).oper[0]^.typ=top_const) and
                             (taicpu(hp3).oper[0]^.val=SS_PC) then
                            begin
                              DebugMsg('GsJGs2GsJ', hp1);

                              asml.Remove(hp3);
                              hp3.free;



                              result:=true;
                            end
                          else
                            break;

                          hp1:=hp2;
                        end;}
                    end;
                end;
              A_SS:
                begin
                  {
                    turn
                      ss 6
                      ...
                      gs 6
                      ld rX
                    into
                      ...
                      ld rX
                  }
                  if (taicpu(p).opcode=A_SS) and
                     (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_const) and
                     (taicpu(p).oper[0]^.val=6) then
                    begin
                      hp1:=p;
                      hp2:=p;

                      for i:=0 to 4 do
                        begin
                          if not GetNextInstruction(hp2,hp2) then
                            exit;

                          if (hp2.typ=ait_instruction) and
                             taicpu(hp2).is_jmp then
                            exit;

                          if (hp2.typ=ait_instruction) and
                             (taicpu(hp2).opcode=A_GS) and
                             (taicpu(hp2).ops=1) and
                             (taicpu(hp2).oper[0]^.typ=top_const) and
                             (taicpu(hp2).oper[0]^.val=6) then
                            begin
                              if not GetNextInstruction(hp2,p) then
                                exit;

                              if (p.typ=ait_instruction) and
                                 (taicpu(p).opcode in [A_LD,A_NUL,A_GS]) then
                                begin
                                  DebugMsg('Ss6 Removal', p);

                                  GetNextInstruction(hp1,p);

                                  asml.Remove(hp2);
                                  hp2.Free;

                                  asml.Remove(hp1);
                                  hp1.Free;

                                  result:=true;
                                end;

                              break;

                              {if RegModifiedBetween(taicpu(p).oper[0]^.reg,hp1,hp2) then
                                exit;

                              asml.Remove(p);
                              asml.InsertAfter(p,hp2);

                              asml.Remove(hp2);
                              hp2.Free;

                              GetNextInstruction(hp1,p);

                              asml.Remove(hp1);
                              hp1.Free;

                              break;}
                            end;
                        end;
                    end;
                end;
              A_ADD:
                begin
                  if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (taicpu(hp1).opcode in [A_LDW,A_LDH,A_LDB]) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_const) and
                     (taicpu(hp1).oper[0]^.val=0) then
                    begin
                      DebugMsg('AddLdw2Ldw', p);

                      taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);

                      asml.remove(p);
                      p.free;

                      p:=hp1;

                      result:=true;
                    end;
                end;
              A_MOV:
                begin
                  {
                    turn
                      mov rx,ry
                    info
                      ld ry
                      st rx
                  }

                  dealloc:=FindRegDeAlloc(taicpu(p).oper[1]^.reg,tai(p.next));

                  hp1:=taicpu.op_reg(A_ST,taicpu(p).oper[0]^.reg);

                  if assigned(dealloc) then
                    AsmL.InsertAfter(hp1, dealloc)
                  else
                    AsmL.InsertAfter(hp1, p);

                  taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                  taicpu(p).ops:=1;
                  taicpu(p).opcode:=A_LD;

                  result:=true;
                end;
              A_NUL:
                begin
                  {
                    turn
                      nul
                      alloc ry
                      st ry
                      ...
                      op ry
                      dealloc ry
                    into
                      ...
                      op 0
                  }
                  if GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_ST) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     GetNextInstructionUsingReg(hp1,hp2,taicpu(hp1).oper[0]^.reg) and
                     (hp2.typ=ait_instruction) and
                     (taicpu(hp2).ops=1) and
                     (taicpu(hp2).opcode<>A_ST) and
                     (taicpu(hp2).oper[0]^.typ=top_reg) and
                     (taicpu(hp2).oper[0]^.reg=taicpu(hp1).oper[0]^.reg) and
                     assigned(FindRegDeAlloc(taicpu(hp1).oper[0]^.reg,tai(hp2.Next))) then
                    begin
                      alloc:=FindRegAllocBackward(taicpu(hp1).oper[0]^.reg, tai(hp1.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(hp1).oper[0]^.reg,tai(hp2.next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.free;

                          asml.Remove(dealloc);
                          dealloc.free;
                        end;

                      DebugMsg('NulStOp2Op0', p);

                      AsmL.Remove(hp1);
                      hp1.free;

                      taicpu(hp2).loadconst(0,0);

                      result:=true;
                    end
                  {
                    turn
                      nul
                      ld/nul
                    into
                      ld/nul
                  }
                  else if GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_NUL,A_LD]) then
                    begin

                      DebugMsg('Dup nul removed', p);

                      asml.remove(p);
                      p.free;
                      p:=hp1;

                      result:=true;
                    end
                  {
                    turn
                      nul
                      add *
                    into
                      ld *
                  }
                  else if GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_ADD]) then
                    begin
                      DebugMsg('NulAdd2Ld', p);

                      asml.remove(p);
                      p.free;
                      p:=hp1;

                      taicpu(p).opcode:=A_LD;

                      result:=true;
                    end
                  {
                    turn
                      nul
                      sub #x
                    into
                      ld #x
                  }
                  else if GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_SUB]) and
                     (taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ=top_const) and
                     (abs(taicpu(hp1).oper[0]^.val)<$8000) then
                    begin
                      DebugMsg('NulSub2Ld', p);

                      asml.remove(p);
                      p.free;
                      p:=hp1;

                      taicpu(p).opcode:=A_LD;
                      taicpu(p).oper[0]^.val:=-taicpu(p).oper[0]^.val;

                      result:=true;
                    end;
                end;
              A_LOAD:
                begin
                  {
                    ss 6
                    ld ref.offset
                    add ref.index
                    ldw ref.base
                    st reg
                    gs 6

                    ss 6
                    ld ref.offset
                    ldw ref.base
                    st reg
                    gs 6
                  }
                  if GetNextInstruction(p,hp1) and
                     (tai(hp1).typ=ait_instruction) and
                     (not (taicpu(hp1).opcode in [A_LD,A_NUL,A_GS])) then
                    begin
                      AsmL.InsertBefore(taicpu.op_const(A_SS,6),p);
                      AsmL.InsertAfter(taicpu.op_const(A_GS,6),p);
                    end;

                  AsmL.InsertAfter(taicpu.op_reg(A_ST,taicpu(p).oper[0]^.reg),p);
                  AsmL.InsertAfter(taicpu.op_reg(A_LDW,taicpu(p).oper[1]^.ref^.base),p);

                  if taicpu(p).oper[1]^.ref^.index<>NR_NO then
                    AsmL.InsertAfter(taicpu.op_reg(A_ADD,taicpu(p).oper[1]^.ref^.index),p);

                  taicpu(p).opcode:=A_LD;
                  taicpu(p).loadconst(0, taicpu(p).oper[1]^.ref^.offset);
                  taicpu(p).ops:=1;

                  result:=true;
                end;
              A_STORE:
                begin
                  {
                    ss 6
                    ld ref.offset
                    add ref.index
                    add ref.base
                    stw reg
                    gs 6
                  }
                  if GetNextInstruction(p,hp1) and
                     (tai(hp1).typ=ait_instruction) and
                     (not (taicpu(hp1).opcode in [A_LD,A_NUL,A_GS])) then
                    begin
                      AsmL.InsertBefore(taicpu.op_const(A_SS,6),p);
                      AsmL.InsertAfter(taicpu.op_const(A_GS,6),p);
                    end;
                  //AsmL.InsertBefore(taicpu.op_const(A_SS,6),p);
                  //AsmL.InsertAfter(taicpu.op_const(A_GS,6),p);

                  AsmL.InsertAfter(taicpu.op_reg(A_STW,taicpu(p).oper[0]^.reg),p);
                  AsmL.InsertAfter(taicpu.op_reg(A_ADD,taicpu(p).oper[1]^.ref^.base),p);

                  if taicpu(p).oper[1]^.ref^.index<>NR_NO then
                    AsmL.InsertAfter(taicpu.op_reg(A_ADD,taicpu(p).oper[1]^.ref^.index),p);

                  taicpu(p).opcode:=A_LD;
                  taicpu(p).loadconst(0, taicpu(p).oper[1]^.ref^.offset);
                  taicpu(p).ops:=1;

                  result:=true;
                end;
            end;
          end;
      end;
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    begin
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
