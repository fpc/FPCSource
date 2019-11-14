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
      cpubase, aopt, aoptx86,
      Aasmbase,aasmtai,aasmdata;

    Type
      TCpuAsmOptimizer = class(TX86AsmOptimizer)
        function PrePeepHoleOptsCpu(var p: tai): boolean; override;
        function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
        function PeepHoleOptPass2Cpu(var p: tai): boolean; override;
        function PostPeepHoleOptsCpu(var p : tai) : boolean; override;

        procedure PostPeepHoleOpts; override;
      end;

    Var
      AsmOptimizer : TCpuAsmOptimizer;

  Implementation

    uses
      verbose,globtype,globals,
      cpuinfo,
      aasmcpu,
      aoptutils,
      aasmcfi,
      procinfo,
      cgutils,
      { units we should get rid off: }
      symsym,symconst;


    { Checks if the register is a 32 bit general purpose register }
    function isgp32reg(reg: TRegister): boolean;
      begin
        {$push}{$warnings off}
        isgp32reg:=(getregtype(reg)=R_INTREGISTER) and (getsupreg(reg)>=RS_EAX) and (getsupreg(reg)<=RS_EBX);
        {$pop}
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


    function TCPUAsmOPtimizer.PrePeepHoleOptsCpu(var p: tai): boolean;
      begin
        Result:=False;
        case p.typ of
          ait_instruction:
            begin
              if InsContainsSegRef(taicpu(p)) then
                begin
                  p := tai(p.next);
                  Result:=true;
                end;
              case taicpu(p).opcode Of
                A_IMUL:
                  Result:=PrePeepholeOptIMUL(p);
                A_SAR,A_SHR:
                  Result:=PrePeepholeOptSxx(p);
                A_XOR:
                  begin
                    if (taicpu(p).oper[0]^.typ = top_reg) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                     { temporarily change this to 'mov reg,0' to make it easier }
                     { for the CSE. Will be changed back in pass 2              }
                      begin
                        taicpu(p).opcode := A_MOV;
                        taicpu(p).loadConst(0,0);
                        Result:=true;
                      end;
                  end;
                else
                  ;
              end;
          end;
        else
          ;
        end;
      end;


    function TCPUAsmOPtimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;

      function WriteOk : Boolean;
        begin
          writeln('Ok');
          Result:=True;
        end;

      var
        hp1,hp2 : tai;
        hp3,hp4: tai;
        v:aint;

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

      begin
        result:=False;
        case p.Typ Of
          ait_instruction:
            begin
              current_filepos:=taicpu(p).fileinfo;
              if InsContainsSegRef(taicpu(p)) then
                begin
                  p:=tai(p.next);
                  Result:=true;
                  exit;
                end;
              case taicpu(p).opcode Of
                A_AND:
                  Result:=OptPass1And(p);
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
                        Result:=true;
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
                          else
                            internalerror(2019050903);
                        end;
                        asml.remove(p);
                        asml.remove(hp1);
                        p.free;
                        hp1.free;
                        p := hp2;
                        Result:=true;
                      end
                  end;
                A_FLD:
                  Result:=OptPass1FLD(p);
                A_FSTP,A_FISTP:
                  Result:=OptPass1FSTP(p);
                A_LEA:
                  Result:=OptPass1LEA(p);
                A_MOV:
                  Result:=OptPass1MOV(p);
                A_MOVSX,
                A_MOVZX :
                  Result:=OptPass1Movx(p);
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
                        Result:=true;
                      end;
                  end;
                A_SHL, A_SAL:
                  Result:=OptPass1SHLSAL(p);
                A_SUB:
                  Result:=OptPass1Sub(p);
                A_MOVAPD,
                A_MOVAPS,
                A_MOVUPD,
                A_MOVUPS,
                A_VMOVAPS,
                A_VMOVAPD,
                A_VMOVUPS,
                A_VMOVUPD:
                  Result:=OptPass1_V_MOVAP(p);
                A_VDIVSD,
                A_VDIVSS,
                A_VSUBSD,
                A_VSUBSS,
                A_VMULSD,
                A_VMULSS,
                A_VADDSD,
                A_VADDSS,
                A_VANDPD,
                A_VANDPS,
                A_VORPD,
                A_VORPS,
                A_VXORPD,
                A_VXORPS:
                  Result:=OptPass1VOP(p);
                A_MULSD,
                A_MULSS,
                A_ADDSD,
                A_ADDSS:
                  Result:=OptPass1OP(p);
                A_VMOVSD,
                A_VMOVSS,
                A_MOVSD,
                A_MOVSS:
                  Result:=OptPass1MOVXX(p);
                A_SETcc:
                  Result:=OptPass1SETcc(p);
                else
                  ;
              end;
            end;
          else
            ;
        end;
      end;


    function TCPUAsmOptimizer.PeepHoleOptPass2Cpu(var p: tai): boolean;
      begin
        Result:=false;
        case p.Typ Of
          Ait_Instruction:
            begin
              if InsContainsSegRef(taicpu(p)) then
                exit;
              case taicpu(p).opcode Of
                A_Jcc:
                  Result:=OptPass2Jcc(p);
                A_Lea:
                  Result:=OptPass2Lea(p);
                A_FSTP,A_FISTP:
                  Result:=OptPass1FSTP(p);
                A_IMUL:
                  Result:=OptPass2Imul(p);
                A_JMP:
                  Result:=OptPass2Jmp(p);
                A_MOV:
                  Result:=OptPass2MOV(p);
                else
                  ;
              end;
            end;
          else
            ;
        end;
      end;


    function TCPUAsmOptimizer.PostPeepHoleOptsCpu(var p : tai) : boolean;
      var
        hp1: tai;
      begin
        Result:=false;
        case p.Typ Of
          Ait_Instruction:
            begin
              if InsContainsSegRef(taicpu(p)) then
                Exit;
              case taicpu(p).opcode Of
                A_CALL:
                  Result:=PostPeepHoleOptCall(p);
                A_LEA:
                  Result:=PostPeepholeOptLea(p);
                A_CMP:
                  Result:=PostPeepholeOptCmp(p);
                A_MOV:
                  Result:=PostPeepholeOptMov(p);
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
                              else
                                ;
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
                  Result:=PostPeepholeOptTestOr(p);
                else
                  ;
              end;
            end;
          else
            ;
        end;
      end;


    procedure TCpuAsmOptimizer.PostPeepHoleOpts;
      begin
        inherited;
        OptReferences;
      end;


begin
  casmoptimizer:=TCpuAsmOptimizer;
end.

