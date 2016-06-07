{
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    This unit contains the peephole optimizer.

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
unit aoptx86;

{$i fpcdefs.inc}

{ $define DEBUG_AOPTCPU}

  interface

    uses
      globtype,
      cpubase,
      aasmtai,aasmcpu,
      cgbase,cgutils,
      aopt,aoptobj;

    type
      TX86AsmOptimizer = class(TAsmOptimizer)
        function RegLoadedWithNewValue(reg : tregister; hp : tai) : boolean; override;
      protected
        procedure PostPeepholeOptMov(const p : tai);

        function OptPass1AND(var p : tai) : boolean;
        function OptPass1VMOVAP(var p : tai) : boolean;
        function OptPass1VOP(const p : tai) : boolean;
        function OptPass1MOV(var p : tai) : boolean;

        function OptPass2MOV(var p : tai) : boolean;

        procedure DebugMsg(const s : string; p : tai);inline;

        procedure AllocRegBetween(reg : tregister; p1,p2 : tai;var initialusedregs : TAllUsedRegs);
        class function IsExitCode(p : tai) : boolean;
        class function isFoldableArithOp(hp1 : taicpu; reg : tregister) : boolean;
        procedure RemoveLastDeallocForFuncRes(p : tai);
      end;

    function MatchInstruction(const instr: tai; const op: TAsmOp; const opsize: topsizes): boolean;
    function MatchInstruction(const instr: tai; const op1,op2: TAsmOp; const opsize: topsizes): boolean;
    function MatchInstruction(const instr: tai; const op1,op2,op3: TAsmOp; const opsize: topsizes): boolean;
    function MatchInstruction(const instr: tai; const ops: array of TAsmOp; const opsize: topsizes): boolean;

    function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
    function MatchOperand(const oper: TOper; const a: tcgint): boolean; inline;
    function MatchOperand(const oper1: TOper; const oper2: TOper): boolean;

    function RefsEqual(const r1, r2: treference): boolean;

    function MatchReference(const ref : treference;base,index : TRegister) : Boolean;

    function MatchOpType(const instr : tai;ot0,ot1 : toptype) : Boolean;

  implementation

    uses
      cutils,
      verbose,
      procinfo,
      symconst,symsym,
      itcpugas;

    function MatchInstruction(const instr: tai; const op: TAsmOp; const opsize: topsizes): boolean;
      begin
        result :=
          (instr.typ = ait_instruction) and
          (taicpu(instr).opcode = op) and
          ((opsize = []) or (taicpu(instr).opsize in opsize));
      end;


    function MatchInstruction(const instr: tai; const op1,op2: TAsmOp; const opsize: topsizes): boolean;
      begin
        result :=
          (instr.typ = ait_instruction) and
          ((taicpu(instr).opcode = op1) or
           (taicpu(instr).opcode = op2)
          ) and
          ((opsize = []) or (taicpu(instr).opsize in opsize));
      end;


    function MatchInstruction(const instr: tai; const op1,op2,op3: TAsmOp; const opsize: topsizes): boolean;
      begin
        result :=
          (instr.typ = ait_instruction) and
          ((taicpu(instr).opcode = op1) or
           (taicpu(instr).opcode = op2) or
           (taicpu(instr).opcode = op3)
          ) and
          ((opsize = []) or (taicpu(instr).opsize in opsize));
      end;


    function MatchInstruction(const instr : tai;const ops : array of TAsmOp;
     const opsize : topsizes) : boolean;
      var
        op : TAsmOp;
      begin
        result:=false;
        for op in ops do
          begin
            if (instr.typ = ait_instruction) and
               (taicpu(instr).opcode = op) and
               ((opsize = []) or (taicpu(instr).opsize in opsize)) then
               begin
                 result:=true;
                 exit;
               end;
          end;
      end;


    function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
      begin
        result := (oper.typ = top_reg) and (oper.reg = reg);
      end;


    function MatchOperand(const oper: TOper; const a: tcgint): boolean; inline;
      begin
        result := (oper.typ = top_const) and (oper.val = a);
      end;


    function MatchOperand(const oper1: TOper; const oper2: TOper): boolean;
      begin
        result := oper1.typ = oper2.typ;

        if result then
          case oper1.typ of
            top_const:
              Result:=oper1.val = oper2.val;
            top_reg:
              Result:=oper1.reg = oper2.reg;
            top_ref:
              Result:=RefsEqual(oper1.ref^, oper2.ref^);
            else
              internalerror(2013102801);
          end
      end;


    function RefsEqual(const r1, r2: treference): boolean;
      begin
        RefsEqual :=
          (r1.offset = r2.offset) and
          (r1.segment = r2.segment) and (r1.base = r2.base) and
          (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
          (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
          (r1.relsymbol = r2.relsymbol);
      end;


    function MatchReference(const ref : treference;base,index : TRegister) : Boolean;
      begin
       Result:=(ref.offset=0) and
         (ref.scalefactor in [0,1]) and
         (ref.segment=NR_NO) and
         (ref.symbol=nil) and
         (ref.relsymbol=nil) and
         ((base=NR_INVALID) or
          (ref.base=base)) and
         ((index=NR_INVALID) or
          (ref.index=index));
      end;


    function MatchOpType(const instr : tai;ot0,ot1 : toptype) : Boolean;
      begin
        Result:=(taicpu(instr).ops=2) and
          (taicpu(instr).oper[0]^.typ=ot0) and
          (taicpu(instr).oper[1]^.typ=ot1);
      end;


{$ifdef DEBUG_AOPTCPU}
    procedure TX86AsmOptimizer.DebugMsg(const s: string;p : tai);
      begin
        asml.insertbefore(tai_comment.Create(strpnew(s)), p);
      end;
{$else DEBUG_AOPTCPU}
    procedure TX86AsmOptimizer.DebugMsg(const s: string;p : tai);inline;
      begin
      end;
{$endif DEBUG_AOPTCPU}


    { allocates register reg between (and including) instructions p1 and p2
      the type of p1 and p2 must not be in SkipInstr
      note that this routine is both called from the peephole optimizer
      where optinfo is not yet initialised) and from the cse (where it is)  }
    procedure TX86AsmOptimizer.AllocRegBetween(reg: tregister; p1, p2: tai; var initialusedregs: TAllUsedRegs);
      var
        hp, start: tai;
        removedsomething,
        firstRemovedWasAlloc,
        lastRemovedWasDealloc: boolean;
      begin
{$ifdef EXTDEBUG}
{        if assigned(p1.optinfo) and
           (ptaiprop(p1.optinfo)^.usedregs <> initialusedregs) then
         internalerror(2004101010); }
{$endif EXTDEBUG}
        start := p1;
       if (reg = NR_ESP) or
          (reg = current_procinfo.framepointer) or
           not(assigned(p1)) then
          { this happens with registers which are loaded implicitely, outside the }
          { current block (e.g. esi with self)                                    }
          exit;
        { make sure we allocate it for this instruction }
        getnextinstruction(p2,p2);
        lastRemovedWasDealloc := false;
        removedSomething := false;
        firstRemovedWasAlloc := false;
{$ifdef allocregdebug}
        hp := tai_comment.Create(strpnew('allocating '+std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))+
          ' from here...'));
        insertllitem(asml,p1.previous,p1,hp);
        hp := tai_comment.Create(strpnew('allocated '+std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))+
          ' till here...'));
        insertllitem(asml,p2,p2.next,hp);
{$endif allocregdebug}
        if not(RegInUsedRegs(reg,initialusedregs)) then
          begin
            hp := tai_regalloc.alloc(reg,nil);
            insertllItem(p1.previous,p1,hp);
            IncludeRegInUsedRegs(reg,initialusedregs);
          end;
        while assigned(p1) and
              (p1 <> p2) do
          begin
            if assigned(p1.optinfo) then
              internalerror(2014022301); // IncludeRegInUsedRegs(reg,ptaiprop(p1.optinfo)^.usedregs);
            p1 := tai(p1.next);
            repeat
              while assigned(p1) and
                    (p1.typ in (SkipInstr-[ait_regalloc])) Do
                p1 := tai(p1.next);

              { remove all allocation/deallocation info about the register in between }
              if assigned(p1) and
                 (p1.typ = ait_regalloc) then
                if tai_regalloc(p1).reg=reg then
                  begin
                    if not removedSomething then
                      begin
                        firstRemovedWasAlloc := tai_regalloc(p1).ratype=ra_alloc;
                        removedSomething := true;
                      end;
                    lastRemovedWasDealloc := (tai_regalloc(p1).ratype=ra_dealloc);
                    hp := tai(p1.Next);
                    asml.Remove(p1);
                    p1.free;
                    p1 := hp;
                  end
                else p1 := tai(p1.next);
            until not(assigned(p1)) or
                  not(p1.typ in SkipInstr);
          end;
        if assigned(p1) then
          begin
            if firstRemovedWasAlloc then
              begin
                hp := tai_regalloc.Alloc(reg,nil);
                insertLLItem(start.previous,start,hp);
              end;
            if lastRemovedWasDealloc then
              begin
                hp := tai_regalloc.DeAlloc(reg,nil);
                insertLLItem(p1.previous,p1,hp);
              end;
          end;
      end;


    function TX86AsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
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
        Result :=
          (((p.opcode = A_MOV) or
            (p.opcode = A_MOVZX) or
            (p.opcode = A_MOVSX) or
            (p.opcode = A_LEA) or
            (p.opcode = A_VMOVSS) or
            (p.opcode = A_VMOVSD) or
            (p.opcode = A_VMOVAPD) or
            (p.opcode = A_VMOVAPS) or
            (p.opcode = A_VMOVQ) or
            (p.opcode = A_MOVSS) or
            (p.opcode = A_MOVSD) or
            (p.opcode = A_MOVQ) or
            (p.opcode = A_MOVAPD) or
            (p.opcode = A_MOVAPS)) and
           (p.oper[1]^.typ = top_reg) and
           (getsupreg(p.oper[1]^.reg) = getsupreg(reg)) and
           ((p.oper[0]^.typ = top_const) or
            ((p.oper[0]^.typ = top_reg) and
             (getsupreg(p.oper[0]^.reg) <> getsupreg(reg))) or
            ((p.oper[0]^.typ = top_ref) and
             not RegInRef(reg,p.oper[0]^.ref^)))) or
          ((p.opcode = A_POP) and
           (getsupreg(p.oper[0]^.reg) = getsupreg(reg)));
      end;


    class function TX86AsmOptimizer.IsExitCode(p : tai) : boolean;
      var
        hp2,hp3 : tai;
      begin
        result:=(p.typ=ait_instruction) and
        ((taicpu(p).opcode = A_RET) or
         ((taicpu(p).opcode=A_LEAVE) and
          GetNextInstruction(p,hp2) and
          (hp2.typ=ait_instruction) and
          (taicpu(hp2).opcode=A_RET)
         ) or
         ((taicpu(p).opcode=A_MOV) and
          (taicpu(p).oper[0]^.typ=top_reg) and
          (taicpu(p).oper[0]^.reg=NR_EBP) and
          (taicpu(p).oper[1]^.typ=top_reg) and
          (taicpu(p).oper[1]^.reg=NR_ESP) and
          GetNextInstruction(p,hp2) and
          (hp2.typ=ait_instruction) and
          (taicpu(hp2).opcode=A_POP) and
          (taicpu(hp2).oper[0]^.typ=top_reg) and
          (taicpu(hp2).oper[0]^.reg=NR_EBP) and
          GetNextInstruction(hp2,hp3) and
          (hp3.typ=ait_instruction) and
          (taicpu(hp3).opcode=A_RET)
         )
        );
      end;


    class function TX86AsmOptimizer.isFoldableArithOp(hp1: taicpu; reg: tregister): boolean;
      begin
        isFoldableArithOp := False;
        case hp1.opcode of
          A_ADD,A_SUB,A_OR,A_XOR,A_AND,A_SHL,A_SHR,A_SAR:
            isFoldableArithOp :=
              ((taicpu(hp1).oper[0]^.typ = top_const) or
               ((taicpu(hp1).oper[0]^.typ = top_reg) and
                (taicpu(hp1).oper[0]^.reg <> reg))) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(hp1).oper[1]^.reg = reg);
          A_INC,A_DEC,A_NEG,A_NOT:
            isFoldableArithOp :=
              (taicpu(hp1).oper[0]^.typ = top_reg) and
              (taicpu(hp1).oper[0]^.reg = reg);
        end;
      end;


    procedure TX86AsmOptimizer.RemoveLastDeallocForFuncRes(p: tai);

      procedure DoRemoveLastDeallocForFuncRes( supreg: tsuperregister);
        var
          hp2: tai;
        begin
          hp2 := p;
          repeat
            hp2 := tai(hp2.previous);
            if assigned(hp2) and
               (hp2.typ = ait_regalloc) and
               (tai_regalloc(hp2).ratype=ra_dealloc) and
               (getregtype(tai_regalloc(hp2).reg) = R_INTREGISTER) and
               (getsupreg(tai_regalloc(hp2).reg) = supreg) then
              begin
                asml.remove(hp2);
                hp2.free;
                break;
              end;
          until not(assigned(hp2)) or regInInstruction(newreg(R_INTREGISTER,supreg,R_SUBWHOLE),hp2);
        end;

      begin
          case current_procinfo.procdef.returndef.typ of
            arraydef,recorddef,pointerdef,
               stringdef,enumdef,procdef,objectdef,errordef,
               filedef,setdef,procvardef,
               classrefdef,forwarddef:
              DoRemoveLastDeallocForFuncRes(RS_EAX);
            orddef:
              if current_procinfo.procdef.returndef.size <> 0 then
                begin
                  DoRemoveLastDeallocForFuncRes(RS_EAX);
                  { for int64/qword }
                  if current_procinfo.procdef.returndef.size = 8 then
                    DoRemoveLastDeallocForFuncRes(RS_EDX);
                end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1VMOVAP(var p : tai) : boolean;
      var
        TmpUsedRegs : TAllUsedRegs;
        hp1,hp2 : tai;
      begin
        result:=false;
        if MatchOpType(taicpu(p),top_reg,top_reg) then
          begin
            { vmova* reg1,reg1
              =>
              <nop> }
            if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) then
              begin
                GetNextInstruction(p,hp1);
                asml.Remove(p);
                p.Free;
                p:=hp1;
                result:=true;
              end
            else if GetNextInstruction(p,hp1) then
              begin
                if MatchInstruction(hp1,[taicpu(p).opcode],[S_NO]) and
                  MatchOpType(taicpu(hp1),top_reg,top_reg) and
                  MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
                  begin
                    { vmova* reg1,reg2
                      vmova* reg2,reg3
                      dealloc reg2
                      =>
                      vmova* reg1,reg3 }
                    CopyUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                    if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
                      begin
                        taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);
                        asml.Remove(hp1);
                        hp1.Free;
                        result:=true;
                      end
                    { special case:
                      vmova* reg1,reg2
                      vmova* reg2,reg1
                      =>
                      vmova* reg1,reg2 }
                    else if MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) then
                      begin
                        asml.Remove(hp1);
                        hp1.Free;
                        result:=true;
                      end
                  end
                else if MatchInstruction(hp1,[A_VFMADD132PD,A_VFNMADD231SD,A_VFMADD231SD],[S_NO]) and
                  { we mix single and double opperations here because we assume that the compiler
                    generates vmovapd only after double operations and vmovaps only after single operations }
                  MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[2]^) and
                  GetNextInstruction(hp1,hp2) and
                  MatchInstruction(hp2,A_VMOVAPD,A_VMOVAPS,[S_NO]) and
                  MatchOperand(taicpu(p).oper[0]^,taicpu(hp2).oper[1]^) then
                  begin
                    CopyUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                    if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs))
                     then
                      begin
                        taicpu(hp1).loadoper(2,taicpu(p).oper[0]^);
                        asml.Remove(p);
                        p.Free;
                        asml.Remove(hp2);
                        hp2.Free;
                        p:=hp1;
                      end;
                  end;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1VOP(const p : tai) : boolean;
      var
        TmpUsedRegs : TAllUsedRegs;
        hp1 : tai;
      begin
        result:=false;
        if GetNextInstruction(p,hp1) and
          { we mix single and double opperations here because we assume that the compiler
            generates vmovapd only after double operations and vmovaps only after single operations }
          MatchInstruction(hp1,A_VMOVAPD,A_VMOVAPS,[S_NO]) and
          MatchOperand(taicpu(p).oper[2]^,taicpu(hp1).oper[0]^) and
          (taicpu(hp1).oper[1]^.typ=top_reg) then
          begin
            CopyUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            if not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg,hp1,TmpUsedRegs)
             ) then
              begin
                taicpu(p).loadoper(2,taicpu(hp1).oper[1]^);
                asml.Remove(hp1);
                hp1.Free;
                result:=true;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1MOV(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        TmpUsedRegs : TAllUsedRegs;
        GetNextIntruction_p : Boolean;
      begin
        Result:=false;
        GetNextIntruction_p:=GetNextInstruction(p, hp1);
        if GetNextIntruction_p and
          MatchInstruction(hp1,A_AND,[]) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          MatchOpType(taicpu(hp1),top_const,top_reg) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) then
          case taicpu(p).opsize Of
            S_L:
              if (taicpu(hp1).oper[0]^.val = $ffffffff) then
                begin
                  DebugMsg('PeepHole Optimization,MovAnd2Mov',p);
                  asml.remove(hp1);
                  hp1.free;
                  Result:=true;
                  exit;
                end;
          end
        else if GetNextIntruction_p and
          MatchInstruction(hp1,A_MOV,[]) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          (getsupreg(taicpu(p).oper[1]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX, RS_ESI, RS_EDI]) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
          begin
            CopyUsedRegs(TmpUsedRegs);
            { we have
                mov x, %treg
                mov %treg, y
            }
            if not(RegInOp(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[1]^)) and
               not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs)) then
              { we've got

                mov x, %treg
                mov %treg, y

                with %treg is not used after }
              case taicpu(p).oper[0]^.typ Of
                top_reg:
                  begin
                    { change
                        mov %reg, %treg
                        mov %treg, y

                        to

                        mov %reg, y
                    }
                    taicpu(p).loadOper(1,taicpu(hp1).oper[1]^);
                    asml.remove(hp1);
                    hp1.free;
                    ReleaseUsedRegs(TmpUsedRegs);
                    Exit;
                  end;
                top_ref:
                  if (taicpu(hp1).oper[1]^.typ = top_reg) then
                    begin
                      { change
                           mov mem, %treg
                           mov %treg, %reg

                           to

                           mov mem, %reg"
                      }
                      taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);
                      asml.remove(hp1);
                      hp1.free;
                      ReleaseUsedRegs(TmpUsedRegs);
                      Exit;
                    end;
              end;
            ReleaseUsedRegs(TmpUsedRegs);
          end
        else
          { Change
             mov %reg1, %reg2
             xxx %reg2, ???

             to

             mov %reg1, %reg2
             xxx %reg1, ???

             to avoid a write/read penalty
          }
          if MatchOpType(taicpu(p),top_reg,top_reg) and
             GetNextInstruction(p,hp1) and
             (tai(hp1).typ = ait_instruction) and
             (taicpu(hp1).ops >= 1) and
             MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
            { we have

              mov %reg1, %reg2
              XXX %reg2, ???
            }
            begin
              if ((taicpu(hp1).opcode = A_OR) or
                  (taicpu(hp1).opcode = A_TEST)) and
                 (taicpu(hp1).oper[1]^.typ = top_reg) and
                 (taicpu(hp1).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) then
                {  we have

                   mov %reg1, %reg2
                   test/or %reg2, %reg2
                }
                begin
                  CopyUsedRegs(TmpUsedRegs);
                  { reg1 will be used after the first instruction,
                    so update the allocation info                  }
                  AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                  if GetNextInstruction(hp1, hp2) and
                     (hp2.typ = ait_instruction) and
                     taicpu(hp2).is_jmp and
                     not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg, hp1, TmpUsedRegs)) then
                      { change

                        mov %reg1, %reg2
                        test/or %reg2, %reg2
                        jxx

                        to

                        test %reg1, %reg1
                        jxx
                      }
                      begin
                        taicpu(hp1).loadoper(0,taicpu(p).oper[0]^);
                        taicpu(hp1).loadoper(1,taicpu(p).oper[0]^);
                        asml.remove(p);
                        p.free;
                        p := hp1;
                        ReleaseUsedRegs(TmpUsedRegs);
                        Exit;
                      end
                    else
                      { change

                        mov %reg1, %reg2
                        test/or %reg2, %reg2

                        to

                        mov %reg1, %reg2
                        test/or %reg1, %reg1

                        }
                      begin
                        taicpu(hp1).loadoper(0,taicpu(p).oper[0]^);
                        taicpu(hp1).loadoper(1,taicpu(p).oper[0]^);
                      end;
                  ReleaseUsedRegs(TmpUsedRegs);
                end
            end
        else
          { leave out the mov from "mov reg, x(%frame_pointer); leave/ret" (with
            x >= RetOffset) as it doesn't do anything (it writes either to a
            parameter or to the temporary storage room for the function
            result)
          }
          if GetNextIntruction_p and
            (tai(hp1).typ = ait_instruction) then
            begin
              if IsExitCode(hp1) and
                MatchOpType(p,top_reg,top_ref) and
                (taicpu(p).oper[1]^.ref^.base = current_procinfo.FramePointer) and
                not(assigned(current_procinfo.procdef.funcretsym) and
                   (taicpu(p).oper[1]^.ref^.offset < tabstractnormalvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset)) and
                (taicpu(p).oper[1]^.ref^.index = NR_NO) then
                begin
                  asml.remove(p);
                  p.free;
                  p := hp1;
                  DebugMsg('Peephole removed deadstore before leave/ret',p);
                  RemoveLastDeallocForFuncRes(p);
                end
              { change
                  mov reg1, mem1
                  cmp x, mem1

                  to

                  mov reg1, mem1
                  cmp x, reg1
              }
              else if MatchOpType(p,top_reg,top_ref) and
                  MatchInstruction(hp1,A_CMP,[taicpu(p).opsize]) and
                  (taicpu(hp1).oper[1]^.typ = top_ref) and
                   RefsEqual(taicpu(p).oper[1]^.ref^, taicpu(hp1).oper[1]^.ref^) then
                  begin
                    taicpu(hp1).loadreg(1,taicpu(p).oper[0]^.reg);
                    AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                  end;
            end;

        { Next instruction is also a MOV ? }
        if GetNextIntruction_p and
          MatchInstruction(hp1,A_MOV,[taicpu(p).opsize]) then
          begin
            if (taicpu(hp1).oper[0]^.typ = taicpu(p).oper[1]^.typ) and
               (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[0]^.typ) then
                {  mov reg1, mem1     or     mov mem1, reg1
                   mov mem2, reg2            mov reg2, mem2}
              begin
                if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[0]^) then
                  { mov reg1, mem1     or     mov mem1, reg1
                    mov mem2, reg1            mov reg2, mem1}
                  begin
                    if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                      { Removes the second statement from
                        mov reg1, mem1/reg2
                        mov mem1/reg2, reg1 }
                      begin
                        if (taicpu(p).oper[0]^.typ = top_reg) then
                          AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                        DebugMsg('PeepHole Optimization,MovMov2Mov1',p);
                        asml.remove(hp1);
                        hp1.free;
                        Result:=true;
                        exit;
                      end
                    else
                      begin
                        CopyUsedRegs(TmpUsedRegs);
                        UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                        if (taicpu(p).oper[1]^.typ = top_ref) and
                          { mov reg1, mem1
                            mov mem2, reg1 }
                           (taicpu(hp1).oper[0]^.ref^.refaddr = addr_no) and
                           GetNextInstruction(hp1, hp2) and
                           MatchInstruction(hp2,A_CMP,[taicpu(p).opsize]) and
                           OpsEqual(taicpu(p).oper[1]^,taicpu(hp2).oper[0]^) and
                           OpsEqual(taicpu(p).oper[0]^,taicpu(hp2).oper[1]^) and
                           not(RegUsedAfterInstruction(taicpu(p).oper[0]^.reg, hp2, TmpUsedRegs)) then
                           { change                   to
                             mov reg1, mem1           mov reg1, mem1
                             mov mem2, reg1           cmp reg1, mem2
                             cmp mem1, reg1
                           }
                          begin
                            asml.remove(hp2);
                            hp2.free;
                            taicpu(hp1).opcode := A_CMP;
                            taicpu(hp1).loadref(1,taicpu(hp1).oper[0]^.ref^);
                            taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);
                            AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,UsedRegs);
                            DebugMsg('Peephole MovMovCmp2MovCmp done',hp1);
                          end;
                        ReleaseUsedRegs(TmpUsedRegs);
                      end;
                  end
                else if (taicpu(p).oper[1]^.typ=top_ref) and
                  OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                  begin
                    taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);
                    DebugMsg('PeepHole Optimization,MovMov2MovMov1',p);
                  end
                else
                  begin
                    CopyUsedRegs(TmpUsedRegs);
                    if GetNextInstruction(hp1, hp2) and
                      MatchOpType(taicpu(p),top_ref,top_reg) and
                      MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) and
                      (taicpu(hp1).oper[1]^.typ = top_ref) and
                      MatchInstruction(hp2,A_MOV,[taicpu(p).opsize]) and
                      MatchOpType(taicpu(hp2),top_ref,top_reg) and
                      RefsEqual(taicpu(hp2).oper[0]^.ref^, taicpu(hp1).oper[1]^.ref^)  then
                      if not RegInRef(taicpu(hp2).oper[1]^.reg,taicpu(hp2).oper[0]^.ref^) and
                         not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,tmpUsedRegs)) then
                        {   mov mem1, %reg1
                            mov %reg1, mem2
                            mov mem2, reg2
                         to:
                            mov mem1, reg2
                            mov reg2, mem2}
                        begin
                          AllocRegBetween(taicpu(hp2).oper[1]^.reg,p,hp2,usedregs);
                          taicpu(p).loadoper(1,taicpu(hp2).oper[1]^);
                          taicpu(hp1).loadoper(0,taicpu(hp2).oper[1]^);
                          asml.remove(hp2);
                          hp2.free;
                        end
{$ifdef i386}
                      { this is enabled for i386 only, as the rules to create the reg sets below
                        are too complicated for x86-64, so this makes this code too error prone
                        on x86-64
                      }
                      else if (taicpu(p).oper[1]^.reg <> taicpu(hp2).oper[1]^.reg) and
                        not(RegInRef(taicpu(p).oper[1]^.reg,taicpu(p).oper[0]^.ref^)) and
                        not(RegInRef(taicpu(hp2).oper[1]^.reg,taicpu(hp2).oper[0]^.ref^)) then
                        {   mov mem1, reg1         mov mem1, reg1
                            mov reg1, mem2         mov reg1, mem2
                            mov mem2, reg2         mov mem2, reg1
                         to:                    to:
                            mov mem1, reg1         mov mem1, reg1
                            mov mem1, reg2         mov reg1, mem2
                            mov reg1, mem2

                         or (if mem1 depends on reg1
                      and/or if mem2 depends on reg2)
                         to:
                             mov mem1, reg1
                             mov reg1, mem2
                             mov reg1, reg2
                        }
                        begin
                          taicpu(hp1).loadRef(0,taicpu(p).oper[0]^.ref^);
                          taicpu(hp1).loadReg(1,taicpu(hp2).oper[1]^.reg);
                          taicpu(hp2).loadRef(1,taicpu(hp2).oper[0]^.ref^);
                          taicpu(hp2).loadReg(0,taicpu(p).oper[1]^.reg);
                          AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp2,usedregs);
                          if (taicpu(p).oper[0]^.ref^.base <> NR_NO) and
                             (getsupreg(taicpu(p).oper[0]^.ref^.base) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI,RS_EDI]) then
                            AllocRegBetween(taicpu(p).oper[0]^.ref^.base,p,hp2,usedregs);
                          if (taicpu(p).oper[0]^.ref^.index <> NR_NO) and
                             (getsupreg(taicpu(p).oper[0]^.ref^.index) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI,RS_EDI]) then
                            AllocRegBetween(taicpu(p).oper[0]^.ref^.index,p,hp2,usedregs);
                        end
                      else if (taicpu(hp1).Oper[0]^.reg <> taicpu(hp2).Oper[1]^.reg) then
                        begin
                          taicpu(hp2).loadReg(0,taicpu(hp1).Oper[0]^.reg);
                          AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp2,usedregs);
                        end
                      else
                        begin
                          asml.remove(hp2);
                          hp2.free;
                        end
{$endif i386}
                        ;
                    ReleaseUsedRegs(TmpUsedRegs);
                  end;
              end
(*          { movl [mem1],reg1
              movl [mem1],reg2

              to

              movl [mem1],reg1
              movl reg1,reg2
             }
             else if (taicpu(p).oper[0]^.typ = top_ref) and
                (taicpu(p).oper[1]^.typ = top_reg) and
                (taicpu(hp1).oper[0]^.typ = top_ref) and
                (taicpu(hp1).oper[1]^.typ = top_reg) and
                (taicpu(p).opsize = taicpu(hp1).opsize) and
                RefsEqual(TReference(taicpu(p).oper[0]^^),taicpu(hp1).oper[0]^^.ref^) and
                (taicpu(p).oper[1]^.reg<>taicpu(hp1).oper[0]^^.ref^.base) and
                (taicpu(p).oper[1]^.reg<>taicpu(hp1).oper[0]^^.ref^.index) then
                taicpu(hp1).loadReg(0,taicpu(p).oper[1]^.reg)
              else*)

            {   movl const1,[mem1]
                movl [mem1],reg1

                to

                movl const1,reg1
                movl reg1,[mem1]
            }
            else if MatchOpType(Taicpu(p),top_const,top_ref) and
                 MatchOpType(Taicpu(hp1),top_ref,top_reg) and
                 (taicpu(p).opsize = taicpu(hp1).opsize) and
                 RefsEqual(taicpu(hp1).oper[0]^.ref^,taicpu(p).oper[1]^.ref^) and
                 not(RegInRef(taicpu(hp1).oper[1]^.reg,taicpu(hp1).oper[0]^.ref^)) then
              begin
                allocregbetween(taicpu(hp1).oper[1]^.reg,p,hp1,usedregs);
                taicpu(hp1).loadReg(0,taicpu(hp1).oper[1]^.reg);
                taicpu(hp1).loadRef(1,taicpu(p).oper[1]^.ref^);
                taicpu(p).loadReg(1,taicpu(hp1).oper[0]^.reg);
                taicpu(hp1).fileinfo := taicpu(p).fileinfo;
              end
          end

        else if (taicpu(p).oper[1]^.typ = top_reg) and
          GetNextIntruction_p and
          (hp1.typ = ait_instruction) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(taicpu(hp2),A_MOV,[]) and
          OpsEqual(taicpu(hp2).oper[1]^, taicpu(p).oper[0]^) and
          (IsFoldableArithOp(taicpu(hp1), taicpu(p).oper[1]^.reg) or
           ((taicpu(p).opsize=S_L) and (taicpu(hp1).opsize=S_Q) and
            IsFoldableArithOp(taicpu(hp1), newreg(R_INTREGISTER,getsupreg(taicpu(p).oper[1]^.reg),R_SUBQ)))
          ) then
          { change   movsX/movzX    reg/ref, reg2
                     add/sub/or/... reg3/$const, reg2
                     mov            reg2 reg/ref
            to       add/sub/or/... reg3/$const, reg/ref      }
          begin
            CopyUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
            If not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs)) then
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
                DebugMsg('PeepHole Optimization '+
                      std_op2str[taicpu(p).opcode]+gas_opsize2str[taicpu(p).opsize]+' '+
                      std_op2str[taicpu(hp1).opcode]+gas_opsize2str[taicpu(hp1).opsize]+' '+
                      std_op2str[taicpu(hp2).opcode]+gas_opsize2str[taicpu(hp2).opsize],p);
                taicpu(hp1).changeopsize(taicpu(hp2).opsize);
                {
                  ->
                    movswl  %si,%eax        movswl  %si,%eax      p
                    decw    %si             addw    %dx,%si       hp1
                    movw    %ax,%si         movw    %ax,%si       hp2
                }
                case taicpu(hp1).ops of
                  1:
                    begin
                      taicpu(hp1).loadoper(0, taicpu(hp2).oper[1]^);
                      if taicpu(hp1).oper[0]^.typ=top_reg then
                        setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                    end;
                  2:
                    begin
                      taicpu(hp1).loadoper(1, taicpu(hp2).oper[1]^);
                      if (taicpu(hp1).oper[0]^.typ=top_reg) and
                        (taicpu(hp1).opcode<>A_SHL) and
                        (taicpu(hp1).opcode<>A_SHR) and
                        (taicpu(hp1).opcode<>A_SAR) then
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
                p.Free;
                hp2.Free;
                p := hp1;
              end;
            ReleaseUsedRegs(TmpUsedRegs);
          end;

        if GetNextIntruction_p and
          MatchInstruction(hp1,A_BTS,A_BTR,[Taicpu(p).opsize]) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_OR,[Taicpu(p).opsize]) and
          MatchOperand(Taicpu(p).oper[0]^,0) and
          (Taicpu(p).oper[1]^.typ = top_reg) and
          MatchOperand(Taicpu(p).oper[1]^,Taicpu(hp1).oper[1]^) and
          MatchOperand(Taicpu(p).oper[1]^,Taicpu(hp2).oper[1]^) then
          { mov reg1,0
            bts reg1,operand1             -->      mov reg1,operand2
            or  reg1,operand2                      bts reg1,operand1}
          begin
            Taicpu(hp2).opcode:=A_MOV;
            asml.remove(hp1);
            insertllitem(hp2,hp2.next,hp1);
            asml.remove(p);
            p.free;
            p:=hp1;
          end;

        if GetNextIntruction_p and
           MatchInstruction(hp1,A_LEA,[S_L]) and
           MatchOpType(Taicpu(p),top_ref,top_reg) and
           ((MatchReference(Taicpu(hp1).oper[0]^.ref^,Taicpu(hp1).oper[1]^.reg,Taicpu(p).oper[1]^.reg) and
             (Taicpu(hp1).oper[0]^.ref^.base<>Taicpu(p).oper[1]^.reg)
            ) or
            (MatchReference(Taicpu(hp1).oper[0]^.ref^,Taicpu(p).oper[1]^.reg,Taicpu(hp1).oper[1]^.reg) and
             (Taicpu(hp1).oper[0]^.ref^.index<>Taicpu(p).oper[1]^.reg)
            )
           ) then
           { mov reg1,ref
             lea reg2,[reg1,reg2]

             to

             add reg2,ref}
          begin
            CopyUsedRegs(TmpUsedRegs);
            { reg1 may not be used afterwards }
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs)) then
              begin
                Taicpu(hp1).opcode:=A_ADD;
                Taicpu(hp1).oper[0]^.ref^:=Taicpu(p).oper[0]^.ref^;
                DebugMsg('Peephole MovLea2Add done',hp1);
                asml.remove(p);
                p.free;
                p:=hp1;
              end;
            ReleaseUsedRegs(TmpUsedRegs);
          end;
      end;


    function TX86AsmOptimizer.OptPass2MOV(var p : tai) : boolean;
      var
       TmpUsedRegs : TAllUsedRegs;
       hp1,hp2: tai;
      begin
        Result:=false;
        if MatchOpType(taicpu(p),top_reg,top_reg) and
          GetNextInstruction(p, hp1) and
          MatchInstruction(hp1,A_MOV,A_MOVZX,A_MOVSX,[]) and
          MatchOpType(taicpu(hp1),top_ref,top_reg) and
          ((taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg)
           or
           (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg)
            ) and
          (getsupreg(taicpu(hp1).oper[1]^.reg) = getsupreg(taicpu(p).oper[1]^.reg)) then
          { mov reg1, reg2
            mov/zx/sx (reg2, ..), reg2      to   mov/zx/sx (reg1, ..), reg2}
          begin
            if (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) then
              taicpu(hp1).oper[0]^.ref^.base := taicpu(p).oper[0]^.reg;
            if (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) then
              taicpu(hp1).oper[0]^.ref^.index := taicpu(p).oper[0]^.reg;
            asml.remove(p);
            p.free;
            p := hp1;
            Result:=true;
            exit;
          end
        else if (taicpu(p).oper[0]^.typ = top_ref) and
          GetNextInstruction(p,hp1) and
          (hp1.typ = ait_instruction) and
          (IsFoldableArithOp(taicpu(hp1),taicpu(p).oper[1]^.reg) or
           ((taicpu(hp1).opcode=A_LEA) and
            (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) and
            ((MatchReference(taicpu(hp1).oper[0]^.ref^,taicpu(p).oper[1]^.reg,NR_INVALID) and
             (taicpu(hp1).oper[0]^.ref^.index<>taicpu(p).oper[1]^.reg)
              ) or
             (MatchReference(taicpu(hp1).oper[0]^.ref^,NR_INVALID,
              taicpu(p).oper[1]^.reg) and
             (taicpu(hp1).oper[0]^.ref^.base<>taicpu(p).oper[1]^.reg))
            )
           )
          ) and
          GetNextInstruction(hp1,hp2) and
          MatchInstruction(hp2,A_MOV,[]) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp2).oper[0]^) and
          (taicpu(hp2).oper[1]^.typ = top_ref) then
          begin
            CopyUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs,tai(hp1.next));
            if (RefsEqual(taicpu(hp2).oper[1]^.ref^, taicpu(p).oper[0]^.ref^) and
              not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2, TmpUsedRegs))) then
              { change   mov            (ref), reg
                         add/sub/or/... reg2/$const, reg
                         mov            reg, (ref)
                         # release reg
                to       add/sub/or/... reg2/$const, (ref)    }
              begin
                case taicpu(hp1).opcode of
                  A_INC,A_DEC,A_NOT,A_NEG :
                    taicpu(hp1).loadRef(0,taicpu(p).oper[0]^.ref^);
                  A_LEA :
                    begin
                      taicpu(hp1).opcode:=A_ADD;
                      if taicpu(hp1).oper[0]^.ref^.index<>taicpu(p).oper[1]^.reg then
                        taicpu(hp1).loadreg(0,taicpu(hp1).oper[0]^.ref^.index)
                      else
                        taicpu(hp1).loadreg(0,taicpu(hp1).oper[0]^.ref^.base);
                      taicpu(hp1).loadRef(1,taicpu(p).oper[0]^.ref^);
                      DebugMsg('Peephole FoldLea done',hp1);
                    end
                  else
                    taicpu(hp1).loadRef(1,taicpu(p).oper[0]^.ref^);
                end;
                asml.remove(p);
                asml.remove(hp2);
                p.free;
                hp2.free;
                p := hp1
              end;
            ReleaseUsedRegs(TmpUsedRegs);
          end;
      end;


    function TX86AsmOptimizer.OptPass1AND(var p : tai) : boolean;
      var
        hp1 : tai;
        GetNextIntruction_p : Boolean;
      begin
        Result:=false;
        GetNextIntruction_p:=GetNextInstruction(p, hp1);
        if GetNextIntruction_p and
          MatchOpType(p,top_const,top_reg) and
          MatchInstruction(hp1,A_AND,[]) and
          MatchOpType(hp1,top_const,top_reg) and
          (getsupreg(taicpu(p).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg)) and
          { the second register must contain the first one, so compare their subreg types }
          (getsubreg(taicpu(p).oper[1]^.reg)<=getsubreg(taicpu(hp1).oper[1]^.reg)) and
          (abs(taicpu(p).oper[0]^.val and taicpu(hp1).oper[0]^.val)<$80000000) then
          { change
              and const1, reg
              and const2, reg
            to
              and (const1 and const2), reg
          }
          begin
            taicpu(hp1).loadConst(0, taicpu(p).oper[0]^.val and taicpu(hp1).oper[0]^.val);
            DebugMsg('Peephole AndAnd2And done',hp1);
            asml.remove(p);
            p.Free;
            p:=hp1;
            Result:=true;
            exit;
          end
        else if GetNextIntruction_p and
          MatchOpType(p,top_const,top_reg) and
          MatchInstruction(hp1,A_MOVZX,[]) and
          (taicpu(hp1).oper[0]^.typ = top_reg) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) and
          (getsubreg(taicpu(hp1).oper[0]^.reg)=getsubreg(taicpu(hp1).oper[1]^.reg)) and
           (((taicpu(p).opsize=S_W) and
             (taicpu(hp1).opsize=S_BW)) or
            ((taicpu(p).opsize=S_L) and
             (taicpu(hp1).opsize in [S_WL,S_BL])) or
             ((taicpu(p).opsize=S_Q) and
             (taicpu(hp1).opsize in [S_BQ,S_WQ,S_LQ]))
            ) then
              begin
                if (((taicpu(hp1).opsize) in [S_BW,S_BL,S_BQ]) and
                    ((taicpu(p).oper[0]^.val and $ff)=taicpu(p).oper[0]^.val)
                     ) or
                   (((taicpu(hp1).opsize) in [S_WL,S_WQ]) and
                    ((taicpu(p).oper[0]^.val and $ffff)=taicpu(p).oper[0]^.val)) or
                   (((taicpu(hp1).opsize)=S_LQ) and
                    ((taicpu(p).oper[0]^.val and $ffffffff)=taicpu(p).oper[0]^.val)
                   ) then
                  begin
                    DebugMsg('Peephole AndMovzToAnd done',p);
                    asml.remove(hp1);
                    hp1.free;
                  end;
              end
        else if GetNextIntruction_p and
          MatchOpType(p,top_const,top_reg) and
          MatchInstruction(hp1,A_MOVSX,A_MOVSXD,[]) and
          (taicpu(hp1).oper[0]^.typ = top_reg) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) and
          (getsupreg(taicpu(hp1).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg)) and
           (((taicpu(p).opsize=S_W) and
             (taicpu(hp1).opsize=S_BW)) or
            ((taicpu(p).opsize=S_L) and
             (taicpu(hp1).opsize in [S_WL,S_BL])) or
             ((taicpu(p).opsize=S_Q) and
             (taicpu(hp1).opsize in [S_BQ,S_WQ,S_LQ]))
            ) then
              begin
                if (((taicpu(hp1).opsize) in [S_BW,S_BL,S_BQ]) and
                    ((taicpu(p).oper[0]^.val and $7f)=taicpu(p).oper[0]^.val)
                     ) or
                   (((taicpu(hp1).opsize) in [S_WL,S_WQ]) and
                    ((taicpu(p).oper[0]^.val and $7fff)=taicpu(p).oper[0]
                     ^.val)) or
                   (((taicpu(hp1).opsize)=S_LQ) and
                    ((taicpu(p).oper[0]^.val and $7fffffff)=taicpu(p).oper[0]
                     ^.val)
                   ) then
                   begin
                     DebugMsg('PeepHole Optimization,AndMovsxToAnd',p);
                     asml.remove(hp1);
                     hp1.free;
                   end;
              end;

   (*                      else
   {change "and x, reg; jxx" to "test x, reg", if reg is deallocated before the
   jump, but only if it's a conditional jump (PFV) }
                    if (taicpu(p).oper[1]^.typ = top_reg) and
                       GetNextInstruction(p, hp1) and
                       (hp1.typ = ait_instruction) and
                       (taicpu(hp1).is_jmp) and
                       (taicpu(hp1).opcode<>A_JMP) and
                       not(getsupreg(taicpu(p).oper[1]^.reg) in UsedRegs) then
                      taicpu(p).opcode := A_TEST;*)
       end;


    procedure TX86AsmOptimizer.PostPeepholeOptMov(const p : tai);
      begin
       if MatchOperand(taicpu(p).oper[0]^,0) and
          (taicpu(p).oper[1]^.typ = Top_Reg) and
          not(RegInUsedRegs(NR_DEFAULTFLAGS,UsedRegs)) then
         { change "mov $0, %reg" into "xor %reg, %reg" }
         begin
           taicpu(p).opcode := A_XOR;
           taicpu(p).loadReg(0,taicpu(p).oper[1]^.reg);
         end;
      end;

end.

