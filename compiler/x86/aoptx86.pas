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
      aasmtai,
      cgbase,cgutils,
      aopt;

    type
      TX86AsmOptimizer = class(TAsmOptimizer)
        function RegLoadedWithNewValue(reg : tregister; hp : tai) : boolean; override;
      protected
        procedure PostPeepholeOptMov(const p : tai);
        function OptPass1VMOVAP(var p : tai) : boolean;
        function OptPass1VOP(const p : tai) : boolean;
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
      verbose,
      aasmcpu,
      aoptobj;

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

