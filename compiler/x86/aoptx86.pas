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
        function InstructionLoadsFromReg(const reg : TRegister; const hp : tai) : boolean; override;
        function RegReadByInstruction(reg : TRegister; hp : tai) : boolean;
      protected
        { checks whether loading a new value in reg1 overwrites the entirety of reg2 }
        function Reg1WriteOverwritesReg2Entirely(reg1, reg2: tregister): boolean;
        { checks whether reading the value in reg1 depends on the value of reg2. This
          is very similar to SuperRegisterEquals, except it takes into account that
          R_SUBH and R_SUBL are independendent (e.g. reading from AL does not
          depend on the value in AH). }
        function Reg1ReadDependsOnReg2(reg1, reg2: tregister): boolean;

        procedure DebugMsg(const s : string; p : tai);inline;

        procedure AllocRegBetween(reg : tregister; p1,p2 : tai;var initialusedregs : TAllUsedRegs);
        class function IsExitCode(p : tai) : boolean;
        class function isFoldableArithOp(hp1 : taicpu; reg : tregister) : boolean;
        procedure RemoveLastDeallocForFuncRes(p : tai);

        function PrePeepholeOptSxx(var p : tai) : boolean;

        function OptPass1AND(var p : tai) : boolean;
        function OptPass1VMOVAP(var p : tai) : boolean;
        function OptPass1VOP(const p : tai) : boolean;
        function OptPass1MOV(var p : tai) : boolean;
        function OptPass1Movx(var p : tai) : boolean;
        function OptPass1MOVAP(var p : tai) : boolean;

        function OptPass2MOV(var p : tai) : boolean;
        function OptPass2Imul(var p : tai) : boolean;
        function OptPass2Jmp(var p : tai) : boolean;
        function OptPass2Jcc(var p : tai) : boolean;

        procedure PostPeepholeOptMov(const p : tai);
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

    { returns true, if ref is a reference using only the registers passed as base and index
      and having an offset }
    function MatchReferenceWithOffset(const ref : treference;base,index : TRegister) : Boolean;

  implementation

    uses
      cutils,verbose,
      globals,
      cpuinfo,
      procinfo,
      aasmbase,
      aoptutils,
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


    function MatchReferenceWithOffset(const ref : treference;base,index : TRegister) : Boolean;
      begin
       Result:=(ref.scalefactor in [0,1]) and
         (ref.segment=NR_NO) and
         (ref.symbol=nil) and
         (ref.relsymbol=nil) and
         ((base=NR_INVALID) or
          (ref.base=base)) and
         ((index=NR_INVALID) or
          (ref.index=index));
      end;


  function TX86AsmOptimizer.InstructionLoadsFromReg(const reg: TRegister;const hp: tai): boolean;
    begin
      Result:=RegReadByInstruction(reg,hp);
    end;


  function TX86AsmOptimizer.RegReadByInstruction(reg: TRegister; hp: tai): boolean;
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


    function TX86AsmOptimizer.Reg1WriteOverwritesReg2Entirely(reg1, reg2: tregister): boolean;
      begin
        if not SuperRegistersEqual(reg1,reg2) then
          exit(false);
        if getregtype(reg1)<>R_INTREGISTER then
          exit(true);  {because SuperRegisterEqual is true}
        case getsubreg(reg1) of
          { A write to R_SUBL doesn't change R_SUBH and if reg2 is R_SUBW or
            higher, it preserves the high bits, so the new value depends on
            reg2's previous value. In other words, it is equivalent to doing:

            reg2 := (reg2 and $ffffff00) or byte(reg1); }
          R_SUBL:
            exit(getsubreg(reg2)=R_SUBL);
          { A write to R_SUBH doesn't change R_SUBL and if reg2 is R_SUBW or
            higher, it actually does a:

            reg2 := (reg2 and $ffff00ff) or (reg1 and $ff00); }
          R_SUBH:
            exit(getsubreg(reg2)=R_SUBH);
          { If reg2 is R_SUBD or larger, a write to R_SUBW preserves the high 16
            bits of reg2:

            reg2 := (reg2 and $ffff0000) or word(reg1); }
          R_SUBW:
            exit(getsubreg(reg2) in [R_SUBL,R_SUBH,R_SUBW]);
          { a write to R_SUBD always overwrites every other subregister,
            because it clears the high 32 bits of R_SUBQ on x86_64 }
          R_SUBD,
          R_SUBQ:
            exit(true);
          else
            internalerror(2017042801);
        end;
      end;


    function TX86AsmOptimizer.Reg1ReadDependsOnReg2(reg1, reg2: tregister): boolean;
      begin
        if not SuperRegistersEqual(reg1,reg2) then
          exit(false);
        if getregtype(reg1)<>R_INTREGISTER then
          exit(true);  {because SuperRegisterEqual is true}
        case getsubreg(reg1) of
          R_SUBL:
            exit(getsubreg(reg2)<>R_SUBH);
          R_SUBH:
            exit(getsubreg(reg2)<>R_SUBL);
          R_SUBW,
          R_SUBD,
          R_SUBQ:
            exit(true);
          else
            internalerror(2017042802);
        end;
      end;


    function TX86AsmOptimizer.PrePeepholeOptSxx(var p : tai) : boolean;
      var
        hp1 : tai;
        l : TCGInt;
      begin
        result:=false;
        { changes the code sequence
          shr/sar const1, x
          shl     const2, x

          to

          either "sar/and", "shl/and" or just "and" depending on const1 and const2 }
        if GetNextInstruction(p, hp1) and
          MatchInstruction(hp1,A_SHL,[]) and
          (taicpu(p).oper[0]^.typ = top_const) and
          (taicpu(hp1).oper[0]^.typ = top_const) and
          (taicpu(hp1).opsize = taicpu(p).opsize) and
          (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[1]^.typ) and
          OpsEqual(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^) then
          begin
            if (taicpu(p).oper[0]^.val > taicpu(hp1).oper[0]^.val) and
              not(cs_opt_size in current_settings.optimizerswitches) then
              begin
                { shr/sar const1, %reg
                  shl     const2, %reg
                  with const1 > const2 }
                taicpu(p).loadConst(0,taicpu(p).oper[0]^.val-taicpu(hp1).oper[0]^.val);
                taicpu(hp1).opcode := A_AND;
                l := (1 shl (taicpu(hp1).oper[0]^.val)) - 1;
                case taicpu(p).opsize Of
                  S_B: taicpu(hp1).loadConst(0,l Xor $ff);
                  S_W: taicpu(hp1).loadConst(0,l Xor $ffff);
                  S_L: taicpu(hp1).loadConst(0,l Xor aint($ffffffff));
                  S_Q: taicpu(hp1).loadConst(0,l Xor aint($ffffffffffffffff));
                  else
                    Internalerror(2017050703)
                end;
              end
            else if (taicpu(p).oper[0]^.val<taicpu(hp1).oper[0]^.val) and
              not(cs_opt_size in current_settings.optimizerswitches) then
              begin
                { shr/sar const1, %reg
                  shl     const2, %reg
                  with const1 < const2 }
                taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val-taicpu(p).oper[0]^.val);
                taicpu(p).opcode := A_AND;
                l := (1 shl (taicpu(p).oper[0]^.val))-1;
                case taicpu(p).opsize Of
                  S_B: taicpu(p).loadConst(0,l Xor $ff);
                  S_W: taicpu(p).loadConst(0,l Xor $ffff);
                  S_L: taicpu(p).loadConst(0,l Xor aint($ffffffff));
                  S_Q: taicpu(p).loadConst(0,l Xor aint($ffffffffffffffff));
                  else
                    Internalerror(2017050702)
                end;
              end
            else if (taicpu(p).oper[0]^.val = taicpu(hp1).oper[0]^.val) then
              begin
                { shr/sar const1, %reg
                  shl     const2, %reg
                  with const1 = const2 }
                taicpu(p).opcode := A_AND;
                l := (1 shl (taicpu(p).oper[0]^.val))-1;
                case taicpu(p).opsize Of
                  S_B: taicpu(p).loadConst(0,l Xor $ff);
                  S_W: taicpu(p).loadConst(0,l Xor $ffff);
                  S_L: taicpu(p).loadConst(0,l Xor aint($ffffffff));
                  S_Q: taicpu(p).loadConst(0,l Xor aint($ffffffffffffffff));
                  else
                    Internalerror(2017050701)
                end;
                asml.remove(hp1);
                hp1.free;
              end;
          end;
      end;


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
        { do it the safe way: always allocate the full super register,
          as we do no register re-allocation in the peephole optimizer,
          this does not hurt
        }
        case getregtype(reg) of
          R_MMREGISTER:
            reg:=newreg(R_MMREGISTER,getsupreg(reg),R_SUBMMWHOLE);
          R_INTREGISTER:
            reg:=newreg(R_INTREGISTER,getsupreg(reg),R_SUBWHOLE);
        end;
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
                begin
                  { same super register, different sub register? }
                  if SuperRegistersEqual(reg,tai_regalloc(p1).reg) and (tai_regalloc(p1).reg<>reg) then
                    begin
                      if (getsubreg(tai_regalloc(p1).reg)>getsubreg(reg)) or (getsubreg(reg)=R_SUBH) then
                        internalerror(2016101501);
                      tai_regalloc(p1).reg:=reg;
                    end;

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
                  else
                    p1 := tai(p1.next);
                end;
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
        if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
          with insprop[p.opcode] do
            begin
              case getsubreg(reg) of
                R_SUBW,R_SUBD,R_SUBQ:
                  Result:=
                    RegLoadedWithNewValue(NR_CARRYFLAG,hp) and
                    RegLoadedWithNewValue(NR_PARITYFLAG,hp) and
                    RegLoadedWithNewValue(NR_AUXILIARYFLAG,hp) and
                    RegLoadedWithNewValue(NR_ZEROFLAG,hp) and
                    RegLoadedWithNewValue(NR_SIGNFLAG,hp) and
                    RegLoadedWithNewValue(NR_OVERFLOWFLAG,hp);
                R_SUBFLAGCARRY:
                  Result:=[Ch_W0CarryFlag,Ch_W1CarryFlag,Ch_WCarryFlag,Ch_WUCarryFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGPARITY:
                  Result:=[Ch_W0ParityFlag,Ch_W1ParityFlag,Ch_WParityFlag,Ch_WUParityFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGAUXILIARY:
                  Result:=[Ch_W0AuxiliaryFlag,Ch_W1AuxiliaryFlag,Ch_WAuxiliaryFlag,Ch_WUAuxiliaryFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGZERO:
                  Result:=[Ch_W0ZeroFlag,Ch_W1ZeroFlag,Ch_WZeroFlag,Ch_WUZeroFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGSIGN:
                  Result:=[Ch_W0SignFlag,Ch_W1SignFlag,Ch_WSignFlag,Ch_WUSignFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGOVERFLOW:
                  Result:=[Ch_W0OverflowFlag,Ch_W1OverflowFlag,Ch_WOverflowFlag,Ch_WUOverflowFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGINTERRUPT:
                  Result:=[Ch_W0IntFlag,Ch_W1IntFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGDIRECTION:
                  Result:=[Ch_W0DirFlag,Ch_W1DirFlag,Ch_WFlags]*Ch<>[];
                else
                  internalerror(2017050501);
              end;
              exit;
            end;
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
            (p.opcode = A_MOVAPS) or
{$ifndef x86_64}
            (p.opcode = A_LDS) or
            (p.opcode = A_LES) or
{$endif not x86_64}
            (p.opcode = A_LFS) or
            (p.opcode = A_LGS) or
            (p.opcode = A_LSS)) and
           (p.ops=2) and  { A_MOVSD can have zero operands, so this check is needed }
           (p.oper[1]^.typ = top_reg) and
           (Reg1WriteOverwritesReg2Entirely(p.oper[1]^.reg,reg)) and
           ((p.oper[0]^.typ = top_const) or
            ((p.oper[0]^.typ = top_reg) and
             not(Reg1ReadDependsOnReg2(p.oper[0]^.reg,reg))) or
            ((p.oper[0]^.typ = top_ref) and
             not RegInRef(reg,p.oper[0]^.ref^)))) or
          ((p.opcode = A_POP) and
           (Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg))) or
          ((p.opcode = A_IMUL) and
           (p.ops=3) and
           (Reg1WriteOverwritesReg2Entirely(p.oper[2]^.reg,reg)) and
           (((p.oper[1]^.typ=top_reg) and not(Reg1ReadDependsOnReg2(p.oper[1]^.reg,reg))) or
            ((p.oper[1]^.typ=top_ref) and not(RegInRef(reg,p.oper[1]^.ref^))))) or
          ((((p.opcode = A_IMUL) or
             (p.opcode = A_MUL)) and
            (p.ops=1)) and
           (((p.oper[0]^.typ=top_reg) and not(Reg1ReadDependsOnReg2(p.oper[0]^.reg,reg))) or
            ((p.oper[0]^.typ=top_ref) and not(RegInRef(reg,p.oper[0]^.ref^)))) and
           (((p.opsize=S_B) and Reg1WriteOverwritesReg2Entirely(NR_AX,reg) and not(Reg1ReadDependsOnReg2(NR_AL,reg))) or
            ((p.opsize=S_W) and Reg1WriteOverwritesReg2Entirely(NR_DX,reg)) or
            ((p.opsize=S_L) and Reg1WriteOverwritesReg2Entirely(NR_EDX,reg))
{$ifdef x86_64}
         or ((p.opsize=S_Q) and Reg1WriteOverwritesReg2Entirely(NR_RDX,reg))
{$endif x86_64}
           )) or
          ((p.opcode = A_CWD) and Reg1WriteOverwritesReg2Entirely(NR_DX,reg)) or
          ((p.opcode = A_CDQ) and Reg1WriteOverwritesReg2Entirely(NR_EDX,reg)) or
{$ifdef x86_64}
          ((p.opcode = A_CQO) and Reg1WriteOverwritesReg2Entirely(NR_RDX,reg)) or
{$endif x86_64}
          ((p.opcode = A_CBW) and Reg1WriteOverwritesReg2Entirely(NR_AX,reg) and not(Reg1ReadDependsOnReg2(NR_AL,reg))) or
{$ifndef x86_64}
          ((p.opcode = A_LDS) and (reg=NR_DS) and not(RegInRef(reg,p.oper[0]^.ref^))) or
          ((p.opcode = A_LES) and (reg=NR_ES) and not(RegInRef(reg,p.oper[0]^.ref^))) or
{$endif not x86_64}
          ((p.opcode = A_LFS) and (reg=NR_FS) and not(RegInRef(reg,p.oper[0]^.ref^))) or
          ((p.opcode = A_LGS) and (reg=NR_GS) and not(RegInRef(reg,p.oper[0]^.ref^))) or
          ((p.opcode = A_LSS) and (reg=NR_SS) and not(RegInRef(reg,p.oper[0]^.ref^))) or
{$ifndef x86_64}
          ((p.opcode = A_AAM) and Reg1WriteOverwritesReg2Entirely(NR_AH,reg)) or
{$endif not x86_64}
          ((p.opcode = A_LAHF) and Reg1WriteOverwritesReg2Entirely(NR_AH,reg)) or
          ((p.opcode = A_LODSB) and Reg1WriteOverwritesReg2Entirely(NR_AL,reg)) or
          ((p.opcode = A_LODSW) and Reg1WriteOverwritesReg2Entirely(NR_AX,reg)) or
          ((p.opcode = A_LODSD) and Reg1WriteOverwritesReg2Entirely(NR_EAX,reg)) or
{$ifdef x86_64}
          ((p.opcode = A_LODSQ) and Reg1WriteOverwritesReg2Entirely(NR_RAX,reg)) or
{$endif x86_64}
          ((p.opcode = A_SETcc) and (p.oper[0]^.typ=top_reg) and Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg)) or
          (((p.opcode = A_FSTSW) or
            (p.opcode = A_FNSTSW)) and
           (p.oper[0]^.typ=top_reg) and
           Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg)) or
          (((p.opcode = A_XOR) or (p.opcode = A_SUB) or (p.opcode = A_SBB)) and
           (p.oper[0]^.typ=top_reg) and (p.oper[1]^.typ=top_reg) and
           (p.oper[0]^.reg=p.oper[1]^.reg) and
           Reg1WriteOverwritesReg2Entirely(p.oper[1]^.reg,reg));
      end;


    class function TX86AsmOptimizer.IsExitCode(p : tai) : boolean;
      var
        hp2,hp3 : tai;
      begin
        { some x86-64 issue a NOP before the real exit code }
        if MatchInstruction(p,A_NOP,[]) then
          GetNextInstruction(p,p);
        result:=assigned(p) and (p.typ=ait_instruction) and
        ((taicpu(p).opcode = A_RET) or
         ((taicpu(p).opcode=A_LEAVE) and
          GetNextInstruction(p,hp2) and
          MatchInstruction(hp2,A_RET,[S_NO])
         ) or
         ((((taicpu(p).opcode=A_MOV) and
           MatchOpType(taicpu(p),top_reg,top_reg) and
           (taicpu(p).oper[0]^.reg=current_procinfo.framepointer) and
           (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG)) or
           ((taicpu(p).opcode=A_LEA) and
           MatchOpType(taicpu(p),top_ref,top_reg) and
           (taicpu(p).oper[0]^.ref^.base=current_procinfo.framepointer) and
           (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG)
           )
          ) and
          GetNextInstruction(p,hp2) and
          MatchInstruction(hp2,A_POP,[reg2opsize(current_procinfo.framepointer)]) and
          MatchOpType(taicpu(hp2),top_reg) and
          (taicpu(hp2).oper[0]^.reg=current_procinfo.framepointer) and
          GetNextInstruction(hp2,hp3) and
          MatchInstruction(hp3,A_RET,[S_NO])
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


    function TX86AsmOptimizer.OptPass1MOVAP(var p : tai) : boolean;
      var
        TmpUsedRegs : TAllUsedRegs;
        hp1,hp2 : tai;
        alloc ,dealloc: tai_regalloc;
      begin
        result:=false;
        if MatchOpType(taicpu(p),top_reg,top_reg) and
          GetNextInstruction(p, hp1) and
          (hp1.typ = ait_instruction) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,taicpu(p).opcode,[]) and
          OpsEqual(taicpu(hp2).oper[1]^, taicpu(p).oper[0]^) and
          MatchOpType(taicpu(hp2),top_reg,top_reg) and
          MatchOperand(taicpu(hp2).oper[0]^,taicpu(p).oper[1]^) and
          (((taicpu(p).opcode=A_MOVAPS) and
            ((taicpu(hp1).opcode=A_ADDSS) or (taicpu(hp1).opcode=A_SUBSS) or
             (taicpu(hp1).opcode=A_MULSS) or (taicpu(hp1).opcode=A_DIVSS))) or
           ((taicpu(p).opcode=A_MOVAPD) and
            ((taicpu(hp1).opcode=A_ADDSD) or (taicpu(hp1).opcode=A_SUBSD) or
             (taicpu(hp1).opcode=A_MULSD) or (taicpu(hp1).opcode=A_DIVSD)))
          ) then
          { change
                     movapX    reg,reg2
                     addsX/subsX/... reg3, reg2
                     movapX    reg2,reg
            to
                     addsX/subsX/... reg3,reg
          }
          begin
            CopyUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
            If not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs)) then
              begin
                DebugMsg('Peephole Optimization MovapXOpMovapX2Op ('+
                      std_op2str[taicpu(p).opcode]+' '+
                      std_op2str[taicpu(hp1).opcode]+' '+
                      std_op2str[taicpu(hp2).opcode]+')',p);
                { we cannot eliminate the first move if
                  the operations uses the same register for source and dest }
                if not(OpsEqual(taicpu(hp1).oper[1]^,taicpu(hp1).oper[0]^)) then
                  begin
                    asml.remove(p);
                    p.Free;
                  end;
                taicpu(hp1).loadoper(1, taicpu(hp2).oper[1]^);
                asml.remove(hp2);
                hp2.Free;
                p:=hp1;
                result:=true;
              end;
            ReleaseUsedRegs(TmpUsedRegs);
          end
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
        {  remove mov reg1,reg1? }
        if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) then
          begin
            GetNextInstruction(p, hp1);
            DebugMsg('PeepHole Optimization,Mov2Nop',p);
            asml.remove(p);
            p.free;
            p:=hp1;
            Result:=true;
            exit;
          end;
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
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
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
                    DebugMsg('PeepHole Optimization,MovMov2Mov 2',p);
                    asml.remove(hp1);
                    hp1.free;
                    ReleaseUsedRegs(TmpUsedRegs);
                    Result:=true;
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
                      DebugMsg('PeepHole Optimization,MovMov2Mov 3',p);
                      asml.remove(hp1);
                      hp1.free;
                      ReleaseUsedRegs(TmpUsedRegs);
                      Result:=true;
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
                MatchOpType(taicpu(p),top_reg,top_ref) and
                (taicpu(p).oper[1]^.ref^.base = current_procinfo.FramePointer) and
                not(assigned(current_procinfo.procdef.funcretsym) and
                   (taicpu(p).oper[1]^.ref^.offset < tabstractnormalvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset)) and
                (taicpu(p).oper[1]^.ref^.index = NR_NO) then
                begin
                  asml.remove(p);
                  p.free;
                  p:=hp1;
                  DebugMsg('Peephole removed deadstore before leave/ret',p);
                  RemoveLastDeallocForFuncRes(p);
                  exit;
                end
              { change
                  mov reg1, mem1
                  test/cmp x, mem1

                  to

                  mov reg1, mem1
                  test/cmp x, reg1
              }
              else if MatchOpType(taicpu(p),top_reg,top_ref) and
                  MatchInstruction(hp1,A_CMP,A_TEST,[taicpu(p).opsize]) and
                  (taicpu(hp1).oper[1]^.typ = top_ref) and
                   RefsEqual(taicpu(p).oper[1]^.ref^, taicpu(hp1).oper[1]^.ref^) then
                  begin
                    taicpu(hp1).loadreg(1,taicpu(p).oper[0]^.reg);
                    DebugMsg('Peephole MovTestCmp2MovTestCmp 1',hp1);
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
                        if taicpu(p).oper[0]^.typ=top_reg then
                          AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                        DebugMsg('PeepHole Optimization,MovMov2Mov 1',p);
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
                    AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,UsedRegs);
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
                          DebugMsg('PeepHole Optimization,MovMovMov2MovMov 1',p);
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
                AllocRegBetween(taicpu(hp1).oper[1]^.reg,p,hp1,usedregs);
                taicpu(hp1).loadReg(0,taicpu(hp1).oper[1]^.reg);
                taicpu(hp1).loadRef(1,taicpu(p).oper[1]^.ref^);
                taicpu(p).loadReg(1,taicpu(hp1).oper[0]^.reg);
                taicpu(hp1).fileinfo := taicpu(p).fileinfo;
                DebugMsg('PeepHole Optimization,MovMov2MovMov 1',p);
              end
          end

        else if (taicpu(p).oper[1]^.typ = top_reg) and
          GetNextIntruction_p and
          (hp1.typ = ait_instruction) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_MOV,[]) and
          OpsEqual(taicpu(hp2).oper[1]^, taicpu(p).oper[0]^) and
          (taicpu(hp2).oper[0]^.typ=top_reg) and
          (SuperRegistersEqual(taicpu(hp2).oper[0]^.reg,taicpu(p).oper[1]^.reg)) and
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
                DebugMsg('Peephole Optimization MovOpMov2Op ('+
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
          end

        else if GetNextIntruction_p and
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
          end

        else if GetNextIntruction_p and
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
          { while the GetNextInstruction(hp1,hp2) call could be factored out,
            doing it separately in both branches allows to do the cheap checks
            with low probability earlier }
          ((IsFoldableArithOp(taicpu(hp1),taicpu(p).oper[1]^.reg) and
            GetNextInstruction(hp1,hp2) and
            MatchInstruction(hp2,A_MOV,[])
           ) or
           ((taicpu(hp1).opcode=A_LEA) and
             GetNextInstruction(hp1,hp2) and
             MatchInstruction(hp2,A_MOV,[]) and
            ((MatchReference(taicpu(hp1).oper[0]^.ref^,taicpu(p).oper[1]^.reg,NR_INVALID) and
             (taicpu(hp1).oper[0]^.ref^.index<>taicpu(p).oper[1]^.reg)
              ) or
             (MatchReference(taicpu(hp1).oper[0]^.ref^,NR_INVALID,
              taicpu(p).oper[1]^.reg) and
             (taicpu(hp1).oper[0]^.ref^.base<>taicpu(p).oper[1]^.reg)) or
             (MatchReferenceWithOffset(taicpu(hp1).oper[0]^.ref^,taicpu(p).oper[1]^.reg,NR_NO)) or
             (MatchReferenceWithOffset(taicpu(hp1).oper[0]^.ref^,NR_NO,taicpu(p).oper[1]^.reg))
            ) and
            ((MatchOperand(taicpu(p).oper[1]^,taicpu(hp2).oper[0]^)) or not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,UsedRegs)))
           )
          ) and
          MatchOperand(taicpu(hp1).oper[taicpu(hp1).ops-1]^,taicpu(hp2).oper[0]^) and
          (taicpu(hp2).oper[1]^.typ = top_ref) then
          begin
            CopyUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs,tai(hp1.next));
            if (RefsEqual(taicpu(hp2).oper[1]^.ref^, taicpu(p).oper[0]^.ref^) and
              not(RegUsedAfterInstruction(taicpu(hp2).oper[0]^.reg,hp2, TmpUsedRegs))) then
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
                      if (taicpu(hp1).oper[0]^.ref^.index<>taicpu(p).oper[1]^.reg) and (taicpu(hp1).oper[0]^.ref^.index<>NR_NO) then
                        taicpu(hp1).loadreg(0,taicpu(hp1).oper[0]^.ref^.index)
                      else if (taicpu(hp1).oper[0]^.ref^.base<>taicpu(p).oper[1]^.reg) and (taicpu(hp1).oper[0]^.ref^.base<>NR_NO) then
                        taicpu(hp1).loadreg(0,taicpu(hp1).oper[0]^.ref^.base)
                      else
                        taicpu(hp1).loadconst(0,taicpu(hp1).oper[0]^.ref^.offset);
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


    function TX86AsmOptimizer.OptPass2Imul(var p : tai) : boolean;
      var
        TmpUsedRegs : TAllUsedRegs;
        hp1 : tai;
      begin
        Result:=false;
        if (taicpu(p).ops >= 2) and
           ((taicpu(p).oper[0]^.typ = top_const) or
            ((taicpu(p).oper[0]^.typ = top_ref) and (taicpu(p).oper[0]^.ref^.refaddr=addr_full))) and
           (taicpu(p).oper[1]^.typ = top_reg) and
           ((taicpu(p).ops = 2) or
            ((taicpu(p).oper[2]^.typ = top_reg) and
             (taicpu(p).oper[2]^.reg = taicpu(p).oper[1]^.reg))) and
           GetLastInstruction(p,hp1) and
           MatchInstruction(hp1,A_MOV,[]) and
           MatchOpType(taicpu(hp1),top_reg,top_reg) and
           ((taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) or
            ((taicpu(hp1).opsize=S_L) and (taicpu(p).opsize=S_Q) and SuperRegistersEqual(taicpu(hp1).oper[1]^.reg,taicpu(p).oper[1]^.reg))) then
          begin
            CopyUsedRegs(TmpUsedRegs);
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,p,TmpUsedRegs)) then
              { change
                  mov reg1,reg2
                  imul y,reg2 to imul y,reg1,reg2 }
              begin
                taicpu(p).ops := 3;
                taicpu(p).loadreg(1,taicpu(hp1).oper[0]^.reg);
                taicpu(p).loadreg(2,taicpu(hp1).oper[1]^.reg);
                DebugMsg('Peephole MovImul2Imul done',p);
                asml.remove(hp1);
                hp1.free;
                result:=true;
              end;
            ReleaseUsedRegs(TmpUsedRegs);
          end;
      end;


    function TX86AsmOptimizer.OptPass2Jmp(var p : tai) : boolean;
      var
        hp1 : tai;
      begin
        {
          change
                 jmp .L1
                 ...
             .L1:
                 ret
          into
                 ret
        }
        result:=false;
        if (taicpu(p).oper[0]^.typ=top_ref) and (taicpu(p).oper[0]^.ref^.refaddr=addr_full) and (taicpu(p).oper[0]^.ref^.base=NR_NO) and
          (taicpu(p).oper[0]^.ref^.index=NR_NO) then
          begin
            hp1:=getlabelwithsym(tasmlabel(taicpu(p).oper[0]^.ref^.symbol));
            if (taicpu(p).condition=C_None) and assigned(hp1) and SkipLabels(hp1,hp1) and
              MatchInstruction(hp1,A_RET,[S_NO]) then
              begin
                tasmlabel(taicpu(p).oper[0]^.ref^.symbol).decrefs;
                taicpu(p).opcode:=A_RET;
                taicpu(p).is_jmp:=false;
                taicpu(p).ops:=taicpu(hp1).ops;
                case taicpu(hp1).ops of
                  0:
                    taicpu(p).clearop(0);
                  1:
                    taicpu(p).loadconst(0,taicpu(hp1).oper[0]^.val);
                  else
                    internalerror(2016041301);
                end;
                result:=true;
              end;
          end;
      end;


    function CanBeCMOV(p : tai) : boolean;
      begin
         CanBeCMOV:=assigned(p) and
           MatchInstruction(p,A_MOV,[S_W,S_L,S_Q]) and
           { we can't use cmov ref,reg because
             ref could be nil and cmov still throws an exception
             if ref=nil but the mov isn't done (FK)
            or ((taicpu(p).oper[0]^.typ = top_ref) and
             (taicpu(p).oper[0]^.ref^.refaddr = addr_no))
           }
           MatchOpType(taicpu(p),top_reg,top_reg);
      end;


    function TX86AsmOptimizer.OptPass2Jcc(var p : tai) : boolean;
      var
        hp1,hp2,hp3: tai;
        carryadd_opcode : TAsmOp;
        l : Longint;
        condition : TAsmCond;
      begin
        { jb @@1                            cmc
          inc/dec operand           -->     adc/sbb operand,0
	@@1:

	... and ...

          jnb @@1
          inc/dec operand           -->     adc/sbb operand,0
	@@1: }
        result:=false;
        if GetNextInstruction(p,hp1) and (hp1.typ=ait_instruction) and
           GetNextInstruction(hp1,hp2) and (hp2.typ=ait_label) and
           (Tasmlabel(Taicpu(p).oper[0]^.ref^.symbol)=Tai_label(hp2).labsym) then
          begin
            carryadd_opcode:=A_NONE;
            if Taicpu(p).condition in [C_NAE,C_B] then
              begin
                if Taicpu(hp1).opcode=A_INC then
                  carryadd_opcode:=A_ADC;
                if Taicpu(hp1).opcode=A_DEC then
                  carryadd_opcode:=A_SBB;
                if carryadd_opcode<>A_NONE then
                  begin
                    Taicpu(p).clearop(0);
                    Taicpu(p).ops:=0;
                    Taicpu(p).is_jmp:=false;
                    Taicpu(p).opcode:=A_CMC;
                    Taicpu(p).condition:=C_NONE;
                    Taicpu(hp1).ops:=2;
                    Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[0]^);
                    Taicpu(hp1).loadconst(0,0);
                    Taicpu(hp1).opcode:=carryadd_opcode;
                    result:=true;
                    exit;
                  end;
              end;
            if Taicpu(p).condition in [C_AE,C_NB] then
              begin
                if Taicpu(hp1).opcode=A_INC then
                  carryadd_opcode:=A_ADC;
                if Taicpu(hp1).opcode=A_DEC then
                  carryadd_opcode:=A_SBB;
                if carryadd_opcode<>A_NONE then
                  begin
                    asml.remove(p);
                    p.free;
                    Taicpu(hp1).ops:=2;
                    Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[0]^);
                    Taicpu(hp1).loadconst(0,0);
                    Taicpu(hp1).opcode:=carryadd_opcode;
                    p:=hp1;
                    result:=true;
                    exit;
                  end;
              end;
          end;
{$ifndef i8086}
        if CPUX86_HAS_CMOV in cpu_capabilities[current_settings.cputype] then
          begin
             { check for
                    jCC   xxx
                    <several movs>
                 xxx:
             }
             l:=0;
             GetNextInstruction(p, hp1);
             while assigned(hp1) and
               CanBeCMOV(hp1) and
               { stop on labels }
               not(hp1.typ=ait_label) do
               begin
                  inc(l);
                  GetNextInstruction(hp1,hp1);
               end;
             if assigned(hp1) then
               begin
                  if FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol),hp1) then
                    begin
                      if (l<=4) and (l>0) then
                        begin
                          condition:=inverse_cond(taicpu(p).condition);
                          hp2:=p;
                          GetNextInstruction(p,hp1);
                          p:=hp1;
                          repeat
                            taicpu(hp1).opcode:=A_CMOVcc;
                            taicpu(hp1).condition:=condition;
                            GetNextInstruction(hp1,hp1);
                          until not(assigned(hp1)) or
                            not(CanBeCMOV(hp1));
                          { wait with removing else GetNextInstruction could
                            ignore the label if it was the only usage in the
                            jump moved away }
                          tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).decrefs;
                          { if the label refs. reach zero, remove any alignment before the label }
                          if (hp1.typ=ait_align) and (tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).getrefs=0) then
                            begin
                              asml.Remove(hp1);
                              hp1.Free;
                            end;
                          asml.remove(hp2);
                          hp2.free;
                          result:=true;
                          exit;
                        end;
                    end
                  else
                    begin
                       { check further for
                              jCC   xxx
                              <several movs 1>
                              jmp   yyy
                      xxx:
                              <several movs 2>
                      yyy:
                       }
                      { hp2 points to jmp yyy }
                      hp2:=hp1;
                      { skip hp1 to xxx }
                      GetNextInstruction(hp1, hp1);
                      if assigned(hp2) and
                        assigned(hp1) and
                        (l<=3) and
                        (hp2.typ=ait_instruction) and
                        (taicpu(hp2).is_jmp) and
                        (taicpu(hp2).condition=C_None) and
                        { real label and jump, no further references to the
                          label are allowed }
                        (tasmlabel(taicpu(p).oper[0]^.ref^.symbol).getrefs=1) and
                        FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol),hp1) then
                         begin
                           l:=0;
                           { skip hp1 to <several moves 2> }
                           GetNextInstruction(hp1, hp1);
                           while assigned(hp1) and
                             CanBeCMOV(hp1) do
                             begin
                               inc(l);
                               GetNextInstruction(hp1, hp1);
                             end;
                           { hp1 points to yyy: }
                           if assigned(hp1) and
                             FindLabel(tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol),hp1) then
                             begin
                                condition:=inverse_cond(taicpu(p).condition);
                                GetNextInstruction(p,hp1);
                                hp3:=p;
                                p:=hp1;
                                repeat
                                  taicpu(hp1).opcode:=A_CMOVcc;
                                  taicpu(hp1).condition:=condition;
                                  GetNextInstruction(hp1,hp1);
                                until not(assigned(hp1)) or
                                  not(CanBeCMOV(hp1));
                                { hp2 is still at jmp yyy }
                                GetNextInstruction(hp2,hp1);
                                { hp2 is now at xxx: }
                                condition:=inverse_cond(condition);
                                GetNextInstruction(hp1,hp1);
                                { hp1 is now at <several movs 2> }
                                repeat
                                  taicpu(hp1).opcode:=A_CMOVcc;
                                  taicpu(hp1).condition:=condition;
                                  GetNextInstruction(hp1,hp1);
                                until not(assigned(hp1)) or
                                  not(CanBeCMOV(hp1));
                                {
                                asml.remove(hp1.next)
                                hp1.next.free;
                                asml.remove(hp1);
                                hp1.free;
                                }
                                { remove jCC }
                                tasmlabel(taicpu(hp3).oper[0]^.ref^.symbol).decrefs;
                                asml.remove(hp3);
                                hp3.free;
                                { remove jmp }
                                tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).decrefs;
                                asml.remove(hp2);
                                hp2.free;
                                result:=true;
                                exit;
                             end;
                         end;
                    end;
               end;
          end;
{$endif i8086}
      end;


    function TX86AsmOptimizer.OptPass1Movx(var p : tai) : boolean;
      var
        hp1,hp2: tai;
      begin
        result:=false;
        if (taicpu(p).oper[1]^.typ = top_reg) and
           GetNextInstruction(p,hp1) and
           (hp1.typ = ait_instruction) and
           IsFoldableArithOp(taicpu(hp1),taicpu(p).oper[1]^.reg) and
           GetNextInstruction(hp1,hp2) and
           MatchInstruction(hp2,A_MOV,[]) and
           (taicpu(hp2).oper[0]^.typ = top_reg) and
           OpsEqual(taicpu(hp2).oper[1]^,taicpu(p).oper[0]^) and
{$ifdef i386}
           { not all registers have byte size sub registers on i386 }
           ((taicpu(hp2).opsize<>S_B) or (getsupreg(taicpu(hp1).oper[0]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX])) and
{$endif i386}
           (((taicpu(hp1).ops=2) and
             (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg))) or
             ((taicpu(hp1).ops=1) and
             (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[0]^.reg)))) and
           not(RegUsedAfterInstruction(taicpu(hp2).oper[0]^.reg,hp2,UsedRegs)) then
          begin
            { change   movsX/movzX    reg/ref, reg2
                       add/sub/or/... reg3/$const, reg2
                       mov            reg2 reg/ref
              to       add/sub/or/... reg3/$const, reg/ref      }

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
            DebugMsg('PeepHole Optimization,var3',p);
            asml.remove(p);
            asml.remove(hp2);
            p.free;
            hp2.free;
            p:=hp1;
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
              begin
                case taicpu(p).opsize Of
                  S_BL, S_BW{$ifdef x86_64}, S_BQ{$endif x86_64}:
                    if (taicpu(hp1).oper[0]^.val = $ff) then
                      begin
                        DebugMsg('PeepHole Optimization,var4',p);
                        asml.remove(hp1);
                        hp1.free;
                      end;
                    S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                      if (taicpu(hp1).oper[0]^.val = $ffff) then
                        begin
                          DebugMsg('PeepHole Optimization,var5',p);
                          asml.remove(hp1);
                          hp1.free;
                        end;
{$ifdef x86_64}
                    S_LQ:
                      if (taicpu(hp1).oper[0]^.val = $ffffffff) then
                        begin
                          if (cs_asm_source in current_settings.globalswitches) then
                            asml.insertbefore(tai_comment.create(strpnew('PeepHole Optimization,var6')),p);
                          asml.remove(hp1);
                          hp1.Free;
                        end;
{$endif x86_64}
                    end;
              end;
            { changes some movzx constructs to faster synonims (all examples
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
                          DebugMsg('PeepHole Optimization,var7',p);
                        end
                      else if GetNextInstruction(p, hp1) and
                        (tai(hp1).typ = ait_instruction) and
                        (taicpu(hp1).opcode = A_AND) and
                        (taicpu(hp1).oper[0]^.typ = top_const) and
                        (taicpu(hp1).oper[1]^.typ = top_reg) and
                        (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                      { Change "movzbw %reg1, %reg2; andw $const, %reg2"
                        to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                        begin
                          DebugMsg('PeepHole Optimization,var8',p);
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
                        { Change "movzbl %al, %eax" to "andl $0x0ffh, %eax" }
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
                        { Change "movzbl %reg1, %reg2; andl $const, %reg2"
                          to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                        begin
                          DebugMsg('PeepHole Optimization,var10',p);
                          taicpu(p).opcode := A_MOV;
                          taicpu(p).changeopsize(S_L);
                          { do not use R_SUBWHOLE
                            as movl %rdx,%eax
                            is invalid in assembler PM }
                          setsubreg(taicpu(p).oper[0]^.reg, R_SUBD);
                          taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                        end
                    end;
{$ifndef i8086}
                  S_WL:
                    begin
                      if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                        not(cs_opt_size in current_settings.optimizerswitches) then
                        { Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax" }
                        begin
                          DebugMsg('PeepHole Optimization,var11',p);
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
                        { Change "movzwl %reg1, %reg2; andl $const, %reg2"
                          to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                        begin
                          DebugMsg('PeepHole Optimization,var12',p);
                          taicpu(p).opcode := A_MOV;
                          taicpu(p).changeopsize(S_L);
                          { do not use R_SUBWHOLE
                            as movl %rdx,%eax
                            is invalid in assembler PM }
                          setsubreg(taicpu(p).oper[0]^.reg, R_SUBD);
                          taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                        end;
                    end;
{$endif i8086}
                  end
                else if (taicpu(p).oper[0]^.typ = top_ref) then
                  begin
                    if GetNextInstruction(p, hp1) and
                      (tai(hp1).typ = ait_instruction) and
                      (taicpu(hp1).opcode = A_AND) and
                      MatchOpType(taicpu(hp1),top_const,top_reg) and
                      (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                      begin
                        taicpu(p).opcode := A_MOV;
                        case taicpu(p).opsize Of
                          S_BL:
                            begin
                              DebugMsg('PeepHole Optimization,var13',p);
                              taicpu(p).changeopsize(S_L);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                            end;
                          S_WL:
                            begin
                              DebugMsg('PeepHole Optimization,var14',p);
                              taicpu(p).changeopsize(S_L);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                            end;
                          S_BW:
                            begin
                              DebugMsg('PeepHole Optimization,var15',p);
                              taicpu(p).changeopsize(S_W);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                            end;
{$ifdef x86_64}
                          S_BQ:
                            begin
                              DebugMsg('PeepHole Optimization,var16',p);
                              taicpu(p).changeopsize(S_Q);
                              taicpu(hp1).loadConst(
                                0, taicpu(hp1).oper[0]^.val and $ff);
                            end;
                          S_WQ:
                            begin
                              DebugMsg('PeepHole Optimization,var17',p);
                              taicpu(p).changeopsize(S_Q);
                              taicpu(hp1).loadConst(0, taicpu(hp1).oper[0]^.val and $ffff);
                            end;
                          S_LQ:
                            begin
                              DebugMsg('PeepHole Optimization,var18',p);
                              taicpu(p).changeopsize(S_Q);
                              taicpu(hp1).loadConst(
                                0, taicpu(hp1).oper[0]^.val and $ffffffff);
                            end;
{$endif x86_64}
                          else
                            Internalerror(2017050704)
                        end;
                      end;
                  end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1AND(var p : tai) : boolean;
      var
        hp1 : tai;
      begin
        Result:=false;

        if not(GetNextInstruction(p, hp1)) then
          exit;

        if MatchOpType(taicpu(p),top_const,top_reg) and
          MatchInstruction(hp1,A_AND,[]) and
          MatchOpType(taicpu(hp1),top_const,top_reg) and
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
        else if MatchOpType(taicpu(p),top_const,top_reg) and
          MatchInstruction(hp1,A_MOVZX,[]) and
          (taicpu(hp1).oper[0]^.typ = top_reg) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) and
          (getsubreg(taicpu(hp1).oper[0]^.reg)=getsubreg(taicpu(hp1).oper[1]^.reg)) and
           (((taicpu(p).opsize=S_W) and
             (taicpu(hp1).opsize=S_BW)) or
            ((taicpu(p).opsize=S_L) and
             (taicpu(hp1).opsize in [S_WL,S_BL]))
{$ifdef x86_64}
              or
             ((taicpu(p).opsize=S_Q) and
              (taicpu(hp1).opsize in [S_BQ,S_WQ,S_LQ]))
{$endif x86_64}
            ) then
              begin
                if (((taicpu(hp1).opsize) in [S_BW,S_BL{$ifdef x86_64},S_BQ{$endif x86_64}]) and
                    ((taicpu(p).oper[0]^.val and $ff)=taicpu(p).oper[0]^.val)
                     ) or
                   (((taicpu(hp1).opsize) in [S_WL{$ifdef x86_64},S_WQ{$endif x86_64}]) and
                    ((taicpu(p).oper[0]^.val and $ffff)=taicpu(p).oper[0]^.val))
{$ifdef x86_64}
                    or
                   (((taicpu(hp1).opsize)=S_LQ) and
                    ((taicpu(p).oper[0]^.val and $ffffffff)=taicpu(p).oper[0]^.val)
                   )
{$endif x86_64}
                  then
                  begin
                    DebugMsg('Peephole AndMovzToAnd done',p);
                    asml.remove(hp1);
                    hp1.free;
                  end;
              end
        else if MatchOpType(taicpu(p),top_const,top_reg) and
          MatchInstruction(hp1,A_MOVSX{$ifdef x86_64},A_MOVSXD{$endif x86_64},[]) and
          (taicpu(hp1).oper[0]^.typ = top_reg) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) and
          (getsupreg(taicpu(hp1).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg)) and
           (((taicpu(p).opsize=S_W) and
             (taicpu(hp1).opsize=S_BW)) or
            ((taicpu(p).opsize=S_L) and
             (taicpu(hp1).opsize in [S_WL,S_BL]))
{$ifdef x86_64}
             or
             ((taicpu(p).opsize=S_Q) and
             (taicpu(hp1).opsize in [S_BQ,S_WQ,S_LQ]))
{$endif x86_64}
            ) then
              begin
                if (((taicpu(hp1).opsize) in [S_BW,S_BL{$ifdef x86_64},S_BQ{$endif x86_64}]) and
                    ((taicpu(p).oper[0]^.val and $7f)=taicpu(p).oper[0]^.val)
                     ) or
                   (((taicpu(hp1).opsize) in [S_WL{$ifdef x86_64},S_WQ{$endif x86_64}]) and
                    ((taicpu(p).oper[0]^.val and $7fff)=taicpu(p).oper[0]^.val))
{$ifdef x86_64}
                   or
                   (((taicpu(hp1).opsize)=S_LQ) and
                    ((taicpu(p).oper[0]^.val and $7fffffff)=taicpu(p).oper[0]^.val)
                   )
{$endif x86_64}
                   then
                   begin
                     DebugMsg('PeepHole Optimization,AndMovsxToAnd',p);
                     asml.remove(hp1);
                     hp1.free;
                   end;
              end
        else if (taicpu(p).oper[1]^.typ = top_reg) and
          (hp1.typ = ait_instruction) and
          (taicpu(hp1).is_jmp) and
          (taicpu(hp1).opcode<>A_JMP) and
          not(RegInUsedRegs(taicpu(p).oper[1]^.reg,UsedRegs)) then
          { change
              and x, reg
              jxx
            to
              test x, reg
              jxx
            if reg is deallocated before the
            jump, but only if it's a conditional jump (PFV)
          }
          taicpu(p).opcode := A_TEST;
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

