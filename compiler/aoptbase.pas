{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the base of all optimizer related objects

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
unit aoptbase;

{$i fpcdefs.inc}

  interface

    uses
      aasmbase,aasmcpu,aasmtai,aasmdata,
      cpubase,
      cgbase,
      cgutils;

    Type
      { the number of tai objects processed by an optimizer object since the last
        time a register was modified                                              }
      { size at each dimension depends on the registers of this type }
      TInstrSinceLastMod = Array[tregistertype] of pbyte;

    { the TAopBase object implements the basic methods that most other }
    { assembler optimizer objects require                              }
    Type
      TAoptBase = class
        { processor independent methods }

        constructor create; virtual;
        destructor destroy;override;
        { returns true if register Reg is used by instruction p1 }
        Function RegInInstruction(Reg: TRegister; p1: tai): Boolean;virtual;
        { returns true if register Reg occurs in operand op }
        class function RegInOp(Reg: TRegister; const op: toper): Boolean; static;
        { returns true if register Reg is used in the reference Ref }
        class function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean; static;

        function RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean;virtual;

        { returns true if the references are completely equal }
        {Function RefsEqual(Const R1, R2: TReference): Boolean;}

        { gets the next tai object after current that contains info relevant }
        { to the optimizer in p1. If there is none, it returns false and     }
        { sets p1 to nil.  If AlsoStopOn is set, it will also stop on these  }
        { object types that are normally skipped over.                       }
        class function GetNextInstruction(Current: tai; out Next: tai; AlsoStopOn: taitypes = []): Boolean; static;

        { gets the previous tai object after current that contains info   }
        { relevant to the optimizer in last. If there is none, it returns }
        { false and sets last to nil.  If AlsoStopOn is set, it will also }
        { stop on these object types that are normally skipped over.      }
        class function GetLastInstruction(Current: tai; out Last: tai; AlsoStopOn: taitypes = []): Boolean; static;

        class function SkipEntryExitMarker(current: tai; out next: tai): boolean; static;

        { processor dependent methods }

        { returns the maximum width component of Reg. Only has to be }
        { overridden for the 80x86 (afaik)                           }
        Function RegMaxSize(Reg: TRegister): TRegister; Virtual;
        { returns true if Reg1 and Reg2 are of the samae width. Only has to }
        { overridden for the 80x86 (afaik)                                  }
        Function RegsSameSize(Reg1, Reg2: TRegister): Boolean; Virtual;
        { returns whether P is a load instruction (load contents from a }
        { memory location or (register) variable into a register)       }
        Function IsLoadMemReg(p: tai): Boolean; Virtual; Abstract;
        { returns whether P is a load constant instruction (load a constant }
        { into a register)                                                  }
        Function IsLoadConstReg(p: tai): Boolean; Virtual; Abstract;
        { returns whether P is a store instruction (store contents from a
          register to a memory location or to a (register) variable)      }
        Function IsStoreRegMem(p: tai): Boolean; Virtual; Abstract;

        { create a paicpu Object that loads the contents of reg1 into reg2 }
        Function a_load_reg_reg(reg1, reg2: TRegister): taicpu; Virtual; Abstract;

        { returns true if reg is used by any instruction between p1 and p2 }
        Function RegUsedBetween(reg: TRegister; p1, p2: tai): Boolean;

        { returns true if reg is modified by any instruction between p1 and p2 }
        function RegModifiedBetween(reg: TRegister; p1, p2: tai): Boolean;

        { returns true if reg is loaded with a new value by hp }
        function RegLoadedWithNewValue(reg: tregister; hp: tai): boolean; Virtual;

        { returns true if hp loads a value from reg }
        function InstructionLoadsFromReg(const reg : TRegister; const hp : tai) : boolean; Virtual;

        { compares reg1 and reg2 having the same type and being the same super registers
          so the register size is neglected }
        class function SuperRegistersEqual(reg1,reg2 : TRegister) : Boolean; static; {$ifdef USEINLINE}inline;{$endif}

        { returns true if changing reg1 changes reg2 or vice versa }
        class function RegistersInterfere(reg1,reg2 : TRegister) : Boolean; static; {$ifdef USEINLINE}inline;{$endif}
    end;

    function labelCanBeSkipped(p: tai_label): boolean; {$ifdef USEINLINE}inline;{$endif}

  implementation

    uses
      verbose,globals,aoptcpub;

  constructor taoptbase.create;
    begin
      inherited create;
    end;


  destructor taoptbase.destroy;
    begin
      inherited destroy;
    end;


  Function TAOptBase.RegInInstruction(Reg: TRegister; p1: tai): Boolean;
    Var
      Count: longint;
    Begin
      result:=false;
      if p1.typ<>ait_instruction then
        exit;
      for Count:=0 to TInstr(p1).ops-1 do
        if RegInOp(Reg, TInstr(p1).oper[Count]^) then
          exit(true);
      result:=false;
    End;


  class function TAOptBase.RegInOp(Reg: TRegister; const op: toper): Boolean;
    Begin
      Case op.typ Of
        Top_Reg: RegInOp := RegistersInterfere(Reg,op.reg);
        Top_Ref: RegInOp := RegInRef(Reg, op.ref^);
        {$ifdef arm}
        Top_Shifterop: RegInOp := op.shifterop^.rs = Reg;
        Top_RegSet: RegInOp := getsupreg(Reg) in op.regset^;
        {$endif arm}
        Else RegInOp := False
      End
    End;


  class function TAOptBase.RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
  Begin
    RegInRef := RegistersInterfere(Ref.Base,Reg)
{$ifdef cpurefshaveindexreg}
    Or RegistersInterfere(Ref.Index,Reg)
{$endif cpurefshaveindexreg}
{$ifdef x86}
    or (Reg=Ref.segment)
    { if Ref.segment isn't set, the cpu uses implicitly ss or ds, depending on the base register }
    or ((Ref.segment=NR_NO) and (
      ((Reg=NR_SS) and (RegistersInterfere(Ref.base,NR_EBP) or RegistersInterfere(Ref.base,NR_ESP))) or
      ((Reg=NR_DS) and not(RegistersInterfere(Ref.base,NR_EBP) or RegistersInterfere(Ref.base,NR_ESP)))
    ))
{$endif x86}
  End;

  Function TAOptBase.RegModifiedByInstruction(Reg: TRegister; p1: tai): Boolean;
  Begin
    Result:=true;
  End;


  function labelCanBeSkipped(p: tai_label): boolean; {$ifdef USEINLINE}inline;{$endif}
  begin
    labelCanBeSkipped := not(p.labsym.is_used) or (p.labsym.labeltype<>alt_jump);
  end;


  class function TAOptBase.GetNextInstruction(Current: tai; out Next: tai; AlsoStopOn: taitypes): Boolean;
  Begin
    Repeat
      Current := tai(Current.Next);
      While Assigned(Current) And
            ((Current.typ In SkipInstr - AlsoStopOn) or
{$ifdef cpudelayslot}
             ((Current.typ=ait_instruction) and
              (taicpu(Current).opcode=A_NOP)
             ) or
{$endif cpudelayslot}
             ((Current.typ = ait_label) And
              labelCanBeSkipped(Tai_Label(Current)))) Do
        begin
          { this won't help the current loop, but it helps when returning from GetNextInstruction
            as the next entry is probably already in the cache }
          prefetch(pointer(Current.Next)^);
          Current := Tai(Current.Next);
        end;
      If Assigned(Current) And
         (Current.typ = ait_Marker) And
         (Tai_Marker(Current).Kind = mark_NoPropInfoStart) Then
        Begin
          While Assigned(Current) And
                ((Current.typ <> ait_Marker) Or
                 (Tai_Marker(Current).Kind <> mark_NoPropInfoEnd)) Do
            begin
              { this won't help the current loop, but it helps when returning from GetNextInstruction
                as the next entry is probably already in the cache }
              prefetch(pointer(Current.Next)^);
              Current := Tai(Current.Next);
            end;
        End;
    Until Not(Assigned(Current)) Or
          (Current.typ <> ait_Marker) Or
          (Tai_Marker(Current).Kind <> mark_NoPropInfoEnd);
    Next := Current;
    If Assigned(Current) And
       Not((Current.typ In SkipInstr - AlsoStopOn) or
           ((Current.typ = ait_label) And
            labelCanBeSkipped(Tai_Label(Current))))
      Then GetNextInstruction := True
      Else
        Begin
          Next := Nil;
          GetNextInstruction := False;
        End;
  End;


  class function TAOptBase.GetLastInstruction(Current: tai; out Last: tai; AlsoStopOn: taitypes): Boolean;
  Begin
    Repeat
      Current := Tai(Current.previous);
      While Assigned(Current) And
            (((Current.typ = ait_Marker) And
              Not(Tai_Marker(Current).Kind in [mark_AsmBlockEnd{,mark_NoPropInfoEnd}])) or
             (Current.typ In SkipInstr - AlsoStopOn) or
             ((Current.typ = ait_label) And
              labelCanBeSkipped(Tai_Label(Current)))) Do
        Current := Tai(Current.previous);
{      If Assigned(Current) And
         (Current.typ = ait_Marker) And
         (Tai_Marker(Current).Kind = mark_NoPropInfoEnd) Then
        Begin
          While Assigned(Current) And
                ((Current.typ <> ait_Marker) Or
                 (Tai_Marker(Current).Kind <> mark_NoPropInfoStart)) Do
            Current := Tai(Current.previous);
        End; }
    Until Not(Assigned(Current)) Or
          (Current.typ <> ait_Marker) Or
          not(tai_Marker(current).Kind in [mark_NoPropInfoStart,mark_NoPropInfoEnd]);
    If Not(Assigned(Current)) or
       (Current.typ In SkipInstr - AlsoStopOn) or
       ((Current.typ = ait_label) And
        labelCanBeSkipped(Tai_Label(Current))) or
       ((Current.typ = ait_Marker) And
        (Tai_Marker(Current).Kind = mark_AsmBlockEnd))
      Then
        Begin
          Last := Nil;
          GetLastInstruction := False
        End
      Else
        Begin
          Last := Current;
          GetLastInstruction := True;
        End;
  End;


  class function TAOptBase.SkipEntryExitMarker(current: tai; out next: tai): boolean;
    begin
      result:=true;
      next:=current;
      if current.typ<>ait_marker then
        exit;
      while GetNextInstruction(next,next) do
        begin
          if (next.typ<>ait_marker) or not(tai_marker(next).Kind in [mark_Position,mark_BlockStart]) then
            exit;
        end;
      result:=false;
    end;


  Function TAOptBase.RegUsedBetween(reg : TRegister;p1,p2 : tai) : Boolean;
  Begin
    Result:=false;
    while assigned(p1) and assigned(p2) and GetNextInstruction(p1,p1) and (p1<>p2) do
      if RegInInstruction(reg,p1) then
        begin
          Result:=true;
          exit;
        end;
  end;


  Function TAOptBase.RegModifiedBetween(reg : TRegister;p1,p2 : tai) : Boolean;
  Begin
    Result:=false;
    while assigned(p1) and assigned(p2) and GetNextInstruction(p1,p1) and (p1<>p2) do
      if RegModifiedByInstruction(reg,p1) then
        begin
          Result:=true;
          exit;
        end;
  end;


  function TAoptBase.RegLoadedWithNewValue(reg : tregister; hp : tai) : boolean;
    begin
      result:=false;
      internalerror(2016012401);
    end;


  function TAoptBase.InstructionLoadsFromReg(const reg : TRegister; const hp : tai) : boolean;
    begin
      { save approximation }
      Result:=true;
    end;


  class function TAOptBase.SuperRegistersEqual(reg1,reg2 : TRegister) : Boolean;{$ifdef USEINLINE}inline;{$endif}
  Begin
    { Do an optimized version of

      Result:=(getregtype(reg1) = getregtype(reg2)) and
      (getsupreg(reg1) = getsupreg(Reg2));

      as SuperRegistersEqual is used a lot
    }
{$ifdef Z80}
    { Z80 registers are indexed in an incompatible way (without R_SUBH), so it
      needs a special check. }
    Result:=super_registers_equal(reg1,reg2);
{$else Z80}
    Result:=(DWord(reg1) and $ff00ffff)=(DWord(reg2) and $ff00ffff);
{$endif Z80}
  end;


  class function TAOptBase.RegistersInterfere(reg1,reg2 : TRegister) : Boolean; static; {$ifdef USEINLINE}inline;{$endif}
    begin
{$ifdef Z80}
      result:=registers_interfere(reg1,reg2);
{$else Z80}
      result:=SuperRegistersEqual(reg1,reg2);
{$endif Z80}
    end;

  { ******************* Processor dependent stuff *************************** }

  Function TAOptBase.RegMaxSize(Reg: TRegister): TRegister;
  Begin
    RegMaxSize := Reg
  End;

  Function TAOptBase.RegsSameSize(Reg1, Reg2: TRegister): Boolean;
  Begin
    RegsSameSize := True
  End;

end.
