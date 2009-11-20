{
    Copyright (c) 1999-2009 by Florian Klaempfl and David Zhang

    This unit implements an asmoutput class for MIPS assembly syntax

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
unit cpugas;

{$i fpcdefs.inc}

  interface

    uses
      cpubase,
       aasmtai, aasmcpu, assemble, aggas;

    type
      TMIPSGNUAssembler = class(TGNUassembler)
        constructor create(smart: boolean); override;
      end;

      TMIPSInstrWriter = class(TCPUInstrWriter)
        procedure WriteInstruction(hp : tai);override;
      end;

  implementation

    uses
      cutils, systems,
      verbose, itcpugas, cgbase, cgutils;

{****************************************************************************}
{                         GNU MIPS  Assembler writer                           }
{****************************************************************************}

    constructor TMIPSGNUAssembler.create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter := TMIPSInstrWriter.create(self);
      end;


{****************************************************************************}
{                  Helper routines for Instruction Writer                    }
{****************************************************************************}

    function GetReferenceString(var ref: TReference): string;
      begin
        GetReferenceString := '';
        with ref do
        begin
          if (base = NR_NO) and (index = NR_NO) then
          begin
            if assigned(symbol) then
              GetReferenceString := symbol.Name;
            if offset > 0 then
              GetReferenceString := GetReferenceString + '+' + ToStr(offset)
            else if offset < 0 then
              GetReferenceString := GetReferenceString + ToStr(offset);
            case refaddr of
              addr_high:
                GetReferenceString := '%hi(' + GetReferenceString + ')';
              addr_low:
                GetReferenceString := '%lo(' + GetReferenceString + ')';
            end;
          end
          else
          begin
      {$ifdef extdebug}
            if assigned(symbol) and
              not(refaddr in [addr_pic,addr_lo]) then
              internalerror(2003052601);
      {$endif extdebug}
            if base <> NR_NO then
              GetReferenceString := GetReferenceString + '(' + gas_regname(base) + ')';
            if index = NR_NO then
            begin
              if offset <> 0 then
                GetReferenceString := ToStr(offset) + GetReferenceString;
              if assigned(symbol) then
              begin
                if refaddr = addr_low then
                  GetReferenceString := '%lo(' + symbol.Name + ')' + GetReferenceString
                else
                  GetReferenceString := symbol.Name + {'+' +} GetReferenceString;
              end;
            end
            else
            begin
  {$ifdef extdebug}
              if (Offset<>0) or assigned(symbol) then
                internalerror(2003052603);
  {$endif extdebug}
              GetReferenceString := GetReferenceString + '(' + gas_regname(index) + ')';

            end;
          end;
        end;
      end;


    function getopstr(const Oper: TOper): string;
      begin
        with Oper do
          case typ of
            top_reg:
              getopstr := gas_regname(reg);
            top_const:
              getopstr := tostr(longint(val));
            top_ref:
              if (oper.ref^.refaddr in [addr_no, addr_pic]) or ((oper.ref^.refaddr = addr_low) and ((oper.ref^.base <> NR_NO) or
                (oper.ref^.index <> NR_NO))) then
                getopstr := getreferencestring(ref^)
              else
                getopstr := getreferencestring(ref^);
            else
              internalerror(10001);
          end;
      end;

      function getopstr_4(const Oper: TOper): string;
      var
        tmpref: treference;
      begin
        with Oper do
          case typ of
            top_ref:
            begin
              tmpref := ref^;
              Inc(tmpref.offset, 4);
              getopstr_4 := getreferencestring(tmpref);
            end;
            else
              internalerror(2007050403);
          end;
      end;


    procedure TMIPSInstrWriter.WriteInstruction(hp: Tai);
      var
        Op: TAsmOp;
        s,s1:  string;
        i:  integer;
        tmpfpu: string;
        tmpfpu_len: longint;
      begin
        if hp.typ <> ait_instruction then
          exit;
        op := taicpu(hp).opcode;

        case op of
          A_P_STK2:
          begin
            s1 := getopstr(taicpu(hp).oper[2]^);
            STK2_LocalSize := align(STK2_LocalSize, 8);
            if s1[1] = '-' then
              str(-STK2_LocalSize, s1)
            else
              str(STK2_LocalSize, s1);
            s := #9 + gas_op2str[A_ADDIU] + #9 + getopstr(taicpu(hp).oper[0]^)+ ',' + getopstr(taicpu(hp).oper[1]^) + ',' + s1;
            owner.AsmWriteLn(s);
          end;
          A_P_FRAME:
          begin
          end;
          A_P_SET_MACRO:
          begin
            s := #9 + '.set' + #9 + 'macro';
            owner.AsmWriteLn(s);
          end;
          A_P_SET_REORDER:
          begin
            s := #9 + '.set' + #9 + 'reorder';
            owner.AsmWriteLn(s);
          end;
          A_P_SET_NOMACRO:
          begin
            s := #9 + '.set' + #9 + 'nomacro';
            owner.AsmWriteLn(s);
          end;
          A_P_SET_NOREORDER:
          begin
            s := #9 + '.set' + #9 + 'noreorder';
            owner.AsmWriteLn(s);
          end;
          A_P_SW:
          begin
            s := #9 + gas_op2str[A_SW] + #9 + getopstr(taicpu(hp).oper[0]^)+ ',' + getopstr(taicpu(hp).oper[2]^) + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
            owner.AsmWriteLn(s);
          end;
          A_P_LW:
          begin
            s := #9 + gas_op2str[A_LW] + #9 + getopstr(taicpu(hp).oper[0]^)+ ',' + getopstr(taicpu(hp).oper[2]^) + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
            owner.AsmWriteLn(s);
          end;
          A_LDC1:
          begin
            tmpfpu := getopstr(taicpu(hp).oper[0]^);
            s := #9 + gas_op2str[A_LWC1] + #9 + tmpfpu + ',' + getopstr(taicpu(hp).oper[1]^); // + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
            owner.AsmWriteLn(s);

            tmpfpu_len := length(tmpfpu);
            tmpfpu[tmpfpu_len] := succ(tmpfpu[tmpfpu_len]);
            s := #9 + gas_op2str[A_LWC1] + #9 + tmpfpu + ',' + getopstr_4(taicpu(hp).oper[1]^); // + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
            owner.AsmWriteLn(s);
          end;
          A_SDC1:
          begin
            tmpfpu := getopstr(taicpu(hp).oper[0]^);
            s := #9 + gas_op2str[A_SWC1] + #9 + tmpfpu + ',' + getopstr(taicpu(hp).oper[1]^); //+ ',' + getopstr(taicpu(hp).oper[2]^) + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
            owner.AsmWriteLn(s);

            tmpfpu_len := length(tmpfpu);
            tmpfpu[tmpfpu_len] := succ(tmpfpu[tmpfpu_len]);
            s := #9 + gas_op2str[A_SWC1] + #9 + tmpfpu + ',' + getopstr_4(taicpu(hp).oper[1]^); //+ ',' + getopstr(taicpu(hp).oper[2]^) + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
            owner.AsmWriteLn(s);
          end;
          else
          begin
            s := #9 + gas_op2str[op] + cond2str[taicpu(hp).condition];
            if taicpu(hp).delayslot_annulled then
              s := s + ',a';
            if taicpu(hp).ops > 0 then
            begin
              s := s + #9 + getopstr(taicpu(hp).oper[0]^);
              for i := 1 to taicpu(hp).ops - 1 do
                s := s + ',' + getopstr(taicpu(hp).oper[i]^);
            end;
            owner.AsmWriteLn(s);
          end;
        end;
      end;


    const
      as_MIPSEL_as_info: tasminfo =
        (
        id: as_gas;
        idtxt: 'AS';
        asmbin: 'as';
        asmcmd: '-mips2 -W -EL -o $OBJ $ASM';
        supported_targets: [system_mips_linux];
        flags: [af_allowdirect, af_needar, af_smartlink_sections];
        labelprefix: '.L';
        comment: '# ';
        );

begin
  RegisterAssembler(as_MIPSEL_as_info, TMIPSGNUAssembler);
end.
