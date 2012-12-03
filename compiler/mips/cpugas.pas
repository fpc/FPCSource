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
      cpubase, aasmbase, globtype,
      aasmtai, aasmcpu, assemble, aggas;

    type
      TMIPSGNUAssembler = class(TGNUassembler)
        nomacro, noreorder, noat : boolean;
        constructor create(smart: boolean); override;
        {# Constructs the command line for calling the assembler }
        function MakeCmdLine: TCmdStr; override;
      end;

      TMIPSInstrWriter = class(TCPUInstrWriter)
        procedure WriteInstruction(hp : tai);override;
      end;

    const
      use_std_regnames : boolean = 
      {$ifndef USE_MIPS_GAS_REGS}
      true;
      {$else}
      false;
      {$endif}

  implementation

    uses
      cutils, systems, cpuinfo,
      verbose, itcpugas, cgbase, cgutils;

    function gas_std_regname(r:Tregister):string;
      var
        hr: tregister;
        p:  longint;
      begin
        { Double uses the same table as single }
        hr := r;
        case getsubreg(hr) of
          R_SUBFD:
            setsubreg(hr, R_SUBFS);
          R_SUBL, R_SUBW, R_SUBD, R_SUBQ:
           setsubreg(hr, R_SUBD);
        end;
        result:=std_regname(hr);
      end;


      function asm_regname(reg : TRegister) : string;

        begin
          if use_std_regnames then
            asm_regname:='$'+gas_std_regname(reg)
          else
            asm_regname:=gas_regname(reg);
        end;

{****************************************************************************}
{                         GNU MIPS  Assembler writer                           }
{****************************************************************************}

    constructor TMIPSGNUAssembler.create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter := TMIPSInstrWriter.create(self);
        nomacro:=false;
        noreorder:=false;
        noat:=false;
      end;

    function TMIPSGNUAssembler.MakeCmdLine: TCmdStr;
      begin
         result := Inherited MakeCmdLine;
         { ABI selection }
         Replace(result,'$ABI','-mabi='+abitypestr[mips_abi]);
         { ARCH selection }
         Replace(result,'$ARCH','-march='+cputypestr[mips_cpu]);
      end;

{****************************************************************************}
{                  Helper routines for Instruction Writer                    }
{****************************************************************************}

    function GetReferenceString(var ref: TReference): string;
      var
        hasgot : boolean;
        gotprefix : string;
      begin
        GetReferenceString := '';
        hasgot:=false;
        with ref do
        begin
          if (base = NR_NO) and (index = NR_NO) then
          begin
            if assigned(symbol) then
              begin
                GetReferenceString := symbol.Name;
                if symbol.typ=AT_FUNCTION then
                  gotprefix:='%call16('
                else
                  gotprefix:='%got(';
                hasgot:=true;
              end;
            if offset > 0 then
              GetReferenceString := GetReferenceString + '+' + ToStr(offset)
            else if offset < 0 then
              GetReferenceString := GetReferenceString + ToStr(offset);
            case refaddr of
              addr_high:
                GetReferenceString := '%hi(' + GetReferenceString + ')';
              addr_low:
                GetReferenceString := '%lo(' + GetReferenceString + ')';
              addr_pic:
                begin
                  if hasgot then
                    GetReferenceString := gotprefix + GetReferenceString + ')'
                  else
                    internalerror(2012070401);
                end;
            end;
          end
          else
          begin
      {$ifdef extdebug}
            if assigned(symbol) and
              not(refaddr in [addr_pic,addr_low]) then
              internalerror(2003052601);
      {$endif extdebug}
            if base <> NR_NO then
              GetReferenceString := GetReferenceString + '(' + asm_regname(base) + ')';
            if index = NR_NO then
            begin
              if offset <> 0 then
                GetReferenceString := ToStr(offset) + GetReferenceString;
              if assigned(symbol) then
              begin
                if refaddr = addr_low then
                  GetReferenceString := '%lo(' + symbol.Name + ')' + GetReferenceString
                else if refaddr = addr_pic then
                  begin
                    if symbol.typ=AT_FUNCTION then
                      gotprefix:='%call16('
                    else
                      gotprefix:='%got(';
                    GetReferenceString := gotprefix + symbol.Name + ')' + GetReferenceString;
                   end
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
              GetReferenceString := GetReferenceString + '(' + asm_regname(index) + ')';

            end;
          end;
        end;
      end;


    function getopstr(const Oper: TOper): string;
      begin
        with Oper do
          case typ of
            top_reg:
              getopstr := asm_regname(reg);
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

{
     function getnextfpreg(tmpfpu : shortstring) : shortstring;
     begin
       case length(tmpfpu) of
       3:
        if (tmpfpu[3] = '9') then
          tmpfpu:='$f10'
        else
          tmpfpu[3] := succ(tmpfpu[3]);
       4:
        if (tmpfpu[4] = '9') then
          tmpfpu:='$f20'
        else
          tmpfpu[4] := succ(tmpfpu[4]);
        else
          internalerror(20120531);
        end;
        getnextfpreg := tmpfpu;
     end;
}

    function is_macro_instruction(op : TAsmOp) : boolean;
      begin
        is_macro_instruction :=
          (op=A_SEQ) or (op=A_SNE);
      end;

    procedure TMIPSInstrWriter.WriteInstruction(hp: Tai);
      var
        Op: TAsmOp;
        s,s1:  string;
        i:  integer;
        tmpfpu: string;
        tmpfpu_len: longint;
        r: TRegister;
      begin
        if hp.typ <> ait_instruction then
          exit;
        op := taicpu(hp).opcode;

        case op of
          A_P_SET_NOMIPS16:
            begin
              s := #9 + '.set' + #9 + 'nomips16';
              owner.AsmWriteLn(s);
            end;
          A_P_MASK,
          A_P_FMASK:
            begin
              s := #9 + gas_op2str[op] + #9'0x' + hexstr(taicpu(hp).oper[0]^.val,8)+ ',' + getopstr(taicpu(hp).oper[1]^) ;
              owner.AsmWriteLn(s);
            end;
          A_P_SET_MACRO:
            begin
              s := #9 + '.set' + #9 + 'macro';
              owner.AsmWriteLn(s);
              TMIPSGNUAssembler(owner).nomacro:=false;
            end;
          A_P_SET_REORDER:
            begin
              s := #9 + '.set' + #9 + 'reorder';
              owner.AsmWriteLn(s);
              TMIPSGNUAssembler(owner).noreorder:=false;
            end;
          A_P_SET_NOMACRO:
            begin
              s := #9 + '.set' + #9 + 'nomacro';
              owner.AsmWriteLn(s);
              TMIPSGNUAssembler(owner).nomacro:=true;
            end;
          A_P_SET_NOREORDER:
            begin
              s := #9 + '.set' + #9 + 'noreorder';
              owner.AsmWriteLn(s);
              TMIPSGNUAssembler(owner).noreorder:=true;
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
              if (target_info.endian = endian_big) then
                begin
                  s := #9 + gas_op2str[A_LDC1] + #9 + getopstr(taicpu(hp).oper[0]^)
                       + ',' + getopstr(taicpu(hp).oper[1]^);
                end
              else
                begin
                  tmpfpu := getopstr(taicpu(hp).oper[0]^);
                  s := #9 + gas_op2str[A_LWC1] + #9 + tmpfpu + ',' + getopstr(taicpu(hp).oper[1]^); // + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
                  owner.AsmWriteLn(s);

{ bug if $f9/$f19
              tmpfpu_len := length(tmpfpu);
              tmpfpu[tmpfpu_len] := succ(tmpfpu[tmpfpu_len]);

}
                  r := taicpu(hp).oper[0]^.reg;
                  setsupreg(r, getsupreg(r) + 1);
                  tmpfpu := asm_regname(r);
                  s := #9 + gas_op2str[A_LWC1] + #9 + tmpfpu + ',' + getopstr_4(taicpu(hp).oper[1]^); // + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
                end;
              owner.AsmWriteLn(s);
            end;
          A_SDC1:
            begin
              if (target_info.endian = endian_big) then
                begin
                  s := #9 + gas_op2str[A_SDC1] + #9 + getopstr(taicpu(hp).oper[0]^)
                       + ',' + getopstr(taicpu(hp).oper[1]^);
                end
              else
                begin
                  tmpfpu := getopstr(taicpu(hp).oper[0]^);
                  s := #9 + gas_op2str[A_SWC1] + #9 + tmpfpu + ',' + getopstr(taicpu(hp).oper[1]^); //+ ',' + getopstr(taicpu(hp).oper[2]^) + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
                  owner.AsmWriteLn(s);

{
              tmpfpu_len := length(tmpfpu);
              tmpfpu[tmpfpu_len] := succ(tmpfpu[tmpfpu_len]);
}
                  r := taicpu(hp).oper[0]^.reg;
                  setsupreg(r, getsupreg(r) + 1);
                  tmpfpu := asm_regname(r);
                  s := #9 + gas_op2str[A_SWC1] + #9 + tmpfpu + ',' + getopstr_4(taicpu(hp).oper[1]^); //+ ',' + getopstr(taicpu(hp).oper[2]^) + '(' + getopstr(taicpu(hp).oper[1]^) + ')';
                end;
              owner.AsmWriteLn(s);
            end;
          else
            begin
              if is_macro_instruction(op) and TMIPSGNUAssembler(owner).nomacro then
                owner.AsmWriteln(#9'.set'#9'macro');
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
              if is_macro_instruction(op) and TMIPSGNUAssembler(owner).nomacro then
                owner.AsmWriteln(#9'.set'#9'nomacro');
            end;
        end;
      end;


    const
      as_MIPSEL_as_info: tasminfo =
        (
        id: as_gas;
        idtxt: 'AS';
        asmbin: 'as';
        asmcmd: '$ABI $ARCH $NOWARN -EL $PIC -o $OBJ $ASM';
        supported_targets: [system_mipsel_linux];
        flags: [ af_needar, af_smartlink_sections];
        labelprefix: '.L';
        comment: '# ';
        dollarsign: '$';
        );
      as_MIPSEB_as_info: tasminfo =
        (
        id: as_gas;
        idtxt: 'AS';
        asmbin: 'as';
        asmcmd: '$ABI $ARCH $NOWARN -EB $PIC -o $OBJ $ASM';
        supported_targets: [system_mipseb_linux];
        flags: [ af_needar, af_smartlink_sections];
        labelprefix: '.L';
        comment: '# ';
        dollarsign: '$';
        );

begin
{$ifdef MIPSEL}
  RegisterAssembler(as_MIPSEL_as_info, TMIPSGNUAssembler);
{$else MIPSEL}
  RegisterAssembler(as_MIPSEB_as_info, TMIPSGNUAssembler);
{$endif MIPSEL}
end.
