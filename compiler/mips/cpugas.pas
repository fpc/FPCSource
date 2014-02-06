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
      globals, verbose, itcpugas, cgbase, cgutils;

    function gas_std_regname(r:Tregister):string;
      var
        hr: tregister;
      begin
        { Double uses the same table as single }
        hr := r;
        case getsubreg(hr) of
          R_SUBFD:
            setsubreg(hr, R_SUBFS);
          R_SUBL, R_SUBW, R_SUBD, R_SUBQ:
           setsubreg(hr, R_SUBD);
        end;
        if getregtype(r)=R_SPECIALREGISTER then
          result:=tostr(getsupreg(r))
        else
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
         Replace(result,'$ARCH','-march='+lower(cputypestr[current_settings.cputype]));
      end;

{****************************************************************************}
{                  Helper routines for Instruction Writer                    }
{****************************************************************************}

    function GetReferenceString(var ref: TReference): string;
      var
        reg: TRegister;
        regstr: string;
      begin
        result:='';
        if assigned(ref.symbol) then
          result:=ref.symbol.name;
        if (ref.offset<0) then
          result:=result+tostr(ref.offset)
        else if (ref.offset>0) then
          begin
            if assigned(ref.symbol) then
              result:=result+'+';
            result:=result+tostr(ref.offset);
          end
        { asmreader appears to treat literal numbers as references }
        else if (ref.symbol=nil) and (ref.base=NR_NO) and (ref.index=NR_NO) then
          result:='0';

        { either base or index may be present, but not both }
        reg:=ref.base;
        if (reg=NR_NO) then
          reg:=ref.index
        else if (ref.index<>NR_NO) then
          InternalError(2013013001);

        if (reg=NR_NO) then
          regstr:=''
        else
          regstr:='('+asm_regname(reg)+')';

        case ref.refaddr of
          addr_no,
          addr_full:
            if assigned(ref.symbol) and (reg<>NR_NO) then
              InternalError(2013013002)
            else
              begin
                result:=result+regstr;
                exit;
              end;
          addr_pic:
            result:='%got('+result;
          addr_high:
            result:='%hi('+result;
          addr_low:
            result:='%lo('+result;
          addr_pic_call16:
            result:='%call16('+result;
          addr_low_pic:
            result:='%got_lo('+result;
          addr_high_pic:
            result:='%got_hi('+result;
          addr_low_call:
            result:='%call_lo('+result;
          addr_high_call:
            result:='%call_hi('+result;
        else
          InternalError(2013013003);
        end;

        result:=result+')'+regstr;
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

    function is_macro_instruction(ai : taicpu) : boolean;
      var
        op: tasmop;
      begin
        op:=ai.opcode;
        is_macro_instruction :=
        { 'seq', 'sge', 'sgeu', 'sgt', 'sgtu', 'sle', 'sleu', 'sne', }
          (op=A_SEQ) or (op = A_SGE) or (op=A_SGEU) or (op=A_SGT) or
          (op=A_SGTU) or (op=A_SLE) or (op=A_SLEU) or (op=A_SNE)
          { JAL is not here! See comments in TCGMIPS.a_call_name. }
          or (op=A_LA) or ((op=A_BC) and
            not (ai.condition in [C_EQ,C_NE,C_GTZ,C_GEZ,C_LTZ,C_LEZ,C_COP1TRUE,C_COP1FALSE])) {or (op=A_JAL)}
          or (op=A_REM) or (op=A_REMU)
          { DIV and DIVU are normally macros, but use $zero as first arg to generate a CPU instruction. }
          or ((op=A_DIV) or (op=A_DIVU) and
            ((ai.ops<>3) or (ai.oper[0]^.typ<>top_reg) or (ai.oper[0]^.reg<>NR_R0)))
          or (op=A_MULO) or (op=A_MULOU)
          { A_LI is only a macro if the immediate is not in thez 16-bit range }
          or (op=A_LI);
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
              owner.AsmWriteLn(#9'.set'#9'nomips16');
            end;
          A_P_MASK,
          A_P_FMASK:
            begin
              s := #9 + gas_op2str[op] + #9'0x' + hexstr(taicpu(hp).oper[0]^.val,8)+ ',' + getopstr(taicpu(hp).oper[1]^) ;
              owner.AsmWriteLn(s);
            end;
          A_P_SET_MACRO:
            begin
              owner.AsmWriteLn(#9'.set'#9'macro');
              TMIPSGNUAssembler(owner).nomacro:=false;
            end;
          A_P_SET_REORDER:
            begin
              owner.AsmWriteLn(#9'.set'#9'reorder');
              TMIPSGNUAssembler(owner).noreorder:=false;
            end;
          A_P_SET_NOMACRO:
            begin
              owner.AsmWriteLn(#9'.set'#9'nomacro');
              TMIPSGNUAssembler(owner).nomacro:=true;
            end;
          A_P_SET_NOREORDER:
            begin
              owner.AsmWriteLn(#9'.set'#9'noreorder');
              TMIPSGNUAssembler(owner).noreorder:=true;
            end;
          A_P_SET_NOAT:
            begin
              owner.AsmWriteln(#9'.set'#9'noat');
              TMIPSGNUAssembler(owner).noat:=true;
            end;
          A_P_SET_AT:
            begin
              owner.AsmWriteln(#9'.set'#9'at');
              TMIPSGNUAssembler(owner).noat:=false;
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
              if is_macro_instruction(taicpu(hp)) and TMIPSGNUAssembler(owner).nomacro then
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
              if is_macro_instruction(taicpu(hp)) and TMIPSGNUAssembler(owner).nomacro then
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
        asmcmd: '$ABI $ARCH $NOWARN -EL $PIC -o $OBJ $EXTRAOPT $ASM';
        supported_targets: [system_mipsel_linux,system_mipsel_android];
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
        asmcmd: '$ABI $ARCH $NOWARN -EB $PIC -o $OBJ $EXTRAOPT $ASM';
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
