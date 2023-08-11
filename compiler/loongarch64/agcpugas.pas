{
    Copyright (C) 2022 Loongson Technology Corporation Limited.

    This unit the GAS asm writers for LoongArch64

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

{****************************************************************************}
{                  Helper routines for Instruction Writer                    }
{****************************************************************************}

unit agcpugas;

{$i fpcdefs.inc}

  interface

    uses
       systems,aasmbase,
       aasmtai,aasmdata,
       aggas,
       assemble,
       cpubase,cgutils,
       globtype;

  type
    TLoongArch64InstrWriter=class(TCPUInstrWriter)
       procedure WriteInstruction(hp : tai);override;
    end;

    TLoongArch64GNUAssembler=class(TGNUassembler)
      constructor CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean); override;
    end;

  var
    curop: TAsmOp;

  implementation

    uses
       cutils,globals,verbose,
       cgbase,rgbase,
       itcpugas,cpuinfo,
       aasmcpu;

    function getreferencestring(asminfo: pasminfo; var ref : treference) : string;
    var
      s : string;
    begin
      { For LoongArch, we support only 4 cases
        1. Symbol, symbol + addend (cannot used by got).
        2. Reg + offset.
        3. Reg + reg.
        4. Reg
      }
      with ref do
        begin
          if assigned(relsymbol) then
            internalerror(2022081001);
          case refaddr of
            addr_b16, addr_b21, addr_b26, addr_pcrel,
            addr_plt, addr_abs_hi20, addr_abs_lo12,
            addr_abs64_lo20, addr_abs64_hi12,
            addr_pc_hi20, addr_got_pc_hi20,
            addr_pc_lo12, addr_got_pc_lo12,
            addr_got, addr_abs:
              begin
                if (base<>NR_NO) or (index<>NR_NO) then
                  internalerror(2022081002)
                else if (refaddr in [addr_got_pc_hi20,addr_got_pc_lo12,addr_got]) and (offset<>0) then
                  internalerror(2022081003)
                else if not assigned(symbol) then
                  internalerror(2022081004);
                s:=ApplyAsmSymbolRestrictions(symbol.name);
                if offset<0 then
                  s:=s+tostr(offset)
                else if offset>0 then
                  s:=s+'+'+tostr(offset);
                case refaddr of
                  addr_b16: s:='%b16('+s+')';
                  addr_b21: s:='%b21('+s+')';
                  addr_b26: s:='%b26('+s+')';
                  addr_plt: s:='%plt('+s+')';
                  addr_abs_hi20: s:='%abs_hi20('+s+')';
                  addr_abs_lo12: s:='%abs_lo12('+s+')';
                  addr_abs64_lo20: s:='%abs64_lo20('+s+')';
                  addr_abs64_hi12: s:='%abs64_hi12('+s+')';
                  addr_pc_hi20: s:='%pc_hi20('+s+')';
                  addr_got_pc_hi20: s:='%got_pc_hi20('+s+')';
                  addr_got_pc_lo12: s:='%got_pc_lo12('+s+')';
                  addr_pc_lo12: s:='%pc_lo12('+s+')';
                else
                  ;
                end;
              end;
            addr_reg_reg:
              begin
                if assigned(symbol) or (offset<>0) then
                  internalerror(2022081005)
                else if (base=NR_NO) or (index=NR_NO) then
                  internalerror(2022081006);
                s:=gas_regname(base)+','+gas_regname(index);
              end;
            addr_reg_12i, addr_reg_14i:
              begin
                if assigned(symbol) or (index<>NR_NO) then
                  internalerror(2022081007)
                else if (refaddr=addr_reg_12i) and (not is_simm12(offset)) then
                  internalerror(2022081008)
                else if (refaddr=addr_reg_14i) and (not is_simm16_and_quadruple(offset)) then
                  internalerror(2022081009);
                s:=gas_regname(base)+','+tostr(offset);
              end;
            addr_reg:
              begin
                if assigned(symbol) or (index<>NR_NO) or (offset<>0) then
                  internalerror(2022081010)
                else if (base=NR_NO) then
                  internalerror(2022081011);
                s:=gas_regname(base);
              end;
          else
            internalerror(2022081012);
          end;
        end; { with ref do }
      getreferencestring:=s;
    end;


    function getopstr(asminfo: pasminfo; const o:toper;use_std_regname : boolean) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg:
          if use_std_regname then
            getopstr:=std_regname(o.reg)
          else
            getopstr:=gas_regname(o.reg);
        top_const:
          getopstr:=tostr(o.val);
        top_ref:
          getopstr:=getreferencestring(asminfo,o.ref^);
      else
        internalerror(2022111901);
      end;
    end;

    Procedure TLoongArch64InstrWriter.WriteInstruction(hp : tai);
    var op: TAsmOp;
        s: string;
	i : byte;
	use_std_regname_index : byte;
        sep: string[3];
    begin
      s:=#9+gas_op2str[taicpu(hp).opcode];
      if taicpu(hp).condition<>C_None then
        s:=s+cond2str[taicpu(hp).condition];

      curop:=taicpu(hp).opcode;
      if curop=A_MOVFCSR2GR then
        use_std_regname_index:=1
      else if curop=A_MOVGR2FCSR then
        use_std_regname_index:=0
      else
        use_std_regname_index:=255;

      if taicpu(hp).ops<>0 then
        begin
          sep:=#9;
          for i:=0 to taicpu(hp).ops-1 do
            begin
               s:=s+sep+getopstr(owner.asminfo,taicpu(hp).oper[i]^,use_std_regname_index=i);
               sep:=',';
            end;
        end;

      owner.writer.AsmWriteLn(s);
    end;


{****************************************************************************}
{                     GNU LoongArch Assembler writer                         }
{****************************************************************************}

    constructor TLoongArch64GNUAssembler.CreateWithWriter(info: pasminfo; wr: TExternalAssemblerOutputFile; freewriter, smart: boolean);
      begin
        inherited;
        InstrWriter := TLoongArch64InstrWriter.create(self);
      end;


  const
    as_loongarch64_gas_info : tasminfo =
       (
         id     : as_gas;
         idtxt  : 'AS';
         asmbin : 'as';
         asmcmd : '-o $OBJ $EXTRAOPT -mabi=lp64d $ASM';
         supported_targets : [system_loongarch64_linux];
         flags : [af_needar,af_smartlink_sections];
         labelprefix : '.L';
         labelmaxlen : -1;
         comment : '# ';
         dollarsign: '$';
       );

begin
  RegisterAssembler(as_loongarch64_gas_info,TLoongArch64GNUAssembler);
end.
