{
    Copyright (c) 2003 by Florian Klaempfl

    This unit implements an asm for the avr32

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
{ This unit implements the GNU Assembler writer for the avr32
}

unit agavr32gas;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmtai,aasmdata,
       aggas,
       cpubase;

    type
      Tavr32GNUAssembler=class(TGNUassembler)
        constructor create(smart: boolean); override;
        function MakeCmdLine: TCmdStr; override;
      end;

      Tavr32InstrWriter=class(TCPUInstrWriter)
        procedure WriteInstruction(hp : tai);override;
      end;


    const
      gas_shiftmode2str : array[tshiftmode] of string[3] = (
        '','<<','>>');

  implementation

    uses
       cutils,globals,verbose,
       systems,
       assemble,
       cpuinfo,aasmcpu,
       itcpugas,
       cgbase,cgutils;

{****************************************************************************}
{                         GNU avr32 Assembler writer                           }
{****************************************************************************}

    const
      selector2str: array[tregisterselector] of string[2] = ('',':b',':l',':u',':t');

    constructor Tavr32GNUAssembler.create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter := Tavr32InstrWriter.create(self);
      end;


    function Tavr32GNUAssembler.MakeCmdLine: TCmdStr;
      begin
        result:=inherited MakeCmdLine;
      end;


{****************************************************************************}
{                  Helper routines for Instruction Writer                    }
{****************************************************************************}

    function getreferencestring(var ref : treference) : string;
      var
        s : string;
        function GetShift(a : aint) : aint;
          begin
            case a of
              1: result:=0;
              2: result:=1;
              4: result:=2;
              8: result:=3;
            else
              internalerror(2011012102);
            end;
          end;

      begin
         with ref do
          begin
{$ifdef extdebug}
            // if base=NR_NO then
            //   internalerror(200308292);

            // if ((index<>NR_NO) or (shiftmode<>SM_None)) and ((offset<>0) or (symbol<>nil)) then
            //   internalerror(200308293);
{$endif extdebug}

            if assigned(symbol) then
              begin
                if (base<>NR_NO) and not(is_pc(base)) then
                  internalerror(200309011);
                s:='pc['+symbol.name;
                s:=symbol.name;
                if offset<0 then
                  s:=s+tostr(offset)
                else if offset>0 then
                  s:=s+'+'+tostr(offset);
                //s:=s+']';
              end
            else
              begin
                s:=gas_regname(base);
                if addressmode=AM_POSTINDEXED then
                  s:=s+'++'
                else if addressmode=AM_PREINDEXED then
                  s:='--'+s;

                if index<>NR_NO then
                  begin
                    if scalefactor > 1 then
                      s:=s+'['+gas_regname(index)+selector2str[indexselector]+'<<'+tostr(getshift(scalefactor))+']'
                    else
                      s:=s+'['+gas_regname(index)+selector2str[indexselector]+']';
                  end
                else if offset<>0 then
                  s:=s+'['+tostr(offset)+']';
              end;

          end;
        getreferencestring:=s;
      end;


    const
      shiftmode2str: array[tshiftmode] of string[3] = ('','<<','>>');

    function getopstr(const o:toper) : string;
      var
        hs : string;
        first : boolean;
        r : tsuperregister;
      begin
        case o.typ of
          top_reg:
            getopstr:=gas_regname(o.reg);
          top_shifterop:
            getopstr:=shiftmode2str[o.shifterop^.shiftmode]+tostr(o.shifterop^.shiftimm);
          top_const:
            getopstr:=tostr(longint(o.val));
          top_regset:
            begin
              getopstr:='';
              first:=true;
              for r:=RS_R0 to RS_R15 do
                if r in o.regset^ then
                  begin
                    if not(first) then
                      getopstr:=getopstr+',';
                    getopstr:=getopstr+gas_regname(newreg(o.regtyp,r,o.subreg));
                    first:=false;
                  end;
            end;
          top_ref:
            if o.ref^.refaddr=addr_full then
              begin
                hs:=o.ref^.symbol.name;
                if o.ref^.offset>0 then
                 hs:=hs+'+'+tostr(o.ref^.offset)
                else
                 if o.ref^.offset<0 then
                  hs:=hs+tostr(o.ref^.offset);
                getopstr:=hs;
              end
            else
              getopstr:=getreferencestring(o.ref^);
          top_coh:
            getopstr:='COH';
          top_selector:
            getopstr:=gas_regname(o.reg)+selector2str[o.selector];
          else
            internalerror(2002070604);
        end;
      end;


    Procedure Tavr32InstrWriter.WriteInstruction(hp : tai);
    var op: TAsmOp;
        postfix,s: string;
        i: byte;
        sep: string[3];
    begin
      op:=taicpu(hp).opcode;
      s:=#9+gas_op2str[op]+cond2str[taicpu(hp).condition];
      if taicpu(hp).oppostfix<>PF_None then
        s:=s+'.'+oppostfix2str[taicpu(hp).oppostfix];

      if taicpu(hp).ops<>0 then
        begin
          sep:=#9;
          for i:=0 to taicpu(hp).ops-1 do
            begin
              if taicpu(hp).oper[i]^.typ = top_shifterop then
                s:=s+getopstr(taicpu(hp).oper[i]^)
              else
                s:=s+sep+getopstr(taicpu(hp).oper[i]^);

              sep:=',';
            end;
        end;
      owner.AsmWriteLn(s);
    end;


    const
       as_avr32_gas_info : tasminfo =
          (
            id     : as_gas;

            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_targets : [system_avr32_embedded];
            flags : [af_allowdirect,af_needar,af_smartlink_sections];
            labelprefix : '.L';
            comment : '# ';
          );


begin
  RegisterAssembler(as_avr32_gas_info,Tavr32GNUAssembler);
end.
