{******************************************************************************
   $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

 ****************************************************************************}
unit CpuGas;
{This unit implements an asmoutput class for SPARC AT&T syntax}
{$MACRO ON}{$INCLUDE fpcdefs.inc}
interface
uses
  cclasses,cpubase,
  globals,
  aasmbase,aasmtai,aasmcpu,assemble,aggas;
type
  TGasSPARC=class(TGnuAssembler)
    procedure WriteInstruction(hp:Tai);override;
  end;
implementation
uses
  finput,
  cutils,systems,
  verbose;
{$DEFINE gas_reg2str:=std_reg2str}
const
  line_length = 70;
var
{$ifdef GDB}
      n_line:byte;     { different types of source lines }
      linecount,
      includecount:longint;
      funcname:pchar;
      stabslastfileinfo:tfileposinfo;
{$endif}
      lastsec:tsection; { last section type written }
      lastfileinfo:tfileposinfo;
      infile,
      lastinfile:tinputfile;
      symendcount:longint;
function GetReferenceString(var ref:TReference):string;
  begin
    GetReferenceString:='+';
    with ref do
      if assigned(symbol)
      then
        GetReferenceString:=symbol.name
      else
        begin
          inc(offset,offsetfixup);
          if base<>R_NONE
          then
            GetReferenceString:=gas_reg2str[base]+'+';
          if index<>R_NONE
          then
            begin
              if ScaleFactor<>0
              then
                GetReferenceString:=GetReferenceString+ToStr(ScaleFactor)+'*';
              GetReferenceString:=GetReferenceString+gas_reg2str[index]+'+';
            end;
          if Offset=0
          then
            SetLength(GetReferenceString,Length(GetReferenceString)-1)
          else if offset<0
          then
            begin
              SetLength(GetReferenceString,Length(GetReferenceString)-1);
              GetReferenceString:=GetReferenceString+tostr(offset);
            end
          else if offset>0
          then
            GetReferenceString:=GetReferenceString+tostr(offset);
        end;
  end;
function getopstr(const Oper:TOper):string;
  var
    hs:string;
  begin
    with Oper do
      case typ of
        top_reg:
          getopstr:=gas_reg2str[reg];
        top_ref:
          getopstr:='['+getreferencestring(ref^)+']';
        top_const:
          getopstr:={'$'+}tostr(longint(val));
        top_symbol:
          begin
            if assigned(sym) then
              hs:={'$'+}sym.name
            else
              hs:='$';
            if symofs>0 then
             hs:=hs+'+'+tostr(symofs)
            else
             if symofs<0 then
              hs:=hs+tostr(symofs)
            else
             if not(assigned(sym)) then
               hs:=hs+'0';
            getopstr:=hs;
          end;
        top_raddr:
          getopstr:=std_reg2str[reg1]+'+'+std_reg2str[reg2];
        top_caddr:
          getopstr:=std_reg2str[regb]+'+'+ToStr(const13);
        else
          internalerror(10001);
      end;
    end;
(*
function getopstr_jmp(const Oper:TOper):string;
  var
    hs:string;
  begin
    with Oper do
      case typ of
        top_reg:
          getopstr_jmp:=gas_reg2str[reg]+'+';
        top_ref:
          getopstr_jmp:=GetReferenceString(ref^);
        top_const:
          getopstr_jmp:=tostr(longint(val));
        top_symbol:
          begin
            hs:=sym.name;
            if symofs>0 then
             hs:=hs+'+'+tostr(symofs)
            else
             if symofs<0 then
              hs:=hs+tostr(symofs);
            getopstr_jmp:=hs;
          end;
        else
          internalerror(10001);
      end;
    end;*)
{****************************************************************************
                            TSPARCATTASMOUTPUT
 ****************************************************************************}
procedure TGasSPARC.WriteInstruction(hp:Tai);
	var
		Op:TAsmOp;
		s:String;
		i:Integer;
	begin
		if hp.typ<>ait_instruction
		then
			Exit;
		op:=taicpu(hp).opcode;
	 {call maybe not translated to call}
		s:=#9+std_op2str[op]+cond2str[taicpu(hp).condition];
   {process operands}
    s:=#9+std_op2str[op];
    if taicpu(hp).ops>0
    then
      begin
        s+=#9+getopstr(taicpu(hp).oper[0]);
        for i:=1 to taicpu(hp).ops-1 do
          s+=','+getopstr(taicpu(hp).oper[i]);
      end;
    AsmWriteLn(s);
  end;
{*****************************************************************************
                                  Initialize
*****************************************************************************}
const
  as_SPARC_as_info:TAsmInfo=(
    id:as_gas;
    idtxt:'AS';
    asmbin:'as';
    asmcmd:'-o $OBJ $ASM';
    supported_target:system_any;
    outputbinary:false;
    allowdirect:true;
    needar:true;
    labelprefix_only_inside_procedure:false;
    labelprefix:'.L';
    comment:'; ';
    secnames:({sec_none}'',           {no section}
              {sec_code}'.text',      {executable code}
              {sec_data}'.data',      {initialized R/W data}
              {sec_bss}'.bss',        {uninitialized R/W data}
              {sec_idata2}'.comment', {comments}
              {sec_idata4}'.debug',   {debugging information}
              {sec_idata5}'.rodata',  {RO data}
              {sec_idata6}'.line',    {line numbers info for symbolic debug}
              {sec_idata7}'.init',    {runtime intialization code}
              {sec_edata}'.fini',     {runtime finalization code}
              {sec_stab}'.stab',
              {sec_stabstr} '.stabstr',
              {sec_common}'.note')    {note info}
  );
initialization
  RegisterAssembler(as_SPARC_as_info,TGasSPARC);
end.
{
    $Log$
    Revision 1.9  2002-11-10 19:07:46  mazen
    * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

    Revision 1.8  2002/10/25 19:37:53  mazen
    * bug of references name missing last character fixed

    Revision 1.7  2002/10/20 19:01:38  mazen
    + op_raddr_reg and op_caddr_reg added to fix functions prologue

    Revision 1.6  2002/10/15 09:00:28  mazen
    * sone coding style modified

}
