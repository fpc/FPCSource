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
{
  This unit implements an asmoutput class for SPARC AT&T syntax
}
unit cpugas;

{$i fpcdefs.inc}

  interface

    uses
      cpubase,
      aasmtai,aasmcpu,assemble,aggas;

    type
      TGasSPARC=class(TGnuAssembler)
        procedure WriteInstruction(hp:Tai);override;
      end;

  implementation

    uses
      cutils,systems,
      verbose;

    function GetReferenceString(var ref:TReference):string;
      begin
        GetReferenceString:='';
        with ref do
          begin
            inc(offset,offsetfixup);
            offsetfixup:=0;
            if (base.enum<>R_INTREGISTER) or (index.enum<>R_INTREGISTER) then
              internalerror(200301081);
            if assigned(symbol) then
              begin
                 if (base.number<>NR_NO) or (index.number<>NR_NO) then
                   internalerror(2003052601);
                 GetReferenceString:=symbol.name;
                 if offset>0 then
                   GetReferenceString:=GetReferenceString+'+'+ToStr(offset)
                 else if offset<0 then
                   GetReferenceString:=GetReferenceString+ToStr(offset);
                 if symaddr=refs_hi then
                   GetReferenceString:='%hi('+GetReferenceString+')'
                 else if symaddr=refs_lo then
                   GetReferenceString:='%lo('+GetReferenceString+')'
                 else
                   internalerror(2003052602);
              end
            else
              begin
                if base.number<>NR_NO then
                  GetReferenceString:=std_reg2str[base.enum]+'+';
                if index.number<>NR_NO then
                  GetReferenceString:=GetReferenceString+std_reg2str[index.enum]+'+';
                if Offset<>0 then
                   internalerror(2003052603);
              end;
          end;
      end;


    function getopstr(const Oper:TOper):string;
      var
        hs:string;
      begin
        with Oper do
          case typ of
            top_reg:
              getopstr:=std_reg2str[reg.enum];
            top_ref:
              getopstr:=getreferencestring(ref^);
            top_const:
              getopstr:=tostr(longint(val));
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

    procedure TGasSPARC.WriteInstruction(hp:Tai);
    	var
    		Op:TAsmOp;
    		s:String;
    		i:Integer;
      begin
        if hp.typ<>ait_instruction then
          exit;
        op:=taicpu(hp).opcode;
        { call maybe not translated to call }
        s:=#9+std_op2str[op]+cond2str[taicpu(hp).condition];
        { process operands }
        s:=#9+std_op2str[op];
        if taicpu(hp).ops>0
        then
          begin
            s:=s+#9+getopstr(taicpu(hp).oper[0]);
            for i:=1 to taicpu(hp).ops-1 do
              s:=s+','+getopstr(taicpu(hp).oper[i]);
          end;
        AsmWriteLn(s);
      end;


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
        comment:';#';
        secnames:({sec_none}'',           {no section}
                  {sec_code}'.text',      {executable code}
                  {sec_data}'.data',      {initialized R/W data}
                  {sec_bss}'.section ".bss"',        {uninitialized R/W data}
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

begin
  RegisterAssembler(as_SPARC_as_info,TGasSPARC);
end.
{
    $Log$
    Revision 1.16  2003-05-30 23:57:08  peter
      * more sparc cleanup
      * accumulator removed, splitted in function_return_reg (called) and
        function_result_reg (caller)

    Revision 1.15  2003/05/28 23:18:31  florian
      * started to fix and clean up the sparc port

    Revision 1.14  2003/05/07 11:55:34  mazen
    - unused units removed from uses clause
    - unused variables removed from implemntation declarations

    Revision 1.13  2003/05/06 14:55:27  mazen
    * comment changed to ;# instead of ##
    * .bss section changed to .section ".bss"

    Revision 1.12  2003/03/15 22:51:58  mazen
    * remaking sparc rtl compile

    Revision 1.11  2003/01/08 18:43:58  daniel
     * Tregister changed into a record

    Revision 1.10  2002/11/16 15:29:16  florian
      * fixed Cish syntax

    Revision 1.9  2002/11/10 19:07:46  mazen
    * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

    Revision 1.8  2002/10/25 19:37:53  mazen
    * bug of references name missing last character fixed

    Revision 1.7  2002/10/20 19:01:38  mazen
    + op_raddr_reg and op_caddr_reg added to fix functions prologue

    Revision 1.6  2002/10/15 09:00:28  mazen
    * sone coding style modified

}
