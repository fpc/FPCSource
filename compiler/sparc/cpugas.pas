{
    $Id$
    Copyright (c) 1999-2003 by Florian Klaempfl

    This unit implements an asmoutput class for SPARC AT&T syntax

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
      aasmtai,aasmcpu,assemble,aggas;

    type
      TGasSPARC=class(TGnuAssembler)
        procedure WriteInstruction(hp:Tai);override;
      end;

implementation

    uses
      cutils,systems,
      verbose,itcpugas,cgbase;


    function GetReferenceString(var ref:TReference):string;
      begin
        GetReferenceString:='';
        with ref do
          begin
            if (base=NR_NO) and (index=NR_NO) then
              begin
                 if assigned(symbol) then
                   GetReferenceString:=symbol.name;
                 if offset>0 then
                   GetReferenceString:=GetReferenceString+'+'+ToStr(offset)
                 else if offset<0 then
                   GetReferenceString:=GetReferenceString+ToStr(offset);
                 case refaddr of
                   addr_hi :
                     GetReferenceString:='%hi('+GetReferenceString+')';
                   addr_lo :
                     GetReferenceString:='%lo('+GetReferenceString+')';
                 end;
              end
            else
              begin
                if assigned(symbol) then
                  internalerror(2003052601);
                if base<>NR_NO then
                  GetReferenceString:=GetReferenceString+gas_regname(base);
                if index=NR_NO then
                  begin
                    { if (Offset<simm13lo) or (Offset>simm13hi) then
                      internalerror(2003053008); }
                    if offset>0 then
                      GetReferenceString:=GetReferenceString+'+'+ToStr(offset)
                    else if offset<0 then
                      GetReferenceString:=GetReferenceString+ToStr(offset);
                  end
                else
                  begin
                    if Offset<>0 then
                      internalerror(2003052603);
                    GetReferenceString:=GetReferenceString+'+'+gas_regname(index);
                  end;
              end;
          end;
      end;


    function getopstr(const Oper:TOper):string;
      begin
        with Oper do
          case typ of
            top_reg:
              getopstr:=gas_regname(reg);
            top_const:
              getopstr:=tostr(longint(val));
            top_ref:
              if Oper.ref^.refaddr=addr_no then
                getopstr:='['+getreferencestring(ref^)+']'
              else
                getopstr:=getreferencestring(ref^);
            else
              internalerror(10001);
          end;
        end;


    procedure TGasSPARC.WriteInstruction(hp:Tai);
      var
        Op:TAsmOp;
        s:String;
        i:Integer;
      begin
        if hp.typ<>ait_instruction then
          exit;
        op:=taicpu(hp).opcode;
        { FMOVd does not exist, rewrite it using 2 FMOVs }
        if op=A_FMOVD then
          begin
            if (taicpu(hp).ops<>2) or
               (taicpu(hp).oper[0]^.typ<>top_reg) or
               (taicpu(hp).oper[1]^.typ<>top_reg) then
              internalerror(200401045);
            { FMOVs %f<even>,%f<even> }
            s:=#9+std_op2str[A_FMOVs]+#9+getopstr(taicpu(hp).oper[0]^)+','+getopstr(taicpu(hp).oper[1]^);
            AsmWriteLn(s);
            { FMOVs %f<odd>,%f<odd> }
            inc(taicpu(hp).oper[0]^.reg);
            inc(taicpu(hp).oper[1]^.reg);
            s:=#9+std_op2str[A_FMOVs]+#9+getopstr(taicpu(hp).oper[0]^)+','+getopstr(taicpu(hp).oper[1]^);
            dec(taicpu(hp).oper[0]^.reg);
            dec(taicpu(hp).oper[1]^.reg);
            AsmWriteLn(s);
          end
        else
          begin
            { call maybe not translated to call }
            s:=#9+std_op2str[op]+cond2str[taicpu(hp).condition];
            if taicpu(hp).delayslot_annulled then
              s:=s+',a';
            if taicpu(hp).ops>0 then
              begin
                s:=s+#9+getopstr(taicpu(hp).oper[0]^);
                for i:=1 to taicpu(hp).ops-1 do
                  s:=s+','+getopstr(taicpu(hp).oper[i]^);
              end;
            AsmWriteLn(s);
          end;
      end;


    const
      as_sparc_as_info : tasminfo =
         (
           id     : as_gas;
           idtxt  : 'AS';
           asmbin : 'as';
           asmcmd : '-o $OBJ $ASM';
           supported_target : system_any;
           flags : [af_allowdirect,af_needar,af_smartlink_sections];
           labelprefix : '.L';
           comment : '# ';
         );

begin
  RegisterAssembler(as_SPARC_as_info,TGasSPARC);
end.
{
    $Log$
    Revision 1.28  2004-06-20 08:55:32  florian
      * logs truncated

    Revision 1.27  2004/06/20 07:11:32  florian
      - offsetfixup removed

    Revision 1.26  2004/06/16 20:07:10  florian
      * dwarf branch merged

    Revision 1.25.2.3  2004/05/30 17:54:14  florian
      + implemented cmp64bit
      * started to fix spilling
      * fixed int64 sub partially

    Revision 1.25.2.2  2004/05/25 21:38:53  peter
      * assembler reader/writer updates

    Revision 1.25.2.1  2004/05/11 21:06:51  peter
      * sparc compiler fixed

}
