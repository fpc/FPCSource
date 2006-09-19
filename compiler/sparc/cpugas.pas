{
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
      aasmtai,aasmdata,aasmcpu,assemble,aggas;

    type
      TGasSPARC=class(TGnuAssembler)
        constructor create(smart: boolean); override;
      end;

     TSPARCInstrWriter=class(TCPUInstrWriter)
       procedure WriteInstruction(hp:Tai);override;
     end;

implementation

    uses
      cutils,systems,
      verbose,itcpugas,cgbase,cgutils;


{****************************************************************************}
{                         GNU PPC Assembler writer                           }
{****************************************************************************}

    constructor TGasSPARC.create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter := TSPARCInstrWriter.create(self);
      end;


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
                   addr_hi:
                     GetReferenceString:='%hi('+GetReferenceString+')';
                   addr_lo:
                     GetReferenceString:='%lo('+GetReferenceString+')';
                 end;
              end
            else
              begin
{$ifdef extdebug}
                if assigned(symbol) and
                  not(refaddr in [addr_pic,addr_lo]) then
                  internalerror(2003052601);
{$endif extdebug}
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
                    {
                    else if (offset=0) and not(assigned(symbol)) then
                      GetReferenceString:=GetReferenceString+ToStr(offset);
                    }
                    if assigned(symbol) then
                      begin
                        if refaddr=addr_lo then
                          GetReferenceString:='%lo('+symbol.name+')+'+GetReferenceString
                        else
                          GetReferenceString:=symbol.name+'+'+GetReferenceString;
                      end;
                  end
                else
                  begin
{$ifdef extdebug}
                    if (Offset<>0) or assigned(symbol) then
                      internalerror(2003052603);
{$endif extdebug}
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
              if (oper.ref^.refaddr in [addr_no,addr_pic]) or ((oper.ref^.refaddr=addr_lo) and ((oper.ref^.base<>NR_NO) or
                (oper.ref^.index<>NR_NO))) then
                getopstr:='['+getreferencestring(ref^)+']'
              else
                getopstr:=getreferencestring(ref^);
            else
              internalerror(10001);
          end;
        end;


    procedure TSPARCInstrWriter.WriteInstruction(hp:Tai);
      var
        Op:TAsmOp;
        s:String;
        i:Integer;
      begin
        if hp.typ<>ait_instruction then
          exit;
        op:=taicpu(hp).opcode;
        { translate pseudoops, this should be move to a separate pass later, so it's done before
          peephole optimization }
        case op of
          A_FABSd:
            begin
              if (taicpu(hp).ops<>2) or
                 (taicpu(hp).oper[0]^.typ<>top_reg) or
                 (taicpu(hp).oper[1]^.typ<>top_reg) then
                internalerror(200401045);
              { FABSs %f<even>,%f<even> }
              s:=#9+std_op2str[A_FABSs]+#9+getopstr(taicpu(hp).oper[0]^)+','+getopstr(taicpu(hp).oper[1]^);
              owner.AsmWriteLn(s);
              { FMOVs %f<odd>,%f<odd> }
              inc(taicpu(hp).oper[0]^.reg);
              inc(taicpu(hp).oper[1]^.reg);
              s:=#9+std_op2str[A_FMOVs]+#9+getopstr(taicpu(hp).oper[0]^)+','+getopstr(taicpu(hp).oper[1]^);
              dec(taicpu(hp).oper[0]^.reg);
              dec(taicpu(hp).oper[1]^.reg);
              owner.AsmWriteLn(s);
            end;
          A_FMOVd:
            begin
              if (taicpu(hp).ops<>2) or
                 (taicpu(hp).oper[0]^.typ<>top_reg) or
                 (taicpu(hp).oper[1]^.typ<>top_reg) then
                internalerror(200401045);
              { FMOVs %f<even>,%f<even> }
              s:=#9+std_op2str[A_FMOVs]+#9+getopstr(taicpu(hp).oper[0]^)+','+getopstr(taicpu(hp).oper[1]^);
              owner.AsmWriteLn(s);
              { FMOVs %f<odd>,%f<odd> }
              inc(taicpu(hp).oper[0]^.reg);
              inc(taicpu(hp).oper[1]^.reg);
              s:=#9+std_op2str[A_FMOVs]+#9+getopstr(taicpu(hp).oper[0]^)+','+getopstr(taicpu(hp).oper[1]^);
              dec(taicpu(hp).oper[0]^.reg);
              dec(taicpu(hp).oper[1]^.reg);
              owner.AsmWriteLn(s);
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
              owner.AsmWriteLn(s);
            end;
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

      as_sparc_gas_info : tasminfo =
         (
           id     : as_ggas;
           idtxt  : 'GAS';
           asmbin : 'gas';
           asmcmd : '-o $OBJ $ASM';
           supported_target : system_any;
           flags : [af_allowdirect,af_needar,af_smartlink_sections];
           labelprefix : '.L';
           comment : '# ';
         );

begin
  RegisterAssembler(as_SPARC_as_info,TGasSPARC);
  RegisterAssembler(as_SPARC_gas_info,TGasSPARC);
end.
