{
    $Id$
    Copyright (c) 2003 by Florian Klaempfl

    This unit implements an asm for the ARM

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
{ This unit implements the GNU Assembler writer for the ARM
}

unit agarmgas;

{$i fpcdefs.inc}

  interface

    uses
       aasmtai,
       aggas,
       cpubase,
       cginfo;

    type
      PARMGNUAssembler=^TARMGNUAssembler;
      TARMGNUAssembler=class(TGNUassembler)
        procedure WriteInstruction(hp : tai);override;
      end;

    const
      gas_shiftmode2str : array[tshiftmode] of string[3] = (
        '','lsl','lsr','asr','ror','rrx');

  implementation

    uses
       cutils,globals,verbose,
       systems,
       assemble,
       aasmcpu,
       itarmgas;

    const
       as_arm_gas_info : tasminfo =
          (
            id     : as_gas;

            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_any;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.text',
              '','','','','','',
              '.stab','.stabstr','COMMON')
          );

    function getreferencestring(var ref : treference) : string;
      var
        s : string;
      begin
         with ref do
          begin
            inc(offset,offsetfixup);

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
                s:=symbol.name;
                if offset<0 then
                  s:=s+tostr(offset)
                else if offset>0 then
                  s:=s+'+'+tostr(offset);
              end
            else
              begin
                s:='['+gas_regname(base);
                if addressmode=AM_POSTINDEXED then
                  s:=s+']';
                if index<>NR_NO then
                  begin
                     if signindex<0 then
                       s:=s+', -'
                     else
                       s:=s+', ';

                     s:=s+gas_regname(index);

                     if shiftmode<>SM_None then
                       s:=s+' ,'+gas_shiftmode2str[shiftmode]+' #'+tostr(shiftimm);
                  end
                else if offset<>0 then
                  s:=s+', #'+tostr(offset);

                case addressmode of
                  AM_OFFSET:
                    s:=s+']';
                  AM_PREINDEXED:
                    s:=s+']!';
                end;
              end;

          end;
        getreferencestring:=s;
      end;


    const
      shiftmode2str: array[tshiftmode] of string[3] = ('','lsl','lsr','asr','ror','rrx');

    function getopstr(const o:toper) : string;
      var
        hs : string;
        first : boolean;
        r : tregister;
      begin
        case o.typ of
          top_reg:
            getopstr:=gas_regname(o.reg);
          top_shifterop:
            begin
              if (o.shifterop^.rs<>NR_NO) and (o.shifterop^.shiftimm=0) then
                getopstr:=shiftmode2str[o.shifterop^.shiftmode]+' '+gas_regname(o.shifterop^.rs)
              else if (o.shifterop^.rs=NR_NO) then
                getopstr:=shiftmode2str[o.shifterop^.shiftmode]+' #'+tostr(o.shifterop^.shiftimm)
              else internalerror(200308282);
            end;
          top_const:
            getopstr:='#'+tostr(longint(o.val));
          top_regset:
            begin
              getopstr:='{';
              first:=true;
              for r:=RS_R0 to RS_R15 do
                if r in o.regset then
                  begin
                    if not(first) then
                      getopstr:=getopstr+',';
                    getopstr:=getopstr+gas_regname(newreg(R_INTREGISTER,r,R_SUBWHOLE));
                    first:=false;
                  end;
              getopstr:=getopstr+'}';
            end;
          top_ref:
            getopstr:=getreferencestring(o.ref^);
          top_symbol:
            begin
              hs:=o.sym.name;
              if o.symofs>0 then
               hs:=hs+'+'+tostr(o.symofs)
              else
               if o.symofs<0 then
                hs:=hs+tostr(o.symofs);
              getopstr:=hs;
            end;
          else
            internalerror(2002070604);
        end;
      end;

    Procedure TARMGNUAssembler.WriteInstruction(hp : tai);
    var op: TAsmOp;
        s: string;
        i: byte;
        sep: string[3];
    begin
      op:=taicpu(hp).opcode;
      s:=#9+gas_op2str[op]+cond2str[taicpu(hp).condition]+oppostfix2str[taicpu(hp).oppostfix];
      if taicpu(hp).ops<>0 then
        begin
          sep:=#9;
          for i:=0 to taicpu(hp).ops-1 do
            begin
               // debug code
               // writeln(s);
               // writeln(taicpu(hp).fileinfo.line);
               s:=s+sep+getopstr(taicpu(hp).oper[i]);
               sep:=',';
            end;
        end;
      AsmWriteLn(s);
    end;


begin
  RegisterAssembler(as_arm_gas_info,TARMGNUAssembler);
end.
{
  $Log$
  Revision 1.10  2003-09-05 23:57:01  florian
    * arm is working again as before the new register naming scheme was implemented

  Revision 1.9  2003/09/04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.8  2003/09/03 19:10:30  florian
    * initial revision of new register naming

  Revision 1.7  2003/09/01 15:11:16  florian
    * fixed reference handling
    * fixed operand postfix for floating point instructions
    * fixed wrong shifter constant handling

  Revision 1.6  2003/08/29 21:36:28  florian
    * fixed procedure entry/exit code
    * started to fix reference handling

  Revision 1.5  2003/08/28 13:26:10  florian
    * another couple of arm fixes

  Revision 1.4  2003/08/28 00:05:29  florian
    * today's arm patches

  Revision 1.3  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.2  2003/08/20 15:50:12  florian
    * more arm stuff

  Revision 1.1  2003/08/16 13:23:01  florian
    * several arm related stuff fixed
}
