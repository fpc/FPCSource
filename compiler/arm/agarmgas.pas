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
       cpubase;

    type
      PARMGNUAssembler=^TARMGNUAssembler;
      TARMGNUAssembler=class(TGNUassembler)
        procedure WriteInstruction(hp : tai);override;
      end;

    var
      gas_reg2str : reg2strtable;

    function gas_regnum_search(const s:string):Tnewregister;
    function gas_regname(const r:Tnewregister):string;


  implementation

    uses
       cutils,globals,verbose,
       systems,
       assemble,
       aasmcpu;

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

          if not assigned(symbol) then
            s := '['
          else
            s:='['+symbol.name;

          if offset<0 then
           s:=s+tostr(offset)
          else
           if (offset>0) then
            begin
              if assigned(symbol) then
               s:=s+'+'+tostr(offset)
              else
               s:=s+tostr(offset);
            end;

           if (index.enum=R_NO) and (base.enum<>R_NO) then
             begin
                if offset=0 then
                  begin
                     if assigned(symbol) then
                       s:=s+'+0'
                     else
                       s:=s+'0';
                  end;
                if base.enum=R_INTREGISTER then
                  s:=s+'('+gas_regname(base.number)+')'
                else
                  s:=s+'('+gas_reg2str[base.enum]+')';
             end
           else if (index.enum<>R_NO) and (base.enum<>R_NO) and (offset=0) then
             s:=s+std_reg2str[base.enum]+','+std_reg2str[index.enum]
           else if ((index.enum<>R_NO) or (base.enum<>R_NO)) then
             internalerror(19992);
        end;
      getreferencestring:=s;
    end;


    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          begin
            if (o.reg.enum < R_R0) or (o.reg.enum > lastreg) then
              internalerror(200303121);
            getopstr_jmp:=std_reg2str[o.reg.enum];
          end;
        top_shifterop:
          begin
          end;
        { no top_ref jumping for powerpc }
        top_const :
          getopstr_jmp:=tostr(o.val);
        top_symbol :
          begin
            hs:=o.sym.name;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
            getopstr_jmp:=hs;
          end;
        top_none:
          getopstr_jmp:='';
        else
{$ifndef testing}
          internalerror(2002070603);
{$else testing}
          begin
            writeln('internalerror 10001');
            halt(1);
          end;
{$endif testing}
      end;
    end;

    const
      shifterop2str: array[tshiftertype] of string[3] = ('','asr','lsl','lsr','ror','rrx');

    function getopstr(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg:
          begin
            if o.reg.enum=R_INTREGISTER then
              getopstr:=gas_regname(o.reg.number)
            else
              getopstr:=gas_reg2str[o.reg.enum];
          end;
        top_shifterop:
          begin
            if (o.shifterop^.rs.enum<>R_NO) and (o.shifterop^.shiftimm=0) then
              begin
                if o.shifterop^.rs.enum=R_INTREGISTER then
                  getopstr:=shifterop2str[o.shifterop^.shiftertype]+' '+gas_regname(o.shifterop^.rs.number)
                else
                  getopstr:=shifterop2str[o.shifterop^.shiftertype]+' '+gas_reg2str[o.shifterop^.rs.enum];
              end
            else if (o.shifterop^.rs.enum=R_NO) then
              getopstr:=shifterop2str[o.shifterop^.shiftertype]+' #'+tostr(o.shifterop^.shiftimm)
            else internalerror(200308282);
          end;
        top_const:
          getopstr:=tostr(longint(o.val));
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
{$ifndef testing}
          internalerror(2002070604);
{$else testing}
          begin
            writeln('internalerror 10001');
            halt(1);
          end;
{$endif testing}
      end;
    end;

    Procedure TARMGNUAssembler.WriteInstruction(hp : tai);
    var op: TAsmOp;
        s: string;
        i: byte;
        sep: string[3];
    begin
      op:=taicpu(hp).opcode;
      if is_calljmp(op) then
        begin
{
          { direct BO/BI in op[0] and op[1] not supported, put them in condition! }
          case op of
             A_B,A_BA,A_BL,A_BLA:
               s:=#9+op2str[op]+#9;
             A_BCTR,A_BCTRL,A_BLR,A_BLRL:
               s:=#9+op2str[op]
             else
               s:=cond2str(op,taicpu(hp).condition)+',';
          end;

          if (taicpu(hp).oper[0].typ <> top_none) then
            s:=s+getopstr_jmp(taicpu(hp).oper[0]);
}
        end
      else
        { process operands }
        begin
          s:=#9+std_op2str[op];
          if taicpu(hp).ops<>0 then
            begin
            {
              if not is_calljmp(op) then
                sep:=','
              else
            }
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
        end;
      AsmWriteLn(s);
    end;

  function gas_regnum_search(const s:string):Tnewregister;
    begin
    end;


  function gas_regname(const r:Tnewregister):string;
    var s:Tsuperregister;
    begin
      s:=r shr 8;
      if s in [RS_R0..RS_R15] then
        gas_regname:='r'+tostr(s-RS_R0)
      else
        begin
          {Generate a systematic name.}
          gas_regname:='reg'+tostr(s)+'d';
        end;
    end;


begin
  RegisterAssembler(as_arm_gas_info,TARMGNUAssembler);
  gas_reg2str:=std_reg2str;
end.
{
  $Log$
  Revision 1.4  2003-08-28 00:05:29  florian
    * today's arm patches

  Revision 1.3  2003/08/24 12:27:26  florian
    * continued to work on the arm port

  Revision 1.2  2003/08/20 15:50:12  florian
    * more arm stuff

  Revision 1.1  2003/08/16 13:23:01  florian
    * several arm related stuff fixed
}
