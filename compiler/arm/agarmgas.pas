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

    const
      gas_shiftmode2str : array[tshiftmode] of string[3] = (
        '','lsl','lsr','asr','ror','rrx');

  implementation

    uses
       cutils,globals,verbose,
       systems,
       assemble,
       aasmcpu,
       itcpugas,
       cgbase,cgutils;

    const
       as_arm_gas_info : tasminfo =
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

    function getreferencestring(var ref : treference) : string;
      var
        s : string;
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
        r : tsuperregister;
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
                if r in o.regset^ then
                  begin
                    if not(first) then
                      getopstr:=getopstr+',';
                    getopstr:=getopstr+gas_regname(newreg(R_INTREGISTER,r,R_SUBWHOLE));
                    first:=false;
                  end;
              getopstr:=getopstr+'}';
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

               { LDM and STM use references as first operand but they are written like a register }
               if (i=0) and (op in [A_LDM,A_STM]) then
                 begin
                   case taicpu(hp).oper[0]^.typ of
                     top_ref:
                       begin
                         s:=s+sep+gas_regname(taicpu(hp).oper[0]^.ref^.index);
                         if taicpu(hp).oper[0]^.ref^.addressmode=AM_PREINDEXED then
                           s:=s+'!';
                       end;
                     top_reg:
                       s:=s+sep+gas_regname(taicpu(hp).oper[0]^.reg);
                     else
                       internalerror(200311292);
                   end;
                 end
               { register count of SFM and LFM is written without # }
               else if (i=1) and (op in [A_SFM,A_LFM]) then
                 begin
                   case taicpu(hp).oper[1]^.typ of
                     top_const:
                       s:=s+sep+tostr(taicpu(hp).oper[1]^.val);
                     else
                       internalerror(200311292);
                   end;
                 end
               else
                 s:=s+sep+getopstr(taicpu(hp).oper[i]^);

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
  Revision 1.23  2004-11-06 17:44:47  florian
    + additional extdebug check for wrong add_reg_instructions added
    * too long manglednames are cut off at 200 chars using a crc

  Revision 1.22  2004/11/01 17:41:28  florian
    * fixed arm compilation with cgutils
    * ...

  Revision 1.21  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.20  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.19.2.1  2004/06/12 17:01:01  florian
    * fixed compilation of arm compiler

  Revision 1.19  2004/03/29 19:19:35  florian
    + arm floating point register saving implemented
    * hopefully stabs generation for MacOSX fixed
    + some defines for arm added

  Revision 1.18  2004/03/06 20:35:19  florian
    * fixed arm compilation
    * cleaned up code generation for exported linux procedures

}
