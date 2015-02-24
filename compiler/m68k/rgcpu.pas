{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the register allocator for m68k

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cgbase,cgutils,cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         function do_spill_replace(list:TAsmList;instr:taicpu;orgreg:tsuperregister;const spilltemp:treference):boolean;override;
       end;

  implementation

    { returns True if source operand of MOVE can be replaced with spilltemp when its destination is ref^. }
    function isvalidmovedest(ref: preference): boolean; inline;
      begin
        { The following is for Coldfire, for other CPUs it maybe can be relaxed. }
        result:=(ref^.symbol=nil) and (ref^.scalefactor<=1) and
          (ref^.index=NR_NO) and (ref^.base<>NR_NO) and (ref^.offset>=low(smallint)) and
          (ref^.offset<=high(smallint));
      end;


    procedure trgcpu.do_spill_read(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : tasmlist;
        hreg     : tregister;
      begin
        if (abs(spilltemp.offset)>32767) and (current_settings.cputype in (cpu_coldfire + [cpu_mc68000])) then
          begin
            helplist:=tasmlist.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=tempreg
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);
{$ifdef DEBUG_SPILLING}
            helplist.concat(tai_comment.Create(strpnew('Spilling: Read, large offset')));
{$endif}

            helplist.concat(taicpu.op_const_reg(A_MOVE,S_L,spilltemp.offset,hreg));
            reference_reset_base(tmpref,spilltemp.base,0,sizeof(aint));
            tmpref.index:=hreg;

            helpins:=spilling_create_load(tmpref,tempreg);
            helplist.concat(helpins);
            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_read(list,pos,spilltemp,tempreg);
      end;


    procedure trgcpu.do_spill_written(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        tmpref   : treference;
        helplist : tasmlist;
        hreg     : tregister;
      begin
        if (abs(spilltemp.offset)>32767) and (current_settings.cputype in (cpu_coldfire + [cpu_mc68000])) then
          begin
            helplist:=tasmlist.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,[R_SUBWHOLE])
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);
{$ifdef DEBUG_SPILLING}
            helplist.concat(tai_comment.Create(strpnew('Spilling: Write, large offset')));
{$endif}

            helplist.concat(taicpu.op_const_reg(A_MOVE,S_L,spilltemp.offset,hreg));
            reference_reset_base(tmpref,spilltemp.base,0,sizeof(aint));
            tmpref.index:=hreg;

            helplist.concat(spilling_create_store(tempreg,tmpref));
            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
    end;


    function trgcpu.do_spill_replace(list:TAsmList;instr:taicpu;orgreg:tsuperregister;const spilltemp:treference):boolean;
      var
        opidx: longint;
      begin
        result:=false;
        opidx:=-1;
        { TODO: support more instructions (on m68k almost all are eligible) }
        if (not (regtype in [R_INTREGISTER,R_ADDRESSREGISTER])) or (instr.opcode<>A_MOVE) then
          exit;
        if (instr.ops<>2) then
          exit;
        if (instr.oper[0]^.typ=top_reg) and (get_alias(getsupreg(instr.oper[0]^.reg))=orgreg) then
          begin
            { source can be replaced if dest is register or a 'simple' reference }
            if (instr.oper[1]^.typ=top_reg) or
              ((instr.oper[1]^.typ=top_ref) and isvalidmovedest(instr.oper[1]^.ref)) then
              opidx:=0;
          end
        else if (instr.oper[1]^.typ=top_reg) and (get_alias(getsupreg(instr.oper[1]^.reg))=orgreg) and
          (instr.oper[0]^.typ=top_reg) then
          opidx:=1;

        if (opidx<0) then
          exit;
        instr.oper[opidx]^.typ:=top_ref;
        new(instr.oper[opidx]^.ref);
        instr.oper[opidx]^.ref^:=spilltemp;
        case instr.opsize of
          S_B: inc(instr.oper[opidx]^.ref^.offset,3);
          S_W: inc(instr.oper[opidx]^.ref^.offset,2);
        end;
        result:=true;
      end;

end.
