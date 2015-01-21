{
    Copyright (c) 1998-2009 by Florian Klaempfl and David Zhang

    This unit implements the register allocator for MIPS(EL)

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
      aasmbase,aasmcpu,aasmtai,aasmdata,
      cgbase,cgutils,
      cpubase,
      rgobj;

    type
      trgcpu=class(trgobj)
        function get_spill_subreg(r : tregister) : tsubregister;override;
        procedure do_spill_read(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);override;
        procedure do_spill_written(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);override;
        function do_spill_replace(list:TAsmList;instr:taicpu;orgreg:tsuperregister;const spilltemp:treference):boolean;override;
      end;

      trgintcpu=class(trgcpu)
        procedure add_cpu_interferences(p:tai);override;
      end;

implementation

    uses
      globtype,
      verbose,cutils,
      cgobj;


    function trgcpu.get_spill_subreg(r : tregister) : tsubregister;
      begin
        if getregtype(r)=R_FPUREGISTER then
          result:=getsubreg(r)
        else
          result:=defaultsub;
      end;


    procedure trgcpu.do_spill_read(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : tasmlist;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>32767 then
          begin
            helplist:=tasmlist.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=tempreg
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            helplist.concat(taicpu.op_reg_const(A_LUI,hreg,spilltemp.offset shr 16));
            helplist.concat(taicpu.op_reg_reg_const(A_ORI,hreg,hreg,spilltemp.offset and $FFFF));
            helplist.concat(taicpu.op_reg_reg_reg(A_ADDU,hreg,hreg,spilltemp.base));

            reference_reset_base(tmpref,hreg,0,sizeof(aint));

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
        if abs(spilltemp.offset)>32767 then
          begin
            helplist:=tasmlist.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,[R_SUBWHOLE])
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            helplist.concat(taicpu.op_reg_const(A_LUI,hreg,spilltemp.offset shr 16));
            helplist.concat(taicpu.op_reg_reg_const(A_ORI,hreg,hreg,spilltemp.offset and $FFFF));
            helplist.concat(taicpu.op_reg_reg_reg(A_ADDU,hreg,hreg,spilltemp.base));

            reference_reset_base(tmpref,hreg,0,sizeof(aint));

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
      begin
        result:=false;
        { Replace 'move  orgreg,src' with 'sw  src,spilltemp'
              and 'move  dst,orgreg' with 'lw  dst,spilltemp' }
        { TODO: A_MOV_S and A_MOV_D for float registers are also replaceable }
        if (instr.opcode<>A_MOVE) or (abs(spilltemp.offset)>32767) then
          exit;
        if (instr.ops<>2) or
           (instr.oper[0]^.typ<>top_reg) or
           (instr.oper[1]^.typ<>top_reg) or
           (getregtype(instr.oper[0]^.reg)<>regtype) or
           (getregtype(instr.oper[1]^.reg)<>regtype) then
          InternalError(2013061001);
        if get_alias(getsupreg(instr.oper[1]^.reg))=orgreg then
          begin
            instr.opcode:=A_LW;
          end
        else if get_alias(getsupreg(instr.oper[0]^.reg))=orgreg then
          begin
            instr.opcode:=A_SW;
            instr.oper[0]^:=instr.oper[1]^;
          end
        else
          InternalError(2013061002);
        instr.oper[1]^.typ:=top_ref;
        new(instr.oper[1]^.ref);
        instr.oper[1]^.ref^:=spilltemp;
        result:=true;
      end;


    procedure trgintcpu.add_cpu_interferences(p: tai);
      var
        supreg: tsuperregister;
      begin
        if p.typ<>ait_instruction then
          exit;
        if (taicpu(p).ops>=1) and (taicpu(p).oper[0]^.typ=top_reg) and
          (getregtype(taicpu(p).oper[0]^.reg)=regtype) and
          (taicpu(p).spilling_get_operation_type(0) in [operand_write,operand_readwrite]) then
          begin
            { prevent merging registers with frame/stack pointer, $zero and $at
              if an instruction writes to the register }
            supreg:=getsupreg(taicpu(p).oper[0]^.reg);
            add_edge(supreg,RS_STACK_POINTER_REG);
            add_edge(supreg,RS_FRAME_POINTER_REG);
            add_edge(supreg,RS_R0);
            add_edge(supreg,RS_R1);
          end;
      end;


end.
