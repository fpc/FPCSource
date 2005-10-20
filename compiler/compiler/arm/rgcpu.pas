{
    Copyright (c) 1998-2003 by Florian Klaempfl

    This unit implements the arm specific class for the register
    allocator

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
       aasmbase,aasmtai,aasmcpu,
       cgbase,cgutils,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         procedure do_spill_read(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);override;
         procedure do_spill_written(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);override;
       end;

       trgintcpu = class(trgcpu)
         procedure add_cpu_interferences(p : tai);override;
       end;

  implementation

    uses
      verbose, cutils,
      cgobj,
      procinfo;


    procedure trgcpu.do_spill_read(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins: tai;
        tmpref : treference;
        helplist : taasmoutput;
        l : tasmlabel;
        hreg : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=taasmoutput.create;
            reference_reset(tmpref);
            { create consts entry }
            objectlibrary.getjumplabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(spilltemp.offset));

            { load consts entry }
            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,R_SUBWHOLE)
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,hreg,tmpref));

            reference_reset_base(tmpref,hreg,0);

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            helpins:=spilling_create_load(tmpref,tempreg);
            helplist.concat(helpins);
            if pos=nil then
              list.insertlistafter(list.first,helplist)
            else
              list.insertlistafter(pos.next,helplist);

            helplist.free;
          end
        else
          inherited do_spill_read(list,pos,spilltemp,tempreg);
      end;


    procedure trgcpu.do_spill_written(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins: tai;
        tmpref : treference;
        helplist : taasmoutput;
        l : tasmlabel;
        hreg : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=taasmoutput.create;
            reference_reset(tmpref);
            { create consts entry }
            objectlibrary.getjumplabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(spilltemp.offset));

            { load consts entry }
            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,R_SUBWHOLE)
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,hreg,tmpref));

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            reference_reset_base(tmpref,hreg,0);

            helplist.concat(spilling_create_store(tempreg,tmpref));

            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist)
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
      end;


    procedure trgintcpu.add_cpu_interferences(p : tai);
      begin
        if p.typ=ait_instruction then
          begin
            case taicpu(p).opcode of
              A_MUL:
                add_edge(getsupreg(taicpu(p).oper[0]^.reg),getsupreg(taicpu(p).oper[1]^.reg));
              A_UMULL,
              A_UMLAL,
              A_SMULL,
              A_SMLAL:
                begin
                  add_edge(getsupreg(taicpu(p).oper[0]^.reg),getsupreg(taicpu(p).oper[1]^.reg));
                  add_edge(getsupreg(taicpu(p).oper[1]^.reg),getsupreg(taicpu(p).oper[2]^.reg));
                  add_edge(getsupreg(taicpu(p).oper[0]^.reg),getsupreg(taicpu(p).oper[2]^.reg));
                end;
            end;
          end;
      end;


end.
