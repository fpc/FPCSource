{
    $Id$
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
       cgbase,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         procedure add_cpu_interferences(p : tai);override;
         procedure do_spill_read(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
         procedure do_spill_written(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
         procedure do_spill_readwritten(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
       end;

  implementation

    uses
      verbose, cutils,
      cgutils,cgobj,
      procinfo;


    procedure trgcpu.add_cpu_interferences(p : tai);
      begin
        if p.typ=ait_instruction then
          begin
            if (taicpu(p).opcode=A_MUL) then
              add_edge(getsupreg(taicpu(p).oper[0]^.reg),getsupreg(taicpu(p).oper[1]^.reg));
          end;
      end;


    procedure trgcpu.do_spill_read(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
     const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins: tai;
        tmpref,ref : treference;
        helplist : taasmoutput;
        l : tasmlabel;
        tmpreg : tregister;
      begin
        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
            helplist:=taasmoutput.create;
            reference_reset(tmpref);
            { create consts entry }
            objectlibrary.getlabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

            { load consts entry }
            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              getregisterinline(helplist,pos,defaultsub,tmpreg)
            else
              tmpreg:=cg.getintregister(helplist,OS_ADDR);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;

            helpins:=spilling_create_load(ref,regs[regidx].tempreg);
            helplist.concat(helpins);
            if pos=nil then
              list.insertlistafter(list.first,helplist)
            else
              list.insertlistafter(pos.next,helplist);

            ungetregisterinline(list,helpins,regs[regidx].tempreg);

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              ungetregisterinline(list,helpins,tmpreg);

            forward_allocation(tai(helpins.next),instr);

            helplist.free;
          end
        else
          inherited do_spill_read(list,instr,pos,regidx,spilltemplist,regs);
      end;


    procedure trgcpu.do_spill_written(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
      const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins: tai;
        ref,tmpref : treference;
        helplist : taasmoutput;
        l : tasmlabel;
        tmpreg : tregister;
      begin
        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
            helplist:=taasmoutput.create;
            reference_reset(tmpref);
            { create consts entry }
            objectlibrary.getlabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

            { load consts entry }
            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              getregisterinline(helplist,pos,defaultsub,tmpreg)
            else
              tmpreg:=cg.getintregister(helplist,OS_ADDR);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;

            helplist.concat(spilling_create_store(regs[regidx].tempreg,ref));
            ungetregisterinline(helplist,tai(helplist.last),regs[regidx].tempreg);
            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,tai(helplist.last),tmpreg);

            list.insertlistafter(instr,helplist);

            helplist.free;
          end
        else
          inherited do_spill_written(list,instr,pos,regidx,spilltemplist,regs);
      end;


    procedure trgcpu.do_spill_readwritten(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
      const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins1, helpins2: tai;
        tmpref,ref : treference;
        helplist : taasmoutput;
        l : tasmlabel;
        tmpreg : tregister;
      begin
        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
            helplist:=taasmoutput.create;
            reference_reset(tmpref);
            { create consts entry }
            objectlibrary.getlabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

            { load consts entry }
            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              getregisterinline(helplist,pos,defaultsub,tmpreg)
            else
              tmpreg:=cg.getintregister(helplist,OS_ADDR);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;

            helpins1:=spilling_create_load(ref,regs[regidx].tempreg);
            helplist.concat(helpins1);
            if pos=nil then
              list.insertlistafter(list.first,helplist)
            else
              list.insertlistafter(pos.next,helplist);

            helpins2:=spilling_create_store(regs[regidx].tempreg,ref);
            list.insertafter(helpins2,instr);
            ungetregisterinline(list,helpins2,regs[regidx].tempreg);

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              ungetregisterinline(list,helpins2,tmpreg);

            forward_allocation(tai(helpins1.next),instr);
          end
        else
          inherited do_spill_readwritten(list,instr,pos,regidx,spilltemplist,regs);
      end;

end.

{
  $Log$
  Revision 1.12  2004-06-20 08:55:31  florian
    * logs truncated

  Revision 1.11  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.10.2.4  2004/06/13 20:38:38  florian
    * fixed floating point register spilling on sparc

  Revision 1.10.2.3  2004/06/13 16:02:39  florian
    * fixed floating point register spilling problems with offsets > 4095

  Revision 1.10.2.2  2004/06/13 10:51:17  florian
    * fixed several register allocator problems (sparc/arm)

  Revision 1.10.2.1  2004/06/12 17:01:01  florian
    * fixed compilation of arm compiler

}
