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
       aasmbase,aasmtai,
       cgbase,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         procedure add_cpu_interferences(p : tai);override;
         procedure do_spill_read(list : taasmoutput;instr : taicpu_abstract;pos: tai; regidx: word;
          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
         procedure do_spill_written(list : taasmoutput;instr : taicpu_abstract;pos: tai; regidx: word;
          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
         procedure do_spill_readwritten(list : taasmoutput;instr : taicpu_abstract;pos: tai; regidx: word;
          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
       end;

  implementation

    uses
      verbose, cutils,
      cgutils,cgobj,
      procinfo,
      aasmcpu;


    procedure trgcpu.add_cpu_interferences(p : tai);
      begin
        if p.typ=ait_instruction then
          begin
            if (taicpu(p).opcode=A_MUL) then
              add_edge(getsupreg(taicpu(p).oper[0]^.reg),getsupreg(taicpu(p).oper[1]^.reg));
          end;
      end;


    procedure trgcpu.do_spill_read(list : taasmoutput;instr : taicpu_abstract;pos: tai; regidx: word;
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
            getregisterinline(helplist,nil,defaultsub,tmpreg);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;

            helpins:=taicpu.op_reg_ref(A_LDR,regs[regidx].tempreg,ref);
            helplist.concat(helpins);
            if pos=nil then
              list.insertlistafter(list.first,helplist)
            else
              list.insertlistafter(pos.next,helplist);

            ungetregisterinline(helplist,tai(helplist.last),regs[regidx].tempreg);

            ungetregisterinline(list,instr,regs[regidx].tempreg);
            forward_allocation(tai(helpins.next),instr);

            helplist.free;
          end
        else
          begin
            helpins:=taicpu.op_reg_ref(A_LDR,regs[regidx].tempreg,ref);
            if pos=nil then
              list.insertafter(helpins,list.first)
            else
              list.insertafter(helpins,pos.next);
            ungetregisterinline(list,instr,regs[regidx].tempreg);
            forward_allocation(tai(helpins.next),instr);
          end;
      end;


    procedure trgcpu.do_spill_written(list : taasmoutput;instr : taicpu_abstract;pos: tai; regidx: word;
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
            getregisterinline(helplist,nil,defaultsub,tmpreg);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;

            helplist.concat(taicpu.op_reg_ref(A_STR,regs[regidx].tempreg,ref));
            ungetregisterinline(helplist,tai(helplist.last),regs[regidx].tempreg);
            ungetregisterinline(helplist,tai(helplist.last),tmpreg);

            list.insertlistafter(instr,helplist);

            helplist.free;
          end
        else
          begin
            helpins:=taicpu.op_reg_ref(A_STR,regs[regidx].tempreg,ref);
            list.insertafter(helpins,instr);
            ungetregisterinline(list,helpins,regs[regidx].tempreg);
          end;
      end;


    procedure trgcpu.do_spill_readwritten(list : taasmoutput;instr : taicpu_abstract;pos: tai; regidx: word;
      const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins1, helpins2: tai;
        tmpref,ref : treference;
        tmpreg : tregister;

      begin
        ref:=spilltemplist[regs[regidx].orgreg];
        internalerror(200403141);
        {
        if abs(ref.offset)>4095 then
          begin
            reference_reset(tmpref);
            { create consts entry }
            objectlibrary.getlabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(ref.offset));

            { load consts entry }
            getregisterinline(list,pos,defaultsub,tmpreg);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            list.concat(taicpu.op_reg_ref(A_LDR,tmpreg,tmpref));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;
          end;
        }
        helpins1:=taicpu.op_reg_ref(A_LDR,regs[regidx].tempreg,ref);
        if pos=nil then
          list.insertafter(helpins1,list.first)
        else
          list.insertafter(helpins1,pos.next);
        ref:=spilltemplist[regs[regidx].orgreg];
        ref.symboldata:=nil;
        helpins2:=taicpu.op_reg_ref(A_STR,regs[regidx].tempreg,ref);
        list.insertafter(helpins2,instr);
        ungetregisterinline(list,helpins2,regs[regidx].tempreg);
        forward_allocation(tai(helpins1.next),instr);
      end;

end.

{
  $Log$
  Revision 1.10  2004-03-14 16:15:40  florian
    * spilling problem fixed
    * handling of floating point memory references fixed

  Revision 1.9  2004/03/06 20:35:20  florian
    * fixed arm compilation
    * cleaned up code generation for exported linux procedures

  Revision 1.8  2004/02/08 23:06:59  florian
    * fixed compilation problem

  Revision 1.7  2004/01/28 15:36:47  florian
    * fixed another couple of arm bugs

  Revision 1.6  2004/01/26 19:05:56  florian
    * fixed several arm issues

  Revision 1.5  2003/11/02 14:30:03  florian
    * fixed ARM for new reg. allocation scheme

  Revision 1.4  2003/09/11 11:55:00  florian
    * improved arm code generation
    * move some protected and private field around
    * the temp. register for register parameters/arguments are now released
      before the move to the parameter register is done. This improves
      the code in a lot of cases.

  Revision 1.3  2003/09/04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.2  2003/08/25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.1  2003/08/16 13:23:01  florian
    * several arm related stuff fixed
}
