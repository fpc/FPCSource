{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the SPARC specific class for the register
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

 ****************************************************************************}
unit rgcpu;

{$i fpcdefs.inc}

  interface

    uses
      aasmbase,aasmcpu,aasmtai,
      cgbase,
      cpubase,
      rgobj;

    type
      trgcpu=class(trgobj)
        procedure add_constraints(reg:tregister);override;
        function get_spill_subreg(r : tregister) : tsubregister;override;
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

    procedure trgcpu.add_constraints(reg:tregister);
      var
        supreg,i: Tsuperregister;
        r: Tregister;
      begin
        case getsubreg(reg) of
          { Let 64bit floats conflict with all odd float regs }
          R_SUBFD:
            begin
              supreg:=getsupreg(reg);
              i:=RS_F1;
              while (i<=RS_F31) do
                begin
                  add_edge(supreg,i);
                  inc(i,2);
                end;
            end;
          { Let 64bit ints conflict with all odd int regs }
          R_SUBQ:
            begin
              supreg:=getsupreg(reg);
              i:=RS_G1;
              while (i<=RS_I7) do
                begin
                  add_edge(supreg,i);
                  inc(i,2);
                end;
            end;
        end;
      end;


    function trgcpu.get_spill_subreg(r : tregister) : tsubregister;
      begin
        if getregtype(r)=R_FPUREGISTER then
          result:=getsubreg(r)
        else
          result:=defaultsub;
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

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              getregisterinline(helplist,nil,defaultsub,tmpreg)
            else
              tmpreg:=cg.getintregister(helplist,OS_ADDR);

            tmpref.offset:=ref.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,tmpreg));

            tmpref.refaddr:=addr_lo;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,tmpreg,tmpref,tmpreg));

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
            helplist.free;

            ungetregisterinline(list,helpins,regs[regidx].tempreg);

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              ungetregisterinline(list,helpins,tmpreg);

            forward_allocation(tai(helpins.next),instr);
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

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              getregisterinline(helplist,pos,defaultsub,tmpreg)
            else
              tmpreg:=cg.getintregister(helplist,OS_ADDR);

            tmpref.offset:=ref.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,tmpreg));

            tmpref.refaddr:=addr_lo;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,tmpreg,tmpref,tmpreg));

            if ref.index<>NR_NO then
              internalerror(200401263);
            ref.index:=tmpreg;
            ref.offset:=0;

            helplist.concat(spilling_create_store(regs[regidx].tempreg,ref));
            list.insertlistafter(instr,helplist);
            helplist.free;

            ungetregisterinline(list,tai(helplist.last),regs[regidx].tempreg);

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              ungetregisterinline(list,tai(helplist.last),tmpreg);
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
        tmpreg : tregister;
      begin
        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
            helplist:=taasmoutput.create;
            reference_reset(tmpref);

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              getregisterinline(helplist,nil,defaultsub,tmpreg)
            else
              tmpreg:=cg.getintregister(helplist,OS_ADDR);

            tmpref.offset:=ref.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,tmpreg));

            tmpref.refaddr:=addr_lo;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,tmpreg,tmpref,tmpreg));

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
            helplist.free;

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
  Revision 1.25  2004-09-27 21:23:26  peter
    * fixed spilling code

  Revision 1.24  2004/08/24 21:02:33  florian
    * fixed longbool(<int64>) on sparc

  Revision 1.23  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.22  2004/06/20 08:47:33  florian
    * spilling of doubles on sparc fixed

  Revision 1.21  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.20.2.4  2004/06/13 20:38:38  florian
    * fixed floating point register spilling on sparc

  Revision 1.20.2.3  2004/06/13 10:51:17  florian
    * fixed several register allocator problems (sparc/arm)

  Revision 1.20.2.2  2004/06/03 19:23:41  florian
    * fixed some spilling issues

}
