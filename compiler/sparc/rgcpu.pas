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
        { Let 64bit floats conflict with all odd float regs }
        if getsubreg(reg)=R_SUBFD then
          begin
            supreg:=getsupreg(reg);
            i:=RS_F1;
            while (i<=RS_F31) do
              begin
                add_edge(supreg,i);
                inc(i,2);
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

            ungetregisterinline(list,tai(helplist.last),regs[regidx].tempreg);

            if getregtype(regs[regidx].tempreg)=R_INTREGISTER then
              ungetregisterinline(list,tai(helplist.last),tmpreg);

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
  Revision 1.22  2004-06-20 08:47:33  florian
    * spilling of doubles on sparc fixed

  Revision 1.21  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.20.2.4  2004/06/13 20:38:38  florian
    * fixed floating point register spilling on sparc

  Revision 1.20.2.3  2004/06/13 10:51:17  florian
    * fixed several register allocator problems (sparc/arm)

  Revision 1.20.2.2  2004/06/03 19:23:41  florian
    * fixed some spilling issues

  Revision 1.20.2.1  2004/05/30 17:54:14  florian
    + implemented cmp64bit
    * started to fix spilling
    * fixed int64 sub partially

  Revision 1.20  2003/10/24 15:20:37  peter
    * added more register functions

  Revision 1.19  2003/10/01 20:34:50  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.18  2003/09/14 19:19:05  peter
    * updates for new ra

  Revision 1.17  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.16.2.2  2003/09/02 17:49:17  peter
    * newra updates

  Revision 1.16.2.1  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.16  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.15  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.14  2003/06/17 16:36:11  peter
    * freeintparaloc added

  Revision 1.13  2003/06/12 22:47:52  mazen
  - unused temp var r removed in GetExplicitRegisterInt function
  * some case added for var and fauncions naming

  Revision 1.12  2003/06/12 21:11:44  peter
    * updates like the powerpc

  Revision 1.11  2003/06/01 21:38:07  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.10  2003/05/31 01:00:51  peter
    * register fixes

  Revision 1.9  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.8  2003/03/15 22:51:58  mazen
  * remaking sparc rtl compile

  Revision 1.7  2003/03/10 21:59:54  mazen
  * fixing index overflow in handling new registers arrays.

  Revision 1.6  2003/02/19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.5  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.4  2002/10/13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.3  2002/10/12 19:03:23  mazen
  * Get/Unget expilit registers to be re-examined

}
