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
        procedure do_spill_read(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);override;
        procedure do_spill_written(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);override;
      end;


implementation

    uses
      verbose, cutils,
      cgutils,cgobj;

    procedure trgcpu.add_constraints(reg:tregister);
      var
        supreg,i : Tsuperregister;
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


    procedure trgcpu.do_spill_read(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : taasmoutput;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=taasmoutput.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=tempreg
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            reference_reset(tmpref);
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,hreg));

            tmpref.refaddr:=addr_lo;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,hreg,tmpref,hreg));

            reference_reset_base(tmpref,hreg,0);

            helpins:=spilling_create_load(spilltemp,tempreg);
            helplist.concat(helpins);
            list.insertlistafter(pos,helplist)
          end
        else
          inherited do_spill_read(list,pos,spilltemp,tempreg);
      end;


    procedure trgcpu.do_spill_written(list:Taasmoutput;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : taasmoutput;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=taasmoutput.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,R_SUBWHOLE)
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            reference_reset(tmpref);
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,hreg));

            tmpref.refaddr:=addr_lo;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,hreg,tmpref,hreg));

            reference_reset_base(tmpref,hreg,0);

            helpins:=spilling_create_store(tempreg,spilltemp);
            helplist.concat(helpins);
            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist)
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
    end;

end.
{
  $Log$
  Revision 1.29  2004-10-05 20:41:02  peter
    * more spilling rewrites

  Revision 1.28  2004/10/04 20:46:22  peter
    * spilling code rewritten for x86. It now used the generic
      spilling routines. Special x86 optimization still needs
      to be added.
    * Spilling fixed when both operands needed to be spilled
    * Cleanup of spilling routine, do_spill_readwritten removed

  Revision 1.27  2004/10/01 17:33:47  peter
    * indents

  Revision 1.26  2004/09/28 20:19:36  peter
    * fixed crash

  Revision 1.25  2004/09/27 21:23:26  peter
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
