{
    Copyright (c) 1998-2009 by Florian Klaempfl and David Zhang

    This unit implements the register allocator for MIPLEL

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
        procedure add_constraints(reg:tregister);override;
        function get_spill_subreg(r : tregister) : tsubregister;override;
        procedure do_spill_read(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);override;
        procedure do_spill_written(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);override;
      end;


implementation

    uses
      globtype,
      verbose,cutils,
      cgobj;

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
              i:=RS_R1;
              while (i<=RS_R31) do
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


    procedure trgcpu.do_spill_read(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : tasmlist;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=tasmlist.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=tempreg
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            reference_reset(tmpref,sizeof(aint));
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_high;
            helplist.concat(taicpu.op_reg_ref(A_LUI,hreg,tmpref));

            tmpref.refaddr:=addr_low;
            helplist.concat(taicpu.op_reg_reg_ref(A_ADDIU,hreg,hreg,tmpref));
            helplist.concat(taicpu.op_reg_reg_reg(A_ADDU,hreg,hreg,spilltemp.base));

            reference_reset_base(tmpref,hreg,0,sizeof(aint));

            helpins:=spilling_create_load(tmpref,tempreg);
            helplist.concat(helpins);
            list.insertlistafter(pos,helplist)
          end
        else
          inherited do_spill_read(list,pos,spilltemp,tempreg);
      end;


    procedure trgcpu.do_spill_written(list:tasmlist;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : tasmlist;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=tasmlist.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,R_SUBWHOLE)
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            reference_reset(tmpref,sizeof(aint));
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_high;
            helplist.concat(taicpu.op_reg_ref(A_LUI,hreg,tmpref));

            tmpref.refaddr:=addr_low;
            helplist.concat(taicpu.op_reg_reg_ref(A_ADDIU,hreg,hreg,tmpref));
            helplist.concat(taicpu.op_reg_reg_reg(A_ADDU,hreg,hreg,spilltemp.base));

            reference_reset_base(tmpref,hreg,0,sizeof(aint));

            helpins:=spilling_create_store(tempreg,tmpref);
            helplist.concat(helpins);
            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist)
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
    end;

end.
