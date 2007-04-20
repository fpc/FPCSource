{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the powerpc specific class for the register
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
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cgbase,cgutils,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         procedure do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
         procedure do_spill_written(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
       end;

  implementation

    uses
      verbose, cutils,
      cgobj,
      procinfo;


    procedure trgcpu.do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        tmpref : treference;
        helplist : TAsmList;
        l : tasmlabel;
        hreg : tregister;
      begin
        if (spilltemp.offset<low(smallint)) or
           (spilltemp.offset>high(smallint)) then
          begin
            helplist:=TAsmList.create;

            if (spilltemp.index<>NR_NO) then
              internalerror(200704201);

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,R_SUBWHOLE)
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);
            reference_reset(tmpref);
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_reg_reg_ref(A_ADDIS,hreg,spilltemp.base,tmpref));
            tmpref:=spilltemp;
            tmpref.refaddr:=addr_lo;
            tmpref.base:=hreg;
            helplist.concat(spilling_create_load(tmpref,tempreg));

            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_read(list,pos,spilltemp,tempreg);
      end;


    procedure trgcpu.do_spill_written(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        tmpref : treference;
        helplist : TAsmList;
        l : tasmlabel;
        hreg : tregister;
      begin
        if (spilltemp.offset<low(smallint)) or
           (spilltemp.offset>high(smallint)) then
          begin
            helplist:=TAsmList.create;

            if (spilltemp.index<>NR_NO) then
              internalerror(200704201);

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,R_SUBWHOLE)
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);
            reference_reset(tmpref);
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_hi;
            helplist.concat(taicpu.op_reg_reg_ref(A_ADDIS,hreg,spilltemp.base,tmpref));
            tmpref:=spilltemp;
            tmpref.refaddr:=addr_lo;
            tmpref.base:=hreg;
            helplist.concat(spilling_create_store(tempreg,tmpref));

            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
      end;

end.
