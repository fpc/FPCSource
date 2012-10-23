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
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cgbase,cgutils,
       cpubase,
       rgobj;

     type

       { trgcpu }

       trgcpu = class(trgobj)
         procedure add_constraints(reg: Tregister); override;
         procedure add_cpu_interferences(p: tai); override;
         procedure do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
         procedure do_spill_written(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
       end;

       trgintcpu = class(trgcpu)
       end;

  implementation

    uses
      verbose, cutils,globtype,
      cgobj,
      procinfo;

    procedure trgcpu.add_constraints(reg: Tregister);
      var
        supreg,i : Tsuperregister;
      begin
        case getsubreg(reg) of
          { Let 64bit ints conflict with all odd int regs }
          R_SUBQ:
            begin
              supreg:=getsupreg(reg);
              i:=RS_R1;
              while (i<=RS_R15) do
                begin
                  add_edge(supreg,i);
                  inc(i,2);
                end;
            end;
        end;
      end;

    procedure trgcpu.add_cpu_interferences(p: tai);
      var
        r : Tsuperregister;
      begin
        if p.typ=ait_instruction then
          begin
            case taicpu(p).opcode of
              A_DIVS,
              A_DIVU:
                begin
                  r:=RS_R1;
                  while r <= RS_R15 do
                    begin
                      add_edge(getsupreg(taicpu(p).oper[0]^.reg), r);
                      inc(r,2);
                    end;
                end;
              A_MACHH,
              A_MACS,
              A_MACU,
              A_MACWH,
              A_MULNWH,
              A_MULS,
              A_MULU,
              A_MULWH:
                begin
                  if taicpu(p).oppostfix=PF_D then
                    begin
                      r:=RS_R1;
                      while r <= RS_R15 do
                        begin
                          add_edge(getsupreg(taicpu(p).oper[0]^.reg), r);
                          inc(r,2);
                        end;
                    end;
                end;
            end;
          end;
      end;

    procedure trgcpu.do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        tmpref : treference;
        helplist : TAsmList;
        l : tasmlabel;
        hreg : tregister;
      begin
        if in_signed_bits(spilltemp.offset,16) then
          begin
            {helplist:=TAsmList.create;
            reference_reset(tmpref,sizeof(aint));
            { create consts entry }
            current_asmdata.getjumplabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(spilltemp.offset));

            { load consts entry }
            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,[R_SUBWHOLE])
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(setoppostfix(taicpu.op_reg_ref(A_LD,hreg,tmpref),PF_W));

            reference_reset_base(tmpref,current_procinfo.framepointer,0,sizeof(aint));
            tmpref.index:=hreg;

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            helplist.concat(spilling_create_load(tmpref,tempreg));
            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;      }
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
        if in_signed_bits(spilltemp.offset,16) then
          begin
            {helplist:=TAsmList.create;
            reference_reset(tmpref,sizeof(aint));
            { create consts entry }
            current_asmdata.getjumplabel(l);
            cg.a_label(current_procinfo.aktlocaldata,l);
            tmpref.symboldata:=current_procinfo.aktlocaldata.last;

            current_procinfo.aktlocaldata.concat(tai_const.Create_32bit(spilltemp.offset));

            { load consts entry }
            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,[R_SUBWHOLE])
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);
            tmpref.symbol:=l;
            tmpref.base:=NR_R15;
            helplist.concat(setoppostfix(taicpu.op_reg_ref(A_LD,hreg,tmpref),PF_W));

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            reference_reset_base(tmpref,current_procinfo.framepointer,0,sizeof(pint));
            tmpref.index:=hreg;

            helplist.concat(spilling_create_store(tempreg,tmpref));

            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;}
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
      end;


end.
