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
       trgcpu = class(trgobj)
         procedure do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
         procedure do_spill_written(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
         procedure add_constraints(reg:tregister);override;
         function  get_spill_subreg(r:tregister) : tsubregister;override;
       end;

       trgcputhumb2 = class(trgobj)
         procedure do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
         procedure do_spill_written(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);override;
       end;

       trgintcputhumb2 = class(trgcputhumb2)
         procedure add_cpu_interferences(p : tai);override;
       end;

       trgintcpu = class(trgcpu)
         procedure add_cpu_interferences(p : tai);override;
       end;

  implementation

    uses
      verbose, cutils,globtype,globals,cpuinfo,
      cgobj,
      procinfo;

    procedure trgintcputhumb2.add_cpu_interferences(p: tai);
      var
        r : tregister;
      begin
        if p.typ=ait_instruction then
          begin
            case taicpu(p).opcode of
              A_ADD:
                begin
                  if taicpu(p).ops = 3 then
                    begin
                      if (taicpu(p).oper[0]^.typ = top_reg) and
                         (taicpu(p).oper[1]^.typ = top_reg) and
                         (taicpu(p).oper[2]^.typ in [top_reg, top_shifterop]) then
                        begin
                          { if d == 13 || (d == 15 && S == ‚Äò0‚Äô) || n == 15 || m IN [13,15] then UNPREDICTABLE; }
                          add_edge(getsupreg(taicpu(p).oper[0]^.reg), RS_R13);
                          if taicpu(p).oppostfix <> PF_S then
                            add_edge(getsupreg(taicpu(p).oper[0]^.reg), RS_R15);

                          add_edge(getsupreg(taicpu(p).oper[1]^.reg), RS_R15);

                          if (taicpu(p).oper[2]^.typ = top_shifterop) and
                             (taicpu(p).oper[2]^.shifterop^.rs <> NR_NO) then
                            begin
                              add_edge(getsupreg(taicpu(p).oper[2]^.shifterop^.rs), RS_R13);
                              add_edge(getsupreg(taicpu(p).oper[2]^.shifterop^.rs), RS_R15);
                            end
                          else if (taicpu(p).oper[2]^.typ = top_reg) then
                            begin
                              add_edge(getsupreg(taicpu(p).oper[2]^.reg), RS_R13);
                              add_edge(getsupreg(taicpu(p).oper[2]^.reg), RS_R15);
                            end;
                        end;
                    end;
                end;
              A_LDRB,
              A_STRB,
              A_STR,
              A_LDR,
              A_LDRH,
              A_STRH,
              A_LDRSB,
              A_LDRSH,
              A_LDRD,
              A_STRD:
                { don't mix up the framepointer and stackpointer with pre/post indexed operations }
                if (taicpu(p).oper[1]^.typ=top_ref) and
                  (taicpu(p).oper[1]^.ref^.addressmode in [AM_PREINDEXED,AM_POSTINDEXED]) then
                  begin
                    add_edge(getsupreg(taicpu(p).oper[1]^.ref^.base),getsupreg(current_procinfo.framepointer));
                    { FIXME: temp variable r is needed here to avoid Internal error 20060521 }
                    {        while compiling the compiler. }
                    r:=NR_STACK_POINTER_REG;
                    if current_procinfo.framepointer<>r then
                      add_edge(getsupreg(taicpu(p).oper[1]^.ref^.base),getsupreg(r));
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
        { don't load spilled register between
          mov lr,pc
          mov pc,r4
          but befure the mov lr,pc
        }
        if assigned(pos.previous) and
          (pos.typ=ait_instruction) and
          (taicpu(pos).opcode=A_MOV) and
          (taicpu(pos).oper[0]^.typ=top_reg) and
          (taicpu(pos).oper[0]^.reg=NR_R14) and
          (taicpu(pos).oper[1]^.typ=top_reg) and
          (taicpu(pos).oper[1]^.reg=NR_PC) then
          pos:=tai(pos.previous);

        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=TAsmList.create;
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
            helplist.concat(taicpu.op_reg_ref(A_LDR,hreg,tmpref));

            reference_reset_base(tmpref,current_procinfo.framepointer,0,sizeof(aint));
            tmpref.index:=hreg;

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

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
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=TAsmList.create;
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
            helplist.concat(taicpu.op_reg_ref(A_LDR,hreg,tmpref));

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            reference_reset_base(tmpref,current_procinfo.framepointer,0,sizeof(pint));
            tmpref.index:=hreg;

            helplist.concat(spilling_create_store(tempreg,tmpref));

            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
      end;


    procedure trgcpu.add_constraints(reg:tregister);
      var
        supreg,i : Tsuperregister;
      begin
        case getsubreg(reg) of
          { Let 32bit floats conflict with all double precision regs > 15
            (since these don't have 32 bit equivalents) }
          R_SUBFS:
            begin
              supreg:=getsupreg(reg);
              for i:=RS_D16 to RS_D31 do
                add_edge(supreg,i);
            end;
        end;
      end;


    function  trgcpu.get_spill_subreg(r:tregister) : tsubregister;
      begin
        if (getregtype(r)<>R_MMREGISTER) then
          result:=defaultsub
        else
          result:=getsubreg(r);
      end;


    procedure trgcputhumb2.do_spill_read(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        tmpref : treference;
        helplist : TAsmList;
        l : tasmlabel;
        hreg : tregister;
      begin
        { don't load spilled register between
          mov lr,pc
          mov pc,r4
          but befure the mov lr,pc
        }
        if assigned(pos.previous) and
          (pos.typ=ait_instruction) and
          (taicpu(pos).opcode=A_MOV) and
          (taicpu(pos).oper[0]^.typ=top_reg) and
          (taicpu(pos).oper[0]^.reg=NR_R14) and
          (taicpu(pos).oper[1]^.typ=top_reg) and
          (taicpu(pos).oper[1]^.reg=NR_PC) then
          pos:=tai(pos.previous);

        if (spilltemp.offset>4095) or (spilltemp.offset<-255) then
          begin
            helplist:=TAsmList.create;
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
            helplist.concat(taicpu.op_reg_ref(A_LDR,hreg,tmpref));

            reference_reset_base(tmpref,current_procinfo.framepointer,0,sizeof(aint));
            tmpref.index:=hreg;

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            helplist.concat(spilling_create_load(tmpref,tempreg));
            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_read(list,pos,spilltemp,tempreg);
      end;


    procedure trgcputhumb2.do_spill_written(list:TAsmList;pos:tai;const spilltemp:treference;tempreg:tregister);
      var
        tmpref : treference;
        helplist : TAsmList;
        l : tasmlabel;
        hreg : tregister;
      begin
        if (spilltemp.offset>4095) or (spilltemp.offset<-255) then
          begin
            helplist:=TAsmList.create;
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
            helplist.concat(taicpu.op_reg_ref(A_LDR,hreg,tmpref));

            if spilltemp.index<>NR_NO then
              internalerror(200401263);

            reference_reset_base(tmpref,current_procinfo.framepointer,0,sizeof(pint));
            tmpref.index:=hreg;

            helplist.concat(spilling_create_store(tempreg,tmpref));

            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited do_spill_written(list,pos,spilltemp,tempreg);
      end;


    procedure trgintcpu.add_cpu_interferences(p : tai);
      var
        r : tregister;
      begin
        if p.typ=ait_instruction then
          begin
            case taicpu(p).opcode of
              A_MLA,
              A_MUL:
                if current_settings.cputype<cpu_armv6 then
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
              A_LDRB,
              A_STRB,
              A_STR,
              A_LDR,
              A_LDRH,
              A_STRH:
                { don't mix up the framepointer and stackpointer with pre/post indexed operations }
                if (taicpu(p).oper[1]^.typ=top_ref) and
                  (taicpu(p).oper[1]^.ref^.addressmode in [AM_PREINDEXED,AM_POSTINDEXED]) then
                  begin
                    add_edge(getsupreg(taicpu(p).oper[1]^.ref^.base),getsupreg(current_procinfo.framepointer));
                    { FIXME: temp variable r is needed here to avoid Internal error 20060521 }
                    {        while compiling the compiler. }
                    r:=NR_STACK_POINTER_REG;
                    if current_procinfo.framepointer<>r then
                      add_edge(getsupreg(taicpu(p).oper[1]^.ref^.base),getsupreg(r));
                  end;
            end;
          end;
      end;


end.
