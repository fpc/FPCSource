{
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
      aasmbase,aasmcpu,aasmtai,aasmsym,aasmdata,
      cgbase,cgutils,
      cpubase,
      rgobj;

    type
      trgcpu=class(trgobj)
        procedure add_constraints(reg:tregister);override;
        function get_spill_subreg(r : tregister) : tsubregister;override;
        procedure do_spill_read(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister); override;
        procedure do_spill_written(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister); override;
        function  do_spill_replace(list:TAsmList;instr:tai_cpu_abstract_sym;orgreg:tsuperregister;const spilltemp:treference):boolean; override;
      end;


implementation

    uses
      verbose,cutils,
      globtype,
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


    procedure trgcpu.do_spill_read(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister);
      var
        helpins  : tai;
        tmpref   : treference;
        helplist : TAsmList;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=TAsmList.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=tempreg
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

{$ifdef SPARC}
            reference_reset(tmpref,sizeof(pint),[]);
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_high;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,hreg));

            tmpref.refaddr:=addr_low;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,hreg,tmpref,hreg));
{$else SPARC}
            if (spilltemp.offset>=-4294967296) and (spilltemp.offset<=-1) then
              begin
                helplist.concat(taicpu.op_const_reg(A_SETHI,(not(aint(spilltemp.offset)) shr 10) and $3fffff,hreg));
                if (aint(spilltemp.offset) and aint($3ff)) or aint($1c00)<>0 then
                  helplist.concat(taicpu.op_reg_const_reg(A_XOR,hreg,(aint(spilltemp.offset) and aint($3ff)) or aint($1c00),hreg));
              end
            else
              Internalerror(2017090901);
{$endif SPARC}

            reference_reset_base(tmpref,hreg,0,spilltemp.temppos,sizeof(aint),[]);
            tmpref.index:=spilltemp.base;

            helpins:=spilling_create_load(tmpref,tempreg);
            helplist.concat(helpins);
            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited;
      end;


    procedure trgcpu.do_spill_written(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister);
      var
        tmpref   : treference;
        helplist : TAsmList;
        hreg     : tregister;
      begin
        if abs(spilltemp.offset)>4095 then
          begin
            helplist:=TAsmList.create;

            if getregtype(tempreg)=R_INTREGISTER then
              hreg:=getregisterinline(helplist,[R_SUBWHOLE])
            else
              hreg:=cg.getintregister(helplist,OS_ADDR);

{$ifdef SPARC}
            reference_reset(tmpref,sizeof(aint),[]);
            tmpref.offset:=spilltemp.offset;
            tmpref.refaddr:=addr_high;
            helplist.concat(taicpu.op_ref_reg(A_SETHI,tmpref,hreg));

            tmpref.refaddr:=addr_low;
            helplist.concat(taicpu.op_reg_ref_reg(A_OR,hreg,tmpref,hreg));
{$else SPARC}
            if (spilltemp.offset>=-4294967296) and (spilltemp.offset<=-1) then
              begin
                helplist.concat(taicpu.op_const_reg(A_SETHI,(not(aint(spilltemp.offset)) shr 10) and $3fffff,hreg));
                if (aint(spilltemp.offset) and aint($3ff)) or aint($1c00)<>0 then
                  helplist.concat(taicpu.op_reg_const_reg(A_XOR,hreg,(aint(spilltemp.offset) and aint($3ff)) or aint($1c00),hreg));
              end
            else
              Internalerror(2017090901);
{$endif SPARC}

            reference_reset_base(tmpref,hreg,0,spilltemp.temppos,sizeof(aint),[]);
            tmpref.index:=spilltemp.base;

            helplist.concat(spilling_create_store(tempreg,tmpref));
            if getregtype(tempreg)=R_INTREGISTER then
              ungetregisterinline(helplist,hreg);

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited;
    end;


    function trgcpu.do_spill_replace(list:TAsmList;instr:tai_cpu_abstract_sym;orgreg:tsuperregister;const spilltemp:treference):boolean;
      var
        opidx: longint;
      begin
        result:=false;
        { Replace 'mov  src,orgreg' with 'st  src,spilltemp'
              and 'mov  orgreg,dst' with 'ld  spilltemp,dst' }
        if (abs(spilltemp.offset)>4095) then
          exit;
        if ((regtype=R_INTREGISTER) and (instr.opcode<>A_MOV)) or
           ((regtype=R_FPUREGISTER) and (instr.opcode<>A_FMOVs) and (instr.opcode<>A_FMOVd)) then
          exit;
        { Ignore mis-encoded stuff like 'mov %something,%y' }
        if (instr.ops<>2) or
           (instr.oper[0]^.typ<>top_reg) or
           (instr.oper[1]^.typ<>top_reg) or
           (getregtype(instr.oper[0]^.reg)<>regtype) or
           (getregtype(instr.oper[1]^.reg)<>regtype) then
          exit;
        opidx:=-1;
        if (getregtype(instr.oper[0]^.reg)=regtype) and (get_alias(getsupreg(instr.oper[0]^.reg))=orgreg) then
          begin
            if (regtype=R_INTREGISTER) then
              instr.opcode:=A_LD_R
            else if (getsubreg(instr.oper[0]^.reg)=R_SUBFS) then
              instr.opcode:=A_LDF
            else
              instr.opcode:=A_LDDF;
            opidx:=0;
          end
        else if (getregtype(instr.oper[1]^.reg)=regtype) and (get_alias(getsupreg(instr.oper[1]^.reg))=orgreg) then
          begin
            if (regtype=R_INTREGISTER) then
              instr.opcode:=A_ST_R
            else if (getsubreg(instr.oper[1]^.reg)=R_SUBFS) then
              instr.opcode:=A_STF
            else
              instr.opcode:=A_STDF;
            opidx:=1;
          end
        else
          InternalError(2013061002);
        instr.oper[opidx]^.typ:=top_ref;
        new(instr.oper[opidx]^.ref);
        instr.oper[opidx]^.ref^:=spilltemp;
        result:=true;
      end;

end.
