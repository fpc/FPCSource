{
    Copyright (c) 1998-2008 by Florian Klaempfl

    This unit implements the Z80 specific class for the register
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
       aasmbase,aasmtai,aasmdata,aasmcpu,aasmsym,
       cgbase,cgutils,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         procedure add_constraints(reg:tregister);override;
         procedure do_spill_read(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister); override;
         procedure do_spill_written(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister); override;
         function do_spill_replace(list : TAsmList;instr : tai_cpu_abstract_sym; orgreg : tsuperregister;const spilltemp : treference) : boolean; override;
       end;

       trgintcpu = class(trgcpu)
         procedure add_cpu_interferences(p : tai);override;
       end;

  implementation

    uses
      verbose, cutils,
      cgobj,
      procinfo;


    procedure trgcpu.add_constraints(reg:tregister);
      var
        supreg,i : Tsuperregister;
      begin
        case getsubreg(reg) of
          R_SUBL,R_SUBH:
            begin
              { Some registers have no 8-bit subregister }
              supreg:=getsupreg(reg);
              add_edge(supreg,RS_IX);
              add_edge(supreg,RS_IY);
              add_edge(supreg,RS_SP);
            end;
          else
            ;
        end;
      end;


    procedure trgcpu.do_spill_read(list: TAsmList; pos: tai; const spilltemp: treference; tempreg: tregister; orgsupreg: tsuperregister);
      var
        tmpref   : treference;
        helplist : TAsmList;
      begin
        if (spilltemp.base=NR_IX) and ((spilltemp.offset<-128) or (spilltemp.offset>127)) then
          begin
            helplist:=TAsmList.create;

            helplist.concat(taicpu.op_reg(A_PUSH,NR_BC));
            helplist.concat(taicpu.op_reg(A_PUSH,NR_IX));
            helplist.concat(taicpu.op_reg(A_POP,NR_BC));
            helplist.concat(taicpu.op_reg_const(A_LD,NR_IY,spilltemp.offset));
            helplist.concat(taicpu.op_reg_reg(A_ADD,NR_IY,NR_BC));
            helplist.concat(taicpu.op_reg(A_POP,NR_BC));
            reference_reset_base(tmpref,NR_IY,0,ctempposinvalid,1,[]);
            helplist.concat(spilling_create_load(tmpref,tempreg));

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
      begin
        if (spilltemp.base=NR_IX) and ((spilltemp.offset<-128) or (spilltemp.offset>127)) then
          begin
            helplist:=TAsmList.create;

            helplist.concat(taicpu.op_reg(A_PUSH,NR_BC));
            helplist.concat(taicpu.op_reg(A_PUSH,NR_IX));
            helplist.concat(taicpu.op_reg(A_POP,NR_BC));
            helplist.concat(taicpu.op_reg_const(A_LD,NR_IY,spilltemp.offset));
            helplist.concat(taicpu.op_reg_reg(A_ADD,NR_IY,NR_BC));
            helplist.concat(taicpu.op_reg(A_POP,NR_BC));
            reference_reset_base(tmpref,NR_IY,0,ctempposinvalid,1,[]);
            helplist.concat(spilling_create_store(tempreg,tmpref));

            list.insertlistafter(pos,helplist);
            helplist.free;
          end
        else
          inherited;
      end;


    procedure trgintcpu.add_cpu_interferences(p : tai);
      var
        r : tsuperregister;
      begin
        //if p.typ=ait_instruction then
        //  begin
        //    case taicpu(p).opcode of
        //      A_CPI,
        //      A_ANDI,
        //      A_ORI,
        //      A_SUBI,
        //      A_SBCI,
        //      A_LDI:
        //        for r:=RS_R0 to RS_R15 do
        //          add_edge(r,GetSupReg(taicpu(p).oper[0]^.reg));
        //      A_MULS:
        //        begin
        //          for r:=RS_R0 to RS_R15 do
        //            add_edge(r,GetSupReg(taicpu(p).oper[0]^.reg));
        //          for r:=RS_R0 to RS_R15 do
        //            add_edge(r,GetSupReg(taicpu(p).oper[1]^.reg));
        //        end;
        //    end;
        //  end;
      end;


    function trgcpu.do_spill_replace(list:TAsmList;instr:tai_cpu_abstract_sym;orgreg:tsuperregister;const spilltemp:treference):boolean;
      var
        b : byte;
      begin
        result:=false;
        if (spilltemp.offset<-128) or (spilltemp.offset>127) then
          exit;

        { Replace 'ld  orgreg,src' with 'ld  spilltemp,src'
          and     'ld  dst,orgreg' with 'ld  dst,spilltemp' }
        with instr do
          begin
            if (opcode=A_LD) and (ops=2) and (oper[1]^.typ=top_reg) and (oper[0]^.typ=top_reg) then
              begin
                if (getregtype(oper[0]^.reg)=regtype) and
                   (get_alias(getsupreg(oper[0]^.reg))=orgreg) and
                   (get_alias(getsupreg(oper[1]^.reg))<>orgreg) then
                  begin
                    instr.loadref(0,spilltemp);
                    result:=true;
                  end
                else if (getregtype(oper[1]^.reg)=regtype) and
                   (get_alias(getsupreg(oper[1]^.reg))=orgreg) and
                   (get_alias(getsupreg(oper[0]^.reg))<>orgreg) then
                  begin
                    instr.loadref(1,spilltemp);
                    result:=true;
                  end;
              end
            { Replace 'ld  orgreg,const' with 'ld  spilltemp,const' }
            else if (opcode=A_LD) and (ops=2) and (oper[1]^.typ=top_const) and (oper[0]^.typ=top_reg) then
              begin
                if (getregtype(oper[0]^.reg)=regtype) and
                   (get_alias(getsupreg(oper[0]^.reg))=orgreg) then
                  begin
                    instr.loadref(0,spilltemp);
                    result:=true;
                  end;
              end
            { Replace 'add A,orgreg' with 'add A,spilltemp'
              and     'adc A,orgreg' with 'adc A,spilltemp'
              and     'sub A,orgreg' with 'sub A,spilltemp'
              and     'sbc A,orgreg' with 'sbc A,spilltemp'
              and     'and A,orgreg' with 'and A,spilltemp'
              and     'or  A,orgreg' with 'or  A,spilltemp'
              and     'xor A,orgreg' with 'xor A,spilltemp'
              and     'cp  A,orgreg' with 'cp  A,spilltemp' }
            else if (opcode in [A_ADD,A_ADC,A_SUB,A_SBC,A_AND,A_OR,A_XOR,A_CP]) and (ops=2) and (oper[1]^.typ=top_reg) and (oper[0]^.typ=top_reg) then
              begin
                { we don't really need to check whether the first register is 'A',
                  because that's the only register allowed as a destination for
                  these instructions }
                if (getregtype(oper[1]^.reg)=regtype) and
                   (get_alias(getsupreg(oper[1]^.reg))=orgreg) and
                   (get_alias(getsupreg(oper[0]^.reg))<>orgreg) then
                  begin
                    instr.loadref(1,spilltemp);
                    result:=true;
                  end;
              end
            { Replace 'bit const,orgreg' with 'bit const,spilltemp'
              and     'set const,orgreg' with 'set const,spilltemp'
              and     'res const,orgreg' with 'res const,spilltemp' }
            else if (opcode in [A_BIT,A_SET,A_RES]) and (ops=2) and (oper[1]^.typ=top_reg) and (oper[0]^.typ=top_const) then
              begin
                if (getregtype(oper[1]^.reg)=regtype) and
                   (get_alias(getsupreg(oper[1]^.reg))=orgreg) then
                  begin
                    instr.loadref(1,spilltemp);
                    result:=true;
                  end;
              end
            { Replace 'inc orgreg' with 'inc spilltemp'
              and     'dec orgreg' with 'dec spilltemp'
              and     'add orgreg' with 'add spilltemp'
              and     'adc orgreg' with 'adc spilltemp'
              and     'sub orgreg' with 'sub spilltemp'
              and     'sbc orgreg' with 'sbc spilltemp'
              and     'and orgreg' with 'and spilltemp'
              and     'or  orgreg' with 'or  spilltemp'
              and     'xor orgreg' with 'xor spilltemp'
              and     'cp  orgreg' with 'cp  spilltemp'
              and     'rlc orgreg' with 'rlc spilltemp'
              and     'rl  orgreg' with 'rl  spilltemp'
              and     'rrc orgreg' with 'rrc spilltemp'
              and     'rr  orgreg' with 'rr  spilltemp'
              and     'sla orgreg' with 'sla spilltemp'
              and     'sra orgreg' with 'sra spilltemp'
              and     'srl orgreg' with 'srl spilltemp' }
            else if (opcode in [A_INC,A_DEC,A_ADD,A_ADC,A_SUB,A_SBC,A_AND,A_OR,A_XOR,A_CP,
                     A_RLC,A_RL,A_RRC,A_RR,A_SLA,A_SRA,A_SRL]) and (ops=1) and (oper[0]^.typ=top_reg) then
              begin
                if (getregtype(oper[0]^.reg)=regtype) and
                   (get_alias(getsupreg(oper[0]^.reg))=orgreg) then
                  begin
                    instr.loadref(0,spilltemp);
                    result:=true;
                  end;
              end;
          end;
      end;

end.
