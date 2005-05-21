{
    Copyright (c) 1998-2004 by Florian Klaempfl

    Generate sparc assembler for in set/case nodes

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
unit ncpuset;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       nset,
       ncgset;

    type
       tcpucasenode = class(tcgcasenode)
         protected
           procedure optimizevalues(var max_linear_list:aint;var max_dist:aword);override;
           function has_jumptable : boolean;override;
           procedure genjumptable(hp : pcaselabel;min_,max_ : aint);override;
       end;


  implementation

    uses
      globals,
      systems,
      cpubase,
      aasmbase,aasmtai,aasmcpu,
      cgbase,cgutils,cgobj,
      procinfo;

    procedure tcpucasenode.optimizevalues(var max_linear_list:aint;var max_dist:aword);
      begin
        { give the jump table a higher priority }
        max_dist:=(max_dist*3) div 2;
      end;


    function tcpucasenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure tcpucasenode.genjumptable(hp : pcaselabel;min_,max_ : aint);
      var
        table : tasmlabel;
        last : TConstExprInt;
        indexreg,jmpreg,basereg : tregister;
        href : treference;
        jumpsegment : TAAsmOutput;

        procedure genitem(t : pcaselabel);
          var
            i : aint;
          begin
            if assigned(t^.less) then
              genitem(t^.less);
            { fill possible hole }
            for i:=last+1 to t^._low-1 do
              jumpSegment.concat(Tai_const.Create_sym(elselabel));
            for i:=t^._low to t^._high do
              jumpSegment.concat(Tai_const.Create_sym(blocklabel(t^.blockid)));
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(t^.greater);
          end;

      begin
        if (cs_create_smart in aktmoduleswitches) or
           (af_smartlink_sections in target_asm.flags) then
          jumpsegment:=current_procinfo.aktlocaldata
        else
          jumpsegment:=datasegment;
        if not(jumptable_no_range) then
          begin
             { case expr less than min_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_lt,aint(min_),hregister,elselabel);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_gt,aint(max_),hregister,elselabel);
          end;
        objectlibrary.getlabel(table);
        indexreg:=cg.getaddressregister(exprasmlist);
        cg.a_op_const_reg_reg(exprasmlist,OP_SHL,OS_ADDR,2,hregister,indexreg);
        { create reference }
        reference_reset_symbol(href,table,0);
        href.offset:=(-aint(min_))*4;
        basereg:=cg.getaddressregister(exprasmlist);
        cg.a_loadaddr_ref_reg(exprasmlist,href,basereg);

        jmpreg:=cg.getaddressregister(exprasmlist);

        reference_reset(href);
        href.index:=indexreg;
        href.base:=basereg;
        cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,jmpreg);

        exprasmlist.concat(taicpu.op_reg(A_JMP,jmpreg));
        { Delay slot }
        exprasmlist.concat(taicpu.op_none(A_NOP));
        { generate jump table }
        if not(cs_littlesize in aktglobalswitches) then
          jumpSegment.concat(Tai_Align.Create_Op(4,0));
        jumpSegment.concat(Tai_label.Create(table));
        last:=min_;
        genitem(hp);
      end;



begin
  ccasenode:=tcpucasenode;
end.
