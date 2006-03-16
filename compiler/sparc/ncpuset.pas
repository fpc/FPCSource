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
      aasmbase,aasmtai,aasmdata,aasmcpu,
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

        procedure genitem(list:TAsmList;t : pcaselabel);
          var
            i : aint;
          begin
            if assigned(t^.less) then
              genitem(list,t^.less);
            { fill possible hole }
            for i:=last+1 to t^._low-1 do
              list.concat(Tai_const.Create_sym(elselabel));
            for i:=t^._low to t^._high do
              list.concat(Tai_const.Create_sym(blocklabel(t^.blockid)));
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(list,t^.greater);
          end;

      begin
        if not(jumptable_no_range) then
          begin
             { case expr less than min_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,jmp_lt,aint(min_),hregister,elselabel);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,jmp_gt,aint(max_),hregister,elselabel);
          end;
        current_asmdata.getjumplabel(table);
        indexreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_ADDR,2,hregister,indexreg);
        { create reference }
        reference_reset_symbol(href,table,0);
        href.offset:=(-aint(min_))*4;
        basereg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,basereg);

        jmpreg:=cg.getaddressregister(current_asmdata.CurrAsmList);

        reference_reset(href);
        href.index:=indexreg;
        href.base:=basereg;
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,jmpreg);

        current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_JMP,jmpreg));
        { Delay slot }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
        { generate jump table }
        new_section(current_procinfo.aktlocaldata,sec_data,current_procinfo.procdef.mangledname,sizeof(aint));
        current_procinfo.aktlocaldata.concat(Tai_label.Create(table));
        last:=min_;
        genitem(current_procinfo.aktlocaldata,hp);
      end;



begin
  ccasenode:=tcpucasenode;
end.
