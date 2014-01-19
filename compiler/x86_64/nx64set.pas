{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86_64 assembler for in set/case nodes

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
unit nx64set;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      node,nset,pass_1,nx86set;

    type
      tx8664casenode = class(tx86casenode)
         procedure optimizevalues(var max_linear_list:aint;var max_dist:aword);override;
         procedure genjumptable(hp : pcaselabel;min_,max_ : aint);override;
      end;


implementation

    uses
      systems,
      verbose,globals,constexp,
      symconst,symdef,defutil,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,procinfo,
      cga,cgutils,cgobj,ncgutil,
      cgx86;


{*****************************************************************************
                            TX8664CASENODE
*****************************************************************************}

    procedure tx8664casenode.optimizevalues(var max_linear_list:aint;var max_dist:aword);
      begin
        inc(max_linear_list,9);
      end;


    { Always generate position-independent jump table, it is twice less in size at a price
      of two extra instructions (which shouldn't cause more slowdown than pipeline trashing) }
    procedure tx8664casenode.genjumptable(hp : pcaselabel; min_,max_ : aint);
      var
        last: TConstExprInt;
        tablelabel: TAsmLabel;
        basereg,indexreg,jumpreg: TRegister;
        href: TReference;
        opcgsize: tcgsize;

      procedure genitem(list:TAsmList;t : pcaselabel);
        var
          i : aint;
        begin
          if assigned(t^.less) then
            genitem(list,t^.less);
          { fill possible hole }
          i:=last.svalue+1;
          while i<=t^._low.svalue-1 do
            begin
              list.concat(Tai_const.Create_rel_sym(aitconst_32bit,tablelabel,elselabel));
              inc(i);
            end;
          i:=t^._low.svalue;
          while i<=t^._high.svalue do
            begin
              list.concat(Tai_const.Create_rel_sym(aitconst_32bit,tablelabel,blocklabel(t^.blockid)));
              inc(i);
            end;
          last:=t^._high;
          if assigned(t^.greater) then
            genitem(list,t^.greater);
        end;

      begin
        last:=min_;
        opcgsize:=def_cgsize(opsize);
        if not(jumptable_no_range) then
          begin
             { a <= x <= b <-> unsigned(x-a) <= (b-a) }
             cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,opcgsize,aint(min_),hregister);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opcgsize,OC_A,aint(max_)-aint(min_),hregister,elselabel);
             min_:=0;
          end;
        { local label in order to avoid using GOT }
        current_asmdata.getlabel(tablelabel,alt_data);
        indexreg:=cg.makeregsize(current_asmdata.CurrAsmList,hregister,OS_ADDR);
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,opcgsize,OS_ADDR,hregister,indexreg);
        { load table address }
        reference_reset_symbol(href,tablelabel,0,4);
        basereg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,basereg);
        { load table slot, 32-bit sign extended }
        reference_reset_base(href,basereg,-aint(min_)*4,4);
        href.index:=indexreg;
        href.scalefactor:=4;
        jumpreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_S32,OS_ADDR,href,jumpreg);
        { add table address }
        reference_reset_base(href,basereg,0,sizeof(pint));
        href.index:=jumpreg;
        href.scalefactor:=1;
        cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,jumpreg);
        { and finally jump }
        emit_reg(A_JMP,S_NO,jumpreg);
        { generate jump table }
        new_section(current_procinfo.aktlocaldata,sec_rodata,current_procinfo.procdef.mangledname,4);
        current_procinfo.aktlocaldata.concat(Tai_label.Create(tablelabel));
        genitem(current_procinfo.aktlocaldata,hp);
      end;

begin
   ccasenode:=tx8664casenode;
end.
