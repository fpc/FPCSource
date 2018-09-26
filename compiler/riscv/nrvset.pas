{
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate Risc-V32/64 assembler for in set/case nodes

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
unit nrvset;

{$i fpcdefs.inc}

interface

    uses
       node,nset,ncgset,cpubase,cgbase,cgobj,aasmbase,aasmtai,aasmdata,globtype;

    type
       trvcasenode = class(tcgcasenode)
         protected
           procedure optimizevalues(var max_linear_list : aint; var max_dist : aword);override;
           function  has_jumptable : boolean;override;
           procedure genjumptable(hp : pcaselabel;min_,max_ : aint);override;
       end;


implementation

    uses
      systems,
      verbose,globals,constexp,
      symconst,symdef,defutil,
      paramgr,
      cpuinfo,
      pass_2,cgcpu,
      ncon,
      tgobj,ncgutil,rgobj,aasmcpu,
      procinfo,
      cgutils;

{*****************************************************************************
                            TCGCASENODE
*****************************************************************************}


    procedure trvcasenode.optimizevalues(var max_linear_list : aint; var max_dist : aword);
      begin
        max_linear_list := 3;
      end;
    

    function trvcasenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure trvcasenode.genjumptable(hp : pcaselabel;min_,max_ : aint);
      var
        table : tasmlabel;
        last : TConstExprInt;
        indexreg : tregister;
        href : treference;

        procedure genitem(list:TAsmList;t : pcaselabel);
          var
            i : TConstExprInt;
          begin
            if assigned(t^.less) then
              genitem(list,t^.less);
            { fill possible hole }
            i:=last+1;
            while i<=t^._low-1 do
              begin
                list.concat(Tai_const.Create_rel_sym(aitconst_32bit,table,elselabel));
                i:=i+1;
              end;
            i:=t^._low;
            while i<=t^._high do
              begin
                list.concat(Tai_const.Create_rel_sym(aitconst_32bit,table,blocklabel(t^.blockid)));
                i:=i+1;
              end;
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(list,t^.greater);
          end;

      begin
        last:=min_;

        {
        .l
          auipc x,hi(tbl-.l)
          addi x,x,lo(tbl-.l)
          sll idx,idx,2
          add idx,idx,x
          lw idx,idx,lo(tbl-.l)
          add idx,idx,x
          jalr x0,idx
        }

        { make it a 32bit register }
        // allocate base and index registers register
        indexreg:= cg.makeregsize(current_asmdata.CurrAsmList, hregister, OS_INT);
        { indexreg := hregister; }
        cg.a_load_reg_reg(current_asmdata.CurrAsmList, def_cgsize(opsize), OS_INT, hregister, indexreg);
        { a <= x <= b <-> unsigned(x-a) <= (b-a) }
        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,aint(min_),indexreg);
        if not(jumptable_no_range) then
          begin
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_A,aint(max_)-aint(min_),indexreg,elselabel);
          end;
        current_asmdata.getjumplabel(table);
        { create reference, indexreg := indexreg * sizeof(jtentry) (= 4) }
        cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_MUL, OS_INT, 4, indexreg);
        reference_reset_symbol(href, table, 0, 4,[]);

        hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,hregister);
        reference_reset_base(href,hregister,0,ctempposinvalid,4,[]);
        href.index:=indexreg;
        indexreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        { load table entry }
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_S32,OS_ADDR,href,indexreg);
        { add table base }
        cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_ADDR,hregister,indexreg);
        { jump }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_JALR,NR_X0, indexreg));

        { generate jump table }
        current_asmdata.CurrAsmList.concat(cai_align.Create(4));
        current_asmdata.CurrAsmList.concat(Tai_label.Create(table));
        genitem(current_asmdata.CurrAsmList,hp);
      end;


begin
   ccasenode:=trvcasenode;
end.
