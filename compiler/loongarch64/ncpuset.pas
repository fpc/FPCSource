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
unit ncpuset;

{$i fpcdefs.inc}

interface

    uses
       node,nset,ncgset,cpubase,cgbase,cgobj,aasmbase,aasmtai,aasmdata,globtype;

    type
       tloongarch64casenode = class(tcgcasenode)
         protected
           procedure optimizevalues(var max_linear_list : int64; var max_dist : qword);override;
           function  has_jumptable : boolean;override;
           procedure genjumptable(hp : pcaselabel;min_,max_ : int64);override;
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


    procedure tloongarch64casenode.optimizevalues(var max_linear_list : int64; var max_dist : qword);
      begin
        max_linear_list := 3;
      end;


    function tloongarch64casenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure tloongarch64casenode.genjumptable(hp : pcaselabel;min_,max_ : int64);
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
                list.concat(Tai_const.Create_sym_offset(elselabel,0));
                i:=i+1;
              end;
            i:=t^._low;
            while i<=t^._high do
              begin
                list.concat(Tai_const.Create_sym_offset(blocklabel(t^.blockid),0));
                i:=i+1;
              end;
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(list,t^.greater);
          end;

      begin
        last:=min_;

        {
          la.pcrel x,tbl
          alsl.d y,idx,x,3
          ld.d z,y,0
          jr z
        }

        indexreg:= cg.makeregsize(current_asmdata.CurrAsmList, hregister, OS_INT);
        { indexreg := hregister; }
        cg.a_load_reg_reg(current_asmdata.CurrAsmList, def_cgsize(opsize), OS_INT, hregister, indexreg);
        { a <= x <= b <-> unsigned(x-a) <= (b-a) }
        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,aint(min_),indexreg);
        if not(jumptable_no_range) then
          cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_A,aint(max_)-aint(min_),indexreg,elselabel);
        current_asmdata.getjumplabel(table);
        hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
        { la.pcrel x,tbl }
        reference_reset_symbol(href, table, 0, 4,[]);
        href.refaddr:=addr_pcrel;
        cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,hregister);
        { alsl.d y,idx,x,3 }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg_const(A_ALSL_D,hregister,indexreg,hregister,3));
        { ld.d z,y,0 }
        reference_reset_base(href,hregister,0,ctempposinvalid,4,[]);
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,hregister);
        { jr z }
        reference_reset_base(href,hregister,0,ctempposinvalid,4,[]);
        href.refaddr:=addr_reg;
        current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_JR,href));

        { generate jump table }
        current_asmdata.CurrAsmList.concat(cai_align.Create(8));
        current_asmdata.CurrAsmList.concat(Tai_label.Create(table));
        genitem(current_asmdata.CurrAsmList,hp);
      end;


begin
   ccasenode:=tloongarch64casenode;
end.
