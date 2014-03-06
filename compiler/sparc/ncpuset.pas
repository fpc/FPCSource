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
       node,
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
      globals,constexp,
      systems,
      cpubase,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cgbase,cgutils,cgobj,
      defutil,procinfo;

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
        base,
        table : tasmlabel;
        last : TConstExprInt;
        indexreg,jmpreg,basereg : tregister;
        href : treference;
        opcgsize : tcgsize;

        procedure genitem(list:TAsmList;t : pcaselabel);
          var
            i : aint;
          begin
            if assigned(t^.less) then
              genitem(list,t^.less);
            { fill possible hole }
            for i:=last.svalue+1 to t^._low.svalue-1 do
              list.concat(Tai_const.Create_rel_sym(aitconst_ptr,base,elselabel));
            for i:=t^._low.svalue to t^._high.svalue do
              list.concat(Tai_const.Create_rel_sym(aitconst_ptr,base,blocklabel(t^.blockid)));
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(list,t^.greater);
          end;

      begin
        opcgsize:=def_cgsize(opsize);
        if not(jumptable_no_range) then
          begin
             { case expr less than min_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opcgsize,jmp_lt,aint(min_),hregister,elselabel);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opcgsize,jmp_gt,aint(max_),hregister,elselabel);
          end;
        current_asmdata.getjumplabel(table);
        indexreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_ADDR,2,hregister,indexreg);
        { create reference }
        current_asmdata.getjumplabel(base);
        cg.a_label(current_asmdata.CurrAsmList,base);
        reference_reset_symbol(href,table,(-aint(min_))*4,sizeof(pint));
        href.relsymbol:=base;
        { Generate the following code:
          .Lbase:
              call  .+8                           # mov   %pc,%o7
              sethi %hi(.LTable-.Lbase),%basereg
              or    %basereg,%lo(.LTable-.Lbase),%basereg
              add   %indexreg,%basereg%,%basereg
              ld    [%o7+%basereg],%jmpreg
              jmp   %o7+%jmpreg                              }
        { CALL overwrites %o7, tell reg.allocator about that }
        cg.getcpuregister(current_asmdata.CurrAsmList,NR_O7);
        current_asmdata.CurrAsmList.concat(taicpu.op_sym_ofs(A_CALL,base,8));

        basereg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        { TODO: incorporate handling such references into cg.a_loadaddr_ref_reg? }
        href.refaddr:=addr_high;
        current_asmdata.CurrAsmList.concat(taicpu.op_ref_reg(A_SETHI,href,basereg));
        href.refaddr:=addr_low;
        current_asmdata.CurrAsmList.concat(taicpu.op_reg_ref_reg(A_OR,basereg,href,basereg));
        { add index }
        cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_ADDR,basereg,indexreg,basereg);

        jmpreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        reference_reset_base(href,NR_O7,0,sizeof(pint));
        href.index:=basereg;
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,jmpreg);
        href.index:=jmpreg;
        href.refaddr:=addr_full;
        current_asmdata.CurrAsmList.concat(taicpu.op_ref(A_JMP,href));
        { Delay slot }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(A_NOP));
        cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_O7);
        { generate jump table }
        last:=min_;
        cg.a_label(current_asmdata.CurrAsmList,table);
        genitem(current_asmdata.CurrAsmList,hp);
      end;



begin
  ccasenode:=tcpucasenode;
end.
