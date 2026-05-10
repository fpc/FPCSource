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
       ncgset,
       compilerbase;

    type
       tcpucasenode = class(tcgcasenode)
         protected
           procedure optimizevalues(var max_linear_list:int64;var max_dist:qword);override;
           function has_jumptable : boolean;override;
           procedure genjumptable(hp : pcaselabel;min_,max_ : int64;ctx:tpassgeneratecodecontext);override;
       end;


  implementation

    uses
      globals,constexp,
      systems,
      cpubase,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cgbase,cgutils,cgobj,
      defutil,procinfo,
      pass_2_context,compiler,nodehelper;

    procedure tcpucasenode.optimizevalues(var max_linear_list:int64;var max_dist:qword);
      begin
        { give the jump table a higher priority }
        max_dist:=(max_dist*3) div 2;
      end;


    function tcpucasenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure tcpucasenode.genjumptable(hp : pcaselabel;min_,max_ : int64;ctx:tpassgeneratecodecontext);
      var
        base,
        table : tasmlabel;
        last : TConstExprInt;
        indexreg,jmpreg,basereg : tregister;
        href : treference;
        opcgsize : tcgsize;

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
                list.concat(Tai_const.Create_rel_sym(aitconst_ptr,base,elselabel));
                i:=i+1;
              end;
            i:=t^._low;
            while i<=t^._high do
              begin
                list.concat(Tai_const.Create_rel_sym(aitconst_ptr,base,blocklabel(t^.blockid)));
                i:=i+1;
              end;
            last:=t^._high;
            if assigned(t^.greater) then
              genitem(list,t^.greater);
          end;

      begin
        opcgsize:=def_cgsize(opsize);
        last:=min_;
        if not(jumptable_no_range) then
          begin
            { a <= x <= b <-> unsigned(x-a) <= (b-a) }
            ctx.cg.a_op_const_reg(ctx.CurrAsmList,OP_SUB,opcgsize,aint(min_),hregister);
            { case expr greater than max_ => goto elselabel }
            ctx.cg.a_cmp_const_reg_label(ctx.CurrAsmList,opcgsize,OC_A,aint(max_)-aint(min_),hregister,elselabel);
            min_:=0;
          end;
        current_asmdata.getjumplabel(table);
        indexreg:=ctx.cg.getaddressregister(ctx.CurrAsmList);
{$ifdef SPARC64}
        ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHL,OS_ADDR,3,hregister,indexreg);
{$else SPARC64}
        ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHL,OS_ADDR,2,hregister,indexreg);
{$endif SPARC64}
        { create reference }
        current_asmdata.getjumplabel(base);
        ctx.cg.a_label(ctx.CurrAsmList,base);
        reference_reset_symbol(href,table,(-aint(min_))*sizeof(pint),sizeof(pint),[]);
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
        ctx.cg.getcpuregister(ctx.CurrAsmList,NR_O7);
        ctx.CurrAsmList.concat(taicpu.op_sym_ofs(A_CALL,base,8));

        basereg:=ctx.cg.getaddressregister(ctx.CurrAsmList);
        { TODO: incorporate handling such references into ctx.cg.a_loadaddr_ref_reg? }
        href.refaddr:=addr_high;
        ctx.CurrAsmList.concat(taicpu.op_ref_reg(A_SETHI,href,basereg));
        href.refaddr:=addr_low;
        ctx.CurrAsmList.concat(taicpu.op_reg_ref_reg(A_OR,basereg,href,basereg));

        { add index }
        ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_ADD,OS_ADDR,basereg,indexreg,basereg);

        jmpreg:=ctx.cg.getaddressregister(ctx.CurrAsmList);
        reference_reset_base(href,NR_O7,0,ctempposinvalid,sizeof(pint),[]);
        href.index:=basereg;
        ctx.cg.a_load_ref_reg(ctx.CurrAsmList,OS_ADDR,OS_ADDR,href,jmpreg);
        href.index:=jmpreg;
        href.refaddr:=addr_full;
        ctx.CurrAsmList.concat(taicpu.op_ref(A_JMP,href));

        { Delay slot }
        ctx.CurrAsmList.concat(taicpu.op_none(A_NOP));
        ctx.cg.ungetcpuregister(ctx.CurrAsmList,NR_O7);

        { generate jump table }
        ctx.CurrAsmList.Concat(tai_align.Create(sizeof(pint)));
        ctx.cg.a_label(ctx.CurrAsmList,table);
        genitem(ctx.CurrAsmList,hp);
      end;



begin
  ccasenode:=tcpucasenode;
end.
