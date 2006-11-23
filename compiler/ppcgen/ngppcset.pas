{
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate PowerPC32/64 assembler for in set/case nodes

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
unit ngppcset;

{$i fpcdefs.inc}

interface

    uses
       node,nset,ncgset,cpubase,cgbase,cgobj,aasmbase,aasmtai,aasmdata,globtype;

    type
       tgppccasenode = class(tcgcasenode)
         protected
           procedure optimizevalues(var max_linear_list : aint; var max_dist : aword);override;
           function  has_jumptable : boolean;override;
           procedure genjumptable(hp : pcaselabel;min_,max_ : aint);override;
           procedure genlinearlist(hp : pcaselabel); override;
       end;


implementation

    uses
      systems,
      verbose,globals,
      symconst,symdef,defutil,
      paramgr,
      cpuinfo,
      pass_2,cgcpu,
      ncon,
      tgobj,ncgutil,regvars,rgobj,aasmcpu,
      procinfo,
      cgutils;

{*****************************************************************************
                            TCGCASENODE
*****************************************************************************}


    procedure tgppccasenode.optimizevalues(var max_linear_list : aint; var max_dist : aword);
    begin
      max_linear_list := 10;
    end;
    

    function tgppccasenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure tgppccasenode.genjumptable(hp : pcaselabel;min_,max_ : aint);
      var
        table : tasmlabel;
        last : TConstExprInt;
        indexreg : tregister;
        href : treference;

        procedure genitem(list:TAsmList;t : pcaselabel);
          var
            i : aint;
          begin
            if assigned(t^.less) then
              genitem(list,t^.less);
            { fill possible hole }
            i:=last+1;
            while i<=t^._low-1 do
              begin
                list.concat(Tai_const.Create_sym(elselabel));
                inc(i);
              end;
            i:=t^._low;
            while i<=t^._high do
              begin
                list.concat(Tai_const.Create_sym(blocklabel(t^.blockid)));
                inc(i);
              end;
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
        { make it a 32bit register }
        // allocate base and index registers register
        indexreg:= cg.makeregsize(current_asmdata.CurrAsmList, hregister, OS_INT);
        { indexreg := hregister; }
        cg.a_load_reg_reg(current_asmdata.CurrAsmList, opsize, OS_INT, hregister, indexreg);
        { create reference, indexreg := indexreg * sizeof(OS_ADDR) }
        cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_MUL, OS_INT, tcgsize2size[OS_ADDR], indexreg);
        reference_reset_symbol(href, table, (-aint(min_)) * tcgsize2size[OS_ADDR]);
        href.index := indexreg;

        cg.a_load_ref_reg(current_asmdata.CurrAsmList, OS_INT, OS_INT, href, indexreg);

        current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_MTCTR, indexreg));
        current_asmdata.CurrAsmList.concat(taicpu.op_none(A_BCTR));

        { generate jump table }
        new_section(current_procinfo.aktlocaldata,sec_rodata,current_procinfo.procdef.mangledname,sizeof(aint));
        current_procinfo.aktlocaldata.concat(Tai_label.Create(table));
        last:=min_;
        genitem(current_procinfo.aktlocaldata,hp);
      end;


    procedure tgppccasenode.genlinearlist(hp : pcaselabel);

      var
         first, lastrange : boolean;
         last : TConstExprInt;

      procedure genitem(t : pcaselabel);

          procedure gensub(value:longint);
          var
            tmpreg: tregister;
          begin
            value := -value;
            if (value >= low(smallint)) and
               (value <= high(smallint)) then
              current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_ADDIC_,hregister,
                hregister,value))
            else
              begin
                tmpreg := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                 cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,value,tmpreg);
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_ADD_,hregister,
                  hregister,tmpreg));
              end;
          end;

        begin
           if (get_min_value(left.resultdef) >= low(smallint)) and
              (get_max_value(left.resultdef) <= high(word)) then
             begin
               genlinearcmplist(hp);
               exit;
             end;
           if assigned(t^.less) then
             genitem(t^.less);
           { need we to test the first value }
           if first and (t^._low>get_min_value(left.resultdef)) then
             begin
               cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_INT,jmp_lt,aword(t^._low),hregister,elselabel);
             end;
           if t^._low=t^._high then
             begin
                if t^._low-last=0 then
                  cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, OC_EQ,0,hregister,blocklabel(t^.blockid))
                else
                  gensub(longint(t^._low-last));
                tcgppc(cg).a_jmp_cond(current_asmdata.CurrAsmList,OC_EQ,blocklabel(t^.blockid));
                last:=t^._low;
                lastrange := false;
             end
           else
             begin
                { it begins with the smallest label, if the value }
                { is even smaller then jump immediately to the    }
                { ELSE-label                                }
                if first then
                  begin
                     { have we to ajust the first value ? }
                     if (t^._low>get_min_value(left.resultdef)) then
                       gensub(longint(t^._low));
                  end
                else
                  begin
                    { if there is no unused label between the last and the }
                    { present label then the lower limit can be checked    }
                    { immediately. else check the range in between:       }
                    gensub(longint(t^._low-last));
                    if ((t^._low-last) <> 1) or
                       (not lastrange) then
                      tcgppc(cg).a_jmp_cond(current_asmdata.CurrAsmList,jmp_lt,elselabel);
                  end;
                gensub(longint(t^._high-t^._low));
                tcgppc(cg).a_jmp_cond(current_asmdata.CurrAsmList,jmp_le,blocklabel(t^.blockid));
                last:=t^._high;
                lastrange := true;
             end;
           first:=false;
           if assigned(t^.greater) then
             genitem(t^.greater);
        end;

      begin
         { do we need to generate cmps? }
         if (with_sign and (min_label<0)) or
            (opsize in [OS_32,OS_64,OS_S64]) then
           genlinearcmplist(hp)
         else
           begin
              last:=0;
              lastrange:=false;
              first:=true;
              genitem(hp);
              cg.a_jmp_always(current_asmdata.CurrAsmList,elselabel);
           end;
      end;


begin
   ccasenode:=tgppccasenode;
end.
