{
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate PowerPC assembler for in set/case nodes

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
unit nppcset;

{$i fpcdefs.inc}

interface

    uses
       node,nset,ncgset,cpubase,cgbase,cgobj,aasmbase,aasmtai,globtype;

    type
       tppccasenode = class(tcgcasenode)
         protected
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


    function tppccasenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure tppccasenode.genjumptable(hp : pcaselabel;min_,max_ : aint);
      var
        table : tasmlabel;
        last : TConstExprInt;
        indexreg : tregister;
        href : treference;

        procedure genitem(list:taasmoutput;t : pcaselabel);
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
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_lt,aint(min_),hregister,elselabel);
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_gt,aint(max_),hregister,elselabel);
          end;
        objectlibrary.getjumplabel(table);
        { make it a 32bit register }
        // allocate base and index registers register
        indexreg:= cg.makeregsize(exprasmlist, hregister, OS_INT);
        { indexreg := hregister; }
        cg.a_load_reg_reg(exprasmlist, opsize, OS_INT, hregister, indexreg);
        { create reference, indexreg := indexreg * sizeof(OS_ADDR) }
        cg.a_op_const_reg(exprasmlist, OP_MUL, OS_INT, tcgsize2size[OS_ADDR], indexreg);
        reference_reset_symbol(href, table, (-aint(min_)) * tcgsize2size[OS_ADDR]);
        href.index := indexreg;

        cg.a_load_ref_reg(exprasmlist, OS_INT, OS_INT, href, indexreg);

        exprasmlist.concat(taicpu.op_reg(A_MTCTR, indexreg));
        exprasmlist.concat(taicpu.op_none(A_BCTR));

        { generate jump table }
        new_section(current_procinfo.aktlocaldata,sec_data,current_procinfo.procdef.mangledname,sizeof(aint));
        current_procinfo.aktlocaldata.concat(Tai_label.Create(table));
        last:=min_;
        genitem(current_procinfo.aktlocaldata,hp);
      end;


    procedure tppccasenode.genlinearlist(hp : pcaselabel);

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
              exprasmlist.concat(taicpu.op_reg_reg_const(A_ADDIC_,hregister,
                hregister,value))
            else
              begin
                tmpreg := cg.getintregister(exprasmlist,OS_INT);
                 cg.a_load_const_reg(exprasmlist,OS_INT,value,tmpreg);
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_ADD_,hregister,
                  hregister,tmpreg));
              end;
          end;

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           { need we to test the first value }
           if first and (t^._low>get_min_value(left.resulttype.def)) then
             begin
               cg.a_cmp_const_reg_label(exprasmlist,OS_INT,jmp_lt,aword(t^._low),hregister,elselabel);
             end;
           if t^._low=t^._high then
             begin
                if t^._low-last=0 then
                  cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_EQ,0,hregister,blocklabel(t^.blockid))
                else
                  gensub(longint(t^._low-last));
                tcgppc(cg).a_jmp_cond(exprasmlist,OC_EQ,blocklabel(t^.blockid));
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
                     if (t^._low>get_min_value(left.resulttype.def)) then
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
                      tcgppc(cg).a_jmp_cond(exprasmlist,jmp_lt,elselabel);
                  end;
                gensub(longint(t^._high-t^._low));
                tcgppc(cg).a_jmp_cond(exprasmlist,jmp_le,blocklabel(t^.blockid));
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
            (opsize = OS_32) then
           genlinearcmplist(hp)
         else
           begin
              last:=0;
              lastrange:=false;
              first:=true;
              genitem(hp);
              cg.a_jmp_always(exprasmlist,elselabel);
           end;
      end;


begin
   ccasenode:=tppccasenode;
end.
