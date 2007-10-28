{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate arm assembler for in set/case nodes

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
unit narmset;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      node,nset,pass_1,ncgset;

    type
      tarmcasenode = class(tcgcasenode)
         procedure optimizevalues(var max_linear_list:aint;var max_dist:aword);override;
         function  has_jumptable : boolean;override;
         procedure genjumptable(hp : pcaselabel;min_,max_ : aint);override;
         procedure genlinearlist(hp : pcaselabel);override;
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
      cgutils,cgobj,ncgutil,
      cgcpu;


{*****************************************************************************
                            TARMCASENODE
*****************************************************************************}

    procedure tarmcasenode.optimizevalues(var max_linear_list:aint;var max_dist:aword);
      begin
        inc(max_linear_list,2)
      end;


    function tarmcasenode.has_jumptable : boolean;
      begin
        has_jumptable:=true;
      end;


    procedure tarmcasenode.genjumptable(hp : pcaselabel;min_,max_ : aint);
      var
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
            for i:=last.svalue+1 to t^._low.svalue-1 do
              list.concat(Tai_const.Create_sym(elselabel));
            for i:=t^._low.svalue to t^._high.svalue do
              list.concat(Tai_const.Create_sym(blocklabel(t^.blockid)));
            last:=t^._high.svalue;
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
        { make it a 32bit register }
        indexreg:=cg.makeregsize(current_asmdata.CurrAsmList,hregister,OS_INT);
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,OS_INT,hregister,indexreg);
        cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_ADDR,min_+1,indexreg,indexreg);
        { create reference }
        reference_reset(href);
        href.base:=NR_PC;
        href.index:=indexreg;
        href.shiftmode:=SM_LSL;
        href.shiftimm:=2;
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,NR_PC);
        { generate jump table }
        last:=min_;
        genitem(current_asmdata.CurrAsmList,hp);
      end;


    procedure tarmcasenode.genlinearlist(hp : pcaselabel);
      var
        first : boolean;
        lastrange : boolean;
        last : TConstExprInt;
        cond_lt,cond_le : tresflags;

        procedure genitem(t : pcaselabel);
          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { need we to test the first value }
             if first and (t^._low>get_min_value(left.resultdef)) then
               begin
                 cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,jmp_lt,aint(t^._low.svalue),hregister,elselabel);
               end;
             if t^._low=t^._high then
               begin
                  if t^._low-last=0 then
                    cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, OC_EQ,0,hregister,blocklabel(t^.blockid))
                  else
                    begin
                      tcgarm(cg).cgsetflags:=true;
                      cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, aint(int64(t^._low-last)), hregister);
                      tcgarm(cg).cgsetflags:=false;
                      cg.a_jmp_flags(current_asmdata.CurrAsmList,F_EQ,blocklabel(t^.blockid));
                    end;
                  last:=t^._low;
                  lastrange:=false;
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                }
                  if first then
                    begin
                       { have we to ajust the first value ? }
                       if (t^._low>get_min_value(left.resultdef)) or (get_min_value(left.resultdef)<>0) then
                         begin
                           tcgarm(cg).cgsetflags:=true;
                           cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, aint(int64(t^._low)), hregister);
                           tcgarm(cg).cgsetflags:=false;
                         end;
                    end
                  else
                    begin
                      { if there is no unused label between the last and the }
                      { present label then the lower limit can be checked    }
                      { immediately. else check the range in between:       }

                      tcgarm(cg).cgsetflags:=true;
                      cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, aint(int64(t^._low-last)), hregister);
                      tcgarm(cg).cgsetflags:=false;
                      { no jump necessary here if the new range starts at }
                      { at the value following the previous one           }
                      if ((t^._low-last) <> 1) or
                         (not lastrange) then
                        cg.a_jmp_flags(current_asmdata.CurrAsmList,cond_lt,elselabel);
                    end;
                  tcgarm(cg).cgsetflags:=true;
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,opsize,aint(int64(t^._high-t^._low)),hregister);
                  tcgarm(cg).cgsetflags:=false;
                  cg.a_jmp_flags(current_asmdata.CurrAsmList,cond_le,blocklabel(t^.blockid));

                  last:=t^._high;
                  lastrange:=true;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           if with_sign then
             begin
                cond_lt:=F_LT;
                cond_le:=F_LE;
             end
           else
              begin
                cond_lt:=F_CC;
                cond_le:=F_LS;
             end;
           { do we need to generate cmps? }
           if (with_sign and (min_label<0)) then
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
   ccasenode:=tarmcasenode;
end.
