{
    Copyright (c) 2016 by the Free Pascal development team

    Generate m68k assembler for in set/case labels

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
unit n68kset;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symtype,
      cgbase,cpuinfo,cpubase,
      node,nset,ncgset;

    type
      tcpucasenode = class(tcgcasenode)
        procedure genlinearlist(hp : pcaselabel); override;
      end;

implementation

    uses
      systems,globals,
      cutils,verbose,
      symdef,paramgr,
      aasmtai,aasmdata,
      nflw,constexp,
      cgutils,cgobj,hlcgobj,
      defutil;

    procedure tcpucasenode.genlinearlist(hp : pcaselabel);

      var
         first : boolean;
         last : TConstExprInt;
         scratch_reg: tregister;
         newsize: tcgsize;
         newdef: tdef;

      procedure genitem(t : pcaselabel);

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           { do we need to test the first value? }
           if first and (t^._low>get_min_value(left.resultdef)) then
             hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,jmp_lt,tcgint(t^._low.svalue),hregister,elselabel);
           if t^._low=t^._high then
             begin
               if t^._low-last=0 then
                 hlcg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,OC_EQ,0,hregister,blocklabel(t^.blockid))
               else
                 begin
                   hlcg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, tcgint(t^._low.svalue-last.svalue), hregister);
                   hlcg.a_jmp_flags(current_asmdata.CurrAsmList,F_E,blocklabel(t^.blockid));
                 end;
               last:=t^._low;
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
                       hlcg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, tcgint(t^._low.svalue), hregister);
                  end
                else
                  begin
                    { if there is no unused label between the last and the }
                    { present label then the lower limit can be checked    }
                    { immediately. else check the range in between:       }
                    hlcg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, tcgint(t^._low.svalue-last.svalue), hregister);
                    hlcg.a_jmp_flags(current_asmdata.CurrAsmList,F_L,elselabel);
                  end;
                hlcg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, tcgint(t^._high.svalue-t^._low.svalue), hregister);
                hlcg.a_jmp_flags(current_asmdata.CurrAsmList,F_LE,blocklabel(t^.blockid));
                last:=t^._high;
             end;
           first:=false;
           if assigned(t^.greater) then
             genitem(t^.greater);
        end;

      begin
         { do we need to generate cmps? }
         if (with_sign and (min_label<0)) then
           genlinearcmplist(hp)
         else
           begin
              { sign/zero extend the value to a full register before starting to
                subtract values, so that on platforms that don't have
                subregisters of the same size as the value we don't generate
                sign/zero-extensions after every subtraction

                make newsize always signed, since we only do this if the size in
                bytes of the register is larger than the original opsize, so
                the value can always be represented by a larger signed type }
              newsize:=tcgsize2signed[reg_cgsize(hregister)];
              if tcgsize2size[newsize]>opsize.size then
                begin
                  newdef:=cgsize_orddef(newsize);
                  scratch_reg:=hlcg.getintregister(current_asmdata.CurrAsmList,newdef);
                  hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,newdef,hregister,scratch_reg);
                  hregister:=scratch_reg;
                  opsize:=newdef;
                end;
              last:=0;
              first:=true;
              genitem(hp);
              hlcg.a_jmp_always(current_asmdata.CurrAsmList,elselabel);
           end;
      end;

begin
  ccasenode:=tcpucasenode;
end.
