{
    Copyright (c) 2015 by Jonas Maebe

    Generate AArch64 assembler for in set/case nodes

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
       taarch64casenode = class(tcgcasenode)
         protected
           procedure optimizevalues(var max_linear_list: int64; var max_dist: qword);override;
           function  has_jumptable: boolean;override;
           procedure genjumptable(hp: pcaselabel ;min_, max_: int64);override;
           procedure genlinearlist(hp: pcaselabel);override;
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


    procedure taarch64casenode.optimizevalues(var max_linear_list: int64; var max_dist: qword);
      begin
        max_linear_list:=10;
      end;
    

    function taarch64casenode.has_jumptable: boolean;
      begin
        has_jumptable:=true;
      end;


    procedure taarch64casenode.genlinearlist(hp : pcaselabel);
      var
        first : boolean;
        lastrange : boolean;
        last : TConstExprInt;
        cond_lt,cond_le : tresflags;
        opcgsize, unsigned_opcgsize: tcgsize;

        procedure genitem(t : pcaselabel);
          var
           ovloc: tlocation;
          begin
            if assigned(t^.less) then
              genitem(t^.less);
            { need we to test the first value }
            if first and (t^._low>get_min_value(left.resultdef)) then
              begin
                cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opcgsize,jmp_lt,aint(t^._low.svalue),hregister,elselabel);
              end;
            if t^._low=t^._high then
              begin
                 if t^._low-last=0 then
                   cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opcgsize, OC_EQ,0,hregister,blocklabel(t^.blockid))
                 else
                   begin
                     { use unsigned_opcgsize here to avoid uncessary sign extensions, at this place hregister will never be negative, because
                       then genlinearlist wouldn't be used }
                     cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList, OP_SUB, unsigned_opcgsize, aint(t^._low.svalue-last.svalue), hregister, hregister,
                       true,ovloc);
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
                          { use unsigned_opcgsize here to avoid uncessary sign extensions, at this place hregister will never be negative, because
                            then genlinearlist wouldn't be use }
                          cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList, OP_SUB, unsigned_opcgsize, aint(t^._low.svalue), hregister, hregister,
                            true,ovloc);
                        end;
                   end
                 else
                   begin
                     { if there is no unused label between the last and the }
                     { present label then the lower limit can be checked    }
                     { immediately. else check the range in between:       }

                     { use unsigned_opcgsize here to avoid uncessary sign extensions, at this place hregister will never be negative, because
                       then genlinearlist wouldn't be use }
                     cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList, OP_SUB, unsigned_opcgsize, aint(t^._low.svalue - last.svalue), hregister, hregister,
                       true,ovloc);
                     { no jump necessary here if the new range starts at }
                     { at the value following the previous one           }
                     if (aint(t^._low.svalue - last.svalue) <> 1) or
                        (not lastrange) then
                       cg.a_jmp_flags(current_asmdata.CurrAsmList,cond_lt,elselabel);
                   end;
                 { use unsigned_opcgsize here to avoid uncessary sign extensions, at this place hregister will never be negative, because
                   then genlinearlist wouldn't be use }
                 cg.a_op_const_reg_reg_checkoverflow(current_asmdata.CurrAsmList,OP_SUB,unsigned_opcgsize,aint(t^._high.svalue - t^._low.svalue), hregister, hregister,
                   true,ovloc);
                 cg.a_jmp_flags(current_asmdata.CurrAsmList,cond_le,blocklabel(t^.blockid));

                 last:=t^._high;
                 lastrange:=true;
              end;
            first:=false;
            if assigned(t^.greater) then
              genitem(t^.greater);
          end;

        begin
           opcgsize:=def_cgsize(opsize);
           case opcgsize of
             OS_8,OS_16,OS_32,OS_S8,OS_S16,OS_S32:
               unsigned_opcgsize:=OS_32;
             OS_64,OS_S64:
               unsigned_opcgsize:=OS_64;
             else
               Internalerror(2019090902);
           end;
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


    procedure taarch64casenode.genjumptable(hp: pcaselabel; min_, max_: int64);
      var
        last: TConstExprInt;
        tablelabel: TAsmLabel;
        basereg,indexreg,jumpreg: TRegister;
        href: TReference;
        opcgsize: tcgsize;
        sectype: TAsmSectiontype;
        jtitemconsttype: taiconst_type;

      procedure genitem(list:TAsmList;t : pcaselabel);
        var
          i : int64;
        begin
          if assigned(t^.less) then
            genitem(list,t^.less);
          { fill possible hole }
          i:=last.svalue+1;
          while i<=t^._low.svalue-1 do
            begin
              list.concat(Tai_const.Create_rel_sym(jtitemconsttype,tablelabel,elselabel));
              inc(i);
            end;
          i:=t^._low.svalue;
          while i<=t^._high.svalue do
            begin
              list.concat(Tai_const.Create_rel_sym(jtitemconsttype,tablelabel,blocklabel(t^.blockid)));
              inc(i);
            end;
          last:=t^._high;
          if assigned(t^.greater) then
            genitem(list,t^.greater);
        end;

      begin
        if not(target_info.system in systems_darwin) then
          jtitemconsttype:=aitconst_32bit
        else
          { see https://gmplib.org/list-archives/gmp-bugs/2012-December/002836.html }
          jtitemconsttype:=aitconst_darwin_dwarf_delta32;

        last:=min_;
        opcgsize:=def_cgsize(opsize);
        { a <= x <= b <-> unsigned(x-a) <= (b-a) }
        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,opcgsize,aint(min_),hregister);
        if not(jumptable_no_range) then
          begin
             { case expr greater than max_ => goto elselabel }
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opcgsize,OC_A,aint(max_)-aint(min_),hregister,elselabel);
             min_:=0;
          end;
        { local label in order to avoid using GOT }
        current_asmdata.getlabel(tablelabel,alt_data);
        indexreg:=cg.makeregsize(current_asmdata.CurrAsmList,hregister,OS_ADDR);
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,opcgsize,OS_ADDR,hregister,indexreg);
        { load table address }
        reference_reset_symbol(href,tablelabel,0,4,[]);
        basereg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,basereg);
        { load table slot, 32-bit sign extended }
        reference_reset_base(href,basereg,0,href.temppos,4,[]);
        href.index:=indexreg;
        href.shiftmode:=SM_LSL;
        href.shiftimm:=2;
        jumpreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_S32,OS_ADDR,href,jumpreg);
        { add table address }
        cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_ADDR,basereg,jumpreg);
        { and finally jump }
        current_asmdata.CurrAsmList.concat(taicpu.op_reg(A_BR,jumpreg));
        { generate jump table }
        if not(target_info.system in systems_darwin) then
          sectype:=sec_rodata
        else
          begin
            { on Mac OS X, dead code stripping ("smart linking") happens based on
              global symbols: every global/static symbol (symbols that do not
              start with "L") marks the start of a new "subsection" that is
              discarded by the linker if there are no references to this symbol.
              This means that if you put the jump table in the rodata section, it
              will become part of the block of data associated with the previous
              non-L-label in the rodata section and stay or be thrown away
              depending on whether that block of data is referenced. Therefore,
              jump tables must be added in the code section and since aktlocaldata
              is inserted right after the routine, it will become part of the
              same subsection that contains the routine's code }
            sectype:=sec_code;
          end;
        new_section(current_procinfo.aktlocaldata,sectype,current_procinfo.procdef.mangledname,4);
        if target_info.system in systems_darwin then
          begin
            { additionally, these tables are now marked via ".data_region jt32"
              and ".end_data_region" }
            current_procinfo.aktlocaldata.concat(tai_directive.Create(asd_data_region,'jt32'));
          end;
        current_procinfo.aktlocaldata.concat(Tai_label.Create(tablelabel));
        genitem(current_procinfo.aktlocaldata,hp);
        if target_info.system in systems_darwin then
          current_procinfo.aktlocaldata.concat(tai_directive.Create(asd_end_data_region,''));
      end;


begin
   ccasenode:=taarch64casenode;
end.
