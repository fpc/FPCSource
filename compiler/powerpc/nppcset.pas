{
    $Id$
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
       node,nset,ncgset,cpubase,cginfo,cgbase,cgobj,aasmbase,aasmtai;

    type

       tppccasenode = class(tcgcasenode)
         protected
           procedure genlinearlist(hp : pcaserecord); override;
       end;


implementation

    uses
      globtype,systems,
      verbose,globals,
      symconst,symdef,defbase,
      paramgr,
      cpuinfo,
      pass_2,cgcpu,
      ncon,
      cga,tgobj,ncgutil,regvars,rgobj,aasmcpu;



{*****************************************************************************
                            TCGCASENODE
*****************************************************************************}


    procedure tppccasenode.genlinearlist(hp : pcaserecord);

      var
         first : boolean;
         last : TConstExprInt;
         resflags: tresflags;

      procedure genitem(t : pcaserecord);

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
                tmpreg := cg.get_scratch_reg_int(exprasmlist);
                cg.a_load_const_reg(exprasmlist,OS_INT,aword(value),tmpreg);
                exprasmlist.concat(taicpu.op_reg_reg_reg(A_ADD_,hregister,
                  hregister,tmpreg));
                cg.free_scratch_reg(exprasmlist,tmpreg);
              end;
          end;

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           { need we to test the first value }
           if first and (t^._low>get_min_value(left.resulttype.def)) then
             begin
               cg.a_cmp_const_reg_label(exprasmlist,OS_INT,jmp_lt,
                 aword(t^._low),hregister,elselabel);
             end;
           if t^._low=t^._high then
             begin
                if t^._low-last=0 then
                  exprasmlist.concat(taicpu.op_reg_reg_const(A_CMPWI,R_CR0,
                    hregister,0))
                else
                  gensub(longint(t^._low-last));
                last:=t^._low;
                resflags.cr := R_CR0;
                resflags.flag := F_EQ;
                cg.a_jmp_flags(exprasmlist,resflags,t^.statement);
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
                    if (t^._low-last) <> 1 then
                      tcgppc(cg).a_jmp_cond(exprasmlist,jmp_lt,elselabel);
                  end;
                gensub(longint(t^._high-t^._low));
                tcgppc(cg).a_jmp_cond(exprasmlist,jmp_le,t^.statement);
                last:=t^._high;
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
              first:=true;
              genitem(hp);
              cg.a_jmp_always(exprasmlist,elselabel);
           end;
      end;




begin
   ccasenode:=tppccasenode;
end.
{
  $Log$
  Revision 1.4  2002-10-21 18:08:05  jonas
    * some range errors fixed

  Revision 1.3  2002/09/09 13:57:45  jonas
    * small optimization to case genlist() case statements

  Revision 1.2  2002/09/08 20:14:33  jonas
    * use genlinearcmplist() for unsigned 32bit case statements instead
      of genlinearlist(), because the addic. instruction always sets the
      flags as if the arguments are signed 32bits (for smaller unsigned
      types, this doesn't matter since they fit in s32bit)

  Revision 1.1  2002/08/11 11:39:12  jonas
    + powerpc-specific genlinearlist

  Revision 1.13  2002/08/11 06:14:40  florian
    * fixed powerpc compilation problems

  Revision 1.12  2002/08/10 17:15:12  jonas
    * optimizations and bugfix

  Revision 1.11  2002/07/28 09:24:18  carl
  + generic case node

  Revision 1.10  2002/07/23 14:31:00  daniel
  * Added internal error when asked to generate code for 'if expr in []'

  Revision 1.9  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.8  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.7  2002/07/21 16:58:20  jonas
    * fixed some bugs in tcginnode.pass_2() and optimized the bit test

  Revision 1.6  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.5  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.4  2002/07/07 10:16:29  florian
    * problems with last commit fixed

  Revision 1.3  2002/07/06 20:19:25  carl
  + generic set handling

  Revision 1.2  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.1  2002/06/16 08:14:56  carl
  + generic sets

}
