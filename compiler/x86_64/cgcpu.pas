{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit implements the code generator for the x86-64.

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
{ This unit implements the code generator for the x86-64.
}
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       cginfo,cgbase,cgobj,cg64f64,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,cpupara,
       node,symconst;

    type
      tcgx86_64 = class(tcgx86)
        class function reg_cgsize(const reg: tregister): tcgsize; override;
        procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);override;
      end;

  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symdef,symsym,defutil,paramgr,
       rgobj,tgobj,rgcpu;


    class function tcgx86_64.reg_cgsize(const reg: tregister): tcgsize;
      const
        opsize_2_cgsize: array[topsize] of tcgsize = (OS_NO,
          OS_8,OS_16,OS_32,OS_NO,OS_NO,OS_NO,OS_NO,OS_NO,OS_NO,
          OS_32,OS_64,OS_64,
          OS_F32,OS_F64,OS_F80,OS_F32,OS_F64,OS_NO,OS_NO,
          OS_NO,OS_NO,OS_NO
        );
      begin
        result := opsize_2_cgsize[reg2opsize(reg)];
      end;


    procedure tcgx86_64.g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword; delsource,loadref : boolean);
      var
         ecxpushed : boolean;
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         srcref,dstref : treference;
         swap : boolean;

{!!!
         procedure maybepushecx;
         begin
           if not(R_ECX in rg.unusedregsint) then
             begin
               list.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ECX));
               ecxpushed:=true;
             end
           else rg.getexplicitregisterint(list,R_ECX);
         end;
}

      begin
{!!!
         if (not loadref) and
            ((len<=8) or
             (not(cs_littlesize in aktglobalswitches ) and (len<=12))) then
           begin
              helpsize:=len shr 3;
              rg.getexplicitregisterint(list,R_RDI);
              dstref:=dest;
              srcref:=source;
              for i:=1 to helpsize do
                begin
                   a_load_ref_reg(list,OS_64,srcref,R_RDI);
                   If (len=8) and delsource then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_64,R_RDI,dstref);
                   inc(srcref.offset,8);
                   inc(dstref.offset,8);
                   dec(len,8);
                end;
              if len>1 then
                begin
                   a_load_ref_reg(list,OS_16,srcref,R_EDI);
                   If (len =4) and delsource then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_16,R_EDI,dstref);
                   inc(srcref.offset,4);
                   inc(dstref.offset,4);
                   dec(len,4);
                end;
              if len>1 then
                begin
                   a_load_ref_reg(list,OS_16,srcref,R_DI);
                   If (len = 2) and delsource then
                     reference_release(list,source);
                   a_load_reg_ref(list,OS_16,R_DI,dstref);
                   inc(srcref.offset,2);
                   inc(dstref.offset,2);
                   dec(len,2);
                end;
              if len>0 then
                begin
                   a_load_ref_reg(list,OS_16,srcref,R_DIL);
                   a_load_reg_ref(list,OS_16,R_DIL,dstref);
                end;
              rg.ungetregisterint(list,R_RDI);
           end
         else
           begin
              rg.getexplicitregisterint(list,R_RDI);
              a_loadaddr_ref_reg(list,dest,R_RDI);
              list.concat(tai_regalloc.Alloc(R_RSI));
              if loadref then
                a_load_ref_reg(list,OS_ADDR,source,R_RSI)
              else
                begin
                  a_loadaddr_ref_reg(list,source,R_RSI);
                  if delsource then
                    reference_release(list,source);
                end;

              list.concat(Taicpu.Op_none(A_CLD,S_NO));
              ecxpushed:=false;
              if cs_littlesize in aktglobalswitches  then
                begin
                   maybepushecx;
                   a_load_const_reg(list,OS_INT,len,R_RCX);
                   list.concat(Taicpu.Op_none(A_REP,S_NO));
                   list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                end
              else
                begin
                   helpsize:=len shr 2;
                   len:=len and 3;
                   if helpsize>1 then
                    begin
                      maybepushecx;
                      a_load_const_reg(list,OS_INT,helpsize,R_RCX);
                      list.concat(Taicpu.Op_none(A_REP,S_NO));
                    end;
                   if helpsize>0 then
                    list.concat(Taicpu.Op_none(A_MOVSD,S_NO));
                   if len>1 then
                     begin
                        dec(len,2);
                        list.concat(Taicpu.Op_none(A_MOVSW,S_NO));
                     end;
                   if len=1 then
                     list.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                end;
              rg.ungetregisterint(list,R_RDI);
              list.concat(tai_regalloc.DeAlloc(R_RSI));
              if ecxpushed then
                list.concat(Taicpu.Op_reg(A_POP,S_L,R_RCX))
              else
                rg.ungetregisterint(list,R_RCX);

              { loading SELF-reference again }
              g_maybe_loadself(list);
           end;
         if delsource then
          tg.ungetiftemp(list,source);
}
      end;
begin
  cg:=tcgx86_64.create;
  cg64:=tcg64f64.create;
end.
{
  $Log$
  Revision 1.5  2003-09-25 13:13:32  florian
    * more x86-64 fixes

  Revision 1.4  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.3  2003/01/05 13:36:54  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.2  2002/07/25 22:55:33  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

}
