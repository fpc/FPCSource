{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 64 bit int
    arithmethics on 32 bit processors

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

unit cg64f32;

  {$i defines.inc}

  interface

    uses
       aasm, cgobj, cpubase,node,symtype;

    type
      tcg64f32 = class(tcg)
        procedure a_load64_reg_ref(list : taasmoutput;reglo, reghi : tregister;const ref : treference);
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reglo,reghi : tregister);
        procedure a_load64_reg_reg(list : taasmoutput;reglosrc,reghisrc,reglodst,reghidst : tregister);
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reglo,reghi : tregister);
        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);

        { override to catch 64bit rangechecks }
        procedure g_rangecheck(list: taasmoutput; const p: tnode;
          const todef: tdef); override;

       private
         { produces range check code for 32bit processors when one of the }
         { operands is 64 bit                                             }
         procedure g_rangecheck64(list : taasmoutput; p : tnode;todef : tdef);

      end;

  implementation

    uses
       globtype,globals,systems,
       cgbase,
       verbose,
       symbase,symconst,symdef,types,
       cpuinfo;

    procedure tcg64f32.a_load64_reg_ref(list : taasmoutput;reglo, reghi : tregister;const ref : treference);
      var
        tmpreg: tregister;
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          begin
            tmpreg := reglo;
            reglo := reghi;
            reghi := tmpreg;
          end;
        a_load_reg_ref(list,OS_32,reglo,ref);
        tmpref := ref;
        inc(tmpref.offset,4);
        a_load_reg_ref(list,OS_32,reghi,tmpref);
      end;

    procedure tcg64f32.a_load64_ref_reg(list : taasmoutput;const ref : treference;reglo,reghi : tregister);
      var
        tmpreg: tregister;
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          begin
            tmpreg := reglo;
            reglo := reghi;
            reghi := tmpreg;
          end;
        a_load_ref_reg(list,OS_32,ref,reglo);
        tmpref := ref;
        inc(tmpref.offset,4);
        a_load_ref_reg(list,OS_32,tmpref,reghi);
      end;

    procedure tcg64f32.a_load64_reg_reg(list : taasmoutput;reglosrc,reghisrc,reglodst,reghidst : tregister);

      begin
        a_load_reg_reg(list,OS_32,reglosrc,reglodst);
        a_load_reg_reg(list,OS_32,reghisrc,reghidst);
      end;


    procedure tcg64f32.a_load64_loc_reg(list : taasmoutput;const l : tlocation;reglo,reghi : tregister);

      begin
        case l.loc of
          LOC_REFERENCE, LOC_MEM:
            a_load64_ref_reg(list,l.reference,reglo,reghi);
          LOC_REGISTER,LOC_CREGISTER:
            a_load64_reg_reg(list,l.registerlow,l.registerhigh,reglo,reghi);
          else
            internalerror(200112292);
        end;
      end;


    procedure tcg64f32.a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          a_load_reg_ref(list,OS_32,reg,ref)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            a_load_reg_ref(list,OS_32,reg,tmpref)
          end;
      end;

    procedure tcg64f32.a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_little then
          a_load_reg_ref(list,OS_32,reg,ref)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            a_load_reg_ref(list,OS_32,reg,tmpref)
          end;
      end;

    procedure tcg64f32.a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_big then
          a_load_ref_reg(list,OS_32,ref,reg)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            a_load_ref_reg(list,OS_32,tmpref,reg)
          end;
      end;

    procedure tcg64f32.a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);
      var
        tmpref: treference;
      begin
        if target_info.endian = endian_little then
          a_load_ref_reg(list,OS_32,ref,reg)
        else
          begin
            tmpref := ref;
            inc(tmpref.offset,4);
            a_load_ref_reg(list,OS_32,tmpref,reg)
          end;
      end;


    procedure tcg64f32.g_rangecheck(list: taasmoutput; const p: tnode;
        const todef: tdef);
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(todef.deftype in [orddef,enumdef,arraydef]) then
          exit;
        { special case for 64bit rangechecks }
        if is_64bitint(p.resulttype.def) or is_64bitint(todef) then
          g_rangecheck64(list,p,todef)
        else
          inherited g_rangecheck(list,p,todef);
      end;


    procedure tcg64f32.g_rangecheck64(list : taasmoutput; p : tnode;todef : tdef);

      var
        neglabel,
        poslabel,
        endlabel: tasmlabel;
        hreg   : tregister;
        hdef   :  torddef;
        fromdef : tdef;
        opsize   : tcgsize;
        oldregisterdef: boolean;
        from_signed,to_signed: boolean;
        got_scratch: boolean;

      begin
         fromdef:=p.resulttype.def;
         from_signed := is_signed(fromdef);
         to_signed := is_signed(todef);

         if not is_64bitint(todef) then
           begin
             oldregisterdef := registerdef;
             registerdef := false;

             { get the high dword in a register }
             if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
               begin
                 hreg := p.location.registerhigh;
                 got_scratch := false
               end
             else
               begin
                 hreg := get_scratch_reg(list);
                 got_scratch := true;
                 a_load64high_ref_reg(list,p.location.reference,hreg);
               end;
             getlabel(poslabel);

             { check high dword, must be 0 (for positive numbers) }
             a_cmp_const_reg_label(list,OS_32,OC_EQ,0,hreg,poslabel);

             { It can also be $ffffffff, but only for negative numbers }
             if from_signed and to_signed then
               begin
                 getlabel(neglabel);
                 a_cmp_const_reg_label(list,OS_32,OC_EQ,aword(-1),hreg,neglabel);
               end;
             { !!! freeing of register should happen directly after compare! (JM) }
             if got_scratch then
               free_scratch_reg(list,hreg);
             { For all other values we have a range check error }
             a_call_name(list,'FPC_RANGEERROR',0);

             { if the high dword = 0, the low dword can be considered a }
             { simple cardinal                                          }
             a_label(list,poslabel);
             hdef:=torddef.create(u32bit,0,longint($ffffffff));
             { the real p.resulttype.def is already saved in fromdef }
             p.resulttype.def := hdef;
             { no use in calling just "g_rangecheck" since that one will }
             { simply call the inherited method too (JM)                 }
             inherited g_rangecheck(list,p,todef);
             hdef.free;
             { restore original resulttype.def }
             p.resulttype.def := todef;

             if from_signed and to_signed then
               begin
                 getlabel(endlabel);
                 a_jmp_cond(list,OC_NONE,endlabel);
                 { if the high dword = $ffffffff, then the low dword (when }
                 { considered as a longint) must be < 0                    }
                 a_label(list,neglabel);
                 if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                   begin
                     hreg := p.location.registerlow;
                     got_scratch := false
                   end
                 else
                   begin
                     hreg := get_scratch_reg(list);
                     got_scratch := true;
                     a_load64low_ref_reg(list,p.location.reference,hreg);
                   end;
                 { get a new neglabel (JM) }
                 getlabel(neglabel);
                 a_cmp_const_reg_label(list,OS_32,OC_LT,0,hreg,neglabel);
                 { !!! freeing of register should happen directly after compare! (JM) }
                 if got_scratch then
                   free_scratch_reg(list,hreg);

                 a_call_name(list,'FPC_RANGEERROR',0);

                 { if we get here, the 64bit value lies between }
                 { longint($80000000) and -1 (JM)               }
                 a_label(list,neglabel);
                 hdef:=torddef.create(s32bit,longint($80000000),-1);
                 p.resulttype.def := hdef;
                 inherited g_rangecheck(list,p,todef);
                 hdef.free;
                 a_label(list,endlabel);
               end;
             registerdef := oldregisterdef;
             p.resulttype.def := fromdef;
             { restore p's resulttype.def }
           end
         else
           { todef = 64bit int }
           { no 64bit subranges supported, so only a small check is necessary }

           { if both are signed or both are unsigned, no problem! }
           if (from_signed xor to_signed) and
              { also not if the fromdef is unsigned and < 64bit, since that will }
              { always fit in a 64bit int (todef is 64bit)                       }
              (from_signed or
               (torddef(fromdef).typ = u64bit)) then
             begin
               { in all cases, there is only a problem if the higest bit is set }
               if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                 begin
                   if is_64bitint(fromdef) then
                     begin
                       hreg := p.location.registerhigh;
                       opsize := OS_32;
                     end
                   else
                     begin
                       hreg := p.location.register;
                       opsize := def_cgsize(p.resulttype.def);
                     end;
                   got_scratch := false;
                 end
               else
                 begin
                   hreg := get_scratch_reg(list);
                   got_scratch := true;

                   opsize := def_cgsize(p.resulttype.def);
                   if opsize in [OS_64,OS_S64] then
                     a_load64high_ref_reg(list,p.location.reference,hreg)
                   else
                     a_load_ref_reg(list,opsize,p.location.reference,hreg);
                 end;
               getlabel(poslabel);
               a_cmp_const_reg_label(list,opsize,OC_GTE,0,hreg,poslabel);

               { !!! freeing of register should happen directly after compare! (JM) }
               if got_scratch then
                 free_scratch_reg(list,hreg);
               a_call_name(list,'FPC_RANGEERROR',0);
               a_label(list,poslabel);
             end;
      end;

(*
    procedure int64f32_assignment_int64_reg(p : passignmentnode);

      begin
      end;


begin
   p2_assignment:=@int64f32_assignement_int64;
*)
end.
{
  $Log$
  Revision 1.2  2001-12-30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj, part in    cg64f32) and should work correctly again (it needed some changes after    the changes of the low and high of tordef's to int64)  * maketojumpbool() is now processor independent (in ncgutil)  * getregister32 is now called getregisterint

  Revision 1.1  2001/12/29 15:29:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.1  2000/07/13 06:30:07  michael
  + Initial import

  Revision 1.1  2000/03/01 15:36:13  florian
    * some new stuff for the new cg

}