{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    PowerPC specific calling conventions

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
{ PowerPC specific calling conventions are handled by this unit
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       cpubase,
       symconst,symbase,symtype,symdef,paramgr;

    type
       tppcparamanager = class(tparamanager)
          function getintparaloc(nr : longint) : tparalocation;override;
          procedure create_param_loc_info(p : tabstractprocdef);override;
          function getfuncretparaloc(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       verbose,
       globtype,
       cpuinfo,cginfo,cgbase,
       defutil;

    function tppcparamanager.getintparaloc(nr : longint) : tparalocation;

      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else if nr<=8 then
           begin
              result.loc:=LOC_REGISTER;
              result.register.enum:=Toldregister(longint(R_2)+nr);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index.enum:=stack_pointer_reg;
              result.reference.offset:=(nr-8)*4;
           end;
      end;

    function getparaloc(p : tdef) : tloc;

      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.deftype of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              getparaloc:=LOC_FPUREGISTER;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            formaldef:
              getparaloc:=LOC_REGISTER;
            classrefdef:
              getparaloc:=LOC_REGISTER;
            recorddef:
              getparaloc:=LOC_REFERENCE;
            objectdef:
              if is_object(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            procvardef:
              if (po_methodpointer in tprocvardef(p).procoptions) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            filedef:
              getparaloc:=LOC_REGISTER;
            arraydef:
              getparaloc:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                getparaloc:=LOC_REGISTER
              else
                getparaloc:=LOC_REFERENCE;
            variantdef:
              getparaloc:=LOC_REFERENCE;
            { avoid problems with errornous definitions }
            errordef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;

    procedure tppcparamanager.create_param_loc_info(p : tabstractprocdef);

      var
         nextintreg,nextfloatreg,nextmmreg : tregister;
         stack_offset : aword;
         hp : tparaitem;
         loc : tloc;
         is_64bit: boolean;

      procedure assignintreg;

        begin
           if nextintreg.enum<=R_10 then
             begin
                hp.paraloc.loc:=LOC_REGISTER;
                hp.paraloc.register:=nextintreg;
                inc(nextintreg.enum);
             end
           else
              begin
                 hp.paraloc.loc:=LOC_REFERENCE;
                 hp.paraloc.reference.index.enum:=stack_pointer_reg;
                 hp.paraloc.reference.offset:=stack_offset;
                 inc(stack_offset,4);
             end;
        end;

      begin
         nextintreg.enum:=R_3;
         nextfloatreg.enum:=R_F1;
         nextmmreg.enum:=R_M1;
         stack_offset:=0;
         { pointer for structured results ? }
         if not is_void(p.rettype.def) then
           begin
              if not(ret_in_reg(p.rettype.def,p.proccalloption)) then
                inc(nextintreg.enum);
           end;

         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         hp:=tparaitem(p.para.last);
         while assigned(hp) do
           begin
              loc:=getparaloc(hp.paratype.def);
              hp.paraloc.sp_fixup:=0;
              case loc of
                 LOC_REGISTER:
                   begin
                      hp.paraloc.size := def_cgsize(hp.paratype.def);
                      { for things like formaldef }
                      if hp.paraloc.size = OS_NO then
                        hp.paraloc.size := OS_ADDR;
                      is_64bit := hp.paraloc.size in [OS_64,OS_S64];
                      if nextintreg.enum<=Toldregister(ord(R_10)-ord(is_64bit))  then
                        begin
                           hp.paraloc.loc:=LOC_REGISTER;
                           hp.paraloc.registerlow:=nextintreg;
                           inc(nextintreg.enum);
                           if is_64bit then
                             begin
                               hp.paraloc.registerhigh:=nextintreg;
                               inc(nextintreg.enum);
                             end;
                        end
                      else
                         begin
                            nextintreg.enum := R_11;
                            hp.paraloc.loc:=LOC_REFERENCE;
                            hp.paraloc.reference.index.enum:=stack_pointer_reg;
                            hp.paraloc.reference.offset:=stack_offset;
                            if not is_64bit then
                              inc(stack_offset,4)
                            else
                              inc(stack_offset,8);
                        end;
                   end;
                 LOC_FPUREGISTER:
                   begin
                      if hp.paratyp in [vs_var,vs_out] then
                        begin
                            if nextintreg.enum<=R_10 then
                             begin
                                hp.paraloc.size:=OS_ADDR;
                                hp.paraloc.loc:=LOC_REGISTER;
                                hp.paraloc.register:=nextintreg;
                                inc(nextintreg.enum);
                             end
                           else
                              begin
                                 {!!!!!!!}
                                 hp.paraloc.size:=def_cgsize(hp.paratype.def);
                                 internalerror(2002071006);
                             end;
                        end
                      else if nextfloatreg.enum<=R_F10 then
                        begin
                           hp.paraloc.size:=def_cgsize(hp.paratype.def);
                           hp.paraloc.loc:=LOC_FPUREGISTER;
                           hp.paraloc.register:=nextfloatreg;
                           inc(nextfloatreg.enum);
                        end
                      else
                         begin
                            {!!!!!!!}
                             hp.paraloc.size:=def_cgsize(hp.paratype.def);
                            internalerror(2002071004);
                        end;
                   end;
                 LOC_REFERENCE:
                   begin
                      hp.paraloc.size:=OS_ADDR;
                      if push_addr_param(hp.paratype.def,p.proccalloption) or (hp.paratyp in [vs_var,vs_out]) then
                        assignintreg
                      else
                        begin
                           hp.paraloc.loc:=LOC_REFERENCE;
                           hp.paraloc.reference.index.enum:=stack_pointer_reg;
                           hp.paraloc.reference.offset:=stack_offset;
                           inc(stack_offset,hp.paratype.def.size);
                        end;
                   end;
                 else
                   internalerror(2002071002);
              end;
              hp:=tparaitem(hp.previous);
           end;
      end;

    function tppcparamanager.getfuncretparaloc(p : tabstractprocdef) : tparalocation;
      begin
         case p.rettype.def.deftype of
            orddef,
            enumdef:
              begin
                getfuncretparaloc.loc:=LOC_REGISTER;
                getfuncretparaloc.register.enum:=R_3;
                getfuncretparaloc.size:=def_cgsize(p.rettype.def);
                if getfuncretparaloc.size in [OS_S64,OS_64] then
                  getfuncretparaloc.registerhigh.enum:=R_4;
              end;
            floatdef:
              begin
                getfuncretparaloc.loc:=LOC_FPUREGISTER;
                getfuncretparaloc.register.enum:=R_F1;
                getfuncretparaloc.size:=def_cgsize(p.rettype.def);
              end;
            { smallsets are OS_INT in R3, others are OS_ADDR in R3 -> the same }
            { ugly, I know :) (JM)                                             }
            setdef,
            variantdef,
            pointerdef,
            formaldef,
            classrefdef,
            recorddef,
            objectdef,
            stringdef,
            procvardef,
            filedef,
            arraydef,
            errordef:
              begin
                getfuncretparaloc.loc:=LOC_REGISTER;
                getfuncretparaloc.register.enum:=R_3;
                getfuncretparaloc.size:=OS_ADDR;
              end;
            else
              internalerror(2002090903);
        end;
      end;


begin
   paramanager:=tppcparamanager.create;
end.
{
  $Log$
  Revision 1.20  2003-01-09 11:22:14  olle
    * made powerpc compiler compile after Daniels Tregister modification

  Revision 1.19  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.18  2002/12/15 19:22:01  florian
    * fixed some crashes and a rte 201

  Revision 1.17  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.16  2002/11/18 17:32:01  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.15  2002/10/02 13:33:36  jonas
    + set, variant support in getfuncretparaloc

  Revision 1.14  2002/09/28 21:27:16  florian
    + getparaloc supports now sets and variants

  Revision 1.13  2002/09/10 21:28:05  jonas
    * int64 paras are now handled correctly (until the registers are used up
      anyway :)
    * the return location is now initialized correctly
    * fixed bug where ret_in_reg() was called for the procdefinition instead
      of for the result of the procedure

  Revision 1.12  2002/09/09 09:11:37  florian
    - removed passes_parameters_in_reg

  Revision 1.11  2002/09/07 17:54:59  florian
    * first part of PowerPC fixes

  Revision 1.10  2002/09/01 21:04:49  florian
    * several powerpc related stuff fixed

  Revision 1.9  2002/08/31 12:43:31  florian
    * ppc compilation fixed

  Revision 1.8  2002/08/18 10:42:38  florian
    * remaining assembler writer bugs fixed, the errors in the
      system unit are inline assembler problems

  Revision 1.7  2002/08/17 22:09:47  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.6  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.5  2002/07/30 20:50:44  florian
    * the code generator knows now if parameters are in registers

  Revision 1.4  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.3  2002/07/26 22:22:10  florian
    * several PowerPC related fixes to get forward with system unit compilation

  Revision 1.2  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled
}
