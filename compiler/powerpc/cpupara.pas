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
       symconst,symbase,symdef,paramgr;

    type
       tppcparamanager = class(tparamanager)
          function getintparaloc(nr : longint) : tparalocation;override;
          procedure create_param_loc_info(p : tabstractprocdef);override;
          function getfuncretparaloc(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       verbose,
       cpuinfo,cginfo,
       symtype,defbase;

    function tppcparamanager.getintparaloc(nr : longint) : tparalocation;

      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else if nr<=8 then
           begin
              result.loc:=LOC_REGISTER;
              result.register:=tregister(longint(R_2)+nr);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index:=stack_pointer_reg;
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

      begin
         nextintreg:=R_3;
         nextfloatreg:=R_F1;
         nextmmreg:=R_M1;
         stack_offset:=0;
         { pointer for structured results ? }
         { !!!nextintreg:=R_4;              }

         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         hp:=tparaitem(p.para.last);
         while assigned(hp) do
           begin
              loc:=getparaloc(hp.paratype.def);
              case loc of
                 LOC_REGISTER:
                   begin
                      hp.paraloc.size:=OS_32;
                      if nextintreg<=R_8 then
                        begin
                           hp.paraloc.loc:=LOC_REGISTER;
                           hp.paraloc.register:=nextintreg;
                           inc(nextintreg);
                        end
                      else
                         begin
                            hp.paraloc.loc:=LOC_REFERENCE;
                            hp.paraloc.reference.index:=stack_pointer_reg;
                            hp.paraloc.reference.offset:=stack_offset;
                            inc(stack_offset,4);
                        end;
                   end;
                 LOC_FPUREGISTER:
                   begin
                      hp.paraloc.size:=OS_F32;
                      if hp.paratyp in [vs_var,vs_out] then
                        begin
                           if nextintreg<=R_8 then
                             begin
                                hp.paraloc.loc:=LOC_REGISTER;
                                hp.paraloc.register:=nextintreg;
                                inc(nextintreg);
                             end
                           else
                              begin
                                 {!!!!!!!}
                                 internalerror(2002071006);
                             end;
                        end
                      else if nextfloatreg<=R_F8 then
                        begin
                           hp.paraloc.loc:=LOC_FPUREGISTER;
                           hp.paraloc.register:=nextfloatreg;
                           inc(nextfloatreg);
                        end
                      else
                         begin
                            {!!!!!!!}
                            internalerror(2002071004);
                        end;
                   end;
                 LOC_REFERENCE:
                   begin
                      if push_addr_param(hp.paratype.def) or (hp.paratyp in [vs_var,vs_out]) then
                        begin
                           if nextintreg<=R_8 then
                             begin
                                hp.paraloc.loc:=LOC_REGISTER;
                                hp.paraloc.register:=nextintreg;
                                inc(nextintreg);
                             end
                           else
                              begin
                                 {!!!!!!!}
                                 internalerror(2002071005);
                             end;
                        end
                      else
                        begin
                           hp.paraloc.loc:=LOC_REFERENCE;
                           hp.paraloc.reference.index:=stack_pointer_reg;
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
         getfuncretparaloc.loc:=LOC_REGISTER;
         getfuncretparaloc.register:=R_3;
      end;

begin
   paramanager:=tppcparamanager.create;
end.
{
  $Log$
  Revision 1.7  2002-08-17 22:09:47  florian
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
