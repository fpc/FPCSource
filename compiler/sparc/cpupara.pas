{*****************************************************************************}
{ File                   : cpupara.pas                                        }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\07\13                                         }
{ Last modification date : 2002\08\20                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
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
 ****************************************************************************}
unit cpupara;
{SPARC specific calling conventions are handled by this unit}
{$INCLUDE fpcdefs.inc}
interface
uses
	cpubase,
	symconst,symbase,symtype,symdef,paramgr;
type
	tSparcparamanager = class(tparamanager)
          function getintparaloc(nr : longint) : tparalocation;override;
          procedure create_param_loc_info(p : tabstractprocdef);override;
          function getfuncretparaloc(p : tabstractprocdef) : tparalocation;override;
	end;
implementation
uses
	verbose,
	globtype,
	cpuinfo,cginfo,cgbase,
	defbase;

    function tSparcparamanager.getintparaloc(nr : longint) : tparalocation;

      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else if nr<=8 then
           begin
              result.loc:=LOC_REGISTER;
              result.register:=tregister(longint(R_O0)+nr);
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

    procedure tSparcparamanager.create_param_loc_info(p : tabstractprocdef);

      var
         nextintreg,nextfloatreg,nextmmreg : tregister;
         stack_offset : aword;
         hp : tparaitem;
         loc : tloc;
         is_64bit: boolean;

      begin
         nextintreg:=R_O3;
         nextfloatreg:=R_F1;
         nextmmreg:=R_NONE;
         stack_offset:=0;
         { pointer for structured results ? }
         if not is_void(p.rettype.def) then
           begin
              if not(ret_in_reg(p.rettype.def)) then
                inc(nextintreg);
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
                      if nextintreg<=tregister(ord(R_O4)-ord(is_64bit))  then
                        begin
                           hp.paraloc.loc:=LOC_REGISTER;
                           hp.paraloc.registerlow:=nextintreg;
                           inc(nextintreg);
                           if is_64bit then
                             begin
                               hp.paraloc.registerhigh:=nextintreg;
                               inc(nextintreg);
                             end;
                        end
                      else
                         begin
                            nextintreg := R_O5;
                            hp.paraloc.loc:=LOC_REFERENCE;
                            hp.paraloc.reference.index:=stack_pointer_reg;
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
                            if nextintreg<=R_O5 then
                             begin
                                hp.paraloc.size:=OS_ADDR;
                                hp.paraloc.loc:=LOC_REGISTER;
                                hp.paraloc.register:=nextintreg;
                                inc(nextintreg);
                             end
                           else
                              begin
                                 {!!!!!!!}
                                 hp.paraloc.size:=def_cgsize(hp.paratype.def);
                                 internalerror(2002071006);
                             end;
                        end
                      else if nextfloatreg<=R_F10 then
                        begin
                           hp.paraloc.size:=def_cgsize(hp.paratype.def);
                           hp.paraloc.loc:=LOC_FPUREGISTER;
                           hp.paraloc.register:=nextfloatreg;
                           inc(nextfloatreg);
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
                      if push_addr_param(hp.paratype.def,p.proccalloption in [pocall_cdecl,pocall_cppdecl]) or (hp.paratyp in [vs_var,vs_out]) then
                        begin
                           if nextintreg<=R_O5 then
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

    function tSparcparamanager.getfuncretparaloc(p : tabstractprocdef) : tparalocation;
      begin
         case p.rettype.def.deftype of
            orddef,
            enumdef:
              begin
                getfuncretparaloc.loc:=LOC_REGISTER;
                getfuncretparaloc.register:=R_O0;
                getfuncretparaloc.size:=def_cgsize(p.rettype.def);
                if getfuncretparaloc.size in [OS_S64,OS_64] then
                  getfuncretparaloc.registerhigh:=R_O1;
              end;
            floatdef:
              begin
                getfuncretparaloc.loc:=LOC_FPUREGISTER;
                getfuncretparaloc.register:=R_F1;
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
                getfuncretparaloc.register:=R_O0;
                getfuncretparaloc.size:=OS_ADDR;
              end;
            else
              internalerror(2002090903);
        end;
      end;

BEGIN
   paramanager:=TSparcParaManager.create;
end.
{
  $Log$
  Revision 1.2  2002-10-04 21:57:42  mazen
  * register allocation for parameters now done in cpupara, but InternalError(200109223) in cgcpu.pas:1053 is still not fixed du to location_force problem in ncgutils.pas:419

  Revision 1.1  2002/08/21 13:30:07  mazen
  *** empty log message ***

  Revision 1.2  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled
}
