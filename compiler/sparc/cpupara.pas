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
UNIT cpupara;
{SPARC specific calling conventions are handled by this unit}
{$INCLUDE fpcdefs.inc}
INTERFACE
USES
  cpubase,
  symconst,symbase,symdef,paramgr;
TYPE
  TSparcParaManager=CLASS(TParaManager)
    FUNCTION getintparaloc(nr:longint):tparalocation;OVERRIDE;
    PROCEDURE create_param_loc_info(p:tabstractprocdef);OVERRIDE;
    FUNCTION GetSelfLocation(p:tabstractprocdef):tparalocation;OVERRIDE;
  end;
IMPLEMENTATION
USES
  verbose,
  cpuinfo,
  symtype;
FUNCTION TSparcParaManager.getintparaloc(nr : longint) : tparalocation;
  BEGIN
    fillchar(result,sizeof(tparalocation),0);
    if nr<1
    then
      internalerror(2002070801)
    else if nr<=8
    then
      BEGIN
        result.loc:=LOC_REGISTER;
        result.register:=tregister(longint(R_O0)+nr);
      end
    else
           BEGIN
              result.loc:=LOC_REFERENCE;
              result.reference.index:=stack_pointer_reg;
              result.reference.offset:=(nr-8)*4;
           end;
      end;

    FUNCTION getparaloc(p : tdef) : tloc;

      BEGIN
         case p.deftype of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              getparaloc:=LOC_FPUREGISTER;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;

    PROCEDURE TSparcParaManager.create_param_loc_info(p : tabstractprocdef);

      var
         nextintreg,nextfloatreg,nextmmreg : tregister;
         stack_offset : aword;
         hp : tparaitem;
         loc : tloc;

      BEGIN
         nextintreg:=R_G3;
         nextfloatreg:=R_F1;
         nextmmreg:=R_L1;
         stack_offset:=0;
         { pointer for structured results ? }
         { !!!nextintreg:=R_4;              }

         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         hp:=tparaitem(p.para.last);
         while assigned(hp) do
           BEGIN
              loc:=getparaloc(hp.paratype.def);
              case loc of
                 LOC_REGISTER:
                   BEGIN
                      if nextintreg<=R_I7 then
                        BEGIN
                           hp.paraloc.loc:=LOC_REGISTER;
                           hp.paraloc.register:=nextintreg;
                           inc(nextintreg);
                        end
                      else
                         BEGIN
                            {!!!!!!!}
                            internalerror(2002071003);
                        end;
                   end;
                 else
                   internalerror(2002071002);
              end;
              hp:=tparaitem(hp.previous);
           end;
      end;

FUNCTION TSparcParaManager.GetSelfLocation(p:tabstractprocdef):tparalocation;
  BEGIN
    getselflocation.loc:=LOC_REFERENCE;
    getselflocation.reference.index:=R_G3{R_ESP};
    getselflocation.reference.offset:=4;
  END;

BEGIN
   paramanager:=TSparcParaManager.create;
end.
{
  $Log$
  Revision 1.1  2002-08-21 13:30:07  mazen
  *** empty log message ***

  Revision 1.2  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled
}
