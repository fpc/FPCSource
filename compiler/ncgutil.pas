{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for all code generators

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
unit ncgutil;

{$i defines.inc}

interface

    uses
      node;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;
    procedure restorefromtemp(p : tnode;isint64 : boolean);
{$endif TEMPS_NOT_PUSH}

    procedure maketojumpbool(p : tnode; loadregvars: tloadregvars);

implementation

  uses
    globals,globtype,systems,verbose,
    types,
    aasm,cgbase,regvars,
    temp_gen,ncon,
    cpubase,cpuinfo,tgcpu,cgobj,cgcpu,cg64f32;


{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;
      var
        href : treference;
        scratchreg : tregister;
        saved : boolean;
      begin
         if needed>usablereg32 then
           begin
              if (p.location.loc=LOC_REGISTER) then
                begin
                   if isint64 then
                     begin
                       gettempofsizereference(8,href);
                       p.temp_offset:=href.offset;
                       { do we have a 64bit processor? }
                       if sizeof(aword) < 8 then
                         begin
                           tcg64f32(cg).a_load64_reg_ref(exprasmlist,
                             p.location.registerlow,p.location.registerhigh,
                             href);
                           ungetregister(p.location.registerhigh);
                           ungetregister(p.location.registerlow);
                         end
                       else
                         begin
                           cg.a_load_reg_ref(exprasmlist,OS_64,
                             p.location.register,href);
                           ungetregister(p.location.register);
                         end;
                     end
                   else
                     begin
                        gettempofsizereference(4,href);
                        p.temp_offset:=href.offset;
                        cg.a_load_reg_ref(exprasmlist,OS_32,
                          p.location.register,href);
                        ungetregister(p.location.register);
                     end;
                   saved:=true;
                end
              else if (p.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p.location.reference.base<>R_NO) or
                       (p.location.reference.index<>R_NO)
                      ) then
                  begin
                     scratchreg := cg.get_scratch_reg(exprasmlist);
                     cg.a_loadaddress_ref_reg(exprasmlist,
                       p.location.reference,scratchreg);
                     del_reference(p.location.reference);
                     gettempofsizereference(target_info.size_of_pointer,href);
                     cg.a_load_reg_ref(exprasmlist,OS_ADDR,scratchreg,href);
                     cg.free_scratch_reg(exprasmlist,scratchreg);
                     p.temp_offset:=href.offset;
                     saved:=true;
                  end
              else saved:=false;
           end
         else saved:=false;
         maybe_savetotemp:=saved;
      end;


    procedure restorefromtemp(p : tnode;isint64 : boolean);
      var
         hregister :  tregister;
         href : treference;

      begin
         hregister:=getregisterint;
         reset_reference(href);
         href.base:=procinfo^.framepointer;
         href.offset:=p.temp_offset;
         if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p.location.registerlow:=hregister;
              if isint64 then
                begin
                  if sizeof(aword) < 8 then
                    begin
                      p.location.registerhigh:=getregisterint;
                      tcg64f32(cg).a_load64_ref_reg(exprasmlist,
                        href,p.location.registerlow,p.location.registerhigh);
                    end
                  else
                    cg.a_load_ref_reg(exprasmlist,OS_64,href,
                      p.location.register);
                end
              else
                cg.a_load_ref_reg(exprasmlist,OS_32,href,p.location.register);
           end
         else
           begin
              reset_reference(p.location.reference);
              p.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails PM
              set_location(p^.left^.location,p^.location);}
           end;
         ungetiftemp(href);
      end;
{$endif TEMPS_NOT_PUSH}


    procedure maketojumpbool(p : tnode; loadregvars: tloadregvars);
    {
      produces jumps to true respectively false labels using boolean expressions

      depending on whether the loading of regvars is currently being
      synchronized manually (such as in an if-node) or automatically (most of
      the other cases where this procedure is called), loadregvars can be
      "lr_load_regvars" or "lr_dont_load_regvars"
    }
      var
        opsize : tcgsize;
        storepos : tfileposinfo;
      begin
         if nf_error in p.flags then
           exit;
         storepos:=aktfilepos;
         aktfilepos:=p.fileinfo;
         if is_boolean(p.resulttype.def) then
           begin
              if loadregvars = lr_load_regvars then
                load_all_regvars(exprasmlist);
              if is_constboolnode(p) then
                begin
                   if tordconstnode(p).value<>0 then
                     cg.a_jmp_cond(exprasmlist,OC_NONE,truelabel)
                   else
                     cg.a_jmp_cond(exprasmlist,OC_NONE,falselabel)
                end
              else
                begin
                   opsize:=def_cgsize(p.resulttype.def);
                   case p.location.loc of
                     LOC_CREGISTER,LOC_REGISTER,LOC_MEM,LOC_REFERENCE :
                       begin
                         if (p.location.loc = LOC_CREGISTER) then
                           load_regvar_reg(exprasmlist,p.location.register);
                         cg.a_cmp_const_loc_label(exprasmlist,opsize,OC_NE,
                           0,p.location,truelabel);
                         { !!! should happen right after cmp (JM) }
                         del_location(p.location);
                         cg.a_jmp_cond(exprasmlist,OC_NONE,falselabel);
                       end;
                     LOC_FLAGS :
                       begin
                         cg.a_jmp_flags(exprasmlist,p.location.resflags,
                           truelabel);
                         cg.a_jmp_cond(exprasmlist,OC_None,falselabel);
                       end;
                   end;
                end;
           end
         else
           internalerror(200112305);
         aktfilepos:=storepos;
      end;

end.

{
  $Log$
  Revision 1.1  2001-12-30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj, part in    cg64f32) and should work correctly again (it needed some changes after    the changes of the low and high of tordef's to int64)  * maketojumpbool() is now processor independent (in ncgutil)  * getregister32 is now called getregisterint


}