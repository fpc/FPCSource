{
    Copyright (c) 2002 by Florian Klaempfl

    Alpha specific calling conventions

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
{ Alpha specific calling conventions are handled by this unit
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       cpubase,
       symconst,symbase,symtype,symdef,paramgr;

    type
       tcpuparamanager = class(tparamanager)
          procedure create_param_loc_info(p : tabstractprocdef);override;
          function getfuncretparaloc(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       verbose,
       globtype,
       cpuinfo,cginfo,cgbase,
       defbase;

    function getparaloc(p : tdef) : tloc;

      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.typ of
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

    procedure tcpuparamanager.create_param_loc_info(p : tabstractprocdef);

      var
         nextintreg,nextfloatreg,nextmmreg : tregister;
         stack_offset : aword;
         hp : tparaitem;
         loc : tloc;
         is_64bit: boolean;

      begin
         nextintreg:=R_3;
         nextfloatreg:=R_F1;
         // nextmmreg:=R_M1;
         stack_offset:=0;
         { pointer for structured results ? }
         if not is_void(p.returndef) then
           begin
              if not(ret_in_reg(p.returndef)) then
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
                      if nextintreg<=tregister(ord(R_10)-ord(is_64bit))  then
                        begin
                           hp.paraloc.loc:=LOC_REGISTER;
                           hp.paraloc.register64.reglo:=nextintreg;
                           inc(nextintreg);
                           if is_64bit then
                             begin
                               hp.paraloc.register64.reghi:=nextintreg;
                               inc(nextintreg);
                             end;
                        end
                      else
                         begin
                            nextintreg := R_11;
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
                            if nextintreg<=R_10 then
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
                           if nextintreg<=R_10 then
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

    function tcpuparamanager.getfuncretparaloc(p : tabstractprocdef) : tparalocation;
      begin
         case p.returndef.typ of
            orddef,
            enumdef:
              begin
                getfuncretparaloc.loc:=LOC_REGISTER;
                getfuncretparaloc.register:=R_3;
                getfuncretparaloc.size:=def_cgsize(p.returndef);
                if getfuncretparaloc.size in [OS_S64,OS_64] then
                  getfuncretparaloc.register64.reghi:=R_4;
              end;
            floatdef:
              begin
                getfuncretparaloc.loc:=LOC_FPUREGISTER;
                getfuncretparaloc.register:=R_F1;
                getfuncretparaloc.size:=def_cgsize(p.returndef);
              end;
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
                getfuncretparaloc.register:=R_3;
                getfuncretparaloc.size:=OS_ADDR;
              end;
            else
              internalerror(2002090903);
        end;
      end;


begin
   paramanager:=tcpuparamanager.create;
end.
