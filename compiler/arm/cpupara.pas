{
    $Id$
    Copyright (c) 2003 by Florian Klaempfl

    ARM specific calling conventions

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
{ ARM specific calling conventions are handled by this unit
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmtai,
       cpubase,
       symconst,symbase,symtype,symdef,paramgr;

    type
       tarmparamanager = class(tparamanager)
          function push_addr_param(def : tdef;calloption : tproccalloption) : boolean;override;
          function getintparaloc(list: taasmoutput; nr : longint) : tparalocation;override;
          procedure freeintparaloc(list: taasmoutput; nr : longint); override;
          procedure create_paraloc_info(p : tabstractprocdef; side: tcallercallee);override;
          function getfuncretparaloc(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       verbose,systems,
       cpuinfo,cginfo,cgbase,
       rgobj,
       defutil,symsym;

    function tarmparamanager.getintparaloc(list: taasmoutput; nr : longint) : tparalocation;

      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else if nr<=4 then
           begin
              result.loc:=LOC_REGISTER;
              result.register.enum:=R_INTREGISTER;
              result.register.number:=NR_R0+(nr-1)*(NR_R1-NR_R0);
              rg.getexplicitregisterint(list,result.register.number);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index.enum:=R_INTREGISTER;
              result.reference.index.number:=NR_STACK_POINTER_REG;
              result.reference.offset:=(nr-4)*4;
           end;
         result.size := OS_INT;
      end;


    procedure tarmparamanager.freeintparaloc(list: taasmoutput; nr : longint);

      var
        r: tregister;

      begin
         if nr<1 then
           internalerror(2003060401)
         else if nr<=4 then
           begin
             r.enum := R_INTREGISTER;
             r.number := NR_R0+(nr-1)*(NR_R1-NR_R0);
             rg.ungetregisterint(list,r);
           end;
      end;


    function getparaloc(p : tdef) : tcgloc;

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

    function tarmparamanager.push_addr_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        case def.deftype of
          recorddef:
            push_addr_param:=true;
          arraydef:
            push_addr_param:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          setdef :
            push_addr_param:=(tsetdef(def).settype<>smallset);
          stringdef :
            push_addr_param:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
          procvardef :
            push_addr_param:=po_methodpointer in tprocvardef(def).procoptions;
          else
            push_addr_param:=inherited push_addr_param(def,calloption);
        end;
      end;

    procedure tarmparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee);

      var
         nextintreg,nextfloatreg,nextmmreg : tregister;
         paradef : tdef;
         paraloc : tparalocation;
         stack_offset : aword;
         hp : tparaitem;
         loc : tcgloc;
         is_64bit: boolean;

      procedure assignintreg;

        begin
           if nextintreg.number<=NR_R10 then
             begin
                paraloc.loc:=LOC_REGISTER;
                paraloc.register:=nextintreg;
                inc(nextintreg.number,NR_R1-NR_R0);
                if target_info.abi=abi_powerpc_aix then
                  inc(stack_offset,4);
             end
           else
              begin
                 paraloc.loc:=LOC_REFERENCE;
                 paraloc.reference.index.enum:=R_INTREGISTER;
                 paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                 paraloc.reference.offset:=stack_offset;
                 inc(stack_offset,4);
             end;
        end;

      begin
         { zero alignment bytes }
         fillchar(nextintreg,sizeof(nextintreg),0);
         fillchar(nextfloatreg,sizeof(nextfloatreg),0);
         fillchar(nextmmreg,sizeof(nextmmreg),0);
         nextintreg.enum:=R_INTREGISTER;
         nextintreg.number:=NR_R0;
         nextfloatreg.enum:=R_F0;
         // nextmmreg:=0;
         stack_offset:=0;

         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         hp:=tparaitem(p.para.first);
         while assigned(hp) do
           begin
              if (hp.paratyp in [vs_var,vs_out]) then
                begin
                  paradef := voidpointertype.def;
                  loc := LOC_REGISTER;
                end
              else
                begin
                  paradef := hp.paratype.def;
                  loc:=getparaloc(paradef);
                end;
              { make sure all alignment bytes are 0 as well }
              fillchar(paraloc,sizeof(paraloc),0);
              case loc of
                 LOC_REGISTER:
                   begin
                      paraloc.size := def_cgsize(paradef);
                      { for things like formaldef }
                      if paraloc.size = OS_NO then
                        paraloc.size := OS_ADDR;
                      is_64bit := paraloc.size in [OS_64,OS_S64];
                      if nextintreg.number<=(NR_R10-ord(is_64bit)*(NR_R1-NR_R0))  then
                        begin
                           paraloc.loc:=LOC_REGISTER;
		           if is_64bit then
                             begin
			       if odd((nextintreg.number-NR_R3) shr 8) and (target_info.abi=abi_powerpc_sysv) Then
                                inc(nextintreg.number,NR_R1-NR_R0);
                               paraloc.registerhigh:=nextintreg;
                               inc(nextintreg.number,NR_R1-NR_R0);
                               if target_info.abi=abi_powerpc_aix then
                                 inc(stack_offset,4);
                             end;
                           paraloc.registerlow:=nextintreg;
                           inc(nextintreg.number,NR_R1-NR_R0);
                           if target_info.abi=abi_powerpc_aix then
                             inc(stack_offset,4);

                        end
                      else
                         begin
                            nextintreg.number := NR_R11;
                            paraloc.loc:=LOC_REFERENCE;
                            paraloc.reference.index.enum:=R_INTREGISTER;
                            paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                            paraloc.reference.offset:=stack_offset;
                            if not is_64bit then
                              inc(stack_offset,4)
                            else
                              inc(stack_offset,8);
                        end;
                   end;
                 LOC_FPUREGISTER:
                   begin
                      paraloc.size:=def_cgsize(paradef);
                      if nextfloatreg.enum<=R_F10 then
                        begin
                           paraloc.loc:=LOC_FPUREGISTER;
                           paraloc.register:=nextfloatreg;
                           inc(nextfloatreg.enum);
                        end
                      else
                         begin
                            {!!!!!!!}
                            paraloc.size:=def_cgsize(paradef);
                            internalerror(2002071004);
                        end;
                   end;
                 LOC_REFERENCE:
                   begin
                      paraloc.size:=OS_ADDR;
                      if push_addr_param(paradef,p.proccalloption) or
                        is_open_array(paradef) or
                        is_array_of_const(paradef) then
                        assignintreg
                      else
                        begin
                           paraloc.loc:=LOC_REFERENCE;
                           paraloc.reference.index.enum:=R_INTREGISTER;
                           paraloc.reference.index.number:=NR_STACK_POINTER_REG;
                           paraloc.reference.offset:=stack_offset;
                           inc(stack_offset,hp.paratype.def.size);
                        end;
                   end;
                 else
                   internalerror(2002071002);
              end;
              if side = callerside then
                hp.callerparaloc:=paraloc
              else
                begin
                  if (paraloc.loc = LOC_REFERENCE) then
                    paraloc.reference.offset := tvarsym(hp.parasym).adjusted_address;
                  hp.calleeparaloc:=paraloc;
                end;
              hp:=tparaitem(hp.next);
           end;
      end;


    function tarmparamanager.getfuncretparaloc(p : tabstractprocdef) : tparalocation;
      begin
         fillchar(result,sizeof(result),0);
         case p.rettype.def.deftype of
            orddef,
            enumdef:
              begin
                getfuncretparaloc.loc:=LOC_REGISTER;
                getfuncretparaloc.register.enum:=R_INTREGISTER;
                getfuncretparaloc.register.number:=NR_R0;
                getfuncretparaloc.size:=def_cgsize(p.rettype.def);
                if getfuncretparaloc.size in [OS_S64,OS_64] then
                  begin
                    getfuncretparaloc.registerhigh.enum:=R_INTREGISTER;
                    getfuncretparaloc.registerhigh.number:=NR_R0;
		    getfuncretparaloc.register.number:=NR_R1;
                  end;
              end;
            floatdef:
              begin
                getfuncretparaloc.loc:=LOC_FPUREGISTER;
                getfuncretparaloc.register.enum:=R_F0;
                getfuncretparaloc.size:=def_cgsize(p.rettype.def);
              end;
            { smallsets are OS_INT in R0, others are OS_ADDR in R0 -> the same }
            { ugly, I know :) (JM)                                             }
            setdef,
            variantdef,
            pointerdef,
            formaldef,
            classrefdef,
            recorddef,
            objectdef,
            procvardef,
            filedef,
            arraydef,
            stringdef:
              begin
                if (p.rettype.def.deftype <> stringdef) or
                   (is_ansistring(p.rettype.def) or
                    is_widestring(p.rettype.def)) then
                  begin
                    getfuncretparaloc.loc:=LOC_REGISTER;
                    getfuncretparaloc.register.enum:=R_INTREGISTER;
                    getfuncretparaloc.register.number:=NR_R0;
                    getfuncretparaloc.size:=OS_ADDR;
                  end
                else
                  internalerror(2003061601);
              end;
            else
              internalerror(2002090903);
        end;
      end;


begin
   paramanager:=tarmparamanager.create;
end.
{
  $Log$
  Revision 1.1  2003-07-21 16:35:30  florian
    * very basic stuff for the arm
}
