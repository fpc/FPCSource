{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generic calling convention handling

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
{# Parameter passing manager. Used to manage how
   parameters are passed to routines.
}
unit paramgr;

{$i fpcdefs.inc}

  interface

    uses
       cclasses,globtype,
       cpubase,cgbase,
       parabase,
       aasmtai,
       symconst,symtype,symsym,symdef;

    type
       {# This class defines some methods to take care of routine
          parameters. It should be overriden for each new processor
       }
       tparamanager = class
          {# Returns true if the return value is actually a parameter
             pointer.
          }
          function ret_in_param(def : tdef;calloption : tproccalloption) : boolean;virtual;

          function push_high_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;virtual;

          { Returns true if a parameter is too large to copy and only
            the address is pushed
          }
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;virtual;
          { return the size of a push }
          function push_size(varspez:tvarspez;def : tdef;calloption : tproccalloption) : longint;
          { Returns true if a parameter needs to be copied on the stack, this
            is required for cdecl procedures
          }
          function copy_value_on_stack(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;virtual;
          {# Returns a structure giving the information on
            the storage of the parameter (which must be
            an integer parameter). This is only used when calling
            internal routines directly, where all parameters must
            be 4-byte values.

            In case the location is a register, this register is allocated.
            Call freeintparaloc() after the call to free the locations again.
            Default implementation: don't do anything at all (in case you don't
            use register parameter passing)

            @param(list Current assembler list)
            @param(nr Parameter number of routine, starting from 1)
          }
          function get_para_align(calloption : tproccalloption):byte;virtual;
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;virtual;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;virtual;
          function get_volatile_registers_flags(calloption : tproccalloption):tcpuregisterset;virtual;
          function get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;virtual;

          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);virtual;abstract;

          {# allocate a parameter location created with create_paraloc_info

            @param(list Current assembler list)
            @param(loc Parameter location)
          }
          procedure allocparaloc(list: taasmoutput; const cgpara: TCGPara); virtual;

          {# free a parameter location allocated with alloccgpara

            @param(list Current assembler list)
            @param(loc Parameter location)
          }
          procedure freeparaloc(list: taasmoutput; const cgpara: TCGPara); virtual;

          { This is used to populate the location information on all parameters
            for the routine as seen in either the caller or the callee. It returns
            the size allocated on the stack
          }
          function  create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;virtual;abstract;

          { This is used to populate the location information on all parameters
            for the routine when it is being inlined. It returns
            the size allocated on the stack
          }
          function  create_inline_paraloc_info(p : tabstractprocdef):longint;virtual;

          { This is used to populate the location information on all parameters
            for the routine that are passed as varargs. It returns
            the size allocated on the stack (including the normal parameters)
          }
          function  create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;virtual;abstract;

          procedure createtempparaloc(list: taasmoutput;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);virtual;
          procedure duplicateparaloc(list: taasmoutput;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);

          function parseparaloc(parasym : tparavarsym;const s : string) : boolean;virtual;abstract;
       end;


    var
       paramanager : tparamanager;


implementation

    uses
       systems,
       cgobj,tgobj,cgutils,
       defutil,verbose;

    { true if uses a parameter as return value }
    function tparamanager.ret_in_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
         ret_in_param:=(def.deftype in [arraydef,recorddef]) or
           ((def.deftype=stringdef) and (tstringdef(def).string_typ in [st_shortstring,st_longstring])) or
           ((def.deftype=procvardef) and (po_methodpointer in tprocvardef(def).procoptions)) or
           ((def.deftype=objectdef) and is_object(def)) or
           (def.deftype=variantdef) or
           ((def.deftype=setdef) and (tsetdef(def).settype<>smallset));
      end;


    function tparamanager.push_high_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
         push_high_param:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and
                          (
                           is_open_array(def) or
                           is_open_string(def) or
                           is_array_of_const(def)
                          );
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out always require address }
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        { Only vs_const, vs_value here }
        case def.deftype of
          variantdef,
          formaldef :
            result:=true;
          recorddef :
            result:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and (def.size>sizeof(aint));
          arraydef :
            begin
              if (calloption in [pocall_cdecl,pocall_cppdecl]) then
               begin
                 { array of const values are pushed on the stack }
                 result:=not is_array_of_const(def);
               end
              else
               begin
                 result:=(
                          (tarraydef(def).highrange>=tarraydef(def).lowrange) and
                          (def.size>sizeof(aint))
                         ) or
                         is_open_array(def) or
                         is_array_of_const(def) or
                         is_array_constructor(def);
               end;
            end;
          objectdef :
            result:=is_object(def);
          stringdef :
            result:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and (tstringdef(def).string_typ in [st_shortstring,st_longstring]);
          procvardef :
            result:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and (po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            result:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and (tsetdef(def).settype<>smallset);
        end;
      end;


    { true if a parameter is too large to push and needs a concatcopy to get the value on the stack }
    function tparamanager.copy_value_on_stack(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        copy_value_on_stack:=false;
        { this is only for cdecl procedures with vs_const,vs_value }
        if not(
               (calloption in [pocall_cdecl,pocall_cppdecl]) and
               (varspez in [vs_value,vs_const])
              ) then
          exit;
        case def.deftype of
          variantdef,
          formaldef :
            copy_value_on_stack:=true;
          recorddef :
            copy_value_on_stack:=(def.size>sizeof(aint));
          arraydef :
            copy_value_on_stack:=(tarraydef(def).highrange>=tarraydef(def).lowrange) and
                                 (def.size>sizeof(aint));
          objectdef :
            copy_value_on_stack:=is_object(def);
          stringdef :
            copy_value_on_stack:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
          procvardef :
            copy_value_on_stack:=(po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            copy_value_on_stack:=(tsetdef(def).settype<>smallset);
        end;
      end;


    { return the size of a push }
    function tparamanager.push_size(varspez:tvarspez;def : tdef;calloption : tproccalloption) : longint;
      begin
        push_size:=-1;
        case varspez of
          vs_out,
          vs_var :
            push_size:=sizeof(aint);
          vs_value,
          vs_const :
            begin
                if push_addr_param(varspez,def,calloption) then
                  push_size:=sizeof(aint)
                else
                  begin
                    { special array are normally pushed by addr, only for
                      cdecl array of const it comes here and the pushsize
                      is unknown }
                    if is_array_of_const(def) then
                      push_size:=0
                    else
                      push_size:=def.size;
                  end;
            end;
        end;
      end;


    function tparamanager.get_para_align(calloption : tproccalloption):byte;
      begin
        result:=std_param_align;
      end;


    function tparamanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[];
      end;


    function tparamanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[];
      end;


    function tparamanager.get_volatile_registers_flags(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[];
      end;


    function tparamanager.get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[];
      end;


    procedure tparamanager.allocparaloc(list: taasmoutput; const cgpara: TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        paraloc:=cgpara.location;
        while assigned(paraloc) do
          begin
            case paraloc^.loc of
              LOC_REGISTER,
              LOC_CREGISTER:
                begin
                  if getsupreg(paraloc^.register)<first_int_imreg then
                    cg.getcpuregister(list,paraloc^.register);
                end;
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER:
                begin
                  if getsupreg(paraloc^.register)<first_fpu_imreg then
                    cg.getcpuregister(list,paraloc^.register);
                end;
              LOC_MMREGISTER,
              LOC_CMMREGISTER :
                begin
                  if getsupreg(paraloc^.register)<first_mm_imreg then
                    cg.getcpuregister(list,paraloc^.register);
                end;
            end;
            paraloc:=paraloc^.next;
          end;
      end;


    procedure tparamanager.freeparaloc(list: taasmoutput; const cgpara: TCGPara);
      var
        paraloc : Pcgparalocation;
{$ifdef cputargethasfixedstack}
        href : treference;
{$endif cputargethasfixedstack}
      begin
        paraloc:=cgpara.location;
        while assigned(paraloc) do
          begin
            case paraloc^.loc of
              LOC_REGISTER,
              LOC_CREGISTER:
                begin
                  if getsupreg(paraloc^.register)<first_int_imreg then
                    cg.ungetcpuregister(list,paraloc^.register);
                end;
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER:
                begin
                  if getsupreg(paraloc^.register)<first_fpu_imreg then
                    cg.ungetcpuregister(list,paraloc^.register);
                end;
              LOC_MMREGISTER,
              LOC_CMMREGISTER :
                begin
                  if getsupreg(paraloc^.register)<first_mm_imreg then
                    cg.ungetcpuregister(list,paraloc^.register);
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
{$ifdef cputargethasfixedstack}
                  { don't use reference_reset_base, because that will depend on cgobj }
                  fillchar(href,sizeof(href),0);
                  href.base:=paraloc^.reference.index;
                  href.offset:=paraloc^.reference.offset;
                  tg.ungettemp(list,href);
{$endif cputargethasfixedstack}
                end;
            end;
            paraloc:=paraloc^.next;
          end;
      end;


    procedure tparamanager.createtempparaloc(list: taasmoutput;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);
      var
        href : treference;
        len  : aint;
        paraloc,
        newparaloc : pcgparalocation;
      begin
        cgpara.reset;
        cgpara.size:=parasym.paraloc[callerside].size;
        cgpara.alignment:=parasym.paraloc[callerside].alignment;
        paraloc:=parasym.paraloc[callerside].location;
        while assigned(paraloc) do
          begin
            if paraloc^.size=OS_NO then
              len:=push_size(parasym.varspez,parasym.vartype.def,calloption)
            else
              len:=tcgsize2size[paraloc^.size];
            newparaloc:=cgpara.add_location;
            newparaloc^.size:=paraloc^.size;
{$warning maybe release this optimization for all targets?}
{$ifdef sparc}
            { Does it fit a register? }
            if len<=sizeof(aint) then
              newparaloc^.loc:=LOC_REGISTER
            else
{$endif sparc}
              newparaloc^.loc:=paraloc^.loc;
            case newparaloc^.loc of
              LOC_REGISTER :
                newparaloc^.register:=cg.getintregister(list,paraloc^.size);
              LOC_FPUREGISTER :
                newparaloc^.register:=cg.getfpuregister(list,paraloc^.size);
              LOC_MMREGISTER :
                newparaloc^.register:=cg.getmmregister(list,paraloc^.size);
              LOC_REFERENCE :
                begin
                  tg.gettemp(list,len,tt_persistent,href);
                  newparaloc^.reference.index:=href.base;
                  newparaloc^.reference.offset:=href.offset;
                end;
            end;
            paraloc:=paraloc^.next;
          end;
      end;


    procedure tparamanager.duplicateparaloc(list: taasmoutput;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);
      var
        paraloc,
        newparaloc : pcgparalocation;
      begin
        cgpara.reset;
        cgpara.size:=parasym.paraloc[callerside].size;
        cgpara.alignment:=parasym.paraloc[callerside].alignment;
        paraloc:=parasym.paraloc[callerside].location;
        while assigned(paraloc) do
          begin
            newparaloc:=cgpara.add_location;
            move(paraloc^,newparaloc^,sizeof(newparaloc^));
            newparaloc^.next:=nil;
            paraloc:=paraloc^.next;
          end;
      end;


    function tparamanager.create_inline_paraloc_info(p : tabstractprocdef):longint;
      begin
        { We need to return the size allocated }
        create_paraloc_info(p,callerside);
        result:=create_paraloc_info(p,calleeside);
      end;


initialization
  ;
finalization
  paramanager.free;
end.

{
   $Log$
   Revision 1.81  2004-11-15 23:35:31  peter
     * tparaitem removed, use tparavarsym instead
     * parameter order is now calculated from paranr value in tparavarsym

   Revision 1.80  2004/10/31 21:45:03  peter
     * generic tlocation
     * move tlocation to cgutils

   Revision 1.79  2004/09/25 14:23:54  peter
     * ungetregister is now only used for cpuregisters, renamed to
       ungetcpuregister
     * renamed (get|unget)explicitregister(s) to ..cpuregister
     * removed location-release/reference_release

   Revision 1.78  2004/09/21 17:25:12  peter
     * paraloc branch merged

   Revision 1.77.4.1  2004/08/31 20:43:06  peter
     * paraloc patch

   Revision 1.77  2004/07/09 23:41:04  jonas
     * support register parameters for inlined procedures + some inline
       cleanups

   Revision 1.76  2004/06/20 08:55:30  florian
     * logs truncated

   Revision 1.75  2004/06/16 20:07:09  florian
     * dwarf branch merged

   Revision 1.74  2004/04/18 15:22:24  florian
     + location support for arguments, currently PowerPC/MorphOS only

   Revision 1.73.2.5  2004/05/03 20:18:52  peter
     * fixes for tprintf

   Revision 1.73.2.4  2004/05/02 20:20:59  florian
     * started to fix callee side result value handling

   Revision 1.73.2.3  2004/05/02 12:45:32  peter
     * enabled cpuhasfixedstack for x86-64 again
     * fixed size of temp allocation for parameters

}

