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
       aasmtai,
       symconst,symtype,symdef;

    type
       tvarargsinfo = (
         va_uses_float_reg
       );

       tvarargspara = class(tlinkedlist)
          varargsinfo : set of tvarargsinfo;
{$ifdef x86_64}
          { x86_64 requires %al to contain the no. SSE regs passed }
          mmregsused  : longint;
{$endif x86_64}
       end;

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
          function getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;virtual;abstract;

          {# allocate a parameter location created with create_paraloc_info

            @param(list Current assembler list)
            @param(loc Parameter location)
          }
          procedure allocparaloc(list: taasmoutput; const loc: tparalocation); virtual;

          {# free a parameter location allocated with allocparaloc

            @param(list Current assembler list)
            @param(loc Parameter location)
          }
          procedure freeparaloc(list: taasmoutput; const loc: tparalocation); virtual;

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
          function  create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;virtual;abstract;

          { Return the location of the low and high part of a 64bit parameter }
          procedure splitparaloc64(const locpara:tparalocation;var loclopara,lochipara:tparalocation);virtual;

          procedure alloctempregs(list: taasmoutput;var locpara:tparalocation);virtual;
          procedure alloctempparaloc(list: taasmoutput;calloption : tproccalloption;paraitem : tparaitem;var locpara:tparalocation);virtual;

          function parseparaloc(paraitem : tparaitem;const s : string) : boolean;virtual;abstract;
       end;


    var
       paramanager : tparamanager;


implementation

    uses
       cpuinfo,systems,
       cgutils,cgobj,tgobj,
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


    procedure tparamanager.allocparaloc(list: taasmoutput; const loc: tparalocation);
      begin
        case loc.loc of
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER :
            begin
              { NR_NO means we don't need to allocate the parameter.
                This is used for inlining parameters which allocates
                the parameters in gen_alloc_parast (PFV) }
              if loc.register<>NR_NO then
                cg.getexplicitregister(list,loc.register);
            end;
          LOC_REFERENCE,
          LOC_CREFERENCE :
            { do nothing by default, most of the time it's the framepointer }
          else
            internalerror(200306091);
        end;
        case loc.lochigh of
          LOC_INVALID :
            ;
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER :
            begin
              { NR_NO means we don't need to allocate the parameter.
                This is used for inlining parameters which allocates
                the parameters in gen_alloc_parast (PFV) }
              if loc.registerhigh<>NR_NO then
                cg.getexplicitregister(list,loc.registerhigh);
            end;
          else
            internalerror(200306092);
        end;
      end;


    procedure tparamanager.freeparaloc(list: taasmoutput; const loc: tparalocation);
      var
        href : treference;
      begin
        case loc.loc of
          LOC_REGISTER, LOC_CREGISTER:
            begin
{$ifndef cpu64bit}
              if (loc.size in [OS_64,OS_S64,OS_F64]) then
                begin
                  cg.ungetregister(list,loc.registerhigh);
                  cg.ungetregister(list,loc.registerlow);
                end
              else
{$endif cpu64bit}
                cg.ungetregister(list,loc.register);
            end;
          LOC_MMREGISTER, LOC_CMMREGISTER,
          LOC_FPUREGISTER, LOC_CFPUREGISTER:
            cg.ungetregister(list,loc.register);
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
{$ifdef cputargethasfixedstack}
              reference_reset_base(href,loc.reference.index,loc.reference.offset);
              tg.ungettemp(list,href);
{$endif cputargethasfixedstack}
            end;
          else
            internalerror(200306093);
        end;
        case loc.lochigh of
          LOC_INVALID :
            ;
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER :
            cg.ungetregister(list,loc.register);
          else
            internalerror(200306094);
        end;
      end;


    procedure tparamanager.splitparaloc64(const locpara:tparalocation;var loclopara,lochipara:tparalocation);
      begin
        lochipara:=locpara;
        loclopara:=locpara;
        case locpara.size of
          OS_S128 :
            begin
              lochipara.size:=OS_S64;
              loclopara.size:=OS_64;
            end;
          OS_128 :
            begin
              lochipara.size:=OS_64;
              loclopara.size:=OS_64;
            end;
          OS_S64 :
            begin
              lochipara.size:=OS_S32;
              loclopara.size:=OS_32;
            end;
          OS_64 :
            begin
              lochipara.size:=OS_32;
              loclopara.size:=OS_32;
            end;
          else
            internalerror(200307023);
        end;
        loclopara.lochigh:=LOC_INVALID;
        lochipara.lochigh:=LOC_INVALID;
        case locpara.loc of
           LOC_REGISTER,
           LOC_CREGISTER,
           LOC_FPUREGISTER,
           LOC_CFPUREGISTER,
           LOC_MMREGISTER,
           LOC_CMMREGISTER :
             begin
               if locpara.lochigh=LOC_INVALID then
                 internalerror(200402061);
               loclopara.register:=locpara.registerlow;
               lochipara.register:=locpara.registerhigh;
             end;
           LOC_REFERENCE:
             begin
               if target_info.endian=endian_big then
                 inc(loclopara.reference.offset,tcgsize2size[loclopara.size])
               else
                 inc(lochipara.reference.offset,tcgsize2size[loclopara.size]);
             end;
           else
             internalerror(200307024);
        end;
      end;


    procedure tparamanager.alloctempregs(list: taasmoutput;var locpara:tparalocation);
      var
        cgsize : tcgsize;
      begin
        if locpara.lochigh<>LOC_INVALID then
          cgsize:=OS_INT
        else
          cgsize:=locpara.size;
        case locpara.loc of
          LOC_REGISTER:
            locpara.register:=cg.getintregister(list,cgsize);
          LOC_FPUREGISTER:
            locpara.register:=cg.getfpuregister(list,cgsize);
          LOC_MMREGISTER:
            locpara.register:=cg.getmmregister(list,cgsize);
          else
            internalerror(200308123);
        end;
        case locpara.lochigh of
          LOC_INVALID:
            ;
          LOC_REGISTER:
            locpara.registerhigh:=cg.getintregister(list,cgsize);
          LOC_FPUREGISTER:
            locpara.registerhigh:=cg.getfpuregister(list,cgsize);
          LOC_MMREGISTER:
            locpara.registerhigh:=cg.getmmregister(list,cgsize);
          else
            internalerror(200308124);
        end;
      end;


    procedure tparamanager.alloctempparaloc(list: taasmoutput;calloption : tproccalloption;paraitem : tparaitem;var locpara:tparalocation);
      var
        href : treference;
        l    : aint;
      begin
        l:=push_size(paraitem.paratyp,paraitem.paratype.def,calloption);
        tg.gettemp(list,l,tt_persistent,href);
        locpara.loc:=LOC_REFERENCE;
        locpara.lochigh:=LOC_INVALID;
        locpara.reference.index:=href.base;
        locpara.reference.offset:=href.offset;
      end;


    function tparamanager.create_inline_paraloc_info(p : tabstractprocdef):longint;
      var
        hp : tparaitem;
        paraloc : tparalocation;
        parasize : longint;
      begin
        parasize:=0;
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            if push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption) then
              paraloc.size:=OS_ADDR
            else
              paraloc.size:=def_cgsize(hp.paratype.def);
            if paraloc.size=OS_NO then
              internalerror(200309301);
            { Indicate parameter is loaded in register, the register
              will be allocated when the allocpara is called }
            paraloc.loc:=LOC_REGISTER;
            paraloc.register:=NR_NO;
(*
                paraloc.loc:=LOC_REFERENCE;
                paraloc.reference.index:=NR_FRAME_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=size_2_align(l);
                paraloc.reference.offset:=parasize+target_info.first_parm_offset;
                varalign:=used_align(varalign,p.paraalign,p.paraalign);
                parasize:=align(parasize+l,varalign);
*)
            hp.paraloc[callerside]:=paraloc;
            hp.paraloc[calleeside]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { We need to return the size allocated }
        result:=parasize;
      end;


initialization
  ;
finalization
  paramanager.free;
end.

{
   $Log$
   Revision 1.76  2004-06-20 08:55:30  florian
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

