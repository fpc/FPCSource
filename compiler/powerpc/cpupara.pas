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
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       cclasses,
       aasmtai,
       cpubase,cpuinfo,
       symconst,symbase,symtype,symdef,symsym,
       paramgr,parabase,cgbase;

    type
       tppcparamanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;

          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;

          procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
         private
          procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras:tparalist;
              var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;
          function parseparaloc(p : tparavarsym;const s : string) : boolean;override;
       end;

  implementation

    uses
       verbose,systems,
       procinfo,
       rgobj,
       defutil,
       cgutils;


    function tppcparamanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result := [RS_R3..RS_R12];
      end;


    function tppcparamanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        case target_info.abi of
          abi_powerpc_aix:
            result := [RS_F0..RS_F13];
          abi_powerpc_sysv:
            result := [RS_F0..RS_F13];
          else
            internalerror(2003091401);
        end;
      end;


    procedure tppcparamanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        cgpara.reset;
        cgpara.size:=OS_INT;
        cgpara.alignment:=get_para_align(calloption);
        paraloc:=cgpara.add_location;
        with paraloc^ do
         begin
           size:=OS_INT;
           if (nr<=8) then
             begin
               if nr=0 then
                 internalerror(200309271);
               loc:=LOC_REGISTER;
               register:=newreg(R_INTREGISTER,RS_R2+nr,R_SUBWHOLE);
             end
           else
             begin
               loc:=LOC_REFERENCE;
               paraloc^.reference.index:=NR_STACK_POINTER_REG;
               reference.offset:=sizeof(aint)*(nr-8);
             end;
          end;
      end;



    function getparaloc(p : tdef) : tcgloc;

      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.deftype of
            orddef:
              result:=LOC_REGISTER;
            floatdef:
              result:=LOC_FPUREGISTER;
            enumdef:
              result:=LOC_REGISTER;
            pointerdef:
              result:=LOC_REGISTER;
            formaldef:
              result:=LOC_REGISTER;
            classrefdef:
              result:=LOC_REGISTER;
            recorddef:
              result:=LOC_REFERENCE;
            objectdef:
              if is_object(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            procvardef:
              if (po_methodpointer in tprocvardef(p).procoptions) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            filedef:
              result:=LOC_REGISTER;
            arraydef:
              result:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                result:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
            variantdef:
              result:=LOC_REFERENCE;
            { avoid problems with errornous definitions }
            errordef:
              result:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;


    function tppcparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      var
        size, dummy: aint;
      begin
        { var,out always require address }
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        case def.deftype of
          recorddef:
            begin
              if (target_info.abi = abi_powerpc_aix) then
                begin
                  // all records should be passed by value under the aix abi,
                  // but we can only fake this for 1, 2 and 4 bytes for now
                  size := def.size;
                  result := (size > 4) or
                            not(byte(size) in [1,2,4]);
                end
              else
                result := true;
            end;
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          setdef :
            result:=(tsetdef(def).settype<>smallset);
          stringdef :
            result:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
          procvardef :
            result:=po_methodpointer in tprocvardef(def).procoptions;
          else
            result:=inherited push_addr_param(varspez,def,calloption);
        end;
      end;


    procedure tppcparamanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      begin
        case target_info.abi of
          abi_powerpc_aix:
            cur_stack_offset:=24;
          abi_powerpc_sysv:
            cur_stack_offset:=8;
          else
            internalerror(2003051901);
        end;
        curintreg:=RS_R3;
        curfloatreg:=RS_F1;
        curmmreg:=RS_M1;
      end;


    procedure tppcparamanager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        retcgsize  : tcgsize;
      begin
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          retcgsize:=OS_ADDR
        else
          retcgsize:=def_cgsize(p.rettype.def);

        location_reset(p.funcretloc[side],LOC_INVALID,OS_NO);
        p.funcretloc[side].size:=retcgsize;
        { void has no location }
        if is_void(p.rettype.def) then
          begin
            p.funcretloc[side].loc:=LOC_VOID;
            exit;
          end;

        { Return in FPU register? }
        if p.rettype.def.deftype=floatdef then
          begin
            p.funcretloc[side].loc:=LOC_FPUREGISTER;
            p.funcretloc[side].register:=NR_FPU_RESULT_REG;
            p.funcretloc[side].size:=retcgsize;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
{$ifndef cpu64bit}
            if retcgsize in [OS_64,OS_S64] then
             begin
               { low 32bits }
               p.funcretloc[side].loc:=LOC_REGISTER;
               if side=callerside then
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
               { high 32bits }
               if side=callerside then
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
             end
            else
{$endif cpu64bit}
             begin
               p.funcretloc[side].loc:=LOC_REGISTER;
               p.funcretloc[side].size:=retcgsize;
               if side=callerside then
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(retcgsize))
               else
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(retcgsize));
             end;
          end
        else
          begin
            p.funcretloc[side].loc:=LOC_REFERENCE;
            p.funcretloc[side].size:=retcgsize;
          end;
      end;


    function tppcparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;

      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result := create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);

        create_funcretloc_info(p,side);
      end;



    function tppcparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras:tparalist;
               var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;
      var
         stack_offset: aword;
         nextintreg,nextfloatreg,nextmmreg, maxfpureg : tsuperregister;
         paradef : tdef;
         paraloc,paraloc2 : pcgparalocation;
         i  : integer;
         hp : tparavarsym;
         loc : tcgloc;
         paracgsize: tcgsize;
         is_64bit: boolean;

      procedure assignintreg;

        begin
          if nextintreg<=ord(NR_R10) then
            begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
               inc(nextintreg);
               if target_info.abi=abi_powerpc_aix then
                 inc(stack_offset,4);
            end
          else
             begin
                paraloc^.loc:=LOC_REFERENCE;
                if (side = callerside) then
                  paraloc^.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc^.reference.index:=NR_R12;
                paraloc^.reference.offset:=stack_offset;
                inc(stack_offset,4);
            end;
        end;

      begin
{$ifdef extdebug}
         if po_explicitparaloc in p.procoptions then
           internalerror(200411141);
{$endif extdebug}

         result:=0;
         nextintreg := curintreg;
         nextfloatreg := curfloatreg;
         nextmmreg := curmmreg;
         stack_offset := cur_stack_offset;
         case target_info.abi of
           abi_powerpc_aix:
             maxfpureg := RS_F13;
           abi_powerpc_sysv:
             maxfpureg := RS_F8;
           else internalerror(2004070912);
         end;

          for i:=0 to paras.count-1 do
            begin
              hp:=tparavarsym(paras[i]);
              { Syscall for Morphos can have already a paraloc set }
              if (vo_has_explicit_paraloc in hp.varoptions) then
                begin
                  if not(vo_is_syscall_lib in hp.varoptions) then
                    internalerror(200412153);
                  continue;
                end;
              hp.paraloc[side].reset;
              { currently only support C-style array of const }
              if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
                 is_array_of_const(hp.vartype.def) then
                begin
                  paraloc:=hp.paraloc[side].add_location;
                  { hack: the paraloc must be valid, but is not actually used }
                  paraloc^.loc := LOC_REGISTER;
                  paraloc^.register := NR_R0;
                  paraloc^.size := OS_ADDR;
                  break;
                end;

              if (hp.varspez in [vs_var,vs_out]) then
                begin
                  paradef:=voidpointertype.def;
                  loc:=LOC_REGISTER;
                  paracgsize := OS_ADDR;
                end
              else
                begin
                  paradef := hp.vartype.def;
                  loc:=getparaloc(paradef);
                  if (hp.vartype.def.deftype = recorddef) and
                     (target_info.abi = abi_powerpc_aix) and
                     (hp.vartype.def.size <= 4) and
                     (byte(hp.vartype.def.size) in [1,2,4]) then
                    begin
                      loc := LOC_REGISTER;
                      paracgsize := def_cgsize(paradef);
                    end
                  else
                    begin
                      paracgsize:=def_cgsize(paradef);
                      { for things like formaldef }
                      if paracgsize=OS_NO then
                        paracgsize:=OS_ADDR;
                   end
                end;
              hp.paraloc[side].alignment:=std_param_align;
              hp.paraloc[side].size:=paracgsize;
              { First location }
              paraloc:=hp.paraloc[side].add_location;
              paraloc^.size:=paracgsize;
              case loc of
                 LOC_REGISTER:
                   begin
                      is_64bit:=paraloc^.size in [OS_64,OS_S64];
                      if nextintreg<=(RS_R10-ord(is_64bit))  then
                        begin
                           paraloc^.loc:=LOC_REGISTER;
{$ifndef cpu64bit}
                           if is_64bit then
                             begin
                               if odd(nextintreg-RS_R3) and (target_info.abi=abi_powerpc_sysv) Then
                                 inc(nextintreg);
                               paraloc^.size:=OS_32;
                               paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                               inc(nextintreg);
                               paraloc2:=hp.paraloc[side].add_location;
                               paraloc2^.loc:=LOC_REGISTER;
                               paraloc2^.size:=OS_32;
                               paraloc2^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                               inc(nextintreg);
                               if target_info.abi=abi_powerpc_aix then
                                 inc(stack_offset,8);
                             end
                           else
{$endif cpu64bit}
                             begin
                               paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                               inc(nextintreg);
                               if target_info.abi=abi_powerpc_aix then
                                 inc(stack_offset,sizeof(aword));
                              end;
                        end
                      else
                         begin
                            nextintreg:=RS_R11;
                            paraloc^.loc:=LOC_REFERENCE;
                            if (side = callerside) then
                              paraloc^.reference.index:=NR_STACK_POINTER_REG
                            else
                              paraloc^.reference.index:=NR_R12;
                            paraloc^.reference.offset:=stack_offset;
                            if not is_64bit then
                              inc(stack_offset,4)
                            else
                              inc(stack_offset,8);
                        end;
                   end;
                 LOC_FPUREGISTER:
                   begin
                      if nextfloatreg<=maxfpureg then
                        begin
                           paraloc^.loc:=LOC_FPUREGISTER;
                           paraloc^.register:=newreg(R_FPUREGISTER,nextfloatreg,R_SUBWHOLE);
                           inc(nextfloatreg);
                        end
                      else
                         begin
                            paraloc^.loc:=LOC_REFERENCE;
                            if (side = callerside) then
                              paraloc^.reference.index:=NR_STACK_POINTER_REG
                            else
                              paraloc^.reference.index:=NR_R12;
                            paraloc^.reference.offset:=stack_offset;
                        end;
                      if target_info.abi=abi_powerpc_aix then
                        begin
                          if paraloc^.size = OS_F32 then
                            begin
                              inc(stack_offset,4);
                              if (nextintreg < RS_R11) then
                                inc(nextintreg);
                            end
                          else
                            begin
                              inc(stack_offset,8);
                              if (nextintreg < RS_R10) then
                                inc(nextintreg,2)
                              else
                                nextintreg := RS_R11;
                            end;
                        end;
                   end;
                 LOC_REFERENCE:
                   begin
                      paraloc^.size:=OS_ADDR;
                      if push_addr_param(hp.varspez,paradef,p.proccalloption) or
                        is_open_array(paradef) or
                        is_array_of_const(paradef) then
                        assignintreg
                      else
                        begin
                           paraloc^.loc:=LOC_REFERENCE;
                           if (side = callerside) then
                             paraloc^.reference.index:=NR_STACK_POINTER_REG
                           else
                             paraloc^.reference.index:=NR_R12;
                           paraloc^.reference.offset:=stack_offset;
                           inc(stack_offset,hp.vartype.def.size);
                        end;
                   end;
                 else
                   internalerror(2002071002);
              end;
           end;
         curintreg:=nextintreg;
         curfloatreg:=nextfloatreg;
         curmmreg:=nextmmreg;
         cur_stack_offset:=stack_offset;
         result:=cur_stack_offset;
      end;


    function tppcparamanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        parasize, l: longint;
        curintreg, firstfloatreg, curfloatreg, curmmreg: tsuperregister;
        i : integer;
        hp: tparavarsym;
        paraloc: pcgparalocation;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);
        firstfloatreg:=curfloatreg;

        result:=create_paraloc_info_intern(p,callerside,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);
        if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,callerside,varargspara,curintreg,curfloatreg,curmmreg,cur_stack_offset)
        else
          begin
            parasize:=cur_stack_offset;
            for i:=0 to varargspara.count-1 do
              begin
                hp:=tparavarsym(varargspara[i]);
                hp.paraloc[callerside].alignment:=4;
                paraloc:=hp.paraloc[callerside].add_location;
                paraloc^.loc:=LOC_REFERENCE;
                paraloc^.size:=def_cgsize(hp.vartype.def);
                paraloc^.reference.index:=NR_STACK_POINTER_REG;
                l:=push_size(hp.varspez,hp.vartype.def,p.proccalloption);
                paraloc^.reference.offset:=parasize;
                parasize:=parasize+l;
              end;
            result:=parasize;
          end;
        if curfloatreg<>firstfloatreg then
          include(varargspara.varargsinfo,va_uses_float_reg);
      end;


    function tppcparamanager.parseparaloc(p : tparavarsym;const s : string) : boolean;
      var
        paraloc : pcgparalocation;
      begin
        result:=false;
        case target_info.system of
          system_powerpc_morphos:
            begin
              p.paraloc[callerside].alignment:=4;
              p.paraloc[callerside].size:=def_cgsize(p.vartype.def);
              paraloc:=p.paraloc[callerside].add_location;
              paraloc^.loc:=LOC_REFERENCE;
              paraloc^.size:=def_cgsize(p.vartype.def);
              paraloc^.reference.index:=newreg(R_INTREGISTER,RS_R2,R_SUBWHOLE);
              { pattern is always uppercase'd }
              if s='D0' then
                paraloc^.reference.offset:=0
              else if s='D1' then
                paraloc^.reference.offset:=4
              else if s='D2' then
                paraloc^.reference.offset:=8
              else if s='D3' then
                paraloc^.reference.offset:=12
              else if s='D4' then
                paraloc^.reference.offset:=16
              else if s='D5' then
                paraloc^.reference.offset:=20
              else if s='D6' then
                paraloc^.reference.offset:=24
              else if s='D7' then
                paraloc^.reference.offset:=28
              else if s='A0' then
                paraloc^.reference.offset:=32
              else if s='A1' then
                paraloc^.reference.offset:=36
              else if s='A2' then
                paraloc^.reference.offset:=40
              else if s='A3' then
                paraloc^.reference.offset:=44
              else if s='A4' then
                paraloc^.reference.offset:=48
              else if s='A5' then
                paraloc^.reference.offset:=52
              { 'A6' (offset 56) is used by mossyscall as libbase, so API
                never passes parameters in it,
                Indeed, but this allows to declare libbase either explicitly
                or let the compiler insert it }
              else if s='A6' then
                paraloc^.reference.offset:=56
              { 'A7' is the stack pointer on 68k, can't be overwritten
                by API calls, so it has no offset }
              else
                exit;

              { copy to callee side }
              p.paraloc[calleeside].add_location^:=paraloc^;
            end;
          else
            internalerror(200404182);
        end;
        result:=true;
      end;


begin
   paramanager:=tppcparamanager.create;
end.
{
  $Log$
  Revision 1.78  2005-01-05 19:01:53  karoly
    * sysv abi also uses F0-F13 as volatile registers

  Revision 1.77  2004/12/24 15:00:11  jonas
    * fixed call-by-value passing of records with size 1, 2 or 4 for AIX abi
      (using a hack, normally all records should by passed by value under the
       aix abi, but that's currently impossible)

  Revision 1.76  2004/12/15 19:30:32  peter
    * syscall with sysv abi for morphos

  Revision 1.75  2004/12/04 21:47:46  jonas
    * modifications to work with the generic code to copy LOC_REFERENCE
      parameters to local temps (fixes tests/test/cg/tmanypara)

  Revision 1.74  2004/11/22 22:01:19  peter
    * fixed varargs
    * replaced dynarray with tlist

  Revision 1.73  2004/11/21 17:54:59  peter
    * ttempcreatenode.create_reg merged into .create with parameter
      whether a register is allowed
    * funcret_paraloc renamed to funcretloc

  Revision 1.72  2004/11/21 17:17:04  florian
    * changed funcret location back to tlocation

  Revision 1.71  2004/11/15 23:35:31  peter
    * tparaitem removed, use tparavarsym instead
    * parameter order is now calculated from paranr value in tparavarsym

  Revision 1.70  2004/11/14 16:26:29  florian
    * fixed morphos syscall

  Revision 1.69  2004/09/25 20:28:20  florian
    * indention fixed

  Revision 1.68  2004/09/21 17:25:13  peter
    * paraloc branch merged

  Revision 1.67.4.3  2004/09/18 20:21:08  jonas
    * fixed ppc, but still needs fix in tgobj

  Revision 1.67.4.2  2004/09/10 11:10:08  florian
    * first part of ppc fixes

  Revision 1.67.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.67  2004/07/19 19:15:50  florian
    * fixed funcretloc writing in units

  Revision 1.66  2004/07/17 13:51:57  florian
    * function result location for syscalls on MOS hopefully correctly set now

  Revision 1.65  2004/07/09 21:45:24  jonas
    * fixed passing of fpu paras on the stack
    * fixed number of fpu parameters passed in registers
    * skip corresponding integer registers when using an fpu register for a
      parameter under the AIX abi

  Revision 1.64  2004/07/01 18:00:37  jonas
    * fix for broken TP-style constructor handling in the compiler

  Revision 1.63  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.62  2004/05/01 22:05:02  florian
    + added lib support for Amiga/MorphOS syscalls

  Revision 1.61  2004/04/18 23:19:48  karoly
   * added correct offsets for PowerPC/MorphOS location support

  Revision 1.60  2004/04/18 15:22:24  florian
    + location support for arguments, currently PowerPC/MorphOS only

  Revision 1.59  2004/02/19 17:07:42  florian
    * fixed arg. area calculation

  Revision 1.58  2004/02/11 23:18:59  florian
    * fixed to compile the rtl again

}
