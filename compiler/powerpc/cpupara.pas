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
       globtype,
       cclasses,
       aasmtai,
       cpubase,cpuinfo,
       symconst,symbase,symtype,symdef,paramgr,cgbase;

    type
       tppcparamanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;override;
         private
          procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
          procedure create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; firstpara: tparaitem;
              var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;
          function parseparaloc(p : tparaitem;const s : string) : boolean;override;
       end;

  implementation

    uses
       verbose,systems,
       procinfo,
       rgobj,
       defutil,symsym,cpupi;


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
            {$warning: the 64bit sysv abi also uses RS_F0..RS_F13 like the aix abi above }
            result := [RS_F0..RS_F8];
          else
            internalerror(2003091401);
        end;
      end;


    function tppcparamanager.getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;

      begin
         fillchar(result,sizeof(tparalocation),0);
         result.lochigh:=LOC_INVALID;
         if nr<1 then
           internalerror(2002070801)
         else if nr<=8 then
           begin
              result.loc:=LOC_REGISTER;
              result.register:=newreg(R_INTREGISTER,RS_R2+nr,R_SUBWHOLE);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index:=NR_STACK_POINTER_REG;
              result.reference.offset:=(nr-8)*4;
           end;
         result.size := OS_INT;
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


    function tppcparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        { var,out always require address }
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        case def.deftype of
          recorddef:
            result:=true;
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



    procedure tppcparamanager.create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        paraloc : tparalocation;
      begin
        fillchar(paraloc,sizeof(tparalocation),0);
        paraloc.size:=def_cgsize(p.rettype.def);
        paraloc.Alignment:= std_param_align;
        { Constructors return self }
        if (p.proctypeoption=potype_constructor) then
          begin
            paraloc.size:=OS_ADDR;
            paraloc.loc:=LOC_REGISTER;
            paraloc.register:=NR_FUNCTION_RESULT_REG;
          end
        else
         { Return in FPU register? }
         if p.rettype.def.deftype=floatdef then
          begin
            paraloc.loc:=LOC_FPUREGISTER;
            paraloc.register:=NR_FPU_RESULT_REG;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            paraloc.loc:=LOC_REGISTER;
{$ifndef cpu64bit}
            if paraloc.size in [OS_64,OS_S64] then
             begin
               paraloc.lochigh:=LOC_REGISTER;
               paraloc.register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG;
               paraloc.register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
             end
            else
{$endif cpu64bit}
             begin
               paraloc.register:=NR_FUNCTION_RESULT_REG
             end;
          end
        else
          begin
            paraloc.loc:=LOC_REFERENCE;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;


    function tppcparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;

      var
        paraloc : tparalocation;
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result := create_paraloc_info_intern(p,side,tparaitem(p.para.first),curintreg,curfloatreg,curmmreg,cur_stack_offset);
        
        create_funcret_paraloc_info(p,side);
      end;



    function tppcparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; firstpara: tparaitem;
               var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;
      var
         stack_offset: aword;
         nextintreg,nextfloatreg,nextmmreg, maxfpureg : tsuperregister;
         paradef : tdef;
         paraloc : tparalocation;
         hp : tparaitem;
         loc : tcgloc;
         is_64bit: boolean;

      procedure assignintreg;

        begin
           if nextintreg<=ord(NR_R10) then
             begin
                paraloc.loc:=LOC_REGISTER;
                paraloc.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                inc(nextintreg);
                if target_info.abi=abi_powerpc_aix then
                  inc(stack_offset,4);
             end
           else
              begin
                 paraloc.loc:=LOC_REFERENCE;
                 paraloc.reference.index:=NR_STACK_POINTER_REG;
                 paraloc.reference.offset:=stack_offset;
                 inc(stack_offset,4);
             end;
        end;

      begin
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

         hp:=firstpara;
         while assigned(hp) do
           begin
              { currently only support C-style array of const }
              if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
                 is_array_of_const(hp.paratype.def) then
                begin
                  { hack: the paraloc must be valid, but is not actually used }
                  hp.paraloc[side].loc := LOC_REGISTER;
                  hp.paraloc[side].lochigh := LOC_INVALID;
                  hp.paraloc[side].register := NR_R0;
                  hp.paraloc[side].size := OS_ADDR;
                  break;
                end;

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
              paraloc.alignment:= std_param_align;
              paraloc.lochigh:=LOC_INVALID;
              case loc of
                 LOC_REGISTER:
                   begin
                      paraloc.size := def_cgsize(paradef);
                      { for things like formaldef }
                      if paraloc.size = OS_NO then
                        paraloc.size := OS_ADDR;
                      is_64bit := paraloc.size in [OS_64,OS_S64];
                      if nextintreg<=(RS_R10-ord(is_64bit))  then
                        begin
                           paraloc.loc:=LOC_REGISTER;
                           if is_64bit then
                             begin
                               if odd(nextintreg-RS_R3) and (target_info.abi=abi_powerpc_sysv) Then
                                 inc(nextintreg);
                               paraloc.registerhigh:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                               paraloc.lochigh:=LOC_REGISTER;
                               inc(nextintreg);
                               if target_info.abi=abi_powerpc_aix then
                                 inc(stack_offset,4);
                             end;
                           paraloc.registerlow:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                           inc(nextintreg);
                           if target_info.abi=abi_powerpc_aix then
                             inc(stack_offset,4);
                        end
                      else
                         begin
                            nextintreg:=RS_R11;
                            paraloc.loc:=LOC_REFERENCE;
                            paraloc.reference.index:=NR_STACK_POINTER_REG;
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
                      if nextfloatreg<=maxfpureg then
                        begin
                           paraloc.loc:=LOC_FPUREGISTER;
                           paraloc.register:=newreg(R_FPUREGISTER,nextfloatreg,R_SUBWHOLE);
                           inc(nextfloatreg);
                        end
                      else
                         begin
                            paraloc.loc:=LOC_REFERENCE;
                            paraloc.reference.index:=NR_STACK_POINTER_REG;
                            paraloc.reference.offset:=stack_offset;
                        end;
                      if target_info.abi=abi_powerpc_aix then
                        begin
                          if paraloc.size = OS_F32 then
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
                      paraloc.size:=OS_ADDR;
                      if push_addr_param(hp.paratyp,paradef,p.proccalloption) or
                        is_open_array(paradef) or
                        is_array_of_const(paradef) then
                        assignintreg
                      else
                        begin
                           paraloc.loc:=LOC_REFERENCE;
                           paraloc.reference.index:=NR_STACK_POINTER_REG;
                           paraloc.reference.offset:=stack_offset;
                           inc(stack_offset,hp.paratype.def.size);
                        end;
                   end;
                 else
                   internalerror(2002071002);
              end;
{
              this is filled in in ncgutil

              if side = calleeside then
                begin
                  if (paraloc.loc = LOC_REFERENCE) then
                    begin
                      if (current_procinfo.procdef <> p) then
                        internalerror(2003112201);
                      inc(paraloc.reference.offset,current_procinfo.calc_stackframe_size);
                    end;
                end;
}
              hp.paraloc[side]:=paraloc;
              hp:=tparaitem(hp.next);
           end;
         curintreg:=nextintreg;
         curfloatreg:=nextfloatreg;
         curmmreg:=nextmmreg;
         cur_stack_offset:=stack_offset;
         result:=cur_stack_offset;
      end;


    function tppcparamanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;
      var
        cur_stack_offset: aword;
        parasize, l: longint;
        curintreg, firstfloatreg, curfloatreg, curmmreg: tsuperregister;
        hp: tparaitem;
        paraloc: tparalocation;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);
        firstfloatreg:=curfloatreg;

        result := create_paraloc_info_intern(p,callerside,tparaitem(p.para.first),curintreg,curfloatreg,curmmreg,cur_stack_offset);
        if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
          { just continue loading the parameters in the registers }
          result := create_paraloc_info_intern(p,callerside,tparaitem(varargspara.first),curintreg,curfloatreg,curmmreg,cur_stack_offset)
        else
          begin
            hp := tparaitem(varargspara.first);
            parasize := cur_stack_offset;
            while assigned(hp) do
              begin
                paraloc.size:=def_cgsize(hp.paratype.def);
                paraloc.lochigh:=LOC_INVALID;
                paraloc.loc:=LOC_REFERENCE;
                paraloc.alignment:=4;
                paraloc.reference.index:=NR_STACK_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                paraloc.reference.offset:=parasize;
                parasize:=parasize+l;
                hp.paraloc[callerside]:=paraloc;
                hp:=tparaitem(hp.next);
              end;
            result := parasize;
          end;
        if curfloatreg<>firstfloatreg then
          include(varargspara.varargsinfo,va_uses_float_reg);
      end;


    function tppcparamanager.parseparaloc(p : tparaitem;const s : string) : boolean;
      begin
        result:=false;
        case target_info.system of
          system_powerpc_morphos:
            begin
              p.paraloc[callerside].loc:=LOC_REFERENCE;
              p.paraloc[callerside].lochigh:=LOC_INVALID;
              p.paraloc[callerside].size:=def_cgsize(p.paratype.def);
              p.paraloc[callerside].alignment:=4;
              p.paraloc[callerside].reference.index:=NR_R2;
              { pattern is always uppercase'd }
              if s='D0' then
                p.paraloc[callerside].reference.offset:=0
              else if s='D1' then
                p.paraloc[callerside].reference.offset:=4
              else if s='D2' then
                p.paraloc[callerside].reference.offset:=8
              else if s='D3' then
                p.paraloc[callerside].reference.offset:=12
              else if s='D4' then
                p.paraloc[callerside].reference.offset:=16
              else if s='D5' then
                p.paraloc[callerside].reference.offset:=20
              else if s='D6' then
                p.paraloc[callerside].reference.offset:=24
              else if s='D7' then
                p.paraloc[callerside].reference.offset:=28
              else if s='A0' then
                p.paraloc[callerside].reference.offset:=32
              else if s='A1' then
                p.paraloc[callerside].reference.offset:=36
              else if s='A2' then
                p.paraloc[callerside].reference.offset:=40
              else if s='A3' then
                p.paraloc[callerside].reference.offset:=44
              else if s='A4' then
                p.paraloc[callerside].reference.offset:=48
              else if s='A5' then
                p.paraloc[callerside].reference.offset:=52
              { 'A6' (offset 56) is used by mossyscall as libbase, so API
                never passes parameters in it,
                Indeed, but this allows to declare libbase either explicitly
                or let the compiler insert it }
              else if s='A6' then
                p.paraloc[callerside].reference.offset:=56
              { 'A7' is the stack pointer on 68k, can't be overwritten
                by API calls, so it has no offset }
              else
                exit;
              p.paraloc[calleeside]:=p.paraloc[callerside];
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
  Revision 1.65  2004-07-09 21:45:24  jonas
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
