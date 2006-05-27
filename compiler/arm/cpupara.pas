{
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
       globtype,globals,
       aasmtai,aasmdata,
       cpuinfo,cpubase,cgbase,
       symconst,symbase,symtype,symdef,parabase,paramgr;

    type
       tarmparamanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
         private
          procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
            var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;
       end;

  implementation

    uses
       verbose,systems,
       rgobj,
       defutil,symsym,
       cgutils;


    function tarmparamanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_INTREGISTERS;
      end;


    function tarmparamanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_FPUREGISTERS;
      end;


    procedure tarmparamanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        if nr<1 then
          internalerror(2002070801);
        cgpara.reset;
        cgpara.size:=OS_INT;
        cgpara.intsize:=tcgsize2size[OS_INT];
        cgpara.alignment:=std_param_align;
        paraloc:=cgpara.add_location;
        with paraloc^ do
          begin
            size:=OS_INT;
            { the four first parameters are passed into registers }
            if nr<=4 then
              begin
                loc:=LOC_REGISTER;
                register:=newreg(R_INTREGISTER,RS_R0+nr-1,R_SUBWHOLE);
              end
            else
              begin
                { the other parameters are passed on the stack }
                loc:=LOC_REFERENCE;
                reference.index:=NR_STACK_POINTER_REG;
                reference.offset:=(nr-5)*4;
              end;
          end;
      end;


    function getparaloc(calloption : tproccalloption; p : tdef) : tcgloc;
      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.deftype of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              if (calloption in [pocall_cdecl,pocall_cppdecl,pocall_softfloat]) or (cs_fp_emulation in aktmoduleswitches) then
                getparaloc:=LOC_REGISTER
              else
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


    function tarmparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        case def.deftype of
          variantdef,
          formaldef,
          recorddef:
            result:=true;
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          objectdef :
            result:=is_object(def);
          setdef :
            result:=(tsetdef(def).settype<>smallset);
          stringdef :
            result:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
        end;
      end;


    procedure tarmparamanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      begin
        curintreg:=RS_R0;
        curfloatreg:=RS_F0;
        curmmreg:=RS_D0;
        cur_stack_offset:=0;
      end;


    function tarmparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
        var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;

      var
        nextintreg,nextfloatreg,nextmmreg : tsuperregister;
        paradef : tdef;
        paraloc : pcgparalocation;
        stack_offset : aword;
        hp : tparavarsym;
        loc : tcgloc;
        paracgsize   : tcgsize;
        paralen : longint;
        i : integer;

      procedure assignintreg;
        begin
           if nextintreg<=RS_R3 then
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
               inc(nextintreg);
             end
           else
             begin
               paraloc^.loc:=LOC_REFERENCE;
               paraloc^.reference.index:=NR_STACK_POINTER_REG;
               paraloc^.reference.offset:=stack_offset;
               inc(stack_offset,4);
            end;
        end;


      begin
        result:=0;
        nextintreg:=curintreg;
        nextfloatreg:=curfloatreg;
        nextmmreg:=curmmreg;
        stack_offset:=cur_stack_offset;

        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            { currently only support C-style array of const,
              there should be no location assigned to the vararg array itself }
            if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
               is_array_of_const(hp.vartype.def) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_R0;
                paraloc^.size:=OS_ADDR;
                break;
              end;

            if push_addr_param(hp.varspez,hp.vartype.def,p.proccalloption) then
              paracgsize:=OS_ADDR
            else
              begin
                paracgsize:=def_cgSize(hp.vartype.def);
                if paracgsize=OS_NO then
                  paracgsize:=OS_ADDR;
              end;

             hp.paraloc[side].reset;
             hp.paraloc[side].size:=paracgsize;
             hp.paraloc[side].Alignment:=std_param_align;

             if (hp.varspez in [vs_var,vs_out]) then
               begin
                 paradef:=voidpointertype.def;
                 loc:=LOC_REGISTER;
               end
             else
               begin
                 paradef:=hp.vartype.def;
                 loc:=getparaloc(p.proccalloption,paradef);
               end;

             paralen:=tcgsize2size[paracgsize];
             hp.paraloc[side].intsize:=paralen;
{$ifdef EXTDEBUG}
             if paralen=0 then
               internalerror(200410311);
{$endif EXTDEBUG}
             while paralen>0 do
               begin
                 paraloc:=hp.paraloc[side].add_location;
                 { for things like formaldef }
                 if paracgsize=OS_NO then
                   paraloc^.size:=OS_ADDR
                 else if paracgsize in [OS_64,OS_S64] then
                   paraloc^.size:=OS_32
                 else if (loc=LOC_REGISTER) and (paracgsize in [OS_F32,OS_F64,OS_F80]) then
                   case paracgsize of
                     OS_F32:
                       paraloc^.size:=OS_32;
                     OS_F64:
                       paraloc^.size:=OS_64;
                     else
                       internalerror(2005082901);
                   end
                 else
                   paraloc^.size:=paracgsize;
                 case loc of
                    LOC_REGISTER:
                      begin
                        { this is not abi compliant }
                        if nextintreg<=RS_R3 then
                          begin
                            paraloc^.loc:=LOC_REGISTER;
                            paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
                            inc(nextintreg);
                          end
                        else
                          begin
                            { LOC_REFERENCE covers always the overleft }
                            paraloc^.loc:=LOC_REFERENCE;
                            paraloc^.size:=int_cgsize(paralen);
                            if (side=callerside) then
                              paraloc^.reference.index:=NR_STACK_POINTER_REG;
                            paraloc^.reference.offset:=stack_offset;
                            inc(stack_offset,align(paralen,4));
                            paralen:=0;
                         end;
                      end;
                    LOC_FPUREGISTER:
                      begin
                        if nextfloatreg<=RS_F3 then
                          begin
                            paraloc^.loc:=LOC_FPUREGISTER;
                            paraloc^.register:=newreg(R_FPUREGISTER,nextfloatreg,R_SUBWHOLE);
                            inc(nextfloatreg);
                          end
                        else
                          begin
                            paraloc^.loc:=LOC_REFERENCE;
                            paraloc^.reference.index:=NR_STACK_POINTER_REG;
                            paraloc^.reference.offset:=stack_offset;
                            case paraloc^.size of
                              OS_F32:
                                inc(stack_offset,4);
                              OS_F64:
                                inc(stack_offset,8);
                              OS_F80:
                                inc(stack_offset,10);
                              OS_F128:
                                inc(stack_offset,16);
                              else
                                internalerror(200403201);
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
                             paraloc^.reference.index:=NR_STACK_POINTER_REG;
                             paraloc^.reference.offset:=stack_offset;
                             inc(stack_offset,hp.vartype.def.size);
                          end;
                      end;
                    else
                      internalerror(2002071002);
                 end;
                 if side=calleeside then
                   begin
                     if paraloc^.loc=LOC_REFERENCE then
                       begin
                         paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                         inc(paraloc^.reference.offset,4);
                       end;
                   end;
                 dec(paralen,tcgsize2size[paraloc^.size]);
               end;
             { hack to swap doubles in int registers }
             if is_double(hp.vartype.def) and (paracgsize=OS_64) and
               (hp.paraloc[side].location^.loc=LOC_REGISTER) then
               begin
                 paraloc:=hp.paraloc[side].location;
                 hp.paraloc[side].location:=hp.paraloc[side].location^.next;
                 hp.paraloc[side].location^.next:=paraloc;
                 paraloc^.next:=nil;
               end;
          end;
        curintreg:=nextintreg;
        curfloatreg:=nextfloatreg;
        curmmreg:=nextmmreg;
        cur_stack_offset:=stack_offset;
        result:=cur_stack_offset;
      end;


    function tarmparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
        retcgsize  : tcgsize;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);

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
            location_reset(p.funcretloc[side],LOC_VOID,OS_NO);
            exit;
          end;

        { Return in FPU register? }
        if p.rettype.def.deftype=floatdef then
          begin
            if (p.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_softfloat]) or (cs_fp_emulation in aktmoduleswitches) then
              begin
                case retcgsize of
                  OS_64,
                  OS_F64:
                    begin
                      { low }
                      p.funcretloc[side].loc:=LOC_REGISTER;
                      p.funcretloc[side].register64.reglo:=NR_FUNCTION_RESULT64_HIGH_REG;
                      p.funcretloc[side].register64.reghi:=NR_FUNCTION_RESULT64_LOW_REG;
                      p.funcretloc[side].size:=OS_64;
                    end;
                  OS_32,
                  OS_F32:
                    begin
                      p.funcretloc[side].loc:=LOC_REGISTER;
                      p.funcretloc[side].register:=NR_FUNCTION_RETURN_REG;
                      p.funcretloc[side].size:=OS_32;
                    end;
                  else
                    internalerror(2005082603);
                end;
              end
            else
              begin
                p.funcretloc[side].loc:=LOC_FPUREGISTER;
                p.funcretloc[side].register:=NR_FPU_RESULT_REG;
              end;
          end
          { Return in register? }
        else if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            if retcgsize in [OS_64,OS_S64] then
              begin
                { low }
                p.funcretloc[side].loc:=LOC_REGISTER;
                p.funcretloc[side].register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG;
                p.funcretloc[side].register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG;
              end
            else
              begin
                p.funcretloc[side].loc:=LOC_REGISTER;
                p.funcretloc[side].register:=NR_FUNCTION_RETURN_REG;
              end;
          end
        else
          begin
            p.funcretloc[side].loc:=LOC_REFERENCE;
            p.funcretloc[side].size:=retcgsize;
          end;
     end;


    function tarmparamanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,callerside,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);
        if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,callerside,varargspara,curintreg,curfloatreg,curmmreg,cur_stack_offset)
        else
          internalerror(200410231);
      end;

begin
   paramanager:=tarmparamanager.create;
end.
