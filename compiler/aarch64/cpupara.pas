{
    Copyright (c) 2003-2012 by Florian Klaempfl and others

    AArch64 specific calling conventions

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
{ AArch64 specific calling conventions are handled by this unit
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,globals,
       aasmtai,aasmdata,
       cpuinfo,cpubase,cgbase,cgutils,
       symconst,symbase,symtype,symdef,parabase,paramgr;

    type
       taarch64paramanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function ret_in_param(def:tdef;pd:tabstractprocdef):boolean;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
          function get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
         private
          procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; var sparesinglereg: tregister);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
            var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; var sparesinglereg: tregister; isvariadic: boolean):longint;
       end;

  implementation

    uses
       verbose,systems,cutils,
       rgobj,
       defutil,symsym,symtable;


    function taarch64paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_INTREGISTERS
      end;


    function taarch64paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[];
      end;


    function taarch64paramanager.get_volatile_registers_mm(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=VOLATILE_MMREGISTERS;
      end;


    function is_hfa_internal(p: tdef; var basedef: tdef; var elecount: longint): boolean;
      var
        i: longint;
        sym: tsym;
        tmpelecount: longint;
      begin
        result:=false;
        case p.typ of
          arraydef:
            begin
              if is_special_array(p) then
                exit;
              case tarraydef(p).elementdef.typ of
                floatdef:
                  begin
                    { an array of empty records has no influence }
                    if tarraydef(p).elementdef.size=0 then
                      begin
                        result:=true;
                        exit
                      end;
                    tmpelecount:=0;
                    if not is_hfa_internal(tarraydef(p).elementdef,basedef,tmpelecount) then
                      exit;
                    { tmpelecount now contains the number of hfa elements in a
                      single array element (e.g. 2 if it's an array of a record
                      containing two singles) -> multiply by number of elements
                      in the array }
                    inc(elecount,tarraydef(p).elecount*tmpelecount);
                    if elecount>4 then
                      exit;
                  end;
                else
                  result:=is_hfa_internal(tarraydef(p).elementdef,basedef,elecount);
                end;
            end;
          floatdef:
            begin
              if not assigned(basedef) then
                basedef:=p
              else if basedef<>p then
                exit;
              inc(elecount);
              result:=true;
            end;
          recorddef,
          objectdef:
            begin
              if (p.typ=objectdef) and
                 not is_object(p) then
                exit;
              for i:=0 to tabstractrecorddef(p).symtable.symlist.count-1 do
                begin
                  sym:=tsym(tabstractrecorddef(p).symtable.symlist[i]);
                  if sym.typ<>fieldvarsym then
                    continue;
                  if not is_hfa_internal(tfieldvarsym(sym).vardef,basedef,elecount) then
                    exit
                end;
              result:=true;
            end;
          else
            exit
        end;
      end;


    { Returns whether a def is a "homogeneous float array" at the machine level.
      This means that in the memory layout, the def only consists of maximally
      4 floating point values that appear consecutively in memory }
    function is_hfa(p: tdef; out basedef: tdef) : boolean;
      var
        elecount: longint;
      begin
        result:=false;
        basedef:=nil;
        elecount:=0;
        result:=is_hfa_internal(p,basedef,elecount);
        result:=
          result and
          (elecount>0) and
          (p.size=basedef.size*elecount)
      end;


    function getparaloc(calloption : tproccalloption; p : tdef; isvariadic: boolean) : tcgloc;
      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.typ of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              getparaloc:=LOC_MMREGISTER;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            formaldef:
              getparaloc:=LOC_REGISTER;
            classrefdef:
              getparaloc:=LOC_REGISTER;
            recorddef:
              getparaloc:=LOC_REGISTER;
            objectdef:
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
              getparaloc:=LOC_REGISTER;
            { avoid problems with errornous definitions }
            errordef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;


    function taarch64paramanager.push_addr_param(varspez: tvarspez; def :tdef; calloption: tproccalloption): boolean;
      var
        hfabasedef: tdef;
      begin
        result:=false;
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          objectdef:
            result:=
              is_object(def) and
              not is_hfa(def,hfabasedef) and
              (def.size>16);
          recorddef:
            { ABI: any composite > 16 bytes that not a hfa/hva
              Special case: MWPascal, which passes all const parameters by
                reference for compatibility reasons
            }
            result:=
              ((varspez=vs_const) and
               (calloption=pocall_mwpascal)) or
              (not is_hfa(def,hfabasedef) and
               (def.size>16));
          variantdef,
          formaldef:
            result:=true;
          { arrays are composites and hence treated the same as records by the
            ABI (watch out for C, where an array is a pointer) }
          arraydef:
            result:=
              (calloption in cdecl_pocalls) or
              is_open_array(def) or
              is_array_of_const(def) or
              is_array_constructor(def) or
              ((tarraydef(def).highrange>=tarraydef(def).lowrange) and
               not is_hfa(def,hfabasedef) and
               (def.size>16));
          setdef :
            result:=def.size>16;
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
        end;
      end;


    function taarch64paramanager.ret_in_param(def:tdef;pd:tabstractprocdef):boolean;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;
        { ABI: if the parameter would be passed in registers, it is returned
            in those registers; otherwise, it's returned by reference }
        result:=push_addr_param(vs_value,def,pd.proccalloption);
      end;


    procedure taarch64paramanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; var sparesinglereg: tregister);
      begin
        curintreg:=RS_R0;
        curfloatreg:=RS_F0;
        curmmreg:=RS_D0;
        cur_stack_offset:=0;
        sparesinglereg := NR_NO;
      end;


    function taarch64paramanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
        var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; var sparesinglereg: tregister; isvariadic: boolean):longint;

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
        firstparaloc: boolean;

      procedure assignintreg;
        begin
          { In case of po_delphi_nested_cc, the parent frame pointer
            is always passed on the stack. }
           if (nextintreg<=RS_R3) and
              (not(vo_is_parentfp in hp.varoptions) or
               not(po_delphi_nested_cc in p.procoptions)) then
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
            paradef:=hp.vardef;

            hp.paraloc[side].reset;

            { currently only support C-style array of const,
              there should be no location assigned to the vararg array itself }
            if (p.proccalloption in cstylearrayofconst) and
               is_array_of_const(paradef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_R0;
                paraloc^.size:=OS_ADDR;
                break;
              end;

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              begin
                paradef:=getpointerdef(paradef);
                loc:=LOC_REGISTER;
                paracgsize := OS_ADDR;
                paralen := tcgsize2size[OS_ADDR];
              end
            else
              begin
                if not is_special_array(paradef) then
                  paralen := paradef.size
                else
                  paralen := tcgsize2size[def_cgsize(paradef)];
                loc := getparaloc(p.proccalloption,paradef,isvariadic);
                if (paradef.typ in [objectdef,arraydef,recorddef]) and
                  not is_special_array(paradef) and
                  (hp.varspez in [vs_value,vs_const]) then
                  paracgsize := int_cgsize(paralen)
                else
                  begin
                    paracgsize:=def_cgsize(paradef);
                    { for things like formaldef }
                    if (paracgsize=OS_NO) then
                      begin
                        paracgsize:=OS_ADDR;
                        paralen:=tcgsize2size[OS_ADDR];
                        paradef:=voidpointertype;
                      end;
                  end
              end;

             hp.paraloc[side].size:=paracgsize;
             hp.paraloc[side].Alignment:=std_param_align;
             hp.paraloc[side].intsize:=paralen;
             hp.paraloc[side].def:=paradef;
             firstparaloc:=true;

{$ifdef EXTDEBUG}
             if paralen=0 then
               internalerror(200410311);
{$endif EXTDEBUG}
             while paralen>0 do
               begin
                 paraloc:=hp.paraloc[side].add_location;

                 if (loc=LOC_REGISTER) and (paracgsize in [OS_F32,OS_F64,OS_F80]) then
                   case paracgsize of
                     OS_F32:
                       paraloc^.size:=OS_32;
                     OS_F64:
                       paraloc^.size:=OS_32;
                     else
                       internalerror(2005082901);
                   end
                 else if (paracgsize in [OS_NO,OS_64,OS_S64]) then
                   paraloc^.size := OS_32
                 else
                   paraloc^.size:=paracgsize;
                 case loc of
                    LOC_REGISTER:
                      begin
                        { align registers for eabi }
                        if (target_info.abi in [abi_eabi,abi_eabihf]) and
                           firstparaloc and
                           (paradef.alignment=8) then
                          begin
                            if (nextintreg in [RS_R1,RS_R3]) then
                              inc(nextintreg)
                            else if nextintreg>RS_R3 then
                              stack_offset:=align(stack_offset,8);
                          end;
                        { this is not abi compliant
                          why? (FK) }
                        if nextintreg<=RS_R3 then
                          begin
                            paraloc^.loc:=LOC_REGISTER;
                            paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
                            inc(nextintreg);
                          end
                        else
                          begin
                            { LOC_REFERENCE always contains everything that's left }
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
                    LOC_MMREGISTER:
                      begin
                        if (nextmmreg<=RS_D7) or
                           ((paraloc^.size = OS_F32) and
                            (sparesinglereg<>NR_NO)) then
                          begin
                            paraloc^.loc:=LOC_MMREGISTER;
                            case paraloc^.size of
                              OS_F32:
                                if sparesinglereg = NR_NO then 
                                  begin     
                                    paraloc^.register:=newreg(R_MMREGISTER,nextmmreg,R_SUBFS);
                                    sparesinglereg:=newreg(R_MMREGISTER,nextmmreg-RS_S0+RS_S1,R_SUBFS);
                                    inc(nextmmreg);
                                  end
                                else
                                  begin
                                    paraloc^.register:=sparesinglereg;
                                    sparesinglereg := NR_NO;
                                  end;
                              OS_F64:
                                begin
                                  paraloc^.register:=newreg(R_MMREGISTER,nextmmreg,R_SUBFD);
                                  inc(nextmmreg);
                                end;
                              else
                                internalerror(2012031601);
                            end;
                          end
                        else
                          begin
                            { once a floating point parameters has been placed
                            on the stack we must not pass any more in vfp regs
                            even if there is a single precision register still
                            free}
                            sparesinglereg := NR_NO;
                            { LOC_REFERENCE always contains everything that's left }
                            paraloc^.loc:=LOC_REFERENCE;
                            paraloc^.size:=int_cgsize(paralen);
                            if (side=callerside) then
                              paraloc^.reference.index:=NR_STACK_POINTER_REG;
                            paraloc^.reference.offset:=stack_offset;
                            inc(stack_offset,align(paralen,4));
                            paralen:=0;
                         end;
                      end;
                    LOC_REFERENCE:
                      begin
                        if push_addr_param(hp.varspez,paradef,p.proccalloption) then
                          begin
                            paraloc^.size:=OS_ADDR;
                            assignintreg
                          end
                        else
                          begin
                            { align stack for eabi }
                            if (target_info.abi in [abi_eabi,abi_eabihf]) and
                               firstparaloc and
                               (paradef.alignment=8) then
                              stack_offset:=align(stack_offset,8);

                             paraloc^.size:=paracgsize;
                             paraloc^.loc:=LOC_REFERENCE;
                             paraloc^.reference.index:=NR_STACK_POINTER_REG;
                             paraloc^.reference.offset:=stack_offset;
                             inc(stack_offset,align(paralen,4));
                             paralen:=0
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
                         { on non-Darwin, the framepointer contains the value
                           of the stack pointer on entry. On Darwin, the
                           framepointer points to the previously saved
                           framepointer (which is followed only by the saved
                           return address -> framepointer + 4 = stack pointer
                           on entry }
                         if not(target_info.system in systems_darwin) then
                           inc(paraloc^.reference.offset,4)
                         else
                           inc(paraloc^.reference.offset,8);
                       end;
                   end;
                 dec(paralen,tcgsize2size[paraloc^.size]);
                 firstparaloc:=false
               end;
          end;
        curintreg:=nextintreg;
        curfloatreg:=nextfloatreg;
        curmmreg:=nextmmreg;
        cur_stack_offset:=stack_offset;
        result:=cur_stack_offset;
      end;


    function  taarch64paramanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
      begin
         if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
           exit;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if result.def.typ=floatdef then
          begin
            if target_info.abi = abi_eabihf then 
              begin
                paraloc^.loc:=LOC_MMREGISTER;
                case retcgsize of
                  OS_64,
                  OS_F64:
                    begin
                      paraloc^.register:=NR_MM_RESULT_REG;
                    end;
                  OS_32,
                  OS_F32:
                    begin
                      paraloc^.register:=NR_S0;
                    end;
                  else
                    internalerror(2012032501);
                end;
                paraloc^.size:=retcgsize;
              end
            else if (p.proccalloption in [pocall_softfloat]) or
               (cs_fp_emulation in current_settings.moduleswitches) or
               (current_settings.fputype in [fpu_vfpv2,fpu_vfpv3,fpu_vfpv3_d16,fpu_fpv4_s16]) then
              begin
                case retcgsize of
                  OS_64,
                  OS_F64:
                    begin
                      paraloc^.loc:=LOC_REGISTER;
                      if target_info.endian = endian_big then
                        paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
                      else
                        paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG;
                      paraloc^.size:=OS_32;
                      paraloc:=result.add_location;
                      paraloc^.loc:=LOC_REGISTER;
                      if target_info.endian = endian_big then
                        paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
                      else
                        paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG;
                      paraloc^.size:=OS_32;
                    end;
                  OS_32,
                  OS_F32:
                    begin
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=NR_FUNCTION_RETURN_REG;
                      paraloc^.size:=OS_32;
                    end;
                  else
                    internalerror(2005082603);
                end;
              end
            else
              begin
                paraloc^.loc:=LOC_FPUREGISTER;
                paraloc^.register:=NR_FPU_RESULT_REG;
                paraloc^.size:=retcgsize;
              end;
          end
          { Return in register }
        else
          begin
            if retcgsize in [OS_64,OS_S64] then
              begin
                paraloc^.loc:=LOC_REGISTER;
                if target_info.endian = endian_big then
                  paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
                else
                  paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG;
                paraloc^.size:=OS_32;
                paraloc:=result.add_location;
                paraloc^.loc:=LOC_REGISTER;
                if target_info.endian = endian_big then
                  paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
                else
                  paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG;
                paraloc^.size:=OS_32;
              end
            else
              begin
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_FUNCTION_RETURN_REG;
                if (result.intsize<>3) then
                  paraloc^.size:=retcgsize
                else
                  paraloc^.size:=OS_32;
              end;
          end;
      end;


    function taarch64paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
        sparesinglereg:tregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset,sparesinglereg);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset,sparesinglereg,false);

        create_funcretloc_info(p,side);
     end;


    function taarch64paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
        sparesinglereg:tregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset,sparesinglereg);

        result:=create_paraloc_info_intern(p,callerside,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset,sparesinglereg,true);
        if (p.proccalloption in cstylearrayofconst) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,callerside,varargspara,curintreg,curfloatreg,curmmreg,cur_stack_offset,sparesinglereg,true)
        else
          internalerror(200410231);
      end;

begin
   paramanager:=taarch64paramanager.create;
end.
