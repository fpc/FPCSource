{
    Copyright (c) 2002 by Florian Klaempfl

    Risc-V32 specific calling conventions

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
       aasmtai,aasmdata,
       cpubase,
       symconst,symtype,symdef,symsym,
       paramgr,parabase,cgbase,cgutils,
       pararv;

    type
       tcpuparamanager = class(trvparamanager)
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;

          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;override;
          function get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
         private
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras:tparalist;
              var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; varargsparas: boolean):longint;
       end;

  implementation

    uses
       cpuinfo,globals,
       verbose,systems,
       defutil,symtable,
       procinfo,cpupi;


    function tcpuparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out,constref always require address }
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          variantdef,
          formaldef :
            result:=true;
          { regular procvars must be passed by value, because you cannot pass
            the address of a local stack location when calling e.g.
            pthread_create with the address of a function (first of all it
            expects the address of the function to execute and not the address
            of a memory location containing that address, and secondly if you
            first store the address on the stack and then pass the address of
            this stack location, then this stack location may no longer be
            valid when the newly started thread accesses it.

            However, for "procedure of object" we must use the same calling
            convention as for "8 byte record" due to the need for
            interchangeability with the TMethod record type.
          }
          procvardef :
            result:=
              (def.size <> sizeof(pint));
          recorddef :
            result := (def.size > 8) or (varspez = vs_const);
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          objectdef :
            result:=is_object(def);
          setdef :
            result:=not is_smallset(def);
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
          else
            ;
        end;
      end;


    function tcpuparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          exit;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if (result.def.typ=floatdef) and
           (not ((cs_fp_emulation in current_settings.moduleswitches) or
                 (current_settings.fputype in [fpu_soft]))) then
          begin
            paraloc^.loc:=LOC_FPUREGISTER;
            paraloc^.register:=NR_FPU_RESULT_REG;
            paraloc^.size:=retcgsize;
            paraloc^.def:=result.def;
          end
        else
         { Return in register }
          begin
            if retcgsize in [OS_64,OS_S64] then
             begin
               { low 32bits }
               paraloc^.loc:=LOC_REGISTER;
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_HIGH_REG;
               paraloc^.size:=OS_32;
               paraloc^.def:=u32inttype;
               { high 32bits }
               paraloc:=result.add_location;
               paraloc^.loc:=LOC_REGISTER;
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_LOW_REG;
               paraloc^.size:=OS_32;
               paraloc^.def:=u32inttype;
             end
            else
             begin
               paraloc^.loc:=LOC_REGISTER;
               if side=callerside then
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
               paraloc^.size:=retcgsize;
               paraloc^.def:=result.def;
             end;
          end;
      end;


    function tcpuparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;

      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result := create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset,false);

        create_funcretloc_info(p,side);
      end;



    function tcpuparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras:tparalist;
               var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; varargsparas: boolean):longint;
      var
         stack_offset: longint;
         paralen: aint;
         nextintreg,nextfloatreg,nextmmreg, maxfpureg : tsuperregister;
         MaxIntReg : TSuperRegister;
         locdef,
         fdef,
         paradef : tdef;
         paraloc : pcgparalocation;
         i  : integer;
         hp : tparavarsym;
         loc : tcgloc;
         paracgsize: tcgsize;
         firstparaloc: boolean;

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
         maxfpureg := RS_F17;
         if CPURV_HAS_16REGISTERS in cpu_capabilities[current_settings.cputype] then
           MaxIntReg := RS_X15 
         else
           MaxIntReg := RS_X17;

          for i:=0 to paras.count-1 do
            begin
              hp:=tparavarsym(paras[i]);
              paradef := hp.vardef;

              hp.paraloc[side].reset;
              { currently only support C-style array of const }
              if (p.proccalloption in cstylearrayofconst) and
                 is_array_of_const(paradef) then
                begin
                  paraloc:=hp.paraloc[side].add_location;
                  { hack: the paraloc must be valid, but is not actually used }
                  paraloc^.loc := LOC_REGISTER;
                  paraloc^.register := NR_X0;
                  paraloc^.size := OS_ADDR;
                  paraloc^.def:=voidpointertype;
                  break;
                end;

              if push_addr_param(hp.varspez,paradef,p.proccalloption) then
                begin
                  paradef:=cpointerdef.getreusable_no_free(paradef);
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
                  paracgsize:=def_cgsize(paradef);
                  { for things like formaldef }
                  if (paracgsize=OS_NO) then
                    begin
                      paracgsize:=OS_ADDR;
                      paralen := tcgsize2size[OS_ADDR];
                    end;
                end;

              loc := getparaloc(paradef);

              hp.paraloc[side].alignment:=std_param_align;
              hp.paraloc[side].size:=paracgsize;
              hp.paraloc[side].intsize:=paralen;
              hp.paraloc[side].def:=paradef;
{$ifndef cpu64bitaddr}
              if (is_64bit(paradef)) and
                 odd(nextintreg-RS_X10) then
                inc(nextintreg);
{$endif not cpu64bitaddr}
              if (paralen = 0) then
                if (paradef.typ = recorddef) then
                  begin
                    paraloc:=hp.paraloc[side].add_location;
                    paraloc^.loc := LOC_VOID;
                  end
                else
                  internalerror(2005011310);
              locdef:=paradef;
              firstparaloc:=true;
              { can become < 0 for e.g. 3-byte records }
              while (paralen > 0) do
                begin
                  paraloc:=hp.paraloc[side].add_location;
                  { In case of po_delphi_nested_cc, the parent frame pointer
                    is always passed on the stack. }
                  if (loc = LOC_REGISTER) and
                     (nextintreg <= MaxIntReg) and
                     (not(vo_is_parentfp in hp.varoptions) or
                      not(po_delphi_nested_cc in p.procoptions)) then
                    begin
                      paraloc^.loc := loc;
                      { make sure we don't lose whether or not the type is signed }
                      if (paradef.typ<>orddef) then
                        begin
                          paracgsize:=int_cgsize(paralen);
                          locdef:=get_paraloc_def(paradef,paralen,firstparaloc);
                        end;
                      if (paracgsize in [OS_NO,OS_64,OS_S64,OS_128,OS_S128]) then
                        begin
                          paraloc^.size:=OS_INT;
                          paraloc^.def:=u32inttype;
                        end
                      else
                        begin
                          paraloc^.size:=paracgsize;
                          paraloc^.def:=locdef;
                        end;
                      paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                      inc(nextintreg);
                      dec(paralen,tcgsize2size[paraloc^.size]);
                    end
                  else if (loc = LOC_FPUREGISTER) and
                          (nextintreg <= MaxIntReg) then
                    begin
                      paraloc^.loc:=loc;
                      paraloc^.size := paracgsize;
                      paraloc^.def := paradef;
                      paraloc^.register:=newreg(R_FPUREGISTER,nextintreg,R_SUBWHOLE);
                      inc(nextintreg);
                      dec(paralen,tcgsize2size[paraloc^.size]);
                    end
                  else { LOC_REFERENCE }
                    begin
                       paraloc^.loc:=LOC_REFERENCE;
                       case loc of
                         LOC_FPUREGISTER:
                           begin
                             paraloc^.size:=int_float_cgsize(paralen);
                             case paraloc^.size of
                               OS_F32: paraloc^.def:=s32floattype;
                               OS_F64: paraloc^.def:=s64floattype;
                               else
                                 internalerror(2013060124);
                             end;
                           end;
                         LOC_REGISTER,
                         LOC_REFERENCE:
                           begin
                             paraloc^.size:=int_cgsize(paralen);
                             if paraloc^.size<>OS_NO then
                               paraloc^.def:=cgsize_orddef(paraloc^.size)
                             else
                               paraloc^.def:=carraydef.getreusable_no_free(u8inttype,paralen);
                           end;
                         else
                           internalerror(2006011101);
                       end;
                       if (side = callerside) then
                         paraloc^.reference.index:=NR_STACK_POINTER_REG
                       else
                         begin
                           paraloc^.reference.index:=NR_FRAME_POINTER_REG;

                           { create_paraloc_info_intern might be also called when being outside of
                             code generation so current_procinfo might be not set }
                           if assigned(current_procinfo) then
                             trv32procinfo(current_procinfo).needs_frame_pointer := true;
                         end;

                       paraloc^.reference.offset:=stack_offset;

                       inc(stack_offset,align(paralen,4));
                       while (paralen > 0) and
                             (nextintreg <= MaxIntReg) do
                          begin
                            inc(nextintreg);
                            dec(paralen,sizeof(pint));
                          end;
                       paralen := 0;
                    end;
                  firstparaloc:=false;
                end;
            end;
         curintreg:=nextintreg;
         curfloatreg:=nextfloatreg;
         curmmreg:=nextmmreg;
         cur_stack_offset:=stack_offset;
         result:=stack_offset;
      end;


    function tcpuparamanager.create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;
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

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset, false);
        if (p.proccalloption in cstylearrayofconst) then
          { just continue loading the parameters in the registers }
          begin
            if assigned(varargspara) then
              begin
                if side=callerside then
                  result:=create_paraloc_info_intern(p,side,varargspara,curintreg,curfloatreg,curmmreg,cur_stack_offset,true)
                else
                  internalerror(2019021919);
                if curfloatreg<>firstfloatreg then
                  include(varargspara.varargsinfo,va_uses_float_reg);
              end;
           end
        else
          internalerror(2019021912);
        create_funcretloc_info(p,side);
      end;

begin
   paramanager:=tcpuparamanager.create;
end.
