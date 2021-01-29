{
    Copyright (c) 2002 by Florian Klaempfl

    Xtensa specific calling conventions

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
       paramgr,parabase,cgbase,cgutils;

    type
       tcpuparamanager = class(tparamanager)
         function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
         function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
         function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;

         function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
         function create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;override;
         function get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
         function ret_in_param(def: tdef; pd: tabstractprocdef): boolean;override;
         function param_use_paraloc(const cgpara: tcgpara): boolean;override;
       private
         { the max. register depends on the used call instruction }
         maxintreg : TSuperRegister;
         procedure init_values(p: tabstractprocdef; side: tcallercallee; var curintreg: tsuperregister; var cur_stack_offset: aword);
         function create_paraloc_info_intern(p : tabstractprocdef; side : tcallercallee;
           paras : tparalist; var curintreg : tsuperregister;
           var cur_stack_offset : aword; varargsparas : boolean) : longint;
         function create_paraloc1_info_intern(p: tabstractprocdef; side: tcallercallee; paradef: tdef; var loc: TCGPara; varspez: tvarspez; varoptions: tvaroptions;
           var curintreg: tsuperregister; var cur_stack_offset: aword; varargsparas, funcret: boolean): longint;
       end;

  implementation

    uses
       cpuinfo,globals,
       verbose,systems,
       defutil,
       symtable,symcpu,
       procinfo,cpupi;


    function tcpuparamanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        { we have actually to check what calling instruction is used, but we do not handle this,
          instead CALL(X)8 is used always }
        if target_info.abi=abi_xtensa_windowed then
          result:=[RS_A8..RS_A15]
        else
          result:=[RS_A0..RS_A11];
      end;


    function tcpuparamanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[RS_F0..RS_F15];
      end;


    function getparaloc(p : tdef) : tcgloc;
      begin
        case p.typ of
          orddef,
          floatdef,
          enumdef,
          pointerdef,
          formaldef,
          classrefdef,
          procvardef,
          recorddef,
          objectdef,
          stringdef,
          filedef,
          arraydef,
          setdef,
          variantdef,
          { avoid problems with errornous definitions }
          errordef:
            result:=LOC_REGISTER;
          else
            internalerror(2020082501);
        end;
      end;


    function tcpuparamanager.param_use_paraloc(const cgpara: tcgpara): boolean;
      begin
        { we always set up a stack frame -> we can always access the parameters
          this way }
        result:=
          (cgpara.location^.loc=LOC_REFERENCE) and
          not assigned(cgpara.location^.next);
      end;


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
          formaldef :
            result:=true;
          recorddef :
            result:=(varspez = vs_const);
          arraydef:
            result:=true;
          objectdef :
            result:=is_object(def) and (varspez = vs_const);
          variantdef,
          setdef :
            result:=(varspez = vs_const);
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
          else
            ;
        end;
      end;


    procedure tcpuparamanager.init_values(p : tabstractprocdef; side : tcallercallee; var curintreg: tsuperregister; var cur_stack_offset: aword);
      begin
        cur_stack_offset:=0;
        case target_info.abi of
          abi_xtensa_windowed:
            begin
              if side=calleeside then
                begin
                  curintreg:=RS_A2;
                  maxintreg:=RS_A7;
                  if current_procinfo.framepointer=NR_STACK_POINTER_REG then
                    cur_stack_offset:=(p as tcpuprocdef).total_stackframe_size;
                end
              else
                begin
                  { we use CALL(X)8 only so far }
                  curintreg:=RS_A10;
                  maxintreg:=RS_A15;
                end;
            end;
          abi_xtensa_call0:
            begin
              curintreg:=RS_A2;
              maxintreg:=RS_A7;
              if (side=calleeside) and (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
                cur_stack_offset:=(p as tcpuprocdef).total_stackframe_size;
            end;
          else
            Internalerror(2020031404);
        end;
      end;


    function tcpuparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
        cur_stack_offset: aword;
        curintreg: tsuperregister;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          exit;

        paraloc:=result.add_location;
        if retcgsize in [OS_64,OS_S64,OS_F64] then
          begin
            { low 32bits }
            paraloc^.loc:=LOC_REGISTER;
            paraloc^.size:=OS_32;
            paraloc^.def:=u32inttype;
            if side=callerside then
              case target_info.abi of
                abi_xtensa_call0:
              paraloc^.register:=NR_A2;
                abi_xtensa_windowed:
                  { only call8 used/supported so far }
                  paraloc^.register:=newreg(R_INTREGISTER,RS_A10,cgsize2subreg(R_INTREGISTER,retcgsize));
                else
                  Internalerror(2020032201);
              end
            else
              paraloc^.register:=NR_A2;

            { high 32bits }
            paraloc:=result.add_location;
            paraloc^.loc:=LOC_REGISTER;
            paraloc^.size:=OS_32;
            paraloc^.def:=u32inttype;
            if side=callerside then
              case target_info.abi of
                abi_xtensa_call0:
                  paraloc^.register:=NR_A3;
                abi_xtensa_windowed:
                  { only call8 used/supported so far }
                  paraloc^.register:=newreg(R_INTREGISTER,RS_A11,cgsize2subreg(R_INTREGISTER,retcgsize));
                else
                  Internalerror(2020032204);
              end
            else
              paraloc^.register:=NR_A3;
          end
        else if (result.def.size>4) and (result.def.size<=16) then
          begin
            init_values(p,side,curintreg,cur_stack_offset);
            create_paraloc1_info_intern(p,side,result.def,result,vs_value,[],curintreg,cur_stack_offset,false,true);

            { check if everything is ok }
            if result.location^.loc=LOC_INVALID then
              Internalerror(2020082901);
          end
        else
          begin
            paraloc^.loc:=LOC_REGISTER;
            if side=callerside then
              case target_info.abi of
                abi_xtensa_call0:
                  paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
                abi_xtensa_windowed:
                  { only call8 used/supported so far }
                  paraloc^.register:=newreg(R_INTREGISTER,RS_A10,cgsize2subreg(R_INTREGISTER,retcgsize));
                else
                  Internalerror(2020031502);
              end
            else
              paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
            paraloc^.size:=OS_32;
            paraloc^.def:=result.def;
          end;
      end;


    function tcpuparamanager.ret_in_param(def:tdef;pd:tabstractprocdef):boolean;
      var
        i: longint;
        sym: tsym;
        basedef: tdef;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;
        case def.typ of
          arraydef,
          objectdef,
          stringdef,
          setdef,
          recorddef:
            result:=def.size>16;
          floatdef,
          variantdef,
          procvardef:
            result:=false
          else
            result:=inherited ret_in_param(def,pd);
        end;
      end;


    function tcpuparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg: tsuperregister;
      begin
        init_values(p,side,curintreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,cur_stack_offset,false);
        if result<cur_stack_offset then
          Internalerror(2020083002);

        create_funcretloc_info(p,side);
      end;


    function tcpuparamanager.create_paraloc1_info_intern(p : tabstractprocdef; side: tcallercallee; paradef:tdef;var loc : TCGPara;varspez : tvarspez;varoptions : tvaroptions;
      var curintreg: tsuperregister; var cur_stack_offset: aword; varargsparas, funcret: boolean):longint;
      var
        paralen: aint;
        locdef,
        fdef : tdef;
        paraloc : pcgparalocation;
        paracgsize: tcgsize;
        firstparaloc: boolean;
        locpara: TCGLoc;
      begin
{$ifdef extdebug}
         if po_explicitparaloc in p.procoptions then
           internalerror(200411141);
{$endif extdebug}
        loc.reset;
        { currently only support C-style array of const }
        if (p.proccalloption in cstylearrayofconst) and
           is_array_of_const(paradef) then
          begin
            paraloc:=loc.add_location;
            { hack: the paraloc must be valid, but is not actually used }
            paraloc^.loc := LOC_REGISTER;
            paraloc^.register := NR_A2;
            paraloc^.size := OS_ADDR;
            paraloc^.def:=voidpointertype;
            result:=cur_stack_offset;
            exit;
          end;

        if not is_special_array(paradef) then
          paralen:=paradef.size
        else
          paralen:=tcgsize2size[def_cgsize(paradef)];

        if (not(funcret) and push_addr_param(varspez,paradef,p.proccalloption)) or
          (funcret and (paralen>24)) then
          begin
            paradef:=cpointerdef.getreusable_no_free(paradef);
            locpara:=LOC_REGISTER;
            paracgsize:=OS_ADDR;
            paralen:=tcgsize2size[OS_ADDR];
          end
        else
          begin
            if (paradef.typ in [objectdef,arraydef,recorddef,setdef,stringdef]) and
               not is_special_array(paradef) and
               (varspez in [vs_value,vs_const]) then
              paracgsize:=int_cgsize(paralen)
            else
              begin
                paracgsize:=def_cgsize(paradef);
                if paracgsize=OS_NO then
                  begin
                    paracgsize:=OS_ADDR;
                    paralen:=tcgsize2size[OS_ADDR];
                    paradef:=voidpointertype;
                  end;
              end;
          end;

        locpara:=getparaloc(paradef);

        if (maxintreg-curintreg+1)*4<paralen then
          begin
            locpara:=LOC_REFERENCE;
            curintreg:=maxintreg+1;
          end;

        loc.alignment:=std_param_align;
        loc.size:=paracgsize;
        loc.intsize:=paralen;
        loc.def:=paradef;
        if (locpara=LOC_REGISTER) and (paradef.alignment>4) and
           odd(curintreg-RS_A2) then
          inc(curintreg);
        if (paralen = 0) then
          if (paradef.typ = recorddef) then
            begin
              paraloc:=loc.add_location;
              paraloc^.loc := LOC_VOID;
            end
          else
            internalerror(2020031407);
        locdef:=paradef;
        firstparaloc:=true;
        { can become < 0 for e.g. 3-byte records }
        while paralen>0 do
          begin
            paraloc:=loc.add_location;
            { In case of po_delphi_nested_cc, the parent frame pointer
              is always passed on the stack. }
            if (locpara=LOC_REGISTER) and
               (curintreg<=maxintreg) and
               (not(vo_is_parentfp in varoptions) or
                not(po_delphi_nested_cc in p.procoptions)) then
              begin
                paraloc^.loc:=locpara;
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
                paraloc^.register:=newreg(R_INTREGISTER,curintreg,R_SUBNONE);
                inc(curintreg);
                dec(paralen,tcgsize2size[paraloc^.size]);
              end
            else { LOC_REFERENCE }
              begin
                 paraloc^.loc:=LOC_REFERENCE;
                 case locpara of
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
                     internalerror(2020031405);
                 end;
                 if side = callerside then
                   paraloc^.reference.index:=NR_STACK_POINTER_REG
                 else
                   paraloc^.reference.index:=current_procinfo.framepointer;

                 cur_stack_offset:=align(cur_stack_offset,paradef.alignment);
                 paraloc^.reference.offset:=cur_stack_offset;

                 inc(cur_stack_offset,align(paralen,4));
                 while (paralen > 0) and
                       (curintreg < maxintreg) do
                    begin
                      inc(curintreg);
                      dec(paralen,sizeof(pint));
                    end;
                 paralen := 0;
              end;
            firstparaloc:=false;
          end;
         result:=cur_stack_offset;
      end;


    function tcpuparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras:tparalist;
      var curintreg: tsuperregister; var cur_stack_offset: aword; varargsparas: boolean):longint;
      var
        i : integer;
      begin
        result:=0;
        for i:=0 to paras.count-1 do
          result:=create_paraloc1_info_intern(p,side,tparavarsym(paras[i]).vardef,tparavarsym(paras[i]).paraloc[side],tparavarsym(paras[i]).varspez,
            tparavarsym(paras[i]).varoptions,curintreg,cur_stack_offset,false,false);
      end;


    function tcpuparamanager.create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        parasize, l: longint;
        curintreg: tsuperregister;
        i : integer;
        hp: tparavarsym;
        paraloc: pcgparalocation;
      begin
        init_values(p,side,curintreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,cur_stack_offset, false);
        if (p.proccalloption in cstylearrayofconst) then
          { just continue loading the parameters in the registers }
          begin
            if assigned(varargspara) then
              begin
                if side=callerside then
                  result:=create_paraloc_info_intern(p,side,varargspara,curintreg,cur_stack_offset,true)
                else
                  internalerror(2020030704);
              end;
           end
        else
          internalerror(2020030703);
        create_funcretloc_info(p,side);
      end;

begin
   paramanager:=tcpuparamanager.create;
end.
