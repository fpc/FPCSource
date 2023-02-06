{
    Copyright (c) 2002 by Florian Klaempfl

    LoongArch64 specific calling conventions

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

{$I fpcdefs.inc}

  interface

    uses
      globtype,
      aasmtai,aasmdata,
      cpubase,
      symconst, symtype, symdef, symsym,
      paramgr, parabase, cgbase, cgutils;

    type
      tcpuparamanager = class(tparamanager)
        function get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset; override;
        function get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset; override;
        function push_addr_param(varspez: tvarspez; def: tdef; calloption: tproccalloption): boolean; override;
        function ret_in_param(def: tdef; pd: tabstractprocdef): boolean; override;

        procedure getcgtempparaloc(list: TAsmList; pd : tabstractprocdef; nr: longint; var cgpara: tcgpara); override;
        function create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint; override;
        function create_varargs_paraloc_info(p: tabstractprocdef; side: tcallercallee; varargspara: tvarargsparalist): longint; override;
        function get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;

      private
        procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
        function create_paraloc_info_intern(p: tabstractprocdef; side: tcallercallee; paras: tparalist; var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; isVararg : boolean): longint;
        function parseparaloc(p: tparavarsym; const s: string): boolean; override;
        procedure create_paraloc_for_def(var para: TCGPara; varspez: tvarspez; paradef: tdef; var nextfloatreg, nextintreg: tsuperregister; var stack_offset: aword; const isVararg, forceintmem: boolean; const side: tcallercallee; const p: tabstractprocdef);
      end;

implementation

    uses
      verbose, systems,
      globals, cpuinfo,
      defutil,symtable,symcpu,
      procinfo, cpupi;

    function tcpuparamanager.get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=[RS_R0..RS_R31]-[RS_R3,RS_R22..RS_R31];
      end;

    function tcpuparamanager.get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=[RS_F0..RS_F31]-[RS_F24..RS_F31];
      end;

    procedure tcpuparamanager.getcgtempparaloc(list: TAsmList; pd : tabstractprocdef; nr: longint; var cgpara: tcgpara);
      var
        paraloc: pcgparalocation;
        psym: tparavarsym;
        pdef: tdef;
      begin
        psym:=tparavarsym(pd.paras[nr-1]);
        pdef:=psym.vardef;
        if push_addr_param(psym.varspez,pdef,pd.proccalloption) then
          pdef:=cpointerdef.getreusable_no_free(pdef);
        cgpara.reset;
        cgpara.size := def_cgsize(pdef);
        cgpara.intsize := tcgsize2size[cgpara.size];
        cgpara.alignment := get_para_align(pd.proccalloption);
        cgpara.def:=pdef;
        paraloc := cgpara.add_location;
        with paraloc^ do
          begin
            size := def_cgsize(pdef);
            def := pdef;
            if (nr <= 8) then
              begin
                if (nr = 0) then
                  internalerror(2022111916);
                loc := LOC_REGISTER;
                register := newreg(R_INTREGISTER, RS_R4 + nr-1, R_SUBWHOLE);
              end
            else
              begin
                loc := LOC_REFERENCE;
                paraloc^.reference.index := NR_STACK_POINTER_REG;
                reference.offset := sizeof(aint) * (nr - 9);
              end;
          end;
      end;

    function getparaloc(p: tdef): tcgloc;
      var
        hfabasedef: tdef;
      begin
        { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
          if push_addr_param for the def is true
        }
        case p.typ of
          orddef:
            result := LOC_REGISTER;
          floatdef:
            result := LOC_FPUREGISTER;
          enumdef:
            result := LOC_REGISTER;
          pointerdef:
            result := LOC_REGISTER;
          formaldef:
            result := LOC_REGISTER;
          classrefdef:
            result := LOC_REGISTER;
          procvardef:
            result := LOC_REGISTER;
          recorddef:
            result := LOC_REGISTER;
            { result := LOC_FPUREGISTER; }
          objectdef:
            if is_object(p) then
              result := LOC_REFERENCE
            else
              result := LOC_REGISTER;
          stringdef:
            if is_shortstring(p) or is_longstring(p) then
              result := LOC_REFERENCE
            else
              result := LOC_REGISTER;
          filedef:
            result := LOC_REGISTER;
          arraydef:
            if is_dynamic_array(p) then
              getparaloc:=LOC_REGISTER
            else
              result := LOC_REFERENCE;
          setdef:
            if is_smallset(p) then
              result := LOC_REGISTER
            else
              result := LOC_REFERENCE;
          variantdef:
            result := LOC_REFERENCE;
          { avoid problems with errornous definitions }
          errordef:
            result := LOC_REGISTER;
        else
          internalerror(2022111917);
        end;
      end;

    function tcpuparamanager.push_addr_param(varspez: tvarspez; def: tdef; calloption: tproccalloption): boolean;
      begin
        result := false;
        { var,out,constref always require address }
        if varspez in [vs_var, vs_out, vs_constref] then
        begin
          result := true;
          exit;
        end;
        case def.typ of
          variantdef,
          formaldef:
            result := true;
          procvardef,
          recorddef:
            result := (def.size > 16);
          arraydef:
            result := (tarraydef(def).highrange >= tarraydef(def).lowrange) or
              is_open_array(def) or
              is_array_of_const(def) or
              is_array_constructor(def);
          objectdef:
            result := is_object(def);
          setdef:
            result := not is_smallset(def);
          stringdef:
            result := tstringdef(def).stringtype in [st_shortstring, st_longstring];
          else
            ;
        end;
      end;

    function tcpuparamanager.ret_in_param(def: tdef; pd: tabstractprocdef): boolean;
      var
        tmpdef: tdef;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;

        { general rule: passed in registers -> returned in registers }
        result:=push_addr_param(vs_value,def,pd.proccalloption);
      end;

    procedure tcpuparamanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      begin
        cur_stack_offset := 0;
        curintreg := RS_R4;
        curfloatreg := RS_F0;
        curmmreg := RS_NO;
      end;

    function tcpuparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc: pcgparalocation;
        retcgsize: tcgsize;
        nextfloatreg, nextintreg, nextmmreg: tsuperregister;
        stack_offset: aword;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          exit;

         { in this case, it must be returned in registers as if it were passed
           as the first parameter }
         init_values(nextintreg,nextfloatreg,nextmmreg,stack_offset);
         create_paraloc_for_def(result,vs_value,result.def,nextfloatreg,nextintreg,stack_offset,false,false,side,p);
         { sanity check (LOC_VOID for empty records) }
         if not assigned(result.location) or
            not(result.location^.loc in [LOC_REGISTER,LOC_FPUREGISTER,LOC_VOID]) then
           internalerror(2022111918);
      end;

    function tcpuparamanager.create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg : tsuperregister;
      begin
        init_values(curintreg, curfloatreg, curmmreg, cur_stack_offset);

        result := create_paraloc_info_intern(p, side, p.paras, curintreg, curfloatreg, curmmreg, cur_stack_offset, false);

        create_funcretloc_info(p, side);
      end;

    function tcpuparamanager.create_paraloc_info_intern(p: tabstractprocdef; side: tcallercallee; paras: tparalist; var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; isVararg : boolean): longint;
      var
        nextintreg, nextfloatreg, nextmmreg : tsuperregister;
        i: integer;
        hp: tparavarsym;
        paraloc: pcgparalocation;
        delphi_nestedfp: boolean;

      begin
{$IFDEF extdebug}
        if po_explicitparaloc in p.procoptions then
          internalerror(2022111919);
{$ENDIF extdebug}

        result := 0;
        nextintreg := curintreg;
        nextfloatreg := curfloatreg;
        nextmmreg := curmmreg;

        for i := 0 to paras.count - 1 do begin
          hp := tparavarsym(paras[i]);

          if (vo_has_explicit_paraloc in hp.varoptions) then begin
            internalerror(2022111920);
          end;

          { currently only support C-style array of const }
          if (p.proccalloption in [pocall_cdecl, pocall_cppdecl]) and
            is_array_of_const(hp.vardef) then begin
            paraloc := hp.paraloc[side].add_location;
            { hack: the paraloc must be valid, but is not actually used }
            paraloc^.loc := LOC_REGISTER;
            paraloc^.register := NR_R0;
            paraloc^.size := OS_ADDR;
            paraloc^.def := voidpointertype;
            break;
          end;
          delphi_nestedfp:=(vo_is_parentfp in hp.varoptions) and (po_delphi_nested_cc in p.procoptions);
          create_paraloc_for_def(hp.paraloc[side], hp.varspez, hp.vardef,
            nextfloatreg, nextintreg, cur_stack_offset, isVararg, delphi_nestedfp, side, p);
        end;

        curintreg := nextintreg;
        curfloatreg := nextfloatreg;
        curmmreg := nextmmreg;
        result := cur_stack_offset;
      end;

    procedure tcpuparamanager.create_paraloc_for_def(var para: TCGPara; varspez: tvarspez; paradef: tdef; var nextfloatreg, nextintreg: tsuperregister; var stack_offset: aword; const isVararg, forceintmem: boolean; const side: tcallercallee; const p: tabstractprocdef);
      var
        paracgsize: tcgsize;
        loc: tcgloc;
        paraloc: pcgparalocation;
        { def to use for all paralocs if <> nil }
        alllocdef,
        { def to use for the current paraloc }
        locdef,
        tmpdef,tmpdef1,tmpdef2: tdef;
        tmpdep1_size,tmpdep2_size,record_offset,para_same_num: integer;
        paralen: aint;
        firstparaloc,
        paraaligned: boolean;
      begin
        alllocdef:=nil;
        locdef:=nil;
        tmpdef1:=nil;
        tmpdef2:=nil;
        para.reset;
        para_same_num := 0;
        record_offset := 0;
        { have we ensured that the next parameter location will be aligned to the
          next 8 byte boundary? }
        paraaligned:=false;
        if push_addr_param(varspez, paradef, p.proccalloption) then
          begin
            paradef := cpointerdef.getreusable_no_free(paradef);
            loc := LOC_REGISTER;
            paracgsize := OS_ADDR;
            paralen := tcgsize2size[OS_ADDR];
          end
        else
          begin
            if not is_special_array(paradef) then
              paralen := paradef.size
            else
              paralen := tcgsize2size[def_cgsize(paradef)];

            if (paradef.typ=recorddef) and
               tabstractrecordsymtable(tabstractrecorddef(paradef).symtable).has_single_field(tmpdef) and
               (tmpdef.typ=floatdef) then
              begin
                paradef:=tmpdef;
                loc:=getparaloc(paradef);
                paracgsize:=def_cgsize(paradef)
              end
            else if (((paradef.typ=arraydef) and not
                    is_special_array(paradef)) or
                    (paradef.typ=recorddef) and (varspez in [vs_value,vs_const]) ) then
              begin
                { general fallback rule: pass aggregate types in integer registers
                  without special adjustments (incl. Darwin h) }
                loc:=LOC_REGISTER;
                paracgsize:=int_cgsize(paralen);
                { Check if the element types in the record are the same}
                if paralen <= 16 then
                  para_same_num := tabstractrecordsymtable(tabstractrecorddef(paradef).symtable).has_double_field(tmpdef1, tmpdef2, record_offset);
                if para_same_num <> 0 then
                  begin
                    if (((tmpdef1.typ = floatdef) or (tmpdef2.typ = floatdef)) and (nextfloatreg > RS_F7)) then
                      para_same_num := 0;
                  end;
                if ((para_same_num <> 0) and (record_offset <= 8))then
                  begin
                    { Floating point elements less than two will be passed through the floating point register}
                    if tmpdef1.typ = floatdef then
                      begin
                        loc:=LOC_FPUREGISTER;
                        paracgsize:=int_float_cgsize(paralen);
                      end;
                    if ((tmpdef1.typ<>floatdef) and (tmpdef2.typ<>floatdef) and (para_same_num=1))then
                      para_same_num := 2;
                  end;
              end
            else
              begin
                loc:=getparaloc(paradef);
                paracgsize:=def_cgsize(paradef);
                { for things like formaldef }
                if (paracgsize=OS_NO) then
                  begin
                    paracgsize:=OS_ADDR;
                    paralen:=tcgsize2size[OS_ADDR];
                  end;
              end
          end;

        { patch FPU values into integer registers if we are processing varargs }
        if (isVararg) and (paradef.typ = floatdef) then
          begin
            loc := LOC_REGISTER;
            if paracgsize = OS_F64 then
              paracgsize := OS_64
            else
              paracgsize := OS_32;
          end;

        para.alignment := std_param_align;
        para.size := paracgsize;
        para.intsize := paralen;
        para.def := paradef;
        if (paralen = 0) then
          if (paradef.typ = recorddef) then
            begin
              paraloc := para.add_location;
              paraloc^.loc := LOC_VOID;
            end
          else
            internalerror(2022111921);

        if not assigned(alllocdef) then
          locdef:=paradef
        else
          begin
            locdef:=alllocdef;
            paracgsize:=def_cgsize(locdef);
          end;
        firstparaloc:=true;

        // Parameters passed in 2 registers are passed in a register starting with an even number.
        if isVararg and
           (paralen > 8) and
           (loc = LOC_REGISTER) and
           (nextintreg <= RS_R11) and
           odd(nextintreg) then
          inc(nextintreg);

        { can become < 0 for e.g. 3-byte records }
        while paralen > 0 do
          begin
            paraloc := para.add_location;
            { In case of po_delphi_nested_cc, the parent frame pointer
              is always passed on the stack. }
            if (loc = LOC_REGISTER) and
               (nextintreg <= RS_R11) and
               not forceintmem then
              begin
                paraloc^.loc := loc;

                { make sure we don't lose whether or not the type is signed }
                if (paracgsize <> OS_NO) and
                   (paradef.typ <> orddef) and
                   not assigned(alllocdef) then
                  begin
                    paracgsize := int_cgsize(paralen);
                    locdef:=get_paraloc_def(paradef, paralen, firstparaloc);
                  end;
                if para_same_num = 1 then
                  begin
                    paraloc^.size := int_cgsize(record_offset);
                    paraloc^.def := locdef;
                    para_same_num := 0;
                    {set for next element}
                    if tmpdef2.typ = floatdef then
                      begin
                        loc := LOC_FPUREGISTER;
                        paracgsize := int_float_cgsize(paralen - record_offset);
                      end
                    else
                      internalerror(2022082601);
                  end
                else if (paracgsize in [OS_NO, OS_128, OS_S128]) then
                  begin
                    if (paralen>4) then
                      begin
                        paraloc^.size := OS_INT;
                        paraloc^.def := osuinttype;
                      end
                    else
                      begin
                        { for 3-byte records aligned in the lower bits of register }
                        paraloc^.size := OS_32;
                        paraloc^.def := u32inttype;
                      end;
                  end
                else
                  begin
                    paraloc^.size := paracgsize;
                    paraloc^.def := locdef;
                  end;

                paraloc^.register := newreg(R_INTREGISTER, nextintreg, R_SUBNONE);
                inc(nextintreg);
                dec(paralen, tcgsize2size[paraloc^.size]);
              end
            else if (loc = LOC_FPUREGISTER) and
                    (nextfloatreg <= RS_F7) then
              begin
                paraloc^.loc := loc;
                if para_same_num = 1 then
                  begin
                    paraloc^.size := int_float_cgsize(record_offset);
                    if record_offset = 8 then
                      paraloc^.def := s64floattype
                    else
                      paraloc^.def := s32floattype;
                    para_same_num := 0;
                    {set for next element}
                    if tmpdef2.typ <> floatdef then
                      begin
                        loc := LOC_REGISTER;
                        paracgsize:=int_cgsize(paralen - record_offset);
                      end
                    else
                      internalerror(2022082602);
                  end
                else if((paracgsize = OS_F128) and (para_same_num = 2)) then
                  begin
                    paraloc^.size := OS_FLOAT;
                    paraloc^.def := s64floattype;
                  end
                else if((paracgsize = OS_F64) and (para_same_num = 2)) then
                  begin
                    paraloc^.size := OS_F32;
                    paraloc^.def := s32floattype;
                  end
                else
                  begin
                    paraloc^.size := paracgsize;
                    paraloc^.def := locdef;
                  end;
                paraloc^.register := newreg(R_FPUREGISTER, nextfloatreg, R_SUBWHOLE);
                inc(nextfloatreg);
                dec(paralen, tcgsize2size[paraloc^.size]);
              end
            else if (loc = LOC_MMREGISTER) then
              begin
                { no mm registers }
                internalerror(2022111922);
              end
            else
              begin
                { either LOC_REFERENCE, or one of the above which must be passed on the
                  stack because of insufficient registers }
                paraloc^.loc := LOC_REFERENCE;
                case loc of
                  LOC_FPUREGISTER:
                    begin
                      paraloc^.size:=int_float_cgsize(paralen);
                      case paraloc^.size of
                        OS_F32: paraloc^.def:=s32floattype;
                        OS_F64: paraloc^.def:=s64floattype;
                      else
                        internalerror(2022111923);
                      end;
                    end;
                  LOC_REGISTER,
                  LOC_REFERENCE:
                    begin
                      paraloc^.size:=int_cgsize(paralen);
                      paraloc^.def:=get_paraloc_def(paradef, paralen, firstparaloc);
                    end;
                else
                  internalerror(2022111924);
                end;
                if (side = callerside) then
                  paraloc^.reference.index := NR_STACK_POINTER_REG
                else
                  begin
                    { during procedure entry, NR_OLD_STACK_POINTER_REG contains the old stack pointer }
                    paraloc^.reference.index := NR_FRAME_POINTER_REG;
                    { create_paraloc_info_intern might be also called when being outside of
                      code generation so current_procinfo might be not set }
                    if assigned(current_procinfo) then
                      tloongarch64procinfo(current_procinfo).needs_frame_pointer := true;
                  end;
                paraloc^.reference.offset := stack_offset;

                { align temp contents to next register size }
                if not paraaligned then
                  inc(stack_offset, align(paralen, 8))
                else
                  inc(stack_offset, paralen);
                paralen := 0;
              end;
            firstparaloc:=false;
          end;
      end;

    function tcpuparamanager.create_varargs_paraloc_info(p: tabstractprocdef; side: tcallercallee; varargspara: tvarargsparalist): longint;
      var
        cur_stack_offset: aword;
        parasize, l: longint;
        curintreg, firstfloatreg, curfloatreg, curmmreg: tsuperregister;
        i: integer;
        hp: tparavarsym;
        paraloc: pcgparalocation;
      begin
        init_values(curintreg, curfloatreg, curmmreg, cur_stack_offset);
        firstfloatreg := curfloatreg;

        result := create_paraloc_info_intern(p, side, p.paras, curintreg, curfloatreg, curmmreg, cur_stack_offset, false);
        if (p.proccalloption in [pocall_cdecl, pocall_cppdecl, pocall_mwpascal]) then
          begin
            { just continue loading the parameters in the registers }
            if assigned(varargspara) then
              begin
                if side=callerside then
                  result := create_paraloc_info_intern(p, side, varargspara, curintreg, curfloatreg, curmmreg, cur_stack_offset, true)
                else
                  internalerror(2022111925);
                if curfloatreg <> firstfloatreg then
                  include(varargspara.varargsinfo, va_uses_float_reg);
              end;
            { varargs routines have to reserve at least 64 bytes for the RiscV ABI }
            if (result < 64) then
              result := 64;
          end
        else
          internalerror(2022111926);

        create_funcretloc_info(p, side);
      end;

    function tcpuparamanager.parseparaloc(p: tparavarsym; const s: string): boolean;
      begin
        internalerror(2022111927);
        result := true;
     end;


begin
  paramanager := tcpuparamanager.create;
end.

