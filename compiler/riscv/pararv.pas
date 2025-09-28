{
    Copyright (c) 2002 by Florian Klaempfl

    RiscV specific calling conventions

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
unit pararv;

{$I fpcdefs.inc}

  interface

    uses
      globtype,
      aasmdata,
      symconst,symtype,symdef,
      cgbase,cgutils,
      parabase,paramgr;

    type
      trvparamanager = class(tparamanager)
        function get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset; override;
        function get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset; override;

        function get_saved_registers_fpu(calloption: tproccalloption): tcpuregisterarray;override;
        function get_saved_registers_int(calloption: tproccalloption): tcpuregisterarray;override;

        function get_funcretloc(p: tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;

        procedure getcgtempparaloc(list: TAsmList; pd: tabstractprocdef; nr: longint; var cgpara: tcgpara);override;

        function create_paraloc_info_intern(p: tabstractprocdef; side: tcallercallee; paras: tparalist; var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; isVararg : boolean): longint;virtual;
        procedure create_paraloc_for_def(var para: TCGPara; varspez: tvarspez; paradef: tdef; var nextfloatreg, nextintreg: tsuperregister; var stack_offset: aword; const isVararg, forceintmem: boolean; const side: tcallercallee; const p: tabstractprocdef);virtual;

        function create_varargs_paraloc_info(p: tabstractprocdef; side: tcallercallee; varargspara: tvarargsparalist): longint;override;

        function push_addr_param(varspez: tvarspez; def: tdef; calloption: tproccalloption): boolean;override;

        function create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint;override;
      protected
        procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      end;

    function getparaloc(p : tdef) : tcgloc;

implementation

    uses
      verbose,
      globals,
      systems,
      cpuinfo,
      symsym,
      symtable,
      defutil,
      cpubase,
      procinfo,cpupi;

    function getparaloc(p : tdef) : tcgloc;
      begin
         case p.typ of
            orddef:
              result:=LOC_REGISTER;
            floatdef:
              if (cs_fp_emulation in current_settings.moduleswitches) or
                 (current_settings.fputype in [fpu_soft]) then
                result := LOC_REGISTER
              else
                result := LOC_FPUREGISTER;
            enumdef:
              result:=LOC_REGISTER;
            pointerdef:
              result:=LOC_REGISTER;
            formaldef:
              result:=LOC_REGISTER;
            classrefdef:
              result:=LOC_REGISTER;
            procvardef:
              { method pointers fit into two registers, so pass procedure variables always in registers }
              result:=LOC_REGISTER;
            recorddef:
              if (p.size > sizeof(pint)*2) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
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
            filedef:
              result:=LOC_REGISTER;
            arraydef:
              if is_dynamic_array(p) then
                getparaloc:=LOC_REGISTER
              else
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


    function trvparamanager.get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=[RS_X0..RS_X31]-[RS_X2,RS_X8..RS_X9,RS_X18..RS_X27];
      end;


    function trvparamanager.get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=[RS_F0..RS_F31];
        if target_info.abi in [abi_riscv_ilp32f,abi_riscv_ilp32d,abi_riscv_lp64f,abi_riscv_lp64d] then
          result:=result-[RS_F8..RS_F9,RS_F18..RS_F27];
      end;


    function trvparamanager.get_saved_registers_int(calloption : tproccalloption):tcpuregisterarray;
      const
        saved_regs: tcpuregisterarray = (RS_X2,RS_X8,RS_X9,RS_X18,RS_X19,RS_X20,RS_X21,RS_X22,RS_X23,RS_X24,RS_X26,RS_X26,RS_X27);
      begin
        result:=saved_regs;
      end;


    function trvparamanager.get_saved_registers_fpu(calloption : tproccalloption):tcpuregisterarray;
      const
        saved_regs: tcpuregisterarray = (RS_F8,RS_F9,RS_F18,RS_F19,RS_F20,RS_F21,RS_F22,RS_F23,RS_F24,RS_F25,RS_F26,RS_F27);
        empty_regs: tcpuregisterarray = ();
      begin
        if target_info.abi in [abi_riscv_ilp32f,abi_riscv_ilp32d,abi_riscv_lp64f,abi_riscv_lp64d] then
          result:=saved_regs
        else
          result:=empty_regs;
      end;


    procedure trvparamanager.getcgtempparaloc(list: TAsmList; pd : tabstractprocdef; nr : longint; var cgpara : tcgpara);
      var
        paraloc : pcgparalocation;
        psym : tparavarsym;
        pdef : tdef;
      begin
        psym:=tparavarsym(pd.paras[nr-1]);
        pdef:=psym.vardef;
        if push_addr_param(psym.varspez,pdef,pd.proccalloption) then
          pdef:=cpointerdef.getreusable_no_free(pdef);
        cgpara.reset;
        cgpara.size:=def_cgsize(pdef);
        cgpara.intsize:=tcgsize2size[cgpara.size];
        cgpara.alignment:=get_para_align(pd.proccalloption);
        cgpara.def:=pdef;
        paraloc:=cgpara.add_location;
        with paraloc^ do
         begin
           size:=def_cgsize(pdef);
           def:=pdef;
           if (nr<=8) then
             begin
               if nr=0 then
                 internalerror(2024121501);
               loc:=LOC_REGISTER;
               register:=newreg(R_INTREGISTER,RS_X10+nr-1,R_SUBWHOLE);
             end
           else
             begin
               loc:=LOC_REFERENCE;
               paraloc^.reference.index:=NR_STACK_POINTER_REG;
               reference.offset:=sizeof(pint)*nr;
             end;
          end;
      end;


    function trvparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
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
          procvardef,
          recorddef:
            result := not(def.size in [0..sizeof(aint)*2]) or (varspez = vs_const);
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


    function trvparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
        nextintreg, nextfloatreg, nextmmreg: tsuperregister;
        stack_offset: aword;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          exit;

        init_values(nextintreg,nextfloatreg,nextmmreg,stack_offset);
        create_paraloc_for_def(result,vs_value,result.def,nextfloatreg,nextintreg,stack_offset,false,false,side,p);
      end;


    procedure trvparamanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      begin
        { register parameter save area begins at 48(r2) }
        cur_stack_offset := 0;
        curintreg := RS_X10;
        curfloatreg := RS_F10;
        curmmreg := RS_NO;
      end;


    function trvparamanager.create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;
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

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset,false);
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
                { not sure if this applies to RiscV 32 as well ... }
{$ifdef RISCV64}
                { varargs routines have to reserve at least 64 bytes for the RiscV ABI }
                if (result < 64) then
                  result := 64;
{$endif RISCV64}
              end;
           end
        else
          internalerror(2019021912);
        create_funcretloc_info(p,side);
      end;


    function trvparamanager.create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg : tsuperregister;
      begin
        init_values(curintreg, curfloatreg, curmmreg, cur_stack_offset);

        result := create_paraloc_info_intern(p, side, p.paras, curintreg, curfloatreg, curmmreg, cur_stack_offset, false);

        create_funcretloc_info(p, side);
      end;


    function trvparamanager.create_paraloc_info_intern(p: tabstractprocdef; side: tcallercallee; paras: tparalist; var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword; isVararg : boolean): longint;
      var
        nextintreg, nextfloatreg, nextmmreg : tsuperregister;
        i: integer;
        hp: tparavarsym;
        paraloc: pcgparalocation;
        delphi_nestedfp: boolean;
      begin
{$IFDEF extdebug}
        if po_explicitparaloc in p.procoptions then
          internalerror(200411141);
{$ENDIF extdebug}

        result := 0;
        nextintreg := curintreg;
        nextfloatreg := curfloatreg;
        nextmmreg := curmmreg;

        for i := 0 to paras.count - 1 do
          begin
            hp := tparavarsym(paras[i]);

            if (vo_has_explicit_paraloc in hp.varoptions) then
              internalerror(2024122201);

            { currently only support C-style array of const }
            if (p.proccalloption in [pocall_cdecl, pocall_cppdecl]) and
              is_array_of_const(hp.vardef) then begin
              paraloc := hp.paraloc[side].add_location;
              { hack: the paraloc must be valid, but is not actually used }
              paraloc^.loc := LOC_REGISTER;
              paraloc^.register := NR_X0;
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


    procedure trvparamanager.create_paraloc_for_def(var para: TCGPara; varspez: tvarspez; paradef: tdef; var nextfloatreg, nextintreg: tsuperregister; var stack_offset: aword; const isVararg, forceintmem: boolean; const side: tcallercallee; const p: tabstractprocdef);
      var
        paracgsize: tcgsize;
        loc: tcgloc;
        paraloc: pcgparalocation;
        { def to use for all paralocs if <> nil }
        alllocdef,
        { def to use for the current paraloc }
        locdef,
        tmpdef: tdef;
        paralen: aint;
        firstparaloc,
        paraaligned: boolean;
      begin
        alllocdef:=nil;
        locdef:=nil;
        para.reset;
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

            if (paradef.typ=recorddef) and not(is_implicit_pointer_object_type(paradef)) and
               tabstractrecordsymtable(tabstractrecorddef(paradef).symtable).has_single_field(tmpdef) and
               (tmpdef.typ=floatdef) then
              begin
                paradef:=tmpdef;
                loc:=getparaloc(paradef);
                paracgsize:=def_cgsize(paradef)
              end
            else if (((paradef.typ=arraydef) and not
                 is_special_array(paradef)) or
                (paradef.typ=recorddef)) then
              begin
                loc:=LOC_REGISTER;
                paracgsize:=int_cgsize(paralen);
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
            internalerror(2024121401);
        if not assigned(alllocdef) then
          locdef:=paradef
        else
          begin
            locdef:=alllocdef;
            paracgsize:=def_cgsize(locdef);
          end;
        firstparaloc:=true;

        { Parameters passed in 2 registers are passed in a register starting with an even number. }
        if isVararg and
           (paralen > sizeof(AInt)) and
           (loc = LOC_REGISTER) and
           (nextintreg <= RS_X17) and
           odd(nextintreg) then
          inc(nextintreg);

        { can become < 0 for e.g. 3-byte records }
        while (paralen > 0) do begin
          paraloc := para.add_location;
          { In case of po_delphi_nested_cc, the parent frame pointer
            is always passed on the stack. }
          if (loc = LOC_REGISTER) and
             (nextintreg <= RS_X17) and
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

               if (paracgsize in [OS_NO, OS_SPAIR, OS_PAIR]) then
                begin
                  if (paralen > 4) then
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
            (nextfloatreg <= RS_F17) then
            begin
              paraloc^.loc := loc;
              paraloc^.size := paracgsize;
              paraloc^.def := locdef;
              paraloc^.register := newreg(R_FPUREGISTER, nextfloatreg, R_SUBWHOLE);
              { the RiscV ABI says that the GPR index is increased for every parameter, no matter
                which type it is stored in

                 not really, https://github.com/riscv/riscv-elf-psabi-doc/blob/master/riscv-elf.md#hardware-floating-point-calling-convention says
                 otherwise, gcc doesn't do it either }
              inc(nextfloatreg);
              dec(paralen, tcgsize2size[paraloc^.size]);
            end
          else if (loc = LOC_MMREGISTER) then
            { no mm registers }
            internalerror(2018072601)
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
                        internalerror(2013060122);
                    end;
                  end;
                LOC_REGISTER,
                LOC_REFERENCE:
                  begin
                    paraloc^.size:=int_cgsize(paralen);
                    paraloc^.def:=get_paraloc_def(paradef, paralen, firstparaloc);
                  end;
                else
                  internalerror(2006011101);
              end;
              if (side = callerside) then
                paraloc^.reference.index := NR_STACK_POINTER_REG
              else
                begin
                  { during procedure entry, NR_OLD_STACK_POINTER_REG contains the old stack pointer }
                  paraloc^.reference.index := NR_FRAME_POINTER_REG;
                  { create_paraloc_info_intern might be also called when being outside of
                    code generation so current_procinfo might be not set }
{$ifdef RISCV64}
                  if assigned(current_procinfo) then
                    trv64procinfo(current_procinfo).needs_frame_pointer := true;
{$endif RISCV64}
{$ifdef RISCV32}
                  if assigned(current_procinfo) then
                    trv32procinfo(current_procinfo).needs_frame_pointer := true;
{$endif RISCV32}
                end;
              paraloc^.reference.offset := stack_offset;

              { align temp contents to next register size }
              if not paraaligned then
                inc(stack_offset, align(paralen, sizeof(AInt)))
              else
                inc(stack_offset, paralen);
              paralen := 0;
            end;
          firstparaloc:=false;
        end;
      end;


end.


