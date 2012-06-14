{
    Copyright (c) 1998-2002 by Florian Klaempfl and David Zhang

    Calling conventions for the MIPSEL

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
 *****************************************************************************}
unit cpupara;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cclasses,
      aasmtai,
      cpubase,cpuinfo,
      symconst,symbase,symsym,symtype,symdef,paramgr,parabase,cgbase;

    const
      MIPS_MAX_OFFSET = 20;
      MIPS_MAX_REGISTERS_USED_IN_CALL = 6;

      { All ABI seem to start with $4 i.e. $a0 }
      MIPS_FIRST_REGISTER_USED_IN_CALL = RS_R4;
      { O32 ABI uses $a0 to $a3, i.e R4 to R7 }
      MIPS_LAST_REGISTER_USED_IN_CALL_ABI_O32 = RS_R7;
      { N32 ABI uses also R8 and R9 }
      MIPS_LAST_REGISTER_USED_IN_CALL_ABI_N32 = RS_R9;
      { The calculation below is based on the assumption
        that all registers used for ABI calls are
        ordered and follow each other }
      MIPS_NB_REGISTERS_USED_IN_CALL_O32 =
        MIPS_LAST_REGISTER_USED_IN_CALL_ABI_O32
        - MIPS_FIRST_REGISTER_USED_IN_CALL + 1;
      MIPS_NB_REGISTERS_USED_IN_CALL_N32 =
        MIPS_LAST_REGISTER_USED_IN_CALL_ABI_N32
        - MIPS_FIRST_REGISTER_USED_IN_CALL + 1;


      { Set O32 ABI as default }
    const
      mips_nb_used_registers = MIPS_NB_REGISTERS_USED_IN_CALL_O32;

      { Might need to be changed if we support N64 ABI later }
      mips_sizeof_register_param = 4;

    type
      tparasupregs = array[0..MIPS_MAX_REGISTERS_USED_IN_CALL-1] of tsuperregister;
      tparasupregsused = array[0..MIPS_MAX_REGISTERS_USED_IN_CALL-1] of boolean;
      tparasupregsoffset = array[0..MIPS_MAX_REGISTERS_USED_IN_CALL-1] of longint;

    const
      parasupregs : tparasupregs = (RS_R4, RS_R5, RS_R6, RS_R7, RS_R8, RS_R9);

    type
      TMIPSParaManager=class(TParaManager)
      var
        param_offset:array[0..MIPS_MAX_OFFSET] of ^Aint;
        intparareg,
        parasize : longint;
        function  push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;override;
        function  get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;override;
        {Returns a structure giving the information on the storage of the parameter
        (which must be an integer parameter)
        @param(nr Parameter number of routine, starting from 1)}
        procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function  create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargsparalist):longint;override;
        function  get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tcgpara;override;
      private
        procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
        procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist);
      end;

implementation

    uses
      cutils,verbose,systems,
      defutil, cpupi, procinfo,
      cgutils,cgobj;



    function TMIPSParaManager.get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;
      begin
        { O32 ABI values }
        result:=[RS_R1..RS_R15,RS_R24..RS_R25,RS_R31];
      end;


    function TMIPSParaManager.get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;
      begin
        { O32 ABI values }
        result:=[RS_F0..RS_F19];
      end;


    procedure TMIPSParaManager.GetIntParaLoc(calloption : tproccalloption; nr : longint;var cgpara : tcgpara);
      var
        paraloc : pcgparalocation;
      begin
        if nr<1 then
          InternalError(2002100806);
        cgpara.reset;
        cgpara.size:=OS_INT;
        cgpara.intsize:=tcgsize2size[OS_INT];
        cgpara.alignment:=std_param_align;
        paraloc:=cgpara.add_location;
        with paraloc^ do
          begin
            { MIPS: ABI dependent number of first parameters
              are passed into registers }
            dec(nr);
            if nr<mips_nb_used_registers then
              begin
                loc:=LOC_REGISTER;
                register:=newreg(R_INTREGISTER,parasupregs[nr],R_SUBWHOLE);
                if assigned(current_procinfo) then
                  begin
                    TMIPSProcInfo(current_procinfo).register_used[nr]:=true;
                    TMIPSProcInfo(current_procinfo).register_offset[nr]:=nr*mips_sizeof_register_param;
                  end;
              end
            else
              begin
                { The other parameters are passed on the stack }
                loc:=LOC_REFERENCE;
                reference.index:=NR_STACK_POINTER_REG;
                reference.offset:=nr*mips_sizeof_register_param;
              end;
            size:=OS_INT;
          end;
      end;

    { true if a parameter is too large to copy and only the address is pushed }
    function TMIPSParaManager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out,constref always require address }
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          recorddef:
            result:=true;
            { According to 032 ABI we should have
              result:=false; buut this cmpletely fails }
          arraydef:
            result:=true; {(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                            is_open_array(def) or
                            is_array_of_const(def) or
                            is_array_constructor(def);}
          variantdef,
          formaldef :
            result:=true;
          objectdef :
            result:=is_object(def);
          stringdef :
            result:=(tstringdef(def).stringtype in [st_shortstring,st_longstring]);
          procvardef :
            result:=not tprocvardef(def).is_addressonly;
          setdef :
            result:=not(is_smallset(def));
        end;
      end;


    procedure TMIPSParaManager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      begin
        p.funcretloc[side]:=get_funcretloc(p,side,p.returndef);
      end;


    function TMIPSParaManager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
      begin
        result.init;
        result.alignment:=get_para_align(p.proccalloption);
        { void has no location }
        if is_void(def) then
          begin
            paraloc:=result.add_location;
            result.size:=OS_NO;
            result.intsize:=0;
            paraloc^.size:=OS_NO;
            paraloc^.loc:=LOC_VOID;
            exit;
          end;
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          begin
            retcgsize:=OS_ADDR;
            result.intsize:=sizeof(pint);
          end
        else
          begin
            retcgsize:=def_cgsize(def);
            result.intsize:=def.size;
          end;
        result.size:=retcgsize;
        { Return is passed as var parameter,
          in this case we use the first register R4 for it }
        if ret_in_param(def,p.proccalloption) then
          begin
            { Reserve first register for ret_in_param }
            if intparareg=0 then
              inc(intparareg);
            if side=calleeside then
              begin
                paraloc:=result.add_location;
                paraloc^.loc:=LOC_REFERENCE;
                paraloc^.reference.index:=NR_STACK_POINTER_REG;
                { return is at offset zero }
                paraloc^.reference.offset:=0;
                paraloc^.size:=retcgsize;
              end
            else
              begin
                getIntParaLoc(p.proccalloption,1,result);
              end;
            exit;
          end;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if p.returndef.typ=floatdef then
          begin
            paraloc^.loc:=LOC_FPUREGISTER;
            paraloc^.register:=NR_FPU_RESULT_REG;
            if retcgsize=OS_F64 then
              setsubreg(paraloc^.register,R_SUBFD);
            paraloc^.size:=retcgsize;
          end
        else
         { Return in register }
          begin
{$ifndef cpu64bitalu}
            if retcgsize in [OS_64,OS_S64] then
             begin
               { low }
               paraloc^.loc:=LOC_REGISTER;
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_LOW_REG;
               paraloc^.size:=OS_32;
               { high }
               paraloc:=result.add_location;
               paraloc^.loc:=LOC_REGISTER;
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_HIGH_REG;
               paraloc^.size:=OS_32;
             end
            else
{$endif cpu64bitalu}
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.size:=retcgsize;
               if side=callerside then
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
             end;
          end
      end;


    procedure TMIPSParaManager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist);
      var
        paraloc      : pcgparalocation;
        i            : integer;
        hp           : tparavarsym;
        paracgsize   : tcgsize;
        paralen      : longint;
	paradef      : tdef;
	fpparareg    : integer;
	can_use_float : boolean;
	reg           : tsuperregister;
	alignment     : longint;
	tmp	      : longint;
      begin
        fpparareg := 0;
	can_use_float := true;
        for i:=0 to paras.count-1 do
          begin
            if i<=MIPS_MAX_OFFSET then
              param_offset[i] := Nil;
            hp:=tparavarsym(paras[i]);
            paradef := hp.vardef;

            { currently only support C-style array of const }
             if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
               is_array_of_const(paradef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_R0;
                paraloc^.size:=OS_ADDR;
                break;
              end;

            if (push_addr_param(hp.varspez,paradef,p.proccalloption)) then
              begin
                paracgsize := OS_ADDR;
	    	paralen := tcgsize2size[paracgsize];
              end
            else
              begin
                paracgsize := def_cgsize(paradef);
                { for things like formaldef }
                if (paracgsize=OS_NO) then
                  begin
                    paracgsize:=OS_ADDR;
                  end;

                if not is_special_array(paradef) then
                  paralen := paradef.size
                else
                  paralen := tcgsize2size[paracgsize];
              end;

            if (paracgsize in [OS_64, OS_S64, OS_F64]) or (hp.vardef.alignment = 8) then
	      alignment := 8
            else
	      alignment := 4;
            hp.paraloc[side].reset;
            hp.paraloc[side].Alignment:=alignment;
            paralen:=tcgsize2size[paracgsize];
            hp.paraloc[side].intsize:=paralen;
            hp.paraloc[side].size:=paracgsize;
	    { check the alignment, mips O32ABI require a nature alignment  }
	    tmp := align(parasize, alignment) - parasize;
	    while tmp > 0 do
              begin
                inc(intparareg);
	        inc(parasize,4);
                dec(tmp,4);
              end;

	    { any non-float args will disable the use the floating regs }
	    { up to two fp args }
	    if (not(paracgsize in [OS_F32, OS_F64])) or (fpparareg = 2) then
              can_use_float := false;

            while paralen>0 do
              begin
                paraloc:=hp.paraloc[side].add_location;
                { We can allocate at maximum 32 bits per register }
                if (paracgsize in [OS_64,OS_S64]) or
                   ((paracgsize in [OS_F32,OS_F64]) and
                     not(can_use_float)) then
                  paraloc^.size:=OS_32
                else
                  paraloc^.size:=paracgsize;
                { ret in param? }
                if vo_is_funcret in hp.varoptions then
                  begin
                    { This should be the first parameter }
                    if assigned(current_procinfo) then
                      begin
                        TMIPSProcInfo(current_procinfo).register_used[0]:=true;
                        TMIPSProcInfo(current_procinfo).register_offset[0]:=0;
                      end;
                    //if (intparareg<>1) then
                    //  Comment(V_Warning,'intparareg should be one for funcret in TMipsParaManager.create_paraloc_info_intern');
                    if side=callerside then
                    begin
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=newreg(R_INTREGISTER,parasupregs[0],R_SUBWHOLE);
                    end
                    else
                    begin
                      paraloc^.loc:=LOC_REFERENCE;
                      if (po_nostackframe in p.procoptions) then
                        paraloc^.reference.index := NR_STACK_POINTER_REG
                      else
                        begin
                          paraloc^.reference.index := NR_FRAME_POINTER_REG;
                          if assigned(current_procinfo) then
                            TMIPSProcinfo(current_procinfo).needs_frame_pointer := true;
                        end;
                      paraloc^.reference.offset:=0;
                      if i<=MIPS_MAX_OFFSET then
                        param_offset[i] := @paraloc^.reference.offset;
                    end;
                    inc(parasize,align(tcgsize2size[paraloc^.size],sizeof(aint)));
                  end
                { In case of po_delphi_nested_cc, the parent frame pointer
                  is always passed on the stack. }
                else if (intparareg<mips_nb_used_registers) and
                   (not(vo_is_parentfp in hp.varoptions) or
                    not(po_delphi_nested_cc in p.procoptions)) then
                  begin
                    if (can_use_float) then
                      begin
                        paraloc^.loc:=LOC_FPUREGISTER;
                        if (fpparareg = 0) then
                          reg := RS_F12
                        else
                          reg := RS_F14;
                        if (paraloc^.size = OS_F64) then
                          begin
                            paraloc^.register:=newreg(R_FPUREGISTER, reg, R_SUBFD);
			    inc(fpparareg);
			    inc(intparareg);
			    inc(intparareg);
			    inc(parasize,8);
                          end
                        else
                          begin
                            paraloc^.register:=newreg(R_FPUREGISTER, reg, R_SUBFS);
			    inc(fpparareg);
			    inc(intparareg);
			    inc(parasize,sizeof(aint));
                          end;
                      end
                    else { not can use float }
                     begin
                       if assigned(current_procinfo) then
                         begin
                           TMIPSProcInfo(current_procinfo).register_used[intparareg]:=true;
                           TMIPSProcInfo(current_procinfo).register_offset[intparareg]:=intparareg*mips_sizeof_register_param;
                         end;
                       if side=callerside then
                         begin
                           paraloc^.loc:=LOC_REGISTER;
                           paraloc^.register:=newreg(R_INTREGISTER,parasupregs[intparareg],R_SUBWHOLE);
                         end
                       else
                         begin
                           paraloc^.loc:=LOC_REFERENCE;
                           if (po_nostackframe in p.procoptions) then
                             paraloc^.reference.index := NR_STACK_POINTER_REG
                           else
                             begin
                               paraloc^.reference.index := NR_FRAME_POINTER_REG;
                               if assigned(current_procinfo) then
                                 TMIPSProcinfo(current_procinfo).needs_frame_pointer := true;
                             end;
                           paraloc^.reference.offset:=intparareg*mips_sizeof_register_param;
                         end;
                       inc(intparareg);
                       inc(parasize,align(tcgsize2size[paraloc^.size],mips_sizeof_register_param));
                     end;
                  end
                else
                  begin
                    paraloc^.loc:=LOC_REFERENCE;
                    if side=callerside then
                      begin
                        paraloc^.reference.index := NR_STACK_POINTER_REG;
                        paraloc^.reference.offset:=parasize;
                      end
                    else
                      begin
                        if (po_nostackframe in p.procoptions) then
                          paraloc^.reference.index := NR_STACK_POINTER_REG
                        else
                          begin
                            paraloc^.reference.index := NR_FRAME_POINTER_REG;
                            if assigned(current_procinfo) then
                              TMIPSProcinfo(current_procinfo).needs_frame_pointer := true;
                          end;
                        paraloc^.reference.offset:=parasize;
                        if i<=MIPS_MAX_OFFSET then
                          param_offset[i] := @paraloc^.reference.offset;
                      end;
                    inc(parasize,align(tcgsize2size[paraloc^.size],mips_sizeof_register_param));
                  end;
                dec(paralen,tcgsize2size[paraloc^.size]);
              end;
          end;
      end;


    function TMIPSParaManager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      begin
        intparareg:=0;
        parasize:=0;
        { Create Function result paraloc }
        create_funcretloc_info(p,callerside);
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,p.paras);
        { append the varargs }
        create_paraloc_info_intern(p,callerside,varargspara);
        { At least 16 bytes must be reserved for parameter
          area in O32 ABI, this can be used by called function,
          even if it has less parameter }
        if (parasize < 16) then
          parasize := 16;
        result:=parasize;
      end;



    function TMIPSParaManager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      begin
        intparareg:=0;
        parasize:=0;
        { Create Function result paraloc }
        create_funcretloc_info(p,side);
        create_paraloc_info_intern(p,side,p.paras);
        { We need to return the size allocated on the stack }
        { At least 16 bytes must be reserved for parameter
          area in O32 ABI, this can be used by called function,
          even if it has less parameter }
        if (parasize < 16) then
          parasize := 16;
        result:=parasize;
      end;


begin
   ParaManager:=TMIPSParaManager.create;
end.
