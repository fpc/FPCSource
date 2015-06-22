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
      symconst,symbase,symsym,symtype,symdef,paramgr,parabase,cgbase,cgutils;

    const
      { The value below is OK for O32 and N32 calling conventions }
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
      mips_nb_used_registers  : longint = MIPS_NB_REGISTERS_USED_IN_CALL_O32;

      { Might need to be changed if we support N64 ABI later }
      mips_sizeof_register_param : longint = 4;

    type
      tparasupregs = array[0..MIPS_MAX_REGISTERS_USED_IN_CALL-1] of tsuperregister;
      tparasupregsused = array[0..MIPS_MAX_REGISTERS_USED_IN_CALL-1] of boolean;

    const

      parasupregs : tparasupregs = (RS_R4, RS_R5, RS_R6, RS_R7, RS_R8, RS_R9);

    type
      tcpuparamanager=class(TParaManager)
        function  push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;override;
        function  get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function  create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargsparalist):longint;override;
        function  get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
        function  param_use_paraloc(const cgpara: tcgpara): boolean; override;
      private
        intparareg,
        intparasize : longint;
        can_use_float : boolean;
        function is_abi_record(def: tdef): boolean;
        procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist);
      end;

implementation

    uses
      cutils,verbose,systems,
      defutil, cpupi, procinfo,
      cgobj;



    function tcpuparamanager.get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;
      begin
        { O32 ABI values }
        result:=[RS_R1..RS_R15,RS_R24..RS_R25,RS_R31];
      end;


    function tcpuparamanager.get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;
      begin
        { O32 ABI values }
        result:=[RS_F0..RS_F19];
      end;


    { whether "def" must be treated as record when used as function result,
      i.e. its address passed in a0 }
    function tcpuparamanager.is_abi_record(def: tdef): boolean;
      begin
        result:=(def.typ=recorddef) or
          ((def.typ=procvardef) and not tprocvardef(def).is_addressonly);
      end;


    function tcpuparamanager.param_use_paraloc(const cgpara: tcgpara): boolean;
      var
        paraloc: pcgparalocation;
      begin
        paraloc:=cgpara.location;
        if not assigned(paraloc) then
          internalerror(200410102);
        result:=(paraloc^.loc=LOC_REFERENCE) and (paraloc^.next=nil);
      end;


    { true if a parameter is too large to copy and only the address is pushed }
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
          recorddef:
            { According to 032 ABI we should have }
            result:=false;
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
            { If we always push records by value, we have to handle methodpointers that way too. }
            result:=false; {not tprocvardef(def).is_addressonly;}
          setdef :
            result:=not(is_smallset(def));
        end;
      end;


    function tcpuparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
        retdef : tdef;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          begin
            { Return is passed as var parameter,
              in this case we use the first register R4 for it }
            if assigned(forcetempdef) then
              retdef:=forcetempdef
            else
              retdef:=p.returndef;
            if ret_in_param(retdef,p) and
              is_abi_record(retdef) then
              begin
                if intparareg=0 then
                  inc(intparareg);
              end;
            exit;
          end;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if result.def.typ=floatdef then
          begin
            paraloc^.loc:=LOC_FPUREGISTER;
            paraloc^.register:=NR_FPU_RESULT_REG;
            if retcgsize=OS_F64 then
              setsubreg(paraloc^.register,R_SUBFD);
            paraloc^.size:=retcgsize;
            paraloc^.def:=result.def;
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
               paraloc^.def:=u32inttype;
               { high }
               paraloc:=result.add_location;
               paraloc^.loc:=LOC_REGISTER;
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_HIGH_REG;
               paraloc^.size:=OS_32;
               paraloc^.def:=u32inttype;
             end
            else
{$endif cpu64bitalu}
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.size:=retcgsize;
               paraloc^.def:=result.def;
               if side=callerside then
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
             end;
          end
      end;


    procedure tcpuparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist);
      var
        paraloc      : pcgparalocation;
        i,j          : integer;
        hp           : tparavarsym;
        paracgsize   : tcgsize;
        paralen      : longint;
        locdef       : tdef;
        paradef      : tdef;
        fpparareg    : integer;
        reg          : tsuperregister;
        alignment    : longint;
        tmp          : longint;
        firstparaloc : boolean;
        reg_and_stack: boolean;
      begin
        fpparareg := 0;
        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            paradef := hp.vardef;

            { currently only support C-style array of const }
             if (p.proccalloption in cstylearrayofconst) and
               is_array_of_const(paradef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_R0;
                paraloc^.size:=OS_ADDR;
                paraloc^.def:=voidpointertype;
                break;
              end;

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              begin
                paracgsize := OS_ADDR;
                paralen := tcgsize2size[paracgsize];
                paradef := cpointerdef.getreusable(paradef);
              end
            else
              begin
                paracgsize := def_cgsize(paradef);
                { for things like formaldef }
                if (paracgsize=OS_NO) and (paradef.typ <> recorddef) then
                  begin
                    paracgsize:=OS_ADDR;
                    paradef:=voidpointertype;
                  end;
                if not is_special_array(paradef) then
                  paralen := paradef.size
                else
                  paralen := tcgsize2size[paracgsize];
              end;

            if (paracgsize in [OS_64, OS_S64, OS_F64]) or (paradef.alignment = 8) then
              alignment := 8
            else
              alignment := 4;
            //writeln('para: ',hp.Name,' typ=',hp.vardef.typ,' paracgsize=',paracgsize,' align=',hp.vardef.alignment);
            hp.paraloc[side].reset;
            hp.paraloc[side].Alignment:=alignment;
            locdef:=paradef;
            if (paracgsize=OS_NO) or
              { Ordinals on caller side must be promoted to machine word }
              ((target_info.endian=endian_big) and     // applies to little-endian too?
              (paradef.typ<>recorddef) and
              (side=callerside) and
              (paralen<tcgsize2size[OS_INT]))then
              begin
                if is_signed(paradef) then
                  begin
                    paracgsize:=OS_S32;
                    locdef:=s32inttype;
                  end
                else
                  begin
                    paracgsize:=OS_32;
                    locdef:=u32inttype;
                  end;
                paralen:=align(paralen,4);
              end
            else
              paralen:=tcgsize2size[paracgsize];
            hp.paraloc[side].intsize:=paralen;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].def:=paradef;

            if (paralen=0) then
              if (paradef.typ=recorddef) then
                begin
                  paraloc:=hp.paraloc[side].add_location;
                  paraloc^.loc:=LOC_VOID;
                end
              else
                internalerror(2013020601);

            { check the alignment, mips O32ABI require a nature alignment  }
            tmp := align(intparasize, alignment) - intparasize;
            while tmp > 0 do
              begin
                inc(intparareg);
                inc(intparasize,4);
                dec(tmp,4);
              end;

            { any non-float args will disable the use the floating regs }
            { up to two fp args }
            if (not(paracgsize in [OS_F32, OS_F64])) or (fpparareg = 2) then
              can_use_float := false;

            firstparaloc:=true;
            { Is parameter split between stack and registers? }
            reg_and_stack:=(side=calleeside) and
              (paralen+intparasize>16) and (intparasize<16);
            while paralen>0 do
              begin
                paraloc:=hp.paraloc[side].add_location;
                { We can allocate at maximum 32 bits per register }
                if (paracgsize in [OS_64,OS_S64]) or
                   ((paracgsize in [OS_F32,OS_F64]) and
                     not(can_use_float)) then
                  begin
                    paraloc^.size:=OS_32;
                    paraloc^.def:=u32inttype;
                  end
                else
                  begin
                    paraloc^.size:=paracgsize;
                    paraloc^.def:=locdef;
                  end;

                { ret in param? }
                if (vo_is_funcret in hp.varoptions) and
                  is_abi_record(hp.vardef) then
                  begin
                    { This should be the first parameter }
                    //if (intparareg<>1) then
                    //  Comment(V_Warning,'intparareg should be one for funcret in tcpuparamanager.create_paraloc_info_intern');
                    paraloc^.loc:=LOC_REGISTER;
                    paraloc^.register:=newreg(R_INTREGISTER,parasupregs[0],R_SUBWHOLE);
                    inc(intparasize,align(tcgsize2size[paraloc^.size],sizeof(aint)));
                  end
                { "In case of po_delphi_nested_cc, the parent frame pointer
                  is always passed on the stack". On other targets it is
                  used to provide caller-side stack cleanup and prevent stackframe
                  optimization. For MIPS this does not matter. }
                else if (intparareg<mips_nb_used_registers) and
                   (not reg_and_stack) {and
                   (not(vo_is_parentfp in hp.varoptions) or
                    not(po_delphi_nested_cc in p.procoptions))} then
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
                            inc(intparasize,8);
                          end
                        else
                          begin
                            paraloc^.register:=newreg(R_FPUREGISTER, reg, R_SUBFS);
                            inc(fpparareg);
                            inc(intparareg);
                            inc(intparasize,sizeof(aint));
                          end;
                      end
                    else { not can use float }
                     begin
                       paraloc^.loc:=LOC_REGISTER;
                       paraloc^.register:=newreg(R_INTREGISTER,parasupregs[intparareg],R_SUBWHOLE);

                       { big-endian targets require that record data stored in parameter
                         registers is left-aligned }
                       if (target_info.endian=endian_big) and
                          (paradef.typ=recorddef) and
                          (paralen<tcgsize2size[OS_INT]) then
                         begin
                           paraloc^.shiftval := (sizeof(aint)-tcgsize2size[paraloc^.size])*(-8);
                           paraloc^.size := OS_INT;
                           paraloc^.def := osuinttype;
                         end;
                       inc(intparareg);
                       inc(intparasize,align(tcgsize2size[paraloc^.size],mips_sizeof_register_param));
                     end;
                  end
                else
                  begin
                    if reg_and_stack then
                      begin
                        for j:=intparareg to mips_nb_used_registers-1 do
                          tmipsprocinfo(current_procinfo).register_used[j]:=true;
                        { all registers used now }
                        intparareg:=mips_nb_used_registers;
                      end;
                    paraloc^.loc:=LOC_REFERENCE;
                    if (paradef.typ=floatdef) then
                      paraloc^.size:=int_float_cgsize(paralen)
                    else
                      paraloc^.size:=int_cgsize(paralen);
                    paraloc^.def:=get_paraloc_def(locdef,paralen,firstparaloc);

                    if side=callerside then
                      begin
                        paraloc^.reference.index := NR_STACK_POINTER_REG;
                        paraloc^.reference.offset:=intparasize;
                      end
                    else
                      begin
                        if (po_nostackframe in p.procoptions) then
                          paraloc^.reference.index := NR_STACK_POINTER_REG
                        else
                          begin
                            paraloc^.reference.index := NR_FRAME_POINTER_REG;
                            if assigned(current_procinfo) then
                              include(current_procinfo.flags,pi_needs_stackframe);
                          end;
                        paraloc^.reference.offset:=intparasize;

                        if (target_info.endian=endian_big) and
                           (paralen<tcgsize2size[OS_INT]) and
                           (paradef.typ<>recorddef) then
                          inc(paraloc^.reference.offset,4-paralen);
                      end;
                    inc(intparasize,align(paralen,mips_sizeof_register_param));
                    paralen:=0;
                  end;
                dec(paralen,tcgsize2size[paraloc^.size]);
                firstparaloc:=false;
              end;
          end;
        { O32 ABI reqires at least 16 bytes }
        if (intparasize < 16) then
          intparasize := 16;
      end;


    function tcpuparamanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      begin
        intparareg:=0;
        intparasize:=0;
        can_use_float := true;
        { Create Function result paraloc }
        create_funcretloc_info(p,callerside);
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,p.paras);
        { append the varargs }
        can_use_float := false;
        { restore correct intparasize value }
        if intparareg < 4 then
          intparasize:=intparareg * 4;
        create_paraloc_info_intern(p,callerside,varargspara);
        { We need to return the size allocated on the stack }
        result:=intparasize;
      end;



    function tcpuparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      begin
        intparareg:=0;
        intparasize:=0;
        can_use_float := true;
        { Create Function result paraloc }
        create_funcretloc_info(p,side);
        create_paraloc_info_intern(p,side,p.paras);
        { We need to return the size allocated on the stack }
        result:=intparasize;
      end;


begin
   ParaManager:=tcpuparamanager.create;
end.
