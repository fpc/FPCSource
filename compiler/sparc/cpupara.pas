{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Calling conventions for the SPARC

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
      aasmtai,aasmdata,
      cpubase,cpuinfo,
      symconst,symbase,symsym,symtype,symdef,paramgr,parabase,cgbase,cgutils;

    type
      TSparcParaManager=class(TParaManager)
        function  push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;override;
        function  get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;override;
        {Returns a structure giving the information on the storage of the parameter
        (which must be an integer parameter)
        @param(nr Parameter number of routine, starting from 1)}
        procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function  create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargsparalist):longint;override;
        function  get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tlocation;override;
      private
        procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
        procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                                             var intparareg,parasize:longint);
      end;

implementation

    uses
      cutils,verbose,systems,
      defutil,
      cgobj;

    type
      tparasupregs = array[0..5] of tsuperregister;
      pparasupregs = ^tparasupregs;
    const
      paraoutsupregs : tparasupregs = (RS_O0,RS_O1,RS_O2,RS_O3,RS_O4,RS_O5);
      parainsupregs  : tparasupregs = (RS_I0,RS_I1,RS_I2,RS_I3,RS_I4,RS_I5);


    function TSparcParaManager.get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;
      begin
        result:=[RS_G1,RS_O0,RS_O1,RS_O2,RS_O3,RS_O4,RS_O5,RS_O6,RS_O7];
      end;


    function tsparcparamanager.get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;
      begin
        result:=[RS_F0..RS_F31];
      end;


    procedure TSparcParaManager.GetIntParaLoc(calloption : tproccalloption; nr : longint;var cgpara : tcgpara);
      var
        paraloc : pcgparalocation;
      begin
        if nr<1 then
          InternalError(2002100806);
        cgpara.reset;
        cgpara.size:=OS_ADDR;
        cgpara.intsize:=sizeof(pint);
        cgpara.alignment:=std_param_align;
        paraloc:=cgpara.add_location;
        with paraloc^ do
          begin
            { The six first parameters are passed into registers }
            dec(nr);
            if nr<6 then
              begin
                loc:=LOC_REGISTER;
                register:=newreg(R_INTREGISTER,(RS_O0+nr),R_SUBWHOLE);
              end
            else
              begin
                { The other parameters are passed on the stack }
                loc:=LOC_REFERENCE;
                reference.index:=NR_STACK_POINTER_REG;
                reference.offset:=92+(nr-6)*4;
              end;
            size:=OS_INT;
          end;
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tsparcparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out always require address }
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          recorddef,
          variantdef,
          formaldef :
            result:=true;
          objectdef :
            result:=is_object(def);
          stringdef :
            result:=(tstringdef(def).stringtype in [st_shortstring,st_longstring]);
          procvardef :
            result:=(po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            result:=not is_smallset(def);
        end;
      end;


    procedure tsparcparamanager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      begin
        p.funcretloc[side]:=get_funcretloc(p,side,p.returndef);
      end;


    function tsparcparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tlocation;
      var
        retcgsize  : tcgsize;
      begin
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          retcgsize:=OS_ADDR
        else
          retcgsize:=def_cgsize(def);

        location_reset(result,LOC_INVALID,OS_NO);
        result.size:=retcgsize;
        { void has no location }
        if is_void(def) then
          begin
            result.loc:=LOC_VOID;
            exit;
          end;
        { Return is passed as var parameter }
        if ret_in_param(def,p.proccalloption) then
          begin
            result.loc:=LOC_REFERENCE;
            result.size:=retcgsize;
            exit;
          end;
        { Return in FPU register? }
        if def.typ=floatdef then
          begin
            result.loc:=LOC_FPUREGISTER;
            result.register:=NR_FPU_RESULT_REG;
            if retcgsize=OS_F64 then
              setsubreg(result.register,R_SUBFD);
            result.size:=retcgsize;
          end
        else
         { Return in register }
          begin
{$ifndef cpu64bitaddr}
            if retcgsize in [OS_64,OS_S64] then
             begin
               result.loc:=LOC_REGISTER;
               { high }
               if (side=callerside) or (po_inline in p.procoptions) then
                 result.register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 result.register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
               { low }
               if (side=callerside) or (po_inline in p.procoptions) then
                 result.register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 result.register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
             end
            else
{$endif not cpu64bitaddr}
             begin
               result.loc:=LOC_REGISTER;
               result.size:=retcgsize;
               if (side=callerside) then
                 result.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 result.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
             end;
          end;
      end;


    procedure tsparcparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist;
                                                           var intparareg,parasize:longint);
      var
        paraloc      : pcgparalocation;
        i            : integer;
        hp           : tparavarsym;
        paracgsize   : tcgsize;
        hparasupregs : pparasupregs;
        paralen      : longint;
      begin
        if side=callerside then
          hparasupregs:=@paraoutsupregs
        else
          hparasupregs:=@parainsupregs;
        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            { currently only support C-style array of const,
              there should be no location assigned to the vararg array itself }
            if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
               is_array_of_const(hp.vardef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_G0;
                paraloc^.size:=OS_ADDR;
                break;
              end;

            if push_addr_param(hp.varspez,hp.vardef,p.proccalloption) then
              paracgsize:=OS_ADDR
            else
              begin
                paracgsize:=def_cgSize(hp.vardef);
                if paracgsize=OS_NO then
                  paracgsize:=OS_ADDR;
              end;
            hp.paraloc[side].reset;
            hp.paraloc[side].size:=paracgsize;
            if (side = callerside) then
              hp.paraloc[side].Alignment:=std_param_align
            else
              hp.paraloc[side].Alignment:=hp.vardef.alignment;
            paralen:=tcgsize2size[paracgsize];
            hp.paraloc[side].intsize:=paralen;
            while paralen>0 do
              begin
                paraloc:=hp.paraloc[side].add_location;
                { Floats are passed in int registers,
                  We can allocate at maximum 32 bits per register }
                if paracgsize in [OS_64,OS_S64,OS_F32,OS_F64] then
                  paraloc^.size:=OS_32
                else
                  paraloc^.size:=paracgsize;
                { ret in param? }
                if vo_is_funcret in hp.varoptions then
                  begin
                    paraloc^.loc:=LOC_REFERENCE;
                    if side=callerside then
                      paraloc^.reference.index:=NR_STACK_POINTER_REG
                    else
                      paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                    paraloc^.reference.offset:=64;
                  end
                else if (intparareg<=high(tparasupregs)) then
                  begin
                    paraloc^.loc:=LOC_REGISTER;
                    paraloc^.register:=newreg(R_INTREGISTER,hparasupregs^[intparareg],R_SUBWHOLE);
                    inc(intparareg);
                  end
                else
                  begin
                    paraloc^.loc:=LOC_REFERENCE;
                    if side=callerside then
                      paraloc^.reference.index:=NR_STACK_POINTER_REG
                    else
                      paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                    paraloc^.reference.offset:=target_info.first_parm_offset+parasize;
                    { Parameters are aligned at 4 bytes }
                    inc(parasize,align(tcgsize2size[paraloc^.size],sizeof(pint)));
                  end;
                dec(paralen,tcgsize2size[paraloc^.size]);
              end;
          end;
      end;


    function TSparcParaManager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        intparareg,
        parasize : longint;
      begin
        intparareg:=0;
        parasize:=0;
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,p.paras,intparareg,parasize);
        { append the varargs }
        create_paraloc_info_intern(p,callerside,varargspara,intparareg,parasize);
        result:=parasize;
      end;



    function tsparcparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        intparareg,
        parasize : longint;
      begin
        intparareg:=0;
        parasize:=0;
        create_paraloc_info_intern(p,side,p.paras,intparareg,parasize);
        { Create Function result paraloc }
        create_funcretloc_info(p,side);
        { We need to return the size allocated on the stack }
        result:=parasize;
      end;


begin
   ParaManager:=TSparcParaManager.create;
end.
