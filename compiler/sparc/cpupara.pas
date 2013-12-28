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
        procedure getintparaloc(pd : tabstractprocdef; nr : longint; var cgpara : tcgpara);override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function  create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargsparalist):longint;override;
        function  get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
      private
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


    procedure TSparcParaManager.GetIntParaLoc(pd : tabstractprocdef; nr : longint; var cgpara : tcgpara);
      var
        paraloc : pcgparalocation;
        psym: tparavarsym;
        pdef: tdef;
      begin
        if nr<1 then
          InternalError(2002100806);
        psym:=tparavarsym(pd.paras[nr-1]);
        pdef:=psym.vardef;
        if push_addr_param(psym.varspez,pdef,pd.proccalloption) then
          pdef:=getpointerdef(pdef);
        cgpara.reset;
        cgpara.size:=def_cgsize(pdef);
        cgpara.intsize:=tcgsize2size[cgpara.size];
        cgpara.alignment:=std_param_align;
        cgpara.def:=pdef;
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
            size:=def_cgsize(pdef);
            def:=pdef;
          end;
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tsparcparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out,constref always require address }
        if varspez in [vs_var,vs_out,vs_constref] then
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
            result:=not tprocvardef(def).is_addressonly;
          setdef :
            result:=not is_smallset(def);
        end;
      end;


    function tsparcparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
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
{$ifndef cpu64bitaddr}
            if retcgsize in [OS_64,OS_S64] then
             begin
               paraloc^.loc:=LOC_REGISTER;
               { high }
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_HIGH_REG;
               paraloc^.size:=OS_32;
               paraloc^.def:=u32inttype;
               { low }
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
{$endif not cpu64bitaddr}
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.size:=retcgsize;
               paraloc^.def:=result.def;
               if (side=callerside) then
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
             end;
          end;
      end;


    procedure tsparcparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist;
                                                           var intparareg,parasize:longint);
      var
        paraloc      : pcgparalocation;
        i            : integer;
        hp           : tparavarsym;
        paradef      : tdef;
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
            paradef:=hp.vardef;
            { currently only support C-style array of const,
              there should be no location assigned to the vararg array itself }
            if (p.proccalloption in cstylearrayofconst) and
               is_array_of_const(paradef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_G0;
                paraloc^.size:=OS_ADDR;
                paraloc^.def:=voidpointertype;
                break;
              end;

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              begin
                paracgsize:=OS_ADDR;
                paradef:=getpointerdef(paradef);
              end
            else
              begin
                paracgsize:=def_cgsize(paradef);
                { for formaldef }
                if paracgsize=OS_NO then
                  begin
                    paracgsize:=OS_ADDR;
                    paradef:=voidpointertype;
                  end;
              end;
            hp.paraloc[side].reset;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].def:=paradef;
            if (side = callerside) then
              hp.paraloc[side].Alignment:=std_param_align
            else
              hp.paraloc[side].Alignment:=paradef.alignment;
            paralen:=tcgsize2size[paracgsize];
            hp.paraloc[side].intsize:=paralen;
            while paralen>0 do
              begin
                paraloc:=hp.paraloc[side].add_location;
                { Floats are passed in int registers,
                  We can allocate at maximum 32 bits per register }
                if paracgsize in [OS_64,OS_S64,OS_F32,OS_F64] then
                  begin
                    paraloc^.size:=OS_32;
                    paraloc^.def:=u32inttype;
                  end
                else
                  begin
                    paraloc^.size:=paracgsize;
                    paraloc^.def:=paradef;
                  end;
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
                { In case of po_delphi_nested_cc, the parent frame pointer
                  is always passed on the stack. }
                else if (intparareg<=high(tparasupregs)) and
                   (not(vo_is_parentfp in hp.varoptions) or
                    not(po_delphi_nested_cc in p.procoptions)) then
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

                    if (target_info.endian=endian_big) and
                       (paralen<tcgsize2size[OS_INT]) and
                       (paradef.typ<>recorddef) then
                      inc(paraloc^.reference.offset,4-paralen);

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
