{
    $Id$
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
      aasmtai,
      cpubase,cpuinfo,
      symconst,symbase,symsym,symtype,symdef,paramgr,parabase,cgbase;

    type
      TSparcParaManager=class(TParaManager)
        function  copy_value_on_stack(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;override;
        function  get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;override;
        {Returns a structure giving the information on the storage of the parameter
        (which must be an integer parameter)
        @param(nr Parameter number of routine, starting from 1)}
        procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function  create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargsparalist):longint;override;
      private
        procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
        procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                                             var intparareg,parasize:longint);
      end;

implementation

    uses
      cutils,verbose,systems,
      defutil,
      cgutils,cgobj;

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
        cgpara.size:=OS_INT;
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


    function tsparcparamanager.copy_value_on_stack(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
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
        case def.deftype of
          recorddef,
          arraydef,
          variantdef,
          formaldef :
            push_addr_param:=true;
          objectdef :
            result:=is_object(def);
          stringdef :
            result:=(tstringdef(def).string_typ in [st_shortstring,st_longstring]);
          procvardef :
            result:=(po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            result:=(tsetdef(def).settype<>smallset);
        end;
      end;


    procedure tsparcparamanager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        retcgsize  : tcgsize;
      begin
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
            p.funcretloc[side].loc:=LOC_VOID;
            exit;
          end;

        { Return in FPU register? }
        if p.rettype.def.deftype=floatdef then
          begin
            p.funcretloc[side].loc:=LOC_FPUREGISTER;
            p.funcretloc[side].register:=NR_FPU_RESULT_REG;
            if retcgsize=OS_F64 then
              setsubreg(p.funcretloc[side].register,R_SUBFD);
            p.funcretloc[side].size:=retcgsize;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
{$ifndef cpu64bit}
            if retcgsize in [OS_64,OS_S64] then
             begin
               p.funcretloc[side].loc:=LOC_REGISTER;
               { high }
               if (side=callerside)  or (p.proccalloption=pocall_inline)then
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
               { low }
               if (side=callerside) or (p.proccalloption=pocall_inline) then
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
             end
            else
{$endif cpu64bit}
             begin
               p.funcretloc[side].loc:=LOC_REGISTER;
               p.funcretloc[side].size:=retcgsize;
               if (side=callerside)  or (p.proccalloption=pocall_inline)then
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(retcgsize))
               else
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(retcgsize));
             end;
          end
        else
          begin
            p.funcretloc[side].loc:=LOC_REFERENCE;
            p.funcretloc[side].size:=retcgsize;
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
               is_array_of_const(hp.vartype.def) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_G0;
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
            paralen:=tcgsize2size[paracgsize];
            while paralen>0 do
              begin
                paraloc:=hp.paraloc[side].add_location;
                { Floats are passed in int registers,
                  We can allocate at maximum 32 bits per register }
                if paracgsize in [OS_64,OS_S64,OS_F32,OS_F64] then
                  paraloc^.size:=OS_32
                else
                  paraloc^.size:=paracgsize;
                if (intparareg<=high(tparasupregs)) then
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
                    inc(parasize,align(tcgsize2size[paraloc^.size],sizeof(aint)));
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
{
  $Log$
  Revision 1.51  2004-11-22 22:01:19  peter
    * fixed varargs
    * replaced dynarray with tlist

  Revision 1.50  2004/11/21 18:13:31  peter
    * fixed funcretloc for sparc

  Revision 1.49  2004/11/21 17:54:59  peter
    * ttempcreatenode.create_reg merged into .create with parameter
      whether a register is allowed
    * funcret_paraloc renamed to funcretloc

  Revision 1.48  2004/11/21 17:17:04  florian
    * changed funcret location back to tlocation

  Revision 1.47  2004/11/15 23:35:31  peter
    * tparaitem removed, use tparavarsym instead
    * parameter order is now calculated from paranr value in tparavarsym

  Revision 1.46  2004/11/07 00:33:45  florian
    * marked o* registers as volatile

  Revision 1.45  2004/10/24 17:32:53  florian
    * fixed several arm compiler bugs

  Revision 1.44  2004/10/05 20:41:02  peter
    * more spilling rewrites

  Revision 1.43  2004/09/27 21:24:17  peter
    * fixed passing of flaot parameters. The general size is still float,
      only the size of the locations is now OS_32

  Revision 1.42  2004/09/25 20:28:39  florian
    * handling of C styled varargs fixed

  Revision 1.41  2004/09/21 17:25:13  peter
    * paraloc branch merged

  Revision 1.40.4.4  2004/09/19 18:08:15  peter
    * fixed order of 64 bit funcret registers

  Revision 1.40.4.3  2004/09/17 17:19:26  peter
    * fixed 64 bit unaryminus for sparc
    * fixed 64 bit inlining
    * signness of not operation

  Revision 1.40.4.2  2004/09/12 13:36:40  peter
    * fixed alignment issues

  Revision 1.40.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.40  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.39  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.38.2.2  2004/05/31 22:08:21  peter
    * fix passing of >6 arguments

  Revision 1.38.2.1  2004/05/25 21:38:35  peter
    * constructors return self

  Revision 1.38  2004/03/15 14:39:56  mazen
  * make sparc para manager quite similar to ppc one to help
    copying evolutions.
  + Add support to var args in registers. need to be verfied as it
    was just copying ppc's one

  Revision 1.37  2004/03/09 13:05:49  mazen
  + give location for 64bit to fix IE 200402061

}
