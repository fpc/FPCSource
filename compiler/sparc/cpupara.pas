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
      symconst,symbase,symtype,symdef,paramgr,cgbase;

    type
      TSparcParaManager=class(TParaManager)
        function  copy_value_on_stack(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
        function  get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;override;
        function  get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;override;
        {Returns a structure giving the information on the storage of the parameter
        (which must be an integer parameter)
        @param(nr Parameter number of routine, starting from 1)}
        function  getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;override;
        procedure allocparaloc(list: taasmoutput; const loc: tparalocation);override;
        procedure freeparaloc(list: taasmoutput; const loc: tparalocation);override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargspara):longint;override;
        procedure splitparaloc64(const locpara:tparalocation;var loclopara,lochipara:tparalocation);override;
      private
        procedure create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
        procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; firstpara: tparaitem;
                                             var intparareg,parasize:longint);
      end;

implementation

    uses
      verbose,systems,
      defutil,cgobj;

    type
      tparasupregs = array[0..5] of tsuperregister;
      pparasupregs = ^tparasupregs;
    const
      paraoutsupregs : tparasupregs = (RS_O0,RS_O1,RS_O2,RS_O3,RS_O4,RS_O5);
      parainsupregs  : tparasupregs = (RS_I0,RS_I1,RS_I2,RS_I3,RS_I4,RS_I5);


    function TSparcParaManager.get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;
      begin
        result:=[RS_G1];
      end;


    function tsparcparamanager.get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;
      begin
        result:=[RS_F0..RS_F31];
      end;


    function TSparcParaManager.GetIntParaLoc(calloption : tproccalloption; nr : longint) : tparalocation;
      begin
        if nr<1 then
          InternalError(2002100806);
        FillChar(GetIntParaLoc,SizeOf(TParaLocation),0);
        result.lochigh:=LOC_INVALID;
        Dec(nr);
        with GetIntParaLoc do
         begin
           { The six first parameters are passed into registers }
           if nr<6 then
            begin
              loc:=LOC_REGISTER;
              register:=newreg(R_INTREGISTER,(RS_O0+nr),R_SUBWHOLE);
            end
           else
           { The other parameters are passed on the stack }
            begin
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


    procedure tsparcparamanager.create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        paraloc : tparalocation;
      begin
        fillchar(paraloc,sizeof(tparalocation),0);
        paraloc.size:=def_cgsize(p.rettype.def);
        paraloc.Alignment:= std_param_align;
        { Constructors return self }
        if (p.proctypeoption=potype_constructor) then
          begin
            paraloc.size:=OS_ADDR;
            paraloc.loc:=LOC_REGISTER;
            if side=callerside then
              paraloc.register:=NR_FUNCTION_RESULT_REG
            else
              paraloc.register:=NR_FUNCTION_RETURN_REG;
          end
        else
         { Return in FPU register? }
         if p.rettype.def.deftype=floatdef then
          begin
            paraloc.loc:=LOC_FPUREGISTER;
            paraloc.register:=NR_FPU_RESULT_REG;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            paraloc.loc:=LOC_REGISTER;
{$ifndef cpu64bit}
            if paraloc.size in [OS_64,OS_S64] then
             begin
               paraloc.lochigh:=LOC_REGISTER;
               if side=callerside then
                 paraloc.register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 paraloc.register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
               if side=callerside then
                 paraloc.register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc.register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
{$endif cpu64bit}
             begin
               if side=callerside then
                 paraloc.register:=NR_FUNCTION_RESULT_REG
               else
                 paraloc.register:=NR_FUNCTION_RETURN_REG;
             end;
          end
        else
          begin
            paraloc.loc:=LOC_REFERENCE;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;


    procedure tsparcparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                                           var intparareg,parasize:longint);
      var
        paraloc : tparalocation;
        hp : tparaitem;
        is_64bit: boolean;
        hparasupregs : pparasupregs;
      begin
        if side=callerside then
          hparasupregs:=@paraoutsupregs
        else
          hparasupregs:=@parainsupregs;
        hp:=firstpara;
        while assigned(hp) do
          begin
            fillchar(paraloc,sizeof(paraloc),0);
            paraloc.Alignment:= std_param_align;
            if push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption) or (hp.paratyp in [vs_var,vs_out]) then
              paraloc.size:=OS_ADDR
            else
              begin
                paraloc.size:=def_cgSize(hp.paratype.def);
                if paraloc.size=OS_NO then
                  paraloc.size:=OS_ADDR;
              end;
            is_64bit:=(paraloc.size in [OS_64,OS_S64,OS_F64]);
            if (intparareg<=high(tparasupregs)-ord(is_64bit)) then
              begin
                paraloc.loc:=LOC_REGISTER;
                { big endian }
                if is_64bit then
                  begin
                    paraloc.registerhigh:=newreg(R_INTREGISTER,hparasupregs^[intparareg],R_SUBWHOLE);
                    paraloc.lochigh:=LOC_REGISTER;
                    inc(intparareg);
                  end;
                paraloc.registerlow:=newreg(R_INTREGISTER,hparasupregs^[intparareg],R_SUBWHOLE);
                inc(intparareg);
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
                { Low part need to be in O5 if still available }
                if (intparareg<=high(tparasupregs)) then
                  begin
                    paraloc.low_in_reg:=true;
                    paraloc.lowreg:=newreg(R_INTREGISTER,hparasupregs^[intparareg],R_SUBWHOLE);
                    inc(intparareg);
                  end;
                if side=callerside then
                  paraloc.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc.reference.index:=NR_FRAME_POINTER_REG;
                paraloc.reference.offset:=target_info.first_parm_offset+parasize;
                if is_64bit and
                   (not paraloc.low_in_reg) then
                  inc(parasize,8)
                else
                  inc(parasize,4);
              end;
            hp.paraloc[side]:=paraloc;
            hp:=TParaItem(hp.Next);
          end;
      end;


    function TSparcParaManager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;
      var
        intparareg,
        parasize : longint;
      begin
        intparareg:=0;
        parasize:=0;
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,tparaitem(p.para.first),intparareg,parasize);
        { append the varargs }
        create_paraloc_info_intern(p,callerside,tparaitem(varargspara.first),intparareg,parasize);
        result:=parasize;
      end;



    function tsparcparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        intparareg,
        parasize : longint;
      begin
        intparareg:=0;
        parasize:=0;
        create_paraloc_info_intern(p,side,tparaitem(p.para.first),intparareg,parasize);
        { Create Function result paraloc }
        create_funcret_paraloc_info(p,side);
        { We need to return the size allocated on the stack }
        result:=parasize;
      end;


    procedure tsparcparamanager.allocparaloc(list: taasmoutput; const loc: tparalocation);
      begin
        if (loc.loc=LOC_REFERENCE) and
           (loc.low_in_reg) then
          cg.GetExplicitRegister(list,loc.lowreg);
        inherited allocparaloc(list,loc);
      end;


    procedure tsparcparamanager.freeparaloc(list: taasmoutput; const loc: tparalocation);
      begin
        if (loc.loc=LOC_REFERENCE) and
           (loc.low_in_reg) then
          cg.UnGetRegister(list,loc.lowreg);
        inherited freeparaloc(list,loc);
      end;

    procedure tsparcparamanager.splitparaloc64(const locpara:tparalocation;var loclopara,lochipara:tparalocation);
      begin
        { Word 0 is in register, word 1 is in reference }
        if (locpara.loc=LOC_REFERENCE) and locpara.low_in_reg then
          begin
            { high }
            lochipara:=locpara;
            if locpara.size=OS_S64 then
              lochipara.size:=OS_S32
            else
              lochipara.size:=OS_32;
            lochipara.low_in_reg:=false;
            { low }
            loclopara:=locpara;
            loclopara.size:=OS_32;
            loclopara.loc:=LOC_REGISTER;
            loclopara.register:=locpara.lowreg;
          end
        else
          inherited splitparaloc64(locpara,loclopara,lochipara);
      end;


begin
   ParaManager:=TSparcParaManager.create;
end.
{
  $Log$
  Revision 1.40  2004-06-20 08:55:32  florian
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
