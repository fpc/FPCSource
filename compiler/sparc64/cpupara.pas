{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Calling conventions for the SPARC64

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
      sppara,
      symconst,symbase,symsym,symtype,symdef,paramgr,parabase,cgbase,cgutils;

    type
      tcpuparamanager=class(TSparcParaManager)
        procedure create_paraloc_info_intern(p : tabstractprocdef; side : tcallercallee; paras : tparalist; var curintreg : LongInt;
          curfloatreg : tsuperregister; var cur_stack_offset : aword);override;
        function push_addr_param(varspez : tvarspez; def : tdef; calloption : tproccalloption) : boolean;override;
        function ret_in_param(def : tdef; pd : tabstractprocdef) : boolean;override;
        function get_funcretloc(p : tabstractprocdef; side : tcallercallee; forcetempdef : tdef) : tcgpara;override;
      private
        function push_addr_param_intern(varspez : tvarspez; def : tdef; calloption : tproccalloption; recsizelimit : aword) : boolean;
        procedure create_paraloc1_info_intern(p : tabstractprocdef; side : tcallercallee; paradef : tdef; var loc : TCGPara; varspez : tvarspez; varoptions : tvaroptions; recsizelimit : aword;
          var curintreg : LongInt; var curfloatreg : tsuperregister; var cur_stack_offset : aword);
      end;

implementation

    uses
      cutils,verbose,systems,
      defutil,
      cgobj;

    { true if a parameter is too large to copy and only the address is pushed }
    function tcpuparamanager.push_addr_param_intern(varspez:tvarspez;def : tdef;calloption : tproccalloption;recsizelimit : aword) : boolean;
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
          recorddef:
            result:=def.size>recsizelimit;
          variantdef:
            result:=false;
          formaldef :
            result:=true;
          objectdef :
            result:=(is_object(def) and (def.size>recsizelimit));
          stringdef :
            result:=(tstringdef(def).stringtype in [st_shortstring,st_longstring]);
          procvardef :
            result:=false;
          setdef :
            result:=not is_smallset(def);
        end;
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tcpuparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=push_addr_param_intern(varspez,def,calloption,16);
      end;


    function tcpuparamanager.ret_in_param(def:tdef;pd:tabstractprocdef):boolean;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;

        case def.typ of
          { it is a matter of interpretation, if objects should be returned in registers according to the abi as the
            abi talks only about structures and unions

            at least for the compiler, it is a problem, if an object is returned in registers

            consider
            tobject1 = object
              function f : tobject1;
              ...
              contructor init;
            end;

            the constructor changes the size of tobject1, so its return location might change from register to memory, this
            is something the compiler could not handle currently, so we do not return objects in registers yet
          objectdef:
            begin
              result:=is_object(def) and (def.size>32);
              exit;
            end;}
          recorddef:
            begin
              result:=def.size>32;
              exit;
            end;
        end;
        result:=inherited ret_in_param(def,pd);
      end;


    procedure tcpuparamanager.create_paraloc1_info_intern(
      p : tabstractprocdef; side: tcallercallee;paradef:tdef;var loc : TCGPara;varspez : tvarspez;varoptions : tvaroptions;recsizelimit : aword;
      var curintreg: LongInt; var curfloatreg: tsuperregister; var cur_stack_offset : aword);

      procedure nextloc(currsize : TCgSize);
        begin
          if curintreg>high(tparasupregs) then
            begin
              if (currsize<low(tcgsize2size)) or (currsize>high(tcgsize2size)) then
                internalerror(2017080101);
              { Parameters are aligned at 8 bytes }
              inc(cur_stack_offset,align(tcgsize2size[currsize],sizeof(pint)));
            end;
          inc(curintreg);
          if currsize=OS_F128 then
            inc(curfloatreg,4)
          else
            inc(curfloatreg,2);
        end;

      var
        paraloc      : pcgparalocation;
        paracgsize   : tcgsize;
        hparasupregs : pparasupregs;
        paralen      : longint;
      begin
        if side=callerside then
          hparasupregs:=@paraoutsupregs
        else
          hparasupregs:=@parainsupregs;
        { currently only support C-style array of const,
          there should be no location assigned to the vararg array itself }
        if (p.proccalloption in cstylearrayofconst) and
           is_array_of_const(paradef) then
          begin
            paraloc:=loc.add_location;
            { hack: the paraloc must be valid, but is not actually used }
            paraloc^.loc:=LOC_REGISTER;
            paraloc^.register:=NR_G0;
            paraloc^.size:=OS_ADDR;
            paraloc^.def:=voidpointertype;
            exit;
          end;

        if push_addr_param_intern(varspez,paradef,p.proccalloption,recsizelimit) then
          begin
            paracgsize:=OS_ADDR;
            paradef:=cpointerdef.getreusable_no_free(paradef);
          end
        else
          begin
            paracgsize:=def_cgsize(paradef);
            if paradef.typ=formaldef then
              begin
                paracgsize:=OS_ADDR;
                paradef:=voidpointertype;
              end;
          end;
        loc.reset;
        loc.size:=paracgsize;
        loc.def:=paradef;
        if side=callerside then
          loc.Alignment:=std_param_align
        else
          loc.Alignment:=paradef.alignment;
        { sparc64 returns records up to a size of 32 in register, we cannot encode this
          in paracgsize, so paracgsize is OS_NO in this case }
        if paracgsize=OS_NO then
          paralen:=paradef.size
        else
          paralen:=tcgsize2size[paracgsize];
        loc.intsize:=paralen;
        while paralen>0 do
          begin
            paraloc:=loc.add_location;
            paraloc^.size:=paracgsize;
            paraloc^.def:=paradef;

            { ret in param? }
            if vo_is_funcret in varoptions then
              begin
                paraloc^.loc:=LOC_REFERENCE;
                paraloc^.reference.offset:=128;
                if side=callerside then
                  paraloc^.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                inc(paraloc^.reference.offset,STACK_BIAS);
              end
            { In case of po_delphi_nested_cc, the parent frame pointer
              is always passed on the stack. }
            else if (curintreg<=high(tparasupregs)) and
               (not(vo_is_parentfp in varoptions) or
                not(po_delphi_nested_cc in p.procoptions)) then
              begin
                if paraloc^.size in [OS_F32,OS_F64,OS_F128] then
                  begin
                    paraloc^.loc:=LOC_FPUREGISTER;
                    case paraloc^.size of
                      OS_F32:
                        { singles are put into the uneven register }
                        paraloc^.register:=newreg(R_FPUREGISTER,curfloatreg+1,R_SUBFS);
                      OS_F64:
                        paraloc^.register:=newreg(R_FPUREGISTER,curfloatreg,R_SUBFD);
                      OS_F128:
                        paraloc^.register:=newreg(R_FPUREGISTER,curfloatreg,R_SUBFQ);
                      else
                        Internalerror(2017072301);
                    end;
                  end
                else
                  begin
                    if paracgsize in [OS_NO,OS_128,OS_S128] then
                      begin
                        if paralen>4 then
                          begin
                            paraloc^.size:=OS_INT;
                            paraloc^.def:=u64inttype;
                          end
                        else
                          begin
                            { for 3-byte records }
                            paraloc^.size:=OS_32;
                            paraloc^.def:=u32inttype;
                          end;
                      end;

                    paraloc^.loc:=LOC_REGISTER;
                    paraloc^.register:=newreg(R_INTREGISTER,hparasupregs^[curintreg],R_SUBWHOLE);
                    { left align }
                    if (target_info.endian=endian_big) and
                       not(paraloc^.size in [OS_64,OS_S64]) and
                       (paradef.typ in [setdef,recorddef,arraydef,objectdef]) then
                      begin
                        paraloc^.shiftval:=-(8-tcgsize2size[paraloc^.size])*8;
                        paraloc^.Size:=OS_64;
                      end;
                  end;
                nextloc(paraloc^.Size);
              end
            else
              begin
                paraloc^.loc:=LOC_REFERENCE;
                paraloc^.reference.offset:=target_info.first_parm_offset+cur_stack_offset;
                if side=callerside then
                  paraloc^.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                inc(paraloc^.reference.offset,STACK_BIAS);

                if (target_info.endian=endian_big) and
                   (paralen<tcgsize2size[OS_INT]) and
                   (paradef.typ<>recorddef) then
                  inc(paraloc^.reference.offset,4-paralen);

                { Parameters are aligned to 8 byte boundaries }
                inc(cur_stack_offset,align(paralen,8));

                { a stack location covers always the remainder of a parameter }
                exit;
              end;
            dec(paralen,tcgsize2size[paraloc^.size]);
          end;
      end;


    procedure tcpuparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist;
                                                  var curintreg: LongInt; curfloatreg: tsuperregister; var cur_stack_offset : aword);
      var
        i : integer;
      begin
        for i:=0 to paras.count-1 do
          create_paraloc1_info_intern(p,side,tparavarsym(paras[i]).vardef,tparavarsym(paras[i]).paraloc[side],tparavarsym(paras[i]).varspez,
            tparavarsym(paras[i]).varoptions,16,curintreg,curfloatreg,cur_stack_offset);
      end;


    function tcpuparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
        curintreg : LongInt;
        curfloatreg : tsuperregister;
        cur_stack_offset : aword;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          exit;

        if ret_in_param(result.def,p) then
          Internalerror(2017080601);

        if is_record(result.def) or is_object(result.def) then
          begin
            curintreg:=0;
            curfloatreg:=RS_F0;
            cur_stack_offset:=0;

            create_paraloc1_info_intern(p,side,result.def,result,vs_value,
              [],32,curintreg,curfloatreg,cur_stack_offset);

            { sparc64 calling conventions are difficult, so better check if everything is ok }
            if result.location^.loc=LOC_INVALID then
              Internalerror(2017080501);
          end
        else
          begin
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


begin
   ParaManager:=tcpuparamanager.create;
end.
