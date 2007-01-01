{
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for x86-64 target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
      cpubase,cgbase,
      symconst,symtype,symsym,symdef,
      aasmtai,aasmdata,
      parabase,paramgr;

    type
       tx86_64paramanager = class(tparamanager)
       private
          procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
          procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist;
                                               var intparareg,mmparareg,parasize:longint;varargsparas: boolean);
       public
          function param_use_paraloc(const cgpara:tcgpara):boolean;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function ret_in_param(def : tdef;calloption : tproccalloption) : boolean;override;
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);override;
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
       end;

  implementation

    uses
       cutils,verbose,
       systems,
       defutil,
       cgutils;

    const
      paraintsupregs : array[0..5] of tsuperregister = (RS_RDI,RS_RSI,RS_RDX,RS_RCX,RS_R8,RS_R9);
      parammsupregs : array[0..7] of tsuperregister = (RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7);

      paraintsupregs_winx64 : array[0..3] of tsuperregister = (RS_RCX,RS_RDX,RS_R8,RS_R9);
      parammsupregs_winx64 : array[0..3] of tsuperregister = (RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3);

    procedure getvalueparaloc(p : tdef;var loc1,loc2:tcgloc);
      begin
        loc1:=LOC_INVALID;
        loc2:=LOC_INVALID;
        case p.typ of
           orddef:
             begin
               loc1:=LOC_REGISTER;
               {$warning TODO 128bit also needs lochigh}
             end;
           floatdef:
             begin
               case tfloatdef(p).floattype of
                  s80real:
                    loc1:=LOC_REFERENCE;
                  s32real,
                  s64real :
                    loc1:=LOC_MMREGISTER;
                  s64currency,
                  s64comp :
                    loc1:=LOC_REGISTER;
                  s128real:
                    begin
                      loc1:=LOC_MMREGISTER;
                      loc2:=LOC_MMREGISTER;
                      {$warning TODO float 128bit needs SSEUP lochigh}
                    end;
               end;
             end;
           recorddef:
             begin
               { win64 abi }
               if ((target_info.system=system_x86_64_win64) and (p.size<=8)) or
               { linux abi }
                 ((target_info.system<>system_x86_64_win64) and (p.size<=16)) then
                 loc1:=LOC_REGISTER
               else
                 loc1:=LOC_REFERENCE;
             end;
           objectdef:
             begin
               if is_object(p) then
                 begin
                   { win64 abi }
                   if ((target_info.system=system_x86_64_win64) and (p.size<=8)) or
                   { linux abi }
                     ((target_info.system<>system_x86_64_win64) and (p.size<=16)) then
                     loc1:=LOC_REGISTER
                   else
                     loc1:=LOC_REFERENCE;
                 end
               else
                 loc1:=LOC_REGISTER;
             end;
           arraydef:
             begin
               if not(is_special_array(p)) and
                  (
                   { win64 abi }
                   ((target_info.system=system_x86_64_win64) and (p.size<=8)) or
                   { linux abi }
                   ((target_info.system<>system_x86_64_win64) and (p.size<=16))
                  ) then
                 loc1:=LOC_REGISTER
               else
                 loc1:=LOC_REFERENCE;
             end;
           variantdef:
             { linux abi }
             if target_info.system<>system_x86_64_win64 then
               loc1:=LOC_REGISTER
             else
               loc1:=LOC_REFERENCE;
           stringdef:
             if is_shortstring(p) or is_longstring(p) then
               loc1:=LOC_REFERENCE
             else
               loc1:=LOC_REGISTER;
           setdef:
             if is_smallset(p) then
               loc1:=LOC_REGISTER
             else
               loc1:=LOC_REFERENCE;
           procvardef:
             begin
               { This is a record < 16 bytes }
               if (po_methodpointer in tprocvardef(p).procoptions) then
                 begin
                   loc1:=LOC_REGISTER;
                   loc2:=LOC_REGISTER;
                 end
               else
                 loc1:=LOC_REGISTER;
             end;
           else
             begin
               { default for pointers,enums,etc }
               loc1:=LOC_REGISTER;
             end;
        end;
      end;


    function tx86_64paramanager.ret_in_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        if target_info.system=system_x86_64_win64 then
          result:=(calloption=pocall_safecall) or
            not(def.size in [1,2,4,8])
        else
          result:=inherited ret_in_param(def,calloption);
      end;


    function tx86_64paramanager.param_use_paraloc(const cgpara:tcgpara):boolean;
      var
        paraloc : pcgparalocation;
      begin
        if not assigned(cgpara.location) then
          internalerror(200410102);
        result:=true;
        { All locations are LOC_REFERENCE }
        paraloc:=cgpara.location;
        while assigned(paraloc) do
          begin
            if (paraloc^.loc<>LOC_REFERENCE) then
              begin
                result:=false;
                exit;
              end;
            paraloc:=paraloc^.next;
          end;
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tx86_64paramanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out always require address }
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        { Only vs_const, vs_value here }
        case def.typ of
          variantdef,
          formaldef :
            result:=true;
          recorddef :
            result:=(varspez=vs_const) or ((target_info.system=system_x86_64_win64) and (def.size>8));
          arraydef :
            begin
              result:=not(
                          { cdecl array of const need to be ignored and therefor be puhsed
                            as value parameter with length 0 }
                          (calloption in [pocall_cdecl,pocall_cppdecl]) and
                          (is_array_of_const(def) or
                           is_dynamic_array(def))
                         );
            end;
          objectdef :
            result:=is_object(def);
          stringdef :
            result:=(tstringdef(def).stringtype in [st_shortstring,st_longstring]);
          procvardef :
            result:=(po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            result:=(tsetdef(def).settype<>smallset);
        end;
      end;


    function tx86_64paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        if target_info.system=system_x86_64_win64 then
          result:=[RS_RAX,RS_RCX,RS_RDX,RS_R8,RS_R9,RS_R10,RS_R11]
        else
          result:=[RS_RAX,RS_RCX,RS_RDX,RS_RSI,RS_RDI,RS_R8,RS_R9,RS_R10,RS_R11];
      end;


    function tx86_64paramanager.get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;
      begin
        if target_info.system=system_x86_64_win64 then
          result:=[RS_XMM0..RS_XMM5]
        else
          result:=[RS_XMM0..RS_XMM15];
      end;


    function tx86_64paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[RS_ST0..RS_ST7];
      end;


    procedure tx86_64paramanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        cgpara.reset;
        cgpara.size:=OS_INT;
        cgpara.intsize:=tcgsize2size[OS_INT];
        cgpara.alignment:=get_para_align(calloption);
        paraloc:=cgpara.add_location;
        with paraloc^ do
         begin
           size:=OS_INT;
           if target_info.system=system_x86_64_win64 then
             begin
               if nr<1 then
                 internalerror(200304303)
               else if nr<=high(paraintsupregs_winx64)+1 then
                 begin
                    loc:=LOC_REGISTER;
                    register:=newreg(R_INTREGISTER,paraintsupregs_winx64[nr-1],R_SUBWHOLE);
                 end
               else
                 begin
                    loc:=LOC_REFERENCE;
                    reference.index:=NR_STACK_POINTER_REG;
                    reference.offset:=(nr-6)*sizeof(aint);
                 end;
             end
           else
             begin
               if nr<1 then
                 internalerror(200304303)
               else if nr<=high(paraintsupregs)+1 then
                 begin
                    loc:=LOC_REGISTER;
                    register:=newreg(R_INTREGISTER,paraintsupregs[nr-1],R_SUBWHOLE);
                 end
               else
                 begin
                    loc:=LOC_REFERENCE;
                    reference.index:=NR_STACK_POINTER_REG;
                    reference.offset:=(nr-6)*sizeof(aint);
                 end;
             end;
          end;
      end;


    procedure tx86_64paramanager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        retcgsize : tcgsize;
      begin
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          retcgsize:=OS_ADDR
        else
          retcgsize:=def_cgsize(p.returndef);
        location_reset(p.funcretloc[side],LOC_INVALID,OS_NO);
        { void has no location }
        if is_void(p.returndef) then
          begin
            location_reset(p.funcretloc[side],LOC_VOID,OS_NO);
            exit;
          end;
        { Return in FPU register? }
        if p.returndef.typ=floatdef then
          begin
            case tfloatdef(p.returndef).floattype of
              s32real,s64real:
                begin
                  p.funcretloc[side].loc:=LOC_MMREGISTER;
                  p.funcretloc[side].register:=NR_MM_RESULT_REG;
                  p.funcretloc[side].size:=retcgsize;
                end;
              s64currency,
              s64comp,
              s80real:
                begin
                  p.funcretloc[side].loc:=LOC_FPUREGISTER;
                  p.funcretloc[side].register:=NR_FPU_RESULT_REG;
                  p.funcretloc[side].size:=retcgsize;
                end;
              else
                internalerror(200405034);
            end;
          end
        else
         { Return in register? }
         if not ret_in_param(p.returndef,p.proccalloption) then
          begin
            p.funcretloc[side].loc:=LOC_REGISTER;
            p.funcretloc[side].size:=retcgsize;
            if side=callerside then
              p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(retcgsize))
            else
              p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(retcgsize));
          end
        else
          begin
            p.funcretloc[side].loc:=LOC_REFERENCE;
            p.funcretloc[side].size:=retcgsize;
          end;
      end;


    procedure tx86_64paramanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee;paras:tparalist;
                                                            var intparareg,mmparareg,parasize:longint;varargsparas: boolean);
      var
        hp         : tparavarsym;
        paraloc    : pcgparalocation;
        subreg     : tsubregister;
        pushaddr   : boolean;
        paracgsize : tcgsize;
        loc        : array[1..2] of tcgloc;
        paralen,
        locidx,
        l,i,
        varalign,
        paraalign  : longint;
      begin
        paraalign:=get_para_align(p.proccalloption);
        { Register parameters are assigned from left to right }
        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            pushaddr:=push_addr_param(hp.varspez,hp.vardef,p.proccalloption);
            if pushaddr then
              begin
                loc[1]:=LOC_REGISTER;
                loc[2]:=LOC_INVALID;
                paracgsize:=OS_ADDR;
                paralen:=sizeof(aint);
              end
            else
              begin
                getvalueparaloc(hp.vardef,loc[1],loc[2]);
                paralen:=push_size(hp.varspez,hp.vardef,p.proccalloption);
                paracgsize:=def_cgsize(hp.vardef);
              end;

            { cheat for now, we should copy the value to an mm reg as well (FK) }
            if varargsparas and
               (target_info.system = system_x86_64_win64) and
               (hp.vardef.typ = floatdef) then
              begin
                loc[1] := LOC_REGISTER;
                loc[2] := LOC_INVALID;
                if paracgsize = OS_F64 then
                  paracgsize := OS_64
                else
                  paracgsize := OS_32;
              end;

            hp.paraloc[side].reset;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].intsize:=paralen;
            hp.paraloc[side].Alignment:=paraalign;
            if paralen>0 then
              begin
                locidx:=1;
                while (paralen>0) do
                  begin
                    if locidx>2 then
                      internalerror(200501283);
                    { Enough registers free? }
                    case loc[locidx] of
                      LOC_REGISTER :
                        begin
                          { winx64 uses different registers }
                          if ((target_info.system=system_x86_64_win64) and
                            (intparareg>high(paraintsupregs_winx64))) or
                            ((target_info.system<>system_x86_64_win64) and
                            (intparareg>high(paraintsupregs))) then
                            loc[locidx]:=LOC_REFERENCE;
                        end;
                      LOC_MMREGISTER :
                        begin
                          { winx64 uses different registers }
                          if ((target_info.system=system_x86_64_win64) and
                            (mmparareg>high(parammsupregs_winx64))) or
                            ((target_info.system<>system_x86_64_win64) and
                            (mmparareg>high(parammsupregs))) then
                            loc[locidx]:=LOC_REFERENCE;
                        end;
                    end;
                    { Allocate }
                    case loc[locidx] of
                      LOC_REGISTER :
                        begin
                          paraloc:=hp.paraloc[side].add_location;
                          paraloc^.loc:=LOC_REGISTER;
                          if (paracgsize=OS_NO) or (loc[2]<>LOC_INVALID) then
                            begin
                              paraloc^.size:=OS_INT;
                              subreg:=R_SUBWHOLE;
                            end
                          else
                            begin
                              paraloc^.size:=paracgsize;
                              { s64comp is pushed in an int register }
                              if paraloc^.size=OS_C64 then
                                paraloc^.size:=OS_64;
                              subreg:=cgsize2subreg(paraloc^.size);
                            end;

                          { winx64 uses different registers }
                          if target_info.system=system_x86_64_win64 then
                            paraloc^.register:=newreg(R_INTREGISTER,paraintsupregs_winx64[intparareg],subreg)
                          else
                            paraloc^.register:=newreg(R_INTREGISTER,paraintsupregs[intparareg],subreg);

                          { matching mm register must be skipped }
                          if target_info.system=system_x86_64_win64 then
                            inc(mmparareg);

                          inc(intparareg);
                          dec(paralen,tcgsize2size[paraloc^.size]);
                        end;
                      LOC_MMREGISTER :
                        begin
                          paraloc:=hp.paraloc[side].add_location;
                          paraloc^.loc:=LOC_MMREGISTER;

                          { winx64 uses different registers }
                          if target_info.system=system_x86_64_win64 then
                            paraloc^.register:=newreg(R_MMREGISTER,parammsupregs_winx64[mmparareg],R_SUBNONE)
                          else
                            paraloc^.register:=newreg(R_MMREGISTER,parammsupregs[mmparareg],R_SUBNONE);
                          if paracgsize=OS_F128 then
                            paraloc^.size:=OS_F64
                          else
                            paraloc^.size:=paracgsize;

                          { matching int register must be skipped }
                          if target_info.system=system_x86_64_win64 then
                            inc(intparareg);

                          inc(mmparareg);
                          dec(paralen,tcgsize2size[paraloc^.size]);
                        end;
                      LOC_REFERENCE :
                        begin
                          paraloc:=hp.paraloc[side].add_location;
                          paraloc^.loc:=LOC_REFERENCE;
                          if paracgsize in [OS_F32,OS_F64,OS_F80,OS_F128] then
                            paraloc^.size:=int_float_cgsize(paralen)
                          else
                            paraloc^.size:=int_cgsize(paralen);
                          if side=callerside then
                            paraloc^.reference.index:=NR_STACK_POINTER_REG
                          else
                            paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                          varalign:=used_align(size_2_align(paralen),paraalign,paraalign);
                          paraloc^.reference.offset:=parasize;
                          parasize:=align(parasize+paralen,varalign);
                          paralen:=0;
                        end;
                    end;
                    if (locidx<2) and
                       (loc[locidx+1]<>LOC_INVALID) then
                      inc(locidx);
                  end;
              end
            else
              begin
                paraloc:=hp.paraloc[side].add_location;
                paraloc^.loc:=LOC_VOID;
              end;
          end;
        { Register parameters are assigned from left-to-right, but the
          offsets on the stack are right-to-left. There is no need
          to reverse the offset, only adapt the calleeside with the
          start offset of the first param on the stack }
        if side=calleeside then
          begin
            for i:=0 to paras.count-1 do
              begin
                hp:=tparavarsym(paras[i]);
                with hp.paraloc[side].location^ do
                  if (loc=LOC_REFERENCE) then
                    inc(reference.offset,target_info.first_parm_offset);
              end;
          end;
      end;


    function tx86_64paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        intparareg,mmparareg,
        parasize : longint;
      begin
        intparareg:=0;
        mmparareg:=0;
        if target_info.system=system_x86_64_win64 then
          parasize:=4*8
        else
          parasize:=0;
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,p.paras,intparareg,mmparareg,parasize,false);
        { append the varargs }
        create_paraloc_info_intern(p,callerside,varargspara,intparareg,mmparareg,parasize,true);
        { store used no. of SSE registers, that needs to be passed in %AL }
        varargspara.mmregsused:=mmparareg;
        result:=parasize;
      end;


    function tx86_64paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        intparareg,mmparareg,
        parasize : longint;
      begin
        intparareg:=0;
        mmparareg:=0;
        if target_info.system=system_x86_64_win64 then
          parasize:=4*8
        else
          parasize:=0;
        create_paraloc_info_intern(p,side,p.paras,intparareg,mmparareg,parasize,false);
        { Create Function result paraloc }
        create_funcretloc_info(p,side);
        { We need to return the size allocated on the stack }
        result:=parasize;
      end;


begin
   paramanager:=tx86_64paramanager.create;
end.
