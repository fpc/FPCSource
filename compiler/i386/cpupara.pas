{
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for i386

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
       aasmtai,aasmdata,cpubase,cgbase,
       symconst,symtype,symsym,symdef,
       parabase,paramgr;

    type
       ti386paramanager = class(tparamanager)
          function param_use_paraloc(const cgpara:tcgpara):boolean;override;
          function ret_in_param(def : tdef;calloption : tproccalloption) : boolean;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function get_para_align(calloption : tproccalloption):byte;override;
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;override;
          { Returns the location for the nr-st 32 Bit int parameter
            if every parameter before is an 32 Bit int parameter as well
            and if the calling conventions for the helper routines of the
            rtl are used.
          }
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
          procedure createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);override;
       private
          procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
          procedure create_stdcall_paraloc_info(p : tabstractprocdef; side: tcallercallee;paras:tparalist;var parasize:longint);
          procedure create_register_paraloc_info(p : tabstractprocdef; side: tcallercallee;paras:tparalist;var parareg,parasize:longint);
       end;


  implementation

    uses
       cutils,
       systems,verbose,
       defutil,
       cgutils;

      const
        parasupregs : array[0..2] of tsuperregister = (RS_EAX,RS_EDX,RS_ECX);

{****************************************************************************
                                TI386PARAMANAGER
****************************************************************************}

    function ti386paramanager.param_use_paraloc(const cgpara:tcgpara):boolean;
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


    function ti386paramanager.ret_in_param(def : tdef;calloption : tproccalloption) : boolean;
      var
        size: longint;
      begin
        case target_info.system of
          system_i386_win32 :
            begin
              if calloption=pocall_safecall then
                begin
                  result:=true;
                  exit;
                end
              else
                case def.typ of
                  recorddef :
                    begin
                      { Win32 GCC returns small records in the FUNCTION_RETURN_REG.
                        For stdcall we follow delphi instead of GCC }
                      if (calloption in [pocall_cdecl,pocall_cppdecl]) and
                         (def.size>0) and
                         (def.size<=8) then
                       begin
                         result:=false;
                         exit;
                       end;
                    end;
                end;
            end;
          system_i386_darwin :
            begin
              case def.typ of
                recorddef :
                  begin
                    size := def.size;
                    if (size > 0) and
                       (size <= 8) and
                       { only if size is a power of 2 }
                       ((size and (size-1)) = 0) then
                      begin
                        result := false;
                        exit;
                      end;
                  end;
              end;
            end;
        end;
        result:=inherited ret_in_param(def,calloption);
      end;


    function ti386paramanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
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
          variantdef :
            begin
              { variants are small enough to be passed by value except if
                required by the windows api

                variants are somethings very delphi/windows specific so do it like
                windows/delphi (FK)
              }
              if ((target_info.system=system_i386_win32) and
                 (calloption=pocall_stdcall) and
                 (varspez=vs_const)) or
                 (calloption=pocall_register) then
                result:=true
              else
                result:=false;
            end;
          formaldef :
            result:=true;
          recorddef :
            begin
              { Delphi stdcall passes records on the stack for call by value }
              if (target_info.system=system_i386_win32) and
                 (calloption=pocall_stdcall) and
                 (varspez=vs_value) then
                result:=false
              else
                result:=
                  (not(calloption in [pocall_cdecl,pocall_cppdecl,pocall_mwpascal]) and
                   (def.size>sizeof(aint))) or
                  (((calloption = pocall_mwpascal) or (target_info.system=system_i386_wince)) and
                   (varspez=vs_const));
            end;
          arraydef :
            begin
              { array of const values are pushed on the stack as
                well as dyn. arrays }
              if (calloption in [pocall_cdecl,pocall_cppdecl]) then
                result:=not(is_array_of_const(def) or
                        is_dynamic_array(def))
              else
                begin
                  result:=(
                           (tarraydef(def).highrange>=tarraydef(def).lowrange) and
                           (def.size>sizeof(aint))
                          ) or
                          is_open_array(def) or
                          is_array_of_const(def) or
                          is_array_constructor(def);
                end;
            end;
          objectdef :
            result:=is_object(def);
          stringdef :
            result:= (tstringdef(def).stringtype in [st_shortstring,st_longstring]);
          procvardef :
            result:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and (po_methodpointer in tprocvardef(def).procoptions);
          setdef :
            result:=not(calloption in [pocall_cdecl,pocall_cppdecl]) and (not is_smallset(def));
        end;
      end;


    function ti386paramanager.get_para_align(calloption : tproccalloption):byte;
      begin
        if calloption=pocall_oldfpccall then
          begin
            if target_info.system in [system_i386_go32v2,system_i386_watcom] then
              result:=2
            else
              result:=4;
          end
        else
          result:=std_param_align;
      end;


    function ti386paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        case calloption of
          pocall_internproc :
            result:=[];
          pocall_register,
          pocall_safecall,
          pocall_stdcall,
          pocall_cdecl,
          pocall_cppdecl,
          pocall_mwpascal :
            result:=[RS_EAX,RS_EDX,RS_ECX];
          pocall_far16,
          pocall_pascal,
          pocall_oldfpccall :
            result:=[RS_EAX,RS_EDX,RS_ECX,RS_ESI,RS_EDI,RS_EBX];
          else
            internalerror(200309071);
        end;
      end;


    function ti386paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[0..first_fpu_imreg-1];
      end;


    function ti386paramanager.get_volatile_registers_mm(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[0..first_mm_imreg-1];
      end;


    procedure ti386paramanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        cgpara.reset;
        cgpara.size:=OS_ADDR;
        cgpara.intsize:=sizeof(pint);
        cgpara.alignment:=get_para_align(calloption);
        paraloc:=cgpara.add_location;
        with paraloc^ do
         begin
           size:=OS_INT;
           if calloption=pocall_register then
             begin
               if (nr<=high(parasupregs)+1) then
                 begin
                   if nr=0 then
                     internalerror(200309271);
                   loc:=LOC_REGISTER;
                   register:=newreg(R_INTREGISTER,parasupregs[nr-1],R_SUBWHOLE);
                 end
               else
                 begin
                   loc:=LOC_REFERENCE;
                   reference.index:=NR_STACK_POINTER_REG;
                   reference.offset:=sizeof(aint)*nr;
                 end;
             end
           else
             begin
               loc:=LOC_REFERENCE;
               reference.index:=NR_STACK_POINTER_REG;
               reference.offset:=sizeof(aint)*nr;
             end;
          end;
      end;


    procedure ti386paramanager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        retcgsize  : tcgsize;
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
        { Return is passed as var parameter }
        if ret_in_param(p.returndef,p.proccalloption) then
          begin
            p.funcretloc[side].loc:=LOC_REFERENCE;
            p.funcretloc[side].size:=retcgsize;
            exit;
          end;
        { Return in FPU register? }
        if p.returndef.typ=floatdef then
          begin
            p.funcretloc[side].loc:=LOC_FPUREGISTER;
            p.funcretloc[side].register:=NR_FPU_RESULT_REG;
            p.funcretloc[side].size:=retcgsize;
          end
        else
         { Return in register }
          begin
            if retcgsize in [OS_64,OS_S64] then
             begin
               { low 32bits }
               p.funcretloc[side].loc:=LOC_REGISTER;
               p.funcretloc[side].size:=OS_64;
               if side=callerside then
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
               { high 32bits }
               if side=callerside then
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
             begin
               p.funcretloc[side].loc:=LOC_REGISTER;
               p.funcretloc[side].size:=retcgsize;
               if side=callerside then
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(retcgsize))
               else
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(retcgsize));
             end;
          end;
      end;


    procedure ti386paramanager.create_stdcall_paraloc_info(p : tabstractprocdef; side: tcallercallee;paras:tparalist;var parasize:longint);
      var
        i  : integer;
        hp : tparavarsym;
        paraloc : pcgparalocation;
        l,
        paralen,
        varalign   : longint;
        paraalign  : shortint;
        pushaddr   : boolean;
        paracgsize : tcgsize;
      begin
        paraalign:=get_para_align(p.proccalloption);
        { we push Flags and CS as long
          to cope with the IRETD
          and we save 6 register + 4 selectors }
        if po_interrupt in p.procoptions then
          inc(parasize,8+6*4+4*2);
        { Offset is calculated like:
           sub esp,12
           mov [esp+8],para3
           mov [esp+4],para2
           mov [esp],para1
           call function
          That means for pushes the para with the
          highest offset (see para3) needs to be pushed first
        }
        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            pushaddr:=push_addr_param(hp.varspez,hp.vardef,p.proccalloption);
            if pushaddr then
              begin
                paralen:=sizeof(aint);
                paracgsize:=OS_ADDR;
              end
            else
              begin
                paralen:=push_size(hp.varspez,hp.vardef,p.proccalloption);
                { darwin/x86 requires that parameters < sizeof(aint) are sign/ }
                { zero extended to sizeof(aint)                                }
                if (target_info.system = system_i386_darwin) and
                   (side = callerside) and
                   (paralen > 0) and
                   (paralen < sizeof(aint)) then
                  begin
                    paralen := sizeof(aint);
                    paracgsize:=OS_INT;
                  end
                else
                  paracgsize:=def_cgsize(hp.vardef);
              end;
            hp.paraloc[side].reset;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].intsize:=paralen;
            hp.paraloc[side].Alignment:=paraalign;
            { Copy to stack? }
            if (paracgsize=OS_NO) or
               (use_fixed_stack) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                paraloc^.loc:=LOC_REFERENCE;
                paraloc^.size:=paracgsize;
                if side=callerside then
                  paraloc^.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                varalign:=used_align(size_2_align(paralen),paraalign,paraalign);

                { don't let push_size return 16, because then we can    }
                { read past the end of the heap since the value is only }
                { 10 bytes long (JM)                                    }
                if (paracgsize = OS_F80) and
                   (target_info.system = system_i386_darwin) then
                  paralen:=16;
                paraloc^.reference.offset:=parasize;
                if side=calleeside then
                  inc(paraloc^.reference.offset,target_info.first_parm_offset);
                parasize:=align(parasize+paralen,varalign);
              end
            else
              begin
                if paralen=0 then
                  internalerror(200501163);
                while (paralen>0) do
                  begin
                    paraloc:=hp.paraloc[side].add_location;
                    paraloc^.loc:=LOC_REFERENCE;
                    { single and double need a single location }
                    if (paracgsize in [OS_F64,OS_F32]) then
                      begin
                        paraloc^.size:=paracgsize;
                        l:=paralen;
                      end
                    else
                      begin
                        { We can allocate at maximum 32 bits per location }
                        if paralen>sizeof(aint) then
                          l:=sizeof(aint)
                        else
                          l:=paralen;
                        paraloc^.size:=int_cgsize(l);
                      end;
                    if side=callerside then
                      paraloc^.reference.index:=NR_STACK_POINTER_REG
                    else
                      paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                    varalign:=used_align(size_2_align(l),paraalign,paraalign);
                    paraloc^.reference.offset:=parasize;
                    if side=calleeside then
                      inc(paraloc^.reference.offset,target_info.first_parm_offset);
                    parasize:=align(parasize+l,varalign);
                    dec(paralen,l);
                  end;
              end;
          end;
      end;


    procedure ti386paramanager.create_register_paraloc_info(p : tabstractprocdef; side: tcallercallee;paras:tparalist;
                                                            var parareg,parasize:longint);
      var
        hp : tparavarsym;
        paraloc : pcgparalocation;
        paracgsize : tcgsize;
        i : integer;
        l,
        paralen,
        varalign : longint;
        pushaddr : boolean;
        paraalign : shortint;
        pass : byte;
      begin
        if paras.count=0 then
          exit;
        paraalign:=get_para_align(p.proccalloption);

        { clean up here so we can later detect properly if a parameter has been
          assigned or not
        }
        for i:=0 to paras.count-1 do
          tparavarsym(paras[i]).paraloc[side].reset;
        { Register parameters are assigned from left to right,
          stack parameters from right to left so assign first the
          register parameters in a first pass, in the second
          pass all unhandled parameters are done }
        for pass:=1 to 2 do
          begin
            if pass=1 then
              i:=0
            else
              i:=paras.count-1;
            while true do
              begin
                hp:=tparavarsym(paras[i]);
                if not(assigned(hp.paraloc[side].location)) then
                  begin

                    pushaddr:=push_addr_param(hp.varspez,hp.vardef,p.proccalloption);
                    if pushaddr then
                      begin
                        paralen:=sizeof(aint);
                        paracgsize:=OS_ADDR;
                      end
                    else
                      begin
                        paralen:=push_size(hp.varspez,hp.vardef,p.proccalloption);
                        paracgsize:=def_cgsize(hp.vardef);
                      end;
                    hp.paraloc[side].size:=paracgsize;
                    hp.paraloc[side].intsize:=paralen;
                    hp.paraloc[side].Alignment:=paraalign;
                    {
                      EAX
                      EDX
                      ECX
                      Stack
                      Stack

                      64bit values,floats,arrays and records are always
                      on the stack.
                    }
                    if (parareg<=high(parasupregs)) and
                       (paralen<=sizeof(aint)) and
                       (
                        not(hp.vardef.typ in [floatdef,recorddef,arraydef]) or
                        pushaddr
                       ) then
                      begin
                        if pass=1 then
                          begin
                            paraloc:=hp.paraloc[side].add_location;
                            paraloc^.size:=paracgsize;
                            paraloc^.loc:=LOC_REGISTER;
                            paraloc^.register:=newreg(R_INTREGISTER,parasupregs[parareg],cgsize2subreg(paracgsize));
                            inc(parareg);
                          end;
                      end
                    else
                      if pass=2 then
                        begin
                          { Copy to stack? }
                          if (use_fixed_stack) or
                             (paracgsize=OS_NO) then
                            begin
                              paraloc:=hp.paraloc[side].add_location;
                              paraloc^.loc:=LOC_REFERENCE;
                              paraloc^.size:=paracgsize;
                              if side=callerside then
                                paraloc^.reference.index:=NR_STACK_POINTER_REG
                              else
                                paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                              varalign:=used_align(size_2_align(paralen),paraalign,paraalign);
                              paraloc^.reference.offset:=parasize;
                              if side=calleeside then
                                inc(paraloc^.reference.offset,target_info.first_parm_offset);
                              parasize:=align(parasize+paralen,varalign);
                            end
                          else
                            begin
                              if paralen=0 then
                                internalerror(200501163);
                              while (paralen>0) do
                                begin
                                  paraloc:=hp.paraloc[side].add_location;
                                  paraloc^.loc:=LOC_REFERENCE;
                                  { Extended and double need a single location }
                                  if (paracgsize in [OS_F64,OS_F32]) then
                                    begin
                                      paraloc^.size:=paracgsize;
                                      l:=paralen;
                                    end
                                  else
                                    begin
                                      { We can allocate at maximum 32 bits per location }
                                      if paralen>sizeof(aint) then
                                        l:=sizeof(aint)
                                      else
                                        l:=paralen;
                                      paraloc^.size:=int_cgsize(l);
                                    end;
                                  if side=callerside then
                                    paraloc^.reference.index:=NR_STACK_POINTER_REG
                                  else
                                    paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                                  varalign:=used_align(size_2_align(l),paraalign,paraalign);
                                  paraloc^.reference.offset:=parasize;
                                  if side=calleeside then
                                    inc(paraloc^.reference.offset,target_info.first_parm_offset);
                                  parasize:=align(parasize+l,varalign);
                                  dec(paralen,l);
                                end;
                            end;
                        end;
                  end;
                case pass of
                  1:
                    begin
                      if i=paras.count-1 then
                        break;
                      inc(i);
                    end;
                  2:
                    begin
                      if i=0 then
                        break;
                      dec(i);
                    end;
                end;
              end;
          end;
      end;


    function ti386paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        parasize,
        parareg : longint;
      begin
        parasize:=0;
        parareg:=0;
        case p.proccalloption of
          pocall_register :
            create_register_paraloc_info(p,side,p.paras,parareg,parasize);
          pocall_internproc :
            begin
              { Use default calling }
              if (pocall_default=pocall_register) then
                create_register_paraloc_info(p,side,p.paras,parareg,parasize)
              else
                create_stdcall_paraloc_info(p,side,p.paras,parasize);
            end;
          else
            create_stdcall_paraloc_info(p,side,p.paras,parasize);
        end;
        create_funcretloc_info(p,side);
        result:=parasize;
      end;


    function ti386paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        parasize : longint;
      begin
        parasize:=0;
        { calculate the registers for the normal parameters }
        create_stdcall_paraloc_info(p,callerside,p.paras,parasize);
        { append the varargs }
        create_stdcall_paraloc_info(p,callerside,varargspara,parasize);
        result:=parasize;
      end;


    procedure ti386paramanager.createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        paraloc:=parasym.paraloc[callerside].location;
        { No need for temps when value is pushed }
        if not(use_fixed_stack) and
           assigned(paraloc) and
           (paraloc^.loc=LOC_REFERENCE) and
           (paraloc^.reference.index=NR_STACK_POINTER_REG) then
          duplicateparaloc(list,calloption,parasym,cgpara)
        else
          inherited createtempparaloc(list,calloption,parasym,cgpara);
      end;


begin
   paramanager:=ti386paramanager.create;
end.
