{
    $Id$
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
{ Generates the argument location information for i386.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       cclasses,globtype,
       aasmtai,
       cpubase,
       cgbase,
       symconst,symtype,symdef,paramgr;

    type
       ti386paramanager = class(tparamanager)
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
          function getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;override;
       private
          procedure create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
          procedure create_stdcall_paraloc_info(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                                var parasize:longint);
          procedure create_register_paraloc_info(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                                 var parareg,parasize:longint);
       end;

  implementation

    uses
       cutils,
       systems,verbose,
       defutil,
       cpuinfo;

      const
        parasupregs : array[0..2] of tsuperregister = (RS_EAX,RS_EDX,RS_ECX);

{****************************************************************************
                                TI386PARAMANAGER
****************************************************************************}

    function ti386paramanager.ret_in_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        case target_info.system of
          system_i386_win32 :
            begin
              case def.deftype of
                recorddef :
                  begin
                    { Win32 GCC returns small records in the FUNCTION_RETURN_REG.
                      For stdcall we follow delphi instead of GCC }
                    if (calloption in [pocall_cdecl,pocall_cppdecl]) and
                       (def.size<=8) then
                     begin
                       result:=false;
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
        case target_info.system of
          system_i386_win32 :
            begin
              case def.deftype of
                recorddef :
                  begin
                    { Win32 passes small records on the stack for call by
                      value }
                    if (calloption in [pocall_stdcall,pocall_cdecl,pocall_cppdecl]) and
                       (varspez=vs_value) and
                       (def.size<=8) then
                     begin
                       result:=false;
                       exit;
                     end;
                  end;
                arraydef :
                  begin
                    { Win32 passes arrays on the stack for call by
                      value }
                    if (calloption in [pocall_stdcall,pocall_cdecl,pocall_cppdecl]) and
                       (varspez=vs_value) and
                       (tarraydef(def).highrange>=tarraydef(def).lowrange) then
                     begin
                       result:=true;
                       exit;
                     end;
                  end;
              end;
            end;
        end;
        result:=inherited push_addr_param(varspez,def,calloption);
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
          pocall_compilerproc :
            begin
              if pocall_default=pocall_oldfpccall then
                result:=[RS_EAX,RS_EDX,RS_ECX,RS_ESI,RS_EDI,RS_EBX]
              else
                result:=[RS_EAX,RS_EDX,RS_ECX];
            end;
          pocall_inline,
          pocall_register,
          pocall_safecall,
          pocall_stdcall,
          pocall_cdecl,
          pocall_cppdecl :
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


    function ti386paramanager.getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;
      begin
         fillchar(result,sizeof(tparalocation),0);
         result.size:=OS_INT;
         result.lochigh:=LOC_INVALID;
         result.alignment:=get_para_align(calloption);
         if calloption=pocall_register then
           begin
             if (nr<=high(parasupregs)+1) then
               begin
                 if nr=0 then
                   internalerror(200309271);
                 result.loc:=LOC_REGISTER;
                 result.register:=newreg(R_INTREGISTER,parasupregs[nr-1],R_SUBWHOLE);
               end
             else
               begin
                 result.loc:=LOC_REFERENCE;
                 result.reference.index:=NR_STACK_POINTER_REG;
                 result.reference.offset:=sizeof(aint)*nr;
               end;
           end
         else
           begin
             result.loc:=LOC_REFERENCE;
             result.reference.index:=NR_STACK_POINTER_REG;
             result.reference.offset:=sizeof(aint)*nr;
           end;
      end;


    procedure ti386paramanager.create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        paraloc : tparalocation;
      begin
        { Function return }
        fillchar(paraloc,sizeof(tparalocation),0);
        if (p.proctypeoption=potype_constructor) then
          paraloc.size:=OS_ADDR
        else
          paraloc.size:=def_cgsize(p.rettype.def);
        paraloc.lochigh:=LOC_INVALID;
        if paraloc.size<>OS_NO then
          begin
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
                if paraloc.size in [OS_64,OS_S64] then
                  begin
                    paraloc.lochigh:=LOC_REGISTER;
                    paraloc.register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
                    paraloc.register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
                  end
                else
                  paraloc.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(paraloc.size));
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
              end;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;


    procedure ti386paramanager.create_stdcall_paraloc_info(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                                           var parasize:longint);
      var
        hp : tparaitem;
        paraloc : tparalocation;
        l,
        varalign,
        paraalign : longint;
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
          That means the for pushes the para with the
          highest offset (see para3) needs to be pushed first
        }
        hp:=firstpara;
        while assigned(hp) do
          begin
            if hp.paratyp in [vs_var,vs_out] then
              paraloc.size:=OS_ADDR
            else
              paraloc.size:=def_cgsize(hp.paratype.def);
            paraloc.loc:=LOC_REFERENCE;
            paraloc.lochigh:=LOC_INVALID;
            paraloc.alignment:=paraalign;
            if side=callerside then
              paraloc.reference.index:=NR_STACK_POINTER_REG
            else
              paraloc.reference.index:=NR_FRAME_POINTER_REG;
            l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
            varalign:=used_align(size_2_align(l),paraalign,paraalign);
            paraloc.reference.offset:=parasize;
            parasize:=align(parasize+l,varalign);
            hp.paraloc[side]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { Adapt offsets for left-to-right calling }
        if p.proccalloption in pushleftright_pocalls then
          begin
            hp:=tparaitem(p.para.first);
            while assigned(hp) do
              begin
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=used_align(size_2_align(l),paraalign,paraalign);
                l:=align(l,varalign);
                hp.paraloc[side].reference.offset:=parasize-hp.paraloc[side].reference.offset-l;
                if side=calleeside then
                  inc(hp.paraloc[side].reference.offset,target_info.first_parm_offset);
                hp:=tparaitem(hp.next);
              end;
          end
        else
          begin
            { Only need to adapt the callee side to include the
              standard stackframe size }
            if side=calleeside then
              begin
                hp:=tparaitem(p.para.first);
                while assigned(hp) do
                  begin
                    inc(hp.paraloc[side].reference.offset,target_info.first_parm_offset);
                    hp:=tparaitem(hp.next);
                  end;
               end;
          end;
      end;


    procedure ti386paramanager.create_register_paraloc_info(p : tabstractprocdef; side: tcallercallee;firstpara:tparaitem;
                                                            var parareg,parasize:longint);
      var
        hp : tparaitem;
        paraloc : tparalocation;
        subreg : tsubregister;
        pushaddr,
        is_64bit : boolean;
        l,
        varalign,
        paraalign : longint;
      begin
        paraalign:=get_para_align(p.proccalloption);
        { Register parameters are assigned from left to right }
        hp:=firstpara;
        while assigned(hp) do
          begin
            pushaddr:=push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption);
            if pushaddr then
              paraloc.size:=OS_ADDR
            else
              paraloc.size:=def_cgsize(hp.paratype.def);
            paraloc.alignment:=paraalign;
            is_64bit:=(paraloc.size in [OS_64,OS_S64,OS_F64]);
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
               not(
                   is_64bit or
                   ((hp.paratype.def.deftype in [floatdef,recorddef,arraydef]) and
                    (not pushaddr))
                  ) then
              begin
                paraloc.loc:=LOC_REGISTER;
                paraloc.lochigh:=LOC_INVALID;
                if (paraloc.size=OS_NO) or is_64bit then
                  subreg:=R_SUBWHOLE
                else
                  subreg:=cgsize2subreg(paraloc.size);
                paraloc.alignment:=paraalign;
                paraloc.register:=newreg(R_INTREGISTER,parasupregs[parareg],subreg);
                inc(parareg);
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
                paraloc.lochigh:=LOC_INVALID;
                if side=callerside then
                  paraloc.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc.reference.index:=NR_FRAME_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=size_2_align(l);
                paraloc.reference.offset:=parasize;
                varalign:=used_align(varalign,paraalign,paraalign);
                parasize:=align(parasize+l,varalign);
              end;
            hp.paraloc[side]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { Register parameters are assigned from left-to-right, adapt offset
          for calleeside to be reversed }
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            if (hp.paraloc[side].loc=LOC_REFERENCE) then
              begin
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=used_align(size_2_align(l),paraalign,paraalign);
                l:=align(l,varalign);
                hp.paraloc[side].reference.offset:=parasize-hp.paraloc[side].reference.offset-l;
                if side=calleeside then
                  inc(hp.paraloc[side].reference.offset,target_info.first_parm_offset);
              end;
            hp:=tparaitem(hp.next);
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
            create_register_paraloc_info(p,side,tparaitem(p.para.first),parareg,parasize);
          pocall_inline,
          pocall_compilerproc,
          pocall_internproc :
            begin
              { Use default calling }
              if (pocall_default=pocall_register) then
                create_register_paraloc_info(p,side,tparaitem(p.para.first),parareg,parasize)
              else
                create_stdcall_paraloc_info(p,side,tparaitem(p.para.first),parasize);
            end;
          else
            create_stdcall_paraloc_info(p,side,tparaitem(p.para.first),parasize);
        end;
        create_funcret_paraloc_info(p,side);
        result:=parasize;
      end;


    function ti386paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargspara):longint;
      var
        parasize : longint;
      begin
        parasize:=0;
        { calculate the registers for the normal parameters }
        create_stdcall_paraloc_info(p,callerside,tparaitem(p.para.first),parasize);
        { append the varargs }
        create_stdcall_paraloc_info(p,callerside,tparaitem(varargspara.first),parasize);
        result:=parasize;
      end;



begin
   paramanager:=ti386paramanager.create;
end.
{
  $Log$
  Revision 1.54  2004-07-09 23:30:13  jonas
    *  changed first_sse_imreg to first_mm_imreg

  Revision 1.53  2004/07/09 23:09:02  peter
    * varargs calculation fixed, it's now the same as the other
      targets

  Revision 1.52  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.51  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.50.2.3  2004/05/02 21:37:35  florian
    * setting of func. ret. for i386 fixed

  Revision 1.50.2.2  2004/05/02 12:45:32  peter
    * enabled cpuhasfixedstack for x86-64 again
    * fixed size of temp allocation for parameters

  Revision 1.50.2.1  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.50  2004/02/09 22:14:17  peter
    * more x86_64 parameter fixes
    * tparalocation.lochigh is now used to indicate if registerhigh
      is used and what the type is

}
