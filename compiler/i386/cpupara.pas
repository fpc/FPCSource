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
       aasmtai,
       cpubase,
       globtype,
       cginfo,
       symconst,symtype,symdef,paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       ti386paramanager = class(tparamanager)
          function ret_in_param(def : tdef;calloption : tproccalloption) : boolean;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function get_volatile_registers_int(calloption : tproccalloption):tsuperregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tsuperregisterset;override;
          function getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;override;
          procedure create_paraloc_info(p : tabstractprocdef; side: tcallercallee);override;
       private
          procedure create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
          function create_stdcall_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
          function create_register_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
       end;

  implementation

    uses
       cutils,
       systems,globals,verbose,
       symsym,
       cpuinfo,
       cgbase;


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
        if calloption=pocall_register then
          begin
            case def.deftype of
              floatdef :
                begin
                  result:=true;
                  exit;
                end;
            end;
          end;
        result:=inherited push_addr_param(varspez,def,calloption);
      end;


    function ti386paramanager.get_volatile_registers_int(calloption : tproccalloption):tsuperregisterset;
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


    function ti386paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tsuperregisterset;
      begin
        result:=[first_fpu_supreg..last_fpu_supreg];;
      end;


    function ti386paramanager.getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;
      begin
         if calloption=pocall_register then
           begin
             if nr<=3 then
               begin
                 getintparaloc.loc:=LOC_REGISTER;
                 getintparaloc.register:=nr-1;
               end
             else
               begin
                 getintparaloc.loc:=LOC_REFERENCE;
                 getintparaloc.reference.index:=NR_EBP;
                 getintparaloc.reference.offset:=4*nr;
               end;
           end
         else
           begin
             getintparaloc.loc:=LOC_REFERENCE;
             getintparaloc.reference.index:=NR_EBP;
             getintparaloc.reference.offset:=4*nr;
           end;
      end;


    procedure ti386paramanager.create_funcret_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        paraloc : tparalocation;
      begin
        { Function return }
        fillchar(paraloc,sizeof(tparalocation),0);
        paraloc.size:=def_cgsize(p.rettype.def);
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
               paraloc.register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
               paraloc.register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
{$endif cpu64bit}
             begin
               paraloc.register:=NR_FUNCTION_RETURN_REG;
             end;
          end
        else
          begin
            paraloc.loc:=LOC_REFERENCE;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;


    function ti386paramanager.create_stdcall_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        hp : tparaitem;
        paraloc : tparalocation;
        l,
        varalign,
        parasize : longint;
      begin
        parasize:=0;
{$warning HACK: framepointer reg shall be a normal parameter}
        if p.parast.symtablelevel>normal_function_level then
          inc(parasize,POINTER_SIZE);
        { we push Flags and CS as long
          to cope with the IRETD
          and we save 6 register + 4 selectors }
        if po_interrupt in p.procoptions then
          inc(parasize,8+6*4+4*2);
        { Assign fields }
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            if hp.paratyp in [vs_var,vs_out] then
              paraloc.size:=OS_ADDR
            else
              paraloc.size:=def_cgsize(hp.paratype.def);
            paraloc.loc:=LOC_REFERENCE;
            if assigned(current_procinfo) then
              paraloc.reference.index:=current_procinfo.framepointer
            else
              paraloc.reference.index:=NR_FRAME_POINTER_REG;
            l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
            varalign:=size_2_align(l);
            paraloc.reference.offset:=parasize+target_info.first_parm_offset;
            varalign:=used_align(varalign,p.paraalign,p.paraalign);
            parasize:=align(parasize+l,varalign);
            if (side=callerside) then
              begin
                paraloc.reference.index:=NR_STACK_POINTER_REG;
                dec(paraloc.reference.offset,POINTER_SIZE);
              end;
            hp.paraloc[side]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { We need to return the size allocated }
        result:=parasize;
      end;


    function ti386paramanager.create_register_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        hp : tparaitem;
        paraloc : tparalocation;
        sr : tsuperregister;
        subreg : tsubregister;
        is_64bit : boolean;
        l,
        varalign,
        parasize : longint;
      begin
        sr:=RS_EAX;
        parasize:=0;
{$warning HACK: framepointer reg shall be a normal parameter}
        if p.parast.symtablelevel>normal_function_level then
          inc(parasize,POINTER_SIZE);
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            if push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption) then
              paraloc.size:=OS_ADDR
            else
              paraloc.size:=def_cgsize(hp.paratype.def);
            is_64bit:=(paraloc.size in [OS_64,OS_S64,OS_F64]);
            {
              EAX
              EDX
              ECX
              Stack
              Stack

              64bit values are in EAX:EDX or on the stack.
            }
            if (sr<=RS_ECX) and not(is_64bit) then
              begin
                paraloc.loc:=LOC_REGISTER;
                if is_64bit then
                  begin
                    paraloc.registerlow:=newreg(R_INTREGISTER,sr,R_SUBD);
                    inc(sr);
                    paraloc.registerhigh:=newreg(R_INTREGISTER,sr,R_SUBD);
                    inc(sr);
                  end
                else
                  begin
                    if (paraloc.size=OS_NO) or is_64bit then
                      subreg:=R_SUBWHOLE
                    else
                      subreg:=cgsize2subreg(paraloc.size);
                    paraloc.register:=newreg(R_INTREGISTER,sr,subreg);
                    inc(sr);
                  end;
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
                if assigned(current_procinfo) then
                  paraloc.reference.index:=current_procinfo.framepointer
                else
                  paraloc.reference.index:=NR_FRAME_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                varalign:=size_2_align(l);
                paraloc.reference.offset:=parasize+target_info.first_parm_offset;
                varalign:=used_align(varalign,p.paraalign,p.paraalign);
                parasize:=align(parasize+l,varalign);
              end;
            if (side=callerside) and
               (paraloc.loc=LOC_REFERENCE) then
              begin
                paraloc.reference.index:=NR_STACK_POINTER_REG;
                dec(paraloc.reference.offset,POINTER_SIZE);
              end;
            hp.paraloc[side]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { We need to return the size allocated }
        result:=parasize;
      end;


    procedure ti386paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee);
      var
        l : longint;
      begin
        if (p.proccalloption=pocall_register) or
           ((pocall_default=pocall_register) and
            (p.proccalloption in [pocall_compilerproc,pocall_internproc])) then
          l:=create_register_paraloc_info(p,side)
        else
          l:=create_stdcall_paraloc_info(p,side);
        create_funcret_paraloc_info(p,side);
        { Store the size of the parameters on the stack }
        if (side=calleeside) then
          current_procinfo.para_stack_size:=l;
      end;


begin
   paramanager:=ti386paramanager.create;
end.
{
  $Log$
  Revision 1.31  2003-09-25 21:30:11  peter
    * parameter fixes

  Revision 1.30  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.29  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.28  2003/09/10 08:31:47  marco
   * Patch from Peter for paraloc

  Revision 1.27  2003/09/09 21:03:17  peter
    * basics for x86 register calling

  Revision 1.26  2003/09/09 15:55:05  peter
    * winapi doesn't like pushing 8 byte record

  Revision 1.25  2003/09/08 18:28:51  peter
    * fix compilerproc for default=oldfpccall

  Revision 1.24  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.23  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.22.2.2  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.22.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.22  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.21  2003/07/05 20:11:41  jonas
    * create_paraloc_info() is now called separately for the caller and
      callee info
    * fixed ppc cycle

  Revision 1.20  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.19  2003/06/17 16:34:19  peter
    * freeintparaloc added

  Revision 1.18  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.17  2003/06/06 14:41:22  peter
    * needs cpuinfo

  Revision 1.16  2003/06/06 07:36:06  michael
  + Forgot a line in patch from peter

  Revision 1.15  2003/06/06 07:35:14  michael
  + Patch to Patch from peter

  Revision 1.14  2003/06/06 07:34:11  michael
  + Patch from peter

  Revision 1.13  2003/06/05 20:58:05  peter
    * updated

  Revision 1.12  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.11  2003/05/13 15:16:13  peter
    * removed ret_in_acc, it's the reverse of ret_in_param
    * fixed ret_in_param for win32 cdecl array

  Revision 1.10  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.9  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.8  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.7  2002/12/24 15:56:50  peter
    * stackpointer_alloc added for adjusting ESP. Win32 needs
      this for the pageprotection

  Revision 1.6  2002/12/17 22:19:33  peter
    * fixed pushing of records>8 bytes with stdcall
    * simplified hightree loading

  Revision 1.5  2002/11/18 17:32:00  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.4  2002/11/15 01:58:56  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.3  2002/08/09 07:33:04  florian
    * a couple of interface related fixes

  Revision 1.2  2002/07/11 14:41:32  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

}
