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
       symtype,symdef,paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       ti386paramanager = class(tparamanager)
          function ret_in_param(def : tdef;calloption : tproccalloption) : boolean;override;
          function push_addr_param(def : tdef;calloption : tproccalloption) : boolean;override;
          function getintparaloc(list: taasmoutput; nr : longint) : tparalocation;override;
          procedure freeintparaloc(list: taasmoutput; nr : longint); override;
          function getparaloc(p : tdef) : tcgloc;
          procedure create_paraloc_info(p : tabstractprocdef);override;
          function getselflocation(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       systems,verbose,
       symconst,symsym,
       cpuinfo,
       cgbase;


    function ti386paramanager.ret_in_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        case target_info.system of
          system_i386_win32 :
            begin
              { Win32 returns small records in the FUNCTION_RETURN_REG }
              case def.deftype of
                recorddef :
                  begin
                    if (calloption in [pocall_stdcall,pocall_cdecl,pocall_cppdecl]) and (def.size<=8) then
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


    function ti386paramanager.push_addr_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        case target_info.system of
          system_i386_win32 :
            begin
              case def.deftype of
                recorddef :
                  begin
                    if (calloption=pocall_stdcall) and (def.size<=8) then
                     begin
                       result:=false;
                       exit;
                     end;
                  end;
                arraydef :
                  begin
                    if (tarraydef(def).highrange>=tarraydef(def).lowrange) and
                       (calloption in [pocall_cdecl,pocall_cppdecl]) then
                     begin
                       result:=true;
                       exit;
                     end;
                  end;
              end;
            end;
        end;
        result:=inherited push_addr_param(def,calloption);
      end;


    function ti386paramanager.getintparaloc(list: taasmoutput; nr : longint) : tparalocation;
      begin
         getintparaloc.loc:=LOC_REFERENCE;
         getintparaloc.reference.index.enum:=R_EBP;
         getintparaloc.reference.offset:=4*nr;
      end;


    procedure ti386paramanager.freeintparaloc(list: taasmoutput; nr : longint);
      begin
        { nothing to release }
      end;


    function ti386paramanager.getparaloc(p : tdef) : tcgloc;
      begin
        result:=LOC_REFERENCE;
      end;


    procedure ti386paramanager.create_paraloc_info(p : tabstractprocdef);
      var
        hp : tparaitem;
        paraloc : tparalocation;
      begin
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
              begin
                paraloc.reference.index.enum:=R_INTREGISTER;
                paraloc.reference.index.number:=NR_FRAME_POINTER_REG;
              end;
            paraloc.reference.offset:=tvarsym(hp.parasym).adjusted_address;
            hp.calleeparaloc:=paraloc;
{$warning callerparaloc shall not be the same as calleeparaloc}
            hp.callerparaloc:=paraloc;
            hp:=tparaitem(hp.next);
          end;
      end;


    function ti386paramanager.getselflocation(p : tabstractprocdef) : tparalocation;
      var
        hsym : tvarsym;
      begin
         hsym:=tvarsym(trecorddef(methodpointertype.def).symtable.search('self'));
         if not assigned(hsym) then
           internalerror(200305251);
         getselflocation.loc:=LOC_REFERENCE;
         getselflocation.sp_fixup:=POINTER_SIZE;
         getselflocation.reference.index.enum:=R_INTREGISTER;
         getselflocation.reference.index.number:=NR_STACK_POINTER_REG;
         getselflocation.reference.offset:=hsym.adjusted_address;
      end;

begin
   paramanager:=ti386paramanager.create;
end.
{
  $Log$
  Revision 1.20  2003-07-02 22:18:04  peter
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
