{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for 680x0

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
{ Generates the argument location information for 680x0.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,
      symconst,symdef,
      paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tm68kparamanager = class(tparamanager)
          function getintparaloc(calloption : tproccalloption;nr : longint) : tparalocation;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
       end;

  implementation

    uses
       verbose,
       globals,
       systems,
       cpuinfo,cgbase,
       defutil;

    function tm68kparamanager.getintparaloc(calloption : tproccalloption;nr : longint) : tparalocation;
      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else
           begin
              { warning : THIS ONLY WORKS WITH INTERNAL ROUTINES,
                WHICH MUST ALWAYS PASS 4-BYTE PARAMETERS!!
              }
              result.loc:=LOC_REFERENCE;
              result.reference.index:=NR_STACK_POINTER_REG;
              result.reference.offset:=target_info.first_parm_offset+nr*4;
           end;
      end;


    function tm68kparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        param_offset : integer;
        hp : tparaitem;
        paraloc: tparalocation;
        l : longint;
        parasize : longint;
      begin
         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         param_offset := target_info.first_parm_offset;
         hp:=tparaitem(p.para.last);
         while assigned(hp) do
           begin
             paraloc.size:=def_cgsize(hp.paratype.def);
             paraloc.loc:=LOC_REFERENCE;
             paraloc.alignment:=4;
             paraloc.reference.index:=NR_FRAME_POINTER_REG;
             l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
             paraloc.reference.offset:=parasize;
             parasize:=parasize+l;
             hp.paraloc[callerside]:=paraloc;
             hp:=tparaitem(hp.next);
           end;
      end;


begin
  paramanager:=tm68kparamanager.create;
end.

{
  $Log$
  Revision 1.5  2004-01-30 12:17:18  florian
    * fixed some m68k compilation problems

  Revision 1.4  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.3  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.2  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.1  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu


}
