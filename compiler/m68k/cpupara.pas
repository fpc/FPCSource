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
       cpubase,
       symdef,paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tm68kparamanager = class(tparamanager)
          function getintparaloc(nr : longint) : tparalocation;override;
          procedure create_param_loc_info(p : tabstractprocdef);override;
          function getselflocation(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       verbose,
       globals,
       globtype,
       systems,
       cpuinfo,cginfo,cgbase,
       defutil;

    function tm68kparamanager.getintparaloc(nr : longint) : tparalocation;
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
              result.reference.index.enum:=frame_pointer_reg;
              result.reference.offset:=target_info.first_parm_offset
                  +nr*4;
           end;
      end;

    procedure tm68kparamanager.create_param_loc_info(p : tabstractprocdef);
      var
        param_offset : integer;  
        hp : tparaitem;
      begin
         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         param_offset := target_info.first_parm_offset;    
         hp:=tparaitem(p.para.last);
         while assigned(hp) do
           begin
              hp.paraloc.loc:=LOC_REFERENCE;
              hp.paraloc.sp_fixup:=0;
              hp.paraloc.reference.index.enum:=frame_pointer_reg;
              hp.paraloc.reference.offset:=param_offset;
              inc(param_offset,aktalignment.paraalign);  
              hp.paraloc.size := def_cgsize(hp.paratype.def);
              hp:=tparaitem(hp.previous);
           end;
      end;

    function tm68kparamanager.getselflocation(p : tabstractprocdef) : tparalocation;
      begin
         getselflocation.loc:=LOC_REFERENCE;
         getselflocation.reference.index.enum:=R_SP;
         getselflocation.reference.offset:=4;
      end;

begin
   paramanager:=tm68kparamanager.create;
end.

{
  $Log$
  Revision 1.4  2003-02-02 19:25:54  carl
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
