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
       verbose;

    function tm68kparamanager.getintparaloc(nr : longint) : tparalocation;
      begin
      end;

    procedure tm68kparamanager.create_param_loc_info(p : tabstractprocdef);
      begin
         { set default para_alignment to target_info.stackalignment }
         { if para_alignment=0 then
           para_alignment:=aktalignment.paraalign;
         }
      end;

    function tm68kparamanager.getselflocation(p : tabstractprocdef) : tparalocation;
      begin
         getselflocation.loc:=LOC_REFERENCE;
         getselflocation.reference.index:=R_SP;
         getselflocation.reference.offset:=4;
      end;

begin
   paramanager:=tm68kparamanager.create;
end.

{
  $Log$
  Revision 1.1  2002-08-12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu


}
