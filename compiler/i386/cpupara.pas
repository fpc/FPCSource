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

  interface

    uses
       cpubase,
       symdef;

    var
       paralocdummy : tparalocation;

    { Returns the location for the nr-st 32 Bit int parameter
      if every parameter before is an 32 Bit int parameter as well
      and if the calling conventions for the helper routines of the
      rtl are used.
    }
    function getintparaloc(nr : longint) : tparalocation;
    procedure create_param_loc_info(const p : tparaitem);

  implementation

    function getintparaloc(nr : longint) : tparalocation;

      begin
      end;
    procedure create_param_loc_info(const p : tparaitem);

      begin
         { set default para_alignment to target_info.stackalignment }
         { if para_alignment=0 then
           para_alignment:=aktalignment.paraalign;
         }
      end;



end.
{
  $Log$
  Revision 1.1  2002-07-07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

}
