{
    $Id$
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
{ Generates the argument location information for x86-64 target.
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
       tx86_64paramanager = class(tparamanager)
          function getintparaloc(nr : longint) : tparalocation;override;
          procedure create_param_loc_info(p : tabstractprocdef);override;
       end;

  implementation

    uses
       verbose;

    function tx86_64paramanager.getintparaloc(nr : longint) : tparalocation;
      begin
      end;

    procedure tx86_64paramanager.create_param_loc_info(p : tabstractprocdef);
      begin
         { set default para_alignment to target_info.stackalignment }
         { if para_alignment=0 then
           para_alignment:=aktalignment.paraalign;
         }
      end;


begin
   paramanager:=tx86_64paramanager.create;
end.
{
  $Log$
  Revision 1.1  2002-07-24 22:38:15  florian
    + initial release of x86-64 target code

}
