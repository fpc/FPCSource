{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    PowerPC specific calling conventions

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
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
{ PowerPC specific calling conventions are handled by this unit
}
unit cpupara;

  interface

    uses
       cpubase;

    var
       paralocdummy : tparalocation;

    function getintparaloc(nr : longint) : tparalocation;

  implementation

    function getintparaloc(nr : longint) : tparalocation;

      begin
      end;

end.
{
  $Log$
  Revision 1.1  2002-07-07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled

}
