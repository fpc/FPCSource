{
    $Id$
    Copyright (c) 1998-2004 by Jonas Maebe, member of the Free Pascal
    Development Team

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
Unit aoptcpud;

{$i fpcdefs.inc}

  interface

    uses
      aoptda;

    type
      TAOptDFACpu = class(TAOptDFA)
      end;

  implementation

end.
{
  $Log$
  Revision 1.1  2004-10-30 15:21:38  florian
    * fixed generic optimizer
    * enabled generic optimizer for sparc
}