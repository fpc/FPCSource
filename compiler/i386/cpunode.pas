{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Includes the i386 code generator

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
unit cpunode;

{$i defines.inc}

  interface

  implementation

    uses
       n386bas,n386ld,n386add,n386cal,n386con,n386flw,n386mat,n386mem,
       n386set,n386inl;

end.
{
  $Log$
  Revision 1.1  2000-10-15 09:39:37  peter
    * moved cpu*.pas to i386/
    * renamed n386 to common cpunode

  Revision 1.1  2000/10/14 10:14:47  peter
    * moehrendorf oct 2000 rewrite

}