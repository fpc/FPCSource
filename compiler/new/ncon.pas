{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit implements load nodes etc.

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
unit ncon;

  interface

    uses
       cpuinfo,tree;

    type
       pstringconstnode = ^tstringconstnode;

       tstringconstnode = object(tnode)
          length : aword;
       end;

  implementation

end.
{
  $Log$
  Revision 1.3  2000-01-07 01:14:53  peter
    * updated copyright to 2000

  Revision 1.2  1999/08/06 18:05:53  florian
    * implemented some stuff for assignments

  Revision 1.1  1999/08/06 16:15:38  florian
    + initial revision
}
