{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the x86-64 specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       rgx86;

     type
       trgcpu = class(trgx86)
       end;

  implementation

end.

{
  $Log$
  Revision 1.9  2004-06-20 08:55:32  florian
    * logs truncated

  Revision 1.8  2004/02/05 01:24:08  florian
    * several fixes to compile x86-64 system

}
