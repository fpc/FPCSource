{
    $Id$
    Copyright (C) 1993-99 by Florian Klaempfl

    This unit handles the temporary variables stuff for PowerPC

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
unit tgcpu;

  interface

    uses
       tgobj;

    type

       ttgpowerpc = Object(ttgobj)
       end;

    var
       tg : ttgppc;

implementation

begin
  tg.init;
end.
{
  $Log$
  Revision 1.1  1999-08-03 23:37:53  jonas
    + initial implementation for PowerPC based on the Alpha stuff


}