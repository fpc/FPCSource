{
    $Id$
    Copyright (C) 1993-99 by Florian Klaempfl

    This unit handles the temporary variables stuff for Alpha

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
    
       ttgalpha = Object(ttgobj)
           end;

    var
       tg : ttgalpha;
       reg_pushes : array[firstreg..lastreg] of longint;
       is_reg_var : array[firstreg..lastreg] of boolean;

implementation

begin
  tg.init;
end.
{
  $Log$
  Revision 1.2  1999-08-03 00:25:28  florian
    * added reg var stuff

  Revision 1.1  1999/08/03 00:24:01  michael
  + Initial implementation

}