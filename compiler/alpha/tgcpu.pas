{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

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

implementation

begin
  tg.init;
end.
{
  $Log$
  Revision 1.1  2002-08-18 09:06:54  florian
    * alpha files moved compiler/alpha

  Revision 1.1  2000/07/13 06:30:10  michael
  + Initial import

  Revision 1.4  2000/01/07 01:14:56  peter
    * updated copyright to 2000

  Revision 1.3  1999/08/03 00:28:24  florian
    - removed the stuff of the last commit :)

  Revision 1.2  1999/08/03 00:25:28  florian
    * added reg var stuff

  Revision 1.1  1999/08/03 00:24:01  michael
  + Initial implementation

}