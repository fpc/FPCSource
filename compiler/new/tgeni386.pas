{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    Dummy

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
unit tgeni386;

  interface

    procedure cleartempgen;
    procedure resettempgen;

  implementation

    uses
       tgcpu;

    procedure cleartempgen;

      begin
         tg.cleartempgen;
      end;

    procedure resettempgen;

      begin
         tg.resettempgen;
      end;

end.
{
  $Log$
  Revision 1.3  2000-01-07 01:14:54  peter
    * updated copyright to 2000

  Revision 1.2  1999/08/02 21:29:09  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.1  1999/08/02 17:15:05  florian
    * dummy implementation

}
