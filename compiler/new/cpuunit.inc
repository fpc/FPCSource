{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    This file includes the appropriate cpu unit

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
{$ifdef i386}
       ,i386base
       ,i386asm
{$else}
       ,cpubase
{$endif}
{
  $Log$
  Revision 1.4  1999-08-01 23:07:13  michael
  + Procbase- -> cpubase

  Revision 1.3  1999/08/01 23:04:51  michael
  + Changes for Alpha

  Revision 1.2  1999/08/01 18:22:34  florian
   * made it again compilable

  Revision 1.1  1998/12/15 22:16:03  florian
    * first version, all planned processors (?) added

}