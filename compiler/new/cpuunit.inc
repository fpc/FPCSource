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
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
{$ifdef alpha}
       ,alpha
{$endif}
{$ifdef sparc}
       ,sparc
{$endif}
{$ifdef powerpc}
       ,powerpc
{$endif}
{$ifdef arm}
       ,arm
{$endif}
{ hehehe, we're optimistic }
{$ifdef merced}
       ,merced
{$endif}
{
  $Log$
  Revision 1.1  1998-12-15 22:16:03  florian
    * first version, all planned processors (?) added

}