{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 inline nodes

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
unit n386inl;

{$i fpcdefs.inc}

interface

    uses
       nx86inl;

    type
       ti386inlinenode = class(tx86inlinenode)
       end;

implementation

  uses
    ninl;

begin
   cinlinenode:=ti386inlinenode;
end.
{
  $Log$
  Revision 1.74  2004-06-20 08:55:31  florian
    * logs truncated

  Revision 1.73  2004/02/05 01:24:08  florian
    * several fixes to compile x86-64 system

  Revision 1.72  2004/02/03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.71  2004/02/02 20:41:59  florian
    + added prefetch(const mem) support

}
