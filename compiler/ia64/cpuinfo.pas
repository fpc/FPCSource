{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Basic Processor information

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
Unit CPUInfo;

{$i defines.inc}

Interface

Type
   AWord = QWord;

   { this must be an ordinal type with the same size as a pointer }
   { to allow some dirty type casts for example when using        }
   { tconstsym.value                                              }
   TPointerOrd = longint;


Const
   { Size of native extended type }
   extended_size = 10;

   c_countusableregsint = 95;
   c_countusableregsfpu = 95;
   c_countusableregsmm  = 0;
   c_countusableregsqp  = 48;

Implementation

end.
{
  $Log$
  Revision 1.3  2002-04-07 13:42:40  carl
  - moved type constant

  Revision 1.2  2001/01/05 17:36:58  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.1  2000/12/31 16:54:19  florian
    + initial revision

}
