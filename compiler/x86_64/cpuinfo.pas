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
Unit cpuinfo;

{$i fpcdefs.inc}

Interface

Type
   AWord = QWord;
   PAWord = ^AWord;

   { the ordinal type used when evaluating constant integer expressions }
   TConstExprInt = int64;
   { ... the same unsigned }
   TConstExprUInt = {$ifdef fpc}qword{$else}int64{$endif};

   { this must be an ordinal type with the same size as a pointer }
   { Note: must be unsigned!! Otherwise, ugly code like           }
   { pointer(-1) will result in a pointer with the value          }
   { $fffffffffffffff on a 32bit machine if the compiler uses     }
   { int64 constants internally (JM)                              }
   TConstPtrUInt = qword;

Const
   { Size of native extended type }
   extended_size = 10;
   { Size of a pointer }
   pointer_size  = 8;
   { Size of a multimedia register }
   mmreg_size = 16;


Implementation

end.
{
  $Log$
  Revision 1.2  2002-07-25 22:55:34  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

}
