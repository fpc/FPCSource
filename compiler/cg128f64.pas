{
    Copyright (c) 1998-2002 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 128 bit int
    arithmetics on 64 bit processors

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
{# This unit implements the code generation for 128 bit int arithmetics on
   64 bit processors.
}
unit cg128f64;

  {$i fpcdefs.inc}

  interface

    uses
      cgobj;

    type
      {# Defines all the methods required on 64-bit processors
         to handle 128-bit integers.
      }
      tcg128f64 = class(tcg128)
      end;


  implementation

end.
