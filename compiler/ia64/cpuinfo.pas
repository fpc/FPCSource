{
    $Id: cpuinfo.pas,v 1.9 2005/02/14 17:13:10 peter Exp $
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

{$i fpcdefs.inc}

Interface

Type
   AWord = QWord;

   { this must be an ordinal type with the same size as a pointer }
   { to allow some dirty type casts for example when using        }
   { tconstsym.value                                              }
   TPointerOrd = longint;

   bestreal = extended;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   { on the ia64 comp will be mapped to int64 }
   ts64comp = comp;

   pbestreal=^bestreal;


Const
   { Size of native extended type }
   extended_size = 10;

   c_countusableregsint = 95;
   c_countusableregsfpu = 95;
   c_countusableregsmm  = 0;
   c_countusableregsqp  = 48;

   { target cpu string (used by compiler options) }
   target_cpu_string = 'ia64';

Implementation

end.
{
  $Log: cpuinfo.pas,v $
  Revision 1.9  2005/02/14 17:13:10  peter
    * truncate log

}
