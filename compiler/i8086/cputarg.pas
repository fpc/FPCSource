{
    Copyright (c) 2001-2002 by Peter Vreman

    Includes the i8086 dependent target units

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
unit cputarg;

{$i fpcdefs.inc}

interface


implementation

    uses
      systems { prevent a syntax error when nothing is included }

{**************************************
             Targets
**************************************}

    {$ifndef NOTARGETMSDOS}
      ,t_msdos
    {$endif}

{**************************************
             Assemblers
**************************************}

    {$ifndef NOAG386ATT}
//      ,agx86att
    {$endif}
    {$ifndef NOAG386NSM}
      ,agx86nsm
    {$endif}
    {$ifndef NOAG386INT}
//      ,agx86int
    {$endif}

        ,ogomf

{**************************************
        Assembler Readers
**************************************}

  {$ifndef NoRa8086Int}
       ,ra8086int
  {$endif NoRa8086Int}
  {$ifndef NoRa8086Att}
       ,ra8086att
  {$endif NoRa8086Att}

{**************************************
             Debuginfo
**************************************}

  {$ifndef NoCFIDwarf}
      ,cfidwarf
  {$endif NoCFIDwarf}
  {$ifndef NoDbgStabs}
      ,dbgstabs
  {$endif NoDbgStabs}
  {$ifndef NoDbgDwarf}
      ,dbgdwarf
  {$endif NoDbgDwarf}

      ;

end.
