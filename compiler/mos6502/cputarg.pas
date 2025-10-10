{
    Copyright (c) 2001-2008 by Peter Vreman

    Includes the MOS Technology 6502 dependent target units

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

    {$ifndef NOTARGETEMBEDDED}
      ,t_embed
    {$endif}
    {$ifndef NOTARGETORIC}
      //,t_oric
    {$endif}

{**************************************
             Assemblers
**************************************}

    {$ifndef NOAGSDAS6500}
      ,agsdas6500
    {$endif}

      ,ogrel

{**************************************
        Assembler Readers
**************************************}

//  {$ifndef NoRaZ80asm}
//      ,raz80asm
//  {$endif NoRaZ80asm}

{**************************************
             Debuginfo
**************************************}

  {$ifndef NoDbgStabs}
      ,dbgstabs
  {$endif NoDbgStabs}
  {$ifndef NoDbgDwarf}
      ,dbgdwarf
  {$endif NoDbgDwarf}
      ;

end.
