{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

    Includes the powerpc dependent target units

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

    {$ifndef NOTARGETLINUX}
      ,t_linux
    {$endif}
    {$ifndef NOTARGETMACOS}
      ,t_macos
    {$endif}

{**************************************
             Assemblers
**************************************}

    {$ifndef NOAGPPCGAS}
      ,agppcgas
    {$endif}
    {$ifndef NOAGPPPCMPW}
      ,agppcmpw
    {$endif}
      ;

end.
{
  $Log$
  Revision 1.5  2002-08-20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output

  Revision 1.4  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.3  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.2  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2002/05/13 19:52:46  peter
    * a ppcppc can be build again
}
