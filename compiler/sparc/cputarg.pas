{
    $Id$
    Copyright (c) 2001 by Peter Vreman

    Includes the i386 dependent target units

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

{$INCLUDE fpcdefs.inc}

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
    {$ifndef NOTARGETSUNOS}
      ,t_sunos
    {$endif}

{**************************************
             Assemblers
**************************************}

      ,CpuGas
      ,ogcoff
      ,ogelf
      ;

end.
{
  $Log$
  Revision 1.3  2003-05-23 21:10:50  florian
    * fixed sparc compiler compilation

  Revision 1.2  2002/09/07 15:25:14  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/22 08:30:50  mazen
  first insertion 2002\08\22

  Revision 1.3  2002/03/28 20:48:04  carl
  - remove go32v1 support

}
