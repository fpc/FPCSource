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

{$i defines.inc}

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
    {$ifndef NOTARGETFREEBSD}
      ,t_fbsd
    {$endif}
    {$ifndef NOTARGETSUNOS}
      ,t_sunos
    {$endif}
    {$ifndef NOTARGETOS2}
      ,t_os2
    {$endif}
    {$ifndef NOTARGETWIN32}
      ,t_win32
    {$endif}
    {$ifndef NOTARGETNETWARE}
      ,t_nwm
    {$endif}
    {$ifndef NOTARGETGO32V2}
      ,t_go32v2
    {$endif}
    {$ifndef NOTARGETBEOS}
      ,t_beos
    {$endif}

{**************************************
             Assemblers
**************************************}

    {$ifndef NOAG386ATT}
      ,ag386att
    {$endif}
    {$ifndef NOAG386NSM}
      ,ag386nsm
    {$endif}
    {$ifndef NOAG386INT}
      ,ag386int
    {$endif}

      ,ogcoff
      ,ogelf
      ;

end.
{
  $Log$
  Revision 1.3  2002-03-28 20:48:04  carl
  - remove go32v1 support

  Revision 1.2  2001/06/03 15:12:47  peter
    * t_beos target inclusion

  Revision 1.1  2001/04/18 22:02:01  peter
    * registration of targets and assemblers

}
