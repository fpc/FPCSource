{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

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
    {$ifndef NOTARGETFREEBSD}
      ,t_fbsd
    {$endif}
    {$ifndef NOTARGETSUNOS}
      ,t_sunos
    {$endif}
    {$ifndef NOTARGETEMX}
      ,t_emx
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
    {$ifndef NOTARGETWDOSX}
      ,t_wdosx
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
  Revision 1.10  2003-03-23 23:33:10  hajny
    + emx target added

  Revision 1.9  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.8  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.6  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.5  2002/04/14 17:00:49  carl
  + att_reg2str -> gas_reg2str

  Revision 1.4  2002/04/04 18:31:37  carl
  + added wdosx support (patch from Pavel)

  Revision 1.3  2002/03/28 20:48:04  carl
  - remove go32v1 support

}
