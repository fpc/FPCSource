{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

  interface

    uses
      procinfo,cgbase,psub;

    type
      tm68kprocinfo = class(tcgprocinfo)
      end;

  implementation

begin
   cprocinfo:=tm68kprocinfo;
end.
{
  $Log$
  Revision 1.5  2004-06-20 08:55:31  florian
    * logs truncated

  Revision 1.4  2004/05/20 21:54:33  florian
    + <pointer> - <pointer> result is divided by the pointer element size now
      this is delphi compatible as well as resulting in the expected result for p1+(p2-p1)

  Revision 1.3  2004/05/01 23:29:01  florian
    * continued to fix m68k compiler compilation

}


