{
    $Id$
    Copyright (C) 1993-98 by Florian Klaempfl

    Version/target constants

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
unit version;
interface

    const
       { word version for ppu file }
       wordversion = (0 shl 12)+99;

       { version string }
       version_nr = '0';
       release_nr = '99';
       patch_nr   = '12';
       minorpatch = 'a';
       version_string = version_nr+'.'+release_nr+'.'+patch_nr+minorpatch;

       { date string }
{$ifdef FPC}
       date_string = {$I %DATE%};
{$else}
       date_string = 'N/A';
{$endif}

       { target cpu string }
{$ifdef i386}
       target_cpu_string = 'i386';
{$endif}
{$ifdef m68k}
       target_cpu_string = 'm68k';
{$endif}
{$ifdef alpha}
       target_cpu_string = 'alpha';
{$endif}

       { source cpu string }
{$ifdef cpu86}
        source_cpu_string = 'i386';
{$endif}
{$ifdef cpu68}
        source_cpu_string = 'm68k';
{$endif}


implementation

begin
end.
{
  $Log$
  Revision 1.6.2.1  1999-06-18 09:54:48  peter
    + minorpatch which is 'a' for 0.99.12a

  Revision 1.6  1999/06/08 12:52:38  peter
    * 0.99.12

  Revision 1.5  1998/12/23 14:02:01  peter
    * daniels patches against the latest versions

  Revision 1.3  1998/12/15 10:23:34  peter
    + -iSO, -iSP, -iTO, -iTP

  Revision 1.2  1998/12/14 12:58:45  peter
    * version 0.99.11

    + globtype,tokens,version unit splitted from globals

}

