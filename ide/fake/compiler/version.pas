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
       wordversion = (0 shl 11)+99;

       { version string }
       version_nr = '0';
       release_nr = '99';
       patch_nr   = '11';
       version_string = version_nr+'.'+release_nr+'.'+patch_nr;

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

end.
{
  $Log$
  Revision 1.1  2000-07-13 09:48:33  michael
  + Initial import

  Revision 1.1  1999/01/28 19:56:12  peter
    * moved to include compiler/gdb independent of each other

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/18 17:17:06  peter
    * updated with new versions from compiler

}

