{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

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
       wordversion = (0 shl 14)+99;

       { version string }

       version_nr = '0';
       release_nr = '99';
       patch_nr   = '15';
{$ifdef newcg}
       minorpatch = ' NCG';
{$else newcg}
       minorpatch = '';
{$endif newcg}
       version_string = version_nr+'.'+release_nr+'.'+patch_nr;
       full_version_string = version_nr+'.'+release_nr+'.'+patch_nr+minorpatch;

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
{$ifdef powerpc}
       target_cpu_string = 'powerpc';
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
  Revision 1.16  2000-02-09 13:23:09  peter
    * log truncated

  Revision 1.15  2000/01/28 20:47:26  michael
  + Changed patch number to 15

  Revision 1.14  2000/01/14 13:05:54  peter
    * version 0.99.14

  Revision 1.13  2000/01/07 01:14:49  peter
    * updated copyright to 2000

  Revision 1.12  1999/08/04 13:03:18  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.11  1999/08/02 17:17:12  florian
    * small changes for the new code generator

  Revision 1.10  1999/08/01 23:36:42  florian
    * some changes to compile the new code generator

}

