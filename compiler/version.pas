{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    const
       { version string }
       version_nr = '1';
       release_nr = '9';
       patch_nr   = '7';
       minorpatch = '';

       { word version for ppu file }
       wordversion = ((ord(version_nr)-ord('0')) shl 14)+
                     ((ord(release_nr)-ord('0')) shl 7)+
                     (ord(patch_nr)-ord('0'));

       { date string }
       date_string = {$I %DATE%};

       { source cpu string }
{$ifdef cpu86}
        source_cpu_string = 'i386';
{$endif cpu86}
{$ifdef cpupowerpc}
        source_cpu_string = 'powerpc';
{$endif cpupowerpc}
{$ifdef cpum68k}
        source_cpu_string = 'm68k';
{$endif cpum68k}
{$ifdef cpuia64}
        source_cpu_string = 'ia64';
{$endif cpuia64}
{$ifdef cpux86_64}
        source_cpu_string = 'x86_64';
{$endif cpux86_64}
{$ifdef cpusparc}
        source_cpu_string = 'sparc';
{$endif cpusparc}
{$ifdef cpusalpha}
        source_cpu_string = 'alpha';
{$endif cpualpha}
{$ifdef cpuvis}
        source_cpu_string = 'vis';
{$endif cpuvis}
{$ifdef cpuarm}
        source_cpu_string = 'arm';
{$endif cpuarm}

function version_string:string;
function full_version_string:string;


implementation

function version_string:string;
begin
  version_string := version_nr+'.'+release_nr+'.'+patch_nr;
end;


function full_version_string:string;
begin
  full_version_string := version_nr+'.'+release_nr+'.'+patch_nr+minorpatch;
end;

end.
{
  $Log$
  Revision 1.30  2005-01-19 21:10:15  peter
    * build wordversion from constants

  Revision 1.29  2005/01/02 10:20:08  florian
    * version and copyright increased

  Revision 1.28  2004/12/30 19:01:29  peter
    * back to 1.9.5 for HEAD

  Revision 1.27  2004/12/30 18:37:27  peter
    * 1.9.6

  Revision 1.26  2004/06/20 08:55:30  florian
    * logs truncated

  Revision 1.25  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.24  2004/05/31 12:00:56  peter
    * 1.9.5 for CVS HEAD

  Revision 1.23  2004/05/29 23:46:50  michael
  + Version 1.9.4

  Revision 1.22.2.1  2004/04/28 21:46:56  peter
    * compile fixes for x86-64

  Revision 1.22  2004/01/11 20:59:41  florian
    * version changed to 1.9.3

}
