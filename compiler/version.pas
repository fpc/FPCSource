{
    $Id: version.pas,v 1.35 2005/04/28 20:17:21 michael Exp $
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
       version_nr = '2';
       release_nr = '0';
       patch_nr   = '1';
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
  $Log: version.pas,v $
  Revision 1.35  2005/04/28 20:17:21  michael
  * version 2.0.0

  Revision 1.34  2005/02/25 11:41:27  peter
    * 1.9.9

  Revision 1.33  2005/02/15 22:31:35  peter
    * back to 1.9.7 for head

  Revision 1.32  2005/02/15 22:28:29  peter
    * 1.9.8

  Revision 1.31  2005/02/14 17:13:09  peter
    * truncate log

  Revision 1.30  2005/01/19 21:10:15  peter
    * build wordversion from constants

  Revision 1.29  2005/01/02 10:20:08  florian
    * version and copyright increased

}
