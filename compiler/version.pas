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
       { word version for ppu file }
       wordversion = (1 shl 14)+(9 shl 7) + 5;

       { version string }
       version_nr = '1';
       release_nr = '9';
       patch_nr   = '5';
       minorpatch = '';

       { date string }
{$ifdef FPC}
       date_string = {$I %DATE%};
{$else}
       date_string = 'N/A';
{$endif}


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
{$ifdef cpu86_64}
        source_cpu_string = 'x86_64';
{$endif cpu86_64}
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
  Revision 1.24  2004-05-31 12:00:56  peter
    * 1.9.5 for CVS HEAD

  Revision 1.23  2004/05/29 23:46:50  michael
  + Version 1.9.4

  Revision 1.22  2004/01/11 20:59:41  florian
    * version changed to 1.9.3

  Revision 1.21  2004/01/03 12:31:44  michael
  + Changed version to 1.9.2

  Revision 1.20  2003/11/06 15:58:27  peter
    * 1.9.1

  Revision 1.19  2003/11/01 16:37:17  peter
    * don't remove .0 when patchlevel = 0

  Revision 1.17  2003/04/24 22:29:58  florian
    * fixed a lot of PowerPC related stuff

  Revision 1.16  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.15  2002/08/10 14:46:31  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.14  2002/08/09 19:15:41  carl
     - removed newcg define

  Revision 1.13  2002/07/04 20:43:02  florian
    * first x86-64 patches

  Revision 1.12  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.11  2002/05/16 19:46:47  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.9  2002/03/24 19:12:11  carl
  + patch for SPARC from Mazen NEIFER

  Revision 1.8  2002/03/01 12:47:21  pierre
   * used shl 7 for release number
}