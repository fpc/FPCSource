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
       { version string }
       wordversion = (0 shl 11)+99;

       version_nr = '0';
       release_nr = '99';
       patch_nr   = '11';
       version_string = version_nr+'.'+release_nr+'.'+patch_nr;

       { target string }
{$ifdef i386}
       target_string = 'i386';
{$endif}
{$ifdef m68k}
       target_string = 'M680x0';
{$endif}
{$ifdef alpha}
       target_string = 'Alpha';
{$endif}

       { date string }
{$ifdef FPC}
       date_string = {$I %DATE%};
{$else}
       date_string = 'N/A';
{$endif}


implementation

end.
{
  $Log$
  Revision 1.2  1998-12-14 12:58:45  peter
    * version 0.99.11

    + globtype,tokens,version unit splitted from globals

}

