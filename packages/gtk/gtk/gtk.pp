{
   $Id$

   GTK - The GIMP Toolkit
   Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

 **********************************************************************}
unit gtk;
interface

{ Always use smartlinking for win32, this solves some undefined functions
  in the development gtk versions which change often (PFV) }
{$ifdef win32}
  {$smartlink on}
{$endif}

{$mode objfpc} { needed for array of const }

uses
  glib,gdk;

{$ifdef win32}
  const
    gtkdll='gtk-1.3';
  {$define gtkwin}
{$else}
  const
    gtkdll='gtk';
  {$linklib c}
  {$linklib Xi}
  {$linklib Xext}
  {$linklib X11}
  {$linklib m}
{$endif}

{$packrecords C}

Type
  PLongint  = ^Longint;
  PByte     = ^Byte;
  PWord     = ^Word;
  PINteger  = ^Integer;
  PCardinal = ^Cardinal;
  PReal     = ^Real;
  PDouble   = ^Double;

{$define gtk_include_files}

{$define read_interface}
{$undef read_implementation}

{$i gtkincludes.pp}

  implementation

{$undef read_interface}
{$define read_implementation}

{$i gtkincludes.pp}

end.
{
  $Log$
  Revision 1.1.2.1  2000-08-06 10:33:10  peter
    * force smartlinking

  Revision 1.1  2000/07/13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.8  1999/07/23 16:11:49  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:37:59  peter
    * win32 fixes

  Revision 1.6  1999/05/10 09:02:50  peter
    * gtk 1.2 port working

  Revision 1.5  1999/05/07 17:40:10  peter
    * more updates

  Revision 1.4  1999/05/07 10:40:22  peter
    * first things for 1.2

  Revision 1.3  1998/11/12 11:35:50  peter
    + array of const

  Revision 1.2  1998/10/22 11:37:37  peter
    * fixes for win32

  Revision 1.1  1998/10/21 20:22:04  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

