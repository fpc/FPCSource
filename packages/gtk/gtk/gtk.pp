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

{$ifdef VER0_99_12}
  {$fatal Can't compile GTK with 0.99.12}
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
  Revision 1.2  2000-07-13 11:33:20  michael
  + removed logs
 
}
