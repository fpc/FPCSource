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

  {$packrecords c}
{$else}
  const
  {$ifdef BSD}
    gtkdll='gtk12';
  {$else}
    gtkdll='gtk';
  {$endif}
  {$linklib c}
  {$linklib Xi}
  {$linklib Xext}
  {$linklib X11}
  {$linklib m}

  {$packrecords C}
{$endif}

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
  Revision 1.3  2002-09-07 15:42:59  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/08/31 04:16:48  marco
   * BSD Libname fixes (eases Lazarus compilation).

  Revision 1.1  2002/01/29 17:55:08  peter
    * splitted to base and extra

}
