{
   $Id$

   GDK - The GIMP Drawing Kit
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
unit gdk;
interface

{$mode objfpc} { needed for array of const }

{ Always use smartlinking for win32, this solves some undefined functions
  in the development gtk versions which change often (PFV) }
{$ifdef win32}
  {$smartlink on}
{$endif}

uses
  glib;

{$ifdef win32}
  const
    gdkdll='gdk-1.3';
  {$define gtkwin}

  {$packrecords C}
{$else}
  const
    gdkdll='gdk';
  {$linklib c}
  {$linklib X11}

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

{$define gdk_include_files}

{$define read_interface}
{$undef read_implementation}

{$i gdktypes.pp}
{$i gdkkeysyms.pp}
{$i gdkprivate.pp}
{$i gdkrgb.pp}
{$i gdkx.pp}
{$i gdkmain.pp}


implementation

{$undef read_interface}
{$define read_implementation}

{$i gdktypes.pp}
{$i gdkkeysyms.pp}
{$i gdkprivate.pp}
{$i gdkrgb.pp}
{$i gdkx.pp}
{$i gdkmain.pp}

end.
{
  $Log$
  Revision 1.1  2002-01-29 17:55:07  peter
    * splitted to base and extra

  Revision 1.5  2000/09/09 18:41:38  peter
    * fixes for gtk win32

  Revision 1.4  2000/09/06 21:14:28  peter
    * packrecords 4 for win32, packrecords c for linux

  Revision 1.3  2000/08/06 10:46:23  peter
    * force smartlink (merged)

  Revision 1.2  2000/07/13 11:33:19  michael
  + removed logs
}
