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
  {$ifdef os2}
    const
      gdkdll='gdk12';
    {$define gtkos2}

    {$packrecords C}
  {$else}
    const
    {$ifdef FreeBSD}
      gdkdll='gdk12';
    {$else}
      gdkdll='gdk';
    {$endif}
    {$linklib c}
    {$linklib X11}

    {$packrecords C}
  {$endif}
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
  Revision 1.5  2002-08-18 19:36:58  marco
   * small fixes for NetBSD that doesn't adhere to the gtk12/glib12 etc naming for older GTK versions.

  Revision 1.4  2003/03/02 02:08:50  hajny
    + OS/2 support for GTK and X11 added by Yuri

  Revision 1.3  2002/09/07 15:42:58  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/08/31 04:16:48  marco
   * BSD Libname fixes (eases Lazarus compilation).

  Revision 1.1  2002/01/29 17:55:07  peter
    * splitted to base and extra

}
