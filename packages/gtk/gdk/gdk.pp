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

uses
  glib;

{$ifdef win32}
  const
    gdkdll='gdk-1.3';
  {$define gtkwin}
{$else}
  const
    gdkdll='gdk';
  {$linklib c}
  {$linklib X11}
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
  Revision 1.1  2000-07-13 06:34:01  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:33  peter
    * moved to packages dir

  Revision 1.10  1999/10/21 14:35:23  peter
    * removed glibdll constant

  Revision 1.9  1999/10/21 08:42:00  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.8  1999/10/06 17:42:47  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.7  1999/07/23 16:11:43  peter
    * use packrecords C

  Revision 1.6  1999/05/11 00:37:46  peter
    * win32 fixes

  Revision 1.5  1999/05/07 10:40:17  peter
    * first things for 1.2

  Revision 1.4  1998/11/12 11:35:46  peter
    + array of const

  Revision 1.3  1998/10/22 11:37:34  peter
    * fixes for win32

  Revision 1.2  1998/10/21 20:22:00  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

