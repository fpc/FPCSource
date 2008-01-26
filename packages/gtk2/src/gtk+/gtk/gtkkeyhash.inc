{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{ gtkkeyhash.h: Keymap aware matching of key bindings

   GTK - The GIMP Toolkit
   Copyright (C) 2002, Red Hat Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
  }
{$ifndef __GTK_KEY_HASH_H__}
{$define __GTK_KEY_HASH_H__}
{$include <gdk/gdk.inc}
type

function _gtk_key_hash_new(keymap:PGdkKeymap; item_destroy_notify:TGDestroyNotify):PGtkKeyHash; cdecl; external gtklib;
procedure _gtk_key_hash_add_entry(key_hash:PGtkKeyHash; keyval:guint; modifiers:TGdkModifierType; value:gpointer); cdecl; external gtklib;
procedure _gtk_key_hash_remove_entry(key_hash:PGtkKeyHash; value:gpointer); cdecl; external gtklib;
function _gtk_key_hash_lookup(key_hash:PGtkKeyHash; hardware_keycode:guint16; state:TGdkModifierType; group:gint):PGSList; cdecl; external gtklib;
function _gtk_key_hash_lookup_keyval(key_hash:PGtkKeyHash; keyval:guint; modifiers:TGdkModifierType):PGSList; cdecl; external gtklib;
procedure _gtk_key_hash_free(key_hash:PGtkKeyHash); cdecl; external gtklib;
{$endif}
{ __GTK_KEY_HASH_H__  }

