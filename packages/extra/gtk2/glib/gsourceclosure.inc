{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{ GObject - GLib Type, Object, Parameter and Signal Library
   Copyright (C) 2001 Red Hat, Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General
   Public License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place, Suite 330,
   Boston, MA 02111-1307, USA.
  }
{$if !defined (__GLIB_GOBJECT_H_INSIDE__) && !defined (GOBJECT_COMPILATION)}
{$error "Only <glib-anObject.h> can be included directly."}
{$endif}
{$ifndef __G_SOURCECLOSURE_H__}
{$include <gobject/gclosure.inc}

procedure g_source_set_closure(source:PGSource; closure:PGClosure); cdecl; external gliblib;
function g_io_channel_get_type:GType; cdecl; external gliblib;
function g_io_condition_get_type:GType; cdecl; external gliblib;
function G_TYPE_IO_CHANNEL : longint;
    { return type might be wrong }
function G_TYPE_IO_CONDITION : longint;
    { return type might be wrong }

{$endif}
{ __G_SOURCECLOSURE_H__  }
function G_TYPE_IO_CHANNEL : longint;
begin
   G_TYPE_IO_CHANNEL:=g_io_channel_get_type;
end;

function G_TYPE_IO_CONDITION : longint;
begin
   G_TYPE_IO_CONDITION:=g_io_condition_get_type;
end;


