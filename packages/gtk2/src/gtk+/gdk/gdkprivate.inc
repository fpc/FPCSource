{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{ GDK - The GIMP Drawing Kit
   Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald

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
{
   Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
   file for a list of people on the GTK+ Team.  See the ChangeLog
   files for a list of changes.  These files are distributed with
   GTK+ at ftp://ftp.gtk.org/pub/gtk/.
  }
{$ifndef __GDK_PRIVATE_H__}
{$define __GDK_PRIVATE_H__}
{$include <gdk/gdktypes.inc}
{$include <gdk/gdkevents.inc}
{$include <gdk/gdkfont.inc}
{$include <gdk/gdkgc.inc}
{$include <gdk/gdkimage.inc}
{$include <gdk/gdkregion.inc}
{$include <gdk/gdkvisual.inc}
{$include <gdk/gdkwindow.inc}
function GDK_PARENT_RELATIVE_BG : PGdkPixmap;
function GDK_NO_BG : PGdkPixmap;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GDK_WINDOW_TYPE(d : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GDK_WINDOW_DESTROYED(d : longint) : longint;

procedure gdk_window_destroy_notify(window:PGdkWindow); cdecl; external gdklib;
procedure gdk_synthesize_window_state(window:PGdkWindow; unset_flags:TGdkWindowState; set_flags:TGdkWindowState); cdecl; external gdklib;
{$endif}
{ __GDK_PRIVATE_H__  }
function GDK_PARENT_RELATIVE_BG : PGdkPixmap;
  begin
     GDK_PARENT_RELATIVE_BG:=PGdkPixmap(1);
  end;

function GDK_NO_BG : PGdkPixmap;
  begin
     GDK_NO_BG:=PGdkPixmap(2);
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GDK_WINDOW_TYPE(d : longint) : longint;
begin
   GDK_WINDOW_TYPE:=(PGdkWindowObject(GDK_WINDOW(d)))^.window_type;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GDK_WINDOW_DESTROYED(d : longint) : longint;
begin
   GDK_WINDOW_DESTROYED:=(PGdkWindowObject(GDK_WINDOW(d)))^.destroyed;
end;


