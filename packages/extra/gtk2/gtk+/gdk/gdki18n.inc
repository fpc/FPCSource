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
{$ifndef __GDK_I18N_H__}
{$define __GDK_I18N_H__}
{$ifndef GDK_DISABLE_DEPRECATED}
{ GDK uses "glib". (And so does GTK).
  }

{$include <gdkconfig.inc}
{ international string support  }
{$include <stdlib.inc}
{$if !defined(GDK_HAVE_BROKEN_WCTYPE) && (defined(GDK_HAVE_WCTYPE_H) || defined(GDK_HAVE_WCHAR_H)) && !defined(X_LOCALE)}
{$ifdef GDK_HAVE_WCTYPE_H}
{$include <wctype.inc}
{$else}
{$ifdef GDK_HAVE_WCHAR_H}
{$include <wchar.inc}
{$endif}
{$endif}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswalnum(c : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswspace(c : longint) : longint;

{$else}
{$include <ctype.inc}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswalnum(c : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswspace(c : longint) : longint;

{$endif}
{$endif}
{ GDK_DISABLE_DEPRECATED  }
{$endif}
{ __GDK_I18N_H__  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswalnum(c : longint) : longint;
begin
   gdk_iswalnum:=iswalnum(c);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswspace(c : longint) : longint;
begin
   gdk_iswspace:=iswspace(c);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswalnum(c : longint) : longint;
begin
   gdk_iswalnum:=(Twchar_t(c)) <= ($FF and (@(isalnum(c))));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gdk_iswspace(c : longint) : longint;
begin
   gdk_iswspace:=(Twchar_t(c)) <= ($FF and (@(isspace(c))));
end;


