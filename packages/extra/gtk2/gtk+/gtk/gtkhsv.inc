{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{ HSV color selector for GTK+

   Copyright (C) 1999 The Free Software Foundation

   Authors: Simon Budig <Simon.Budig@unix-ag.org> (original code)
            Federico Mena-Quintero <federico@gimp.org> (cleanup for GTK+)
            Jonathan Blandford <jrb@redhat.com> (cleanup for GTK+)

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
{$ifndef __GTK_HSV_H__}
{$define __GTK_HSV_H__}
{
   Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
   file for a list of people on the GTK+ Team.  See the ChangeLog
   files for a list of changes.  These files are distributed with
   GTK+ at ftp://ftp.gtk.org/pub/gtk/.
  }
{$include gtkcontainer.inc}
{ C++ extern C conditionnal removed }
function GTK_TYPE_HSV : GType;
function GTK_HSV(obj: pointer) : longint;
function GTK_HSV_CLASS(klass: pointer) : longint;
function GTK_IS_HSV(obj: pointer) : boolean;
function GTK_IS_HSV_CLASS(klass: pointer) : boolean;
function GTK_HSV_GET_CLASS(obj: pointer) : longint;

type
{ Private data  }
   PGtkHSV = ^TGtkHSV;
   TGtkHSV = record
        parent_instance : TGtkWidget;
        priv : gpointer;
     end;

{ Notification signals  }
{ Keybindings  }
   PGtkHSVClass = ^TGtkHSVClass;
   TGtkHSVClass = record
        parent_class : TGtkWidgetClass;
        changed : procedure (hsv:PGtkHSV); cdecl;
        move : procedure (hsv:PGtkHSV; _type:TGtkDirectionType); cdecl;
     end;


function gtk_hsv_get_type:TGtkType; cdecl; external gtklib;
function gtk_hsv_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_hsv_set_color(hsv:PGtkHSV; h:Tdouble; s:Tdouble; v:Tdouble); cdecl; external gtklib;
procedure gtk_hsv_get_color(hsv:PGtkHSV; h:Pgdouble; s:Pgdouble; v:Pgdouble); cdecl; external gtklib;
procedure gtk_hsv_set_metrics(hsv:PGtkHSV; size:gint; ring_width:gint); cdecl; external gtklib;
procedure gtk_hsv_get_metrics(hsv:PGtkHSV; size:Pgint; ring_width:Pgint); cdecl; external gtklib;
function gtk_hsv_is_adjusting(hsv:PGtkHSV):gboolean; cdecl; external gtklib;
procedure gtk_hsv_to_rgb(h:gdouble; s:gdouble; v:gdouble; r:Pgdouble; g:Pgdouble;
            b:Pgdouble); cdecl; external gtklib;
procedure gtk_rgb_to_hsv(r:gdouble; g:gdouble; b:gdouble; h:Pgdouble; s:Pgdouble;
            v:Pgdouble); cdecl; external gtklib;
{ C++ end of extern C conditionnal removed }
{$endif}
{ __GTK_HSV_H__  }
function GTK_TYPE_HSV : GType;
begin
   GTK_TYPE_HSV:=gtk_hsv_get_type;
end;

function GTK_HSV(obj: pointer) : PGtkHSV;
begin
   GTK_HSV:=PGtkHSV(GTK_CHECK_CAST(obj,GTK_TYPE_HSV));
end;

function GTK_HSV_CLASS(klass: pointer) : PGtkHSVClass;
begin
   GTK_HSV_CLASS:=PGtkHSVClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HSV));
end;

function GTK_IS_HSV(obj: pointer) : boolean;
begin
   GTK_IS_HSV:=GTK_CHECK_TYPE(obj,GTK_TYPE_HSV);
end;

function GTK_IS_HSV_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HSV_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HSV);
end;

function GTK_HSV_GET_CLASS(obj: pointer) : PGtkHSVClass;
begin
   GTK_HSV_GET_CLASS:=PGtkHSVClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HSV));
end;


