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
   Copyright (C) 1997-1999, 2000-2001 Tim Janik and Red Hat, Inc.

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

   gparamspecs.h: GLib default param specs
  }
{$if !defined (__GLIB_GOBJECT_H_INSIDE__) && !defined (GOBJECT_COMPILATION)}
{$error "Only <glib-anObject.h> can be included directly."}
{$endif}
{$ifndef __G_PARAMSPECS_H__}
{$define __G_PARAMSPECS_H__}
{$include        <gobject/gvalue.inc}
{$include        <gobject/genums.inc}
{$include        <gobject/gboxed.inc}
{$include        <gobject/gobject.inc}
{ --- type macros ---  }
function G_TYPE_PARAM_CHAR : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_CHAR(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_CHAR(pspec : longint) : longint;
function G_TYPE_PARAM_UCHAR : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UCHAR(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UCHAR(pspec : longint) : longint;
function G_TYPE_PARAM_BOOLEAN : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_BOOLEAN(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_BOOLEAN(pspec : longint) : longint;
function G_TYPE_PARAM_INT : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_INT(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_INT(pspec : longint) : longint;
function G_TYPE_PARAM_UINT : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UINT(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UINT(pspec : longint) : longint;
function G_TYPE_PARAM_LONG : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_LONG(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_LONG(pspec : longint) : longint;
function G_TYPE_PARAM_ULONG : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_ULONG(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_ULONG(pspec : longint) : longint;
function G_TYPE_PARAM_INT64 : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_INT64(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_INT64(pspec : longint) : longint;
function G_TYPE_PARAM_UINT64 : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UINT64(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UINT64(pspec : longint) : longint;
function G_TYPE_PARAM_UNICHAR : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UNICHAR(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UNICHAR(pspec : longint) : longint;
function G_TYPE_PARAM_ENUM : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_ENUM(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_ENUM(pspec : longint) : longint;
function G_TYPE_PARAM_FLAGS : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_FLAGS(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_FLAGS(pspec : longint) : longint;
function G_TYPE_PARAM_FLOAT : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_FLOAT(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_FLOAT(pspec : longint) : longint;
function G_TYPE_PARAM_DOUBLE : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_DOUBLE(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_DOUBLE(pspec : longint) : longint;
function G_TYPE_PARAM_STRING : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_STRING(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_STRING(pspec : longint) : longint;
function G_TYPE_PARAM_PARAM : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_PARAM(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_PARAM(pspec : longint) : longint;
function G_TYPE_PARAM_BOXED : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_BOXED(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_BOXED(pspec : longint) : longint;
function G_TYPE_PARAM_POINTER : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_POINTER(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_POINTER(pspec : longint) : longint;
function G_TYPE_PARAM_VALUE_ARRAY : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_VALUE_ARRAY(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_VALUE_ARRAY(pspec : longint) : longint;
function G_TYPE_PARAM_OBJECT : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_OBJECT(pspec : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_OBJECT(pspec : longint) : longint;

{ --- typedefs & structures ---  }
type
   PGParamSpecChar = ^TGParamSpecChar;
   TGParamSpecChar = record
        parent_instance : TGParamSpec;
        minimum : gint8;
        maximum : gint8;
        default_value : gint8;
     end;

   PGParamSpecUChar = ^TGParamSpecUChar;
   TGParamSpecUChar = record
        parent_instance : TGParamSpec;
        minimum : guint8;
        maximum : guint8;
        default_value : guint8;
     end;

   PGParamSpecBoolean = ^TGParamSpecBoolean;
   TGParamSpecBoolean = record
        parent_instance : TGParamSpec;
        default_value : gboolean;
     end;

   PGParamSpecInt = ^TGParamSpecInt;
   TGParamSpecInt = record
        parent_instance : TGParamSpec;
        minimum : gint;
        maximum : gint;
        default_value : gint;
     end;

   PGParamSpecUInt = ^TGParamSpecUInt;
   TGParamSpecUInt = record
        parent_instance : TGParamSpec;
        minimum : guint;
        maximum : guint;
        default_value : guint;
     end;

   PGParamSpecLong = ^TGParamSpecLong;
   TGParamSpecLong = record
        parent_instance : TGParamSpec;
        minimum : glong;
        maximum : glong;
        default_value : glong;
     end;

   PGParamSpecULong = ^TGParamSpecULong;
   TGParamSpecULong = record
        parent_instance : TGParamSpec;
        minimum : gulong;
        maximum : gulong;
        default_value : gulong;
     end;

   PGParamSpecInt64 = ^TGParamSpecInt64;
   TGParamSpecInt64 = record
        parent_instance : TGParamSpec;
        minimum : Tgint64;
        maximum : Tgint64;
        default_value : Tgint64;
     end;

   PGParamSpecUInt64 = ^TGParamSpecUInt64;
   TGParamSpecUInt64 = record
        parent_instance : TGParamSpec;
        minimum : Tguint64;
        maximum : Tguint64;
        default_value : Tguint64;
     end;

   PGParamSpecUnichar = ^TGParamSpecUnichar;
   TGParamSpecUnichar = record
        parent_instance : TGParamSpec;
        default_value : gunichar;
     end;

   PGParamSpecEnum = ^TGParamSpecEnum;
   TGParamSpecEnum = record
        parent_instance : TGParamSpec;
        enum_class : PGEnumClass;
        default_value : gint;
     end;

   PGParamSpecFlags = ^TGParamSpecFlags;
   TGParamSpecFlags = record
        parent_instance : TGParamSpec;
        flags_class : PGFlagsClass;
        default_value : guint;
     end;

   PGParamSpecFloat = ^TGParamSpecFloat;
   TGParamSpecFloat = record
        parent_instance : TGParamSpec;
        minimum : gfloat;
        maximum : gfloat;
        default_value : gfloat;
        epsilon : gfloat;
     end;

   PGParamSpecDouble = ^TGParamSpecDouble;
   TGParamSpecDouble = record
        parent_instance : TGParamSpec;
        minimum : gdouble;
        maximum : gdouble;
        default_value : gdouble;
        epsilon : gdouble;
     end;

   PGParamSpecString = ^TGParamSpecString;
   TGParamSpecString = record
        parent_instance : TGParamSpec;
        default_value : Pgchar;
        cset_first : Pgchar;
        cset_nth : Pgchar;
        substitutor : gchar;
        flag0 : word;
     end;

const
   bm_TGParamSpecString_null_fold_if_empty = $1;
   bp_TGParamSpecString_null_fold_if_empty = 0;
   bm_TGParamSpecString_ensure_non_null = $2;
   bp_TGParamSpecString_ensure_non_null = 1;
function null_fold_if_empty(var a : TGParamSpecString) : guint;
procedure set_null_fold_if_empty(var a : TGParamSpecString; __null_fold_if_empty : guint);
function ensure_non_null(var a : TGParamSpecString) : guint;
procedure set_ensure_non_null(var a : TGParamSpecString; __ensure_non_null : guint);
type
   PGParamSpecParam = ^TGParamSpecParam;
   TGParamSpecParam = record
        parent_instance : TGParamSpec;
     end;

   PGParamSpecBoxed = ^TGParamSpecBoxed;
   TGParamSpecBoxed = record
        parent_instance : TGParamSpec;
     end;

   PGParamSpecPointer = ^TGParamSpecPointer;
   TGParamSpecPointer = record
        parent_instance : TGParamSpec;
     end;

   PGParamSpecValueArray = ^TGParamSpecValueArray;
   TGParamSpecValueArray = record
        parent_instance : TGParamSpec;
        element_spec : PGParamSpec;
        fixed_n_elements : guint;
     end;

   PGParamSpecObject = ^TGParamSpecObject;
   TGParamSpecObject = record
        parent_instance : TGParamSpec;
     end;

{ --- GParamSpec prototypes ---  }




function g_param_spec_char(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gint8; maximum:gint8;
           default_value:gint8; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_uchar(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:guint8; maximum:guint8;
           default_value:guint8; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_boolean(name:Pgchar; nick:Pgchar; blurb:Pgchar; default_value:gboolean; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_int(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gint; maximum:gint;
           default_value:gint; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_uint(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:guint; maximum:guint;
           default_value:guint; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_long(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:glong; maximum:glong;
           default_value:glong; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_ulong(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gulong; maximum:gulong;
           default_value:gulong; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_int64(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:Tgint64; maximum:Tgint64;
           default_value:Tgint64; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_uint64(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:Tguint64; maximum:Tguint64;
           default_value:Tguint64; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_unichar(name:Pgchar; nick:Pgchar; blurb:Pgchar; default_value:gunichar; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_enum(name:Pgchar; nick:Pgchar; blurb:Pgchar; enum_type:GType; default_value:gint;
           flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_flags(name:Pgchar; nick:Pgchar; blurb:Pgchar; flags_type:GType; default_value:guint;
           flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_float(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gfloat; maximum:gfloat;
           default_value:gfloat; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_double(name:Pgchar; nick:Pgchar; blurb:Pgchar; minimum:gdouble; maximum:gdouble;
           default_value:gdouble; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;




function g_param_spec_string(name:Pgchar; nick:Pgchar; blurb:Pgchar; default_value:Pgchar; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_param(name:Pgchar; nick:Pgchar; blurb:Pgchar; param_type:GType; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_boxed(name:Pgchar; nick:Pgchar; blurb:Pgchar; boxed_type:GType; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_pointer(name:Pgchar; nick:Pgchar; blurb:Pgchar; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_value_array(name:Pgchar; nick:Pgchar; blurb:Pgchar; element_spec:PGParamSpec; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;



function g_param_spec_object(name:Pgchar; nick:Pgchar; blurb:Pgchar; object_type:GType; flags:TGParamFlags):PGParamSpec; cdecl; external gliblib;
  var
     g_param_spec_types : PGType;cvar;public;
{$endif}
{ __G_PARAMSPECS_H__  }
function G_TYPE_PARAM_CHAR : longint;
begin
   G_TYPE_PARAM_CHAR:=g_param_spec_types[0];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_CHAR(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_CHAR:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_CHAR);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_CHAR(pspec : longint) : longint;
begin
   G_PARAM_SPEC_CHAR:=PGParamSpecChar(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_CHAR));
end;

function G_TYPE_PARAM_UCHAR : longint;
begin
   G_TYPE_PARAM_UCHAR:=g_param_spec_types[1];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UCHAR(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_UCHAR:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UCHAR);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UCHAR(pspec : longint) : longint;
begin
   G_PARAM_SPEC_UCHAR:=PGParamSpecUChar(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UCHAR));
end;

function G_TYPE_PARAM_BOOLEAN : longint;
begin
   G_TYPE_PARAM_BOOLEAN:=g_param_spec_types[2];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_BOOLEAN(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_BOOLEAN:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_BOOLEAN);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_BOOLEAN(pspec : longint) : longint;
begin
   G_PARAM_SPEC_BOOLEAN:=PGParamSpecBoolean(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_BOOLEAN));
end;

function G_TYPE_PARAM_INT : longint;
begin
   G_TYPE_PARAM_INT:=g_param_spec_types[3];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_INT(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_INT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_INT);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_INT(pspec : longint) : longint;
begin
   G_PARAM_SPEC_INT:=PGParamSpecInt(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_INT));
end;

function G_TYPE_PARAM_UINT : longint;
begin
   G_TYPE_PARAM_UINT:=g_param_spec_types[4];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UINT(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_UINT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UINT);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UINT(pspec : longint) : longint;
begin
   G_PARAM_SPEC_UINT:=PGParamSpecUInt(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UINT));
end;

function G_TYPE_PARAM_LONG : longint;
begin
   G_TYPE_PARAM_LONG:=g_param_spec_types[5];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_LONG(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_LONG:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_LONG);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_LONG(pspec : longint) : longint;
begin
   G_PARAM_SPEC_LONG:=PGParamSpecLong(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_LONG));
end;

function G_TYPE_PARAM_ULONG : longint;
begin
   G_TYPE_PARAM_ULONG:=g_param_spec_types[6];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_ULONG(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_ULONG:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_ULONG);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_ULONG(pspec : longint) : longint;
begin
   G_PARAM_SPEC_ULONG:=PGParamSpecULong(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_ULONG));
end;

function G_TYPE_PARAM_INT64 : longint;
begin
   G_TYPE_PARAM_INT64:=g_param_spec_types[7];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_INT64(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_INT64:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_INT64);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_INT64(pspec : longint) : longint;
begin
   G_PARAM_SPEC_INT64:=PGParamSpecInt64(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_INT64));
end;

function G_TYPE_PARAM_UINT64 : longint;
begin
   G_TYPE_PARAM_UINT64:=g_param_spec_types[8];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UINT64(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_UINT64:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UINT64);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UINT64(pspec : longint) : longint;
begin
   G_PARAM_SPEC_UINT64:=PGParamSpecUInt64(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UINT64));
end;

function G_TYPE_PARAM_UNICHAR : longint;
begin
   G_TYPE_PARAM_UNICHAR:=g_param_spec_types[9];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_UNICHAR(pspec : longint) : longint;
begin
   G_PARAM_SPEC_UNICHAR:=PGParamSpecUnichar(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_UNICHAR));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_UNICHAR(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_UNICHAR:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_UNICHAR);
end;

function G_TYPE_PARAM_ENUM : longint;
begin
   G_TYPE_PARAM_ENUM:=g_param_spec_types[10];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_ENUM(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_ENUM:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_ENUM);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_ENUM(pspec : longint) : longint;
begin
   G_PARAM_SPEC_ENUM:=PGParamSpecEnum(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_ENUM));
end;

function G_TYPE_PARAM_FLAGS : longint;
begin
   G_TYPE_PARAM_FLAGS:=g_param_spec_types[11];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_FLAGS(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_FLAGS:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_FLAGS);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_FLAGS(pspec : longint) : longint;
begin
   G_PARAM_SPEC_FLAGS:=PGParamSpecFlags(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_FLAGS));
end;

function G_TYPE_PARAM_FLOAT : longint;
begin
   G_TYPE_PARAM_FLOAT:=g_param_spec_types[12];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_FLOAT(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_FLOAT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_FLOAT);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_FLOAT(pspec : longint) : longint;
begin
   G_PARAM_SPEC_FLOAT:=PGParamSpecFloat(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_FLOAT));
end;

function G_TYPE_PARAM_DOUBLE : longint;
begin
   G_TYPE_PARAM_DOUBLE:=g_param_spec_types[13];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_DOUBLE(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_DOUBLE:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_DOUBLE);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_DOUBLE(pspec : longint) : longint;
begin
   G_PARAM_SPEC_DOUBLE:=PGParamSpecDouble(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_DOUBLE));
end;

function G_TYPE_PARAM_STRING : longint;
begin
   G_TYPE_PARAM_STRING:=g_param_spec_types[14];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_STRING(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_STRING:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_STRING);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_STRING(pspec : longint) : longint;
begin
   G_PARAM_SPEC_STRING:=PGParamSpecString(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_STRING));
end;

function G_TYPE_PARAM_PARAM : longint;
begin
   G_TYPE_PARAM_PARAM:=g_param_spec_types[15];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_PARAM(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_PARAM:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_PARAM);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_PARAM(pspec : longint) : longint;
begin
   G_PARAM_SPEC_PARAM:=PGParamSpecParam(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_PARAM));
end;

function G_TYPE_PARAM_BOXED : longint;
begin
   G_TYPE_PARAM_BOXED:=g_param_spec_types[16];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_BOXED(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_BOXED:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_BOXED);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_BOXED(pspec : longint) : longint;
begin
   G_PARAM_SPEC_BOXED:=PGParamSpecBoxed(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_BOXED));
end;

function G_TYPE_PARAM_POINTER : longint;
begin
   G_TYPE_PARAM_POINTER:=g_param_spec_types[17];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_POINTER(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_POINTER:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_POINTER);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_POINTER(pspec : longint) : longint;
begin
   G_PARAM_SPEC_POINTER:=PGParamSpecPointer(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_POINTER));
end;

function G_TYPE_PARAM_VALUE_ARRAY : longint;
begin
   G_TYPE_PARAM_VALUE_ARRAY:=g_param_spec_types[18];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_VALUE_ARRAY(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_VALUE_ARRAY:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_VALUE_ARRAY);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_VALUE_ARRAY(pspec : longint) : longint;
begin
   G_PARAM_SPEC_VALUE_ARRAY:=PGParamSpecValueArray(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_VALUE_ARRAY));
end;

function G_TYPE_PARAM_OBJECT : longint;
begin
   G_TYPE_PARAM_OBJECT:=g_param_spec_types[19];
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_IS_PARAM_SPEC_OBJECT(pspec : longint) : longint;
begin
   G_IS_PARAM_SPEC_OBJECT:=G_TYPE_CHECK_INSTANCE_TYPE(pspec,G_TYPE_PARAM_OBJECT);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function G_PARAM_SPEC_OBJECT(pspec : longint) : longint;
begin
   G_PARAM_SPEC_OBJECT:=PGParamSpecObject(G_TYPE_CHECK_INSTANCE_CAST(pspec,G_TYPE_PARAM_OBJECT));
end;

function null_fold_if_empty(var a : TGParamSpecString) : guint;
begin
   null_fold_if_empty:=(a.flag0 and bm_TGParamSpecString_null_fold_if_empty) shr bp_TGParamSpecString_null_fold_if_empty;
end;

procedure set_null_fold_if_empty(var a : TGParamSpecString; __null_fold_if_empty : guint);
begin
   a.flag0:=a.flag0 or ((__null_fold_if_empty shl bp_TGParamSpecString_null_fold_if_empty) and bm_TGParamSpecString_null_fold_if_empty);
end;

function ensure_non_null(var a : TGParamSpecString) : guint;
begin
   ensure_non_null:=(a.flag0 and bm_TGParamSpecString_ensure_non_null) shr bp_TGParamSpecString_ensure_non_null;
end;

procedure set_ensure_non_null(var a : TGParamSpecString; __ensure_non_null : guint);
begin
   a.flag0:=a.flag0 or ((__ensure_non_null shl bp_TGParamSpecString_ensure_non_null) and bm_TGParamSpecString_ensure_non_null);
end;


