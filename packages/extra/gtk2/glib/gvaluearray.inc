
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

   gvaluearray.h: GLib array type holding GValues
  }

{ --- typedefs & structs ---  }
type
{< private > }
   PGValueArray = ^TGValueArray;
   TGValueArray = record
        n_values : guint;
        values : PGValue;
        n_prealloced : guint;
     end;

{ --- prototypes ---  }

function g_value_array_get_nth(value_array:PGValueArray; index:guint):PGValue; cdecl; external gobjectlib;
function g_value_array_new(n_prealloced:guint):PGValueArray; cdecl; external gobjectlib;
procedure g_value_array_free(value_array:PGValueArray); cdecl; external gobjectlib;

function g_value_array_copy(value_array:PGValueArray):PGValueArray; cdecl; external gobjectlib;

function g_value_array_prepend(value_array:PGValueArray; value:PGValue):PGValueArray; cdecl; external gobjectlib;

function g_value_array_append(value_array:PGValueArray; value:PGValue):PGValueArray; cdecl; external gobjectlib;

function g_value_array_insert(value_array:PGValueArray; index:guint; value:PGValue):PGValueArray; cdecl; external gobjectlib;
function g_value_array_remove(value_array:PGValueArray; index:guint):PGValueArray; cdecl; external gobjectlib;
function g_value_array_sort(value_array:PGValueArray; compare_func:TGCompareFunc):PGValueArray; cdecl; external gobjectlib;
function g_value_array_sort_with_data(value_array:PGValueArray; compare_func:TGCompareDataFunc; user_data:gpointer):PGValueArray; cdecl; external gobjectlib;

