{ -*- mode: C; c-file-style: "gnu" -*- }
{ dbus-errors.h Error reporting
 *
 * Copyright (C) 2002  Red Hat Inc.
 * Copyright (C) 2003  CodeFactory AB
 *
 * Licensed under the Academic Free License version 2.1
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 }

//#include <dbus/dbus-macros.h>
//#include <dbus/dbus-types.h>

type
  PDBusError = ^DBusError;

{
 * Object representing an exception.
 }
  DBusError = record
    name: PAnsiChar;    {< error name }
    message: PAnsiChar; {< error message }

    dummy1: cuint; //: 1; {< placeholder }
//  unsigned int dummy2 : 1; {< placeholder }
//  unsigned int dummy3 : 1; {< placeholder }
//  unsigned int dummy4 : 1; {< placeholder }
//  unsigned int dummy5 : 1; {< placeholder }

    padding1: Pointer; {< placeholder }
  end;

procedure dbus_error_init(error: PDBusError); cdecl; external LibDBus;
procedure dbus_error_free(error: PDBusError); cdecl; external LibDBus;
procedure dbus_set_error(error: PDBusError; const name, message: PAnsiChar; others: array of const); cdecl; external LibDBus;
procedure dbus_set_error_const(error: PDBusError; const name, message: PAnsiChar); cdecl; external LibDBus;
procedure dbus_move_error(src, dest: PDBusError); cdecl; external LibDBus;
function dbus_error_has_name(const error: PDBusError; const name: PAnsiChar): dbus_bool_t; cdecl; external LibDBus;
function dbus_error_is_set(const error: PDBusError): dbus_bool_t; cdecl; external LibDBus;

