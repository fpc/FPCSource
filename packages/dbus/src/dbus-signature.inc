{ -*- mode: C; c-file-style: "gnu" -*- }
{ dbus-signatures.h utility functions for D-Bus types
 *
 * Copyright (C) 2005 Red Hat Inc.
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

{#include <dbus/dbus-macros.h>
#include <dbus/dbus-types.h>
#include <dbus/dbus-errors.h>}

{
 * DBusSignatureIter struct; contains no public fields 
 }
type
  DBusSignatureIter = record
    dummy1: Pointer;         {< Don't use this }
    dummy2: Pointer;         {< Don't use this }
    dummy8: dbus_uint32_t;   {< Don't use this }
    dummy12: cint;           {< Don't use this }
    dummy17: cint;           {< Don't use this }
  end;
  
  PDBusSignatureIter = ^DBusSignatureIter;

procedure dbus_signature_iter_init(iter: PDBusSignatureIter; const signature: PAnsiChar); cdecl; external LibDBus;

function dbus_signature_iter_get_current_type(const iter: PDBusSignatureIter): Integer; cdecl; external LibDBus;

function dbus_signature_iter_get_signature(const iter: PDBusSignatureIter): PAnsiChar; cdecl; external LibDBus;

function dbus_signature_iter_get_element_type(const iter: PDBusSignatureIter): Integer; cdecl; external LibDBus;

function dbus_signature_iter_next(const iter: PDBusSignatureIter): dbus_bool_t; cdecl; external LibDBus;

procedure dbus_signature_iter_recurse(const iter: PDBusSignatureIter; subiter: PDBusSignatureIter); cdecl; external LibDBus;

function dbus_signature_validate(const signature: PAnsiChar; error: PDBusError): dbus_bool_t; cdecl; external LibDBus;

function dbus_signature_validate_single(const signature: PAnsiChar; error: PDBusError): dbus_bool_t; cdecl; external LibDBus;

function dbus_type_is_basic(typecode: cint): dbus_bool_t; cdecl; external LibDBus;
function dbus_type_is_container(typecode: cint): dbus_bool_t; cdecl; external LibDBus;
function dbus_type_is_fixed(typecode: cint): dbus_bool_t; cdecl; external LibDBus;

