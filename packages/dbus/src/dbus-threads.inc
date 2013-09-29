{ -*- mode: C; c-file-style: "gnu" -*- }
{ dbus-threads.h  D-Bus threads handling
 *
 * Copyright (C) 2002  Red Hat Inc.
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
#include <dbus/dbus-types.h>}

type
  DBusMutex = record end;
  PDBusMutex = ^DBusMutex;

  DBusCondVar = record end;
  PDBusCondVar = ^DBusCondVar;
  
  DBusMutexNewFunction = function (): DBusMutex; cdecl;
  DBusMutexFreeFunction = procedure (mutex: PDBusMutex); cdecl;
  DBusMutexLockFunction = function (mutex: PDBusMutex): dbus_bool_t; cdecl;
  DBusMutexUnlockFunction = function (mutex: PDBusMutex): dbus_bool_t; cdecl;

  DBusRecursiveMutexNewFunction = function(): DBusMutex; cdecl;
  DBusRecursiveMutexFreeFunction = procedure (mutex: PDBusMutex); cdecl;
  DBusRecursiveMutexLockFunction = procedure (mutex: PDBusMutex); cdecl;
  DBusRecursiveMutexUnlockFunction = procedure (mutex: PDBusMutex); cdecl;

  DBusCondVarNewFunction = function (): PDBusCondVar; cdecl;
  DBusCondVarFreeFunction = procedure (cond: PDBusCondVar); cdecl;
  DBusCondVarWaitFunction = procedure (cond: PDBusCondVar; mutex: PDBusMutex); cdecl;
  DBusCondVarWaitTimeoutFunction = procedure (cond: PDBusCondVar; mutex: PDBusMutex;
   timeout_milliseconds: cint); cdecl;
  DBusCondVarWakeOneFunction = procedure (cond: PDBusCondVar); cdecl;
  DBusCondVarWakeAllFunction = procedure (cond: PDBusCondVar); cdecl;

  DBusThreadFunctionsMask =
  (
   DBUS_THREAD_FUNCTIONS_MUTEX_NEW_MASK      = 1 shl 0,
   DBUS_THREAD_FUNCTIONS_MUTEX_FREE_MASK     = 1 shl 1,
   DBUS_THREAD_FUNCTIONS_MUTEX_LOCK_MASK     = 1 shl 2,
   DBUS_THREAD_FUNCTIONS_MUTEX_UNLOCK_MASK   = 1 shl 3,
   DBUS_THREAD_FUNCTIONS_CONDVAR_NEW_MASK    = 1 shl 4,
   DBUS_THREAD_FUNCTIONS_CONDVAR_FREE_MASK   = 1 shl 5,
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAIT_MASK   = 1 shl 6,
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAIT_TIMEOUT_MASK   = 1 shl 7,
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAKE_ONE_MASK = 1 shl 8,
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAKE_ALL_MASK = 1 shl 9,
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_NEW_MASK    = 1 shl 10,
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_FREE_MASK   = 1 shl 11,
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_LOCK_MASK   = 1 shl 12,
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_UNLOCK_MASK = 1 shl 13,

   DBUS_THREAD_FUNCTIONS_ALL_MASK     = (1 shl 14) - 1
  );

{
 * Functions that must be implemented to make the D-Bus
 * library thread-aware. 
 }
  DBusThreadFunctions = record
   mask: cuint; {< Mask indicating which functions are present. }

   mutex_new: DBusMutexNewFunction; {< Function to create a mutex }
   mutex_free: DBusMutexFreeFunction; {< Function to free a mutex }
   mutex_lock: DBusMutexLockFunction; {< Function to lock a mutex }
   mutex_unlock: DBusMutexUnlockFunction; {< Function to unlock a mutex }

   condvar_new: DBusCondVarNewFunction; {< Function to create a condition variable }
   condvar_free: DBusCondVarFreeFunction; {< Function to free a condition variable }
   condvar_wait: DBusCondVarWaitFunction; {< Function to wait on a condition }
   condvar_wait_timeout: DBusCondVarWaitTimeoutFunction; {< Function to wait on a condition with a timeout }
   condvar_wake_one: DBusCondVarWakeOneFunction; {< Function to wake one thread waiting on the condition }
   condvar_wake_all: DBusCondVarWakeAllFunction; {< Function to wake all threads waiting on the condition }

   recursive_mutex_new: DBusRecursiveMutexNewFunction; {< Function to create a recursive mutex }
   recursive_mutex_free: DBusRecursiveMutexFreeFunction; {< Function to free a recursive mutex }
   recursive_mutex_lock: DBusRecursiveMutexLockFunction; {< Function to lock a recursive mutex }
   recursive_mutex_unlock: DBusRecursiveMutexUnlockFunction; {< Function to unlock a recursive mutex }

   padding1: procedure; {< Reserved for future expansion }
   padding2: procedure; {< Reserved for future expansion }
   padding3: procedure; {< Reserved for future expansion }
   padding4: procedure; {< Reserved for future expansion }
  end;
  
  PDBusThreadFunctions = ^DBusThreadFunctions;

function dbus_threads_init(const functions: PDBusThreadFunctions): dbus_bool_t; cdecl; external LibDBus;
function dbus_threads_init_default: dbus_bool_t; cdecl; external LibDBus;

