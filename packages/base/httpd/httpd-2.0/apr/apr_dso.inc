{ Copyright 2000-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{
 * @file apr_dso.h
 * @brief APR Dynamic Object Handling Routines
 }

{#include "apr.h"
#include "apr_pools.h"
#include "apr_errno.h"}

{
 * @defgroup apr_dso Dynamic Object Handling
 * @ingroup APR 
 }

{$define APR_HAS_DSO}

{$if defined(APR_HAS_DSO) or defined(DOXYGEN)}

{
 * Structure for referencing dynamic objects
 }
type
  apr_dso_handle_t  = record
  end;
  Papr_dso_handle_t = ^apr_dso_handle_t;
  PPapr_dso_handle_t = ^Papr_dso_handle_t;

{
 * Structure for referencing symbols from dynamic objects
 }
  apr_dso_handle_sym_t = Pointer;
  Papr_dso_handle_sym_t = ^apr_dso_handle_sym_t;
  PPapr_dso_handle_sym_t = ^Papr_dso_handle_sym_t;

{
 * Load a DSO library.
 * @param res_handle Location to store new handle for the DSO.
 * @param path Path to the DSO library
 * @param ctx Pool to use.
 * @bug We aught to provide an alternative to RTLD_GLOBAL, which
 * is the only supported method of loading DSOs today.
 }
function apr_dso_load(res_handle: PPapr_dso_handle_t; const path: PChar;
 ctx: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dso_load' + LibSuff12;

{
 * Close a DSO library.
 * @param handle handle to close.
 }
function apr_dso_unload(handle: Papr_dso_handle_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dso_unload' + LibSuff4;

{
 * Load a symbol from a DSO handle.
 * @param ressym Location to store the loaded symbol
 * @param handle handle to load the symbol from.
 * @param symname Name of the symbol to load.
 }
function apr_dso_sym(ressym: Papr_dso_handle_t; handle: Papr_dso_handle_t;
 const symname: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dso_sym' + LibSuff12;

{
 * Report more information when a DSO function fails.
 * @param dso The dso handle that has been opened
 * @param buf Location to store the dso error
 * @param bufsize The size of the provided buffer
 }
function apr_dso_error(dso: Papr_dso_handle_t; buf: PChar;
 bufsize: apr_size_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dso_error' + LibSuff12;

{$endif}

