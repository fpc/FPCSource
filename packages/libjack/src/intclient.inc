(*
*  Copyright (C) 2004 Jack O'Quin
*
*  This program is free software; you can redistribute it and/or modify
*  it under the terms of the GNU Lesser General Public License as published by
*  the Free Software Foundation; either version 2.1 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public License
*  along with this program; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*
*)

{$ifndef __jack_intclient_h__}
{$define __jack_intclient_h__}

//#ifdef __cplusplus
//extern "C"
//{
//#endif

//#include <jack/types.h>

(**
 * Get an internal client's name.  This is useful when @ref
 * JackUseExactName was not specified on jack_internal_client_load()
 * and @ref JackNameNotUnique status was returned.  In that case, the
 * actual name will differ from the @a client_name requested.
 *
 * @param client requesting JACK client's handle.
 *
 * @param intclient handle returned from jack_internal_client_load()
 * or jack_internal_client_handle().
 *
 * @return NULL if unsuccessful, otherwise pointer to the internal
 * client name obtained from the heap via malloc().  The caller should
 * jack_free() this storage when no longer needed.
 *)
function jack_get_internal_client_name (client: Pjack_client_t;
                                        intclient: jack_intclient_t): PChar; cdecl; external libjack;

(**
 * Return the @ref jack_intclient_t handle for an internal client
 * running in the JACK server.
 *
 * @param client requesting JACK client's handle.
 *
 * @param client_name for the internal client of no more than
 * jack_client_name_size() characters.  The name scope is local to the
 * current server.
 *
 * @param status (if non-NULL) an address for JACK to return
 * information from this operation.  This status word is formed by
 * OR-ing together the relevant @ref JackStatus bits.
 *
 * @return Opaque internal client handle if successful.  If 0, the
 * internal client was not found, and @a *status includes the @ref
 * JackNoSuchClient and @ref JackFailure bits.
 *)
function jack_internal_client_handle (client: Pjack_client_t;
                                      const client_name: PChar;
                                      status: Pjack_status_t): jack_intclient_t; cdecl; external libjack;

(**
 * Load an internal client into the JACK server.
 *
 * Internal clients run inside the JACK server process.  They can use
 * most of the same functions as external clients.  Each internal
 * client is built as a shared object module, which must declare
 * jack_initialize() and jack_finish() entry points called at load and
 * unload times.  See @ref inprocess.c for an example.
 *
 * @param client loading JACK client's handle.
 *
 * @param client_name of at most jack_client_name_size() characters
 * for the internal client to load.  The name scope is local to the
 * current server.
 *
 * @param options formed by OR-ing together @ref JackOptions bits.
 * Only the @ref JackLoadOptions bits are valid.
 *
 * @param status (if non-NULL) an address for JACK to return
 * information from the load operation.  This status word is formed by
 * OR-ing together the relevant @ref JackStatus bits.
 *
 * <b>Optional parameters:</b> depending on corresponding [@a options
 * bits] additional parameters may follow @a status (in this order).
 *
 * @arg [@ref JackLoadName] <em>(char * ) load_name</em> is the shared
 * object file from which to load the new internal client (otherwise
 * use the @a client_name).
 *
 * @arg [@ref JackLoadInit] <em>(char * ) load_init</em> an arbitrary
 * string passed to the internal client's jack_initialize() routine
 * (otherwise NULL), of no more than @ref JACK_LOAD_INIT_LIMIT bytes.
 *
 * @return Opaque internal client handle if successful.  If this is 0,
 * the load operation failed, the internal client was not loaded, and
 * @a *status includes the @ref JackFailure bit.
 *)
function jack_internal_client_load (client: Pjack_client_t;
        const client_name: PChar;
        options: jack_options_t;
        status: Pjack_status_t): jack_intclient_t; cdecl; varargs; external libjack;
(**
 * Unload an internal client from a JACK server.  This calls the
 * intclient's jack_finish() entry point then removes it.  See @ref
 * inprocess.c for an example.
 *
 * @param client unloading JACK client's handle.
 *
 * @param intclient handle returned from jack_internal_client_load() or
 * jack_internal_client_handle().
 *
 * @return 0 if successful, otherwise @ref JackStatus bits.
 *)
function jack_internal_client_unload (client: Pjack_client_t;
        intclient: jack_intclient_t): jack_status_t; cdecl; external libjack;

//#ifdef __cplusplus
//}
//#endif

{$endif __jack_intclient_h__}
