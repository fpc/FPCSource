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
 * @file apr_poll.h
 * @brief APR Poll interface
 }
{#include "apr.h"
#include "apr_pools.h"
#include "apr_errno.h"
#include "apr_inherit.h" 
#include "apr_file_io.h" 
#include "apr_network_io.h" 

#if APR_HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif}

{
 * @defgroup apr_poll Poll Routines
 * @ingroup APR 
 }

{
 * @defgroup apr_poll_opt Poll options
 }
const
  APR_POLLIN   = $001;     {< Can read without blocking }
  APR_POLLPRI  = $002;     {< Priority data available }
  APR_POLLOUT  = $004;     {< Can write without blocking }
  APR_POLLERR  = $010;     {< Pending error }
  APR_POLLHUP  = $020;     {< Hangup occurred }
  APR_POLLNVAL = $040;     {< Descriptior invalid }

{ Used in apr_pollfd_t to determine what the apr_descriptor is }
type
  apr_datatype_e = (
    APR_NO_DESC,                {< nothing here }
    APR_POLL_SOCKET,            {< descriptor refers to a socket }
    APR_POLL_FILE,              {< descriptor refers to a file }
    APR_POLL_LASTDESC           {< descriptor is the last one in the list }
  );

{ Union of either an APR file or socket. }
  apr_descriptor = record
    case Integer of
     1: (f: Papr_file_t);        {< file }
     2: (s: Papr_socket_t);      {< socket }
  end;

{ @see apr_pollfd_t }
  Papr_pollfd_t = ^apr_pollfd_t;

  PPapr_pollfd_t = ^Papr_pollfd_t;

{ Poll descriptor set. }
  apr_pollfd_t = record
    p: Papr_pool_t;              {< associated pool }
    desc_type: apr_datatype_e;   {< descriptor type }
    reqevents: apr_int16_t;      {< requested events }
    rtnevents: apr_int16_t;      {< returned events }
    desc: apr_descriptor;        {< @see apr_descriptor }
    client_data: Pointer;        {< allows app to associate context }
  end;

{
 * Setup the memory required for poll to operate properly
 * @param new_poll The poll structure to be used. 
 * @param num The number of socket descriptors to be polled.
 * @param cont The pool to operate on.
 * @deprecated This function is deprecated, APR applications should control the pollset memory themselves.
 }
function apr_poll_setup(new_poll: PPapr_pollfd_t; num: apr_int32_t;
 cont: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll_setup' + LibSuff12;

{
 * Poll the sockets in the poll structure
 * @param aprset The poll structure we will be using. 
 * @param numsock The number of sockets we are polling
 * @param nsds The number of sockets signalled.
 * @param timeout The amount of time in microseconds to wait.  This is 
 *                a maximum, not a minimum.  If a socket is signalled, we 
 *                will wake up before this time.  A negative number means 
 *                wait until a socket is signalled.
 * @remark
 * <PRE>
 * The number of sockets signalled is returned in the second argument. 
 *
 *        This is a blocking call, and it will not return until either a 
 *        socket has been signalled, or the timeout has expired. 
 * </PRE>
 }
function apr_poll(aprset: Papr_pollfd_t; numsock: apr_int32_t;
 nsds: Papr_int32_t; timeout: apr_interval_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll' + LibSuff20;

{
 * Add a socket to the poll structure.
 * @param aprset The poll structure we will be using. 
 * @param sock The socket to add to the current poll structure. 
 * @param event The events to look for when we do the poll.  One of:
 * <PRE>
 *            APR_POLLIN       signal if read will not block
 *            APR_POLLPRI      signal if prioirty data is availble to be read
 *            APR_POLLOUT      signal if write will not block
 * </PRE>
 * @deprecated This function is deprecated, APR applications should control the pollset memory themselves.
 }
function apr_poll_socket_add(aprset: Papr_pollfd_t; sock: Papr_socket_t;
 event: apr_int16_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll_socket_add' + LibSuff12;

{
 * Modify a socket in the poll structure with mask.
 * @param aprset The poll structure we will be using. 
 * @param sock The socket to modify in poll structure. 
 * @param events The events to stop looking for during the poll.  One of:
 * <PRE>
 *            APR_POLLIN       signal if read will not block
 *            APR_POLLPRI      signal if priority data is available to be read
 *            APR_POLLOUT      signal if write will not block
 * </PRE>
 * @deprecated This function is deprecated, APR applications should control the pollset memory themselves.
 }
function apr_poll_socket_mask(aprset: Papr_pollfd_t; sock: Papr_socket_t;
 events: apr_int16_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll_socket_mask' + LibSuff12;

{
 * Remove a socket from the poll structure.
 * @param aprset The poll structure we will be using. 
 * @param sock The socket to remove from the current poll structure. 
 * @deprecated This function is deprecated, APR applications should control the pollset memory themselves.
 }
function apr_poll_socket_remove(aprset: Papr_pollfd_t; sock: Papr_socket_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll_socket_remove' + LibSuff8;

{
 * Clear all events in the poll structure.
 * @param aprset The poll structure we will be using. 
 * @param events The events to clear from all sockets.  One of:
 * <PRE>
 *            APR_POLLIN       signal if read will not block
 *            APR_POLLPRI      signal if priority data is available to be read
 *            APR_POLLOUT      signal if write will not block
 * </PRE>
 * @deprecated This function is deprecated, APR applications should control the pollset memory themselves.
 }
function apr_poll_socket_clear(aprset: Papr_pollfd_t;
 events: apr_int16_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll_socket_clear' + LibSuff8;

{
 * Get the return events for the specified socket.
 * @param event The returned events for the socket.  One of:
 * <PRE>
 *            APR_POLLIN       Data is available to be read 
 *            APR_POLLPRI      Priority data is availble to be read
 *            APR_POLLOUT      Write will succeed
 *            APR_POLLERR      An error occurred on the socket
 *            APR_POLLHUP      The connection has been terminated
 *            APR_POLLNVAL     This is an invalid socket to poll on.
 *                             Socket not open.
 * </PRE>
 * @param sock The socket we wish to get information about. 
 * @param aprset The poll structure we will be using. 
 * @deprecated This function is deprecated, APR applications should control the pollset memory themselves.
 }
function apr_poll_revents_get(event: Papr_int16_t; sock: Papr_socket_t;
 aprset: Papr_pollfd_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_poll_revents_get' + LibSuff12;

{ General-purpose poll API for arbitrarily large numbers of
 * file descriptors
 }

{ Opaque structure used for pollset API }
type
  apr_pollset_t = record end;
  Papr_pollset_t = ^apr_pollset_t;
  PPapr_pollset_t = ^Papr_pollset_t;

{
 * Setup a pollset object
 * @param pollset  The pointer in which to return the newly created object 
 * @param size The maximum number of descriptors that this pollset can hold
 * @param p The pool from which to allocate the pollset
 * @param flags Optional flags to modify the operation of the pollset
 *              (reserved for future expansion)
 }
function apr_pollset_create(pollset: PPapr_pollset_t; size: apr_uint32_tso_handle_t;
 p: Papr_pool_t; flags: apr_uint32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pollset_create' + LibSuff16;

{
 * Destroy a pollset object
 * @param pollset The pollset to destroy
 }
function apr_pollset_destroy(pollset: Papr_pollset_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pollset_destroy' + LibSuff4;

{
 * Add a socket or file descriptor to a pollset
 * @param pollset The pollset to which to add the descriptor
 * @param descriptor The descriptor to add
 * @remark If you set client_data in the descriptor, that value
 *         will be returned in the client_data field whenever this
 *         descriptor is signalled in apr_pollset_poll().
 }
function apr_pollset_add(pollset: Papr_pollset_t;
 const descriptor: Papr_pollfd_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pollset_add' + LibSuff8;

{
 * Remove a descriptor from a pollset
 * @param pollset The pollset from which to remove the descriptor
 * @param descriptor The descriptor to remove
 }
function apr_pollset_remove(pollset: Papr_pollset_t;
 const descriptor: Papr_pollfd_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pollset_remove' + LibSuff8;

{
 * Block for activity on the descriptor(s) in a pollset
 * @param pollset The pollset to use
 * @param timeout Timeout in microseconds
 * @param num Number of signalled descriptors (output parameter)
 * @param descriptors Array of signalled descriptors (output parameter)
 }
function apr_pollset_poll(pollset: Papr_pollset_t; timeout: apr_interval_time_t;
 num: Papr_int32_t; const descriptors: PPapr_pollfd_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pollset_poll' + LibSuff20;

