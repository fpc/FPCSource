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
 * @file apr_network_io.h
 * @brief APR Network library
 }

{#include "apr.h"
#include "apr_pools.h"
#include "apr_file_io.h"
#include "apr_errno.h"
#include "apr_inherit.h" 

#if APR_HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif}

{
 * @defgroup apr_network_io Network Routines
 * @ingroup APR 
 }

const
{ Maximum seconds to linger }
  APR_MAX_SECS_TO_LINGER = 30;

{ Maximum hostname length }
  APRMAXHOSTLEN = 256;

{ Default 'any' address }
  APR_ANYADDR  = '0.0.0.0';

{
 * @defgroup apr_sockopt Socket option definitions
 }
  APR_SO_LINGER       = 1;    {< Linger }
  APR_SO_KEEPALIVE    = 2;    {< Keepalive }
  APR_SO_DEBUG        = 4;    {< Debug }
  APR_SO_NONBLOCK     = 8;    {< Non-blocking IO }
  APR_SO_REUSEADDR    = 16;   {< Reuse addresses }
  APR_SO_SNDBUF       = 64;   {< Send buffer }
  APR_SO_RCVBUF       = 128;  {< Receive buffer }
  APR_SO_DISCONNECTED = 256;  {< Disconnected }
  APR_TCP_NODELAY     = 512;  {< For SCTP sockets, this is mapped
                                   * to STCP_NODELAY internally.
                                   }
  APR_TCP_NOPUSH      = 1024; {< No push }
  APR_RESET_NODELAY   = 2048; {< This flag is ONLY set internally
                                   * when we set APR_TCP_NOPUSH with
                                   * APR_TCP_NODELAY set to tell us that
                                   * APR_TCP_NODELAY should be turned on
                                   * again when NOPUSH is turned off
                                   }
  APR_INCOMPLETE_READ = 4096; {< Set on non-blocking sockets
				   * (timeout != 0) on which the
				   * previous read() did not fill a buffer
				   * completely.  the next apr_socket_recv() 
                                   * will first call select()/poll() rather than
				   * going straight into read().  (Can also
				   * be set by an application to force a
				   * select()/poll() call before the next
				   * read, in cases where the app expects
				   * that an immediate read would fail.)
                                   }
  APR_INCOMPLETE_WRITE = 8192;{< like APR_INCOMPLETE_READ, but for write
                                   * @see APR_INCOMPLETE_READ
                                   }
  APR_IPV6_V6ONLY    = 16384; {< Don't accept IPv4 connections on an
                                   * IPv6 listening socket.
                                   }
  APR_TCP_DEFER_ACCEPT = 32768;{< Delay accepting of new connections
                                   * until data is available.
                                   * @see apr_socket_accept_filter
                                   }

{ Define what type of socket shutdown should occur. }
type
  apr_shutdown_how_e = (
    APR_SHUTDOWN_READ,          {< no longer allow read request }
    APR_SHUTDOWN_WRITE,         {< no longer allow write requests }
    APR_SHUTDOWN_READWRITE      {< no longer allow read or write requests }
  );

const
  APR_IPV4_ADDR_OK = $01;  {< @see apr_sockaddr_info_get() }
  APR_IPV6_ADDR_OK = $02;  {< @see apr_sockaddr_info_get() }

{$ifndef APR_HAVE_IN_ADDR}
{
 * We need to make sure we always have an in_addr type, so APR will just
 * define it ourselves, if the platform doesn't provide it.
 }
{type
  in_addr = record
    s_addr: apr_uint32_t; < storage to hold the IP#
  end;}
{$endif}

{ @def APR_INADDR_NONE
 * Not all platforms have a real INADDR_NONE.  This macro replaces
 * INADDR_NONE on all platforms.
 }
{$ifdef INADDR_NONE}
  APR_INADDR_NONE = INADDR_NONE;
{$else}
  APR_INADDR_NONE = $ffffffff;
{$endif}

const
{
 * @def APR_INET
 * Not all platforms have these defined, so we'll define them here
 * The default values come from FreeBSD 4.1.1
 }
//#define APR_INET     AF_INET
{ @def APR_UNSPEC
 * Let the system decide which address family to use
 }
{#ifdef AF_UNSPEC
#define APR_UNSPEC   AF_UNSPEC
#else
#define APR_UNSPEC   0
#endif
#if APR_HAVE_IPV6

/** @def APR_INET6
* IPv6 Address Family. Not all platforms may have this defined.
*/

#define APR_INET6    AF_INET6
#endif}

{
 * @defgroup IP_Proto IP Protocol Definitions for use when creating sockets
 }
  APR_PROTO_TCP      = 6;   {< TCP  }
  APR_PROTO_UDP     = 17;   {< UDP  }
  APR_PROTO_SCTP   = 132;   {< SCTP }

{
 * Enum to tell us if we're interested in remote or local socket
 }
type
  apr_interface_e = (
    APR_LOCAL,
    APR_REMOTE
  );

{
 * The specific declaration of inet_addr's ... some platforms fall back
 * inet_network (this is not good, but necessary)
 }

{$ifdef APR_HAVE_INET_ADDR}
  apr_inet_addr   = inet_addr;
{$else}
{$ifdef APR_HAVE_INET_NETWORK}        { only DGUX, as far as I know }
{
 * @warning
 * not generally safe... inet_network() and inet_addr() perform
 * different functions }
  apr_inet_addr   = inet_network;
{$endif}
{$endif}

{ A structure to represent sockets }
  apr_socket_t = record
  end;
  Papr_socket_t = ^apr_socket_t;
  PPapr_socket_t = ^Papr_socket_t;

{
 * A structure to encapsulate headers and trailers for apr_socket_sendfile
 }
  Papr_hdtr_t = ^apr_hdtr_t;
  { A structure to represent in_addr }
  apr_in_addr_t = record
  end;
{ A structure to represent an IP subnet }
  apr_ipsubnet_t = record
  end;
  Papr_ipsubnet_t = ^apr_ipsubnet_t;
  PPapr_ipsubnet_t = ^Papr_ipsubnet_t;

{ @remark use apr_uint16_t just in case some system has a short that isn't 16 bits... }
  apr_port_t = apr_uint16_t;
  Papr_port_t = ^apr_port_t;

{ @remark It's defined here as I think it should all be platform safe...
 * @see apr_sockaddr_t
 }
  Papr_sockaddr_t = ^apr_sockaddr_t;
  PPapr_sockaddr_t = ^Papr_sockaddr_t;

  sa_t = record
   case Integer of
    { IPv4 sockaddr structure }
    0: (sin: sockaddr_in);
{$ifdef APR_HAVE_IPV6}
    { IPv6 sockaddr structure }
    1: (sin6: sockaddr_in6);
{$endif}
{$ifdef APR_HAVE_SA_STORAGE}
    { Placeholder to ensure that the size of this union is not
    * dependent on whether APR_HAVE_IPV6 is defined. }
    2: (sas: sockaddr_storage);
{$endif}
  end;

{
 * APRs socket address type, used to ensure protocol independence
 }
  apr_sockaddr_t = record
    { The pool to use... }
    pool: Papr_pool_t;
    { The hostname }
    hostname: PChar;
    { Either a string of the port number or the service name for the port }
    servname: PChar;
    { The numeric port }
    port: apr_port_t;
    { The family }
    family: apr_int32_t;
    { How big is the sockaddr we're using? }
    salen: apr_socklen_t;
    { How big is the ip address structure we're using? }
    ipaddr_len: cint;
    { How big should the address buffer be?  16 for v4 or 46 for v6
     *  used in inet_ntop... }
    addr_str_len: cint;
    { This points to the IP address structure within the appropriate
     *  sockaddr structure.  }
    ipaddr_ptr: Pointer;
    { If multiple addresses were found by apr_sockaddr_info_get(), this
     *  points to a representation of the next address. }
    next: Papr_sockaddr_t;
    { Union of either IPv4 or IPv6 sockaddr. }
    sa: sa_t;
  end;

{$ifdef APR_HAS_SENDFILE}
{
 * Support reusing the socket on platforms which support it (from disconnect,
 * specifically Win32.
 * @remark Optional flag passed into apr_socket_sendfile() 
 }
  APR_SENDFILE_DISCONNECT_SOCKET     = 1;
{$endif}

{ A structure to encapsulate headers and trailers for apr_socket_sendfile }
  apr_hdtr_t = record
    { An iovec to store the headers sent before the file. }
    headers: Piovec;
    { number of headers in the iovec }
    numheaders: cint;
    { An iovec to store the trailers sent after the file. }
    trailers: Piovec;
    { number of trailers in the iovec }
    numtrailers: Integer;
  end;

{ function definitions }

{
 * Create a socket.
 * @param new_sock The new socket that has been set up.
 * @param family The address family of the socket (e.g., APR_INET).
 * @param type The type of the socket (e.g., SOCK_STREAM).
 * @param cont The pool to use
 }
function apr_socket_create(new_sock: PPapr_socket_t;
 family, type_, protocol: Integer; cont: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_create' + LibSuff20;

{
 * Shutdown either reading, writing, or both sides of a socket.
 * @param thesocket The socket to close 
 * @param how How to shutdown the socket.  One of:
 * <PRE>
 *            APR_SHUTDOWN_READ         no longer allow read requests
 *            APR_SHUTDOWN_WRITE        no longer allow write requests
 *            APR_SHUTDOWN_READWRITE    no longer allow read or write requests 
 * </PRE>
 * @see apr_shutdown_how_e
 * @remark This does not actually close the socket descriptor, it just
 *      controls which calls are still valid on the socket.
 }
function apr_socket_shutdown(thesocket: Papr_socket_t;
 how: apr_shutdown_how_e): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_shutdown' + LibSuff8;

{
 * Close a socket.
 * @param thesocket The socket to close 
 }
function apr_socket_close(thesocket: Papr_socket_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_close' + LibSuff4;

{
 * Bind the socket to its associated port
 * @param sock The socket to bind 
 * @param sa The socket address to bind to
 * @remark This may be where we will find out if there is any other process
 *      using the selected port.
 }
function apr_socket_bind(sock: Papr_socket_t;
 sa: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_bind' + LibSuff8;

{
 * Listen to a bound socket for connections.
 * @param sock The socket to listen on 
 * @param backlog The number of outstanding connections allowed in the sockets
 *                listen queue.  If this value is less than zero, the listen
 *                queue size is set to zero.  
 }
function apr_socket_listen(sock: Papr_socket_t;
 backlog: apr_int32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_listen' + LibSuff8;

{
 * Accept a new connection request
 * @param new_sock A copy of the socket that is connected to the socket that
 *                 made the connection request.  This is the socket which should
 *                 be used for all future communication.
 * @param sock The socket we are listening on.
 * @param connection_pool The pool for the new socket.
 }
function apr_socket_accept(new_sock: PPapr_socket_t;
 sock: Papr_socket_t; connection_pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_accept' + LibSuff12;

{
 * Issue a connection request to a socket either on the same machine 
 * or a different one.
 * @param sock The socket we wish to use for our side of the connection 
 * @param sa The address of the machine we wish to connect to.  If NULL,
 *           APR assumes that the sockaddr_in in the apr_socket is 
 *           completely filled out.
 }
function apr_socket_connect(sock: Papr_socket_t; sa: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_connect' + LibSuff8;

{
 * Create apr_sockaddr_t from hostname, address family, and port.
 * @param sa The new apr_sockaddr_t.
 * @param hostname The hostname or numeric address string to resolve/parse, or
 *               NULL to build an address that corresponds to 0.0.0.0 or ::
 * @param family The address family to use, or APR_UNSPEC if the system should 
 *               decide.
 * @param port The port number.
 * @param flags Special processing flags:
 * <PRE>
 *       APR_IPV4_ADDR_OK          first query for IPv4 addresses; only look
 *                                 for IPv6 addresses if the first query failed;
 *                                 only valid if family is APR_UNSPEC and hostname
 *                                 isn't NULL; mutually exclusive with
 *                                 APR_IPV6_ADDR_OK
 *       APR_IPV6_ADDR_OK          first query for IPv6 addresses; only look
 *                                 for IPv4 addresses if the first query failed;
 *                                 only valid if family is APR_UNSPEC and hostname
 *                                 isn't NULL and APR_HAVE_IPV6; mutually exclusive
 *                                 with APR_IPV4_ADDR_OK
 * </PRE>
 * @param p The pool for the apr_sockaddr_t and associated storage.
 }
function apr_sockaddr_info_get(sa: PPapr_socket_t;
 const hostname: PChar; family: apr_int32_t;
 port: apr_port_t; flags: apr_int32_t; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_sockaddr_info_get' + LibSuff24;

{
 * Look up the host name from an apr_sockaddr_t.
 * @param hostname The hostname.
 * @param sa The apr_sockaddr_t.
 * @param flags Special processing flags.
 }
function apr_getnameinfo(hostname: PPChar; sa: Papr_socket_t; flags: apr_int32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_getnameinfo' + LibSuff12;

{
 * Parse hostname/IP address with scope id and port.
 *
 * Any of the following strings are accepted:
 *   8080                  (just the port number)
 *   www.apache.org        (just the hostname)
 *   www.apache.org:8080   (hostname and port number)
 *   [fe80::1]:80          (IPv6 numeric address string only)
 *   [fe80::1%eth0]        (IPv6 numeric address string and scope id)
 *
 * Invalid strings:
 *                         (empty string)
 *   [abc]                 (not valid IPv6 numeric address string)
 *   abc:65536             (invalid port number)
 *
 * @param addr The new buffer containing just the hostname.  On output, *addr 
 *             will be NULL if no hostname/IP address was specfied.
 * @param scope_id The new buffer containing just the scope id.  On output, 
 *                 *scope_id will be NULL if no scope id was specified.
 * @param port The port number.  On output, *port will be 0 if no port was 
 *             specified.
 *             ### FIXME: 0 is a legal port (per RFC 1700). this should
 *             ### return something besides zero if the port is missing.
 * @param str The input string to be parsed.
 * @param p The pool from which *addr and *scope_id are allocated.
 * @remark If scope id shouldn't be allowed, check for scope_id != NULL in 
 *         addition to checking the return code.  If addr/hostname should be 
 *         required, check for addr == NULL in addition to checking the 
 *         return code.
 }
function apr_parse_addr_port(addr, scope_id: PPChar; port: Papr_port_t;
 const str: PChar; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_parse_addr_port' + LibSuff20;

{
 * Get name of the current machine
 * @param buf A buffer to store the hostname in.
 * @param len The maximum length of the hostname that can be stored in the
 *            buffer provided.  The suggested length is APRMAXHOSTLEN + 1.
 * @param cont The pool to use.
 * @remark If the buffer was not large enough, an error will be returned.
 }
function apr_gethostname(buf: PChar; len: Integer; cont: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_gethostname' + LibSuff12;

{
 * Return the data associated with the current socket
 * @param data The user data associated with the socket.
 * @param key The key to associate with the user data.
 * @param sock The currently open socket.
 }
function apr_socket_data_get(data: PPointer; const key: PChar;
 sock: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_data_get' + LibSuff12;

{
 * Set the data associated with the current socket.
 * @param sock The currently open socket.
 * @param data The user data to associate with the socket.
 * @param key The key to associate with the data.
 * @param cleanup The cleanup to call when the socket is destroyed.
 }
type
  cleanup_t = function (param: Pointer): apr_status_t;
  
function apr_socket_data_set(sock: Papr_socket_t; data: Pointer;
 const key: PChar; cleanup: cleanup_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_data_set' + LibSuff16;

{
 * Send data over a network.
 * @param sock The socket to send the data over.
 * @param buf The buffer which contains the data to be sent. 
 * @param len On entry, the number of bytes to send; on exit, the number
 *            of bytes sent.
 * @remark
 * <PRE>
 * This functions acts like a blocking write by default.  To change 
 * this behavior, use apr_socket_timeout_set().
 *
 * It is possible for both bytes to be sent and an error to be returned.
 *
 * APR_EINTR is never returned.
 * </PRE>
 }
function apr_socket_send(sock: Papr_socket_t; const buf: PChar;
 len: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_send' + LibSuff12;

{
 * Send multiple packets of data over a network.
 * @param sock The socket to send the data over.
 * @param vec The array of iovec structs containing the data to send 
 * @param nvec The number of iovec structs in the array
 * @param len Receives the number of bytes actually written
 * @remark
 * <PRE>
 * This functions acts like a blocking write by default.  To change 
 * this behavior, use apr_socket_timeout_set().
 * The number of bytes actually sent is stored in argument 3.
 *
 * It is possible for both bytes to be sent and an error to be returned.
 *
 * APR_EINTR is never returned.
 * </PRE>
 }
function apr_socket_sendv(sock: Papr_socket_t; const vec: Piovec;
 nvec: apr_int32_t; len: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_sendv' + LibSuff16;

{
 * @param sock The socket to send from
 * @param where The apr_sockaddr_t describing where to send the data
 * @param flags The flags to use
 * @param buf  The data to send
 * @param len  The length of the data to send
 }
function apr_socket_sendto(sock: Papr_socket_t; where: Papr_sockaddr_t;
 flags: apr_int32_t; const buf: PChar; len: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_sendto' + LibSuff20;

{
 * @param from The apr_sockaddr_t to fill in the recipient info
 * @param sock The socket to use
 * @param flags The flags to use
 * @param buf  The buffer to use
 * @param len  The length of the available buffer
 }
function apr_socket_recvfrom(from: Papr_sockaddr_t; sock: Papr_socket_t;
 flags: apr_int32_t; buf: PChar; len: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_recvfrom' + LibSuff20;

{$if defined(APR_HAS_SENDFILE) or defined(DOXYGEN)}

{
 * Send a file from an open file descriptor to a socket, along with 
 * optional headers and trailers
 * @param sock The socket to which we're writing
 * @param file The open file from which to read
 * @param hdtr A structure containing the headers and trailers to send
 * @param offset Offset into the file where we should begin writing
 * @param len (input)  - Number of bytes to send from the file 
 *            (output) - Number of bytes actually sent, 
 *                       including headers, file, and trailers
 * @param flags APR flags that are mapped to OS specific flags
 * @remark This functions acts like a blocking write by default.  To change 
 *         this behavior, use apr_socket_timeout_set().
 *         The number of bytes actually sent is stored in argument 5.
 }
function apr_socket_sendfile(sock: Papr_socket_t; file_: Papr_file_t;
 hdtr: Papr_hdtr_t; offset: Papr_off_t; len: Papr_size_t;
 flags: apr_int32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_sendfile' + LibSuff24;

{$endif} { APR_HAS_SENDFILE }

{
 * Read data from a network.
 * @param sock The socket to read the data from.
 * @param buf The buffer to store the data in. 
 * @param len On entry, the number of bytes to receive; on exit, the number
 *            of bytes received.
 * @remark
 * <PRE>
 * This functions acts like a blocking read by default.  To change 
 * this behavior, use apr_socket_timeout_set().
 * The number of bytes actually sent is stored in argument 3.
 *
 * It is possible for both bytes to be received and an APR_EOF or
 * other error to be returned.
 *
 * APR_EINTR is never returned.
 * </PRE>
 }
function apr_socket_recv(sock: Papr_socket_t; buf: PChar; len: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_recv' + LibSuff12;

{
 * Setup socket options for the specified socket
 * @param sock The socket to set up.
 * @param opt The option we would like to configure.  One of:
 * <PRE>
 *            APR_SO_DEBUG      --  turn on debugging information 
 *            APR_SO_KEEPALIVE  --  keep connections active
 *            APR_SO_LINGER     --  lingers on close if data is present
 *            APR_SO_NONBLOCK   --  Turns blocking on/off for socket
 *                                  When this option is enabled, use
 *                                  the APR_STATUS_IS_EAGAIN() macro to
 *                                  see if a send or receive function
 *                                  could not transfer data without
 *                                  blocking.
 *            APR_SO_REUSEADDR  --  The rules used in validating addresses
 *                                  supplied to bind should allow reuse
 *                                  of local addresses.
 *            APR_SO_SNDBUF     --  Set the SendBufferSize
 *            APR_SO_RCVBUF     --  Set the ReceiveBufferSize
 * </PRE>
 * @param on Value for the option.
 }
function apr_socket_opt_set(sock: Papr_socket_t; opt, on_: apr_int32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_opt_set' + LibSuff12;

{
 * Setup socket timeout for the specified socket
 * @param sock The socket to set up.
 * @param t Value for the timeout.
 * <PRE>
 *   t > 0  -- read and write calls return APR_TIMEUP if specified time
 *             elapsess with no data read or written
 *   t == 0 -- read and write calls never block
 *   t < 0  -- read and write calls block
 * </PRE>
 }
function apr_socket_timeout_set(sock: Papr_socket_t; t: apr_interval_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_timeout_set' + LibSuff12;

{
 * Query socket options for the specified socket
 * @param sock The socket to query
 * @param opt The option we would like to query.  One of:
 * <PRE>
 *            APR_SO_DEBUG      --  turn on debugging information 
 *            APR_SO_KEEPALIVE  --  keep connections active
 *            APR_SO_LINGER     --  lingers on close if data is present
 *            APR_SO_NONBLOCK   --  Turns blocking on/off for socket
 *            APR_SO_REUSEADDR  --  The rules used in validating addresses
 *                                  supplied to bind should allow reuse
 *                                  of local addresses.
 *            APR_SO_SNDBUF     --  Set the SendBufferSize
 *            APR_SO_RCVBUF     --  Set the ReceiveBufferSize
 *            APR_SO_DISCONNECTED -- Query the disconnected state of the socket.
 *                                  (Currently only used on Windows)
 * </PRE>
 * @param on Socket option returned on the call.
 }
function apr_socket_opt_get(sock: Papr_socket_t; opt, on_: apr_int32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_opt_get' + LibSuff12;

{
 * Query socket timeout for the specified socket
 * @param sock The socket to query
 * @param t Socket timeout returned from the query.
 }
function apr_socket_timeout_get(sock: Papr_socket_t; t: Papr_interval_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_timeout_get' + LibSuff8;

{
 * Query the specified socket if at the OOB/Urgent data mark
 * @param sock The socket to query
 * @param atmark Is set to true if socket is at the OOB/urgent mark,
 *               otherwise is set to false.
 }
function apr_socket_atmark(sock: Papr_socket_t; atmark: PInteger): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_atmark' + LibSuff8;

{
 * Return an apr_sockaddr_t from an apr_socket_t
 * @param sa The returned apr_sockaddr_t.
 * @param which Which interface do we want the apr_sockaddr_t for?
 * @param sock The socket to use
 }
function apr_socket_addr_get(sa: PPapr_sockaddr_t;
 which: apr_interface_e; sock: Papr_socket_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_addr_get' + LibSuff12;

{
 * Return the IP address (in numeric address string format) in
 * an APR socket address.  APR will allocate storage for the IP address 
 * string from the pool of the apr_sockaddr_t.
 * @param addr The IP address.
 * @param sockaddr The socket address to reference.
 }
function apr_sockaddr_ip_get(addr: PPChar; sockaddr: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_sockaddr_ip_get' + LibSuff8;

{
 * See if the IP addresses in two APR socket addresses are
 * equivalent.  Appropriate logic is present for comparing
 * IPv4-mapped IPv6 addresses with IPv4 addresses.
 *
 * @param addr1 One of the APR socket addresses.
 * @param addr2 The other APR socket address.
 * @remark The return value will be non-zero if the addresses
 * are equivalent.
 }
function apr_sockaddr_equal(const addr1, addr2: Papr_sockaddr_t): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_sockaddr_equal' + LibSuff8;

{
 * Return the type of the socket.
 * @param sock The socket to query.
 * @param type The returned type (e.g., SOCK_STREAM).
 }
function apr_socket_type_get(sock: Papr_socket_t; type_: PInteger): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_type_get' + LibSuff8;

{
 * Given an apr_sockaddr_t and a service name, set the port for the service
 * @param sockaddr The apr_sockaddr_t that will have its port set
 * @param servname The name of the service you wish to use
 }
function apr_getservbyname(sockaddr: Papr_sockaddr_t; const servname: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_getservbyname' + LibSuff8;

{
 * Build an ip-subnet representation from an IP address and optional netmask or
 * number-of-bits.
 * @param ipsub The new ip-subnet representation
 * @param ipstr The input IP address string
 * @param mask_or_numbits The input netmask or number-of-bits string, or NULL
 * @param p The pool to allocate from
 }
function apr_ipsubnet_create(ipsub: PPapr_ipsubnet_t;
 const ipstr, mask_or_numbits: PChar; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_ipsubnet_create' + LibSuff16;

{
 * Test the IP address in an apr_sockaddr_t against a pre-built ip-subnet
 * representation.
 * @param ipsub The ip-subnet representation
 * @param sa The socket address to test
 * @return non-zero if the socket address is within the subnet, 0 otherwise
 }
function apr_ipsubnet_test(ipsub: Papr_ipsubnet_t; sa: Papr_sockaddr_t): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_ipsubnet_test' + LibSuff8;

{$if defined(APR_HAS_SO_ACCEPTFILTER) or defined(DOXYGEN)}
{
 * Set an OS level accept filter.
 * @param sock The socket to put the accept filter on.
 * @param name The accept filter
 * @param args Any extra args to the accept filter.  Passing NULL here removes
 *             the accept filter. 
 }
function apr_socket_accept_filter(sock: Papr_socket_t; name, args: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_accept_filter' + LibSuff12;

{$endif}

{
 * Return the protocol of the socket.
 * @param sock The socket to query.
 * @param protocol The returned protocol (e.g., APR_PROTO_TCP).
 }
function apr_socket_protocol_get(sock: Papr_socket_t; protocol: PInteger): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_socket_protocol_get' + LibSuff8;

{
 * Get the pool used by the socket.
 }
//APR_POOL_DECLARE_ACCESSOR(socket);

{
 * Set a socket to be inherited by child processes.
 }
//APR_DECLARE_INHERIT_SET(socket);

{
 * Unset a socket from being inherited by child processes.
 }
//APR_DECLARE_INHERIT_UNSET(socket);

{
 * @defgroup apr_mcast IP Multicast
 }

{
 * Join a Multicast Group
 * @param sock The socket to join a multicast group
 * @param join The address of the multicast group to join
 * @param iface Address of the interface to use.  If NULL is passed, the
 *              default multicast interface will be used. (OS Dependent)
 * @param source Source Address to accept transmissions from (non-NULL
 *               implies Source-Specific Multicast)
 }
function apr_mcast_join(sock: Papr_socket_t;
 join, iface, source: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_mcast_join' + LibSuff16;

{
 * Leave a Multicast Group.  All arguments must be the same as
 * apr_mcast_join.
 * @param sock The socket to leave a multicast group
 * @param addr The address of the multicast group to leave
 * @param iface Address of the interface to use.  If NULL is passed, the
 *              default multicast interface will be used. (OS Dependent)
 * @param source Source Address to accept transmissions from (non-NULL
 *               implies Source-Specific Multicast)
 }
function apr_mcast_leave(sock: Papr_socket_t;
 addr, iface, source: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_mcast_leave' + LibSuff16;

{
 * Set the Multicast Time to Live (ttl) for a multicast transmission.
 * @param sock The socket to set the multicast ttl
 * @param ttl Time to live to Assign. 0-255, default=1
 * @remark If the TTL is 0, packets will only be seen by sockets on
 * the local machine, and only when multicast loopback is enabled.
 }
function apr_mcast_hops(sock: Papr_socket_t; ttl: apr_byte_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_mcast_hops' + LibSuff8;

{
 * Toggle IP Multicast Loopback
 * @param sock The socket to set multicast loopback
 * @param opt 0=disable, 1=enable
 }
function apr_mcast_loopback(sock: Papr_socket_t; opt: apr_byte_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_mcast_loopback' + LibSuff8;

{
 * Set the Interface to be used for outgoing Multicast Transmissions.
 * @param sock The socket to set the multicast interface on
 * @param iface Address of the interface to use for Multicast
 }
function apr_mcast_interface(sock: Papr_socket_t; iface: Papr_sockaddr_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_mcast_interface' + LibSuff8;

