{ Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
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
 * httpd.h: header for simple (ha! not anymore) http daemon
 }

{ Headers in which EVERYONE has an interest... }
{$include ap_config.inc}
{$include ap_alloc.inc}
{$include buff.inc}
{$include ap.inc}

{$include util_uri.inc}

{ ----------------------------- config dir ------------------------------ }

{ Define this to be the default server home dir. Most things later in this
 * file with a relative pathname will have this added.
 }
const
  {$ifdef OS2}
    { Set default for OS/2 file system }
    HTTPD_ROOT = '/os2httpd';
  {$else}{$ifdef WINDOWS}
    { Set default for Windows file system }
    HTTPD_ROOT = '/apache';
  {$else}{$if defined(BEOS) or defined(BONE)}
    { Set the default for BeOS }
    HTTPD_ROOT = '/boot/home/apache';
  {$else}{$ifdef NETWARE}
    { Set the default for NetWare }
    HTTPD_ROOT = 'sys:/apache';
  {$else}
    HTTPD_ROOT = '/usr/local/apache';
  {$endif}
  {$endif}
  {$endif}
  {$endif}

{ Default location of documents.  Can be overridden by the DocumentRoot
 * directive.
 }
  {$ifdef OS2}
    { Set default for OS/2 file system }
    DOCUMENT_LOCATION = HTTPD_ROOT + '/docs';
  {$else}
    DOCUMENT_LOCATION = HTTPD_ROOT + '/htdocs';
  {$endif}

  { Max. number of dynamically loaded modules }
  DYNAMIC_MODULE_LIMIT = 64;

  { Default administrator's address }
  DEFAULT_ADMIN = '[no address given]';

  { The target name of the installed Apache }
  TARGET = 'httpd';

{
 * --------- You shouldn't have to edit anything below this line ----------
 *
 * Any modifications to any defaults not defined above should be done in the 
 * respective config. file. 
 *
 }


{ -- Internal representation for a HTTP protocol number, e.g., HTTP/1.1 -- }

{#define HTTP_VERSION(major,minor) (1000*(major)+(minor))
#define HTTP_VERSION_MAJOR(number) ((number)/1000)
#define HTTP_VERSION_MINOR(number) ((number)%1000)}


{ -------------- Port number for server running standalone --------------- }

  DEFAULT_HTTP_PORT =	80;
  DEFAULT_HTTPS_PORT =	443;
//#define ap_is_default_port(port,r)	((port) == ap_default_port(r))
{$ifdef NETWARE}
#define ap_http_method(r) ap_os_http_method((void*)r)
#define ap_default_port(r) ap_os_default_port((void*)r)
{$else}
//  ap_http_method(r) = 'http';
//  ap_default_port(r) = DEFAULT_HTTP_PORT;
{$endif}

{ --------- Default user name and group name running standalone ---------- }
{ --- These may be specified as numbers by placing a # before a number --- }

  DEFAULT_USER = '#-1';
  DEFAULT_GROUP = '#-1';

{$ifndef DEFAULT_ERRORLOG}
{$if defined(OS2) or defined(WINDOWS)}
  DEFAULT_ERRORLOG = 'logs/error.log';
{$else}
  DEFAULT_ERRORLOG = 'logs/error_log';
{$endif}
{$endif} { DEFAULT_ERRORLOG }

  DEFAULT_PIDLOG = 'logs/httpd.pid';

  DEFAULT_SCOREBOARD = 'logs/apache_runtime_status';

  DEFAULT_LOCKFILE = 'logs/accept.lock';

{ Define this to be what your HTML directory content files are called }

  DEFAULT_INDEX = 'index.html';

{ Define this to 1 if you want fancy indexing, 0 otherwise }

  DEFAULT_INDEXING = 0;

{ Define this to be what type you'd like returned for files with unknown }
{ suffixes.  MUST be all lower case. }

  DEFAULT_CONTENT_TYPE = 'text/plain';

{ Define this to be what your per-directory security files are called }
{$ifndef DEFAULT_ACCESS_FNAME}
{$ifdef OS2}
{ Set default for OS/2 file system }
  DEFAULT_ACCESS_FNAME = 'htaccess';
{$else}
  DEFAULT_ACCESS_FNAME = '.htaccess';
{$endif}
{$endif} { DEFAULT_ACCESS_FNAME }

{ The name of the server config file }

  SERVER_CONFIG_FILE = 'conf/httpd.conf';

{ The name of the document config file }

  RESOURCE_CONFIG_FILE = 'conf/srm.conf';

{ The name of the MIME types file }

  TYPES_CONFIG_FILE = 'conf/mime.types';

{ The name of the access file }

  ACCESS_CONFIG_FILE = 'conf/access.conf';

{ Whether we should enable rfc1413 identity checking }

  DEFAULT_RFC1413 = 0;

{ The default directory in user's home dir }

  DEFAULT_USER_DIR = 'public_html';

{ The default path for CGI scripts if none is currently set }

  DEFAULT_PATH = '/bin:/usr/bin:/usr/ucb:/usr/bsd:/usr/local/bin';

{ The path to the shell interpreter, for parsed docs }

  {$if defined(OS2) or defined(WINDOWS)}
{ Set default for OS/2 and Windows file system }
    SHELL_PATH = 'CMD.EXE';
  {$else}
    SHELL_PATH = '/bin/sh';
  {$endif}

{ The path to the suExec wrapper, can be overridden in Configuration }

  SUEXEC_BIN = HTTPD_ROOT + '/bin/suexec';

{ The default string lengths }
  HUGE_STRING_LEN = 8192;
  MAX_STRING_LEN = HUGE_STRING_LEN;

{ The timeout for waiting for messages }

  DEFAULT_TIMEOUT = 300;

{ The timeout for waiting for keepalive timeout until next request }

  DEFAULT_KEEPALIVE_TIMEOUT = 15;

{ The number of requests to entertain per connection }

  DEFAULT_KEEPALIVE = 100;

{ The size of the server's internal read-write buffers }
  IOBUFSIZE = 8192;

{ The max number of regex captures that can be expanded by ap_pregsub }
  AP_MAX_REG_MATCH = 10;

{ Number of servers to spawn off by default --- also, if fewer than
 * this free when the caretaker checks, it will spawn more.
 }

  DEFAULT_START_DAEMON = 5;

{ Maximum number of *free* server processes --- more than this, and
 * they will die off.
 }

  DEFAULT_MAX_FREE_DAEMON = 10;

{ Minimum --- fewer than this, and more will be created }

  DEFAULT_MIN_FREE_DAEMON = 5;

{ Limit on the total --- clients will be locked out if more servers than
 * this are needed.  It is intended solely to keep the server from crashing
 * when things get out of hand.
 *
 * We keep a hard maximum number of servers, for two reasons --- first off,
 * in case something goes seriously wrong, we want to stop the fork bomb
 * short of actually crashing the machine we're running on by filling some
 * kernel table.  Secondly, it keeps the size of the scoreboard file small
 * enough that we can read the whole thing without worrying too much about
 * the overhead.
 }
{$ifndef HARD_SERVER_LIMIT}
{$ifdef WINDOWS}
  HARD_SERVER_LIMIT = 1024;
{$else}{$if defined(NETWARE)}
  HARD_SERVER_LIMIT = 2048;
{$else}
  HARD_SERVER_LIMIT = 256;
{$endif}
{$endif}
{$endif}

{
 * Special Apache error codes. These are basically used
 *  in http_main.c so we can keep track of various errors.
 *
 *   APEXIT_OK:
 *     A normal exit
 *   APEXIT_INIT:
 *     A fatal error arising during the server's init sequence
 *   APEXIT_CHILDINIT:
 *     The child died during it's init sequence
 *   APEXIT_CHILDFATAL:
 *     A fatal error, resulting in the whole server aborting.
 *     If a child exits with this error, the parent process
 *     considers this a server-wide fatal error and aborts.
 *                 
 }
  APEXIT_OK	      =	$0;
  APEXIT_INIT	      =	$2;
  APEXIT_CHILDINIT    = $3;
  APEXIT_CHILDFATAL   =	$f;

{
 * (Unix, OS/2 only)
 * Interval, in microseconds, between scoreboard maintenance.  During
 * each scoreboard maintenance cycle the parent decides if it needs to
 * spawn a new child (to meet MinSpareServers requirements), or kill off
 * a child (to meet MaxSpareServers requirements).  It will only spawn or
 * kill one child per cycle.  Setting this too low will chew cpu.  The
 * default is probably sufficient for everyone.  But some people may want
 * to raise this on servers which aren't dedicated to httpd and where they
 * don't like the httpd waking up each second to see what's going on.
 }

 SCOREBOARD_MAINTENANCE_INTERVAL = 1000000;

{ Number of requests to try to handle in a single process.  If <= 0,
 * the children don't die off.  That's the default here, since I'm still
 * interested in finding and stanching leaks.
 }

  DEFAULT_MAX_REQUESTS_PER_CHILD = 0;

  DEFAULT_THREADS_PER_CHILD = 50;

  DEFAULT_EXCESS_REQUESTS_PER_CHILD = 0;

{ The maximum length of the queue of pending connections, as defined
 * by listen(2).  Under some systems, it should be increased if you
 * are experiencing a heavy TCP SYN flood attack.
 *
 * It defaults to 511 instead of 512 because some systems store it 
 * as an 8-bit datatype; 512 truncated to 8-bits is 0, while 511 is 
 * 255 when truncated.
 }

  DEFAULT_LISTENBACKLOG = 511;

{ Limits on the size of various request items.  These limits primarily
 * exist to prevent simple denial-of-service attacks on a server based
 * on misuse of the protocol.  The recommended values will depend on the
 * nature of the server resources -- CGI scripts and database backends
 * might require large values, but most servers could get by with much
 * smaller limits than we use below.  The request message body size can
 * be limited by the per-dir config directive LimitRequestBody.
 *
 * Internal buffer sizes are two bytes more than the DEFAULT_LIMIT_REQUEST_LINE
 * and DEFAULT_LIMIT_REQUEST_FIELDSIZE below, which explains the 8190.
 * These two limits can be lowered (but not raised) by the server config
 * directives LimitRequestLine and LimitRequestFieldsize, respectively.
 *
 * DEFAULT_LIMIT_REQUEST_FIELDS can be modified or disabled (set = 0) by
 * the server config directive LimitRequestFields.
 }

  { default limit on bytes in Request-Line (Method+URI+HTTP-version) }
  DEFAULT_LIMIT_REQUEST_LINE = 8190;

  { default limit on bytes in any one header field  }
  DEFAULT_LIMIT_REQUEST_FIELDSIZE = 8190;

  { default limit on number of request header fields }
  DEFAULT_LIMIT_REQUEST_FIELDS = 100;

{
 * The default default character set name to add if AddDefaultCharset is 
 * enabled.  Overridden with AddDefaultCharsetName.
 }
  DEFAULT_ADD_DEFAULT_CHARSET_NAME = 'iso-8859-1';

{
 * The below defines the base string of the Server: header. Additional
 * tokens can be added via the ap_add_version_component() API call.
 *
 * The tokens are listed in order of their significance for identifying the
 * application.
 *
 * "Product tokens should be short and to the point -- use of them for 
 * advertizing or other non-essential information is explicitly forbidden."
 *
 * Example: "Apache/1.1.0 MrWidget/0.1-alpha" 
 }

  SERVER_BASEVENDOR   = 'Apache Group';
  SERVER_BASEPRODUCT  = 'Apache';
  SERVER_BASEREVISION = '1.3.37';
  SERVER_BASEVERSION  = SERVER_BASEPRODUCT + '/' + SERVER_BASEREVISION;

  SERVER_PRODUCT  = SERVER_BASEPRODUCT;
  SERVER_REVISION = SERVER_BASEREVISION;
  SERVER_VERSION  = SERVER_PRODUCT + '/' + SERVER_REVISION;

type
  server_token_type = (
    SrvTk_MIN,		{ eg: Apache/1.3.0 }
    SrvTk_OS,		{ eg: Apache/1.3.0 (UNIX) }
    SrvTk_FULL,		{ eg: Apache/1.3.0 (UNIX) PHP/3.0 FooBar/1.2b }
    SrvTk_PRODUCT_ONLY	{ eg: Apache }
  );

//API_EXPORT(const char *) ap_get_server_version(void);
//API_EXPORT(void) ap_add_version_component(const char *component);
//API_EXPORT(const char *) ap_get_server_built(void);

{ Numeric release version identifier: MMNNFFRBB: major minor fix final beta
 * Always increases along the same track as the source branch.
 * For example, Apache 1.4.2 would be '10402100', 2.5b7 would be '20500007'.
 }
const
  APACHE_RELEASE = 10337100;

  SERVER_PROTOCOL = 'HTTP/1.1';
  SERVER_SUPPORT = 'http://www.apache.org/';

  DECLINED = -1;		{ Module declines to handle }
  DONE = -2;			{ Module has served the response completely
				 *  - it's safe to die() with no more output
                                 }
  OK = 0;			{ Module has handled this stage. }


{ ----------------------- HTTP Status Codes  ------------------------- }

{ The size of the static array in http_protocol.c for storing
 * all of the potential response status-lines (a sparse table).
 * A future version should dynamically generate the table at startup.
 }
  RESPONSE_CODES = 55;

  HTTP_CONTINUE                     = 100;
  HTTP_SWITCHING_PROTOCOLS          = 101;
  HTTP_PROCESSING                   = 102;
  HTTP_OK                           = 200;
  HTTP_CREATED                      = 201;
  HTTP_ACCEPTED                     = 202;
  HTTP_NON_AUTHORITATIVE            = 203;
  HTTP_NO_CONTENT                   = 204;
  HTTP_RESET_CONTENT                = 205;
  HTTP_PARTIAL_CONTENT              = 206;
  HTTP_MULTI_STATUS                 = 207;
  HTTP_MULTIPLE_CHOICES             = 300;
  HTTP_MOVED_PERMANENTLY            = 301;
  HTTP_MOVED_TEMPORARILY            = 302;
  HTTP_SEE_OTHER                    = 303;
  HTTP_NOT_MODIFIED                 = 304;
  HTTP_USE_PROXY                    = 305;
  HTTP_TEMPORARY_REDIRECT           = 307;
  HTTP_BAD_REQUEST                  = 400;
  HTTP_UNAUTHORIZED                 = 401;
  HTTP_PAYMENT_REQUIRED             = 402;
  HTTP_FORBIDDEN                    = 403;
  HTTP_NOT_FOUND                    = 404;
  HTTP_METHOD_NOT_ALLOWED           = 405;
  HTTP_NOT_ACCEPTABLE               = 406;
  HTTP_PROXY_AUTHENTICATION_REQUIRED= 407;
  HTTP_REQUEST_TIME_OUT             = 408;
  HTTP_CONFLICT                     = 409;
  HTTP_GONE                         = 410;
  HTTP_LENGTH_REQUIRED              = 411;
  HTTP_PRECONDITION_FAILED          = 412;
  HTTP_REQUEST_ENTITY_TOO_LARGE     = 413;
  HTTP_REQUEST_URI_TOO_LARGE        = 414;
  HTTP_UNSUPPORTED_MEDIA_TYPE       = 415;
  HTTP_RANGE_NOT_SATISFIABLE        = 416;
  HTTP_EXPECTATION_FAILED           = 417;
  HTTP_UNPROCESSABLE_ENTITY         = 422;
  HTTP_LOCKED                       = 423;
  HTTP_FAILED_DEPENDENCY            = 424;
  HTTP_INTERNAL_SERVER_ERROR        = 500;
  HTTP_NOT_IMPLEMENTED              = 501;
  HTTP_BAD_GATEWAY                  = 502;
  HTTP_SERVICE_UNAVAILABLE          = 503;
  HTTP_GATEWAY_TIME_OUT             = 504;
  HTTP_VERSION_NOT_SUPPORTED        = 505;
  HTTP_VARIANT_ALSO_VARIES          = 506;
  HTTP_INSUFFICIENT_STORAGE         = 507;
  HTTP_NOT_EXTENDED                 = 510;

  DOCUMENT_FOLLOWS   = HTTP_OK;
  PARTIAL_CONTENT    = HTTP_PARTIAL_CONTENT;
  MULTIPLE_CHOICES   = HTTP_MULTIPLE_CHOICES;
  MOVED              = HTTP_MOVED_PERMANENTLY;
  REDIRECT           = HTTP_MOVED_TEMPORARILY;
  USE_LOCAL_COPY     = HTTP_NOT_MODIFIED;
  BAD_REQUEST        = HTTP_BAD_REQUEST;
  AUTH_REQUIRED      = HTTP_UNAUTHORIZED;
  FORBIDDEN          = HTTP_FORBIDDEN;
  NOT_FOUND          = HTTP_NOT_FOUND;
  METHOD_NOT_ALLOWED = HTTP_METHOD_NOT_ALLOWED;
  NOT_ACCEPTABLE     = HTTP_NOT_ACCEPTABLE;
  LENGTH_REQUIRED    = HTTP_LENGTH_REQUIRED;
  PRECONDITION_FAILED= HTTP_PRECONDITION_FAILED;
  SERVER_ERROR       = HTTP_INTERNAL_SERVER_ERROR;
  NOT_IMPLEMENTED    = HTTP_NOT_IMPLEMENTED;
  BAD_GATEWAY        = HTTP_BAD_GATEWAY;
  VARIANT_ALSO_VARIES= HTTP_VARIANT_ALSO_VARIES;

{#define ap_is_HTTP_INFO(x)         (((x) >= 100)&&((x) < 200))
#define ap_is_HTTP_SUCCESS(x)      (((x) >= 200)&&((x) < 300))
#define ap_is_HTTP_REDIRECT(x)     (((x) >= 300)&&((x) < 400))
#define ap_is_HTTP_ERROR(x)        (((x) >= 400)&&((x) < 600))
#define ap_is_HTTP_CLIENT_ERROR(x) (((x) >= 400)&&((x) < 500))
#define ap_is_HTTP_SERVER_ERROR(x) (((x) >= 500)&&((x) < 600))

#define ap_status_drops_connection(x) \
                                   (((x) == HTTP_BAD_REQUEST)           || \
                                    ((x) == HTTP_REQUEST_TIME_OUT)      || \
                                    ((x) == HTTP_LENGTH_REQUIRED)       || \
                                    ((x) == HTTP_REQUEST_ENTITY_TOO_LARGE) || \
                                    ((x) == HTTP_REQUEST_URI_TOO_LARGE) || \
                                    ((x) == HTTP_INTERNAL_SERVER_ERROR) || \
                                    ((x) == HTTP_SERVICE_UNAVAILABLE) || \
				    ((x) == HTTP_NOT_IMPLEMENTED))}

{ Methods recognized (but not necessarily handled) by the server.
 * These constants are used in bit shifting masks of size int, so it is
 * unsafe to have more methods than bits in an int.  HEAD == M_GET.
 }
  M_GET       = 0;
  M_PUT       = 1;
  M_POST      = 2;
  M_DELETE    = 3;
  M_CONNECT   = 4;
  M_OPTIONS   = 5;
  M_TRACE     = 6;
  M_PATCH     = 7;
  M_PROPFIND  = 8;
  M_PROPPATCH = 9;
  M_MKCOL    = 10;
  M_COPY     = 11;
  M_MOVE     = 12;
  M_LOCK     = 13;
  M_UNLOCK   = 14;
  M_INVALID  = 15;

  METHODS    = 16;

  CGI_MAGIC_TYPE = 'application/x-httpd-cgi';
  INCLUDES_MAGIC_TYPE = 'text/x-server-parsed-html';
  INCLUDES_MAGIC_TYPE3 = 'text/x-server-parsed-html3';
{$ifdef CHARSET_EBCDIC}
  ASCIITEXT_MAGIC_TYPE_PREFIX = 'text/x-ascii-'; { Text files whose content-type starts with this are passed thru unconverted }
{$endif} {CHARSET_EBCDIC}
  MAP_FILE_MAGIC_TYPE = 'application/x-type-map';
  ASIS_MAGIC_TYPE = 'httpd/send-as-is';
  DIR_MAGIC_TYPE = 'httpd/unix-directory';
  STATUS_MAGIC_TYPE = 'application/x-httpd-status';

{
 * Define the HTML doctype strings centrally.
 }
  DOCTYPE_HTML_2_0  = '<!DOCTYPE HTML PUBLIC "-//IETF//' +
                          'DTD HTML 2.0//EN">' + LineEnding;
  DOCTYPE_HTML_3_2  = '<!DOCTYPE HTML PUBLIC "-//W3C//' +
                          'DTD HTML 3.2 Final//EN">' + LineEnding;
  DOCTYPE_HTML_4_0S = '<!DOCTYPE HTML PUBLIC "-//W3C//' +
                          'DTD HTML 4.0//EN"' + LineEnding +
                          '"http://www.w3.org/TR/REC-html40/strict.dtd">' + LineEnding;
  DOCTYPE_HTML_4_0T = '<!DOCTYPE HTML PUBLIC "-//W3C//' +
                          'DTD HTML 4.0 Transitional//EN"' + LineEnding +
                          '"http://www.w3.org/TR/REC-html40/loose.dtd">' + LineEnding;
  DOCTYPE_HTML_4_0F = '<!DOCTYPE HTML PUBLIC "-//W3C//' +
                          'DTD HTML 4.0 Frameset//EN"' + LineEnding +
                          '"http://www.w3.org/TR/REC-html40/frameset.dtd">' + LineEnding;

{ Just in case your linefeed isn't the one the other end is expecting. }
{$ifndef CHARSET_EBCDIC}
  LF = 10;
  CR = 13;
  CRLF = #15#12;
//#define OS_ASC(c) (c)
{$else} { CHARSET_EBCDIC }
//#include "ap_ebcdic.h"
{ OSD_POSIX uses the EBCDIC charset. The transition ASCII->EBCDIC is done in
 * the buff package (bread/bputs/bwrite), so everywhere else, we use
 * "native EBCDIC" CR and NL characters. These are therefore defined as
 * '\r' and '\n'.
 * NB: this is not the whole truth - sometimes \015 and \012 are contained
 * in literal (EBCDIC!) strings, so these are not converted but passed.
 }
  CR = '\r';
  LF = '\n';
  CRLF = '\r\n';
#define OS_ASC(c) (os_toascii[c])
{$endif} { CHARSET_EBCDIC }

{ Possible values for request_rec.read_body (set by handling module):
 *    REQUEST_NO_BODY          Send 413 error if message has any body
 *    REQUEST_CHUNKED_ERROR    Send 411 error if body without Content-Length
 *    REQUEST_CHUNKED_DECHUNK  If chunked, remove the chunks for me.
 *    REQUEST_CHUNKED_PASS     Pass the chunks to me without removal.
 }
  REQUEST_NO_BODY         = 0;
  REQUEST_CHUNKED_ERROR   = 1;
  REQUEST_CHUNKED_DECHUNK = 2;
  REQUEST_CHUNKED_PASS    = 3;
  
  { Things moved up }
  
  DEFAULT_VHOST_ADDR = $ffffffff;

{ Things which may vary per file-lookup WITHIN a request ---
 * e.g., state of MIME config.  Basically, the name of an object, info
 * about the object, and any other info we may ahve which may need to
 * change as we go poking around looking for it (e.g., overridden by
 * .htaccess files).
 *
 * Note how the default state of almost all these things is properly
 * zero, so that allocating it with pcalloc does the right thing without
 * a whole lot of hairy initialization... so long as we are willing to
 * make the (fairly) portable assumption that the bit pattern of a NULL
 * pointer is, in fact, zero.
 }

{ This represents the result of calling htaccess; these are cached for
 * each request.
 }
type
  Phtaccess_result = ^htaccess_result;
  
  htaccess_result = record
    dir: PChar;			{ the directory to which this applies }
    override_: cint;		{ the overrides allowed for the .htaccess file }
    htaccess: Pointer;		{ the configuration directives }
{ the next one, or NULL if no more; N.B. never change this }
    next: Phtaccess_result;
  end;

  Pconn_rec = ^conn_rec;
  Pserver_rec = ^server_rec;
  Prequest_rec = ^request_rec;
  Plisten_rec = ^listen_rec;

{.$include util_uri.inc}

  proxyreqtype = (
    NOT_PROXY=0,
    STD_PROXY,
    PROXY_PASS
  );

  request_rec = record

    pool: Pap_pool;
    connection: Pconn_rec;
    server: Pserver_rec;

    next: Prequest_rec;		{ If we wind up getting redirected,
				 * pointer to the request we redirected to.
                                 }
    prev: Prequest_rec;		{ If this is an internal redirect,
				 * pointer to where we redirected *from*.
                                 }

    main: Prequest_rec;		{ If this is a sub_request (see request.h)
				 * pointer back to the main request.
                                 }

    { Info about the request itself... we begin with stuff that only
     * protocol.c should ever touch...
     }

    the_request: PChar;		{ First line of request, so we can log it }
    assbackwards: cint;		{ HTTP/0.9, "simple" request }
    proxyreq: proxyreqtype;     { A proxy request (calculated during
				 * post_read_request or translate_name) }
    header_only: cint;		{ HEAD request, as opposed to GET }
    protocol: PChar;		{ Protocol, as given to us, or HTTP/0.9 }
    proto_num: cint;		{ Number version of protocol; 1.1 = 1001 }
    hostname: PChar;	{ Host, as set by full URI or Host: }

    request_time: time_t;	{ When the request started }

    status_line: PChar;	{ Status line, if set by script }
    status: cint;			{ In any case }

    { Request method, two ways; also, protocol, etc..  Outside of protocol.c,
     * look, but don't touch.
     }

    method: PChar;		{ GET, HEAD, POST, etc. }
    method_number: cint;		{ M_GET, M_POST, etc. }

    {
	allowed is a bitvector of the allowed methods.

	A handler must ensure that the request method is one that
	it is capable of handling.  Generally modules should DECLINE
	any request methods they do not handle.  Prior to aborting the
	handler like this the handler should set r->allowed to the list
	of methods that it is willing to handle.  This bitvector is used
	to construct the "Allow:" header required for OPTIONS requests,
	and METHOD_NOT_ALLOWED and NOT_IMPLEMENTED status codes.

	Since the default_handler deals with OPTIONS, all modules can
	usually decline to deal with OPTIONS.  TRACE is always allowed,
	modules don't need to set it explicitly.

	Since the default_handler will always handle a GET, a
	module which does *not* implement GET should probably return
	METHOD_NOT_ALLOWED.  Unfortunately this means that a Script GET
	handler can't be installed by mod_actions.
    }
    allowed: cint;		{ Allowed methods - for 405, OPTIONS, etc }

    sent_bodyct: cint;		{ byte count in stream is for body }
    bytes_sent: clong;		{ body byte count, for easy access }
    mtime: time_t;		{ Time the resource was last modified }

    { HTTP/1.1 connection-level features }

    chunked: cint;		{ sending chunked transfer-coding }
    byterange: cint;		{ number of byte ranges }
    boundary: PChar;		{ multipart/byteranges boundary }
    range: PChar;		{ The Range: header }
    clength: clong;		{ The "real" content length }

    remaining: clong;		{ bytes left to read }
    read_length: clong;		{ bytes that have been read }
    read_body: cint;		{ how the request body should be read }
    read_chunked: cint;		{ reading chunked transfer-coding }
    expecting_100: cuint;	{ is client waiting for a 100 response? }

    { MIME header environments, in and out.  Also, an array containing
     * environment variables to be passed to subprocesses, so people can
     * write modules to add to that environment.
     *
     * The difference between headers_out and err_headers_out is that the
     * latter are printed even on error, and persist across internal redirects
     * (so the headers printed for ErrorDocument handlers will have them).
     *
     * The 'notes' table is for notes from one module to another, with no
     * other set purpose in mind...
     }

    headers_in: PTable;
    headers_out: PTable;
    err_headers_out: PTable;
    subprocess_env: PTable;
    notes: PTable;

    { content_type, handler, content_encoding, content_language, and all
     * content_languages MUST be lowercased strings.  They may be pointers
     * to static strings; they should not be modified in place.
     }
    content_type: PChar;	{ Break these out --- we dispatch on 'em }
    handler: PChar;    	        { What we *really* dispatch on           }

    content_encoding: PChar;
    content_language: PChar;	{ for back-compat. only -- do not use }
    content_languages: Parray_header;	{ array of (char*) }

    vlist_validator: PChar;      { variant list validator (if negotiated) }

    no_cache: cint;
    no_local_copy: cint;

    { What object is being requested (either directly, or via include
     * or content-negotiation mapping).
     }

    unparsed_uri: PChar;	{ the uri without any parsing performed }
    uri: PChar;			{ the path portion of the URI }
    filename: PChar;		{ filename if found, otherwise NULL }
    path_info: PChar;
    args: PChar;		{ QUERY_ARGS, if any }
    finfo: Integer;//stat;      { ST_MODE set to zero if no such file }
    parsed_uri: uri_components;	{ components of uri, dismantled }

    { Various other config info which may change with .htaccess files
     * These are config vectors, with one void* pointer for each module
     * (the thing pointed to being the module's business).
     }

    per_dir_config: Pointer;	{ Options set in config files, etc. }
    request_config: Pointer;	{ Notes on *this* request }

{
 * a linked list of the configuration directives in the .htaccess files
 * accessed by this request.
 * N.B. always add to the head of the list, _never_ to the end.
 * that way, a sub request's list can (temporarily) point to a parent's list
 }
    htaccess: Phtaccess_result;

    { On systems with case insensitive file systems (Windows, OS/2, etc.),
     * r->filename is case canonicalized (folded to either lower or upper 
     * case, depending on the specific system) to accomodate file access
     * checking. case_preserved_filename is the same as r->filename 
     * except case is preserved. There is at least one instance where Apache 
     * needs access to the case preserved filename: Java class files published 
     * with WebDAV need to preserve filename case to make the Java compiler 
     * happy.
     }
    case_preserved_filename: PChar;

{$ifdef CHARSET_EBCDIC}
    { We don't want subrequests to modify our current conversion flags.
     * These flags save the state of the conversion flags when subrequests
     * are run.
     }
    struct {
        unsigned conv_in:1;    { convert ASCII->EBCDIC when read()ing? }
        unsigned conv_out:1;   { convert EBCDIC->ASCII when write()ing? }
    } ebcdic;
{$endif}

{ Things placed at the end of the record to avoid breaking binary
 * compatibility.  It would be nice to remember to reorder the entire
 * record to improve 64bit alignment the next time we need to break
 * binary compatibility for some other reason.
 }
  end;


{ Things which are per connection
 }

  sockaddr_in = record end;
 
  conn_rec = record

    pool: Pap_pool;
    server: Pserver_rec;
    base_server: Pserver_rec;	{ Physical vhost this conn come in on }
    vhost_lookup_data: Pointer;	{ used by http_vhost.c }

    { Information about the connection itself }

    child_num: cint;		{ The number of the child handling conn_rec }
    client: PBUFF;		{ Connection to the guy }

    { Who is the client? }

    local_addr: sockaddr_in;	{ local address }
    remote_addr: sockaddr_in;	{ remote address }
    remote_ip: PChar;		{ Client's IP address }
    remote_host: PChar;		{ Client's DNS name, if known.
				 * NULL if DNS hasn't been checked,
				 * "" if it has and no address was found.
				 * N.B. Only access this though
				 * get_remote_host() }
    remote_logname: PChar;	{ Only ever set if doing rfc1413 lookups.
				 * N.B. Only access this through
				 * get_remote_logname() }
    user: PChar;			{ If an authentication check was made,
				 * this gets set to the user name.  We assume
				 * that there's only one user per connection(!)
                                 }
    ap_auth_type: PChar;		{ Ditto. }

//    unsigned aborted:1;		{ Are we still talking? }
//    signed int keepalive:2;	{ Are we using HTTP Keep-Alive?
//				 * -1 fatal error, 0 undecided, 1 yes }
//    unsigned keptalive:1;	{ Did we use HTTP Keep-Alive? }
//    signed int double_reverse:2;{ have we done double-reverse DNS?
//				 * -1 yes/failure, 0 not yet, 1 yes/success }
    keepalives: cint;		{ How many times have we used it? }
    local_ip: PChar;		{ server IP address }
    local_host: PChar;		{ used for ap_get_server_name when
				 * UseCanonicalName is set to DNS
				 * (ignores setting of HostnameLookups) }
  end;

{ Per-vhost config... }

{ The address 255.255.255.255, when used as a virtualhost address,
 * will become the "default" server when the ip doesn't match other vhosts.
 }

  { Moved up }

  Pserver_addr_rec = ^server_addr_rec;
  
  in_addr = record end;

  server_addr_rec = record
    next: Pserver_addr_rec;
    host_addr: in_addr; 	{ The bound address, for this server }
    host_port: cushort; 	{ The bound port, for this server }
    virthost: PChar;		{ The name given in <VirtualHost> }
  end;

  server_rec = record

    next: Pserver_rec;

    { description of where the definition came from }
    defn_name: PChar;
    defn_line_number: cuint;

    { Full locations of server config info }

    srm_confname: PChar;
    access_confname: PChar;

    { Contact information }

    server_admin: PChar;
    server_hostname: PChar;
    port: cushort;	{ for redirects, etc. }

    { Log files --- note that transfer log is now in the modules... }

    error_fname: PChar;
    error_log: Pointer; //FILE *
    loglevel: cint;

    { Module-specific configuration for server, and defaults... }

    is_virtual: cint;		{ true if this is the virtual server }
    module_config: Pointer;	{ Config vector containing pointers to
				 * modules' per-server config structures.
                                 }
    lookup_defaults: Pointer;	{ MIME type info, etc., before we start
				 * checking per-directory info.
                                 }
    { Transaction handling }

    addrs: Pserver_addr_rec;
    timeout: cint;		{ Timeout, in seconds, before we give up }
    keep_alive_timeout: cint;	{ Seconds we'll wait for another request }
    keep_alive_max: cint;	{ Maximum requests per connection }
    keep_alive: cint;		{ Use persistent connections? }
    send_buffer_size: cint;	{ size of TCP send buffer (in bytes) }

    path: PChar;		{ Pathname for ServerPath }
    pathlen: cint;		{ Length of path }

    names: Parray_header;	{ Normal names for ServerAlias servers }
    wild_names: Parray_header;	{ Wildcarded names for ServerAlias servers }

    server_uid: uid_t;        { effective user id when calling exec wrapper }
    server_gid: gid_t;        { effective group id when calling exec wrapper }

    limit_req_line: cint;      { limit on size of the HTTP request line    }
    limit_req_fieldsize: cint; { limit on size of any request header field }
    limit_req_fields: cint;    { limit on number of request header fields  }
  end;

  { These are more like real hosts than virtual hosts }

  listen_rec = record
    next: Plisten_rec;
    local_addr: sockaddr_in;	{ local IP address and port }
    fd: cint;
    used: cint;			{ Only used during restart }
    { more stuff here, like which protocol is bound to the port }
  end;

  tm = record end;
  
  Ptm = ^tm;
  
{ Prototypes for utilities... util.c.
 }

//extern void ap_util_init(void);

{ Time }
{extern API_VAR_EXPORT const char ap_month_snames[12][4];
extern API_VAR_EXPORT const char ap_day_snames[7][4];}

function ap_get_gmtoff(tz: Pcint): Ptm;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_time: PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_field_noparam(p: Ppool; const intype: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_ht_time(p: Ppool; t: time_t; const fmt: PChar; gmt: cint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_gm_timestr_822(p: Ppool; t: time_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ String handling. The *_nc variants allow you to use non-const char **s as
   arguments (unfortunately C won't automatically convert a char ** to a const
   char **) }

function ap_getword(p: Ppool; const line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_nc(p: Ppool; line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_white(p: Ppool; const line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_white_nc(p: Ppool; line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_nulls(p: Ppool; const line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_nulls_nc(p: Ppool; line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_conf(p: Ppool; const line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getword_conf_nc(p: Ppool; line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;


function ap_size_list_item(const field: PPChar; len: Pcint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_list_item(p: Ppool; const field: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_find_list_item(p: Ppool; const line, tok: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;


function ap_get_token(p: Ppool; const accept_line: PPChar; accept_white: cint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_find_token(p: Ppool; const line, tok: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_find_last_token(p: Ppool; const line, tok: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;


function ap_unescape_url(url: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_no2slash(name: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_getparents(name: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_escape_path_segment(p: Ppool; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_os_escape_path(p: Ppool; const path: PChar; partial: cint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

//#define ap_escape_uri(ppool,path) ap_os_escape_path(ppool,path,1)

function ap_escape_html(p: Ppool; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{API_EXPORT(char *) ap_construct_server(pool *p, const char *hostname,
				    unsigned port, const request_rec *r);
API_EXPORT(char *) ap_escape_logitem(pool *p, const char *str);
API_EXPORT(size_t) ap_escape_errorlog_item(char *dest, const char *source,
                                           size_t buflen);
API_EXPORT(char *) ap_escape_shell_cmd(pool *p, const char *s);

API_EXPORT(int) ap_count_dirs(const char *path);
API_EXPORT(char *) ap_make_dirstr_prefix(char *d, const char *s, int n);
API_EXPORT(char *) ap_make_dirstr_parent(pool *p, const char *s);}
{ deprecated.  The previous two routines are preferred. }
{API_EXPORT(char *) ap_make_dirstr(pool *a, const char *s, int n);
API_EXPORT(char *) ap_make_full_path(pool *a, const char *dir, const char *f);

API_EXPORT(int) ap_is_matchexp(const char *str);
API_EXPORT(int) ap_strcmp_match(const char *str, const char *exp);
API_EXPORT(int) ap_strcasecmp_match(const char *str, const char *exp);
API_EXPORT(char *) ap_stripprefix(const char *bigstring, const char *prefix);
API_EXPORT(char *) ap_strcasestr(const char *s1, const char *s2);
API_EXPORT(char *) ap_pbase64decode(pool *p, const char *bufcoded);
API_EXPORT(char *) ap_pbase64encode(pool *p, char *string); 
API_EXPORT(char *) ap_uudecode(pool *p, const char *bufcoded);
API_EXPORT(char *) ap_uuencode(pool *p, char *string); }

{$if defined(OS2) or defined(WINDOWS)}
//API_EXPORT(char *) ap_double_quotes(pool *p, const char *str);
//API_EXPORT(char *) ap_caret_escape_args(pool *p, const char *str);
{$endif}

{$ifdef OS2}
void os2pathname(char *path);
{$endif}

{API_EXPORT(int)    ap_regexec(const regex_t *preg, const char *string,
                              size_t nmatch, regmatch_t pmatch[], int eflags);
API_EXPORT(size_t) ap_regerror(int errcode, const regex_t *preg, 
                               char *errbuf, size_t errbuf_size);
API_EXPORT(char *) ap_pregsub(pool *p, const char *input, const char *source,
                              size_t nmatch, regmatch_t pmatch[]);

API_EXPORT(void) ap_content_type_tolower(char *);
API_EXPORT(void) ap_str_tolower(char *);
API_EXPORT(int) ap_ind(const char *, char);}	{ Sigh... }
{API_EXPORT(int) ap_rind(const char *, char);

API_EXPORT(char *) ap_escape_quotes (pool *p, const char *instring);
API_EXPORT(void) ap_remove_spaces(char *dest, char *src);}

{ Common structure for reading of config files / passwd files etc. }
type

  Pconfigfile_t = ^configfile_t;
  
  getch_t = function (param: Pointer): cint;
  
  getstr_t = function (buf: Pointer; bufsiz: size_t; param: Pointer): Pointer;
  
  close_t = function (param: Pointer): cint;
  
  configfile_t = record
    getch: getch_t;	{ a getc()-like function }
    getstr: getstr_t;   { a fgets()-like function }
    close: close_t;	{ a close hander function }
    param: Pointer;	{ the argument passed to getch/getstr/close }
    name: PChar;	{ the filename / description }
    line_number: cuint;	{ current line number, starting at 1 }
  end;

{ Open a configfile_t as FILE, return open configfile_t struct pointer }
//API_EXPORT(configfile_t *) ap_pcfg_openfile(pool *p, const char *name);

{ Allocate a configfile_t handle with user defined functions and params }
//API_EXPORT(configfile_t *) ap_pcfg_open_custom(pool *p, const char *descr,
//    void *param,
//    int( *getc_func)(void*),
//    void *( *gets_func) (void *buf, size_t bufsiz, void *param),
//    int( *close_func)(void *param));

{ Read one line from open configfile_t, strip LF, increase line number }
//API_EXPORT(int) ap_cfg_getline(char *buf, size_t bufsize, configfile_t *cfp);

{ Read one char from open configfile_t, increase line number upon LF }
//API_EXPORT(int) ap_cfg_getc(configfile_t *cfp);

{ Detach from open configfile_t, calling the close handler }
//API_EXPORT(int) ap_cfg_closefile(configfile_t *cfp);

{$ifdef NEED_STRERROR}
//char *strerror(int err);
{$endif}

{ Misc system hackery }

function ap_uname2id(const name: PChar): uid_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_gname2id(const name: PChar): gid_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_is_directory(const name: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_is_rdirectory(const name: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_can_exec(const stat: Pointer): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_chdir_file(const file_: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{$ifndef HAVE_CANONICAL_FILENAME}
{
 *  We can't define these in os.h because of dependence on pool pointer.
 }
{#define ap_os_canonical_filename(p,f)  (f)
#define ap_os_case_canonical_filename(p,f)  (f)
#define ap_os_systemcase_filename(p,f)  (f)}
{$else}
//API_EXPORT(char *) ap_os_canonical_filename(pool *p, const char *file);
{$ifdef WINDOWS}
//API_EXPORT(char *) ap_os_case_canonical_filename(pool *pPool, const char *szFile);
//API_EXPORT(char *) ap_os_systemcase_filename(pool *pPool, const char *szFile);
{$else}{$ifdef OS2}
//API_EXPORT(char *) ap_os_case_canonical_filename(pool *pPool, const char *szFile);
//API_EXPORT(char *) ap_os_systemcase_filename(pool *pPool, const char *szFile);
{$else}{$ifdef NETWARE}
//API_EXPORT(char *) ap_os_case_canonical_filename(pool *pPool, const char *szFile);
//#define ap_os_systemcase_filename(p,f) ap_os_case_canonical_filename(p,f)
{$else}
//#define ap_os_case_canonical_filename(p,f) ap_os_canonical_filename(p,f)
//#define ap_os_systemcase_filename(p,f) ap_os_canonical_filename(p,f)
{$endif}
{$endif}
{$endif}
{$endif}

{$ifdef CHARSET_EBCDIC}
//API_EXPORT(int)    ap_checkconv(struct request_rec *r);    { for downloads }
//API_EXPORT(int)    ap_checkconv_in(struct request_rec *r); { for uploads }
{$endif} {#ifdef CHARSET_EBCDIC}

{API_EXPORT(char *) ap_get_local_host(pool *);
API_EXPORT(unsigned long) ap_get_virthost_addr(char *hostname, unsigned short *port);

extern API_VAR_EXPORT time_t ap_restart_time;}

{
 * Apache tries to keep all of its long term filehandles (such as log files,
 * and sockets) above this number.  This is to workaround problems in many
 * third party libraries that are compiled with a small FD_SETSIZE.  There
 * should be no reason to lower this, because it's only advisory.  If a file
 * can't be allocated above this number then it will remain in the "slack"
 * area.
 *
 * Only the low slack line is used by default.  If HIGH_SLACK_LINE is defined
 * then an attempt is also made to keep all non-FILE * files above the high
 * slack line.  This is to work around a Solaris C library limitation, where it
 * uses an unsigned char to store the file descriptor.
 }

const
  LOW_SLACK_LINE  = 15;

  HIGH_SLACK_LINE = 255;

{
 * The ap_slack() function takes a fd, and tries to move it above the indicated
 * line.  It returns an fd which may or may not have moved above the line, and
 * never fails.  If the high line was requested and it fails it will also try
 * the low line.
 }
{#ifdef NO_SLACK
#define ap_slack(fd,line)   (fd)
#else
int ap_slack(int fd, int line);
const
  AP_SLACK_LOW	= 1;
  AP_SLACK_HIGH	= 2;
#endif

API_EXPORT(char *) ap_escape_quotes(pool *p, const char *instr);}

{
 * Redefine assert() to something more useful for an Apache...
 }
{API_EXPORT(void) ap_log_assert(const char *szExp, const char *szFile, int nLine)
			    __attribute__((noreturn));
#define ap_assert(exp) ((exp) ? (void)0 : ap_log_assert(#exp,__FILE__,__LINE__))}

{ The optimized timeout code only works if we're not MULTITHREAD and we're
 * also not using a scoreboard file
 }
{#if !defined (MULTITHREAD) && (defined (USE_MMAP_SCOREBOARD) || defined (USE_SHMGET_SCOREBOARD))
#define OPTIMIZE_TIMEOUTS
#endif}

{ A set of flags which indicate places where the server should raise(SIGSTOP).
 * This is useful for debugging, because you can then attach to that process
 * with gdb and continue.  This is important in cases where one_process
 * debugging isn't possible.
 }
  SIGSTOP_DETACH	  = 1;
  SIGSTOP_MAKE_CHILD	  = 2;
  SIGSTOP_SPAWN_CHILD  	  = 4;
  SIGSTOP_PIPED_LOG_SPAWN = 8;
  SIGSTOP_CGI_CHILD	  = 16;

{#ifdef DEBUG_SIGSTOP
extern int raise_sigstop_flags;
#define RAISE_SIGSTOP(x)	do begin
	if (raise_sigstop_flags & SIGSTOP_##x) raise(SIGSTOP);\
    end while (0)
#else
#define RAISE_SIGSTOP(x)
#endif

API_EXPORT(extern const char *) ap_psignature(const char *prefix, request_rec *r);}

  { strtoul does not exist on sunos4. }

//  strtoul = strtoul_is_not_a_portable_function_use_strtol_instead

{$ifdef AP_ENABLE_EXCEPTION_HOOK}
{ The exception hook allows a module to run from the server's signal
 * handler, and perform tasks such as logging the current request or
 * getting a backtrace or performing other diagnostic functions.  All
 * operating system requirements for running in a signal handler must
 * be respected, or the process may not exit properly.
 *
 * AP_ENABLE_EXCEPTION_HOOK is already defined for platforms that have
 * been tested.  It likely will work on other platforms.  In order to
 * test, define AP_ENABLE_EXCEPTION_HOOK at configure time.
 }
type
  ap_exception_info_t = record
    sig: cint;
    pid: pid_t;
  end;
  
  Pap_exception_info_t = ^ap_exception_info_t;

{ Register a function to be called after a fatal exception (on *X systems, a
 * "synchronous signal" such as SIGSEGV, SIGILL, etc.).
 *
 * Returns 0 on success, non-zero on failure.
 * If EnableExceptionHook directive is not set to "on", this function will
 * report failure and no such hooks will be called.
 }
 fn_afeh_t = procedure (param: Pap_exception_info_t);
 
function ap_add_fatal_exception_hook(fn: fn_afeh_t): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{$endif} { AP_ENABLE_EXCEPTION_HOOK }

