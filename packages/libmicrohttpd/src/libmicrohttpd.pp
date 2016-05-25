{
    This file is part of the Free Pascal packages
    Copyright (c) 2015 by Silvio Clecio, Gilson Nunes and Joao Morais

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

   The GNU libmicrohttpd library is governed by GNU copyright, see the
   GNU libmicrohttpd website for this:

   https://www.gnu.org/software/libmicrohttpd/
}

unit libmicrohttpd;

interface

uses
{$IFDEF MSWINDOWS}
  WinSock2,
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF UNIX}BaseUnix, Unix,{$ENDIF}CTypes, Sockets
{$ELSE}
  WinTypes
{$ENDIF};

const
  MHD_LIB_NAME = {$IFDEF MSWINDOWS}'libmicrohttpd-10'{$ELSE}'microhttpd'{$ENDIF};

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

{$IFDEF UNIX}
  FD_SETSIZE = 64;
{$ENDIF}
{$IFDEF UINT64_MAX}
  MHD_SIZE_UNKNOWN = UINT64_MAX;
{$ELSE}
  MHD_SIZE_UNKNOWN = -1;
{$ENDIF}
{$IFDEF SIZE_MAX}
  MHD_CONTENT_READER_END_OF_STREAM = SIZE_MAX;
  MHD_CONTENT_READER_END_WITH_ERROR = SIZE_MAX - 1;
{$ELSE}
  MHD_CONTENT_READER_END_OF_STREAM = -1;
  MHD_CONTENT_READER_END_WITH_ERROR = -2;
{$ENDIF}
{$IFNDEF MHD_SOCKET_DEFINED}
  {$IF NOT DEFINED(WIN32) OR DEFINED(_SYS_TYPES_FD_SET)}
  MHD_POSIX_SOCKETS = 1;
  MHD_INVALID_SOCKET = -1;
  {$ELSE}
  MHD_WINSOCK_SOCKETS = 1;
  MHD_INVALID_SOCKET = INVALID_SOCKET;
  {$ENDIF}
  MHD_SOCKET_DEFINED = 1;
{$ENDIF}
  MHD_VERSION = $00094601;
  MHD_YES = 1;
  MHD_NO = 0;
  MHD_INVALID_NONCE = -1;
  MHD_ICY_FLAG = -2147483648;
  MHD_LONG_LONG_PRINTF = 'll';
  MHD_UNSIGNED_LONG_LONG_PRINTF = '%llu';

  // HTTP codes
  MHD_HTTP_CONTINUE = 100;
  MHD_HTTP_SWITCHING_PROTOCOLS = 101;
  MHD_HTTP_PROCESSING = 102;
  MHD_HTTP_OK = 200;
  MHD_HTTP_CREATED = 201;
  MHD_HTTP_ACCEPTED = 202;
  MHD_HTTP_NON_AUTHORITATIVE_INFORMATION = 203;
  MHD_HTTP_NO_CONTENT = 204;
  MHD_HTTP_RESET_CONTENT = 205;
  MHD_HTTP_PARTIAL_CONTENT = 206;
  MHD_HTTP_MULTI_STATUS = 207;
  MHD_HTTP_MULTIPLE_CHOICES = 300;
  MHD_HTTP_MOVED_PERMANENTLY = 301;
  MHD_HTTP_FOUND = 302;
  MHD_HTTP_SEE_OTHER = 303;
  MHD_HTTP_NOT_MODIFIED = 304;
  MHD_HTTP_USE_PROXY = 305;
  MHD_HTTP_SWITCH_PROXY = 306;
  MHD_HTTP_TEMPORARY_REDIRECT = 307;
  MHD_HTTP_BAD_REQUEST = 400;
  MHD_HTTP_UNAUTHORIZED = 401;
  MHD_HTTP_PAYMENT_REQUIRED = 402;
  MHD_HTTP_FORBIDDEN = 403;
  MHD_HTTP_NOT_FOUND = 404;
  MHD_HTTP_METHOD_NOT_ALLOWED = 405;
  MHD_HTTP_NOT_ACCEPTABLE = 406;
  MHD_HTTP_PROXY_AUTHENTICATION_REQUIRED = 407;
  MHD_HTTP_REQUEST_TIMEOUT = 408;
  MHD_HTTP_CONFLICT = 409;
  MHD_HTTP_GONE = 410;
  MHD_HTTP_LENGTH_REQUIRED = 411;
  MHD_HTTP_PRECONDITION_FAILED = 412;
  MHD_HTTP_REQUEST_ENTITY_TOO_LARGE = 413;
  MHD_HTTP_REQUEST_URI_TOO_LONG = 414;
  MHD_HTTP_UNSUPPORTED_MEDIA_TYPE = 415;
  MHD_HTTP_REQUESTED_RANGE_NOT_SATISFIABLE = 416;
  MHD_HTTP_EXPECTATION_FAILED = 417;
  MHD_HTTP_UNPROCESSABLE_ENTITY = 422;
  MHD_HTTP_LOCKED = 423;
  MHD_HTTP_FAILED_DEPENDENCY = 424;
  MHD_HTTP_UNORDERED_COLLECTION = 425;
  MHD_HTTP_UPGRADE_REQUIRED = 426;
  MHD_HTTP_NO_RESPONSE = 444;
  MHD_HTTP_RETRY_WITH = 449;
  MHD_HTTP_BLOCKED_BY_WINDOWS_PARENTAL_CONTROLS = 450;
  MHD_HTTP_UNAVAILABLE_FOR_LEGAL_REASONS = 451;
  MHD_HTTP_INTERNAL_SERVER_ERROR = 500;
  MHD_HTTP_NOT_IMPLEMENTED = 501;
  MHD_HTTP_BAD_GATEWAY = 502;
  MHD_HTTP_SERVICE_UNAVAILABLE = 503;
  MHD_HTTP_GATEWAY_TIMEOUT = 504;
  MHD_HTTP_HTTP_VERSION_NOT_SUPPORTED = 505;
  MHD_HTTP_VARIANT_ALSO_NEGOTIATES = 506;
  MHD_HTTP_INSUFFICIENT_STORAGE = 507;
  MHD_HTTP_BANDWIDTH_LIMIT_EXCEEDED = 509;
  MHD_HTTP_NOT_EXTENDED = 510;

  // HTTP headers
  MHD_HTTP_HEADER_ACCEPT = 'Accept';
  MHD_HTTP_HEADER_ACCEPT_CHARSET = 'Accept-Charset';
  MHD_HTTP_HEADER_ACCEPT_ENCODING = 'Accept-Encoding';
  MHD_HTTP_HEADER_ACCEPT_LANGUAGE = 'Accept-Language';
  MHD_HTTP_HEADER_ACCEPT_RANGES = 'Accept-Ranges';
  MHD_HTTP_HEADER_AGE = 'Age';
  MHD_HTTP_HEADER_ALLOW = 'Allow';
  MHD_HTTP_HEADER_AUTHORIZATION = 'Authorization';
  MHD_HTTP_HEADER_CACHE_CONTROL = 'Cache-Control';
  MHD_HTTP_HEADER_CONNECTION = 'Connection';
  MHD_HTTP_HEADER_CONTENT_ENCODING = 'Content-Encoding';
  MHD_HTTP_HEADER_CONTENT_LANGUAGE = 'Content-Language';
  MHD_HTTP_HEADER_CONTENT_LENGTH = 'Content-Length';
  MHD_HTTP_HEADER_CONTENT_LOCATION = 'Content-Location';
  MHD_HTTP_HEADER_CONTENT_MD5 = 'Content-MD5';
  MHD_HTTP_HEADER_CONTENT_RANGE = 'Content-Range';
  MHD_HTTP_HEADER_CONTENT_TYPE = 'Content-Type';
  MHD_HTTP_HEADER_COOKIE = 'Cookie';
  MHD_HTTP_HEADER_DATE = 'Date';
  MHD_HTTP_HEADER_ETAG = 'ETag';
  MHD_HTTP_HEADER_EXPECT = 'Expect';
  MHD_HTTP_HEADER_EXPIRES = 'Expires';
  MHD_HTTP_HEADER_FROM = 'From';
  MHD_HTTP_HEADER_HOST = 'Host';
  MHD_HTTP_HEADER_IF_MATCH = 'If-Match';
  MHD_HTTP_HEADER_IF_MODIFIED_SINCE = 'If-Modified-Since';
  MHD_HTTP_HEADER_IF_NONE_MATCH = 'If-None-Match';
  MHD_HTTP_HEADER_IF_RANGE = 'If-Range';
  MHD_HTTP_HEADER_IF_UNMODIFIED_SINCE = 'If-Unmodified-Since';
  MHD_HTTP_HEADER_LAST_MODIFIED = 'Last-Modified';
  MHD_HTTP_HEADER_LOCATION = 'Location';
  MHD_HTTP_HEADER_MAX_FORWARDS = 'Max-Forwards';
  MHD_HTTP_HEADER_PRAGMA = 'Pragma';
  MHD_HTTP_HEADER_PROXY_AUTHENTICATE = 'Proxy-Authenticate';
  MHD_HTTP_HEADER_PROXY_AUTHORIZATION = 'Proxy-Authorization';
  MHD_HTTP_HEADER_RANGE = 'Range';
  MHD_HTTP_HEADER_REFERER = 'Referer';
  MHD_HTTP_HEADER_RETRY_AFTER = 'Retry-After';
  MHD_HTTP_HEADER_SERVER = 'Server';
  MHD_HTTP_HEADER_SET_COOKIE = 'Set-Cookie';
  MHD_HTTP_HEADER_SET_COOKIE2 = 'Set-Cookie2';
  MHD_HTTP_HEADER_TE = 'TE';
  MHD_HTTP_HEADER_TRAILER = 'Trailer';
  MHD_HTTP_HEADER_TRANSFER_ENCODING = 'Transfer-Encoding';
  MHD_HTTP_HEADER_UPGRADE = 'Upgrade';
  MHD_HTTP_HEADER_USER_AGENT = 'User-Agent';
  MHD_HTTP_HEADER_VARY = 'Vary';
  MHD_HTTP_HEADER_VIA = 'Via';
  MHD_HTTP_HEADER_WARNING = 'Warning';
  MHD_HTTP_HEADER_WWW_AUTHENTICATE = 'WWW-Authenticate';
  MHD_HTTP_HEADER_ACCESS_CONTROL_ALLOW_ORIGIN = 'Access-Control-Allow-Origin';
  MHD_HTTP_HEADER_CONTENT_DISPOSITION = 'Content-Disposition';
  MHD_HTTP_VERSION_1_0 = 'HTTP/1.0';
  MHD_HTTP_VERSION_1_1 = 'HTTP/1.1';
  MHD_HTTP_METHOD_CONNECT = 'CONNECT';
  MHD_HTTP_METHOD_DELETE = 'DELETE';
  MHD_HTTP_METHOD_GET = 'GET';
  MHD_HTTP_METHOD_HEAD = 'HEAD';
  MHD_HTTP_METHOD_OPTIONS = 'OPTIONS';
  MHD_HTTP_METHOD_POST = 'POST';
  MHD_HTTP_METHOD_PUT = 'PUT';
  MHD_HTTP_METHOD_PATCH = 'PATCH';
  MHD_HTTP_METHOD_TRACE = 'TRACE';
  MHD_HTTP_POST_ENCODING_FORM_URLENCODED = 'application/x-www-form-urlencoded';
  MHD_HTTP_POST_ENCODING_MULTIPART_FORMDATA = 'multipart/form-data';

  // MHD_ValueKind enum items
  MHD_RESPONSE_HEADER_KIND = 0;
  MHD_HEADER_KIND = 1;
  MHD_COOKIE_KIND = 2;
  MHD_POSTDATA_KIND = 4;
  MHD_GET_ARGUMENT_KIND = 8;
  MHD_FOOTER_KIND = 16;

  // MHD_FLAG enum items
  MHD_NO_FLAG = 0;
  MHD_USE_DEBUG = 1;
  MHD_USE_SSL = 2;
  MHD_USE_THREAD_PER_CONNECTION = 4;
  MHD_USE_SELECT_INTERNALLY = 8;
  MHD_USE_IPv6 = 16;
  MHD_USE_PEDANTIC_CHECKS = 32;
  MHD_USE_POLL = 64;
  MHD_USE_POLL_INTERNALLY = MHD_USE_SELECT_INTERNALLY or MHD_USE_POLL;
  MHD_SUPPRESS_DATE_NO_CLOCK = 128;
  MHD_USE_NO_LISTEN_SOCKET = 256;
  MHD_USE_EPOLL_LINUX_ONLY = 512;
  MHD_USE_EPOLL_INTERNALLY_LINUX_ONLY = MHD_USE_SELECT_INTERNALLY or MHD_USE_EPOLL_LINUX_ONLY;
  MHD_USE_PIPE_FOR_SHUTDOWN = 1024;
  MHD_USE_DUAL_STACK = MHD_USE_IPv6 or 2048;
  MHD_USE_EPOLL_TURBO = 4096;
  MHD_USE_SUSPEND_RESUME = 8192 or MHD_USE_PIPE_FOR_SHUTDOWN;
  MHD_USE_TCP_FASTOPEN = 16384;

  // MHD_OPTION enum items
  MHD_OPTION_END = 0;
  MHD_OPTION_CONNECTION_MEMORY_LIMIT = 1;
  MHD_OPTION_CONNECTION_LIMIT = 2;
  MHD_OPTION_CONNECTION_TIMEOUT = 3;
  MHD_OPTION_NOTIFY_COMPLETED = 4;
  MHD_OPTION_PER_IP_CONNECTION_LIMIT = 5;
  MHD_OPTION_SOCK_ADDR = 6;
  MHD_OPTION_URI_LOG_CALLBACK = 7;
  MHD_OPTION_HTTPS_MEM_KEY = 8;
  MHD_OPTION_HTTPS_MEM_CERT = 9;
  MHD_OPTION_HTTPS_CRED_TYPE = 10;
  MHD_OPTION_HTTPS_PRIORITIES = 11;
  MHD_OPTION_LISTEN_SOCKET = 12;
  MHD_OPTION_EXTERNAL_LOGGER = 13;
  MHD_OPTION_THREAD_POOL_SIZE = 14;
  MHD_OPTION_ARRAY = 15;
  MHD_OPTION_UNESCAPE_CALLBACK = 16;
  MHD_OPTION_DIGEST_AUTH_RANDOM = 17;
  MHD_OPTION_NONCE_NC_SIZE = 18;
  MHD_OPTION_THREAD_STACK_SIZE = 19;
  MHD_OPTION_HTTPS_MEM_TRUST = 20;
  MHD_OPTION_CONNECTION_MEMORY_INCREMENT = 21;
  MHD_OPTION_HTTPS_CERT_CALLBACK = 22;
  MHD_OPTION_TCP_FASTOPEN_QUEUE_SIZE = 23;
  MHD_OPTION_HTTPS_MEM_DHPARAMS = 24;
  MHD_OPTION_LISTENING_ADDRESS_REUSE = 25;
  MHD_OPTION_HTTPS_KEY_PASSWORD = 26;
  MHD_OPTION_NOTIFY_CONNECTION = 27;

  // MHD_RequestTerminationCode enum items
  MHD_REQUEST_TERMINATED_COMPLETED_OK = 0;
  MHD_REQUEST_TERMINATED_WITH_ERROR = 1;
  MHD_REQUEST_TERMINATED_TIMEOUT_REACHED = 2;
  MHD_REQUEST_TERMINATED_DAEMON_SHUTDOWN = 3;
  MHD_REQUEST_TERMINATED_READ_ERROR = 4;
  MHD_REQUEST_TERMINATED_CLIENT_ABORT = 5;

  // MHD_ConnectionNotificationCode enum items
  MHD_CONNECTION_NOTIFY_STARTED = 0;
  MHD_CONNECTION_NOTIFY_CLOSED = 1;

  // MHD_ConnectionInfoType enum items
  MHD_CONNECTION_INFO_CIPHER_ALGO = 0;
  MHD_CONNECTION_INFO_PROTOCOL = 1;
  MHD_CONNECTION_INFO_CLIENT_ADDRESS = 2;
  MHD_CONNECTION_INFO_GNUTLS_SESSION = 3;
  MHD_CONNECTION_INFO_GNUTLS_CLIENT_CERT = 4;
  MHD_CONNECTION_INFO_DAEMON = 5;
  MHD_CONNECTION_INFO_CONNECTION_FD = 6;
  MHD_CONNECTION_INFO_SOCKET_CONTEXT = 7;

  // MHD_DaemonInfoType enum items
  MHD_DAEMON_INFO_KEY_SIZE = 0;
  MHD_DAEMON_INFO_MAC_KEY_SIZE = 1;
  MHD_DAEMON_INFO_LISTEN_FD = 2;
  MHD_DAEMON_INFO_EPOLL_FD_LINUX_ONLY = 3;
  MHD_DAEMON_INFO_CURRENT_CONNECTIONS = 4;

  // MHD_ResponseFlags enum items
  MHD_RF_NONE = 0;
  MHD_RF_HTTP_VERSION_1_0_ONLY = 1;

  // MHD_ResponseOptions enum items
  MHD_RO_END = 0;

  // MHD_ResponseMemoryMode enum items
  MHD_RESPMEM_PERSISTENT = 0;
  MHD_RESPMEM_MUST_FREE = 1;
  MHD_RESPMEM_MUST_COPY = 2;

  // MHD_CONNECTION_OPTION enum items
  MHD_CONNECTION_OPTION_TIMEOUT = 0;

  // MHD_FEATURE enum items
  MHD_FEATURE_MESSGES = 1;
  MHD_FEATURE_SSL = 2;
  MHD_FEATURE_HTTPS_CERT_CALLBACK = 3;
  MHD_FEATURE_IPv6 = 4;
  MHD_FEATURE_IPv6_ONLY = 5;
  MHD_FEATURE_POLL = 6;
  MHD_FEATURE_EPOLL = 7;
  MHD_FEATURE_SHUTDOWN_LISTEN_SOCKET = 8;
  MHD_FEATURE_SOCKETPAIR = 9;
  MHD_FEATURE_TCP_FASTOPEN = 10;
  MHD_FEATURE_BASIC_AUTH = 11;
  MHD_FEATURE_DIGEST_AUTH = 12;
  MHD_FEATURE_POSTPROCESSOR = 13;
  MHD_FEATURE_HTTPS_KEY_PASSWORD = 14;
  MHD_FEATURE_LARGE_FILE = 15;

type
  Pcchar = PAnsiChar;
  Ppcchar = ^Pcchar;
  va_list = Pointer;
{$IFDEF FPC}
  cint = CTypes.cint;
  cuint = CTypes.cuint;
  cuint16 = CTypes.cuint16;
  cuint64 = CTypes.cuint64;
  culonglong = CTypes.culonglong;
  socklen_t = {$IFDEF UNIX}BaseUnix.socklen_t{$ELSE}LongInt{$ENDIF};
  size_t = {$IFDEF UNIX}BaseUnix{$ELSE}Sockets{$ENDIF}.size_t;
  Psize_t = {$IFDEF UNIX}BaseUnix.pSize_t{$ELSE}^Sockets.size_t{$ENDIF};
  Pfd_set = {$IFDEF UNIX}BaseUnix.pFDSet{$ELSE}WinSock2.PFDSet{$ENDIF};
  ssize_t = {$IFDEF UNIX}BaseUnix{$ELSE}Sockets{$ENDIF}.ssize_t;
  psockaddr = Sockets.psockaddr;
{$ELSE}
  cint = LongInt;
  cuint = LongWord;
  cuint16 = Word;
  cuint64 = UInt64;
  culonglong = UInt64;
  socklen_t = LongInt;
  size_t = WinTypes.SIZE_T;
  Psize_t = WinTypes.PSIZE_T;
  Pfd_set = WinSock2.PFdSet;
  ssize_t = WinTypes.SSIZE_T;
  psockaddr = WinSock2.PSockAddr;
{$ENDIF}
{$IFNDEF MHD_SOCKET_DEFINED}
  PMHD_socket = ^MHD_socket;
  {$IF NOT DEFINED(WIN32) OR DEFINED(_SYS_TYPES_FD_SET)}
  MHD_socket = cint;
  {$ELSE}
  MHD_socket = TSocket;
  {$ENDIF}
{$ENDIF}
  MHD_LONG_LONG = clonglong;
  PMHD_LONG_LONG = ^MHD_LONG_LONG;
  MHD_UNSIGNED_LONG_LONG = culonglong;
  PMHD_UNSIGNED_LONG_LONG = ^MHD_UNSIGNED_LONG_LONG;

  // enums
  MHD_ValueKind = LongInt;

  MHD_FLAG = LongInt;

  MHD_OPTION = LongInt;

  MHD_RequestTerminationCode = LongInt;

  MHD_ConnectionNotificationCode = LongInt;

  MHD_ConnectionInfoType = LongInt;

  MHD_DaemonInfoType = LongInt;

  MHD_ResponseFlags = LongInt;

  MHD_ResponseOptions = LongInt;

  MHD_ResponseMemoryMode = LongInt;

  MHD_CONNECTION_OPTION = LongInt;

  MHD_FEATURE = LongInt;

  MHD_Daemon = record
  end;
  PMHD_Daemon = ^MHD_Daemon;

  MHD_Connection = record
  end;
  PMHD_Connection = ^MHD_Connection;

  MHD_Response = record
  end;
  PMHD_Response = ^MHD_Response;

  MHD_PostProcessor = record
  end;
  PMHD_PostProcessor = ^MHD_PostProcessor;

  MHD_OptionItem = record
    option: MHD_OPTION;
    value: ssize_t;
    ptr_value: Pointer;
  end;
  PMHD_OptionItem = ^MHD_OptionItem;

  MHD_ConnectionInfo = record
  case LongInt of
    0: (cipher_algorithm: cint);
    1: (protocol: cint);
    2: (connect_fd: MHD_socket);
    3: (tls_session: Pointer);
    4: (client_cert: Pointer);
    5: (client_addr: psockaddr);
    6: (daemon: PMHD_Daemon);
    7: (socket_context: ^Pointer);
  end;
  PMHD_ConnectionInfo = ^MHD_ConnectionInfo;

  MHD_DaemonInfo = record
  case LongInt of
    0: (key_size: size_t);
    1: (mac_key_size: size_t);
    2: (listen_fd: MHD_socket);
    3: (num_connections: cuint);
  end;
  PMHD_DaemonInfo = ^MHD_DaemonInfo;

  MHD_LogCallback = procedure(cls: Pointer; fm: Pcchar; ap: va_list); cdecl;
  MHD_PanicCallback = procedure(cls: Pointer; &file: Pcchar; line: cuint; reason: Pcchar); cdecl;
  MHD_AcceptPolicyCallback = function(cls: Pointer; addr: psockaddr; addrlen: socklen_t): cint; cdecl;
  MHD_AccessHandlerCallback = function(cls: Pointer; connection: PMHD_Connection; url: Pcchar; method: Pcchar; version: Pcchar; upload_data: Pcchar; upload_data_size: pSize_t; con_cls: PPointer): cint; cdecl;
  MHD_RequestCompletedCallback = procedure(cls: Pointer; connection: PMHD_Connection; con_cls: PPointer; toe: MHD_RequestTerminationCode); cdecl;
  MHD_NotifyConnectionCallback = procedure(cls: Pointer; connection: PMHD_Connection; socket_context: PPointer; toe: MHD_ConnectionNotificationCode); cdecl;
  MHD_KeyValueIterator = function(cls: Pointer; kind: MHD_ValueKind; key: Pcchar; value: Pcchar): cint; cdecl;
  MHD_ContentReaderCallback = function(cls: pointer; pos: cuint64; buf: Pcchar; max: size_t): ssize_t; cdecl;
  MHD_ContentReaderFreeCallback = procedure(cls: Pointer); cdecl;
  MHD_PostDataIterator = function(cls: Pointer; kind: MHD_ValueKind; key: Pcchar; filename: Pcchar; content_type: Pcchar; transfer_encoding: Pcchar; data: Pcchar; off: cuint64; size: size_t): cint; cdecl;

  function MHD_get_reason_phrase_for(code: cuint): Pcchar; cdecl; external MHD_LIB_NAME name 'MHD_get_reason_phrase_for';
  function MHD_start_daemon_va(flags: cuint; port: cuint16; apc: MHD_AcceptPolicyCallback; apc_cls: Pointer; dh: MHD_AccessHandlerCallback; dh_cls: Pointer; ap: va_list): PMHD_Daemon; cdecl; external MHD_LIB_NAME name 'MHD_start_daemon_va';
  function MHD_start_daemon(flags: cuint; port: cuint16; apc: MHD_AcceptPolicyCallback; apc_cls: Pointer; dh: MHD_AccessHandlerCallback; dh_cls: Pointer): PMHD_Daemon; cdecl; varargs; external MHD_LIB_NAME name 'MHD_start_daemon';
  function MHD_quiesce_daemon(daemon: PMHD_Daemon): MHD_socket; cdecl; external MHD_LIB_NAME name 'MHD_quiesce_daemon';
  procedure MHD_stop_daemon(daemon: PMHD_Daemon); cdecl; external MHD_LIB_NAME name 'MHD_stop_daemon';
  function MHD_add_connection(daemon: PMHD_Daemon; client_socket: MHD_socket; addr: psockaddr; addrlen: socklen_t): cint; cdecl; external MHD_LIB_NAME name 'MHD_add_connection';
  function MHD_get_fdset(daemon: PMHD_Daemon; read_fd_set: Pfd_set; write_fd_set: Pfd_set; except_fd_set: Pfd_set; max_fd: PMHD_socket): cint; cdecl; external MHD_LIB_NAME name 'MHD_get_fdset';
  function MHD_get_fdset2(daemon: PMHD_Daemon; read_fd_set: Pfd_set; write_fd_set: Pfd_set; except_fd_set: Pfd_set; max_fd: PMHD_socket; fd_setsize: cuint): cint; cdecl; external MHD_LIB_NAME name 'MHD_get_fdset2';
  function MHD_get_timeout(daemon: PMHD_Daemon; timeout: PMHD_UNSIGNED_LONG_LONG): cint; cdecl; external MHD_LIB_NAME name 'MHD_get_timeout';
  function MHD_run(daemon: PMHD_Daemon): cint; cdecl; external MHD_LIB_NAME name 'MHD_run';
  function MHD_run_from_select(daemon: PMHD_Daemon; read_fd_set: Pfd_set; write_fd_set: Pfd_set; except_fd_set: Pfd_set): cint; cdecl; external MHD_LIB_NAME name 'MHD_run_from_select';
  function MHD_get_connection_values(connection: PMHD_Connection; kind: MHD_ValueKind; iterator: MHD_KeyValueIterator; iterator_cls: pointer): cint; cdecl; external MHD_LIB_NAME name 'MHD_get_connection_values';
  function MHD_set_connection_value(connection: PMHD_Connection; kind: MHD_ValueKind; key: Pcchar; value: Pcchar): cint; cdecl; external MHD_LIB_NAME name 'MHD_set_connection_value';
  procedure MHD_set_panic_func(cb: MHD_PanicCallback; cls: Pointer); cdecl; external MHD_LIB_NAME name 'MHD_set_panic_func';
  function MHD_http_unescape(val: Pcchar): size_t; cdecl; external MHD_LIB_NAME name 'MHD_http_unescape';
  function MHD_lookup_connection_value(connection: PMHD_Connection; kind: MHD_ValueKind; key: Pcchar): Pcchar; cdecl; external MHD_LIB_NAME name 'MHD_lookup_connection_value';
  function MHD_queue_response(connection: PMHD_Connection; status_code: cuint; response: PMHD_Response): cint; cdecl; external MHD_LIB_NAME name 'MHD_queue_response';
  procedure MHD_suspend_connection(connection: PMHD_Connection); cdecl; external MHD_LIB_NAME name 'MHD_suspend_connection';
  procedure MHD_resume_connection(connection: PMHD_Connection); cdecl; external MHD_LIB_NAME name 'MHD_resume_connection';
  function MHD_set_response_options(response: PMHD_Response; flags: MHD_ResponseFlags): cint; cdecl; varargs; external MHD_LIB_NAME name 'MHD_set_response_options';
  function MHD_create_response_from_callback(size: cuint64; block_size: size_t; crc: MHD_ContentReaderCallback; crc_cls: pointer; crfc: MHD_ContentReaderFreeCallback): PMHD_Response; cdecl; external MHD_LIB_NAME name 'MHD_create_response_from_callback';
  function MHD_create_response_from_buffer(size: size_t; buffer: Pointer; mode: MHD_ResponseMemoryMode): PMHD_Response; cdecl; external MHD_LIB_NAME name 'MHD_create_response_from_buffer';
  function MHD_create_response_from_fd(size: size_t; fd: cint): PMHD_Response; cdecl; external MHD_LIB_NAME name 'MHD_create_response_from_fd';
  function MHD_create_response_from_fd64(size: cuint64; fd: cint): PMHD_Response; cdecl; external MHD_LIB_NAME name 'MHD_create_response_from_fd64';
  function MHD_create_response_from_fd_at_offset64(size: cuint64; fd: cint; offset: cuint64): PMHD_Response;cdecl;external MHD_LIB_NAME name 'MHD_create_response_from_fd_at_offset64';
  procedure MHD_destroy_response(response: PMHD_Response); cdecl; external MHD_LIB_NAME name 'MHD_destroy_response';
  function MHD_add_response_header(response: PMHD_Response; header: Pcchar; content: Pcchar): cint; cdecl; external MHD_LIB_NAME name 'MHD_add_response_header';
  function MHD_add_response_footer(response: PMHD_Response; footer: Pcchar; content: Pcchar): cint; cdecl; external MHD_LIB_NAME name 'MHD_add_response_footer';
  function MHD_del_response_header(response: PMHD_Response; header: Pcchar; content: Pcchar): cint; cdecl; external MHD_LIB_NAME name 'MHD_del_response_header';
  function MHD_get_response_headers(response: PMHD_Response; iterator: MHD_KeyValueIterator; iterator_cls: Pointer): cint; cdecl; external MHD_LIB_NAME name 'MHD_get_response_headers';
  function MHD_get_response_header(response: PMHD_Response; key: Pcchar): Pcchar; cdecl; external MHD_LIB_NAME name 'MHD_get_response_header';
  function MHD_create_post_processor(connection: PMHD_Connection; buffer_size: size_t; iter: MHD_PostDataIterator; iter_cls: pointer): PMHD_PostProcessor; cdecl; external MHD_LIB_NAME name 'MHD_create_post_processor';
  function MHD_post_process(pp: PMHD_PostProcessor; post_data: Pcchar; post_data_len: size_t): cint; cdecl; external MHD_LIB_NAME name 'MHD_post_process';
  function MHD_destroy_post_processor(pp: PMHD_PostProcessor): cint; cdecl; external MHD_LIB_NAME name 'MHD_destroy_post_processor';
  function MHD_digest_auth_get_username(connection: PMHD_Connection): Pcchar; cdecl; external MHD_LIB_NAME name 'MHD_digest_auth_get_username';
  function MHD_digest_auth_check(connection: PMHD_Connection; realm: Pcchar; username: Pcchar; password: Pcchar; nonce_timeout: cuint): cint; cdecl; external MHD_LIB_NAME name 'MHD_digest_auth_check';
  function MHD_queue_auth_fail_response(connection: PMHD_Connection; realm: Pcchar; opaque: Pcchar; response: PMHD_Response; signal_stale: cint): cint; cdecl; external MHD_LIB_NAME name 'MHD_queue_auth_fail_response';
  function MHD_basic_auth_get_username_password(connection: PMHD_Connection; password: PPcchar): Pcchar; cdecl; external MHD_LIB_NAME name 'MHD_basic_auth_get_username_password';
  function MHD_queue_basic_auth_fail_response(connection: PMHD_Connection; realm: Pcchar; response: PMHD_Response): cint; cdecl; external MHD_LIB_NAME name 'MHD_queue_basic_auth_fail_response';
  function MHD_get_connection_info(connection: PMHD_Connection; info_type: MHD_ConnectionInfoType): PMHD_ConnectionInfo; cdecl; varargs; external MHD_LIB_NAME name 'MHD_get_connection_info';
  function MHD_set_connection_option(connection: PMHD_Connection; option: MHD_CONNECTION_OPTION): cint; cdecl; varargs; external MHD_LIB_NAME name 'MHD_set_connection_option';
  function MHD_get_daemon_info(daemon: PMHD_Daemon; info_type: MHD_DaemonInfoType): PMHD_DaemonInfo; cdecl; varargs; external MHD_LIB_NAME name 'MHD_get_daemon_info';
  function MHD_get_version: Pcchar; cdecl; external MHD_LIB_NAME name 'MHD_get_version';
  function MHD_is_feature_supported(feature: MHD_FEATURE): cint; cdecl; external MHD_LIB_NAME name 'MHD_is_feature_supported';

implementation

end.
