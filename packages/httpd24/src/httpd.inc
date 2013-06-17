{
 Converted using the Apache 2.4.3 httpd-2.4.3 source files
 to be used with FreePascal (fpc)
}
{* XXX - We need to push more stuff to other .h files, or even .c files, to
 * make this file smaller}

{ Headers in which EVERYONE has an interest...  }
{$include ap_config.inc}
{$include ap_mmn.inc}
{$include ap_release.inc}
{all translated apr are included in the parent httpd.pas unit with using the apr.pas unit}
////{$include "apr_general.inc"}
//{$include "apr_tables.inc"}
//{$include "apr_pools.inc"}
////{$include "apr_time.inc"}
////{$include "apr_network_io.inc"}
{$include aprutil/apr_buckets.inc}
{$include aprutil/apr_uri.inc}
//{$include "apr_poll.inc"}
//{$include "apr_thread_proc.inc"}
////{$include "os.inc"}
{$include ap_regex.inc}

{ Note: apr_uri.h is also included, see below  }

{ ----------------------------- config dir ------------------------------  }
{* Define this to be the default server home dir. Most things later in this
 * file with a relative pathname will have this added.
  }

const
{$ifdef OS2}
  {* Set default for OS/2 file system  }
    HTTPD_ROOT = '/os2httpd';    
{$else}
  {$ifdef WIN32}
  {* Set default for Windows file system  }
    HTTPD_ROOT = '/apache';    
  {$else}
    {$ifdef NETWARE}
  {* Set the default for NetWare  }
    HTTPD_ROOT = '/apache';    
   {$else}
  {* Set for all other OSs  }
    HTTPD_ROOT = '/usr/local/apache';    
    {$endif}
  {$endif}
{$endif}
{ HTTPD_ROOT  }

{* --------- You shouldn't have to edit anything below this line ----------
 *
 * Any modifications to any defaults not defined above should be done in the
 * respective configuration file.
 *}

{*
 * Default location of documents.  Can be overridden by the DocumentRoot
 * directive.}
{$ifdef OS2}
  { Set default for OS/2 file system  }
    DOCUMENT_LOCATION  = HTTPD_ROOT + '/docs';
{$else}
  { Set default for non OS/2 file system  }
    DOCUMENT_LOCATION  = HTTPD_ROOT + '/htdocs';
{$endif}

    {* Maximum number of dynamically loaded modules  }
    DYNAMIC_MODULE_LIMIT = 256;      

    {* Default administrator's address  }
    DEFAULT_ADMIN = '[no address given]';      

    {* The name of the log files  }
    DEFAULT_ERRORLOG = 'logs/error_log';      

    {* Define this to be what your per-directory security files are called  }
{$ifdef OS2}
    { Set default for OS/2 file system  }
    DEFAULT_ACCESS_FNAME = 'htaccess';      
{$else}
    DEFAULT_ACCESS_FNAME = '.htaccess';      
{$endif}

    {* The name of the server config file  }
    SERVER_CONFIG_FILE = 'conf/httpd.conf';      

    {* The default path for CGI scripts if none is currently set  }
    DEFAULT_PATH = '/bin:/usr/bin:/usr/ucb:/usr/bsd:/usr/local/bin';      

    {* The path to the suExec wrapper, can be overridden in Configuration  }
    SUEXEC_BIN = HTTPD_ROOT + '/bin/suexec';

    {* The timeout for waiting for messages  }
    DEFAULT_TIMEOUT = 60;      

    {* The timeout for waiting for keepalive timeout until next request  }
    DEFAULT_KEEPALIVE_TIMEOUT = 5;      

    {* The number of requests to entertain per connection  }
    DEFAULT_KEEPALIVE = 100;      

    {
     * Limits on the size of various request items.  These limits primarily
     * exist to prevent simple denial-of-service attacks on a server based
     * on misuse of the protocol.  The recommended values will depend on the
     * nature of the server resources -- CGI scripts and database backends
     * might require large values, but most servers could get by with much
     * smaller limits than we use below.  The request message body size can
     * be limited by the per-dir config directive LimitRequestBody.
     *
     * Internal buffer sizes are two bytes more than the DEFAULT_LIMIT_REQUEST_LINE
     * and DEFAULT_LIMIT_REQUEST_FIELDSIZE below, which explains the 8190.
     * These two limits can be lowered or raised by the server config
     * directives LimitRequestLine and LimitRequestFieldsize, respectively.
     *
     * DEFAULT_LIMIT_REQUEST_FIELDS can be modified or disabled (set = 0) by
     * the server config directive LimitRequestFields.
      }
    {* default limit on bytes in Request-Line (Method+URI+HTTP-version)  }
    DEFAULT_LIMIT_REQUEST_LINE = 8190;      

    {* default limit on bytes in any one header field   }
    DEFAULT_LIMIT_REQUEST_FIELDSIZE = 8190;      

    {* default limit on number of request header fields  }
    DEFAULT_LIMIT_REQUEST_FIELDS = 100;      

    {*
     * The default default character set name to add if AddDefaultCharset is
     * enabled.  Overridden with AddDefaultCharsetName.
      }
    DEFAULT_ADD_DEFAULT_CHARSET_NAME = 'iso-8859-1';      

    {* default HTTP Server protocol  }
    AP_SERVER_PROTOCOL = 'HTTP/1.1';      

    { ------------------ stuff that modules are allowed to look at -----------  }
    {* Define this to be what your HTML directory content files are called  }
    AP_DEFAULT_INDEX = 'index.html';      

    {* The name of the MIME types file  }
    AP_TYPES_CONFIG_FILE = 'conf/mime.types';      

    {
     * Define the HTML doctype strings centrally.
      }
    {* HTML 2.0 Doctype  }
    DOCTYPE_HTML_2_0 = '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">' + LineEnding;      

    {* HTML 3.2 Doctype  }
    DOCTYPE_HTML_3_2 = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">' + LineEnding;      

    {* HTML 4.0 Strict Doctype  }
    DOCTYPE_HTML_4_0S = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN"' + LineEnding +
                        'http://www.w3.org/TR/REC-html40/strict.dtd">' + LineEnding;      

    {* HTML 4.0 Transitional Doctype  }
    DOCTYPE_HTML_4_0T = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"' + LineEnding +
                        '"http://www.w3.org/TR/REC-html40/loose.dtd">' + LineEnding;      

    {* HTML 4.0 Frameset Doctype  }
    DOCTYPE_HTML_4_0F = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Frameset//EN"' + LineEnding +
                        '"http://www.w3.org/TR/REC-html40/frameset.dtd">' + LineEnding;      

    {* XHTML 1.0 Strict Doctype  }
    DOCTYPE_XHTML_1_0S = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"' + LineEnding +
                         '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' + LineEnding;

    {* XHTML 1.0 Transitional Doctype  }
    DOCTYPE_XHTML_1_0T = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"' + LineEnding +
                         '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + LineEnding;      

    {* XHTML 1.0 Frameset Doctype  }
    DOCTYPE_XHTML_1_0F = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN"' + LineEnding +
                         '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">' + LineEnding;

    {* Internal representation for a HTTP protocol number, e.g., HTTP/1.1  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function HTTP_VERSION(major,minor : Integer) : Integer;    

    {* Major part of HTTP protocol  }
    function HTTP_VERSION_MAJOR(number : Integer) : Integer;

    {* Minor part of HTTP protocol  }
    function HTTP_VERSION_MINOR(number : Integer) : Integer;

    { -------------- Port number for server running standalone ---------------  }
    const
    {* default HTTP Port  }
      DEFAULT_HTTP_PORT = 80;      
    {* default HTTPS Port  }
      DEFAULT_HTTPS_PORT = 443;      
    {*
     * Check whether @a port is the default port for the request @a r.
     * @param port The port number
     * @param r The request
     * @see #ap_default_port
      }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
//    function ap_is_default_port(port,r : longint) : longint;    

  {*
   * Get the default port for a request (which depends on the scheme).
   * @param r The request
    }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function ap_default_port(r : longint) : longint;  

  {*
   * Get the scheme for a request.
   * @param r The request
    }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function ap_http_scheme(r : longint) : longint;  

  const
  {* The length of a Huge string  }
    HUGE_STRING_LEN = 8192;

  {* The default string length  }
    MAX_STRING_LEN = HUGE_STRING_LEN;    

  {* The size of the server's internal read-write buffers  }
    AP_IOBUFSIZE = 8192;    

  {* The max number of regex captures that can be expanded by ap_pregsub  }
    AP_MAX_REG_MATCH = 10;    

  {*
   * APR_HAS_LARGE_FILES introduces the problem of spliting sendfile into
   * mutiple buckets, no greater than MAX(apr_size_t), and more granular
   * than that in case the brigade code/filters attempt to read it directly.
   * ### 16mb is an invention, no idea if it is reasonable.
    }
  { 2^24  }
    AP_MAX_SENDFILE = 16777216;    

  {*
   * MPM child process exit status values
   * The MPM parent process may check the status to see if special
   * error handling is required.
    }
  {* a normal exit  }
    APEXIT_OK = $0;    

  {* A fatal error arising during the server's init sequence  }
    APEXIT_INIT = $2;    

  {*  The child died during its init sequence  }
    APEXIT_CHILDINIT = $3;    

  {*
   *   The child exited due to a resource shortage.
   *   The parent should limit the rate of forking until
   *   the situation is resolved.
    }
    APEXIT_CHILDSICK = $7;    

  {*
   *     A fatal error, resulting in the whole server aborting.
   *     If a child exits with this error, the parent process
   *     considers this a server-wide fatal error and aborts.
    }
    APEXIT_CHILDFATAL = $f;    

  {*
   * Stuff marked #AP_DECLARE is part of the API, and intended for use
   * by modules. Its purpose is to allow us to add attributes that
   * particular platforms or compilers require to every exported function.
    }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function AP_DECLARE(_type : longint) : longint;  

  {*
   * Stuff marked #AP_DECLARE_NONSTD is part of the API, and intended for
   * use by modules.  The difference between #AP_DECLARE and
   * #AP_DECLARE_NONSTD is that the latter is required for any functions
   * which use varargs or are used via indirect function call.  This
   * is to accomodate the two calling conventions in windows dlls.
    }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function AP_DECLARE_NONSTD(_type : longint) : longint;  


{$define AP_DECLARE_DATA}  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function AP_MODULE_DECLARE(_type : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function AP_MODULE_DECLARE_NONSTD(_type : longint) : longint;  

{$define AP_MODULE_DECLARE_DATA}  

  {*
   * @internal
   * modules should not use functions marked AP_CORE_DECLARE
    }
//  const
//    AP_CORE_DECLARE = AP_DECLARE;    

  {*
   * @internal
   * modules should not use functions marked AP_CORE_DECLARE_NONSTD
    }
//  const
//    AP_CORE_DECLARE_NONSTD = AP_DECLARE_NONSTD;    

  {*
   * @defgroup APACHE_APR_STATUS_T HTTPD specific values of apr_status_t
   * @
    }

//  const
//    AP_START_USERERR = APR_OS_START_USERERR+2000;    
//    AP_USERERR_LEN = 1000;    
  {* The function declines to handle the request  }
//    AP_DECLINED = AP_START_USERERR+0;    

  {* @  }
  {*
   * @brief The numeric version information is broken out into fields within this
   * structure.
    }
  {*< major number  }
  {*< minor number  }
  {*< patch number  }
(* Const before type ignored *)
  {*< additional string like "-dev"  }

  type
    Pap_version_t = ^ap_version_t;
    ap_version_t = record
        major : Integer;
        minor : Integer;
        patch : Integer;
        add_string : PChar;
      end;

  {*
   * Return httpd's version information in a numeric form.
   *
   *  @param version Pointer to a version structure for returning the version
   *                 information.
    }
  procedure ap_get_server_revision(version: Pap_version_t);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_get_server_revision' + LibSuff4;

  {*
   * Get the server banner in a form suitable for sending over the
   * network, with the level of information controlled by the
   * ServerTokens directive.
   * @return The server banner
    }
  function ap_get_server_banner: PChar;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_get_server_revision' + LibSuff0;

  {*
   * Get the server description in a form suitable for local displays,
   * status reports, or logging.  This includes the detailed server
   * version and information about some modules.  It is not affected
   * by the ServerTokens directive.
   * @return The server description
    }
  function ap_get_server_description: PChar;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_get_server_revision' + LibSuff0;

  {*
   * Add a component to the server description and banner strings
   * @param pconf The pool to allocate the component from
   * @param component The string to add
    }
  procedure ap_add_version_component(pconf: Papr_pool_t; const component: PChar);
   {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   external LibHTTPD name LibNamePrefix + 'ap_add_version_component' + LibSuff8;

  {*
   * Get the date a time that the server was built
   * @return The server build time string
    }
  function ap_get_server_built: PChar;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_get_server_built' + LibSuff0;

  { non-HTTP status codes returned by hooks  }
  const
    OK = 0;    		{*< Module has handled this stage.  }
    DECLINED = -1;	{*< Module declines to handle  }
    DONE = -2;		{*< Module has served the response completely
                         *  - it's safe to die() with no more output
			}
    SUSPENDED = -(3);	{*< Module will handle the remainder of the request.
                         * The core will never invoke the request again,  }

  {* Returned by the bottom-most filter if no data was written.
   *  @see ap_pass_brigade().  }
    AP_NOBODY_WROTE = -100;    
  {* Returned by the bottom-most filter if no data was read.
   *  @see ap_get_brigade().  }
    AP_NOBODY_READ = -101;    
  {* Returned by any filter if the filter chain encounters an error
   *  and has already dealt with the error response.    }
    AP_FILTER_ERROR = -102;    

  {*
   * @defgroup HTTP_Status HTTP Status Codes
   * @
    }
  {*
   * The size of the static status_lines array in http_protocol.c for
   * storing all of the potential response status-lines (a sparse table).
   * When adding a new code here add it to status_lines as well.
   * A future version should dynamically generate the apr_table_t at startup.
    }
    RESPONSE_CODES =				83;    

    HTTP_CONTINUE =				100;    
    HTTP_SWITCHING_PROTOCOLS =			101;    
    HTTP_PROCESSING =				102;    
    HTTP_OK =					200;    
    HTTP_CREATED =				201;    
    HTTP_ACCEPTED =				202;    
    HTTP_NON_AUTHORITATIVE =			203;    
    HTTP_NO_CONTENT =				204;    
    HTTP_RESET_CONTENT =			205;    
    HTTP_PARTIAL_CONTENT =			206;    
    HTTP_MULTI_STATUS =				207;    
    HTTP_ALREADY_REPORTED =			208;    
    HTTP_IM_USED =				226;    
    HTTP_MULTIPLE_CHOICES =			300;    
    HTTP_MOVED_PERMANENTLY =			301;    
    HTTP_MOVED_TEMPORARILY =			302;    
    HTTP_SEE_OTHER =				303;    
    HTTP_NOT_MODIFIED =				304;    
    HTTP_USE_PROXY =				305;    
    HTTP_TEMPORARY_REDIRECT =			307;    
    HTTP_PERMANENT_REDIRECT =			308;    
    HTTP_BAD_REQUEST =				400;    
    HTTP_UNAUTHORIZED =				401;    
    HTTP_PAYMENT_REQUIRED =			402;    
    HTTP_FORBIDDEN =				403;    
    HTTP_NOT_FOUND =				404;    
    HTTP_METHOD_NOT_ALLOWED =			405;    
    HTTP_NOT_ACCEPTABLE =			406;    
    HTTP_PROXY_AUTHENTICATION_REQUIRED =	407;    
    HTTP_REQUEST_TIME_OUT =			408;    
    HTTP_CONFLICT =				409;    
    HTTP_GONE =					410;    
    HTTP_LENGTH_REQUIRED =			411;    
    HTTP_PRECONDITION_FAILED =			412;    
    HTTP_REQUEST_ENTITY_TOO_LARGE =		413;    
    HTTP_REQUEST_URI_TOO_LARGE =		414;    
    HTTP_UNSUPPORTED_MEDIA_TYPE =		415;    
    HTTP_RANGE_NOT_SATISFIABLE =		416;    
    HTTP_EXPECTATION_FAILED =			417;    
    HTTP_UNPROCESSABLE_ENTITY =			422;    
    HTTP_LOCKED =				423;    
    HTTP_FAILED_DEPENDENCY =			424;    
    HTTP_UPGRADE_REQUIRED =			426;    
    HTTP_PRECONDITION_REQUIRED =		428;    
    HTTP_TOO_MANY_REQUESTS =			429;    
    HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE =	431;    
    HTTP_INTERNAL_SERVER_ERROR =		500;    
    HTTP_NOT_IMPLEMENTED =			501;    
    HTTP_BAD_GATEWAY =				502;    
    HTTP_SERVICE_UNAVAILABLE =			503;    
    HTTP_GATEWAY_TIME_OUT =			504;    
    HTTP_VERSION_NOT_SUPPORTED =		505;    
    HTTP_VARIANT_ALSO_VARIES =			506;    
    HTTP_INSUFFICIENT_STORAGE =			507;    
    HTTP_LOOP_DETECTED =			508;    
    HTTP_NOT_EXTENDED =				510;    
    HTTP_NETWORK_AUTHENTICATION_REQUIRED =	511;    

  {* is the status code informational  }
  function ap_is_HTTP_INFO(x : Integer): Boolean;

  {* is the status code OK ? }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function ap_is_HTTP_SUCCESS(x : Integer): Boolean;

  {* is the status code a redirect  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function ap_is_HTTP_REDIRECT(x : Integer): Boolean;

  {* is the status code a error (client or server)  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function ap_is_HTTP_ERROR(x : Integer): Boolean;

  {* is the status code a client error   }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function ap_is_HTTP_CLIENT_ERROR(x : Integer): Boolean;

  {* is the status code a server error   }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function ap_is_HTTP_SERVER_ERROR(x : Integer): Boolean;

  {* is the status code a (potentially) valid response code?   }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function ap_is_HTTP_VALID_RESPONSE(x : Integer): Boolean;

  {* should the status code drop the connection  }
  function ap_status_drops_connection(x : Integer): Boolean;

    {* @  }
    {*
     * @defgroup Methods List of Methods recognized by the server
     * @ingroup APACHE_CORE_DAEMON
     * @
     *
     * @brief Methods recognized (but not necessarily handled) by the server.
     *
     * These constants are used in bit shifting masks of size int, so it is
     * unsafe to have more methods than bits in an int.  HEAD == M_GET.
     * This list must be tracked by the list in http_protocol.c in routine
     * ap_method_name_of().
     *
      }
  const
    M_GET =			0;      {* RFC 2616: HTTP  }
    M_PUT =			1;      {  :              }
    M_POST =			2;      
    M_DELETE =			3;      
    M_CONNECT =			4;      
    M_OPTIONS =			5;      
    M_TRACE =			6;      {* RFC 2616: HTTP  }
    M_PATCH =			7;      {* no rfc(!)  ### remove this one?  }
    M_PROPFIND =		8;      {* RFC 2518: WebDAV  }
    M_PROPPATCH =		9;      {  :                }
    M_MKCOL =			10;      
    M_COPY =			11;      
    M_MOVE =			12;      
    M_LOCK =			13;      
    M_UNLOCK =			14;     {* RFC 2518: WebDAV  }
    M_VERSION_CONTROL =		15;     {* RFC 3253: WebDAV Versioning  }
    M_CHECKOUT =		16;     {  :                           }
    M_UNCHECKOUT =		17;      
    M_CHECKIN =			18;      
    M_UPDATE =			19;      
    M_LABEL =			20;      
    M_REPORT =			21;      
    M_MKWORKSPACE =		22;      
    M_MKACTIVITY =		23;      
    M_BASELINE_CONTROL =	24;      
    M_MERGE =			25;      
    M_INVALID =			26;     {* no valid method  }

    {*
     * METHODS needs to be equal to the number of bits
     * we are using for limit masks.
      }
    METHODS = 64;      
    {*
     * The method mask bit to shift for anding with a bitmask.
      }

    { was #define dname def_expr }
    AP_METHOD_BIT = apr_int64_t(1);      

  {* @see ap_method_list_t  }
  type
  {*
   * @struct ap_method_list_t
   * @brief  Structure for handling HTTP methods.
   *
   * Methods known to the server are accessed via a bitmask shortcut;
   * extension methods are handled by an array.
    }
    Pap_method_list_t = ^ap_method_list_t;
    ap_method_list_t = record
  {* The bitmask used for known methods  }
        method_mask : apr_int64_t;
  {* the array used for extension methods  }
        method_list : Papr_array_header_t;
      end;

  {*
   * @defgroup module_magic Module Magic mime types
   * @
    }
  const
  {* Magic for mod_cgi[d]  }
    CGI_MAGIC_TYPE = 'application/x-httpd-cgi';    
  {* Magic for mod_include  }
    INCLUDES_MAGIC_TYPE = 'text/x-server-parsed-html';    
  {* Magic for mod_include  }
    INCLUDES_MAGIC_TYPE3 = 'text/x-server-parsed-html3';    
  {* Magic for mod_dir  }
    DIR_MAGIC_TYPE = 'httpd/unix-directory';    
  {* @  }

  { Just in case your linefeed isn't the one the other end is expecting.  }
  const
  {* linefeed  }
    LF = 10;    
  {* carrige return  }
    CR = 13;    
  {* carrige return /Line Feed Combo  }
    CRLF = #015#012;    

  {*
   * @defgroup values_request_rec_body Possible values for request_rec.read_body
   * @
   * Possible values for request_rec.read_body (set by handling module):
    }
  {* Send 413 error if message has any body  }
    REQUEST_NO_BODY =		0;    
  {* Send 411 error if body without Content-Length  }
    REQUEST_CHUNKED_ERROR =	1;    
  {* If chunked, remove the chunks for me.  }
    REQUEST_CHUNKED_DECHUNK =	2;    

  {* @ // values_request_rec_body  }
  {*
   * @defgroup values_request_rec_used_path_info Possible values for request_rec.used_path_info
   * @ingroup APACHE_CORE_DAEMON
   * @
   * Possible values for request_rec.used_path_info:
    }
  {* Accept the path_info from the request  }
    AP_REQ_ACCEPT_PATH_INFO =	0;    
  {* Return a 404 error if path_info was given  }
    AP_REQ_REJECT_PATH_INFO =	1;    
  {* Module may chose to use the given path_info  }
    AP_REQ_DEFAULT_PATH_INFO =	2;    

  {* @ // values_request_rec_used_path_info  }
  {
   * Things which may vary per file-lookup WITHIN a request ---
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
  {*
   * @brief This represents the result of calling htaccess; these are cached for
   * each request.
    }
  type
    Phtaccess_result = ^htaccess_result;
    htaccess_result = record
  {* the directory to which this applies  }
        dir : Pchar;
  {* the overrides allowed for the .htaccess file  }
        override : Integer;
  {* the override options allowed for the .htaccess file  }
        override_opts : Integer;
  {* Table of allowed directives for override  }
        override_list : Papr_table_t;
  {* the configuration directives  }
        htaccess : Pap_conf_vector_t;
  {* the next one, or NULL if no more; N.B. never change this  }
        next : Phtaccess_result;
      end;

  { The following four types define a hierarchy of activities, so that
   * given a request_rec r you can write r->connection->server->process
   * to get to the process_rec.  While this reduces substantially the
   * number of arguments that various hooks require beware that in
   * threaded versions of the server you must consider multiplexing
   * issues.   }

//{$include "apr_uri.inc"}

  type
    {*
     * @brief A structure that represents one process
      }
    Pprocess_rec = ^process_rec;
    process_rec = record
    {* Global pool. Cleared upon normal exit  }
          pool : Papr_pool_t;
    {* Configuration pool. Cleared upon restart  }
          pconf : Papr_pool_t;
    {* The program name used to execute the program  }
          short_name : PChar;
    {* The command line arguments  }
          argv : PChar;
    {* Number of command line arguments passed to the program  }
          argc : Integer;
      end;

 {*
 * @struct server_addr_rec
 * @brief  A structure to be used for Per-vhost config
  }
  type
  Pserver_addr_rec = ^server_addr_rec;
  server_addr_rec = record
{* The next server in the list  }
      next : Pserver_addr_rec;
{* The name given in "<VirtualHost>"  }
      virthost : Pchar;
{* The bound address, for this server  }
      host_addr : Papr_sockaddr_t;
{* The bound port, for this server  }
      host_port : apr_port_t;
    end;

  Pap_logconf = ^ap_logconf;
  ap_logconf = record
{* The per-module log levels  }
      module_levels : ^char;
{* The log level for this server  }
      level : longint;
    end;

{*
 * @brief A structure to store information for each virtual server
  }
  PPserver_rec = ^Pserver_rec;
  Pserver_rec = ^server_rec;
  server_rec = record
{* The process this server is running in  }
      process : Pprocess_rec;
{* The next server in the list  }
      next : Pserver_rec;

{ Log files --- note that transfer log is now in the modules...  }

{* The name of the error log  }
      error_fname : Pchar;
{* A file descriptor that references the error log  }
      error_log : Papr_file_t;
{* The log level configuration  }
      log : ap_logconf;

{ Module-specific configuration for server, and defaults...  }

{* Config vector containing pointers to modules' per-server config
 *  structures.  }
      module_config : Pap_conf_vector_t;
{* MIME type info, etc., before we start checking per-directory info  }
      lookup_defaults : Pap_conf_vector_t;
{* The name of the server  }
      defn_name : Pchar;
{* The line of the config file that the server was defined on  }
      defn_line_number : dword;
{* true if this is the virtual server  }
      is_virtual : char;

{ Information for redirects  }

{* for redirects, etc.  }
      port : apr_port_t;
{* The server request scheme for redirect responses  }
      server_scheme : Pchar;

{ Contact information  }

{* The admin's contact information  }
      server_admin : Pchar;
{* The server hostname  }
      server_hostname : Pchar;

{ Transaction handling  }

{* I haven't got a clue  }
      addrs : Pserver_addr_rec;
{* Timeout, as an apr interval, before we give up  }
      timeout : apr_interval_time_t;
{* The apr interval we will wait for another request  }
      keep_alive_timeout : apr_interval_time_t;
{* Maximum requests per connection  }
      keep_alive_max : Integer;
{* Use persistent connections?  }
      keep_alive : Integer;

{* Normal names for ServerAlias servers  }
      names : Papr_array_header_t;
{* Wildcarded names for ServerAlias servers  }
      wild_names : Papr_array_header_t;

{* Pathname for ServerPath  }
      path : Pchar;
{* Length of path  }
      pathlen : Integer;

{* limit on size of the HTTP request line     }
      limit_req_line : Integer;
{* limit on size of any request header field  }
      limit_req_fieldsize : Integer;
{* limit on number of request header fields   }
      limit_req_fields : Integer;

{* Opaque storage location  }
      context : pointer;
    end;


  {*
   * @brief Structure to store things which are per connection
    }
  type
    ap_conn_keepalive_e = (AP_CONN_UNKNOWN,AP_CONN_CLOSE,AP_CONN_KEEPALIVE);

    {*
     * Enumeration of connection states
     * The two states CONN_STATE_LINGER_NORMAL and CONN_STATE_LINGER_SHORT may
     * only be set by the MPM. Use CONN_STATE_LINGER outside of the MPM.}
    conn_state_e = (
      CONN_STATE_CHECK_REQUEST_LINE_READABLE,
      CONN_STATE_READ_REQUEST_LINE,
      CONN_STATE_HANDLER,
      CONN_STATE_WRITE_COMPLETION,
      CONN_STATE_SUSPENDED,
      CONN_STATE_LINGER,	{ connection may be closed with lingering  }
      CONN_STATE_LINGER_NORMAL,	{ MPM has started lingering close with normal timeout  }
      CONN_STATE_LINGER_SHORT);	{ MPM has started lingering close with short timeout  }

    {*
     * @brief A structure to contain connection state information}
    Pconn_state_t = ^conn_state_t;
    conn_state_t = record
        {* Current state of the connection  }
        state : conn_state_e;
      end;

    Pconn_rec = ^conn_rec;
    conn_rec = record
  {* Pool associated with this connection  }
        pool : Papr_pool_t;
  {* Physical vhost this conn came in on  }
        base_server : Pserver_rec;
  {* used by http_vhost.c  }
        vhost_lookup_data : pointer;
  { Information about the connection itself  }
  {* local address  }
        local_addr : Papr_sockaddr_t;
  {* remote address; this is the end-point of the next hop, for the address
   *  of the request creator, see useragent_addr in request_rec
   }
        remote_addr {client_addr} : Papr_sockaddr_t; //fpc -> renamed to stay compatible with older apache versions and older fcl-web
  {* Client's IP address; this is the end-point of the next hop, for the
   *  IP of the request creator, see useragent_ip in request_rec
   }
        remote_ip {client_ip} : Pchar;               //fpc -> renamed to stay compatible with older apache versions and older fcl-web
  {* Client's DNS name, if known.  NULL if DNS hasn't been checked,
   *  "" if it has and no address was found.  N.B. Only access this though
   * get_remote_host()  }
        remote_host : Pchar;
  {* Only ever set if doing rfc1413 lookups.  N.B. Only access this through
   *  get_remote_logname()  }
        remote_logname : Pchar;
  {* server IP address  }
        local_ip : Pchar;
  {* used for ap_get_server_name when UseCanonicalName is set to DNS
   *  (ignores setting of HostnameLookups)  }
        local_host : Pchar;
  {* ID of this connection; unique at any point in time  }
        id : clong;
  {* Config vector containing pointers to connections per-server
   *  config structures.  }
        conn_config : Pap_conf_vector_t;
  {* Notes on *this* connection: send note from one module to
   *  another. must remain valid for all requests on this conn  }
        notes : Papr_table_t;
  {* A list of input filters to be used for this connection  }
        input_filters : Pap_filter_t;
  {* A list of output filters to be used for this connection  }
        output_filters : Pap_filter_t;
  {* handle to scoreboard information for this connection  }
        sbh : pointer;
  {* The bucket allocator to use for all bucket/brigade creations  }
        bucket_alloc : Papr_bucket_alloc_t;
  {* The current state of this connection; may be NULL if not used by MPM  }
        cs : Pconn_state_t;
  {* Is there data pending in the input filters?  }
        data_in_input_filters : cint;
  {* Is there data pending in the output filters?  }
        data_in_output_filters : cint;
  {* Are there any filters that clogg/buffer the input stream, breaking
   *  the event mpm.
   }
        clogging_input_filters: cint;
  {* have we done double-reverse DNS? -1 yes/failure, 0 not yet,
   *  1 yes/success  }
        double_reverse: Cardinal;
  {* Are we still talking?  }
        aborted : Cardinal;
  {* Are we going to keep the connection alive for another request?
   * @see ap_conn_keepalive_e  }
        keepalive : ap_conn_keepalive_e;
  {* How many times have we used it?  }
        keepalives : Integer;
  {* Optional connection log level configuration. May point to a server or
   *  per_dir config, i.e. must be copied before modifying  }
        log : Pap_logconf;
  {* Id to identify this connection in error log. Set when the first
   *  error log entry for this connection is generated.
   }
        log_id : Pchar;
  {* This points to the current thread being used to process this request,
   * over the lifetime of a request, the value may change. Users of the connection
   * record should not rely upon it staying the same between calls that invole
   * the MPM.
   }
//{$if APR_HAS_THREADS}
//        current_thread : Papr_thread_t;
//{$endif}
      end;

  {*
   * @brief A structure that represents the current request
    }
    Prequest_rec = ^request_rec;
    request_rec = record
  {* The pool associated with the request  }
        pool : Papr_pool_t;
  {* The connection to the client  }
        connection : Pconn_rec;
  {* The virtual host for this request  }
        server : Pserver_rec;
  {* Pointer to the redirected request if this is an external redirect  }
        next : Prequest_rec;
  {* Pointer to the previous request if this is an internal redirect  }
        prev : Prequest_rec;
  {* Pointer to the main request if this is a sub-request (see http_request.h)  }
        main : Prequest_rec;

  { Info about the request itself... we begin with stuff that only protocol.c should ever touch...  }
  {* First line of request  }
        the_request : Pchar;
  {* HTTP/0.9, "simple" request (e.g. GET /foo\n w/no headers)  }
        assbackwards : Integer;
  {* A proxy request (calculated during post_read_request/translate_name)
   *  possible values PROXYREQ_NONE, PROXYREQ_PROXY, PROXYREQ_REVERSE,
   *                  PROXYREQ_RESPONSE
   }
        proxyreq : Integer;
  {* HEAD request, as opposed to GET  }
        header_only : Integer;
  {* Protocol version number of protocol; 1.1 = 1001  }
        proto_num : Integer;
  {* Protocol string, as given to us, or HTTP/0.9  }
        protocol : Pchar;
  {* Host, as set by full URI or Host:  }
        hostname : Pchar;
  {* Time when the request started  }
        request_time : apr_time_t;
  {* Status line, if set by script  }
        status_line : Pchar;
  {* Status line  }
        status : Integer;
  { Request method, two ways; also, protocol, etc..  Outside of protocol.c,
       * look, but don't touch.
        }
  {* M_GET, M_POST, etc.  }
        method_number : Integer;
  {* Request method (eg. GET, HEAD, POST, etc.)  }
        method : Pchar;

  {*
   *  'allowed' is a bitvector of the allowed methods.
   *
   *  A handler must ensure that the request method is one that
   *  it is capable of handling.  Generally modules should DECLINE
   *  any request methods they do not handle.  Prior to aborting the
   *  handler like this the handler should set r->allowed to the list
   *  of methods that it is willing to handle.  This bitvector is used
   *  to construct the "Allow:" header required for OPTIONS requests,
   *  and HTTP_METHOD_NOT_ALLOWED and HTTP_NOT_IMPLEMENTED status codes.
   *
   *  Since the default_handler deals with OPTIONS, all modules can
   *  usually decline to deal with OPTIONS.  TRACE is always allowed,
   *  modules don't need to set it explicitly.
   *
   *  Since the default_handler will always handle a GET, a
   *  module which does *not* implement GET should probably return
   *  HTTP_METHOD_NOT_ALLOWED.  Unfortunately this means that a Script GET
   *  handler can't be installed by mod_actions.
   }
        allowed : apr_int64_t;
  {* Array of extension methods  }
        allowed_xmethods : Papr_array_header_t;
  {* List of allowed methods  }
        allowed_methods : Pap_method_list_t;
  {* byte count in stream is for body  }
        sent_bodyct : apr_off_t;
  {* body byte count, for easy access  }
        bytes_sent : apr_off_t;
  {* Last modified time of the requested resource  }
        mtime : apr_time_t;
  { HTTP/1.1 connection-level features  }
  {* The Range: header  }
        range : Pchar;
  {* The "real" content length  }
        clength : apr_off_t;
  {* sending chunked transfer-coding  }
        chunked : Integer;
  {* Method for reading the request body
   * (eg. REQUEST_CHUNKED_ERROR, REQUEST_NO_BODY,
   *  REQUEST_CHUNKED_DECHUNK, etc...)  }
        read_body : Integer;
  {* reading chunked transfer-coding  }
        read_chunked : Integer;
  {* is client waiting for a 100 response?  }
        expecting_100 : Cardinal;
  {* The optional kept body of the request.  }
        kept_body : Papr_bucket_brigade;
  {* For ap_body_to_table(): parsed body  }
  {  XXX: ap_body_to_table has been removed. Remove body_table too or
   * XXX: keep it to reintroduce ap_body_to_table without major bump?  }
        body_table : Papr_table_t;
  {* Remaining bytes left to read from the request body  }
        remaining : apr_off_t;
  {* Number of bytes that have been read  from the request body  }
        read_length : apr_off_t;

  { MIME header environments, in and out.  Also, an array containing
  * environment variables to be passed to subprocesses, so people can
  * write modules to add to that environment.
  *
  * The difference between headers_out and err_headers_out is that the
  * latter are printed even on error, and persist across internal redirects
  * (so the headers printed for ErrorDocument handlers will have them).
  *
  * The 'notes' apr_table_t is for notes from one module to another, with no
  * other set purpose in mind...
  }
  {* MIME header environment from the request  }
        headers_in : Papr_table_t;
  {* MIME header environment for the response  }
        headers_out : Papr_table_t;
  {* MIME header environment for the response, printed even on errors and
   * persist across internal redirects  }
        err_headers_out : Papr_table_t;
  {* Array of environment variables to be used for sub processes  }
        subprocess_env : Papr_table_t;
  {* Notes from one module to another  }
        notes : Papr_table_t;

  { content_type, handler, content_encoding, and all content_languages
  * MUST be lowercased strings.  They may be pointers to static strings;
  * they should not be modified in place.
  }
  {* The content-type for the current request  }
        content_type : Pchar;	{ Break these out --- we dispatch on 'em  }
  {* The handler string that we use to call a handler function  }
        handler : Pchar;	{ What we *really* dispatch on  }
  {* How to encode the data  }
        content_encoding : Pchar;
  {* Array of strings representing the content languages  }
        content_languages : Papr_array_header_t;
  {* variant list validator (if negotiated)  }
        vlist_validator : Pchar;
  {* If an authentication check was made, this gets set to the user name.  }
        user : Pchar;
  {* If an authentication check was made, this gets set to the auth type.  }
        ap_auth_type : Pchar;

  { What object is being requested (either directly, or via include
       * or content-negotiation mapping).
        }
  {* The URI without any parsing performed  }
        unparsed_uri : Pchar;
  {* The path portion of the URI, or "/" if no path provided  }
        uri : Pchar;
  {* The filename on disk corresponding to this response  }
        filename : Pchar;
  { XXX: What does this mean? Please define "canonicalize" -aaron  }
  {* The true filename, we canonicalize r->filename if these don't match  }
        canonical_filename : Pchar;
  {* The PATH_INFO extracted from this request  }
        path_info : Pchar;
  {* The QUERY_ARGS extracted from this request  }
        args : Pchar;

  {*
   * Flag for the handler to accept or reject path_info on
   * the current request.  All modules should respect the
   * AP_REQ_ACCEPT_PATH_INFO and AP_REQ_REJECT_PATH_INFO
   * values, while AP_REQ_DEFAULT_PATH_INFO indicates they
   * may follow existing conventions.  This is set to the
   * user's preference upon HOOK_VERY_FIRST of the fixups.
   }
        used_path_info : Integer;
  {* A flag to determine if the eos bucket has been sent yet  }
        eos_sent : Integer;

  { Various other config info which may change with .htaccess files
  * These are config vectors, with one void* pointer for each module
  * (the thing pointed to being the module's business).
  }
  {* Options set in config files, etc.  }
        per_dir_config : Pap_conf_vector_t;
  {* Notes on *this* request  }
        request_config : Pap_conf_vector_t;
  {* Optional request log level configuration. Will usually point
   *  to a server or per_dir config, i.e. must be copied before
   *  modifying  }
        log : Pap_logconf;
  {* Id to identify request in access and error log. Set when the first
   *  error log entry for this request is generated.
   }
        log_id : Pchar;
  {*
   * A linked list of the .htaccess configuration directives
   * accessed by this request.
   * N.B. always add to the head of the list, _never_ to the end.
   * that way, a sub request's list can (temporarily) point to a parent's list
  }
        htaccess : Phtaccess_result;
  {* A list of output filters to be used for this request  }
        output_filters : Pap_filter_t;
  {* A list of input filters to be used for this request  }
        input_filters : Pap_filter_t;
  {* A list of protocol level output filters to be used for this
   *  request  }
        proto_output_filters : Pap_filter_t;
  {* A list of protocol level input filters to be used for this
   *  request  }
        proto_input_filters : Pap_filter_t;
  {* This response can not be cached  }
        no_cache : Integer;
  {* There is no local copy of this response  }
        no_local_copy : Integer;
  {* Mutex protect callbacks registered with ap_mpm_register_timed_callback
   * from being run before the original handler finishes running
   }
        invoke_mtx : Papr_thread_mutex_t;
  {* A struct containing the components of URI  }
        parsed_uri : apr_uri_t;
  {*  finfo.protection (st_mode) set to zero if no such file  }
        finfo : apr_finfo_t;
  {* remote address information from conn_rec, can be overridden if
   * necessary by a module.
   * This is the address that originated the request.
   }
        useragent_addr : Papr_sockaddr_t;
        useragent_ip : Pchar;
      end;

  {*
   * @defgroup ProxyReq Proxy request types
   *
   * Possible values of request_rec->proxyreq. A request could be normal,
   *  proxied or reverse proxied. Normally proxied and reverse proxied are
   *  grouped together as just "proxied", but sometimes it's necessary to
   *  tell the difference between the two, such as for authentication.
   * @
    }
  

  const
    PROXYREQ_NONE =	0;{*< No proxy  }
    PROXYREQ_PROXY =	1;{*< Standard proxy  }
    PROXYREQ_REVERSE =	2;{*< Reverse proxy  }
    PROXYREQ_RESPONSE =	3;{*< Origin response  }
  { @  }

  {*
   * @brief Enumeration of connection keepalive options
   }
//  type  {moved up}
//    ap_conn_keepalive_e = (AP_CONN_UNKNOWN,AP_CONN_CLOSE,AP_CONN_KEEPALIVE);

  { Per-vhost config...  }
  {*
   * The address 255.255.255.255, when used as a virtualhost address,
   * will become the "default" server when the ip doesn't match other vhosts.
    }
  const
    DEFAULT_VHOST_ADDR = $ffffffff;    

  {*
   * Get the context_document_root for a request. This is a generalization of
   * the document root, which is too limited in the presence of mappers like
   * mod_userdir and mod_alias. The context_document_root is the directory
   * on disk that maps to the context_prefix URI prefix.
   * @param r The request
   * @note For resources that do not map to the file system or for very complex
   * mappings, this information may still be wrong.
    }
function ap_context_document_root(r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_context_document_root' + LibSuff4;

  {*
   * Get the context_prefix for a request. The context_prefix URI prefix
   * maps to the context_document_root on disk.
   * @param r The request
    }
function ap_context_prefix(r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_context_prefix' + LibSuff4;

  {* Set context_prefix and context_document_root for a request.
   * @param r The request
   * @param prefix the URI prefix, without trailing slash
   * @param document_root the corresponding directory on disk, without trailing
   * slash
   * @note If one of prefix of document_root is NULL, the corrsponding
   * property will not be changed.
    }
procedure ap_set_context_info(r: Prequest_rec; const prefix, document_root: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_set_context_info' + LibSuff12;

  {* Set per-request document root. This is for mass virtual hosting modules
   * that want to provide the correct DOCUMENT_ROOT value to scripts.
   * @param r The request
   * @param document_root the document root for the request.
    }
procedure ap_set_document_root(r: Prequest_rec; const document_root: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_set_document_root' + LibSuff8;

  {*
   * Examine a field value (such as a media-/content-type) string and return
   * it sans any parameters; e.g., strip off any ';charset=foo' and the like.
   * @param p Pool to allocate memory from
   * @param intype The field to examine
   * @return A copy of the field minus any parameters
    }
function ap_field_noparam(p: Papr_pool_t; const intype: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_field_noparam' + LibSuff8;

  {*
   * Convert a time from an integer into a string in a specified format
   * @param p The pool to allocate memory from
   * @param t The time to convert
   * @param fmt The format to use for the conversion
   * @param gmt Convert the time for GMT?
   * @return The string that represents the specified time
    }
function ap_ht_time(p: Papr_pool_t; t: apr_time_t; const fmt: PChar; gmt: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_ht_time' + LibSuff20;


  { String handling. The *_nc variants allow you to use non-const char **s as
     arguments (unfortunately C won't automatically convert a char ** to a const
     char **)  }
  {*
   * Get the characters until the first occurance of a specified character
   * @param p The pool to allocate memory from
   * @param line The string to get the characters from
   * @param stop The character to stop at
   * @return A copy of the characters up to the first stop character
    }
function ap_getword(p: Papr_pool_t; const line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword' + LibSuff12;

  {*
   * Get the characters until the first occurance of a specified character
   * @param p The pool to allocate memory from
   * @param line The string to get the characters from
   * @param stop The character to stop at
   * @return A copy of the characters up to the first stop character
   * @note This is the same as ap_getword(), except it doesn't use const char **.
    }
function ap_getword_nc(p: Papr_pool_t; line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_nc' + LibSuff12;

  {*
   * Get the first word from a given string.  A word is defined as all characters
   * up to the first whitespace.
   * @param p The pool to allocate memory from
   * @param line The string to traverse
   * @return The first word in the line
    }
function ap_getword_white(p: Papr_pool_t; const line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_white' + LibSuff8;

  {*
   * Get the first word from a given string.  A word is defined as all characters
   * up to the first whitespace.
   * @param p The pool to allocate memory from
   * @param line The string to traverse
   * @return The first word in the line
   * @note The same as ap_getword_white(), except it doesn't use const char**
    }
function ap_getword_white_nc(p: Papr_pool_t; line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_white_nc' + LibSuff8;

  {*
   * Get all characters from the first occurance of @a stop to the first "\0"
   * @param p The pool to allocate memory from
   * @param line The line to traverse
   * @param stop The character to start at
   * @return A copy of all caracters after the first occurance of the specified
   *         character
    }
function ap_getword_nulls(p: Papr_pool_t; const line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_nulls' + LibSuff12;

  {*
   * Get all characters from the first occurance of @a stop to the first "\0"
   * @param p The pool to allocate memory from
   * @param line The line to traverse
   * @param stop The character to start at
   * @return A copy of all caracters after the first occurance of the specified
   *         character
   * @note The same as ap_getword_nulls(), except it doesn't use const char **.
    }
function ap_getword_nulls_nc(p: Papr_pool_t; line: PPChar; stop: Char): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_nulls_nc' + LibSuff12;

  {*
   * Get the second word in the string paying attention to quoting
   * @param p The pool to allocate from
   * @param line The line to traverse
   * @return A copy of the string
    }
function ap_getword_conf(p: Papr_pool_t; const line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_conf' + LibSuff8;

  {*
   * Get the second word in the string paying attention to quoting
   * @param p The pool to allocate from
   * @param line The line to traverse
   * @return A copy of the string
   * @note The same as ap_getword_conf(), except it doesn't use const char **.
    }
function ap_getword_conf_nc(p: Papr_pool_t; line: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getword_conf_nc' + LibSuff8;

  {*
   * Check a string for any config define or environment variable construct
   * and replace each of them by the value of that variable, if it exists.
   * The default syntax of the constructs is $ENV but can be changed by
   * setting the define::* config defines. If the variable does not exist,
   * leave the $ENV construct alone but print a warning.
   * @param p The pool to allocate from
   * @param word The string to check
   * @return The string with the replaced environment variables
    }
//AP_DECLARE(const char *) ap_resolve_env(apr_pool_t *p, const char * word);
function ap_resolve_env(p: Papr_pool_t; const word_: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_resolve_env' + LibSuff8;

  {*
   * Size an HTTP header field list item, as separated by a comma.
   * @param field The field to size
   * @param len The length of the field
   * @return The return value is a pointer to the beginning of the non-empty
   * list item within the original string (or NULL if there is none) and the
   * address of field is shifted to the next non-comma, non-whitespace
   * character.  len is the length of the item excluding any beginning whitespace.
    }
//AP_DECLARE(const char *) ap_size_list_item(const char **field, int *len);
function ap_size_list_item(const field: PPChar; len: PInteger): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_size_list_item' + LibSuff8;

  {*
   * Retrieve an HTTP header field list item, as separated by a comma,
   * while stripping insignificant whitespace and lowercasing anything not in
   * a quoted string or comment.
   * @param p The pool to allocate from
   * @param field The field to retrieve
   * @return The return value is a new string containing the converted list
   *         item (or NULL if none) and the address pointed to by field is
   *         shifted to the next non-comma, non-whitespace.
    }
function ap_get_list_item(p: Papr_pool_t; const field: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_get_list_item' + LibSuff8;

  {*
   * Find an item in canonical form (lowercase, no extra spaces) within
   * an HTTP field value list.
   * @param p The pool to allocate from
   * @param line The field value list to search
   * @param tok The token to search for
   * @return 1 if found, 0 if not found.
    }
function ap_find_list_item(p: Papr_pool_t; const line, tok: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_list_item' + LibSuff12;

  {*
   * Retrieve a token, spacing over it and adjusting the pointer to
   * the first non-white byte afterwards.  Note that these tokens
   * are delimited by semis and commas and can also be delimited
   * by whitespace at the caller's option.
   * @param p The pool to allocate from
   * @param accept_line The line to retrieve the token from (adjusted afterwards)
   * @param accept_white Is it delimited by whitespace
   * @return the token
    }
function ap_get_token(p: Papr_pool_t; const accept_line: PPChar; accept_white: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_get_token' + LibSuff12;

  {*
   * Find http tokens, see the definition of token from RFC2068
   * @param p The pool to allocate from
   * @param line The line to find the token
   * @param tok The token to find
   * @return 1 if the token is found, 0 otherwise
    }
function ap_find_token(p: Papr_pool_t; const line, tok: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_token' + LibSuff12;

  {*
   * find http tokens from the end of the line
   * @param p The pool to allocate from
   * @param line The line to find the token
   * @param tok The token to find
   * @return 1 if the token is found, 0 otherwise
    }
function ap_find_last_token(p: Papr_pool_t; const line, tok: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_last_token' + LibSuff12;

  {*
   * Check for an Absolute URI syntax
   * @param u The string to check
   * @return 1 if URI, 0 otherwise
    }
function ap_is_url(const u: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_is_url' + LibSuff4;

  {*
   * Unescape a string
   * @param url The string to unescape
   * @return 0 on success, non-zero otherwise
    }
function ap_unescape_all(url: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_unescape_all' + LibSuff4;

  {*
   * Unescape a URL
   * @param url The url to unescape
   * @return 0 on success, non-zero otherwise
    }
function ap_unescape_url(url: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_unescape_url' + LibSuff4;

  {*
   * Unescape a URL, but leaving %2f (slashes) escaped
   * @param url The url to unescape
   * @param decode_slashes Whether or not slashes should be decoded
   * @return 0 on success, non-zero otherwise
    }
function ap_unescape_url_keep2f(url: PChar; decode_slashes: Integer): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_unescape_url_keep2f' + LibSuff8;

  {*
   * Unescape an application/x-www-form-urlencoded string
   * @param query The query to unescape
   * @return 0 on success, non-zero otherwise
    }
function ap_unescape_urlencoded(query: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_unescape_urlencoded' + LibSuff4;

  {*
   * Convert all double slashes to single slashes
   * @param name The string to convert
    }
procedure ap_no2slash(name: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_no2slash' + LibSuff4;

  {*
   * Remove all ./ and xx/../ substrings from a file name. Also remove
   * any leading ../ or /../ substrings.
   * @param name the file name to parse
    }
procedure ap_getparents(name: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_getparents' + LibSuff4;

  {*
   * Escape a path segment, as defined in RFC 1808
   * @param p The pool to allocate from
   * @param s The path to convert
   * @return The converted URL
    }
function ap_escape_path_segment(p: Papr_pool_t; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_path_segment' + LibSuff8;

  {*
   * Escape a path segment, as defined in RFC 1808, to a preallocated buffer.
   * @param c The preallocated buffer to write to
   * @param s The path to convert
   * @return The converted URL (c)
    }
function ap_escape_path_segment_buffer(c: PChar; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_path_segment_buffer' + LibSuff8;

  {*
   * convert an OS path to a URL in an OS dependant way.
   * @param p The pool to allocate from
   * @param path The path to convert
   * @param partial if set, assume that the path will be appended to something
   *        with a '/' in it (and thus does not prefix "./")
   * @return The converted URL
    }
function ap_os_escape_path(p: Papr_pool_t; const path: PChar; partial: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_os_escape_path' + LibSuff12;


  {* @see ap_os_escape_path  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//#define ap_escape_uri(ppool,path) ap_os_escape_path(ppool,path,1)
function ap_escape_uri(ppool: Papr_pool_t; const path: PChar) : PChar;  

{*
 * Escape a string as application/x-www-form-urlencoded
 * @param p The pool to allocate from
 * @param s The path to convert
 * @return The converted URL
  }
function ap_escape_urlencoded(p: Papr_pool_t; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_urlencoded' + LibSuff8;

{*
 * Escape a string as application/x-www-form-urlencoded, to a preallocated buffer
 * @param c The preallocated buffer to write to
 * @param s The path to convert
 * @return The converted URL (c)
  }
function ap_escape_urlencoded_buffer(c: PChar; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_urlencoded_buffer' + LibSuff8;

{*
 * Escape an html string
 * @param p The pool to allocate from
 * @param s The html to escape
 * @return The escaped string
  }
//#define ap_escape_html(p,s) ap_escape_html2(p,s,0)
function ap_escape_html(p: Papr_pool_t; const s: PChar) : PChar;

{*
 * Escape an html string
 * @param p The pool to allocate from
 * @param s The html to escape
 * @param toasc Whether to escape all non-ASCII chars to \&\#nnn;
 * @return The escaped string
  }
function ap_escape_html2(p: Papr_pool_t; const s: PChar; toasc: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_html2' + LibSuff12;

{*
 * Escape a string for logging
 * @param p The pool to allocate from
 * @param str The string to escape
 * @return The escaped string
  }
function ap_escape_logitem(p: Papr_pool_t; const str: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_logitem' + LibSuff8;

{*
 * Escape a string for logging into the error log (without a pool)
 * @param dest The buffer to write to
 * @param source The string to escape
 * @param buflen The buffer size for the escaped string (including "\0")
 * @return The len of the escaped string (always < maxlen)
  }
function ap_escape_errorlog_item(dest: PChar; const source: PChar;
 buflen: apr_size_t): apr_size_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_errorlog_item' + LibSuff12;

{*
 * Construct a full hostname
 * @param p The pool to allocate from
 * @param hostname The hostname of the server
 * @param port The port the server is running on
 * @param r The current request
 * @return The server's hostname
  }
function ap_construct_server(p: Papr_pool_t; const hostname: PChar;
 port: apr_port_t; const r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_construct_server' + LibSuff16;

{*
 * Escape a shell command
 * @param p The pool to allocate from
 * @param s The command to escape
 * @return The escaped shell command
  }
function ap_escape_shell_cmd(p: Papr_pool_t; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_shell_cmd' + LibSuff8;

{*
 * Count the number of directories in a path
 * @param path The path to count
 * @return The number of directories
  }
function ap_count_dirs(const path: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_count_dirs' + LibSuff4;

{*
 * Copy at most @a n leading directories of @a s into @a d. @a d
 * should be at least as large as @a s plus 1 extra byte
 *
 * @param d The location to copy to
 * @param s The location to copy from
 * @param n The number of directories to copy
 * @return value is the ever useful pointer to the trailing "\0" of d
 * @note on platforms with drive letters, n = 0 returns the "/" root,
 * whereas n = 1 returns the "d:/" root.  On all other platforms, n = 0
 * returns the empty string.   }
function ap_make_dirstr_prefix(d: PChar; const s: PChar; n: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_make_dirstr_prefix' + LibSuff12;

{*
 * Return the parent directory name (including trailing /) of the file
 * @a s
 * @param p The pool to allocate from
 * @param s The file to get the parent of
 * @return A copy of the file's parent directory
  }
function ap_make_dirstr_parent(p: Papr_pool_t; const s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_make_dirstr_parent' + LibSuff8;

{*
 * Given a directory and filename, create a single path from them.  This
 * function is smart enough to ensure that there is a single '/' between the
 * directory and file names
 * @param a The pool to allocate from
 * @param dir The directory name
 * @param f The filename
 * @return A copy of the full path
 * @note Never consider using this function if you are dealing with filesystem
 * names that need to remain canonical, unless you are merging an apr_dir_read
 * path and returned filename.  Otherwise, the result is not canonical.
  }
function ap_make_full_path(a: Papr_pool_t; const dir, f: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_make_full_path' + LibSuff12;

{*
 * Test if the given path has an an absolute path.
 * @param p The pool to allocate from
 * @param dir The directory name
 * @note The converse is not necessarily true, some OS's (Win32/OS2/Netware) have
 * multiple forms of absolute paths.  This only reports if the path is absolute
 * in a canonical sense.
  }
function ap_os_is_path_absolute(p: Papr_pool_t; const dir: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_os_is_path_absolute' + LibSuff8;

{*
 * Does the provided string contain wildcard characters?  This is useful
 * for determining if the string should be passed to strcmp_match or to strcmp.
 * The only wildcard characters recognized are '?' and '*'
 * @param str The string to check
 * @return 1 if the string has wildcards, 0 otherwise
  }
function ap_is_matchexp(const str: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_is_matchexp' + LibSuff4;

{*
 * Determine if a string matches a patterm containing the wildcards '?' or '*'
 * @param str The string to check
 * @param expected The pattern to match against
 * @return 0 if the two strings match, 1 otherwise
  }
function ap_strcmp_match(const str, expected: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_strcmp_match' + LibSuff8;

{*
 * Determine if a string matches a patterm containing the wildcards '?' or '*',
 * ignoring case
 * @param str The string to check
 * @param expected The pattern to match against
 * @return 0 if the two strings match, 1 otherwise
  }
function ap_strcasecmp_match(const str, expected: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_strcasecmp_match' + LibSuff8;

{*
 * Find the first occurrence of the substring s2 in s1, regardless of case
 * @param s1 The string to search
 * @param s2 The substring to search for
 * @return A pointer to the beginning of the substring
 * @remark See apr_strmatch() for a faster alternative
  }
function ap_strcasestr(const s1, s2: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_strcasestr' + LibSuff8;

{*
 * Return a pointer to the location inside of bigstring immediately after prefix
 * @param bigstring The input string
 * @param prefix The prefix to strip away
 * @return A pointer relative to bigstring after prefix
  }
function ap_stripprefix(const bigstring, prefix: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_stripprefix' + LibSuff8;

{*
 * Decode a base64 encoded string into memory allocated from a pool
 * @param p The pool to allocate from
 * @param bufcoded The encoded string
 * @return The decoded string
  }
function ap_pbase64decode(p: Papr_pool_t; const bufcoded: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pbase64decode' + LibSuff8;

{*
 * Encode a string into memory allocated from a pool in base 64 format
 * @param p The pool to allocate from
 * @param string The plaintext string
 * @return The encoded string
  }
//AP_DECLARE(char *) ap_pbase64encode(apr_pool_t *p, char *string);
function ap_pbase64encode(p: Papr_pool_t; string_: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pbase64encode' + LibSuff8;

{*
 * Compile a regular expression to be used later. The regex is freed when
 * the pool is destroyed.
 * @param p The pool to allocate from
 * @param pattern the regular expression to compile
 * @param cflags The bitwise or of one or more of the following:
 *   @li REG_EXTENDED - Use POSIX extended Regular Expressions
 *   @li REG_ICASE    - Ignore case
 *   @li REG_NOSUB    - Support for substring addressing of matches
 *       not required
 *   @li REG_NEWLINE  - Match-any-character operators don't match new-line
 * @return The compiled regular expression
  }
function ap_pregcomp(p: Papr_pool_t; const pattern: PChar; cflags: Integer): Pap_regex_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pregcomp' + LibSuff12;

{*
 * Free the memory associated with a compiled regular expression
 * @param p The pool the regex was allocated from
 * @param reg The regular expression to free
 * @note This function is only necessary if the regex should be cleaned
 * up before the pool
  }
procedure ap_pregfree(p: Papr_pool_t; reg: Pap_regex_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pregfree' + LibSuff8;

{*
 * After performing a successful regex match, you may use this function to
 * perform a series of string substitutions based on subexpressions that were
 * matched during the call to ap_regexec. This function is limited to
 * result strings of 64K. Consider using ap_pregsub_ex() instead.
 * @param p The pool to allocate from
 * @param input An arbitrary string containing $1 through $9.  These are
 *              replaced with the corresponding matched sub-expressions
 * @param source The string that was originally matched to the regex
 * @param nmatch the nmatch returned from ap_pregex
 * @param pmatch the pmatch array returned from ap_pregex
 * @return The substituted string, or NULL on error
  }
//AP_DECLARE(char *) ap_pregsub(apr_pool_t *p, const char *input,
//                              const char *source, apr_size_t nmatch,
//                              ap_regmatch_t pmatch[]);
function ap_pregsub(p: Papr_pool_t; const input, source: PChar;
 nmatch: apr_size_t; pmatch: array of ap_regmatch_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pregsub' + LibSuff20;

{*
 * After performing a successful regex match, you may use this function to
 * perform a series of string substitutions based on subexpressions that were
 * matched during the call to ap_regexec
 * @param p The pool to allocate from
 * @param result where to store the result, will be set to NULL on error
 * @param input An arbitrary string containing $1 through $9.  These are
 *              replaced with the corresponding matched sub-expressions
 * @param source The string that was originally matched to the regex
 * @param nmatch the nmatch returned from ap_pregex
 * @param pmatch the pmatch array returned from ap_pregex
 * @param maxlen the maximum string length to return, 0 for unlimited
 * @return The substituted string, or NULL on error
  }
//AP_DECLARE(apr_status_t) ap_pregsub_ex(apr_pool_t *p, char **result,
//                                       const char *input, const char *source,
//                                       apr_size_t nmatch,
//                                       ap_regmatch_t pmatch[],
//                                       apr_size_t maxlen);
function ap_pregsub_ex(p: Papr_pool_t; result_: PPChar; const input, source: PChar;
                                       nmatch: apr_size_t;
                                       pmatch: array of ap_regmatch_t;
                                       maxlen: apr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pregsub_ex' + LibSuff28;

{*
 * We want to downcase the type/subtype for comparison purposes
 * but nothing else because ;parameter=foo values are case sensitive.
 * @param s The content-type to convert to lowercase
  }
procedure ap_content_type_tolower(s: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_content_type_tolower' + LibSuff4;

{*
 * convert a string to all lowercase
 * @param s The string to convert to lowercase
  }
procedure ap_str_tolower(s: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_str_tolower' + LibSuff4;

{*
 * convert a string to all uppercase
 * @param s The string to convert to uppercase
  }
procedure ap_str_toupper(s: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_str_toupper' + LibSuff4;

{*
 * Search a string from left to right for the first occurrence of a
 * specific character
 * @param str The string to search
 * @param c The character to search for
 * @return The index of the first occurrence of c in str
  }
//AP_DECLARE(int) ap_ind(const char *str, char c);        /* Sigh... */
function ap_ind(const str: PChar; c: Char): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_ind' + LibSuff8;

{*
 * Search a string from right to left for the first occurrence of a
 * specific character
 * @param str The string to search
 * @param c The character to search for
 * @return The index of the first occurrence of c in str
  }
function ap_rind(const str: PChar; c: Char): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_rind' + LibSuff8;

{*
 * Given a string, replace any bare &quot; with \\&quot; .
 * @param p The pool to allocate memory from
 * @param instring The string to search for &quot;
 * @return A copy of the string with escaped quotes
  }
function ap_escape_quotes(p: Papr_pool_t; const instring: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_escape_quotes' + LibSuff8;

{*
 * Given a string, append the PID deliminated by delim.
 * Usually used to create a pid-appended filepath name
 * (eg: /a/b/foo -> /a/b/foo.6726). A function, and not
 * a macro, to avoid unistd.h dependency
 * @param p The pool to allocate memory from
 * @param string The string to append the PID to
 * @param delim The string to use to deliminate the string from the PID
 * @return A copy of the string with the PID appended
  }
function ap_append_pid(p: Papr_pool_t; const string_, delim: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_append_pid' + LibSuff12;

{*
 * Parse a given timeout parameter string into an apr_interval_time_t value.
 * The unit of the time interval is given as postfix string to the numeric
 * string. Currently the following units are understood:
 *
 * ms    : milliseconds
 * s     : seconds
 * mi[n] : minutes
 * h     : hours
 *
 * If no unit is contained in the given timeout parameter the default_time_unit
 * will be used instead.
 * @param timeout_parameter The string containing the timeout parameter.
 * @param timeout The timeout value to be returned.
 * @param default_time_unit The default time unit to use if none is specified
 * in timeout_parameter.
 * @return Status value indicating whether the parsing was successful or not.
  }
function ap_timeout_parameter_parse(const timeout_parameter: PChar;
                                    timeout: Papr_interval_time_t;
                                    const default_time_unit: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_timeout_parameter_parse' + LibSuff12;

{*
 * Determine if a request has a request body or not.
 *
 * @param r the request_rec of the request
 * @return truth value
  }
function ap_request_has_body(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_request_has_body' + LibSuff4;

{*
 * Cleanup a string (mainly to be filesystem safe)
 * We only allow '_' and alphanumeric chars. Non-printable
 * map to 'x' and all others map to '_'
 *
 * @param  p pool to use to allocate dest
 * @param  src string to clean up
 * @param  dest cleaned up, allocated string
 * @return Status value indicating whether the cleaning was successful or not.
  }
function ap_pstr2_alnum(p: Papr_pool_t; const src: PChar;
                                        const dest: PPChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pstr2_alnum' + LibSuff12;

{*
 * Cleanup a string (mainly to be filesystem safe)
 * We only allow '_' and alphanumeric chars. Non-printable
 * map to 'x' and all others map to '_'
 *
 * @param  src string to clean up
 * @param  dest cleaned up, pre-allocated string
 * @return Status value indicating whether the cleaning was successful or not.
  }
function ap_str2_alnum(const src: PChar; dest: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_str2_alnum' + LibSuff8;

{*
 * Structure to store the contents of an HTTP form of the type
 * application/x-www-form-urlencoded.
 *
 * Currently it contains the name as a char* of maximum length
 * HUGE_STRING_LEN, and a value in the form of a bucket brigade
 * of arbitrary length.
  }
(* Const before type ignored *)

type
  ap_form_pair_t = record
      name : PChar;
      value : Papr_bucket_brigade;
    end;
{*
 * Read the body and parse any form found, which must be of the
 * type application/x-www-form-urlencoded.
 * @param r request containing POSTed form data
 * @param f filter
 * @param ptr returned array of ap_form_pair_t
 * @param num max num of params or -1 for unlimited
 * @param size max size allowed for parsed data
 * @return OK or HTTP error
  }
//AP_DECLARE(int) ap_parse_form_data(request_rec *r, struct ap_filter_t *f,
//                                   apr_array_header_t **ptr,
//                                   apr_size_t num, apr_size_t size);
function ap_parse_form_data(r: Prequest_rec; f: Pap_filter_t;
                            ptr: PPapr_array_header_t;
                            num: apr_size_t; size: apr_size_t): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_parse_form_data' + LibSuff20;

{ Misc system hackery  }
{*
 * Given the name of an object in the file system determine if it is a directory
 * @param p The pool to allocate from
 * @param name The name of the object to check
 * @return 1 if it is a directory, 0 otherwise
  }
function ap_is_rdirectory(p: Papr_pool_t; const name: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_is_rdirectory' + LibSuff8;

{*
 * Given the name of an object in the file system determine if it is a directory - this version is symlink aware
 * @param p The pool to allocate from
 * @param name The name of the object to check
 * @return 1 if it is a directory, 0 otherwise
  }
function ap_is_directory(p: Papr_pool_t; const name: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_is_directory' + LibSuff8;

//#ifdef _OSD_POSIX
//extern int os_init_job_environment(server_rec *s, const char *user_name, int one_process);
//#endif /* _OSD_POSIX */
//{$ifdef _OSD_POSIX}
//function os_init_job_environment(s:Pserver_rec; const user_name:Pchar; one_process:integer):integer;cdecl;
//{$endif} { _OSD_POSIX  }

{*
 * Determine the local host name for the current machine
 * @param p The pool to allocate from
 * @return A copy of the local host name
  }
//char *ap_get_local_host(apr_pool_t *p);

{*
 * Log an assertion to the error log
 * @param szExp The assertion that failed
 * @param szFile The file the assertion is in
 * @param nLine The line the assertion is defined on
  }
procedure ap_log_assert(const szExp, szFile: PChar; nLine: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_log_assert' + LibSuff12;

{*
 * @internal Internal Assert function
  }
{#define ap_assert(exp) ((exp) ? (void)0 : ap_log_assert(#exp,__FILE__,__LINE__)) }
{*
 * Redefine assert() to something more useful for an Apache...
 *
 * Use ap_assert() if the condition should always be checked.
 * Use AP_DEBUG_ASSERT() if the condition should only be checked when AP_DEBUG
 * is defined.
  }
{#ifdef AP_DEBUG
#define AP_DEBUG_ASSERT(exp) ap_assert(exp)
#else
#define AP_DEBUG_ASSERT(exp) ((void)0)
#endif }
{*
 * @defgroup stopsignal Flags which indicate places where the server should stop for debugging.
 * @
 * A set of flags which indicate places where the server should raise(SIGSTOP).
 * This is useful for debugging, because you can then attach to that process
 * with gdb and continue.  This is important in cases where one_process
 * debugging isn't possible.
  }
{* stop on a Detach  }
{#define SIGSTOP_DETACH                  1 }
{* stop making a child process  }
{#define SIGSTOP_MAKE_CHILD              2 }
{* stop spawning a child process  }
{#define SIGSTOP_SPAWN_CHILD             4 }
{* stop spawning a child process with a piped log  }
{#define SIGSTOP_PIPED_LOG_SPAWN         8 }
{* stop spawning a CGI child process  }
{#define SIGSTOP_CGI_CHILD               16 }
{* Macro to get GDB started  }
{#ifdef DEBUG_SIGSTOP
extern int raise_sigstop_flags;
#define RAISE_SIGSTOP(x) do  if (raise_sigstop_flags & SIGSTOP_##x) raise(SIGSTOP);  while (0)
#else
#define RAISE_SIGSTOP(x)
#endif }
{* @  }

{*
 * Get HTML describing the address and (optionally) admin of the server.
 * @param prefix Text which is prepended to the return value
 * @param r The request_rec
 * @return HTML describing the server, allocated in @a r's pool.
  }
function ap_psignature(const prefix: PChar; r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_psignature' + LibSuff8;


{* strtoul does not exist on sunos4.  }
{$ifdef strtoul}
{$undef strtoul}
{$endif}

///** strtoul does not exist on sunos4. */
//#ifdef strtoul
//#undef strtoul
//#endif
//#define strtoul strtoul_is_not_a_portable_function_use_strtol_instead

{ The C library has functions that allow const to be silently dropped ...
     these macros detect the drop in maintainer mode, but use the native
     methods for normal builds

     Note that on some platforms (e.g., AIX with gcc, Solaris with gcc), string.h needs
     to be included before the macros are defined or compilation will fail.
   }
{#include <string.h>

AP_DECLARE(char *) ap_strchr(char *s, int c);
AP_DECLARE(const char *) ap_strchr_c(const char *s, int c);
AP_DECLARE(char *) ap_strrchr(char *s, int c);
AP_DECLARE(const char *) ap_strrchr_c(const char *s, int c);
AP_DECLARE(char *) ap_strstr(char *s, const char *c);
AP_DECLARE(const char *) ap_strstr_c(const char *s, const char *c);

#ifdef AP_DEBUG

#undef strchr
# define strchr(s, c)  ap_strchr(s,c)
#undef strrchr
# define strrchr(s, c) ap_strrchr(s,c)
#undef strstr
# define strstr(s, c)  ap_strstr(s,c)

#else
 }
{* use this instead of strchr  }
{# define ap_strchr(s, c)     strchr(s, c) }
{* use this instead of strchr  }
{# define ap_strchr_c(s, c)   strchr(s, c) }
{* use this instead of strrchr  }
{# define ap_strrchr(s, c)    strrchr(s, c) }
{* use this instead of strrchr  }
{# define ap_strrchr_c(s, c)  strrchr(s, c) }
{* use this instead of strrstr }
{# define ap_strstr(s, c)     strstr(s, c) }
{* use this instead of strrstr }
{# define ap_strstr_c(s, c)   strstr(s, c) }
{#endif }

{*
 * Generate pseudo random bytes.
 * This is a convenience interface to apr_random. It is cheaper but less
 * secure than apr_generate_random_bytes().
 * @param buf where to store the bytes
 * @param size number of bytes to generate
 * @note ap_random_insecure_bytes() is thread-safe, it uses a mutex on
 *       threaded MPMs.
  }
procedure ap_random_insecure_bytes(buf: Pointer; size: apr_size_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_random_insecure_bytes' + LibSuff8;

{*
 * Get a pseudo random number in a range.
 * @param min low end of range
 * @param max high end of range
 * @return a number in the range
  }
function ap_random_pick(min, max: apr_uint32_t): apr_uint32_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_random_pick' + LibSuff8;

{*
 * Abort with a error message signifying out of memory
  }
//AP_DECLARE(void) ap_abort_on_oom(void) __attribute__((noreturn));
procedure ap_abort_on_oom;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_abort_on_oom' + LibSuff0;

{*
 * Wrapper for malloc() that calls ap_abort_on_oom() if out of memory
 * @param size size of the memory block
 * @return pointer to the allocated memory
 * @note ap_malloc may be implemented as a macro
  }
function ap_malloc(size: size_t): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_malloc' + LibSuff4;

{*
 * Wrapper for calloc() that calls ap_abort_on_oom() if out of memory
 * @param nelem number of elements to allocate memory for
 * @param size size of a single element
 * @return pointer to the allocated memory
 * @note ap_calloc may be implemented as a macro
  }
//AP_DECLARE(void *) ap_calloc(size_t nelem, size_t size)
//                   __attribute__((malloc))
//                   AP_FN_ATTR_ALLOC_SIZE2(1,2);

{*
 * Wrapper for realloc() that calls ap_abort_on_oom() if out of memory
 * @param ptr pointer to the old memory block (or NULL)
 * @param size new size of the memory block
 * @return pointer to the reallocated memory
 * @note ap_realloc may be implemented as a macro
  }
//AP_DECLARE(void *) ap_realloc(void *ptr, size_t size)

//#define AP_NORESTART APR_OS_START_USEERR + 1

{ !APACHE_HTTPD_H  }
{* @ //APACHE Daemon       }
{* @ //APACHE Core         }
{* @ //APACHE super group  }

