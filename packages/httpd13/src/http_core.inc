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

{****************************************************************
 *
 * The most basic server code is encapsulated in a single module
 * known as the core, which is just *barely* functional enough to
 * serve documents, though not terribly well.
 *
 * Largely for NCSA back-compatibility reasons, the core needs to
 * make pieces of its config structures available to other modules.
 * The accessors are declared here, along with the interpretation
 * of one of them (allow_options).
 }

const
  OPT_NONE = 0;
  OPT_INDEXES = 1;
  OPT_INCLUDES = 2;
  OPT_SYM_LINKS = 4;
  OPT_EXECCGI = 8;
  OPT_UNSET = 16;
  OPT_INCNOEXEC = 32;
  OPT_SYM_OWNER = 64;
  OPT_MULTI = 128;
  OPT_ALL = (OPT_INDEXES or OPT_INCLUDES or OPT_SYM_LINKS or OPT_EXECCGI);

{ options for get_remote_host() }
{ REMOTE_HOST returns the hostname, or NULL if the hostname
 * lookup fails.  It will force a DNS lookup according to the
 * HostnameLookups setting.
 }
  REMOTE_HOST = (0);

{ REMOTE_NAME returns the hostname, or the dotted quad if the
 * hostname lookup fails.  It will force a DNS lookup according
 * to the HostnameLookups setting.
 }
  REMOTE_NAME = (1);

{ REMOTE_NOLOOKUP is like REMOTE_NAME except that a DNS lookup is
 * never forced.
 }
  REMOTE_NOLOOKUP = (2);

{ REMOTE_DOUBLE_REV will always force a DNS lookup, and also force
 * a double reverse lookup, regardless of the HostnameLookups
 * setting.  The result is the (double reverse checked) hostname,
 * or NULL if any of the lookups fail.
 }
  REMOTE_DOUBLE_REV = (3);

  SATISFY_ALL = 0;
  SATISFY_ANY = 1;
  SATISFY_NOSPEC = 2;

{ default maximum of internal redirects }
  AP_DEFAULT_MAX_INTERNAL_REDIRECTS = 20;

{ default maximum subrequest nesting level }
  AP_DEFAULT_MAX_SUBREQ_DEPTH = 20;

function ap_allow_options(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_allow_overrides(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_default_type(r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_document_root(r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;
                                      { Don't use this!  If your request went
				      * through a Userdir, or something like
				      * that, it'll screw you.  But it's
				      * back-compatible...
                                      }
                                      
function ap_get_remote_host(conn: Pconn_rec; dir_config: Pointer; type_: cint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_remote_logname(r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Used for constructing self-referencing URLs, and things like SERVER_PORT,
 * and SERVER_NAME.
 }
function ap_construct_url(p: Ppool; const uri: PChar; r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_server_name(r: Prequest_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_server_port(const r: Prequest_rec): cuint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_limit_req_body(const r: Prequest_rec): culong;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_custom_response(r: Prequest_rec; status: cint; string_: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_exists_config_define(name: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Check if the current request is beyond the configured max. number of redirects or subrequests
 * @param r The current request
 * @return true (is exceeded) or false
 }
function ap_is_recursion_limit_exceeded(const r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Authentication stuff.  This is one of the places where compatibility
 * with the old config files *really* hurts; they don't discriminate at
 * all between different authentication schemes, meaning that we need
 * to maintain common state for all of them in the core, and make it
 * available to the other modules through interfaces.
 }
    
type
  require_line = record
    method_mask: cint;
    requirement: PChar;
  end;
     
{API_EXPORT(const char *) ap_auth_type (request_rec *);
API_EXPORT(const char *) ap_auth_name (request_rec *);     
API_EXPORT(const char *) ap_auth_nonce (request_rec *);
API_EXPORT(int) ap_satisfies (request_rec *r);
API_EXPORT(const array_header *) ap_requires (request_rec *);    }

{$ifdef WINDOWS}
{
 * CGI Script stuff for Win32...
 }
type
  file_type_e = ( eFileTypeUNKNOWN, eFileTypeBIN, eFileTypeEXE16, eFileTypeEXE32,
   eFileTypeSCRIPT, eCommandShell16, eCommandShell32 );
  interpreter_source_e = ( INTERPRETER_SOURCE_UNSET, INTERPRETER_SOURCE_REGISTRY,
   INTERPRETER_SOURCE_SHEBANG );

function ap_get_win32_interpreter(const param1: Prequest_rec; param2: PPChar): file_type_e;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;
{$endif}

{.$ifdef CORE_PRIVATE}

{
 * Core is also unlike other modules in being implemented in more than
 * one file... so, data structures are declared here, even though most of
 * the code that cares really is in http_core.c.  Also, another accessor.
 }

//API_EXPORT(char *) ap_response_code_string (request_rec *r, int error_index);

//extern API_VAR_EXPORT module core_module;

{ Per-directory configuration }

type
  allow_options_t = Char;
  overrides_t = Char
  ;
{
 * Bits of info that go into making an ETag for a file
 * document.  Why a long?  Because char historically
 * proved too short for Options, and int can be different
 * sizes on different platforms.
 }
  etag_components_t = culong;

const
  ETAG_UNSET = 0;
  ETAG_NONE  = (1 shl 0);
  ETAG_MTIME = (1 shl 1);
  ETAG_INODE = (1 shl 2);
  ETAG_SIZE  = (1 shl 3);
  ETAG_BACKWARD = (ETAG_MTIME or ETAG_INODE or ETAG_SIZE);
  ETAG_ALL   = (ETAG_MTIME or ETAG_INODE or ETAG_SIZE);
  
  { Things moved up }
  
  HOSTNAME_LOOKUP_OFF	= 0;
  HOSTNAME_LOOKUP_ON	= 1;
  HOSTNAME_LOOKUP_DOUBLE= 2;
  HOSTNAME_LOOKUP_UNSET	= 3;

  USE_CANONICAL_NAME_OFF  = (0);
  USE_CANONICAL_NAME_ON   = (1);
  USE_CANONICAL_NAME_DNS  = (2);
  USE_CANONICAL_NAME_UNSET= (3);

  ADD_DEFAULT_CHARSET_OFF   = (0);
  ADD_DEFAULT_CHARSET_ON    = (1);
  ADD_DEFAULT_CHARSET_UNSET = (2);

{$ifdef CHARSET_EBCDIC}

  { Configurable EBCDIC Conversion stuff }
  { Direction specific conversion: }
  dir_Out = 0;               { 0utput (returned contents in a GET or POST) }
  dir_In  = 1;               { 1nput  (uploaded contents in a PUT / POST) }

  { Conversion Enabled/Disabled: }
  conv_Unset = '?';          { Conversion unconfigured }
  conv_Off   = '0';          { BINARY or ASCII file (no conversion) }
  conv_On    = '1';          { TEXT file (EBCDIC->ASCII for dir_Out; ASCII->EBCDIC for dir_In) }

  LEGACY_KLUDGE = 1; { After a couple of versions this legacy kludge should be set to 0 }
  ASCIITEXT_MAGIC_TYPE_PREFIX = 'text/x-ascii-';     { Text files whose content-type starts with this are passed thru unconverted }

{$endif}

type
  ap_flag_e = (
    AP_FLAG_UNSET = 0,
    AP_FLAG_ON = 1,
    AP_FLAG_OFF = 2
  );
  
  server_signature_t = ( srv_sig_unset, srv_sig_off, srv_sig_on,
   srv_sig_withmail);


  core_dir_config = record
    { path of the directory/regex/etc.  see also d_is_fnmatch below }
    d: PChar;
    { the number of slashes in d }
    d_components: cuint;

    { If (opts & OPT_UNSET) then no absolute assignment to options has
     * been made.
     * invariant: (opts_add & opts_remove) == 0
     * Which said another way means that the last relative (options + or -)
     * assignment made to each bit is recorded in exactly one of opts_add
     * or opts_remove.
     }
    opts: allow_options_t;
    opts_add: allow_options_t;
    opts_remove: allow_options_t;
    override_: overrides_t;
    
    { MIME typing --- the core doesn't do anything at all with this,
     * but it does know what to slap on a request for a document which
     * goes untyped by other mechanisms before it slips out the door...
     }
    
    ap_default_type: PChar;
  
    { Authentication stuff.  Groan... }
    
    satisfy: cint;
    ap_auth_type: PChar;
    ap_auth_name: PChar;
    ap_requires: Parray_header;

    { Custom response config. These can contain text or a URL to redirect to.
     * if response_code_strings is NULL then there are none in the config,
     * if it's not null then it's allocated to sizeof(char*)*RESPONSE_CODES.
     * This lets us do quick merges in merge_core_dir_configs().
     }
  
    response_code_strings: PPChar; { from ErrorDocument, not from
                                   * ap_custom_response()
                                   }

    { Hostname resolution etc }
    
    { Moved Up }

//    unsigned int hostname_lookups : 4;

//    signed int do_rfc1413 : 2;   { See if client is advertising a username? }

//    signed int content_md5 : 2;  { calculate Content-MD5? }

    { Moved Up }

//    unsigned use_canonical_name : 2;

    { since is_fnmatch(conf->d) was being called so frequently in
     * directory_walk() and its relatives, this field was created and
     * is set to the result of that call.
     }
//    unsigned d_is_fnmatch : 1;

    { should we force a charset on any outgoing parameterless content-type?
     * if so, which charset?
     }
     
    { Moved up }

//    unsigned add_default_charset : 2;
    add_default_charset_name: PChar;

    { System Resource Control }
{$ifdef RLIMIT_CPU}
    limit_cpu: Prlimit;
{$endif}
{$if defined(RLIMIT_DATA) or defined(RLIMIT_VMEM) or defined(RLIMIT_AS)}
    limit_mem: Prlimit;
{$endif}
{$ifdef RLIMIT_NPROC}
    limit_nproc: Prlimit;
{$endif}
    limit_req_body: culong;  { limit on bytes in request msg body }

    { logging options }
    server_signature: server_signature_t;
    loglevel: cint;
    
    { Access control }
    sec: Parray_header;
    r: Pregex_t;

{$ifdef WINDOWS}
    { Where to find interpreter to run scripts }
    script_interpreter_source: interpreter_source_e;
{$endif}
    
{$ifdef CHARSET_EBCDIC}

    { Moved up }

    { The configuration args {On|Off}[={In|Out|InOut}] are currently stored
     * as character strings ("0" = conv_Off, "1" = conv_On)
     }
    ebcdicconversion_by_ext_in: Ptable;
    ebcdicconversion_by_ext_out: Ptable;
    ebcdicconversion_by_type_in: Ptable;
    ebcdicconversion_by_type_out: Ptable;

    { Moved up }
    
    x_ascii_magic_kludge: cint;   { whether to handle the text/x-ascii- kludge }

{$ifdef ADD_EBCDICCONVERT_DEBUG_HEADER}
    ebcdicconversion_debug_header: cint; { whether to add an X-EBCDIC-Debug-{In,Out} header to the response }
{$endif}
{$endif} { CHARSET_EBCDIC }

    {
     * What attributes/data should be included in ETag generation?
     }
    etag_bits: etag_components_t;
    etag_add: etag_components_t;
    etag_remove: etag_components_t;

    {
     * Do we allow ISINDEX CGI scripts to pass their query argument as
     * direct command line parameters or argv elements?
     }
    cgi_command_args: ap_flag_e;

    { Digest auth. }
    ap_auth_nonce: PChar;

  end;

{ Per-server core configuration }

type
  core_server_config = record
  
{$ifdef GPROF}
    gprof_dir: PChar;
{$endif}

    { Name translations --- we want the core to be able to do *something*
     * so it's at least a minimally functional web server on its own (and
     * can be tested that way).  But let's keep it to the bare minimum:
     }
    ap_document_root: PChar;
  
    { Access control }

    access_name: PChar;
    sec: Parray_header;
    sec_url: Parray_header;

    { recursion backstopper }
    recursion_limit_set: cint; { boolean }
    redirect_limit: cint;      { maximum number of internal redirects }
    subreq_limit: cint;        { maximum nesting level of subrequests }

    { TRACE control }
    trace_enable: cint;        { see AP_TRACE_ below }

  end;

{ trace_enable options }
const
  AP_TRACE_UNSET    =-1;
  AP_TRACE_DISABLE  = 0;
  AP_TRACE_ENABLE   = 1;
  AP_TRACE_EXTENDED = 2;

{ for http_config.c }
//CORE_EXPORT(void) ap_core_reorder_directories(pool *, server_rec *);

{ for mod_perl }
//CORE_EXPORT(void) ap_add_per_dir_conf (server_rec *s, void *dir_config);
//CORE_EXPORT(void) ap_add_per_url_conf (server_rec *s, void *url_config);
//CORE_EXPORT(void) ap_add_file_conf(core_dir_config *conf, void *url_config);
//CORE_EXPORT_NONSTD(const char *) ap_limit_section (cmd_parms *cmd, void *dummy, const char *arg);

{.$endif}

