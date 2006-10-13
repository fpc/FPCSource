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
 * The central data structures around here...
 }

{ Command dispatch structures... }

{ Note that for all of these except RAW_ARGS, the config routine is
 * passed a freshly allocated string which can be modified or stored
 * or whatever... it's only necessary to do pstrdup() stuff with
 * RAW_ARGS.
 }
type
  cmd_how = (
    RAW_ARGS,			{ cmd_func parses command line itself }
    TAKE1,			{ one argument only }
    TAKE2,			{ two arguments only }
    ITERATE,			{ one argument, occuring multiple times
				 * (e.g., IndexIgnore)
                                 }
    ITERATE2,			{ two arguments, 2nd occurs multiple times
				 * (e.g., AddIcon)
                                 }
    FLAG,			{ One of 'On' or 'Off' }
    NO_ARGS,			{ No args at all, e.g. </Directory> }
    TAKE12,			{ one or two arguments }
    TAKE3,			{ three arguments only }
    TAKE23,			{ two or three arguments }
    TAKE123,			{ one, two or three arguments }
    TAKE13			{ one or three arguments }
  );

  cs_func_t = function (): PChar;
  
  command_struct = record
    name: PChar;		{ Name of this command }
    func: cs_func_t;       	{ Function invoked }
    cmd_data: Pointer;		{ Extra data, for functions which
				 * implement multiple commands...
                                 }
    req_override: cint;		{ What overrides need to be allowed to
				 * enable this command.
                                 }
    args_how: cmd_how;  	{ What the command expects as arguments }

    errmsg: PChar;		{ 'usage' message, in case of syntax errors }
  end;

  command_rec = command_struct;

  Pcommand_rec = ^command_rec;

{ The allowed locations for a configuration directive are the union of
 * those indicated by each set bit in the req_override mask.
 *
 * (req_override & RSRC_CONF)   => *.conf outside <Directory> or <Location>
 * (req_override & ACCESS_CONF) => *.conf inside <Directory> or <Location>
 * (req_override & OR_AUTHCFG)  => *.conf inside <Directory> or <Location>
 *                                 and .htaccess when AllowOverride AuthConfig
 * (req_override & OR_LIMIT)    => *.conf inside <Directory> or <Location>
 *                                 and .htaccess when AllowOverride Limit
 * (req_override & OR_OPTIONS)  => *.conf anywhere
 *                                 and .htaccess when AllowOverride Options
 * (req_override & OR_FILEINFO) => *.conf anywhere
 *                                 and .htaccess when AllowOverride FileInfo
 * (req_override & OR_INDEXES)  => *.conf anywhere
 *                                 and .htaccess when AllowOverride Indexes
 }
const
  OR_NONE = 0;
  OR_LIMIT = 1;
  OR_OPTIONS = 2;
  OR_FILEINFO = 4;
  OR_AUTHCFG = 8;
  OR_INDEXES = 16;
  OR_UNSET = 32;
  ACCESS_CONF = 64;
  RSRC_CONF = 128;
  OR_ALL = (OR_LIMIT or OR_OPTIONS or OR_FILEINFO or OR_AUTHCFG or OR_INDEXES);

{ This can be returned by a function if they don't wish to handle
 * a command. Make it something not likely someone will actually use
 * as an error code.
 }

  DECLINE_CMD = '\a\b';

{
 * This structure is passed to a command which is being invoked,
 * to carry a large variety of miscellaneous data which is all of
 * use to *somebody*...
 }

type
  cmd_parms = record
    info: Pointer;		{ Argument to command from cmd_table }
    override: cint;		{ Which allow-override bits are set }
    limited: cint;		{ Which methods are <Limit>ed }

    config_file: Pconfigfile_t;	{ Config file structure from pcfg_openfile() }

    pool: Pap_pool;		{ Pool to allocate new storage in }
    temp_pool: Ppool;  	        { Pool for scratch memory; persists during
				 * configuration, but wiped before the first
				 * request is served...
                                 }
    server: Pserver_rec;	{ Server_rec being configured for }
    path: PChar;	        { If configuring for a directory,
				 * pathname of that directory.
				 * NOPE!  That's what it meant previous to the
				 * existance of <Files>, <Location> and regex
				 * matching.  Now the only usefulness that can
				 * be derived from this field is whether a command
				 * is being called in a server context (path == NULL)
				 * or being called in a dir context (path != NULL).
                                 }
    cmd: Pcommand_rec;	        { configuration command }
    end_token: PChar;  	        { end token required to end a nested section }
    context: Pointer;		{ per_dir_config vector passed
				 * to handle_command }
  end;

{ This structure records the existence of handlers in a module... }

  handler_t = function (param1: Prequest_rec): Integer; cdecl;

  Phandler_rec = ^handler_rec;
  
  handler_rec = record
    content_type: PChar;	{ MUST be all lower case }
    handler: handler_t;
  end;

{
 * Module structures.  Just about everything is dispatched through
 * these, directly or indirectly (through the command and handler
 * tables).
 }

  Pmodule_struct = ^module_struct;
  
{$ifdef ULTRIX_BRAIN_DEATH}
    init_t = procedure (); cdecl;
    create_dir_config_t = function (): Pointer; cdecl;
    merge_dir_config_t = function (): Pointer; cdecl;
    create_server_config_t = function (): Pointer; cdecl;
    merge_server_config_t = function (): Pointer; cdecl;
{$else}
    init_t = procedure (param1: Pserver_rec; param2: Ppool); cdecl;
    create_dir_config_t = function (p: Ppool; dir: PChar): Pointer; cdecl;
    merge_dir_config_t = function (p: PPool; base_conf, new_conf: Pointer): Pointer; cdecl;
    create_server_config_t = function (p: Ppool; s: Pserver_rec): Pointer; cdecl;
    merge_server_config_t = function (p: Ppool; base_conf, new_conf: Pointer): Pointer; cdecl;
{$endif}

  hook_t = function (param1: Prequest_rec): cint; cdecl;

{$ifdef ULTRIX_BRAIN_DEATH}
  child_init_t = procedure ();
  child_exit_t = procedure ();
{$else}
  child_init_t = procedure (param1: Pserver_rec; param2: Ppool); cdecl;
  child_exit_t = procedure (param1: Pserver_rec; param2: Ppool); cdecl;
{$endif}

  module_struct = record
    version: cint;		{ API version, *not* module version;
				 * check that module is compatible with this
				 * version of the server.
                                 }
    minor_version: cint;          { API minor version. Provides API feature
                                 * milestones. Not checked during module init
                                 }
    module_index: cint;		{ Index to this modules structures in
				 * config vectors.
                                 }

    name: PChar;
    dynamic_load_handle: Pointer;

    next: Pmodule_struct;

    magic: culong;              { Magic Cookie to identify a module structure;
                                 * It's mainly important for the DSO facility
                                 * (see also mod_so).
                                 }

    { init() occurs after config parsing, but before any children are
     * forked.
     * Modules should not rely on the order in which create_server_config
     * and create_dir_config are called.
     }

    init: init_t;
    create_dir_config: create_dir_config_t;
    merge_dir_config: merge_dir_config_t;
    create_server_config: create_server_config_t;
    merge_server_config: merge_server_config_t;

    cmds: Pcommand_rec;
    handlers: Phandler_rec;

    { Hooks for getting into the middle of server ops...

     * translate_handler --- translate URI to filename
     * access_checker --- check access by host address, etc.   All of these
     *                    run; if all decline, that's still OK.
     * check_user_id --- get and validate user id from the HTTP request
     * auth_checker --- see if the user (from check_user_id) is OK *here*.
     *                  If all of *these* decline, the request is rejected
     *                  (as a SERVER_ERROR, since the module which was
     *                  supposed to handle this was configured wrong).
     * type_checker --- Determine MIME type of the requested entity;
     *                  sets content_type, _encoding and _language fields.
     * logger --- log a transaction.
     * post_read_request --- run right after read_request or internal_redirect,
     *                  and not run during any subrequests.
     }

    translate_handler: hook_t;
    ap_check_user_id: hook_t;
    auth_checker: hook_t;
    access_checker: hook_t;
    type_checker: hook_t;
    fixer_upper: hook_t;
    logger: hook_t;
    header_parser: hook_t;

    { Regardless of the model the server uses for managing "units of
     * execution", i.e. multi-process, multi-threaded, hybrids of those,
     * there is the concept of a "heavy weight process".  That is, a
     * process with its own memory space, file spaces, etc.  This method,
     * child_init, is called once for each heavy-weight process before
     * any requests are served.  Note that no provision is made yet for
     * initialization per light-weight process (i.e. thread).  The
     * parameters passed here are the same as those passed to the global
     * init method above.
     }

    child_init: child_init_t;
    child_exit: child_exit_t;

    post_read_request: hook_t;
  end;
  
  module = module_struct;

  Pmodule = ^module;

{ Initializer for the first few module slots, which are only
 * really set up once we start running.  Note that the first two slots
 * provide a version check; this should allow us to deal with changes to
 * the API. The major number should reflect changes to the API handler table
 * itself or removal of functionality. The minor number should reflect
 * additions of functionality to the existing API. (the server can detect
 * an old-format module, and either handle it back-compatibly, or at least
 * signal an error). See src/include/ap_mmn.h for MMN version history.
 }

procedure STANDARD_MODULE_STUFF(var mod_: module);

{ Generic accessors for other modules to get at their own module-specific
 * data
 }

{API_EXPORT(void *) ap_get_module_config(void *conf_vector, module *m);
API_EXPORT(void) ap_set_module_config(void *conf_vector, module *m, void *val);

#define ap_get_module_config(v,m)	\
    (((void **)(v))[(m)->module_index])
#define ap_set_module_config(v,m,val)	\
    ((((void **)(v))[(m)->module_index]) = (val))}

{ Generic command handling function... }

{API_EXPORT_NONSTD(const char *) ap_set_string_slot(cmd_parms *, char *, char *);
API_EXPORT_NONSTD(const char *) ap_set_string_slot_lower(cmd_parms *, char *, char *);
API_EXPORT_NONSTD(const char *) ap_set_flag_slot(cmd_parms *, char *, int);
API_EXPORT_NONSTD(const char *) ap_set_file_slot(cmd_parms *, char *, char *);}

{ For modules which need to read config files, open logs, etc. ...
 * this returns the fname argument if it begins with '/'; otherwise
 * it relativizes it wrt server_root.
 }

//API_EXPORT(char *) ap_server_root_relative(pool *p, char *fname);

{ Finally, the hook for dynamically loading modules in... }

procedure ap_add_module(m: Pmodule);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_remove_module(m: Pmodule);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_add_loaded_module(m: Pmodule);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_add_named_module(const name: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_clear_module_list();
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_find_module_name(m: Pmodule): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_find_linked_module(const name: PChar): Pmodule;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ for implementing subconfigs and customized config files }
{API_EXPORT(const char *) ap_srm_command_loop(cmd_parms *parms, void *config);

#ifdef CORE_PRIVATE

extern API_VAR_EXPORT module *top_module;

extern module *ap_prelinked_modules[];
extern module *ap_preloaded_modules[];
extern API_VAR_EXPORT module **ap_loaded_modules;}

{ For mod_so.c... }

//API_EXPORT(void) ap_single_module_configure(pool *p, server_rec *s, module *m);

{ For http_main.c... }

{API_EXPORT(server_rec *) ap_read_config(pool *conf_pool, pool *temp_pool, char *config_name);
API_EXPORT(void) ap_init_modules(pool *p, server_rec *s);
API_EXPORT(void) ap_child_init_modules(pool *p, server_rec *s);
API_EXPORT(void) ap_child_exit_modules(pool *p, server_rec *s);
API_EXPORT(void) ap_setup_prelinked_modules(void);
API_EXPORT(void) ap_show_directives(void);
API_EXPORT(void) ap_show_modules(void);
void ap_cleanup_method_ptrs(void);}

{ For http_request.c... }

{CORE_EXPORT(void *) ap_create_request_config(pool *p);
CORE_EXPORT(void *) ap_create_per_dir_config(pool *p);
CORE_EXPORT(void *) ap_merge_per_dir_configs(pool *p, void *base, void *new);}

{ For http_core.c... (<Directory> command and virtual hosts) }

{CORE_EXPORT(int) ap_parse_htaccess(void **result, request_rec *r, int override,
		const char *path, const char *access_name);

CORE_EXPORT(const char *) ap_init_virtual_host(pool *p, const char *hostname,
				server_rec *main_server, server_rec **);
CORE_EXPORT(void) ap_process_resource_config(server_rec *s, char *fname, pool *p, pool *ptemp);
}
{ ap_check_cmd_context() definitions: }
//API_EXPORT(const char *) ap_check_cmd_context(cmd_parms *cmd, unsigned forbidden);

{ ap_check_cmd_context():              Forbidden in: }
const
  NOT_IN_VIRTUALHOST    = $01; { <Virtualhost> }
  NOT_IN_LIMIT          = $02; { <Limit> }
  NOT_IN_DIRECTORY      = $04; { <Directory> }
  NOT_IN_LOCATION       = $08; { <Location> }
  NOT_IN_FILES          = $10; { <Files> }
  NOT_IN_DIR_LOC_FILE   = (NOT_IN_DIRECTORY or NOT_IN_LOCATION or NOT_IN_FILES); { <Directory>/<Location>/<Files>}
  GLOBAL_ONLY           = (NOT_IN_VIRTUALHOST or NOT_IN_LIMIT or NOT_IN_DIR_LOC_FILE);


{ Module-method dispatchers, also for http_request.c }

//API_EXPORT(int) ap_translate_name(request_rec *);
//API_EXPORT(int) ap_check_access(request_rec *);	{ check access on non-auth basis }
//API_EXPORT(int) ap_check_user_id(request_rec *);	{ obtain valid username from client auth }
//API_EXPORT(int) ap_check_auth(request_rec *);	{ check (validated) user is authorized here }
//API_EXPORT(int) ap_find_types(request_rec *);	{ identify MIME type }
//API_EXPORT(int) ap_run_fixups(request_rec *);	{ poke around for other metainfo, etc.... }
//API_EXPORT(int) ap_invoke_handler(request_rec *);
//API_EXPORT(int) ap_log_transaction(request_rec *r);
//API_EXPORT(int) ap_header_parse(request_rec *);
//API_EXPORT(int) ap_run_post_read_request(request_rec *);

{ for mod_perl }

//CORE_EXPORT(const command_rec *) ap_find_command(const char *name, const command_rec *cmds);
//CORE_EXPORT(const command_rec *) ap_find_command_in_modules(const char *cmd_name, module **mod);
//CORE_EXPORT(void *) ap_set_config_vectors(cmd_parms *parms, void *config, module *mod);
//CORE_EXPORT(const char *) ap_handle_command(cmd_parms *parms, void *config, const char *l);

//#endif

