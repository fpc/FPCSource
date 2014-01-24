{* Licensed to the Apache Software Foundation (ASF) under one or more
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
 *}

(**
 * @file http_config.h
 * @brief Apache Configuration
 *
 * @defgroup APACHE_CORE_CONFIG Configuration
 * @ingroup  APACHE_CORE
 * @{
 *)

{$ifndef APACHE_HTTP_CONFIG_H}
 {$define APACHE_HTTP_CONFIG_H}

//#include "util_cfgtree.h"
//#include "ap_config.h"
//#include "apr_tables.h"

{*
 * The central data structures around here...
 *}

{* Command dispatch structures... *}

{fpc -> moved const sections up here}
(**
 * @defgroup ConfigDirectives Allowed locations for configuration directives.
 *
 * The allowed locations for a configuration directive are the union of
 * those indicated by each set bit in the req_override mask.
 *
 * @{
 *)
const
  OR_NONE          = 0;    {**< *.conf is not available anywhere in this override *}
  OR_LIMIT         = 1;    {**< *.conf inside &lt;Directory&gt; or &lt;Location&gt;
                            and .htaccess when AllowOverride Limit *}
  OR_OPTIONS       = 2;    {**< *.conf anywhere
                            and .htaccess when AllowOverride Options *}
  OR_FILEINFO      = 4;    {**< *.conf anywhere
                            and .htaccess when AllowOverride FileInfo *}
  OR_AUTHCFG       = 8;    {**< *.conf inside &lt;Directory&gt; or &lt;Location&gt;
                            and .htaccess when AllowOverride AuthConfig *}
  OR_INDEXES       = 16;   {**< *.conf anywhere
                            and .htaccess when AllowOverride Indexes *}
  OR_UNSET         = 32;   {**< bit to indicate that AllowOverride has not been set *}
  ACCESS_CONF      = 64;   {**< *.conf inside &lt;Directory&gt; or &lt;Location&gt; *}
  RSRC_CONF        = 128;  {**< *.conf outside &lt;Directory&gt; or &lt;Location&gt; *}
  EXEC_ON_READ     = 256;  {**< force directive to execute a command
                            which would modify the configuration (like including another
                            file, or IFModule *}
{* Flags to determine whether syntax errors in .htaccess should be
 * treated as nonfatal (log and ignore errors)
 *}
  NONFATAL_OVERRIDE = 512; {* Violation of AllowOverride rule *}
  NONFATAL_UNKNOWN  = 1024;{* Unrecognised directive *}
  NONFATAL_ALL      = (NONFATAL_OVERRIDE or NONFATAL_UNKNOWN);

{** this directive can be placed anywhere *}
  OR_ALL            = (OR_LIMIT or OR_OPTIONS or OR_FILEINFO or OR_AUTHCFG or OR_INDEXES);

(** @} *)

{**
 * This can be returned by a function if they don't wish to handle
 * a command. Make it something not likely someone will actually use
 * as an error code.
 *}
  DECLINE_CMD = '\a\b';

{**
 * How the directives arguments should be parsed.
 * @remark Note that for all of these except RAW_ARGS, the config routine is
 *      passed a freshly allocated string which can be modified or stored
 *      or whatever...
 *}
type
  cmd_how = (
    RAW_ARGS,           {**< cmd_func parses command line itself *}
    TAKE1,              {**< one argument only *}
    TAKE2,              {**< two arguments only *}
    ITERATE,            {**< one argument, occuring multiple times
                         * (e.g., IndexIgnore)
                         *}
    ITERATE2,           {**< two arguments, 2nd occurs multiple times
                         * (e.g., AddIcon)
                         *}
    FLAG,               {**< One of 'On' or 'Off' *}
    NO_ARGS,            {**< No args at all, e.g. &lt;/Directory&gt; *}
    TAKE12,             {**< one or two arguments *}
    TAKE3,              {**< three arguments only *}
    TAKE23,             {**< two or three arguments *}
    TAKE123,            {**< one, two or three arguments *}
    TAKE13,             {**< one or three arguments *}
    TAKE_ARGV           {**< an argc and argv are passed *}
);

{**
 * This structure is passed to a command which is being invoked,
 * to carry a large variety of miscellaneous data which is all of
 * use to *somebody*...
 *}
// typedef struct cmd_parms_struct cmd_parms;
  Pcmd_parms = ^cmd_parms_struct;

//#if defined(AP_HAVE_DESIGNATED_INITIALIZER) || defined(DOXYGEN)

  tnoargsfunc   = function(parms: Pcmd_parms; mconfig: pointer): Pchar; cdecl;
  trawargsfunc  = function(parms: Pcmd_parms; mconfig: pointer; args: Pchar): Pchar; cdecl;
  ttakeargvfunc = function(parms: Pcmd_parms; mconfig: pointer; argc: longint; argv: PPchar): Pchar; cdecl;
  ttake1func    = function(parms: Pcmd_parms; mconfig: pointer; w: Pchar): Pchar; cdecl;
  ttake2func    = function(parms: Pcmd_parms; mconfig: pointer; w, w2: Pchar): Pchar; cdecl;
  ttake3func    = function(parms: Pcmd_parms; mconfig: pointer; w, w2, w3: Pchar): Pchar; cdecl;
  tflagfunc     = function(parms: Pcmd_parms; mconfig: pointer; on_: longint): Pchar; cdecl;

{**
 * All the types of functions that can be used in directives
 * @internal
 *}
  cmd_func = record
     case longint of
       {** function to call for a no-args *}
       0 : ( no_args : tnoargsfunc);

       {** function to call for a raw-args *}
       1 : ( raw_args : trawargsfunc);

       {** function to call for a argv/argc *}
       2 : ( take_argv : ttakeargvfunc);

       {** function to call for a take1 *}
       3 : ( take1 : ttake1func);

       {** function to call for a take2 *}
       4 : ( take2 : ttake2func);

       {** function to call for a take3 *}
       5 : ( take3 : ttake3func);

       {** function to call for a flag *}
       6 : ( flag : tflagfunc);
     end;
  Pcmd_func = ^cmd_func;
(*
/** This configuration directive does not take any arguments */
# define AP_NO_ARGS     func.no_args
/** This configuration directive will handle its own parsing of arguments*/
# define AP_RAW_ARGS    func.raw_args
/** This configuration directive will handle its own parsing of arguments*/
# define AP_TAKE_ARGV   func.take_argv
/** This configuration directive takes 1 argument*/
# define AP_TAKE1       func.take1
/** This configuration directive takes 2 arguments */
# define AP_TAKE2       func.take2
/** This configuration directive takes 3 arguments */
# define AP_TAKE3       func.take3
/** This configuration directive takes a flag (on/off) as a argument*/
# define AP_FLAG        func.flag

/** mechanism for declaring a directive with no arguments */
# define AP_INIT_NO_ARGS(directive, func, mconfig, where, help) \
    { directive, { .no_args=func }, mconfig, where, RAW_ARGS, help }
/** mechanism for declaring a directive with raw argument parsing */
# define AP_INIT_RAW_ARGS(directive, func, mconfig, where, help) \
    { directive, { .raw_args=func }, mconfig, where, RAW_ARGS, help }
/** mechanism for declaring a directive with raw argument parsing */
# define AP_INIT_TAKE_ARGV(directive, func, mconfig, where, help) \
    { directive, { .take_argv=func }, mconfig, where, TAKE_ARGV, help }
/** mechanism for declaring a directive which takes 1 argument */
# define AP_INIT_TAKE1(directive, func, mconfig, where, help) \
    { directive, { .take1=func }, mconfig, where, TAKE1, help }
/** mechanism for declaring a directive which takes multiple arguments */
# define AP_INIT_ITERATE(directive, func, mconfig, where, help) \
    { directive, { .take1=func }, mconfig, where, ITERATE, help }
/** mechanism for declaring a directive which takes 2 arguments */
# define AP_INIT_TAKE2(directive, func, mconfig, where, help) \
    { directive, { .take2=func }, mconfig, where, TAKE2, help }
/** mechanism for declaring a directive which takes 1 or 2 arguments */
# define AP_INIT_TAKE12(directive, func, mconfig, where, help) \
    { directive, { .take2=func }, mconfig, where, TAKE12, help }
/** mechanism for declaring a directive which takes multiple 2 arguments */
# define AP_INIT_ITERATE2(directive, func, mconfig, where, help) \
    { directive, { .take2=func }, mconfig, where, ITERATE2, help }
/** mechanism for declaring a directive which takes 1 or 3 arguments */
# define AP_INIT_TAKE13(directive, func, mconfig, where, help) \
    { directive, { .take3=func }, mconfig, where, TAKE13, help }
/** mechanism for declaring a directive which takes 2 or 3 arguments */
# define AP_INIT_TAKE23(directive, func, mconfig, where, help) \
    { directive, { .take3=func }, mconfig, where, TAKE23, help }
/** mechanism for declaring a directive which takes 1 to 3 arguments */
# define AP_INIT_TAKE123(directive, func, mconfig, where, help) \
    { directive, { .take3=func }, mconfig, where, TAKE123, help }
/** mechanism for declaring a directive which takes 3 arguments */
# define AP_INIT_TAKE3(directive, func, mconfig, where, help) \
    { directive, { .take3=func }, mconfig, where, TAKE3, help }
/** mechanism for declaring a directive which takes a flag (on/off) argument */
# define AP_INIT_FLAG(directive, func, mconfig, where, help) \
    { directive, { .flag=func }, mconfig, where, FLAG, help }

#else /* AP_HAVE_DESIGNATED_INITIALIZER */

typedef const char *( *cmd_func) ();

# define AP_NO_ARGS  func
# define AP_RAW_ARGS func
# define AP_TAKE_ARGV func
# define AP_TAKE1    func
# define AP_TAKE2    func
# define AP_TAKE3    func
# define AP_FLAG     func

# define AP_INIT_NO_ARGS(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, RAW_ARGS, help }
# define AP_INIT_RAW_ARGS(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, RAW_ARGS, help }
# define AP_INIT_TAKE_ARGV(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE_ARGV, help }
# define AP_INIT_TAKE1(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE1, help }
# define AP_INIT_ITERATE(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, ITERATE, help }
# define AP_INIT_TAKE2(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE2, help }
# define AP_INIT_TAKE12(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE12, help }
# define AP_INIT_ITERATE2(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, ITERATE2, help }
# define AP_INIT_TAKE13(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE13, help }
# define AP_INIT_TAKE23(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE23, help }
# define AP_INIT_TAKE123(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE123, help }
# define AP_INIT_TAKE3(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, TAKE3, help }
# define AP_INIT_FLAG(directive, func, mconfig, where, help) \
    { directive, func, mconfig, where, FLAG, help }

#endif /* AP_HAVE_DESIGNATED_INITIALIZER */
*)
{**
 * The command record structure.  Each modules can define a table of these
 * to define the directives it will implement.
 *}
  command_struct = record
    {** Name of this command *}
    name: PChar;
    {** The function to be called when this directive is parsed *}
    func: cmd_func;
    {** Extra data, for functions which implement multiple commands... *}
    cmd_data: Pointer;
    {** What overrides need to be allowed to enable this command. *}
    req_override: Integer;
    {** What the command expects as arguments *}
    args_how: cmd_how;

    {** 'usage' message, in case of syntax errors *}
    errmsg: PChar;
  end;
  command_rec = command_struct;
  Pcommand_rec = ^command_rec;


{** Common structure for reading of config files / passwd files etc. *}
  Pap_configfile_t = ^ap_configfile_t;
  PPap_configfile_t = ^Pap_configfile_t;
  ap_configfile_t = record
    {**< an apr_file_getc()-like function *}
    getch : function (ch:Pchar; param:pointer):apr_status_t;cdecl;
    {**< an apr_file_gets()-like function *}
    getstr : function (buf:pointer; bufsiz:apr_size_t; param:pointer):apr_status_t;cdecl;
    {**< a close handler function *}
    close : function (param:pointer):apr_status_t;cdecl;
    {**< the argument passed to getch/getstr/close *}
    param : pointer;
    {**< the filename / description *}
    name : Pchar;
    {**< current line number, starting at 1 *}
    line_number : dword;
  end;

{**
 * This structure is passed to a command which is being invoked,
 * to carry a large variety of miscellaneous data which is all of
 * use to *somebody*...
 *}
  cmd_parms_struct = record
    {** Argument to command from cmd_table *}
    info : pointer;
    {** Which allow-override bits are set *}
    override_ : longint;
    {** Which allow-override-opts bits are set *}
    override_opts : longint;
    {** Table of directives allowed per AllowOverrideList *}
    override_list : Papr_table_t;
    {** Which methods are &lt;Limit&gt;ed *}
    limited : apr_int64_t;
    {** methods which are limited *}
    limited_xmethods : Papr_array_header_t;
    {** methods which are xlimited *}
    xlimited : Pap_method_list_t;

    {** Config file structure. *}
    config_file : Pap_configfile_t;
    {** the directive specifying this command *}
    directive : Pap_directive_t;

    {** Pool to allocate new storage in *}
    pool : Papr_pool_t;
    {** Pool for scratch memory; persists during configuration, but
     *  wiped before the first request is served...  *}
    temp_pool : Papr_pool_t;
    {** Server_rec being configured for *}
    server : Pserver_rec;
    {** If configuring for a directory, pathname of that directory.
     *  NOPE!  That's what it meant previous to the existence of &lt;Files&gt;,
     * &lt;Location&gt; and regex matching.  Now the only usefulness that can be
     * derived from this field is whether a command is being called in a
     * server context (path == NULL) or being called in a dir context
     * (path != NULL).  *}
    path : Pchar;
    {** configuration command *}
    cmd : Pcommand_rec;

    {** per_dir_config vector passed to handle_command *}
    context : Pap_conf_vector_t;
    {** directive with syntax error *}
    err_directive : Pap_directive_t;
  end;

{**
 * Module structures.  Just about everything is dispatched through
 * these, directly or indirectly (through the command and handler
 * tables).
 *}
 Pmodule_struct = ^module_struct;
 module_struct = record
    {** API version, *not* module version; check that module is
     * compatible with this version of the server.
     *}
    version : longint;
    {** API minor version. Provides API feature milestones. Not checked
     *  during module init *}
    minor_version : longint;
    {** Index to this modules structures in config vectors.  *}
    module_index : longint;

    {** The name of the module's C file *}
    name : Pchar;
    {** The handle for the DSO.  Internal use only *}
    dynamic_load_handle : pointer;

    {** A pointer to the next module in the list
     *  @var module_struct *next
     *}
    next : Pmodule_struct;

    {** Magic Cookie to identify a module structure;  It's mainly
     *  important for the DSO facility (see also mod_so).  *}
    magic : dword;

    {** Function to allow MPMs to re-write command line arguments.  This
     *  hook is only available to MPMs.
     *  @param The process that the server is running in.
     *}
    rewrite_args : procedure (process:Pprocess_rec);cdecl;

    {** Function to allow all modules to create per directory configuration
     *  structures.
     *  @param p The pool to use for all allocations.
     *  @param dir The directory currently being processed.
     *  @return The per-directory structure created
     *}
    create_dir_config : function (p:Papr_pool_t; dir:Pchar):pointer;cdecl;

    {** Function to allow all modules to merge the per directory configuration
     *  structures for two directories.
     *  @param p The pool to use for all allocations.
     *  @param base_conf The directory structure created for the parent directory.
     *  @param new_conf The directory structure currently being processed.
     *  @return The new per-directory structure created
     *}
    merge_dir_config : function (p:Papr_pool_t; base_conf:pointer; new_conf:pointer):pointer;cdecl;

    {** Function to allow all modules to create per server configuration
     *  structures.
     *  @param p The pool to use for all allocations.
     *  @param s The server currently being processed.
     *  @return The per-server structure created
     *}
    create_server_config : function (p:Papr_pool_t; s:Pserver_rec):pointer;cdecl;

    {** Function to allow all modules to merge the per server configuration
     *  structures for two servers.
     *  @param p The pool to use for all allocations.
     *  @param base_conf The directory structure created for the parent directory.
     *  @param new_conf The directory structure currently being processed.
     *  @return The new per-directory structure created
     *}
    merge_server_config : function (p:Papr_pool_t; base_conf:pointer; new_conf:pointer):pointer;cdecl;

    {** A command_rec table that describes all of the directives this module
     * defines. *}
    cmds : Pcommand_rec;

    {** A hook to allow modules to hook other points in the request processing.
     *  In this function, modules should call the ap_hook_*() functions to
     *  register an interest in a specific step in processing the current
     *  request.
     *  @param p the pool to use for all allocations
     *}
    register_hooks : procedure (p:Papr_pool_t);cdecl;
  end;
  module = module_struct;
  Pmodule = ^module;
  PPmodule = ^Pmodule;

{**
 * The APLOG_USE_MODULE macro is used choose which module a file belongs to.
 * This is necessary to allow per-module loglevel configuration.
 *
 * APLOG_USE_MODULE indirectly sets APLOG_MODULE_INDEX and APLOG_MARK.
 *
 * If a module should be backward compatible with versions before 2.3.6,
 * APLOG_USE_MODULE needs to be enclosed in a ifdef APLOG_USE_MODULE block.
 *
 * @param foo name of the module symbol of the current module, without the
 *            trailing "_module" part
 * @see APLOG_MARK
 *}
{#define APLOG_USE_MODULE(foo) \
    extern module AP_MODULE_DECLARE_DATA foo##_module;                  \
    static int * const aplog_module_index = &(foo##_module.module_index)

/**
 * AP_DECLARE_MODULE is a convenience macro that combines a call of
 * APLOG_USE_MODULE with the definition of the module symbol.
 *
 * If a module should be backward compatible with versions before 2.3.6,
 * APLOG_USE_MODULE should be used explicitly instead of AP_DECLARE_MODULE.
 */
#define AP_DECLARE_MODULE(foo) \
    APLOG_USE_MODULE(foo);                         \
    module AP_MODULE_DECLARE_DATA foo##_module
}
(**
 * @defgroup ModuleInit Module structure initializers
 *
 * Initializer for the first few module slots, which are only
 * really set up once we start running.  Note that the first two slots
 * provide a version check; this should allow us to deal with changes to
 * the API. The major number should reflect changes to the API handler table
 * itself or removal of functionality. The minor number should reflect
 * additions of functionality to the existing API. (the server can detect
 * an old-format module, and either handle it back-compatibly, or at least
 * signal an error). See src/include/ap_mmn.h for MMN version history.
 * @{
 *)

{** The one used in Apache 1.3, which will deliberately cause an error *}
//#define STANDARD_MODULE_STUFF   this_module_needs_to_be_ported_to_apache_2_0

{** Use this in all standard modules *}
(*#define STANDARD20_MODULE_STUFF MODULE_MAGIC_NUMBER_MAJOR, \
                                MODULE_MAGIC_NUMBER_MINOR, \
                                -1, \
                                __FILE__, \
                                NULL, \
                                NULL, \
                                MODULE_MAGIC_COOKIE, \
                                NULL      /* rewrite args spot */*)
procedure STANDARD20_MODULE_STUFF(var mod_: module);

{** Use this only in MPMs *}
(*#define MPM20_MODULE_STUFF      MODULE_MAGIC_NUMBER_MAJOR, \
                                MODULE_MAGIC_NUMBER_MINOR, \
                                -1, \
                                __FILE__, \
                                NULL, \
                                NULL, \
                                MODULE_MAGIC_COOKIE*)
procedure MPM20_MODULE_STUFF(var mod_: module);

(** @} *)

{* CONFIGURATION VECTOR FUNCTIONS *}

{** configuration vector structure *}
//typedef struct ap_conf_vector_t ap_conf_vector_t;
{ Moved to httpd.pas}
//  ap_conf_vector_t = record end;
//  Pap_conf_vector_t = ^ap_conf_vector_t;
//  PPap_conf_vector_t = ^Pap_conf_vector_t;

{**
 * Generic accessors for other modules to get at their own module-specific
 * data
 * @param cv The vector in which the modules configuration is stored.
 *        usually r->per_dir_config or s->module_config
 * @param m The module to get the data for.
 * @return The module-specific data
 *}
//AP_DECLARE(void *) ap_get_module_config(const ap_conf_vector_t *cv,
//                                        const module *m);
{not found in httpd binary libraries, ignored}

{**
 * Generic accessors for other modules to set at their own module-specific
 * data
 * @param cv The vector in which the modules configuration is stored.
 *        usually r->per_dir_config or s->module_config
 * @param m The module to set the data for.
 * @param val The module-specific data to set
 *}
//AP_DECLARE(void) ap_set_module_config(ap_conf_vector_t *cv, const module *m,
//                                      void *val);

//{$ifndef AP_DEBUG}
//
//function ap_get_module_config(v: Pap_conf_vector_t; m: Pmodule): Pap_conf_vector_t;
//
//procedure ap_set_module_config(v: Pap_conf_vector_t; m: Pmodule; val: Pap_conf_vector_t);
//
//{$endif} {* AP_DEBUG *}

{**
 * Generic accessor for modules to get the module-specific loglevel
 * @param s The server from which to get the loglevel.
 * @param index The module_index of the module to get the loglevel for.
 * @return The module-specific loglevel
 *}
//AP_DECLARE(int) ap_get_server_module_loglevel(const server_rec *s, int index);

{**
 * Generic accessor for modules the module-specific loglevel
 * @param c The connection from which to get the loglevel.
 * @param index The module_index of the module to get the loglevel for.
 * @return The module-specific loglevel
 *}
//AP_DECLARE(int) ap_get_conn_module_loglevel(const conn_rec *c, int index);

{**
 * Generic accessor for modules the module-specific loglevel
 * @param c The connection from which to get the loglevel.
 * @param s The server from which to get the loglevel if c does not have a
 *          specific loglevel configuration.
 * @param index The module_index of the module to get the loglevel for.
 * @return The module-specific loglevel
 *}
//AP_DECLARE(int) ap_get_conn_server_module_loglevel(const conn_rec *c,
//                                                   const server_rec *s,
//                                                   int index);

{**
 * Generic accessor for modules to get the module-specific loglevel
 * @param r The request from which to get the loglevel.
 * @param index The module_index of the module to get the loglevel for.
 * @return The module-specific loglevel
 *}
//AP_DECLARE(int) ap_get_request_module_loglevel(const request_rec *r, int index);

{**
 * Accessor to set module-specific loglevel
 * @param p A pool
 * @param l The ap_logconf struct to modify.
 * @param index The module_index of the module to set the loglevel for.
 * @param level The new log level
 *}
procedure ap_set_module_loglevel(p: Papr_pool_t; l: Pap_logconf;
                                        index, level: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_set_module_loglevel' + LibSuff16;
{
#if !defined(AP_DEBUG)

#define ap_get_conn_logconf(c)                     \
    ((c)->log             ? (c)->log             : \
     &(c)->base_server->log)

#define ap_get_conn_server_logconf(c,s)                             \
    ( ( (c)->log != &(c)->base_server->log && (c)->log != NULL )  ? \
      (c)->log                                                    : \
      &(s)->log )

#define ap_get_request_logconf(r)                  \
    ((r)->log             ? (r)->log             : \
     (r)->connection->log ? (r)->connection->log : \
     &(r)->server->log)

#define ap_get_module_loglevel(l,i)                                     \
    (((i) < 0 || (l)->module_levels == NULL || (l)->module_levels[i] < 0) ?  \
     (l)->level :                                                         \
     (l)->module_levels[i])

#define ap_get_server_module_loglevel(s,i)  \
    (ap_get_module_loglevel(&(s)->log,i))

#define ap_get_conn_module_loglevel(c,i)  \
    (ap_get_module_loglevel(ap_get_conn_logconf(c),i))

#define ap_get_conn_server_module_loglevel(c,s,i)  \
    (ap_get_module_loglevel(ap_get_conn_server_logconf(c,s),i))

#define ap_get_request_module_loglevel(r,i)  \
    (ap_get_module_loglevel(ap_get_request_logconf(r),i))

#endif /* AP_DEBUG */
}
{**
 * Set all module-specific loglevels to val
 * @param l The log config for which to set the loglevels.
 * @param val the value to set all loglevels to
 *}
procedure ap_reset_module_loglevels(l: Pap_logconf; val: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_reset_module_loglevels' + LibSuff8;

{**
 * Generic command handling function for strings
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_string_slot(cmd_parms *cmd,
//                                                   void *struct_ptr,
//                                                   const char *arg);
function ap_set_string_slot(cmd: Pcmd_parms;
                            struct_ptr: Pointer;
                            const arg: PChar): PChar; cdecl;
  external LibHTTPD name 'ap_set_string_slot';

{**
 * Generic command handling function for integers
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_int_slot(cmd_parms *cmd,
//                                                void *struct_ptr,
//                                                const char *arg);
function ap_set_int_slot(cmd: Pcmd_parms; struct_ptr: Pointer; const arg: PChar): PChar; cdecl;
  external LibHTTPD name 'ap_set_int_slot';

{**
 * Parsing function for log level
 * @param str The string to parse
 * @param val The parsed log level
 * @return An error string or NULL on success
 *}
function ap_parse_log_level(const str: PChar; val: Integer): PChar;
{$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
external LibHTTPD name LibNamePrefix + 'ap_parse_log_level' + LibSuff8;

{**
 * Return true if the specified method is limited by being listed in
 * a &lt;Limit&gt; container, or by *not* being listed in a &lt;LimitExcept&gt;
 * container.
 *
 * @param   method  Pointer to a string specifying the method to check.
 * @param   cmd     Pointer to the cmd_parms structure passed to the
 *                  directive handler.
 * @return  0 if the method is not limited in the current scope
 *}
function ap_method_is_limited(cmd: Pcmd_parms; const method: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_method_is_limited' + LibSuff8;

{**
 * Generic command handling function for strings, always sets the value
 * to a lowercase string
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_string_slot_lower(cmd_parms *cmd,
//                                                         void *struct_ptr,
//                                                         const char *arg);
function ap_set_string_slot_lower(cmd: Pcmd_parms;
                                  struct_ptr: Pointer;
                                  const arg: PChar): PChar; cdecl;
  external LibHTTPD name 'ap_set_string_slot_lower';

{**
 * Generic command handling function for flags stored in an int
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive (either 1 or 0)
 * @return An error string or NULL on success
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_flag_slot(cmd_parms *cmd,
//                                                 void *struct_ptr,
//                                                 int arg);
function ap_set_flag_slot(cmd: Pcmd_parms;
                          struct_ptr: Pointer;
                          const arg: PChar): PChar; cdecl;
  external LibHTTPD name 'ap_set_flag_slot';

{**
 * Generic command handling function for flags stored in a char
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive (either 1 or 0)
 * @return An error string or NULL on success
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_flag_slot_char(cmd_parms *cmd,
//                                                      void *struct_ptr,
//                                                      int arg);
function ap_set_flag_slot_char(cmd: Pcmd_parms;
                               struct_ptr: Pointer;
                               arg: Integer): PChar; cdecl;
  external LibHTTPD name 'ap_set_flag_slot_char';

{**
 * Generic command handling function for files
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return An error string or NULL on success
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_file_slot(cmd_parms *cmd,
//                                                 void *struct_ptr,
//                                                 const char *arg);
function ap_set_file_slot(cmd: Pcmd_parms; struct_ptr: Pointer; const arg: PChar): PChar; cdecl;
  external LibHTTPD name 'ap_set_file_slot';

{**
 * Generic command handling function to respond with cmd->help as an error
 * @param cmd The command parameters for this directive
 * @param struct_ptr pointer into a given type
 * @param arg The argument to the directive
 * @return The cmd->help value as the error string
 * @note This allows simple declarations such as:
 * @code
 *     AP_INIT_RAW_ARGS("Foo", ap_set_deprecated, NULL, OR_ALL,
 *         "The Foo directive is no longer supported, use Bar"),
 * @endcode
 *}
//AP_DECLARE_NONSTD(const char *) ap_set_deprecated(cmd_parms *cmd,
//                                                  void *struct_ptr,
//                                                  const char *arg);
 function ap_set_deprecated(cmd: Pcmd_parms;
                            struct_ptr: Pointer;
                            const arg: PChar): PChar; cdecl;
   external LibHTTPD name 'ap_set_deprecated';

{**
 * For modules which need to read config files, open logs, etc. this returns
 * the canonical form of fname made absolute to ap_server_root.
 * @param p pool to allocate data from
 * @param fname The file name
 *}
function ap_server_root_relative(p: Papr_pool_t; const fname: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_server_root_relative' + LibSuff8;

{**
 * Compute the name of a run-time file (e.g., shared memory "file") relative
 * to the appropriate run-time directory.  Absolute paths are returned as-is.
 * The run-time directory is configured via the DefaultRuntimeDir directive or
 * at build time.
 *}
function ap_runtime_dir_relative(p: Papr_pool_t; const fname: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_runtime_dir_relative' + LibSuff8;

{* Finally, the hook for dynamically loading modules in... *}

{**
 * Add a module to the server
 * @param m The module structure of the module to add
 * @param p The pool of the same lifetime as the module
 * @param s The module's symbol name (used for logging)
 *}
function ap_add_module(m: Pmodule; p: Papr_pool_t; s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_add_module' + LibSuff12;

{**
 * Remove a module from the server.  There are some caveats:
 * when the module is removed, its slot is lost so all the current
 * per-dir and per-server configurations are invalid. So we should
 * only ever call this function when you are invalidating almost
 * all our current data. I.e. when doing a restart.
 * @param m the module structure of the module to remove
 *}
procedure ap_remove_module(m: Pmodule);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_remove_module' + LibSuff4;

{**
 * Add a module to the chained modules list and the list of loaded modules
 * @param mod The module structure of the module to add
 * @param p The pool with the same lifetime as the module
 * @param s The module's symbol name (used for logging)
 *}
function ap_add_loaded_module(mod_: Pmodule; p: Papr_pool_t; s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_add_loaded_module' + LibSuff12;

{**
 * Remove a module fromthe chained modules list and the list of loaded modules
 * @param mod the module structure of the module to remove
 *}
procedure ap_remove_loaded_module(m: Pmodule);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_remove_loaded_module' + LibSuff4;

{**
 * Find the name of the specified module
 * @param m The module to get the name for
 * @return the name of the module
 *}
 function ap_find_module_name(m: Pmodule): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_find_module_name' + LibSuff4;

{**
 * Find the short name of the module identified by the specified module index
 * @param module_index The module index to get the name for
 * @return the name of the module, NULL if not found
 *}
function ap_find_module_short_name(module_index: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_module_short_name' + LibSuff4;

{**
 * Find a module based on the name of the module
 * @param name the name of the module
 * @return the module structure if found, NULL otherwise
 *}
function ap_find_linked_module(const name: PChar): Pmodule;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_linked_module' + LibSuff4;

{**
 * Open a ap_configfile_t as apr_file_t
 * @param ret_cfg open ap_configfile_t struct pointer
 * @param p The pool to allocate the structure from
 * @param name the name of the file to open
 *}
function ap_pcfg_openfile(ret_cfg: PPap_configfile_t;
 p: Papr_pool_t; const name: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_pcfg_openfile' + LibSuff12;

{**
 * Allocate a ap_configfile_t handle with user defined functions and params
 * @param p The pool to allocate from
 * @param descr The name of the file
 * @param param The argument passed to getch/getstr/close
 * @param getc_func The getch function
 * @param gets_func The getstr function
 * @param close_func The close function
 *}
//AP_DECLARE(ap_configfile_t *) ap_pcfg_open_custom(apr_pool_t *p,
//    const char *descr,
//    void *param,
//    apr_status_t (*getc_func) (char *ch, void *param),
//    apr_status_t (*gets_func) (void *buf, apr_size_t bufsiz, void *param),
//    apr_status_t (*close_func) (void *param));
type
 getc_func_t = function (ch: PChar; param: Pointer): apr_status_t;
 gets_func_t = function (buf: Pointer; bufsiz: apr_size_t; param: Pointer): apr_status_t;
 close_func_t = function (param: Pointer): apr_status_t;

 function ap_pcfg_open_custom(p: Papr_pool_t;
                              const descr: PChar;
                              param: Pointer;
                              getc_func: getc_func_t;
                              gets_func: gets_func_t;
                              close_func: close_func_t): Pap_configfile_t;
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_pcfg_open_custom' + LibSuff24;

{**
 * Read one line from open ap_configfile_t, strip leading and trailing
 * whitespace, increase line number
 * @param buf place to store the line read
 * @param bufsize size of the buffer
 * @param cfp File to read from
 * @return error status, APR_ENOSPC if bufsize is too small for the line
 *}
function ap_cfg_getline(buf: PChar; bufsize: apr_size_t; cfp: Pap_configfile_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_cfg_getline' + LibSuff12;

{**
 * Read one char from open configfile_t, increase line number upon LF
 * @param ch place to store the char read
 * @param cfp The file to read from
 * @return error status
 *}
function ap_cfg_getc(ch: PChar; cfp: Pap_configfile_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_cfg_getc' + LibSuff8;

{**
 * Detach from open ap_configfile_t, calling the close handler
 * @param cfp The file to close
 * @return 1 on sucess, 0 on failure
 *}
function ap_cfg_closefile(cfp: Pap_configfile_t): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_cfg_closefile' + LibSuff4;

{**
 * Convert a return value from ap_cfg_getline or ap_cfg_getc to a user friendly
 * string.
 * @param p The pool to allocate the string from
 * @param cfp The config file
 * @param rc The return value to convert
 * @return The error string, NULL if rc == APR_SUCCESS
 *}
function ap_pcfg_strerror(p: Papr_pool_t; cfp: Pap_configfile_t;
                                          rc: apr_status_t): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_pcfg_strerror' + LibSuff12;

{**
 * Read all data between the current &lt;foo&gt; and the matching &lt;/foo&gt;.  All
 * of this data is forgotten immediately.
 * @param cmd The cmd_parms to pass to the directives inside the container
 * @param directive The directive name to read until
 * @return Error string on failure, NULL on success
 * @note If cmd->pool == cmd->temp_pool, ap_soak_end_container() will assume
 *       .htaccess context and use a lower maximum line length.
 *}
function ap_soak_end_container(cmd: Pcmd_parms; directive: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_soak_end_container' + LibSuff8;

{**
 * Read all data between the current &lt;foo&gt; and the matching &lt;/foo&gt; and build
 * a config tree from it
 * @param p pool to allocate from
 * @param temp_pool Temporary pool to allocate from
 * @param parms The cmd_parms to pass to all directives read
 * @param current The current node in the tree
 * @param curr_parent The current parent node
 * @param orig_directive The directive to read until hit.
 * @return Error string on failure, NULL on success
 * @note If p == temp_pool, ap_build_cont_config() will assume .htaccess
 *       context and use a lower maximum line length.
*}
function ap_build_cont_config(p,
                              temp_pool: Papr_pool_t;
                              parms: Pcmd_parms;
                              current,
                              curr_parent: PPap_directive_t;
                              orig_directive: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_build_cont_config' + LibSuff24;

{**
 * Build a config tree from a config file
 * @param parms The cmd_parms to pass to all of the directives in the file
 * @param conf_pool The pconf pool
 * @param temp_pool The temporary pool
 * @param conftree Place to store the root node of the config tree
 * @return Error string on erro, NULL otherwise
 * @note If conf_pool == temp_pool, ap_build_config() will assume .htaccess
 *       context and use a lower maximum line length.
 *}
function ap_build_config(parms: Pcmd_parms;
                         conf_pool,
                         temp_pool: Papr_pool_t;
                         conftree: PPap_directive_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_build_config' + LibSuff16;

{**
 * Walk a config tree and setup the server's internal structures
 * @param conftree The config tree to walk
 * @param parms The cmd_parms to pass to all functions
 * @param section_vector The per-section config vector.
 * @return Error string on error, NULL otherwise
 *}
function ap_walk_config(conftree: Pap_directive_t;
                        parms: Pcmd_parms;
                        section_vector: Pap_conf_vector_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_walk_config' + LibSuff12;

(**
 * @defgroup ap_check_cmd_context Check command context
 * @{
 *)
{**
 * Check the context a command is used in.
 * @param cmd The command to check
 * @param forbidden Where the command is forbidden.
 * @return Error string on error, NULL on success
 *}
function ap_check_cmd_context(cmd: Pcmd_parms;
                              forbidden: dword): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_check_cmd_context' + LibSuff8;

const

  NOT_IN_VIRTUALHOST     = $01;{**< Forbidden in &lt;VirtualHost&gt; *}
  NOT_IN_LIMIT           = $02;{**< Forbidden in &lt;Limit&gt; *}
  NOT_IN_DIRECTORY       = $04;{**< Forbidden in &lt;Directory&gt; *}
  NOT_IN_LOCATION        = $08;{**< Forbidden in &lt;Location&gt; *}
  NOT_IN_FILES           = $10;{**< Forbidden in &lt;Files&gt; *}
  NOT_IN_HTACCESS        = $20;{**< Forbidden in .htaccess files *}
{** Forbidden in &lt;Directory&gt;/&lt;Location&gt;/&lt;Files&gt;*}
  NOT_IN_DIR_LOC_FILE    = (NOT_IN_DIRECTORY or NOT_IN_LOCATION or NOT_IN_FILES);
{** Forbidden in &lt;VirtualHost&gt;/&lt;Limit&gt;/&lt;Directory&gt;/&lt;Location&gt;/&lt;Files&gt; *}
  GLOBAL_ONLY            = (NOT_IN_VIRTUALHOST or NOT_IN_LIMIT or NOT_IN_DIR_LOC_FILE);

(** @} *)

{**
 * @brief This structure is used to assign symbol names to module pointers
 *}
type
  ap_module_symbol_t = record
    name: PChar;
    modp: Pmodule;
  end;

{**
 * The topmost module in the list
 * @var module *ap_top_module
 *}
//AP_DECLARE_DATA extern module *ap_top_module;

{**
 * Array of all statically linked modules
 * @var module *ap_prelinked_modules[]
 *}
//AP_DECLARE_DATA extern module *ap_prelinked_modules[];
{**
 * Array of all statically linked modulenames (symbols)
 * @var ap_module_symbol_t ap_prelinked_module_symbols[]
 *}
//AP_DECLARE_DATA extern ap_module_symbol_t ap_prelinked_module_symbols[];
{**
 * Array of all preloaded modules
 * @var module *ap_preloaded_modules[]
 *}
//AP_DECLARE_DATA extern module *ap_preloaded_modules[];
{**
 * Array of all loaded modules
 * @var module **ap_loaded_modules
 *}
//AP_DECLARE_DATA extern module **ap_loaded_modules;

{* For mod_so.c... *}
{** Run a single module's two create_config hooks
 *  @param p the pool to allocate from
 *  @param s The server to configure for.
 *  @param m The module to configure
 *}
procedure ap_single_module_configure(p: Papr_pool_t; s: Pserver_rec;
                                     m: Pmodule);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_single_module_configure' + LibSuff12;

{* For http_main.c... *}
{**
 * Add all of the prelinked modules into the loaded module list
 * @param process The process that is currently running the server
 *}
function ap_setup_prelinked_modules(process: Pprocess_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_setup_prelinked_modules' + LibSuff4;

{**
 * Show the preloaded configuration directives, the help string explaining
 * the directive arguments, in what module they are handled, and in
 * what parts of the configuration they are allowed.  Used for httpd -h.
 *}
procedure ap_show_directives;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_show_directives' + LibSuff0;

{**
 * Show the preloaded module names.  Used for httpd -l.
 *}
procedure ap_show_modules;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_show_modules' + LibSuff0;

{**
 * Show the MPM name.  Used in reporting modules such as mod_info to
 * provide extra information to the user
 *}
function ap_show_mpm: PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_show_mpm' + LibSuff0;

{**
 * Read all config files and setup the server
 * @param process The process running the server
 * @param temp_pool A pool to allocate temporary data from.
 * @param config_name The name of the config file
 * @param conftree Place to store the root of the config tree
 * @return The setup server_rec list.
 *}
function ap_read_config(process: Pprocess_rec;
                        temp_pool: Papr_pool_t;
                        const config_name: PChar;
                        conftree: PPap_directive_t): Pserver_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_read_config' + LibSuff16;

{**
 * Run all rewrite args hooks for loaded modules
 * @param process The process currently running the server
 *}
procedure ap_run_rewrite_args(process: Pprocess_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_run_rewrite_args' + LibSuff4;

{**
 * Run the register hooks function for a specified module
 * @param m The module to run the register hooks function fo
 * @param p The pool valid for the lifetime of the module
 *}
procedure ap_register_hooks(m: Pmodule; p: Papr_pool_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_register_hooks' + LibSuff8;

{**
 * Setup all virtual hosts
 * @param p The pool to allocate from
 * @param main_server The head of the server_rec list
 *}
procedure ap_fixup_virtual_hosts(p: Papr_pool_t;
                                 main_server: Pserver_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_fixup_virtual_hosts' + LibSuff8;

{**
 * Reserve some modules slots for modules loaded by other means than
 * EXEC_ON_READ directives.
 * Relevant modules should call this in the pre_config stage.
 * @param count The number of slots to reserve.
 *}
procedure ap_reserve_module_slots(count: Longint);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_reserve_module_slots' + LibSuff4;

{**
 * Reserve some modules slots for modules loaded by a specific
 * non-EXEC_ON_READ config directive.
 * This counts how often the given directive is used in the config and calls
 * ap_reserve_module_slots() accordingly.
 * @param directive The name of the directive
 *}
procedure ap_reserve_module_slots_directive(const directive: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_reserve_module_slots_directive' + LibSuff4;

{* For http_request.c... *}

{**
 * Setup the config vector for a request_rec
 * @param p The pool to allocate the config vector from
 * @return The config vector
 *}
function ap_create_request_config(p: Papr_pool_t): Pap_conf_vector_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_create_request_config' + LibSuff4;

{**
 * Setup the config vector for per dir module configs
 * @param p The pool to allocate the config vector from
 * @return The config vector
 *}
//AP_CORE_DECLARE(ap_conf_vector_t *) ap_create_per_dir_config(apr_pool_t *p);
function ap_create_per_dir_config(p: Papr_pool_t): Pap_conf_vector_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_create_per_dir_config' + LibSuff4;

{**
 * Run all of the modules merge per dir config functions
 * @param p The pool to pass to the merge functions
 * @param base The base directory config structure
 * @param new_conf The new directory config structure
 *}
function ap_merge_per_dir_configs(p: Papr_pool_t;
                                  base,
                                  new_conf: Pap_conf_vector_t): Pap_conf_vector_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_merge_per_dir_configs' + LibSuff12;

{**
 * Allocate new ap_logconf and make (deep) copy of old ap_logconf
 * @param p The pool to alloc from
 * @param old The ap_logconf to copy (may be NULL)
 * @return The new ap_logconf struct
 *}
function ap_new_log_config(p: Papr_pool_t;
                           const old: Pap_logconf): Pap_logconf;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_new_log_config' + LibSuff8;

{**
 * Merge old ap_logconf into new ap_logconf.
 * old and new must have the same life time.
 * @param old_conf The ap_logconf to merge from
 * @param new_conf The ap_logconf to merge into
 *}
procedure ap_merge_log_config(const old_conf: Pap_logconf;
                              new_conf: Pap_logconf);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_merge_log_config' + LibSuff8;

{* For http_connection.c... *}
{**
 * Setup the config vector for a connection_rec
 * @param p The pool to allocate the config vector from
 * @return The config vector
 *}
function ap_create_conn_config(p: Papr_pool_t): Pap_conf_vector_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_create_conn_config' + LibSuff4;

{* For http_core.c... (&lt;Directory&gt; command and virtual hosts) *}

{**
 * parse an htaccess file
 * @param result htaccess_result
 * @param r The request currently being served
 * @param override Which overrides are active
 * @param override_opts Which allow-override-opts bits are set
 * @param override_list Table of directives allowed for override
 * @param path The path to the htaccess file
 * @param access_name The list of possible names for .htaccess files
 * int The status of the current request
 *}
function ap_parse_htaccess(result_: PPap_conf_vector_t;
                           r: Prequest_rec;
                           override_: Longint;
                           override_opts: Longint;
                           override_list: Papr_table_t;
                           const path: PChar;
                           const access_name: PChar): Longint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_parse_htaccess' + LibSuff28;

{**
 * Setup a virtual host
 * @param p The pool to allocate all memory from
 * @param hostname The hostname of the virtual hsot
 * @param main_server The main server for this Apache configuration
 * @param ps Place to store the new server_rec
 * return Error string on error, NULL on success
 *}
function ap_init_virtual_host(p: Papr_pool_t;
                              const hostname: PChar;
                              main_server: Pserver_rec;
                              ps: PPserver_rec): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_init_virtual_host' + LibSuff16;

{**
 * Process a config file for Apache
 * @param s The server rec to use for the command parms
 * @param fname The name of the config file
 * @param conftree The root node of the created config tree
 * @param p Pool for general allocation
 * @param ptemp Pool for temporary allocation
 *}
function ap_process_resource_config(s: Pserver_rec;
                                    const fname: PChar;
                                    conftree: PPap_directive_t;
                                    p, ptemp: Papr_pool_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_process_resource_config' + LibSuff20;

{**
 * Process all matching files as Apache configs
 * @param s The server rec to use for the command parms
 * @param fname The filename pattern of the config file
 * @param conftree The root node of the created config tree
 * @param p Pool for general allocation
 * @param ptemp Pool for temporary allocation
 * @param optional Whether a no-match wildcard is allowed
 * @see apr_fnmatch for pattern handling
 *}
function ap_process_fnmatch_configs(s: Pserver_rec;
                                    const fname: PChar;
                                    conftree: PPap_directive_t;
                                    p, ptemp: Papr_pool_t;
                                    optional: Longint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_process_fnmatch_configs' + LibSuff24;

{**
 * Process all directives in the config tree
 * @param s The server rec to use in the command parms
 * @param conftree The config tree to process
 * @param p The pool for general allocation
 * @param ptemp The pool for temporary allocations
 * @return OK if no problems
 *}
function ap_process_config_tree(s: Pserver_rec;
                                conftree: Pap_directive_t;
                                p, ptemp: Papr_pool_t): Longint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_process_config_tree' + LibSuff16;

{**
 * Store data which will be retained across unload/load of modules
 * @param key The unique key associated with this module's retained data
 * @param size in bytes of the retained data (to be allocated)
 * @return Address of new retained data structure, initially cleared
 *}
function ap_retained_data_create(const key: Pchar; size: apr_size_t): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_retained_data_create' + LibSuff8;

{**
 * Retrieve data which was stored by ap_retained_data_create()
 * @param key The unique key associated with this module's retained data
 * @return Address of previously retained data structure, or NULL if not yet saved
 *}
function ap_retained_data_get(const key: Pchar): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_retained_data_get' + LibSuff4;

{* Module-method dispatchers, also for http_request.c *}
{**
 * Run the handler phase of each module until a module accepts the
 * responsibility of serving the request
 * @param r The current request
 * @return The status of the current request
 *}
function ap_invoke_handler(r: Prequest_rec): Longint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_invoke_handler' + LibSuff4;

{* for mod_perl *}

{**
 * Find a given directive in a command_rec table
 * @param name The directive to search for
 * @param cmds The table to search
 * @return The directive definition of the specified directive
 *}
function ap_find_command(const name: PChar;
                         const cmds: Pcommand_rec): Pcommand_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_command' + LibSuff8;

{**
 * Find a given directive in a list of modules.
 * @param cmd_name The directive to search for
 * @param mod Pointer to the first module in the linked list; will be set to
 *            the module providing cmd_name
 * @return The directive definition of the specified directive.
 *         *mod will be changed to point to the module containing the
 *         directive.
 *}
function ap_find_command_in_modules(const cmd_name: PChar;
                                    mod_: PPmodule): Pcommand_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_find_command_in_modules' + LibSuff8;

{**
 * Ask a module to create per-server and per-section (dir/loc/file) configs
 * (if it hasn't happened already). The results are stored in the server's
 * config, and the specified per-section config vector.
 * @param server The server to operate upon.
 * @param section_vector The per-section config vector.
 * @param section Which section to create a config for.
 * @param mod The module which is defining the config data.
 * @param pconf A pool for all configuration allocations.
 * @return The (new) per-section config data.
 *}
function ap_set_config_vectors(server: Pserver_rec;
                               section_vector: Pap_conf_vector_t;
                               const section: PChar;
                               mod_: Pmodule;
                               pconf: Papr_pool_t): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_set_config_vectors' + LibSuff20;

  {* Hooks *}

{**
 * Run the header parser functions for each module
 * @param r The current request
 * @return OK or DECLINED
 *}
//AP_DECLARE_HOOK(int,header_parser,(request_rec *r))
(*macro expanded:
 typedef
  int ap_HOOK_header_parser_t (request_rec *r);
  void ap_hook_header_parser(ap_HOOK_header_parser_t *pf,
                             const char * const *aszPre,
                             const char * const *aszSucc,
                             int nOrder);

  int ap_run_header_parser (request_rec *r);

  apr_array_header_t * ap_hook_get_header_parser(void);

  struct ap_LINK_header_parser_t {
    ap_HOOK_header_parser_t *pFunc;
    const char *szName;
    const char * const *aszPredecessors;
    const char * const *aszSuccessors;
    int nOrder;
  } ap_LINK_header_parser_t;
*)
type
  ap_HOOK_header_parser_t = function(r: Prequest_rec): Longint; cdecl;
  procedure ap_hook_header_parser(pf: ap_HOOK_header_parser_t;
                                  const aszPre: PPChar;
                                  const aszSucc: PPChar;
                                  nOrder: Longint);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_header_parser' + LibSuff16;

  function ap_run_header_parser(r: Prequest_rec): Longint;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_header_parser' + LibSuff4;

  function ap_hook_get_header_parser: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_header_parser' + LibSuff0;

  {rest of macro is ignored (struct ap_LINK_header_parser_t)}

{**
 * Run the pre_config function for each module
 * @param pconf The config pool
 * @param plog The logging streams pool
 * @param ptemp The temporary pool
 * @return OK or DECLINED on success anything else is a error
 *}
//AP_DECLARE_HOOK(int,pre_config,(apr_pool_t *pconf,apr_pool_t *plog,
//                                apr_pool_t *ptemp))
(*macro expanded:
  typedef
   int ap_HOOK_pre_config_t (apr_pool_t *pconf,apr_pool_t *plog, apr_pool_t *ptemp);

   void ap_hook_pre_config(ap_HOOK_pre_config_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);

   int ap_run_pre_config (apr_pool_t *pconf,apr_pool_t *plog, apr_pool_t *ptemp);

   apr_array_header_t * ap_hook_get_pre_config(void);

   struct ap_LINK_pre_config_t { ap_HOOK_pre_config_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_pre_config_t;
*)
type
   ap_HOOK_pre_config_t = function(pconf, plog, ptemp: Papr_pool_t): Longint; cdecl;

   procedure ap_hook_pre_config(pf: ap_HOOK_pre_config_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Longint);
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_hook_pre_config' + LibSuff16;

   function ap_run_pre_config(pconf, plog, ptemp: Papr_pool_t): Longint;
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_run_pre_config' + LibSuff12;

   function ap_hook_get_pre_config: Papr_array_header_t;
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_hook_get_pre_config' + LibSuff0;

   {rest of macro is ignored}

{**
 * Run the check_config function for each module
 * @param pconf The config pool
 * @param plog The logging streams pool
 * @param ptemp The temporary pool
 * @param s the server to operate upon
 * @return OK or DECLINED on success anything else is a error
 *}
//AP_DECLARE_HOOK(int,check_config,(apr_pool_t *pconf, apr_pool_t *plog,
//                                  apr_pool_t *ptemp, server_rec *s))
(*macro expanded:
   int ap_HOOK_check_config_t (apr_pool_t *pconf, apr_pool_t *plog, apr_pool_t *ptemp, server_rec *s);
   void ap_hook_check_config(ap_HOOK_check_config_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);

   int ap_run_check_config (apr_pool_t *pconf, apr_pool_t *plog, apr_pool_t *ptemp, server_rec *s);

   apr_array_header_t * ap_hook_get_check_config(void);

   struct ap_LINK_check_config_t { ap_HOOK_check_config_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_check_config_t;
*)
type
   ap_HOOK_check_config_t = function(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Longint; cdecl;
   procedure ap_hook_check_config(pf: ap_HOOK_check_config_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Longint);
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_hook_check_config' + LibSuff16;

   function ap_run_check_config(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Longint;
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_run_check_config' + LibSuff16;

   function ap_hook_get_check_config: Papr_array_header_t;
     {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
     external LibHTTPD name LibNamePrefix + 'ap_hook_get_check_config' + LibSuff0;

   {rest of macro is ignored}

{**
 * Run the test_config function for each module; this hook is run
 * only if the server was invoked to test the configuration syntax.
 * @param pconf The config pool
 * @param s The list of server_recs
 * @note To avoid reordering problems due to different buffering, hook
 *       functions should only apr_file_*() to print to stdout/stderr and
 *       not simple printf()/fprintf().
 *     
 *}
//AP_DECLARE_HOOK(void,test_config,(apr_pool_t *pconf, server_rec *s))
(*macro expanded:
   void ap_HOOK_test_config_t (apr_pool_t *pconf, server_rec *s);
   void ap_hook_test_config(ap_HOOK_test_config_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
   void ap_run_test_config (apr_pool_t *pconf, server_rec *s);
   apr_array_header_t * ap_hook_get_test_config(void);

   typedef struct ap_LINK_test_config_t { ap_HOOK_test_config_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_test_config_t;
*)
type
  ap_HOOK_test_config_t = procedure(pconf: Papr_pool_t; s: Pserver_rec); cdecl;

  procedure ap_hook_test_config(pf: ap_HOOK_test_config_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_test_config' + LibSuff16;

  procedure ap_run_test_config(pconf: Papr_pool_t; s: Pserver_rec);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_test_config' + LibSuff8;

  function ap_hook_get_test_config: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_test_config' + LibSuff0;

  {rest of macro is ignored}

{**
 * Run the post_config function for each module
 * @param pconf The config pool
 * @param plog The logging streams pool
 * @param ptemp The temporary pool
 * @param s The list of server_recs
 * @return OK or DECLINED on success anything else is a error
 *}
//AP_DECLARE_HOOK(int,post_config,(apr_pool_t *pconf,apr_pool_t *plog,
//                                 apr_pool_t *ptemp,server_rec *s))
(*macro expanded:
  int ap_HOOK_post_config_t (apr_pool_t *pconf,apr_pool_t *plog, apr_pool_t *ptemp,server_rec *s);

  void ap_hook_post_config(ap_HOOK_post_config_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
  int ap_run_post_config (apr_pool_t *pconf,apr_pool_t *plog, apr_pool_t *ptemp,server_rec *s);
  apr_array_header_t * ap_hook_get_post_config(void);

  struct ap_LINK_post_config_t { ap_HOOK_post_config_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_post_config_t;
*)
type
  ap_HOOK_post_config_t = function(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Longint; cdecl;

  procedure ap_hook_post_config(pf: ap_HOOK_post_config_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_post_config' + LibSuff16;

  function ap_run_post_config(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Longint;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_post_config' + LibSuff16;

  function ap_hook_get_post_config: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_post_config' + LibSuff0;

  {rest of macro is ignored}

{**
 * Run the open_logs functions for each module
 * @param pconf The config pool
 * @param plog The logging streams pool
 * @param ptemp The temporary pool
 * @param s The list of server_recs
 * @return OK or DECLINED on success anything else is a error
 *}
//AP_DECLARE_HOOK(int,open_logs,(apr_pool_t *pconf,apr_pool_t *plog,
//                               apr_pool_t *ptemp,server_rec *s))
(*macro expanded:
  int ap_HOOK_open_logs_t (apr_pool_t *pconf,apr_pool_t *plog, apr_pool_t *ptemp,server_rec *s);

  void ap_hook_open_logs(ap_HOOK_open_logs_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
  int ap_run_open_logs (apr_pool_t *pconf,apr_pool_t *plog, apr_pool_t *ptemp,server_rec *s);
  apr_array_header_t * ap_hook_get_open_logs(void);

  struct ap_LINK_open_logs_t { ap_HOOK_open_logs_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_open_logs_t;
*)
type
  ap_HOOK_open_logs_t = function(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Longint; cdecl;

  procedure ap_hook_open_logs(pf: ap_HOOK_open_logs_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_open_logs' + LibSuff16;

  function ap_run_open_logs(pconf, plog, ptemp: Papr_pool_t; s: Pserver_rec): Longint;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_open_logs' + LibSuff16;

  function ap_hook_get_open_logs: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_open_logs' + LibSuff0;

  {rest of macro is ignored}

{**
 * Run the child_init functions for each module
 * @param pchild The child pool
 * @param s The list of server_recs in this server
 *}
//AP_DECLARE_HOOK(void,child_init,(apr_pool_t *pchild, server_rec *s))
(*macro expanded:
  void ap_HOOK_child_init_t (apr_pool_t *pchild, server_rec *s);

  void ap_hook_child_init(ap_HOOK_child_init_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
  void ap_run_child_init (apr_pool_t *pchild, server_rec *s);
  apr_array_header_t * ap_hook_get_child_init(void);

  struct ap_LINK_child_init_t { ap_HOOK_child_init_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_child_init_t;
*)
type
  ap_HOOK_child_init_t = procedure(pchild: Papr_pool_t; s: Pserver_rec); cdecl;

  procedure ap_hook_child_init(pf: ap_HOOK_child_init_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_child_init' + LibSuff16;

  procedure ap_run_child_init(pchild: Papr_pool_t; s: Pserver_rec);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_child_init' + LibSuff8;

  function ap_hook_get_child_init: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_child_init' + LibSuff0;

  {rest of macro is ignored}

{**
 * Run the handler functions for each module
 * @param r The request_rec
 * @remark non-wildcard handlers should HOOK_MIDDLE, wildcard HOOK_LAST
 *}
//AP_DECLARE_HOOK(int,handler,(request_rec *r))
(*macro expanded:
    int ap_HOOK_handler_t (request_rec *r);

    void ap_hook_handler(ap_HOOK_handler_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
    int ap_run_handler (request_rec *r);
    apr_array_header_t * ap_hook_get_handler(void);

    struct ap_LINK_handler_t { ap_HOOK_handler_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_handler_t;
*)
type
  ap_HOOK_handler_t = function(r: Prequest_rec): Longint; cdecl;

  procedure ap_hook_handler(pf: ap_HOOK_handler_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_handler' + LibSuff16;

  function ap_run_handler(r: Prequest_rec): Longint;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_handler' + LibSuff4;

  function ap_hook_get_handler: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_handler' + LibSuff0;

  {rest of macro is ignored}

{**
 * Run the quick handler functions for each module. The quick_handler
 * is run before any other requests hooks are called (location_walk,
 * directory_walk, access checking, et. al.). This hook was added
 * to provide a quick way to serve content from a URI keyed cache.
 *
 * @param r The request_rec
 * @param lookup_uri Controls whether the caller actually wants content or not.
 * lookup is set when the quick_handler is called out of
 * ap_sub_req_lookup_uri()
 *}
//AP_DECLARE_HOOK(int,quick_handler,(request_rec *r, int lookup_uri))
(*macro expanded:
  int ap_HOOK_quick_handler_t (request_rec *r, int lookup_uri);

  void ap_hook_quick_handler(ap_HOOK_quick_handler_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
  int ap_run_quick_handler (request_rec *r, int lookup_uri);
  apr_array_header_t * ap_hook_get_quick_handler(void);

  struct ap_LINK_quick_handler_t { ap_HOOK_quick_handler_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_quick_handler_t;
*)
type
  ap_HOOK_quick_handler_t = function(r: Prequest_rec; lookup_uri: Longint): Longint; cdecl;

  procedure ap_hook_quick_handler(pf: ap_HOOK_quick_handler_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_quick_handler' + LibSuff16;

  function ap_run_quick_handler(r: Prequest_rec; lookup_uri: Longint): Longint;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_quick_handler' + LibSuff8;

  function ap_hook_get_quick_handler: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_quick_handler' + LibSuff0;

  {rest of macro is ignored}

{**
 * Retrieve the optional functions for each module.
 * This is run immediately before the server starts. Optional functions should
 * be registered during the hook registration phase.
 *}
//AP_DECLARE_HOOK(void,optional_fn_retrieve,(void))
(*macro expanded:
  void ap_HOOK_optional_fn_retrieve_t (void);

  void ap_hook_optional_fn_retrieve(ap_HOOK_optional_fn_retrieve_t *pf, const char * const *aszPre, const char * const *aszSucc, int nOrder);
  void ap_run_optional_fn_retrieve (void);
  apr_array_header_t * ap_hook_get_optional_fn_retrieve(void);

  struct ap_LINK_optional_fn_retrieve_t { ap_HOOK_optional_fn_retrieve_t *pFunc; const char *szName; const char * const *aszPredecessors; const char * const *aszSuccessors; int nOrder; } ap_LINK_optional_fn_retrieve_t;
*)
type
  ap_HOOK_optional_fn_retrieve_t = procedure; cdecl;

  procedure ap_hook_optional_fn_retrieve(pf: ap_HOOK_optional_fn_retrieve_t; const aszPre: PPChar; const aszSucc: PPChar; nOrder: Integer);
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_optional_fn_retrieve' + LibSuff16;

  procedure ap_run_optional_fn_retrieve;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_run_optional_fn_retrieve' + LibSuff0;

  function ap_hook_get_optional_fn_retrieve: Papr_array_header_t;
    {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
    external LibHTTPD name LibNamePrefix + 'ap_hook_get_optional_fn_retrieve' + LibSuff0;

  {rest of macro is ignored}

{**
 * A generic pool cleanup that will reset a pointer to NULL. For use with
 * apr_pool_cleanup_register.
 * @param data The address of the pointer
 * @return APR_SUCCESS
 *}
//AP_DECLARE_NONSTD(apr_status_t) ap_pool_cleanup_set_null(void *data);
function ap_pool_cleanup_set_null(data: Pointer): apr_status_t; cdecl;
  external LibHTTPD name LibNamePrefix + 'ap_pool_cleanup_set_null';

{$endif}  {* !APACHE_HTTP_CONFIG_H *}
(** @} *)
