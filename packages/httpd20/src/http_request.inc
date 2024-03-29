{ Copyright 1999-2005 The Apache Software Foundation or its licensors, as
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

{#include "apr_hooks.h"
#include "util_filter.h"}

const
  AP_SUBREQ_NO_ARGS = 0;
  AP_SUBREQ_MERGE_ARGS = 1;

{
 * @file http_request.h
 * @brief Apache Request library
 }

{ http_request.c is the code which handles the main line of request
 * processing, once a request has been read in (finding the right per-
 * directory configuration, building it if necessary, and calling all
 * the module dispatch functions in the right order).
 *
 * The pieces here which are public to the modules, allow them to learn
 * how the server would handle some other file or URI, or perhaps even
 * direct the server to serve that other file instead of the one the
 * client requested directly.
 *
 * There are two ways to do that.  The first is the sub_request mechanism,
 * which handles looking up files and URIs as adjuncts to some other
 * request (e.g., directory entries for multiviews and directory listings);
 * the lookup functions stop short of actually running the request, but
 * (e.g., for includes), a module may call for the request to be run
 * by calling run_sub_req.  The space allocated to create sub_reqs can be
 * reclaimed by calling destroy_sub_req --- be sure to copy anything you care
 * about which was allocated in its apr_pool_t elsewhere before doing this.
 }

{
 * An internal handler used by the ap_process_request, all subrequest mechanisms
 * and the redirect mechanism.
 * @param r The request, subrequest or internal redirect to pre-process
 * @return The return code for the request
 }
function ap_process_request_internal(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_process_request_internal' + LibSuff4;

{
 * Create a subrequest from the given URI.  This subrequest can be
 * inspected to find information about the requested URI
 * @param new_uri The URI to lookup
 * @param r The current request
 * @param next_filter The first filter the sub_request should use.  If this is
 *                    NULL, it defaults to the first filter for the main request
 * @return The new request record
 * @deffunc request_rec * ap_sub_req_lookup_uri(const AnsiChar *new_uri, const request_rec *r)
 }
function ap_sub_req_lookup_uri(const new_uri: PAnsiChar;
 const r: Prequest_rec; next_filter: Pap_filter_t): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_sub_req_lookup_uri' + LibSuff12;

{
 * Create a subrequest for the given file.  This subrequest can be
 * inspected to find information about the requested file
 * @param new_file The file to lookup
 * @param r The current request
 * @param next_filter The first filter the sub_request should use.  If this is
 *                    NULL, it defaults to the first filter for the main request
 * @return The new request record
 * @deffunc request_rec * ap_sub_req_lookup_file(const AnsiChar *new_file, const request_rec *r)
 }
function ap_sub_req_lookup_file(const new_file: PAnsiChar;
 const r: Prequest_rec; next_filter: Pap_filter_t): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_sub_req_lookup_file' + LibSuff12;

{
 * Create a subrequest for the given apr_dir_read result.  This subrequest 
 * can be inspected to find information about the requested file
 * @param finfo The apr_dir_read result to lookup
 * @param r The current request
 * @param subtype What type of subrequest to perform, one of;
 * <PRE>
 *      AP_SUBREQ_NO_ARGS     ignore r->args and r->path_info
 *      AP_SUBREQ_MERGE_ARGS  merge r->args and r->path_info
 * </PRE>
 * @param next_filter The first filter the sub_request should use.  If this is
 *                    NULL, it defaults to the first filter for the main request
 * @return The new request record
 * @deffunc request_rec * ap_sub_req_lookup_dirent(apr_finfo_t *finfo, int subtype, const request_rec *r)
 * @tip The apr_dir_read flags value APR_FINFO_MIN|APR_FINFO_NAME flag is the 
 * minimum recommended query if the results will be passed to apr_dir_read.
 * The file info passed must include the name, and must have the same relative
 * directory as the current request.
 }
function ap_sub_req_lookup_dirent(const finfo: Papr_finfo_t;
 const r: Prequest_rec; subtype: Integer; next_filter: Pap_filter_t): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_sub_req_lookup_dirent' + LibSuff16;

{
 * Create a subrequest for the given URI using a specific method.  This
 * subrequest can be inspected to find information about the requested URI
 * @param method The method to use in the new subrequest
 * @param new_uri The URI to lookup
 * @param r The current request
 * @param next_filter The first filter the sub_request should use.  If this is
 *                    NULL, it defaults to the first filter for the main request
 * @return The new request record
 * @deffunc request_rec * ap_sub_req_method_uri(const AnsiChar *method, const AnsiChar *new_uri, const request_rec *r)
 }
function ap_sub_req_method_uri(const method, new_uri: PAnsiChar;
 const r: Prequest_rec; next_filter: Pap_filter_t): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_sub_req_method_uri' + LibSuff16;

{
 * An output filter to strip EOS buckets from sub-requests.  This always
 * has to be inserted at the end of a sub-requests filter stack.
 * @param f The current filter
 * @param bb The brigade to filter
 * @deffunc apr_status_t ap_sub_req_output_filter(ap_filter_t *f, apr_bucket_brigade *bb)
 }
function ap_sub_req_output_filter(f: Pap_filter_t;
 bb: Papr_bucket_brigade): apr_status_t;
 cdecl; external LibHTTPD name 'ap_sub_req_output_filter';

{
 * Run the handler for the subrequest
 * @param r The subrequest to run
 * @return The return code for the subrequest
 * @deffunc int ap_run_sub_req(request_rec *r)
 }
function ap_run_sub_req(r: Prequest_rec): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_run_sub_req' + LibSuff4;

{
 * Free the memory associated with a subrequest
 * @param r The subrequest to finish
 * @deffunc void ap_destroy_sub_req(request_rec *r)
 }
procedure ap_destroy_sub_req(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_destroy_sub_req' + LibSuff4;

{
 * Then there's the case that you want some other request to be served
 * as the top-level request INSTEAD of what the client requested directly.
 * If so, call this from a handler, and then immediately return OK.
 }

{
 * Redirect the current request to some other uri
 * @param new_uri The URI to replace the current request with
 * @param r The current request
 * @deffunc void ap_internal_redirect(const AnsiChar *new_uri, request_rec *r)
 }
procedure ap_internal_redirect(const new_uri: PAnsiChar; r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_internal_redirect' + LibSuff8;

{
 * This function is designed for things like actions or CGI scripts, when
 * using AddHandler, and you want to preserve the content type across
 * an internal redirect.
 * @param new_uri The URI to replace the current request with.
 * @param r The current request
 * @deffunc void ap_internal_redirect_handler(const AnsiChar *new_uri, request_rec *r)
 }
procedure ap_internal_redirect_handler(const new_uri: PAnsiChar; r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_internal_redirect_handler' + LibSuff8;

{
 * Redirect the current request to a sub_req, merging the pools
 * @param sub_req A subrequest created from this request
 * @param r The current request
 * @deffunc void ap_internal_fast_redirect(request_rec *sub_req, request_rec *r)
 * @tip the sub_req's pool will be merged into r's pool, be very careful
 * not to destroy this subrequest, it will be destroyed with the main request!
 }
procedure ap_internal_fast_redirect(sub_req, r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_internal_fast_redirect' + LibSuff8;

{
 * Can be used within any handler to determine if any authentication
 * is required for the current request
 * @param r The current request
 * @return 1 if authentication is required, 0 otherwise
 * @deffunc int ap_some_auth_required(request_rec *r)
 }
function ap_some_auth_required(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_some_auth_required' + LibSuff4;

{
 * Determine if the current request is the main request or a subrequest
 * @param r The current request
 * @return 1 if this is the main request, 0 otherwise
 * @deffunc int ap_is_initial_req(request_rec *r)
 }
function ap_is_initial_req(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_is_initial_req' + LibSuff4;

{
 * Function to set the r->mtime field to the specified value if it's later
 * than what's already there.
 * @param r The current request
 * @param dependency_time Time to set the mtime to
 * @deffunc void ap_update_mtime(request_rec *r, apr_time_t dependency_mtime)
 }
procedure ap_update_mtime(r: Prequest_rec; dependency_mtime: apr_time_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_update_mtime' + LibSuff12;

{
 * Add one or more methods to the list permitted to access the resource.
 * Usually executed by the content handler before the response header is
 * sent, but sometimes invoked at an earlier phase if a module knows it
 * can set the list authoritatively.  Note that the methods are ADDED
 * to any already permitted unless the reset flag is non-zero.  The
 * list is used to generate the Allow response header field when it
 * is needed.
 * @param   r     The pointer to the request identifying the resource.
 * @param   reset Boolean flag indicating whether this list should
 *                completely replace any current settings.
 * @param   ...   A NULL-terminated list of strings, each identifying a
 *                method name to add.
 * @return  None.
 * @deffunc void ap_allow_methods(request_rec *r, int reset, ...)
 }
procedure ap_allow_methods(r: Prequest_rec; reset: Integer; others: array of const);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name 'ap_allow_methods';

//AP_DECLARE(void) (request_rec *r, int reset, ...);

{
 * Add one or more methods to the list permitted to access the resource.
 * Usually executed by the content handler before the response header is
 * sent, but sometimes invoked at an earlier phase if a module knows it
 * can set the list authoritatively.  Note that the methods are ADDED
 * to any already permitted unless the reset flag is non-zero.  The
 * list is used to generate the Allow response header field when it
 * is needed.
 * @param   r     The pointer to the request identifying the resource.
 * @param   reset Boolean flag indicating whether this list should
 *                completely replace any current settings.
 * @param   ...   A list of method identifiers, from the "M_" series
 *                defined in httpd.h, terminated with a value of -1
 *                (e.g., "M_GET, M_POST, M_OPTIONS, -1")
 * @return  None.
 * @deffunc void ap_allow_standard_methods(request_rec *r, int reset, ...)
 }
procedure ap_allow_standard_methods(r: Prequest_rec; reset: Integer; others: array of const);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name 'ap_allow_standard_methods';

//AP_DECLARE(void) (request_rec *r, int reset, ...);

const
  MERGE_ALLOW = 0;
  REPLACE_ALLOW = 1;

//#ifdef CORE_PRIVATE
{ Function called by main.c to handle first-level request }
//void ap_process_request(request_rec *);
{
 * Kill the current request
 * @param type Why the request is dieing
 * @param r The current request
 * @deffunc void ap_die(int type, request_rec *r)
 }
procedure ap_die(type_: Integer; r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_die' + LibSuff8;

//#endif COREPRIVATE

{ Hooks }

{
 * Gives modules a chance to create their request_config entry when the
 * request is created.
 * @param r The current request
 * @ingroup hooks
 }
type
  ap_HOOK_create_request_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_create_request(pf: ap_HOOK_create_request_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_create_request' + LibSuff16;

{
 * This hook allow modules an opportunity to translate the URI into an
 * actual filename.  If no modules do anything special, the server's default
 * rules will be followed.
 * @param r The current request
 * @return OK, DECLINED, or HTTP_...
 * @ingroup hooks
 }
type
  ap_HOOK_translate_name_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_translate_name(pf: ap_HOOK_translate_name_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_translate_name' + LibSuff16;

{
 * This hook allow modules to set the per_dir_config based on their own
 * context (such as <Proxy > sections) and responds to contextless requests 
 * such as TRACE that need no security or filesystem mapping.
 * based on the filesystem.
 * @param r The current request
 * @return DONE (or HTTP_) if this contextless request was just fulfilled 
 * (such as TRACE), OK if this is not a file, and DECLINED if this is a file.
 * The core map_to_storage (HOOK_RUN_REALLY_LAST) will directory_walk
 * and file_walk the r->filename.
 * 
 * @ingroup hooks
 }
type
  ap_HOOK_map_to_storage_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_map_to_storage(pf: ap_HOOK_map_to_storage_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_map_to_storage' + LibSuff16;

{
 * This hook is used to analyze the request headers, authenticate the user,
 * and set the user information in the request record (r->user and
 * r->ap_auth_type). This hook is only run when Apache determines that
 * authentication/authorization is required for this resource (as determined
 * by the 'Require' directive). It runs after the access_checker hook, and
 * before the auth_checker hook.
 *
 * @param r The current request
 * @return OK, DECLINED, or HTTP_...
 * @ingroup hooks
 }
type
  ap_HOOK_check_user_id_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_check_user_id(pf: ap_HOOK_check_user_id_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_check_user_id' + LibSuff16;

{
 * Allows modules to perform module-specific fixing of header fields.  This
 * is invoked just before any content-handler
 * @param r The current request
 * @return OK, DECLINED, or HTTP_...
 * @ingroup hooks
 }
type
  ap_HOOK_fixups_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_fixups(pf: ap_HOOK_fixups_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_fixups' + LibSuff16;

{
 * This routine is called to determine and/or set the various document type
 * information bits, like Content-type (via r->content_type), language, et
 * cetera.
 * @param r the current request
 * @return OK, DECLINED, or HTTP_...
 * @ingroup hooks
 }
type
  ap_HOOK_type_checker_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_type_checker(pf: ap_HOOK_type_checker_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_type_checker' + LibSuff16;

{
 * This hook is used to apply additional access control to this resource.
 * It runs *before* a user is authenticated, so this hook is really to
 * apply additional restrictions independent of a user. It also runs
 * independent of 'Require' directive usage.
 *
 * @param r the current request
 * @return OK, DECLINED, or HTTP_...
 * @ingroup hooks
 }
type
  ap_HOOK_access_checker_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_access_checker(pf: ap_HOOK_access_checker_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_access_checker' + LibSuff16;

{
 * This hook is used to check to see if the resource being requested
 * is available for the authenticated user (r->user and r->ap_auth_type).
 * It runs after the access_checker and check_user_id hooks. Note that
 * it will *only* be called if Apache determines that access control has
 * been applied to this resource (through a 'Require' directive).
 *
 * @param r the current request
 * @return OK, DECLINED, or HTTP_...
 * @ingroup hooks
 }
type
  ap_HOOK_auth_checker_t = function (r: Prequest_rec): Integer; cdecl;

procedure ap_hook_auth_checker(pf: ap_HOOK_auth_checker_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_auth_checker' + LibSuff16;

{
 * This hook allows modules to insert filters for the current request
 * @param r the current request
 * @ingroup hooks
 }
type
  ap_HOOK_insert_filter_t = procedure (r: Prequest_rec); cdecl;

procedure ap_hook_insert_filter(pf: ap_HOOK_insert_filter_t; const aszPre: PPAnsiChar;
 const aszSucc: PPAnsiChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_insert_filter' + LibSuff16;

function ap_location_walk(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_location_walk' + LibSuff4;

function ap_directory_walk(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_directory_walk' + LibSuff4;

function ap_file_walk(r: Prequest_rec): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_file_walk' + LibSuff4;

