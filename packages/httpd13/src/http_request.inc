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
 * about which was allocated in its pool elsewhere before doing this.
 }

function ap_sub_req_lookup_uri(const new_file: PChar;
 const r: Prequest_rec): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_sub_req_lookup_file(const new_file: PChar;
 const r: Prequest_rec): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_sub_req_method_uri(const method, new_file: PChar;
 const r: Prequest_rec): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;


function ap_run_sub_req(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_destroy_sub_req(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{
 * Then there's the case that you want some other request to be served
 * as the top-level request INSTEAD of what the client requested directly.
 * If so, call this from a handler, and then immediately return OK.
 }

procedure ap_internal_redirect(const new_uri: PChar; param2: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_internal_redirect_handler(const new_uri: PChar; param2: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_some_auth_required(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_is_initial_req(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_update_mtime(r: Prequest_rec; dependency_mtime: time_t): time_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{$ifdef CORE_PRIVATE}
{ Function called by main.c to handle first-level request }
API_EXPORT(void) ap_process_request(request_rec *);
API_EXPORT(void) ap_die(int type, request_rec *r);
{$endif}

