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
 * Prototypes for routines which either talk directly back to the user,
 * or control the ones that eventually do.
 }

{ Read a request and fill in the fields. }

function ap_read_request(c: Pconn_rec): Prequest_rec;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Send a single HTTP header field }

function ap_send_header_field(r: Prequest_rec; const fieldname, fieldval: PChar): cint;
 cdecl; external LibHTTPD;

{ Send the minimal part of an HTTP response header... but modules should be
 * very careful about using this, and should prefer ap_send_http_header().
 * Much of the HTTP/1.1 implementation correctness depends on code in
 * ap_send_http_header().
 }
procedure ap_basic_http_header(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Send the Status-Line and header fields for HTTP response }

procedure ap_send_http_header(l: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Send the response to special method requests }

function ap_send_http_trace(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_send_http_options(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Finish up stuff after a request }

procedure ap_finalize_request_protocol(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Send error back to client... last arg indicates error status in case
 * we get an error in the process of trying to deal with an ErrorDocument
 * to handle some other error.  In that case, we print the default report
 * for the first thing that went wrong, and more briefly report on the
 * problem with the ErrorDocument.
 }

procedure ap_send_error_response(r: Prequest_rec; recursive_error: cint);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Set last modified header line from the lastmod date of the associated file.
 * Also, set content length.
 *
 * May return an error status, typically USE_LOCAL_COPY (that when the
 * permit_cache argument is set to one).
 }

function ap_set_content_length(r: Prequest_rec; length: clong): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_set_keepalive(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_rationalize_mtime(r: Prequest_rec; mtime: time_t): time_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_make_etag(r: Prequest_rec; force_weak: cint): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_set_etag(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_set_last_modified(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_meets_conditions(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Other ways to send stuff at the client.  All of these keep track
 * of bytes_sent automatically.  This indirection is intended to make
 * it a little more painless to slide things like HTTP-NG packetization
 * underneath the main body of the code later.  In the meantime, it lets
 * us centralize a bit of accounting (bytes_sent).
 *
 * These also return the number of bytes written by the call.
 * They should only be called with a timeout registered, for obvious reaasons.
 * (Ditto the send_header stuff).
 }

//function ap_send_fd(f: PFILE; r: Prequest_rec): clong;
// {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

//function ap_send_fd_length(f: PFILE; r: Prequest_rec; length: clong): clong;
// {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_send_fb(f: PBUFF; r: Prequest_rec): clong;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_send_fb_length(f: PBUFF; r: Prequest_rec): clong;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_send_mmap(mm: Pointer; r: Prequest_rec; offset, length: size_t): size_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Hmmm... could macrofy these for now, and maybe forever, though the
 * definitions of the macros would get a whole lot hairier.
 }

function ap_rputc(c: cint; r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_rputs(const str: PChar; r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_rwrite(const buf: Pointer; nbyte: cint; r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_rvputs(r: Prequest_rec; others: array of const): cint;
 cdecl; external LibHTTPD;

function ap_vrprintf(r: Prequest_rec; const fmt: PChar; vlist: va_list): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_rprintf(r: Prequest_rec; const fmt: PChar; others: array of const): cint;
 cdecl; external LibHTTPD;

{				__attribute__((format(printf,2,3)));}

function ap_rflush(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{
 * Index used in custom_responses array for a specific error code
 * (only use outside protocol.c is in getting them configured).
 }

function ap_index_of_response(status: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Reading a block of data from the client connection (e.g., POST arg) }

function ap_setup_client_block(r: Prequest_rec; read_policy: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_should_client_block(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_client_block(r: Prequest_rec; buffer: PChar; bufsiz: cint): clong;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_discard_request_body(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Sending a byterange }

function ap_set_byterange(r: Prequest_rec): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_each_byterange(r: Prequest_rec; offset, length: Pclong): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ Support for the Basic authentication protocol.  Note that there's
 * nothing that prevents these from being in mod_auth.c, except that other
 * modules which wanted to provide their own variants on finding users and
 * passwords for Basic auth (a fairly common request) would then require
 * mod_auth to be loaded or they wouldn't work.
 *
 * get_basic_auth_pw returns 0 (OK) if it set the 'pw' argument (and assured
 * a correct value in r->connection->user); otherwise it returns an error
 * code, either SERVER_ERROR if things are really confused, AUTH_REQUIRED
 * if no authentication at all seemed to be in use, or DECLINED if there
 * was authentication but it wasn't Basic (in which case, the caller should
 * presumably decline as well).
 *
 * note_basic_auth_failure arranges for the right stuff to be scribbled on
 * the HTTP return so that the client knows how to authenticate itself the
 * next time. As does note_digest_auth_failure for Digest auth.
 *
 * note_auth_failure does the same thing, but will call the correct one
 * based on the authentication type in use.
 *
 }

procedure ap_note_auth_failure(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_note_basic_auth_failure(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_note_digest_auth_failure(r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_basic_auth_pw(r: Prequest_rec; const pw: PPChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{
 * Setting up the protocol fields for subsidiary requests...
 * Also, a wrapup function to keep the internal accounting straight.
 }

procedure ap_set_sub_req_protocol(rnew, r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_finalize_sub_req_protocol(sub_r: Prequest_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ This is also useful for putting sub_reqs and internal_redirects together }

//CORE_EXPORT(void) ap_parse_uri(request_rec *r, const char *uri);

{ Get the method number associated with the given string, assumed to
 * contain an HTTP method.  Returns M_INVALID if not recognized.
 }
function ap_method_number_of(const method: PChar): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_getline(s: PChar; n: cint; in_: PBUFF; fold: cint): cint;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_get_chunk_size(b: PChar): clong;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

