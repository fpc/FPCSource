(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/sessions.c

program sessions;

{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix, cmem, cutils, libmicrohttpd;

const
  (**
   * Invalid method page.
   *)
  METHOD_ERROR = '<html><head><title>Illegal request</title></head><body>Go away.</body></html>';

  (**
   * Invalid URL page.
   *)
  NOT_FOUND_ERROR = '<html><head><title>Not found</title></head><body>Go away.</body></html>';

  (**
   * Front page. (/)
   *)
  MAIN_PAGE = '<html><head><title>Welcome</title></head><body><form action="/2" method="post">What is your name? <input type="text" name="v1" value="%s" /><input type="submit" value="Next" /></body></html>';

  (**
   * Second page. (/2)
   *)
  SECOND_PAGE = '<html><head><title>Tell me more</title></head><body><a href="/">previous</a> <form action="/S" method="post">%s, what is your job? <input type="text" name="v2" value="%s" /><input type="submit" value="Next" /></body></html>';

  (**
   * Second page (/S)
   *)
  SUBMIT_PAGE = '<html><head><title>Ready to submit?</title></head><body><form action="/F" method="post"><a href="/2">previous </a> <input type="hidden" name="DONE" value="yes" /><input type="submit" value="Submit" /></body></html>';

  (**
   * Last page.
   *)
  LAST_PAGE = '<html><head><title>Thank you</title></head><body>Thank you.</body></html>';

  (**
   * Name of our cookie.
   *)
  COOKIE_NAME = 'session';

type
  (**
   * State we keep for each user/session/browser.
   *)
  PSession = ^TSession;
  TSession = packed record
    (**
     * We keep all sessions in a linked list.
     *)
    next: PSession;

    (**
     * Unique ID for this session.
     *)
    sid: array[0..33] of Char;

    (**
     * Reference counter giving the number of connections
     * currently using this session.
     *)
    rc: cint;

    (**
     * Time when this session was last active.
     *)
    start: time_t;

    (**
     * String submitted via form.
     *)
    value_1: array[0..64] of Char;

    (**
     * Another value submitted via form.
     *)
    value_2: array[0..64] of Char;
  end;

  (**
   * Data kept per request.
   *)
  TRequest = packed record

    (**
     * Associated session.
     *)
    session: PSession;

    (**
     * Post processor handling form data (IF this is
     * a POST request).
     *)
    pp: PMHD_PostProcessor;

    (**
     * URL to serve in response to this POST (if this request
     * was a 'POST')
     *)
    post_url: pcchar;

  end;
  PRequest = ^TRequest;

var
  (**
   * Linked list of all active sessions.  Yes, O(n) but a
   * hash table would be overkill for a simple example...
   *)
  _sessions: PSession;

  (**
   * Return the session handle for this connection, or
   * create one if this is a new user.
   *)
  function get_session(connection: PMHD_Connection): PSession;
  var
    ret: PSession;
    cookie: pcchar;
  begin
    cookie := MHD_lookup_connection_value(connection, MHD_COOKIE_KIND, COOKIE_NAME);
    if cookie <> nil then
    begin
      (* find existing session *)
      ret := _sessions;
      while nil <> ret do
      begin
        if StrComp(cookie, ret^.sid) = 0 then
          Break;
        ret := ret^.next;
      end;
      if nil <> ret then
      begin
        Inc(ret^.rc);
        Exit(ret);
      end;
    end;
    (* create fresh session *)
    ret := CAlloc(1, SizeOf(TSession));
    if nil = ret then
    begin
      WriteLn(stderr, 'calloc error: ', strerror(errno^));
      Exit(nil);
    end;
    (* not a super-secure way to generate a random session ID,
       but should do for a simple example... *)
    snprintf(ret^.sid, SizeOf(ret^.sid), '%X%X%X%X', Cardinal(rand),
      Cardinal(rand), Cardinal(rand), Cardinal(rand));
    Inc(ret^.rc);
    ret^.start := FpTime;
    ret^.next := _sessions;
    _sessions := ret;
    Result := ret;
  end;

(**
 * Type of handler that generates a reply.
 *
 * @param cls content for the page (handler-specific)
 * @param mime mime type to use
 * @param session session information
 * @param connection connection to process
 * @param MHD_YES on success, MHD_NO on failure
 *)
type
  TPageHandler = function(cls: Pointer; mime: Pcchar; session: PSession;
    connection: PMHD_Connection): LongInt; cdecl;

  (**
   * Entry we generate for each page served.
   *)

  { TPage }

  TPage = packed record
    (**
     * Acceptable URL for this page.
     *)
    url: Pcchar;

    (**
     * Mime type to set for the page.
     *)
    mime: Pcchar;

    (**
     * Handler to call to generate response.
     *)
    handler: TPageHandler;

    (**
     * Extra argument to handler.
     *)
    handler_cls: Pcchar;
  end;

  (**
   * Add header to response to set a session cookie.
   *
   * @param session session to use
   * @param response response to modify
   *)
  procedure add_session_cookie(session: PSession; response: PMHD_Response);
  var
    cstr: array[0..256] of Char;
  begin
    snprintf(cstr, SizeOf(cstr), '%s=%s', COOKIE_NAME, session^.sid);
    if MHD_NO =
      MHD_add_response_header(response, MHD_HTTP_HEADER_SET_COOKIE, cstr) then
      WriteLn(stderr, 'Failed to set session cookie header!');
  end;

  (**
   * Handler that returns a simple static HTTP page that
   * is passed in via 'cls'.
   *
   * @param cls a 'const char *' with the HTML webpage to return
   * @param mime mime type to use
   * @param session session handle
   * @param connection connection to use
   *)
  function serve_simple_form(cls: Pointer; mime: Pcchar; session: PSession;
    connection: PMHD_Connection): cint; cdecl;
  var
    ret: cint;
    form: Pcchar;
    response: PMHD_Response;
  begin
    form := cls;
    (* return static form *)
    response := MHD_create_response_from_buffer(Length(form), Pointer(form),
      MHD_RESPMEM_PERSISTENT);
    add_session_cookie(session, response);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_ENCODING, mime);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  (**
   * Handler that adds the 'v1' value to the given HTML code.
   *
   * @param cls a 'const char *' with the HTML webpage to return
   * @param mime mime type to use
   * @param session session handle
   * @param connection connection to use
   *)
  function fill_v1_form(cls: Pointer; mime: Pcchar; session: PSession;
    connection: PMHD_Connection): cint; cdecl;
  var
    ret: cint;
    form: Pcchar;
    reply: Pcchar;
    response: PMHD_Response;
  begin
    form := cls;
    if asprintf(@reply, form, session^.value_1) = -1 then
      (* oops *)
      Exit(MHD_NO);
    (* return static form *)
    response := MHD_create_response_from_buffer(Length(reply), Pointer(reply),
      MHD_RESPMEM_MUST_FREE);
    add_session_cookie(session, response);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_ENCODING, mime);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  (**
   * Handler that adds the 'v1' and 'v2' values to the given HTML code.
   *
   * @param cls a 'const char *' with the HTML webpage to return
   * @param mime mime type to use
   * @param session session handle
   * @param connection connection to use
   *)
  function fill_v1_v2_form(cls: Pointer; mime: Pcchar; session: PSession;
    connection: PMHD_Connection): cint; cdecl;
  var
    ret: cint;
    form: Pcchar;
    reply: Pcchar;
    response: PMHD_Response;
  begin
    form := cls;
    if asprintf(@reply, form, session^.value_1, session^.value_2) = -1 then
      (* oops *)
      Exit(MHD_NO);
    (* return static form *)
    response := MHD_create_response_from_buffer(Length(reply), Pointer(reply),
      MHD_RESPMEM_MUST_FREE);
    add_session_cookie(session, response);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_ENCODING, mime);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  (**
   * Handler used to generate a 404 reply.
   *
   * @param cls a 'const char *' with the HTML webpage to return
   * @param mime mime type to use
   * @param session session handle
   * @param connection connection to use
   *)
  function not_found_page(cls: Pointer; mime: Pcchar; session: PSession;
    connection: PMHD_Connection): cint; cdecl;
  var
    ret: cint;
    response: PMHD_Response;
  begin
    (* unsupported HTTP method *)
    response := MHD_create_response_from_buffer(Length(NOT_FOUND_ERROR),
      Pcchar(NOT_FOUND_ERROR), MHD_RESPMEM_PERSISTENT);
    ret := MHD_queue_response(connection, MHD_HTTP_NOT_FOUND, response);
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_ENCODING, mime);
    MHD_destroy_response(response);
    Result := ret;
  end;

const
  (**
   * List of all pages served by this HTTP server.
   *)
  pages: array[0..4] of TPage = (
    (url: '/';  mime: 'text/html'; handler: @fill_v1_form; handler_cls: MAIN_PAGE),
    (url: '/2'; mime: 'text/html'; handler: @fill_v1_v2_form; handler_cls: SECOND_PAGE),
    (url: '/S'; mime: 'text/html'; handler: @serve_simple_form; handler_cls: SUBMIT_PAGE),
    (url: '/F'; mime: 'text/html'; handler: @serve_simple_form; handler_cls: LAST_PAGE),
    (url: nil; mime: nil; handler: @not_found_page; handler_cls: nil) (* 404 *)
  );

  (**
   * Iterator over key-value pairs where the value
   * maybe made available in increments and/or may
   * not be zero-terminated.  Used for processing
   * POST data.
   *
   * @param cls user-specified closure
   * @param kind type of the value
   * @param key 0-terminated key for the value
   * @param filename name of the uploaded file, NULL if not known
   * @param content_type mime-type of the data, NULL if not known
   * @param transfer_encoding encoding of the data, NULL if not known
   * @param data pointer to size bytes of data at the
   *              specified offset
   * @param off offset of data in the overall value
   * @param size number of bytes in data available
   * @return MHD_YES to continue iterating,
   *         MHD_NO to abort the iteration
   *)
  function post_iterator(cls: Pointer; kind: MHD_ValueKind; key: Pcchar;
    filename: Pcchar; content_type: Pcchar; transfer_encoding: Pcchar;
    data: Pcchar; off: cuint64; size: size_t): cint; cdecl;
  var
    request: PRequest;
    session: PSession;
  begin
    request := cls;
    session := request^.session;
    if StrComp('DONE', key) = 0 then
    begin
      WriteLn(stdout, Format('Session `%s'' submitted `%s'', `%s''', [
        session^.sid, session^.value_1, session^.value_2]));
      Exit(MHD_YES);
    end;
    if StrComp('v1', key) = 0 then
    begin
      if (size + off) > SizeOf(session^.value_1) then
        size := SizeOf(session^.value_1) - off;
      Move(data^, session^.value_1[off], size);
      if (size + off) < SizeOf(session^.value_1) then
        session^.value_1[size + off] := #0;
      Exit(MHD_YES);
    end;
    if StrComp('v2', key) = 0 then
    begin
      if (size + off) > SizeOf(session^.value_2) then
        size := SizeOf(session^.value_2) - off;
      Move(data^, session^.value_2[off], size);
      if (size + off) < SizeOf(session^.value_2) then
        session^.value_2[size + off] := #0;
      Exit(MHD_YES);
    end;
    WriteLn(stderr, Format('Unsupported form value `%s''', [key]));
    Result := MHD_YES;
  end;

  (**
   * Main MHD callback for handling requests.
   *
   *
   * @param cls argument given together with the function
   *        pointer when the handler was registered with MHD
   * @param connection handle to connection which is being processed
   * @param url the requested url
   * @param method the HTTP method used ("GET", "PUT", etc.)
   * @param version the HTTP version string (i.e. "HTTP/1.1")
   * @param upload_data the data being uploaded (excluding HEADERS,
   *        for a POST that fits into memory and that is encoded
   *        with a supported encoding, the POST data will NOT be
   *        given in upload_data and is instead available as
   *        part of MHD_get_connection_values; very large POST
   *        data *will* be made available incrementally in
   *        upload_data)
   * @param upload_data_size set initially to the size of the
   *        upload_data provided; the method must update this
   *        value to the number of bytes NOT processed;
   * @param ptr pointer that the callback can set to some
   *        address and that will be preserved by MHD for future
   *        calls for this request; since the access handler may
   *        be called many times (i.e., for a PUT/POST operation
   *        with plenty of upload data) this allows the application
   *        to easily associate some request-specific state.
   *        If necessary, this state can be cleaned up in the
   *        global "MHD_RequestCompleted" callback (which
   *        can be set with the MHD_OPTION_NOTIFY_COMPLETED).
   *        Initially, <tt>*con_cls</tt> will be NULL.
   * @return MHS_YES if the connection was handled successfully,
   *         MHS_NO if the socket must be closed due to a serios
   *         error while handling the request
   *)
  function create_response(cls: Pointer; connection: PMHD_Connection;
    url: Pcchar; method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  var
    response: PMHD_Response;
    request: PRequest;
    session: PSession;
    ret: cint;
    i: Cardinal;
  begin
    request := ptr^;
    if nil = request then
    begin
      request := CAlloc(1, SizeOf(TRequest));
      if nil = request then
      begin
        WriteLn(stderr, 'calloc error: ', strerror(errno^));
        Exit(MHD_NO);
      end;
      ptr^ := request;
      if StrComp(method, MHD_HTTP_METHOD_POST) = 0 then
      begin
        request^.pp := MHD_create_post_processor(connection, 1024,
          @post_iterator, request);
        if nil = request^.pp then
        begin
          WriteLn(stderr, Format('Failed to setup post processor for `%s''',
            [url]));
          Exit(MHD_NO); (* internal error *)
        end;
      end;
      Exit(MHD_YES);
    end;
    if nil = request^.session then
    begin
      request^.session := get_session(connection);
      if nil = request^.session then
      begin
        WriteLn(stderr, Format('Failed to setup session for `%s''', [url]));
        Exit(MHD_NO); (* internal error *)
      end;
    end;
    session := request^.session;
    session^.start := FpTime;
    if StrComp(method, MHD_HTTP_METHOD_POST) = 0 then
    begin
      (* evaluate POST data *)
      MHD_post_process(request^.pp, upload_data, upload_data_size^);
      if upload_data_size^ <> 0 then
      begin
        upload_data_size^ := 0;
        Exit(MHD_YES);
      end;
      (* done with POST data, serve response *)
      MHD_destroy_post_processor(request^.pp);
      request^.pp := nil;
      method := MHD_HTTP_METHOD_GET; (* fake 'GET' *)
      if nil <> request^.post_url then
        url := request^.post_url;
    end;
    if (StrComp(method, MHD_HTTP_METHOD_GET) = 0) or
      (StrComp(method, MHD_HTTP_METHOD_HEAD) = 0) then
    begin
      (* find out which page to serve *)
      i := 0;
      while (pages[i].url <> nil) and (StrComp(pages[i].url, url) <> 0) do
        Inc(i);
      ret := pages[i].handler(pages[i].handler_cls, pages[i].mime, session,
        connection);
      if ret <> MHD_YES then
        WriteLn(stderr, Format('Failed to create page for `%s''', [url]));
      Exit(ret);
    end;
    (* unsupported HTTP method *)
    response := MHD_create_response_from_buffer(Length(METHOD_ERROR),
      Pcchar(METHOD_ERROR), MHD_RESPMEM_PERSISTENT);
    ret := MHD_queue_response(connection, MHD_HTTP_NOT_ACCEPTABLE, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  (**
   * Callback called upon completion of a request.
   * Decrements session reference counter.
   *
   * @param cls not used
   * @param connection connection that completed
   * @param con_cls session handle
   * @param toe status code
   *)
  procedure request_completed_callback(cls: Pointer; connection: PMHD_Connection;
    con_cls: PPointer; toe: MHD_RequestTerminationCode);
  var
    request: PRequest;
  begin
    request := con_cls^;
    if nil = request then
      Exit;
    if nil <> request^.session then
      Dec(request^.session^.rc);
    if nil <> request^.pp then
      MHD_destroy_post_processor(request^.pp);
    Free(request);
  end;

  (**
   * Clean up handles of sessions that have been idle for
   * too long.
   *)
  procedure expire_sessions;
  var
    pos: PSession;
    prev: PSession;
    next: PSession;
    now: time_t;
  begin
    now := FpTime;
    prev := nil;
    pos := _sessions;
    while nil <> pos do
    begin
      next := pos^.next;
      if (now - pos^.start) > (60 * 60) then
      begin
        (* expire sessions after 1h *)
        if nil = prev then
          _sessions := pos^.next
        else
          prev^.next := next;
        Free(pos);
      end
      else
        prev := pos;
      pos := next;
    end;
  end;

(**
 * Call with the port number as the only argument.
 * Never terminates (other than by signals, such as CTRL-C).
 *)
var
  d: PMHD_Daemon;
  tv: timeval;
  tvp: ptimeval;
  rs: TFDSet;
  ws: TFDSet;
  es: TFDSet;
  max: cint;
  mhd_timeout: MHD_UNSIGNED_LONG_LONG;
begin
  if argc <> 2 then
  begin
    WriteLn(argv[0], ' PORT');
    Halt(1);
  end;
  (* initialize PRNG *)
  Randomize;

  d := MHD_start_daemon(MHD_USE_DEBUG, StrToInt(argv[1]), nil, nil,
    @create_response, nil, MHD_OPTION_CONNECTION_TIMEOUT, cuint(15),
    MHD_OPTION_NOTIFY_COMPLETED, @request_completed_callback, nil, MHD_OPTION_END);
  if nil = d then
    Halt(1);

  while True do
  begin
    expire_sessions;
    max := 0;
    fpFD_ZERO(rs);
    fpFD_ZERO(ws);
    fpFD_ZERO(es);
    if MHD_YES <> MHD_get_fdset(d, @rs, @ws, @es, @max) then
      Break; (* fatal internal error *)
    if MHD_get_timeout(d, @mhd_timeout) = MHD_YES then
    begin
      tv.tv_sec := mhd_timeout div 1000;
      tv.tv_usec := (mhd_timeout - (tv.tv_sec * 1000)) * 1000;
      tvp := @tv;
    end
    else
      tvp := nil;
    if -1 = fpSelect(max + 1, @rs, @ws, @es, tvp) then
    begin
      if (ESysEINTR <> errno^) then
        WriteLn(stderr, 'Aborting due to error during select: ', strerror(errno^));
      Break;
    end;
    MHD_run(d);
  end;
  MHD_stop_daemon(d);
end.
