(*
     This file is part of libmicrohttpd
     Copyright (C) 2013 Christian Grothoff (and other contributing authors)

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2.1 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(**
 * @file demo_https.pp (Original: demo_https.c)
 * @brief complex demonstration site: create directory index, offer
 *        upload via form and HTTP POST, download with mime type detection
 *        and error reporting (403, etc.) --- and all of this with
 *        high-performance settings (large buffers, thread pool).
 *        If you want to benchmark MHD, this code should be used to
 *        run tests against.  Note that the number of threads may need
 *        to be adjusted depending on the number of available cores.
 *        Logic is identical to demo.pp, just adds HTTPS support.
 * @author Christian Grothoff
 *)

program demo_https;

{$mode objfpc}{$H+}
{$MACRO ON}
{$IF DEFINED(CPU_COUNT) AND (CPU_COUNT + 0) < 2}
  {$UNDEF CPU_COUNT}
{$ENDIF}
{$IF NOT DEFINED(CPU_COUNT)}
  {$DEFINE CPU_COUNT := 2}
{$ENDIF}

uses
  sysutils, pthreads, ctypes, BaseUnix, cmem, cutils, libmicrohttpd;

type
{$i magic.inc}

const

  (**
   * Number of threads to run in the thread pool.  Should (roughly) match
   * the number of cores on your system.
   *)
  NUMBER_OF_THREADS = CPU_COUNT;

  (**
   * How many bytes of a file do we give to libmagic to determine the mime type?
   * 16k might be a bit excessive, but ought not hurt performance much anyway,
   * and should definitively be on the safe side.
   *)
  MAGIC_HEADER_SIZE = 16 * 1024;

  (**
   * Page returned for file-not-found.
   *)
  FILE_NOT_FOUND_PAGE: Pcchar = '<html><head><title>File not found</title></head><body>File not found</body></html>';

  (**
   * Page returned for internal errors.
   *)
  INTERNAL_ERROR_PAGE: Pcchar = '<html><head><title>Internal error</title></head><body>Internal error</body></html>';


  (**
   * Page returned for refused requests.
   *)
  REQUEST_REFUSED_PAGE: Pcchar = '<html><head><title>Request refused</title></head><body>Request refused (file exists?)</body></html>';

  (**
   * Head of index page.
   *)
  INDEX_PAGE_HEADER = '<html>'#10'<head><title>Welcome</title></head>'#10'<body>'#10+
    '<h1>Upload</h1>'#10+
    '<form method="POST" enctype="multipart/form-data" action="/">'#10+
    '<dl><dt>Content type:</dt><dd>'+
    '<input type="radio" name="category" value="books">Book</input>'+
    '<input type="radio" name="category" value="images">Image</input>'+
    '<input type="radio" name="category" value="music">Music</input>'+
    '<input type="radio" name="category" value="software">Software</input>'+
    '<input type="radio" name="category" value="videos">Videos</input>'#10+
    '<input type="radio" name="category" value="other" checked>Other</input></dd>'+
    '<dt>Language:</dt><dd>'+
    '<input type="radio" name="language" value="no-lang" checked>none</input>'+
    '<input type="radio" name="language" value="en">English</input>'+
    '<input type="radio" name="language" value="de">German</input>'+
    '<input type="radio" name="language" value="fr">French</input>'+
    '<input type="radio" name="language" value="es">Spanish</input></dd>'#10+
    '<dt>File:</dt><dd>'+
    '<input type="file" name="upload"/></dd></dl>'+
    '<input type="submit" value="Send!"/>'#10+
    '</form>'#10+
    '<h1>Download</h1>'#10+
    '<ol>'#10;

  (**
   * Footer of index page.
   *)
  INDEX_PAGE_FOOTER = '</ol>'#10'</body>'#10'</html>';

  (**
   * NULL-terminated array of supported upload categories.  Should match HTML
   * in the form.
   *)
  categories: array[0..6] of Pcchar = (
    'books',
    'images',
    'music',
    'software',
    'videos',
    'other',
    nil
  );

type

  (**
   * Specification of a supported language.
   *)
  Language = packed record
    (**
     * Directory name for the language.
     *)
    dirname: Pcchar;

    (**
     * Long name for humans.
     *)
    longname: Pcchar;
  end;
  PLanguage = ^Language;

const
  (**
   * NULL-terminated array of supported upload categories.  Should match HTML
   * in the form.
   *)
  languages: array[0..5] of Language = (
    (dirname: 'no-lang'; longname: 'No language specified'),
    (dirname: 'en'; longname: 'English'),
    (dirname: 'de'; longname: 'German'),
    (dirname: 'fr'; longname: 'French'),
    (dirname: 'es'; longname: 'Spanish'),
    (dirname: nil; longname: nil)
  );

var
  (**
   * Response returned if the requested file does not exist (or is not accessible).
   *)
  file_not_found_response: PMHD_Response;

  (**
   * Response returned for internal errors.
   *)
  internal_error_response: PMHD_Response;

  (**
   * Response returned for '/' (GET) to list the contents of the directory and allow upload.
   *)
  cached_directory_response: PMHD_Response;

  (**
   * Response returned for refused uploads.
   *)
  request_refused_response: PMHD_Response;

  (**
   * Mutex used when we update the cached directory response object.
   *)
  mutex: pthread_mutex_t;

  (**
   * Global handle to MAGIC data.
   *)
  magic: magic_t;

  (**
   * Mark the given response as HTML for the brower.
   *
   * @param response response to mark
   *)
  procedure mark_as_html(response: PMHD_Response);
  begin
    MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE, 'text/html');
  end;

  (**
   * Replace the existing 'cached_directory_response' with the
   * given response.
   *
   * @param response new directory response
   *)
  procedure update_cached_response(response: PMHD_Response);
  begin
    pthread_mutex_lock(@mutex);
    if nil <> cached_directory_response then
      MHD_destroy_response(cached_directory_response);
    cached_directory_response := response;
    pthread_mutex_unlock(@mutex);
  end;

type
  (**
   * Context keeping the data for the response we're building.
   *)
  ResponseDataContext = packed record
    (**
     * Response data string.
     *)
    buf: Pcchar;

    (**
     * Number of bytes allocated for 'buf'.
     *)
    buf_len: size_t;

    (**
     * Current position where we append to 'buf'. Must be smaller or equal to 'buf_len'.
     *)
    off: size_t;
  end;
  PResponseDataContext = ^ResponseDataContext;

  (**
   * Create a listing of the files in 'dirname' in HTML.
   *
   * @param rdc where to store the list of files
   * @param dirname name of the directory to list
   * @return MHD_YES on success, MHD_NO on error
   *)
  function list_directory(rdc: PResponseDataContext; dirname: Pcchar): cint; cdecl;
  var
    fullname: array[0..PATH_MAX] of AnsiChar;
    sbuf: stat;
    dir: pDir;
    de: pDirent;
    r: Pointer;
  begin
    dir := FpOpendir(dirname);
    if nil = dir then
      Exit(MHD_NO);
    while True do
    begin
      de := FpReaddir(dir^);
      if de = nil then
        Break;
      if '.' = de^.d_name[0] then
        Continue;
      if SizeOf(fullname) <= size_t(
        snprintf(fullname, SizeOf(fullname), '%s/%s', dirname, de^.d_name)) then
        Continue; (* ugh, file too long? how can this be!? *)
      if 0 <> FpStat(PAnsiChar(fullname), sbuf) then
        Continue; (* ugh, failed to 'stat' *)
      if not fpS_ISREG(sbuf.st_mode) then
        Continue; (* not a regular file, skip *)
      if rdc^.off + 1024 > rdc^.buf_len then
      begin
        if (2 * rdc^.buf_len + 1024) < rdc^.buf_len then
          Break; (* more than SIZE_T _index_ size? Too big for us *)
        rdc^.buf_len := 2 * rdc^.buf_len + 1024;
        r := ReAlloc(rdc^.buf, rdc^.buf_len);
        if nil = r then
          Break; (* out of memory *)
        rdc^.buf := r;
      end;
      rdc^.off += snprintf(@rdc^.buf[rdc^.off], rdc^.buf_len - rdc^.off,
                    '<li><a href="/%s">%s</a></li>'#10, fullname, de^.d_name);
    end;
    FpClosedir(dir^);
    Result := MHD_YES;
  end;

  (**
   * Re-scan our local directory and re-build the index.
   *)
  procedure update_directory;
  const
    initial_allocation: size_t = 32 * 1024; (* initial size for response buffer *)
  var
    response: PMHD_Response;
    rdc: ResponseDataContext;
    language_idx: cuint;
    category_idx: cuint;
    language: PLanguage;
    category: Pcchar;
    dir_name: array[0..128] of AnsiChar;
    sbuf: stat;
  begin
    rdc.buf_len := initial_allocation;
    rdc.buf := Malloc(rdc.buf_len);
    if nil = rdc.buf then
    begin
      update_cached_response(nil);
      Exit;
    end;
    rdc.off := snprintf(rdc.buf, rdc.buf_len, '%s', INDEX_PAGE_HEADER);
    language_idx := 0;
    while True do
    begin
      try
        if languages[language_idx].dirname = nil then
          Break;
        language := @languages[language_idx];
        if 0 <> FpStat(language^.dirname, sbuf) then
          Continue; (* empty *)
        (* we ensured always +1k room, filenames are ~256 bytes,
           so there is always still enough space for the header
           without need for an additional reallocation check. *)
        rdc.off += snprintf(@rdc.buf[rdc.off], rdc.buf_len - rdc.off,
                     '<h2>%s</h2>'#10, language^.longname);
        category_idx := 0;
        while True do
        begin
          try
            if categories[category_idx] = nil then
              Break;
            category := categories[category_idx];
            snprintf(dir_name, sizeof(dir_name), '%s/%s', language^.dirname, category);
            if 0 <> FpStat(PAnsiChar(dir_name), sbuf) then
              Continue; (* empty *)
            (* we ensured always +1k room, filenames are ~256 bytes,
               so there is always still enough space for the header
               without need for an additional reallocation check. *)
            rdc.off += snprintf(@rdc.buf[rdc.off], rdc.buf_len - rdc.off,
                         '<h3>%s</h3>'#10, category);
            if MHD_NO = list_directory(@rdc, dir_name) then
            begin
              Free(rdc.buf);
              update_cached_response(nil);
              Exit;
            end;
          finally
            Inc(category_idx);
          end;
        end;
      finally
        Inc(language_idx);
      end;
    end;
    (* we ensured always +1k room, filenames are ~256 bytes,
         so there is always still enough space for the footer
         without need for a final reallocation check. *)
    rdc.off += snprintf(@rdc.buf[rdc.off], rdc.buf_len - rdc.off, '%s',
      INDEX_PAGE_FOOTER);
    initial_allocation := rdc.buf_len; (* remember for next time *)
    response := MHD_create_response_from_buffer(rdc.off, rdc.buf,
      MHD_RESPMEM_MUST_FREE);
    mark_as_html(response);
{$IFDEF FORCE_CLOSE}
    MHD_add_response_header (response, MHD_HTTP_HEADER_CONNECTION, 'close');
{$ENDIF}
    update_cached_response(response);
  end;

type
  (**
   * Context we keep for an upload.
   *)
  UploadContext = packed record
    (**
     * Handle where we write the uploaded file to.
     *)
    fd: cint;

    (**
     * Name of the file on disk (used to remove on errors).
     *)
    filename: Pcchar;

    (**
     * Language for the upload.
     *)
    language: Pcchar;

    (**
     * Category for the upload.
     *)
    category: Pcchar;

    (**
     * Post processor we're using to process the upload.
     *)
    pp: PMHD_PostProcessor;

    (**
     * Handle to connection that we're processing the upload for.
     *)
    connection: PMHD_Connection;

    (**
     * Response to generate, NULL to use directory.
     *)
    response: PMHD_Response;
  end;
  PUploadContext = ^UploadContext;

  (**
   * Append the 'size' bytes from 'data' to '*ret', adding
   * 0-termination.  If '*ret' is NULL, allocate an empty string first.
   *
   * @param ret string to update, NULL or 0-terminated
   * @param data data to append
   * @param size number of bytes in 'data'
   * @return MHD_NO on allocation failure, MHD_YES on success
   *)
  function do_append(ret: Ppcchar; data: Pcchar; size: size_t): cint; cdecl;
  var
    buf: Pcchar;
    old_len: size_t;
  begin
    if nil = ret^ then
      old_len := 0
    else
      old_len := strlen(ret^);
    buf := Malloc(old_len + size + 1);
    if nil = buf then
      Exit(MHD_NO);
    Move(ret^^, buf, old_len);
    if nil <> ret^ then
      Free(ret^);
    Move(data^, buf[old_len], size);
    buf[old_len + size] := #0;
    ret^ := buf;
    Result := MHD_YES;
  end;

  (**
   * Iterator over key-value pairs where the value
   * maybe made available in increments and/or may
   * not be zero-terminated.  Used for processing
   * POST data.
   *
   * @param cls user-specified closure
   * @param kind type of the value, always MHD_POSTDATA_KIND when called from MHD
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
  function process_upload_data(cls: Pointer; kind: MHD_ValueKind; key: Pcchar;
    filename: Pcchar; content_type: Pcchar; transfer_encoding: Pcchar;
    data: Pcchar; off: cuint64; size: size_t): cint; cdecl;
  var
    uc: PUploadContext;
    i: cint;
    fn: array[0..PATH_MAX] of AnsiChar;
  begin
    uc := cls;
    if 0 = strcomp(key, 'category') then
      Exit(do_append(@uc^.category, data, size));
    if 0 = strcomp(key, 'language') then
      Exit(do_append(@uc^.language, data, size));
    if 0 <> strcomp(key, 'upload') then
    begin
      WriteLn(stderr, Format('Ignoring unexpected form value `%s''', [key]));
      Exit(MHD_YES); (* ignore *)
    end;
    if nil = filename then
    begin
      WriteLn(stderr, 'No filename, aborting upload');
      Exit(MHD_NO); (* no filename, error *)
    end;
    if (nil = uc^.category) or (nil = uc^.language) then
    begin
      WriteLn(stderr, Format('Missing form data for upload `%s''', [filename]));
      uc^.response := request_refused_response;
      Exit(MHD_NO);
    end;
    if -1 = uc^.fd then
    begin
      if (nil <> strstr(filename, '..')) or (nil <> strchr(filename, Ord('/'))) or
        (nil <> strchr(filename, Ord('\'))) then
      begin
        uc^.response := request_refused_response;
        Exit(MHD_NO);
      end;
      (* create directories -- if they don't exist already *)
{$IFDEF MSWINDOWS}
      FpMkdir(uc^.language);
{$ELSE}
      FpMkdir(uc^.language, S_IRWXU);
{$ENDIF}
      snprintf(fn, SizeOf(fn), '%s/%s', uc^.language, uc^.category);
{$IFDEF MSWINDOWS}
      FpMkdir(fn);
{$ELSE}
      FpMkdir(PAnsiChar(fn), S_IRWXU);
{$ENDIF}
      (* open file *)
      snprintf(fn, sizeof(fn), '%s/%s/%s', uc^.language, uc^.category, filename);
      for i := strlen(fn) - 1 downto 0 do
        if isprint(fn[i]) = 1 then
          fn[i] := '_';
      uc^.fd := FpOpen(PAnsiChar(fn), O_CREAT or O_EXCL
{$IFDEF O_LARGEFILE}
                  or O_LARGEFILE
{$ENDIF}
                  or O_WRONLY, S_IRUSR or S_IWUSR);
      if -1 = uc^.fd then
      begin
        WriteLn(stderr, Format('Error opening file `%s'' for upload: %s',
          [fn, strerror(errno^)]));
        uc^.response := request_refused_response;
        Exit(MHD_NO);
      end;
      uc^.filename := strdup(fn);
    end;
    if (0 <> size) and (size <> size_t(FpWrite(uc^.fd, data, size))) then
    begin
      (* write failed; likely: disk full *)
      WriteLn(stderr, Format('Error writing to file `%s'': %s', [uc^.filename,
        strerror(errno^)]));
      uc^.response := internal_error_response;
      FpClose(uc^.fd);
      uc^.fd := -1;
      if nil <> uc^.filename then
      begin
        FpUnlink(uc^.filename);
        Free(uc^.filename);
        uc^.filename := nil;
      end;
      Exit(MHD_NO);
    end;
    Exit(MHD_YES);
  end;

  (**
   * Function called whenever a request was completed.
   * Used to clean up 'struct UploadContext' objects.
   *
   * @param cls client-defined closure, NULL
   * @param connection connection handle
   * @param con_cls value as set by the last call to
   *        the MHD_AccessHandlerCallback, points to NULL if this was
   *            not an upload
   * @param toe reason for request termination
   *)
  procedure response_completed_callback(cls: Pointer; connection: PMHD_Connection;
    con_cls: PPointer; toe: MHD_RequestTerminationCode); cdecl;
  var
    uc: PUploadContext;
  begin
    uc := con_cls^;
    if nil = uc then
      Exit; (* this request wasn't an upload request *)
    if nil <> uc^.pp then
    begin
      MHD_destroy_post_processor(uc^.pp);
      uc^.pp := nil;
    end;
    if -1 <> uc^.fd then
    begin
      FpClose(uc^.fd);
      if nil <> uc^.filename then
      begin
        WriteLn(stderr, Format(
          'Upload of file `%s'' failed (incomplete or aborted), removing file.',
          [uc^.filename]));
        FpUnlink(uc^.filename);
      end;
    end;
    if nil <> uc^.filename then
      Free(uc^.filename);
    Free(uc);
  end;

  (**
   * Return the current directory listing.
   *
   * @param connection connection to return the directory for
   * @return MHD_YES on success, MHD_NO on error
   *)
  function return_directory_response(connection: PMHD_Connection): cint;
  var
    ret: cint;
  begin
    pthread_mutex_lock(@mutex);
    if nil = cached_directory_response then
      ret := MHD_queue_response(connection, MHD_HTTP_INTERNAL_SERVER_ERROR,
        internal_error_response)
    else
      ret := MHD_queue_response(connection, MHD_HTTP_OK,
        cached_directory_response);
    pthread_mutex_unlock(@mutex);
    Result := ret;
  end;

  (**
   * Main callback from MHD, used to generate the page.
   *
   * @param cls NULL
   * @param connection connection handle
   * @param url requested URL
   * @param method GET, PUT, POST, etc.
   * @param version HTTP version
   * @param upload_data data from upload (PUT/POST)
   * @param upload_data_size number of bytes in "upload_data"
   * @param ptr our context
   * @return MHD_YES on success, MHD_NO to drop connection
   *)
  function generate_page(cls: Pointer; connection: PMHD_Connection; url: Pcchar;
    method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  var
    response: PMHD_Response;
    ret: cint;
    fd: cint;
    buf: stat;
    (* should be file download *)
    file_data: array[0..MAGIC_HEADER_SIZE] of AnsiChar;
    got: ssize_t ;
    mime: Pcchar;
    uc: PUploadContext;
  begin
    if 0 <> strcomp(url, '/') then
    begin
      if 0 <> strcomp(method, MHD_HTTP_METHOD_GET) then
        Exit(MHD_NO);  (* unexpected method (we're not polite...) *)
      if (0 = FpStat(@url[1], buf)) and (nil = strstr(@url[1], '..')) and
        ('/' <> url[1]) then
        fd := FpOpen(@url[1], O_RDONLY)
      else
        fd := -1;
      if -1 = fd then
        Exit(MHD_queue_response(connection, MHD_HTTP_NOT_FOUND,
          file_not_found_response));
      (* read beginning of the file to determine mime type  *)
      got := FpRead(fd, file_data, SizeOf(file_data));
      if -1 <> got then
        mime := magic_buffer(magic, Pcchar(file_data), got)
      else
        mime := nil;
      lseek(fd, 0, SEEK_SET);
      response := MHD_create_response_from_fd(buf.st_size, fd);
      if nil = response then
      begin
        (* internal error (i.e. out of memory) *)
        FpClose(fd);
        Exit(MHD_NO);
      end;
      (* add mime type if we had one *)
      if nil <> mime then
        MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE, mime);
      ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
      MHD_destroy_response(response);
      Exit(ret);
    end;
    if 0 = strcomp(method, MHD_HTTP_METHOD_POST) then
    begin
      (* upload! *)
      uc := ptr^;
      if nil = uc then
      begin
        uc := Malloc(SizeOf(UploadContext));
        if nil = uc then
          Exit(MHD_NO); (* out of memory, close connection *)
        memset(uc, 0, SizeOf(UploadContext));
        uc^.fd := -1;
        uc^.connection := connection;
        uc^.pp := MHD_create_post_processor(connection, 64 * 1024 (* buffer size *),
                   @process_upload_data, uc);
        if nil = uc^.pp then
        begin
          (* out of memory, close connection *)
          Free(uc);
          Exit(MHD_NO);
        end;
        ptr^ := uc;
        Exit(MHD_YES);
      end;
      if 0 <> upload_data_size^ then
      begin
        if nil = uc^.response then
          MHD_post_process(uc^.pp, upload_data, upload_data_size^);
        upload_data_size^ := 0;
        Exit(MHD_YES);
      end;
      (* end of upload, finish it! *)
      MHD_destroy_post_processor(uc^.pp);
      uc^.pp := nil;
      if -1 <> uc^.fd then
      begin
        FpClose(uc^.fd);
        uc^.fd := -1;
      end;
      if nil <> uc^.response then
        Exit(MHD_queue_response(connection, MHD_HTTP_FORBIDDEN, uc^.response))
      else
      begin
        update_directory;
        Exit(return_directory_response(connection));
      end;
    end;
    if 0 = strcomp(method, MHD_HTTP_METHOD_GET) then
      Exit(return_directory_response(connection));
    (* unexpected request, refuse *)
    Result := MHD_queue_response(connection, MHD_HTTP_FORBIDDEN,
      request_refused_response);
  end;

  (**
   * Function called if we get a SIGPIPE. Does nothing.
   *
   * @param sig will be SIGPIPE (ignored)
   *)
  procedure catcher(signal: longint; info: psiginfo; context: psigcontext); cdecl;
  begin
    (* do nothing *)
  end;

  (**
   * setup handlers to ignore SIGPIPE.
   *)
  procedure ignore_sigpipe;
  var
    oldsig: sigactionrec;
    sig: sigactionrec;
  begin
    sig.sa_handler := @catcher;
    FpsigEmptySet(sig.sa_mask);
  {$IFDEF SA_INTERRUPT}
    sig.sa_flags := SA_INTERRUPT; (* SunOS *)
  {$ELSE}
    sig.sa_flags := SA_RESTART;
  {$ENDIF}
    if 0 <> FPSigaction(SIGPIPE, @sig, @oldsig) then
      WriteLn(stderr, Format('Failed to install SIGPIPE handler: %s',
        [strerror(errno^)]));
  end;

const
  (* test server key *)
  srv_signed_key_pem: array[0..1674] of AnsiChar =
    '-----BEGIN RSA PRIVATE KEY-----'#10+
    'MIIEowIBAAKCAQEAvfTdv+3fgvVTKRnP/HVNG81cr8TrUP/iiyuve/THMzvFXhCW'#10+
    '+K03KwEku55QvnUndwBfU/ROzLlv+5hotgiDRNFT3HxurmhouySBrJNJv7qWp8IL'#10+
    'q4sw32vo0fbMu5BZF49bUXK9L3kW2PdhTtSQPWHEzNrCxO+YgCilKHkY3vQNfdJ0'#10+
    '20Q5EAAEseD1YtWCIpRvJzYlZMpjYB1ubTl24kwrgOKUJYKqM4jmF4DVQp4oOK/6'#10+
    'QYGGh1QmHRPAy3CBII6sbb+sZT9cAqU6GYQVB35lm4XAgibXV6KgmpVxVQQ69U6x'#10+
    'yoOl204xuekZOaG9RUPId74Rtmwfi1TLbBzo2wIDAQABAoIBADu09WSICNq5cMe4'#10+
    '+NKCLlgAT1NiQpLls1gKRbDhKiHU9j8QWNvWWkJWrCya4QdUfLCfeddCMeiQmv3K'#10+
    'lJMvDs+5OjJSHFoOsGiuW2Ias7IjnIojaJalfBml6frhJ84G27IXmdz6gzOiTIer'#10+
    'DjeAgcwBaKH5WwIay2TxIaScl7AwHBauQkrLcyb4hTmZuQh6ArVIN6+pzoVuORXM'#10+
    'bpeNWl2l/HSN3VtUN6aCAKbN/X3o0GavCCMn5Fa85uJFsab4ss/uP+2PusU71+zP'#10+
    'sBm6p/2IbGvF5k3VPDA7X5YX61sukRjRBihY8xSnNYx1UcoOsX6AiPnbhifD8+xQ'#10+
    'Tlf8oJUCgYEA0BTfzqNpr9Wxw5/QXaSdw7S/0eP5a0C/nwURvmfSzuTD4equzbEN'#10+
    'd+dI/s2JMxrdj/I4uoAfUXRGaabevQIjFzC9uyE3LaOyR2zhuvAzX+vVcs6bSXeU'#10+
    'pKpCAcN+3Z3evMaX2f+z/nfSUAl2i4J2R+/LQAWJW4KwRky/m+cxpfUCgYEA6bN1'#10+
    'b73bMgM8wpNt6+fcmS+5n0iZihygQ2U2DEud8nZJL4Nrm1dwTnfZfJBnkGj6+0Q0'#10+
    'cOwj2KS0/wcEdJBP0jucU4v60VMhp75AQeHqidIde0bTViSRo3HWKXHBIFGYoU3T'#10+
    'LyPyKndbqsOObnsFXHn56Nwhr2HLf6nw4taGQY8CgYBoSW36FLCNbd6QGvLFXBGt'#10+
    '2lMhEM8az/K58kJ4WXSwOLtr6MD/WjNT2tkcy0puEJLm6BFCd6A6pLn9jaKou/92'#10+
    'SfltZjJPb3GUlp9zn5tAAeSSi7YMViBrfuFiHObij5LorefBXISLjuYbMwL03MgH'#10+
    'Ocl2JtA2ywMp2KFXs8GQWQKBgFyIVv5ogQrbZ0pvj31xr9HjqK6d01VxIi+tOmpB'#10+
    '4ocnOLEcaxX12BzprW55ytfOCVpF1jHD/imAhb3YrHXu0fwe6DXYXfZV4SSG2vB7'#10+
    'IB9z14KBN5qLHjNGFpMQXHSMek+b/ftTU0ZnPh9uEM5D3YqRLVd7GcdUhHvG8P8Q'#10+
    'C9aXAoGBAJtID6h8wOGMP0XYX5YYnhlC7dOLfk8UYrzlp3xhqVkzKthTQTj6wx9R'#10+
    'GtC4k7U1ki8oJsfcIlBNXd768fqDVWjYju5rzShMpo8OCTS6ipAblKjCxPPVhIpv'#10+
    'tWPlbSn1qj6wylstJ5/3Z+ZW5H4wIKp5jmLiioDhcP0L/Ex3Zx8O'#10+
    '-----END RSA PRIVATE KEY-----'#10;

  (* test server CA signed certificates *)
  srv_signed_cert_pem: array[0..1138] of AnsiChar =
    '-----BEGIN CERTIFICATE-----'#10+
    'MIIDGzCCAgWgAwIBAgIES0KCvTALBgkqhkiG9w0BAQUwFzEVMBMGA1UEAxMMdGVz'#10+
    'dF9jYV9jZXJ0MB4XDTEwMDEwNTAwMDcyNVoXDTQ1MDMxMjAwMDcyNVowFzEVMBMG'#10+
    'A1UEAxMMdGVzdF9jYV9jZXJ0MIIBHzALBgkqhkiG9w0BAQEDggEOADCCAQkCggEA'#10+
    'vfTdv+3fgvVTKRnP/HVNG81cr8TrUP/iiyuve/THMzvFXhCW+K03KwEku55QvnUn'#10+
    'dwBfU/ROzLlv+5hotgiDRNFT3HxurmhouySBrJNJv7qWp8ILq4sw32vo0fbMu5BZ'#10+
    'F49bUXK9L3kW2PdhTtSQPWHEzNrCxO+YgCilKHkY3vQNfdJ020Q5EAAEseD1YtWC'#10+
    'IpRvJzYlZMpjYB1ubTl24kwrgOKUJYKqM4jmF4DVQp4oOK/6QYGGh1QmHRPAy3CB'#10+
    'II6sbb+sZT9cAqU6GYQVB35lm4XAgibXV6KgmpVxVQQ69U6xyoOl204xuekZOaG9'#10+
    'RUPId74Rtmwfi1TLbBzo2wIDAQABo3YwdDAMBgNVHRMBAf8EAjAAMBMGA1UdJQQM'#10+
    'MAoGCCsGAQUFBwMBMA8GA1UdDwEB/wQFAwMHIAAwHQYDVR0OBBYEFOFi4ilKOP1d'#10+
    'XHlWCMwmVKr7mgy8MB8GA1UdIwQYMBaAFP2olB4s2T/xuoQ5pT2RKojFwZo2MAsG'#10+
    'CSqGSIb3DQEBBQOCAQEAHVWPxazupbOkG7Did+dY9z2z6RjTzYvurTtEKQgzM2Vz'#10+
    'GQBA+3pZ3c5mS97fPIs9hZXfnQeelMeZ2XP1a+9vp35bJjZBBhVH+pqxjCgiUflg'#10+
    'A3Zqy0XwwVCgQLE2HyaU3DLUD/aeIFK5gJaOSdNTXZLv43K8kl4cqDbMeRpVTbkt'#10+
    'YmG4AyEOYRNKGTqMEJXJoxD5E3rBUNrVI/XyTjYrulxbNPcMWEHKNeeqWpKDYTFo'#10+
    'Bb01PCthGXiq/4A2RLAFosadzRa8SBpoSjPPfZ0b2w4MJpReHqKbR5+T2t6hzml6'#10+
    '4ToyOKPDmamiTuN5KzLN3cw7DQlvWMvqSOChPLnA3Q=='#10+
    '-----END CERTIFICATE-----'#10;

  (**
   * Entry point to demo.  Note: this HTTP server will make all
   * files in the current directory and its subdirectories available
   * to anyone.  Press ENTER to stop the server once it has started.
   *
   * @param argc number of arguments in argv
   * @param argv first and only argument should be the port number
   * @return 0 on success
   *)
var
  d: PMHD_Daemon;
  port: cuint;
begin
  if (argc <> 2) or (1 <> sscanf(argv[1], '%u', @port)) or
    (UINT16_MAX < port) then
  begin
    WriteLn(stderr, argv[0], ' PORT');
    Halt(1);
  end;
  ignore_sigpipe;
  magic := magic_open(MAGIC_MIME_TYPE);
  magic_load(magic, nil);
  pthread_mutex_init(@mutex, nil);
  file_not_found_response := MHD_create_response_from_buffer(
    strlen(FILE_NOT_FOUND_PAGE), FILE_NOT_FOUND_PAGE,
    MHD_RESPMEM_PERSISTENT);
  mark_as_html(file_not_found_response);
  request_refused_response := MHD_create_response_from_buffer(
    strlen(REQUEST_REFUSED_PAGE), REQUEST_REFUSED_PAGE,
    MHD_RESPMEM_PERSISTENT);
  mark_as_html(request_refused_response);
  internal_error_response := MHD_create_response_from_buffer(
    strlen(INTERNAL_ERROR_PAGE), INTERNAL_ERROR_PAGE,
    MHD_RESPMEM_PERSISTENT);
  mark_as_html(internal_error_response);
  update_directory;
  d := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY or MHD_USE_DEBUG or MHD_USE_SSL
{$IFDEF EPOLL_SUPPORT}
         or MHD_USE_EPOLL_LINUX_ONLY
{$ENDIF},
         port, nil, nil, @generate_page, nil,
         MHD_OPTION_CONNECTION_MEMORY_LIMIT, size_t(256 * 1024),
{$IFDEF PRODUCTION}
         MHD_OPTION_PER_IP_CONNECTION_LIMIT, cuint(64),
{$ENDIF}
         MHD_OPTION_CONNECTION_TIMEOUT, cuint(120 (* seconds *)),
         MHD_OPTION_THREAD_POOL_SIZE, cuint(NUMBER_OF_THREADS),
         MHD_OPTION_NOTIFY_COMPLETED, @response_completed_callback, nil,
         MHD_OPTION_HTTPS_MEM_KEY, srv_signed_key_pem,
         MHD_OPTION_HTTPS_MEM_CERT, srv_signed_cert_pem,
         MHD_OPTION_END);
  if nil = d then
    Halt(1);
  WriteLn(stderr, 'HTTP server running. Press ENTER to stop the server');
  ReadLn;
  MHD_stop_daemon(d);
  MHD_destroy_response(file_not_found_response);
  MHD_destroy_response(request_refused_response);
  MHD_destroy_response(internal_error_response);
  update_cached_response(nil);
  pthread_mutex_destroy(@mutex);
  magic_close(magic);
end.

