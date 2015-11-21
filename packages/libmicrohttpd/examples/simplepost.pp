(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/simplepost.c

program simplepost;

{$mode objfpc}{$H+}

uses
  SysUtils, cmem, cutils, libmicrohttpd;

const
  PORT = 8888;
  POSTBUFFERSIZE = 512;
  MAXNAMESIZE = 20;
  MAXANSWERSIZE = 512;
  GET = 0;
  POST = 1;

  askpage: Pcchar =
    '<html><body>'+
    'What''s your name, Sir?<br>'+
    '<form action="/namepost" method="post">'+
    '<input name="name" type="text">'+
    '<input type="submit" value=" Send "></form>'+
    '</body></html>';

  greetingpage: Pcchar = '<html><body><h1>Welcome, %s!</center></h1></body></html>';

  errorpage: Pcchar = '<html><body>This doesn''t seem to be right.</body></html>';

type
  Tconnection_info_struct = packed record
    connectiontype: cint;
    answerstring: Pcchar;
    postprocessor: PMHD_PostProcessor;
  end;
  Pconnection_info_struct = ^Tconnection_info_struct;

  function send_page(connection: PMHD_Connection; page: Pcchar): cint; cdecl;
  var
    ret: cint;
    response: PMHD_Response;
  begin
    response := MHD_create_response_from_buffer(Length(page),
      Pointer(page), MHD_RESPMEM_PERSISTENT);
    if not Assigned(response) then
      Exit(MHD_NO);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  function iterate_post(coninfo_cls: Pointer; kind: MHD_ValueKind;
    key: Pcchar; filename: Pcchar; content_type: Pcchar; transfer_encoding: Pcchar;
    Data: Pcchar; off: cuint64; size: size_t): cint; cdecl;
  var
    con_info: Pconnection_info_struct;
    answerstring: Pcchar;
  begin
    con_info := coninfo_cls;
    if 0 = strcomp(key, 'name') then
    begin
      if (size > 0) and (size <= MAXNAMESIZE) then
      begin
        answerstring := Malloc(MAXANSWERSIZE);
        if not Assigned(answerstring) then
          Exit(MHD_NO);
        snprintf(answerstring, MAXANSWERSIZE, greetingpage, Data);
        con_info^.answerstring := answerstring;
      end
      else
        con_info^.answerstring := nil;
      Exit(MHD_NO);
    end;
    Result := MHD_YES;
  end;

  procedure request_completed(cls: Pointer; connection: PMHD_Connection;
    con_cls: PPointer; toe: MHD_RequestTerminationCode); cdecl;
  var
    con_info: Pconnection_info_struct;
  begin
    con_info := con_cls^;
    if nil = con_info then
      Exit;
    if con_info^.connectiontype = POST then
    begin
      MHD_destroy_post_processor(con_info^.postprocessor);
      if Assigned(con_info^.answerstring) then
        Free(con_info^.answerstring);
    end;
    Free(con_info);
    con_cls^ := nil;
  end;

  function answer_to_connection(cls: Pointer; connection: PMHD_Connection;
    url: Pcchar; method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; con_cls: PPointer): cint; cdecl;
  var
    con_info: Pconnection_info_struct;
  begin
    if nil = con_cls^ then
    begin
      con_info := Malloc(SizeOf(Tconnection_info_struct));
      if nil = con_info then
        Exit(MHD_NO);
      con_info^.answerstring := nil;
      if 0 = strcomp(method, 'POST') then
      begin
        con_info^.postprocessor :=
          MHD_create_post_processor(connection, POSTBUFFERSIZE,
          @iterate_post, Pointer(con_info));
        if nil = con_info^.postprocessor then
        begin
          Free(con_info);
          Exit(MHD_NO);
        end;
        con_info^.connectiontype := POST;
      end
      else
        con_info^.connectiontype := GET;
      con_cls^ := Pointer(con_info);
      Exit(MHD_YES);
    end;
    if 0 = strcomp(method, 'GET') then
      Exit(send_page(connection, askpage));
    if 0 = strcomp(method, 'POST') then
    begin
      con_info := con_cls^;
      if upload_data_size^ <> 0 then
      begin
        MHD_post_process(con_info^.postprocessor, upload_data, upload_data_size^);
        upload_data_size^ := 0;
        Exit(MHD_YES);
      end
      else
      if nil <> con_info^.answerstring then
        Exit(send_page(connection, con_info^.answerstring));
    end;
    Result := send_page(connection, errorpage);
  end;

var
  daemon: PMHD_Daemon;
begin
  daemon := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY, PORT, nil, nil,
    @answer_to_connection, nil, MHD_OPTION_NOTIFY_COMPLETED, @request_completed,
    nil, MHD_OPTION_END);
  if nil = daemon then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(daemon);
end.
