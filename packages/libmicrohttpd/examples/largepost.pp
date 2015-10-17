(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/largepost.c

program largepost;

{$mode objfpc}{$H+}

uses
  libmicrohttpd, SysUtils, cutils;

type
  TConnectionInfoStruct = record
    ConnectionType: cint;
    PostProcessor: PMHD_PostProcessor;
    Fp: FILEptr;
    AnswerString: Pcchar;
    AnswerCode: cint;
  end;
  PConnectionInfoStruct = ^TConnectionInfoStruct;

const
  PORT = 8888;
  POSTBUFFERSIZE = 512;
  MAXCLIENTS = 2;
  GET = 0;
  POST = 1;

var
  NrOfUploadingClients: Cardinal;
  AskPage: Pcchar =
    '<html><body>'+#10+
    'Upload a file, please!<br>'+#10+
    'There are %d clients uploading at the moment.<br>'+#10+
    '<form action="/filepost" method="post" enctype="multipart/form-data">'+#10+
    '<input name="file" type="file">'+#10+
    '<input type="submit" value="Send"></form>'+#10+
    '</body></html>';
  BusyPage: Pcchar = '<html><body>This server is busy, please try again later.</body></html>';
  CompletePage: Pcchar = '<html><body>The upload has been completed.</body></html>';
  ErrorPage: Pcchar = '<html><body>This doesn''t seem to be right.</body></html>';
  ServerErrorPage: Pcchar = '<html><body>An internal server error has occured.</body></html>';
  FileExistsPage: Pcchar = '<html><body>This file already exists.</body></html>';

  function SendPage(AConnection: PMHD_Connection; APage: Pcchar; AStatusCode: cint): cint;
  var
    VRet: cint;
    VResponse: PMHD_Response;
  begin
    VResponse := MHD_create_response_from_buffer(Length(APage),
      Pointer(APage), MHD_RESPMEM_MUST_COPY);
    if not Assigned(VResponse) then
      Exit(MHD_NO);
    MHD_add_response_header(VResponse, MHD_HTTP_HEADER_CONTENT_TYPE, 'text/html');
    VRet := MHD_queue_response(AConnection, AStatusCode, VResponse);
    MHD_destroy_response(VResponse);
    Result := VRet;
  end;

  function IteratePost(AConInfoCls: Pointer; AKind: MHD_ValueKind; AKey: Pcchar;
    AFileName: Pcchar; AContentType: Pcchar; ATransferEncoding: Pcchar;
    AData: Pcchar; AOff: cuint64; ASize: size_t): cint; cdecl;
  var
    VConInfo: PConnectionInfoStruct;
  begin
    VConInfo := AConInfoCls;
    VConInfo^.AnswerString := ServerErrorPage;
    VConInfo^.AnswerCode := MHD_HTTP_INTERNAL_SERVER_ERROR;
    if StrComp(AKey, 'file') <> 0 then
      Exit(MHD_NO);
    if not Assigned(VConInfo^.Fp) then
    begin
      if FileExists(AFileName) then
      begin
        VConInfo^.AnswerString := FileExistsPage;
        VConInfo^.AnswerCode := MHD_HTTP_FORBIDDEN;
        Exit(MHD_NO);
      end;
      VConInfo^.Fp := fopen(AFileName, fappendwrite);
      if not Assigned(VConInfo^.Fp) then
        Exit(MHD_NO);
    end;
    if ASize > 0 then
      if fwrite(AData, ASize, SizeOf(AnsiChar), VConInfo^.Fp) = 0 then
        Exit(MHD_NO);
    VConInfo^.AnswerString := CompletePage;
    VConInfo^.AnswerCode := MHD_HTTP_OK;
    Result := MHD_YES;
  end;

  procedure RequestCompleted(ACls: Pointer; AConnection: PMHD_Connection;
    AConCls: PPointer; AToe: MHD_RequestTerminationCode); cdecl;
  var
    VConInfo: PConnectionInfoStruct;
  begin
    VConInfo := AConCls^;
    if not Assigned(VConInfo) then
      Exit;
    if VConInfo^.ConnectionType = POST then
    begin
      if Assigned(VConInfo^.PostProcessor) then
      begin
        MHD_destroy_post_processor(VConInfo^.PostProcessor);
        Dec(NrOfUploadingClients);
      end;
      if Assigned(VConInfo^.Fp) then
        fclose(VConInfo^.Fp);
    end;
    FreeMem(VConInfo);
    AConCls^ := nil;
  end;

  function AnswerToConnection(ACls: Pointer; AConnection: PMHD_Connection;
    AUrl: Pcchar; AMethod: Pcchar; AVersion: Pcchar; AUploadData: Pcchar;
    AUploadDataSize: Psize_t; AConCls: PPointer): cint; cdecl;
  var
    VBuffer: array[0..1024] of AnsiChar;
    VConInfo: PConnectionInfoStruct;
  begin
    if not Assigned(AConCls^) then
    begin
      if NrOfUploadingClients >= MAXCLIENTS then
        Exit(SendPage(AConnection, BusyPage, MHD_HTTP_SERVICE_UNAVAILABLE));
      VConInfo := AllocMem(SizeOf(TConnectionInfoStruct));
      if not Assigned(VConInfo) then
        Exit(MHD_NO);
      VConInfo^.Fp := nil;
      if StrComp(AMethod, 'POST') = 0 then
      begin
        VConInfo^.PostProcessor := MHD_create_post_processor(AConnection,
          POSTBUFFERSIZE, @IteratePost, VConInfo);
        if not Assigned(VConInfo^.PostProcessor) then
        begin
          FreeMem(VConInfo);
          Exit(MHD_NO);
        end;
        Inc(NrOfUploadingClients);
        VConInfo^.ConnectionType := POST;
        VConInfo^.AnswerCode := MHD_HTTP_OK;
        VConInfo^.AnswerString := CompletePage;
      end
      else
        VConInfo^.ConnectionType := GET;
      AConCls^ := VConInfo;
      Exit(MHD_YES);
    end;
    if StrComp(AMethod, 'GET') = 0 then
    begin
      StrLFmt(VBuffer, SizeOf(VBuffer), AskPage, [NrOfUploadingClients]);
      Exit(SendPage(AConnection, VBuffer, MHD_HTTP_OK));
    end;
    if StrComp(AMethod, 'POST') = 0 then
    begin
      VConInfo := AConCls^;
      if AUploadDataSize^ <> 0 then
      begin
        MHD_post_process(VConInfo^.PostProcessor, AUploadData, AUploadDataSize^);
        AUploadDataSize^ := 0;
        Exit(MHD_YES);
      end
      else
      begin
        if Assigned(VConInfo^.Fp) then
        begin
          fclose(VConInfo^.Fp);
          VConInfo^.Fp := nil;
        end;
        (* Now it is safe to open and inspect the file before calling send_page with a response *)
        Exit(SendPage(AConnection, VConInfo^.AnswerString, VConInfo^.AnswerCode));
      end;
    end;
    Result := SendPage(AConnection, ErrorPage, MHD_HTTP_BAD_REQUEST);
  end;

var
  VDaemon: PMHD_Daemon;
begin
  VDaemon := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY, PORT, nil, nil,
    @AnswerToConnection, nil, MHD_OPTION_NOTIFY_COMPLETED, @RequestCompleted,
      nil, MHD_OPTION_END);
  if not Assigned(VDaemon) then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(VDaemon);
end.

