(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/basicauthentication.c

program basicauthentication;

{$mode objfpc}{$H+}

uses
  libmicrohttpd, SysUtils;

const
  PORT = 8888;

  function AnswerToConnection(ACls: Pointer; AConnection: PMHD_Connection;
    AUrl: Pcchar; AMethod: Pcchar; AVersion: Pcchar; AUploadData: Pcchar;
    AUploadDataSize: Psize_t; AConCls: PPointer): cint; cdecl;
  var
    VPage: Pcchar;
    VUser: Pcchar;
    VPass: Pcchar;
    VReturn: cint;
    VFail: Boolean;
    VResponse: PMHD_Response;
  begin
    if StrComp(AMethod, 'GET') <> 0 then
      Exit(MHD_NO);
    if not Assigned(AConCls^) then
    begin
      AConCls^ := AConnection;
      Exit(MHD_YES);
    end;
    VPass := nil;
    VUser := MHD_basic_auth_get_username_password(AConnection, @VPass);
    VFail := (VUser = nil) or (StrComp(VUser, 'root') <> 0) or
      (StrComp(VPass, 'pa$$w0rd') <> 0);
    if VUser <> nil then
      VUser := nil;
    if VPass <> nil then
      VPass := nil;
    if VFail then
    begin
      VPage := '<html><body>Go away.</body></html>';
      VResponse := MHD_create_response_from_buffer(Length(VPage),
        Pointer(VPage), MHD_RESPMEM_PERSISTENT);
      VReturn := MHD_queue_basic_auth_fail_response(AConnection,
        'my realm', VResponse);
    end
    else
    begin
      VPage := '<html><body>A secret.</body></html>';
      VResponse := MHD_create_response_from_buffer(Length(VPage),
        Pointer(VPage), MHD_RESPMEM_PERSISTENT);
      VReturn := MHD_queue_response(AConnection, MHD_HTTP_OK, VResponse);
    end;
    MHD_destroy_response(VResponse);
    Result := VReturn;
  end;

var
  VDaemon: PMHD_Daemon;
begin
  VDaemon := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY, PORT, nil,
    nil, @AnswerToConnection, nil, MHD_OPTION_END);
  if not Assigned(VDaemon) then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(VDaemon);
end.
