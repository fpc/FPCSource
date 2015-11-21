(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/responseheaders.c

program responseheaders;

{$mode objfpc}{$H+}

uses
  BaseUnix, SysUtils, libmicrohttpd;

const
  PORT = 8888;
  FILENAME = 'picture.png';
  MIMETYPE = 'image/png';

  function AnswerToConnection(ACls: Pointer; AConnection: PMHD_Connection;
    AUrl: Pcchar; AMethod: Pcchar; AVersion: Pcchar; AUploadData: Pcchar;
    AUploadDataSize: Psize_t; AConCls: PPointer): cint; cdecl;
  const
    errorstr: Pcchar = '<html><body>An internal server error has occured!</body></html>';
  var
    VFd: cint;
    VReturn: cint;
    VResponse: PMHD_Response;
    VSbuf: TStat;
  begin
    if StrComp(AMethod, 'GET') <> 0 then
      Exit(MHD_NO);
    VFd := FpOpen(FILENAME, O_RDONLY);
    VSbuf := Default(TStat);
    if (VFd = -1) or (FpFStat(VFd, VSbuf) <> 0) then
    begin
      (* error accessing file *)
      if VFd <> -1 then
        FpClose(VFd);
      VResponse := MHD_create_response_from_buffer(Length(errorstr),
        Pointer(errorstr), MHD_RESPMEM_PERSISTENT);
      if Assigned(VResponse) then
      begin
        VReturn := MHD_queue_response(AConnection,
          MHD_HTTP_INTERNAL_SERVER_ERROR, VResponse);
        MHD_destroy_response(VResponse);
        Exit(VReturn);
      end
      else
        Exit(MHD_NO);
    end;
    VResponse := MHD_create_response_from_fd_at_offset64(VSbuf.st_size, VFd, 0);
    MHD_add_response_header(VResponse, 'Content-Type', MIMETYPE);
    VReturn := MHD_queue_response(AConnection, MHD_HTTP_OK, VResponse);
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
