(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/hellobrowser.c

program hellobrowser;

{$mode objfpc}{$H+}

uses
  libmicrohttpd;

const
  PORT = 8888;

  function AnswerToConnection(ACls: Pointer; AConnection: PMHD_Connection;
    AUrl: Pcchar; AMethod: Pcchar; AVersion: Pcchar; AUploadData: Pcchar;
    AUploadDataSize: Psize_t; AConCls: PPointer): cint; cdecl;
  const
    PAGE: Pcchar = 'Hello world';
  var
    VReturn: cint;
    VResponse: PMHD_Response;
  begin
    VResponse := MHD_create_response_from_buffer(Length(PAGE), Pointer(PAGE),
      MHD_RESPMEM_PERSISTENT);
    VReturn := MHD_queue_response(AConnection, MHD_HTTP_OK, VResponse);
    MHD_destroy_response(VResponse);
    Result := VReturn;
  end;

var
  VDaemon: PMHD_Daemon;
begin
  VDaemon := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY, PORT, nil, nil,
    @AnswerToConnection, nil, MHD_OPTION_END);
  if not Assigned(VDaemon) then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(VDaemon)
end.

