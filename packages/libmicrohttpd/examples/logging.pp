(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/logging.c

program logging;

{$mode objfpc}{$H+}

uses
  libmicrohttpd, sysutils;

const
  PORT = 8888;

  function PrintOutKey(ACls: Pointer; AKind: MHD_ValueKind; AKey: Pcchar;
    AValue: Pcchar): cint; cdecl;
  begin
    WriteLn(Format('%s: %s', [AKey, AValue]));
    Result := MHD_YES;
  end;

  function AnswerToConnection(ACls: Pointer; AConnection: PMHD_Connection;
    AUrl: Pcchar; AMethod: Pcchar; AVersion: Pcchar; AUploadData: Pcchar;
    AUploadDataSize: Psize_t; AConCls: PPointer): cint; cdecl;
  begin
    WriteLn(Format('New %s request for %s using version %s',
      [AMethod, AUrl, AVersion]));
    MHD_get_connection_values(AConnection, MHD_HEADER_KIND, @PrintOutKey, nil);
    Result := MHD_NO;
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

