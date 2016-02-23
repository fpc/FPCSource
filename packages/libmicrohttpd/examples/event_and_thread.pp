(*

  @Example: `event_and_thread`.
  @Description: Use event-driven for usual requests and threads to slowly requests.
  @Authors: Silvio Clecio and Gilson Nunes

*)

program event_and_thread;

// Shows `event_and_thread` details on Linux:
//
//   $ ps axo pid,ppid,rss,vsz,nlwp,cmd | grep 'event_and_thread'
//
// But if you prefer to see only the number of thread of `event_and_thread`:
//
//   $ ps axo nlwp,cmd | grep 'event_and_thread'

{$mode objfpc}{$H+}
{$MACRO ON}
{$DEFINE DEBUG}
{.$DEFINE WAIT_CLIENTS_DISCONNECT}
{$DEFINE TIMEOUT := 10}
{.$DEFINE CONTINGENCY_CONTROL}
{$IF DEFINED(CONTINGENCY_CONTROL)}
  {$DEFINE MAX_THREAD_COUNT := 2}
{$ENDIF}

uses
{$IFDEF UNIX}
  cthreads, BaseUnix,
{$ELSE}
  Sockets,
{$ENDIF}
  Classes, SysUtils, cutils, libmicrohttpd;

  procedure MHD_socket_close(fd: cint);
  begin
{$IFDEF UNIX}
    FpClose(fd);
{$ELSE}
    CloseSocket(fd);
{$ENDIF}
  end;

const
  PORT = 8888;

var
  _threads: TFPList;
  _mutex: TRTLCriticalSection;

type

  { TConnectionHandler }

  TConnectionHandler = packed record
    Connection: PMHD_Connection;
    Url: Pcchar;
  end;

  { TSlothThread }

  TSlothThread = class(TThread)
  private
    FHandler: TConnectionHandler;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandler: TConnectionHandler);
    destructor Destroy; override;
  end;

  { TSlothThread }

  constructor TSlothThread.Create(AHandler: TConnectionHandler);
  begin
    inherited Create(True);
    FreeOnTerminate := True;
    FHandler := AHandler;
  end;

  destructor TSlothThread.Destroy;
  begin
    _threads.Remove(Self);
    inherited Destroy;
  end;

  procedure TSlothThread.Execute;
  const
    page: AnsiString =
      '<html><body>I''m a sloth, and my URL is "%s". T: %s</body></html>';
  var
    i: Byte;
    s: AnsiString;
    response: PMHD_Response;
  begin
    for i := 1 to TIMEOUT do
    begin
      if Terminated then
        Break;
      Sleep(1000);
    end;
    if not Terminated then
    begin
      s := Format(page, [FHandler.Url, DateTimeToStr(Now)]);
      response := MHD_create_response_from_buffer(Length(s), Pointer(s),
        MHD_RESPMEM_MUST_COPY);
      MHD_queue_response(FHandler.Connection, MHD_HTTP_OK, response);
      MHD_resume_connection(FHandler.Connection);
      MHD_destroy_response(response);
    end;
  end;

  { daemon }

  function RequestHandler(cls: Pointer; connection: PMHD_Connection;
    url: Pcchar; method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  const
    page = '<html><body>Hello world! T: %s</body></html>';
{$IF DEFINED(CONTINGENCY_CONTROL)}
    busy_page: Pcchar = '<html><body>The server is busy. :-(</body></html>';
{$ENDIF}
  var
    s: string;
    ret: cint;
    thr: TThread;
    response: PMHD_Response;
    handler: TConnectionHandler;
  begin
    if method <> 'GET' then
      Exit(MHD_NO);

    { By Gilson Nunes:
      "The connection state for first call is `MHD_CONNECTION_HEADERS_PROCESSED`
       and `MHD_CONNECTION_FOOTERS_RECEIVED` for the next, so the flag below
       ensures that the response will be delivered to the client after `MHD`
       finish all the request processing." }
    if not Assigned(ptr^) then
    begin
      ptr^ := Pointer(1);
      Exit(MHD_YES);
    end;
    ptr^ := nil;

    if (strcomp(url, '/sloth1') = 0) or (strcomp(url, '/sloth2') = 0) then
    begin
{$IF DEFINED(CONTINGENCY_CONTROL)}
      if _threads.Count = MAX_THREAD_COUNT then
      begin
        response := MHD_create_response_from_buffer(Length(busy_page),
          busy_page, MHD_RESPMEM_PERSISTENT);
        ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
        MHD_destroy_response(response);
        Exit(ret);
      end;
{$ENDIF}
      MHD_suspend_connection(connection);
      handler.Connection := connection;
      handler.Url := url;
      thr := TSlothThread.Create(handler);
      EnterCriticalsection(_mutex);
      try
        _threads.Add(thr);
      finally
        LeaveCriticalsection(_mutex);
      end;
      thr.Start;
      Result := MHD_YES;
    end
    else
    begin
      s := Format(page, [DateTimeToStr(Now)]);
      response := MHD_create_response_from_buffer(Length(s), Pointer(s),
        MHD_RESPMEM_MUST_COPY);
      ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
      MHD_destroy_response(response);
      Result := ret;
    end;
  end;

var
  _daemon: PMHD_Daemon;

  procedure StopServer;
  var
    i: Integer;
    thr: TThread;
    sckt: MHD_socket;
    connections: PMHD_DaemonInfo;
  begin
    sckt := MHD_quiesce_daemon(_daemon);
{$IFDEF MSWINDOWS}
    if LongWord(sckt) <> MHD_INVALID_SOCKET then
{$ELSE}
    if sckt <> MHD_INVALID_SOCKET then
{$ENDIF}
      MHD_socket_close(sckt);
    EnterCriticalsection(_mutex);
    try
      WriteLn('Threads: ', _threads.Count);
      for i := Pred(_threads.Count) downto 0 do
      begin
        thr := TThread(_threads[i]);
        WriteLn('Finishing thread $', HexStr(thr), ' ...');
        if Assigned(thr) then
          thr.Terminate;
      end;
      while _threads.Count > 0 do
        Sleep(500);
    finally
      LeaveCriticalsection(_mutex);
    end;
    connections := MHD_get_daemon_info(_daemon, MHD_DAEMON_INFO_CURRENT_CONNECTIONS);
    if Assigned(connections) then
    begin
      WriteLn('Connections: ', connections^.num_connections);
{$IFDEF WAIT_CLIENTS_DISCONNECT}
      while True do
      begin
        if connections^.num_connections = 0 then
          Break;
        Sleep(500);
      end;
{$ENDIF}
    end;
    MHD_stop_daemon(_daemon);
    WriteLn('Bye!');
  end;

  procedure SigProc(sig: cint); cdecl;
  begin
    WriteLn;
    StopServer;
    FreeAndNil(_threads);
    Halt;
  end;

begin
  InitCriticalSection(_mutex);
  _threads := TFPList.Create;
  try
    _daemon := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY or
      MHD_USE_SUSPEND_RESUME or MHD_USE_DEBUG,
      PORT, nil, nil, @RequestHandler, nil,
{$IF DEFINED(CONTINGENCY_CONTROL)}
      MHD_OPTION_THREAD_POOL_SIZE, cuint(MAX_THREAD_COUNT),
{$ENDIF}
      MHD_OPTION_CONNECTION_TIMEOUT, cuint(TIMEOUT + 1),
      MHD_OPTION_END);
    if not Assigned(_daemon) then
      Halt(1);
    signal(SIGINT, @SigProc);
{$IFDEF MSWINDOWS}
    signal(SIGBREAK, @SigProc);
{$ELSE}
    signal(SIGTERM, @SigProc);
{$ENDIF}
    WriteLn('HTTP server running. Press [Ctrl+C] to stop the server ...');
    while Assigned(_daemon) do
      Sleep(100);
  finally
    FreeAndNil(_threads);
    DoneCriticalsection(_mutex);
  end;
end.
