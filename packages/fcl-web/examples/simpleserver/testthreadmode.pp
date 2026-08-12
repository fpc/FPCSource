program TestThreadMode;

{
  Demonstrate the difference between simpleserver's connection-handling modes:
  
  -t none|polled|thread|threadpool

  A browser opens several TCP connections and often sends nothing on some of them
  (speculative "preconnect" sockets). Such a silent connection must not delay a real
  request on another connection.

  Scenario 1  - one silent connection A (never sends) + one real request B.
    In "-t none" the single thread blocks reading A, so B is not served until A hits
    RequestReadTimeout (~30 s). In the other modes B is answered in milliseconds.

  Scenario 2 - a flood of N silent connections + one real request B.
    none       - cannot even accept the flood (listen backlog fills); B blocked.
    polled     - parks all N idle sockets on ONE thread; B stays flat.
    thread     - serves B fast but spawns one thread per idle socket (unbounded).
    threadpool - if N >= pool workers, idle sockets exhaust the pool and B blocks.

  We measure how long B waits for its response while the silent connection(s) are
  held open, capped at B_READ_TIMEOUT so a "blocked" case is observed quickly.

  Unix only (uses BaseUnix). Build: fpc testthreadmode.pp
  Usage: ./testthreadmode [path-to-simpleserver] [base-port] [flood-count]
}

{$mode objfpc}{$H+}

uses
  BaseUnix, Unix, Sockets, SysUtils, Process;

const
  Modes: array[0..3] of string = ('none', 'polled', 'thread', 'threadpool');
  B_READ_TIMEOUT = 6000;   // ms cap for B's connect + response
  A_SETTLE       = 300;    // ms to let the server accept the silent socket(s)
  FLOOD_COUNT    = 12;     // silent connections in scenario 2
  DEFAULT_BIN    = '/home/tixeo/FPC/FPC/src/packages/fcl-web/examples/simpleserver/simpleserver';
  GETREQ         = 'GET / HTTP/1.0'#13#10'Host: test'#13#10'Connection: close'#13#10#13#10;

// Microsecond monotonic-ish timestamp for latency measurement.
function NowMicro: Int64;

var
  tv: TTimeVal;

begin
  fpgettimeofday(@tv, nil);
  Result := Int64(tv.tv_sec) * 1000000 + tv.tv_usec;
end;


// Wait until aFD is readable (or writable) within aTimeoutMs. True if ready.
function WaitReady(aFD: cint; aForWrite: Boolean; aTimeoutMs: Integer): Boolean;

var
  fds: TFDSet;
  tv: TTimeVal;
  r: cint;

begin
  fpFD_ZERO(fds);
  fpFD_SET(aFD, fds);
  tv.tv_sec := aTimeoutMs div 1000;
  tv.tv_usec := (aTimeoutMs mod 1000) * 1000;
  if aForWrite then
    r := fpSelect(aFD + 1, nil, @fds, nil, @tv)
  else
    r := fpSelect(aFD + 1, @fds, nil, nil, @tv);
  Result := (r > 0) and (fpFD_ISSET(aFD, fds) <> 0);
end;


// True if a non-blocking connect on aFD completed without error.
function ConnectedOK(aFD: cint): Boolean;

var
  err: cint;
  l: TSocklen;

begin
  err := 0;
  l := SizeOf(err);
  if fpgetsockopt(aFD, SOL_SOCKET, SO_ERROR, @err, @l) <> 0 then
    Exit(False);
  Result := (err = 0);
end;


// Create a non-blocking socket and start a connect to 127.0.0.1:aPort.
// Returns the fd (connect may still be in progress) or -1 on failure.
function StartConnect(aPort: Word): cint;

var
  lFD, lFlags: cint;
  lAddr: TInetSockAddr;

begin
  Result := -1;
  lFD := fpSocket(AF_INET, SOCK_STREAM, 0);
  if lFD < 0 then
    Exit;
  lFlags := fpfcntl(lFD, F_GETFL, 0);
  fpfcntl(lFD, F_SETFL, lFlags or O_NONBLOCK);
  FillChar(lAddr, SizeOf(lAddr), 0);
  lAddr.sin_family := AF_INET;
  lAddr.sin_port := htons(aPort);
  lAddr.sin_addr.s_addr := htonl($7F000001);   // 127.0.0.1
  fpConnect(lFD, psockaddr(@lAddr), SizeOf(lAddr));  // non-blocking: -1/EINPROGRESS
  Result := lFD;
end;


// Open connection B, send a real request, time the response.
// Returns True if served; aLatencyMs/aStatus are set on success.
function ProbeRealRequest(aPort: Word; out aLatencyMs: Double; out aStatus: string): Boolean;

var
  lFD: cint;
  t0: Int64;
  n: PtrInt;
  buf: array[0..4095] of Byte;
  s: string;
  p: Integer;

begin
  Result := False;
  aLatencyMs := B_READ_TIMEOUT;
  aStatus := '';
  lFD := StartConnect(aPort);
  if lFD < 0 then
    Exit;
  try
    if not WaitReady(lFD, True, B_READ_TIMEOUT) then
      Exit;                                   // connect stalled -> blocked
    if not ConnectedOK(lFD) then
      Exit;
    t0 := NowMicro;
    fpSend(lFD, @GETREQ[1], Length(GETREQ), 0);
    if not WaitReady(lFD, False, B_READ_TIMEOUT) then
      Exit;                                   // no response in time -> blocked
    n := fpRecv(lFD, @buf[0], SizeOf(buf), 0);
    if n <= 0 then
      Exit;
    aLatencyMs := (NowMicro - t0) / 1000.0;
    SetString(s, PAnsiChar(@buf[0]), n);
    p := Pos(#13#10, s);
    if p > 0 then
      aStatus := Copy(s, 1, p - 1)
    else
      aStatus := Copy(s, 1, 40);
    Result := True;
  finally
    fpClose(lFD);
  end;
end;


// Scenario 1: one silent connection held open, then a real request.
function MeasureSingle(aPort: Word; out aLatencyMs: Double; out aStatus: string): Boolean;

var
  lSilent: cint;

begin
  lSilent := StartConnect(aPort);
  try
    Sleep(A_SETTLE);        // let the server accept (and, in -t none, block on) it first
    Result := ProbeRealRequest(aPort, aLatencyMs, aStatus);
  finally
    if lSilent >= 0 then
      fpClose(lSilent);
  end;
end;


// Scenario 2: N silent connections held open, then a real request.
function MeasureFlood(aPort: Word; aN: Integer; out aLatencyMs: Double;
  out aStatus: string; out aEstablished: Integer): Boolean;

var
  lFds: array of cint;
  i: Integer;

begin
  SetLength(lFds, aN);
  for i := 0 to aN - 1 do
    lFds[i] := StartConnect(aPort);   // fire all at once; non-blocking, no stacking
  try
    Sleep(A_SETTLE);
    aEstablished := 0;
    for i := 0 to aN - 1 do
      if (lFds[i] >= 0) and WaitReady(lFds[i], True, 0) and ConnectedOK(lFds[i]) then
        Inc(aEstablished);
    Result := ProbeRealRequest(aPort, aLatencyMs, aStatus);
  finally
    for i := 0 to aN - 1 do
      if lFds[i] >= 0 then
        fpClose(lFds[i]);
  end;
end;


// Poll until something accepts TCP connections on aPort.
function WaitListening(aPort: Word; aTimeoutMs: Integer): Boolean;

var
  lDeadline: Int64;
  lFD: cint;
  lOk: Boolean;

begin
  Result := False;
  lDeadline := NowMicro + Int64(aTimeoutMs) * 1000;
  while NowMicro < lDeadline do
  begin
    lFD := StartConnect(aPort);
    if lFD >= 0 then
    begin
      lOk := WaitReady(lFD, True, 200) and ConnectedOK(lFD);
      fpClose(lFD);
      if lOk then
        Exit(True);
    end;
    Sleep(50);
  end;
end;


// Launch simpleserver in the given mode, quietly, serving a temp dir.
function StartServer(const aBinary, aMode: string; aPort: Word;
  out aProc: TProcess; out aTmpDir: string): Boolean;

var
  f: TextFile;

begin
  aTmpDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('sstest_%s_%d', [aMode, aPort]);
  ForceDirectories(aTmpDir);
  AssignFile(f, IncludeTrailingPathDelimiter(aTmpDir) + 'index.html');
  Rewrite(f);
  WriteLn(f, '<html><body>ok</body></html>');
  CloseFile(f);
  aProc := TProcess.Create(nil);
  aProc.Executable := aBinary;
  aProc.Parameters.Add('-p');
  aProc.Parameters.Add(IntToStr(aPort));
  aProc.Parameters.Add('-d');
  aProc.Parameters.Add(aTmpDir);
  aProc.Parameters.Add('-t');
  aProc.Parameters.Add(aMode);
  aProc.Parameters.Add('-q');                 // keep the server quiet
  aProc.CurrentDirectory := aTmpDir;
  aProc.Options := [poUsePipes, poStderrToOutPut];
  aProc.Execute;
  Result := WaitListening(aPort, 5000);
end;


procedure StopServer(aProc: TProcess; const aTmpDir: string);

begin
  if Assigned(aProc) then
  begin
    if aProc.Running then
      aProc.Terminate(0);
    aProc.Free;
  end;
  DeleteFile(IncludeTrailingPathDelimiter(aTmpDir) + 'index.html');
  RemoveDir(aTmpDir);
end;


function FmtLatency(aServed: Boolean; aLatencyMs: Double): string;

begin
  if aServed then
    Result := Format('%.1f ms', [aLatencyMs])
  else
    Result := '>' + IntToStr(B_READ_TIMEOUT) + ' ms';
end;


var
  Binary, tmp, status: string;
  BasePort: Word;
  Flood, i, est: Integer;
  proc: TProcess;
  lat: Double;
  served: Boolean;

begin
  Binary := DEFAULT_BIN;
  if ParamCount >= 1 then
    Binary := ParamStr(1);
  BasePort := 3899;
  if ParamCount >= 2 then
    BasePort := StrToIntDef(ParamStr(2), 3899);
  Flood := FLOOD_COUNT;
  if ParamCount >= 3 then
    Flood := StrToIntDef(ParamStr(3), FLOOD_COUNT);

  if not FileExists(Binary) then
  begin
    WriteLn('simpleserver binary not found: ', Binary);
    WriteLn('Build it first, or pass the path as the first argument.');
    Halt(1);
  end;

  WriteLn('Testing: ', Binary);
  WriteLn;

  // ---- Scenario 1 ----------------------------------------------------------
  WriteLn('Scenario 1: hold one connection silent, time a real request on another.');
  WriteLn(Format('  %-11s  %-12s  %s', ['mode', 'B latency', 'verdict / response']));
  WriteLn('  -----------  ------------  ----------------------------------------------');
  for i := 0 to High(Modes) do
  begin
    if StartServer(Binary, Modes[i], BasePort + i, proc, tmp) then
    begin
      served := MeasureSingle(BasePort + i, lat, status);
      if served then
        WriteLn(Format('  %-11s  %-12s  served: %s',
          [Modes[i], FmtLatency(True, lat), status]))
      else
        WriteLn(Format('  %-11s  %-12s  BLOCKED (no response in %d s)',
          [Modes[i], FmtLatency(False, lat), B_READ_TIMEOUT div 1000]));
    end
    else
      WriteLn(Format('  %-11s  %-12s  did not start (rebuilt with -t support?)',
        [Modes[i], '-']));
    StopServer(proc, tmp);
    Sleep(300);
  end;

  // ---- Scenario 2 ----------------------------------------------------------
  WriteLn;
  WriteLn(Format('Scenario 2: hold %d connections silent, time a real request on another.',
    [Flood]));
  WriteLn(Format('  %-11s  %-12s  %-8s  %s',
    ['mode', 'B latency', 'idle up', 'verdict / response']));
  WriteLn('  -----------  ------------  --------  ----------------------------------------');
  for i := 0 to High(Modes) do
  begin
    if StartServer(Binary, Modes[i], BasePort + 20 + i, proc, tmp) then
    begin
      served := MeasureFlood(BasePort + 20 + i, Flood, lat, status, est);
      if served then
        WriteLn(Format('  %-11s  %-12s  %-8s  served: %s',
          [Modes[i], FmtLatency(True, lat), Format('%d/%d', [est, Flood]), status]))
      else
        WriteLn(Format('  %-11s  %-12s  %-8s  BLOCKED (no response in %d s)',
          [Modes[i], FmtLatency(False, lat), Format('%d/%d', [est, Flood]),
           B_READ_TIMEOUT div 1000]));
    end
    else
      WriteLn(Format('  %-11s  %-12s  %-8s  did not start (rebuilt with -t support?)',
        [Modes[i], '-', '-']));
    StopServer(proc, tmp);
    Sleep(300);
  end;

  WriteLn;
  WriteLn('Interpreting the results:');
  WriteLn('  none       - one silent socket already stalls real traffic; under a flood it');
  WriteLn('               cannot even accept connections (listen backlog fills).');
  WriteLn('  polled     - one thread, yet stays flat: idle sockets are parked, not read.');
  WriteLn('  thread     - fast, but spawns one OS thread per idle connection.');
  WriteLn('  threadpool - may BLOCK under the flood if idle sockets >= pool workers;');
  WriteLn('               that pool-exhaustion risk is exactly what polled avoids.');
end.
