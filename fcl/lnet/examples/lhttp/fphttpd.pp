{ Web serving daemon

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

program fphttpd;

{$mode objfpc}{$h+}

uses
  SysUtils, Classes, lNet,
{$ifdef UNIX}
  BaseUnix, Errors,
{$endif}
  lhttp, lwebserver, lHTTPSettings, lSpawnFCGI;

var
  Server: TLHTTPServer;
  FileHandler: TFileHandler;
  CGIHandler: TCGIHandler;
  PHPCGIHandler: TPHPFastCGIHandler;
  Quit: Boolean = False;
  
procedure HandleTermSignal(sig: longint); cdecl;
begin
  Quit := true;
end;
  
procedure MainLoop;
var
  lRuns: dword;
begin
  lRuns := $FFFFFFFF;
  while (lRuns > 0) and (Server.Connected) and (not Quit) do
  begin
    Server.CallAction;
    if Assigned(PHPCGIHandler.Pool.Timer) then
    begin
      Server.Eventer.Timeout := 2000;
      PHPCGIHandler.Pool.Timer.CallAction;
    end;
    dec(lruns);
    if (lruns and $FFF) = 0 then
      writeln(lruns);
  end;
end;

function SetServer: Boolean;
begin
  Result:=False;
  Server := TLHTTPServer.Create(nil);
  Server.ServerSoftware := 'fpHTTPd/0.4';
  FileHandler := TFileHandler.Create;
  FileHandler.MimeTypeFile := GetMimeFile;
  FileHandler.DocumentRoot := GetHTTPPath;
  CGIHandler := TCGIHandler.Create;
  CGIHandler.CGIRoot := GetCGIRoot;
  CGIHandler.DocumentRoot := GetHTTPPath;
  CGIHandler.EnvPath := GetCGIPath;
  CGIHandler.ScriptPathPrefix := GetScriptPathPrefix;
  PHPCGIHandler := TPHPFastCGIHandler.Create;
  PHPCGIHandler.Host := 'localhost';
  PHPCGIHandler.Port := GetPHPCGIPort;
  PHPCGIHandler.AppEnv := GetPHPCGIEnv;
  PHPCGIHandler.AppName := GetPHPCGIBinary;
  PHPCGIHandler.EnvPath := GetCGIPath;
  Server.RegisterHandler(FileHandler);
  Server.RegisterHandler(CGIHandler);
  FileHandler.DirIndexList.Add('index.html');
  FileHandler.DirIndexList.Add('index.htm');
  FileHandler.DirIndexList.Add('index.php');
  FileHandler.DirIndexList.Add('index.cgi');
  FileHandler.RegisterHandler(PHPCGIHandler);
  Server.TimeOut := 300000;
  if not Server.Listen(GetPort) then
    Writeln('Error starting server.')
  else
    Result:=True;
end;

procedure HandleSignals;
begin
  {$ifdef UNIX}
  FpSignal(SIGTERM, @HandleTermSignal);
  FpSignal(SIGINT, @HandleTermSignal);
  FpSignal(SIGHUP, signalhandler(SIG_IGN));
  {$else}
  {$endif}
end;

function Daemonize: Integer;
  {$ifdef UNIX}
var
  PID: TPid;
begin
  Result:=-1;
  PID:=fpFork;
  if PID = 0 then begin
    if SetServer then
      Result:=1;
    if Result > 0 then
      EnableWriteln:=False;
  end else if PID < 0 then
    Writeln('Error on fork: ', StrError(fpGetErrno))
  else
    Result:=0;
  {$else}
begin
  Result:=-1;
  {$endif}
end;

procedure Run(const BG: Boolean);
begin
  HandleSignals;
  InitSettings;
  if BG then begin
    case Daemonize of
      1: MainLoop;
      0: Writeln('Succesfully started server');
     -1: Writeln('Unable to start daemon/service, please try with -c');
    end;
  end else if SetServer then begin
    Writeln('Succesfully started server');
    MainLoop;
  end;
  FileHandler.Free;
  CGIHandler.Free;
  PHPCGIHandler.Free;
  Server.Free;
end;

procedure Spawn;
begin
  SpawnFCGIProcess(ParamStr(2), ParamStr(4), StrToInt(ParamStr(3)));
end;

procedure WriteUsage;
begin
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' [-h|-c|-s <fcgi port [enviro]>]');
  Writeln('       -h -- write this help message');
  Writeln('       -c -- starts server in console (not as daemon/service)');
  Writeln('       -s -- spawn a fastcgi process');
  Writeln('          ++ <args>');
  Writeln('             -- fcgi is the name of the fastcgi app, with full path');
  Writeln('             -- port is the port on which fastcgi will listen');
  Writeln('             -- enviro is the environment variable string with ":" as separator');
  Writeln('Example:');
  Writeln('        to spawn a php-cgi located in /usr/local/bin/php-cgi with 5 children on port 6000:');
  Writeln('        ', ExtractFileName(ParamStr(0)), ' -s /usr/local/bin/php-cgi 6000 PHP_FCGI_CHILDREN=5');
end;

begin
  if ParamStr(1) = '-c' then
    Run(LowerCase(ParamStr(1)) <> '-c')
  else if ParamStr(1) = '-s' then begin
    if ParamCount >= 3 then
      Spawn
    else
      WriteUsage;
  end else
   WriteUsage;
end.
