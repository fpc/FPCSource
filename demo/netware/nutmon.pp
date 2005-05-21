Program nutmon;
{

    Simple nut ups monitor for netware, see http://www.networkupstools.org

    This program can be used to shut down a netware server on power
    failure. It requires nut to be installed on a *nix server (the serial
    or usb ups control is not connected to the netware server, this will
    be handled by the upsd on a *nix server)

    FreePascal >= 1.9.5 (http://www.freepascal.org) is needed to compile this.

    This source is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This code is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    A copy of the GNU General Public License is available on the World
    Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also
    obtain it by writing to the Free Software Foundation,
    Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

    First Version 2004/12/16 Armin Diehl <armin@freepascal.org>

 **********************************************************************}
{$mode objfpc}
{$M 65535,0,0}
{$if defined(netware)}
{$if defined(netware_clib)}
{$description nut ups monitor - clib}
{$else}
{$description nut ups monitor - libc}
{$endif}
{$copyright Copyright 2004 Armin Diehl <armin@freepascal.org>}
{$screenname DEFAULT}  // dont use none because writeln will not work with none
{$version 1.0.0}
{$endif netware}

uses
  sysutils, nutconnection, inifiles
  {$if defined(netware_libc)}
  ,libc
  {$elseif defined(netware_clib)}
  ,nwserv,nwnit
  {$endif}
  ;


const
  CMD_NONE = 0;
  CMD_STATUS = 1;
  CMD_TESTSHUTDOWN = 2;

var
  nut : TNutConnection;
  nutUser : string;
  nutPassword : string;
  nutPollfreq : integer;
  nutPollfreqAlert : integer;
  nutReconnectFreq : integer;
  nutUpsName : string;
  terminated : boolean = false;
  upsStatus,lastupsStatus : TUpsStatus;
  waitSemaphore: longint;
  commandAfterDown,powerOffFileName : ansistring;
  downIfCapaityBelow:integer = 0;
{$if defined(netware)}
  CmdParserStruct : TcommandParserStructure;
  CurrentCommand : byte;
  oldNetwareUnloadProc : pointer;
  MainLoopTerminated : boolean = false;
{$endif}

const mainSection = 'nutmon';

procedure readConfig;
var fn : string;
    t  : tiniFile;
begin
  fn := ChangeFileExt(paramstr(0),'.ini');
  t := TIniFile.Create (fn);
  try
    nut.host := t.readString (mainSection,'host','');
    if nut.host = '' then
    begin
      writeln (stderr,paramstr(0)+': host= not specified in '+fn+' exiting');
      halt;
    end;
    nut.port := word (t.readInteger (mainSection,'port',NutDefaultPort));
    nutUser := t.readString (mainSection,'user','');
    if nutUser = '' then
    begin
      writeln (stderr,paramstr(0)+': user= not specified in '+fn+' exiting');
      halt;
    end;
    nutPassword := t.readString (mainSection,'password','');
    if nutPassword = '' then
    begin
      writeln (stderr,paramstr(0)+': password= not specified in '+fn+' exiting');
      halt;
    end;
    nutUpsName := t.readString (mainSection,'upsname','');
    if nutUpsname = '' then
    begin
      writeln (stderr,paramstr(0)+': upsname= not specified in '+fn+' exiting');
      halt;
    end;
    nutPollfreq := t.readInteger (mainSection,'pollfreq',10);
    nutPollfreqAlert := t.readInteger (mainSection,'pollfrqalert',5);
    nut.Debug := (t.readInteger (mainSection,'debug',0) > 0);
    commandAfterDown := t.readString (mainSection,'commandAfterDown','');
    nutReconnectFreq := t.readInteger (mainSection,'reconnectFreq',30);
    powerOffFileName := t.readString (mainSection,'createPoweroffFile','');
    downIfCapaityBelow := t.readInteger (mainSection,'downIfCapacityBelow',0);
  finally
    t.free;
  end;
end;

{$if defined(netware)}
procedure onNetwareUnload;
var i : integer;
begin
  terminated := true;
  SignalLocalSemaphore (waitSemaphore);  // this ends doDelay
  // here we wait for the main thread to terminate
  // we have to wait because system.pp will deinit winsock
  // to allow unload in case a blocking winsock call is
  // active. In case we wont wait here, our tcp socket
  // will be destroyed before we have the chance to send
  // a logout command to upsd
  i := 500;
  System.NetwareUnloadProc := oldNetwareUnloadProc;
  while (i > 0) and (not MainLoopTerminated) do
  begin
    dec(i);
    delay(500);
  end;
end;
{$endif}


procedure doDelay (seconds : integer);
{$if defined(netware)}
begin
  TimedWaitOnLocalSemaphore (waitSemaphore,seconds*1000);
end;
{$else}
var i : integer;
begin
  i := seconds * 2;
  while (not terminated) and (i > 0) do
  begin
    sysutils.sleep(500);
    dec(i);
  end;
end;
{$endif}

var lastAlert : TUpsStatus = [UPS_Online];

procedure doAlert (status : TUpsStatus);
{$if defined(netware)}
var nwAlert : TNetWareAlertStructure;
    s : AnsiString;
begin
  FillChar(nwAlert, sizeof(nwAlert),0);
  nwAlert.nwAlertID := ALERT_UPS;
  nwAlert.nwTargetNotificationBits :=  NOTIFY_ERROR_LOG_BIT+NOTIFY_CONSOLE_BIT;
  nwAlert.nwAlertLocus := LOCUS_UPS;
  nwAlert.nwAlertClass := CLASS_GENERAL_INFORMATION;
  nwAlert.nwAlertSeverity := SEVERITY_CRITICAL;
  if UPS_lowBatt in Status then
    s := 'UPS low Battery, shutting down' else
  if UPS_FSD in Status then
    s := 'UPS Forced Shuttdown' else
  if UPS_online in Status then
    s := 'Power/communication Restored, UPS is online' else
  if UPS_onBatt in Status then
    s := 'Power Failure, UPS is on battery' else
  if UPS_Stale in Status then
    s := 'Lost communication to UPS' else
  if UPS_Disconnected in Status then
    s := 'Lost communication to upsd';
  if lastAlert <> status then
    if (UPS_onBatt in Status) or
       (UPS_lowBatt in Status) or
       (UPS_FSD in Status) or
       (UPS_Online in Status) then
         nwAlert.nwTargetNotificationBits := nwAlert.nwTargetNotificationBits + NOTIFY_EVERYONE_BIT;
  lastAlert := status;

  nwAlert.nwControlString := pchar(s);

  NetWareAlert(GetNlmHandle, @nwAlert, 0, []);
end;
{$else}
begin
end;
{$endif}


procedure doStatusChange (newStatus,oldStatus : TUpsStatus);
begin
  writeln (#13'nutmon: ups status change from '+UpsStatus2Txt (oldStatus)+' to '+UpsStatus2Txt (newStatus));
  doAlert (newStatus);
end;


procedure doShutdown (Reason : AnsiString = 'Server shutting down because of power failure');
var err:integer;
begin
  if poweroffFileName <> '' then
  begin
    err := FileCreate (powerOffFileName);
    if err <> -1 then
      FileClose (err)
    else
      writeln (#13,'nutmon: warning, can not create power off flag file ('+powerOffFileName+')');
  end;
  {$if defined(netware_clib)}
  SendConsoleBroadcast(pchar(Reason),0,nil);
  err := DownFileServer (1);
  try
    nut.login := false;   // notify upds that we are shutting down
    writeln (#13'numon: informed upsd that we have done shutdown');
  except
    on e:Exception do
    begin
      writeln (#13'nutmon: got exception while trying to logout (',e.Message,')');
      try
        nut.connected := false;
      except
      end;
    end;
  end;
  if err = 0 then
    writeln (#13'nutmon: Server is down')
  else
    writeln (#13'nutmon: DownFileServer returned error ',Err);
  if commandAfterDown <> '' then
    nwserv._system (pchar(commandAfterDown));
  repeat
    sysutils.sleep(30);
  until false;
  {$elseif defined(netware_libc)}
  ShutdownServer(nil,false,nil,0);
  repeat
    sysutils.sleep(30);
  until false;
  {$else}
  writeln (stderr,'no shutdown call available, terminating');
  halt;
  {$endif}

end;

procedure mainLoop;
var s : string;
begin
  while not terminated do
  begin
    if not nut.connected then
    begin
      try
        nut.connected := true;
        try
          nut.upsName := nutUpsName;
        except
          if nut.LastResult <> NutDataStale then
          begin
            writeln(stderr,#13'invalid ups name, terminating');
            nut.free;
            halt;
          end else
          begin   // special case: on start UPS is in stale status, disconnect and try later
            upsStatus := [UPS_Stale];
            if (upsStatus <> lastUpsStatus) then doStatusChange (upsStatus, lastUpsStatus);
            lastUpsStatus := upsStatus;
            nut.connected := false;
          end;
        end;

        try
          nut.UpsStatus;
        except
          on e:exception do
          begin
            writeln(stderr,#13'unable get ups status ('+e.Message+'), terminating');
            nut.free;
            halt;
          end;
        end;

        try
          nut.Username := nutUser;
          nut.Password := nutPassword;
          nut.Login := true;
        except
          on e:exception do
          begin
            writeln(stderr,#13'unable to login ('+e.Message+'), terminating');
            nut.free;
            halt;
          end;
        end;
        lastUpsStatus := [UPS_disconnected];
        WriteLn(#13'nutmon: connected to '+nutUpsName+'@'+nut.Host);
      except
        on e:exception do
        begin
          writeln (stderr,#13'nutmon: connect error, will retry in ',nutReconnectFreq,' seconds ('+e.message+')');
          doDelay (nutReconnectFreq);
        end;
      end;
    end else
    begin  // we are connected, poll status
      try
        upsStatus := nut.upsStatus;
        if (upsStatus <> lastUpsStatus) then doStatusChange (upsStatus, lastUpsStatus);
        lastUpsStatus := upsStatus;
        if (UPS_lowBatt in upsStatus) or
           (UPS_FSD in upsStatus) then doShutdown;
        if downIfCapaityBelow > 0 then
          if (UPS_onBatt in upsStatus) then
            if nut.UpsChargeInt < downIfCapaityBelow then
              //writeln ('battery below ',downIfCapaityBelow);
              doShutdown ('Server shutting down,power failure and battery < '+IntToStr(downIfCapaityBelow)+'%');
        if UPS_online in upsStatus then
          doDelay (nutPollfreq)
        else
          doDelay (nutPollfreqAlert);
      except
      end;
    end;
    {$if defined(netware)}
    if CurrentCommand <> CMD_NONE then
    begin
      case CurrentCommand of
        CMD_STATUS: begin
                      if nut.connected then
                      begin
                        writeln (#13'UPS Status:');
                        writeln ('   connected to: ',nut.UpsName+'@',nut.host,':',nut.Port);
                        writeln ('         UPS is: ',UpsStatus2Txt(nut.UpsStatus));
                        try
                          s := nut.upsMfr;
                          writeln ('   manufacturer: ',s);
                        except
                        end;
                        try
                          s := nut.upsModel;
                          writeln ('          model: ',s);
                        except
                        end;
                        try
                          s := nut.UpsLoad;
                          writeln ('   Percent load: ',s);
                        except
                        end;
                        try
                          s := nut.upsTemperature;
                          writeln ('           temp: ',s);
                        except
                        end;
                        try
                          s := nut.upsInputVoltage;
                          writeln ('  input Voltage: ',s);
                        except
                        end;
                        try
                          s := nut.upsOutputVoltage;
                          writeln (' output Voltage: ',s);
                        except
                        end;
                        try
                          s := nut.upsInputFrequency;
                          writeln ('input Frequency: ',s);
                        except
                        end;
                        try
                          s := nut.upsRuntime;
                          writeln ('Battery Runtime: ',s);
                        except
                        end;
                        try
                          s := nut.upsCharge;
                          writeln (' Battery Charge: ',s);
                        except
                        end;
                        try
                          s := nut.numLogins;
                          writeln ('     num Logins: ',s);
                        except
                        end;
                        Writeln (nut.Version);
                      end else
                        writeln (#13'UPS Status: not connected to upsd');
                    end;
        CMD_TESTSHUTDOWN:
                    begin
                      upsStatus := [UPS_FSD];
                      doStatusChange (upsStatus, lastUpsStatus);
                      doShutdown;
                    end;
      end;
      CurrentCommand := CMD_NONE;
    end;
    {$endif}
  end;
end;



{$if defined(netware)}
// handle the command "UPS STATUS"
// only set the requested command and let the main thread handle it
function UpsCommandlineParser (ScreenId : scr_t; commandLine : pchar) : longint; cdecl;
begin
  if strlicomp(commandLine,'ups status',10) = 0 then
  begin
    result := HANDLEDCOMMAND;
    CurrentCommand := CMD_STATUS;
    SignalLocalSemaphore (waitSemaphore);
  end else
  if strlicomp(commandLine,'ups testshutdown',16) = 0 then
  begin
    result := HANDLEDCOMMAND;
    CurrentCommand := CMD_TESTSHUTDOWN;
    SignalLocalSemaphore (waitSemaphore);
  end else
    result := NOTMYCOMMAND;
end;
{$endif}



begin
  try
    {$if defined(netware)}
    waitSemaphore := OpenLocalSemaphore (0);
    CmdParserStruct.Link := nil;
    CmdParserStruct.parseRoutine := @UpsCommandLineParser;
    CmdParserStruct.RTag         := AllocateResourceTag (GetNlmHandle,'nutmon command line parser',ConsoleCommandSignature);

    if RegisterConsoleCommand(CmdParserStruct) <> 0 then
      writeln (stderr,#13'nutmon: RegisterConsoleCommand failed (ups status console command will not work)')
    else begin
      writeln (#13'nutmon console commands available:');
      writeln (#13'ups status         - show ups status');
      writeln (#13'ups testshutdown   - shutdown as if a low power condition is reached');
      writeln;
    end;
    CurrentCommand := CMD_NONE;
    oldNetwareUnloadProc := System.NetwareUnloadProc;
    System.NetwareUnloadProc := @onNetwareUnload;
    {$endif}
    nut := TNutConnection.create;
    readConfig;
    if poweroffFileName <> '' then
      if FileExists (powerOffFileName) then
        if not DeleteFile (powerOffFileName) then
          writeln (#13,'nutmon: warning, can not delete power off flag file ('+powerOffFileName+')');
    if downIfCapaityBelow > 0 then
      writeln (#13'nutmon: will shutdown if battery < ',downIfCapaityBelow,'%');
    mainLoop;
    nut.login := false;
    nut.connected := false;
    nut.free;
  finally
    {$if defined(netware)}
    CloseLocalSemaphore (waitSemaphore);
    UnRegisterConsoleCommand (CmdParserStruct);
    MainLoopTerminated := true;
    {$endif}
  end;
end.
