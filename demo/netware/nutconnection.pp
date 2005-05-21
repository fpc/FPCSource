unit nutconnection;
{
    This file is part of nutmon for netware
    Copyright (c) 2004 armin diehl (armin@freepascal.org)

    Simple class to connect to the nut upsd, see
    http://www.networkupstools.org for details about the
    protocol

    Tested with nut 2.0.1pre4

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

interface

uses sysutils, ssockets;

Const
  NutDefaultPort = 3493;
  NutLineEnd    = #10;
  NutDataStale = 'ERR DATA-STALE';

type
  TNutException = class (Exception);
  TUpsStat   = (UPS_disconnected,UPS_stale,UPS_online,UPS_onBatt,UPS_lowBatt,UPS_FSD);
  TUpsStatus = set of TUpsStat;
  TNutConnection = class (TObject)
    private
     fSock : TInetSocket;
     fHost : string;
     fPort : word;
     fUpsName,fUserName,fPassword : string;
     fLogin : boolean;
     fDebug : boolean;
     fLastResponse : string;

      function isConnected : boolean;
      procedure doConnect (c : boolean);
      procedure setHost (s : string);
      procedure setPort (w : word);
      procedure sendCommand (s : string);
      function getOneLine : string;
      function getVersion : string;
      procedure setUpsName (s : string);
      procedure checkConnected;
      function getValue (name : string) : string;
      function getUpsLoad : string;
      function getUpsMfr : string;
      function getUpsModel : string;
      function getUpsRuntime : string;
      function getUpsCharge : string;
      function getUpsChargeInt : integer;
      function getUpsTemperature : string;
      function getInputFrequency : string;
      function getInputVoltage : string;
      function getOutputVoltage : string;
      function getUpsStatus : TUpsStatus;
      procedure setUserName (user : string);
      procedure setPassword (pwd : string);
      procedure doLogin (login : boolean);
      function getNumLogins : string;
    public
      property connected : boolean read isConnected write doConnect;
      property host : string read fHost write setHost;
      property port : word read fPort write setPort;
      property version : string read getVersion;
      property upsName : string read fUpsName write setUpsName;
      property upsload : string read getUpsLoad;
      property upsMfr : string read getUpsMfr;
      property upsModel : string read getUpsModel;
      property upsRuntime : string read getUpsRuntime;
      property upsCharge : string read getUpsCharge;
      property upsChargeInt : integer read getUpsChargeInt;
      property upsTemperature : string read getUpsTemperature;
      property upsInputFrequency : string read getInputFrequency;
      property upsInputVoltage : string read getInputVoltage;
      property upsOutputVoltage : string read getOutputVoltage;
      property upsStatus : TUpsStatus read getUpsStatus;
      property Username : string read fUsername write setUsername;
      property Password : string read fPassword write setPassword;
      // in case login is set to true (and username,password are ok)
      // upsd knows that this system gets power from the ups and
      // will switch off only after login was set to false
      property Login : boolean read fLogin write doLogin;
      property Debug : boolean read fDebug write fDebug;
      property LastResult : string read fLastResponse;
      property numLogins : string read getNumLogins;
  end;

function UpsStatus2Txt (status : TUpsStatus) : string;

implementation

function TNutConnection.isConnected : boolean;
begin
  result := (fSock <> nil);
end;

procedure TNutConnection.doConnect (c : boolean);
begin
  if fSock <> nil then
  begin
    fSock.Free;
    fSock := nil;
    fLogin := false;
  end;
  if c then
    fSock := TInetSocket.Create (fHost, fPort);
end;


procedure TNutConnection.setHost (s : string);
begin
  if fHost <> s then
  begin
    fHost := s;
    doConnect (isConnected);
  end;
end;

procedure TNutConnection.setPort (w : word);
begin
  if w <> fPort then
  begin
    fPort := w;
    doConnect (isConnected);
  end;
end;

procedure TNutConnection.checkConnected;
begin
  if not isConnected then
    raise (TNutException.Create ('not connected'));
end;

procedure TNutConnection.sendCommand (s : string);
var len : longint;
begin
  checkConnected;
  if fDebug then
    writeln (stderr,'S: "'+s+'"');
  s := s + NutLineEnd;
  len := fSock.Write (s[1],length(s));
  if (len <> length(s)) then
  begin
    if fDebug then
      writeln (stderr,'send error');
    doConnect (false);
    raise (TNutException.Create ('send failed, disconnected from upsd'));
  end;
end;

function TNutConnection.getOneLine : string;
var c : char;
begin
  checkConnected;
  fLastResponse := '';
  result := '';
  while (fSock.read (c,1) = 1) do
  begin
    if c = NutLineEnd then
    begin
      if fDebug then
        writeln (stderr,'R: "'+result+'"');
      fLastResponse := result;
      exit;
    end;
    result := result + c;
  end;
end;

function TNutConnection.getVersion : string;
begin
  sendCommand ('VER');
  result := getOneLine;
end;


procedure TNutConnection.setUpsName (s : string);
var res : string;
begin
  fUpsName := '';
  sendCommand ('GET NUMLOGINS '+s);
  res := getOneLine;
  if copy (res,1,10) <> 'NUMLOGINS ' then
    Raise (TNutException.Create ('setUpsName, unknown response from upsd'));
  fUpsName := s;
end;

function TNutConnection.getValue (name : string) : string;
var s : string;
begin
  if fUpsName = '' then
    raise (TNutException.Create ('upsName not set'));
  sendCommand ('GET VAR '+fUpsName+' '+name);
  s := getOneLine;
  if s = 'ERR DATA-STALE' then
  begin
    result := s;
    exit;
  end;
  if copy (s,1,4) <> 'VAR ' then
    raise (TNutException.Create ('result from GET VAR invalid, does not begin with "VAR "'));
  delete (s,1,4);
  if ansiUpperCase (copy (s,1,length(fUpsName))) <> ansiUpperCase (fUpsName) then
    raise (TNutException.Create ('result from GET VAR invalid, second param was not upsName'));
  delete (s,1,length(fUpsName)+1);
  delete (s,1,length(name)+1);
  if copy (s,1,1) = '"' then delete (s,1,1);
  if copy (s,length(s),1) = '"' then delete (s,length(s),1);
  result := s;
end;

function TNutConnection.getUpsLoad : string;
begin
  result := getValue ('ups.load');
end;

function TNutConnection.getUpsMfr : string;
begin
  result := getValue ('ups.mfr');
end;

function TNutConnection.getUpsModel : string;
begin
  result := getValue ('ups.model');
end;

function TNutConnection.getUpsRuntime : string;
begin
  result := getValue ('battery.runtime');
end;

function TNutConnection.getUpsCharge : string;
begin
  result := getValue ('battery.charge');
end;


function TNutConnection.getUpsChargeInt : integer;
var s : string;
    p : integer;
begin
  try
    s := getUpsCharge;
    p := Pos ('.',s);
    if p > 0 then
      delete (s,p,255);
    result := StrToInt (s);
  except
    result := 100;
  end;
end;

function TNutConnection.getUpsTemperature : string;
begin
  result := getValue ('ups.temperature');
end;

function TNutConnection.getInputFrequency : string;
begin
  result := getValue ('input.frequency');
end;

function TNutConnection.getInputVoltage : string;
begin
  result := getValue ('input.voltage');
end;

function TNutConnection.getOutputVoltage : string;
begin
  result := getValue ('output.voltage');
end;

function TNutConnection.getNumLogins : string;
var res : string;
    p : integer;
begin
  if fUpsName = '' then
    Raise (TNutException.Create ('getNumLogins, upsName not set'));
  sendCommand ('GET NUMLOGINS '+fUpsName);
  res := getOneLine;
  if copy (res,1,10) <> 'NUMLOGINS ' then
    Raise (TNutException.Create ('setUpsName, unknown response from upsd'));
  delete (res,1,10);
  p := pos (' ',res);
  if p > 0 then
    delete (res,1,p);
  result :=res;
end;

function TNutConnection.getUpsStatus : TUpsStatus;
var s,value : string;
    i : integer;
begin
  try
    s := getValue ('ups.status');
    result := [];
    if s = NutDataStale then
      result := [UPS_stale]
    else
    repeat
      i := pos (' ',s);
      if (i > 0) then
      begin
        value := trim(ansiuppercase(copy(s,1,i-1)));
        delete (s,1,i); s:=trim(s);
      end else
      begin
        value := trim(ansiuppercase(s));
        s := '';
      end;
      if value = 'OL' then
        result := result + [UPS_online]
      else if value = 'OB' then
        result := result + [UPS_onBatt]
      else if value = 'LB' then
        result := result + [UPS_LowBatt]
      else if value = 'FSD' then
        result := result + [UPS_FSD];
    until s = '';
  except
    result := [UPS_disconnected];
  end;
end;


procedure TNutConnection.setUserName (user : string);
var res : string;
begin
  fUserName := user;
  if fUserName <> '' then
  begin
    sendCommand ('USERNAME '+user);
    res := getOneLine;
    if res <> 'OK' then
      raise (TNutException.Create (format ('username failed (%s)',[res])));
  end;
end;

procedure TNutConnection.setPassword (pwd : string);
var res : string;
begin
  fPassword := pwd;
  if pwd <> '' then
  begin
    sendCommand ('PASSWORD '+pwd);
    res := getOneLine;
    if res <> 'OK' then
      raise (TNutException.Create (format ('password failed (%s)',[res])));
  end;
end;

procedure TNutConnection.doLogin (login : boolean);
var res : string;
begin
  if login then
  begin
    if fLogin then
      raise (TNutException.Create ('already logged in'));
    if (fUsername = '') or (fPassword = '') or (fUpsName = '') then
      raise (TNutException.Create ('Login requires UpsName, Username and Password'));
    sendCommand ('LOGIN '+fUpsName);
    res := getOneLine;
    if res <> 'OK' then
      raise (TNutException.Create (format ('login failed (%s)',[res])));
    fLogin := true;
  end else
  if fLogin then
  begin
    sendCommand ('LOGOUT');
    res := getOneLine;
    if (copy(res,1,2) <> 'OK') and (copy(res,1,6)<> 'Goodby') then
      raise (TNutException.Create (format('logout failed, got "%s"',[res])));
    fLogin := false;
    doConnect(false);
  end;
end;

function UpsStatus2Txt (status : TUpsStatus) : string;
begin
  result := '';
  if UPS_disconnected in status then result := result + 'disconnected ';
  if UPS_stale        in status then result := result + 'stale ';
  if UPS_online       in status then result := result + 'online ';
  if UPS_onBatt       in status then result := result + 'onBattery ';
  if UPS_lowBatt      in status then result := result + 'LowBattery ';
  if UPS_FSD          in status then result := result + 'ForeceShutdown ';
  result := trim(result);
end;


end.
