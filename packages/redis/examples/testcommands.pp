{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by the Free Pascal development team

    Simple Redis test program, donated by Mario Ray Mahardhika

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testcommands;

uses
  sysutils, redis;

const
  MapKey    = 'key';
  MapValue  = 'value';
  ListKey   = 'listkey';
  ListValue = 'v';

var
  GTCPClient: TAbstractTCPClient;
  GRedis: TRedis;
  GRESP: TRESP;
  i: Integer;
begin
  GTCPClient := TSSocketsTCPClient.Create(Redis.DefaultHost, Redis.DefaultPort, Redis.DefaultConnectTimeout, Redis.DefaultCanReadTimeout);
  GRedis := TRedis.Create(GTCPClient);

  Writeln('Testing SET');
  GRESP := GRedis.SendCommand(['SET', MapKey, MapValue]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.StrValue);
  end;

  Writeln('Testing GET:');
  GRESP := GRedis.SendCommand(['GET', MapKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.StrValue);
  end;

  Writeln('Testing LPUSH:');
  for i := 1 to 3 do begin
    GRESP := GRedis.SendCommand(['LPUSH', ListKey, ListValue+IntTostr(i)]);
    if GRESP.RESPType = rtError then begin
      WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
      Halt(1);
    end else begin
      WriteLn(GRESP.IntValue);
    end;
  end;

  Writeln('Testing LRANGE:');
  GRESP := GRedis.SendCommand(['LRANGE', ListKey, '0', '-1']);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    for i := 0 to GRESP.ElementCount - 1 do begin
      if i > 0 then Write(', ');
      Write(GRESP.Elements[i].StrValue);
    end;
    WriteLn;
  end;

  Writeln('Testing GET on array (will result in error):');
  GRESP := GRedis.SendCommand(['GET', ListKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue+' (this is expected)');
  end else begin
    WriteLn('Unexpected reply:', GRESP.StrValue);
  end;

  Writeln('Testing RPOP:');
  for i := 1 to 3 do begin
    GRESP := GRedis.SendCommand(['RPOP', ListKey]);
    if GRESP.RESPType = rtError then begin
      WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
      Halt(1);
    end else begin
      WriteLn(GRESP.StrValue);
    end;
  end;

  Writeln('Testing DEL (',MapKey,') :');
  GRESP := GRedis.SendCommand(['DEL', MapKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.IntValue);
  end;

  Writeln('Testing DEL (',ListKey,') :');
  GRESP := GRedis.SendCommand(['DEL', ListKey]);
  if GRESP.RESPType = rtError then begin
    WriteLn(StdErr, GRESP.ErrorType + ': ' + GRESP.StrValue);
    Halt(1);
  end else begin
    WriteLn(GRESP.IntValue);
  end;

  GRedis.Free;
  GTCPClient.Free;
end.
