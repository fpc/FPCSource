program simplehttpserver;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, strutils,Classes, fphttpserver, fpmimetypes, testhttpserver, custapp;

Type

  { THTTPServer }

  THTTPServer = Class(TTestHTTPServer)
  Protected
    Procedure DoIdle(Sender : TObject);
    procedure DoWriteInfo(S: string);
    Procedure LogEVent(Sender : TObject; aType : TEventType; Const Msg : String);
  end;

Var
  Serv : THTTPServer;

{ THTTPServer }

procedure THTTPServer.DoIdle(Sender: TObject);
begin
  // Writeln('Idle, waiting for connections');
end;

procedure THTTPServer.DoWriteInfo(S: string);
begin
  Writeln(S);
end;

procedure THTTPServer.LogEVent(Sender: TObject; aType: TEventType;
  const Msg: String);
begin
  WriteLn('[',aType,'] ',Msg);
end;

begin
  if IndexText(ParamStr(1),['-h','--help'])<>-1 then
    begin
    Writeln('Usage: ',ExtractFileName(ParamStr(0)),' [dir [port]]');
    Writeln('Default dir is binary location');
    Writeln('Default port is 8080');
    Halt(0);
    end;
  Serv:=THTTPServer.Create(Nil);
  try
    if ParamCount=0 then
      Serv.BaseDir:=ExtractFilePath(ParamStr(0))
    else
      Serv.BaseDir:=ParamStr(1);
    if ParamCount>1 then
      Serv.Port:=StrToIntDef(ParamStr(2),8080)
    else
      Serv.Port:=8080;
    {$ifdef unix}
    Serv.MimeTypesFile:='/etc/mime.types';
    {$endif}
    Serv.ThreadMode:=tmThreadPool;
    Serv.AcceptIdleTimeout:=10;
    Serv.OnAcceptIdle:=@Serv.DoIdle;
    Serv.WriteInfo:=@Serv.DoWriteInfo;
    Serv.KeepConnections:=True;
    Serv.OnLog:=@Serv.LogEVent;
    Serv.LogMoments:=AllLogMoments;
    Serv.Active:=True;
  finally
    Serv.Free;
  end;
end.

