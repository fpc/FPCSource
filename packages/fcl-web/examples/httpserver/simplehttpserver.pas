program simplehttpserver;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, fphttpserver, fpmimetypes, testhttpserver;

Type

  { TTestHTTPServer }

  THTTPServer = Class(TTestHTTPServer)
  Protected
    Procedure DoIdle(Sender : TObject);
    procedure DoWriteInfo(S: string);
  end;

Var
  Serv : THTTPServer;

{ THTTPServer }

procedure THTTPServer.DoIdle(Sender: TObject);
begin
  Writeln('Idle, waiting for connections');
end;

procedure THTTPServer.DoWriteInfo(S: string);
begin
  Writeln(S);
end;

begin
  Serv:=THTTPServer.Create(Nil);
  try
    Serv.BaseDir:=ExtractFilePath(ParamStr(0));
{$ifdef unix}
    Serv.MimeTypesFile:='/etc/mime.types';
{$endif}
    Serv.Threaded:=False;
    Serv.Port:=8080;
    Serv.AcceptIdleTimeout:=1000;
    Serv.OnAcceptIdle:=@Serv.DoIdle;
    Serv.WriteInfo:=@Serv.DoWriteInfo;
    Serv.Active:=True;
  finally
    Serv.Free;
  end;
end.

