program RPCServ;

uses SysUtils, Classes, fpAsync, HTTPSvlt, svrclass, svrclass_XMLRPC;

type

  TServerApplication = class(TComponent)
  private
    EventLoop: TEventLoop;
    HttpServer: THttpServer;
    ServerClass: TServerClass;
    XMLRPCServlet: TServerClassXMLRPCServlet;
    procedure OnKeyboardData(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

constructor TServerApplication.Create;
begin
  inherited Create(nil);

  EventLoop := TEventLoop.Create;

  ServerClass := TServerClass.Create;

  XMLRPCServlet := TServerClassXMLRPCServlet.Create(Self);
  XMLRPCServlet.Name := 'XMLRPCServlet';
  XMLRPCServlet.ServerClass := ServerClass;

  HttpServer := THttpServer.Create(Self);
  HttpServer.EventLoop := EventLoop;
  if ParamCount = 2 then
    HttpServer.Port := StrToInt(ParamStr(1))
  else
    HttpServer.Port := 12345;
  HTTPServer.AddServlet(XMLRPCServlet, '/xmlrpc');

  WriteLn('Listening on port ', HttpServer.Port);
end;

destructor TServerApplication.Destroy;
begin
  HTTPServer.Free;
  XMLRPCServlet.Free;
  ServerClass.Free;
  EventLoop.Free;
  inherited Destroy;
end;

procedure TServerApplication.Run;
begin
  EventLoop.SetDataAvailableNotify(StdInputHandle, @OnKeyboardData, nil);
  HttpServer.Active := True;
  EventLoop.Run;
end;

procedure TServerApplication.OnKeyboardData(Sender: TObject);
begin
  EventLoop.Break;
end;

var
  App: TServerApplication;
begin
  App := TServerApplication.Create;
  try
    App.Run;
  finally
    App.Free;
  end;
end.
