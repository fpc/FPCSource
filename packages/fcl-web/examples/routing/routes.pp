unit routes;

{$mode objfpc}{$H+}

interface

uses
 sysutils, classes, httpdefs, httproute;

Var
  BaseURL : String;

Procedure RegisterRoutes;

implementation

uses webutil, fphttp;

Type

  { TMyModule }

  TMyModule = Class(TCustomHTTPModule)
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;
  
  { TMyIntf }

  TMyIntf = Class(TObject,IRouteInterface)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
  end;

  { TMyHandler }

  TMyHandler = Class(TRouteObject)
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);override;
  end;

Var
  C1,C2 : TComponent;
  MyIntf : TMyIntf;


Procedure DumpRoutes(L : TStrings; AURL : String);

  Function DefaultReps(S : String) : string;

  begin
    Result:=StringReplace(S,'*path','somepath',[]);
    Result:=StringReplace(Result,':param1','theparam1',[]);
    Result:=StringReplace(Result,':param2','theparam2',[]);
    Result:=StringReplace(Result,':param','theparam',[]);
    If (Result<>'') and (Result[1]='/') then
      Delete(Result,1,1);
  end;

Var
  I : Integer;
  P : String;

begin
  THTTPRouter.SanitizeRoute(AURL);
  L.Add('<A NAME="routes"/>');
  L.Add('<H1>Try these routes:</H1>');
  For I:=0 to HTTPRouter.RouteCount-1 do
    begin
    P:=DefaultReps(HTTPRouter[i].URLPattern);
    L.Add('<A HREF="'+BaseURL+'/'+P+'">'+P+'</a><br>');
    end;
end;

Procedure RequestToResponse(ATitle : String; ARequest : TRequest; AResponse : TResponse; RouteParams : Array of String);

Var
  L : TStrings;
  S : String;

begin
  L:=TStringList.Create;
  try
    L.Add('<HTML>');
    L.Add('<HEAD>');
    L.Add('<TITLE>'+ATitle+'</TITLE>');
    L.Add('</HEAD>');
    L.Add('<BODY>');
    L.Add('<H1>'+ATitle+'</H1>');
    L.Add('<A HREF="#routes">Jump to routes overview</A>');
    if (Length(RouteParams)>0) then
      begin
      L.Add('<H2>Routing parameters:</H2>');
      L.Add('<table>');
      L.Add('<tr><th>Param</th><th>Value</th></tr>');
      for S in RouteParams do
        L.Add('<tr><td>'+S+'</th><th>'+ARequest.RouteParams[S]+'</th></tr>');
      L.Add('</table>');
      end;
    DumpRequest(ARequest,L,False);
    DumpRoutes(L,ARequest.URL);
    L.Add('</BODY>');
    L.Add('</HTML>');
    AResponse.Content:=L.Text;
    AResponse.SendResponse;
  finally
    L.Free;
  end;
end;

Procedure RequestToResponse(ATitle : String; ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse(ATitle,ARequest,AResponse,[]);
end;

Procedure SimpleCallBack(ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse('Simple callback',ARequest,AResponse);
end;

Procedure DefaultCallBack(ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse('Default callback (*path)',ARequest,AResponse,['path']);
end;

Procedure ParamPathMiddle(ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse('Path in the middle (onepath/*path/new)',ARequest,AResponse,['path']);
end;

Procedure ParamPath(ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse('Parametrized path (onepath/:param)',ARequest,AResponse,['param']);
end;

Procedure ParamPaths2(ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse('Parametrized path (onepath/:param)',ARequest,AResponse,['param1','param2']);
end;

Procedure ComponentPath(AData : Pointer; ARequest : TRequest; AResponse : TResponse);

begin
  RequestToResponse('Component path (component: '+TComponent(AData).Name+')',ARequest,AResponse);
end;



{ TMyModule }

procedure TMyModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  RequestToResponse('Old-fashioned Module',ARequest,AResponse);
end;

{ TMyHandler }

procedure TMyHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  RequestToResponse('Route object',ARequest,AResponse);
end;

{ TMyIntf }

procedure TMyIntf.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  RequestToResponse('Interface object',ARequest,AResponse);
end;

Procedure RegisterRoutes;

begin
  if (C1=Nil) then
    begin
    C1:=TComponent.Create(Nil);
    C1.Name:='ComponentRoute1';
    C2:=TComponent.Create(Nil);
    C2.Name:='ComponentRoute2';
    MyIntf:=TMyIntf.Create;
    end;
  HTTPRouter.RegisterRoute('simple',rmall,@SimpleCallBack);
  HTTPRouter.RegisterRoute('onepath/:param',rmall,@ParamPath);
  HTTPRouter.RegisterRoute('twopaths/:param1/:param2',rmall,@ParamPaths2);
  HTTPRouter.RegisterRoute('onepath/*path/new',rmall,@ParamPathMiddle);
  RegisterHTTPModule('module',TMyModule,True);
  HTTPRouter.RegisterRoute('/component/1',C1,rmall,@ComponentPath);
  HTTPRouter.RegisterRoute('/component/2',C2,rmall,@ComponentPath);
  HTTPRouter.RegisterRoute('/interfaced',rmall,MyIntf);
  HTTPRouter.RegisterRoute('/routed/object',rmall,TMyHandler);
  // This will catch all other paths
  HTTPRouter.RegisterRoute('*path',rmall,@DefaultCallBack,True);
end;

begin
  FreeAndNil(C1);
  FreeAndNil(C2);
end.

