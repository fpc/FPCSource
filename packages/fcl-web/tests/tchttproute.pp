unit tchttproute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, tcwebmodule, testregistry, httpdefs, httproute, fphttp, fpweb, custweb;

Type

  { TMyModule }

  TMyModule = Class(TCustomHTTPModule)
  Private
    class Var
      FCallCount : Integer;
      FCallRequest : TRequest;
      FCallResponse : TResponse;
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TTestHTTPRoute }
  TMyHTTPRouter = Class(THTTPRouter);

  { TMyInterfacedHandler }

  TMyInterfacedHandler = class(TObject,IRouteInterface)
  private
    FCallCount: Integer;
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse);
    Property CallCount : Integer Read FCallCount;
  end;

  { TMyObjectHandler }

  TMyObjectHandler = Class(TRouteObject)
    class Var
      FCallCount : Integer;
      FCallRequest : TRequest;
      FCallResponse : TResponse;
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  TTestHTTPRoute = class(TTestCase)
  private
    FInterfacedHandler: TMyInterfacedHandler;
    FEventCalled : Integer;
    FRequest: TFakeRequest;
    FResponse: TFakeResponse;
    FRouteParams: TStrings;
    FGetRouteMethod: TRouteMethod;
    FGetRoutePath: string;
    FBeforeCalledCount:integer;
    FAfterCalledCount:integer;
    FDoException : Boolean;
    FModuleItem: TModuleItem;
    FModuleCallCount : Integer;
    FWebhandler : TWebhandler;
    procedure DoGetRoute;
    procedure DoRouteRequest;
    function GetWebHandler: TWebhandler;
  protected
    Procedure MyRouteEvent(ARequest : TRequest; AResponse : TResponse);
    Procedure MyRouteEvent2(ARequest : TRequest; AResponse : TResponse);
    Procedure MyRouteEvent3(ARequest : TRequest; AResponse : TResponse);
    procedure SetUp; override;
    procedure TearDown; override;
    Property InterfacedHandler : TMyInterfacedHandler Read FInterfacedHandler;
    Property RouteParams : TStrings Read FRouteParams;
    Property FakeRequest : TFakeRequest Read FRequest;
    Property FakeResponse : TFakeResponse Read FResponse;
    Property WebHandler : TWebhandler Read GetWebHandler;
  public
    procedure DoAfterRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure DoBeforeRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure DoModuleRoute(Sender: TModuleItem; ARequest: TRequest; AResponse: TResponse);
  published
    procedure TestHookUp;
    Procedure TestAddEvent;
    Procedure TestAddEventMethod;
    Procedure TestAddEventDefault;
    Procedure TestAddInterface;
    Procedure TestAddInterfaceMethod;
    Procedure TestAddInterfaceDefault;
    Procedure TestAddCallBackex;
    Procedure TestAddCallBackMethodEx;
    Procedure TestAddCallBackDefaultEx;
    Procedure TestAddCallBack;
    Procedure TestAddCallBackMethod;
    Procedure TestAddCallBackDefault;
    Procedure TestAddRouteObject;
    Procedure TestAddRouteObjectMethod;
    Procedure TestAddRouteObjectDefault;
    Procedure TestFindRouteStatic;
    Procedure TestFindRouteStaticNoMatch;
    Procedure TestGetRouteStatic;
    Procedure TestGetRouteStaticNoMatch;
    Procedure TestGetRouteStaticNoMethodMatch;
    Procedure TestFindRouteStatic2Paths;
    Procedure TestFindRouteStatic2PathsNoMatch;
    Procedure TestFindRouteStaticMethodMismatch;
    Procedure TestFindRouteWildCard;
    Procedure TestFindRouteNamedWildCard;
    Procedure TestFindRouteNamedWildCard2;
    Procedure TestFindRouteWildCard3;
    Procedure TestFindRouteWildCard3Named;
    Procedure TestFindRouteParam;
    Procedure TestFindRouteParam2;
    Procedure TestFindRouteWildcardParam;
    Procedure TestFindRouteWildcardParamNoMatch;
    Procedure TestSetServiceClass;
    Procedure TestRouteRequestEvent;
    Procedure TestRouteRequestCallback;
    Procedure TestRouteRequestInterface;
    Procedure TestRouteRequestObject;
    Procedure TestRouteRequestException;
    Procedure TestRouteModule;
    procedure TestRouteModuleAfterRoute;
    procedure TestRouteModuleAfterRoute2;
    Procedure TestWebModuleHandlerLegacy;
    Procedure TestWebModuleHandlerNew;
  end;

implementation


Var
  CallBackCalled : Integer;
  CallBackData : Pointer;

Procedure MyRouteCallBackEx(Data : Pointer;ARequest : TRequest; AResponse : TResponse);

begin
  CallBackCalled:=1;
  CallBackData:=Data;
end;

Procedure MyRouteCallBack2Ex(Data : Pointer;ARequest : TRequest; AResponse : TResponse);

begin
  CallBackCalled:=2;
  CallBackData:=Data;
end;

Procedure MyRouteCallBack3Ex(Data : Pointer;ARequest : TRequest; AResponse : TResponse);

begin
  CallBackCalled:=3;
  CallBackData:=Data;
end;

Procedure MyRouteCallBack(ARequest : TRequest; AResponse : TResponse);

begin
  CallBackCalled:=1;
  CallBackData:=Nil;
end;

Procedure MyRouteCallBack2(ARequest : TRequest; AResponse : TResponse);

begin
  CallBackCalled:=2;
  CallBackData:=Nil;
end;

Procedure MyRouteCallBack3(ARequest : TRequest; AResponse : TResponse);

begin
  CallBackCalled:=3;
  CallBackData:=Nil;
end;

{ TMyObjectHandler }

procedure TMyObjectHandler.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  Inc(FCallCount);
  FCallRequest:=ARequest;
  FCallResponse:=AResponse;
end;

{ TMyModule }

procedure TMyModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  Inc(FCallCount);
  FCallRequest:=ARequest;
  FCallResponse:=AResponse;
end;


{ TMyInterfacedHandler }

procedure TMyInterfacedHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  Inc(FCallCount);
end;

procedure TTestHTTPRoute.TestHookUp;
begin
  AssertEquals('No routes registered.',0,HTTPRouter.RouteCount);
  AssertEquals('Routeclass.',THTTPRouter,THTTPRouter.ServiceClass);
  AssertNotNull('Have interfaced handler',InterfacedHandler);
  AssertEquals('interfaced handler not called',0,InterfacedHandler.CallCount);
  AssertEquals('No callbacks',0,CallBackCalled);
  AssertEquals('No events',0,FEventCalled);
  AssertEquals('No module calls',0,TMyModule.FCallCount);
  AssertNull('No module request',TMyModule.FCallRequest);
  AssertNull('No module response',TMyModule.FCallResponse);
end;

procedure TTestHTTPRoute.TestAddEvent;

Var
  E : THTTPRouteEvent;

begin
  HTTPRouter.RegisterRoute('*path',@MyRouteEvent);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteEvent,HTTPRouter[0].ClassType);
  E:=THTTPRouteEvent(HTTPRouter[0]);
  AssertTrue('Route event correct',E.Event=@MyRouteEvent);
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmAll=E.Method);
end;

procedure TTestHTTPRoute.TestAddEventMethod;

Var
  E : THTTPRouteEvent;

begin
  HTTPRouter.RegisterRoute('*path',rmPOST, @MyRouteEvent);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteEvent,HTTPRouter[0].ClassType);
  E:=THTTPRouteEvent(HTTPRouter[0]);
  AssertTrue('Route event correct',E.Event=@MyRouteEvent);
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPOST=E.Method);
end;

procedure TTestHTTPRoute.TestAddEventDefault;
Var
  E : THTTPRouteEvent;

begin
  HTTPRouter.RegisterRoute('*path',rmPOST, @MyRouteEvent,True);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteEvent,HTTPRouter[0].ClassType);
  E:=THTTPRouteEvent(HTTPRouter[0]);
  AssertTrue('Route event correct',E.Event=@MyRouteEvent);
  AssertEquals('Route class not default',True,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPOST=E.Method);
end;

procedure TTestHTTPRoute.TestAddInterface;

Var
  E : THTTPRouteInterface;

begin
  HTTPRouter.RegisterRoute('*path',InterfacedHandler);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteInterface,HTTPRouter[0].ClassType);
  E:=THTTPRouteInterface(HTTPRouter[0]);
  AssertTrue('Route interface correct',Pointer(E.Intf)=Pointer(InterfacedHandler as IRouteInterface));
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URLPattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmAll=E.Method);
end;

procedure TTestHTTPRoute.TestAddInterfaceMethod;

Var
  E : THTTPRouteInterface;

begin
  HTTPRouter.RegisterRoute('*path',rmPost,InterfacedHandler);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteInterface,HTTPRouter[0].ClassType);
  E:=THTTPRouteInterface(HTTPRouter[0]);
  AssertTrue('Route interface correct',Pointer(E.Intf)=Pointer(InterfacedHandler as IRouteInterface));
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URLPattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddInterfaceDefault;
Var
  E : THTTPRouteInterface;

begin
  HTTPRouter.RegisterRoute('*path',rmPost,InterfacedHandler,True);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteInterface,HTTPRouter[0].ClassType);
  E:=THTTPRouteInterface(HTTPRouter[0]);
  AssertTrue('Route interface correct',Pointer(E.Intf)=Pointer(InterfacedHandler as IRouteInterface));
  AssertEquals('Route class not default',True,E.Default);
  AssertEquals('Route URLPattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddCallBackex;

Var
  E : THTTPRouteCallBackex;

begin
  HTTPRouter.RegisterRoute('*path',@E,@MyRouteCallBackex);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteCallBackex,HTTPRouter[0].ClassType);
  E:=THTTPRouteCallBackex(HTTPRouter[0]);
  AssertTrue('Route event correct',Pointer(E.CallBack)=Pointer(@MyRouteCallBackex));
  AssertTrue('Data pointer correct',E.Data=@E);
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmAll=E.Method);
end;

procedure TTestHTTPRoute.TestAddCallBackMethodEx;

Var
  E : THTTPRouteCallBackex;

begin
  HTTPRouter.RegisterRoute('*path',@E,rmPOST,@MyRouteCallBackex);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteCallBackex,HTTPRouter[0].ClassType);
  E:=THTTPRouteCallBackex(HTTPRouter[0]);
  AssertTrue('Route event correct',Pointer(E.CallBack)=Pointer(@MyRouteCallBackex));
  AssertTrue('Data pointer correct',E.Data=@E);
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddCallBackDefaultEx;
Var
  E : THTTPRouteCallBackex;

begin
  HTTPRouter.RegisterRoute('*path',@E,rmPOST,@MyRouteCallBackex,true);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteCallBackex,HTTPRouter[0].ClassType);
  E:=THTTPRouteCallBackex(HTTPRouter[0]);
  AssertTrue('Route event correct',Pointer(E.CallBack)=Pointer(@MyRouteCallBackex));
  AssertTrue('Data pointer correct',E.Data=@E);
  AssertEquals('Route class not default',true,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddCallBack;

Var
  E : THTTPRouteCallBack;

begin
  HTTPRouter.RegisterRoute('*path',@MyRouteCallBack);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteCallBack,HTTPRouter[0].ClassType);
  E:=THTTPRouteCallBack(HTTPRouter[0]);
  AssertTrue('Route event correct',Pointer(E.CallBack)=Pointer(@MyRouteCallBack));
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmAll=E.Method);
end;

procedure TTestHTTPRoute.TestAddCallBackMethod;

Var
  E : THTTPRouteCallBack;

begin
  HTTPRouter.RegisterRoute('*path',rmPOST,@MyRouteCallBack);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteCallBack,HTTPRouter[0].ClassType);
  E:=THTTPRouteCallBack(HTTPRouter[0]);
  AssertTrue('Route event correct',Pointer(E.CallBack)=Pointer(@MyRouteCallBack));
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddCallBackDefault;
Var
  E : THTTPRouteCallBack;

begin
  HTTPRouter.RegisterRoute('*path',rmPOST,@MyRouteCallBack,true);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteCallBack,HTTPRouter[0].ClassType);
  E:=THTTPRouteCallBack(HTTPRouter[0]);
  AssertTrue('Route event correct',Pointer(E.CallBack)=Pointer(@MyRouteCallBack));
  AssertEquals('Route class not default',true,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddRouteObject;

Var
  E : THTTPRouteObject;

begin
  HTTPRouter.RegisterRoute('*path',TMyObjectHandler);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteObject,HTTPRouter[0].ClassType);
  E:=THTTPRouteObject(HTTPRouter[0]);
  AssertEquals('Route event correct',TMyObjectHandler,E.ObjectCLass);
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmAll=E.Method);
end;

procedure TTestHTTPRoute.TestAddRouteObjectMethod;

Var
  E : THTTPRouteObject;

begin
  HTTPRouter.RegisterRoute('*path',rmPost,TMyObjectHandler);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteObject,HTTPRouter[0].ClassType);
  E:=THTTPRouteObject(HTTPRouter[0]);
  AssertEquals('Route event correct',TMyObjectHandler,E.ObjectCLass);
  AssertEquals('Route class not default',False,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestAddRouteObjectDefault;
Var
  E : THTTPRouteObject;

begin
  HTTPRouter.RegisterRoute('*path',rmPost,TMyObjectHandler,True);
  AssertEquals('1 route registered.',1,HTTPRouter.RouteCount);
  AssertEquals('Route class correct',THTTPRouteObject,HTTPRouter[0].ClassType);
  E:=THTTPRouteObject(HTTPRouter[0]);
  AssertEquals('Route event correct',TMyObjectHandler,E.ObjectCLass);
  AssertEquals('Route class not default',True,E.Default);
  AssertEquals('Route URL pattern','*path/',E.URLPattern);
  AssertTrue('Correct method',rmPost=E.Method);
end;

procedure TTestHTTPRoute.TestFindRouteStatic;

Var
  R,F : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path3',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
end;

procedure TTestHTTPRoute.TestFindRouteStaticNoMatch;

Var
  F : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path3',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path4',rmPOST,RouteParams,MM);
  AssertNull('Found no route',F);
  AssertEquals('No route mismatch',False,MM);
end;

procedure TTestHTTPRoute.TestGetRouteStatic;

Var
  R,F : THTTPRoute;

begin
  HTTPRouter.RegisterRoute('/path1',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path3',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.GetHTTPRoute('/path2',rmPOST,RouteParams);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
end;

procedure TTestHTTPRoute.DoGetRoute;

begin
  HTTPRouter.GetHTTPRoute(FGetRoutePath,FGetRouteMethod,RouteParams);
end;

procedure TTestHTTPRoute.TestGetRouteStaticNoMatch;

begin
  HTTPRouter.RegisterRoute('/path1',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path3',rmAll,@MyRouteCallBack,False);
  FGetRoutePath:='/pathNNNN';
  FGetRouteMethod:=rmPost;
  AssertException('No route found raises exception',EHTTPRoute,@DoGetRoute,'Not found')
end;

procedure TTestHTTPRoute.TestGetRouteStaticNoMethodMatch;

begin
  HTTPRouter.RegisterRoute('/path1',rmGet,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmGet,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path3',rmGet,@MyRouteCallBack,False);
  FGetRoutePath:='/path1';
  FGetRouteMethod:=rmPost;
  AssertException('No route found raises exception',EHTTPRoute,@DoGetRoute,'Method not allowed')
end;

procedure TTestHTTPRoute.TestFindRouteStatic2Paths;

Var
  R,F : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('/path2/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2/c',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/b',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
end;

procedure TTestHTTPRoute.TestFindRouteStatic2PathsNoMatch;

Var
  F : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/c',rmPOST,RouteParams,MM);
  AssertNull('No route',F);
  AssertEquals('No route mismatch',False,MM);
end;

procedure TTestHTTPRoute.TestFindRouteStaticMethodMismatch;
Var
  F : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2/b',rmGet,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/b',rmPOST,RouteParams,MM);
  AssertNull('No route',F);
  AssertEquals('No route mismatch',True,MM);
end;

procedure TTestHTTPRoute.TestFindRouteWildCard;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('/*',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/b',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('No route params',0,RouteParams.Count);
end;

procedure TTestHTTPRoute.TestFindRouteNamedWildCard;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('/*thepath',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/b',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('Route params',1,RouteParams.Count);
  AssertEquals('Wildcard path correctly registered','path2/b',RouteParams.Values['thepath']);
end;

procedure TTestHTTPRoute.TestFindRouteNamedWildCard2;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('/path2/*thepath',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/b',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('Route params',1,RouteParams.Count);
  AssertEquals('Wildcard path correctly registered','b',RouteParams.Values['thepath']);
end;

procedure TTestHTTPRoute.TestFindRouteWildCard3;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('*/c',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/c',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('No route params',0,RouteParams.Count);
end;

procedure TTestHTTPRoute.TestFindRouteWildCard3Named;
Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('*start/c',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/c',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('route params',1,RouteParams.Count);
  AssertEquals('Wildcard path correctly registered','path2',RouteParams.Values['start']);
end;

procedure TTestHTTPRoute.TestFindRouteParam;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute(':start/c',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/c',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('route params',1,RouteParams.Count);
  AssertEquals('Param path correctly registered','path2',RouteParams.Values['start']);
end;

procedure TTestHTTPRoute.TestFindRouteParam2;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute(':start/:end',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path2/c',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('route params',2,RouteParams.Count);
  AssertEquals('Param 1 correctly registered','path2',RouteParams.Values['start']);
  AssertEquals('Param 2 correctly registered','c',RouteParams.Values['end']);
end;

procedure TTestHTTPRoute.TestFindRouteWildcardParam;

Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('*start/:end',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path1/path2/c',rmPOST,RouteParams,MM);
  AssertNotNull('Found route',F);
  AssertSame('Correct route found',R,F);
  AssertEquals('No route mismatch',False,MM);
  AssertEquals('route params',2,RouteParams.Count);
  AssertEquals('Param 1 correctly registered','path1/path2',RouteParams.Values['start']);
  AssertEquals('Param 2 correctly registered','c',RouteParams.Values['end']);
end;

procedure TTestHTTPRoute.TestFindRouteWildcardParamNoMatch;
Var
  F,R : THTTPRoute;
  MM : Boolean;

begin
  HTTPRouter.RegisterRoute('/path1/b',rmAll,@MyRouteCallBack,False);
  HTTPRouter.RegisterRoute('/path2',rmAll,@MyRouteCallBack,False);
  R:=HTTPRouter.RegisterRoute('*start/:end',rmAll,@MyRouteCallBack,False);
  F:=HTTPRouter.FindHTTPRoute('/path1',rmPOST,RouteParams,MM);
  AssertNull('Found route',F);
end;

procedure TTestHTTPRoute.TestSetServiceClass;
begin
  THTTPRouter.SetServiceClass(TMyHTTPRouter);
  AssertEquals('Correct service class',TMyHTTPRouter,THTTPRouter.ServiceClass);
  AssertEquals('Correct service class used for singleton',TMyHTTPRouter,HTTPRouter.ClassType);
end;

procedure TTestHTTPRoute.DoRouteRequest;

begin
  HTTPRouter.RouteRequest(FakeRequest,FakeResponse);
end;

function TTestHTTPRoute.GetWebHandler: TWebhandler;

Var
  F: TFakeWebhandler;
begin
  if FWebhandler=Nil then
    begin
    F:=TFakeWebhandler.Create(Nil);
    F.FakeRequest:=Self.FakeRequest;
    F.FakeResponse:=Self.FakeResponse;
    FWebhandler:=F;
    end;
  Result:=FWebhandler;
end;

procedure TTestHTTPRoute.TestRouteRequestEvent;

begin
  HTTPRouter.RegisterRoute('*path',@MyRouteEvent);
  FakeRequest.PathInfo:='me';
  RouteParams.Values['path']:='me';
  HTTPRouter.BeforeRequest:=@DoBeforeRequest;
  HTTPRouter.AfterRequest:=@DoAfterRequest;
  DoRouteRequest;
  AssertEquals('MyRouteEvent called',1,FEventCalled);
  AssertEquals('Before request called once',1,FBeforeCalledCount);
  AssertEquals('After request called once',1,FAfterCalledCount);
end;

procedure TTestHTTPRoute.TestRouteRequestCallback;
begin
  HTTPRouter.RegisterRoute('*path',@MyRouteCallBack);
  FakeRequest.PathInfo:='me';
  HTTPRouter.BeforeRequest:=@DoBeforeRequest;
  HTTPRouter.AfterRequest:=@DoAfterRequest;
  DoRouteRequest;
  AssertEquals('MyRouteEvent called',1,CallBackCalled);
  AssertEquals('Before request called once',1,FBeforeCalledCount);
  AssertEquals('After request called once',1,FAfterCalledCount);
end;

procedure TTestHTTPRoute.TestRouteRequestInterface;
begin
  HTTPRouter.RegisterRoute('*path',InterfacedHandler);
  FakeRequest.PathInfo:='me';
  HTTPRouter.BeforeRequest:=@DoBeforeRequest;
  HTTPRouter.AfterRequest:=@DoAfterRequest;
  DoRouteRequest;
  AssertEquals('MyRouteEvent called',1,InterfacedHandler.CallCount);
  AssertEquals('Before request called once',1,FBeforeCalledCount);
  AssertEquals('After request called once',1,FAfterCalledCount);
end;

procedure TTestHTTPRoute.TestRouteRequestObject;
begin
  HTTPRouter.RegisterRoute('*path',TMyObjectHandler);
  FakeRequest.PathInfo:='me';
  HTTPRouter.BeforeRequest:=@DoBeforeRequest;
  HTTPRouter.AfterRequest:=@DoAfterRequest;
  DoRouteRequest;
  AssertEquals('TMyObjectHandler.handleRequest called',1,TMyObjectHandler.FCallCount);
  AssertEquals('Before request called once',1,FBeforeCalledCount);
  AssertEquals('After request called once',1,FAfterCalledCount);
end;

procedure TTestHTTPRoute.TestRouteRequestException;
begin
  FDoException:=true;
  HTTPRouter.RegisterRoute('*path',@MyRouteEvent);
  FakeRequest.PathInfo:='me';
  HTTPRouter.BeforeRequest:=@DoBeforeRequest;
  HTTPRouter.AfterRequest:=@DoAfterRequest;
  AssertException('Raise exception',EXception,@DoRouteRequest);
  AssertEquals('MyRouteEvent called',1,FEventCalled);
  AssertEquals('Before request called once',1,FBeforeCalledCount);
  AssertEquals('After request not called',0,FAfterCalledCount);
end;

procedure TTestHTTPRoute.TestRouteModule;
begin
  RegisterHTTPModule('my',TMyModule,True);
  // Should not be called, as the module registration takes precedence.
  HTTPRouter.RegisterRoute('/my/no',@MyRouteEvent);
  ModuleFactory.OnModuleRequest:=@DoModuleRoute;
  FakeRequest.PathInfo:='/my/no/';
  DoRouteRequest;
  AssertEquals('MyRouteEvent not called',0,FEventCalled);
  AssertEquals('Module route event called',1,FModuleCallCount);
  AssertSame('Module route event called with correct module',ModuleFactory.Modules[0],FModuleItem);
end;

procedure TTestHTTPRoute.TestRouteModuleAfterRoute;

begin
  HTTPRouter.RegisterRoute('/my/no',@MyRouteEvent);
  // Should not be called, as the event registration takes precedence.
  RegisterHTTPModule('my',TMyModule,True);
  ModuleFactory.OnModuleRequest:=@DoModuleRoute;
  FakeRequest.PathInfo:='/my/no/';
  DoRouteRequest;
  AssertEquals('MyRouteEvent not called',1,FEventCalled);
  AssertEquals('Module route event called',0,FModuleCallCount);
end;

procedure TTestHTTPRoute.TestRouteModuleAfterRoute2;
begin
  HTTPRouter.RegisterRoute('/my/no',@MyRouteEvent);
  RegisterHTTPModule('my',TMyModule,True);
  ModuleFactory.OnModuleRequest:=@DoModuleRoute;
  FakeRequest.PathInfo:='/my/ap/';
  DoRouteRequest;
  AssertEquals('MyRouteEvent not called',0,FEventCalled);
  AssertEquals('Module route event called',1,FModuleCallCount);
  AssertSame('Module route event called with correct module',ModuleFactory.Modules[0],FModuleItem);
end;

procedure TTestHTTPRoute.TestWebModuleHandlerLegacy;
begin
  WebHandler.LegacyRouting:=True;
  // will not be called because of legacy routing
  HTTPRouter.RegisterRoute('/my/no',@MyRouteEvent);
  RegisterHTTPModule('my',TMyModule,True);
  ModuleFactory.OnModuleRequest:=@DoModuleRoute;
  FakeRequest.PathInfo:='/my/no/';
  WebHandler.Run;
  AssertEquals('MyRouteEvent not called',0,FEventCalled);
  AssertEquals('Module handler called',1,TMyModule.FCallCount);
  AssertSame('Module handler request correct',FakeRequest,TMyModule.FCallRequest);
  AssertSame('Module handler response correct',FakeResponse,TMyModule.FCallResponse);
end;

procedure TTestHTTPRoute.TestWebModuleHandlerNew;

begin
  WebHandler.LegacyRouting:=False;
  // will not be called because of legacy routing
  HTTPRouter.RegisterRoute('/my/no',@MyRouteEvent);
  RegisterHTTPModule('my',TMyModule,True);
  ModuleFactory.OnModuleRequest:=@DoModuleRoute;
  FakeRequest.PathInfo:='/my/no/';
  WebHandler.Run;
  AssertEquals('MyRouteEvent not called',1,FEventCalled);
  AssertEquals('Module handler not called',0,TMyModule.FCallCount);
  AssertSame('Module handler request correct',Nil,TMyModule.FCallRequest);
  AssertSame('Module handler response correct',Nil,TMyModule.FCallResponse);
end;

procedure TTestHTTPRoute.MyRouteEvent(ARequest: TRequest; AResponse: TResponse);

Var
  I : integer;
  N,V : string;

begin
  FEventCalled:=1;
  for I:=0 to RouteParams.Count-1 do
    begin
    RouteParams.GetNameValue(I,N,V);
    AssertEquals('Have route parameter '+N,V,ARequest.RouteParams[N]);
    end;
  if FDoException then
    Raise Exception.Create('An error');
end;

procedure TTestHTTPRoute.MyRouteEvent2(ARequest: TRequest; AResponse: TResponse);
begin
  FEventCalled:=2;
end;

procedure TTestHTTPRoute.MyRouteEvent3(ARequest: TRequest; AResponse: TResponse);
begin
  FEventCalled:=3;
end;

procedure TTestHTTPRoute.SetUp;

begin
  // Resets all.
  THTTPRouter.SetServiceClass(THTTPRouter);
  FInterfacedHandler:=TMyInterfacedHandler.Create;
  FRouteParams:=TStringList.Create;
  FRequest:=TFakeRequest.Create;
  FResponse:=TFakeResponse.Create(FRequest);
  ModuleFactory.Clear;
  CallBackCalled:=0;
  FEventCalled:=0;
  TMyModule.FCallCount:=0;
  TMyModule.FCallRequest:=Nil;
  TMyModule.FCallResponse:=Nil;
end;

procedure TTestHTTPRoute.TearDown;

begin
  CallBackCalled:=0;
  FEventCalled:=0;
  FreeAndNil(FRouteParams);
  FreeAndNil(FInterfacedHandler);
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  THTTPRouter.SetServiceClass(Nil);
end;

procedure TTestHTTPRoute.DoAfterRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
begin
  AssertSame('Sender is router',HTTPRouter,Sender);
  AssertSame('Request is original request',FakeRequest,ARequest);
  AssertSame('Response is original response',FakeResponse,AResponse);
  Inc(FAfterCalledCount);
end;

procedure TTestHTTPRoute.DoBeforeRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
begin
  AssertSame('Sender is router',HTTPRouter,Sender);
  AssertSame('Request is original request',FakeRequest,ARequest);
  AssertSame('Response is original response',FakeResponse,AResponse);
  Inc(FBeforeCalledCount);
end;

procedure TTestHTTPRoute.DoModuleRoute(Sender: TModuleItem; ARequest: TRequest; AResponse: TResponse);
begin
  FModuleItem:=Sender;
  Inc(FModuleCallCount);
end;

initialization

  RegisterTest(TTestHTTPRoute);
end.

