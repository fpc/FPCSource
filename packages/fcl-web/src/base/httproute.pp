{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017 by the Free Pascal development team

    HTTPRoute: HTTP request router

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{
  Note:
  The MatchPattern routine was taken from Brook Framework's router unit, by Silvio Clecio.
}

{$mode objfpc}
{$H+}

unit httproute;

interface

uses
  Classes, SysUtils, httpdefs;

Type
  EHTTPRoute = Class(EHTTP);

  // Forward definitions;

  THTTPRouter = Class;
  THTTPRouterClass = Class of THTTPRouter;
  // Some common HTTP methods.

  TRouteMethod = (rmUnknown,rmAll,rmGet,rmPost,rmPut,rmDelete,rmOptions,rmHead, rmTrace);

  { THTTPRoute }

  THTTPRoute = Class(TCollectionItem)
  private
    FDefault: Boolean;
    FMethod: TRouteMethod;
    FURLPattern: String;
    procedure SetURLPattern(AValue: String);
  Protected
    Procedure DoHandleRequest(ARequest : TRequest; AResponse : TResponse); virtual;
  Public
    Destructor Destroy; override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse);
    Function Matches(Const APattern : String; AMethod : TRouteMethod) : Boolean;
    Function MatchPattern(Const Path : String; L : TStrings) : Boolean;
    Function MatchMethod(Const AMethod : TRouteMethod) : Boolean;
  Published
    Property Default : Boolean Read FDefault Write FDefault;
    Property URLPattern : String Read FURLPattern Write SetURLPattern;
    Property Method : TRouteMethod Read FMethod Write FMethod;
  end;
  THTTPRouteClass = Class of THTTPRoute;

  { THTTPRouteList }

  THTTPRouteList = Class (TCollection)
  private
    function GetR(AIndex : Integer): THTTPRoute;
    procedure SetR(AIndex : Integer; AValue: THTTPRoute);
  Public
    Property Routes[AIndex : Integer] : THTTPRoute Read GetR Write SetR; default;
  end;

  TRouteCallBack = Procedure (ARequest: TRequest; AResponse: TResponse);

  { THTTPRouteCallback }

  THTTPRouteCallback = Class(THTTPRoute)
  private
    FCallBack: TRouteCallBack;
  Protected
    Procedure DoHandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  Public
    Property CallBack : TRouteCallBack Read FCallBack Write FCallback;
  end;

  TRouteCallBackEx = Procedure (AData : Pointer; ARequest: TRequest; AResponse: TResponse);

  { THTTPRouteCallbackex }

  THTTPRouteCallbackEx = Class(THTTPRoute)
  private
    FCallBack: TRouteCallBackex;
    FData: Pointer;
  Protected
    Procedure DoHandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  Public
    Property CallBack : TRouteCallBackex Read FCallBack Write FCallback;
    Property Data : Pointer Read FData Write FData;
  end;

  TRouteEvent = Procedure (ARequest: TRequest; AResponse: TResponse) of object;

  { THTTPRouteEvent }

  THTTPRouteEvent = Class(THTTPRoute)
  private
    FEvent: TRouteEvent;
  Protected
    Procedure DoHandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  Public
    Property Event : TRouteEvent Read FEvent Write FEvent;
  end;

{$INTERFACES CORBA}
  IRouteInterface = Interface ['{10115353-10BA-4B00-FDA5-80B69AC4CAD0}']
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse);
  end;

  { THTTPRouteInterface }

  THTTPRouteInterface = Class(THTTPRoute)
  private
    FIntf: IRouteInterface;
  Protected
    Procedure DoHandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  Public
    Property Intf : IRouteInterface Read FIntf Write FIntf;
  end;

  TRouteObject = Class(TObject,IRouteInterface)
  Public
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); virtual; abstract;
  end;
  TRouteObjectClass = Class of TRouteObject;

  { THTTPRouteObject }

  THTTPRouteObject = Class(THTTPRoute)
  private
    FClass: TRouteObjectClass;
  Protected
    Procedure DoHandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  Public
    Property ObjectCLass : TRouteObjectClass Read FClass Write FClass;
  end;

  THTTPRouteRequestEvent = Procedure (Sender : TObject; ARequest : TRequest; AResponse : TResponse) of object;

  { THTTPRouter }

  THTTPRouter = Class(TComponent)
  private
    FAfterRequest: THTTPRouteRequestEvent;
    FBeforeRequest: THTTPRouteRequestEvent;
    FRoutes : THTTPRouteList;
    function GetR(AIndex : Integer): THTTPRoute;
    Class Procedure DoneService;
    Class
      Var FService : THTTPRouter;
          FServiceClass : THTTPRouterClass;
    function GetRouteCount: Integer;
  Protected
    // Return an instance of given class with Pattern, Method, IsDefault filled in.
    function CreateHTTPRoute(AClass: THTTPRouteClass; const APattern: String; AMethod: TRouteMethod; IsDefault: Boolean ): THTTPRoute; virtual;
    // Override this if you want to use another collection class.
    Function CreateRouteList : THTTPRouteList; virtual;
    Procedure CheckDuplicate(APattern : String; AMethod : TRouteMethod; isDefault : Boolean);
    // Actually route request. Override this for customized behaviour.
    Procedure DoRouteRequest(ARequest : TRequest; AResponse : TResponse); virtual;
    // Extract route from request. This is PathInfo by default (sanitized);
    Function GetRequestPath(ARequest : TRequest) : String; virtual;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    // Delete given route by index.
    Procedure DeleteRoute(AIndex : Integer);
    // Delete given route by index.
    Procedure DeleteRouteByID(AID : Integer);
    // Delete given route by index. The route object will be freed.
    Procedure DeleteRoute(ARoute : THTTPRoute);
    // Sanitize route path. Strips of query parameters and makes sure it ends in /
    class function SanitizeRoute(const Path: String): String;
    // Global instance.
    Class Function Service : THTTPRouter;
    // Class for global instance when it is created;
    Class Function ServiceClass : THTTPRouterClass;
    // This will destroy the service
    Class Procedure SetServiceClass(AClass : THTTPRouterClass);
    // Convert string to HTTP Route method
    Class Function StringToRouteMethod(Const S : String) : TRouteMethod;
    // Register event based route
    Function RegisterRoute(Const APattern : String; AEvent: TRouteEvent; IsDefault : Boolean = False) : THTTPRoute;overload;
    Function RegisterRoute(Const APattern : String; AMethod : TRouteMethod; AEvent: TRouteEvent; IsDefault : Boolean = False): THTTPRoute;overload;
    // Register interface based route. Programmer is responsible for the lifetime of the interface.
    Function RegisterRoute(Const APattern : String; const AIntf: IRouteInterface; IsDefault : Boolean = False) : THTTPRoute; overload;
    Function RegisterRoute(Const APattern : String; AMethod : TRouteMethod; const AIntf: IRouteInterface; IsDefault : Boolean = False): THTTPRoute; overload;
    // Object class based route. The router is responsible for the lifetime of the object instance
    Function RegisterRoute(Const APattern : String; const AObjectClass: TRouteObjectClass; IsDefault : Boolean = False) : THTTPRoute; overload;
    Function RegisterRoute(Const APattern : String; AMethod : TRouteMethod; const AobjectClass: TRouteObjectClass; IsDefault : Boolean = False): THTTPRoute; overload;
    // Register callback based route
    Function RegisterRoute(Const APattern : String; AData : Pointer; ACallBack: TRouteCallBackex; IsDefault : Boolean = False) : THTTPRoute;overload;
    Function RegisterRoute(Const APattern : String; AData : Pointer; AMethod : TRouteMethod; ACallBack: TRouteCallBackEx; IsDefault : Boolean = False): THTTPRoute;overload;
    // Register callbackEx based route
    Function RegisterRoute(Const APattern : String; ACallBack: TRouteCallBack; IsDefault : Boolean = False) : THTTPRoute;overload;
    Function RegisterRoute(Const APattern : String; AMethod : TRouteMethod; ACallBack: TRouteCallBack; IsDefault : Boolean = False): THTTPRoute;overload;
    // Find route. Matches Path on the various patterns. If a pattern is found, then the method is tested.
    // Returns the route that matches the pattern and method.
    function FindHTTPRoute(const Path: String; AMethod: TRouteMethod; Params: TStrings; out MethodMismatch: Boolean): THTTPRoute;
    function GetHTTPRoute(const Path: String; AMethod: TRouteMethod; Params: TStrings): THTTPRoute;
    // Do actual routing. Exceptions raised will not be caught. Request must be initialized
    Procedure RouteRequest(ARequest : TRequest; AResponse : TResponse);
    // Indexed access to the registered routes.
    Property Routes [AIndex : Integer]  : THTTPRoute Read GetR; Default;
    // Number of registered routes.
    Property RouteCount : Integer Read GetRouteCount;
    // Called before the request is routed.
    Property BeforeRequest : THTTPRouteRequestEvent Read FBeforeRequest Write FBeforeRequest;
    // Called after the request is routed, if no exception was raised during or before the request.
    Property AfterRequest : THTTPRouteRequestEvent Read FAfterRequest Write FAfterRequest;
  end;

Function RouteMethodToString (R : TRouteMethod)  : String;
// Shortcut for THTTPRouter.Service;
Function HTTPRouter : THTTPRouter;

Const
  RouteMethodNames : Array[TRouteMethod] of String = ('','','GET','POST','PUT','DELETE','OPTIONS','HEAD','TRACE');

implementation

uses strutils, typinfo;

Resourcestring
  EDuplicateRoute = 'Duplicate route pattern: %s and method: %s';
  EDuplicateDefaultRoute = 'Duplicate default route registered with pattern: %s and method: %s';

function RouteMethodToString(R: TRouteMethod): String;

begin
  if R=rmUnknown then
    Result:=''
  else if R=rmAll then
    Result:='*'
  else
    Result:=GetEnumName(TypeInfo(TRouteMethod),Ord(R));
end;

function HTTPRouter: THTTPRouter;
begin
  Result:=THTTPRouter.Service;
end;

{ THTTPRouteCallback }

procedure THTTPRouteCallback.DoHandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  CallBack(ARequest, AResponse);
end;

{ THTTPRouteObject }

procedure THTTPRouteObject.DoHandleRequest(ARequest: TRequest;
  AResponse: TResponse);
Var
  O : TRouteObject;

begin
  O:=ObjectClass.Create;
  try
    O.HandleRequest(ARequest,AResponse);
  finally
    O.Free;
  end;
end;

{ THTTPRouter }

function THTTPRouter.GetR(AIndex : Integer): THTTPRoute;
begin
  Result:=FRoutes[AIndex]
end;

class procedure THTTPRouter.DoneService;
begin
  FreeAndNil(FService);
end;

function THTTPRouter.GetRouteCount: Integer;
begin
  Result:=FRoutes.Count;
end;

function THTTPRouter.CreateRouteList: THTTPRouteList;
begin
  Result:=THTTPRouteList.Create(THTTPRoute);
end;

procedure THTTPRouter.CheckDuplicate(APattern: String; AMethod: TRouteMethod;
  isDefault: Boolean);
Var
  I,DI : Integer;
  R : THTTPRoute;

begin
  DI:=-1;
  For I:=0 to FRoutes.Count-1 do
    begin
    R:=FRoutes[I];
    if R.Default then
      DI:=I;
    if R.Matches(APattern,AMethod) then
      Raise EHTTPRoute.CreateFmt(EDuplicateRoute,[APattern,RouteMethodToString(AMethod)]);
    end;
  if isDefault and (DI<>-1) then
    Raise EHTTPRoute.CreateFmt(EDuplicateDefaultRoute,[APattern,RouteMethodToString(AMethod)]);
end;

procedure THTTPRouter.DoRouteRequest(ARequest: TRequest; AResponse: TResponse);

Var
  APath : String;
  AMethod : TRouteMethod;
  R : THTTPRoute;
  L : TStrings;
  I : Integer;
  N,V : string;

begin
  APath:=GetRequestPath(ARequest);
  AMethod:=StringToRouteMethod(ARequest.Method);
  L:=TStringList.Create;
  try
    R:=GetHTTPRoute(APath,AMethod,L);
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,V);
      if (N<>'') then
        ARequest.RouteParams[N]:=V;
      end;
    R.HandleRequest(ARequest,AResponse);
  finally
    L.Free;
  end;
end;

function THTTPRouter.GetRequestPath(ARequest: TRequest): String;
begin
  Result:=SanitizeRoute(ARequest.PathInfo);
end;

constructor THTTPRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  froutes:=CreateRouteList;
end;

destructor THTTPRouter.Destroy;
begin
  FreeAndNil(FRoutes);
  inherited Destroy;
end;

procedure THTTPRouter.DeleteRoute(AIndex: Integer);
begin
  FRoutes.Delete(Aindex)
end;

procedure THTTPRouter.DeleteRouteByID(AID: Integer);
begin
  FRoutes.FindItemID(AID).Free;
end;

procedure THTTPRouter.DeleteRoute(ARoute: THTTPRoute);
begin
  ARoute.Free;
end;

class function THTTPRouter.Service: THTTPRouter;
begin
  if FService=Nil then
    FService:=ServiceClass.Create(Nil);
  Result:=FService;
end;

class function THTTPRouter.ServiceClass: THTTPRouterClass;
begin
  If FServiceClass=nil then
    FServiceClass:=THTTPRouter;
  Result:=FServiceClass;
end;

class procedure THTTPRouter.SetServiceClass(AClass: THTTPRouterClass);
begin
  if Assigned(FService) then
    FreeAndNil(FService);
  FServiceClass:=AClass;
end;

class function THTTPRouter.StringToRouteMethod(const S: String): TRouteMethod;


Var
  MN : String;

begin
  Result:=High(TRouteMethod);
  MN:=Uppercase(S);
  While (Result>=Low(TRouteMethod)) and (RouteMethodNames[Result]<>MN) do
    Result:=Pred(Result);
  if Result=rmAll then Result:=rmUnknown;
end;

function THTTPRouter.RegisterRoute(const APattern: String;AData : Pointer;
  ACallBack: TRouteCallBackEx; IsDefault: Boolean): THTTPRoute;
begin
  Result:= RegisterRoute(APattern,AData,rmAll,ACallBack,IsDefault);
end;

function THTTPRouter.RegisterRoute(const APattern: String;AData : Pointer;
  AMethod: TRouteMethod; ACallBack: TRouteCallBackEx; IsDefault: Boolean
  ): THTTPRoute;

begin
  Result:=CreateHTTPRoute(THTTPRouteCallbackex,APattern,AMethod,IsDefault);
  THTTPRouteCallbackex(Result).CallBack:=ACallBack;
  THTTPRouteCallbackex(Result).Data:=AData;
end;

function THTTPRouter.RegisterRoute(const APattern: String; ACallBack: TRouteCallBack; IsDefault: Boolean
  ): THTTPRoute;
begin
  Result:= RegisterRoute(APattern,rmAll,ACallBack,IsDefault);
end;

function THTTPRouter.RegisterRoute(const APattern: String; AMethod: TRouteMethod; ACallBack: TRouteCallBack;
  IsDefault: Boolean): THTTPRoute;
begin
  Result:=CreateHTTPRoute(THTTPRouteCallback,APattern,AMethod,IsDefault);
  THTTPRouteCallback(Result).CallBack:=ACallBack;
end;

function THTTPRouter.RegisterRoute(const APattern: String; AEvent: TRouteEvent;
  IsDefault: Boolean): THTTPRoute;
begin
  Result:= RegisterRoute(APattern,rmAll,AEvent,IsDefault);
end;

function THTTPRouter.RegisterRoute(const APattern: String;
  AMethod: TRouteMethod; AEvent: TRouteEvent; IsDefault: Boolean): THTTPRoute;

begin
  Result:=CreateHTTPRoute(THTTPRouteEvent,APattern,AMethod,IsDefault);
  THTTPRouteEvent(Result).Event:=AEvent;
end;

function THTTPRouter.RegisterRoute(const APattern: String;
  const AIntf: IRouteInterface; IsDefault: Boolean): THTTPRoute;
begin
  Result:=RegisterRoute(APattern,rmAll,AIntf,IsDefault);
end;

function THTTPRouter.CreateHTTPRoute(AClass : THTTPRouteClass; const APattern: String;AMethod: TRouteMethod; IsDefault: Boolean) : THTTPRoute;

begin
  CheckDuplicate(APattern,AMethod,isDefault);
  Result:=AClass.Create(FRoutes);
  With Result do
    begin
    URLPattern:=APattern;
    Default:=IsDefault;
    Method:=AMethod;
    end;
end;

function THTTPRouter.RegisterRoute(const APattern: String;AMethod: TRouteMethod; const AIntf: IRouteInterface; IsDefault: Boolean ): THTTPRoute;

begin
  Result:=CreateHTTPRoute(THTTPRouteInterface,APattern,AMethod,IsDefault);
  THTTPRouteInterface(Result).Intf:=AIntf;
end;

function THTTPRouter.RegisterRoute(const APattern: String; const AObjectClass: TRouteObjectClass; IsDefault: Boolean): THTTPRoute;
begin
  Result:=RegisterRoute(APattern,rmAll,AObjectClass,IsDefault);
end;

function THTTPRouter.RegisterRoute(const APattern: String; AMethod: TRouteMethod; const AobjectClass: TRouteObjectClass;
  IsDefault: Boolean): THTTPRoute;
begin
  Result:=CreateHTTPRoute(THTTPRouteObject,APattern,AMethod,IsDefault);
  THTTPRouteObject(Result).ObjectCLass:=AObjectClass;
end;

Class function THTTPRouter.SanitizeRoute(const Path: String) : String;

Var
  APathInfo : String;

begin
  APathInfo:=Path;
  Delete(APathInfo,Pos('?', APathInfo), MaxInt);
  Result:=IncludeHTTPPathDelimiter(APathInfo);
end;

function THTTPRouter.FindHTTPRoute(const Path: String; AMethod: TRouteMethod; Params : TStrings; Out MethodMismatch : Boolean): THTTPRoute;

Var
  I : Integer;
  APathInfo : String;

begin
  APathInfo:=SanitizeRoute(Path);
  MethodMisMatch:=False;
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<FRoutes.Count) do
    begin
    Result:=FRoutes[i];
    If Not Result.MatchPattern(APathInfo,Params) then
      Result:=Nil
    else if Not Result.MatchMethod(AMethod) then
      begin
      Result:=Nil;
      Params.Clear;
      MethodMisMatch:=True;
      end;
    Inc(I);
    end;
end;

function THTTPRouter.GetHTTPRoute(const Path: String; AMethod: TRouteMethod; Params : TStrings): THTTPRoute;

Const
  Status : Array[Boolean] of Integer = (404,405);
  StatusText :Array[Boolean] of String = ('Not found','Method not allowed');

Var
  MethodMisMatch : Boolean;
  E:EHTTPRoute;

begin
  Result:=FindHTTPRoute(Path,AMethod,Params,MethodMisMatch);
  if Not Assigned(Result) then
    begin
    E:=EHTTPRoute.Create(StatusText[MethodMisMatch]);
    E.StatusText:=StatusText[MethodMisMatch];
    E.StatusCode:=Status[MethodMisMatch];
    Raise E;
    end;
end;

procedure THTTPRouter.RouteRequest(ARequest: TRequest; AResponse: TResponse);
begin
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,ARequest,AResponse);
  DoRouteRequest(ARequest,AResponse);
  If Assigned(FAfterRequest) then
    FAfterRequest(Self,ARequest,AResponse);
end;

{ THTTPRouteInterface }

procedure THTTPRouteInterface.DoHandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  Intf.HandleRequest(ARequest, AResponse);
end;

{ THTTPRouteEvent }

procedure THTTPRouteEvent.DoHandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  Event(ARequest, AResponse);
end;

{ THTTPRouteList }

function THTTPRouteList.GetR(AIndex : Integer): THTTPRoute;
begin
  Result:=Items[AIndex] as THTTPRoute;
end;

procedure THTTPRouteList.SetR(AIndex : Integer; AValue: THTTPRoute);
begin
  Items[AIndex]:=AValue;
end;

{ THTTPRoute }

procedure THTTPRoute.SetURLPattern(AValue: String);

Var
  V : String;

begin
  V:=IncludeHTTPPathDelimiter(AValue);
  if (V<>'/') and (V[1]='/') then
    Delete(V,1,1);
  if FURLPattern=V then Exit;
  FURLPattern:=V;
end;

procedure THTTPRoute.DoHandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  // Do nothing
end;

destructor THTTPRoute.Destroy;
begin

  inherited Destroy;
end;

procedure THTTPRoute.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  DoHandleRequest(ARequest,AResponse);
end;

function THTTPRoute.Matches(const APattern: String; AMethod: TRouteMethod
  ): Boolean;
begin
  Result:=(CompareText(URLPattern,APattern)=0)
          and ((Method=rmAll) or (AMethod=Method))
end;

Function THTTPRoute.MatchPattern(Const Path : String; L : TStrings) : Boolean;

  Function StartsWith(C : Char; S : String): Boolean; 
  
  begin
    Result:=(Length(S)>0) and (S[1]=C);
  end;
  
  Function EndsWith(C : Char; S : String): Boolean; 
  
  Var
  L : Integer;
  
  begin
    L:=Length(S);
    Result:=(L>0) and (S[L]=C);
  end;
  

  procedure ExtractNextPathLevel(var ALeft: string;
    var ALvl: string; var ARight: string; const ADelim: Char = '/');
  var
    P: Integer;
  begin
    if (ALvl<>ADelim) then
      begin
      ALeft:=ALeft+ALvl;
      if StartsWith(ADelim,ARight) then
        begin
        ALeft:=ALeft+ADelim;
        Delete(ARight,1,1);
        end;
      end;
    P:=Pos(ADelim,ARight);
    if P=0 then
      P:=Length(ARight)+1;
    ALvl:=Copy(ARight,1,P-1);
    ARight:=Copy(ARight,P,MaxInt);
  end;

  procedure ExtractPrevPathLevel(var ALeft: string;
    var ALvl: string; var ARight: string; const ADelim: Char = '/');
  var
    P,L: Integer;
  begin
    if (ALvl<>ADelim) then
      begin
      ARight:=ALvl+ARight;
      L:=Length(ALeft);
      if EndsWith(ADelim,ALeft) then
        begin
        ARight:=ADelim+ARight;
        Delete(ALeft,L,1);
        end;
      end;
    P:=RPos(ADelim,ALeft);
    ALvl:=Copy(ALeft,P+1,MaxInt);
    ALeft:=Copy(ALeft,1,P);
  end;

var
  APathInfo : String;
  APattern : String;
  VLeftPat, VRightPat, VLeftVal, VRightVal, VVal, VPat, VName: string;

begin
  Result:= False;
  if (URLPattern='') then
     Exit; // Maybe empty pattern should match any path?
  APathInfo:=Path;
  APattern:=URLPattern;
  Delete(APattern, Pos('?', APattern), MaxInt);
  Delete(APathInfo, Pos('?', APathInfo), MaxInt);
  if StartsWith('/',APattern) then
    Delete(APattern,1,1);
  if StartsWith('/',APathInfo) then
    Delete(APathInfo,1,1);
  VLeftPat := '';
  VLeftVal := '';
  VPat := '/'; // init value is '/', not ''
  VVal := '/'; // init value is '/', not ''
  VRightPat := APattern;
  VRightVal := APathInfo;
  repeat
    // Extract next part
    ExtractNextPathLevel(VLeftPat, VPat, VRightPat);
    ExtractNextPathLevel(VLeftVal, VVal, VRightVal);
    if StartsWith(':',VPat) then
      begin
      L.Values[Copy(VPat,2,Maxint)]:=VVal;
      end
    else
      if StartsWith('*',VPat) then
        begin
        // *path
        VName := Copy(VPat, 2, MaxInt);
        VLeftPat := VRightPat;
        VLeftVal := VVal + VRightVal;
        VPat := '/'; // init value is '/', not ''
        VVal := '/'; // init value is '/', not ''
        VRightPat := '';
        VRightVal := '';
        // if AutoAddSlash ...
        if EndsWith('/',VLeftPat) and not EndsWith('/',VLeftVal) then
          Delete(VLeftPat, Length(VLeftPat), 1);
        repeat
          // Extract backwards
          ExtractPrevPathLevel(VLeftPat, VPat, VRightPat);
          ExtractPrevPathLevel(VLeftVal, VVal, VRightVal);
          if StartsWith(':', VPat) then
            begin
            // *path/:field
            L.Values[Copy(VPat,2,Maxint)]:=VVal;
            end
          else
            // *path/const
            if not ((VPat='') and (VLeftPat='')) and (VPat<>VVal) then
              Exit;
          // Check if we already done
          if (VLeftPat='') or (VLeftVal='') then
            begin
            if VLeftPat='' then
              begin
              if (VName<>'') then
                L.Values[VName]:=VLeftVal+VVal;
              Result:=True;
              end;
            Exit;
          end;
        until False;
        end
      else
        // const
        if (VPat <> VVal) then
          Exit;
    // Check if we already done
    if (VRightPat='') or (VRightVal='') then
      begin
      if (VRightPat='') and (VRightVal='') then
        Result:=True
      else if (VRightPat='/') then
        Result := True;
      Exit;
      end;
  until False;
end;

function THTTPRoute.MatchMethod(const AMethod: TRouteMethod): Boolean;
begin
  Result:=(Method=rmAll) or (Method=AMethod);
end;

{ THTTPRouteCallbackex }

procedure THTTPRouteCallbackEx.DoHandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  CallBack(Data,ARequest, AResponse);
end;

finalization
  THTTPRouter.DoneService;
end.

