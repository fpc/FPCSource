{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST HTTP BASIC authentication.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sqldbrestauth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, sqldbrestconst, sqldbrestio, httpdefs;

Type
  TAuthenticateEvent = procedure (Sender : TObject; aRequest : TRequest) of object;

  { TRestAuthenticator }

  TRestAuthenticator = Class(TComponent)
  private
    FAfterAuthenticate: TAuthenticateEvent;
    FBeforeAuthenticate: TAuthenticateEvent;
  Protected
    function DoAuthenticateRequest(io : TRestIO) : Boolean; virtual; abstract;
  Public
    Function AuthenticateRequest(io : TRestIO) : Boolean;
    Function NeedConnection : Boolean; virtual;
  Published
    Property BeforeAuthenticate : TAuthenticateEvent Read FBeforeAuthenticate Write FBeforeAuthenticate;
    Property AfterAuthenticate : TAuthenticateEvent Read FAfterAuthenticate Write FAfterAuthenticate;
  end;

  TBasicAuthenticationEvent = procedure (sender : TObject; Const aUserName,aPassword : UTF8String; Var allow : Boolean; Var UserID : UTF8String) of object;

  { TRestBasicAuthenticator }

  TRestBasicAuthenticator = Class(TRestAuthenticator)
  private
    FAuthConnection: TSQLConnection;
    FAuthenticationRealm: UTF8String;
    FAuthSQL: TStringList;
    FDefaultPassword: UTF8String;
    FDefaultUserID: UTF8String;
    FDefaultUserName: UTF8String;
    FOnBasicAuthentication: TBasicAuthenticationEvent;
    function GetAuthenticationRealm: UTF8String;
    function GetAuthSQL: TStrings;
    function IsNotDefaultRealm: Boolean;
    procedure SetAuthConnection(AValue: TSQLConnection);
    procedure SetAuthSQL(AValue: TStrings);
  Protected
    function HaveAuthSQL: Boolean;
    function AuthenticateUserUsingSQl(IO: TRestIO; const UN, PW: UTF8String; out UID: UTF8String): Boolean; virtual;
  Public
    Constructor Create(AOwner :TComponent); override;
    Destructor Destroy; override;
    class function ExtractUserNamePassword(Req: TRequest; out UN, PW: UTF8String): Boolean;
    class function ExtractUserName(Req: TRequest) : UTF8String;
    Function NeedConnection : Boolean; override;
    function DoAuthenticateRequest(IO : TRestIO) : Boolean; override;
  Published
    Property AuthConnection : TSQLConnection Read FAuthConnection Write SetAuthConnection;
    Property AuthenticateUserSQL : TStrings Read GetAuthSQL Write SetAuthSQL;
    Property DefaultUserName : UTF8String Read FDefaultUserName Write FDefaultUserName;
    Property DefaultPassword : UTF8String Read FDefaultPassword Write FDefaultPassword;
    Property DefaultUserID : UTF8String Read FDefaultUserID Write FDefaultUserID ;
    Property AuthenticationRealm : UTF8String Read GetAuthenticationRealm Write FAuthenticationRealm Stored IsNotDefaultRealm;
    Property OnBasicAuthentication : TBasicAuthenticationEvent Read FOnBasicAuthentication Write FOnBasicAuthentication;
  end;

implementation

uses strutils, base64;

{ TRestBasicAuthenticator }

function TRestBasicAuthenticator.GetAuthenticationRealm: UTF8String;
begin
  Result:=FAuthenticationRealm;
  if Result='' then
    Result:=DefaultAuthenticationRealm;
end;

function TRestBasicAuthenticator.GetAuthSQL: TStrings;
begin
  Result:=FAuthSQL;
end;

function TRestBasicAuthenticator.IsNotDefaultRealm: Boolean;
begin
  Result:=(GetAuthenticationRealm<>DefaultAuthenticationRealm);
end;

procedure TRestBasicAuthenticator.SetAuthConnection(AValue: TSQLConnection);
begin
  if FAuthConnection=AValue then Exit;
  If Assigned(FAuthConnection) then
    FAuthConnection.RemoveFreeNotification(Self);
  FAuthConnection:=AValue;
  If Assigned(FAuthConnection) then
    FAuthConnection.FreeNotification(Self);
end;

procedure TRestBasicAuthenticator.SetAuthSQL(AValue: TStrings);
begin
  if FAuthSQL=AValue then Exit;
  FAuthSQL.Assign(AValue);
end;

constructor TRestBasicAuthenticator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthSQL:=TStringList.Create;
end;

destructor TRestBasicAuthenticator.Destroy;
begin
  FreeAndNil(FAuthSQL);
  inherited Destroy;
end;

function TRestBasicAuthenticator.NeedConnection: Boolean;
begin
  Result:=HaveAuthSQL and (AuthConnection=Nil);
end;

function TRestBasicAuthenticator.HaveAuthSQL: Boolean;

begin
  Result:=(FAuthSQL.Count>0) and (Trim(FAuthSQL.Text)<>'');
end;

function TRestBasicAuthenticator.AuthenticateUserUsingSQl(IO: TRestIO;
  const UN, PW: UTF8String; out UID: UTF8String): Boolean;

Var
  Conn : TSQLConnection;
  Q : TSQLQuery;
  P : TParam;

begin
  Result:=HaveAuthSQL;
  if not Result then
     exit;
  Conn:=Self.AuthConnection;
  if Conn=Nil then
    Conn:=IO.Connection;
  Result:=Conn<>Nil;
  if not Result then
    exit;
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=Conn;
    if IO.Transaction<>nil then
      Q.Transaction:=IO.Transaction;
    Q.SQL:=FAuthSQL;
    P:=Q.Params.FindParam('UserName');
    if P<>Nil then
      P.AsString:=UN;
    P:=Q.Params.FindParam('Password');
    if P<>Nil then
      P.AsString:=PW;
    Q.UniDirectional:=True;
    Q.UsePrimaryKeyAsKey:=False;
    Q.Open;
    Result:=Not (Q.EOF and Q.BOF);
    If Result then
      UID:=Q.Fields[0].AsString;
  finally
    Q.Free;
  end;
end;

class function TRestBasicAuthenticator.ExtractUserNamePassword(Req: TRequest;
  out UN, PW: UTF8String): Boolean;

Var
  S,A : String;

begin
  S:=Req.Authorization;
  Result:=(S<>'');
  if not Result then
    begin
    UN:='';
    PW:='';
    end
  else
    begin
    A:=ExtractWord(1,S,[' ']);
    S:=ExtractWord(2,S,[' ']);
    if Not SameText(A,'BASIC') then
      Exit(False);
    S:=DecodeStringBase64(S);
    UN:=ExtractWord(1,S,[':']);
    PW:=ExtractWord(2,S,[':']);
    end;
end;

class function TRestBasicAuthenticator.ExtractUserName(Req: TRequest): UTF8String;

Var
  PW : UTF8String;

begin
  if not ExtractUserNamePassword(Req,Result,PW) then
    Result:='?';
end;

function TRestBasicAuthenticator.DoAuthenticateRequest(IO: TRestIO): Boolean;

Var
  UID,UN,PW : UTF8String;

begin
  Result:=False;
  UID:='';
  if ExtractUserNamePassword(IO.Request,UN,PW) then
    begin
    if (UN<>'') and (PW<>'') then
      If (DefaultUserName<>'') and (DefaultPassword<>'') then
        begin
        Result:=(UN=DefaultUserName) and (PW=DefaultPassword);
        If Result then
          begin
          UID:=DefaultUserID;
          If UID='' then
            UID:=DefaultUserName;
          end;
        end
      else
        UID:=UN;
    if Assigned(FOnBasicAuthentication) then
       FOnBasicAuthentication(Self,UN,PW,Result,UID);
    if not Result then
      Result:=AuthenticateUserUsingSQl(IO,UN,PW,UID);
    end;
  If Result then
    IO.UserID:=UID
  else
    begin
    IO.Response.Code:=IO.RestStatuses.GetStatusCode(rsUnauthorized);
    IO.Response.CodeText:=SUnauthorized;
    IO.Response.WWWAuthenticate:=Format('BASIC Realm: "%s"',[AuthenticationRealm]);
    end;
end;

{ TRestAuthenticator }

function TRestAuthenticator.AuthenticateRequest(io: TRestIO): Boolean;
begin
  If Assigned(FBeforeAuthenticate) then
    FBeforeAuthenticate(self,IO.Request);
  Result:=DoAuthenticateRequest(IO);
  If Assigned(FAfterAuthenticate) then
    FAfterAuthenticate(self,IO.Request);
end;

function TRestAuthenticator.NeedConnection: Boolean;
begin
  Result:=False;
end;


end.

