unit googleadexchangebuyer2;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TClientUser = Class;
  TClientUserInvitation = Class;
  TListClientUserInvitationsResponse = Class;
  TListClientUsersResponse = Class;
  TClient = Class;
  TListClientsResponse = Class;
  TClientUserArray = Array of TClientUser;
  TClientUserInvitationArray = Array of TClientUserInvitation;
  TListClientUserInvitationsResponseArray = Array of TListClientUserInvitationsResponse;
  TListClientUsersResponseArray = Array of TListClientUsersResponse;
  TClientArray = Array of TClient;
  TListClientsResponseArray = Array of TListClientsResponse;
  //Anonymous types, using auto-generated names
  TListClientUserInvitationsResponseTypeinvitationsArray = Array of TClientUserInvitation;
  TListClientUsersResponseTypeusersArray = Array of TClientUser;
  TListClientsResponseTypeclientsArray = Array of TClient;
  
  { --------------------------------------------------------------------
    TClientUser
    --------------------------------------------------------------------}
  
  TClientUser = Class(TGoogleBaseObject)
  Private
    Femail : String;
    FclientAccountId : String;
    Fstatus : String;
    FuserId : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientAccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property clientAccountId : String Index 8 Read FclientAccountId Write SetclientAccountId;
    Property status : String Index 16 Read Fstatus Write Setstatus;
    Property userId : String Index 24 Read FuserId Write SetuserId;
  end;
  TClientUserClass = Class of TClientUser;
  
  { --------------------------------------------------------------------
    TClientUserInvitation
    --------------------------------------------------------------------}
  
  TClientUserInvitation = Class(TGoogleBaseObject)
  Private
    Femail : String;
    FclientAccountId : String;
    FinvitationId : String;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientAccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinvitationId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property clientAccountId : String Index 8 Read FclientAccountId Write SetclientAccountId;
    Property invitationId : String Index 16 Read FinvitationId Write SetinvitationId;
  end;
  TClientUserInvitationClass = Class of TClientUserInvitation;
  
  { --------------------------------------------------------------------
    TListClientUserInvitationsResponse
    --------------------------------------------------------------------}
  
  TListClientUserInvitationsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Finvitations : TListClientUserInvitationsResponseTypeinvitationsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setinvitations(AIndex : Integer; const AValue : TListClientUserInvitationsResponseTypeinvitationsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property invitations : TListClientUserInvitationsResponseTypeinvitationsArray Index 8 Read Finvitations Write Setinvitations;
  end;
  TListClientUserInvitationsResponseClass = Class of TListClientUserInvitationsResponse;
  
  { --------------------------------------------------------------------
    TListClientUsersResponse
    --------------------------------------------------------------------}
  
  TListClientUsersResponse = Class(TGoogleBaseObject)
  Private
    Fusers : TListClientUsersResponseTypeusersArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setusers(AIndex : Integer; const AValue : TListClientUsersResponseTypeusersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property users : TListClientUsersResponseTypeusersArray Index 0 Read Fusers Write Setusers;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListClientUsersResponseClass = Class of TListClientUsersResponse;
  
  { --------------------------------------------------------------------
    TClient
    --------------------------------------------------------------------}
  
  TClient = Class(TGoogleBaseObject)
  Private
    FvisibleToSeller : boolean;
    Fstatus : String;
    FentityType : String;
    Frole : String;
    FclientName : String;
    FclientAccountId : String;
    FentityId : String;
    FentityName : String;
  Protected
    //Property setters
    Procedure SetvisibleToSeller(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetentityType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientAccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetentityId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetentityName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property visibleToSeller : boolean Index 0 Read FvisibleToSeller Write SetvisibleToSeller;
    Property status : String Index 8 Read Fstatus Write Setstatus;
    Property entityType : String Index 16 Read FentityType Write SetentityType;
    Property role : String Index 24 Read Frole Write Setrole;
    Property clientName : String Index 32 Read FclientName Write SetclientName;
    Property clientAccountId : String Index 40 Read FclientAccountId Write SetclientAccountId;
    Property entityId : String Index 48 Read FentityId Write SetentityId;
    Property entityName : String Index 56 Read FentityName Write SetentityName;
  end;
  TClientClass = Class of TClient;
  
  { --------------------------------------------------------------------
    TListClientsResponse
    --------------------------------------------------------------------}
  
  TListClientsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fclients : TListClientsResponseTypeclientsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setclients(AIndex : Integer; const AValue : TListClientsResponseTypeclientsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property clients : TListClientsResponseTypeclientsArray Index 8 Read Fclients Write Setclients;
  end;
  TListClientsResponseClass = Class of TListClientsResponse;
  
  { --------------------------------------------------------------------
    TAccountsClientsUsersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsClientsUsersResource, method List
  
  TAccountsClientsUsersListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TAccountsClientsUsersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(clientAccountId: string; userId: string; accountId: string; aClientUser : TClientUser) : TClientUser;
    Function Get(clientAccountId: string; userId: string; accountId: string) : TClientUser;
    Function List(clientAccountId: string; accountId: string; AQuery : string  = '') : TListClientUsersResponse;
    Function List(clientAccountId: string; accountId: string; AQuery : TAccountsClientsUserslistOptions) : TListClientUsersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsClientsInvitationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsClientsInvitationsResource, method List
  
  TAccountsClientsInvitationsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TAccountsClientsInvitationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(clientAccountId: string; accountId: string; aClientUserInvitation : TClientUserInvitation) : TClientUserInvitation;overload;
    Function Get(clientAccountId: string; invitationId: string; accountId: string) : TClientUserInvitation;
    Function List(clientAccountId: string; accountId: string; AQuery : string  = '') : TListClientUserInvitationsResponse;
    Function List(clientAccountId: string; accountId: string; AQuery : TAccountsClientsInvitationslistOptions) : TListClientUserInvitationsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsClientsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsClientsResource, method List
  
  TAccountsClientsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TAccountsClientsResource = Class(TGoogleResource)
  Private
    FUsersInstance : TAccountsClientsUsersResource;
    FInvitationsInstance : TAccountsClientsInvitationsResource;
    Function GetUsersInstance : TAccountsClientsUsersResource;virtual;
    Function GetInvitationsInstance : TAccountsClientsInvitationsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(clientAccountId: string; accountId: string; aClient : TClient) : TClient;
    Function Get(clientAccountId: string; accountId: string) : TClient;
    Function Create(accountId: string; aClient : TClient) : TClient;overload;
    Function List(accountId: string; AQuery : string  = '') : TListClientsResponse;
    Function List(accountId: string; AQuery : TAccountsClientslistOptions) : TListClientsResponse;
    Function CreateUsersResource(AOwner : TComponent) : TAccountsClientsUsersResource;virtual;overload;
    Function CreateUsersResource : TAccountsClientsUsersResource;virtual;overload;
    Function CreateInvitationsResource(AOwner : TComponent) : TAccountsClientsInvitationsResource;virtual;overload;
    Function CreateInvitationsResource : TAccountsClientsInvitationsResource;virtual;overload;
    Property UsersResource : TAccountsClientsUsersResource Read GetUsersInstance;
    Property InvitationsResource : TAccountsClientsInvitationsResource Read GetInvitationsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  TAccountsResource = Class(TGoogleResource)
  Private
    FClientsUsersInstance : TAccountsClientsUsersResource;
    FClientsInvitationsInstance : TAccountsClientsInvitationsResource;
    FClientsInstance : TAccountsClientsResource;
    Function GetClientsUsersInstance : TAccountsClientsUsersResource;virtual;
    Function GetClientsInvitationsInstance : TAccountsClientsInvitationsResource;virtual;
    Function GetClientsInstance : TAccountsClientsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateClientsUsersResource(AOwner : TComponent) : TAccountsClientsUsersResource;virtual;overload;
    Function CreateClientsUsersResource : TAccountsClientsUsersResource;virtual;overload;
    Function CreateClientsInvitationsResource(AOwner : TComponent) : TAccountsClientsInvitationsResource;virtual;overload;
    Function CreateClientsInvitationsResource : TAccountsClientsInvitationsResource;virtual;overload;
    Function CreateClientsResource(AOwner : TComponent) : TAccountsClientsResource;virtual;overload;
    Function CreateClientsResource : TAccountsClientsResource;virtual;overload;
    Property ClientsUsersResource : TAccountsClientsUsersResource Read GetClientsUsersInstance;
    Property ClientsInvitationsResource : TAccountsClientsInvitationsResource Read GetClientsInvitationsInstance;
    Property ClientsResource : TAccountsClientsResource Read GetClientsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAdexchangebuyer2API
    --------------------------------------------------------------------}
  
  TAdexchangebuyer2API = Class(TGoogleAPI)
  Private
    FAccountsClientsUsersInstance : TAccountsClientsUsersResource;
    FAccountsClientsInvitationsInstance : TAccountsClientsInvitationsResource;
    FAccountsClientsInstance : TAccountsClientsResource;
    FAccountsInstance : TAccountsResource;
    Function GetAccountsClientsUsersInstance : TAccountsClientsUsersResource;virtual;
    Function GetAccountsClientsInvitationsInstance : TAccountsClientsInvitationsResource;virtual;
    Function GetAccountsClientsInstance : TAccountsClientsResource;virtual;
    Function GetAccountsInstance : TAccountsResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateAccountsClientsUsersResource(AOwner : TComponent) : TAccountsClientsUsersResource;virtual;overload;
    Function CreateAccountsClientsUsersResource : TAccountsClientsUsersResource;virtual;overload;
    Function CreateAccountsClientsInvitationsResource(AOwner : TComponent) : TAccountsClientsInvitationsResource;virtual;overload;
    Function CreateAccountsClientsInvitationsResource : TAccountsClientsInvitationsResource;virtual;overload;
    Function CreateAccountsClientsResource(AOwner : TComponent) : TAccountsClientsResource;virtual;overload;
    Function CreateAccountsClientsResource : TAccountsClientsResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsClientsUsersResource : TAccountsClientsUsersResource Read GetAccountsClientsUsersInstance;
    Property AccountsClientsInvitationsResource : TAccountsClientsInvitationsResource Read GetAccountsClientsInvitationsInstance;
    Property AccountsClientsResource : TAccountsClientsResource Read GetAccountsClientsInstance;
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TClientUser
  --------------------------------------------------------------------}


Procedure TClientUser.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClientUser.SetclientAccountId(AIndex : Integer; const AValue : String); 

begin
  If (FclientAccountId=AValue) then exit;
  FclientAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClientUser.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClientUser.SetuserId(AIndex : Integer; const AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClientUserInvitation
  --------------------------------------------------------------------}


Procedure TClientUserInvitation.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClientUserInvitation.SetclientAccountId(AIndex : Integer; const AValue : String); 

begin
  If (FclientAccountId=AValue) then exit;
  FclientAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClientUserInvitation.SetinvitationId(AIndex : Integer; const AValue : String); 

begin
  If (FinvitationId=AValue) then exit;
  FinvitationId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListClientUserInvitationsResponse
  --------------------------------------------------------------------}


Procedure TListClientUserInvitationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListClientUserInvitationsResponse.Setinvitations(AIndex : Integer; const AValue : TListClientUserInvitationsResponseTypeinvitationsArray); 

begin
  If (Finvitations=AValue) then exit;
  Finvitations:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListClientUserInvitationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'invitations' : SetLength(Finvitations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListClientUsersResponse
  --------------------------------------------------------------------}


Procedure TListClientUsersResponse.Setusers(AIndex : Integer; const AValue : TListClientUsersResponseTypeusersArray); 

begin
  If (Fusers=AValue) then exit;
  Fusers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListClientUsersResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListClientUsersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'users' : SetLength(Fusers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TClient
  --------------------------------------------------------------------}


Procedure TClient.SetvisibleToSeller(AIndex : Integer; const AValue : boolean); 

begin
  If (FvisibleToSeller=AValue) then exit;
  FvisibleToSeller:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.SetentityType(AIndex : Integer; const AValue : String); 

begin
  If (FentityType=AValue) then exit;
  FentityType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.SetclientName(AIndex : Integer; const AValue : String); 

begin
  If (FclientName=AValue) then exit;
  FclientName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.SetclientAccountId(AIndex : Integer; const AValue : String); 

begin
  If (FclientAccountId=AValue) then exit;
  FclientAccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.SetentityId(AIndex : Integer; const AValue : String); 

begin
  If (FentityId=AValue) then exit;
  FentityId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClient.SetentityName(AIndex : Integer; const AValue : String); 

begin
  If (FentityName=AValue) then exit;
  FentityName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListClientsResponse
  --------------------------------------------------------------------}


Procedure TListClientsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListClientsResponse.Setclients(AIndex : Integer; const AValue : TListClientsResponseTypeclientsArray); 

begin
  If (Fclients=AValue) then exit;
  Fclients:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListClientsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'clients' : SetLength(Fclients,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsClientsUsersResource
  --------------------------------------------------------------------}


Class Function TAccountsClientsUsersResource.ResourceName : String;

begin
  Result:='users';
end;

Class Function TAccountsClientsUsersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=Tadexchangebuyer2API;
end;

Function TAccountsClientsUsersResource.Update(clientAccountId: string; userId: string; accountId: string; aClientUser : TClientUser) : TClientUser;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}/users/{userId}';
  _Methodid   = 'adexchangebuyer2.accounts.clients.users.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'userId',userId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aClientUser,TClientUser) as TClientUser;
end;

Function TAccountsClientsUsersResource.Get(clientAccountId: string; userId: string; accountId: string) : TClientUser;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}/users/{userId}';
  _Methodid   = 'adexchangebuyer2.accounts.clients.users.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'userId',userId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TClientUser) as TClientUser;
end;

Function TAccountsClientsUsersResource.List(clientAccountId: string; accountId: string; AQuery : string = '') : TListClientUsersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}/users';
  _Methodid   = 'adexchangebuyer2.accounts.clients.users.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListClientUsersResponse) as TListClientUsersResponse;
end;


Function TAccountsClientsUsersResource.List(clientAccountId: string; accountId: string; AQuery : TAccountsClientsUserslistOptions) : TListClientUsersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(clientAccountId,accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsClientsInvitationsResource
  --------------------------------------------------------------------}


Class Function TAccountsClientsInvitationsResource.ResourceName : String;

begin
  Result:='invitations';
end;

Class Function TAccountsClientsInvitationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=Tadexchangebuyer2API;
end;

Function TAccountsClientsInvitationsResource.Create(clientAccountId: string; accountId: string; aClientUserInvitation : TClientUserInvitation) : TClientUserInvitation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}/invitations';
  _Methodid   = 'adexchangebuyer2.accounts.clients.invitations.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aClientUserInvitation,TClientUserInvitation) as TClientUserInvitation;
end;

Function TAccountsClientsInvitationsResource.Get(clientAccountId: string; invitationId: string; accountId: string) : TClientUserInvitation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}/invitations/{invitationId}';
  _Methodid   = 'adexchangebuyer2.accounts.clients.invitations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'invitationId',invitationId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TClientUserInvitation) as TClientUserInvitation;
end;

Function TAccountsClientsInvitationsResource.List(clientAccountId: string; accountId: string; AQuery : string = '') : TListClientUserInvitationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}/invitations';
  _Methodid   = 'adexchangebuyer2.accounts.clients.invitations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListClientUserInvitationsResponse) as TListClientUserInvitationsResponse;
end;


Function TAccountsClientsInvitationsResource.List(clientAccountId: string; accountId: string; AQuery : TAccountsClientsInvitationslistOptions) : TListClientUserInvitationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(clientAccountId,accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsClientsResource
  --------------------------------------------------------------------}


Class Function TAccountsClientsResource.ResourceName : String;

begin
  Result:='clients';
end;

Class Function TAccountsClientsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=Tadexchangebuyer2API;
end;

Function TAccountsClientsResource.Update(clientAccountId: string; accountId: string; aClient : TClient) : TClient;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}';
  _Methodid   = 'adexchangebuyer2.accounts.clients.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aClient,TClient) as TClient;
end;

Function TAccountsClientsResource.Get(clientAccountId: string; accountId: string) : TClient;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/accounts/{accountId}/clients/{clientAccountId}';
  _Methodid   = 'adexchangebuyer2.accounts.clients.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clientAccountId',clientAccountId,'accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TClient) as TClient;
end;

Function TAccountsClientsResource.Create(accountId: string; aClient : TClient) : TClient;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2beta1/accounts/{accountId}/clients';
  _Methodid   = 'adexchangebuyer2.accounts.clients.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aClient,TClient) as TClient;
end;

Function TAccountsClientsResource.List(accountId: string; AQuery : string = '') : TListClientsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2beta1/accounts/{accountId}/clients';
  _Methodid   = 'adexchangebuyer2.accounts.clients.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListClientsResponse) as TListClientsResponse;
end;


Function TAccountsClientsResource.List(accountId: string; AQuery : TAccountsClientslistOptions) : TListClientsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,_Q);
end;



Function TAccountsClientsResource.GetUsersInstance : TAccountsClientsUsersResource;

begin
  if (FUsersInstance=Nil) then
    FUsersInstance:=CreateUsersResource;
  Result:=FUsersInstance;
end;

Function TAccountsClientsResource.CreateUsersResource : TAccountsClientsUsersResource;

begin
  Result:=CreateUsersResource(Self);
end;


Function TAccountsClientsResource.CreateUsersResource(AOwner : TComponent) : TAccountsClientsUsersResource;

begin
  Result:=TAccountsClientsUsersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsClientsResource.GetInvitationsInstance : TAccountsClientsInvitationsResource;

begin
  if (FInvitationsInstance=Nil) then
    FInvitationsInstance:=CreateInvitationsResource;
  Result:=FInvitationsInstance;
end;

Function TAccountsClientsResource.CreateInvitationsResource : TAccountsClientsInvitationsResource;

begin
  Result:=CreateInvitationsResource(Self);
end;


Function TAccountsClientsResource.CreateInvitationsResource(AOwner : TComponent) : TAccountsClientsInvitationsResource;

begin
  Result:=TAccountsClientsInvitationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsResource
  --------------------------------------------------------------------}


Class Function TAccountsResource.ResourceName : String;

begin
  Result:='accounts';
end;

Class Function TAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=Tadexchangebuyer2API;
end;



Function TAccountsResource.GetClientsUsersInstance : TAccountsClientsUsersResource;

begin
  if (FClientsUsersInstance=Nil) then
    FClientsUsersInstance:=CreateClientsUsersResource;
  Result:=FClientsUsersInstance;
end;

Function TAccountsResource.CreateClientsUsersResource : TAccountsClientsUsersResource;

begin
  Result:=CreateClientsUsersResource(Self);
end;


Function TAccountsResource.CreateClientsUsersResource(AOwner : TComponent) : TAccountsClientsUsersResource;

begin
  Result:=TAccountsClientsUsersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetClientsInvitationsInstance : TAccountsClientsInvitationsResource;

begin
  if (FClientsInvitationsInstance=Nil) then
    FClientsInvitationsInstance:=CreateClientsInvitationsResource;
  Result:=FClientsInvitationsInstance;
end;

Function TAccountsResource.CreateClientsInvitationsResource : TAccountsClientsInvitationsResource;

begin
  Result:=CreateClientsInvitationsResource(Self);
end;


Function TAccountsResource.CreateClientsInvitationsResource(AOwner : TComponent) : TAccountsClientsInvitationsResource;

begin
  Result:=TAccountsClientsInvitationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetClientsInstance : TAccountsClientsResource;

begin
  if (FClientsInstance=Nil) then
    FClientsInstance:=CreateClientsResource;
  Result:=FClientsInstance;
end;

Function TAccountsResource.CreateClientsResource : TAccountsClientsResource;

begin
  Result:=CreateClientsResource(Self);
end;


Function TAccountsResource.CreateClientsResource(AOwner : TComponent) : TAccountsClientsResource;

begin
  Result:=TAccountsClientsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAdexchangebuyer2API
  --------------------------------------------------------------------}

Class Function TAdexchangebuyer2API.APIName : String;

begin
  Result:='adexchangebuyer2';
end;

Class Function TAdexchangebuyer2API.APIVersion : String;

begin
  Result:='v2beta1';
end;

Class Function TAdexchangebuyer2API.APIRevision : String;

begin
  Result:='20160519';
end;

Class Function TAdexchangebuyer2API.APIID : String;

begin
  Result:='adexchangebuyer2:v2beta1';
end;

Class Function TAdexchangebuyer2API.APITitle : String;

begin
  Result:='Ad Exchange Buyer API II';
end;

Class Function TAdexchangebuyer2API.APIDescription : String;

begin
  Result:='Accesses the latest features for managing Ad Exchange accounts and Real-Time Bidding configurations.';
end;

Class Function TAdexchangebuyer2API.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAdexchangebuyer2API.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAdexchangebuyer2API.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAdexchangebuyer2API.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAdexchangebuyer2API.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/ad-exchange/buyer-rest/guides/client-access/';
end;

Class Function TAdexchangebuyer2API.APIrootUrl : string;

begin
  Result:='https://adexchangebuyer.googleapis.com/';
end;

Class Function TAdexchangebuyer2API.APIbasePath : string;

begin
  Result:='';
end;

Class Function TAdexchangebuyer2API.APIbaseURL : String;

begin
  Result:='https://adexchangebuyer.googleapis.com/';
end;

Class Function TAdexchangebuyer2API.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdexchangebuyer2API.APIservicePath : string;

begin
  Result:='';
end;

Class Function TAdexchangebuyer2API.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAdexchangebuyer2API.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/adexchange.buyer';
  Result[0].Description:='Manage your Ad Exchange buyer account configuration';
  
end;

Class Function TAdexchangebuyer2API.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAdexchangebuyer2API.RegisterAPIResources;

begin
  TClientUser.RegisterObject;
  TClientUserInvitation.RegisterObject;
  TListClientUserInvitationsResponse.RegisterObject;
  TListClientUsersResponse.RegisterObject;
  TClient.RegisterObject;
  TListClientsResponse.RegisterObject;
end;


Function TAdexchangebuyer2API.GetAccountsClientsUsersInstance : TAccountsClientsUsersResource;

begin
  if (FAccountsClientsUsersInstance=Nil) then
    FAccountsClientsUsersInstance:=CreateAccountsClientsUsersResource;
  Result:=FAccountsClientsUsersInstance;
end;

Function TAdexchangebuyer2API.CreateAccountsClientsUsersResource : TAccountsClientsUsersResource;

begin
  Result:=CreateAccountsClientsUsersResource(Self);
end;


Function TAdexchangebuyer2API.CreateAccountsClientsUsersResource(AOwner : TComponent) : TAccountsClientsUsersResource;

begin
  Result:=TAccountsClientsUsersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyer2API.GetAccountsClientsInvitationsInstance : TAccountsClientsInvitationsResource;

begin
  if (FAccountsClientsInvitationsInstance=Nil) then
    FAccountsClientsInvitationsInstance:=CreateAccountsClientsInvitationsResource;
  Result:=FAccountsClientsInvitationsInstance;
end;

Function TAdexchangebuyer2API.CreateAccountsClientsInvitationsResource : TAccountsClientsInvitationsResource;

begin
  Result:=CreateAccountsClientsInvitationsResource(Self);
end;


Function TAdexchangebuyer2API.CreateAccountsClientsInvitationsResource(AOwner : TComponent) : TAccountsClientsInvitationsResource;

begin
  Result:=TAccountsClientsInvitationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyer2API.GetAccountsClientsInstance : TAccountsClientsResource;

begin
  if (FAccountsClientsInstance=Nil) then
    FAccountsClientsInstance:=CreateAccountsClientsResource;
  Result:=FAccountsClientsInstance;
end;

Function TAdexchangebuyer2API.CreateAccountsClientsResource : TAccountsClientsResource;

begin
  Result:=CreateAccountsClientsResource(Self);
end;


Function TAdexchangebuyer2API.CreateAccountsClientsResource(AOwner : TComponent) : TAccountsClientsResource;

begin
  Result:=TAccountsClientsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangebuyer2API.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TAdexchangebuyer2API.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TAdexchangebuyer2API.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAdexchangebuyer2API.RegisterAPI;
end.
