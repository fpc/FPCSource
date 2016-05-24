unit googlefirebaserules;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TRelease = Class;
  TSource = Class;
  TSourcePosition = Class;
  TTestRulesetResponse = Class;
  TRuleset = Class;
  TListReleasesResponse = Class;
  TListRulesetsResponse = Class;
  TEmpty = Class;
  TFile = Class;
  TTestRulesetRequest = Class;
  TIssue = Class;
  TReleaseArray = Array of TRelease;
  TSourceArray = Array of TSource;
  TSourcePositionArray = Array of TSourcePosition;
  TTestRulesetResponseArray = Array of TTestRulesetResponse;
  TRulesetArray = Array of TRuleset;
  TListReleasesResponseArray = Array of TListReleasesResponse;
  TListRulesetsResponseArray = Array of TListRulesetsResponse;
  TEmptyArray = Array of TEmpty;
  TFileArray = Array of TFile;
  TTestRulesetRequestArray = Array of TTestRulesetRequest;
  TIssueArray = Array of TIssue;
  //Anonymous types, using auto-generated names
  TSourceTypefilesArray = Array of TFile;
  TTestRulesetResponseTypeissuesArray = Array of TIssue;
  TListReleasesResponseTypereleasesArray = Array of TRelease;
  TListRulesetsResponseTyperulesetsArray = Array of TRuleset;
  
  { --------------------------------------------------------------------
    TRelease
    --------------------------------------------------------------------}
  
  TRelease = Class(TGoogleBaseObject)
  Private
    FupdateTime : String;
    FcreateTime : String;
    Fname : String;
    FrulesetName : String;
  Protected
    //Property setters
    Procedure SetupdateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrulesetName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property updateTime : String Index 0 Read FupdateTime Write SetupdateTime;
    Property createTime : String Index 8 Read FcreateTime Write SetcreateTime;
    Property name : String Index 16 Read Fname Write Setname;
    Property rulesetName : String Index 24 Read FrulesetName Write SetrulesetName;
  end;
  TReleaseClass = Class of TRelease;
  
  { --------------------------------------------------------------------
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
  Private
    Ffiles : TSourceTypefilesArray;
  Protected
    //Property setters
    Procedure Setfiles(AIndex : Integer; const AValue : TSourceTypefilesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property files : TSourceTypefilesArray Index 0 Read Ffiles Write Setfiles;
  end;
  TSourceClass = Class of TSource;
  
  { --------------------------------------------------------------------
    TSourcePosition
    --------------------------------------------------------------------}
  
  TSourcePosition = Class(TGoogleBaseObject)
  Private
    FfileName : String;
    Fcolumn : integer;
    Fline : integer;
  Protected
    //Property setters
    Procedure SetfileName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcolumn(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setline(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property fileName : String Index 0 Read FfileName Write SetfileName;
    Property column : integer Index 8 Read Fcolumn Write Setcolumn;
    Property line : integer Index 16 Read Fline Write Setline;
  end;
  TSourcePositionClass = Class of TSourcePosition;
  
  { --------------------------------------------------------------------
    TTestRulesetResponse
    --------------------------------------------------------------------}
  
  TTestRulesetResponse = Class(TGoogleBaseObject)
  Private
    Fissues : TTestRulesetResponseTypeissuesArray;
  Protected
    //Property setters
    Procedure Setissues(AIndex : Integer; const AValue : TTestRulesetResponseTypeissuesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property issues : TTestRulesetResponseTypeissuesArray Index 0 Read Fissues Write Setissues;
  end;
  TTestRulesetResponseClass = Class of TTestRulesetResponse;
  
  { --------------------------------------------------------------------
    TRuleset
    --------------------------------------------------------------------}
  
  TRuleset = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
    FcreateTime : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
    Property createTime : String Index 8 Read FcreateTime Write SetcreateTime;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TRulesetClass = Class of TRuleset;
  
  { --------------------------------------------------------------------
    TListReleasesResponse
    --------------------------------------------------------------------}
  
  TListReleasesResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Freleases : TListReleasesResponseTypereleasesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreleases(AIndex : Integer; const AValue : TListReleasesResponseTypereleasesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property releases : TListReleasesResponseTypereleasesArray Index 8 Read Freleases Write Setreleases;
  end;
  TListReleasesResponseClass = Class of TListReleasesResponse;
  
  { --------------------------------------------------------------------
    TListRulesetsResponse
    --------------------------------------------------------------------}
  
  TListRulesetsResponse = Class(TGoogleBaseObject)
  Private
    Frulesets : TListRulesetsResponseTyperulesetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setrulesets(AIndex : Integer; const AValue : TListRulesetsResponseTyperulesetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property rulesets : TListRulesetsResponseTyperulesetsArray Index 0 Read Frulesets Write Setrulesets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListRulesetsResponseClass = Class of TListRulesetsResponse;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TFile
    --------------------------------------------------------------------}
  
  TFile = Class(TGoogleBaseObject)
  Private
    Fcontent : String;
    Fname : String;
    Ffingerprint : String;
  Protected
    //Property setters
    Procedure Setcontent(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property content : String Index 0 Read Fcontent Write Setcontent;
    Property name : String Index 8 Read Fname Write Setname;
    Property fingerprint : String Index 16 Read Ffingerprint Write Setfingerprint;
  end;
  TFileClass = Class of TFile;
  
  { --------------------------------------------------------------------
    TTestRulesetRequest
    --------------------------------------------------------------------}
  
  TTestRulesetRequest = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
  end;
  TTestRulesetRequestClass = Class of TTestRulesetRequest;
  
  { --------------------------------------------------------------------
    TIssue
    --------------------------------------------------------------------}
  
  TIssue = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fseverity : String;
    FsourcePosition : TSourcePosition;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourcePosition(AIndex : Integer; const AValue : TSourcePosition); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property severity : String Index 8 Read Fseverity Write Setseverity;
    Property sourcePosition : TSourcePosition Index 16 Read FsourcePosition Write SetsourcePosition;
  end;
  TIssueClass = Class of TIssue;
  
  { --------------------------------------------------------------------
    TProjectsRulesetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsRulesetsResource, method List
  
  TProjectsRulesetsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsRulesetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(_name: string; aRuleset : TRuleset) : TRuleset;overload;
    Function Get(_name: string) : TRuleset;
    Function List(_name: string; AQuery : string  = '') : TListRulesetsResponse;
    Function List(_name: string; AQuery : TProjectsRulesetslistOptions) : TListRulesetsResponse;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsReleasesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsReleasesResource, method List
  
  TProjectsReleasesListOptions = Record
    pageSize : integer;
    filter : String;
    pageToken : String;
  end;
  
  TProjectsReleasesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(_name: string; aRelease : TRelease) : TRelease;
    Function Create(_name: string; aRelease : TRelease) : TRelease;overload;
    Function Get(_name: string) : TRelease;
    Function List(_name: string; AQuery : string  = '') : TListReleasesResponse;
    Function List(_name: string; AQuery : TProjectsReleaseslistOptions) : TListReleasesResponse;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FRulesetsInstance : TProjectsRulesetsResource;
    FReleasesInstance : TProjectsReleasesResource;
    Function GetRulesetsInstance : TProjectsRulesetsResource;virtual;
    Function GetReleasesInstance : TProjectsReleasesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Test(_name: string; aTestRulesetRequest : TTestRulesetRequest) : TTestRulesetResponse;
    Function CreateRulesetsResource(AOwner : TComponent) : TProjectsRulesetsResource;virtual;overload;
    Function CreateRulesetsResource : TProjectsRulesetsResource;virtual;overload;
    Function CreateReleasesResource(AOwner : TComponent) : TProjectsReleasesResource;virtual;overload;
    Function CreateReleasesResource : TProjectsReleasesResource;virtual;overload;
    Property RulesetsResource : TProjectsRulesetsResource Read GetRulesetsInstance;
    Property ReleasesResource : TProjectsReleasesResource Read GetReleasesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TFirebaserulesAPI
    --------------------------------------------------------------------}
  
  TFirebaserulesAPI = Class(TGoogleAPI)
  Private
    FProjectsRulesetsInstance : TProjectsRulesetsResource;
    FProjectsReleasesInstance : TProjectsReleasesResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsRulesetsInstance : TProjectsRulesetsResource;virtual;
    Function GetProjectsReleasesInstance : TProjectsReleasesResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
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
    Function CreateProjectsRulesetsResource(AOwner : TComponent) : TProjectsRulesetsResource;virtual;overload;
    Function CreateProjectsRulesetsResource : TProjectsRulesetsResource;virtual;overload;
    Function CreateProjectsReleasesResource(AOwner : TComponent) : TProjectsReleasesResource;virtual;overload;
    Function CreateProjectsReleasesResource : TProjectsReleasesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsRulesetsResource : TProjectsRulesetsResource Read GetProjectsRulesetsInstance;
    Property ProjectsReleasesResource : TProjectsReleasesResource Read GetProjectsReleasesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TRelease
  --------------------------------------------------------------------}


Procedure TRelease.SetupdateTime(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTime=AValue) then exit;
  FupdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelease.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelease.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelease.SetrulesetName(AIndex : Integer; const AValue : String); 

begin
  If (FrulesetName=AValue) then exit;
  FrulesetName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.Setfiles(AIndex : Integer; const AValue : TSourceTypefilesArray); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSource.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'files' : SetLength(Ffiles,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSourcePosition
  --------------------------------------------------------------------}


Procedure TSourcePosition.SetfileName(AIndex : Integer; const AValue : String); 

begin
  If (FfileName=AValue) then exit;
  FfileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourcePosition.Setcolumn(AIndex : Integer; const AValue : integer); 

begin
  If (Fcolumn=AValue) then exit;
  Fcolumn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourcePosition.Setline(AIndex : Integer; const AValue : integer); 

begin
  If (Fline=AValue) then exit;
  Fline:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestRulesetResponse
  --------------------------------------------------------------------}


Procedure TTestRulesetResponse.Setissues(AIndex : Integer; const AValue : TTestRulesetResponseTypeissuesArray); 

begin
  If (Fissues=AValue) then exit;
  Fissues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestRulesetResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'issues' : SetLength(Fissues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRuleset
  --------------------------------------------------------------------}


Procedure TRuleset.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRuleset.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRuleset.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListReleasesResponse
  --------------------------------------------------------------------}


Procedure TListReleasesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListReleasesResponse.Setreleases(AIndex : Integer; const AValue : TListReleasesResponseTypereleasesArray); 

begin
  If (Freleases=AValue) then exit;
  Freleases:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListReleasesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'releases' : SetLength(Freleases,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListRulesetsResponse
  --------------------------------------------------------------------}


Procedure TListRulesetsResponse.Setrulesets(AIndex : Integer; const AValue : TListRulesetsResponseTyperulesetsArray); 

begin
  If (Frulesets=AValue) then exit;
  Frulesets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListRulesetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListRulesetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rulesets' : SetLength(Frulesets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFile
  --------------------------------------------------------------------}


Procedure TFile.Setcontent(AIndex : Integer; const AValue : String); 

begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFile.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestRulesetRequest
  --------------------------------------------------------------------}


Procedure TTestRulesetRequest.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIssue
  --------------------------------------------------------------------}


Procedure TIssue.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIssue.Setseverity(AIndex : Integer; const AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TIssue.SetsourcePosition(AIndex : Integer; const AValue : TSourcePosition); 

begin
  If (FsourcePosition=AValue) then exit;
  FsourcePosition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsRulesetsResource
  --------------------------------------------------------------------}


Class Function TProjectsRulesetsResource.ResourceName : String;

begin
  Result:='rulesets';
end;

Class Function TProjectsRulesetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfirebaserulesAPI;
end;

Function TProjectsRulesetsResource.Create(_name: string; aRuleset : TRuleset) : TRuleset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}/rulesets';
  _Methodid   = 'firebaserules.projects.rulesets.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRuleset,TRuleset) as TRuleset;
end;

Function TProjectsRulesetsResource.Get(_name: string) : TRuleset;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'firebaserules.projects.rulesets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRuleset) as TRuleset;
end;

Function TProjectsRulesetsResource.List(_name: string; AQuery : string = '') : TListRulesetsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}/rulesets';
  _Methodid   = 'firebaserules.projects.rulesets.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListRulesetsResponse) as TListRulesetsResponse;
end;


Function TProjectsRulesetsResource.List(_name: string; AQuery : TProjectsRulesetslistOptions) : TListRulesetsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsRulesetsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+name}';
  _Methodid   = 'firebaserules.projects.rulesets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsReleasesResource
  --------------------------------------------------------------------}


Class Function TProjectsReleasesResource.ResourceName : String;

begin
  Result:='releases';
end;

Class Function TProjectsReleasesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfirebaserulesAPI;
end;

Function TProjectsReleasesResource.Update(_name: string; aRelease : TRelease) : TRelease;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/{+name}';
  _Methodid   = 'firebaserules.projects.releases.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRelease,TRelease) as TRelease;
end;

Function TProjectsReleasesResource.Create(_name: string; aRelease : TRelease) : TRelease;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}/releases';
  _Methodid   = 'firebaserules.projects.releases.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRelease,TRelease) as TRelease;
end;

Function TProjectsReleasesResource.Get(_name: string) : TRelease;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'firebaserules.projects.releases.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRelease) as TRelease;
end;

Function TProjectsReleasesResource.List(_name: string; AQuery : string = '') : TListReleasesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}/releases';
  _Methodid   = 'firebaserules.projects.releases.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListReleasesResponse) as TListReleasesResponse;
end;


Function TProjectsReleasesResource.List(_name: string; AQuery : TProjectsReleaseslistOptions) : TListReleasesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsReleasesResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+name}';
  _Methodid   = 'firebaserules.projects.releases.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TfirebaserulesAPI;
end;

Function TProjectsResource.Test(_name: string; aTestRulesetRequest : TTestRulesetRequest) : TTestRulesetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:test';
  _Methodid   = 'firebaserules.projects.test';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestRulesetRequest,TTestRulesetResponse) as TTestRulesetResponse;
end;



Function TProjectsResource.GetRulesetsInstance : TProjectsRulesetsResource;

begin
  if (FRulesetsInstance=Nil) then
    FRulesetsInstance:=CreateRulesetsResource;
  Result:=FRulesetsInstance;
end;

Function TProjectsResource.CreateRulesetsResource : TProjectsRulesetsResource;

begin
  Result:=CreateRulesetsResource(Self);
end;


Function TProjectsResource.CreateRulesetsResource(AOwner : TComponent) : TProjectsRulesetsResource;

begin
  Result:=TProjectsRulesetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetReleasesInstance : TProjectsReleasesResource;

begin
  if (FReleasesInstance=Nil) then
    FReleasesInstance:=CreateReleasesResource;
  Result:=FReleasesInstance;
end;

Function TProjectsResource.CreateReleasesResource : TProjectsReleasesResource;

begin
  Result:=CreateReleasesResource(Self);
end;


Function TProjectsResource.CreateReleasesResource(AOwner : TComponent) : TProjectsReleasesResource;

begin
  Result:=TProjectsReleasesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TFirebaserulesAPI
  --------------------------------------------------------------------}

Class Function TFirebaserulesAPI.APIName : String;

begin
  Result:='firebaserules';
end;

Class Function TFirebaserulesAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TFirebaserulesAPI.APIRevision : String;

begin
  Result:='20160517';
end;

Class Function TFirebaserulesAPI.APIID : String;

begin
  Result:='firebaserules:v1';
end;

Class Function TFirebaserulesAPI.APITitle : String;

begin
  Result:='Firebase Rules API';
end;

Class Function TFirebaserulesAPI.APIDescription : String;

begin
  Result:='Creates and manages rules that determine when a Firebase Rules-enabled service should permit a request.';
end;

Class Function TFirebaserulesAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TFirebaserulesAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TFirebaserulesAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TFirebaserulesAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TFirebaserulesAPI.APIdocumentationLink : String;

begin
  Result:='https://firebase.google.com/docs/storage/security/start';
end;

Class Function TFirebaserulesAPI.APIrootUrl : string;

begin
  Result:='https://firebaserules.googleapis.com/';
end;

Class Function TFirebaserulesAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TFirebaserulesAPI.APIbaseURL : String;

begin
  Result:='https://firebaserules.googleapis.com/';
end;

Class Function TFirebaserulesAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TFirebaserulesAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TFirebaserulesAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TFirebaserulesAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TFirebaserulesAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TFirebaserulesAPI.RegisterAPIResources;

begin
  TRelease.RegisterObject;
  TSource.RegisterObject;
  TSourcePosition.RegisterObject;
  TTestRulesetResponse.RegisterObject;
  TRuleset.RegisterObject;
  TListReleasesResponse.RegisterObject;
  TListRulesetsResponse.RegisterObject;
  TEmpty.RegisterObject;
  TFile.RegisterObject;
  TTestRulesetRequest.RegisterObject;
  TIssue.RegisterObject;
end;


Function TFirebaserulesAPI.GetProjectsRulesetsInstance : TProjectsRulesetsResource;

begin
  if (FProjectsRulesetsInstance=Nil) then
    FProjectsRulesetsInstance:=CreateProjectsRulesetsResource;
  Result:=FProjectsRulesetsInstance;
end;

Function TFirebaserulesAPI.CreateProjectsRulesetsResource : TProjectsRulesetsResource;

begin
  Result:=CreateProjectsRulesetsResource(Self);
end;


Function TFirebaserulesAPI.CreateProjectsRulesetsResource(AOwner : TComponent) : TProjectsRulesetsResource;

begin
  Result:=TProjectsRulesetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TFirebaserulesAPI.GetProjectsReleasesInstance : TProjectsReleasesResource;

begin
  if (FProjectsReleasesInstance=Nil) then
    FProjectsReleasesInstance:=CreateProjectsReleasesResource;
  Result:=FProjectsReleasesInstance;
end;

Function TFirebaserulesAPI.CreateProjectsReleasesResource : TProjectsReleasesResource;

begin
  Result:=CreateProjectsReleasesResource(Self);
end;


Function TFirebaserulesAPI.CreateProjectsReleasesResource(AOwner : TComponent) : TProjectsReleasesResource;

begin
  Result:=TProjectsReleasesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TFirebaserulesAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TFirebaserulesAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TFirebaserulesAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TFirebaserulesAPI.RegisterAPI;
end.
