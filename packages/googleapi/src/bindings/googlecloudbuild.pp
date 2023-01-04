unit googlecloudbuild;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TStatus = Class;
  TBuildOperationMetadata = Class;
  TSource = Class;
  TOperation = Class;
  TBuiltImage = Class;
  TStorageSource = Class;
  TResults = Class;
  TBuild = Class;
  TCancelBuildRequest = Class;
  TListOperationsResponse = Class;
  TBuildStep = Class;
  TListBuildsResponse = Class;
  TStatusArray = Array of TStatus;
  TBuildOperationMetadataArray = Array of TBuildOperationMetadata;
  TSourceArray = Array of TSource;
  TOperationArray = Array of TOperation;
  TBuiltImageArray = Array of TBuiltImage;
  TStorageSourceArray = Array of TStorageSource;
  TResultsArray = Array of TResults;
  TBuildArray = Array of TBuild;
  TCancelBuildRequestArray = Array of TCancelBuildRequest;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TBuildStepArray = Array of TBuildStep;
  TListBuildsResponseArray = Array of TListBuildsResponse;
  //Anonymous types, using auto-generated names
  TStatusTypedetailsItem = Class;
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TResultsTypeimagesArray = Array of TBuiltImage;
  TBuildTypestepsArray = Array of TBuildStep;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TListBuildsResponseTypebuildsArray = Array of TBuild;
  
  { --------------------------------------------------------------------
    TStatusTypedetailsItem
    --------------------------------------------------------------------}
  
  TStatusTypedetailsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStatusTypedetailsItemClass = Class of TStatusTypedetailsItem;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fdetails : TStatusTypedetailsArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property details : TStatusTypedetailsArray Index 8 Read Fdetails Write Setdetails;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TBuildOperationMetadata
    --------------------------------------------------------------------}
  
  TBuildOperationMetadata = Class(TGoogleBaseObject)
  Private
    Fbuild : TBuild;
  Protected
    //Property setters
    Procedure Setbuild(AIndex : Integer; const AValue : TBuild); virtual;
  Public
  Published
    Property build : TBuild Index 0 Read Fbuild Write Setbuild;
  end;
  TBuildOperationMetadataClass = Class of TBuildOperationMetadata;
  
  { --------------------------------------------------------------------
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
  Private
    FstorageSource : TStorageSource;
  Protected
    //Property setters
    Procedure SetstorageSource(AIndex : Integer; const AValue : TStorageSource); virtual;
  Public
  Published
    Property storageSource : TStorageSource Index 0 Read FstorageSource Write SetstorageSource;
  end;
  TSourceClass = Class of TSource;
  
  { --------------------------------------------------------------------
    TOperationTypemetadata
    --------------------------------------------------------------------}
  
  TOperationTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTypemetadataClass = Class of TOperationTypemetadata;
  
  { --------------------------------------------------------------------
    TOperationTyperesponse
    --------------------------------------------------------------------}
  
  TOperationTyperesponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTyperesponseClass = Class of TOperationTyperesponse;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    Ferror : TStatus;
    Fdone : boolean;
    Fmetadata : TOperationTypemetadata;
    Fresponse : TOperationTyperesponse;
    Fname : String;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property error : TStatus Index 0 Read Ferror Write Seterror;
    Property done : boolean Index 8 Read Fdone Write Setdone;
    Property metadata : TOperationTypemetadata Index 16 Read Fmetadata Write Setmetadata;
    Property response : TOperationTyperesponse Index 24 Read Fresponse Write Setresponse;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TBuiltImage
    --------------------------------------------------------------------}
  
  TBuiltImage = Class(TGoogleBaseObject)
  Private
    Fdigest : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setdigest(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property digest : String Index 0 Read Fdigest Write Setdigest;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TBuiltImageClass = Class of TBuiltImage;
  
  { --------------------------------------------------------------------
    TStorageSource
    --------------------------------------------------------------------}
  
  TStorageSource = Class(TGoogleBaseObject)
  Private
    Fbucket : String;
    Fgeneration : String;
    F_object : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setbucket(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgeneration(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_object(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property bucket : String Index 0 Read Fbucket Write Setbucket;
    Property generation : String Index 8 Read Fgeneration Write Setgeneration;
    Property _object : String Index 16 Read F_object Write Set_object;
  end;
  TStorageSourceClass = Class of TStorageSource;
  
  { --------------------------------------------------------------------
    TResults
    --------------------------------------------------------------------}
  
  TResults = Class(TGoogleBaseObject)
  Private
    Fimages : TResultsTypeimagesArray;
  Protected
    //Property setters
    Procedure Setimages(AIndex : Integer; const AValue : TResultsTypeimagesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property images : TResultsTypeimagesArray Index 0 Read Fimages Write Setimages;
  end;
  TResultsClass = Class of TResults;
  
  { --------------------------------------------------------------------
    TBuild
    --------------------------------------------------------------------}
  
  TBuild = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fresults : TResults;
    Fstatus : String;
    FfinishTime : String;
    Ftimeout : String;
    Fsteps : TBuildTypestepsArray;
    Fsource : TSource;
    FcreateTime : String;
    FstatusDetail : String;
    Fimages : TStringArray;
    FstartTime : String;
    FlogsBucket : String;
    FprojectId : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresults(AIndex : Integer; const AValue : TResults); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfinishTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Settimeout(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; const AValue : TBuildTypestepsArray); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusDetail(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimages(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlogsBucket(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property results : TResults Index 8 Read Fresults Write Setresults;
    Property status : String Index 16 Read Fstatus Write Setstatus;
    Property finishTime : String Index 24 Read FfinishTime Write SetfinishTime;
    Property timeout : String Index 32 Read Ftimeout Write Settimeout;
    Property steps : TBuildTypestepsArray Index 40 Read Fsteps Write Setsteps;
    Property source : TSource Index 48 Read Fsource Write Setsource;
    Property createTime : String Index 56 Read FcreateTime Write SetcreateTime;
    Property statusDetail : String Index 64 Read FstatusDetail Write SetstatusDetail;
    Property images : TStringArray Index 72 Read Fimages Write Setimages;
    Property startTime : String Index 80 Read FstartTime Write SetstartTime;
    Property logsBucket : String Index 88 Read FlogsBucket Write SetlogsBucket;
    Property projectId : String Index 96 Read FprojectId Write SetprojectId;
  end;
  TBuildClass = Class of TBuild;
  
  { --------------------------------------------------------------------
    TCancelBuildRequest
    --------------------------------------------------------------------}
  
  TCancelBuildRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCancelBuildRequestClass = Class of TCancelBuildRequest;
  
  { --------------------------------------------------------------------
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Foperations : TListOperationsResponseTypeoperationsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property operations : TListOperationsResponseTypeoperationsArray Index 8 Read Foperations Write Setoperations;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TBuildStep
    --------------------------------------------------------------------}
  
  TBuildStep = Class(TGoogleBaseObject)
  Private
    Fargs : TStringArray;
    Fdir : String;
    Fname : String;
    Fenv : TStringArray;
  Protected
    //Property setters
    Procedure Setargs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setdir(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setenv(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property args : TStringArray Index 0 Read Fargs Write Setargs;
    Property dir : String Index 8 Read Fdir Write Setdir;
    Property name : String Index 16 Read Fname Write Setname;
    Property env : TStringArray Index 24 Read Fenv Write Setenv;
  end;
  TBuildStepClass = Class of TBuildStep;
  
  { --------------------------------------------------------------------
    TListBuildsResponse
    --------------------------------------------------------------------}
  
  TListBuildsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fbuilds : TListBuildsResponseTypebuildsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setbuilds(AIndex : Integer; const AValue : TListBuildsResponseTypebuildsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property builds : TListBuildsResponseTypebuildsArray Index 8 Read Fbuilds Write Setbuilds;
  end;
  TListBuildsResponseClass = Class of TListBuildsResponse;
  
  { --------------------------------------------------------------------
    TProjectsBuildsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsBuildsResource, method List
  
  TProjectsBuildsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsBuildsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(projectId: string; aBuild : TBuild) : TOperation;overload;
    Function Get(projectId: string; id: string) : TBuild;
    Function List(projectId: string; AQuery : string  = '') : TListBuildsResponse;
    Function List(projectId: string; AQuery : TProjectsBuildslistOptions) : TListBuildsResponse;
    Function Cancel(projectId: string; id: string; aCancelBuildRequest : TCancelBuildRequest) : TBuild;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FBuildsInstance : TProjectsBuildsResource;
    Function GetBuildsInstance : TProjectsBuildsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateBuildsResource(AOwner : TComponent) : TProjectsBuildsResource;virtual;overload;
    Function CreateBuildsResource : TProjectsBuildsResource;virtual;overload;
    Property BuildsResource : TProjectsBuildsResource Read GetBuildsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOperationsResource, method List
  
  TOperationsListOptions = Record
    pageSize : integer;
    filter : String;
    pageToken : String;
  end;
  
  TOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_name: string) : TOperation;
    Function List(_name: string; AQuery : string  = '') : TListOperationsResponse;
    Function List(_name: string; AQuery : TOperationslistOptions) : TListOperationsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudbuildAPI
    --------------------------------------------------------------------}
  
  TCloudbuildAPI = Class(TGoogleAPI)
  Private
    FProjectsBuildsInstance : TProjectsBuildsResource;
    FProjectsInstance : TProjectsResource;
    FOperationsInstance : TOperationsResource;
    Function GetProjectsBuildsInstance : TProjectsBuildsResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetOperationsInstance : TOperationsResource;virtual;
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
    Function CreateProjectsBuildsResource(AOwner : TComponent) : TProjectsBuildsResource;virtual;overload;
    Function CreateProjectsBuildsResource : TProjectsBuildsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TOperationsResource;virtual;overload;
    Function CreateOperationsResource : TOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsBuildsResource : TProjectsBuildsResource Read GetProjectsBuildsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property OperationsResource : TOperationsResource Read GetOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TStatusTypedetailsItem
  --------------------------------------------------------------------}


Class Function TStatusTypedetailsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStatus
  --------------------------------------------------------------------}


Procedure TStatus.Setcode(AIndex : Integer; const AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBuildOperationMetadata
  --------------------------------------------------------------------}


Procedure TBuildOperationMetadata.Setbuild(AIndex : Integer; const AValue : TBuild); 

begin
  If (Fbuild=AValue) then exit;
  Fbuild:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.SetstorageSource(AIndex : Integer; const AValue : TStorageSource); 

begin
  If (FstorageSource=AValue) then exit;
  FstorageSource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypemetadata
  --------------------------------------------------------------------}


Class Function TOperationTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperationTyperesponse
  --------------------------------------------------------------------}


Class Function TOperationTyperesponse.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBuiltImage
  --------------------------------------------------------------------}


Procedure TBuiltImage.Setdigest(AIndex : Integer; const AValue : String); 

begin
  If (Fdigest=AValue) then exit;
  Fdigest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuiltImage.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStorageSource
  --------------------------------------------------------------------}


Procedure TStorageSource.Setbucket(AIndex : Integer; const AValue : String); 

begin
  If (Fbucket=AValue) then exit;
  Fbucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStorageSource.Setgeneration(AIndex : Integer; const AValue : String); 

begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStorageSource.Set_object(AIndex : Integer; const AValue : String); 

begin
  If (F_object=AValue) then exit;
  F_object:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TStorageSource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_object' : Result:='object';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TResults
  --------------------------------------------------------------------}


Procedure TResults.Setimages(AIndex : Integer; const AValue : TResultsTypeimagesArray); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TResults.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'images' : SetLength(Fimages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBuild
  --------------------------------------------------------------------}


Procedure TBuild.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.Setresults(AIndex : Integer; const AValue : TResults); 

begin
  If (Fresults=AValue) then exit;
  Fresults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.SetfinishTime(AIndex : Integer; const AValue : String); 

begin
  If (FfinishTime=AValue) then exit;
  FfinishTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.Settimeout(AIndex : Integer; const AValue : String); 

begin
  If (Ftimeout=AValue) then exit;
  Ftimeout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.Setsteps(AIndex : Integer; const AValue : TBuildTypestepsArray); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.SetstatusDetail(AIndex : Integer; const AValue : String); 

begin
  If (FstatusDetail=AValue) then exit;
  FstatusDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.Setimages(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.SetlogsBucket(AIndex : Integer; const AValue : String); 

begin
  If (FlogsBucket=AValue) then exit;
  FlogsBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuild.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBuild.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'steps' : SetLength(Fsteps,ALength);
  'images' : SetLength(Fimages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCancelBuildRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOperationsResponse.Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListOperationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBuildStep
  --------------------------------------------------------------------}


Procedure TBuildStep.Setargs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fargs=AValue) then exit;
  Fargs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuildStep.Setdir(AIndex : Integer; const AValue : String); 

begin
  If (Fdir=AValue) then exit;
  Fdir:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuildStep.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBuildStep.Setenv(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fenv=AValue) then exit;
  Fenv:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBuildStep.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'args' : SetLength(Fargs,ALength);
  'env' : SetLength(Fenv,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListBuildsResponse
  --------------------------------------------------------------------}


Procedure TListBuildsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBuildsResponse.Setbuilds(AIndex : Integer; const AValue : TListBuildsResponseTypebuildsArray); 

begin
  If (Fbuilds=AValue) then exit;
  Fbuilds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListBuildsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'builds' : SetLength(Fbuilds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectsBuildsResource
  --------------------------------------------------------------------}


Class Function TProjectsBuildsResource.ResourceName : String;

begin
  Result:='builds';
end;

Class Function TProjectsBuildsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudbuildAPI;
end;

Function TProjectsBuildsResource.Create(projectId: string; aBuild : TBuild) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/builds';
  _Methodid   = 'cloudbuild.projects.builds.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBuild,TOperation) as TOperation;
end;

Function TProjectsBuildsResource.Get(projectId: string; id: string) : TBuild;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/builds/{id}';
  _Methodid   = 'cloudbuild.projects.builds.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBuild) as TBuild;
end;

Function TProjectsBuildsResource.List(projectId: string; AQuery : string = '') : TListBuildsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/builds';
  _Methodid   = 'cloudbuild.projects.builds.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListBuildsResponse) as TListBuildsResponse;
end;


Function TProjectsBuildsResource.List(projectId: string; AQuery : TProjectsBuildslistOptions) : TListBuildsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,_Q);
end;

Function TProjectsBuildsResource.Cancel(projectId: string; id: string; aCancelBuildRequest : TCancelBuildRequest) : TBuild;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/builds/{id}:cancel';
  _Methodid   = 'cloudbuild.projects.builds.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCancelBuildRequest,TBuild) as TBuild;
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
  Result:=TcloudbuildAPI;
end;



Function TProjectsResource.GetBuildsInstance : TProjectsBuildsResource;

begin
  if (FBuildsInstance=Nil) then
    FBuildsInstance:=CreateBuildsResource;
  Result:=FBuildsInstance;
end;

Function TProjectsResource.CreateBuildsResource : TProjectsBuildsResource;

begin
  Result:=CreateBuildsResource(Self);
end;


Function TProjectsResource.CreateBuildsResource(AOwner : TComponent) : TProjectsBuildsResource;

begin
  Result:=TProjectsBuildsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TOperationsResource
  --------------------------------------------------------------------}


Class Function TOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudbuildAPI;
end;

Function TOperationsResource.Get(_name: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'cloudbuild.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TOperationsResource.List(_name: string; AQuery : string = '') : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'cloudbuild.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListOperationsResponse) as TListOperationsResponse;
end;


Function TOperationsResource.List(_name: string; AQuery : TOperationslistOptions) : TListOperationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;



{ --------------------------------------------------------------------
  TCloudbuildAPI
  --------------------------------------------------------------------}

Class Function TCloudbuildAPI.APIName : String;

begin
  Result:='cloudbuild';
end;

Class Function TCloudbuildAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCloudbuildAPI.APIRevision : String;

begin
  Result:='20160523';
end;

Class Function TCloudbuildAPI.APIID : String;

begin
  Result:='cloudbuild:v1';
end;

Class Function TCloudbuildAPI.APITitle : String;

begin
  Result:='Google Cloud Container Builder API';
end;

Class Function TCloudbuildAPI.APIDescription : String;

begin
  Result:='Builds container images in the cloud.';
end;

Class Function TCloudbuildAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudbuildAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudbuildAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudbuildAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudbuildAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/container-builder/docs/';
end;

Class Function TCloudbuildAPI.APIrootUrl : string;

begin
  Result:='https://cloudbuild.googleapis.com/';
end;

Class Function TCloudbuildAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TCloudbuildAPI.APIbaseURL : String;

begin
  Result:='https://cloudbuild.googleapis.com/';
end;

Class Function TCloudbuildAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudbuildAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TCloudbuildAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudbuildAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TCloudbuildAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCloudbuildAPI.RegisterAPIResources;

begin
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TBuildOperationMetadata.RegisterObject;
  TSource.RegisterObject;
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TBuiltImage.RegisterObject;
  TStorageSource.RegisterObject;
  TResults.RegisterObject;
  TBuild.RegisterObject;
  TCancelBuildRequest.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TBuildStep.RegisterObject;
  TListBuildsResponse.RegisterObject;
end;


Function TCloudbuildAPI.GetProjectsBuildsInstance : TProjectsBuildsResource;

begin
  if (FProjectsBuildsInstance=Nil) then
    FProjectsBuildsInstance:=CreateProjectsBuildsResource;
  Result:=FProjectsBuildsInstance;
end;

Function TCloudbuildAPI.CreateProjectsBuildsResource : TProjectsBuildsResource;

begin
  Result:=CreateProjectsBuildsResource(Self);
end;


Function TCloudbuildAPI.CreateProjectsBuildsResource(AOwner : TComponent) : TProjectsBuildsResource;

begin
  Result:=TProjectsBuildsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCloudbuildAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TCloudbuildAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TCloudbuildAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCloudbuildAPI.GetOperationsInstance : TOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TCloudbuildAPI.CreateOperationsResource : TOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TCloudbuildAPI.CreateOperationsResource(AOwner : TComponent) : TOperationsResource;

begin
  Result:=TOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TCloudbuildAPI.RegisterAPI;
end.
