unit googlescript;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TExecutionRequest = Class;
  TOperation = Class;
  TStatus = Class;
  TExecutionError = Class;
  TScriptStackTraceElement = Class;
  TExecutionResponse = Class;
  TExecutionRequestArray = Array of TExecutionRequest;
  TOperationArray = Array of TOperation;
  TStatusArray = Array of TStatus;
  TExecutionErrorArray = Array of TExecutionError;
  TScriptStackTraceElementArray = Array of TScriptStackTraceElement;
  TExecutionResponseArray = Array of TExecutionResponse;
  //Anonymous types, using auto-generated names
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TStatusTypedetailsItem = Class;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TExecutionErrorTypescriptStackTraceElementsArray = Array of TScriptStackTraceElement;
  
  { --------------------------------------------------------------------
    TExecutionRequest
    --------------------------------------------------------------------}
  
  TExecutionRequest = Class(TGoogleBaseObject)
  Private
    F_function : String;
    Fparameters : TTJSONSchemaArray;
    FsessionState : String;
    FdevMode : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_function(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparameters(AIndex : Integer; const AValue : TTJSONSchemaArray); virtual;
    Procedure SetsessionState(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdevMode(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _function : String Index 0 Read F_function Write Set_function;
    Property parameters : TTJSONSchemaArray Index 8 Read Fparameters Write Setparameters;
    Property sessionState : String Index 16 Read FsessionState Write SetsessionState;
    Property devMode : boolean Index 24 Read FdevMode Write SetdevMode;
  end;
  TExecutionRequestClass = Class of TExecutionRequest;
  
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
    Fname : String;
    Fmetadata : TOperationTypemetadata;
    Fdone : boolean;
    Ferror : TStatus;
    Fresponse : TOperationTyperesponse;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property metadata : TOperationTypemetadata Index 8 Read Fmetadata Write Setmetadata;
    Property done : boolean Index 16 Read Fdone Write Setdone;
    Property error : TStatus Index 24 Read Ferror Write Seterror;
    Property response : TOperationTyperesponse Index 32 Read Fresponse Write Setresponse;
  end;
  TOperationClass = Class of TOperation;
  
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
    Fmessage : String;
    Fdetails : TStatusTypedetailsArray;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property details : TStatusTypedetailsArray Index 16 Read Fdetails Write Setdetails;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TExecutionError
    --------------------------------------------------------------------}
  
  TExecutionError = Class(TGoogleBaseObject)
  Private
    FscriptStackTraceElements : TExecutionErrorTypescriptStackTraceElementsArray;
    FerrorMessage : String;
    FerrorType : String;
  Protected
    //Property setters
    Procedure SetscriptStackTraceElements(AIndex : Integer; const AValue : TExecutionErrorTypescriptStackTraceElementsArray); virtual;
    Procedure SeterrorMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorType(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property scriptStackTraceElements : TExecutionErrorTypescriptStackTraceElementsArray Index 0 Read FscriptStackTraceElements Write SetscriptStackTraceElements;
    Property errorMessage : String Index 8 Read FerrorMessage Write SeterrorMessage;
    Property errorType : String Index 16 Read FerrorType Write SeterrorType;
  end;
  TExecutionErrorClass = Class of TExecutionError;
  
  { --------------------------------------------------------------------
    TScriptStackTraceElement
    --------------------------------------------------------------------}
  
  TScriptStackTraceElement = Class(TGoogleBaseObject)
  Private
    F_function : String;
    FlineNumber : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_function(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlineNumber(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property _function : String Index 0 Read F_function Write Set_function;
    Property lineNumber : integer Index 8 Read FlineNumber Write SetlineNumber;
  end;
  TScriptStackTraceElementClass = Class of TScriptStackTraceElement;
  
  { --------------------------------------------------------------------
    TExecutionResponse
    --------------------------------------------------------------------}
  
  TExecutionResponse = Class(TGoogleBaseObject)
  Private
    Fresult : TJSONSchema;
  Protected
    //Property setters
    Procedure Setresult(AIndex : Integer; const AValue : TJSONSchema); virtual;
  Public
  Published
    Property result : TJSONSchema Index 0 Read Fresult Write Setresult;
  end;
  TExecutionResponseClass = Class of TExecutionResponse;
  
  { --------------------------------------------------------------------
    TScriptsResource
    --------------------------------------------------------------------}
  
  TScriptsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Run(scriptId: string; aExecutionRequest : TExecutionRequest) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TScriptAPI
    --------------------------------------------------------------------}
  
  TScriptAPI = Class(TGoogleAPI)
  Private
    FScriptsInstance : TScriptsResource;
    Function GetScriptsInstance : TScriptsResource;virtual;
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
    Function CreateScriptsResource(AOwner : TComponent) : TScriptsResource;virtual;overload;
    Function CreateScriptsResource : TScriptsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ScriptsResource : TScriptsResource Read GetScriptsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TExecutionRequest
  --------------------------------------------------------------------}


Procedure TExecutionRequest.Set_function(AIndex : Integer; const AValue : String); 

begin
  If (F_function=AValue) then exit;
  F_function:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecutionRequest.Setparameters(AIndex : Integer; const AValue : TTJSONSchemaArray); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecutionRequest.SetsessionState(AIndex : Integer; const AValue : String); 

begin
  If (FsessionState=AValue) then exit;
  FsessionState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecutionRequest.SetdevMode(AIndex : Integer; const AValue : boolean); 

begin
  If (FdevMode=AValue) then exit;
  FdevMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TExecutionRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_function' : Result:='function';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExecutionRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'parameters' : SetLength(Fparameters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;





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



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
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
  TExecutionError
  --------------------------------------------------------------------}


Procedure TExecutionError.SetscriptStackTraceElements(AIndex : Integer; const AValue : TExecutionErrorTypescriptStackTraceElementsArray); 

begin
  If (FscriptStackTraceElements=AValue) then exit;
  FscriptStackTraceElements:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecutionError.SeterrorMessage(AIndex : Integer; const AValue : String); 

begin
  If (FerrorMessage=AValue) then exit;
  FerrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExecutionError.SeterrorType(AIndex : Integer; const AValue : String); 

begin
  If (FerrorType=AValue) then exit;
  FerrorType:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExecutionError.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'scriptstacktraceelements' : SetLength(FscriptStackTraceElements,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TScriptStackTraceElement
  --------------------------------------------------------------------}


Procedure TScriptStackTraceElement.Set_function(AIndex : Integer; const AValue : String); 

begin
  If (F_function=AValue) then exit;
  F_function:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TScriptStackTraceElement.SetlineNumber(AIndex : Integer; const AValue : integer); 

begin
  If (FlineNumber=AValue) then exit;
  FlineNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TScriptStackTraceElement.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_function' : Result:='function';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TExecutionResponse
  --------------------------------------------------------------------}


Procedure TExecutionResponse.Setresult(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (Fresult=AValue) then exit;
  Fresult:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TScriptsResource
  --------------------------------------------------------------------}


Class Function TScriptsResource.ResourceName : String;

begin
  Result:='scripts';
end;

Class Function TScriptsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TscriptAPI;
end;

Function TScriptsResource.Run(scriptId: string; aExecutionRequest : TExecutionRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/scripts/{scriptId}:run';
  _Methodid   = 'script.scripts.run';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['scriptId',scriptId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExecutionRequest,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TScriptAPI
  --------------------------------------------------------------------}

Class Function TScriptAPI.APIName : String;

begin
  Result:='script';
end;

Class Function TScriptAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TScriptAPI.APIRevision : String;

begin
  Result:='20160426';
end;

Class Function TScriptAPI.APIID : String;

begin
  Result:='script:v1';
end;

Class Function TScriptAPI.APITitle : String;

begin
  Result:='Google Apps Script Execution API';
end;

Class Function TScriptAPI.APIDescription : String;

begin
  Result:='Executes Google Apps Script projects.';
end;

Class Function TScriptAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TScriptAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TScriptAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TScriptAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TScriptAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/apps-script/execution/rest/v1/scripts/run';
end;

Class Function TScriptAPI.APIrootUrl : string;

begin
  Result:='https://script.googleapis.com/';
end;

Class Function TScriptAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TScriptAPI.APIbaseURL : String;

begin
  Result:='https://script.googleapis.com/';
end;

Class Function TScriptAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TScriptAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TScriptAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TScriptAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,11);
  Result[0].Name:='https://mail.google.com/';
  Result[0].Description:='View and manage your mail';
  Result[1].Name:='https://www.google.com/calendar/feeds';
  Result[1].Description:='Manage your calendars';
  Result[2].Name:='https://www.google.com/m8/feeds';
  Result[2].Description:='Manage your contacts';
  Result[3].Name:='https://www.googleapis.com/auth/admin.directory.group';
  Result[3].Description:='View and manage the provisioning of groups on your domain';
  Result[4].Name:='https://www.googleapis.com/auth/admin.directory.user';
  Result[4].Description:='View and manage the provisioning of users on your domain';
  Result[5].Name:='https://www.googleapis.com/auth/drive';
  Result[5].Description:='View and manage the files in your Google Drive';
  Result[6].Name:='https://www.googleapis.com/auth/forms';
  Result[6].Description:='View and manage your forms in Google Drive';
  Result[7].Name:='https://www.googleapis.com/auth/forms.currentonly';
  Result[7].Description:='View and manage forms that this application has been installed in';
  Result[8].Name:='https://www.googleapis.com/auth/groups';
  Result[8].Description:='View and manage your Google Groups';
  Result[9].Name:='https://www.googleapis.com/auth/spreadsheets';
  Result[9].Description:='View and manage your spreadsheets in Google Drive';
  Result[10].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[10].Description:='View your email address';
  
end;

Class Function TScriptAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TScriptAPI.RegisterAPIResources;

begin
  TExecutionRequest.RegisterObject;
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TExecutionError.RegisterObject;
  TScriptStackTraceElement.RegisterObject;
  TExecutionResponse.RegisterObject;
end;


Function TScriptAPI.GetScriptsInstance : TScriptsResource;

begin
  if (FScriptsInstance=Nil) then
    FScriptsInstance:=CreateScriptsResource;
  Result:=FScriptsInstance;
end;

Function TScriptAPI.CreateScriptsResource : TScriptsResource;

begin
  Result:=CreateScriptsResource(Self);
end;


Function TScriptAPI.CreateScriptsResource(AOwner : TComponent) : TScriptsResource;

begin
  Result:=TScriptsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TScriptAPI.RegisterAPI;
end.
