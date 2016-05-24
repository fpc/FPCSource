unit googletagmanager;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = Class;
  TAccountAccess = Class;
  TCondition = Class;
  TContainer = Class;
  TContainerAccess = Class;
  TContainerVersion = Class;
  TContainerVersionHeader = Class;
  TCreateContainerVersionRequestVersionOptions = Class;
  TCreateContainerVersionResponse = Class;
  TEnvironment = Class;
  TFolder = Class;
  TFolderEntities = Class;
  TListAccountUsersResponse = Class;
  TListAccountsResponse = Class;
  TListContainerVersionsResponse = Class;
  TListContainersResponse = Class;
  TListEnvironmentsResponse = Class;
  TListFoldersResponse = Class;
  TListTagsResponse = Class;
  TListTriggersResponse = Class;
  TListVariablesResponse = Class;
  TMacro = Class;
  TParameter = Class;
  TPublishContainerVersionResponse = Class;
  TRule = Class;
  TSetupTag = Class;
  TTag = Class;
  TTeardownTag = Class;
  TTrigger = Class;
  TUserAccess = Class;
  TVariable = Class;
  TAccountArray = Array of TAccount;
  TAccountAccessArray = Array of TAccountAccess;
  TConditionArray = Array of TCondition;
  TContainerArray = Array of TContainer;
  TContainerAccessArray = Array of TContainerAccess;
  TContainerVersionArray = Array of TContainerVersion;
  TContainerVersionHeaderArray = Array of TContainerVersionHeader;
  TCreateContainerVersionRequestVersionOptionsArray = Array of TCreateContainerVersionRequestVersionOptions;
  TCreateContainerVersionResponseArray = Array of TCreateContainerVersionResponse;
  TEnvironmentArray = Array of TEnvironment;
  TFolderArray = Array of TFolder;
  TFolderEntitiesArray = Array of TFolderEntities;
  TListAccountUsersResponseArray = Array of TListAccountUsersResponse;
  TListAccountsResponseArray = Array of TListAccountsResponse;
  TListContainerVersionsResponseArray = Array of TListContainerVersionsResponse;
  TListContainersResponseArray = Array of TListContainersResponse;
  TListEnvironmentsResponseArray = Array of TListEnvironmentsResponse;
  TListFoldersResponseArray = Array of TListFoldersResponse;
  TListTagsResponseArray = Array of TListTagsResponse;
  TListTriggersResponseArray = Array of TListTriggersResponse;
  TListVariablesResponseArray = Array of TListVariablesResponse;
  TMacroArray = Array of TMacro;
  TParameterArray = Array of TParameter;
  TPublishContainerVersionResponseArray = Array of TPublishContainerVersionResponse;
  TRuleArray = Array of TRule;
  TSetupTagArray = Array of TSetupTag;
  TTagArray = Array of TTag;
  TTeardownTagArray = Array of TTeardownTag;
  TTriggerArray = Array of TTrigger;
  TUserAccessArray = Array of TUserAccess;
  TVariableArray = Array of TVariable;
  //Anonymous types, using auto-generated names
  TConditionTypeparameterArray = Array of TParameter;
  TContainerVersionTypefolderArray = Array of TFolder;
  TContainerVersionTypemacroArray = Array of TMacro;
  TContainerVersionTyperuleArray = Array of TRule;
  TContainerVersionTypetagArray = Array of TTag;
  TContainerVersionTypetriggerArray = Array of TTrigger;
  TContainerVersionTypevariableArray = Array of TVariable;
  TFolderEntitiesTypetagArray = Array of TTag;
  TFolderEntitiesTypetriggerArray = Array of TTrigger;
  TFolderEntitiesTypevariableArray = Array of TVariable;
  TListAccountUsersResponseTypeuserAccessArray = Array of TUserAccess;
  TListAccountsResponseTypeaccountsArray = Array of TAccount;
  TListContainerVersionsResponseTypecontainerVersionArray = Array of TContainerVersion;
  TListContainerVersionsResponseTypecontainerVersionHeaderArray = Array of TContainerVersionHeader;
  TListContainersResponseTypecontainersArray = Array of TContainer;
  TListEnvironmentsResponseTypeenvironmentsArray = Array of TEnvironment;
  TListFoldersResponseTypefoldersArray = Array of TFolder;
  TListTagsResponseTypetagsArray = Array of TTag;
  TListTriggersResponseTypetriggersArray = Array of TTrigger;
  TListVariablesResponseTypevariablesArray = Array of TVariable;
  TMacroTypeparameterArray = Array of TParameter;
  TParameterTypelistArray = Array of TParameter;
  TParameterTypemapArray = Array of TParameter;
  TRuleTypeconditionArray = Array of TCondition;
  TTagTypeparameterArray = Array of TParameter;
  TTagTypesetupTagArray = Array of TSetupTag;
  TTagTypeteardownTagArray = Array of TTeardownTag;
  TTriggerTypeautoEventFilterArray = Array of TCondition;
  TTriggerTypecustomEventFilterArray = Array of TCondition;
  TTriggerTypefilterArray = Array of TCondition;
  TUserAccessTypecontainerAccessArray = Array of TContainerAccess;
  TVariableTypeparameterArray = Array of TParameter;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Ffingerprint : String;
    Fname : String;
    FshareData : boolean;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshareData(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property fingerprint : String Index 8 Read Ffingerprint Write Setfingerprint;
    Property name : String Index 16 Read Fname Write Setname;
    Property shareData : boolean Index 24 Read FshareData Write SetshareData;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountAccess
    --------------------------------------------------------------------}
  
  TAccountAccess = Class(TGoogleBaseObject)
  Private
    Fpermission : TStringArray;
  Protected
    //Property setters
    Procedure Setpermission(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permission : TStringArray Index 0 Read Fpermission Write Setpermission;
  end;
  TAccountAccessClass = Class of TAccountAccess;
  
  { --------------------------------------------------------------------
    TCondition
    --------------------------------------------------------------------}
  
  TCondition = Class(TGoogleBaseObject)
  Private
    Fparameter : TConditionTypeparameterArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setparameter(AIndex : Integer; const AValue : TConditionTypeparameterArray); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property parameter : TConditionTypeparameterArray Index 0 Read Fparameter Write Setparameter;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TConditionClass = Class of TCondition;
  
  { --------------------------------------------------------------------
    TContainer
    --------------------------------------------------------------------}
  
  TContainer = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcontainerId : String;
    FdomainName : TStringArray;
    FenabledBuiltInVariable : TStringArray;
    Ffingerprint : String;
    Fname : String;
    Fnotes : String;
    FpublicId : String;
    FtimeZoneCountryId : String;
    FtimeZoneId : String;
    FusageContext : TStringArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdomainName(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetenabledBuiltInVariable(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpublicId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimeZoneCountryId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimeZoneId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetusageContext(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property containerId : String Index 8 Read FcontainerId Write SetcontainerId;
    Property domainName : TStringArray Index 16 Read FdomainName Write SetdomainName;
    Property enabledBuiltInVariable : TStringArray Index 24 Read FenabledBuiltInVariable Write SetenabledBuiltInVariable;
    Property fingerprint : String Index 32 Read Ffingerprint Write Setfingerprint;
    Property name : String Index 40 Read Fname Write Setname;
    Property notes : String Index 48 Read Fnotes Write Setnotes;
    Property publicId : String Index 56 Read FpublicId Write SetpublicId;
    Property timeZoneCountryId : String Index 64 Read FtimeZoneCountryId Write SettimeZoneCountryId;
    Property timeZoneId : String Index 72 Read FtimeZoneId Write SettimeZoneId;
    Property usageContext : TStringArray Index 80 Read FusageContext Write SetusageContext;
  end;
  TContainerClass = Class of TContainer;
  
  { --------------------------------------------------------------------
    TContainerAccess
    --------------------------------------------------------------------}
  
  TContainerAccess = Class(TGoogleBaseObject)
  Private
    FcontainerId : String;
    Fpermission : TStringArray;
  Protected
    //Property setters
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpermission(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property containerId : String Index 0 Read FcontainerId Write SetcontainerId;
    Property permission : TStringArray Index 8 Read Fpermission Write Setpermission;
  end;
  TContainerAccessClass = Class of TContainerAccess;
  
  { --------------------------------------------------------------------
    TContainerVersion
    --------------------------------------------------------------------}
  
  TContainerVersion = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fcontainer : TContainer;
    FcontainerId : String;
    FcontainerVersionId : String;
    Fdeleted : boolean;
    Ffingerprint : String;
    Ffolder : TContainerVersionTypefolderArray;
    Fmacro : TContainerVersionTypemacroArray;
    Fname : String;
    Fnotes : String;
    Frule : TContainerVersionTyperuleArray;
    Ftag : TContainerVersionTypetagArray;
    Ftrigger : TContainerVersionTypetriggerArray;
    Fvariable : TContainerVersionTypevariableArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcontainer(AIndex : Integer; const AValue : TContainer); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfolder(AIndex : Integer; const AValue : TContainerVersionTypefolderArray); virtual;
    Procedure Setmacro(AIndex : Integer; const AValue : TContainerVersionTypemacroArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrule(AIndex : Integer; const AValue : TContainerVersionTyperuleArray); virtual;
    Procedure Settag(AIndex : Integer; const AValue : TContainerVersionTypetagArray); virtual;
    Procedure Settrigger(AIndex : Integer; const AValue : TContainerVersionTypetriggerArray); virtual;
    Procedure Setvariable(AIndex : Integer; const AValue : TContainerVersionTypevariableArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property container : TContainer Index 8 Read Fcontainer Write Setcontainer;
    Property containerId : String Index 16 Read FcontainerId Write SetcontainerId;
    Property containerVersionId : String Index 24 Read FcontainerVersionId Write SetcontainerVersionId;
    Property deleted : boolean Index 32 Read Fdeleted Write Setdeleted;
    Property fingerprint : String Index 40 Read Ffingerprint Write Setfingerprint;
    Property folder : TContainerVersionTypefolderArray Index 48 Read Ffolder Write Setfolder;
    Property macro : TContainerVersionTypemacroArray Index 56 Read Fmacro Write Setmacro;
    Property name : String Index 64 Read Fname Write Setname;
    Property notes : String Index 72 Read Fnotes Write Setnotes;
    Property rule : TContainerVersionTyperuleArray Index 80 Read Frule Write Setrule;
    Property tag : TContainerVersionTypetagArray Index 88 Read Ftag Write Settag;
    Property trigger : TContainerVersionTypetriggerArray Index 96 Read Ftrigger Write Settrigger;
    Property variable : TContainerVersionTypevariableArray Index 104 Read Fvariable Write Setvariable;
  end;
  TContainerVersionClass = Class of TContainerVersion;
  
  { --------------------------------------------------------------------
    TContainerVersionHeader
    --------------------------------------------------------------------}
  
  TContainerVersionHeader = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcontainerId : String;
    FcontainerVersionId : String;
    Fdeleted : boolean;
    Fname : String;
    FnumMacros : String;
    FnumRules : String;
    FnumTags : String;
    FnumTriggers : String;
    FnumVariables : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumMacros(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumRules(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumTags(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumTriggers(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumVariables(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property containerId : String Index 8 Read FcontainerId Write SetcontainerId;
    Property containerVersionId : String Index 16 Read FcontainerVersionId Write SetcontainerVersionId;
    Property deleted : boolean Index 24 Read Fdeleted Write Setdeleted;
    Property name : String Index 32 Read Fname Write Setname;
    Property numMacros : String Index 40 Read FnumMacros Write SetnumMacros;
    Property numRules : String Index 48 Read FnumRules Write SetnumRules;
    Property numTags : String Index 56 Read FnumTags Write SetnumTags;
    Property numTriggers : String Index 64 Read FnumTriggers Write SetnumTriggers;
    Property numVariables : String Index 72 Read FnumVariables Write SetnumVariables;
  end;
  TContainerVersionHeaderClass = Class of TContainerVersionHeader;
  
  { --------------------------------------------------------------------
    TCreateContainerVersionRequestVersionOptions
    --------------------------------------------------------------------}
  
  TCreateContainerVersionRequestVersionOptions = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fnotes : String;
    FquickPreview : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetquickPreview(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property notes : String Index 8 Read Fnotes Write Setnotes;
    Property quickPreview : boolean Index 16 Read FquickPreview Write SetquickPreview;
  end;
  TCreateContainerVersionRequestVersionOptionsClass = Class of TCreateContainerVersionRequestVersionOptions;
  
  { --------------------------------------------------------------------
    TCreateContainerVersionResponse
    --------------------------------------------------------------------}
  
  TCreateContainerVersionResponse = Class(TGoogleBaseObject)
  Private
    FcompilerError : boolean;
    FcontainerVersion : TContainerVersion;
  Protected
    //Property setters
    Procedure SetcompilerError(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcontainerVersion(AIndex : Integer; const AValue : TContainerVersion); virtual;
  Public
  Published
    Property compilerError : boolean Index 0 Read FcompilerError Write SetcompilerError;
    Property containerVersion : TContainerVersion Index 8 Read FcontainerVersion Write SetcontainerVersion;
  end;
  TCreateContainerVersionResponseClass = Class of TCreateContainerVersionResponse;
  
  { --------------------------------------------------------------------
    TEnvironment
    --------------------------------------------------------------------}
  
  TEnvironment = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FauthorizationCode : String;
    FauthorizationTimestampMs : String;
    FcontainerId : String;
    FcontainerVersionId : String;
    Fdescription : String;
    FenableDebug : boolean;
    FenvironmentId : String;
    Ffingerprint : String;
    Fname : String;
    F_type : String;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetauthorizationCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetauthorizationTimestampMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetenableDebug(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetenvironmentId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property authorizationCode : String Index 8 Read FauthorizationCode Write SetauthorizationCode;
    Property authorizationTimestampMs : String Index 16 Read FauthorizationTimestampMs Write SetauthorizationTimestampMs;
    Property containerId : String Index 24 Read FcontainerId Write SetcontainerId;
    Property containerVersionId : String Index 32 Read FcontainerVersionId Write SetcontainerVersionId;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property enableDebug : boolean Index 48 Read FenableDebug Write SetenableDebug;
    Property environmentId : String Index 56 Read FenvironmentId Write SetenvironmentId;
    Property fingerprint : String Index 64 Read Ffingerprint Write Setfingerprint;
    Property name : String Index 72 Read Fname Write Setname;
    Property _type : String Index 80 Read F_type Write Set_type;
    Property url : String Index 88 Read Furl Write Seturl;
  end;
  TEnvironmentClass = Class of TEnvironment;
  
  { --------------------------------------------------------------------
    TFolder
    --------------------------------------------------------------------}
  
  TFolder = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcontainerId : String;
    Ffingerprint : String;
    FfolderId : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfolderId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property containerId : String Index 8 Read FcontainerId Write SetcontainerId;
    Property fingerprint : String Index 16 Read Ffingerprint Write Setfingerprint;
    Property folderId : String Index 24 Read FfolderId Write SetfolderId;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TFolderClass = Class of TFolder;
  
  { --------------------------------------------------------------------
    TFolderEntities
    --------------------------------------------------------------------}
  
  TFolderEntities = Class(TGoogleBaseObject)
  Private
    Ftag : TFolderEntitiesTypetagArray;
    Ftrigger : TFolderEntitiesTypetriggerArray;
    Fvariable : TFolderEntitiesTypevariableArray;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; const AValue : TFolderEntitiesTypetagArray); virtual;
    Procedure Settrigger(AIndex : Integer; const AValue : TFolderEntitiesTypetriggerArray); virtual;
    Procedure Setvariable(AIndex : Integer; const AValue : TFolderEntitiesTypevariableArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property tag : TFolderEntitiesTypetagArray Index 0 Read Ftag Write Settag;
    Property trigger : TFolderEntitiesTypetriggerArray Index 8 Read Ftrigger Write Settrigger;
    Property variable : TFolderEntitiesTypevariableArray Index 16 Read Fvariable Write Setvariable;
  end;
  TFolderEntitiesClass = Class of TFolderEntities;
  
  { --------------------------------------------------------------------
    TListAccountUsersResponse
    --------------------------------------------------------------------}
  
  TListAccountUsersResponse = Class(TGoogleBaseObject)
  Private
    FuserAccess : TListAccountUsersResponseTypeuserAccessArray;
  Protected
    //Property setters
    Procedure SetuserAccess(AIndex : Integer; const AValue : TListAccountUsersResponseTypeuserAccessArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property userAccess : TListAccountUsersResponseTypeuserAccessArray Index 0 Read FuserAccess Write SetuserAccess;
  end;
  TListAccountUsersResponseClass = Class of TListAccountUsersResponse;
  
  { --------------------------------------------------------------------
    TListAccountsResponse
    --------------------------------------------------------------------}
  
  TListAccountsResponse = Class(TGoogleBaseObject)
  Private
    Faccounts : TListAccountsResponseTypeaccountsArray;
  Protected
    //Property setters
    Procedure Setaccounts(AIndex : Integer; const AValue : TListAccountsResponseTypeaccountsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accounts : TListAccountsResponseTypeaccountsArray Index 0 Read Faccounts Write Setaccounts;
  end;
  TListAccountsResponseClass = Class of TListAccountsResponse;
  
  { --------------------------------------------------------------------
    TListContainerVersionsResponse
    --------------------------------------------------------------------}
  
  TListContainerVersionsResponse = Class(TGoogleBaseObject)
  Private
    FcontainerVersion : TListContainerVersionsResponseTypecontainerVersionArray;
    FcontainerVersionHeader : TListContainerVersionsResponseTypecontainerVersionHeaderArray;
  Protected
    //Property setters
    Procedure SetcontainerVersion(AIndex : Integer; const AValue : TListContainerVersionsResponseTypecontainerVersionArray); virtual;
    Procedure SetcontainerVersionHeader(AIndex : Integer; const AValue : TListContainerVersionsResponseTypecontainerVersionHeaderArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property containerVersion : TListContainerVersionsResponseTypecontainerVersionArray Index 0 Read FcontainerVersion Write SetcontainerVersion;
    Property containerVersionHeader : TListContainerVersionsResponseTypecontainerVersionHeaderArray Index 8 Read FcontainerVersionHeader Write SetcontainerVersionHeader;
  end;
  TListContainerVersionsResponseClass = Class of TListContainerVersionsResponse;
  
  { --------------------------------------------------------------------
    TListContainersResponse
    --------------------------------------------------------------------}
  
  TListContainersResponse = Class(TGoogleBaseObject)
  Private
    Fcontainers : TListContainersResponseTypecontainersArray;
  Protected
    //Property setters
    Procedure Setcontainers(AIndex : Integer; const AValue : TListContainersResponseTypecontainersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property containers : TListContainersResponseTypecontainersArray Index 0 Read Fcontainers Write Setcontainers;
  end;
  TListContainersResponseClass = Class of TListContainersResponse;
  
  { --------------------------------------------------------------------
    TListEnvironmentsResponse
    --------------------------------------------------------------------}
  
  TListEnvironmentsResponse = Class(TGoogleBaseObject)
  Private
    Fenvironments : TListEnvironmentsResponseTypeenvironmentsArray;
  Protected
    //Property setters
    Procedure Setenvironments(AIndex : Integer; const AValue : TListEnvironmentsResponseTypeenvironmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property environments : TListEnvironmentsResponseTypeenvironmentsArray Index 0 Read Fenvironments Write Setenvironments;
  end;
  TListEnvironmentsResponseClass = Class of TListEnvironmentsResponse;
  
  { --------------------------------------------------------------------
    TListFoldersResponse
    --------------------------------------------------------------------}
  
  TListFoldersResponse = Class(TGoogleBaseObject)
  Private
    Ffolders : TListFoldersResponseTypefoldersArray;
  Protected
    //Property setters
    Procedure Setfolders(AIndex : Integer; const AValue : TListFoldersResponseTypefoldersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property folders : TListFoldersResponseTypefoldersArray Index 0 Read Ffolders Write Setfolders;
  end;
  TListFoldersResponseClass = Class of TListFoldersResponse;
  
  { --------------------------------------------------------------------
    TListTagsResponse
    --------------------------------------------------------------------}
  
  TListTagsResponse = Class(TGoogleBaseObject)
  Private
    Ftags : TListTagsResponseTypetagsArray;
  Protected
    //Property setters
    Procedure Settags(AIndex : Integer; const AValue : TListTagsResponseTypetagsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property tags : TListTagsResponseTypetagsArray Index 0 Read Ftags Write Settags;
  end;
  TListTagsResponseClass = Class of TListTagsResponse;
  
  { --------------------------------------------------------------------
    TListTriggersResponse
    --------------------------------------------------------------------}
  
  TListTriggersResponse = Class(TGoogleBaseObject)
  Private
    Ftriggers : TListTriggersResponseTypetriggersArray;
  Protected
    //Property setters
    Procedure Settriggers(AIndex : Integer; const AValue : TListTriggersResponseTypetriggersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property triggers : TListTriggersResponseTypetriggersArray Index 0 Read Ftriggers Write Settriggers;
  end;
  TListTriggersResponseClass = Class of TListTriggersResponse;
  
  { --------------------------------------------------------------------
    TListVariablesResponse
    --------------------------------------------------------------------}
  
  TListVariablesResponse = Class(TGoogleBaseObject)
  Private
    Fvariables : TListVariablesResponseTypevariablesArray;
  Protected
    //Property setters
    Procedure Setvariables(AIndex : Integer; const AValue : TListVariablesResponseTypevariablesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variables : TListVariablesResponseTypevariablesArray Index 0 Read Fvariables Write Setvariables;
  end;
  TListVariablesResponseClass = Class of TListVariablesResponse;
  
  { --------------------------------------------------------------------
    TMacro
    --------------------------------------------------------------------}
  
  TMacro = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcontainerId : String;
    FdisablingRuleId : TStringArray;
    FenablingRuleId : TStringArray;
    Ffingerprint : String;
    FmacroId : String;
    Fname : String;
    Fnotes : String;
    Fparameter : TMacroTypeparameterArray;
    FparentFolderId : String;
    FscheduleEndMs : String;
    FscheduleStartMs : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisablingRuleId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetenablingRuleId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmacroId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparameter(AIndex : Integer; const AValue : TMacroTypeparameterArray); virtual;
    Procedure SetparentFolderId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property containerId : String Index 8 Read FcontainerId Write SetcontainerId;
    Property disablingRuleId : TStringArray Index 16 Read FdisablingRuleId Write SetdisablingRuleId;
    Property enablingRuleId : TStringArray Index 24 Read FenablingRuleId Write SetenablingRuleId;
    Property fingerprint : String Index 32 Read Ffingerprint Write Setfingerprint;
    Property macroId : String Index 40 Read FmacroId Write SetmacroId;
    Property name : String Index 48 Read Fname Write Setname;
    Property notes : String Index 56 Read Fnotes Write Setnotes;
    Property parameter : TMacroTypeparameterArray Index 64 Read Fparameter Write Setparameter;
    Property parentFolderId : String Index 72 Read FparentFolderId Write SetparentFolderId;
    Property scheduleEndMs : String Index 80 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : String Index 88 Read FscheduleStartMs Write SetscheduleStartMs;
    Property _type : String Index 96 Read F_type Write Set_type;
  end;
  TMacroClass = Class of TMacro;
  
  { --------------------------------------------------------------------
    TParameter
    --------------------------------------------------------------------}
  
  TParameter = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Flist : TParameterTypelistArray;
    Fmap : TParameterTypemapArray;
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlist(AIndex : Integer; const AValue : TParameterTypelistArray); virtual;
    Procedure Setmap(AIndex : Integer; const AValue : TParameterTypemapArray); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property list : TParameterTypelistArray Index 8 Read Flist Write Setlist;
    Property map : TParameterTypemapArray Index 16 Read Fmap Write Setmap;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property value : String Index 32 Read Fvalue Write Setvalue;
  end;
  TParameterClass = Class of TParameter;
  
  { --------------------------------------------------------------------
    TPublishContainerVersionResponse
    --------------------------------------------------------------------}
  
  TPublishContainerVersionResponse = Class(TGoogleBaseObject)
  Private
    FcompilerError : boolean;
    FcontainerVersion : TContainerVersion;
  Protected
    //Property setters
    Procedure SetcompilerError(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcontainerVersion(AIndex : Integer; const AValue : TContainerVersion); virtual;
  Public
  Published
    Property compilerError : boolean Index 0 Read FcompilerError Write SetcompilerError;
    Property containerVersion : TContainerVersion Index 8 Read FcontainerVersion Write SetcontainerVersion;
  end;
  TPublishContainerVersionResponseClass = Class of TPublishContainerVersionResponse;
  
  { --------------------------------------------------------------------
    TRule
    --------------------------------------------------------------------}
  
  TRule = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    Fcondition : TRuleTypeconditionArray;
    FcontainerId : String;
    Ffingerprint : String;
    Fname : String;
    Fnotes : String;
    FruleId : String;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcondition(AIndex : Integer; const AValue : TRuleTypeconditionArray); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetruleId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property condition : TRuleTypeconditionArray Index 8 Read Fcondition Write Setcondition;
    Property containerId : String Index 16 Read FcontainerId Write SetcontainerId;
    Property fingerprint : String Index 24 Read Ffingerprint Write Setfingerprint;
    Property name : String Index 32 Read Fname Write Setname;
    Property notes : String Index 40 Read Fnotes Write Setnotes;
    Property ruleId : String Index 48 Read FruleId Write SetruleId;
  end;
  TRuleClass = Class of TRule;
  
  { --------------------------------------------------------------------
    TSetupTag
    --------------------------------------------------------------------}
  
  TSetupTag = Class(TGoogleBaseObject)
  Private
    FstopOnSetupFailure : boolean;
    FtagName : String;
  Protected
    //Property setters
    Procedure SetstopOnSetupFailure(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettagName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property stopOnSetupFailure : boolean Index 0 Read FstopOnSetupFailure Write SetstopOnSetupFailure;
    Property tagName : String Index 8 Read FtagName Write SettagName;
  end;
  TSetupTagClass = Class of TSetupTag;
  
  { --------------------------------------------------------------------
    TTag
    --------------------------------------------------------------------}
  
  TTag = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FblockingRuleId : TStringArray;
    FblockingTriggerId : TStringArray;
    FcontainerId : String;
    Ffingerprint : String;
    FfiringRuleId : TStringArray;
    FfiringTriggerId : TStringArray;
    FliveOnly : boolean;
    Fname : String;
    Fnotes : String;
    Fparameter : TTagTypeparameterArray;
    FparentFolderId : String;
    Fpriority : TParameter;
    FscheduleEndMs : String;
    FscheduleStartMs : String;
    FsetupTag : TTagTypesetupTagArray;
    FtagFiringOption : String;
    FtagId : String;
    FteardownTag : TTagTypeteardownTagArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetblockingRuleId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetblockingTriggerId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfiringRuleId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetfiringTriggerId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetliveOnly(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparameter(AIndex : Integer; const AValue : TTagTypeparameterArray); virtual;
    Procedure SetparentFolderId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpriority(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsetupTag(AIndex : Integer; const AValue : TTagTypesetupTagArray); virtual;
    Procedure SettagFiringOption(AIndex : Integer; const AValue : String); virtual;
    Procedure SettagId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetteardownTag(AIndex : Integer; const AValue : TTagTypeteardownTagArray); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property blockingRuleId : TStringArray Index 8 Read FblockingRuleId Write SetblockingRuleId;
    Property blockingTriggerId : TStringArray Index 16 Read FblockingTriggerId Write SetblockingTriggerId;
    Property containerId : String Index 24 Read FcontainerId Write SetcontainerId;
    Property fingerprint : String Index 32 Read Ffingerprint Write Setfingerprint;
    Property firingRuleId : TStringArray Index 40 Read FfiringRuleId Write SetfiringRuleId;
    Property firingTriggerId : TStringArray Index 48 Read FfiringTriggerId Write SetfiringTriggerId;
    Property liveOnly : boolean Index 56 Read FliveOnly Write SetliveOnly;
    Property name : String Index 64 Read Fname Write Setname;
    Property notes : String Index 72 Read Fnotes Write Setnotes;
    Property parameter : TTagTypeparameterArray Index 80 Read Fparameter Write Setparameter;
    Property parentFolderId : String Index 88 Read FparentFolderId Write SetparentFolderId;
    Property priority : TParameter Index 96 Read Fpriority Write Setpriority;
    Property scheduleEndMs : String Index 104 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : String Index 112 Read FscheduleStartMs Write SetscheduleStartMs;
    Property setupTag : TTagTypesetupTagArray Index 120 Read FsetupTag Write SetsetupTag;
    Property tagFiringOption : String Index 128 Read FtagFiringOption Write SettagFiringOption;
    Property tagId : String Index 136 Read FtagId Write SettagId;
    Property teardownTag : TTagTypeteardownTagArray Index 144 Read FteardownTag Write SetteardownTag;
    Property _type : String Index 152 Read F_type Write Set_type;
  end;
  TTagClass = Class of TTag;
  
  { --------------------------------------------------------------------
    TTeardownTag
    --------------------------------------------------------------------}
  
  TTeardownTag = Class(TGoogleBaseObject)
  Private
    FstopTeardownOnFailure : boolean;
    FtagName : String;
  Protected
    //Property setters
    Procedure SetstopTeardownOnFailure(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettagName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property stopTeardownOnFailure : boolean Index 0 Read FstopTeardownOnFailure Write SetstopTeardownOnFailure;
    Property tagName : String Index 8 Read FtagName Write SettagName;
  end;
  TTeardownTagClass = Class of TTeardownTag;
  
  { --------------------------------------------------------------------
    TTrigger
    --------------------------------------------------------------------}
  
  TTrigger = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FautoEventFilter : TTriggerTypeautoEventFilterArray;
    FcheckValidation : TParameter;
    FcontainerId : String;
    FcustomEventFilter : TTriggerTypecustomEventFilterArray;
    FenableAllVideos : TParameter;
    FeventName : TParameter;
    Ffilter : TTriggerTypefilterArray;
    Ffingerprint : String;
    Finterval : TParameter;
    Flimit : TParameter;
    Fname : String;
    FparentFolderId : String;
    FtriggerId : String;
    F_type : String;
    FuniqueTriggerId : TParameter;
    FvideoPercentageList : TParameter;
    FwaitForTags : TParameter;
    FwaitForTagsTimeout : TParameter;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetautoEventFilter(AIndex : Integer; const AValue : TTriggerTypeautoEventFilterArray); virtual;
    Procedure SetcheckValidation(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcustomEventFilter(AIndex : Integer; const AValue : TTriggerTypecustomEventFilterArray); virtual;
    Procedure SetenableAllVideos(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure SeteventName(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : TTriggerTypefilterArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setinterval(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure Setlimit(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetparentFolderId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettriggerId(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuniqueTriggerId(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure SetvideoPercentageList(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure SetwaitForTags(AIndex : Integer; const AValue : TParameter); virtual;
    Procedure SetwaitForTagsTimeout(AIndex : Integer; const AValue : TParameter); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property autoEventFilter : TTriggerTypeautoEventFilterArray Index 8 Read FautoEventFilter Write SetautoEventFilter;
    Property checkValidation : TParameter Index 16 Read FcheckValidation Write SetcheckValidation;
    Property containerId : String Index 24 Read FcontainerId Write SetcontainerId;
    Property customEventFilter : TTriggerTypecustomEventFilterArray Index 32 Read FcustomEventFilter Write SetcustomEventFilter;
    Property enableAllVideos : TParameter Index 40 Read FenableAllVideos Write SetenableAllVideos;
    Property eventName : TParameter Index 48 Read FeventName Write SeteventName;
    Property filter : TTriggerTypefilterArray Index 56 Read Ffilter Write Setfilter;
    Property fingerprint : String Index 64 Read Ffingerprint Write Setfingerprint;
    Property interval : TParameter Index 72 Read Finterval Write Setinterval;
    Property limit : TParameter Index 80 Read Flimit Write Setlimit;
    Property name : String Index 88 Read Fname Write Setname;
    Property parentFolderId : String Index 96 Read FparentFolderId Write SetparentFolderId;
    Property triggerId : String Index 104 Read FtriggerId Write SettriggerId;
    Property _type : String Index 112 Read F_type Write Set_type;
    Property uniqueTriggerId : TParameter Index 120 Read FuniqueTriggerId Write SetuniqueTriggerId;
    Property videoPercentageList : TParameter Index 128 Read FvideoPercentageList Write SetvideoPercentageList;
    Property waitForTags : TParameter Index 136 Read FwaitForTags Write SetwaitForTags;
    Property waitForTagsTimeout : TParameter Index 144 Read FwaitForTagsTimeout Write SetwaitForTagsTimeout;
  end;
  TTriggerClass = Class of TTrigger;
  
  { --------------------------------------------------------------------
    TUserAccess
    --------------------------------------------------------------------}
  
  TUserAccess = Class(TGoogleBaseObject)
  Private
    FaccountAccess : TAccountAccess;
    FaccountId : String;
    FcontainerAccess : TUserAccessTypecontainerAccessArray;
    FemailAddress : String;
    FpermissionId : String;
  Protected
    //Property setters
    Procedure SetaccountAccess(AIndex : Integer; const AValue : TAccountAccess); virtual;
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerAccess(AIndex : Integer; const AValue : TUserAccessTypecontainerAccessArray); virtual;
    Procedure SetemailAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpermissionId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountAccess : TAccountAccess Index 0 Read FaccountAccess Write SetaccountAccess;
    Property accountId : String Index 8 Read FaccountId Write SetaccountId;
    Property containerAccess : TUserAccessTypecontainerAccessArray Index 16 Read FcontainerAccess Write SetcontainerAccess;
    Property emailAddress : String Index 24 Read FemailAddress Write SetemailAddress;
    Property permissionId : String Index 32 Read FpermissionId Write SetpermissionId;
  end;
  TUserAccessClass = Class of TUserAccess;
  
  { --------------------------------------------------------------------
    TVariable
    --------------------------------------------------------------------}
  
  TVariable = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FcontainerId : String;
    FdisablingTriggerId : TStringArray;
    FenablingTriggerId : TStringArray;
    Ffingerprint : String;
    Fname : String;
    Fnotes : String;
    Fparameter : TVariableTypeparameterArray;
    FparentFolderId : String;
    FscheduleEndMs : String;
    FscheduleStartMs : String;
    F_type : String;
    FvariableId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisablingTriggerId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetenablingTriggerId(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparameter(AIndex : Integer; const AValue : TVariableTypeparameterArray); virtual;
    Procedure SetparentFolderId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; const AValue : String); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvariableId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property containerId : String Index 8 Read FcontainerId Write SetcontainerId;
    Property disablingTriggerId : TStringArray Index 16 Read FdisablingTriggerId Write SetdisablingTriggerId;
    Property enablingTriggerId : TStringArray Index 24 Read FenablingTriggerId Write SetenablingTriggerId;
    Property fingerprint : String Index 32 Read Ffingerprint Write Setfingerprint;
    Property name : String Index 40 Read Fname Write Setname;
    Property notes : String Index 48 Read Fnotes Write Setnotes;
    Property parameter : TVariableTypeparameterArray Index 56 Read Fparameter Write Setparameter;
    Property parentFolderId : String Index 64 Read FparentFolderId Write SetparentFolderId;
    Property scheduleEndMs : String Index 72 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : String Index 80 Read FscheduleStartMs Write SetscheduleStartMs;
    Property _type : String Index 88 Read F_type Write Set_type;
    Property variableId : String Index 96 Read FvariableId Write SetvariableId;
  end;
  TVariableClass = Class of TVariable;
  
  { --------------------------------------------------------------------
    TAccountsContainersEnvironmentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersEnvironmentsResource, method Patch
  
  TAccountsContainersEnvironmentsPatchOptions = Record
    fingerprint : String;
  end;
  
  
  //Optional query Options for TAccountsContainersEnvironmentsResource, method Update
  
  TAccountsContainersEnvironmentsUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersEnvironmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aEnvironment : TEnvironment) : TEnvironment;overload;
    Procedure Delete(accountId: string; containerId: string; environmentId: string);
    Function Get(accountId: string; containerId: string; environmentId: string) : TEnvironment;
    Function List(accountId: string; containerId: string) : TListEnvironmentsResponse;
    Function Patch(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : string  = '') : TEnvironment;
    Function Patch(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : TAccountsContainersEnvironmentspatchOptions) : TEnvironment;
    Function Update(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : string  = '') : TEnvironment;
    Function Update(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : TAccountsContainersEnvironmentsupdateOptions) : TEnvironment;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersFoldersEntitiesResource
    --------------------------------------------------------------------}
  
  TAccountsContainersFoldersEntitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; containerId: string; folderId: string) : TFolderEntities;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersFoldersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersFoldersResource, method Update
  
  TAccountsContainersFoldersUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersFoldersResource = Class(TGoogleResource)
  Private
    FEntitiesInstance : TAccountsContainersFoldersEntitiesResource;
    Function GetEntitiesInstance : TAccountsContainersFoldersEntitiesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aFolder : TFolder) : TFolder;overload;
    Procedure Delete(accountId: string; containerId: string; folderId: string);
    Function Get(accountId: string; containerId: string; folderId: string) : TFolder;
    Function List(accountId: string; containerId: string) : TListFoldersResponse;
    Function Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : string  = '') : TFolder;
    Function Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : TAccountsContainersFoldersupdateOptions) : TFolder;
    Function CreateEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateEntitiesResource : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Property EntitiesResource : TAccountsContainersFoldersEntitiesResource Read GetEntitiesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersMove_foldersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersMove_foldersResource, method Update
  
  TAccountsContainersMove_foldersUpdateOptions = Record
    tagId : String;
    triggerId : String;
    variableId : String;
  end;
  
  TAccountsContainersMove_foldersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : string  = '');
    Procedure Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : TAccountsContainersMove_foldersupdateOptions);
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersReauthorize_environmentsResource
    --------------------------------------------------------------------}
  
  TAccountsContainersReauthorize_environmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment) : TEnvironment;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersTagsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersTagsResource, method Update
  
  TAccountsContainersTagsUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersTagsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aTag : TTag) : TTag;overload;
    Procedure Delete(accountId: string; containerId: string; tagId: string);
    Function Get(accountId: string; containerId: string; tagId: string) : TTag;
    Function List(accountId: string; containerId: string) : TListTagsResponse;
    Function Update(accountId: string; containerId: string; tagId: string; aTag : TTag; AQuery : string  = '') : TTag;
    Function Update(accountId: string; containerId: string; tagId: string; aTag : TTag; AQuery : TAccountsContainersTagsupdateOptions) : TTag;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersTriggersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersTriggersResource, method Update
  
  TAccountsContainersTriggersUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersTriggersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aTrigger : TTrigger) : TTrigger;overload;
    Procedure Delete(accountId: string; containerId: string; triggerId: string);
    Function Get(accountId: string; containerId: string; triggerId: string) : TTrigger;
    Function List(accountId: string; containerId: string) : TListTriggersResponse;
    Function Update(accountId: string; containerId: string; triggerId: string; aTrigger : TTrigger; AQuery : string  = '') : TTrigger;
    Function Update(accountId: string; containerId: string; triggerId: string; aTrigger : TTrigger; AQuery : TAccountsContainersTriggersupdateOptions) : TTrigger;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersVariablesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersVariablesResource, method Update
  
  TAccountsContainersVariablesUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersVariablesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aVariable : TVariable) : TVariable;overload;
    Procedure Delete(accountId: string; containerId: string; variableId: string);
    Function Get(accountId: string; containerId: string; variableId: string) : TVariable;
    Function List(accountId: string; containerId: string) : TListVariablesResponse;
    Function Update(accountId: string; containerId: string; variableId: string; aVariable : TVariable; AQuery : string  = '') : TVariable;
    Function Update(accountId: string; containerId: string; variableId: string; aVariable : TVariable; AQuery : TAccountsContainersVariablesupdateOptions) : TVariable;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersVersionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersVersionsResource, method List
  
  TAccountsContainersVersionsListOptions = Record
    headers : boolean;
    includeDeleted : boolean;
  end;
  
  
  //Optional query Options for TAccountsContainersVersionsResource, method Publish
  
  TAccountsContainersVersionsPublishOptions = Record
    fingerprint : String;
  end;
  
  
  //Optional query Options for TAccountsContainersVersionsResource, method Update
  
  TAccountsContainersVersionsUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersVersionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aCreateContainerVersionRequestVersionOptions : TCreateContainerVersionRequestVersionOptions) : TCreateContainerVersionResponse;overload;
    Procedure Delete(accountId: string; containerId: string; containerVersionId: string);
    Function Get(accountId: string; containerId: string; containerVersionId: string) : TContainerVersion;
    Function List(accountId: string; containerId: string; AQuery : string  = '') : TListContainerVersionsResponse;
    Function List(accountId: string; containerId: string; AQuery : TAccountsContainersVersionslistOptions) : TListContainerVersionsResponse;
    Function Publish(accountId: string; containerId: string; containerVersionId: string; AQuery : string  = '') : TPublishContainerVersionResponse;
    Function Publish(accountId: string; containerId: string; containerVersionId: string; AQuery : TAccountsContainersVersionspublishOptions) : TPublishContainerVersionResponse;
    Function Restore(accountId: string; containerId: string; containerVersionId: string) : TContainerVersion;
    Function Undelete(accountId: string; containerId: string; containerVersionId: string) : TContainerVersion;
    Function Update(accountId: string; containerId: string; containerVersionId: string; aContainerVersion : TContainerVersion; AQuery : string  = '') : TContainerVersion;
    Function Update(accountId: string; containerId: string; containerVersionId: string; aContainerVersion : TContainerVersion; AQuery : TAccountsContainersVersionsupdateOptions) : TContainerVersion;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersResource, method Update
  
  TAccountsContainersUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersResource = Class(TGoogleResource)
  Private
    FEnvironmentsInstance : TAccountsContainersEnvironmentsResource;
    FFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;
    FFoldersInstance : TAccountsContainersFoldersResource;
    FMove_foldersInstance : TAccountsContainersMove_foldersResource;
    FReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;
    FTagsInstance : TAccountsContainersTagsResource;
    FTriggersInstance : TAccountsContainersTriggersResource;
    FVariablesInstance : TAccountsContainersVariablesResource;
    FVersionsInstance : TAccountsContainersVersionsResource;
    Function GetEnvironmentsInstance : TAccountsContainersEnvironmentsResource;virtual;
    Function GetFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;virtual;
    Function GetFoldersInstance : TAccountsContainersFoldersResource;virtual;
    Function GetMove_foldersInstance : TAccountsContainersMove_foldersResource;virtual;
    Function GetReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;virtual;
    Function GetTagsInstance : TAccountsContainersTagsResource;virtual;
    Function GetTriggersInstance : TAccountsContainersTriggersResource;virtual;
    Function GetVariablesInstance : TAccountsContainersVariablesResource;virtual;
    Function GetVersionsInstance : TAccountsContainersVersionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; aContainer : TContainer) : TContainer;overload;
    Procedure Delete(accountId: string; containerId: string);
    Function Get(accountId: string; containerId: string) : TContainer;
    Function List(accountId: string) : TListContainersResponse;
    Function Update(accountId: string; containerId: string; aContainer : TContainer; AQuery : string  = '') : TContainer;
    Function Update(accountId: string; containerId: string; aContainer : TContainer; AQuery : TAccountsContainersupdateOptions) : TContainer;
    Function CreateEnvironmentsResource(AOwner : TComponent) : TAccountsContainersEnvironmentsResource;virtual;overload;
    Function CreateEnvironmentsResource : TAccountsContainersEnvironmentsResource;virtual;overload;
    Function CreateFoldersEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateFoldersResource(AOwner : TComponent) : TAccountsContainersFoldersResource;virtual;overload;
    Function CreateFoldersResource : TAccountsContainersFoldersResource;virtual;overload;
    Function CreateMove_foldersResource(AOwner : TComponent) : TAccountsContainersMove_foldersResource;virtual;overload;
    Function CreateMove_foldersResource : TAccountsContainersMove_foldersResource;virtual;overload;
    Function CreateReauthorize_environmentsResource(AOwner : TComponent) : TAccountsContainersReauthorize_environmentsResource;virtual;overload;
    Function CreateReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource;virtual;overload;
    Function CreateTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;virtual;overload;
    Function CreateTagsResource : TAccountsContainersTagsResource;virtual;overload;
    Function CreateTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateTriggersResource : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateVariablesResource : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;virtual;overload;
    Function CreateVersionsResource : TAccountsContainersVersionsResource;virtual;overload;
    Property EnvironmentsResource : TAccountsContainersEnvironmentsResource Read GetEnvironmentsInstance;
    Property FoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource Read GetFoldersEntitiesInstance;
    Property FoldersResource : TAccountsContainersFoldersResource Read GetFoldersInstance;
    Property Move_foldersResource : TAccountsContainersMove_foldersResource Read GetMove_foldersInstance;
    Property Reauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource Read GetReauthorize_environmentsInstance;
    Property TagsResource : TAccountsContainersTagsResource Read GetTagsInstance;
    Property TriggersResource : TAccountsContainersTriggersResource Read GetTriggersInstance;
    Property VariablesResource : TAccountsContainersVariablesResource Read GetVariablesInstance;
    Property VersionsResource : TAccountsContainersVersionsResource Read GetVersionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsPermissionsResource
    --------------------------------------------------------------------}
  
  TAccountsPermissionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; aUserAccess : TUserAccess) : TUserAccess;overload;
    Procedure Delete(accountId: string; permissionId: string);
    Function Get(accountId: string; permissionId: string) : TUserAccess;
    Function List(accountId: string) : TListAccountUsersResponse;
    Function Update(accountId: string; permissionId: string; aUserAccess : TUserAccess) : TUserAccess;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method Update
  
  TAccountsUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Private
    FContainersEnvironmentsInstance : TAccountsContainersEnvironmentsResource;
    FContainersFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;
    FContainersFoldersInstance : TAccountsContainersFoldersResource;
    FContainersMove_foldersInstance : TAccountsContainersMove_foldersResource;
    FContainersReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;
    FContainersTagsInstance : TAccountsContainersTagsResource;
    FContainersTriggersInstance : TAccountsContainersTriggersResource;
    FContainersVariablesInstance : TAccountsContainersVariablesResource;
    FContainersVersionsInstance : TAccountsContainersVersionsResource;
    FContainersInstance : TAccountsContainersResource;
    FPermissionsInstance : TAccountsPermissionsResource;
    Function GetContainersEnvironmentsInstance : TAccountsContainersEnvironmentsResource;virtual;
    Function GetContainersFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;virtual;
    Function GetContainersFoldersInstance : TAccountsContainersFoldersResource;virtual;
    Function GetContainersMove_foldersInstance : TAccountsContainersMove_foldersResource;virtual;
    Function GetContainersReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;virtual;
    Function GetContainersTagsInstance : TAccountsContainersTagsResource;virtual;
    Function GetContainersTriggersInstance : TAccountsContainersTriggersResource;virtual;
    Function GetContainersVariablesInstance : TAccountsContainersVariablesResource;virtual;
    Function GetContainersVersionsInstance : TAccountsContainersVersionsResource;virtual;
    Function GetContainersInstance : TAccountsContainersResource;virtual;
    Function GetPermissionsInstance : TAccountsPermissionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string) : TAccount;
    Function List : TListAccountsResponse;
    Function Update(accountId: string; aAccount : TAccount; AQuery : string  = '') : TAccount;
    Function Update(accountId: string; aAccount : TAccount; AQuery : TAccountsupdateOptions) : TAccount;
    Function CreateContainersEnvironmentsResource(AOwner : TComponent) : TAccountsContainersEnvironmentsResource;virtual;overload;
    Function CreateContainersEnvironmentsResource : TAccountsContainersEnvironmentsResource;virtual;overload;
    Function CreateContainersFoldersEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateContainersFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateContainersFoldersResource(AOwner : TComponent) : TAccountsContainersFoldersResource;virtual;overload;
    Function CreateContainersFoldersResource : TAccountsContainersFoldersResource;virtual;overload;
    Function CreateContainersMove_foldersResource(AOwner : TComponent) : TAccountsContainersMove_foldersResource;virtual;overload;
    Function CreateContainersMove_foldersResource : TAccountsContainersMove_foldersResource;virtual;overload;
    Function CreateContainersReauthorize_environmentsResource(AOwner : TComponent) : TAccountsContainersReauthorize_environmentsResource;virtual;overload;
    Function CreateContainersReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource;virtual;overload;
    Function CreateContainersTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;virtual;overload;
    Function CreateContainersTagsResource : TAccountsContainersTagsResource;virtual;overload;
    Function CreateContainersTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateContainersTriggersResource : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateContainersVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateContainersVariablesResource : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateContainersVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;virtual;overload;
    Function CreateContainersVersionsResource : TAccountsContainersVersionsResource;virtual;overload;
    Function CreateContainersResource(AOwner : TComponent) : TAccountsContainersResource;virtual;overload;
    Function CreateContainersResource : TAccountsContainersResource;virtual;overload;
    Function CreatePermissionsResource(AOwner : TComponent) : TAccountsPermissionsResource;virtual;overload;
    Function CreatePermissionsResource : TAccountsPermissionsResource;virtual;overload;
    Property ContainersEnvironmentsResource : TAccountsContainersEnvironmentsResource Read GetContainersEnvironmentsInstance;
    Property ContainersFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource Read GetContainersFoldersEntitiesInstance;
    Property ContainersFoldersResource : TAccountsContainersFoldersResource Read GetContainersFoldersInstance;
    Property ContainersMove_foldersResource : TAccountsContainersMove_foldersResource Read GetContainersMove_foldersInstance;
    Property ContainersReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource Read GetContainersReauthorize_environmentsInstance;
    Property ContainersTagsResource : TAccountsContainersTagsResource Read GetContainersTagsInstance;
    Property ContainersTriggersResource : TAccountsContainersTriggersResource Read GetContainersTriggersInstance;
    Property ContainersVariablesResource : TAccountsContainersVariablesResource Read GetContainersVariablesInstance;
    Property ContainersVersionsResource : TAccountsContainersVersionsResource Read GetContainersVersionsInstance;
    Property ContainersResource : TAccountsContainersResource Read GetContainersInstance;
    Property PermissionsResource : TAccountsPermissionsResource Read GetPermissionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TTagmanagerAPI
    --------------------------------------------------------------------}
  
  TTagmanagerAPI = Class(TGoogleAPI)
  Private
    FAccountsContainersEnvironmentsInstance : TAccountsContainersEnvironmentsResource;
    FAccountsContainersFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;
    FAccountsContainersFoldersInstance : TAccountsContainersFoldersResource;
    FAccountsContainersMove_foldersInstance : TAccountsContainersMove_foldersResource;
    FAccountsContainersReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;
    FAccountsContainersTagsInstance : TAccountsContainersTagsResource;
    FAccountsContainersTriggersInstance : TAccountsContainersTriggersResource;
    FAccountsContainersVariablesInstance : TAccountsContainersVariablesResource;
    FAccountsContainersVersionsInstance : TAccountsContainersVersionsResource;
    FAccountsContainersInstance : TAccountsContainersResource;
    FAccountsPermissionsInstance : TAccountsPermissionsResource;
    FAccountsInstance : TAccountsResource;
    Function GetAccountsContainersEnvironmentsInstance : TAccountsContainersEnvironmentsResource;virtual;
    Function GetAccountsContainersFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;virtual;
    Function GetAccountsContainersFoldersInstance : TAccountsContainersFoldersResource;virtual;
    Function GetAccountsContainersMove_foldersInstance : TAccountsContainersMove_foldersResource;virtual;
    Function GetAccountsContainersReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;virtual;
    Function GetAccountsContainersTagsInstance : TAccountsContainersTagsResource;virtual;
    Function GetAccountsContainersTriggersInstance : TAccountsContainersTriggersResource;virtual;
    Function GetAccountsContainersVariablesInstance : TAccountsContainersVariablesResource;virtual;
    Function GetAccountsContainersVersionsInstance : TAccountsContainersVersionsResource;virtual;
    Function GetAccountsContainersInstance : TAccountsContainersResource;virtual;
    Function GetAccountsPermissionsInstance : TAccountsPermissionsResource;virtual;
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
    Function CreateAccountsContainersEnvironmentsResource(AOwner : TComponent) : TAccountsContainersEnvironmentsResource;virtual;overload;
    Function CreateAccountsContainersEnvironmentsResource : TAccountsContainersEnvironmentsResource;virtual;overload;
    Function CreateAccountsContainersFoldersEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateAccountsContainersFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource;virtual;overload;
    Function CreateAccountsContainersFoldersResource(AOwner : TComponent) : TAccountsContainersFoldersResource;virtual;overload;
    Function CreateAccountsContainersFoldersResource : TAccountsContainersFoldersResource;virtual;overload;
    Function CreateAccountsContainersMove_foldersResource(AOwner : TComponent) : TAccountsContainersMove_foldersResource;virtual;overload;
    Function CreateAccountsContainersMove_foldersResource : TAccountsContainersMove_foldersResource;virtual;overload;
    Function CreateAccountsContainersReauthorize_environmentsResource(AOwner : TComponent) : TAccountsContainersReauthorize_environmentsResource;virtual;overload;
    Function CreateAccountsContainersReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource;virtual;overload;
    Function CreateAccountsContainersTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;virtual;overload;
    Function CreateAccountsContainersTagsResource : TAccountsContainersTagsResource;virtual;overload;
    Function CreateAccountsContainersTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateAccountsContainersTriggersResource : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateAccountsContainersVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateAccountsContainersVariablesResource : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateAccountsContainersVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;virtual;overload;
    Function CreateAccountsContainersVersionsResource : TAccountsContainersVersionsResource;virtual;overload;
    Function CreateAccountsContainersResource(AOwner : TComponent) : TAccountsContainersResource;virtual;overload;
    Function CreateAccountsContainersResource : TAccountsContainersResource;virtual;overload;
    Function CreateAccountsPermissionsResource(AOwner : TComponent) : TAccountsPermissionsResource;virtual;overload;
    Function CreateAccountsPermissionsResource : TAccountsPermissionsResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsContainersEnvironmentsResource : TAccountsContainersEnvironmentsResource Read GetAccountsContainersEnvironmentsInstance;
    Property AccountsContainersFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource Read GetAccountsContainersFoldersEntitiesInstance;
    Property AccountsContainersFoldersResource : TAccountsContainersFoldersResource Read GetAccountsContainersFoldersInstance;
    Property AccountsContainersMove_foldersResource : TAccountsContainersMove_foldersResource Read GetAccountsContainersMove_foldersInstance;
    Property AccountsContainersReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource Read GetAccountsContainersReauthorize_environmentsInstance;
    Property AccountsContainersTagsResource : TAccountsContainersTagsResource Read GetAccountsContainersTagsInstance;
    Property AccountsContainersTriggersResource : TAccountsContainersTriggersResource Read GetAccountsContainersTriggersInstance;
    Property AccountsContainersVariablesResource : TAccountsContainersVariablesResource Read GetAccountsContainersVariablesInstance;
    Property AccountsContainersVersionsResource : TAccountsContainersVersionsResource Read GetAccountsContainersVersionsInstance;
    Property AccountsContainersResource : TAccountsContainersResource Read GetAccountsContainersInstance;
    Property AccountsPermissionsResource : TAccountsPermissionsResource Read GetAccountsPermissionsInstance;
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetshareData(AIndex : Integer; const AValue : boolean); 

begin
  If (FshareData=AValue) then exit;
  FshareData:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountAccess
  --------------------------------------------------------------------}


Procedure TAccountAccess.Setpermission(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermission=AValue) then exit;
  Fpermission:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccountAccess.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permission' : SetLength(Fpermission,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCondition
  --------------------------------------------------------------------}


Procedure TCondition.Setparameter(AIndex : Integer; const AValue : TConditionTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCondition.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCondition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'parameter' : SetLength(Fparameter,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TContainer
  --------------------------------------------------------------------}


Procedure TContainer.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetdomainName(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdomainName=AValue) then exit;
  FdomainName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetenabledBuiltInVariable(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FenabledBuiltInVariable=AValue) then exit;
  FenabledBuiltInVariable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetpublicId(AIndex : Integer; const AValue : String); 

begin
  If (FpublicId=AValue) then exit;
  FpublicId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SettimeZoneCountryId(AIndex : Integer; const AValue : String); 

begin
  If (FtimeZoneCountryId=AValue) then exit;
  FtimeZoneCountryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SettimeZoneId(AIndex : Integer; const AValue : String); 

begin
  If (FtimeZoneId=AValue) then exit;
  FtimeZoneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetusageContext(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FusageContext=AValue) then exit;
  FusageContext:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TContainer.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'domainname' : SetLength(FdomainName,ALength);
  'enabledbuiltinvariable' : SetLength(FenabledBuiltInVariable,ALength);
  'usagecontext' : SetLength(FusageContext,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TContainerAccess
  --------------------------------------------------------------------}


Procedure TContainerAccess.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerAccess.Setpermission(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermission=AValue) then exit;
  Fpermission:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TContainerAccess.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permission' : SetLength(Fpermission,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TContainerVersion
  --------------------------------------------------------------------}


Procedure TContainerVersion.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setcontainer(AIndex : Integer; const AValue : TContainer); 

begin
  If (Fcontainer=AValue) then exit;
  Fcontainer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.SetcontainerVersionId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerVersionId=AValue) then exit;
  FcontainerVersionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setfolder(AIndex : Integer; const AValue : TContainerVersionTypefolderArray); 

begin
  If (Ffolder=AValue) then exit;
  Ffolder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setmacro(AIndex : Integer; const AValue : TContainerVersionTypemacroArray); 

begin
  If (Fmacro=AValue) then exit;
  Fmacro:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setrule(AIndex : Integer; const AValue : TContainerVersionTyperuleArray); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Settag(AIndex : Integer; const AValue : TContainerVersionTypetagArray); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Settrigger(AIndex : Integer; const AValue : TContainerVersionTypetriggerArray); 

begin
  If (Ftrigger=AValue) then exit;
  Ftrigger:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setvariable(AIndex : Integer; const AValue : TContainerVersionTypevariableArray); 

begin
  If (Fvariable=AValue) then exit;
  Fvariable:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TContainerVersion.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'folder' : SetLength(Ffolder,ALength);
  'macro' : SetLength(Fmacro,ALength);
  'rule' : SetLength(Frule,ALength);
  'tag' : SetLength(Ftag,ALength);
  'trigger' : SetLength(Ftrigger,ALength);
  'variable' : SetLength(Fvariable,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TContainerVersionHeader
  --------------------------------------------------------------------}


Procedure TContainerVersionHeader.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetcontainerVersionId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerVersionId=AValue) then exit;
  FcontainerVersionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumMacros(AIndex : Integer; const AValue : String); 

begin
  If (FnumMacros=AValue) then exit;
  FnumMacros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumRules(AIndex : Integer; const AValue : String); 

begin
  If (FnumRules=AValue) then exit;
  FnumRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumTags(AIndex : Integer; const AValue : String); 

begin
  If (FnumTags=AValue) then exit;
  FnumTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumTriggers(AIndex : Integer; const AValue : String); 

begin
  If (FnumTriggers=AValue) then exit;
  FnumTriggers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumVariables(AIndex : Integer; const AValue : String); 

begin
  If (FnumVariables=AValue) then exit;
  FnumVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateContainerVersionRequestVersionOptions
  --------------------------------------------------------------------}


Procedure TCreateContainerVersionRequestVersionOptions.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionRequestVersionOptions.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionRequestVersionOptions.SetquickPreview(AIndex : Integer; const AValue : boolean); 

begin
  If (FquickPreview=AValue) then exit;
  FquickPreview:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateContainerVersionResponse
  --------------------------------------------------------------------}


Procedure TCreateContainerVersionResponse.SetcompilerError(AIndex : Integer; const AValue : boolean); 

begin
  If (FcompilerError=AValue) then exit;
  FcompilerError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionResponse.SetcontainerVersion(AIndex : Integer; const AValue : TContainerVersion); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnvironment
  --------------------------------------------------------------------}


Procedure TEnvironment.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetauthorizationCode(AIndex : Integer; const AValue : String); 

begin
  If (FauthorizationCode=AValue) then exit;
  FauthorizationCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetauthorizationTimestampMs(AIndex : Integer; const AValue : String); 

begin
  If (FauthorizationTimestampMs=AValue) then exit;
  FauthorizationTimestampMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetcontainerVersionId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerVersionId=AValue) then exit;
  FcontainerVersionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetenableDebug(AIndex : Integer; const AValue : boolean); 

begin
  If (FenableDebug=AValue) then exit;
  FenableDebug:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetenvironmentId(AIndex : Integer; const AValue : String); 

begin
  If (FenvironmentId=AValue) then exit;
  FenvironmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEnvironment.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFolder
  --------------------------------------------------------------------}


Procedure TFolder.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFolder.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFolder.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFolder.SetfolderId(AIndex : Integer; const AValue : String); 

begin
  If (FfolderId=AValue) then exit;
  FfolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFolder.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFolderEntities
  --------------------------------------------------------------------}


Procedure TFolderEntities.Settag(AIndex : Integer; const AValue : TFolderEntitiesTypetagArray); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFolderEntities.Settrigger(AIndex : Integer; const AValue : TFolderEntitiesTypetriggerArray); 

begin
  If (Ftrigger=AValue) then exit;
  Ftrigger:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFolderEntities.Setvariable(AIndex : Integer; const AValue : TFolderEntitiesTypevariableArray); 

begin
  If (Fvariable=AValue) then exit;
  Fvariable:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFolderEntities.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'tag' : SetLength(Ftag,ALength);
  'trigger' : SetLength(Ftrigger,ALength);
  'variable' : SetLength(Fvariable,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListAccountUsersResponse
  --------------------------------------------------------------------}


Procedure TListAccountUsersResponse.SetuserAccess(AIndex : Integer; const AValue : TListAccountUsersResponseTypeuserAccessArray); 

begin
  If (FuserAccess=AValue) then exit;
  FuserAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListAccountUsersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'useraccess' : SetLength(FuserAccess,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListAccountsResponse
  --------------------------------------------------------------------}


Procedure TListAccountsResponse.Setaccounts(AIndex : Integer; const AValue : TListAccountsResponseTypeaccountsArray); 

begin
  If (Faccounts=AValue) then exit;
  Faccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListAccountsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'accounts' : SetLength(Faccounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListContainerVersionsResponse
  --------------------------------------------------------------------}


Procedure TListContainerVersionsResponse.SetcontainerVersion(AIndex : Integer; const AValue : TListContainerVersionsResponseTypecontainerVersionArray); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListContainerVersionsResponse.SetcontainerVersionHeader(AIndex : Integer; const AValue : TListContainerVersionsResponseTypecontainerVersionHeaderArray); 

begin
  If (FcontainerVersionHeader=AValue) then exit;
  FcontainerVersionHeader:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListContainerVersionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'containerversion' : SetLength(FcontainerVersion,ALength);
  'containerversionheader' : SetLength(FcontainerVersionHeader,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListContainersResponse
  --------------------------------------------------------------------}


Procedure TListContainersResponse.Setcontainers(AIndex : Integer; const AValue : TListContainersResponseTypecontainersArray); 

begin
  If (Fcontainers=AValue) then exit;
  Fcontainers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListContainersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'containers' : SetLength(Fcontainers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListEnvironmentsResponse
  --------------------------------------------------------------------}


Procedure TListEnvironmentsResponse.Setenvironments(AIndex : Integer; const AValue : TListEnvironmentsResponseTypeenvironmentsArray); 

begin
  If (Fenvironments=AValue) then exit;
  Fenvironments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListEnvironmentsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'environments' : SetLength(Fenvironments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListFoldersResponse
  --------------------------------------------------------------------}


Procedure TListFoldersResponse.Setfolders(AIndex : Integer; const AValue : TListFoldersResponseTypefoldersArray); 

begin
  If (Ffolders=AValue) then exit;
  Ffolders:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListFoldersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'folders' : SetLength(Ffolders,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListTagsResponse
  --------------------------------------------------------------------}


Procedure TListTagsResponse.Settags(AIndex : Integer; const AValue : TListTagsResponseTypetagsArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTagsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'tags' : SetLength(Ftags,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListTriggersResponse
  --------------------------------------------------------------------}


Procedure TListTriggersResponse.Settriggers(AIndex : Integer; const AValue : TListTriggersResponseTypetriggersArray); 

begin
  If (Ftriggers=AValue) then exit;
  Ftriggers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTriggersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'triggers' : SetLength(Ftriggers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListVariablesResponse
  --------------------------------------------------------------------}


Procedure TListVariablesResponse.Setvariables(AIndex : Integer; const AValue : TListVariablesResponseTypevariablesArray); 

begin
  If (Fvariables=AValue) then exit;
  Fvariables:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListVariablesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variables' : SetLength(Fvariables,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMacro
  --------------------------------------------------------------------}


Procedure TMacro.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetdisablingRuleId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdisablingRuleId=AValue) then exit;
  FdisablingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetenablingRuleId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FenablingRuleId=AValue) then exit;
  FenablingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetmacroId(AIndex : Integer; const AValue : String); 

begin
  If (FmacroId=AValue) then exit;
  FmacroId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setparameter(AIndex : Integer; const AValue : TMacroTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetparentFolderId(AIndex : Integer; const AValue : String); 

begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetscheduleEndMs(AIndex : Integer; const AValue : String); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetscheduleStartMs(AIndex : Integer; const AValue : String); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMacro.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMacro.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'disablingruleid' : SetLength(FdisablingRuleId,ALength);
  'enablingruleid' : SetLength(FenablingRuleId,ALength);
  'parameter' : SetLength(Fparameter,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TParameter
  --------------------------------------------------------------------}


Procedure TParameter.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setlist(AIndex : Integer; const AValue : TParameterTypelistArray); 

begin
  If (Flist=AValue) then exit;
  Flist:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setmap(AIndex : Integer; const AValue : TParameterTypemapArray); 

begin
  If (Fmap=AValue) then exit;
  Fmap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TParameter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TParameter.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'list' : SetLength(Flist,ALength);
  'map' : SetLength(Fmap,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPublishContainerVersionResponse
  --------------------------------------------------------------------}


Procedure TPublishContainerVersionResponse.SetcompilerError(AIndex : Integer; const AValue : boolean); 

begin
  If (FcompilerError=AValue) then exit;
  FcompilerError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishContainerVersionResponse.SetcontainerVersion(AIndex : Integer; const AValue : TContainerVersion); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRule
  --------------------------------------------------------------------}


Procedure TRule.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setcondition(AIndex : Integer; const AValue : TRuleTypeconditionArray); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetruleId(AIndex : Integer; const AValue : String); 

begin
  If (FruleId=AValue) then exit;
  FruleId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'condition' : SetLength(Fcondition,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSetupTag
  --------------------------------------------------------------------}


Procedure TSetupTag.SetstopOnSetupFailure(AIndex : Integer; const AValue : boolean); 

begin
  If (FstopOnSetupFailure=AValue) then exit;
  FstopOnSetupFailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetupTag.SettagName(AIndex : Integer; const AValue : String); 

begin
  If (FtagName=AValue) then exit;
  FtagName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTag
  --------------------------------------------------------------------}


Procedure TTag.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetblockingRuleId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FblockingRuleId=AValue) then exit;
  FblockingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetblockingTriggerId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FblockingTriggerId=AValue) then exit;
  FblockingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetfiringRuleId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FfiringRuleId=AValue) then exit;
  FfiringRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetfiringTriggerId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FfiringTriggerId=AValue) then exit;
  FfiringTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetliveOnly(AIndex : Integer; const AValue : boolean); 

begin
  If (FliveOnly=AValue) then exit;
  FliveOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setparameter(AIndex : Integer; const AValue : TTagTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetparentFolderId(AIndex : Integer; const AValue : String); 

begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setpriority(AIndex : Integer; const AValue : TParameter); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetscheduleEndMs(AIndex : Integer; const AValue : String); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetscheduleStartMs(AIndex : Integer; const AValue : String); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetsetupTag(AIndex : Integer; const AValue : TTagTypesetupTagArray); 

begin
  If (FsetupTag=AValue) then exit;
  FsetupTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SettagFiringOption(AIndex : Integer; const AValue : String); 

begin
  If (FtagFiringOption=AValue) then exit;
  FtagFiringOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SettagId(AIndex : Integer; const AValue : String); 

begin
  If (FtagId=AValue) then exit;
  FtagId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetteardownTag(AIndex : Integer; const AValue : TTagTypeteardownTagArray); 

begin
  If (FteardownTag=AValue) then exit;
  FteardownTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTag.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTag.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'blockingruleid' : SetLength(FblockingRuleId,ALength);
  'blockingtriggerid' : SetLength(FblockingTriggerId,ALength);
  'firingruleid' : SetLength(FfiringRuleId,ALength);
  'firingtriggerid' : SetLength(FfiringTriggerId,ALength);
  'parameter' : SetLength(Fparameter,ALength);
  'setuptag' : SetLength(FsetupTag,ALength);
  'teardowntag' : SetLength(FteardownTag,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTeardownTag
  --------------------------------------------------------------------}


Procedure TTeardownTag.SetstopTeardownOnFailure(AIndex : Integer; const AValue : boolean); 

begin
  If (FstopTeardownOnFailure=AValue) then exit;
  FstopTeardownOnFailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTeardownTag.SettagName(AIndex : Integer; const AValue : String); 

begin
  If (FtagName=AValue) then exit;
  FtagName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTrigger
  --------------------------------------------------------------------}


Procedure TTrigger.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetautoEventFilter(AIndex : Integer; const AValue : TTriggerTypeautoEventFilterArray); 

begin
  If (FautoEventFilter=AValue) then exit;
  FautoEventFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcheckValidation(AIndex : Integer; const AValue : TParameter); 

begin
  If (FcheckValidation=AValue) then exit;
  FcheckValidation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcustomEventFilter(AIndex : Integer; const AValue : TTriggerTypecustomEventFilterArray); 

begin
  If (FcustomEventFilter=AValue) then exit;
  FcustomEventFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetenableAllVideos(AIndex : Integer; const AValue : TParameter); 

begin
  If (FenableAllVideos=AValue) then exit;
  FenableAllVideos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SeteventName(AIndex : Integer; const AValue : TParameter); 

begin
  If (FeventName=AValue) then exit;
  FeventName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setfilter(AIndex : Integer; const AValue : TTriggerTypefilterArray); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setinterval(AIndex : Integer; const AValue : TParameter); 

begin
  If (Finterval=AValue) then exit;
  Finterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setlimit(AIndex : Integer; const AValue : TParameter); 

begin
  If (Flimit=AValue) then exit;
  Flimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetparentFolderId(AIndex : Integer; const AValue : String); 

begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SettriggerId(AIndex : Integer; const AValue : String); 

begin
  If (FtriggerId=AValue) then exit;
  FtriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetuniqueTriggerId(AIndex : Integer; const AValue : TParameter); 

begin
  If (FuniqueTriggerId=AValue) then exit;
  FuniqueTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetvideoPercentageList(AIndex : Integer; const AValue : TParameter); 

begin
  If (FvideoPercentageList=AValue) then exit;
  FvideoPercentageList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetwaitForTags(AIndex : Integer; const AValue : TParameter); 

begin
  If (FwaitForTags=AValue) then exit;
  FwaitForTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetwaitForTagsTimeout(AIndex : Integer; const AValue : TParameter); 

begin
  If (FwaitForTagsTimeout=AValue) then exit;
  FwaitForTagsTimeout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTrigger.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTrigger.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'autoeventfilter' : SetLength(FautoEventFilter,ALength);
  'customeventfilter' : SetLength(FcustomEventFilter,ALength);
  'filter' : SetLength(Ffilter,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserAccess
  --------------------------------------------------------------------}


Procedure TUserAccess.SetaccountAccess(AIndex : Integer; const AValue : TAccountAccess); 

begin
  If (FaccountAccess=AValue) then exit;
  FaccountAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetcontainerAccess(AIndex : Integer; const AValue : TUserAccessTypecontainerAccessArray); 

begin
  If (FcontainerAccess=AValue) then exit;
  FcontainerAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetemailAddress(AIndex : Integer; const AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetpermissionId(AIndex : Integer; const AValue : String); 

begin
  If (FpermissionId=AValue) then exit;
  FpermissionId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUserAccess.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'containeraccess' : SetLength(FcontainerAccess,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariable
  --------------------------------------------------------------------}


Procedure TVariable.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetcontainerId(AIndex : Integer; const AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetdisablingTriggerId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdisablingTriggerId=AValue) then exit;
  FdisablingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetenablingTriggerId(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FenablingTriggerId=AValue) then exit;
  FenablingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setnotes(AIndex : Integer; const AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setparameter(AIndex : Integer; const AValue : TVariableTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetparentFolderId(AIndex : Integer; const AValue : String); 

begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetscheduleEndMs(AIndex : Integer; const AValue : String); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetscheduleStartMs(AIndex : Integer; const AValue : String); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetvariableId(AIndex : Integer; const AValue : String); 

begin
  If (FvariableId=AValue) then exit;
  FvariableId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVariable.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariable.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'disablingtriggerid' : SetLength(FdisablingTriggerId,ALength);
  'enablingtriggerid' : SetLength(FenablingTriggerId,ALength);
  'parameter' : SetLength(Fparameter,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsContainersEnvironmentsResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersEnvironmentsResource.ResourceName : String;

begin
  Result:='environments';
end;

Class Function TAccountsContainersEnvironmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersEnvironmentsResource.Create(accountId: string; containerId: string; aEnvironment : TEnvironment) : TEnvironment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/environments';
  _Methodid   = 'tagmanager.accounts.containers.environments.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEnvironment,TEnvironment) as TEnvironment;
end;

Procedure TAccountsContainersEnvironmentsResource.Delete(accountId: string; containerId: string; environmentId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/environments/{environmentId}';
  _Methodid   = 'tagmanager.accounts.containers.environments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'environmentId',environmentId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersEnvironmentsResource.Get(accountId: string; containerId: string; environmentId: string) : TEnvironment;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/environments/{environmentId}';
  _Methodid   = 'tagmanager.accounts.containers.environments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'environmentId',environmentId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEnvironment) as TEnvironment;
end;

Function TAccountsContainersEnvironmentsResource.List(accountId: string; containerId: string) : TListEnvironmentsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/environments';
  _Methodid   = 'tagmanager.accounts.containers.environments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListEnvironmentsResponse) as TListEnvironmentsResponse;
end;

Function TAccountsContainersEnvironmentsResource.Patch(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : string = '') : TEnvironment;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'accounts/{accountId}/containers/{containerId}/environments/{environmentId}';
  _Methodid   = 'tagmanager.accounts.containers.environments.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'environmentId',environmentId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEnvironment,TEnvironment) as TEnvironment;
end;


Function TAccountsContainersEnvironmentsResource.Patch(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : TAccountsContainersEnvironmentspatchOptions) : TEnvironment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Patch(accountId,containerId,environmentId,aEnvironment,_Q);
end;

Function TAccountsContainersEnvironmentsResource.Update(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : string = '') : TEnvironment;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/environments/{environmentId}';
  _Methodid   = 'tagmanager.accounts.containers.environments.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'environmentId',environmentId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEnvironment,TEnvironment) as TEnvironment;
end;


Function TAccountsContainersEnvironmentsResource.Update(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment; AQuery : TAccountsContainersEnvironmentsupdateOptions) : TEnvironment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,environmentId,aEnvironment,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersFoldersEntitiesResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersFoldersEntitiesResource.ResourceName : String;

begin
  Result:='entities';
end;

Class Function TAccountsContainersFoldersEntitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersFoldersEntitiesResource.List(accountId: string; containerId: string; folderId: string) : TFolderEntities;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/folders/{folderId}/entities';
  _Methodid   = 'tagmanager.accounts.containers.folders.entities.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'folderId',folderId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFolderEntities) as TFolderEntities;
end;



{ --------------------------------------------------------------------
  TAccountsContainersFoldersResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersFoldersResource.ResourceName : String;

begin
  Result:='folders';
end;

Class Function TAccountsContainersFoldersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersFoldersResource.Create(accountId: string; containerId: string; aFolder : TFolder) : TFolder;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/folders';
  _Methodid   = 'tagmanager.accounts.containers.folders.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aFolder,TFolder) as TFolder;
end;

Procedure TAccountsContainersFoldersResource.Delete(accountId: string; containerId: string; folderId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/folders/{folderId}';
  _Methodid   = 'tagmanager.accounts.containers.folders.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'folderId',folderId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersFoldersResource.Get(accountId: string; containerId: string; folderId: string) : TFolder;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/folders/{folderId}';
  _Methodid   = 'tagmanager.accounts.containers.folders.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'folderId',folderId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TFolder) as TFolder;
end;

Function TAccountsContainersFoldersResource.List(accountId: string; containerId: string) : TListFoldersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/folders';
  _Methodid   = 'tagmanager.accounts.containers.folders.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListFoldersResponse) as TListFoldersResponse;
end;

Function TAccountsContainersFoldersResource.Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : string = '') : TFolder;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/folders/{folderId}';
  _Methodid   = 'tagmanager.accounts.containers.folders.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'folderId',folderId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aFolder,TFolder) as TFolder;
end;


Function TAccountsContainersFoldersResource.Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : TAccountsContainersFoldersupdateOptions) : TFolder;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,folderId,aFolder,_Q);
end;



Function TAccountsContainersFoldersResource.GetEntitiesInstance : TAccountsContainersFoldersEntitiesResource;

begin
  if (FEntitiesInstance=Nil) then
    FEntitiesInstance:=CreateEntitiesResource;
  Result:=FEntitiesInstance;
end;

Function TAccountsContainersFoldersResource.CreateEntitiesResource : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=CreateEntitiesResource(Self);
end;


Function TAccountsContainersFoldersResource.CreateEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=TAccountsContainersFoldersEntitiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsContainersMove_foldersResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersMove_foldersResource.ResourceName : String;

begin
  Result:='move_folders';
end;

Class Function TAccountsContainersMove_foldersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Procedure TAccountsContainersMove_foldersResource.Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : string = '');

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/move_folders/{folderId}';
  _Methodid   = 'tagmanager.accounts.containers.move_folders.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'folderId',folderId]);
  ServiceCall(_HTTPMethod,_P,AQuery,aFolder,Nil);
end;


Procedure TAccountsContainersMove_foldersResource.Update(accountId: string; containerId: string; folderId: string; aFolder : TFolder; AQuery : TAccountsContainersMove_foldersupdateOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'tagId',AQuery.tagId);
  AddToQuery(_Q,'triggerId',AQuery.triggerId);
  AddToQuery(_Q,'variableId',AQuery.variableId);
  Update(accountId,containerId,folderId,aFolder,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersReauthorize_environmentsResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersReauthorize_environmentsResource.ResourceName : String;

begin
  Result:='reauthorize_environments';
end;

Class Function TAccountsContainersReauthorize_environmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersReauthorize_environmentsResource.Update(accountId: string; containerId: string; environmentId: string; aEnvironment : TEnvironment) : TEnvironment;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/reauthorize_environments/{environmentId}';
  _Methodid   = 'tagmanager.accounts.containers.reauthorize_environments.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'environmentId',environmentId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEnvironment,TEnvironment) as TEnvironment;
end;



{ --------------------------------------------------------------------
  TAccountsContainersTagsResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersTagsResource.ResourceName : String;

begin
  Result:='tags';
end;

Class Function TAccountsContainersTagsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersTagsResource.Create(accountId: string; containerId: string; aTag : TTag) : TTag;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/tags';
  _Methodid   = 'tagmanager.accounts.containers.tags.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTag,TTag) as TTag;
end;

Procedure TAccountsContainersTagsResource.Delete(accountId: string; containerId: string; tagId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/tags/{tagId}';
  _Methodid   = 'tagmanager.accounts.containers.tags.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'tagId',tagId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersTagsResource.Get(accountId: string; containerId: string; tagId: string) : TTag;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/tags/{tagId}';
  _Methodid   = 'tagmanager.accounts.containers.tags.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'tagId',tagId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTag) as TTag;
end;

Function TAccountsContainersTagsResource.List(accountId: string; containerId: string) : TListTagsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/tags';
  _Methodid   = 'tagmanager.accounts.containers.tags.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListTagsResponse) as TListTagsResponse;
end;

Function TAccountsContainersTagsResource.Update(accountId: string; containerId: string; tagId: string; aTag : TTag; AQuery : string = '') : TTag;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/tags/{tagId}';
  _Methodid   = 'tagmanager.accounts.containers.tags.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'tagId',tagId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTag,TTag) as TTag;
end;


Function TAccountsContainersTagsResource.Update(accountId: string; containerId: string; tagId: string; aTag : TTag; AQuery : TAccountsContainersTagsupdateOptions) : TTag;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,tagId,aTag,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersTriggersResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersTriggersResource.ResourceName : String;

begin
  Result:='triggers';
end;

Class Function TAccountsContainersTriggersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersTriggersResource.Create(accountId: string; containerId: string; aTrigger : TTrigger) : TTrigger;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/triggers';
  _Methodid   = 'tagmanager.accounts.containers.triggers.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTrigger,TTrigger) as TTrigger;
end;

Procedure TAccountsContainersTriggersResource.Delete(accountId: string; containerId: string; triggerId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/triggers/{triggerId}';
  _Methodid   = 'tagmanager.accounts.containers.triggers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'triggerId',triggerId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersTriggersResource.Get(accountId: string; containerId: string; triggerId: string) : TTrigger;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/triggers/{triggerId}';
  _Methodid   = 'tagmanager.accounts.containers.triggers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'triggerId',triggerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTrigger) as TTrigger;
end;

Function TAccountsContainersTriggersResource.List(accountId: string; containerId: string) : TListTriggersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/triggers';
  _Methodid   = 'tagmanager.accounts.containers.triggers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListTriggersResponse) as TListTriggersResponse;
end;

Function TAccountsContainersTriggersResource.Update(accountId: string; containerId: string; triggerId: string; aTrigger : TTrigger; AQuery : string = '') : TTrigger;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/triggers/{triggerId}';
  _Methodid   = 'tagmanager.accounts.containers.triggers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'triggerId',triggerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aTrigger,TTrigger) as TTrigger;
end;


Function TAccountsContainersTriggersResource.Update(accountId: string; containerId: string; triggerId: string; aTrigger : TTrigger; AQuery : TAccountsContainersTriggersupdateOptions) : TTrigger;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,triggerId,aTrigger,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersVariablesResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersVariablesResource.ResourceName : String;

begin
  Result:='variables';
end;

Class Function TAccountsContainersVariablesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersVariablesResource.Create(accountId: string; containerId: string; aVariable : TVariable) : TVariable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/variables';
  _Methodid   = 'tagmanager.accounts.containers.variables.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVariable,TVariable) as TVariable;
end;

Procedure TAccountsContainersVariablesResource.Delete(accountId: string; containerId: string; variableId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/variables/{variableId}';
  _Methodid   = 'tagmanager.accounts.containers.variables.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'variableId',variableId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersVariablesResource.Get(accountId: string; containerId: string; variableId: string) : TVariable;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/variables/{variableId}';
  _Methodid   = 'tagmanager.accounts.containers.variables.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'variableId',variableId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVariable) as TVariable;
end;

Function TAccountsContainersVariablesResource.List(accountId: string; containerId: string) : TListVariablesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/variables';
  _Methodid   = 'tagmanager.accounts.containers.variables.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListVariablesResponse) as TListVariablesResponse;
end;

Function TAccountsContainersVariablesResource.Update(accountId: string; containerId: string; variableId: string; aVariable : TVariable; AQuery : string = '') : TVariable;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/variables/{variableId}';
  _Methodid   = 'tagmanager.accounts.containers.variables.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'variableId',variableId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aVariable,TVariable) as TVariable;
end;


Function TAccountsContainersVariablesResource.Update(accountId: string; containerId: string; variableId: string; aVariable : TVariable; AQuery : TAccountsContainersVariablesupdateOptions) : TVariable;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,variableId,aVariable,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersVersionsResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersVersionsResource.ResourceName : String;

begin
  Result:='versions';
end;

Class Function TAccountsContainersVersionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersVersionsResource.Create(accountId: string; containerId: string; aCreateContainerVersionRequestVersionOptions : TCreateContainerVersionRequestVersionOptions) : TCreateContainerVersionResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions';
  _Methodid   = 'tagmanager.accounts.containers.versions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateContainerVersionRequestVersionOptions,TCreateContainerVersionResponse) as TCreateContainerVersionResponse;
end;

Procedure TAccountsContainersVersionsResource.Delete(accountId: string; containerId: string; containerVersionId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions/{containerVersionId}';
  _Methodid   = 'tagmanager.accounts.containers.versions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'containerVersionId',containerVersionId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersVersionsResource.Get(accountId: string; containerId: string; containerVersionId: string) : TContainerVersion;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions/{containerVersionId}';
  _Methodid   = 'tagmanager.accounts.containers.versions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'containerVersionId',containerVersionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TContainerVersion) as TContainerVersion;
end;

Function TAccountsContainersVersionsResource.List(accountId: string; containerId: string; AQuery : string = '') : TListContainerVersionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions';
  _Methodid   = 'tagmanager.accounts.containers.versions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListContainerVersionsResponse) as TListContainerVersionsResponse;
end;


Function TAccountsContainersVersionsResource.List(accountId: string; containerId: string; AQuery : TAccountsContainersVersionslistOptions) : TListContainerVersionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'headers',AQuery.headers);
  AddToQuery(_Q,'includeDeleted',AQuery.includeDeleted);
  Result:=List(accountId,containerId,_Q);
end;

Function TAccountsContainersVersionsResource.Publish(accountId: string; containerId: string; containerVersionId: string; AQuery : string = '') : TPublishContainerVersionResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions/{containerVersionId}/publish';
  _Methodid   = 'tagmanager.accounts.containers.versions.publish';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'containerVersionId',containerVersionId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPublishContainerVersionResponse) as TPublishContainerVersionResponse;
end;


Function TAccountsContainersVersionsResource.Publish(accountId: string; containerId: string; containerVersionId: string; AQuery : TAccountsContainersVersionspublishOptions) : TPublishContainerVersionResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Publish(accountId,containerId,containerVersionId,_Q);
end;

Function TAccountsContainersVersionsResource.Restore(accountId: string; containerId: string; containerVersionId: string) : TContainerVersion;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions/{containerVersionId}/restore';
  _Methodid   = 'tagmanager.accounts.containers.versions.restore';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'containerVersionId',containerVersionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TContainerVersion) as TContainerVersion;
end;

Function TAccountsContainersVersionsResource.Undelete(accountId: string; containerId: string; containerVersionId: string) : TContainerVersion;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions/{containerVersionId}/undelete';
  _Methodid   = 'tagmanager.accounts.containers.versions.undelete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'containerVersionId',containerVersionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TContainerVersion) as TContainerVersion;
end;

Function TAccountsContainersVersionsResource.Update(accountId: string; containerId: string; containerVersionId: string; aContainerVersion : TContainerVersion; AQuery : string = '') : TContainerVersion;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/versions/{containerVersionId}';
  _Methodid   = 'tagmanager.accounts.containers.versions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'containerVersionId',containerVersionId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aContainerVersion,TContainerVersion) as TContainerVersion;
end;


Function TAccountsContainersVersionsResource.Update(accountId: string; containerId: string; containerVersionId: string; aContainerVersion : TContainerVersion; AQuery : TAccountsContainersVersionsupdateOptions) : TContainerVersion;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,containerVersionId,aContainerVersion,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersResource.ResourceName : String;

begin
  Result:='containers';
end;

Class Function TAccountsContainersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersResource.Create(accountId: string; aContainer : TContainer) : TContainer;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers';
  _Methodid   = 'tagmanager.accounts.containers.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aContainer,TContainer) as TContainer;
end;

Procedure TAccountsContainersResource.Delete(accountId: string; containerId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}';
  _Methodid   = 'tagmanager.accounts.containers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersResource.Get(accountId: string; containerId: string) : TContainer;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}';
  _Methodid   = 'tagmanager.accounts.containers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TContainer) as TContainer;
end;

Function TAccountsContainersResource.List(accountId: string) : TListContainersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers';
  _Methodid   = 'tagmanager.accounts.containers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListContainersResponse) as TListContainersResponse;
end;

Function TAccountsContainersResource.Update(accountId: string; containerId: string; aContainer : TContainer; AQuery : string = '') : TContainer;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}';
  _Methodid   = 'tagmanager.accounts.containers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aContainer,TContainer) as TContainer;
end;


Function TAccountsContainersResource.Update(accountId: string; containerId: string; aContainer : TContainer; AQuery : TAccountsContainersupdateOptions) : TContainer;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,aContainer,_Q);
end;



Function TAccountsContainersResource.GetEnvironmentsInstance : TAccountsContainersEnvironmentsResource;

begin
  if (FEnvironmentsInstance=Nil) then
    FEnvironmentsInstance:=CreateEnvironmentsResource;
  Result:=FEnvironmentsInstance;
end;

Function TAccountsContainersResource.CreateEnvironmentsResource : TAccountsContainersEnvironmentsResource;

begin
  Result:=CreateEnvironmentsResource(Self);
end;


Function TAccountsContainersResource.CreateEnvironmentsResource(AOwner : TComponent) : TAccountsContainersEnvironmentsResource;

begin
  Result:=TAccountsContainersEnvironmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;

begin
  if (FFoldersEntitiesInstance=Nil) then
    FFoldersEntitiesInstance:=CreateFoldersEntitiesResource;
  Result:=FFoldersEntitiesInstance;
end;

Function TAccountsContainersResource.CreateFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=CreateFoldersEntitiesResource(Self);
end;


Function TAccountsContainersResource.CreateFoldersEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=TAccountsContainersFoldersEntitiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetFoldersInstance : TAccountsContainersFoldersResource;

begin
  if (FFoldersInstance=Nil) then
    FFoldersInstance:=CreateFoldersResource;
  Result:=FFoldersInstance;
end;

Function TAccountsContainersResource.CreateFoldersResource : TAccountsContainersFoldersResource;

begin
  Result:=CreateFoldersResource(Self);
end;


Function TAccountsContainersResource.CreateFoldersResource(AOwner : TComponent) : TAccountsContainersFoldersResource;

begin
  Result:=TAccountsContainersFoldersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetMove_foldersInstance : TAccountsContainersMove_foldersResource;

begin
  if (FMove_foldersInstance=Nil) then
    FMove_foldersInstance:=CreateMove_foldersResource;
  Result:=FMove_foldersInstance;
end;

Function TAccountsContainersResource.CreateMove_foldersResource : TAccountsContainersMove_foldersResource;

begin
  Result:=CreateMove_foldersResource(Self);
end;


Function TAccountsContainersResource.CreateMove_foldersResource(AOwner : TComponent) : TAccountsContainersMove_foldersResource;

begin
  Result:=TAccountsContainersMove_foldersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;

begin
  if (FReauthorize_environmentsInstance=Nil) then
    FReauthorize_environmentsInstance:=CreateReauthorize_environmentsResource;
  Result:=FReauthorize_environmentsInstance;
end;

Function TAccountsContainersResource.CreateReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource;

begin
  Result:=CreateReauthorize_environmentsResource(Self);
end;


Function TAccountsContainersResource.CreateReauthorize_environmentsResource(AOwner : TComponent) : TAccountsContainersReauthorize_environmentsResource;

begin
  Result:=TAccountsContainersReauthorize_environmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetTagsInstance : TAccountsContainersTagsResource;

begin
  if (FTagsInstance=Nil) then
    FTagsInstance:=CreateTagsResource;
  Result:=FTagsInstance;
end;

Function TAccountsContainersResource.CreateTagsResource : TAccountsContainersTagsResource;

begin
  Result:=CreateTagsResource(Self);
end;


Function TAccountsContainersResource.CreateTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;

begin
  Result:=TAccountsContainersTagsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetTriggersInstance : TAccountsContainersTriggersResource;

begin
  if (FTriggersInstance=Nil) then
    FTriggersInstance:=CreateTriggersResource;
  Result:=FTriggersInstance;
end;

Function TAccountsContainersResource.CreateTriggersResource : TAccountsContainersTriggersResource;

begin
  Result:=CreateTriggersResource(Self);
end;


Function TAccountsContainersResource.CreateTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;

begin
  Result:=TAccountsContainersTriggersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetVariablesInstance : TAccountsContainersVariablesResource;

begin
  if (FVariablesInstance=Nil) then
    FVariablesInstance:=CreateVariablesResource;
  Result:=FVariablesInstance;
end;

Function TAccountsContainersResource.CreateVariablesResource : TAccountsContainersVariablesResource;

begin
  Result:=CreateVariablesResource(Self);
end;


Function TAccountsContainersResource.CreateVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;

begin
  Result:=TAccountsContainersVariablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetVersionsInstance : TAccountsContainersVersionsResource;

begin
  if (FVersionsInstance=Nil) then
    FVersionsInstance:=CreateVersionsResource;
  Result:=FVersionsInstance;
end;

Function TAccountsContainersResource.CreateVersionsResource : TAccountsContainersVersionsResource;

begin
  Result:=CreateVersionsResource(Self);
end;


Function TAccountsContainersResource.CreateVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;

begin
  Result:=TAccountsContainersVersionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsPermissionsResource
  --------------------------------------------------------------------}


Class Function TAccountsPermissionsResource.ResourceName : String;

begin
  Result:='permissions';
end;

Class Function TAccountsPermissionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsPermissionsResource.Create(accountId: string; aUserAccess : TUserAccess) : TUserAccess;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/permissions';
  _Methodid   = 'tagmanager.accounts.permissions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUserAccess,TUserAccess) as TUserAccess;
end;

Procedure TAccountsPermissionsResource.Delete(accountId: string; permissionId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/permissions/{permissionId}';
  _Methodid   = 'tagmanager.accounts.permissions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'permissionId',permissionId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsPermissionsResource.Get(accountId: string; permissionId: string) : TUserAccess;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/permissions/{permissionId}';
  _Methodid   = 'tagmanager.accounts.permissions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'permissionId',permissionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUserAccess) as TUserAccess;
end;

Function TAccountsPermissionsResource.List(accountId: string) : TListAccountUsersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/permissions';
  _Methodid   = 'tagmanager.accounts.permissions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListAccountUsersResponse) as TListAccountUsersResponse;
end;

Function TAccountsPermissionsResource.Update(accountId: string; permissionId: string; aUserAccess : TUserAccess) : TUserAccess;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/permissions/{permissionId}';
  _Methodid   = 'tagmanager.accounts.permissions.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'permissionId',permissionId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUserAccess,TUserAccess) as TUserAccess;
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
  Result:=TtagmanagerAPI;
end;

Function TAccountsResource.Get(accountId: string) : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}';
  _Methodid   = 'tagmanager.accounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccount) as TAccount;
end;

Function TAccountsResource.List : TListAccountsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts';
  _Methodid   = 'tagmanager.accounts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TListAccountsResponse) as TListAccountsResponse;
end;

Function TAccountsResource.Update(accountId: string; aAccount : TAccount; AQuery : string = '') : TAccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}';
  _Methodid   = 'tagmanager.accounts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAccount,TAccount) as TAccount;
end;


Function TAccountsResource.Update(accountId: string; aAccount : TAccount; AQuery : TAccountsupdateOptions) : TAccount;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,aAccount,_Q);
end;



Function TAccountsResource.GetContainersEnvironmentsInstance : TAccountsContainersEnvironmentsResource;

begin
  if (FContainersEnvironmentsInstance=Nil) then
    FContainersEnvironmentsInstance:=CreateContainersEnvironmentsResource;
  Result:=FContainersEnvironmentsInstance;
end;

Function TAccountsResource.CreateContainersEnvironmentsResource : TAccountsContainersEnvironmentsResource;

begin
  Result:=CreateContainersEnvironmentsResource(Self);
end;


Function TAccountsResource.CreateContainersEnvironmentsResource(AOwner : TComponent) : TAccountsContainersEnvironmentsResource;

begin
  Result:=TAccountsContainersEnvironmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;

begin
  if (FContainersFoldersEntitiesInstance=Nil) then
    FContainersFoldersEntitiesInstance:=CreateContainersFoldersEntitiesResource;
  Result:=FContainersFoldersEntitiesInstance;
end;

Function TAccountsResource.CreateContainersFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=CreateContainersFoldersEntitiesResource(Self);
end;


Function TAccountsResource.CreateContainersFoldersEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=TAccountsContainersFoldersEntitiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersFoldersInstance : TAccountsContainersFoldersResource;

begin
  if (FContainersFoldersInstance=Nil) then
    FContainersFoldersInstance:=CreateContainersFoldersResource;
  Result:=FContainersFoldersInstance;
end;

Function TAccountsResource.CreateContainersFoldersResource : TAccountsContainersFoldersResource;

begin
  Result:=CreateContainersFoldersResource(Self);
end;


Function TAccountsResource.CreateContainersFoldersResource(AOwner : TComponent) : TAccountsContainersFoldersResource;

begin
  Result:=TAccountsContainersFoldersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersMove_foldersInstance : TAccountsContainersMove_foldersResource;

begin
  if (FContainersMove_foldersInstance=Nil) then
    FContainersMove_foldersInstance:=CreateContainersMove_foldersResource;
  Result:=FContainersMove_foldersInstance;
end;

Function TAccountsResource.CreateContainersMove_foldersResource : TAccountsContainersMove_foldersResource;

begin
  Result:=CreateContainersMove_foldersResource(Self);
end;


Function TAccountsResource.CreateContainersMove_foldersResource(AOwner : TComponent) : TAccountsContainersMove_foldersResource;

begin
  Result:=TAccountsContainersMove_foldersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;

begin
  if (FContainersReauthorize_environmentsInstance=Nil) then
    FContainersReauthorize_environmentsInstance:=CreateContainersReauthorize_environmentsResource;
  Result:=FContainersReauthorize_environmentsInstance;
end;

Function TAccountsResource.CreateContainersReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource;

begin
  Result:=CreateContainersReauthorize_environmentsResource(Self);
end;


Function TAccountsResource.CreateContainersReauthorize_environmentsResource(AOwner : TComponent) : TAccountsContainersReauthorize_environmentsResource;

begin
  Result:=TAccountsContainersReauthorize_environmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersTagsInstance : TAccountsContainersTagsResource;

begin
  if (FContainersTagsInstance=Nil) then
    FContainersTagsInstance:=CreateContainersTagsResource;
  Result:=FContainersTagsInstance;
end;

Function TAccountsResource.CreateContainersTagsResource : TAccountsContainersTagsResource;

begin
  Result:=CreateContainersTagsResource(Self);
end;


Function TAccountsResource.CreateContainersTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;

begin
  Result:=TAccountsContainersTagsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersTriggersInstance : TAccountsContainersTriggersResource;

begin
  if (FContainersTriggersInstance=Nil) then
    FContainersTriggersInstance:=CreateContainersTriggersResource;
  Result:=FContainersTriggersInstance;
end;

Function TAccountsResource.CreateContainersTriggersResource : TAccountsContainersTriggersResource;

begin
  Result:=CreateContainersTriggersResource(Self);
end;


Function TAccountsResource.CreateContainersTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;

begin
  Result:=TAccountsContainersTriggersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersVariablesInstance : TAccountsContainersVariablesResource;

begin
  if (FContainersVariablesInstance=Nil) then
    FContainersVariablesInstance:=CreateContainersVariablesResource;
  Result:=FContainersVariablesInstance;
end;

Function TAccountsResource.CreateContainersVariablesResource : TAccountsContainersVariablesResource;

begin
  Result:=CreateContainersVariablesResource(Self);
end;


Function TAccountsResource.CreateContainersVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;

begin
  Result:=TAccountsContainersVariablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersVersionsInstance : TAccountsContainersVersionsResource;

begin
  if (FContainersVersionsInstance=Nil) then
    FContainersVersionsInstance:=CreateContainersVersionsResource;
  Result:=FContainersVersionsInstance;
end;

Function TAccountsResource.CreateContainersVersionsResource : TAccountsContainersVersionsResource;

begin
  Result:=CreateContainersVersionsResource(Self);
end;


Function TAccountsResource.CreateContainersVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;

begin
  Result:=TAccountsContainersVersionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersInstance : TAccountsContainersResource;

begin
  if (FContainersInstance=Nil) then
    FContainersInstance:=CreateContainersResource;
  Result:=FContainersInstance;
end;

Function TAccountsResource.CreateContainersResource : TAccountsContainersResource;

begin
  Result:=CreateContainersResource(Self);
end;


Function TAccountsResource.CreateContainersResource(AOwner : TComponent) : TAccountsContainersResource;

begin
  Result:=TAccountsContainersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetPermissionsInstance : TAccountsPermissionsResource;

begin
  if (FPermissionsInstance=Nil) then
    FPermissionsInstance:=CreatePermissionsResource;
  Result:=FPermissionsInstance;
end;

Function TAccountsResource.CreatePermissionsResource : TAccountsPermissionsResource;

begin
  Result:=CreatePermissionsResource(Self);
end;


Function TAccountsResource.CreatePermissionsResource(AOwner : TComponent) : TAccountsPermissionsResource;

begin
  Result:=TAccountsPermissionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TTagmanagerAPI
  --------------------------------------------------------------------}

Class Function TTagmanagerAPI.APIName : String;

begin
  Result:='tagmanager';
end;

Class Function TTagmanagerAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TTagmanagerAPI.APIRevision : String;

begin
  Result:='20160310';
end;

Class Function TTagmanagerAPI.APIID : String;

begin
  Result:='tagmanager:v1';
end;

Class Function TTagmanagerAPI.APITitle : String;

begin
  Result:='Tag Manager API';
end;

Class Function TTagmanagerAPI.APIDescription : String;

begin
  Result:='Accesses Tag Manager accounts and containers.';
end;

Class Function TTagmanagerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TTagmanagerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TTagmanagerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TTagmanagerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TTagmanagerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/tag-manager/api/v1/';
end;

Class Function TTagmanagerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TTagmanagerAPI.APIbasePath : string;

begin
  Result:='/tagmanager/v1/';
end;

Class Function TTagmanagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/tagmanager/v1/';
end;

Class Function TTagmanagerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TTagmanagerAPI.APIservicePath : string;

begin
  Result:='tagmanager/v1/';
end;

Class Function TTagmanagerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TTagmanagerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,7);
  Result[0].Name:='https://www.googleapis.com/auth/tagmanager.delete.containers';
  Result[0].Description:='Delete your Google Tag Manager containers';
  Result[1].Name:='https://www.googleapis.com/auth/tagmanager.edit.containers';
  Result[1].Description:='Manage your Google Tag Manager containers';
  Result[2].Name:='https://www.googleapis.com/auth/tagmanager.edit.containerversions';
  Result[2].Description:='Manage your Google Tag Manager container versions';
  Result[3].Name:='https://www.googleapis.com/auth/tagmanager.manage.accounts';
  Result[3].Description:='Manage your Google Tag Manager accounts';
  Result[4].Name:='https://www.googleapis.com/auth/tagmanager.manage.users';
  Result[4].Description:='Manage user permissions of your Google Tag Manager data';
  Result[5].Name:='https://www.googleapis.com/auth/tagmanager.publish';
  Result[5].Description:='Publish your Google Tag Manager containers';
  Result[6].Name:='https://www.googleapis.com/auth/tagmanager.readonly';
  Result[6].Description:='View your Google Tag Manager containers';
  
end;

Class Function TTagmanagerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TTagmanagerAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
  TAccountAccess.RegisterObject;
  TCondition.RegisterObject;
  TContainer.RegisterObject;
  TContainerAccess.RegisterObject;
  TContainerVersion.RegisterObject;
  TContainerVersionHeader.RegisterObject;
  TCreateContainerVersionRequestVersionOptions.RegisterObject;
  TCreateContainerVersionResponse.RegisterObject;
  TEnvironment.RegisterObject;
  TFolder.RegisterObject;
  TFolderEntities.RegisterObject;
  TListAccountUsersResponse.RegisterObject;
  TListAccountsResponse.RegisterObject;
  TListContainerVersionsResponse.RegisterObject;
  TListContainersResponse.RegisterObject;
  TListEnvironmentsResponse.RegisterObject;
  TListFoldersResponse.RegisterObject;
  TListTagsResponse.RegisterObject;
  TListTriggersResponse.RegisterObject;
  TListVariablesResponse.RegisterObject;
  TMacro.RegisterObject;
  TParameter.RegisterObject;
  TPublishContainerVersionResponse.RegisterObject;
  TRule.RegisterObject;
  TSetupTag.RegisterObject;
  TTag.RegisterObject;
  TTeardownTag.RegisterObject;
  TTrigger.RegisterObject;
  TUserAccess.RegisterObject;
  TVariable.RegisterObject;
end;


Function TTagmanagerAPI.GetAccountsContainersEnvironmentsInstance : TAccountsContainersEnvironmentsResource;

begin
  if (FAccountsContainersEnvironmentsInstance=Nil) then
    FAccountsContainersEnvironmentsInstance:=CreateAccountsContainersEnvironmentsResource;
  Result:=FAccountsContainersEnvironmentsInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersEnvironmentsResource : TAccountsContainersEnvironmentsResource;

begin
  Result:=CreateAccountsContainersEnvironmentsResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersEnvironmentsResource(AOwner : TComponent) : TAccountsContainersEnvironmentsResource;

begin
  Result:=TAccountsContainersEnvironmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersFoldersEntitiesInstance : TAccountsContainersFoldersEntitiesResource;

begin
  if (FAccountsContainersFoldersEntitiesInstance=Nil) then
    FAccountsContainersFoldersEntitiesInstance:=CreateAccountsContainersFoldersEntitiesResource;
  Result:=FAccountsContainersFoldersEntitiesInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersFoldersEntitiesResource : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=CreateAccountsContainersFoldersEntitiesResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersFoldersEntitiesResource(AOwner : TComponent) : TAccountsContainersFoldersEntitiesResource;

begin
  Result:=TAccountsContainersFoldersEntitiesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersFoldersInstance : TAccountsContainersFoldersResource;

begin
  if (FAccountsContainersFoldersInstance=Nil) then
    FAccountsContainersFoldersInstance:=CreateAccountsContainersFoldersResource;
  Result:=FAccountsContainersFoldersInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersFoldersResource : TAccountsContainersFoldersResource;

begin
  Result:=CreateAccountsContainersFoldersResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersFoldersResource(AOwner : TComponent) : TAccountsContainersFoldersResource;

begin
  Result:=TAccountsContainersFoldersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersMove_foldersInstance : TAccountsContainersMove_foldersResource;

begin
  if (FAccountsContainersMove_foldersInstance=Nil) then
    FAccountsContainersMove_foldersInstance:=CreateAccountsContainersMove_foldersResource;
  Result:=FAccountsContainersMove_foldersInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersMove_foldersResource : TAccountsContainersMove_foldersResource;

begin
  Result:=CreateAccountsContainersMove_foldersResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersMove_foldersResource(AOwner : TComponent) : TAccountsContainersMove_foldersResource;

begin
  Result:=TAccountsContainersMove_foldersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersReauthorize_environmentsInstance : TAccountsContainersReauthorize_environmentsResource;

begin
  if (FAccountsContainersReauthorize_environmentsInstance=Nil) then
    FAccountsContainersReauthorize_environmentsInstance:=CreateAccountsContainersReauthorize_environmentsResource;
  Result:=FAccountsContainersReauthorize_environmentsInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersReauthorize_environmentsResource : TAccountsContainersReauthorize_environmentsResource;

begin
  Result:=CreateAccountsContainersReauthorize_environmentsResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersReauthorize_environmentsResource(AOwner : TComponent) : TAccountsContainersReauthorize_environmentsResource;

begin
  Result:=TAccountsContainersReauthorize_environmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersTagsInstance : TAccountsContainersTagsResource;

begin
  if (FAccountsContainersTagsInstance=Nil) then
    FAccountsContainersTagsInstance:=CreateAccountsContainersTagsResource;
  Result:=FAccountsContainersTagsInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersTagsResource : TAccountsContainersTagsResource;

begin
  Result:=CreateAccountsContainersTagsResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;

begin
  Result:=TAccountsContainersTagsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersTriggersInstance : TAccountsContainersTriggersResource;

begin
  if (FAccountsContainersTriggersInstance=Nil) then
    FAccountsContainersTriggersInstance:=CreateAccountsContainersTriggersResource;
  Result:=FAccountsContainersTriggersInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersTriggersResource : TAccountsContainersTriggersResource;

begin
  Result:=CreateAccountsContainersTriggersResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;

begin
  Result:=TAccountsContainersTriggersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersVariablesInstance : TAccountsContainersVariablesResource;

begin
  if (FAccountsContainersVariablesInstance=Nil) then
    FAccountsContainersVariablesInstance:=CreateAccountsContainersVariablesResource;
  Result:=FAccountsContainersVariablesInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersVariablesResource : TAccountsContainersVariablesResource;

begin
  Result:=CreateAccountsContainersVariablesResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;

begin
  Result:=TAccountsContainersVariablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersVersionsInstance : TAccountsContainersVersionsResource;

begin
  if (FAccountsContainersVersionsInstance=Nil) then
    FAccountsContainersVersionsInstance:=CreateAccountsContainersVersionsResource;
  Result:=FAccountsContainersVersionsInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersVersionsResource : TAccountsContainersVersionsResource;

begin
  Result:=CreateAccountsContainersVersionsResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;

begin
  Result:=TAccountsContainersVersionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersInstance : TAccountsContainersResource;

begin
  if (FAccountsContainersInstance=Nil) then
    FAccountsContainersInstance:=CreateAccountsContainersResource;
  Result:=FAccountsContainersInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersResource : TAccountsContainersResource;

begin
  Result:=CreateAccountsContainersResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersResource(AOwner : TComponent) : TAccountsContainersResource;

begin
  Result:=TAccountsContainersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsPermissionsInstance : TAccountsPermissionsResource;

begin
  if (FAccountsPermissionsInstance=Nil) then
    FAccountsPermissionsInstance:=CreateAccountsPermissionsResource;
  Result:=FAccountsPermissionsInstance;
end;

Function TTagmanagerAPI.CreateAccountsPermissionsResource : TAccountsPermissionsResource;

begin
  Result:=CreateAccountsPermissionsResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsPermissionsResource(AOwner : TComponent) : TAccountsPermissionsResource;

begin
  Result:=TAccountsPermissionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TTagmanagerAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TTagmanagerAPI.RegisterAPI;
end.
