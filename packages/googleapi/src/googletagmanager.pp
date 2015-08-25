unit googletagmanager;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 16-5-15 08:53:08
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
  TListAccountUsersResponse = Class;
  TListAccountsResponse = Class;
  TListContainerVersionsResponse = Class;
  TListContainersResponse = Class;
  TListMacrosResponse = Class;
  TListRulesResponse = Class;
  TListTagsResponse = Class;
  TListTriggersResponse = Class;
  TListVariablesResponse = Class;
  TMacro = Class;
  TParameter = Class;
  TPublishContainerVersionResponse = Class;
  TRule = Class;
  TTag = Class;
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
  TListAccountUsersResponseArray = Array of TListAccountUsersResponse;
  TListAccountsResponseArray = Array of TListAccountsResponse;
  TListContainerVersionsResponseArray = Array of TListContainerVersionsResponse;
  TListContainersResponseArray = Array of TListContainersResponse;
  TListMacrosResponseArray = Array of TListMacrosResponse;
  TListRulesResponseArray = Array of TListRulesResponse;
  TListTagsResponseArray = Array of TListTagsResponse;
  TListTriggersResponseArray = Array of TListTriggersResponse;
  TListVariablesResponseArray = Array of TListVariablesResponse;
  TMacroArray = Array of TMacro;
  TParameterArray = Array of TParameter;
  TPublishContainerVersionResponseArray = Array of TPublishContainerVersionResponse;
  TRuleArray = Array of TRule;
  TTagArray = Array of TTag;
  TTriggerArray = Array of TTrigger;
  TUserAccessArray = Array of TUserAccess;
  TVariableArray = Array of TVariable;
  //Anonymous types, using auto-generated names
  TConditionTypeparameterArray = Array of TParameter;
  TContainerVersionTypemacroArray = Array of TMacro;
  TContainerVersionTyperuleArray = Array of TRule;
  TContainerVersionTypetagArray = Array of TTag;
  TContainerVersionTypetriggerArray = Array of TTrigger;
  TContainerVersionTypevariableArray = Array of TVariable;
  TListAccountUsersResponseTypeuserAccessArray = Array of TUserAccess;
  TListAccountsResponseTypeaccountsArray = Array of TAccount;
  TListContainerVersionsResponseTypecontainerVersionArray = Array of TContainerVersion;
  TListContainerVersionsResponseTypecontainerVersionHeaderArray = Array of TContainerVersionHeader;
  TListContainersResponseTypecontainersArray = Array of TContainer;
  TListMacrosResponseTypemacrosArray = Array of TMacro;
  TListRulesResponseTyperulesArray = Array of TRule;
  TListTagsResponseTypetagsArray = Array of TTag;
  TListTriggersResponseTypetriggersArray = Array of TTrigger;
  TListVariablesResponseTypevariablesArray = Array of TVariable;
  TMacroTypeparameterArray = Array of TParameter;
  TParameterTypelistArray = Array of TParameter;
  TParameterTypemapArray = Array of TParameter;
  TRuleTypeconditionArray = Array of TCondition;
  TTagTypeparameterArray = Array of TParameter;
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
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetshareData(AIndex : Integer; AValue : boolean); virtual;
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
    Procedure Setpermission(AIndex : Integer; AValue : TStringArray); virtual;
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
    Procedure Setparameter(AIndex : Integer; AValue : TConditionTypeparameterArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdomainName(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetenabledBuiltInVariable(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure SetpublicId(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeZoneCountryId(AIndex : Integer; AValue : String); virtual;
    Procedure SettimeZoneId(AIndex : Integer; AValue : String); virtual;
    Procedure SetusageContext(AIndex : Integer; AValue : TStringArray); virtual;
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
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setpermission(AIndex : Integer; AValue : TStringArray); virtual;
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
    Fmacro : TContainerVersionTypemacroArray;
    Fname : String;
    Fnotes : String;
    Frule : TContainerVersionTyperuleArray;
    Ftag : TContainerVersionTypetagArray;
    Ftrigger : TContainerVersionTypetriggerArray;
    Fvariable : TContainerVersionTypevariableArray;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setcontainer(AIndex : Integer; AValue : TContainer); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setmacro(AIndex : Integer; AValue : TContainerVersionTypemacroArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure Setrule(AIndex : Integer; AValue : TContainerVersionTyperuleArray); virtual;
    Procedure Settag(AIndex : Integer; AValue : TContainerVersionTypetagArray); virtual;
    Procedure Settrigger(AIndex : Integer; AValue : TContainerVersionTypetriggerArray); virtual;
    Procedure Setvariable(AIndex : Integer; AValue : TContainerVersionTypevariableArray); virtual;
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
    Property macro : TContainerVersionTypemacroArray Index 48 Read Fmacro Write Setmacro;
    Property name : String Index 56 Read Fname Write Setname;
    Property notes : String Index 64 Read Fnotes Write Setnotes;
    Property rule : TContainerVersionTyperuleArray Index 72 Read Frule Write Setrule;
    Property tag : TContainerVersionTypetagArray Index 80 Read Ftag Write Settag;
    Property trigger : TContainerVersionTypetriggerArray Index 88 Read Ftrigger Write Settrigger;
    Property variable : TContainerVersionTypevariableArray Index 96 Read Fvariable Write Setvariable;
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
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerVersionId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumMacros(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumRules(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumTags(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumTriggers(AIndex : Integer; AValue : String); virtual;
    Procedure SetnumVariables(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure SetquickPreview(AIndex : Integer; AValue : boolean); virtual;
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
    Procedure SetcompilerError(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcontainerVersion(AIndex : Integer; AValue : TContainerVersion); virtual;
  Public
  Published
    Property compilerError : boolean Index 0 Read FcompilerError Write SetcompilerError;
    Property containerVersion : TContainerVersion Index 8 Read FcontainerVersion Write SetcontainerVersion;
  end;
  TCreateContainerVersionResponseClass = Class of TCreateContainerVersionResponse;
  
  { --------------------------------------------------------------------
    TListAccountUsersResponse
    --------------------------------------------------------------------}
  
  TListAccountUsersResponse = Class(TGoogleBaseObject)
  Private
    FuserAccess : TListAccountUsersResponseTypeuserAccessArray;
  Protected
    //Property setters
    Procedure SetuserAccess(AIndex : Integer; AValue : TListAccountUsersResponseTypeuserAccessArray); virtual;
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
    Procedure Setaccounts(AIndex : Integer; AValue : TListAccountsResponseTypeaccountsArray); virtual;
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
    Procedure SetcontainerVersion(AIndex : Integer; AValue : TListContainerVersionsResponseTypecontainerVersionArray); virtual;
    Procedure SetcontainerVersionHeader(AIndex : Integer; AValue : TListContainerVersionsResponseTypecontainerVersionHeaderArray); virtual;
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
    Procedure Setcontainers(AIndex : Integer; AValue : TListContainersResponseTypecontainersArray); virtual;
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
    TListMacrosResponse
    --------------------------------------------------------------------}
  
  TListMacrosResponse = Class(TGoogleBaseObject)
  Private
    Fmacros : TListMacrosResponseTypemacrosArray;
  Protected
    //Property setters
    Procedure Setmacros(AIndex : Integer; AValue : TListMacrosResponseTypemacrosArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property macros : TListMacrosResponseTypemacrosArray Index 0 Read Fmacros Write Setmacros;
  end;
  TListMacrosResponseClass = Class of TListMacrosResponse;
  
  { --------------------------------------------------------------------
    TListRulesResponse
    --------------------------------------------------------------------}
  
  TListRulesResponse = Class(TGoogleBaseObject)
  Private
    Frules : TListRulesResponseTyperulesArray;
  Protected
    //Property setters
    Procedure Setrules(AIndex : Integer; AValue : TListRulesResponseTyperulesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property rules : TListRulesResponseTyperulesArray Index 0 Read Frules Write Setrules;
  end;
  TListRulesResponseClass = Class of TListRulesResponse;
  
  { --------------------------------------------------------------------
    TListTagsResponse
    --------------------------------------------------------------------}
  
  TListTagsResponse = Class(TGoogleBaseObject)
  Private
    Ftags : TListTagsResponseTypetagsArray;
  Protected
    //Property setters
    Procedure Settags(AIndex : Integer; AValue : TListTagsResponseTypetagsArray); virtual;
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
    Procedure Settriggers(AIndex : Integer; AValue : TListTriggersResponseTypetriggersArray); virtual;
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
    Procedure Setvariables(AIndex : Integer; AValue : TListVariablesResponseTypevariablesArray); virtual;
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
    FscheduleEndMs : String;
    FscheduleStartMs : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisablingRuleId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetenablingRuleId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure SetmacroId(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure Setparameter(AIndex : Integer; AValue : TMacroTypeparameterArray); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
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
    Property scheduleEndMs : String Index 72 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : String Index 80 Read FscheduleStartMs Write SetscheduleStartMs;
    Property _type : String Index 88 Read F_type Write Set_type;
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
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setlist(AIndex : Integer; AValue : TParameterTypelistArray); virtual;
    Procedure Setmap(AIndex : Integer; AValue : TParameterTypemapArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetcompilerError(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcontainerVersion(AIndex : Integer; AValue : TContainerVersion); virtual;
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
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure Setcondition(AIndex : Integer; AValue : TRuleTypeconditionArray); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure SetruleId(AIndex : Integer; AValue : String); virtual;
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
    Fpriority : TParameter;
    FscheduleEndMs : String;
    FscheduleStartMs : String;
    FtagId : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetblockingRuleId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetblockingTriggerId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure SetfiringRuleId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetfiringTriggerId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetliveOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure Setparameter(AIndex : Integer; AValue : TTagTypeparameterArray); virtual;
    Procedure Setpriority(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; AValue : String); virtual;
    Procedure SettagId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
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
    Property priority : TParameter Index 88 Read Fpriority Write Setpriority;
    Property scheduleEndMs : String Index 96 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : String Index 104 Read FscheduleStartMs Write SetscheduleStartMs;
    Property tagId : String Index 112 Read FtagId Write SettagId;
    Property _type : String Index 120 Read F_type Write Set_type;
  end;
  TTagClass = Class of TTag;
  
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
    FtriggerId : String;
    F_type : String;
    FuniqueTriggerId : TParameter;
    FvideoPercentageList : TParameter;
    FwaitForTags : TParameter;
    FwaitForTagsTimeout : TParameter;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetautoEventFilter(AIndex : Integer; AValue : TTriggerTypeautoEventFilterArray); virtual;
    Procedure SetcheckValidation(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomEventFilter(AIndex : Integer; AValue : TTriggerTypecustomEventFilterArray); virtual;
    Procedure SetenableAllVideos(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SeteventName(AIndex : Integer; AValue : TParameter); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : TTriggerTypefilterArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setinterval(AIndex : Integer; AValue : TParameter); virtual;
    Procedure Setlimit(AIndex : Integer; AValue : TParameter); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SettriggerId(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetuniqueTriggerId(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetvideoPercentageList(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetwaitForTags(AIndex : Integer; AValue : TParameter); virtual;
    Procedure SetwaitForTagsTimeout(AIndex : Integer; AValue : TParameter); virtual;
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
    Property triggerId : String Index 96 Read FtriggerId Write SettriggerId;
    Property _type : String Index 104 Read F_type Write Set_type;
    Property uniqueTriggerId : TParameter Index 112 Read FuniqueTriggerId Write SetuniqueTriggerId;
    Property videoPercentageList : TParameter Index 120 Read FvideoPercentageList Write SetvideoPercentageList;
    Property waitForTags : TParameter Index 128 Read FwaitForTags Write SetwaitForTags;
    Property waitForTagsTimeout : TParameter Index 136 Read FwaitForTagsTimeout Write SetwaitForTagsTimeout;
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
    Procedure SetaccountAccess(AIndex : Integer; AValue : TAccountAccess); virtual;
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerAccess(AIndex : Integer; AValue : TUserAccessTypecontainerAccessArray); virtual;
    Procedure SetemailAddress(AIndex : Integer; AValue : String); virtual;
    Procedure SetpermissionId(AIndex : Integer; AValue : String); virtual;
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
    FscheduleEndMs : String;
    FscheduleStartMs : String;
    F_type : String;
    FvariableId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisablingTriggerId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetenablingTriggerId(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnotes(AIndex : Integer; AValue : String); virtual;
    Procedure Setparameter(AIndex : Integer; AValue : TVariableTypeparameterArray); virtual;
    Procedure SetscheduleEndMs(AIndex : Integer; AValue : String); virtual;
    Procedure SetscheduleStartMs(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariableId(AIndex : Integer; AValue : String); virtual;
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
    Property scheduleEndMs : String Index 64 Read FscheduleEndMs Write SetscheduleEndMs;
    Property scheduleStartMs : String Index 72 Read FscheduleStartMs Write SetscheduleStartMs;
    Property _type : String Index 80 Read F_type Write Set_type;
    Property variableId : String Index 88 Read FvariableId Write SetvariableId;
  end;
  TVariableClass = Class of TVariable;
  
  { --------------------------------------------------------------------
    TAccountsContainersMacrosResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersMacrosResource, method Update
  
  TAccountsContainersMacrosUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersMacrosResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aMacro : TMacro) : TMacro;overload;
    Procedure Delete(accountId: string; containerId: string; macroId: string);
    Function Get(accountId: string; containerId: string; macroId: string) : TMacro;
    Function List(accountId: string; containerId: string) : TListMacrosResponse;
    Function Update(accountId: string; containerId: string; macroId: string; aMacro : TMacro; AQuery : string  = '') : TMacro;
    Function Update(accountId: string; containerId: string; macroId: string; aMacro : TMacro; AQuery : TAccountsContainersMacrosupdateOptions) : TMacro;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsContainersRulesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsContainersRulesResource, method Update
  
  TAccountsContainersRulesUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsContainersRulesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(accountId: string; containerId: string; aRule : TRule) : TRule;overload;
    Procedure Delete(accountId: string; containerId: string; ruleId: string);
    Function Get(accountId: string; containerId: string; ruleId: string) : TRule;
    Function List(accountId: string; containerId: string) : TListRulesResponse;
    Function Update(accountId: string; containerId: string; ruleId: string; aRule : TRule; AQuery : string  = '') : TRule;
    Function Update(accountId: string; containerId: string; ruleId: string; aRule : TRule; AQuery : TAccountsContainersRulesupdateOptions) : TRule;
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
    FMacrosInstance : TAccountsContainersMacrosResource;
    FRulesInstance : TAccountsContainersRulesResource;
    FTagsInstance : TAccountsContainersTagsResource;
    FTriggersInstance : TAccountsContainersTriggersResource;
    FVariablesInstance : TAccountsContainersVariablesResource;
    FVersionsInstance : TAccountsContainersVersionsResource;
    Function GetMacrosInstance : TAccountsContainersMacrosResource;virtual;
    Function GetRulesInstance : TAccountsContainersRulesResource;virtual;
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
    Function CreateMacrosResource(AOwner : TComponent) : TAccountsContainersMacrosResource;virtual;overload;
    Function CreateMacrosResource : TAccountsContainersMacrosResource;virtual;overload;
    Function CreateRulesResource(AOwner : TComponent) : TAccountsContainersRulesResource;virtual;overload;
    Function CreateRulesResource : TAccountsContainersRulesResource;virtual;overload;
    Function CreateTagsResource(AOwner : TComponent) : TAccountsContainersTagsResource;virtual;overload;
    Function CreateTagsResource : TAccountsContainersTagsResource;virtual;overload;
    Function CreateTriggersResource(AOwner : TComponent) : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateTriggersResource : TAccountsContainersTriggersResource;virtual;overload;
    Function CreateVariablesResource(AOwner : TComponent) : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateVariablesResource : TAccountsContainersVariablesResource;virtual;overload;
    Function CreateVersionsResource(AOwner : TComponent) : TAccountsContainersVersionsResource;virtual;overload;
    Function CreateVersionsResource : TAccountsContainersVersionsResource;virtual;overload;
    Property MacrosResource : TAccountsContainersMacrosResource Read GetMacrosInstance;
    Property RulesResource : TAccountsContainersRulesResource Read GetRulesInstance;
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
    FContainersMacrosInstance : TAccountsContainersMacrosResource;
    FContainersRulesInstance : TAccountsContainersRulesResource;
    FContainersTagsInstance : TAccountsContainersTagsResource;
    FContainersTriggersInstance : TAccountsContainersTriggersResource;
    FContainersVariablesInstance : TAccountsContainersVariablesResource;
    FContainersVersionsInstance : TAccountsContainersVersionsResource;
    FContainersInstance : TAccountsContainersResource;
    FPermissionsInstance : TAccountsPermissionsResource;
    Function GetContainersMacrosInstance : TAccountsContainersMacrosResource;virtual;
    Function GetContainersRulesInstance : TAccountsContainersRulesResource;virtual;
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
    Function CreateContainersMacrosResource(AOwner : TComponent) : TAccountsContainersMacrosResource;virtual;overload;
    Function CreateContainersMacrosResource : TAccountsContainersMacrosResource;virtual;overload;
    Function CreateContainersRulesResource(AOwner : TComponent) : TAccountsContainersRulesResource;virtual;overload;
    Function CreateContainersRulesResource : TAccountsContainersRulesResource;virtual;overload;
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
    Property ContainersMacrosResource : TAccountsContainersMacrosResource Read GetContainersMacrosInstance;
    Property ContainersRulesResource : TAccountsContainersRulesResource Read GetContainersRulesInstance;
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
    FAccountsContainersMacrosInstance : TAccountsContainersMacrosResource;
    FAccountsContainersRulesInstance : TAccountsContainersRulesResource;
    FAccountsContainersTagsInstance : TAccountsContainersTagsResource;
    FAccountsContainersTriggersInstance : TAccountsContainersTriggersResource;
    FAccountsContainersVariablesInstance : TAccountsContainersVariablesResource;
    FAccountsContainersVersionsInstance : TAccountsContainersVersionsResource;
    FAccountsContainersInstance : TAccountsContainersResource;
    FAccountsPermissionsInstance : TAccountsPermissionsResource;
    FAccountsInstance : TAccountsResource;
    Function GetAccountsContainersMacrosInstance : TAccountsContainersMacrosResource;virtual;
    Function GetAccountsContainersRulesInstance : TAccountsContainersRulesResource;virtual;
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
    Function CreateAccountsContainersMacrosResource(AOwner : TComponent) : TAccountsContainersMacrosResource;virtual;overload;
    Function CreateAccountsContainersMacrosResource : TAccountsContainersMacrosResource;virtual;overload;
    Function CreateAccountsContainersRulesResource(AOwner : TComponent) : TAccountsContainersRulesResource;virtual;overload;
    Function CreateAccountsContainersRulesResource : TAccountsContainersRulesResource;virtual;overload;
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
    Property AccountsContainersMacrosResource : TAccountsContainersMacrosResource Read GetAccountsContainersMacrosInstance;
    Property AccountsContainersRulesResource : TAccountsContainersRulesResource Read GetAccountsContainersRulesInstance;
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


Procedure TAccount.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetshareData(AIndex : Integer; AValue : boolean); 

begin
  If (FshareData=AValue) then exit;
  FshareData:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountAccess
  --------------------------------------------------------------------}


Procedure TAccountAccess.Setpermission(AIndex : Integer; AValue : TStringArray); 

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


Procedure TCondition.Setparameter(AIndex : Integer; AValue : TConditionTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TContainer.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetdomainName(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdomainName=AValue) then exit;
  FdomainName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetenabledBuiltInVariable(AIndex : Integer; AValue : TStringArray); 

begin
  If (FenabledBuiltInVariable=AValue) then exit;
  FenabledBuiltInVariable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetpublicId(AIndex : Integer; AValue : String); 

begin
  If (FpublicId=AValue) then exit;
  FpublicId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SettimeZoneCountryId(AIndex : Integer; AValue : String); 

begin
  If (FtimeZoneCountryId=AValue) then exit;
  FtimeZoneCountryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SettimeZoneId(AIndex : Integer; AValue : String); 

begin
  If (FtimeZoneId=AValue) then exit;
  FtimeZoneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainer.SetusageContext(AIndex : Integer; AValue : TStringArray); 

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


Procedure TContainerAccess.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerAccess.Setpermission(AIndex : Integer; AValue : TStringArray); 

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


Procedure TContainerVersion.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setcontainer(AIndex : Integer; AValue : TContainer); 

begin
  If (Fcontainer=AValue) then exit;
  Fcontainer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.SetcontainerVersionId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerVersionId=AValue) then exit;
  FcontainerVersionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setmacro(AIndex : Integer; AValue : TContainerVersionTypemacroArray); 

begin
  If (Fmacro=AValue) then exit;
  Fmacro:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setrule(AIndex : Integer; AValue : TContainerVersionTyperuleArray); 

begin
  If (Frule=AValue) then exit;
  Frule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Settag(AIndex : Integer; AValue : TContainerVersionTypetagArray); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Settrigger(AIndex : Integer; AValue : TContainerVersionTypetriggerArray); 

begin
  If (Ftrigger=AValue) then exit;
  Ftrigger:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersion.Setvariable(AIndex : Integer; AValue : TContainerVersionTypevariableArray); 

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


Procedure TContainerVersionHeader.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetcontainerVersionId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerVersionId=AValue) then exit;
  FcontainerVersionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumMacros(AIndex : Integer; AValue : String); 

begin
  If (FnumMacros=AValue) then exit;
  FnumMacros:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumRules(AIndex : Integer; AValue : String); 

begin
  If (FnumRules=AValue) then exit;
  FnumRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumTags(AIndex : Integer; AValue : String); 

begin
  If (FnumTags=AValue) then exit;
  FnumTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumTriggers(AIndex : Integer; AValue : String); 

begin
  If (FnumTriggers=AValue) then exit;
  FnumTriggers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContainerVersionHeader.SetnumVariables(AIndex : Integer; AValue : String); 

begin
  If (FnumVariables=AValue) then exit;
  FnumVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateContainerVersionRequestVersionOptions
  --------------------------------------------------------------------}


Procedure TCreateContainerVersionRequestVersionOptions.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionRequestVersionOptions.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionRequestVersionOptions.SetquickPreview(AIndex : Integer; AValue : boolean); 

begin
  If (FquickPreview=AValue) then exit;
  FquickPreview:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateContainerVersionResponse
  --------------------------------------------------------------------}


Procedure TCreateContainerVersionResponse.SetcompilerError(AIndex : Integer; AValue : boolean); 

begin
  If (FcompilerError=AValue) then exit;
  FcompilerError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateContainerVersionResponse.SetcontainerVersion(AIndex : Integer; AValue : TContainerVersion); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListAccountUsersResponse
  --------------------------------------------------------------------}


Procedure TListAccountUsersResponse.SetuserAccess(AIndex : Integer; AValue : TListAccountUsersResponseTypeuserAccessArray); 

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


Procedure TListAccountsResponse.Setaccounts(AIndex : Integer; AValue : TListAccountsResponseTypeaccountsArray); 

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


Procedure TListContainerVersionsResponse.SetcontainerVersion(AIndex : Integer; AValue : TListContainerVersionsResponseTypecontainerVersionArray); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListContainerVersionsResponse.SetcontainerVersionHeader(AIndex : Integer; AValue : TListContainerVersionsResponseTypecontainerVersionHeaderArray); 

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


Procedure TListContainersResponse.Setcontainers(AIndex : Integer; AValue : TListContainersResponseTypecontainersArray); 

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
  TListMacrosResponse
  --------------------------------------------------------------------}


Procedure TListMacrosResponse.Setmacros(AIndex : Integer; AValue : TListMacrosResponseTypemacrosArray); 

begin
  If (Fmacros=AValue) then exit;
  Fmacros:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListMacrosResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'macros' : SetLength(Fmacros,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListRulesResponse
  --------------------------------------------------------------------}


Procedure TListRulesResponse.Setrules(AIndex : Integer; AValue : TListRulesResponseTyperulesArray); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListRulesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rules' : SetLength(Frules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListTagsResponse
  --------------------------------------------------------------------}


Procedure TListTagsResponse.Settags(AIndex : Integer; AValue : TListTagsResponseTypetagsArray); 

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


Procedure TListTriggersResponse.Settriggers(AIndex : Integer; AValue : TListTriggersResponseTypetriggersArray); 

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


Procedure TListVariablesResponse.Setvariables(AIndex : Integer; AValue : TListVariablesResponseTypevariablesArray); 

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


Procedure TMacro.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetdisablingRuleId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdisablingRuleId=AValue) then exit;
  FdisablingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetenablingRuleId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FenablingRuleId=AValue) then exit;
  FenablingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetmacroId(AIndex : Integer; AValue : String); 

begin
  If (FmacroId=AValue) then exit;
  FmacroId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Setparameter(AIndex : Integer; AValue : TMacroTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetscheduleEndMs(AIndex : Integer; AValue : String); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.SetscheduleStartMs(AIndex : Integer; AValue : String); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMacro.Set_type(AIndex : Integer; AValue : String); 

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


Procedure TParameter.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setlist(AIndex : Integer; AValue : TParameterTypelistArray); 

begin
  If (Flist=AValue) then exit;
  Flist:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setmap(AIndex : Integer; AValue : TParameterTypemapArray); 

begin
  If (Fmap=AValue) then exit;
  Fmap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParameter.Setvalue(AIndex : Integer; AValue : String); 

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


Procedure TPublishContainerVersionResponse.SetcompilerError(AIndex : Integer; AValue : boolean); 

begin
  If (FcompilerError=AValue) then exit;
  FcompilerError:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublishContainerVersionResponse.SetcontainerVersion(AIndex : Integer; AValue : TContainerVersion); 

begin
  If (FcontainerVersion=AValue) then exit;
  FcontainerVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRule
  --------------------------------------------------------------------}


Procedure TRule.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setcondition(AIndex : Integer; AValue : TRuleTypeconditionArray); 

begin
  If (Fcondition=AValue) then exit;
  Fcondition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetruleId(AIndex : Integer; AValue : String); 

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
  TTag
  --------------------------------------------------------------------}


Procedure TTag.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetblockingRuleId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FblockingRuleId=AValue) then exit;
  FblockingRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetblockingTriggerId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FblockingTriggerId=AValue) then exit;
  FblockingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetfiringRuleId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FfiringRuleId=AValue) then exit;
  FfiringRuleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetfiringTriggerId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FfiringTriggerId=AValue) then exit;
  FfiringTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetliveOnly(AIndex : Integer; AValue : boolean); 

begin
  If (FliveOnly=AValue) then exit;
  FliveOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setparameter(AIndex : Integer; AValue : TTagTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setpriority(AIndex : Integer; AValue : TParameter); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetscheduleEndMs(AIndex : Integer; AValue : String); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SetscheduleStartMs(AIndex : Integer; AValue : String); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.SettagId(AIndex : Integer; AValue : String); 

begin
  If (FtagId=AValue) then exit;
  FtagId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Set_type(AIndex : Integer; AValue : String); 

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
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTrigger
  --------------------------------------------------------------------}


Procedure TTrigger.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetautoEventFilter(AIndex : Integer; AValue : TTriggerTypeautoEventFilterArray); 

begin
  If (FautoEventFilter=AValue) then exit;
  FautoEventFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcheckValidation(AIndex : Integer; AValue : TParameter); 

begin
  If (FcheckValidation=AValue) then exit;
  FcheckValidation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetcustomEventFilter(AIndex : Integer; AValue : TTriggerTypecustomEventFilterArray); 

begin
  If (FcustomEventFilter=AValue) then exit;
  FcustomEventFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetenableAllVideos(AIndex : Integer; AValue : TParameter); 

begin
  If (FenableAllVideos=AValue) then exit;
  FenableAllVideos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SeteventName(AIndex : Integer; AValue : TParameter); 

begin
  If (FeventName=AValue) then exit;
  FeventName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setfilter(AIndex : Integer; AValue : TTriggerTypefilterArray); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setinterval(AIndex : Integer; AValue : TParameter); 

begin
  If (Finterval=AValue) then exit;
  Finterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setlimit(AIndex : Integer; AValue : TParameter); 

begin
  If (Flimit=AValue) then exit;
  Flimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SettriggerId(AIndex : Integer; AValue : String); 

begin
  If (FtriggerId=AValue) then exit;
  FtriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetuniqueTriggerId(AIndex : Integer; AValue : TParameter); 

begin
  If (FuniqueTriggerId=AValue) then exit;
  FuniqueTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetvideoPercentageList(AIndex : Integer; AValue : TParameter); 

begin
  If (FvideoPercentageList=AValue) then exit;
  FvideoPercentageList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetwaitForTags(AIndex : Integer; AValue : TParameter); 

begin
  If (FwaitForTags=AValue) then exit;
  FwaitForTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrigger.SetwaitForTagsTimeout(AIndex : Integer; AValue : TParameter); 

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


Procedure TUserAccess.SetaccountAccess(AIndex : Integer; AValue : TAccountAccess); 

begin
  If (FaccountAccess=AValue) then exit;
  FaccountAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetcontainerAccess(AIndex : Integer; AValue : TUserAccessTypecontainerAccessArray); 

begin
  If (FcontainerAccess=AValue) then exit;
  FcontainerAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetemailAddress(AIndex : Integer; AValue : String); 

begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserAccess.SetpermissionId(AIndex : Integer; AValue : String); 

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


Procedure TVariable.SetaccountId(AIndex : Integer; AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetcontainerId(AIndex : Integer; AValue : String); 

begin
  If (FcontainerId=AValue) then exit;
  FcontainerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetdisablingTriggerId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdisablingTriggerId=AValue) then exit;
  FdisablingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetenablingTriggerId(AIndex : Integer; AValue : TStringArray); 

begin
  If (FenablingTriggerId=AValue) then exit;
  FenablingTriggerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setnotes(AIndex : Integer; AValue : String); 

begin
  If (Fnotes=AValue) then exit;
  Fnotes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setparameter(AIndex : Integer; AValue : TVariableTypeparameterArray); 

begin
  If (Fparameter=AValue) then exit;
  Fparameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetscheduleEndMs(AIndex : Integer; AValue : String); 

begin
  If (FscheduleEndMs=AValue) then exit;
  FscheduleEndMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetscheduleStartMs(AIndex : Integer; AValue : String); 

begin
  If (FscheduleStartMs=AValue) then exit;
  FscheduleStartMs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetvariableId(AIndex : Integer; AValue : String); 

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
  TAccountsContainersMacrosResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersMacrosResource.ResourceName : String;

begin
  Result:='macros';
end;

Class Function TAccountsContainersMacrosResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersMacrosResource.Create(accountId: string; containerId: string; aMacro : TMacro) : TMacro;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/macros';
  _Methodid   = 'tagmanager.accounts.containers.macros.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aMacro,TMacro) as TMacro;
end;

Procedure TAccountsContainersMacrosResource.Delete(accountId: string; containerId: string; macroId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/macros/{macroId}';
  _Methodid   = 'tagmanager.accounts.containers.macros.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'macroId',macroId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersMacrosResource.Get(accountId: string; containerId: string; macroId: string) : TMacro;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/macros/{macroId}';
  _Methodid   = 'tagmanager.accounts.containers.macros.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'macroId',macroId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMacro) as TMacro;
end;

Function TAccountsContainersMacrosResource.List(accountId: string; containerId: string) : TListMacrosResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/macros';
  _Methodid   = 'tagmanager.accounts.containers.macros.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListMacrosResponse) as TListMacrosResponse;
end;

Function TAccountsContainersMacrosResource.Update(accountId: string; containerId: string; macroId: string; aMacro : TMacro; AQuery : string = '') : TMacro;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/macros/{macroId}';
  _Methodid   = 'tagmanager.accounts.containers.macros.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'macroId',macroId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aMacro,TMacro) as TMacro;
end;


Function TAccountsContainersMacrosResource.Update(accountId: string; containerId: string; macroId: string; aMacro : TMacro; AQuery : TAccountsContainersMacrosupdateOptions) : TMacro;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,macroId,aMacro,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsContainersRulesResource
  --------------------------------------------------------------------}


Class Function TAccountsContainersRulesResource.ResourceName : String;

begin
  Result:='rules';
end;

Class Function TAccountsContainersRulesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtagmanagerAPI;
end;

Function TAccountsContainersRulesResource.Create(accountId: string; containerId: string; aRule : TRule) : TRule;

Const
  _HTTPMethod = 'POST';
  _Path       = 'accounts/{accountId}/containers/{containerId}/rules';
  _Methodid   = 'tagmanager.accounts.containers.rules.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRule,TRule) as TRule;
end;

Procedure TAccountsContainersRulesResource.Delete(accountId: string; containerId: string; ruleId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/containers/{containerId}/rules/{ruleId}';
  _Methodid   = 'tagmanager.accounts.containers.rules.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'ruleId',ruleId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsContainersRulesResource.Get(accountId: string; containerId: string; ruleId: string) : TRule;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/rules/{ruleId}';
  _Methodid   = 'tagmanager.accounts.containers.rules.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'ruleId',ruleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRule) as TRule;
end;

Function TAccountsContainersRulesResource.List(accountId: string; containerId: string) : TListRulesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/containers/{containerId}/rules';
  _Methodid   = 'tagmanager.accounts.containers.rules.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListRulesResponse) as TListRulesResponse;
end;

Function TAccountsContainersRulesResource.Update(accountId: string; containerId: string; ruleId: string; aRule : TRule; AQuery : string = '') : TRule;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'accounts/{accountId}/containers/{containerId}/rules/{ruleId}';
  _Methodid   = 'tagmanager.accounts.containers.rules.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'containerId',containerId,'ruleId',ruleId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aRule,TRule) as TRule;
end;


Function TAccountsContainersRulesResource.Update(accountId: string; containerId: string; ruleId: string; aRule : TRule; AQuery : TAccountsContainersRulesupdateOptions) : TRule;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'fingerprint',AQuery.fingerprint);
  Result:=Update(accountId,containerId,ruleId,aRule,_Q);
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



Function TAccountsContainersResource.GetMacrosInstance : TAccountsContainersMacrosResource;

begin
  if (FMacrosInstance=Nil) then
    FMacrosInstance:=CreateMacrosResource;
  Result:=FMacrosInstance;
end;

Function TAccountsContainersResource.CreateMacrosResource : TAccountsContainersMacrosResource;

begin
  Result:=CreateMacrosResource(Self);
end;


Function TAccountsContainersResource.CreateMacrosResource(AOwner : TComponent) : TAccountsContainersMacrosResource;

begin
  Result:=TAccountsContainersMacrosResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsContainersResource.GetRulesInstance : TAccountsContainersRulesResource;

begin
  if (FRulesInstance=Nil) then
    FRulesInstance:=CreateRulesResource;
  Result:=FRulesInstance;
end;

Function TAccountsContainersResource.CreateRulesResource : TAccountsContainersRulesResource;

begin
  Result:=CreateRulesResource(Self);
end;


Function TAccountsContainersResource.CreateRulesResource(AOwner : TComponent) : TAccountsContainersRulesResource;

begin
  Result:=TAccountsContainersRulesResource.Create(AOwner);
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



Function TAccountsResource.GetContainersMacrosInstance : TAccountsContainersMacrosResource;

begin
  if (FContainersMacrosInstance=Nil) then
    FContainersMacrosInstance:=CreateContainersMacrosResource;
  Result:=FContainersMacrosInstance;
end;

Function TAccountsResource.CreateContainersMacrosResource : TAccountsContainersMacrosResource;

begin
  Result:=CreateContainersMacrosResource(Self);
end;


Function TAccountsResource.CreateContainersMacrosResource(AOwner : TComponent) : TAccountsContainersMacrosResource;

begin
  Result:=TAccountsContainersMacrosResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetContainersRulesInstance : TAccountsContainersRulesResource;

begin
  if (FContainersRulesInstance=Nil) then
    FContainersRulesInstance:=CreateContainersRulesResource;
  Result:=FContainersRulesInstance;
end;

Function TAccountsResource.CreateContainersRulesResource : TAccountsContainersRulesResource;

begin
  Result:=CreateContainersRulesResource(Self);
end;


Function TAccountsResource.CreateContainersRulesResource(AOwner : TComponent) : TAccountsContainersRulesResource;

begin
  Result:=TAccountsContainersRulesResource.Create(AOwner);
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
  Result:='20150121';
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
  Result:='API for accessing Tag Manager accounts and containers.';
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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TTagmanagerAPI.APIbasePath : string;

begin
  Result:='/tagmanager/v1/';
end;

Class Function TTagmanagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/tagmanager/v1/';
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
  TListAccountUsersResponse.RegisterObject;
  TListAccountsResponse.RegisterObject;
  TListContainerVersionsResponse.RegisterObject;
  TListContainersResponse.RegisterObject;
  TListMacrosResponse.RegisterObject;
  TListRulesResponse.RegisterObject;
  TListTagsResponse.RegisterObject;
  TListTriggersResponse.RegisterObject;
  TListVariablesResponse.RegisterObject;
  TMacro.RegisterObject;
  TParameter.RegisterObject;
  TPublishContainerVersionResponse.RegisterObject;
  TRule.RegisterObject;
  TTag.RegisterObject;
  TTrigger.RegisterObject;
  TUserAccess.RegisterObject;
  TVariable.RegisterObject;
end;


Function TTagmanagerAPI.GetAccountsContainersMacrosInstance : TAccountsContainersMacrosResource;

begin
  if (FAccountsContainersMacrosInstance=Nil) then
    FAccountsContainersMacrosInstance:=CreateAccountsContainersMacrosResource;
  Result:=FAccountsContainersMacrosInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersMacrosResource : TAccountsContainersMacrosResource;

begin
  Result:=CreateAccountsContainersMacrosResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersMacrosResource(AOwner : TComponent) : TAccountsContainersMacrosResource;

begin
  Result:=TAccountsContainersMacrosResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TTagmanagerAPI.GetAccountsContainersRulesInstance : TAccountsContainersRulesResource;

begin
  if (FAccountsContainersRulesInstance=Nil) then
    FAccountsContainersRulesInstance:=CreateAccountsContainersRulesResource;
  Result:=FAccountsContainersRulesInstance;
end;

Function TTagmanagerAPI.CreateAccountsContainersRulesResource : TAccountsContainersRulesResource;

begin
  Result:=CreateAccountsContainersRulesResource(Self);
end;


Function TTagmanagerAPI.CreateAccountsContainersRulesResource(AOwner : TComponent) : TAccountsContainersRulesResource;

begin
  Result:=TAccountsContainersRulesResource.Create(AOwner);
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
