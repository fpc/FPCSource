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
//Generated on: 9-5-15 13:22:58
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = class;
  TAccountAccess = class;
  TCondition = class;
  TContainer = class;
  TContainerAccess = class;
  TContainerVersion = class;
  TContainerVersionHeader = class;
  TCreateContainerVersionRequestVersionOptions = class;
  TCreateContainerVersionResponse = class;
  TListAccountUsersResponse = class;
  TListAccountsResponse = class;
  TListContainerVersionsResponse = class;
  TListContainersResponse = class;
  TListMacrosResponse = class;
  TListRulesResponse = class;
  TListTagsResponse = class;
  TListTriggersResponse = class;
  TListVariablesResponse = class;
  TMacro = class;
  TParameter = class;
  TPublishContainerVersionResponse = class;
  TRule = class;
  TTag = class;
  TTrigger = class;
  TUserAccess = class;
  TVariable = class;
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
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method Update
  
  TAccountsUpdateOptions = Record
    fingerprint : String;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string) : TAccount;
    Function List : TListAccountsResponse;
    Function Update(accountId: string; aAccount : TAccount; AQuery : string  = '') : TAccount;
    Function Update(accountId: string; aAccount : TAccount; AQuery : TAccountsupdateOptions) : TAccount;
  end;
  
  
  { --------------------------------------------------------------------
    TTagmanagerAPI
    --------------------------------------------------------------------}
  
  TTagmanagerAPI = Class(TGoogleAPI)
  Private
    FAccountsInstance : TAccountsResource;
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
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    //Add default on-demand instances for resources
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





{ --------------------------------------------------------------------
  TListAccountsResponse
  --------------------------------------------------------------------}


Procedure TListAccountsResponse.Setaccounts(AIndex : Integer; AValue : TListAccountsResponseTypeaccountsArray); 

begin
  If (Faccounts=AValue) then exit;
  Faccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;





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





{ --------------------------------------------------------------------
  TListContainersResponse
  --------------------------------------------------------------------}


Procedure TListContainersResponse.Setcontainers(AIndex : Integer; AValue : TListContainersResponseTypecontainersArray); 

begin
  If (Fcontainers=AValue) then exit;
  Fcontainers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListMacrosResponse
  --------------------------------------------------------------------}


Procedure TListMacrosResponse.Setmacros(AIndex : Integer; AValue : TListMacrosResponseTypemacrosArray); 

begin
  If (Fmacros=AValue) then exit;
  Fmacros:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListRulesResponse
  --------------------------------------------------------------------}


Procedure TListRulesResponse.Setrules(AIndex : Integer; AValue : TListRulesResponseTyperulesArray); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTagsResponse
  --------------------------------------------------------------------}


Procedure TListTagsResponse.Settags(AIndex : Integer; AValue : TListTagsResponseTypetagsArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListTriggersResponse
  --------------------------------------------------------------------}


Procedure TListTriggersResponse.Settriggers(AIndex : Integer; AValue : TListTriggersResponseTypetriggersArray); 

begin
  If (Ftriggers=AValue) then exit;
  Ftriggers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListVariablesResponse
  --------------------------------------------------------------------}


Procedure TListVariablesResponse.Setvariables(AIndex : Integer; AValue : TListVariablesResponseTypevariablesArray); 

begin
  If (Fvariables=AValue) then exit;
  Fvariables:=AValue;
  MarkPropertyChanged(AIndex);
end;





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
  Result.API:=Self;
end;



initialization
  TTagmanagerAPI.RegisterAPI;
end.
